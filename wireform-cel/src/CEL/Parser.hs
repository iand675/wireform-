{-# LANGUAGE BangPatterns #-}

{- | Lexer and recursive-descent parser for CEL source text.

The parser implements the grammar from the CEL language definition,
including the precedence / associativity table, the full lexical syntax for
numeric, string, and bytes literals (raw and triple-quoted forms, the
complete escape-sequence set), reserved-word rejection, and the
message/struct construction form @Name{...}@.

Negative integer literals are folded at parse time so that
@-9223372036854775808@ (whose magnitude does not fit in a signed 64-bit
integer) parses to the minimum @int@ value, matching reference CEL behavior.
-}
module CEL.Parser (
  parse,
  parseExpr,
) where

import CEL.Internal.Number (parseDecimalDouble)
import CEL.Syntax
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (chr, digitToInt, ord)
import Data.Functor (($>))
import Data.Int (Int64)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64, Word8)
import Wireform.Parser
import Wireform.Parser.Driver (parseByteString)
import Wireform.Parser.Internal (Pure)


-- | Parse CEL source text into an 'Expr', or return a human-readable error.
parse :: Text -> Either String Expr
parse src = do
  toks <- tokenize src
  case runP (parseExpr <* expectTok TkEOF) toks of
    Left msg -> Left msg
    Right (e, _) -> Right e


----------------------------------------------------------------------
-- Tokens
----------------------------------------------------------------------

data Tok
  = TkIntLit !Integer
  | TkUIntLit !Integer
  | TkDoubleLit !Double
  | TkStringLit !Text
  | TkBytesLit !ByteString
  | TkBool !Bool
  | TkNull
  | TkIdent !Text
  | TkEscIdent !Text
  | TkLParen
  | TkRParen
  | TkLBracket
  | TkRBracket
  | TkLBrace
  | TkRBrace
  | TkComma
  | TkDot
  | TkColon
  | TkQuestion
  | TkOr
  | TkAnd
  | TkNot
  | TkMinus
  | TkPlus
  | TkStar
  | TkSlash
  | TkPercent
  | TkEq
  | TkNe
  | TkLt
  | TkLe
  | TkGt
  | TkGe
  | TkIn
  | TkEOF
  deriving stock (Eq, Show)


{- | Reserved identifiers that may not be used as variable / function /
selector / field names.
-}
reservedWords :: Set.Set Text
reservedWords =
  Set.fromList
    [ "as"
    , "break"
    , "const"
    , "continue"
    , "else"
    , "for"
    , "function"
    , "if"
    , "import"
    , "let"
    , "loop"
    , "package"
    , "namespace"
    , "return"
    , "var"
    , "void"
    , "while"
    ]


----------------------------------------------------------------------
-- Lexer
----------------------------------------------------------------------

-- The lexer is a 'Wireform.Parser' tokenizer over the UTF-8 bytes of the
-- source. Whitespace and @//@ line comments are skipped between tokens; the
-- delicate parts (escape interpretation, numeric value parsing) are delegated
-- to the pure helpers further below.
type L = Parser Pure String


-- | Tokenize CEL source text into the token stream consumed by the grammar.
tokenize :: Text -> Either String [Tok]
tokenize src = case parseByteString lexTokens (TE.encodeUtf8 src) of
  Right toks -> Right toks
  Left perr -> Left (renderParseError perr)


renderParseError :: ParseError String -> String
renderParseError = \case
  ParseErr _ e -> e
  ParseUnexpectedEof _ _ -> "unexpected end of input"
  _ -> "lexical error"


lexTokens :: L [Tok]
lexTokens = skipTrivia *> loop []
  where
    loop acc = do
      end <- atEnd
      if end
        then pure (reverse (TkEOF : acc))
        else do
          t <- lexToken
          skipTrivia
          loop (t : acc)


-- Skip whitespace and @//@ line comments.
skipTrivia :: L ()
skipTrivia = skipMany (skipWhitespace <|> skipLineComment)
  where
    skipWhitespace = skipSatisfyAscii isSpaceChar
    isSpaceChar c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f'
    skipLineComment = (word8 slash *> word8 slash) *> skipMany (satisfy (/= '\n'))


-- One token. The driver loop guarantees we are not at end-of-input.
lexToken :: L Tok
lexToken = do
  c <- lookahead anyChar
  case c of
    '(' -> consume TkLParen
    ')' -> consume TkRParen
    '[' -> consume TkLBracket
    ']' -> consume TkRBracket
    '{' -> consume TkLBrace
    '}' -> consume TkRBrace
    ',' -> consume TkComma
    ':' -> consume TkColon
    '?' -> consume TkQuestion
    '+' -> consume TkPlus
    '-' -> consume TkMinus
    '*' -> consume TkStar
    '/' -> consume TkSlash
    '%' -> consume TkPercent
    '.' -> branch (lookahead (word8 dot *> satisfyAscii isDigit)) lexNumber (consume TkDot)
    '|' -> (byteString "||" $> TkOr) <|> err "unexpected '|' (did you mean '||'?)"
    '&' -> (byteString "&&" $> TkAnd) <|> err "unexpected '&' (did you mean '&&'?)"
    '=' -> (byteString "==" $> TkEq) <|> err "unexpected character: '='"
    '!' -> (byteString "!=" $> TkNe) <|> consume TkNot
    '<' -> (byteString "<=" $> TkLe) <|> consume TkLt
    '>' -> (byteString ">=" $> TkGe) <|> consume TkGt
    '"' -> lexString False
    '\'' -> lexString False
    '\x60' -> lexBacktick
    _
      | isStrPrefix c -> lexPrefixedOrIdent
      | isIdentStart c -> lexIdent
      | isDigit c -> lexNumber
      | otherwise -> err ("unexpected character: " ++ show c)
  where
    consume t = anyChar_ $> t


-- Identifier (or a keyword, classified by 'identTok').
lexIdent :: L Tok
lexIdent = do
  name <- byteStringOf (satisfy isIdentStart *> skipMany (satisfy isIdentChar))
  pure (identTok (TE.decodeUtf8 name))


-- A string/bytes prefix (@r@/@b@) directly followed by a quote, otherwise an
-- identifier that happens to start with that letter.
lexPrefixedOrIdent :: L Tok
lexPrefixedOrIdent = stringWithPrefix <|> lexIdent
  where
    stringWithPrefix = do
      (raw, bytes) <- scanPrefix
      if bytes then lexBytes raw else lexString raw


-- Consume a run of @r@/@R@/@b@/@B@ prefix letters (at most one of each),
-- succeeding only when directly followed by a quote.
scanPrefix :: L (Bool, Bool)
scanPrefix = go False False
  where
    go raw bytes =
      withOption
        (satisfyAscii isPrefixLetter)
        ( \c ->
            if c == 'r' || c == 'R'
              then if raw then empty else go True bytes
              else if bytes then empty else go raw True
        )
        (lookahead (satisfyAscii isQuote) $> (raw, bytes))
    isPrefixLetter c = c == 'r' || c == 'R' || c == 'b' || c == 'B'


-- Backtick-quoted (escaped) identifier, e.g. @`content-type`@.
lexBacktick :: L Tok
lexBacktick = do
  _ <- word8 backtick
  name <- byteStringOf (skipMany (satisfy (/= '\x60')))
  word8 backtick `cut` "unterminated backtick-quoted identifier"
  pure (TkEscIdent (TE.decodeUtf8 name))


----------------------------------------------------------------------
-- String / bytes literals
----------------------------------------------------------------------

lexString :: Bool -> L Tok
lexString raw = do
  content <- scanQuoted raw
  either err (pure . TkStringLit) (processString raw content)


lexBytes :: Bool -> L Tok
lexBytes raw = do
  content <- scanQuoted raw
  either err (pure . TkBytesLit) (processBytes raw content)


-- Read a quoted literal (single- or triple-quoted), returning the raw,
-- still-escaped content as a 'String'.
scanQuoted :: Bool -> L String
scanQuoted raw = do
  q <- satisfyAscii isQuote
  let qb = byteOf q
  branch
    (lookahead (word8 qb *> word8 qb))
    (skip 2 *> scanTriple raw q qb)
    (scanSingle raw q)


scanSingle :: Bool -> Char -> L String
scanSingle raw q = do
  content <- byteStringOf (skipMany singleChar)
  word8 (byteOf q) `cut` "unterminated string literal"
  pure (decodeBytes content)
  where
    singleChar
      | raw = normalChar
      | otherwise =
          branch (word8 backslash) (anyChar_ `cut` "unterminated escape in string literal") normalChar
    normalChar =
      withOption
        (lookahead anyChar)
        ( \c ->
            if c == '\n' || c == '\r'
              then err "newline in single-quoted string literal"
              else if c == q then empty else anyChar_
        )
        empty


scanTriple :: Bool -> Char -> Word8 -> L String
scanTriple raw _q qb = do
  content <- byteStringOf (skipMany tripleChar)
  (word8 qb *> word8 qb *> word8 qb) `cut` "unterminated triple-quoted string literal"
  pure (decodeBytes content)
  where
    tripleChar = branch (lookahead (word8 qb *> word8 qb *> word8 qb)) empty body
    body
      | raw = anyChar_
      | otherwise =
          branch (word8 backslash) (anyChar_ `cut` "unterminated escape in string literal") anyChar_


----------------------------------------------------------------------
-- Numeric literals
----------------------------------------------------------------------

lexNumber :: L Tok
lexNumber = hexLiteral <|> decimalLiteral


hexLiteral :: L Tok
hexLiteral = do
  _ <- byteString "0x" <|> byteString "0X"
  digits <- byteStringOf (skipSome (satisfyAscii isHexDigit)) `cut` "malformed hex literal"
  integerToken 16 16 digits


decimalLiteral :: L Tok
decimalLiteral = do
  intPart <- byteStringOf (skipMany (satisfyAscii isDigit))
  mFrac <- optional fraction
  mExp <- optional exponentPart
  case (mFrac, mExp) of
    (Nothing, Nothing)
      | BS.null intPart -> empty -- a lone '.' is not a number
      | otherwise -> integerToken 10 20 intPart
    _ ->
      let lexeme = TE.decodeLatin1 (intPart <> maybe BS.empty id mFrac <> maybe BS.empty id mExp)
      in case parseDecimalDouble lexeme of
           Just d | isInfinite d -> err "double literal out of range"
           Just d -> pure (TkDoubleLit d)
           Nothing -> err "malformed double literal"


-- Bound the accumulation before converting. Leading zeros do not affect
-- either the range or the signed and unsigned suffix rules.
integerToken :: Word64 -> Int -> ByteString -> L Tok
integerToken base maxDigits digits
  | count > maxDigits = outOfRange
  -- Nineteen decimal or sixteen hexadecimal digits always fit a Word64.
  -- Check the twentieth decimal digit before multiplying the prefix.
  | base == 10, count == 20, Just (prefix, lastByte) <- BS.unsnoc significant =
      let leading = BS.foldl' step 0 prefix
          trailing = digitValue lastByte
          (bound, lastAllowed) = (maxBound :: Word64) `quotRem` 10
      in if leading > bound || (leading == bound && trailing > lastAllowed)
           then outOfRange
           else emit (leading * 10 + trailing)
  | otherwise = emit (BS.foldl' step 0 significant)
  where
    significant = BS.dropWhile (== 48) digits
    count = BS.length significant
    outOfRange = err "integer literal out of range"
    step acc d = acc * base + digitValue d
    digitValue d
      | d <= 57 = fromIntegral (d - 48)
      | d <= 70 = fromIntegral (d - 55)
      | otherwise = fromIntegral (d - 87)
    emit n = let magnitude = toInteger n in withUintSuffix (TkUIntLit magnitude) (TkIntLit magnitude)


-- A fraction @.digits@ — the dot is only consumed when a digit follows.
fraction :: L ByteString
fraction = byteStringOf (word8 dot *> skipSome (satisfyAscii isDigit))


-- An exponent @e[+-]?digits@.
exponentPart :: L ByteString
exponentPart =
  byteStringOf
    ( satisfyAscii (\c -> c == 'e' || c == 'E')
        *> optional_ (satisfyAscii (\c -> c == '+' || c == '-'))
        *> skipSome (satisfyAscii isDigit)
    )


-- Optionally consume a @u@/@U@ suffix, choosing the unsigned vs signed token.
withUintSuffix :: Tok -> Tok -> L Tok
withUintSuffix uintTok intTok =
  withOption (satisfyAscii (\c -> c == 'u' || c == 'U')) (const (pure uintTok)) (pure intTok)


----------------------------------------------------------------------
-- Small lexing helpers
----------------------------------------------------------------------

isQuote :: Char -> Bool
isQuote c = c == '"' || c == '\''


-- Bytes captured by the tokenizer are always slices of valid UTF-8 (the source
-- is decoded from 'Text'), taken on character boundaries.
decodeBytes :: ByteString -> String
decodeBytes = T.unpack . TE.decodeUtf8


byteOf :: Char -> Word8
byteOf = fromIntegral . ord


dot, slash, backslash, backtick :: Word8
dot = byteOf '.'
slash = byteOf '/'
backslash = byteOf '\\'
backtick = byteOf '\x60'


identTok :: Text -> Tok
identTok t = case t of
  "true" -> TkBool True
  "false" -> TkBool False
  "null" -> TkNull
  "in" -> TkIn
  _ -> TkIdent t


isStrPrefix :: Char -> Bool
isStrPrefix c = c == 'r' || c == 'R' || c == 'b' || c == 'B'


isIdentStart :: Char -> Bool
isIdentStart c = c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')


isIdentChar :: Char -> Bool
isIdentChar c = isIdentStart c || (c >= '0' && c <= '9')


----------------------------------------------------------------------
-- Numeric value helpers
----------------------------------------------------------------------

isHexDigit :: Char -> Bool
isHexDigit c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')


----------------------------------------------------------------------
-- String / bytes escape processing
----------------------------------------------------------------------

{- | Process the content of a string literal, interpreting escapes (unless
raw) and validating Unicode code points.
-}
processString :: Bool -> String -> Either String Text
processString True content = Right (T.pack content)
processString False content = T.pack <$> goS content
  where
    goS [] = Right []
    goS ('\\' : rest) = do
      (cp, rest') <- escapeCodePoint rest
      validateCodePoint cp
      (chr cp :) <$> goS rest'
    goS (c : cs) = (c :) <$> goS cs


-- | Process the content of a bytes literal into a raw byte sequence.
processBytes :: Bool -> String -> Either String ByteString
processBytes True content = Right (TE.encodeUtf8 (T.pack content))
processBytes False content = BS.pack <$> goB content
  where
    goB [] = Right []
    goB ('\\' : rest) = do
      (bytes, rest') <- escapeBytes rest
      (bytes ++) <$> goB rest'
    goB (c : cs) = (utf8Bytes c ++) <$> goB cs


utf8Bytes :: Char -> [Word8]
utf8Bytes = BS.unpack . TE.encodeUtf8 . T.singleton


{- | Decode one escape sequence (after the backslash) for a string literal,
returning a Unicode code point and the remaining input.
-}
escapeCodePoint :: String -> Either String (Int, String)
escapeCodePoint [] = Left "unterminated escape sequence"
escapeCodePoint (c : cs) = case c of
  'a' -> Right (0x07, cs)
  'b' -> Right (0x08, cs)
  'f' -> Right (0x0C, cs)
  'n' -> Right (0x0A, cs)
  'r' -> Right (0x0D, cs)
  't' -> Right (0x09, cs)
  'v' -> Right (0x0B, cs)
  '\\' -> Right (ord '\\', cs)
  '?' -> Right (ord '?', cs)
  '"' -> Right (ord '"', cs)
  '\'' -> Right (ord '\'', cs)
  '`' -> Right (ord '`', cs)
  'x' -> hexN 2 cs
  'X' -> hexN 2 cs
  'u' -> hexN 4 cs
  'U' -> hexN 8 cs
  _
    | c >= '0' && c <= '3' -> octal3 (c : cs)
    | otherwise -> Left ("invalid escape sequence: \\" ++ [c])


{- | Decode one escape sequence for a bytes literal, where @\\x@ and octal
escapes denote raw octets rather than code points.
-}
escapeBytes :: String -> Either String ([Word8], String)
escapeBytes [] = Left "unterminated escape sequence"
escapeBytes (c : cs) = case c of
  'a' -> Right ([0x07], cs)
  'b' -> Right ([0x08], cs)
  'f' -> Right ([0x0C], cs)
  'n' -> Right ([0x0A], cs)
  'r' -> Right ([0x0D], cs)
  't' -> Right ([0x09], cs)
  'v' -> Right ([0x0B], cs)
  '\\' -> Right ([fromIntegral (ord '\\')], cs)
  '?' -> Right ([fromIntegral (ord '?')], cs)
  '"' -> Right ([fromIntegral (ord '"')], cs)
  '\'' -> Right ([fromIntegral (ord '\'')], cs)
  '`' -> Right ([fromIntegral (ord '`')], cs)
  'x' -> byteHex cs
  'X' -> byteHex cs
  'u' -> do (cp, r) <- hexN 4 cs; validateCodePoint cp; Right (utf8Bytes (chr cp), r)
  'U' -> do (cp, r) <- hexN 8 cs; validateCodePoint cp; Right (utf8Bytes (chr cp), r)
  _
    | c >= '0' && c <= '7' -> do (cp, r) <- octalByte (c : cs); Right ([fromIntegral cp], r)
    | otherwise -> Left ("invalid escape sequence: \\" ++ [c])
  where
    byteHex s = do (n, r) <- hexN 2 s; Right ([fromIntegral n], r)


hexN :: Int -> String -> Either String (Int, String)
hexN n s =
  let (ds, rest) = splitAt n s
  in if length ds == n && all isHexDigit ds
       then Right (foldl' (\acc d -> acc * 16 + digitToInt d) 0 ds, rest)
       else Left "invalid hexadecimal escape sequence"


octal3 :: String -> Either String (Int, String)
octal3 s =
  let (ds, rest) = splitAt 3 s
  in if length ds == 3 && all isOctDigit ds
       then
         let v = foldl' (\acc d -> acc * 8 + digitToInt d) 0 ds
         in if v <= 0o377 then Right (v, rest) else Left "octal escape out of range"
       else Left "invalid octal escape sequence"


octalByte :: String -> Either String (Int, String)
octalByte = octal3


isOctDigit :: Char -> Bool
isOctDigit c = c >= '0' && c <= '7'


validateCodePoint :: Int -> Either String ()
validateCodePoint cp
  | cp < 0 || cp > 0x10FFFF = Left "invalid Unicode code point in escape sequence"
  | cp >= 0xD800 && cp <= 0xDFFF = Left "surrogate code point in escape sequence"
  | otherwise = Right ()


----------------------------------------------------------------------
-- Parser monad
----------------------------------------------------------------------

newtype P a = P {runP :: [Tok] -> Either String (a, [Tok])}


instance Functor P where
  fmap f (P g) = P $ \ts -> case g ts of
    Left e -> Left e
    Right (a, ts') -> Right (f a, ts')


instance Applicative P where
  pure a = P $ \ts -> Right (a, ts)
  P f <*> P g = P $ \ts -> case f ts of
    Left e -> Left e
    Right (h, ts') -> case g ts' of
      Left e -> Left e
      Right (a, ts'') -> Right (h a, ts'')


instance Monad P where
  P g >>= f = P $ \ts -> case g ts of
    Left e -> Left e
    Right (a, ts') -> runP (f a) ts'


peek :: P Tok
peek = P $ \ts -> case ts of
  (t : _) -> Right (t, ts)
  [] -> Right (TkEOF, ts)


advance :: P Tok
advance = P $ \ts -> case ts of
  (t : rest) -> Right (t, rest)
  [] -> Right (TkEOF, [])


expectTok :: Tok -> P ()
expectTok expected = do
  t <- advance
  if t == expected
    then pure ()
    else parseError ("expected " ++ show expected ++ " but found " ++ show t)


parseError :: String -> P a
parseError msg = P $ \_ -> Left msg


{- | Consume an identifier in a position where it names a variable / function:
reserved words are rejected, but backtick-escaped identifiers are accepted.
-}
identName :: P Text
identName = do
  t <- advance
  case t of
    TkIdent n
      | Set.member n reservedWords -> parseError ("reserved word used as name: " ++ T.unpack n)
      | otherwise -> pure n
    TkEscIdent n -> pure n
    _ -> parseError ("expected identifier but found " ++ show t)


{- | Consume a selector / field-init name. Per the grammar @SELECTOR@ only
excludes the keyword tokens (@true@/@false@/@null@/@in@, which are not
'TkIdent's), so reserved words such as @as@ or @for@ /are/ valid here.
Backtick-escaped identifiers are also accepted.
-}
selectorName :: P Text
selectorName = do
  t <- advance
  case t of
    TkIdent n -> pure n
    TkEscIdent n -> pure n
    _ -> parseError ("expected selector but found " ++ show t)


----------------------------------------------------------------------
-- Grammar
----------------------------------------------------------------------

{- | Top-level expression parser (exposed for testing against the token
stream is not needed; 'parse' is the public entry point).
-}
parseExpr :: P Expr
parseExpr = do
  c <- parseOr
  t <- peek
  case t of
    TkQuestion -> do
      _ <- advance
      thenE <- parseOr
      expectTok TkColon
      elseE <- parseExpr
      pure (ECond c thenE elseE)
    _ -> pure c


parseOr :: P Expr
parseOr = do
  l <- parseAnd
  goOr l
  where
    goOr l = do
      t <- peek
      case t of
        TkOr -> do _ <- advance; r <- parseAnd; goOr (EOr l r)
        _ -> pure l


parseAnd :: P Expr
parseAnd = do
  l <- parseRel
  goAnd l
  where
    goAnd l = do
      t <- peek
      case t of
        TkAnd -> do _ <- advance; r <- parseRel; goAnd (EAnd l r)
        _ -> pure l


parseRel :: P Expr
parseRel = do
  l <- parseAdd
  goRel l
  where
    goRel l = do
      t <- peek
      case relOp t of
        Just op -> do _ <- advance; r <- parseAdd; goRel (ERel op l r)
        Nothing -> pure l


relOp :: Tok -> Maybe RelOp
relOp = \case
  TkEq -> Just Eq
  TkNe -> Just Ne
  TkLt -> Just Lt
  TkLe -> Just Le
  TkGt -> Just Gt
  TkGe -> Just Ge
  TkIn -> Just In
  _ -> Nothing


parseAdd :: P Expr
parseAdd = do
  l <- parseMul
  goAdd l
  where
    goAdd l = do
      t <- peek
      case t of
        TkPlus -> do _ <- advance; r <- parseMul; goAdd (EArith Add l r)
        TkMinus -> do _ <- advance; r <- parseMul; goAdd (EArith Sub l r)
        _ -> pure l


parseMul :: P Expr
parseMul = do
  l <- parseUnary
  goMul l
  where
    goMul l = do
      t <- peek
      case t of
        TkStar -> do _ <- advance; r <- parseUnary; goMul (EArith Mul l r)
        TkSlash -> do _ <- advance; r <- parseUnary; goMul (EArith Div l r)
        TkPercent -> do _ <- advance; r <- parseUnary; goMul (EArith Mod l r)
        _ -> pure l


parseUnary :: P Expr
parseUnary = do
  t <- peek
  case t of
    TkNot -> do
      n <- countRun TkNot
      m <- parseMember
      pure (applyN n ENot m)
    TkMinus -> do
      n <- countRun TkMinus
      parseNegated n
    _ -> parseMember


{- | After consuming a run of @n@ unary minus signs, parse the operand,
folding a directly-following integer literal into a signed literal so that
the minimum 64-bit integer can be represented.
-}
parseNegated :: Int -> P Expr
parseNegated n = do
  t <- peek
  case t of
    TkIntLit mag -> do
      _ <- advance
      let signed = if odd n then negate mag else mag
      lit <- intLiteral signed
      parseMemberSuffix lit
    _ -> do
      m <- parseMember
      pure (applyN n ENeg m)


countRun :: Tok -> P Int
countRun tk = go 0
  where
    go !acc = do
      t <- peek
      if t == tk then advance >> go (acc + 1) else pure acc


applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN k f x = applyN (k - 1) f (f x)


intLiteral :: Integer -> P Expr
intLiteral n
  | n < intMin || n > intMax = parseError "integer literal out of range"
  | otherwise = pure (ELit (LInt (fromInteger n)))
  where
    intMin = toInteger (minBound :: Int64)
    intMax = toInteger (maxBound :: Int64)


uintLiteral :: Integer -> P Expr
uintLiteral n
  | n < 0 || n > toInteger (maxBound :: Word64) = parseError "unsigned integer literal out of range"
  | otherwise = pure (ELit (LUInt (fromInteger n)))


parseMember :: P Expr
parseMember = do
  p <- parsePrimary
  parseMemberSuffix p


parseMemberSuffix :: Expr -> P Expr
parseMemberSuffix e = do
  t <- peek
  case t of
    TkDot -> do
      _ <- advance
      sel <- selectorName
      t2 <- peek
      case t2 of
        TkLParen -> do
          args <- parseCallArgs
          validateMacroBinders sel args
          parseMemberSuffix (ECall (Just e) sel args)
        _ -> parseMemberSuffix (ESelect e sel)
    TkLBracket -> do
      _ <- advance
      idx <- parseExpr
      expectTok TkRBracket
      parseMemberSuffix (EIndex e idx)
    TkLBrace
      | Just (root, segs) <- identPath e -> do
          fields <- parseFieldInits
          parseMemberSuffix (EStruct root segs fields)
    _ -> pure e


-- Recognized macro signatures require identifiers before evaluation.
validateMacroBinders :: Text -> [Expr] -> P ()
validateMacroBinders name args = case args of
  [v, _] | quantifier || name == "map" || name == "filter" -> void (binder v)
  [v, _, _] | name == "map" -> void (binder v)
  [a, b, _] | quantifier || transform -> validatePair a b
  [a, b, _, _] | transform -> validatePair a b
  _ -> pure ()
  where
    quantifier = name `elem` ["all", "exists", "exists_one", "existsOne"]
    transform = name == "transformList" || name == "transformMap"
    binder (EIdent False v) = pure v
    binder _ = parseError ("iteration variable in " <> T.unpack name <> " must be a simple identifier")
    validatePair a b = do
      first <- binder a
      second <- binder b
      when (first == second) $
        parseError ("duplicate iteration variable in " <> T.unpack name <> ": " <> T.unpack first)


parsePrimary :: P Expr
parsePrimary = do
  t <- peek
  case t of
    TkIntLit n -> advance >> intLiteral n
    TkUIntLit n -> advance >> uintLiteral n
    TkDoubleLit d -> advance >> pure (ELit (LDouble d))
    TkStringLit s -> advance >> pure (ELit (LString s))
    TkBytesLit b -> advance >> pure (ELit (LBytes b))
    TkBool b -> advance >> pure (ELit (LBool b))
    TkNull -> advance >> pure (ELit LNull)
    TkLParen -> do
      _ <- advance
      e <- parseExpr
      expectTok TkRParen
      pure e
    TkLBracket -> do
      _ <- advance
      es <- parseExprListTrailing TkRBracket
      expectTok TkRBracket
      pure (EList es)
    TkLBrace -> do
      _ <- advance
      entries <- parseMapInitsTrailing
      expectTok TkRBrace
      pure (EMap entries)
    TkDot -> do
      _ <- advance
      name <- identName
      t2 <- peek
      case t2 of
        TkLParen -> parseGlobalCall ("." <> name)
        _ -> pure (EIdent True name)
    TkIdent n
      | Set.member n reservedWords -> parseError ("reserved word used as name: " ++ T.unpack n)
      | otherwise -> do
          _ <- advance
          t2 <- peek
          case t2 of
            TkLParen -> parseGlobalCall n
            _ -> pure (EIdent False n)
    _ -> parseError ("unexpected token: " ++ show t)


parseGlobalCall :: Text -> P Expr
parseGlobalCall name = do
  args <- parseCallArgs
  case (name, args) of
    ("has", [ESelect _ _]) -> pure ()
    ("has", [_]) -> parseError "has() requires a field selection argument, e.g. has(x.y)"
    _ -> pure ()
  pure (ECall Nothing name args)


parseCallArgs :: P [Expr]
parseCallArgs = do
  expectTok TkLParen
  t <- peek
  case t of
    TkRParen -> advance >> pure []
    _ -> do
      args <- parseExprList
      expectTok TkRParen
      pure args


parseExprList :: P [Expr]
parseExprList = do
  e <- parseExpr
  go [e]
  where
    go acc = do
      t <- peek
      case t of
        TkComma -> do _ <- advance; e <- parseExpr; go (e : acc)
        _ -> pure (reverse acc)


{- | Expression list permitting a single optional trailing comma before the
given closing token (used by list and map literals).
-}
parseExprListTrailing :: Tok -> P [Expr]
parseExprListTrailing closeTok = do
  t <- peek
  if t == closeTok
    then pure []
    else do
      e <- parseExpr
      go [e]
  where
    go acc = do
      t <- peek
      case t of
        TkComma -> do
          _ <- advance
          t2 <- peek
          if t2 == closeTok then pure (reverse acc) else do e <- parseExpr; go (e : acc)
        _ -> pure (reverse acc)


parseMapInitsTrailing :: P [(Expr, Expr)]
parseMapInitsTrailing = do
  t <- peek
  case t of
    TkRBrace -> pure []
    _ -> do
      entry <- parseMapEntry
      go [entry]
  where
    go acc = do
      t <- peek
      case t of
        TkComma -> do
          _ <- advance
          t2 <- peek
          if t2 == TkRBrace then pure (reverse acc) else do e <- parseMapEntry; go (e : acc)
        _ -> pure (reverse acc)
    parseMapEntry = do
      k <- parseExpr
      expectTok TkColon
      v <- parseExpr
      pure (k, v)


parseFieldInits :: P [(Text, Expr)]
parseFieldInits = do
  expectTok TkLBrace
  t <- peek
  case t of
    TkRBrace -> advance >> pure []
    _ -> do
      f <- parseField
      go [f]
  where
    go acc = do
      t <- peek
      case t of
        TkComma -> do
          _ <- advance
          t2 <- peek
          if t2 == TkRBrace
            then advance >> pure (reverse acc)
            else do f <- parseField; go (f : acc)
        TkRBrace -> advance >> pure (reverse acc)
        _ -> parseError ("expected ',' or '}' in struct but found " ++ show t)
    parseField = do
      name <- selectorName
      expectTok TkColon
      v <- parseExpr
      pure (name, v)
