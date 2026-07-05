{- | Recursive-descent parser for the query language, implementing the
normative grammar of spec §4.8 exactly: GraphQL-style ignored tokens
(whitespace, commas, @#@ comments), the reserved-name rule, @\@depth@ as
the entire directive grammar (any other @\@@ is a parse error), and JSON
string literals per RFC 8259 §7.

Numbers follow the grammar's @IntValue@\/@NumberValue@ split lexically,
but classification is by value: a literal whose value is integral (@42@,
@1e2@, @1.0@) is stored as 'QInt'; only non-integral values become 'QNum'.
Explicit @null@ is not a value (§4.8 rule 6) and is rejected here.

The grammar admits any number of query definitions; the 'Document' shape
(exactly one, §4.8 rule 1) is enforced structurally by 'parseDocument',
so a rule-1 violation surfaces as a 'ParseError' rather than a
validation diagnostic.
-}
module Lattice.Query.Parser (
  ParseError (..),
  parseDocument,
  parseFragmentDef,
  parseImportFile,
) where

import Data.Char (chr, ord)
import Data.Scientific qualified as Sci
import Data.Text (Text)
import Data.Text qualified as T
import Lattice.Query.AST
import Lattice.Types


data ParseError = ParseError
  { peOffset :: Int
  -- ^ Code-point offset into the source.
  , peMessage :: Text
  }
  deriving stock (Eq, Show)


parseDocument :: Text -> Either ParseError Document
parseDocument src = runP src $ do
  skipIgnored
  defs <- pDefinitions
  assembleDocument defs


{- | Parse one standalone @fragment Name [(params)] on Type { ... }@
declaration (used by the IDL parser for schema fragment bodies). A pure
surface parse: parameters, selections, and arguments keep source order.
-}
parseFragmentDef :: Text -> Either ParseError FragmentDefQ
parseFragmentDef src = runP src $ do
  skipIgnored
  o <- getOffset
  kw <- pName
  if kw /= "fragment"
    then failAt o "expected a fragment definition"
    else do
      fd <- pFragmentBody
      pEOF
      pure fd


{- | Parse an imported (@.lq@) file: imports and fragment definitions only.
A query definition in an imported document is an error (§4.5: imports
share fragments; queries are never imported).
-}
parseImportFile :: Text -> Either ParseError ([Text], [FragmentDefQ])
parseImportFile src = runP src $ do
  skipIgnored
  defs <- pDefinitions
  let step (imps, frags) = \case
        DImport _ p -> Right (p : imps, frags)
        DFragment f -> Right (imps, f : frags)
        DQuery o _ ->
          Left (ParseError o "imported documents must not contain query definitions")
  case foldM step ([], []) defs of
    Left e -> P (\_ -> Left e)
    Right (imps, frags) -> pure (reverse imps, reverse frags)
  where
    foldM f z = go z
      where
        go acc [] = Right acc
        go acc (x : xs) = f acc x >>= \acc' -> go acc' xs


-- ---------------------------------------------------------------------------
-- Parser monad
-- ---------------------------------------------------------------------------

data S = S
  { sIn :: Text
  , sOff :: Int
  -- ^ Code-point offset from the start of the source.
  }


newtype P a = P {unP :: S -> Either ParseError (a, S)}


instance Functor P where
  fmap f (P g) = P $ \s -> case g s of
    Left e -> Left e
    Right (a, s') -> Right (f a, s')


instance Applicative P where
  pure a = P $ \s -> Right (a, s)
  P mf <*> P ma = P $ \s -> case mf s of
    Left e -> Left e
    Right (f, s') -> case ma s' of
      Left e -> Left e
      Right (a, s'') -> Right (f a, s'')


instance Monad P where
  P ma >>= k = P $ \s -> case ma s of
    Left e -> Left e
    Right (a, s') -> unP (k a) s'


runP :: Text -> P a -> Either ParseError a
runP src p = fst <$> unP p (S src 0)


pErr :: Text -> P a
pErr msg = P $ \s -> Left (ParseError (sOff s) msg)


failAt :: Int -> Text -> P a
failAt off msg = P $ \_ -> Left (ParseError off msg)


getOffset :: P Int
getOffset = P $ \s -> Right (sOff s, s)


peekChar :: P (Maybe Char)
peekChar = P $ \s -> Right (fst <$> T.uncons (sIn s), s)


-- | Advance @n@ code points. The caller has already inspected them.
advance :: Int -> P ()
advance n = P $ \(S t o) -> Right ((), S (T.drop n t) (o + n))


-- ---------------------------------------------------------------------------
-- Lexical layer
-- ---------------------------------------------------------------------------

{- | Skip ignored tokens: spaces, tabs, line terminators, commas, and
@#@-to-end-of-line comments (§4.8).
-}
skipIgnored :: P ()
skipIgnored = P $ \s -> Right ((), go s)
  where
    go st@(S t o) = case T.uncons t of
      Just (c, rest)
        | c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == ',' ->
            go (S rest (o + 1))
        | c == '#' ->
            let body = T.takeWhile (\x -> x /= '\n' && x /= '\r') rest
                n = T.length body
             in go (S (T.drop n rest) (o + 1 + n))
      _ -> st


isNameStart :: Char -> Bool
isNameStart c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'


isNameCont :: Char -> Bool
isNameCont c = isNameStart c || isAsciiDigit c


isAsciiDigit :: Char -> Bool
isAsciiDigit c = c >= '0' && c <= '9'


reservedNames :: [Text]
reservedNames = ["query", "fragment", "import", "on", "true", "false"]


checkReserved :: Int -> Text -> Text -> P ()
checkReserved o what nm
  | nm `elem` reservedNames =
      failAt o ("reserved name '" <> nm <> "' cannot be used as " <> what <> " (§4.8 rule 2)")
  | otherwise = pure ()


-- | A Name token with no leading skip (for @$name@ and @\@depth@ adjacency).
pNameRaw :: P Text
pNameRaw = P $ \(S t o) -> case T.uncons t of
  Just (c, _)
    | isNameStart c ->
        let nm = T.takeWhile isNameCont t
            n = T.length nm
         in Right (nm, S (T.drop n t) (o + n))
  _ -> Left (ParseError o "expected a name")


-- | A Name token, consuming trailing ignored tokens.
pName :: P Text
pName = pNameRaw <* skipIgnored


-- | A single punctuator character, consuming trailing ignored tokens.
punct :: Char -> P ()
punct c = do
  mc <- peekChar
  case mc of
    Just c' | c' == c -> advance 1 >> skipIgnored
    _ -> pErr ("expected '" <> T.singleton c <> "'")


-- | @...@, consuming trailing ignored tokens (@... on@ and @...Name@ both lex).
pEllipsis :: P ()
pEllipsis = do
  s <- P (\st -> Right (st, st))
  if "..." `T.isPrefixOf` sIn s
    then advance 3 >> skipIgnored
    else pErr "expected '...'"


pEOF :: P ()
pEOF = do
  mc <- peekChar
  case mc of
    Nothing -> pure ()
    Just _ -> pErr "unexpected input after the definition"


{- | Number token. Returns the classified value ('QInt' when the value is
integral, 'QNum' otherwise) and whether the spelling was a plain
@IntValue@ (no fraction, no exponent) — @\@depth@ requires the latter.
-}
pNumberTok :: P (QValue, Bool)
pNumberTok = do
  o <- getOffset
  neg <- eat '-'
  intDigits <- pDigits1 "expected a digit"
  if T.length intDigits > 1 && T.head intDigits == '0'
    then failAt o "leading zeros are not permitted in numbers"
    else pure ()
  fracDigits <- pFraction
  expPart <- pExponent
  -- Two adjacent tokens that would lex as one need separating (§4.8):
  -- a number may not be immediately followed by a name or more digits.
  mc <- peekChar
  case mc of
    Just c
      | isNameCont c || c == '.' ->
          pErr "a number must be separated from what follows it"
    _ -> pure ()
  skipIgnored
  let signApply :: Integer -> Integer
      signApply = if neg then negate else id
      intVal = signApply (digitsToInteger intDigits)
  case (fracDigits, expPart) of
    (Nothing, Nothing) -> pure (QInt intVal, True)
    _ -> do
      let fracLen = maybe 0 T.length fracDigits
          coefficient =
            signApply
              ( digitsToInteger intDigits * 10 ^ fracLen
                  + maybe 0 digitsToInteger fracDigits
              )
          e10 = maybe 0 id expPart - fracLen
          sci = Sci.normalize (Sci.scientific coefficient e10)
      case Sci.floatingOrInteger sci :: Either Double Integer of
        Right i -> pure (QInt i, False)
        Left _ -> pure (QNum sci, False)
  where
    eat c = do
      mc <- peekChar
      case mc of
        Just c' | c' == c -> advance 1 >> pure True
        _ -> pure False
    pDigits0 = P $ \(S t o) ->
      let ds = T.takeWhile isAsciiDigit t
          n = T.length ds
       in Right (ds, S (T.drop n t) (o + n))
    pDigits1 msg = do
      ds <- pDigits0
      if T.null ds then pErr msg else pure ds
    pFraction = do
      mc <- peekChar
      case mc of
        Just '.' -> do
          advance 1
          Just <$> pDigits1 "expected a digit after '.'"
        _ -> pure Nothing
    pExponent = do
      mc <- peekChar
      case mc of
        Just c | c == 'e' || c == 'E' -> do
          advance 1
          negE <- do
            mc2 <- peekChar
            case mc2 of
              Just '-' -> advance 1 >> pure True
              Just '+' -> advance 1 >> pure False
              _ -> pure False
          ds <- pDigits1 "expected a digit in the exponent"
          if T.length ds > 5
            then pErr "numeric exponent out of range"
            else pure (Just (fromInteger ((if negE then negate else id) (digitsToInteger ds)) :: Int))
        _ -> pure Nothing
    digitsToInteger = T.foldl' (\acc c -> acc * 10 + toInteger (ord c - ord '0')) 0


-- | A JSON string per RFC 8259 §7, consuming trailing ignored tokens.
pStringTok :: P Text
pStringTok = do
  mc <- peekChar
  case mc of
    Just '"' -> advance 1 >> go []
    _ -> pErr "expected a string"
  where
    go acc = do
      mc <- peekChar
      case mc of
        Nothing -> pErr "unterminated string"
        Just '"' -> do
          advance 1
          skipIgnored
          pure (T.pack (reverse acc))
        Just '\\' -> advance 1 >> escape acc
        Just c
          | c < '\x20' -> pErr "unescaped control character in string"
          | otherwise -> advance 1 >> go (c : acc)
    escape acc = do
      mc <- peekChar
      case mc of
        Nothing -> pErr "unterminated string escape"
        Just c -> case c of
          '"' -> advance 1 >> go ('"' : acc)
          '\\' -> advance 1 >> go ('\\' : acc)
          '/' -> advance 1 >> go ('/' : acc)
          'b' -> advance 1 >> go ('\b' : acc)
          'f' -> advance 1 >> go ('\f' : acc)
          'n' -> advance 1 >> go ('\n' : acc)
          'r' -> advance 1 >> go ('\r' : acc)
          't' -> advance 1 >> go ('\t' : acc)
          'u' -> advance 1 >> unicode acc
          _ -> pErr ("invalid string escape '\\" <> T.singleton c <> "'")
    unicode acc = do
      hi <- pHex4
      if hi >= 0xD800 && hi <= 0xDBFF
        then do
          -- A high surrogate must pair with an immediately following \uXXXX
          -- low surrogate (RFC 8259 §7).
          o <- getOffset
          s <- P (\st -> Right (st, st))
          if "\\u" `T.isPrefixOf` sIn s
            then do
              advance 2
              lo <- pHex4
              if lo >= 0xDC00 && lo <= 0xDFFF
                then
                  go
                    ( chr (0x10000 + (hi - 0xD800) * 0x400 + (lo - 0xDC00))
                        : acc
                    )
                else failAt o "expected a low surrogate after a high surrogate"
            else failAt o "lone surrogate in string escape"
        else
          if hi >= 0xDC00 && hi <= 0xDFFF
            then pErr "lone low surrogate in string escape"
            else go (chr hi : acc)
    pHex4 = do
      d1 <- pHexDigit
      d2 <- pHexDigit
      d3 <- pHexDigit
      d4 <- pHexDigit
      pure (((d1 * 16 + d2) * 16 + d3) * 16 + d4)
    pHexDigit = do
      mc <- peekChar
      case mc of
        Just c
          | isAsciiDigit c -> advance 1 >> pure (ord c - ord '0')
          | c >= 'a' && c <= 'f' -> advance 1 >> pure (ord c - ord 'a' + 10)
          | c >= 'A' && c <= 'F' -> advance 1 >> pure (ord c - ord 'A' + 10)
        _ -> pErr "expected a hex digit in \\u escape"


-- ---------------------------------------------------------------------------
-- Grammar productions
-- ---------------------------------------------------------------------------

data Def
  = DImport Int Text
  | DFragment FragmentDefQ
  | -- | Carries the offset of its @query@ keyword for the rule-1 diagnostic.
    DQuery Int QueryDef


pDefinitions :: P [Def]
pDefinitions = go []
  where
    go acc = do
      mc <- peekChar
      case mc of
        Nothing -> pure (reverse acc)
        Just _ -> do
          d <- pDefinition
          go (d : acc)


pDefinition :: P Def
pDefinition = do
  o <- getOffset
  kw <- pName
  case kw of
    "import" -> DImport o <$> pStringTok
    "query" -> DQuery o <$> pQueryBody
    "fragment" -> DFragment <$> pFragmentBody
    _ -> failAt o ("expected 'query', 'fragment', or 'import', found '" <> kw <> "'")


assembleDocument :: [Def] -> P Document
assembleDocument defs = do
  let imports = foldr (\d acc -> case d of DImport _ p -> p : acc; _ -> acc) [] defs
      frags = foldr (\d acc -> case d of DFragment f -> f : acc; _ -> acc) [] defs
      queries = foldr (\d acc -> case d of DQuery o q -> (o, q) : acc; _ -> acc) [] defs
  case queries of
    [(_, q)] ->
      pure Document {docImports = imports, docFragments = frags, docQuery = q}
    [] -> pErr "a document must contain exactly one query definition (§4.8 rule 1)"
    (_ : (o, _) : _) ->
      failAt o "a document may contain only one query definition (§4.8 rule 1)"


pQueryBody :: P QueryDef
pQueryBody = do
  mc <- peekChar
  nm <- case mc of
    Just c | isNameStart c -> Just <$> pName
    _ -> pure Nothing
  vars <- pOptionalVarDefs
  sel <- pSelectionSet
  pure QueryDef {qName = nm, qVars = vars, qSelection = sel}


pFragmentBody :: P FragmentDefQ
pFragmentBody = do
  o <- getOffset
  nm <- pName
  checkReserved o "a fragment name" nm
  params <- pOptionalVarDefs
  o2 <- getOffset
  kw <- pName
  if kw /= "on"
    then failAt o2 "expected 'on' in fragment definition"
    else do
      tn <- pName
      sel <- pSelectionSet
      pure
        FragmentDefQ
          { fdName = FragmentName nm
          , fdParams = params
          , fdOn = tn
          , fdSelection = sel
          }


pOptionalVarDefs :: P [VarDef]
pOptionalVarDefs = do
  mc <- peekChar
  case mc of
    Just '(' -> do
      punct '('
      go []
    _ -> pure []
  where
    go acc = do
      mc <- peekChar
      case mc of
        Just ')'
          | null acc -> pErr "expected at least one variable definition"
          | otherwise -> punct ')' >> pure (reverse acc)
        Just '$' -> do
          v <- pVarDef
          go (v : acc)
        _ -> pErr "expected a variable definition or ')'"


pVarDef :: P VarDef
pVarDef = do
  v <- pVariableName
  punct ':'
  ty <- pTypeRef
  mc <- peekChar
  def <- case mc of
    Just '=' -> punct '=' >> (Just <$> pValue)
    _ -> pure Nothing
  pure VarDef {vdName = v, vdType = ty, vdDefault = def}


pTypeRef :: P TypeRefQ
pTypeRef = do
  nm <- pName
  mc <- peekChar
  opt <- case mc of
    Just '?' -> punct '?' >> pure True
    _ -> pure False
  pure TypeRefQ {trName = nm, trOptional = opt}


-- | @$name@ — the @$@ and the name are one token (no space); reserved-checked.
pVariableName :: P VarName
pVariableName = do
  mc <- peekChar
  case mc of
    Just '$' -> do
      advance 1
      o <- getOffset
      nm <- pNameRaw
      skipIgnored
      checkReserved o "a variable name" nm
      pure (VarName nm)
    _ -> pErr "expected a variable ($name)"


pSelectionSet :: P SelectionSet
pSelectionSet = do
  punct '{'
  go []
  where
    go acc = do
      mc <- peekChar
      case mc of
        Nothing -> pErr "unterminated selection set"
        Just '}'
          | null acc -> pErr "a selection set requires at least one selection"
          | otherwise -> punct '}' >> pure (reverse acc)
        Just _ -> do
          s <- pSelection
          go (s : acc)


pSelection :: P Selection
pSelection = do
  mc <- peekChar
  case mc of
    Just '.' -> pSpreadOrInline
    Just c | isNameStart c -> SField <$> pField
    _ -> pErr "expected a field, fragment spread, or inline fragment"


pSpreadOrInline :: P Selection
pSpreadOrInline = do
  pEllipsis
  o <- getOffset
  nm <- pName
  if nm == "on"
    then do
      tn <- pName
      sel <- pSelectionSet
      pure (SInline (TypeName tn) sel)
    else do
      checkReserved o "a fragment name" nm
      args <- pOptionalArguments
      pure (SSpread (FragmentName nm) args)


pField :: P Field
pField = do
  nm <- pName
  args <- pOptionalArguments
  dep <- pOptionalDepth
  mc <- peekChar
  sel <- case mc of
    Just '{' -> Just <$> pSelectionSet
    _ -> pure Nothing
  pure Field {fName = FieldName nm, fArgs = args, fDepth = dep, fSelection = sel}


{- | @\@depth(n)@. @\@depth@ is a single token (§4.8: quoted terminal), so
the name must be adjacent to the @\@@; any other directive name is a
parse error, not a validation error (rule 4).
-}
pOptionalDepth :: P (Maybe Int)
pOptionalDepth = do
  mc <- peekChar
  case mc of
    Just '@' -> do
      o <- getOffset
      advance 1
      nm <- pNameRaw
      if nm /= "depth"
        then
          failAt o ("unknown directive '@" <> nm <> "'; @depth is the entire directive grammar (§4.8 rule 4)")
        else do
          skipIgnored
          punct '('
          n <- pIntValue
          punct ')'
          pure (Just n)
    _ -> pure Nothing


pIntValue :: P Int
pIntValue = do
  o <- getOffset
  (v, plain) <- pNumberTok
  case v of
    QInt i
      | plain
      , i >= toInteger (minBound :: Int)
      , i <= toInteger (maxBound :: Int) ->
          pure (fromInteger i)
    _ -> failAt o "expected an integer"


pOptionalArguments :: P [Argument]
pOptionalArguments = do
  mc <- peekChar
  case mc of
    Just '(' -> do
      punct '('
      go []
    _ -> pure []
  where
    go acc = do
      mc <- peekChar
      case mc of
        Just ')'
          | null acc -> pErr "expected at least one argument"
          | otherwise -> punct ')' >> pure (reverse acc)
        Just c | isNameStart c -> do
          a <- pArgument
          go (a : acc)
        _ -> pErr "expected an argument or ')'"


pArgument :: P Argument
pArgument = do
  nm <- pName
  punct ':'
  v <- pValue
  pure Argument {argName = ArgName nm, argValue = v}


pValue :: P QValue
pValue = do
  mc <- peekChar
  case mc of
    Just '$' -> QVar <$> pVariableName
    Just '"' -> QString <$> pStringTok
    Just '[' -> pListValue
    Just c
      | c == '-' || isAsciiDigit c -> fst <$> pNumberTok
      | isNameStart c -> do
          o <- getOffset
          nm <- pName
          case nm of
            "true" -> pure (QBool True)
            "false" -> pure (QBool False)
            "null" ->
              failAt o "explicit null does not exist; omit the argument instead (§4.8 rule 6)"
            _
              | nm `elem` reservedNames ->
                  failAt o ("reserved name '" <> nm <> "' cannot be used as an enum value (§4.8 rule 2)")
              | otherwise -> pure (QEnum nm)
    _ -> pErr "expected a value"


pListValue :: P QValue
pListValue = do
  punct '['
  go []
  where
    go acc = do
      mc <- peekChar
      case mc of
        Just ']' -> punct ']' >> pure (QList (reverse acc))
        Nothing -> pErr "unterminated list value"
        Just _ -> do
          v <- pValue
          go (v : acc)
