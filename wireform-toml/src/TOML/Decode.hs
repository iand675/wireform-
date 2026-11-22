{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

{- | TOML 1.0 / 1.1 text decoder.

Implements a single-pass character cursor parser over 'Text'.
Supports every TOML 1.0 construct (and the parts of TOML 1.1 that
don't change the data model):

* Bare keys, quoted keys, and dotted keys.
* Integers in decimal / @0x@ hex / @0o@ octal / @0b@ binary, with
  underscore separators and an optional sign.
* Floats including @inf@ / @nan@ (with optional sign), exponents,
  and underscore separators.
* Booleans.
* Local-date, local-time, local-datetime, and offset-datetime
  per RFC 3339 (with an optional space instead of @T@).
* Basic, literal, multi-line basic and multi-line literal strings,
  with all TOML escape sequences (@\\xNN@ is /not/ a TOML escape;
  @\\u@ / @\\U@ /are/).
* Arrays and inline tables, including the multi-line forms allowed
  in the relevant spec versions.
* @[a.b.c]@ tables and @[[a]]@ array-of-tables headers.
* Duplicate-key detection: redefining a bare key, redefining a
  table you've already opened, defining a sub-table of an inline
  table, etc., are all rejected.
-}
module TOML.Decode (
  decode,
  decodeBS,
) where

import Control.Monad qualified
import Data.ByteString (ByteString)
import Data.Char (
  chr,
  digitToInt,
  isAsciiLower,
  isAsciiUpper,
  isDigit,
  isHexDigit,
  isOctDigit,
  ord,
 )
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import TOML.Value qualified as TV


-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

decode :: Text -> Either String TV.Value
decode src =
  case runP parseDocument (initialState src) of
    Left err -> Left err
    Right (doc, _) -> assembleDocumentE doc


decodeBS :: ByteString -> Either String TV.Value
decodeBS bs = case TE.decodeUtf8' bs of
  Left e -> Left ("TOML: invalid UTF-8: " ++ show e)
  Right t -> decode t


-- ---------------------------------------------------------------------------
-- Parser monad
-- ---------------------------------------------------------------------------

{- | Parser state. Crucial perf invariant: 'psSrc' is the /remaining/
text suffix, not the full original source. We advance via
'T.uncons' (amortised O(1)) instead of indexing into a fixed
buffer (where 'T.index' is O(pos)). 'psPos' is kept around purely
so that 'mark' / 'sliceFrom' can compute consumed substring
lengths without re-walking the text.
-}
data PState = PState
  { psSrc :: !Text
  -- ^ remaining text suffix from current position
  , psPos :: !Int
  -- ^ chars consumed so far (offset into the original)
  , psLine :: !Int
  -- ^ 1-based current line, for error messages
  , psCol :: !Int
  -- ^ 1-based current column, for error messages
  }


initialState :: Text -> PState
initialState t = PState t 0 1 1


newtype P a = P {runP :: PState -> Either String (a, PState)}


instance Functor P where
  fmap f (P g) = P $ \s -> case g s of
    Left e -> Left e
    Right (x, s') -> Right (f x, s')


instance Applicative P where
  pure x = P $ \s -> Right (x, s)
  P pf <*> P px = P $ \s -> case pf s of
    Left e -> Left e
    Right (f, s') -> case px s' of
      Left e -> Left e
      Right (x, s'') -> Right (f x, s'')


instance Monad P where
  P g >>= k = P $ \s -> case g s of
    Left e -> Left e
    Right (x, s') -> runP (k x) s'


failP :: String -> P a
failP msg = P $ \s ->
  Left
    ( "TOML: "
        ++ msg
        ++ " (line "
        ++ show (psLine s)
        ++ ", col "
        ++ show (psCol s)
        ++ ")"
    )


-- | Look at the next character without consuming it.
peek :: P (Maybe Char)
peek = P $ \s -> case T.uncons (psSrc s) of
  Nothing -> Right (Nothing, s)
  Just (c, _) -> Right (Just c, s)


{- | Look at the character @n@ positions ahead (0 = current). Used
with small @n@ (≤4) for lookahead in a few token classifiers.
-}
peekN :: Int -> P (Maybe Char)
peekN n0 = P $ \s -> Right (go n0 (psSrc s), s)
  where
    go !_ t | T.null t = Nothing
    go !n t = case T.uncons t of
      Nothing -> Nothing
      Just (c, rest)
        | n == 0 -> Just c
        | otherwise -> go (n - 1) rest


-- | Consume one character. Fails if at EOF.
advance :: P Char
advance = P $ \s -> case T.uncons (psSrc s) of
  Nothing ->
    Left
      ( "TOML: unexpected EOF (line "
          ++ show (psLine s)
          ++ ", col "
          ++ show (psCol s)
          ++ ")"
      )
  Just (c, rest) ->
    let (l', col') = case c of
          '\n' -> (psLine s + 1, 1)
          _ -> (psLine s, psCol s + 1)
    in Right (c, PState rest (psPos s + 1) l' col')


-- | Consume one character that must be @c@.
expect :: Char -> P ()
expect c = do
  mc <- peek
  case mc of
    Just x | x == c -> Control.Monad.void advance
    Just x -> failP $ "expected " ++ show c ++ " but got " ++ show x
    Nothing -> failP $ "expected " ++ show c ++ " but got EOF"


{- | Consume the literal text @t@; fail if absent. We pre-compute
'T.length' on the literal (small + constant) and walk the
suffix once via 'T.splitAt'.
-}
expectText :: Text -> P ()
expectText t = P $ \s ->
  let n = T.length t
      (chunk, rest) = T.splitAt n (psSrc s)
  in if chunk /= t
       then Left ("TOML: expected literal " ++ T.unpack t)
       else
         let (l', col') = bumpLineCol (psLine s) (psCol s) chunk
         in Right ((), PState rest (psPos s + n) l' col')


{- | A 'mark' captures the source suffix and offset at the current
position so we can later cheaply slice out the consumed substring
via 'sliceFrom' without re-walking the original 'Text'.
-}
data Mark = Mark !Text !Int


mark :: P Mark
mark = P $ \s -> Right (Mark (psSrc s) (psPos s), s)


{- | Extract the substring consumed since 'mark'. O(k) where @k@ is
the consumed length, regardless of how far we've advanced from
the beginning of input.
-}
sliceFrom :: Mark -> P Text
sliceFrom (Mark srcAtStart startPos) = P $ \s ->
  Right (T.take (psPos s - startPos) srcAtStart, s)


-- | The current source suffix (i.e. unparsed remainder).
getSrc :: P Text
getSrc = P $ \s -> Right (psSrc s, s)


{- | Like 'advance' but skips @n@ characters at once. We walk one
char at a time to keep the line/col counters accurate.
-}
advanceN :: Int -> P ()
advanceN n
  | n <= 0 = pure ()
  | otherwise = advance >> advanceN (n - 1)


-- | Bump (line, col) by walking a known-short prefix.
bumpLineCol :: Int -> Int -> Text -> (Int, Int)
bumpLineCol = go
  where
    go !l !c t = case T.uncons t of
      Nothing -> (l, c)
      Just ('\n', rest) -> go (l + 1) 1 rest
      Just (_, rest) -> go l (c + 1) rest


-- ---------------------------------------------------------------------------
-- Document structure
-- ---------------------------------------------------------------------------

{- | Intermediate parse output. We collect a sequence of @TopAction@s
and then assemble the final 'TV.Value' in 'assembleDocument' so
that we can do duplicate-key + table-redefinition checks across
the whole document.
-}
data TopAction
  = APair !KeyPath !TV.Value
  | ATable !KeyPath
  | AArrayOf !KeyPath
  deriving (Eq, Show)


{- | A dotted key path: every segment is a single (already-decoded)
key, so @"a.b.c"@ parses to @["a","b","c"]@ and @"a.\"b.c\""@ to
@["a","b.c"]@.
-}
type KeyPath = [Text]


newtype Doc = Doc [TopAction]


-- ---------------------------------------------------------------------------
-- Top-level parser
-- ---------------------------------------------------------------------------

parseDocument :: P Doc
parseDocument = do
  acts <- loop []
  pure (Doc (reverse acts))
  where
    loop acc = do
      skipWhitespaceAndNewlines
      mc <- peek
      case mc of
        Nothing -> pure acc
        Just '[' -> do
          isArr <- isArrayHeader
          if isArr
            then do
              path <- parseArrayHeader
              skipLineEnd
              loop (AArrayOf path : acc)
            else do
              path <- parseTableHeader
              skipLineEnd
              loop (ATable path : acc)
        Just _ -> do
          (kp, v) <- parseKeyValue
          skipLineEnd
          loop (APair kp v : acc)


{- | A line-end is any of: comment + newline, plain newline, or EOF.
We tolerate trailing whitespace.
-}
skipLineEnd :: P ()
skipLineEnd = do
  skipInlineWs
  mc <- peek
  case mc of
    Nothing -> pure ()
    Just '#' -> skipComment >> skipLineEnd
    Just '\n' -> Control.Monad.void advance
    Just '\r' -> do
      _ <- advance
      mc2 <- peek
      case mc2 of
        Just '\n' -> Control.Monad.void advance
        _ -> pure ()
    Just c -> failP $ "unexpected " ++ show c ++ " at end of line"


{- | Skip whitespace and blank / comment lines until the next
significant character.
-}
skipWhitespaceAndNewlines :: P ()
skipWhitespaceAndNewlines = do
  mc <- peek
  case mc of
    Just ' ' -> advance >> skipWhitespaceAndNewlines
    Just '\t' -> advance >> skipWhitespaceAndNewlines
    Just '\n' -> advance >> skipWhitespaceAndNewlines
    Just '\r' -> do
      mn <- peekN 1
      case mn of
        Just '\n' -> advance >> advance >> skipWhitespaceAndNewlines
        _ -> failP "bare carriage return outside CRLF"
    Just '#' -> skipComment >> skipWhitespaceAndNewlines
    _ -> pure ()


skipInlineWs :: P ()
skipInlineWs = do
  mc <- peek
  case mc of
    Just ' ' -> advance >> skipInlineWs
    Just '\t' -> advance >> skipInlineWs
    _ -> pure ()


skipComment :: P ()
skipComment = do
  expect '#'
  loop
  where
    loop = do
      mc <- peek
      case mc of
        Nothing -> pure ()
        Just '\n' -> pure ()
        Just '\r' -> do
          -- A bare CR (not followed by LF) is forbidden inside a
          -- comment per TOML 1.0 §5; CRLF is the only legal way to
          -- end the comment line.
          mn <- peekN 1
          case mn of
            Just '\n' -> pure ()
            _ -> failP "bare carriage return in comment"
        Just c
          | isControlForbidden c ->
              failP $
                "control character "
                  ++ show c
                  ++ " not allowed in comment"
          | otherwise -> advance >> loop


-- | Forbidden control chars in TOML comments / strings.
isControlForbidden :: Char -> Bool
isControlForbidden c =
  let o = ord c
  in (o < 0x20 && c /= '\t') || o == 0x7F


-- ---------------------------------------------------------------------------
-- Headers
-- ---------------------------------------------------------------------------

isArrayHeader :: P Bool
isArrayHeader = do
  mc1 <- peek
  mc2 <- peekN 1
  pure (mc1 == Just '[' && mc2 == Just '[')


parseTableHeader :: P KeyPath
parseTableHeader = do
  expect '['
  skipInlineWs
  kp <- parseKeyPath
  skipInlineWs
  expect ']'
  pure kp


parseArrayHeader :: P KeyPath
parseArrayHeader = do
  expect '['
  expect '['
  skipInlineWs
  kp <- parseKeyPath
  skipInlineWs
  expect ']'
  expect ']'
  pure kp


-- ---------------------------------------------------------------------------
-- Key paths
-- ---------------------------------------------------------------------------

parseKeyPath :: P KeyPath
parseKeyPath = do
  k <- parseKeyOne
  loop [k]
  where
    loop acc = do
      skipInlineWs
      mc <- peek
      case mc of
        Just '.' -> do
          _ <- advance
          skipInlineWs
          k <- parseKeyOne
          loop (k : acc)
        _ -> pure (reverse acc)


parseKeyOne :: P Text
parseKeyOne = do
  mc <- peek
  case mc of
    Just '"' -> parseBasicStringKey
    Just '\'' -> parseLiteralStringKey
    Just c | isBareKeyChar c -> parseBareKey
    _ -> failP "expected key"


isBareKeyChar :: Char -> Bool
isBareKeyChar c =
  isAsciiUpper c
    || isAsciiLower c
    || isDigit c
    || c == '_'
    || c == '-'


parseBareKey :: P Text
parseBareKey = do
  m <- mark
  loop
  !key <- sliceFrom m
  if T.null key
    then failP "empty bare key"
    else pure key
  where
    loop = do
      mc <- peek
      case mc of
        Just c | isBareKeyChar c -> advance >> loop
        _ -> pure ()


parseBasicStringKey :: P Text
parseBasicStringKey = do
  expect '"'
  parseBasicStringInner


parseLiteralStringKey :: P Text
parseLiteralStringKey = do
  expect '\''
  parseLiteralStringInner


-- ---------------------------------------------------------------------------
-- Key-value pair
-- ---------------------------------------------------------------------------

parseKeyValue :: P (KeyPath, TV.Value)
parseKeyValue = do
  kp <- parseKeyPath
  skipInlineWs
  expect '='
  skipInlineWs
  v <- parseValue
  pure (kp, v)


-- ---------------------------------------------------------------------------
-- Values
-- ---------------------------------------------------------------------------

parseValue :: P TV.Value
parseValue = do
  mc <- peek
  case mc of
    Nothing -> failP "expected value, got EOF"
    Just c -> case c of
      '"' -> parseStringValue
      '\'' -> parseLiteralStringValue
      '[' -> parseArrayValue
      '{' -> parseInlineTableValue
      't' -> parseLiteralKeyword "true" (TV.TBool True)
      'f' -> parseLiteralKeyword "false" (TV.TBool False)
      _ -> parseScalar


parseLiteralKeyword :: Text -> TV.Value -> P TV.Value
parseLiteralKeyword kw v = do
  expectText kw
  -- Make sure the next char doesn't extend the token.
  mc <- peek
  case mc of
    Just c
      | isBareKeyChar c || c == '.' ->
          failP $ "unexpected continuation after " ++ T.unpack kw
    _ -> pure v


-- ---------------------------------------------------------------------------
-- String values
-- ---------------------------------------------------------------------------

parseStringValue :: P TV.Value
parseStringValue = do
  -- Could be triple basic.
  mc1 <- peekN 1
  mc2 <- peekN 2
  if mc1 == Just '"' && mc2 == Just '"'
    then do
      advanceN 3
      TV.TString <$> parseMultilineBasicStringInner
    else do
      expect '"'
      TV.TString <$> parseBasicStringInner


parseLiteralStringValue :: P TV.Value
parseLiteralStringValue = do
  mc1 <- peekN 1
  mc2 <- peekN 2
  if mc1 == Just '\'' && mc2 == Just '\''
    then do
      advanceN 3
      TV.TString <$> parseMultilineLiteralStringInner
    else do
      expect '\''
      TV.TString <$> parseLiteralStringInner


{- | Inside a basic single-line @"..."@ string. The opening @"@ is
assumed already consumed.
-}
parseBasicStringInner :: P Text
parseBasicStringInner = T.pack . reverse <$> loop []
  where
    loop acc = do
      mc <- peek
      case mc of
        Nothing -> failP "unterminated basic string"
        Just '"' -> do _ <- advance; pure acc
        Just '\n' -> failP "newline in basic string"
        Just '\r' -> failP "carriage return in basic string"
        Just '\\' -> do
          _ <- advance
          c <- decodeBasicEscape
          loop (c : acc)
        Just c
          | isControlForbidden c ->
              failP $ "control character " ++ show (ord c) ++ " in basic string"
          | otherwise -> do _ <- advance; loop (c : acc)


{- | Inside a literal single-line @'...'@ string. The opening @'@ is
assumed already consumed.
-}
parseLiteralStringInner :: P Text
parseLiteralStringInner = T.pack . reverse <$> loop []
  where
    loop acc = do
      mc <- peek
      case mc of
        Nothing -> failP "unterminated literal string"
        Just '\'' -> do _ <- advance; pure acc
        Just '\n' -> failP "newline in literal string"
        Just '\r' -> failP "carriage return in literal string"
        Just c
          | isControlForbidden c && c /= '\t' ->
              failP $
                "control character "
                  ++ show (ord c)
                  ++ " in literal string"
          | otherwise -> do _ <- advance; loop (c : acc)


-- | Inside a @"""...."""@ string. The opening triple is consumed.
parseMultilineBasicStringInner :: P Text
parseMultilineBasicStringInner = do
  -- A leading newline directly after the opening delimiter is
  -- discarded.
  mc <- peek
  _ <- case mc of
    Just '\n' -> Control.Monad.void advance
    Just '\r' -> do
      _ <- advance
      mc2 <- peek
      case mc2 of
        Just '\n' -> Control.Monad.void advance
        _ -> pure ()
    _ -> pure ()
  T.pack . reverse <$> loop []
  where
    loop acc = do
      mc <- peek
      case mc of
        Nothing -> failP "unterminated multi-line basic string"
        Just '"' -> do
          mc1 <- peekN 1
          mc2 <- peekN 2
          mc3 <- peekN 3
          mc4 <- peekN 4
          case (mc1, mc2, mc3, mc4) of
            -- 5 quotes in a row: """"" — close + 2 literal "
            (Just '"', Just '"', Just '"', Just '"') -> do
              advanceN 5
              pure ('"' : '"' : acc)
            -- 4 quotes: """" — close + 1 literal "
            (Just '"', Just '"', Just '"', _) -> do
              advanceN 4
              pure ('"' : acc)
            (Just '"', Just '"', _, _) -> do
              advanceN 3
              pure acc
            _ -> do
              _ <- advance
              loop ('"' : acc)
        Just '\\' -> do
          isLEB <- isLineEndBackslash
          if isLEB
            then do
              _ <- advance -- consume the backslash
              skipTrailingWsThenNewline
              trimWS
              loop acc
            else do
              _ <- advance
              c <- decodeBasicEscape
              loop (c : acc)
        Just '\r' -> do
          mn <- peekN 1
          case mn of
            Just '\n' -> do _ <- advance; _ <- advance; loop ('\n' : acc)
            _ -> failP "bare carriage return in multi-line basic string"
        Just c
          | isControlForbidden c && c /= '\n' && c /= '\t' ->
              failP $
                "control character "
                  ++ show (ord c)
                  ++ " in multi-line basic string"
          | otherwise -> do _ <- advance; loop (c : acc)

    -- After "\<newline>", consume any whitespace/newlines.
    trimWS = do
      mc <- peek
      case mc of
        Just ' ' -> advance >> trimWS
        Just '\t' -> advance >> trimWS
        Just '\n' -> advance >> trimWS
        Just '\r' -> advance >> trimWS
        _ -> pure ()

    -- Skip trailing inline whitespace then exactly one newline.
    skipTrailingWsThenNewline = do
      mc <- peek
      case mc of
        Just ' ' -> advance >> skipTrailingWsThenNewline
        Just '\t' -> advance >> skipTrailingWsThenNewline
        Just '\n' -> Control.Monad.void advance
        Just '\r' -> do
          _ <- advance
          mc2 <- peek
          case mc2 of
            Just '\n' -> Control.Monad.void advance
            _ -> pure ()
        _ -> pure ()


-- | Inside a @'''…'''@ string. The opening triple is consumed.
parseMultilineLiteralStringInner :: P Text
parseMultilineLiteralStringInner = do
  mc <- peek
  _ <- case mc of
    Just '\n' -> Control.Monad.void advance
    Just '\r' -> do
      _ <- advance
      mc2 <- peek
      case mc2 of
        Just '\n' -> Control.Monad.void advance
        _ -> pure ()
    _ -> pure ()
  T.pack . reverse <$> loop []
  where
    loop acc = do
      mc <- peek
      case mc of
        Nothing -> failP "unterminated multi-line literal string"
        Just '\'' -> do
          mc1 <- peekN 1
          mc2 <- peekN 2
          mc3 <- peekN 3
          mc4 <- peekN 4
          case (mc1, mc2, mc3, mc4) of
            (Just '\'', Just '\'', Just '\'', Just '\'') -> do
              advanceN 5
              pure ('\'' : '\'' : acc)
            (Just '\'', Just '\'', Just '\'', _) -> do
              advanceN 4
              pure ('\'' : acc)
            (Just '\'', Just '\'', _, _) -> do
              advanceN 3
              pure acc
            _ -> do
              _ <- advance
              loop ('\'' : acc)
        Just '\r' -> do
          mn <- peekN 1
          case mn of
            Just '\n' -> do _ <- advance; _ <- advance; loop ('\n' : acc)
            _ -> failP "bare carriage return in multi-line literal string"
        Just c
          | isControlForbidden c && c /= '\n' && c /= '\t' ->
              failP $
                "control character "
                  ++ show (ord c)
                  ++ " in multi-line literal string"
          | otherwise -> do _ <- advance; loop (c : acc)


{- | Decode the character /after/ a @\\@ in a basic / multi-line
basic string. The backslash itself must already be consumed.
-}
decodeBasicEscape :: P Char
decodeBasicEscape = do
  c <- advance
  case c of
    'b' -> pure '\b'
    't' -> pure '\t'
    'n' -> pure '\n'
    'f' -> pure '\f'
    'r' -> pure '\r'
    '"' -> pure '"'
    '\\' -> pure '\\'
    'e' -> pure '\x1B' -- TOML 1.1
    'x' -> readHexChar 2 -- TOML 1.1
    'u' -> readHexChar 4
    'U' -> readHexChar 8
    _ -> failP $ "unknown escape \\" ++ [c]


readHexChar :: Int -> P Char
readHexChar n = do
  cs <-
    replicateP
      n
      ( do
          mc <- peek
          case mc of
            Just x | isHexDigit x -> advance
            _ -> failP "invalid hex escape"
      )
  let !v = foldl (\acc x -> acc * 16 + digitToInt x) 0 cs
  if v <= 0x10FFFF && not (v >= 0xD800 && v <= 0xDFFF)
    then pure (chr v)
    else failP $ "hex escape out of unicode range: " ++ show v


replicateP :: Int -> P a -> P [a]
replicateP 0 _ = pure []
replicateP n p = do
  x <- p
  xs <- replicateP (n - 1) p
  pure (x : xs)


{- | Lookahead: is the current position a backslash followed only by
inline whitespace and then a newline? Used to detect the
multi-line-basic line-ending-backslash form (which trims following
whitespace).
-}
isLineEndBackslash :: P Bool
isLineEndBackslash = do
  src <- getSrc
  pure $ case T.uncons src of
    Just ('\\', rest) -> scan rest
    _ -> False
  where
    scan t = case T.uncons t of
      Just (' ', rest) -> scan rest
      Just ('\t', rest) -> scan rest
      Just ('\n', _) -> True
      Just ('\r', _) -> True
      _ -> False


-- ---------------------------------------------------------------------------
-- Arrays
-- ---------------------------------------------------------------------------

parseArrayValue :: P TV.Value
parseArrayValue = do
  expect '['
  TV.TArray . V.fromList <$> goItems []
  where
    goItems acc = do
      skipArrayWs
      mc <- peek
      case mc of
        Just ']' -> do _ <- advance; pure (reverse acc)
        Nothing -> failP "unterminated array"
        _ -> do
          v <- parseValue
          skipArrayWs
          mc2 <- peek
          case mc2 of
            Just ',' -> do
              _ <- advance
              skipArrayWs
              -- Allow trailing comma.
              mc3 <- peek
              case mc3 of
                Just ']' -> do _ <- advance; pure (reverse (v : acc))
                _ -> goItems (v : acc)
            Just ']' -> do _ <- advance; pure (reverse (v : acc))
            _ -> failP "expected ',' or ']' in array"


skipArrayWs :: P ()
skipArrayWs = do
  mc <- peek
  case mc of
    Just ' ' -> advance >> skipArrayWs
    Just '\t' -> advance >> skipArrayWs
    Just '\n' -> advance >> skipArrayWs
    Just '\r' -> advance >> skipArrayWs
    Just '#' -> skipComment >> skipArrayWs
    _ -> pure ()


-- ---------------------------------------------------------------------------
-- Inline tables
-- ---------------------------------------------------------------------------

parseInlineTableValue :: P TV.Value
parseInlineTableValue = do
  expect '{'
  skipInlineTableWs
  mc <- peek
  case mc of
    Just '}' -> do _ <- advance; pure (TV.TTable V.empty)
    _ -> do
      pairs <- goItems []
      case foldlM applyInline (V.empty, Set.empty) pairs of
        Left e -> failP e
        Right (v, _) -> pure (TV.TTable v)
  where
    goItems acc = do
      skipInlineTableWs
      (kp, v) <- parseKeyValue
      skipInlineTableWs
      mc <- peek
      case mc of
        Just ',' -> do
          _ <- advance
          skipInlineTableWs
          -- Trailing comma allowed (TOML 1.1).
          mc2 <- peek
          case mc2 of
            Just '}' -> do
              _ <- advance
              pure (reverse ((kp, v) : acc))
            _ -> goItems ((kp, v) : acc)
        Just '}' -> do _ <- advance; pure (reverse ((kp, v) : acc))
        _ -> failP "expected ',' or '}' in inline table"


{- | Inline tables in TOML 1.1 may span multiple lines and contain
comments. We follow the lenient form by default so 1.1 documents
parse cleanly; 1.0 documents that don't use the new form are
unaffected.
-}
skipInlineTableWs :: P ()
skipInlineTableWs = do
  mc <- peek
  case mc of
    Just ' ' -> advance >> skipInlineTableWs
    Just '\t' -> advance >> skipInlineTableWs
    Just '\n' -> advance >> skipInlineTableWs
    Just '\r' -> advance >> skipInlineTableWs
    Just '#' -> skipComment >> skipInlineTableWs
    _ -> pure ()


-- A pure left-fold over Either, so we can short-circuit.
foldlM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldlM _ acc [] = pure acc
foldlM f acc (x : xs) = do
  acc' <- f acc x
  foldlM f acc' xs


{- | Build an inline table by folding the parsed @(KeyPath, Value)@
pairs. We track which sub-tables were created implicitly by an
earlier dotted key (and may therefore be extended by another)
vs. set explicitly to an inline-table value (which may not).
-}
applyInline
  :: (V.Vector (Text, TV.Value), Set KeyPath)
  -> (KeyPath, TV.Value)
  -> Either String (V.Vector (Text, TV.Value), Set KeyPath)
applyInline (tbl, dotted) (kp, v) =
  insertPathInline [] tbl kp v dotted


{- | Insert a value at a dotted path inside an inline table.

@path@ is the absolute prefix already traversed (so we can mark
newly-created sub-tables as @dotted@). @dotted@ is the set of
absolute paths whose sub-tables were created implicitly by a
previous dotted key in this inline; only those are extensible.
-}
insertPathInline
  :: KeyPath
  -- ^ prefix already traversed
  -> V.Vector (Text, TV.Value)
  -> KeyPath
  -> TV.Value
  -> Set KeyPath
  -> Either String (V.Vector (Text, TV.Value), Set KeyPath)
insertPathInline _ _ [] _ _ = Left "TOML: empty key path"
insertPathInline _pref tbl [k] v dotted =
  case lookupKey k tbl of
    Just _ ->
      Left $
        "TOML: duplicate key "
          ++ T.unpack k
          ++ " in inline table"
    Nothing -> Right (V.snoc tbl (k, v), dotted)
insertPathInline pref tbl (k : ks) v dotted = do
  let here = pref ++ [k]
  case lookupKey k tbl of
    Nothing -> do
      (sub, dotted') <-
        insertPathInline
          here
          V.empty
          ks
          v
          (Set.insert here dotted)
      pure (V.snoc tbl (k, TV.TTable sub), dotted')
    Just (TV.TTable sub)
      | Set.member here dotted -> do
          (sub', dotted') <- insertPathInline here sub ks v dotted
          pure (replaceKey k (TV.TTable sub') tbl, dotted')
      | otherwise ->
          Left $
            "TOML: cannot extend inline-table key "
              ++ T.unpack k
    Just _ ->
      Left $ "TOML: cannot extend non-table inline key " ++ T.unpack k


lookupKey :: Text -> V.Vector (Text, TV.Value) -> Maybe TV.Value
lookupKey k tbl = goV 0
  where
    !len = V.length tbl
    goV !i
      | i >= len = Nothing
      | otherwise = case tbl V.! i of
          (k', v)
            | k' == k -> Just v
            | otherwise -> goV (i + 1)


-- ---------------------------------------------------------------------------
-- Scalars (numbers, dates, times, datetimes)
-- ---------------------------------------------------------------------------

{- | Parse a scalar that doesn't begin with a quote / bracket /
brace / true / false. We grab a maximal run of value characters
and then resolve it.
-}
parseScalar :: P TV.Value
parseScalar = do
  m <- mark
  loop
  -- TOML local-datetime / offset-datetime allows a single space
  -- between the date and time portion (e.g. @1987-07-05 17:45:00@).
  -- After the first run, peek for that pattern and absorb it.
  maybeAbsorbDateTimeSpace m
  !raw <- sliceFrom m
  if T.null raw
    then failP "expected value"
    else resolveScalar raw
  where
    loop = do
      mc <- peek
      case mc of
        Just c | scalarChar c -> advance >> loop
        _ -> pure ()

    -- A scalar runs until whitespace, EOL, '#', or a flow terminator.
    scalarChar c =
      not
        ( c == ' '
            || c == '\t'
            || c == '\n'
            || c == '\r'
            || c == '#'
            || c == ','
            || c == ']'
            || c == '}'
        )

    -- If the current accumulated text looks like @YYYY-MM-DD@ and
    -- the next chars are @ HH:MM…@, swallow the space and continue
    -- consuming the time portion.
    maybeAbsorbDateTimeSpace startM = do
      raw <- sliceFrom startM
      if not (T.length raw == 10 && looksLikeJustDate raw)
        then pure ()
        else do
          mc1 <- peek
          mc2 <- peekN 1
          mc3 <- peekN 2
          mc4 <- peekN 3
          case (mc1, mc2, mc3, mc4) of
            (Just ' ', Just a, Just b, Just ':')
              | isDigit a && isDigit b -> do
                  _ <- advance -- consume the space
                  loop -- continue absorbing time chars
            _ -> pure ()


looksLikeJustDate :: Text -> Bool
looksLikeJustDate t =
  T.length t == 10
    && T.index t 4 == '-'
    && T.index t 7 == '-'
    && all (isDigit . T.index t) [0, 1, 2, 3, 5, 6, 8, 9]


resolveScalar :: Text -> P TV.Value
resolveScalar raw
  | raw == "inf" || raw == "+inf" = pure (TV.TFloat (1 / 0))
  | raw == "-inf" = pure (TV.TFloat (-(1 / 0)))
  | raw == "nan" || raw == "+nan" = pure (TV.TFloat (0 / 0))
  | raw == "-nan" = pure (TV.TFloat (negate (0 / 0)))
  | T.isPrefixOf "0x" raw = parseHexInt raw
  | T.isPrefixOf "0o" raw = parseOctInt raw
  | T.isPrefixOf "0b" raw = parseBinInt raw
  | otherwise =
      case classifyDateTime raw of
        Just v -> pure v
        Nothing
          | looksLikeFloat raw -> parseFloatLit raw
          | otherwise -> parseIntLit raw


-- ---------------------------------------------------------------------------
-- Integers
-- ---------------------------------------------------------------------------

parseHexInt :: Text -> P TV.Value
parseHexInt raw =
  case stripPrefixGen "0x" raw of
    Just body -> do
      (clean, ok) <- pure (cleanUnderscores body isHexDigit)
      if ok && not (T.null clean)
        then pure (TV.TInteger (T.foldl' (\a c -> a * 16 + fromIntegral (digitToInt c)) 0 clean))
        else failP $ "invalid hex integer: " ++ T.unpack raw
    _ -> failP "internal: parseHexInt"


parseOctInt :: Text -> P TV.Value
parseOctInt raw =
  case stripPrefixGen "0o" raw of
    Just body -> do
      let (clean, ok) = cleanUnderscores body isOctDigit
      if ok && not (T.null clean)
        then pure (TV.TInteger (T.foldl' (\a c -> a * 8 + fromIntegral (digitToInt c)) 0 clean))
        else failP $ "invalid octal integer: " ++ T.unpack raw
    _ -> failP "internal: parseOctInt"


parseBinInt :: Text -> P TV.Value
parseBinInt raw =
  case stripPrefixGen "0b" raw of
    Just body -> do
      let isBin c = c == '0' || c == '1'
          (clean, ok) = cleanUnderscores body isBin
      if ok && not (T.null clean)
        then pure (TV.TInteger (T.foldl' (\a c -> a * 2 + fromIntegral (digitToInt c)) 0 clean))
        else failP $ "invalid binary integer: " ++ T.unpack raw
    _ -> failP "internal: parseBinInt"


stripPrefixGen :: Text -> Text -> Maybe Text
stripPrefixGen p t
  | T.isPrefixOf p t = Just (T.drop (T.length p) t)
  | otherwise = Nothing


{- | Strip @_@ separators while validating that they appear only
between two valid digits. Returns @(cleaned, ok)@.
-}
cleanUnderscores :: Text -> (Char -> Bool) -> (Text, Bool)
cleanUnderscores raw digitOK = go (T.unpack raw) [] False True
  where
    -- (input, accReversed, lastWasUnderscore, atStart)
    go [] acc lastUS atStart =
      let valid = not lastUS && not atStart
      in (T.pack (reverse acc), valid)
    go ('_' : _) _ True _ = (T.empty, False) -- "__"
    go ('_' : _) _ _ True = (T.empty, False) -- leading "_"
    go ('_' : rest) acc False False = go rest acc True False
    go (c : rest) acc _ _
      | digitOK c = go rest (c : acc) False False
      | otherwise = (T.empty, False)


parseIntLit :: Text -> P TV.Value
parseIntLit raw0 = do
  let (signMaybe, body) = case T.uncons raw0 of
        Just ('+', r) -> (Nothing, r)
        Just ('-', r) -> (Just True, r)
        _ -> (Nothing, raw0)
      (clean, ok) = cleanUnderscores body isDigit
  if not ok || T.null clean
    then failP $ "invalid integer: " ++ T.unpack raw0
    else
      -- TOML rejects leading zeros on decimal literals (except "0").
      if T.length clean > 1 && T.head clean == '0'
        then failP $ "leading zero in integer: " ++ T.unpack raw0
        else
          let !mag = T.foldl' (\a c -> a * 10 + toInteger (digitToInt c)) 0 clean
          in pure
               ( TV.TInteger
                   ( case signMaybe of
                       Just True -> negate mag
                       _ -> mag
                   )
               )


-- ---------------------------------------------------------------------------
-- Floats
-- ---------------------------------------------------------------------------

looksLikeFloat :: Text -> Bool
looksLikeFloat t =
  -- Has '.' or 'e/E' somewhere, but isn't just a sign.
  case T.uncons t of
    Just (c, _) | c == '.' -> True
    _ ->
      T.any (\c -> c == '.' || c == 'e' || c == 'E') t
        && T.any isDigit t


parseFloatLit :: Text -> P TV.Value
parseFloatLit raw = do
  let (sign, body) = case T.uncons raw of
        Just ('+', r) -> (1.0, r)
        Just ('-', r) -> (-1.0, r)
        _ -> (1.0, raw)
  -- Validate body: must have digit on either side of '.' and 'e';
  -- underscores must be surrounded by digits.
  if not (validFloat body)
    then failP $ "invalid float: " ++ T.unpack raw
    else
      let cleaned = T.filter (/= '_') body
          str = T.unpack cleaned
      in case reads str :: [(Double, String)] of
           [(d, "")] -> pure (TV.TFloat (sign * d))
           _ -> failP $ "invalid float: " ++ T.unpack raw


-- | Sanity check a float literal body (post-sign).
validFloat :: Text -> Bool
validFloat t
  | T.null t = False
  | T.head t == '_' || T.last t == '_' = False
  | T.head t == '.' || T.last t == '.' = False
  | T.head t == '0'
      && T.length t > 1
      && T.index t 1 /= '.'
      && T.index t 1 /= 'e'
      && T.index t 1 /= 'E' =
      False
  | T.any
      ( \c ->
          not
            ( isDigit c
                || c == '.'
                || c == 'e'
                || c == 'E'
                || c == '_'
                || c == '+'
                || c == '-'
            )
      )
      t =
      False
  | otherwise = noDoubleUnderscore t && noUnderscoreNextToNonDigit t


noDoubleUnderscore :: Text -> Bool
noDoubleUnderscore = not . T.isInfixOf "__"


noUnderscoreNextToNonDigit :: Text -> Bool
noUnderscoreNextToNonDigit t = goT 0
  where
    !len = T.length t
    goT !i
      | i >= len = True
      | T.index t i == '_' =
          (i > 0 && isDigit (T.index t (i - 1)))
            && (i + 1 < len && isDigit (T.index t (i + 1)))
            && goT (i + 1)
      | otherwise = goT (i + 1)


-- ---------------------------------------------------------------------------
-- Date / time / datetime
-- ---------------------------------------------------------------------------

{- | Try to parse a TOML date / time / datetime scalar from a raw
token, validating each component (month, day, hour, minute,
second, optional offset) against its legal range. Returns
'Nothing' if the shape doesn't match; @'Just' v@ if valid.
-}
classifyDateTime :: Text -> Maybe TV.Value
classifyDateTime raw
  | hasDateShape raw =
      if T.length raw == 10
        then if validDate raw then Just (TV.TDate raw) else Nothing
        else
          if not (validDate raw)
            then Nothing
            else parseDateTime raw
  | hasTimeShape raw =
      if validTimePart 0 (T.length raw) raw
        then Just (TV.TTime raw)
        else Nothing
  | otherwise = Nothing
  where
    hasDateShape t =
      T.length t >= 10
        && T.index t 4 == '-'
        && T.index t 7 == '-'
        && digitsAt 0 4 t
        && digitsAt 5 7 t
        && digitsAt 8 10 t

    hasTimeShape t =
      T.length t >= 5
        && digitsAt 0 2 t
        && T.index t 2 == ':'
        && digitsAt 3 5 t


parseDateTime :: Text -> Maybe TV.Value
parseDateTime raw
  | T.length raw <= 10 = Nothing
  | not (sep == 'T' || sep == 't' || sep == ' ') = Nothing
  | otherwise =
      let timeEnd = case findOffsetStart raw of
            Just i -> i
            Nothing -> T.length raw
      in if validTimePart 11 timeEnd raw && (timeEnd == T.length raw || validOffset (T.drop timeEnd raw))
           then Just (TV.TDateTime raw)
           else Nothing
  where
    sep = T.index raw 10


{- | Where does the offset start in a datetime string? (Position of
@Z@, @z@, or @+@ / @-@ /after/ the time portion.)
-}
findOffsetStart :: Text -> Maybe Int
findOffsetStart t = (+ 11) <$> T.findIndex (\c -> c == 'Z' || c == 'z' || c == '+' || c == '-') (T.drop 11 t)


-- | Validate @HH:MM[:SS[.fff]]@ from column @c@ up to @end@.
validTimePart :: Int -> Int -> Text -> Bool
validTimePart !c !end t
  | end < c + 5 = False
  | not (digitsAt c (c + 2) t) = False
  | T.index t (c + 2) /= ':' = False
  | not (digitsAt (c + 3) (c + 5) t) = False
  | hh > 23 || mm > 59 = False
  | end == c + 5 = True
  | otherwise =
      end >= secondsEnd
        && T.index t (c + 5) == ':'
        && digitsAt (c + 6) secondsEnd t
        && parseDigits (c + 6) 2 t <= 60 -- 60 for leap second tolerance
        && ( end == secondsEnd
               || (end > fractionStart && T.index t secondsEnd == '.' && T.all isDigit fraction)
           )
  where
    hh = parseDigits c 2 t
    mm = parseDigits (c + 3) 2 t
    secondsEnd = c + 8
    fractionStart = secondsEnd + 1
    fraction = T.take (end - fractionStart) (T.drop fractionStart t)


-- | Valid offset suffix: @Z@ / @z@ alone, or @±HH:MM@.
validOffset :: Text -> Bool
validOffset t = case T.unpack t of
  "Z" -> True
  "z" -> True
  (s : rest)
    | s == '+' || s == '-' ->
        let body = T.pack rest
        in T.length body == 5
             && digitsAt 0 2 body
             && T.index body 2 == ':'
             && digitsAt 3 5 body
             && parseDigits 0 2 body <= 23
             && parseDigits 3 5 body <= 59
  _ -> False


{- | Validate a date in the @YYYY-MM-DD@ shape: month 1..12, day
in range for that month (with leap-year handling for Feb).
-}
validDate :: Text -> Bool
validDate t
  | T.length t < 10 = False
  | not (digitsAt 0 4 t && digitsAt 5 7 t && digitsAt 8 10 t) = False
  | T.index t 4 /= '-' || T.index t 7 /= '-' = False
  | mm < 1 || mm > 12 = False
  | dd < 1 || dd > daysInMonth yy mm = False
  | otherwise = True
  where
    yy = parseDigits 0 4 t
    mm = parseDigits 5 2 t
    dd = parseDigits 8 2 t


daysInMonth :: Int -> Int -> Int
daysInMonth yy mm = case mm of
  1 -> 31
  2 -> if isLeap yy then 29 else 28
  3 -> 31
  4 -> 30
  5 -> 31
  6 -> 30
  7 -> 31
  8 -> 31
  9 -> 30
  10 -> 31
  11 -> 30
  12 -> 31
  _ -> 0
  where
    isLeap y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || y `mod` 400 == 0


parseDigits :: Int -> Int -> Text -> Int
parseDigits !off !n t =
  T.foldl' (\a c -> a * 10 + digitToInt c) 0 (T.take n (T.drop off t))


digitsAt :: Int -> Int -> Text -> Bool
digitsAt a b t =
  all (\i -> i < T.length t && isDigit (T.index t i)) [a .. b - 1]


-- ---------------------------------------------------------------------------
-- Document assembly
-- ---------------------------------------------------------------------------

{- | Tracks the tables we've seen and their kinds, so that we can
detect duplicate / illegal redefinition.

The 'bsPending' field is a perf optimisation: it defers the
expensive per-pair AoT-tail rewrites that the natural
@applyAction@-per-pair implementation would do. With a 100-entry
@[[people]]@ + 3 fields each, the naive path costs ~300 walks
through a 100-element @TArray@, each of which @V.init@ + @V.snoc@s
to replace the last element. Deferring drops it to ~100 walks
(one per header transition).
-}
data BuildState = BuildState
  { bsRoot :: !(V.Vector (Text, TV.Value))
  , bsCurrent :: !KeyPath
  -- ^ active header (where pairs live)
  , bsPending :: ![(KeyPath, TV.Value)]
  {- ^ Reversed list of pairs accumulated under the current header
  that haven't been merged into 'bsRoot' yet. Flushed at every
  header transition and at end-of-document.
  -}
  , bsTables :: !(Set KeyPath)
  -- ^ paths defined by [a.b] headers
  , bsArrays :: !(Set KeyPath)
  -- ^ paths defined by [[a]] headers
  , bsInline :: !(Set KeyPath)
  -- ^ paths whose value is an inline table
  , bsDotted :: !(Set KeyPath)
  -- ^ tables created via @a.b = …@ dotted keys
  , bsStatic :: !(Set KeyPath)
  -- ^ paths whose value is a static (value) array
  }


emptyBS :: BuildState
emptyBS = BuildState V.empty [] [] Set.empty Set.empty Set.empty Set.empty Set.empty


{- | Assemble the parsed document. Surfaces table-redefinition /
duplicate-key conflicts as parse errors.
-}
assembleDocumentE :: Doc -> Either String TV.Value
assembleDocumentE (Doc acts) = do
  bs <- foldlM applyAction emptyBS acts
  bs' <- flushPending bs
  pure (TV.TTable (bsRoot bs'))


applyAction :: BuildState -> TopAction -> Either String BuildState
applyAction bs = \case
  ATable kp -> do
    when (Set.member kp (bsTables bs)) $
      Left ("TOML: table " ++ showKP kp ++ " redefined")
    when (Set.member kp (bsArrays bs)) $
      Left
        ( "TOML: "
            ++ showKP kp
            ++ " is an array of tables, cannot be redefined as table"
        )
    when (Set.member kp (bsInline bs)) $
      Left ("TOML: cannot redefine inline table at " ++ showKP kp)
    when (Set.member kp (bsDotted bs)) $
      Left
        ( "TOML: "
            ++ showKP kp
            ++ " was implicitly defined via dotted keys"
        )
    when (anyAncestorInline kp (bsInline bs)) $
      Left ("TOML: cannot extend inline table at " ++ showKP kp)
    when (anyAncestorStatic kp (bsStatic bs)) $
      Left
        ( "TOML: cannot extend static-array element at "
            ++ showKP kp
        )
    bs1 <- flushPending bs
    root' <- ensureTablePath kp (bsRoot bs1) NoAOT
    pure
      bs1
        { bsRoot = root'
        , bsCurrent = kp
        , bsTables = Set.insert kp (bsTables bs1)
        }
  AArrayOf kp -> do
    when (Set.member kp (bsTables bs)) $
      Left
        ( "TOML: "
            ++ showKP kp
            ++ " is a table, cannot be redefined as array"
        )
    when (Set.member kp (bsStatic bs)) $
      Left
        ( "TOML: "
            ++ showKP kp
            ++ " is a static array, cannot be extended via [[...]]"
        )
    when (anyAncestorInline kp (bsInline bs)) $
      Left ("TOML: cannot extend inline table at " ++ showKP kp)
    when (anyAncestorStatic kp (bsStatic bs)) $
      Left
        ( "TOML: cannot extend static-array element at "
            ++ showKP kp
        )
    bs1 <- flushPending bs
    root' <- appendAOT kp (bsRoot bs1)
    -- A new AOT element wipes out any "already-defined" tracking
    -- for paths nested under @kp@: the previous element's sub-table
    -- headers no longer count against the new element. The AOT
    -- itself remains marked.
    let tablesPruned = Set.filter (not . isStrictDescendant kp) (bsTables bs1)
        arraysPruned = Set.filter (not . isStrictDescendant kp) (bsArrays bs1)
        inlinePruned = Set.filter (not . isStrictDescendant kp) (bsInline bs1)
        dottedPruned = Set.filter (not . isStrictDescendant kp) (bsDotted bs1)
    pure
      bs1
        { bsRoot = root'
        , bsCurrent = kp
        , bsArrays = Set.insert kp arraysPruned
        , bsTables = tablesPruned
        , bsInline = inlinePruned
        , bsDotted = dottedPruned
        }
  APair kp v -> do
    let owner = bsCurrent bs
        fullPath = owner ++ kp
        -- Every proper prefix /below the current header/ that the
        -- dotted key implicitly creates as a table.
        prefixesBelow =
          let segs = map (: []) kp
              builds = scanl1 (++) segs
              lastIdx = length builds - 1
          in take lastIdx (map (owner ++) builds)
    when (Set.member fullPath (bsTables bs)) $
      Left ("TOML: " ++ showKP fullPath ++ " already defined as table")
    when (Set.member fullPath (bsArrays bs)) $
      Left ("TOML: " ++ showKP fullPath ++ " already defined as array")
    when (anyAncestorInline fullPath (bsInline bs)) $
      Left ("TOML: cannot extend inline table at " ++ showKP fullPath)
    -- A dotted key may not reach into a sub-table that was already
    -- defined by an explicit @[a.b.c]@ header: this is the
    -- "append-with-dotted-keys" injection rule from
    -- toml-lang/toml#859.
    case firstThat (`Set.member` bsTables bs) prefixesBelow of
      Just hit ->
        Left
          ( "TOML: dotted key extends previously-defined table "
              ++ showKP hit
          )
      Nothing -> pure ()
    case firstThat (`Set.member` bsArrays bs) prefixesBelow of
      Just hit ->
        Left
          ( "TOML: dotted key extends array-of-tables "
              ++ showKP hit
          )
      Nothing -> pure ()
    -- Defer the actual tree mutation: cons the pair onto the
    -- pending buffer and flush it at the next header transition.
    -- 'flushPending' walks down to the active header's leaf table
    -- exactly once and inserts every pending pair there, instead
    -- of paying that walk per pair.
    --
    -- We still need to keep the duplicate-key bookkeeping eager
    -- (insertLocal at flush time would also catch duplicate keys
    -- inside a single header, but the cross-header bsTables /
    -- bsArrays / bsInline checks above would not).
    let (inline', static') = case v of
          TV.TTable _ -> (Set.insert fullPath (bsInline bs), bsStatic bs)
          TV.TArray _ -> (bsInline bs, Set.insert fullPath (bsStatic bs))
          _ -> (bsInline bs, bsStatic bs)
    pure
      bs
        { bsPending = (kp, v) : bsPending bs
        , bsDotted = foldr Set.insert (bsDotted bs) prefixesBelow
        , bsInline = inline'
        , bsStatic = static'
        }


{- | Drain 'bsPending' into 'bsRoot' under 'bsCurrent'. Walks to the
header's leaf table once and merges every pair via 'insertLocal',
amortising the AoT-tail walk across all pairs in the header.
-}
flushPending :: BuildState -> Either String BuildState
flushPending bs
  | null (bsPending bs) = Right bs
  | otherwise = do
      let owner = bsCurrent bs
          pairs = reverse (bsPending bs)
      root' <- batchInsertUnder owner pairs (bsRoot bs)
      pure bs {bsRoot = root', bsPending = []}


{- | Insert a list of @(key-path, value)@ pairs under a header path
in one walk. For an enclosing AoT we insert into the /last/ table
of the array (matching @[[a]]@ tail semantics).
-}
batchInsertUnder
  :: KeyPath
  -> [(KeyPath, TV.Value)]
  -> V.Vector (Text, TV.Value)
  -> Either String (V.Vector (Text, TV.Value))
batchInsertUnder [] pairs tbl =
  foldlM (\acc (kp, v) -> insertLocal kp v acc) tbl pairs
batchInsertUnder (h : hs) pairs tbl = case lookupKey h tbl of
  Nothing -> do
    sub <- batchInsertUnder hs pairs V.empty
    pure (V.snoc tbl (h, TV.TTable sub))
  Just (TV.TTable sub) -> do
    sub' <- batchInsertUnder hs pairs sub
    pure (replaceKey h (TV.TTable sub') tbl)
  Just (TV.TArray xs)
    | not (V.null xs)
    , TV.TTable lst <- V.last xs -> do
        lst' <- batchInsertUnder hs pairs lst
        let xs' = V.snoc (V.init xs) (TV.TTable lst')
        pure (replaceKey h (TV.TArray xs') tbl)
  Just _ ->
    Left $ "TOML: cannot extend non-table key " ++ T.unpack h


firstThat :: (a -> Bool) -> [a] -> Maybe a
firstThat _ [] = Nothing
firstThat p (x : xs) = if p x then Just x else firstThat p xs


{- | Whether an array-of-tables segment may appear along the path.
For 'ensureTablePath' from a [a.b] header we never create an AOT
mid-path; for 'insertPairUnder' we follow the current AOT's tail.
-}
data AOTMode = NoAOT


when :: Monad m => Bool -> m () -> m ()
when True m = m
when False _ = pure ()


showKP :: KeyPath -> String
showKP = T.unpack . T.intercalate "."


anyAncestorInline :: KeyPath -> Set KeyPath -> Bool
anyAncestorInline kp inl = any (`Set.member` inl) (properPrefixes kp)
  where
    properPrefixes [] = []
    properPrefixes [_] = []
    properPrefixes xs =
      scanl1
        (++)
        (map (: []) (init xs))


{- | @isStrictDescendant prefix path@ holds when @path@ extends
@prefix@ by at least one segment.
-}
isStrictDescendant :: KeyPath -> KeyPath -> Bool
isStrictDescendant prefix path =
  length path > length prefix && take (length prefix) path == prefix


{- | True when any proper prefix of @kp@ is bound to a static
(value-only) array.
-}
anyAncestorStatic :: KeyPath -> Set KeyPath -> Bool
anyAncestorStatic kp ss = any (`Set.member` ss) (properPrefixes kp)
  where
    properPrefixes [] = []
    properPrefixes [_] = []
    properPrefixes xs =
      scanl1
        (++)
        (map (: []) (init xs))


{- | Ensure every table along the path exists (creating empty ones
as needed), and return the updated root. Used for @[a.b]@
headers; the path may traverse through the last element of an
existing array-of-tables.
-}
ensureTablePath
  :: KeyPath
  -> V.Vector (Text, TV.Value)
  -> AOTMode
  -> Either String (V.Vector (Text, TV.Value))
ensureTablePath [] tbl _ = Right tbl
ensureTablePath (k : ks) tbl aot = case lookupKey k tbl of
  Nothing -> do
    sub <- ensureTablePath ks V.empty aot
    pure (V.snoc tbl (k, TV.TTable sub))
  Just (TV.TTable sub) -> do
    sub' <- ensureTablePath ks sub aot
    pure (replaceKey k (TV.TTable sub') tbl)
  Just (TV.TArray xs)
    | not (V.null xs)
    , TV.TTable lst <- V.last xs -> do
        lst' <- ensureTablePath ks lst aot
        let xs' = V.snoc (V.init xs) (TV.TTable lst')
        pure (replaceKey k (TV.TArray xs') tbl)
  Just _ ->
    Left $ "TOML: cannot extend non-table key " ++ T.unpack k


{- | Append a fresh empty table to the array-of-tables at the given
path, creating intermediate tables (or following existing AOT
tails) as needed.
-}
appendAOT
  :: KeyPath
  -> V.Vector (Text, TV.Value)
  -> Either String (V.Vector (Text, TV.Value))
appendAOT [] _ = Left "TOML: empty array-of-tables key"
appendAOT [k] tbl = case lookupKey k tbl of
  Nothing ->
    Right (V.snoc tbl (k, TV.TArray (V.singleton (TV.TTable V.empty))))
  Just (TV.TArray xs) ->
    let xs' = V.snoc xs (TV.TTable V.empty)
    in Right (replaceKey k (TV.TArray xs') tbl)
  Just _ ->
    Left $ "TOML: " ++ T.unpack k ++ " is not an array of tables"
appendAOT (k : ks) tbl = case lookupKey k tbl of
  Nothing -> do
    sub <- appendAOT ks V.empty
    pure (V.snoc tbl (k, TV.TTable sub))
  Just (TV.TTable sub) -> do
    sub' <- appendAOT ks sub
    pure (replaceKey k (TV.TTable sub') tbl)
  Just (TV.TArray xs)
    | not (V.null xs)
    , TV.TTable lst <- V.last xs -> do
        lst' <- appendAOT ks lst
        let xs' = V.snoc (V.init xs) (TV.TTable lst')
        pure (replaceKey k (TV.TArray xs') tbl)
  Just _ ->
    Left $ "TOML: cannot extend non-table key " ++ T.unpack k


-- | Insert a dotted-key pair into a single table.
insertLocal
  :: KeyPath
  -> TV.Value
  -> V.Vector (Text, TV.Value)
  -> Either String (V.Vector (Text, TV.Value))
insertLocal [] _ _ = Left "TOML: empty key path"
insertLocal [k] v tbl = case lookupKey k tbl of
  Just _ -> Left $ "TOML: duplicate key " ++ T.unpack k
  Nothing -> Right (V.snoc tbl (k, v))
insertLocal (k : ks) v tbl = case lookupKey k tbl of
  Nothing -> do
    sub <- insertLocal ks v V.empty
    pure (V.snoc tbl (k, TV.TTable sub))
  Just (TV.TTable sub) -> do
    sub' <- insertLocal ks v sub
    pure (replaceKey k (TV.TTable sub') tbl)
  Just _ ->
    Left $ "TOML: cannot extend non-table key " ++ T.unpack k


{- | Replace the value associated with @k@ in @tbl@. Assumes @k@ is
present.
-}
replaceKey
  :: Text
  -> TV.Value
  -> V.Vector (Text, TV.Value)
  -> V.Vector (Text, TV.Value)
replaceKey k v = V.map (\(k', x) -> if k' == k then (k', v) else (k', x))
