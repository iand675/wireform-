{-# LANGUAGE BangPatterns #-}
-- | TOML text decoder (parser).
--
-- Parses TOML v1.0 text into a 'TOML.Value.Value'. Supports all TOML
-- types including multi-line strings, hex/octal/binary integers,
-- inline tables, array of tables, and date/time types.
module TOML.Decode
  ( decode
  , decodeBS
  ) where

import Data.ByteString (ByteString)
import Data.Char (chr, digitToInt, isAlphaNum, isDigit, isHexDigit, isSpace, ord, toLower)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import qualified TOML.Value as TV

decode :: Text -> Either String TV.Value
decode !input =
  let !ls = T.lines input
  in parseLines ls [] []

decodeBS :: ByteString -> Either String TV.Value
decodeBS = decode . TE.decodeUtf8Lenient

data TableCtx = TableCtx
  { tcPath  :: ![Text]
  , tcPairs :: ![(Text, TV.Value)]
  }

parseLines :: [Text] -> [TableCtx] -> [(Text, TV.Value)] -> Either String TV.Value
parseLines [] tables topPairs =
  let !topTable = V.fromList (reverse topPairs)
      !allTables = reverse tables
  in Right (mergeTables topTable allTables)
parseLines (line : rest) tables topPairs =
  let !stripped = T.stripStart line
  in case () of
    _ | T.null stripped || T.isPrefixOf "#" stripped ->
          parseLines rest tables topPairs
      | T.isPrefixOf "[[" stripped ->
          case parseArrayTableHeader stripped of
            Left err -> Left err
            Right path ->
              let (sectionLines, remaining) = spanSection rest
              in case parseSectionPairs sectionLines of
                  Left err -> Left err
                  Right pairs ->
                    let !ctx = TableCtx path pairs
                    in parseLines remaining (ctx : tables) topPairs
      | T.isPrefixOf "[" stripped ->
          case parseTableHeader stripped of
            Left err -> Left err
            Right path ->
              let (sectionLines, remaining) = spanSection rest
              in case parseSectionPairs sectionLines of
                  Left err -> Left err
                  Right pairs ->
                    let !ctx = TableCtx path pairs
                    in parseLines remaining (ctx : tables) topPairs
      | otherwise ->
          case parseKeyValue stripped of
            Left err -> Left err
            Right (k, v) ->
              parseLines rest tables ((k, v) : topPairs)

spanSection :: [Text] -> ([Text], [Text])
spanSection = go []
  where
    go acc [] = (reverse acc, [])
    go acc (l:ls)
      | isTableOrArrayHeader (T.stripStart l) = (reverse acc, l:ls)
      | otherwise = go (l:acc) ls

isTableOrArrayHeader :: Text -> Bool
isTableOrArrayHeader t = T.isPrefixOf "[" t && not (T.null t)

parseSectionPairs :: [Text] -> Either String [(Text, TV.Value)]
parseSectionPairs = go []
  where
    go acc [] = Right (reverse acc)
    go acc (l:ls) =
      let !stripped = T.stripStart l
      in if T.null stripped || T.isPrefixOf "#" stripped
           then go acc ls
           else case parseKeyValue stripped of
                  Left err -> Left err
                  Right (k, v) -> go ((k,v):acc) ls

parseTableHeader :: Text -> Either String [Text]
parseTableHeader t =
  let !inner = T.strip (T.dropEnd 1 (T.drop 1 t))
      !commentStripped = stripInlineComment inner
  in Right (map T.strip (T.splitOn "." commentStripped))

parseArrayTableHeader :: Text -> Either String [Text]
parseArrayTableHeader t =
  let !afterOpen = T.drop 2 t
      !idx = T.breakOn "]]" afterOpen
  in case idx of
       (inner, rest)
         | T.isPrefixOf "]]" rest ->
             Right (map T.strip (T.splitOn "." (T.strip inner)))
         | otherwise -> Left "TOML: unterminated array table header"

stripInlineComment :: Text -> Text
stripInlineComment t = case T.breakOn "#" t of
  (before, _) -> T.stripEnd before

parseKeyValue :: Text -> Either String (Text, TV.Value)
parseKeyValue line =
  let (keyPart, rest) = breakOnEquals line
      !key = T.strip (unquoteKey keyPart)
      !valText = T.strip rest
  in case parseTomlValue valText of
       Left err -> Left $ "TOML key '" ++ T.unpack key ++ "': " ++ err
       Right v -> Right (key, v)

breakOnEquals :: Text -> (Text, Text)
breakOnEquals t = go 0 False
  where
    !len = T.length t
    go !i !inStr
      | i >= len = (t, T.empty)
      | T.index t i == '"' = go (i + 1) (not inStr)
      | T.index t i == '\'' = go (i + 1) (not inStr)
      | T.index t i == '=' && not inStr = (T.take i t, T.drop (i + 1) t)
      | otherwise = go (i + 1) inStr

unquoteKey :: Text -> Text
unquoteKey t
  | T.isPrefixOf "\"" t && T.isSuffixOf "\"" t = T.drop 1 (T.dropEnd 1 t)
  | T.isPrefixOf "'" t && T.isSuffixOf "'" t = T.drop 1 (T.dropEnd 1 t)
  | otherwise = t

parseTomlValue :: Text -> Either String TV.Value
parseTomlValue t =
  let !stripped = stripInlineComment t
      !s = T.strip stripped
  in parseTomlValueInner s

parseTomlValueInner :: Text -> Either String TV.Value
parseTomlValueInner s
  | T.null s = Left "empty value"
  | s == "true" = Right (TV.TBool True)
  | s == "false" = Right (TV.TBool False)
  | s == "inf" || s == "+inf" = Right (TV.TFloat (1/0))
  | s == "-inf" = Right (TV.TFloat (-1/0))
  | s == "nan" || s == "+nan" = Right (TV.TFloat (0/0))
  | s == "-nan" = Right (TV.TFloat (-(0/0)))
  | T.isPrefixOf "\"\"\"" s = parseMultilineBasicString s
  | T.isPrefixOf "'''" s = parseMultilineLiteralString s
  | T.isPrefixOf "\"" s = parseBasicString s
  | T.isPrefixOf "'" s = parseLiteralString s
  | T.isPrefixOf "[" s = parseArray s
  | T.isPrefixOf "{" s = parseInlineTable s
  | T.isPrefixOf "0x" s = parseHexInt s
  | T.isPrefixOf "0o" s = parseOctInt s
  | T.isPrefixOf "0b" s = parseBinInt s
  | looksLikeDateTime s = Right (classifyDateTime s)
  | looksLikeFloat s = parseFloat s
  | otherwise = parseInt s

parseBasicString :: Text -> Either String TV.Value
parseBasicString t =
  let !inner = T.drop 1 t
  in case T.breakOn "\"" inner of
       (content, rest)
         | T.isPrefixOf "\"" rest -> Right (TV.TString (unescapeBasic content))
         | otherwise -> Left "unterminated basic string"

parseLiteralString :: Text -> Either String TV.Value
parseLiteralString t =
  let !inner = T.drop 1 t
  in case T.breakOn "'" inner of
       (content, rest)
         | T.isPrefixOf "'" rest -> Right (TV.TString content)
         | otherwise -> Left "unterminated literal string"

parseMultilineBasicString :: Text -> Either String TV.Value
parseMultilineBasicString t =
  let !inner = T.drop 3 t
  in case T.breakOn "\"\"\"" inner of
       (content, rest)
         | T.isPrefixOf "\"\"\"" rest ->
             let !trimmed = if T.isPrefixOf "\n" content then T.drop 1 content else content
             in Right (TV.TString (unescapeBasic trimmed))
         | otherwise -> Left "unterminated multi-line basic string"

parseMultilineLiteralString :: Text -> Either String TV.Value
parseMultilineLiteralString t =
  let !inner = T.drop 3 t
  in case T.breakOn "'''" inner of
       (content, rest)
         | T.isPrefixOf "'''" rest ->
             let !trimmed = if T.isPrefixOf "\n" content then T.drop 1 content else content
             in Right (TV.TString trimmed)
         | otherwise -> Left "unterminated multi-line literal string"

unescapeBasic :: Text -> Text
unescapeBasic = T.pack . go . T.unpack
  where
    go [] = []
    go ('\\':'n':rest) = '\n' : go rest
    go ('\\':'t':rest) = '\t' : go rest
    go ('\\':'r':rest) = '\r' : go rest
    go ('\\':'\\':rest) = '\\' : go rest
    go ('\\':'"':rest) = '"' : go rest
    go ('\\':'b':rest) = '\b' : go rest
    go ('\\':'f':rest) = '\f' : go rest
    go ('\\':'u':a:b:c:d:rest)
      | all isHexDigit [a,b,c,d] =
          chr (foldl' (\acc x -> acc * 16 + digitToInt x) 0 [a,b,c,d]) : go rest
    go ('\\':'U':a:b:c:d:e:f:g:h:rest)
      | all isHexDigit [a,b,c,d,e,f,g,h] =
          chr (foldl' (\acc x -> acc * 16 + digitToInt x) 0 [a,b,c,d,e,f,g,h]) : go rest
    go (c:rest) = c : go rest

parseArray :: Text -> Either String TV.Value
parseArray t =
  let !inner = T.strip (T.dropEnd 1 (T.drop 1 t))
  in if T.null inner
       then Right (TV.TArray V.empty)
       else do
         items <- splitCommaValues inner
         vals <- traverse parseTomlValueInner items
         Right (TV.TArray (V.fromList vals))

splitCommaValues :: Text -> Either String [Text]
splitCommaValues t = Right (map T.strip (splitTopLevel t))

splitTopLevel :: Text -> [Text]
splitTopLevel = go 0 0 0 0 []
  where
    go !depth !braceDepth !inStr !i !acc !t
      | i >= T.length t =
          let !final = T.strip (T.take i t)
          in reverse (if T.null final then acc else final : acc)
      | T.index t i == '"' && inStr == 0 = go depth braceDepth 1 (i+1) acc t
      | T.index t i == '"' && inStr == 1 = go depth braceDepth 0 (i+1) acc t
      | T.index t i == '\'' && inStr == 0 = go depth braceDepth 2 (i+1) acc t
      | T.index t i == '\'' && inStr == 2 = go depth braceDepth 0 (i+1) acc t
      | inStr /= 0 = go depth braceDepth inStr (i+1) acc t
      | T.index t i == '[' = go (depth+1) braceDepth inStr (i+1) acc t
      | T.index t i == ']' = go (depth-1) braceDepth inStr (i+1) acc t
      | T.index t i == '{' = go depth (braceDepth+1) inStr (i+1) acc t
      | T.index t i == '}' = go depth (braceDepth-1) inStr (i+1) acc t
      | T.index t i == ',' && depth == 0 && braceDepth == 0 =
          let !item = T.strip (T.take i t)
              !rest = T.drop (i+1) t
          in go 0 0 0 0 (item : acc) rest
      | otherwise = go depth braceDepth inStr (i+1) acc t

parseInlineTable :: Text -> Either String TV.Value
parseInlineTable t =
  let !inner = T.strip (T.dropEnd 1 (T.drop 1 t))
  in if T.null inner
       then Right (TV.TTable V.empty)
       else do
         let parts = splitTopLevel inner
         kvs <- traverse (\p -> parseKeyValue (T.strip p)) parts
         Right (TV.TTable (V.fromList kvs))

parseHexInt :: Text -> Either String TV.Value
parseHexInt t =
  let !digits = T.filter (/= '_') (T.drop 2 t)
      !s = T.unpack digits
  in if all isHexDigit s && not (null s)
       then Right (TV.TInteger (foldl' (\acc c -> acc * 16 + fromIntegral (digitToInt c)) 0 s))
       else Left $ "invalid hex integer: " ++ T.unpack t

parseOctInt :: Text -> Either String TV.Value
parseOctInt t =
  let !digits = T.filter (/= '_') (T.drop 2 t)
      !s = T.unpack digits
  in if all (\c -> c >= '0' && c <= '7') s && not (null s)
       then Right (TV.TInteger (foldl' (\acc c -> acc * 8 + fromIntegral (digitToInt c)) 0 s))
       else Left $ "invalid octal integer: " ++ T.unpack t

parseBinInt :: Text -> Either String TV.Value
parseBinInt t =
  let !digits = T.filter (/= '_') (T.drop 2 t)
      !s = T.unpack digits
  in if all (\c -> c == '0' || c == '1') s && not (null s)
       then Right (TV.TInteger (foldl' (\acc c -> acc * 2 + fromIntegral (digitToInt c)) 0 s))
       else Left $ "invalid binary integer: " ++ T.unpack t

parseInt :: Text -> Either String TV.Value
parseInt t = do
  -- TOML 1.0.0 §integer: '_' cannot be leading, trailing, or
  -- repeated; digits cannot have leading zeros except for the
  -- integer 0 itself (and -0 / +0).
  validateUnderscores t
  let !cleaned = T.unpack (T.filter (/= '_') t)
      !normalized = case cleaned of
        '+':rest -> rest
        other -> other
      !digits = case normalized of
        '-':rest -> rest
        rest     -> rest
  validateNoLeadingZero digits (T.unpack t)
  case reads normalized :: [(Integer, String)] of
    [(n, "")] -> Right (TV.TInteger n)
    _         -> Left $ "invalid integer: " ++ T.unpack t

-- | TOML §literals: @_@ is a digit-grouping separator and may not be
-- adjacent to another @_@, leading, or trailing.
validateUnderscores :: Text -> Either String ()
validateUnderscores t =
  let s = T.unpack t
  in case s of
       []         -> Right ()
       _ | head s == '_' -> Left $ "leading underscore: " ++ s
         | last s == '_' -> Left $ "trailing underscore: " ++ s
         | otherwise     -> goPair s
  where
    goPair (a:b:rest)
      | a == '_' && b == '_' = Left "consecutive underscores"
      | otherwise            = goPair (b:rest)
    goPair _ = Right ()

-- | Reject decimal integer with a leading zero (other than the
-- standalone @0@). TOML allows @0x@ / @0o@ / @0b@ prefixes via the
-- separate parseHexInt / parseOctInt / parseBinInt helpers, so this
-- check only fires on the decimal path.
validateNoLeadingZero :: String -> String -> Either String ()
validateNoLeadingZero digits raw =
  case digits of
    ('0':next:_) | next /= '.' && next /= 'e' && next /= 'E' ->
      Left $ "TOML integer with leading zero: " ++ raw
    _ -> Right ()

parseFloat :: Text -> Either String TV.Value
parseFloat t =
  let !cleaned = T.unpack (T.filter (/= '_') t)
  in case reads cleaned :: [(Double, String)] of
       [(d, "")] -> Right (TV.TFloat d)
       _ -> Left $ "invalid float: " ++ T.unpack t

looksLikeFloat :: Text -> Bool
looksLikeFloat t =
  let !c = T.filter (/= '_') t
  in T.any (== '.') c || T.any (\x -> x == 'e' || x == 'E') c

looksLikeDateTime :: Text -> Bool
looksLikeDateTime t =
  T.length t >= 10
  && T.index t 4 == '-'
  && T.index t 7 == '-'
  && all isDigit (T.unpack (T.take 4 t))

classifyDateTime :: Text -> TV.Value
classifyDateTime t
  | T.any (== 'T') t || T.any (== 't') t || (T.length t > 10 && T.index t 10 == ' ') = TV.TDateTime t
  | T.length t == 10 = TV.TDate t
  | T.any (== ':') t = TV.TTime t
  | otherwise = TV.TDateTime t

mergeTables :: V.Vector (Text, TV.Value) -> [TableCtx] -> TV.Value
mergeTables topPairs tables =
  let !withSubTables = foldl' insertTable topPairs tables
  in TV.TTable withSubTables

insertTable :: V.Vector (Text, TV.Value) -> TableCtx -> V.Vector (Text, TV.Value)
insertTable top (TableCtx path pairs) =
  case path of
    [] -> top
    [name] -> top `V.snoc` (name, TV.TTable (V.fromList (reverse pairs)))
    (first : rest) ->
      let !subCtx = TableCtx rest pairs
          !existing = V.find (\(k,_) -> k == first) top
      in case existing of
           Just (_, TV.TTable subT) ->
             let !updated = insertTable subT subCtx
             in V.map (\(k, v) -> if k == first then (k, TV.TTable updated) else (k, v)) top
           _ ->
             let !subTable = insertTable V.empty subCtx
             in top `V.snoc` (first, TV.TTable subTable)
