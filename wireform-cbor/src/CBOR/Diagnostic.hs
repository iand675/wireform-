{-# LANGUAGE BangPatterns #-}

{- | CBOR diagnostic notation (RFC 8949 Section 8).

Human-readable text representation of CBOR values. Useful for
debugging, test fixtures, and protocol documentation.

Examples:

@
toDiagnostic (UInt 42)          == "42"
toDiagnostic (NInt 0)           == "-1"
toDiagnostic (TextString "hi")  == "\"hi\""
toDiagnostic (Array [UInt 1])   == "[1]"
toDiagnostic (Tag 0 (TextString "2013-03-21T20:04:00Z"))
  == "0(\"2013-03-21T20:04:00Z\")"
@
-}
module CBOR.Diagnostic (
  toDiagnostic,
  fromDiagnostic,
) where

import CBOR.Internal (integerValue)
import CBOR.Value qualified as C
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Char (chr, digitToInt, isDigit, isHexDigit, isSpace, ord)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word (Word64)


------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------

toDiagnostic :: C.Value -> Text
toDiagnostic = renderValue


renderValue :: C.Value -> Text
renderValue (C.UInt n) = T.pack (show n)
renderValue (C.NInt n) = T.pack (show (negate (fromIntegral n :: Integer) - 1))
renderValue (C.Bool True) = "true"
renderValue (C.Bool False) = "false"
renderValue C.Null = "null"
renderValue C.Undefined = "undefined"
renderValue (C.Float16 f) = renderFloat (realToFrac f :: Double)
renderValue (C.Float32 f) = renderFloat (realToFrac f :: Double)
renderValue (C.Float64 d) = renderFloat d
renderValue (C.ByteString b) = "h'" <> bytesToHex b <> "'"
renderValue (C.TextString t) = TE.decodeUtf8 (BSL.toStrict (Aeson.encode t))
renderValue (C.Array vec) = "[" <> renderItems vec <> "]"
renderValue (C.Map vec) = "{" <> renderPairs vec <> "}"
renderValue (C.Tag n v) = T.pack (show n) <> "(" <> renderValue v <> ")"
renderValue (C.Simple n) = "simple(" <> T.pack (show n) <> ")"


renderFloat :: Double -> Text
renderFloat d
  | isNaN d = "NaN"
  | isInfinite d = if d > 0 then "Infinity" else "-Infinity"
  | otherwise =
      let s = show d
      in T.pack $
           if '.' `elem` s || 'e' `elem` s || 'E' `elem` s
             then s
             else s ++ ".0"


renderItems :: V.Vector C.Value -> Text
renderItems vec
  | V.null vec = ""
  | otherwise = T.intercalate ", " (V.toList (V.map renderValue vec))


renderPairs :: V.Vector (C.Value, C.Value) -> Text
renderPairs vec
  | V.null vec = ""
  | otherwise = T.intercalate ", " (V.toList (V.map renderPair vec))
  where
    renderPair (k, v) = renderValue k <> ": " <> renderValue v


bytesToHex :: ByteString -> Text
bytesToHex = T.pack . concatMap byteToHex . BS.unpack
  where
    byteToHex b = [hexChar (b `div` 16), hexChar (b `mod` 16)]
    hexChar n
      | n < 10 = chr (ord '0' + fromIntegral n)
      | otherwise = chr (ord 'a' + fromIntegral n - 10)


------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------

type Parser a = String -> Either String (a, String)


fromDiagnostic :: Text -> Either String C.Value
fromDiagnostic t =
  case parseValue (T.unpack t) of
    Left err -> Left err
    Right (val, rest)
      | all isSpace rest -> Right val
      | otherwise -> Left $ "CBOR.Diagnostic: trailing characters: " ++ take 20 rest


parseValue :: Parser C.Value
parseValue input = case dropWhile isSpace input of
  [] -> Left "CBOR.Diagnostic: unexpected end of input"
  '"' : rest -> parseTextString rest
  'h' : '\'' : rest -> parseByteString rest
  't' : 'r' : 'u' : 'e' : rest
    | notIdentCont rest -> Right (C.Bool True, rest)
  'f' : 'a' : 'l' : 's' : 'e' : rest
    | notIdentCont rest -> Right (C.Bool False, rest)
  'n' : 'u' : 'l' : 'l' : rest
    | notIdentCont rest -> Right (C.Null, rest)
  'u' : 'n' : 'd' : 'e' : 'f' : 'i' : 'n' : 'e' : 'd' : rest
    | notIdentCont rest -> Right (C.Undefined, rest)
  'N' : 'a' : 'N' : rest
    | notIdentCont rest -> Right (C.Float64 (0 / 0), rest)
  'I' : 'n' : 'f' : 'i' : 'n' : 'i' : 't' : 'y' : rest
    | notIdentCont rest -> Right (C.Float64 (1 / 0), rest)
  '-' : 'I' : 'n' : 'f' : 'i' : 'n' : 'i' : 't' : 'y' : rest
    | notIdentCont rest -> Right (C.Float64 ((-1) / 0), rest)
  's' : 'i' : 'm' : 'p' : 'l' : 'e' : '(' : rest -> parseSimple rest
  '[' : rest -> parseArray rest
  '{' : rest -> parseMap rest
  '-' : rest -> parseNegNumber rest
  s@(c : _) | isDigit c -> parseNumberOrTag s
  other -> Left $ "CBOR.Diagnostic: unexpected input: " ++ take 20 other


notIdentCont :: String -> Bool
notIdentCont [] = True
notIdentCont (c : _) = not (isDigit c || c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))


parseTextString :: Parser C.Value
parseTextString = go ['"']
  where
    go _ [] = Left "CBOR.Diagnostic: unterminated string"
    go acc ('"' : rest) =
      case Aeson.eitherDecodeStrict' (TE.encodeUtf8 (T.pack (reverse ('"' : acc)))) of
        Left err -> Left ("CBOR.Diagnostic: invalid text string: " ++ err)
        Right value -> Right (C.TextString value, rest)
    -- Leave escape and Unicode validation to the JSON string decoder.
    go acc ('\\' : c : rest) = go (c : '\\' : acc) rest
    go acc (c : rest) = go (c : acc) rest


parseByteString :: Parser C.Value
parseByteString = go []
  where
    go acc [] = Left "CBOR.Diagnostic: unterminated byte string"
    go acc ('\'' : rest) = Right (C.ByteString (BS.pack (reverse acc)), rest)
    go acc (c1 : c2 : rest)
      | isHexDigit c1 && isHexDigit c2 =
          let !b = fromIntegral (digitToInt c1 * 16 + digitToInt c2)
          in go (b : acc) rest
    go acc (c : rest)
      | isSpace c = go acc rest
      | otherwise = Left $ "CBOR.Diagnostic: invalid hex character: " ++ [c]


parseSimple :: Parser C.Value
parseSimple input = case span isDigit input of
  ([], _) -> Left "CBOR.Diagnostic: expected number after simple("
  (digits, ')' : rest) ->
    case read digits :: Integer of
      20 -> Right (C.Bool False, rest)
      21 -> Right (C.Bool True, rest)
      22 -> Right (C.Null, rest)
      23 -> Right (C.Undefined, rest)
      n
        | n >= 24 && n < 32 -> Left "CBOR.Diagnostic: reserved simple value"
        | n > 255 -> Left "CBOR.Diagnostic: simple value out of range (0-255)"
        | otherwise -> Right (C.Simple (fromIntegral n), rest)
  _ -> Left "CBOR.Diagnostic: expected ')' after simple value"


parseArray :: Parser C.Value
parseArray input = case dropWhile isSpace input of
  ']' : rest -> Right (C.Array V.empty, rest)
  _ -> parseArrayItems [] input
  where
    parseArrayItems acc s = do
      (val, rest1) <- parseValue s
      case dropWhile isSpace rest1 of
        ',' : rest2 -> parseArrayItems (val : acc) rest2
        ']' : rest2 -> Right (C.Array (V.fromList (reverse (val : acc))), rest2)
        _ -> Left "CBOR.Diagnostic: expected ',' or ']' in array"


parseMap :: Parser C.Value
parseMap input = case dropWhile isSpace input of
  '}' : rest -> Right (C.Map V.empty, rest)
  _ -> parseMapItems [] input
  where
    parseMapItems acc s = do
      (key, rest1) <- parseValue s
      case dropWhile isSpace rest1 of
        ':' : rest2 -> do
          (val, rest3) <- parseValue rest2
          case dropWhile isSpace rest3 of
            ',' : rest4 -> parseMapItems ((key, val) : acc) rest4
            '}' : rest4 -> Right (C.Map (V.fromList (reverse ((key, val) : acc))), rest4)
            _ -> Left "CBOR.Diagnostic: expected ',' or '}' in map"
        _ -> Left "CBOR.Diagnostic: expected ':' in map"


parseNegNumber :: Parser C.Value
parseNegNumber input = case span isDigit input of
  ([], _) -> Left "CBOR.Diagnostic: expected digit after '-'"
  (digits, rest) ->
    case rest of
      '.' : _ -> parseNegFloat digits rest
      'e' : _ -> parseNegFloat digits rest
      'E' : _ -> parseNegFloat digits rest
      _ ->
        let n = read digits :: Integer
        in if n == 0
             then Left "CBOR.Diagnostic: negative integer -0 not valid in CBOR"
             else Right (integerValue (negate n), rest)


parseNegFloat :: String -> String -> Either String (C.Value, String)
parseNegFloat intPart rest =
  let (fracAndExp, rest2) = spanFloat rest
      fullStr = "-" ++ intPart ++ fracAndExp
  in case reads fullStr :: [(Double, String)] of
       [(d, "")] -> Right (C.Float64 d, rest2)
       _ -> Left $ "CBOR.Diagnostic: invalid float: " ++ fullStr


parseNumberOrTag :: Parser C.Value
parseNumberOrTag input =
  let (digits, rest) = span isDigit input
  in case rest of
       '(' : rest2 -> do
         let tagNum = read digits :: Integer
         if tagNum > toInteger (maxBound :: Word64)
           then Left "CBOR.Diagnostic: tag number out of range"
           else do
             (val, rest3) <- parseValue rest2
             case dropWhile isSpace rest3 of
               ')' : rest4 -> Right (C.Tag (fromInteger tagNum) val, rest4)
               _ -> Left "CBOR.Diagnostic: expected ')' after tag content"
       '.' : _ -> parsePosFloat digits rest
       'e' : _ -> parsePosFloat digits rest
       'E' : _ -> parsePosFloat digits rest
       _ -> Right (integerValue (read digits), rest)


parsePosFloat :: String -> String -> Either String (C.Value, String)
parsePosFloat intPart rest =
  let (fracAndExp, rest2) = spanFloat rest
      fullStr = intPart ++ fracAndExp
  in case reads fullStr :: [(Double, String)] of
       [(d, "")] -> Right (C.Float64 d, rest2)
       _ -> Left $ "CBOR.Diagnostic: invalid float: " ++ fullStr


spanFloat :: String -> (String, String)
spanFloat s =
  let (frac, s1) = case s of
        '.' : rest -> let (ds, r) = span isDigit rest in ('.' : ds, r)
        _ -> ("", s)
      (ex, s2) = case s1 of
        e : rest | e == 'e' || e == 'E' ->
          case rest of
            '+' : rest2 -> let (ds, r) = span isDigit rest2 in (e : '+' : ds, r)
            '-' : rest2 -> let (ds, r) = span isDigit rest2 in (e : '-' : ds, r)
            _ -> let (ds, r) = span isDigit rest in (e : ds, r)
        _ -> ("", s1)
  in (frac ++ ex, s2)
