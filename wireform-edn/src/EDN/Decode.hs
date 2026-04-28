{-# LANGUAGE BangPatterns #-}
-- | EDN (Extensible Data Notation) text decoding via recursive descent.
--
-- Parses EDN text into an 'EDN.Value.Value'. Handles all EDN types:
-- nil, booleans, integers, floats, strings, characters, keywords,
-- symbols, lists, vectors, maps, sets, tagged literals, and the
-- @#_@ discard reader macro. Whitespace includes commas per the EDN spec.
--
-- The ByteString-based parser uses SIMD-accelerated whitespace skipping
-- via 'Proto.Wire.FFI.skipWhitespaceBS'.
module EDN.Decode
  ( decode
  , decodeBS
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Char (isDigit, digitToInt, chr, ord)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word8)

import qualified EDN.Value as E
import Wireform.FFI (skipWhitespaceBS)

-- | Parse an EDN value from 'Text'.
decode :: Text -> Either String E.Value
decode !input = decodeBS (TE.encodeUtf8 input)

-- | Parse an EDN value from a UTF-8 'ByteString'.
decodeBS :: ByteString -> Either String E.Value
decodeBS !bs =
  let !off0 = skipWS bs 0
  in case parseValue bs off0 of
       Left err -> Left err
       Right (val, off1) ->
         let !off2 = skipWS bs off1
         in if off2 >= BS.length bs
              then Right val
              else Left "EDN.Decode: trailing characters"

type Parser a = ByteString -> Int -> Either String (a, Int)

skipWS :: ByteString -> Int -> Int
skipWS !bs !off = skipWhitespaceBS bs off
{-# INLINE skipWS #-}

peekByte :: ByteString -> Int -> Maybe Word8
peekByte !bs !off
  | off >= BS.length bs = Nothing
  | otherwise = Just (BSU.unsafeIndex bs off)
{-# INLINE peekByte #-}

getByte :: ByteString -> Int -> Maybe (Word8, Int)
getByte !bs !off
  | off >= BS.length bs = Nothing
  | otherwise = Just (BSU.unsafeIndex bs off, off + 1)
{-# INLINE getByte #-}

parseValue :: Parser E.Value
parseValue bs off
  | off >= BS.length bs = Left "EDN.Decode: unexpected end of input"
  | otherwise =
    let !b = BSU.unsafeIndex bs off
    in case b of
      0x6E -> parseNilOrSymbol bs off      -- 'n'
      0x74 -> parseTrueOrSymbol bs off     -- 't'
      0x66 -> parseFalseOrSymbol bs off    -- 'f'
      0x22 -> parseString bs (off + 1)     -- '"'
      0x5C -> parseCharLiteral bs (off + 1) -- '\'
      0x3A -> parseKeyword bs (off + 1)    -- ':'
      0x28 -> parseList bs (off + 1)       -- '('
      0x5B -> parseVector bs (off + 1)     -- '['
      0x7B -> parseMap bs (off + 1)        -- '{'
      0x23 -> parseHash bs (off + 1)       -- '#'
      0x2D -> parseNegativeOrSymbol bs off  -- minus
      0x2B -> parsePlusOrSymbol bs off  -- plus
      _ | b >= 0x30 && b <= 0x39 -> parseNumber bs off
        | isSymbolStartB b -> parseSymbol bs off
        | otherwise -> Left $ "EDN.Decode: unexpected character: " ++ show (chr (fromIntegral b))

spanToken :: ByteString -> Int -> (String, Int)
spanToken !bs !off = go off
  where
    !len = BS.length bs
    go !i
      | i >= len = (decodeRange bs off i, i)
      | isTokenCharB (BSU.unsafeIndex bs i) = go (i + 1)
      | otherwise = (decodeRange bs off i, i)

decodeRange :: ByteString -> Int -> Int -> String
decodeRange !bs !start !end =
  T.unpack (TE.decodeUtf8Lenient (BSU.unsafeTake (end - start) (BSU.unsafeDrop start bs)))

decodeRangeText :: ByteString -> Int -> Int -> Text
decodeRangeText !bs !start !end =
  TE.decodeUtf8Lenient (BSU.unsafeTake (end - start) (BSU.unsafeDrop start bs))

parseNilOrSymbol :: Parser E.Value
parseNilOrSymbol bs off =
  let (tok, rest) = spanToken bs off
  in if tok == "nil"
     then Right (E.Nil, rest)
     else Right (toSymbol tok, rest)

parseTrueOrSymbol :: Parser E.Value
parseTrueOrSymbol bs off =
  let (tok, rest) = spanToken bs off
  in if tok == "true"
     then Right (E.Bool True, rest)
     else Right (toSymbol tok, rest)

parseFalseOrSymbol :: Parser E.Value
parseFalseOrSymbol bs off =
  let (tok, rest) = spanToken bs off
  in if tok == "false"
     then Right (E.Bool False, rest)
     else Right (toSymbol tok, rest)

parseNegativeOrSymbol :: Parser E.Value
parseNegativeOrSymbol bs off
  | off + 1 < BS.length bs
  , let !b = BSU.unsafeIndex bs (off + 1)
  , b >= 0x30 && b <= 0x39
  = parseNumber bs off
parseNegativeOrSymbol bs off = parseSymbol bs off

parsePlusOrSymbol :: Parser E.Value
parsePlusOrSymbol bs off
  | off + 1 < BS.length bs
  , let !b = BSU.unsafeIndex bs (off + 1)
  , b >= 0x30 && b <= 0x39
  = parseNumber bs off
parsePlusOrSymbol bs off = parseSymbol bs off

parseNumber :: Parser E.Value
parseNumber bs off =
  let (tok, rest) = spanToken bs off
  in case parseNumberToken tok of
       Left err -> Left err
       Right val -> Right (val, rest)

parseNumberToken :: String -> Either String E.Value
parseNumberToken tok =
  case break (\c -> c == '.' || c == 'e' || c == 'E') tok of
    (_, []) ->
      case tok of
        ('+':digits) -> Right (E.Integer (read digits))
        _            -> Right (E.Integer (read tok))
    _ ->
      let tokClean = case tok of { ('+':r) -> r; _ -> tok }
      in case reads tokClean :: [(Double, String)] of
           [(d, "")] -> Right (E.Float d)
           [(d, "M")] -> Right (E.Float d)
           _ -> Left $ "EDN.Decode: invalid number: " ++ tok

parseString :: Parser E.Value
parseString bs = go [] where
  !len = BS.length bs
  go !acc !off
    | off >= len = Left "EDN.Decode: unterminated string"
    | otherwise =
      let !b = BSU.unsafeIndex bs off
      in case b of
        0x22 -> Right (E.String (T.pack (reverse acc)), off + 1)  -- '"'
        0x5C  -- '\'
          | off + 1 >= len -> Left "EDN.Decode: unterminated string escape"
          | otherwise ->
            let !c = BSU.unsafeIndex bs (off + 1)
            in case c of
              0x6E -> go ('\n':acc) (off + 2)      -- 'n'
              0x74 -> go ('\t':acc) (off + 2)      -- 't'
              0x72 -> go ('\r':acc) (off + 2)      -- 'r'
              0x22 -> go ('"':acc) (off + 2)       -- '"'
              0x5C -> go ('\\':acc) (off + 2)      -- '\'
              0x75 -> case parseUnicodeEscape bs (off + 2) of  -- 'u'
                                Left err -> Left err
                                Right (ch, off') -> go (ch:acc) off'
              _ -> Left $ "EDN.Decode: invalid string escape: \\" ++ [chr (fromIntegral c)]
        _ -> go (chr (fromIntegral b) : acc) (off + 1)

parseUnicodeEscape :: ByteString -> Int -> Either String (Char, Int)
parseUnicodeEscape bs off
  | off + 4 > BS.length bs = Left "EDN.Decode: incomplete unicode escape"
  | otherwise =
    let hex = map (chr . fromIntegral . BSU.unsafeIndex bs) [off, off+1, off+2, off+3]
    in if all isHexDigitC hex
         then Right (chr (foldl (\a d -> a * 16 + digitToInt d) 0 hex), off + 4)
         else Left "EDN.Decode: invalid unicode escape"

parseCharLiteral :: Parser E.Value
parseCharLiteral bs off
  | off >= BS.length bs = Left "EDN.Decode: unexpected end of input after \\"
  | otherwise =
    let (tok, rest) = spanCharToken bs off
    in case tok of
         "newline" -> Right (E.Char '\n', rest)
         "return"  -> Right (E.Char '\r', rest)
         "space"   -> Right (E.Char ' ', rest)
         "tab"     -> Right (E.Char '\t', rest)
         ['u', a, b, c, d] | all isHexDigitC [a,b,c,d] ->
           Right (E.Char (chr (foldl (\acc x -> acc * 16 + digitToInt x) 0 [a,b,c,d])), rest)
         [c] -> Right (E.Char c, rest)
         _   -> Left $ "EDN.Decode: invalid character literal: \\" ++ tok

isHexDigitC :: Char -> Bool
isHexDigitC c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

spanCharToken :: ByteString -> Int -> (String, Int)
spanCharToken bs off
  | off >= BS.length bs = ([], off)
  | otherwise =
    let !b = BSU.unsafeIndex bs off
        !c = chr (fromIntegral b)
    in if isAlphaNum c
         then let (rest, end) = spanAlphaNum bs (off + 1)
              in (c : rest, end)
         else ([c], off + 1)
  where
    isAlphaNum x = (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') || isDigit x

spanAlphaNum :: ByteString -> Int -> (String, Int)
spanAlphaNum !bs = go
  where
    !len = BS.length bs
    go !i
      | i >= len = ([], i)
      | otherwise =
        let !b = BSU.unsafeIndex bs i
            !c = chr (fromIntegral b)
        in if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || isDigit c
             then let (rest, end) = go (i + 1)
                  in (c : rest, end)
             else ([], i)

parseKeyword :: Parser E.Value
parseKeyword bs off =
  let (tok, rest) = spanToken bs off
  in case break (== '/') tok of
       (_, [])     -> Right (E.Keyword Nothing (T.pack tok), rest)
       (ns, '/':n) -> Right (E.Keyword (Just (T.pack ns)) (T.pack n), rest)
       _           -> Left $ "EDN.Decode: invalid keyword: :" ++ tok

parseSymbol :: Parser E.Value
parseSymbol bs off =
  let (tok, rest) = spanToken bs off
  in Right (toSymbol tok, rest)

toSymbol :: String -> E.Value
toSymbol tok =
  case break (== '/') tok of
    (_, [])     -> E.Symbol Nothing (T.pack tok)
    (_, "/")    -> E.Symbol Nothing (T.pack tok)
    (ns, '/':n) -> E.Symbol (Just (T.pack ns)) (T.pack n)
    _           -> E.Symbol Nothing (T.pack tok)

parseList :: Parser E.Value
parseList = parseCollection 0x29 E.List  -- ')'

parseVector :: Parser E.Value
parseVector = parseCollection 0x5D E.Vector  -- ']'

parseCollection :: Word8 -> (V.Vector E.Value -> E.Value) -> Parser E.Value
parseCollection close ctor = go []
  where
    go !acc bs !off =
      let !off' = skipWS bs off
      in if off' >= BS.length bs
           then Left $ "EDN.Decode: unterminated collection, expected " ++ show (chr (fromIntegral close))
           else let !b = BSU.unsafeIndex bs off'
                in if b == close
                     then Right (ctor (V.fromList (reverse acc)), off' + 1)
                     -- @#_ X@ at the start of a position drops X and
                     -- continues without contributing to the collection.
                     else if b == 0x23
                            && off' + 1 < BS.length bs
                            && BSU.unsafeIndex bs (off' + 1) == 0x5F
                            then case skipOneDiscard bs (off' + 2) of
                                   Left e   -> Left e
                                   Right o2 -> go acc bs o2
                            else do
                              (val, off'') <- parseValue bs off'
                              go (val:acc) bs off''

-- | Consume @#_@-followed-by-value-to-discard, returning the offset
-- after the discarded value. Used by collection parsers so a leading
-- or interior discard form doesn't yield a value to the container.
skipOneDiscard :: ByteString -> Int -> Either String Int
skipOneDiscard bs off =
  let !off' = skipWS bs off
  in if off' >= BS.length bs
       then Left "EDN.Decode: #_ with no value to drop"
       else do
         (_, off'') <- parseValue bs off'
         pure off''

parseMap :: Parser E.Value
parseMap = go []
  where
    go !acc bs !off =
      let !off' = skipWS bs off
      in if off' >= BS.length bs
           then Left "EDN.Decode: unterminated map"
           else if BSU.unsafeIndex bs off' == 0x7D  -- '}'
                  then Right (E.Map (V.fromList (reverse acc)), off' + 1)
                  -- Allow @#_ K@ or @#_ V@ before either the key or
                  -- value slot; the discard is transparent and
                  -- doesn't shift the alignment of the remaining
                  -- key/value pairs.
                  else if BSU.unsafeIndex bs off' == 0x23
                       && off' + 1 < BS.length bs
                       && BSU.unsafeIndex bs (off' + 1) == 0x5F
                       then case skipOneDiscard bs (off' + 2) of
                              Left e   -> Left e
                              Right o2 -> go acc bs o2
                       else do
                         (k, off'')   <- parseValue bs off'
                         (v, off'''') <- parseMapValueSlot bs off''
                         go ((k, v) : acc) bs off''''

-- | Parse the value slot of a map, transparently consuming any
-- leading @#_ X@ discard forms.
parseMapValueSlot :: Parser E.Value
parseMapValueSlot bs off =
  let !off' = skipWS bs off
  in if off' >= BS.length bs
       then Left "EDN.Decode: unterminated map, missing value"
       else if BSU.unsafeIndex bs off' == 0x23
            && off' + 1 < BS.length bs
            && BSU.unsafeIndex bs (off' + 1) == 0x5F
            then case skipOneDiscard bs (off' + 2) of
                   Left e   -> Left e
                   Right o2 -> parseMapValueSlot bs o2
            else parseValue bs off'

parseHash :: Parser E.Value
parseHash bs off
  | off >= BS.length bs = Left "EDN.Decode: unexpected end of input after #"
  | otherwise =
    let !b = BSU.unsafeIndex bs off
    in case b of
      0x7B -> parseSet bs (off + 1)      -- '{'
      0x5F -> parseDiscard bs (off + 1) -- '_'
      0x23 -> parseDispatch bs (off + 1) -- '#'
      _            -> parseTagged bs off

parseSet :: Parser E.Value
parseSet = go []
  where
    go !acc bs !off =
      let !off' = skipWS bs off
      in if off' >= BS.length bs
           then Left "EDN.Decode: unterminated set"
           else if BSU.unsafeIndex bs off' == 0x7D  -- '}'
                  then Right (E.Set (V.fromList (reverse acc)), off' + 1)
                  else if BSU.unsafeIndex bs off' == 0x23
                       && off' + 1 < BS.length bs
                       && BSU.unsafeIndex bs (off' + 1) == 0x5F
                       then case skipOneDiscard bs (off' + 2) of
                              Left e   -> Left e
                              Right o2 -> go acc bs o2
                       else do
                         (val, off'') <- parseValue bs off'
                         go (val:acc) bs off''

-- | Consume a @#_@ discard form at value position. The collection /
-- map parsers handle their own discard skipping so that @#_@ at the
-- start of a slot doesn't yield a value to the container; this
-- handler is reached only from 'parseValue' (top-level or as part
-- of a tagged value), where the caller does expect a real value
-- after one or more discards.
parseDiscard :: Parser E.Value
parseDiscard bs off =
  let !off' = skipWS bs off
  in if off' >= BS.length bs
       then Left "EDN.Decode: unexpected end of input after #_"
       else do
         (_, off'') <- parseValue bs off'
         let !off''' = skipWS bs off''
         parseValue bs off'''

parseDispatch :: Parser E.Value
parseDispatch bs off =
  let (tok, rest) = spanToken bs off
  in case tok of
       "Inf"  -> Right (E.Float (1/0), rest)
       "-Inf" -> Right (E.Float (-1/0), rest)
       "NaN"  -> Right (E.Float (0/0), rest)
       _      -> Left $ "EDN.Decode: unknown dispatch: ##" ++ tok

parseTagged :: Parser E.Value
parseTagged bs off =
  let (tok, rest) = spanToken bs off
  in case break (== '/') tok of
       (name, []) -> do
         let !off' = skipWS bs rest
         (val, off'') <- parseValue bs off'
         Right (E.Tagged (T.pack "") (T.pack name) val, off'')
       (ns, '/':name) -> do
         let !off' = skipWS bs rest
         (val, off'') <- parseValue bs off'
         Right (E.Tagged (T.pack ns) (T.pack name) val, off'')
       _ -> Left $ "EDN.Decode: invalid tag: #" ++ tok

isTokenCharB :: Word8 -> Bool
isTokenCharB b =
  b /= 0x20 && b /= 0x09 && b /= 0x0A && b /= 0x0D  -- whitespace
  && b /= 0x2C  -- comma
  && b /= 0x22  -- "
  && b /= 0x3B  -- ;
  && b /= 0x28 && b /= 0x29  -- ()
  && b /= 0x5B && b /= 0x5D  -- []
  && b /= 0x7B && b /= 0x7D  -- {}
  && b /= 0x5C  -- backslash
  && b /= 0x23  -- #
{-# INLINE isTokenCharB #-}

isSymbolStartB :: Word8 -> Bool
isSymbolStartB b =
  (b >= 0x61 && b <= 0x7A) || (b >= 0x41 && b <= 0x5A)  -- a-z, A-Z
  || b == 0x2E || b == 0x2A || b == 0x21 || b == 0x5F  -- . * ! _
  || b == 0x3F || b == 0x24 || b == 0x25 || b == 0x26  -- ? $ % &
  || b == 0x3D || b == 0x3C || b == 0x3E || b == 0x2F  -- = < > /
  || b == 0x2B || b == 0x2D  -- + -
  || b > 0x7F  -- non-ASCII
{-# INLINE isSymbolStartB #-}
