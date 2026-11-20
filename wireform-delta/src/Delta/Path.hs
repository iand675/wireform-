-- | Resolve local filesystem paths from URI-encoded Delta log entries.
module Delta.Path (
  dataFilePath,
  filePathKey,
  storedPathKeys,
  localAbsolutePath,
  safeRelativePath,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Array qualified as TA
import Data.Text.Encoding qualified as TE
import Data.Text.Internal qualified as TI
import Data.Word (Word8)
import Foreign.Storable (pokeByteOff)
import System.FilePath (hasDrive, pathSeparator, (</>))


-- Canonicalize escape spelling for reconciliation. Decode only RFC 2396
-- unreserved characters, and encode literal spaces from legacy writers.
-- Reserved delimiters and escaped percent signs keep their URI meaning.
filePathKey :: Text -> Text
filePathKey path@(TI.Text bytes start size) = go start
  where
    end = start + size
    go !index
      | index == end = path
      | byte == 0x20 = normalizeEscapes path index
      | byte == 0x25 =
          case readEscape bytes end index of
            Just escaped
              | uriUnreserved escaped || lowerHexAt (index + 1) || lowerHexAt (index + 2) ->
                  normalizeEscapes path index
              | otherwise -> go (index + 3)
            Nothing -> path
      | otherwise = go (index + 1)
      where
        byte = TA.unsafeIndex bytes index
    lowerHexAt index = TA.unsafeIndex bytes index - 0x61 <= 5
{-# INLINE filePathKey #-}


normalizeEscapes :: Text -> Int -> Text
normalizeEscapes path@(TI.Text bytes start size) first
  | spaces > (maxBound - size) `quot` 2 = path
  | otherwise =
      let (result, lengthOrInvalid) = TA.run2 $ do
            out <- TA.new (size + 2 * spaces)
            TA.copyI prefix out 0 bytes start
            let emit !i !j !value = TA.unsafeWrite out j value >> go i (j + 1)
                writeEscape !i !j !value = do
                  TA.unsafeWrite out j 0x25
                  TA.unsafeWrite out (j + 1) (upperHex (value `quot` 16))
                  emit i (j + 2) (upperHex (value `rem` 16))
                go !i !j
                  | i == end = pure (out, j)
                  | byte == 0x20 = writeEscape (i + 1) j byte
                  | byte == 0x25 = case readEscape bytes end i of
                      Nothing -> pure (out, -1)
                      Just escaped
                        | uriUnreserved escaped -> emit (i + 3) j escaped
                        | otherwise -> writeEscape (i + 3) j escaped
                  | otherwise = emit (i + 1) j byte
                  where
                    byte = TA.unsafeIndex bytes i
            go first prefix
      in if lengthOrInvalid < 0 then path else TI.Text result 0 lengthOrInvalid
  where
    end = start + size
    prefix = first - start
    spaces = T.count " " (TI.Text bytes first (end - first))
    upperHex n = if n < 10 then n + 0x30 else n + 0x37
{-# NOINLINE normalizeEscapes #-}


readEscape :: TA.Array -> Int -> Int -> Maybe Word8
readEscape bytes end index
  | end - index >= 3
  , Just hi <- hex (TA.unsafeIndex bytes (index + 1))
  , Just lo <- hex (TA.unsafeIndex bytes (index + 2)) = Just (16 * hi + lo)
  | otherwise = Nothing
{-# INLINE readEscape #-}


uriUnreserved :: Word8 -> Bool
uriUnreserved byte =
  byte - 0x30 <= 9 || byte - 0x41 <= 25 || byte - 0x61 <= 25
    || case byte of
      0x21 -> True -- !
      0x27 -> True -- '
      0x28 -> True -- (
      0x29 -> True -- )
      0x2a -> True -- *
      0x2d -> True -- -
      0x2e -> True -- .
      0x5f -> True -- _
      0x7e -> True -- ~
      _ -> False
{-# INLINE uriUnreserved #-}


storedPathKeys :: (a -> Text) -> Map.Map Text a -> Map.Map Text a
storedPathKeys stored files = Map.foldlWithKey' restore files files
  where
    restore result key entry
      | path == key = result
      | otherwise = Map.insert path entry (Map.delete key result)
      where
        path = stored entry


dataFilePath :: FilePath -> Text -> Either String FilePath
dataFilePath root uri
  | T.isPrefixOf "/" uri || T.any (== ':') (T.takeWhile (/= '/') uri) = localAbsolutePath uri
  | otherwise = (root </>) <$> safeRelativePath uri


localAbsolutePath :: Text -> Either String FilePath
localAbsolutePath uri
  | T.isPrefixOf "/" uri = absolute uri
  | T.toLower (T.take 5 uri) == "file:" = case T.stripPrefix "//" rest of
      Nothing -> absolute rest
      Just authorityAndPath ->
        let (authority, path) = T.breakOn "/" authorityAndPath
        in if T.null authority || T.toLower authority == "localhost" then absolute path else invalid
  | otherwise = invalid
  where
    rest = T.drop 5 uri
    absolute path
      | T.isPrefixOf "/" path = do
          decoded <- decodeUriPath path
          if T.isPrefixOf "//" decoded then invalid else pure (T.unpack decoded)
      | otherwise = invalid
    invalid = Left "Delta.Path: expected a local absolute path or file URI"


safeRelativePath :: Text -> Either String FilePath
safeRelativePath uri = do
  path <- decodeUriPath uri
  let segments = T.splitOn "/" path
      filePath = T.unpack path
  if (pathSeparator /= '\\' || not (hasDrive filePath)) && all validSegment segments && any (/= ".") segments
    then pure filePath
    else Left "Delta.Path: invalid relative path"
  where
    validSegment part = not (T.null part) && part /= ".."


decodeUriPath :: Text -> Either String Text
decodeUriPath uri = case T.findIndex (\c -> c == '%' || invalidRaw c) uri of
  Nothing -> Right uri
  Just first
    | T.index uri first == '%'
    , not (T.any invalidRaw (T.drop (first + 1) uri)) ->
      case decodeEscapes (TE.encodeUtf8 uri) of
        (_, False) -> Left "Delta.Path: invalid percent escape"
        (bytes, True) -> case TE.decodeUtf8' bytes of
          Right path | not (T.any invalidDecoded path) -> Right path
          Right _ -> invalid
          Left _ -> invalid
    | otherwise -> invalid
  where
    invalidRaw c = c == '\0' || c == '\\' || c == '?' || c == '#'
    -- A backslash decoded from %5C is an ordinary filename character
    -- on POSIX and a path separator on Windows.
    invalidDecoded c = c == '\0' || (pathSeparator == '\\' && c == '\\')
    invalid = Left "Delta.Path: invalid path"


-- A percent escape consumes three bytes and emits one, so the encoded
-- length bounds the allocation. Only complete escapes read past the '%'.
decodeEscapes :: BS.ByteString -> (BS.ByteString, Bool)
decodeEscapes bytes = BSI.unsafeCreateUptoN' size $ \out ->
  let go !i !j
        | i == size = pure (j, True)
        | byte /= 37 = pokeByteOff out j byte >> go (i + 1) (j + 1)
        | size - i >= 3
        , Just hi <- hex (BSU.unsafeIndex bytes (i + 1))
        , Just lo <- hex (BSU.unsafeIndex bytes (i + 2)) =
            pokeByteOff out j (16 * hi + lo) >> go (i + 3) (j + 1)
        | otherwise = pure (0, False)
        where
          byte = BSU.unsafeIndex bytes i
  in go 0 0
  where
    size = BS.length bytes


hex :: Word8 -> Maybe Word8
hex c
  | c - 0x30 <= 9 = Just (c - 0x30)
  | c - 0x41 <= 5 = Just (c - 0x37)
  | c - 0x61 <= 5 = Just (c - 0x57)
  | otherwise = Nothing
{-# INLINE hex #-}
