{-# LANGUAGE OverloadedStrings #-}

-- | Connect content-coding negotiation and (de)compression.
--
-- Connect supports @identity@, @gzip@, @br@ (Brotli), and @zstd@. These
-- differ from grpc-spec's compression set (gzip\/deflate\/snappy), so a
-- focused local enum is used rather than bending grpc-spec's
-- 'Network.GRPC.Spec.Compression'.
module Network.Connect.Compression (
  -- * Codings
  ContentCoding (..),
  codingName,
  codingFromName,
  parseUnaryContentCoding,
  parseStreamingContentCoding,

  -- * (De)compression
  compress,
  decompress,

  -- * Negotiation
  negotiate,
  negotiateStreamingResponseCoding,
  negotiateUnaryResponseCoding,
  parseAcceptEncoding,
  renderAcceptEncoding,
  renderCodingList,
) where
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)

import Control.Exception (SomeException, evaluate, try)
import Data.Bits ((.|.), bit, setBit, testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Codec.Compression.Brotli qualified as Brotli
import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Zstd qualified as Zstd
import Codec.Compression.Zstd.Lazy qualified as ZstdL
import Network.Connect.Protocol (hConnectContentEncoding)
import Network.HTTP.ContentCoding qualified as HttpCoding
import Network.HTTP.Headers qualified as HttpHeader
import Network.HTTP.Headers.AcceptEncoding qualified as AcceptEncoding
import Network.HTTP.Headers.ContentEncoding qualified as ContentEncoding
import Network.HTTP.Headers.Parsing.Util (ows, parseFieldLine)
import Network.HTTP.Types.Header (Headers, lookupSingletonHeader)

-- | A Connect content-coding. Connect supports @identity@, @gzip@, @br@
-- (Brotli), and @zstd@ — a different set from grpc-spec, so this is a local
-- enum rather than 'Network.GRPC.Spec.Compression'.
data ContentCoding = Identity | Gzip | Br | Zstd
  deriving stock (Eq, Show, Ord, Enum, Bounded)

-- | The wire token for a coding.
codingName :: ContentCoding -> ByteString
codingName Identity = "identity"
codingName Gzip = "gzip"
codingName Br = "br"
codingName Zstd = "zstd"

-- | Inverse of 'codingName'.
codingFromName :: ByteString -> Maybe ContentCoding
codingFromName "identity" = Just Identity
codingFromName "gzip" = Just Gzip
codingFromName "br" = Just Br
codingFromName "zstd" = Just Zstd
codingFromName _ = Nothing

-- | Decode the HTTP field for a unary message. Connect allows a single
-- applied coding and defaults an omitted field to identity.
parseUnaryContentCoding :: [ByteString] -> Either String ContentCoding
parseUnaryContentCoding [] = Right Identity
parseUnaryContentCoding [value] | Just coding <- codingFromName value = Right coding
parseUnaryContentCoding (first : rest) = do
  field <- HttpHeader.parseFromHeaders HttpHeader.defaultHeaderSettings (first :| rest)
  case field of
    ContentEncoding.ContentEncoding coding ->
      maybe (Left "unsupported Content-Encoding") Right (fromHttpCoding coding)
    ContentEncoding.ContentEncodingEmpty -> Left "empty Content-Encoding"
    ContentEncoding.ContentEncodingChain {} -> Left "multiple Content-Encoding codings"

-- | Streaming messages declare one coding token in a single field line.
-- An omitted field means identity.
parseStreamingContentCoding :: Headers -> Either String ContentCoding
parseStreamingContentCoding headers = case lookupSingletonHeader hConnectContentEncoding headers of
  Left err -> Left err
  Right Nothing -> Right Identity
  Right (Just value) -> case codingFromName value of
    Just coding -> Right coding
    Nothing -> do
      coding <- parseFieldLine "Connect-Content-Encoding" (ows *> HttpCoding.contentCodingParser <* ows) value
      maybe (Left "unsupported Connect-Content-Encoding") Right (fromHttpCoding coding)

toHttpCoding :: ContentCoding -> HttpCoding.ContentCoding
toHttpCoding Identity = HttpCoding.Identity
toHttpCoding Gzip = HttpCoding.GZip
toHttpCoding Br = HttpCoding.Brotli
toHttpCoding Zstd = HttpCoding.ZStd

fromHttpCoding :: HttpCoding.ContentCoding -> Maybe ContentCoding
fromHttpCoding HttpCoding.Identity = Just Identity
fromHttpCoding HttpCoding.GZip = Just Gzip
fromHttpCoding HttpCoding.Brotli = Just Br
fromHttpCoding HttpCoding.ZStd = Just Zstd
fromHttpCoding _ = Nothing

-- | Compress a strict 'ByteString' under the given coding. 'Identity' is
-- the identity. (Compression does not throw on valid input.)
compress :: ContentCoding -> ByteString -> ByteString
compress Identity = id
compress Gzip = BL.toStrict . GZip.compress . BL.fromStrict
compress Br = BL.toStrict . Brotli.compress . BL.fromStrict
compress Zstd = Zstd.compress 3

-- | Decompress a strict 'ByteString'. Returns @Left msg@ on decode failure.
-- gzip and brotli decompression is lazy and throws an 'IOException' on
-- corrupt input, so the result is forced under a handler.
decompress :: ContentCoding -> ByteString -> IO (Either String ByteString)
decompress Identity bs = pure (Right bs)
decompress Gzip bs = tryLazyDecompress GZip.decompress bs
decompress Br bs = tryLazyDecompress Brotli.decompress bs
decompress Zstd bs = tryLazyDecompress ZstdL.decompress bs

-- | Run a lazy decompressor, fully forcing the result so any pure
-- exception surfaces as a caught 'IOException'.
tryLazyDecompress
  :: (BL.ByteString -> BL.ByteString) -> ByteString -> IO (Either String ByteString)
tryLazyDecompress dec bs = do
  outcome <-
    try (evaluate (BL.toStrict (dec (BL.fromStrict bs)))) :: IO (Either SomeException ByteString)
  pure $ case outcome of
    Right out -> Right out
    Left _ -> Left "decompression failed: corrupt input"

-- | Parse an @accept-encoding@-style comma-separated coding list into the
-- ordered list of known codings. Connect's streaming field uses bare coding
-- tokens. Empty, malformed and unknown members are ignored.
parseAcceptEncoding :: ByteString -> [ContentCoding]
parseAcceptEncoding value = foldCodingTokens (:) value []

-- Scan trimmed spans in place so negotiation can stop at the first match.
foldCodingTokens :: (ContentCoding -> a -> a) -> ByteString -> a -> a
foldCodingTokens step bytes end = go 0
  where
    size = BS.length bytes
    go start
      | start >= size = end
      | otherwise =
          let !comma = delimiter start
              !first = trimStart start comma
              !limit = trimEnd first comma
              rest = if comma == size then end else go (comma + 1)
           in case limit - first of
                2 | letter first 'b' && letter (first + 1) 'r' -> step Br rest
                4 | gzipAt first -> step Gzip rest
                  | zstdAt first -> step Zstd rest
                6 | letter first 'x', BSU.unsafeIndex bytes (first + 1) == 45, gzipAt (first + 2) -> step Gzip rest
                8 | identityAt first -> step Identity rest
                _ -> rest
    delimiter i
      | i == size || BSU.unsafeIndex bytes i == 0x2c = i
      | otherwise = delimiter (i + 1)
    trimStart i limit
      | i < limit && isOWS (BSU.unsafeIndex bytes i) = trimStart (i + 1) limit
      | otherwise = i
    trimEnd first i
      | first < i && isOWS (BSU.unsafeIndex bytes (i - 1)) = trimEnd first (i - 1)
      | otherwise = i
    isOWS w = w == 32 || w == 9
    -- Each caller has checked the full token length. The ASCII case bit is
    -- folded only when comparing with a letter, so punctuation stays exact.
    letter i c = (BSU.unsafeIndex bytes i .|. 0x20) == fromIntegral (fromEnum c)
    gzipAt i = letter i 'g' && letter (i + 1) 'z' && letter (i + 2) 'i' && letter (i + 3) 'p'
    zstdAt i = letter i 'z' && letter (i + 1) 's' && letter (i + 2) 't' && letter (i + 3) 'd'
    identityAt i =
      letter i 'i' && letter (i + 1) 'd' && letter (i + 2) 'e' && letter (i + 3) 'n'
        && letter (i + 4) 't' && letter (i + 5) 'i' && letter (i + 6) 't' && letter (i + 7) 'y'
{-# INLINE foldCodingTokens #-}

-- | Pick the first client-listed coding the server supports, else
-- 'Identity'. Both arguments are parsed preference lists; the first is the
-- server's supported set, the second the client's ordered preference.
negotiate :: [ContentCoding] -> [ContentCoding] -> ContentCoding
negotiate serverSupported clientPreferred =
  case filter (`elem` serverSupported) clientPreferred of
    (c : _) -> c
    [] -> Identity

-- | Select the first supported streaming coding across the preference
-- field lines in wire order. An omitted field implies the validated request
-- coding. A present field with no recognized match selects identity.
negotiateStreamingResponseCoding :: [ContentCoding] -> ContentCoding -> [ByteString] -> ContentCoding
negotiateStreamingResponseCoding _ requestCoding [] = requestCoding
negotiateStreamingResponseCoding available _ values =
  let !supported = foldl' (\mask coding -> setBit mask (fromEnum coding)) (bit (fromEnum Identity) :: Word) available
      select coding next
        | testBit supported (fromEnum coding) = coding
        | otherwise = next
   in foldr (foldCodingTokens select) Identity values

-- | Select the HTTP response coding. Only an omitted Accept-Encoding
-- implicitly advertises the request's coding.
negotiateUnaryResponseCoding :: [ContentCoding] -> Maybe ContentCoding -> [ByteString] -> Maybe ContentCoding
negotiateUnaryResponseCoding _ requestCoding [] = Just (fromMaybe Identity requestCoding)
negotiateUnaryResponseCoding available _ (first : rest) =
  case HttpHeader.parseFromHeaders HttpHeader.defaultHeaderSettings (first :| rest) of
    Left _ -> Just Identity
    Right accepted ->
      AcceptEncoding.preferredContentCoding (Just accepted) supported >>= fromHttpCoding
  where
    supported = HttpCoding.Identity : map toHttpCoding (filter (/= Identity) available)

-- | Render an ordered Connect preference field.
renderAcceptEncoding :: [ContentCoding] -> ByteString
renderAcceptEncoding = BS.intercalate ", " . map codingName

-- | Render a coding list as a comma-separated token string (for the
-- supported-encodings diagnostic in an @unimplemented@ error).
renderCodingList :: [ContentCoding] -> String
renderCodingList = T.unpack . decodeUtf8 . renderAcceptEncoding
