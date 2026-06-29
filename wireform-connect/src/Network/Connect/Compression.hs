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

  -- * (De)compression
  compress,
  decompress,

  -- * Negotiation
  negotiate,
  parseAcceptEncoding,
  renderCodingList,
) where
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)

import Control.Exception (IOException, evaluate, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Codec.Compression.Brotli qualified as Brotli
import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Zstd qualified as Zstd

-- | A Connect content-coding.
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
decompress Zstd bs = case Zstd.decompress bs of
  Zstd.Decompress out -> pure (Right out)
  Zstd.Skip -> pure (Right BS.empty)
  Zstd.Error msg -> pure (Left ("zstd decompress: " <> msg))

-- | Run a lazy decompressor, fully forcing the result so any pure
-- exception surfaces as a caught 'IOException'.
tryLazyDecompress
  :: (BL.ByteString -> BL.ByteString) -> ByteString -> IO (Either String ByteString)
tryLazyDecompress dec bs = do
  outcome <-
    try (evaluate (BL.toStrict (dec (BL.fromStrict bs)))) :: IO (Either IOException ByteString)
  pure $ case outcome of
    Right out -> Right out
    Left _ -> Left "decompression failed: corrupt input"

-- | Parse an @accept-encoding@-style comma-separated coding list into the
-- ordered list of codings (unknown tokens dropped). This implements the
-- simplified Connect subset (no quality values).
parseAcceptEncoding :: ByteString -> [ContentCoding]
parseAcceptEncoding bs = go (BS.split 0x2C bs)
  where
    go [] = []
    go (c : cs) = case codingFromName (trim c) of
      Just coding -> coding : go cs
      Nothing -> go cs
    trim = BS.dropWhile isWS . BS.dropWhileEnd isWS
    isWS w = w == 0x20 || w == 0x09

-- | Pick the first client-listed coding the server supports, else
-- 'Identity'. Both arguments are parsed preference lists; the first is the
-- server's supported set, the second the client's ordered preference.
negotiate :: [ContentCoding] -> [ContentCoding] -> ContentCoding
negotiate serverSupported clientPreferred =
  case filter (`elem` serverSupported) clientPreferred of
    (c : _) -> c
    [] -> Identity

-- | Render a coding list as a comma-separated token string (for the
-- supported-encodings diagnostic in an @unimplemented@ error).
renderCodingList :: [ContentCoding] -> String
renderCodingList cs = T.unpack (decodeUtf8 (BS.intercalate ", " (map codingName cs)))
