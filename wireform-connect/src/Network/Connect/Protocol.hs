{-# LANGUAGE OverloadedStrings #-}

-- | Connect protocol wire vocabulary: codecs, the content-type matrix,
-- reserved header names, and unary-GET query parameters.
--
-- Includes parsing for Connect media types and request timeouts.
module Network.Connect.Protocol (
  -- * Codecs
  Codec (..),
  codecName,
  codecFromName,

  -- * Streaming kind
  IsStreaming (..),

  -- * Content-type matrix
  unaryContentType,
  streamContentType,
  parseContentType,

  -- * Header names
  hConnectProtocolVersion,
  hConnectTimeoutMs,
  parseTimeoutMillis,
  hConnectContentEncoding,
  hConnectAcceptEncoding,

  -- * Wire constants
  connectProtocolVersion,
  trailerPrefix,

  -- * Unary-GET query parameters
  qpMessage,
  qpEncoding,
  qpBase64,
  qpCompression,
  qpConnect,
  connectGetVersion,
) where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.Word (Word64)
import Network.HTTP.Client.Media (MediaType (..), parseMediaType)
import Network.HTTP.Types.Header (HeaderName)

-- | A Connect wire codec: 'CodecProto' (binary Protobuf) or 'CodecJSON'
-- (the proto3 JSON mapping).
data Codec = CodecProto | CodecJSON
  deriving stock (Eq, Show, Ord, Enum, Bounded)

-- | The codec suffix used in content-types and the GET @encoding@ query param.
codecName :: Codec -> ByteString
codecName CodecProto = "proto"
codecName CodecJSON = "json"

-- | Inverse of 'codecName'.
codecFromName :: ByteString -> Maybe Codec
codecFromName "proto" = Just CodecProto
codecFromName "json" = Just CodecJSON
codecFromName _ = Nothing

-- | Whether an exchange is unary (bare message) or streaming (enveloped).
data IsStreaming = Unary | Streaming
  deriving stock (Eq, Show)

-- | @application\/<codec>@ — the unary content-type.
unaryContentType :: Codec -> ByteString
unaryContentType c = "application/" <> codecName c

-- | @application\/connect+<codec>@ — the streaming content-type.
streamContentType :: Codec -> ByteString
streamContentType c = "application/connect+" <> codecName c

-- | Parse a Connect content type. JSON accepts only UTF-8 when a charset
-- is supplied. Protobuf and other media-type parameters are independent
-- of the charset parameter.
parseContentType :: ByteString -> Maybe (IsStreaming, Codec)
parseContentType "application/proto" = Just (Unary, CodecProto)
parseContentType "application/json" = Just (Unary, CodecJSON)
parseContentType "application/connect+proto" = Just (Streaming, CodecProto)
parseContentType "application/connect+json" = Just (Streaming, CodecJSON)
parseContentType bytes = do
  media <- either (const Nothing) Just (parseMediaType bytes)
  guard (mtType media == "application")
  let subtype = mtSubType media
      (streaming, codecBytes) = case BS.stripPrefix "connect+" subtype of
        Just suffix -> (Streaming, suffix)
        Nothing -> (Unary, subtype)
  codec <- codecFromName codecBytes
  guard (codec /= CodecJSON || all validCharset (mtParameters media))
  pure (streaming, codec)
  where
    validCharset (name, value) = name /= "charset" || CI.mk value == "utf-8"

------------------------------------------------------------------------
-- Header names
------------------------------------------------------------------------

-- | @connect-protocol-version@
hConnectProtocolVersion :: HeaderName
hConnectProtocolVersion = CI.mk "connect-protocol-version"

-- | @connect-timeout-ms@
hConnectTimeoutMs :: HeaderName
hConnectTimeoutMs = CI.mk "connect-timeout-ms"

-- | Parse one to ten ASCII digits, including zero. Keep the conversion to
-- microseconds within the host 'Int' range used by @System.Timeout.timeout@.
parseTimeoutMillis :: ByteString -> Maybe Int
parseTimeoutMillis bytes
  | BS.null bytes || BS.length bytes > 10 || not (BS.all isDigit bytes) = Nothing
  | otherwise = Just (fromIntegral (min millis maxMillis))
  where
    isDigit b = b >= 48 && b <= 57
    millis = BS.foldl' (\n b -> n * 10 + fromIntegral (b - 48)) 0 bytes :: Word64
    maxMillis = fromIntegral (maxBound `quot` 1000 :: Int)

-- | @connect-content-encoding@ (streaming compression)
hConnectContentEncoding :: HeaderName
hConnectContentEncoding = CI.mk "connect-content-encoding"

-- | @connect-accept-encoding@ (streaming compression preference)
hConnectAcceptEncoding :: HeaderName
hConnectAcceptEncoding = CI.mk "connect-accept-encoding"

------------------------------------------------------------------------
-- Wire constants
------------------------------------------------------------------------

-- | The Connect protocol version sent on unary requests
-- (@connect-protocol-version: 1@).
connectProtocolVersion :: ByteString
connectProtocolVersion = "1"

-- | The @trailer-@ prefix applied to unary trailing-metadata keys.
trailerPrefix :: ByteString
trailerPrefix = "trailer-"

------------------------------------------------------------------------
-- Unary-GET query parameters
------------------------------------------------------------------------

-- | @message@
qpMessage :: ByteString
qpMessage = "message"

-- | @encoding@
qpEncoding :: ByteString
qpEncoding = "encoding"

-- | @base64@
qpBase64 :: ByteString
qpBase64 = "base64"

-- | @compression@
qpCompression :: ByteString
qpCompression = "compression"

-- | @connect@
qpConnect :: ByteString
qpConnect = "connect"

-- | @v1@ — the value of the @connect@ query parameter.
connectGetVersion :: ByteString
connectGetVersion = "v1"
