{-# LANGUAGE OverloadedStrings #-}

-- | Connect protocol wire vocabulary: codecs, the content-type matrix,
-- reserved header names, and unary-GET query parameters.
--
-- This module is pure values; the only logic is the content-type
-- parse/render matrix.
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

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Network.HTTP.Types.Header (HeaderName)

-- | A Connect message codec.
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

-- | Parse a Connect content-type into (streaming-kind, codec).
--
-- Recognizes the four exact wire strings:
--
-- @application\/proto@, @application\/json@ (unary) and
-- @application\/connect+proto@, @application\/connect+json@ (streaming).
-- The @application\/@ prefix is matched case-insensitively (HTTP header
-- values are case-insensitive for media types); the codec suffix must
-- match exactly. Returns 'Nothing' for anything else (the call site
-- responds 415 Unsupported Media Type).
parseContentType :: ByteString -> Maybe (IsStreaming, Codec)
parseContentType bs = do
  let (media, rest) = BS.break (== 0x2F) bs -- split on first '/'
  if CI.mk media /= "application"
    then Nothing
    else case rest of
      -- Drop the leading '/'.
      r | BS.head r == 0x2F -> classify (BS.drop 1 r)
      _ -> Nothing
  where
    classify r
      | Just c <- codecFromName r = Just (Unary, c)
      | "connect+" `BS.isPrefixOf` r =
          codecFromName (BS.drop (BS.length "connect+") r)
            >>= \c -> Just (Streaming, c)
      | otherwise = Nothing

------------------------------------------------------------------------
-- Header names
------------------------------------------------------------------------

-- | @connect-protocol-version@
hConnectProtocolVersion :: HeaderName
hConnectProtocolVersion = CI.mk "connect-protocol-version"

-- | @connect-timeout-ms@
hConnectTimeoutMs :: HeaderName
hConnectTimeoutMs = CI.mk "connect-timeout-ms"

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
