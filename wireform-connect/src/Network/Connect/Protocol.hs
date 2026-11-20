{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
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
  ConnectContentType (..),
  ConnectContentTypeError (..),
  unaryContentType,
  streamContentType,
  parseContentType,
  lookupConnectContentType,

  -- * Header names
  hConnectProtocolVersion,
  hConnectTimeoutMs,
  parseTimeoutMillis,
  renderTimeoutMillis,
  boundTimeoutMillis,
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

#include "MachDeps.h"

import Data.Bifunctor (first)
import Data.Bits ((.&.), (.|.), finiteBitSize, shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.CaseInsensitive qualified as CI
import Data.Int (Int64)
#if defined(WORDS_BIGENDIAN)
import Data.Word (byteSwap32, byteSwap64)
#endif
import GHC.Exts (Addr#, Int (I#), Word (W#), indexWord8OffAddr#, indexWord8OffAddrAsWord32#, indexWord8OffAddrAsWord64#, word8ToWord#)
import GHC.ForeignPtr (ForeignPtr (ForeignPtr), unsafeWithForeignPtr)
import GHC.Word (Word32 (W32#), Word64 (W64#))
import Network.HTTP.Client.Media (MediaType (..), parseMediaType)
import Network.HTTP.Headers.Mason qualified as M
import Network.HTTP.Types.Header (HeaderName, Headers, hContentType, lookupSingletonHeader)

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

-- | The envelope and codec selected by a Connect media type.
data ConnectContentType = ConnectContentType
  { connectMediaStreaming :: !IsStreaming
  , connectMediaCodec :: !Codec
  }
  deriving stock (Eq, Show)

-- | Why a message does not have a usable Connect media type.
data ConnectContentTypeError
  = MissingContentType
  | RepeatedContentType
  | MalformedContentType
  | NonApplicationContentType
  | UnsupportedConnectContentType !IsStreaming
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
parseContentType bytes = case classifyContentType bytes of
  -- Keep the four results static for callers of the original tuple API.
  Right (ConnectContentType Unary CodecProto) -> Just (Unary, CodecProto)
  Right (ConnectContentType Unary CodecJSON) -> Just (Unary, CodecJSON)
  Right (ConnectContentType Streaming CodecProto) -> Just (Streaming, CodecProto)
  Right (ConnectContentType Streaming CodecJSON) -> Just (Streaming, CodecJSON)
  Left _ -> Nothing

-- | Validate the singleton HTTP field before selecting the Connect codec.
lookupConnectContentType :: Headers -> Either ConnectContentTypeError ConnectContentType
lookupConnectContentType headers = case lookupSingletonHeader hContentType headers of
  Left _ -> Left RepeatedContentType
  Right Nothing -> Left MissingContentType
  Right (Just value) -> classifyContentType value

classifyContentType :: ByteString -> Either ConnectContentTypeError ConnectContentType
classifyContentType "application/proto" = Right (ConnectContentType Unary CodecProto)
classifyContentType "application/json" = Right (ConnectContentType Unary CodecJSON)
classifyContentType "application/connect+proto" = Right (ConnectContentType Streaming CodecProto)
classifyContentType "application/connect+json" = Right (ConnectContentType Streaming CodecJSON)
classifyContentType bytes = do
  media <- first (const MalformedContentType) (parseMediaType bytes)
  if mtType media /= "application"
    then Left NonApplicationContentType
    else case BS.stripPrefix "connect+" (mtSubType media) of
      Just suffix -> selectCodec media Streaming suffix
      Nothing -> selectCodec media Unary (mtSubType media)
  where
    selectCodec media streaming name = case codecFromName name of
      Just codec | codec /= CodecJSON || all validCharset (mtParameters media) ->
        Right (ConnectContentType streaming codec)
      _ -> Left (UnsupportedConnectContentType streaming)
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
parseTimeoutMillis (BSI.BS fp@(ForeignPtr start _) size)
  | size == 0 || size > 10 = Nothing
  | otherwise = BSI.accursedUnutterablePerformIO $ unsafeWithForeignPtr fp $ \_ ->
      let !millis = parseMillis start size
      in pure $! if millis < 0 then Nothing else Just $! toTimerMillis millis
{-# INLINE parseTimeoutMillis #-}

-- Loads are gated by the slice length and also work on unaligned field values.
-- A negative value marks a failed decode and never reaches the public result.
parseMillis :: Addr# -> Int -> Int64
parseMillis start size
  | size == 1 = if firstDigit <= 9 then fromIntegral firstDigit else -1
  | size < 4 = scan 0 0
  | size < 8 = continue 4 (parseFourDigits start)
  | otherwise = continue 8 (parseEightDigits start)
  where
    firstDigit = digitAt 0

    continue offset n
      | n < 0 = -1
      | otherwise = scan offset n

    scan !offset !n
      | offset == size = n
      | digit > 9 = -1
      | otherwise = scan (offset + 1) (n * 10 + fromIntegral digit)
      where
        digit = digitAt offset

    digitAt (I# i) = W# (word8ToWord# (indexWord8OffAddr# start i)) - 48
{-# INLINE parseMillis #-}

-- The first out-of-range lane sets a high bit on the lower or upper bound
-- check. The OR also catches non-ASCII lanes. With all lanes valid, decimal
-- pairs and groups fit within their byte or two-byte lanes without carry.
parseFourDigits :: Addr# -> Int64
parseFourDigits start
  | (ascii .|. digits .|. (ascii + 0x46464646)) .&. 0x80808080 /= 0 = -1
  | otherwise = fromIntegral ((pairs * 100 + (pairs `shiftR` 16)) .&. 0xFFFF)
  where
    ascii =
#if defined(WORDS_BIGENDIAN)
      byteSwap32 (W32# (indexWord8OffAddrAsWord32# start 0#))
#else
      W32# (indexWord8OffAddrAsWord32# start 0#)
#endif
    digits = ascii - 0x30303030
    pairs = (digits * 10 + (digits `shiftR` 8)) .&. 0x00FF00FF
{-# INLINE parseFourDigits #-}

parseEightDigits :: Addr# -> Int64
parseEightDigits start
  | (ascii .|. digits .|. (ascii + 0x4646464646464646)) .&. 0x8080808080808080 /= 0 = -1
  | otherwise = fromIntegral ((quads * 10000 + (quads `shiftR` 32)) .&. 0xFFFFFFFF)
  where
    ascii =
#if defined(WORDS_BIGENDIAN)
      byteSwap64 (W64# (indexWord8OffAddrAsWord64# start 0#))
#else
      W64# (indexWord8OffAddrAsWord64# start 0#)
#endif
    digits = ascii - 0x3030303030303030
    pairs = (digits * 10 + (digits `shiftR` 8)) .&. 0x00FF00FF00FF00FF
    quads = (pairs * 100 + (pairs `shiftR` 16)) .&. 0x0000FFFF0000FFFF
{-# INLINE parseEightDigits #-}

toTimerMillis :: Int64 -> Int
toTimerMillis millis
  | finiteBitSize (0 :: Int) == 64 = fromIntegral millis
  | otherwise = fromIntegral (min millis (fromIntegral maxTimeoutMillis))
{-# INLINE toTimerMillis #-}

-- | Restrict a configured duration to the wire and local timer range.
-- A negative duration is already expired and is represented by zero.
boundTimeoutMillis :: Int -> Int
boundTimeoutMillis = max 0 . min maxTimeoutMillis

-- | Render a configured duration as a valid @Connect-Timeout-Ms@ value.
renderTimeoutMillis :: Int -> ByteString
renderTimeoutMillis = M.toStrictByteString . M.intDec . boundTimeoutMillis

maxTimeoutMillis :: Int
maxTimeoutMillis = fromIntegral (min (9999999999 :: Word64) (fromIntegral (maxBound `quot` 1000 :: Int)))

-- | @connect-content-encoding@ (streaming compression)
hConnectContentEncoding :: HeaderName
hConnectContentEncoding = CI.mk "connect-content-encoding"

-- | @connect-accept-encoding@ (streaming compression preference)
hConnectAcceptEncoding :: HeaderName
hConnectAcceptEncoding = CI.mk "connect-accept-encoding"

------------------------------------------------------------------------
-- Wire constants
------------------------------------------------------------------------

-- | The Connect protocol version sent on POST requests
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
