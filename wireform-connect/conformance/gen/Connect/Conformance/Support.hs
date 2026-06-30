{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Shared helpers for the connectrpc conformance client and server harness
-- programs: the stdin\/stdout size-delimited framing the test runner speaks,
-- conformance enum → Connect mappings, @google.protobuf.Any@ pack\/unpack for
-- the conformance message types, and conversion between Connect
-- 'CustomMetadata' and the conformance @Header@ message.
module Connect.Conformance.Support
  ( -- * Size-delimited framing (4-byte big-endian length prefix)
    recvMsg
  , sendMsg
    -- * Enum mappings
  , toConnectCodec
  , toContentCoding
    -- * Metadata <-> conformance Header
  , metadataToHeaders
  , headersToMetadata
    -- * google.protobuf.Any
  , packMsgAny
  , unpackMsgAny
    -- * Runtime Any type registry
  , conformanceTypeRegistry
  , registerConformanceTypes
  ) where

import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (foldl', nub)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Vector qualified as V
import Data.Word (Word8)
import System.IO (Handle, hFlush)

import Connect.Conformance.Proto qualified as P
import Network.Connect.Compression qualified as CC
import Network.Connect.Protocol (Codec (..))
import Network.GRPC.Spec
  ( CustomMetadata
  , HeaderName (AsciiHeader, BinaryHeader)
  , customMetadataName
  , customMetadataValue
  , safeCustomMetadata
  , safeHeaderName
  )
import Proto.Decode (DecodeError, MessageDecode, decodeMessage)
import Proto.Encode (MessageEncode, encodeMessage)
import Proto.Google.Protobuf.Any (Any (..), defaultAny)
import Proto.Google.Protobuf.Any.Util (typeNameFromUrl, typeUrlPrefix)
import Proto.Schema (ProtoMessage, protoMessageName)
import Proto.Registry (TypeRegistry, registerGlobalAnyCodecs, registerMessage)
import Proto.Internal.JSON.WellKnown (standardWktRegistry)

------------------------------------------------------------------------
-- Size-delimited framing
------------------------------------------------------------------------

-- | Read one size-delimited message: a 4-byte big-endian length followed by
-- that many payload bytes. 'Nothing' on a clean EOF (no more messages).
recvMsg :: Handle -> IO (Maybe ByteString)
recvMsg h = do
  lenBytes <- hGetExactly h 4
  if BS.length lenBytes < 4
    then pure Nothing
    else do
      let n =
            foldl' (\acc w -> acc `shiftL` 8 + fromIntegral w) (0 :: Int) (BS.unpack lenBytes)
      body <- hGetExactly h n
      if BS.length body < n then pure Nothing else pure (Just body)

-- | Write one size-delimited message (4-byte big-endian length + payload),
-- then flush.
sendMsg :: Handle -> ByteString -> IO ()
sendMsg h payload = do
  let n = BS.length payload
      hdr =
        BS.pack
          [ fromIntegral ((n `shiftR` 24) .&. 0xFF) :: Word8
          , fromIntegral ((n `shiftR` 16) .&. 0xFF)
          , fromIntegral ((n `shiftR` 8) .&. 0xFF)
          , fromIntegral (n .&. 0xFF)
          ]
  BS.hPut h hdr
  BS.hPut h payload
  hFlush h

-- | Read exactly @n@ bytes (looping over short reads); returns fewer only at EOF.
hGetExactly :: Handle -> Int -> IO ByteString
hGetExactly h n = go n []
  where
    go 0 acc = pure (BS.concat (reverse acc))
    go remaining acc = do
      chunk <- BS.hGet h remaining
      if BS.null chunk
        then pure (BS.concat (reverse acc))
        else go (remaining - BS.length chunk) (chunk : acc)

------------------------------------------------------------------------
-- Enum mappings
------------------------------------------------------------------------

-- | Map a conformance 'P.Codec' to a Connect 'Codec'. 'Nothing' for codecs the
-- Connect implementation does not support (text\/unspecified).
toConnectCodec :: P.Codec -> Maybe Codec
toConnectCodec P.Codec'CodecProto = Just CodecProto
toConnectCodec P.Codec'CodecJson = Just CodecJSON
toConnectCodec _ = Nothing

-- | Map a conformance 'P.Compression' to a Connect 'CC.ContentCoding'.
-- 'Nothing' for codings the Connect implementation does not support
-- (deflate\/snappy\/unspecified).
toContentCoding :: P.Compression -> Maybe CC.ContentCoding
toContentCoding P.Compression'CompressionIdentity = Just CC.Identity
toContentCoding P.Compression'CompressionGzip = Just CC.Gzip
toContentCoding P.Compression'CompressionBr = Just CC.Br
toContentCoding P.Compression'CompressionZstd = Just CC.Zstd
toContentCoding _ = Nothing

------------------------------------------------------------------------
-- Metadata <-> conformance Header
------------------------------------------------------------------------

-- | Render a 'HeaderName' to its on-the-wire name text.
headerNameText :: HeaderName -> Text
headerNameText (AsciiHeader bs) = decodeUtf8Lenient bs
headerNameText (BinaryHeader bs) = decodeUtf8Lenient bs

-- | Group Connect 'CustomMetadata' into conformance @Header@ messages,
-- preserving first-seen name order and collecting repeated values.
metadataToHeaders :: [CustomMetadata] -> [P.Header]
metadataToHeaders cms = map build names
  where
    pairs = map (\c -> (headerNameText (customMetadataName c), decodeUtf8Lenient (customMetadataValue c))) cms
    names = nub (map fst pairs)
    build n =
      P.defaultHeader
        { P.headerName = n
        , P.headerValue = V.fromList (map snd (filter ((== n) . fst) pairs))
        }

-- | Expand conformance @Header@ messages into Connect 'CustomMetadata' (one
-- entry per value). Names that are not valid metadata header names are dropped.
headersToMetadata :: [P.Header] -> [CustomMetadata]
headersToMetadata = concatMap one
  where
    one h = mapMaybe (mk (P.headerName h)) (V.toList (P.headerValue h))
    mk n v = do
      hn <- safeHeaderName (encodeUtf8 (T.toLower n))
      safeCustomMetadata hn (encodeUtf8 v)

------------------------------------------------------------------------
-- google.protobuf.Any
------------------------------------------------------------------------

-- | Pack a generated message into an 'Any', using its fully-qualified proto
-- name for the @type.googleapis.com/...@ type URL.
packMsgAny :: forall a. (ProtoMessage a, MessageEncode a) => a -> Any
packMsgAny msg =
  defaultAny
    { anyTypeUrl = typeUrlPrefix <> protoMessageName (Proxy @a)
    , anyValue = encodeMessage msg
    }

-- | Unpack an 'Any' into a generated message if the type URL's name matches.
-- 'Nothing' on a type mismatch; @Just (Left _)@ on a decode failure.
unpackMsgAny :: forall a. (ProtoMessage a, MessageDecode a) => Any -> Maybe (Either DecodeError a)
unpackMsgAny a
  | typeNameFromUrl (anyTypeUrl a) == protoMessageName (Proxy @a) = Just (decodeMessage (anyValue a))
  | otherwise = Nothing

------------------------------------------------------------------------
-- Runtime Any type registry
------------------------------------------------------------------------

-- | A 'TypeRegistry' of the conformance request message types (plus the
-- standard well-known types). These are the types that get packed into
-- @google.protobuf.Any@ and echoed back inside
-- @ConformancePayload.request_info.requests@; registering them makes
-- @Any@ values serialise to the canonical proto3 JSON
-- @{"\@type": …, …inlined…}@ form the reference impl expects (rather than
-- the degenerate @{"value": <base64>}@ fallback for unknown types).
conformanceTypeRegistry :: TypeRegistry
conformanceTypeRegistry =
  registerMessage (Proxy @P.UnaryRequest)
    . registerMessage (Proxy @P.IdempotentUnaryRequest)
    . registerMessage (Proxy @P.ServerStreamRequest)
    . registerMessage (Proxy @P.ClientStreamRequest)
    . registerMessage (Proxy @P.BidiStreamRequest)
    . registerMessage (Proxy @P.UnimplementedRequest)
    $ standardWktRegistry

-- | Register the conformance message types into the process-global Any
-- registry. Must be called once at program startup, before any RPC is
-- handled (i.e. before any @Any@ value is (de)serialised).
registerConformanceTypes :: IO ()
registerConformanceTypes = registerGlobalAnyCodecs conformanceTypeRegistry
