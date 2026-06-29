
-- | Message-body codecs shared by the Connect client and server.
--
-- A Connect 'Network.Connect.Protocol.Codec' dispatches to either the binary
-- Protobuf path (grpc-spec's @rpcSerialize*@ \/ @rpcDeserialize*@, which go
-- through @wireform-proto@'s wire codec) or the proto3-JSON path
-- (vanilla aeson 'ToJSON' \/ 'FromJSON' on the generated message type; the
-- 'Proto' newtype bridge lives in @grpc-spec@).
module Network.Connect.Codec (
  encodeInputBody,
  decodeInputBody,
  encodeOutputBody,
  decodeOutputBody,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Proxy (Proxy)
import Network.Connect.Protocol (Codec (..))
import Network.GRPC.Spec
  ( Input
  , Output
  , SupportsClientRpc (..)
  , SupportsServerRpc (..)
  )

-- Binary serialization helpers return lazy ByteStrings; we convert to/from
-- strict at the boundary.

-- | Encode a request input message to a strict body under the given codec.
encodeInputBody
  :: (SupportsClientRpc rpc, Aeson.ToJSON (Input rpc))
  => Codec
  -> Proxy rpc
  -> Input rpc
  -> ByteString
encodeInputBody codec p inp = case codec of
  CodecProto -> BL.toStrict (rpcSerializeInput p inp)
  CodecJSON -> BL.toStrict (Aeson.encode inp)

-- | Decode a request input message from a strict body.
decodeInputBody
  :: (SupportsServerRpc rpc, Aeson.FromJSON (Input rpc))
  => Codec
  -> Proxy rpc
  -> ByteString
  -> Either String (Input rpc)
decodeInputBody codec p bs = case codec of
  CodecProto -> rpcDeserializeInput p (BL.fromStrict bs)
  CodecJSON -> Aeson.eitherDecodeStrict' bs

-- | Encode a response output message to a strict body.
encodeOutputBody
  :: (SupportsServerRpc rpc, Aeson.ToJSON (Output rpc))
  => Codec
  -> Proxy rpc
  -> Output rpc
  -> ByteString
encodeOutputBody codec p out = case codec of
  CodecProto -> BL.toStrict (rpcSerializeOutput p out)
  CodecJSON -> BL.toStrict (Aeson.encode out)

-- | Decode a response output message from a strict body.
decodeOutputBody
  :: (SupportsClientRpc rpc, Aeson.FromJSON (Output rpc))
  => Codec
  -> Proxy rpc
  -> ByteString
  -> Either String (Output rpc)
decodeOutputBody codec p bs = case codec of
  CodecProto -> rpcDeserializeOutput p (BL.fromStrict bs)
  CodecJSON -> Aeson.eitherDecodeStrict' bs
