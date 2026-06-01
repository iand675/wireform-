{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | RPC type definitions for the gRPC interop test service.
--
-- Defines custom RPC marker types that satisfy grpc-spec's IsRPC,
-- SupportsClientRpc, SupportsServerRpc using wireform-proto's
-- MessageEncode/MessageDecode for serialization.
module Interop.API (
    -- * Service marker
    TestService
  , UnimplementedService

    -- * RPC methods
  , EmptyCall
  , UnaryCall
  , StreamingOutputCall
  , StreamingInputCall
  , FullDuplexCall
  , HalfDuplexCall
  , UnimplementedCall
  , UnimplementedServiceCall
  ) where

import Control.DeepSeq (NFData(..))
import Data.ByteString.Lazy qualified as BL
import Data.Proxy (Proxy(..))

import Network.GRPC.Spec
import Proto.Encode (MessageEncode, encodeMessage)
import Proto.Decode (MessageDecode, decodeMessage)

import Proto.Interop


-- | Marker type for grpc.testing.TestService
data TestService

-- | Marker type for grpc.testing.UnimplementedService
data UnimplementedService

-- Each method is a distinct phantom type indexed by service and method name.
-- We manually wire up Input/Output/streaming types and IsRPC instances.

data EmptyCall
data UnaryCall
data StreamingOutputCall
data StreamingInputCall
data FullDuplexCall
data HalfDuplexCall
data UnimplementedCall
data UnimplementedServiceCall

-- Input/Output type family instances
type instance Input  EmptyCall = Empty
type instance Output EmptyCall = Empty

type instance Input  UnaryCall = SimpleRequest
type instance Output UnaryCall = SimpleResponse

type instance Input  StreamingOutputCall = StreamingOutputCallRequest
type instance Output StreamingOutputCall = StreamingOutputCallResponse

type instance Input  StreamingInputCall = StreamingInputCallRequest
type instance Output StreamingInputCall = StreamingInputCallResponse

type instance Input  FullDuplexCall = StreamingOutputCallRequest
type instance Output FullDuplexCall = StreamingOutputCallResponse

type instance Input  HalfDuplexCall = StreamingOutputCallRequest
type instance Output HalfDuplexCall = StreamingOutputCallResponse

type instance Input  UnimplementedCall = Empty
type instance Output UnimplementedCall = Empty

type instance Input  UnimplementedServiceCall = Empty
type instance Output UnimplementedServiceCall = Empty

-- Metadata: use NoMetadata for all RPCs (interop tests don't use typed metadata)
type instance RequestMetadata          EmptyCall = NoMetadata
type instance ResponseInitialMetadata  EmptyCall = NoMetadata
type instance ResponseTrailingMetadata EmptyCall = NoMetadata

type instance RequestMetadata          UnaryCall = NoMetadata
type instance ResponseInitialMetadata  UnaryCall = NoMetadata
type instance ResponseTrailingMetadata UnaryCall = NoMetadata

type instance RequestMetadata          StreamingOutputCall = NoMetadata
type instance ResponseInitialMetadata  StreamingOutputCall = NoMetadata
type instance ResponseTrailingMetadata StreamingOutputCall = NoMetadata

type instance RequestMetadata          StreamingInputCall = NoMetadata
type instance ResponseInitialMetadata  StreamingInputCall = NoMetadata
type instance ResponseTrailingMetadata StreamingInputCall = NoMetadata

type instance RequestMetadata          FullDuplexCall = NoMetadata
type instance ResponseInitialMetadata  FullDuplexCall = NoMetadata
type instance ResponseTrailingMetadata FullDuplexCall = NoMetadata

type instance RequestMetadata          HalfDuplexCall = NoMetadata
type instance ResponseInitialMetadata  HalfDuplexCall = NoMetadata
type instance ResponseTrailingMetadata HalfDuplexCall = NoMetadata

type instance RequestMetadata          UnimplementedCall = NoMetadata
type instance ResponseInitialMetadata  UnimplementedCall = NoMetadata
type instance ResponseTrailingMetadata UnimplementedCall = NoMetadata

type instance RequestMetadata          UnimplementedServiceCall = NoMetadata
type instance ResponseInitialMetadata  UnimplementedServiceCall = NoMetadata
type instance ResponseTrailingMetadata UnimplementedServiceCall = NoMetadata

-- NFData instances for message types (required by IsRPC)
instance NFData Empty where rnf !_ = ()
instance NFData BoolValue where rnf !_ = ()
instance NFData PayloadType where rnf !_ = ()
instance NFData Payload where rnf !_ = ()
instance NFData EchoStatus where rnf !_ = ()
instance NFData SimpleRequest where rnf !_ = ()
instance NFData SimpleResponse where rnf !_ = ()
instance NFData StreamingInputCallRequest where rnf !_ = ()
instance NFData StreamingInputCallResponse where rnf !_ = ()
instance NFData ResponseParameters where rnf !_ = ()
instance NFData StreamingOutputCallRequest where rnf !_ = ()
instance NFData StreamingOutputCallResponse where rnf !_ = ()
instance NFData ReconnectParams where rnf !_ = ()
instance NFData ReconnectInfo where rnf !_ = ()

-- Streaming type instances
instance HasStreamingType EmptyCall where
  type RpcStreamingType EmptyCall = NonStreaming
instance SupportsStreamingType EmptyCall NonStreaming

instance HasStreamingType UnaryCall where
  type RpcStreamingType UnaryCall = NonStreaming
instance SupportsStreamingType UnaryCall NonStreaming

instance HasStreamingType StreamingOutputCall where
  type RpcStreamingType StreamingOutputCall = ServerStreaming
instance SupportsStreamingType StreamingOutputCall ServerStreaming

instance HasStreamingType StreamingInputCall where
  type RpcStreamingType StreamingInputCall = ClientStreaming
instance SupportsStreamingType StreamingInputCall ClientStreaming

instance HasStreamingType FullDuplexCall where
  type RpcStreamingType FullDuplexCall = BiDiStreaming
instance SupportsStreamingType FullDuplexCall BiDiStreaming

instance HasStreamingType HalfDuplexCall where
  type RpcStreamingType HalfDuplexCall = BiDiStreaming
instance SupportsStreamingType HalfDuplexCall BiDiStreaming

instance HasStreamingType UnimplementedCall where
  type RpcStreamingType UnimplementedCall = NonStreaming
instance SupportsStreamingType UnimplementedCall NonStreaming

instance HasStreamingType UnimplementedServiceCall where
  type RpcStreamingType UnimplementedServiceCall = NonStreaming
instance SupportsStreamingType UnimplementedServiceCall NonStreaming

-- IsRPC instances
instance IsRPC EmptyCall where
  rpcContentType  _ = defaultRpcContentType "proto"
  rpcServiceName  _ = "grpc.testing.TestService"
  rpcMethodName   _ = "EmptyCall"
  rpcMessageType  _ = Just "grpc.testing.Empty"

instance IsRPC UnaryCall where
  rpcContentType  _ = defaultRpcContentType "proto"
  rpcServiceName  _ = "grpc.testing.TestService"
  rpcMethodName   _ = "UnaryCall"
  rpcMessageType  _ = Just "grpc.testing.SimpleRequest"

instance IsRPC StreamingOutputCall where
  rpcContentType  _ = defaultRpcContentType "proto"
  rpcServiceName  _ = "grpc.testing.TestService"
  rpcMethodName   _ = "StreamingOutputCall"
  rpcMessageType  _ = Just "grpc.testing.StreamingOutputCallRequest"

instance IsRPC StreamingInputCall where
  rpcContentType  _ = defaultRpcContentType "proto"
  rpcServiceName  _ = "grpc.testing.TestService"
  rpcMethodName   _ = "StreamingInputCall"
  rpcMessageType  _ = Just "grpc.testing.StreamingInputCallRequest"

instance IsRPC FullDuplexCall where
  rpcContentType  _ = defaultRpcContentType "proto"
  rpcServiceName  _ = "grpc.testing.TestService"
  rpcMethodName   _ = "FullDuplexCall"
  rpcMessageType  _ = Just "grpc.testing.StreamingOutputCallRequest"

instance IsRPC HalfDuplexCall where
  rpcContentType  _ = defaultRpcContentType "proto"
  rpcServiceName  _ = "grpc.testing.TestService"
  rpcMethodName   _ = "HalfDuplexCall"
  rpcMessageType  _ = Just "grpc.testing.StreamingOutputCallRequest"

instance IsRPC UnimplementedCall where
  rpcContentType  _ = defaultRpcContentType "proto"
  rpcServiceName  _ = "grpc.testing.TestService"
  rpcMethodName   _ = "UnimplementedCall"
  rpcMessageType  _ = Just "grpc.testing.Empty"

instance IsRPC UnimplementedServiceCall where
  rpcContentType  _ = defaultRpcContentType "proto"
  rpcServiceName  _ = "grpc.testing.UnimplementedService"
  rpcMethodName   _ = "UnimplementedCall"
  rpcMessageType  _ = Just "grpc.testing.Empty"

-- Helper: serialize via wireform-proto
wpSerialize :: MessageEncode a => a -> BL.ByteString
wpSerialize = BL.fromStrict . encodeMessage

-- Helper: deserialize via wireform-proto
wpDeserialize :: MessageDecode a => BL.ByteString -> Either String a
wpDeserialize bs = case decodeMessage (BL.toStrict bs) of
  Left err -> Left (show err)
  Right v  -> Right v

-- SupportsClientRpc instances
instance SupportsClientRpc EmptyCall where
  rpcSerializeInput   _ = wpSerialize
  rpcDeserializeOutput _ = wpDeserialize

instance SupportsClientRpc UnaryCall where
  rpcSerializeInput   _ = wpSerialize
  rpcDeserializeOutput _ = wpDeserialize

instance SupportsClientRpc StreamingOutputCall where
  rpcSerializeInput   _ = wpSerialize
  rpcDeserializeOutput _ = wpDeserialize

instance SupportsClientRpc StreamingInputCall where
  rpcSerializeInput   _ = wpSerialize
  rpcDeserializeOutput _ = wpDeserialize

instance SupportsClientRpc FullDuplexCall where
  rpcSerializeInput   _ = wpSerialize
  rpcDeserializeOutput _ = wpDeserialize

instance SupportsClientRpc HalfDuplexCall where
  rpcSerializeInput   _ = wpSerialize
  rpcDeserializeOutput _ = wpDeserialize

instance SupportsClientRpc UnimplementedCall where
  rpcSerializeInput   _ = wpSerialize
  rpcDeserializeOutput _ = wpDeserialize

instance SupportsClientRpc UnimplementedServiceCall where
  rpcSerializeInput   _ = wpSerialize
  rpcDeserializeOutput _ = wpDeserialize

-- SupportsServerRpc instances
instance SupportsServerRpc EmptyCall where
  rpcDeserializeInput  _ = wpDeserialize
  rpcSerializeOutput   _ = wpSerialize

instance SupportsServerRpc UnaryCall where
  rpcDeserializeInput  _ = wpDeserialize
  rpcSerializeOutput   _ = wpSerialize

instance SupportsServerRpc StreamingOutputCall where
  rpcDeserializeInput  _ = wpDeserialize
  rpcSerializeOutput   _ = wpSerialize

instance SupportsServerRpc StreamingInputCall where
  rpcDeserializeInput  _ = wpDeserialize
  rpcSerializeOutput   _ = wpSerialize

instance SupportsServerRpc FullDuplexCall where
  rpcDeserializeInput  _ = wpDeserialize
  rpcSerializeOutput   _ = wpSerialize

instance SupportsServerRpc HalfDuplexCall where
  rpcDeserializeInput  _ = wpDeserialize
  rpcSerializeOutput   _ = wpSerialize

instance SupportsServerRpc UnimplementedCall where
  rpcDeserializeInput  _ = wpDeserialize
  rpcSerializeOutput   _ = wpSerialize
