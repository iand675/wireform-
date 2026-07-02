{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-unused-imports -Wno-orphans -Wno-unused-matches #-}
module Proto.Interop where

import Proto.TH (loadProto)
import Data.Reflection (Given(..))
import Proto.Internal.JSON.Extension (ExtensionRegistry, emptyExtensionRegistry)

import GHC.OverloadedLabels (IsLabel(..))
import Proto.Lens (Lens', field)
import Proto.Schema (HasField)

import Proto.Google.Protobuf.Any qualified
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Spec (SupportsStreamingType, HasStreamingType(..), SupportsClientRpc(..), SupportsServerRpc(..), IsRPC(..), Input, Output, NoMetadata, RequestMetadata, ResponseInitialMetadata, ResponseTrailingMetadata, ServiceMethods)
import Control.Monad.Catch (MonadThrow(throwM))
import Control.Monad.State (StateT, execStateT, modify)
import Data.Default (Default(def))
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.Bifunctor (first)
import Proto.Encode (encodeMessage)
import Proto.Decode (decodeMessage)

import Network.GRPC.Spec.Util.Protobuf qualified as Protobuf

instance Given ExtensionRegistry where
  given = emptyExtensionRegistry

instance (HasField msg name a, Functor f) => IsLabel name ((a -> f a) -> msg -> f msg) where
  fromLabel = field @name

data TestService
data UnimplementedService
data PingService

$(loadProto "proto/empty.proto")
$(loadProto "proto/messages.proto")
$(loadProto "proto/test.proto")
$(loadProto "proto/ping.proto")

type EmptyCall           = Protobuf TestService "emptyCall"
type UnaryCall           = Protobuf TestService "unaryCall"
type StreamingInputCall  = Protobuf TestService "streamingInputCall"
type StreamingOutputCall = Protobuf TestService "streamingOutputCall"
type FullDuplexCall      = Protobuf TestService "fullDuplexCall"
type UnimplementedCall   = Protobuf TestService "unimplementedCall"
type UnimplementedServiceCall = Protobuf UnimplementedService "unimplementedCall"
type Ping = Protobuf PingService "ping"

type instance Input EmptyCall = Proto Empty
type instance Output EmptyCall = Proto Empty
instance IsRPC EmptyCall where
  rpcContentType _ = "application/grpc+proto"
  rpcServiceName _ = "grpc.testing.TestService"
  rpcMethodName _ = "EmptyCall"
  rpcMessageType _ = Just "grpc.testing.Empty"
instance SupportsClientRpc EmptyCall where
  rpcSerializeInput _ = Protobuf.buildLazy . getProto
  rpcDeserializeOutput _ = fmap Proto . Protobuf.parseLazy
instance SupportsServerRpc EmptyCall where
  rpcDeserializeInput _ = fmap Proto . Protobuf.parseLazy
  rpcSerializeOutput _ = Protobuf.buildLazy . getProto
instance SupportsStreamingType EmptyCall 'NonStreaming
instance HasStreamingType EmptyCall where
  type RpcStreamingType EmptyCall = 'NonStreaming

type instance Input UnaryCall = Proto SimpleRequest
type instance Output UnaryCall = Proto SimpleResponse
instance IsRPC UnaryCall where
  rpcContentType _ = "application/grpc+proto"
  rpcServiceName _ = "grpc.testing.TestService"
  rpcMethodName _ = "UnaryCall"
  rpcMessageType _ = Just "grpc.testing.SimpleRequest"
instance SupportsClientRpc UnaryCall where
  rpcSerializeInput _ = Protobuf.buildLazy . getProto
  rpcDeserializeOutput _ = fmap Proto . Protobuf.parseLazy
instance SupportsServerRpc UnaryCall where
  rpcDeserializeInput _ = fmap Proto . Protobuf.parseLazy
  rpcSerializeOutput _ = Protobuf.buildLazy . getProto
instance SupportsStreamingType UnaryCall 'NonStreaming
instance HasStreamingType UnaryCall where
  type RpcStreamingType UnaryCall = 'NonStreaming

type instance Input StreamingInputCall = Proto StreamingInputCallRequest
type instance Output StreamingInputCall = Proto StreamingInputCallResponse
instance IsRPC StreamingInputCall where
  rpcContentType _ = "application/grpc+proto"
  rpcServiceName _ = "grpc.testing.TestService"
  rpcMethodName _ = "StreamingInputCall"
  rpcMessageType _ = Just "grpc.testing.StreamingInputCallRequest"
instance SupportsClientRpc StreamingInputCall where
  rpcSerializeInput _ = Protobuf.buildLazy . getProto
  rpcDeserializeOutput _ = fmap Proto . Protobuf.parseLazy
instance SupportsServerRpc StreamingInputCall where
  rpcDeserializeInput _ = fmap Proto . Protobuf.parseLazy
  rpcSerializeOutput _ = Protobuf.buildLazy . getProto
instance SupportsStreamingType StreamingInputCall 'ClientStreaming
instance HasStreamingType StreamingInputCall where
  type RpcStreamingType StreamingInputCall = 'ClientStreaming

type instance Input StreamingOutputCall = Proto StreamingOutputCallRequest
type instance Output StreamingOutputCall = Proto StreamingOutputCallResponse
instance IsRPC StreamingOutputCall where
  rpcContentType _ = "application/grpc+proto"
  rpcServiceName _ = "grpc.testing.TestService"
  rpcMethodName _ = "StreamingOutputCall"
  rpcMessageType _ = Just "grpc.testing.StreamingOutputCallRequest"
instance SupportsClientRpc StreamingOutputCall where
  rpcSerializeInput _ = Protobuf.buildLazy . getProto
  rpcDeserializeOutput _ = fmap Proto . Protobuf.parseLazy
instance SupportsServerRpc StreamingOutputCall where
  rpcDeserializeInput _ = fmap Proto . Protobuf.parseLazy
  rpcSerializeOutput _ = Protobuf.buildLazy . getProto
instance SupportsStreamingType StreamingOutputCall 'ServerStreaming
instance HasStreamingType StreamingOutputCall where
  type RpcStreamingType StreamingOutputCall = 'ServerStreaming

type instance Input FullDuplexCall = Proto StreamingOutputCallRequest
type instance Output FullDuplexCall = Proto StreamingOutputCallResponse
instance IsRPC FullDuplexCall where
  rpcContentType _ = "application/grpc+proto"
  rpcServiceName _ = "grpc.testing.TestService"
  rpcMethodName _ = "FullDuplexCall"
  rpcMessageType _ = Just "grpc.testing.StreamingOutputCallRequest"
instance SupportsClientRpc FullDuplexCall where
  rpcSerializeInput _ = Protobuf.buildLazy . getProto
  rpcDeserializeOutput _ = fmap Proto . Protobuf.parseLazy
instance SupportsServerRpc FullDuplexCall where
  rpcDeserializeInput _ = fmap Proto . Protobuf.parseLazy
  rpcSerializeOutput _ = Protobuf.buildLazy . getProto
instance SupportsStreamingType FullDuplexCall 'BiDiStreaming
instance HasStreamingType FullDuplexCall where
  type RpcStreamingType FullDuplexCall = 'BiDiStreaming

type instance Input UnimplementedCall = Proto Empty
type instance Output UnimplementedCall = Proto Empty
instance IsRPC UnimplementedCall where
  rpcContentType _ = "application/grpc+proto"
  rpcServiceName _ = "grpc.testing.TestService"
  rpcMethodName _ = "UnimplementedCall"
  rpcMessageType _ = Just "grpc.testing.Empty"
instance SupportsClientRpc UnimplementedCall where
  rpcSerializeInput _ = Protobuf.buildLazy . getProto
  rpcDeserializeOutput _ = fmap Proto . Protobuf.parseLazy
instance SupportsServerRpc UnimplementedCall where
  rpcDeserializeInput _ = fmap Proto . Protobuf.parseLazy
  rpcSerializeOutput _ = Protobuf.buildLazy . getProto
instance SupportsStreamingType UnimplementedCall 'NonStreaming
instance HasStreamingType UnimplementedCall where
  type RpcStreamingType UnimplementedCall = 'NonStreaming

type instance RequestMetadata          (Protobuf TestService meth) = InteropReqMeta
type instance ResponseInitialMetadata  (Protobuf TestService meth) = InteropRespInitMeta
type instance ResponseTrailingMetadata (Protobuf TestService meth) = InteropRespTrailMeta

data InteropReqMeta = InteropReqMeta {
      interopExpectInit :: Maybe Strict.ByteString
    , interopExpectTrail :: Maybe Strict.ByteString
    }
  deriving (Show, Eq)

newtype InteropRespInitMeta = InteropRespInitMeta {
      interopActualInit :: Maybe Strict.ByteString
    }
  deriving (Show, Eq)

newtype InteropRespTrailMeta = InteropRespTrailMeta {
      interopActualTrail :: Maybe Strict.ByteString
    }
  deriving (Show, Eq)

grpcTestEchoInitial :: HeaderName
grpcTestEchoInitial = "x-grpc-test-echo-initial"

grpcTestEchoTrailingBin :: HeaderName
grpcTestEchoTrailingBin = "x-grpc-test-echo-trailing-bin"

instance Default InteropReqMeta where
  def = InteropReqMeta {
        interopExpectInit  = Nothing
      , interopExpectTrail = Nothing
      }

instance BuildMetadata InteropReqMeta where
  buildMetadata md = concat [
        [ CustomMetadata grpcTestEchoInitial val
        | Just val <- [interopExpectInit md]
        ]
      , [ CustomMetadata grpcTestEchoTrailingBin val
        | Just val <- [interopExpectTrail md]
        ]
      ]

instance ParseMetadata InteropRespInitMeta where
  parseMetadata headers =
      case headers of
        [] ->
          return $ InteropRespInitMeta $ Nothing
        [md] | customMetadataName md == grpcTestEchoInitial ->
          return $ InteropRespInitMeta $ Just (customMetadataValue md)
        _otherwise ->
          throwM $ UnexpectedMetadata headers

instance ParseMetadata InteropRespTrailMeta where
  parseMetadata headers =
      case headers of
        [] ->
          return $ InteropRespTrailMeta $ Nothing
        [md] | customMetadataName md == grpcTestEchoTrailingBin ->
          return $ InteropRespTrailMeta $ Just (customMetadataValue md)
        _otherwise ->
          throwM $ UnexpectedMetadata headers

instance Default InteropRespInitMeta where
  def = InteropRespInitMeta Nothing

instance Default InteropRespTrailMeta where
  def = InteropRespTrailMeta Nothing

instance ParseMetadata InteropReqMeta where
  parseMetadata = flip execStateT def . mapM go
    where
      go :: MonadThrow m => CustomMetadata -> StateT InteropReqMeta m ()
      go md
        | customMetadataName md == grpcTestEchoInitial
        = modify $ \x -> x{interopExpectInit = Just $ customMetadataValue md}

        | customMetadataName md == grpcTestEchoTrailingBin
        = modify $ \x -> x{interopExpectTrail = Just $ customMetadataValue md}

        | otherwise
        = throwM $ UnexpectedMetadata [md]

instance BuildMetadata InteropRespInitMeta where
  buildMetadata md = concat [
        [ CustomMetadata grpcTestEchoInitial val
        | Just val <- [interopActualInit md]
        ]
      ]

instance BuildMetadata InteropRespTrailMeta where
  buildMetadata md = concat [
        [ CustomMetadata grpcTestEchoTrailingBin val
        | Just val <- [interopActualTrail md]
        ]
      ]

instance StaticMetadata InteropRespTrailMeta where
  metadataHeaderNames _ = [grpcTestEchoTrailingBin]

type UnimplementedServiceCall' = Protobuf UnimplementedService "unimplementedCall"
type instance Input UnimplementedServiceCall = Proto Empty
type instance Output UnimplementedServiceCall = Proto Empty
instance IsRPC UnimplementedServiceCall where
  rpcContentType _ = "application/grpc+proto"
  rpcServiceName _ = "grpc.testing.UnimplementedService"
  rpcMethodName _ = "UnimplementedCall"
  rpcMessageType _ = Just "grpc.testing.Empty"
instance SupportsClientRpc UnimplementedServiceCall where
  rpcSerializeInput _ = Protobuf.buildLazy . getProto
  rpcDeserializeOutput _ = fmap Proto . Protobuf.parseLazy
instance SupportsServerRpc UnimplementedServiceCall where
  rpcDeserializeInput _ = fmap Proto . Protobuf.parseLazy
  rpcSerializeOutput _ = Protobuf.buildLazy . getProto
instance SupportsStreamingType UnimplementedServiceCall 'NonStreaming
instance HasStreamingType UnimplementedServiceCall where
  type RpcStreamingType UnimplementedServiceCall = 'NonStreaming

type instance RequestMetadata          (Protobuf UnimplementedService meth) = NoMetadata
type instance ResponseInitialMetadata  (Protobuf UnimplementedService meth) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf UnimplementedService meth) = NoMetadata

type instance Input Ping = Proto PingMessage
type instance Output Ping = Proto PongMessage
instance IsRPC Ping where
  rpcContentType _ = "application/grpc+proto"
  rpcServiceName _ = "grpc.testing.PingService"
  rpcMethodName _ = "ping"
  rpcMessageType _ = Just "grpc.testing.PingMessage"
instance SupportsClientRpc Ping where
  rpcSerializeInput _ = Protobuf.buildLazy . getProto
  rpcDeserializeOutput _ = fmap Proto . Protobuf.parseLazy
instance SupportsServerRpc Ping where
  rpcDeserializeInput _ = fmap Proto . Protobuf.parseLazy
  rpcSerializeOutput _ = Protobuf.buildLazy . getProto
instance SupportsStreamingType Ping 'NonStreaming
instance HasStreamingType Ping where
  type RpcStreamingType Ping = 'NonStreaming

type instance RequestMetadata          (Protobuf PingService meth) = NoMetadata
type instance ResponseInitialMetadata  (Protobuf PingService meth) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf PingService meth) = NoMetadata

type instance ServiceMethods PingService = '["ping"]
type instance ServiceMethods TestService = '["cacheableUnaryCall", "emptyCall", "fullDuplexCall", "halfDuplexCall", "streamingInputCall", "streamingOutputCall", "unaryCall", "unimplementedCall"]
type instance ServiceMethods UnimplementedService = '["unimplementedCall"]
