{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : Proto.API.Payments
Description : gRPC service binding for @payments.v1.PaymentService@.

wireform-proto generates the /message/ types from @payments.proto@, but it
does not yet generate the gRPC /service/ glue, so — exactly like the
vendored @grapesy@ examples (@Proto.API.Helloworld@,
@Proto.API.Interop@) — we hand-write the per-method RPC instances here.

There is a single unary method, 'CreatePayment'. The instances tell the
transport how to (de)serialise the request/response through the
wireform-proto codec and pin the streaming flavour to @NonStreaming@.
-}
module Proto.API.Payments (
  PaymentService,
  CreatePayment,
  module Proto.Payments,
) where

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Spec (
  HasStreamingType (..),
  Input,
  IsRPC (..),
  NoMetadata,
  Output,
  RequestMetadata,
  ResponseInitialMetadata,
  ResponseTrailingMetadata,
  ServiceMethods,
  SupportsClientRpc (..),
  SupportsServerRpc (..),
  SupportsStreamingType,
 )
import Network.GRPC.Spec.Util.Protobuf qualified as Protobuf
import Proto.Payments


{- | Phantom tag for the service. Methods are attached via the 'Protobuf'
RPC type and the 'ServiceMethods' instance below.
-}
data PaymentService


-- | The one unary RPC: create a payment, get back an acknowledgement.
type CreatePayment = Protobuf PaymentService "createPayment"


type instance Input CreatePayment = Proto PaymentRequest


type instance Output CreatePayment = Proto PaymentResponse


instance IsRPC CreatePayment where
  rpcContentType _ = "application/grpc+proto"
  rpcServiceName _ = "payments.v1.PaymentService"
  rpcMethodName _ = "CreatePayment"
  rpcMessageType _ = Just "payments.v1.PaymentRequest"


instance SupportsClientRpc CreatePayment where
  rpcSerializeInput _ = Protobuf.buildLazy . getProto
  rpcDeserializeOutput _ = fmap Proto . Protobuf.parseLazy


instance SupportsServerRpc CreatePayment where
  rpcDeserializeInput _ = fmap Proto . Protobuf.parseLazy
  rpcSerializeOutput _ = Protobuf.buildLazy . getProto


instance SupportsStreamingType CreatePayment 'NonStreaming


instance HasStreamingType CreatePayment where
  type RpcStreamingType CreatePayment = 'NonStreaming


type instance RequestMetadata (Protobuf PaymentService meth) = NoMetadata


type instance ResponseInitialMetadata (Protobuf PaymentService meth) = NoMetadata


type instance ResponseTrailingMetadata (Protobuf PaymentService meth) = NoMetadata


type instance ServiceMethods PaymentService = '["createPayment"]
