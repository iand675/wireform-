{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ServiceCodegen (testServiceCodegen) where

import Control.Monad (unless)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (Refl))
import Network.GRPC.Common.Protobuf (Proto)
import Network.GRPC.Protobuf.TH (loadProtoServices)
import Network.GRPC.Spec (Input, Output, ServiceMethods, rpcMessageType, rpcMethodName, rpcServiceName)
import Proto.TH (loadProto)


$(loadProto "test-minimal/service_codegen.proto")
$(loadProtoServices "test-minimal/service_codegen.proto")


testServiceCodegen :: IO ()
testServiceCodegen = do
  let types :: (Input Ping :~: Proto First'Request, Output Ping :~: Proto Second'Request,
                Input Ping''Alpha'ping :~: Proto Second'Request, Input Ping''Beta'Ping :~: Proto Second'Request)
      types = (Refl, Refl, Refl, Refl)
      methods :: (ServiceMethods Alpha :~: '["ping", "ping''ping"], ServiceMethods Idle :~: '[])
      methods = (Refl, Refl)
  case (types, methods) of
    ((Refl, Refl, Refl, Refl), (Refl, Refl)) ->
      unless
        ( rpcServiceName (Proxy @Ping) == "example.rpc_demo.Alpha"
            && rpcServiceName (Proxy @Ping''Beta'Ping) == "example.rpc_demo.Beta"
            && rpcMethodName (Proxy @Ping''Alpha'ping) == "ping"
            && rpcMethodName (Proxy @Ping''Beta'Ping) == "Ping"
            && rpcMessageType (Proxy @Ping) == Just "example.rpc_demo.First.Request"
            && rpcMessageType (Proxy @Ping''Alpha'ping) == Just "example.rpc_demo.Second.Request"
            && rpcMessageType (Proxy @Ping''Beta'Ping) == Just "example.rpc_demo.Second.Request"
        )
        (fail "gRPC service codegen changed the proto service, method, or message name")
