{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Generated @ConformanceService@ RPC tags (@Protobuf "ConformanceService"
-- "meth"@) for the six conformance endpoints, via @loadProtoServices@. The
-- message types these methods reference all live in @service.proto@ and are
-- brought into scope by importing "Connect.Conformance.Proto" (which splices
-- them). The same tags drive both the Connect client and server.
module Connect.Conformance.Service where

import Connect.Conformance.Proto
import Network.GRPC.Protobuf.TH (loadProtoServices)

$(loadProtoServices "conformance/proto/connectrpc/conformance/v1/service.proto")
