{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-orphans #-}

-- | Generated message types + Connect/gRPC service tags for the
-- @connectrpc.eliza.v1.ElizaService@ test fixture.
--
-- @loadProto@ emits the message records (with proto3-JSON aeson instances and
-- the wire codec); @loadProtoServices@ emits the protocol-agnostic
-- @Protobuf ElizaService "<meth>"@ tags consumed unchanged by both gRPC and
-- Connect.
module Connect.TestProto where

import Network.GRPC.Protobuf.TH (loadProtoServices)
import Proto.TH (loadProto)

$(loadProto "test/proto/eliza.proto")

$(loadProtoServices "test/proto/eliza.proto")
