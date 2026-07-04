{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-orphans #-}

-- | Generated message types + Connect service tags for the @sim.echo.v1@
-- fixture used by the connect-explore fault campaign.
--
-- @loadProto@ emits the 'EchoMessage' record (proto wire codec + proto3-JSON
-- aeson) and @loadProtoServices@ emits the transport-agnostic
-- @Protobuf EchoService "Echo"@ tag (+ 'ServiceMethods' instance and default
-- 'NoMetadata') that the Connect server\/client consume unchanged.
module Connect.EchoProto where

import Network.GRPC.Protobuf.TH (loadProtoServices)
import Proto.TH (loadProto)

$(loadProto "test-connect-explore/proto/echo.proto")

$(loadProtoServices "test-connect-explore/proto/echo.proto")
