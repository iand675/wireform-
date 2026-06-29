-- | wireform-connect: the Connect RPC protocol for Haskell.
--
-- Import "Network.Connect.Server" to serve Connect RPCs and
-- "Network.Connect.Client" to call them. This umbrella re-exports the core
-- types; the service-method tags consumed by both come from
-- @wireform-grpc@'s @'Network.GRPC.Protobuf.TH.loadProtoServices'@ (the same
-- tags gRPC uses) — Connect is purely a new transport over them.
module Network.Connect
  ( -- * Re-exports
    module Network.Connect.Protocol
  , module Network.Connect.Error
  , module Network.Connect.Envelope
  , module Network.Connect.Metadata
  , module Network.Connect.Codec
  , module Network.Connect.Compression
  , module Network.Connect.Server
  , module Network.Connect.Client
  ) where

import Network.Connect.Codec
import Network.Connect.Compression
import Network.Connect.Envelope
import Network.Connect.Error
import Network.Connect.Metadata
import Network.Connect.Protocol
import Network.Connect.Server
import Network.Connect.Client
