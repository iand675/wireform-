-- |
-- Module      : Network.Connect
-- Copyright   : (c) 2026 Ian Duncan
-- License     : BSD-3-Clause
--
-- @wireform-connect@ is a native Haskell implementation of the
-- [Connect RPC protocol](https://connectrpc.com/docs/protocol): a client and
-- server for unary and all three streaming RPC kinds, over both HTTP\/1.1
-- and HTTP\/2, with binary Protobuf and JSON codecs, GET support for
-- side-effect-free unary calls, @identity@\/@gzip@\/@br@\/@zstd@ compression,
-- the gRPC-derived error-code model, leading and trailing metadata, and the
-- streaming @EndStreamResponse@ envelope.
--
-- = When to use Connect
--
-- Connect speaks the same Protobuf services as gRPC but over plain HTTP:
-- no HTTP\/2 requirement, no custom framing for unary calls, and curl-\/browser-
-- friendly JSON. Reach for @wireform-connect@ when you want a gRPC-style
-- service that is also directly callable from any HTTP client; reach for
-- "Network.GRPC" when you need gRPC's wire compatibility specifically.
--
-- = A transport, not a codegen
--
-- Connect is purely a new transport over the existing service description.
-- The @'Network.GRPC.Spec.Protobuf' serv \"meth\"@ service-method tags that
-- @'Network.GRPC.Protobuf.TH.loadProtoServices'@ emits for gRPC, together
-- with the message types from @'Proto.TH.loadProto'@, drive Connect
-- /unchanged/ — there is no Connect-specific code generator.
--
-- = Getting started
--
-- Import "Network.Connect.Server" to serve Connect RPCs and
-- "Network.Connect.Client" to call them. This umbrella module re-exports the
-- full public surface; for streaming-handlers and call continuations see the
-- per-module docs.
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
