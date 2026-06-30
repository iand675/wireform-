{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generated Haskell types for the connectrpc conformance protos
-- (@connectrpc.conformance.v1@), vendored verbatim from connectrpc/conformance
-- v1.0.5 under @conformance/proto/@.
--
-- All four files are spliced in one module in dependency order. The
-- @config.proto@ types are generated first; @service.proto@,
-- @client_compat.proto@, and @server_compat.proto@ reference them
-- across the proto @import@ boundary. 'loadProtoWith' follows those
-- imports (via 'loIncludeDirs') purely to build the resolution scope,
-- so cross-file enum references (e.g. @Error.code : Code@) are correctly
-- classified as enums on the wire. @google.protobuf.*@ references resolve
-- through the bridge's WKT registry.
module Connect.Conformance.Proto where

import Proto.TH (LoadOpts (..), defaultLoadOpts, loadProtoWith)

$(loadProtoWith defaultLoadOpts {loIncludeDirs = ["conformance/proto", "."]} "conformance/proto/connectrpc/conformance/v1/config.proto")

$(loadProtoWith defaultLoadOpts {loIncludeDirs = ["conformance/proto", "."]} "conformance/proto/connectrpc/conformance/v1/service.proto")

$(loadProtoWith defaultLoadOpts {loIncludeDirs = ["conformance/proto", "."]} "conformance/proto/connectrpc/conformance/v1/client_compat.proto")

$(loadProtoWith defaultLoadOpts {loIncludeDirs = ["conformance/proto", "."]} "conformance/proto/connectrpc/conformance/v1/server_compat.proto")
