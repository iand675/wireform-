{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generated protobuf types for gRPC interop testing.
module Proto.Interop where

import Data.Reflection (Given(..))
import Proto.Internal.JSON.Extension (ExtensionRegistry, emptyExtensionRegistry)
import Proto.TH

-- Proto3 messages have no extensions; satisfy the Generated JSON
-- instances' Given constraint with an empty registry.
instance Given ExtensionRegistry where
  given = emptyExtensionRegistry

$(loadProtoWith defaultLoadOpts
    { loIncludeDirs = ["test-conformance/proto/"]
    }
    "test-conformance/proto/grpc/testing/messages.proto")

$(loadProtoWith defaultLoadOpts
    { loIncludeDirs = ["test-conformance/proto/"]
    }
    "test-conformance/proto/grpc/testing/empty.proto")
