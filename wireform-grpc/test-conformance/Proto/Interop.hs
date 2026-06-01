{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generated protobuf types for gRPC interop testing.
--
-- UndecidableInstances is required because the TH-generated ToJSON/FromJSON
-- instances carry a @Given ExtensionRegistry@ constraint (for proto2 extension
-- JSON support). At call sites, use @give registry $ ...@ to satisfy it.
module Proto.Interop where

import Proto.TH

$(loadProtoWith defaultLoadOpts
    { loIncludeDirs = ["test-conformance/proto/"]
    }
    "test-conformance/proto/grpc/testing/messages.proto")

$(loadProtoWith defaultLoadOpts
    { loIncludeDirs = ["test-conformance/proto/"]
    }
    "test-conformance/proto/grpc/testing/empty.proto")
