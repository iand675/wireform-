{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module THGoogleImports where

import Proto.TH (LoadOpts (..), defaultLoadOpts, loadProto, loadProtoWith)

$(loadProto "test-regression/proto/google/protobuf/audit_mode.proto")
$(loadProtoWith defaultLoadOpts {loIncludeDirs = ["test-regression/proto"]} "test-regression/proto/google_import.proto")
