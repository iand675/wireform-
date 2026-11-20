{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Conformance.MigrationProto2
  ( TestAllTypesProto2
  , testAllTypesProto2UnknownFields
  , migrationProto2Registry
  ) where

import Proto.Internal.JSON.Extension (ExtensionRegistry)
import Proto.TH (loadProto)


$(loadProto "test-conformance/protos/test_messages_proto2_editions.proto")


migrationProto2Registry :: ExtensionRegistry
migrationProto2Registry =
  registerExt_protobuf_test_messages_editions_proto2_extension_int32
    <> registerExt_protobuf_test_messages_editions_proto2_extension_string
    <> registerExt_protobuf_test_messages_editions_proto2_extension_bytes
    <> registerExt_protobuf_test_messages_editions_proto2_groupfield
