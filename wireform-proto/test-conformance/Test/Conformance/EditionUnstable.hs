{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Conformance.EditionUnstable
  ( TestAllTypesEditionUnstable
  , testAllTypesEditionUnstableUnknownFields
  , unstableExtensionRegistry
  ) where

import Proto.Internal.JSON.Extension (ExtensionRegistry)
import Proto.TH (loadProto)


$(loadProto "test-conformance/protos/test_messages_edition_unstable.proto")


unstableExtensionRegistry :: ExtensionRegistry
unstableExtensionRegistry = registerExt_protobuf_test_messages_edition_unstable_extension_int32
