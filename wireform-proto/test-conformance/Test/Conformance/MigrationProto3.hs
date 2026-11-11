{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Conformance.MigrationProto3
  ( TestAllTypesProto3
  , testAllTypesProto3UnknownFields
  ) where

import Proto.TH (loadProto)


$(loadProto "test-conformance/protos/test_messages_proto3_editions.proto")
