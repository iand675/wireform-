{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module THImportScopeExplicit where

import Data.Map.Strict qualified as Map
import Proto.Gen.ImportStatus qualified as Remote
import Proto.TH (LoadOpts (..), defaultLoadOpts, loadProtoWithTypeNames)

$(loadProtoWithTypeNames
    defaultLoadOpts {loIncludeDirs = ["test-regression/proto"]}
    (Map.singleton "example.imported.ImportStatus" ''Remote.ImportStatus)
    "test-regression/proto/import_scope.proto"
 )
