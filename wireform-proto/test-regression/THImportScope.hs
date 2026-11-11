{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module THImportScope where

import Language.Haskell.TH (recover)
import Proto.Gen.ImportStatus qualified
import Proto.TH (LoadOpts (..), defaultLoadOpts, loadProtoWith)

$(loadProtoWith defaultLoadOpts {loIncludeDirs = ["test-regression/proto"]} "test-regression/proto/import_scope.proto")

unresolvedImportRejected :: Bool
unresolvedImportRejected =
  $(recover
      [|True|]
      (loadProtoWith defaultLoadOpts {loIncludeDirs = []} "test-regression/proto/import_scope.proto" >> [|False|])
   )
