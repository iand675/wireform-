{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module THOptionImports where

import Language.Haskell.TH (recover)
import Proto.TH (LoadOpts (..), defaultLoadOpts, loadProtoWith)

$(loadProtoWith defaultLoadOpts {loIncludeDirs = ["test-regression/proto"]} "test-regression/proto/option_import.proto")

optionOnlyTypeRejected :: Bool
optionOnlyTypeRejected =
  $(recover
      [|True|]
      (loadProtoWith defaultLoadOpts {loIncludeDirs = ["test-regression/proto"]} "test-regression/proto/option_type_invalid.proto" >> [|False|])
   )

missingOptionImportRejected :: Bool
missingOptionImportRejected =
  $(recover
      [|True|]
      (loadProtoWith defaultLoadOpts {loIncludeDirs = []} "test-regression/proto/option_import.proto" >> [|False|])
   )
