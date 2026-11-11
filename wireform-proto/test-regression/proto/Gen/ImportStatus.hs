{-# LANGUAGE TemplateHaskell #-}

module Proto.Gen.ImportStatus where

import Proto.TH (loadProto)

$(loadProto "test-regression/proto/import_status.proto")
