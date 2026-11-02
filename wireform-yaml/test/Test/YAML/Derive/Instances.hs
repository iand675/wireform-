{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.YAML.Derive.Instances () where

import Test.YAML.Derive.Types
import YAML.Class (FromYAML, ToYAML)
import YAML.Derive


deriveYAML ''Profile
deriveYAML ''Tag
deriveYAML ''Color
deriveYAML ''Shape


deriveYAML ''OptionalRecord


deriveYAML ''DefaultRecord
deriveYAML ''CoercedRecord


deriveYAML ''OptionalCoerced


deriveYAML ''Box
deriveYAML ''WrappedF
deriveYAML ''Tree
deriveYAML ''Phantom


deriveYAML ''RecordChoice


instance ToYAML GenericRecord


instance FromYAML GenericRecord


deriveYAML ''CoercedBox
deriveYAML ''OptionalCoercedBox
