{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
-- | Implementation of the 'minProperties' keyword
--
-- The minProperties keyword requires that an object value has at least
-- the specified number of properties.
module JsonSchema.Keywords.MinProperties
  ( minPropertiesKeyword
  ) where

import Data.Aeson (Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Numeric.Natural (Natural)
import qualified Data.Scientific as Sci
import qualified Data.Aeson.KeyMap as KeyMap

import JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordNavigation(..), CompileFunc, ValidateFunc, LegacyValidateFunc, legacyValidate)
import JsonSchema.Types (Schema)

-- | Compiled data for the 'minProperties' keyword
newtype MinPropertiesData = MinPropertiesData Natural
  deriving (Show, Eq, Typeable)

-- | Compile function for 'minProperties' keyword
compileMinProperties :: CompileFunc MinPropertiesData
compileMinProperties value _schema _ctx = case value of
  Number n | Sci.isInteger n && n >= 0 ->
    Right $ MinPropertiesData (fromInteger $ truncate n)
  _ -> Left "minProperties must be a non-negative integer"

-- | Validate function for 'minProperties' keyword
validateMinProperties :: ValidateFunc MinPropertiesData
validateMinProperties = legacyValidate "minProperties" validateMinPropertiesLegacy

validateMinPropertiesLegacy :: LegacyValidateFunc MinPropertiesData
validateMinPropertiesLegacy _recursiveValidator (MinPropertiesData minProps) _ctx (Object objMap) =
  let propCount = fromIntegral (KeyMap.size objMap) :: Natural
  in if propCount >= minProps
     then pure []
     else pure ["Object has " <> T.pack (show propCount) <> " properties, but minProperties is " <> T.pack (show minProps)]
validateMinPropertiesLegacy _ _ _ _ = pure []  -- Only applies to objects

-- | The 'minProperties' keyword definition
minPropertiesKeyword :: KeywordDefinition
minPropertiesKeyword = KeywordDefinition
  { keywordName = "minProperties"
  , keywordCompile = compileMinProperties
  , keywordValidate = validateMinProperties
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

