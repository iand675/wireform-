{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
-- | Implementation of the 'maxItems' keyword
--
-- The maxItems keyword requires that an array value has at most the
-- specified number of items.
module JsonSchema.Keywords.MaxItems
  ( maxItemsKeyword
  ) where

import Data.Aeson (Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Numeric.Natural (Natural)
import qualified Data.Scientific as Sci

import JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordNavigation(..), CompileFunc, ValidateFunc, LegacyValidateFunc, legacyValidate)
import JsonSchema.Types (Schema)

-- | Compiled data for the 'maxItems' keyword
newtype MaxItemsData = MaxItemsData Natural
  deriving (Show, Eq, Typeable)

-- | Compile function for 'maxItems' keyword
compileMaxItems :: CompileFunc MaxItemsData
compileMaxItems value _schema _ctx = case value of
  Number n | Sci.isInteger n && n >= 0 ->
    Right $ MaxItemsData (fromInteger $ truncate n)
  _ -> Left "maxItems must be a non-negative integer"

-- | Validate function for 'maxItems' keyword
validateMaxItems :: ValidateFunc MaxItemsData
validateMaxItems = legacyValidate "maxItems" validateMaxItemsLegacy

validateMaxItemsLegacy :: LegacyValidateFunc MaxItemsData
validateMaxItemsLegacy _recursiveValidator (MaxItemsData maxLen) _ctx (Array arr) =
  let arrLength = fromIntegral (length arr) :: Natural
  in if arrLength <= maxLen
     then pure []
     else pure ["Array length " <> T.pack (show arrLength) <> " exceeds maxItems " <> T.pack (show maxLen)]
validateMaxItemsLegacy _ _ _ _ = pure []  -- Only applies to arrays

-- | The 'maxItems' keyword definition
maxItemsKeyword :: KeywordDefinition
maxItemsKeyword = KeywordDefinition
  { keywordName = "maxItems"
  , keywordCompile = compileMaxItems
  , keywordValidate = validateMaxItems
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

