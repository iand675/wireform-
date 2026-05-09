{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'uniqueItems' keyword
--
-- The uniqueItems keyword requires that all items in an array are unique
-- when set to true. When false, no constraint is applied.
module JsonSchema.Keywords.UniqueItems
  ( uniqueItemsKeyword
  ) where

import Data.Aeson (Value(..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Foldable (toList)
import qualified Data.Set as Set

import JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordNavigation(..), CompileFunc, ValidateFunc)
import JsonSchema.Types (Schema, validationFailure, ValidationAnnotations(..), ValidationResult, pattern ValidationSuccess)

-- | Compiled data for the 'uniqueItems' keyword
newtype UniqueItemsData = UniqueItemsData Bool
  deriving (Show, Eq, Typeable)

-- | Compile function for 'uniqueItems' keyword
compileUniqueItems :: CompileFunc UniqueItemsData
compileUniqueItems value _schema _ctx = case value of
  Bool b -> Right $ UniqueItemsData b
  _ -> Left "uniqueItems must be a boolean"

-- | Validate function for 'uniqueItems' keyword
validateUniqueItems :: ValidateFunc UniqueItemsData
validateUniqueItems _recursiveValidator (UniqueItemsData True) _ctx (Array arr) =
  let items = toList arr
      uniqueItems = length items == length (nubOrd items)
  in if uniqueItems
     then pure (ValidationSuccess mempty)
     else pure (validationFailure "uniqueItems" "Array contains duplicate items")
  where
    -- Simple deduplication using Ord (works for most JSON values)
    nubOrd :: Ord a => [a] -> [a]
    nubOrd = Set.toList . Set.fromList
validateUniqueItems _ (UniqueItemsData False) _ _ = pure (ValidationSuccess mempty)  -- uniqueItems: false means no constraint
validateUniqueItems _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to arrays when true

-- | The 'uniqueItems' keyword definition
uniqueItemsKeyword :: KeywordDefinition
uniqueItemsKeyword = KeywordDefinition
  { keywordName = "uniqueItems"
  , keywordCompile = compileUniqueItems
  , keywordValidate = validateUniqueItems
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

