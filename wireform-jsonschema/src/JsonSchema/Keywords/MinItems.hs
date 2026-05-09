{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'minItems' keyword
--
-- The minItems keyword requires that an array value has at least the
-- specified number of items.
module JsonSchema.Keywords.MinItems
  ( minItemsKeyword
  ) where

import Data.Aeson (Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Numeric.Natural (Natural)
import qualified Data.Scientific as Sci

import JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordNavigation(..), CompileFunc, ValidateFunc)
import JsonSchema.Types (Schema, validationFailure, ValidationAnnotations(..), ValidationResult, pattern ValidationSuccess)

-- | Compiled data for the 'minItems' keyword
newtype MinItemsData = MinItemsData Natural
  deriving (Show, Eq, Typeable)

-- | Compile function for 'minItems' keyword
compileMinItems :: CompileFunc MinItemsData
compileMinItems value _schema _ctx = case value of
  Number n | Sci.isInteger n && n >= 0 ->
    Right $ MinItemsData (fromInteger $ truncate n)
  _ -> Left "minItems must be a non-negative integer"

-- | Validate function for 'minItems' keyword
validateMinItems :: ValidateFunc MinItemsData
validateMinItems _recursiveValidator (MinItemsData minLen) _ctx (Array arr) =
  let arrLength = fromIntegral (length arr) :: Natural
  in if arrLength >= minLen
     then pure (ValidationSuccess mempty)
     else pure (validationFailure "minItems" $
                 "Array length " <> T.pack (show arrLength) <> " is less than minItems " <> T.pack (show minLen))
validateMinItems _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to arrays

-- | The 'minItems' keyword definition
minItemsKeyword :: KeywordDefinition
minItemsKeyword = KeywordDefinition
  { keywordName = "minItems"
  , keywordCompile = compileMinItems
  , keywordValidate = validateMinItems
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

