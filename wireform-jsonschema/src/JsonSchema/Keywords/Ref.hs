{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of reference keywords: $ref, $dynamicRef, $recursiveRef
--
-- These keywords point to other schemas. The resolution and validation logic
-- is intentionally left to the validator core because it requires deep integration
-- with:
-- - Schema registry and base URI tracking
-- - Dynamic scope management (for $dynamicRef/$recursiveRef)
-- - Recursion depth tracking
-- - Cross-draft handling
--
-- This module only defines the keyword placeholders so that these special keywords
-- are recognized by the keyword registry and don't get filtered out during
-- schema processing. The actual validation logic remains in Validator.hs.
module JsonSchema.Keywords.Ref
  ( refKeyword
  , dynamicRefKeyword
  , recursiveRefKeyword
  ) where

import Data.Aeson (Value(..))
import Data.Typeable (Typeable)

import JsonSchema.Types 
  ( Reference(..)
  , ValidationResult, pattern ValidationSuccess
  )
import JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..)
  )

-- | Compiled data for ref keywords
newtype RefData = RefData Reference
  deriving (Typeable)

-- | Compile a reference keyword
compileRefKeyword :: CompileFunc RefData
compileRefKeyword (String refText) _schema _ctx =
  Right $ RefData (Reference refText)
compileRefKeyword _ _ _ =
  Left "Reference keyword must be a string"

-- | Validate function placeholder
--
-- Actual validation is handled by validateAgainstObject's validateRef helper
-- which has access to full validation context including registry, dynamic scope, etc.
validateRefPlaceholder :: ValidateFunc RefData
validateRefPlaceholder _recursiveValidator _refData _ctx _val =
  pure $ ValidationSuccess mempty

-- | Keyword definition for $ref
refKeyword :: KeywordDefinition
refKeyword = KeywordDefinition
  { keywordName = "$ref"
  , keywordCompile = compileRefKeyword
  , keywordValidate = validateRefPlaceholder
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

-- | Keyword definition for $dynamicRef (2020-12+)
dynamicRefKeyword :: KeywordDefinition
dynamicRefKeyword = KeywordDefinition
  { keywordName = "$dynamicRef"
  , keywordCompile = compileRefKeyword
  , keywordValidate = validateRefPlaceholder
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

-- | Keyword definition for $recursiveRef (2019-09)
recursiveRefKeyword :: KeywordDefinition
recursiveRefKeyword = KeywordDefinition
  { keywordName = "$recursiveRef"
  , keywordCompile = compileRefKeyword
  , keywordValidate = validateRefPlaceholder
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

