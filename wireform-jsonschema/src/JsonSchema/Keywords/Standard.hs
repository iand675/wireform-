{-# LANGUAGE OverloadedStrings #-}
-- | Standard JSON Schema keywords implemented using the pluggable keyword system
--
-- This module re-exports all standard JSON Schema keywords from their
-- individual modules. These keywords are no longer privileged - they're
-- implemented exactly like custom keywords would be.
module JsonSchema.Keywords.Standard
  ( -- * Basic Keywords
    constKeyword
  , enumKeyword
  , typeKeyword
    -- * String Keywords
  , minLengthKeyword
  , maxLengthKeyword
  , patternKeyword
    -- * Numeric Keywords
  , minimumKeyword
  , maximumKeyword
  , multipleOfKeyword
  , exclusiveMinimumKeyword
  , exclusiveMaximumKeyword
    -- * Array Keywords
  , minItemsKeyword
  , maxItemsKeyword
  , uniqueItemsKeyword
  , itemsKeyword
  , prefixItemsKeyword
  , containsKeyword
  , minContainsKeyword
  , maxContainsKeyword
    -- * Object Keywords
  , requiredKeyword
  , minPropertiesKeyword
  , maxPropertiesKeyword
  , propertiesKeyword
  , patternPropertiesKeyword
  , additionalPropertiesKeyword
  , propertyNamesKeyword
  , dependentRequiredKeyword
  , dependentSchemasKeyword
  , unevaluatedPropertiesKeyword
  , unevaluatedItemsKeyword
  , formatKeyword
    -- * Conditional Keywords
  , ifKeyword
  , thenKeyword
  , elseKeyword
    -- * Registry
  , draft202012Registry
  , draft201909Registry
  , draft07Registry
  , draft06Registry
  , draft04Registry
  , standardKeywordRegistry
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import JsonSchema.Keyword (KeywordRegistry(..), emptyKeywordRegistry, registerKeyword)
-- Basic keywords
import JsonSchema.Keywords.Const (constKeyword)
import JsonSchema.Keywords.Enum (enumKeyword)
import JsonSchema.Keywords.Type (typeKeyword)
-- String keywords
import JsonSchema.Keywords.MinLength (minLengthKeyword)
import JsonSchema.Keywords.MaxLength (maxLengthKeyword)
import JsonSchema.Keywords.Pattern (patternKeyword)
-- Numeric keywords
import JsonSchema.Keywords.Minimum (minimumKeyword)
import JsonSchema.Keywords.Maximum (maximumKeyword)
import JsonSchema.Keywords.MultipleOf (multipleOfKeyword)
import JsonSchema.Keywords.ExclusiveMinimum (exclusiveMinimumKeyword)
import JsonSchema.Keywords.ExclusiveMaximum (exclusiveMaximumKeyword)
-- Array keywords
import JsonSchema.Keywords.MinItems (minItemsKeyword)
import JsonSchema.Keywords.MaxItems (maxItemsKeyword)
import JsonSchema.Keywords.UniqueItems (uniqueItemsKeyword)
import JsonSchema.Keywords.Items (itemsKeyword)
import JsonSchema.Keywords.PrefixItems (prefixItemsKeyword)
import JsonSchema.Keywords.Contains (containsKeyword, minContainsKeyword, maxContainsKeyword)
-- Object keywords
import JsonSchema.Keywords.Required (requiredKeyword)
import JsonSchema.Keywords.MinProperties (minPropertiesKeyword)
import JsonSchema.Keywords.MaxProperties (maxPropertiesKeyword)
import JsonSchema.Keywords.Properties (propertiesKeyword)
import JsonSchema.Keywords.PatternProperties (patternPropertiesKeyword)
import JsonSchema.Keywords.AdditionalProperties (additionalPropertiesKeyword)
import JsonSchema.Keywords.PropertyNames (propertyNamesKeyword)
import JsonSchema.Keywords.Dependencies (dependenciesKeyword)
import JsonSchema.Keywords.DependentRequired (dependentRequiredKeyword)
import JsonSchema.Keywords.DependentSchemas (dependentSchemasKeyword)
import JsonSchema.Keywords.UnevaluatedProperties (unevaluatedPropertiesKeyword)
import JsonSchema.Keywords.UnevaluatedItems (unevaluatedItemsKeyword)
import JsonSchema.Keywords.FormatKeyword (formatKeyword)
import JsonSchema.Keywords.ContentMediaType (contentMediaTypeKeyword)
import JsonSchema.Keywords.ContentEncoding (contentEncodingKeyword)

-- Conditional keywords
import JsonSchema.Keywords.Conditional (ifKeyword, thenKeyword, elseKeyword)
-- Navigable keywords (for $ref resolution)
import qualified JsonSchema.Keywords.Navigation as Nav
import qualified JsonSchema.Keywords.AllOf as AllOf
import qualified JsonSchema.Keywords.AnyOf as AnyOf
import qualified JsonSchema.Keywords.OneOf as OneOf
import qualified JsonSchema.Keywords.Not as Not
-- Draft-04 specific keywords
import qualified JsonSchema.Keywords.Draft04.Minimum as D04
import qualified JsonSchema.Keywords.Draft04.Maximum as D04

-- | Registry containing all keywords defined in Draft 2020-12
draft202012Registry :: KeywordRegistry
draft202012Registry =
  -- Basic validation
  registerKeyword constKeyword $
  registerKeyword enumKeyword $
  registerKeyword typeKeyword $
  -- String validation
  registerKeyword minLengthKeyword $
  registerKeyword maxLengthKeyword $
  registerKeyword patternKeyword $
  registerKeyword formatKeyword $
  registerKeyword contentMediaTypeKeyword $
  registerKeyword contentEncodingKeyword $
  -- Numeric validation
  registerKeyword minimumKeyword $
  registerKeyword maximumKeyword $
  registerKeyword multipleOfKeyword $
  registerKeyword exclusiveMinimumKeyword $
  registerKeyword exclusiveMaximumKeyword $
  -- Array validation
  registerKeyword minItemsKeyword $
  registerKeyword maxItemsKeyword $
  registerKeyword uniqueItemsKeyword $
  registerKeyword itemsKeyword $
  registerKeyword prefixItemsKeyword $
  registerKeyword containsKeyword $
  registerKeyword minContainsKeyword $
  registerKeyword maxContainsKeyword $
  -- Object validation
  registerKeyword requiredKeyword $
  registerKeyword minPropertiesKeyword $
  registerKeyword maxPropertiesKeyword $
  registerKeyword propertiesKeyword $
  registerKeyword patternPropertiesKeyword $
  registerKeyword additionalPropertiesKeyword $
  registerKeyword dependenciesKeyword $
  registerKeyword propertyNamesKeyword $
  registerKeyword dependentRequiredKeyword $
  registerKeyword dependentSchemasKeyword $
  registerKeyword unevaluatedPropertiesKeyword $
  registerKeyword unevaluatedItemsKeyword $
  -- Conditional keywords (if/then/else, Draft-07+)
  registerKeyword ifKeyword $
  registerKeyword thenKeyword $
  registerKeyword elseKeyword $
  -- Navigable keywords (for $ref resolution)
  registerKeyword AllOf.allOfKeyword $
  registerKeyword AnyOf.anyOfKeyword $
  registerKeyword OneOf.oneOfKeyword $
  registerKeyword Not.notKeyword $
  registerKeyword Nav.defsKeyword $
  emptyKeywordRegistry

-- | Registry for Draft 2019-09
-- 
-- Draft 2019-09 adds:
-- - dependentSchemas/dependentRequired (replaces dependencies)
-- - unevaluatedProperties/unevaluatedItems
-- - minContains/maxContains
-- - $recursiveRef/$recursiveAnchor
-- 
-- Does NOT include prefixItems (added in 2020-12)
draft201909Registry :: KeywordRegistry
draft201909Registry =
  -- Basic validation
  registerKeyword constKeyword $
  registerKeyword enumKeyword $
  registerKeyword typeKeyword $
  -- String validation
  registerKeyword minLengthKeyword $
  registerKeyword maxLengthKeyword $
  registerKeyword patternKeyword $
  registerKeyword formatKeyword $
  registerKeyword contentMediaTypeKeyword $
  registerKeyword contentEncodingKeyword $
  -- Numeric validation
  registerKeyword minimumKeyword $
  registerKeyword maximumKeyword $
  registerKeyword multipleOfKeyword $
  registerKeyword exclusiveMinimumKeyword $
  registerKeyword exclusiveMaximumKeyword $
  -- Array validation
  registerKeyword minItemsKeyword $
  registerKeyword maxItemsKeyword $
  registerKeyword uniqueItemsKeyword $
  registerKeyword itemsKeyword $
  registerKeyword containsKeyword $
  registerKeyword minContainsKeyword $
  registerKeyword maxContainsKeyword $
  -- Object validation
  registerKeyword requiredKeyword $
  registerKeyword minPropertiesKeyword $
  registerKeyword maxPropertiesKeyword $
  registerKeyword propertiesKeyword $
  registerKeyword patternPropertiesKeyword $
  registerKeyword additionalPropertiesKeyword $
  registerKeyword dependenciesKeyword $
  registerKeyword propertyNamesKeyword $
  registerKeyword dependentRequiredKeyword $
  registerKeyword dependentSchemasKeyword $
  registerKeyword unevaluatedPropertiesKeyword $
  registerKeyword unevaluatedItemsKeyword $
  -- Conditional keywords (if/then/else, Draft-07+)
  registerKeyword ifKeyword $
  registerKeyword thenKeyword $
  registerKeyword elseKeyword $
  -- Navigable keywords (for $ref resolution)
  registerKeyword AllOf.allOfKeyword $
  registerKeyword AnyOf.anyOfKeyword $
  registerKeyword OneOf.oneOfKeyword $
  registerKeyword Not.notKeyword $
  registerKeyword Nav.defsKeyword $
  emptyKeywordRegistry

-- | Registry for Draft-07
--
-- Draft-07 adds:
-- - if/then/else conditional keywords
-- - contentMediaType/contentEncoding
--
-- Does NOT include:
-- - dependentSchemas/dependentRequired (added in 2019-09)
-- - unevaluatedProperties/unevaluatedItems (added in 2019-09)
-- - minContains/maxContains (added in 2019-09)
-- - prefixItems (added in 2020-12)
draft07Registry :: KeywordRegistry
draft07Registry =
  -- Basic validation
  registerKeyword constKeyword $
  registerKeyword enumKeyword $
  registerKeyword typeKeyword $
  -- String validation
  registerKeyword minLengthKeyword $
  registerKeyword maxLengthKeyword $
  registerKeyword patternKeyword $
  registerKeyword formatKeyword $
  registerKeyword contentMediaTypeKeyword $
  registerKeyword contentEncodingKeyword $
  -- Numeric validation
  registerKeyword minimumKeyword $
  registerKeyword maximumKeyword $
  registerKeyword multipleOfKeyword $
  registerKeyword exclusiveMinimumKeyword $
  registerKeyword exclusiveMaximumKeyword $
  -- Array validation
  registerKeyword minItemsKeyword $
  registerKeyword maxItemsKeyword $
  registerKeyword uniqueItemsKeyword $
  registerKeyword itemsKeyword $
  registerKeyword containsKeyword $
  -- Object validation
  registerKeyword requiredKeyword $
  registerKeyword minPropertiesKeyword $
  registerKeyword maxPropertiesKeyword $
  registerKeyword propertiesKeyword $
  registerKeyword patternPropertiesKeyword $
  registerKeyword additionalPropertiesKeyword $
  registerKeyword dependenciesKeyword $
  registerKeyword propertyNamesKeyword $
  -- Conditional keywords (if/then/else, Draft-07+)
  registerKeyword ifKeyword $
  registerKeyword thenKeyword $
  registerKeyword elseKeyword $
  -- Navigable keywords (for $ref resolution)
  registerKeyword AllOf.allOfKeyword $
  registerKeyword AnyOf.anyOfKeyword $
  registerKeyword OneOf.oneOfKeyword $
  registerKeyword Not.notKeyword $
  registerKeyword Nav.defsKeyword $
  emptyKeywordRegistry

-- | Registry for Draft-06
--
-- Draft-06 adds:
-- - const keyword
-- - contains keyword
-- - propertyNames keyword
--
-- Does NOT include:
-- - if/then/else (added in Draft-07)
-- - contentMediaType/contentEncoding (added in Draft-07)
-- - dependentSchemas/dependentRequired (added in 2019-09)
-- - unevaluatedProperties/unevaluatedItems (added in 2019-09)
-- - minContains/maxContains (added in 2019-09)
-- - prefixItems (added in 2020-12)
draft06Registry :: KeywordRegistry
draft06Registry =
  -- Basic validation
  registerKeyword constKeyword $
  registerKeyword enumKeyword $
  registerKeyword typeKeyword $
  -- String validation
  registerKeyword minLengthKeyword $
  registerKeyword maxLengthKeyword $
  registerKeyword patternKeyword $
  registerKeyword formatKeyword $
  -- Numeric validation
  registerKeyword minimumKeyword $
  registerKeyword maximumKeyword $
  registerKeyword multipleOfKeyword $
  registerKeyword exclusiveMinimumKeyword $
  registerKeyword exclusiveMaximumKeyword $
  -- Array validation
  registerKeyword minItemsKeyword $
  registerKeyword maxItemsKeyword $
  registerKeyword uniqueItemsKeyword $
  registerKeyword itemsKeyword $
  registerKeyword containsKeyword $
  -- Object validation
  registerKeyword requiredKeyword $
  registerKeyword minPropertiesKeyword $
  registerKeyword maxPropertiesKeyword $
  registerKeyword propertiesKeyword $
  registerKeyword patternPropertiesKeyword $
  registerKeyword additionalPropertiesKeyword $
  registerKeyword dependenciesKeyword $
  registerKeyword propertyNamesKeyword $
  -- Navigable keywords (for $ref resolution)
  registerKeyword AllOf.allOfKeyword $
  registerKeyword AnyOf.anyOfKeyword $
  registerKeyword OneOf.oneOfKeyword $
  registerKeyword Not.notKeyword $
  registerKeyword Nav.defsKeyword $
  emptyKeywordRegistry

-- | Backwards-compatible alias for the latest registry
standardKeywordRegistry :: KeywordRegistry
standardKeywordRegistry = draft202012Registry

-- | Registry for Draft-04 schemas
--
-- Uses Draft-04 specific numeric keywords where exclusiveMinimum/Maximum
-- are boolean modifiers rather than standalone numeric keywords.
draft04Registry :: KeywordRegistry
draft04Registry =
  -- Basic validation
  registerKeyword constKeyword $
  registerKeyword enumKeyword $
  registerKeyword typeKeyword $
  -- String validation
  registerKeyword minLengthKeyword $
  registerKeyword maxLengthKeyword $
  registerKeyword patternKeyword $
  registerKeyword formatKeyword $
  -- Numeric validation (Draft-04 specific)
  registerKeyword D04.minimumKeyword $
  registerKeyword D04.maximumKeyword $
  registerKeyword D04.exclusiveMinimumKeyword $
  registerKeyword D04.exclusiveMaximumKeyword $
  registerKeyword multipleOfKeyword $
  -- Array validation
  registerKeyword minItemsKeyword $
  registerKeyword maxItemsKeyword $
  registerKeyword uniqueItemsKeyword $
  registerKeyword itemsKeyword $
  -- Object validation
  registerKeyword requiredKeyword $
  registerKeyword minPropertiesKeyword $
  registerKeyword maxPropertiesKeyword $
  registerKeyword propertiesKeyword $
  registerKeyword patternPropertiesKeyword $
  registerKeyword additionalPropertiesKeyword $
  registerKeyword dependenciesKeyword $
  -- Navigable keywords (for $ref resolution)
  registerKeyword AllOf.allOfKeyword $
  registerKeyword AnyOf.anyOfKeyword $
  registerKeyword OneOf.oneOfKeyword $
  registerKeyword Not.notKeyword $
  registerKeyword Nav.defsKeyword $
  emptyKeywordRegistry
