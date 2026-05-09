-- | Keyword registration and management API
--
-- This module provides the public API for registering custom keywords
-- and managing keyword registries. It enables users to extend JSON Schema
-- with domain-specific validation keywords.
--
-- Example usage:
--
-- @
-- -- Define a custom keyword for credit card validation
-- creditCardKeyword :: KeywordDefinition
-- creditCardKeyword = mkKeywordDefinition
--   "x-creditCard"
--   compileCreditCard
--   validateCreditCard
--
-- -- Register in a registry
-- registry <- registerKeyword creditCardKeyword emptyKeywordRegistry
-- @
module JsonSchema.Keyword
  ( -- * Keyword Registry
    KeywordRegistry(..)
  , emptyKeywordRegistry
  , registerKeyword
  , lookupKeyword
  , getRegisteredKeywords
    -- * Keyword Definition Helpers
  , mkKeywordDefinition
  , mkNavigableKeyword
  , mkSimpleKeyword
    -- * Re-exports from Types
  , KeywordDefinition(..)
  , KeywordNavigation(..)
  , CompileFunc
  , ValidateFunc
  , CompilationContext(..)
  , CompiledKeyword(..)
  , SomeCompiledData(..)
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Aeson (Value)

import JsonSchema.Keyword.Types
import JsonSchema.Types (Schema)

-- | Empty keyword registry with no custom keywords
emptyKeywordRegistry :: KeywordRegistry
emptyKeywordRegistry = KeywordRegistry Map.empty

-- | Register a custom keyword in the registry
--
-- If a keyword with the same name already exists, it will be replaced
-- with the new definition (allowing for keyword shadowing).
registerKeyword :: KeywordDefinition -> KeywordRegistry -> KeywordRegistry
registerKeyword kw@(KeywordDefinition name _ _ _ _) (KeywordRegistry m) =
  KeywordRegistry (Map.insert name kw m)

-- | Look up a keyword by name in the registry
--
-- Returns Nothing if the keyword is not registered.
lookupKeyword :: Text -> KeywordRegistry -> Maybe KeywordDefinition
lookupKeyword name (KeywordRegistry m) = Map.lookup name m

-- | Get all registered keyword names
--
-- Useful for debugging and introspection.
getRegisteredKeywords :: KeywordRegistry -> [Text]
getRegisteredKeywords (KeywordRegistry m) = Map.keys m

-- | Helper to create a keyword definition (no navigation)
--
-- This is a convenience function that wraps the KeywordDefinition constructor
-- with a more ergonomic API. For keywords without subschemas.
--
-- Note: Keywords handle their own applicability by pattern matching on Value
-- types in their validate functions. They should return ValidationSuccess mempty
-- for non-applicable types.
mkKeywordDefinition
  :: Typeable a
  => Text                    -- ^ Keyword name
  -> CompileFunc a           -- ^ Compile function
  -> ValidateFunc a          -- ^ Validate function (should handle applicability via pattern matching)
  -> KeywordDefinition
mkKeywordDefinition name compile validate = KeywordDefinition
  { keywordName = name
  , keywordCompile = compile
  , keywordValidate = validate
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

-- | Helper to create a navigable keyword definition
--
-- For keywords that contain subschemas (e.g., properties, allOf, not).
--
-- Note: Keywords handle their own applicability by pattern matching on Value
-- types in their validate functions. They should return ValidationSuccess mempty
-- for non-applicable types.
mkNavigableKeyword
  :: Typeable a
  => Text                    -- ^ Keyword name
  -> CompileFunc a           -- ^ Compile function
  -> ValidateFunc a          -- ^ Validate function (should handle applicability via pattern matching)
  -> KeywordNavigation       -- ^ Navigation support
  -> KeywordDefinition
mkNavigableKeyword name compile validate nav = KeywordDefinition
  { keywordName = name
  , keywordCompile = compile
  , keywordValidate = validate
  , keywordNavigation = nav
  , keywordPostValidate = Nothing
  }

-- | Create a simple keyword that doesn't need compilation
--
-- For keywords that just validate the instance directly without needing
-- a compilation phase, this helper creates a trivial compile function
-- that just passes through the keyword value.
--
-- Note: Keywords handle their own applicability by pattern matching on Value
-- types in their validate functions. They should return ValidationSuccess mempty
-- for non-applicable types.
mkSimpleKeyword
  :: Typeable a
  => Text                    -- ^ Keyword name
  -> (Value -> Either Text a)  -- ^ Parse keyword value
  -> (a -> Value -> [Text])    -- ^ Validate function (old signature for backward compat)
  -> KeywordDefinition
mkSimpleKeyword name parseValue validateValue =
  KeywordDefinition
    { keywordName = name
    , keywordCompile = \val _schema _ctx -> parseValue val
    , keywordValidate = legacyValidate name (\_ compiledData _ val -> pure (validateValue compiledData val))
    , keywordNavigation = NoNavigation
    , keywordPostValidate = Nothing
    }
