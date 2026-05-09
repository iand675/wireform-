{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Core types for the pluggable keyword system
--
-- This module defines the types that allow custom keywords to be registered
-- and used in JSON Schema validation, following the hyperjump architecture
-- pattern of compile-then-validate.
module JsonSchema.Keyword.Types
  (     -- * Keyword Definition
    KeywordDefinition(..)
  , KeywordNavigation(..)
  , CompileFunc
  , ValidateFunc
  , PostValidateFunc
  , LegacyValidateFunc
  , legacyValidate
    -- * Keyword Registry
  , KeywordRegistry(..)
    -- * Compilation Context
  , CompilationContext(..)
  , CompiledKeyword(..)
  , SomeCompiledData(..)
    -- * Validation Context
  , ValidationContext'(..)
    -- * Monadic Compilation
  , CompileM
  , CompilationState(..)
  , runCompileM
  , getCompilationState
  , modifyCompilationState
  , liftEither
    -- * Utilities
  , combineValidationResults
  ) where

import Control.Monad.Trans.State.Strict (StateT, runStateT, get, modify')
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (Reader)
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Typeable (Typeable)

import JsonSchema.Types
  ( Schema
  , ValidationResult
  , ValidationConfig
  , ValidationAnnotations(..)
  , validationFailure
  , pattern ValidationSuccess
  , pattern ValidationFailure
  )


-- | Compilation context provided to keyword compile functions
--
-- This follows hyperjump's pattern of giving compile functions access to:
-- - The schema registry for resolving references
-- - A function to resolve $ref URIs
-- - The current schema being compiled
-- - The parent schema path for error reporting
-- - The keyword registry for monadic compilation (optional)
data CompilationContext = CompilationContext
  { contextRegistry :: Map Text Schema
    -- ^ Access to the full schema registry (keyed by URI as Text)
  , contextResolveRef :: Text -> Either Text Schema
    -- ^ Function to resolve $ref URIs to schemas
  , contextCurrentSchema :: Schema
    -- ^ The schema currently being compiled
  , contextParentPath :: [Text]
    -- ^ Path from root to current schema (for error messages)
  , contextKeywordRegistry :: KeywordRegistry
    -- ^ Keyword registry for monadic compilation
    -- This allows compile functions to access adjacent keywords on-demand
  , contextParseSubschema :: Value -> Either Text Schema
    -- ^ Function to parse a subschema value with proper base URI context
    -- This preserves the parent schema's base URI for correct reference resolution
  }

-- | Compile function signature: processes keyword value at schema parse time
--
-- The compile function receives:
-- 1. The keyword's value from the schema (e.g., for "maxLength": 10, this is Number 10)
-- 2. The full schema containing this keyword
-- 3. A compilation context with registry access and resolveRef
--
-- Returns compiled data that will be passed to the validate function,
-- or an error message if compilation fails.
type CompileFunc a = Value -> Schema -> CompilationContext -> Either Text a

-- | Validate function signature: validates instance data using compiled keyword
--
-- The validate function receives:
-- 1. A recursive validator for subschemas (Schema -> Value -> ValidationResult)
-- 2. The compiled data from the compile phase
-- 3. A validation context (instance and schema paths)
-- 4. The instance value being validated
--
-- Returns validation errors in a Reader monad with ValidationConfig as the environment.
-- This allows keywords to access configuration (e.g., format assertion vs annotation mode)
-- without explicit parameter passing.
--
-- For simple keywords that don't need recursive validation (e.g., type, enum),
-- the recursive validator parameter can be ignored.
type ValidateFunc a = 
  (Schema -> Value -> ValidationResult)  -- ^ Recursive validator for subschemas
  -> a                                   -- ^ Compiled data
  -> ValidationContext'                  -- ^ Validation context (paths)
  -> Value                               -- ^ Instance value
  -> Reader ValidationConfig ValidationResult  -- ^ Validation result in Reader monad

-- | Post-validation function signature: validates with access to annotations from other keywords
--
-- This mechanism provides dependency-based keyword ordering, which is the correct
-- approach according to the JSON Schema specification. The spec does not define
-- keyword evaluation order, but some keywords have semantic dependencies:
--
-- * 'unevaluatedProperties' and 'unevaluatedItems' must run after keywords that
--   evaluate properties/items (like 'properties', 'patternProperties', 'items', etc.)
--   to see which properties/items were already evaluated.
--
-- * These keywords need access to annotations collected from earlier keywords
--   to determine what remains "unevaluated".
--
-- This is superior to priority-based ordering because:
-- 1. It's spec-compliant: dependencies are explicit and semantic, not arbitrary
-- 2. It's type-safe: keywords declare their dependencies via the function signature
-- 3. It's maintainable: no magic numbers or priority conflicts
-- 4. It matches other implementations: most JSON Schema validators use dependency-based ordering
--
-- Used for keywords like unevaluatedProperties/unevaluatedItems that need to know
-- which properties/items were evaluated by other keywords.
--
-- The function receives:
-- 1. A recursive validator for subschemas
-- 2. The compiled data from the compile phase
-- 3. A validation context (instance and schema paths)
-- 4. The instance value being validated
-- 5. Annotations collected from other keywords validated before this one
--
-- Returns validation result in a Reader monad with ValidationConfig as the environment.
type PostValidateFunc a =
  (Schema -> Value -> ValidationResult)  -- ^ Recursive validator for subschemas
  -> a                                   -- ^ Compiled data
  -> ValidationContext'                  -- ^ Validation context (paths)
  -> Value                               -- ^ Instance value
  -> ValidationAnnotations               -- ^ Annotations from previously validated keywords
  -> Reader ValidationConfig ValidationResult  -- ^ Validation result in Reader monad

-- | Legacy validate function signature returning error messages.
type LegacyValidateFunc a =
  (Schema -> Value -> ValidationResult)
  -> a
  -> ValidationContext'
  -> Value
  -> Reader ValidationConfig [Text]

-- | Combine multiple validation results, accumulating annotations and failures.
combineValidationResults :: [ValidationResult] -> ValidationResult
combineValidationResults = foldl combine (ValidationSuccess mempty)
  where
    combine (ValidationFailure e1) (ValidationFailure e2) = ValidationFailure (e1 <> e2)
    combine failure@(ValidationFailure _) _ = failure
    combine _ failure@(ValidationFailure _) = failure
    combine (ValidationSuccess anns1) (ValidationSuccess anns2) = ValidationSuccess (anns1 <> anns2)

-- | Helper to adapt legacy validators that return error messages into the new result type.
legacyValidate :: Text -> LegacyValidateFunc a -> ValidateFunc a
legacyValidate keyword legacyFn recursiveValidator compiledData ctx value = do
  errors <- legacyFn recursiveValidator compiledData ctx value
  let results =
        if null errors
          then [ValidationSuccess mempty]
          else map (validationFailure keyword) errors
  pure $ combineValidationResults results

-- | Validation context for keyword validators
--
-- Provides path information for error reporting during validation.
-- This is simpler than the main ValidationContext and used specifically
-- for keyword validation.
data ValidationContext' = ValidationContext'
  { kwContextInstancePath :: [Text]
    -- ^ Path to current location in instance (for error reporting)
  , kwContextSchemaPath :: [Text]
    -- ^ Path to current location in schema (for error reporting)
  }
  deriving (Show, Eq)

-- | Existentially quantified compiled keyword data
--
-- Allows storing compiled data of different types in the same collection,
-- while preserving type safety via Typeable.
data SomeCompiledData = forall a. Typeable a => SomeCompiledData a

-- | Result of compiling a keyword
data CompiledKeyword = CompiledKeyword
  { compiledKeywordName :: Text
    -- ^ Name of the keyword
  , compiledData :: SomeCompiledData
    -- ^ The compiled data (existentially quantified)
  , compiledValidate :: (Schema -> Value -> ValidationResult) -> ValidationContext' -> Value -> Reader ValidationConfig ValidationResult
    -- ^ Type-erased validate function (closed over compiled data)
    -- Takes: recursive validator, validation context, value
    -- Returns validation result in Reader monad with ValidationConfig
  , compiledPostValidate :: Maybe ((Schema -> Value -> ValidationResult) -> ValidationContext' -> Value -> ValidationAnnotations -> Reader ValidationConfig ValidationResult)
    -- ^ Optional post-validation function that receives annotations from other keywords
    -- If present, this keyword will be validated after regular keywords
  , compiledAdjacentData :: Map Text Value
    -- ^ Values from adjacent keywords accessed during compilation
  }

instance Show CompiledKeyword where
  show (CompiledKeyword name _ _ _ _) = "CompiledKeyword{" ++ show name ++ "}"

instance Eq CompiledKeyword where
  (CompiledKeyword name1 _ _ _ _) == (CompiledKeyword name2 _ _ _ _) = name1 == name2

-- ============================================================================
-- Monadic Compilation
-- ============================================================================

-- | Compilation state for monadic keyword compilation
--
-- Tracks compiled keywords and compilation progress to support:
-- - Lazy, on-demand compilation of adjacent keywords
-- - Memoization of compiled results
-- - Circular dependency detection
data CompilationState = CompilationState
  { stateCompiled :: Map Text CompiledKeyword
    -- ^ Keywords that have been compiled so far
  , stateCompiling :: Set Text
    -- ^ Keywords currently being compiled (for cycle detection)
  , stateSchema :: Schema
    -- ^ The schema currently being compiled
  , stateContext :: CompilationContext
    -- ^ Additional compilation context
  }

-- | Monadic compilation context
--
-- Provides stateful keyword compilation with:
-- - Access to already-compiled keywords
-- - Ability to request compilation of adjacent keywords
-- - Automatic memoization and cycle detection
newtype CompileM a = CompileM
  { unCompileM :: StateT CompilationState (Either Text) a
  }
  deriving newtype (Functor, Applicative, Monad)

-- | Run a monadic compilation
runCompileM :: CompileM a -> CompilationState -> Either Text (a, CompilationState)
runCompileM (CompileM action) state = runStateT action state

-- | Get the current compilation state
getCompilationState :: CompileM CompilationState
getCompilationState = CompileM get

-- | Modify the compilation state
modifyCompilationState :: (CompilationState -> CompilationState) -> CompileM ()
modifyCompilationState f = CompileM $ modify' f

-- | Lift an Either into the compilation monad
liftEither :: Either Text a -> CompileM a
liftEither = CompileM . lift

-- | Navigation modes for keywords that contain subschemas
--
-- Defines how JSON Pointer navigation should work for a keyword
data KeywordNavigation
  = NoNavigation
    -- ^ Keyword doesn't contain subschemas (e.g., type, enum, const)
  
  | SingleSchema (Schema -> Maybe Schema)
    -- ^ Single subschema (e.g., not, contains, propertyNames)
  
  | SchemaMap (Schema -> Maybe (Map Text Schema))
    -- ^ Map of subschemas keyed by Text (e.g., properties, dependentSchemas)
    -- Next pointer segment is used as map key
  
  | SchemaArray (Schema -> Maybe [Schema])
    -- ^ Array of subschemas (e.g., allOf, anyOf, oneOf)
    -- Next pointer segment must be numeric index
  
  | CustomNavigation (Schema -> Text -> [Text] -> Maybe (Schema, [Text]))
    -- ^ Custom navigation logic for complex cases (e.g., items with dual behavior)
    -- Takes: parent schema, current segment, remaining segments
    -- Returns: (resolved schema, remaining segments to process)

-- | Definition of a custom keyword
--
-- This is the main registration point for custom keywords. Users provide:
-- - A unique name for the keyword
-- - A compile function that processes the keyword value at schema parse time
-- - A validate function that uses the compiled data to validate instances
-- - Optional navigation support for subschema resolution
-- - Optional post-validation function for keywords that need annotation access
--
-- Note: Keywords handle their own applicability by pattern matching on the
-- instance Value type in their validate functions. They should return
-- ValidationSuccess mempty for non-applicable types (e.g., string keywords
-- return success for non-string values).
data KeywordDefinition = forall a. Typeable a => KeywordDefinition
  { keywordName :: Text
    -- ^ Unique identifier for this keyword (e.g., "maxLength", "x-creditCard")
  , keywordCompile :: CompileFunc a
    -- ^ Compile phase: process keyword value and schema structure
  , keywordValidate :: ValidateFunc a
    -- ^ Validate phase: check instance against compiled keyword
    -- Should pattern match on Value type and return ValidationSuccess mempty
    -- for non-applicable types (e.g., "-- Only applies to strings")
  , keywordNavigation :: KeywordNavigation
    -- ^ How to navigate into subschemas (if any)
  , keywordPostValidate :: Maybe (PostValidateFunc a)
    -- ^ Optional post-validation function that receives annotations from other keywords
    -- If provided, this keyword will be validated after regular keywords and will
    -- receive annotations collected from them. Used for unevaluatedProperties/Items.
  }

-- | Registry of custom keywords
--
-- Maps keyword names to their definitions. Used during schema parsing
-- and validation to look up custom keyword handlers.
newtype KeywordRegistry = KeywordRegistry
  { keywordMap :: Map Text KeywordDefinition
  }

instance Show KeywordRegistry where
  show (KeywordRegistry m) =
    "KeywordRegistry{keywords=" ++ show (Map.keys m) ++ "}"
