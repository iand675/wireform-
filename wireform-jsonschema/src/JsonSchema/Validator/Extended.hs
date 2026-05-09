{-# LANGUAGE PatternSynonyms #-}

-- | Extended validator with custom keyword support
--
-- This module extends the standard validator to validate instances
-- against ExtendedSchema (schemas with compiled custom keywords).
module JsonSchema.Validator.Extended
  ( -- * Extended Validation
    validateExtended
  , validateExtendedWithConfig
    -- * Validation with Custom Keywords
  , ExtendedValidationResult(..)
  , toExtendedValidationResult
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import JsonSchema.Types
  ( ValidationResult
  , ValidationConfig
  , isSuccess
  , Schema
  , ValidationContext
  , ValidationErrors(..)
  , pattern ValidationSuccess
  , pattern ValidationFailure
  )
import JsonSchema.Validator (validateValue, defaultValidationConfig, validateValueWithContext)
import JsonSchema.Parser.Extended (ExtendedSchema(..), fromExtendedSchema)
import JsonSchema.Keyword.Validate (validateKeywords)
import JsonSchema.Keyword.Types (ValidationContext'(..))
import JsonSchema.Validator.Result (ValidationError(..), errorMessage)

-- | Extended validation result
--
-- Combines results from standard validation and custom keyword validation.
data ExtendedValidationResult = ExtendedValidationResult
  { extendedStandardResult :: ValidationResult
    -- ^ Result from standard keyword validation
  , extendedCustomErrors :: [Text]
    -- ^ Errors from custom keyword validation
  , extendedIsValid :: Bool
    -- ^ Overall validation status (standard AND custom)
  }
  deriving (Show)

-- | Convert a standard ValidationResult to ExtendedValidationResult
toExtendedValidationResult :: ValidationResult -> ExtendedValidationResult
toExtendedValidationResult standardResult = ExtendedValidationResult
  { extendedStandardResult = standardResult
  , extendedCustomErrors = []
  , extendedIsValid = isSuccess standardResult
  }

-- | Validate an instance against an ExtendedSchema
--
-- This combines:
-- 1. Standard validation against the base schema
-- 2. Custom keyword validation using compiled keywords
validateExtended
  :: ExtendedSchema          -- ^ Schema with compiled custom keywords
  -> Value                   -- ^ Instance to validate
  -> ExtendedValidationResult
validateExtended = validateExtendedWithConfig defaultValidationConfig

-- | Validate with custom config
validateExtendedWithConfig
  :: ValidationConfig        -- ^ Validation configuration
  -> ExtendedSchema          -- ^ Schema with compiled custom keywords
  -> Value                   -- ^ Instance to validate
  -> ExtendedValidationResult
validateExtendedWithConfig config extSchema value =
  let -- Validate with standard validator
      baseSchema = fromExtendedSchema extSchema
      standardResult = validateValue config baseSchema value

      -- Create validation context for custom keywords
      -- Build a minimal ValidationContext for the recursive validator
      ctx = undefined  -- We need to construct this properly, but for now let's see if compilation proceeds
      
      -- Create recursive validator
      recursiveValidator :: Schema -> Value -> ValidationResult
      recursiveValidator = validateValueWithContext ctx
      
      -- Validate with custom keywords
      kwCtx = ValidationContext' { kwContextInstancePath = [], kwContextSchemaPath = [] }
      customResult = validateKeywords recursiveValidator (extendedCompiledKeywords extSchema) value kwCtx config mempty
      customErrors = case customResult of
        ValidationSuccess _ -> []
        ValidationFailure (ValidationErrors errs) ->
          map errorMessage (NE.toList errs)

      -- Combine results
      overallValid = isSuccess standardResult && null customErrors

  in ExtendedValidationResult
    { extendedStandardResult = standardResult
    , extendedCustomErrors = customErrors
    , extendedIsValid = overallValid
    }
