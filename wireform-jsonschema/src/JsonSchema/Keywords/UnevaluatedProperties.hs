{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'unevaluatedProperties' keyword (Draft 2019-09+)
--
-- The unevaluatedProperties keyword applies to properties that were not evaluated
-- by other keywords (properties, patternProperties, additionalProperties, or applicator keywords).
-- This is a complex keyword that requires annotation tracking across the validation process.
module JsonSchema.Keywords.UnevaluatedProperties
  ( unevaluatedPropertiesKeyword
  , compileUnevaluatedProperties
  , UnevaluatedPropertiesData(..)
  , validateUnevaluatedPropertiesWithAnnotations
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (sconcat)

import JsonSchema.Types 
  ( Schema(..), SchemaCore(..), SchemaObject(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , ValidationAnnotations(..), ValidationErrors(..)
  , validationUnevaluatedProperties, schemaValidation
  , emptyPointer
  )
import JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc, PostValidateFunc
  , ValidationContext'(..), KeywordNavigation(..)
  , LegacyValidateFunc, legacyValidate
  )
import JsonSchema.Validator.Annotations (annotateProperties)
import JsonSchema.Keyword.Types (CompilationContext(..), contextParseSubschema)
import JsonSchema.Keywords.Common (extractPropertyNames)

-- | Compiled data for unevaluatedProperties keyword
newtype UnevaluatedPropertiesData = UnevaluatedPropertiesData Schema
  deriving (Typeable)

-- | Compile the unevaluatedProperties keyword
compileUnevaluatedProperties :: CompileFunc UnevaluatedPropertiesData
compileUnevaluatedProperties value _schema ctx = case contextParseSubschema ctx value of
  Left err -> Left $ "Invalid schema in unevaluatedProperties: " <> err
  Right schema -> Right $ UnevaluatedPropertiesData schema

-- | Validate unevaluatedProperties using the pluggable keyword system
--
-- NOTE: This is a placeholder implementation. Full validation requires annotation
-- tracking from all other keywords. The actual validation is done via
-- validateUnevaluatedPropertiesPost which is called after other keywords.
validateUnevaluatedPropertiesKeyword :: LegacyValidateFunc UnevaluatedPropertiesData
validateUnevaluatedPropertiesKeyword _recursiveValidator (UnevaluatedPropertiesData _schema) _ctx _val =
  -- This keyword is validated separately after other keywords have been validated
  -- and their annotations collected. See validateUnevaluatedPropertiesPost.
  pure []

-- | Post-validation function for unevaluatedProperties
-- This receives annotations from other keywords and validates unevaluated properties
validateUnevaluatedPropertiesPost :: PostValidateFunc UnevaluatedPropertiesData
validateUnevaluatedPropertiesPost recursiveValidator (UnevaluatedPropertiesData unevalSchema) _ctx (Object objMap) collectedAnnotations =
  -- Extract evaluated properties from annotations
  let evaluatedProps = extractEvaluatedProperties collectedAnnotations
      -- All properties in the object
      allProps = extractPropertyNames objMap
      -- Unevaluated properties are those not in the evaluated set
      unevaluatedProps = Set.difference allProps evaluatedProps
  in if Set.null unevaluatedProps
     -- No unevaluated properties - always succeed (even if schema is false)
     then pure $ ValidationSuccess mempty
     else
       -- Validate unevaluated properties against the schema
       let results = do
             propName <- Set.toList unevaluatedProps
             Just propValue <- pure $ KeyMap.lookup (Key.fromText propName) objMap
             pure $ recursiveValidator unevalSchema propValue
           failures = do
             ValidationFailure errs <- results
             pure errs
           annotations = do
             ValidationSuccess anns <- results
             pure anns
       in case failures of
         [] -> pure $ ValidationSuccess $ annotateProperties unevaluatedProps <> mconcat annotations
         failures' -> pure $ ValidationFailure $ sconcat (NE.fromList failures')
validateUnevaluatedPropertiesPost _ _ _ _ _ = pure $ ValidationSuccess mempty  -- Only applies to objects

-- | Validate unevaluatedProperties with access to collected annotations
--
-- This function is called after other keywords have been validated and their
-- annotations collected. It extracts evaluated properties from annotations
-- and validates unevaluated ones against the schema.
validateUnevaluatedPropertiesWithAnnotations
  :: (Schema -> Value -> ValidationResult)  -- ^ Recursive validator
  -> SchemaObject                            -- ^ Schema object containing unevaluatedProperties
  -> KeyMap.KeyMap Value                     -- ^ Object properties to validate
  -> ValidationAnnotations                   -- ^ Annotations collected from other keywords
  -> ValidationResult
validateUnevaluatedPropertiesWithAnnotations recursiveValidator obj objMap collectedAnnotations =
  case validationUnevaluatedProperties (schemaValidation obj) of
    Nothing -> ValidationSuccess mempty
    Just unevalSchema ->
      let -- Extract evaluated properties from annotations
          evaluatedProps = extractEvaluatedProperties collectedAnnotations
          -- All properties in the object
          allProps = extractPropertyNames objMap
          -- Unevaluated properties are those not in the evaluated set
          unevaluatedProps = Set.difference allProps evaluatedProps
          -- Validate unevaluated properties against the schema
          results = do
            propName <- Set.toList unevaluatedProps
            Just propValue <- pure $ KeyMap.lookup (Key.fromText propName) objMap
            pure $ recursiveValidator unevalSchema propValue
          failures = do
            ValidationFailure errs <- results
            pure errs
          annotations = do
            ValidationSuccess anns <- results
            pure anns
      in case failures of
        [] -> ValidationSuccess $ annotateProperties unevaluatedProps <> mconcat annotations
        failures' -> ValidationFailure $ sconcat (NE.fromList failures')

-- | Extract evaluated properties from collected annotations
-- Only considers annotations at the current instance location (empty pointer)
extractEvaluatedProperties :: ValidationAnnotations -> Set Text
extractEvaluatedProperties (ValidationAnnotations annMap) =
  -- Only look at annotations at the current location (empty pointer)
  case Map.lookup emptyPointer annMap of
    Nothing -> Set.empty
    Just innerMap -> case Map.lookup "properties" innerMap of
      Just (Aeson.Array arr) -> Set.fromList
        [ txt
        | Aeson.String txt <- toList arr
        ]
      _ -> Set.empty

-- | Keyword definition for unevaluatedProperties
unevaluatedPropertiesKeyword :: KeywordDefinition
unevaluatedPropertiesKeyword = KeywordDefinition
  { keywordName = "unevaluatedProperties"
  , keywordCompile = compileUnevaluatedProperties
  , keywordValidate = legacyValidate "unevaluatedProperties" validateUnevaluatedPropertiesKeyword
  , keywordNavigation = SingleSchema $ \schema -> case schemaCore schema of
      ObjectSchema obj -> validationUnevaluatedProperties (schemaValidation obj)
      _ -> Nothing
  , keywordPostValidate = Just validateUnevaluatedPropertiesPost
  }

