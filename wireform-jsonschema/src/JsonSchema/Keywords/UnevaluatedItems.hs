{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'unevaluatedItems' keyword (Draft 2019-09+)
--
-- The unevaluatedItems keyword applies to array items that were not evaluated
-- by other keywords (items, prefixItems, contains, or applicator keywords).
-- This is a complex keyword that requires annotation tracking across the validation process.
module JsonSchema.Keywords.UnevaluatedItems
  ( unevaluatedItemsKeyword
  , compileUnevaluatedItems
  , UnevaluatedItemsData(..)
  , validateUnevaluatedItemsWithAnnotations
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import Control.Monad.Reader (Reader)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import Data.Foldable (toList)
import Control.Monad (guard)
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as Sci
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (sconcat)

import JsonSchema.Types 
  ( Schema(..), SchemaCore(..), SchemaObject(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , ValidationAnnotations(..), ValidationErrors(..)
  , validationUnevaluatedItems, schemaValidation
  , emptyPointer, validationError
  )
import JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc, PostValidateFunc
  , ValidationContext'(..), KeywordNavigation(..)
  , LegacyValidateFunc, legacyValidate
  )
import JsonSchema.Validator.Annotations (annotateItems)
import JsonSchema.Keyword.Types (CompilationContext(..), contextParseSubschema)

-- | Compiled data for unevaluatedItems keyword
newtype UnevaluatedItemsData = UnevaluatedItemsData Schema
  deriving (Typeable)

-- | Compile the unevaluatedItems keyword
compileUnevaluatedItems :: CompileFunc UnevaluatedItemsData
compileUnevaluatedItems value _schema ctx = case contextParseSubschema ctx value of
  Left err -> Left $ "Invalid schema in unevaluatedItems: " <> err
  Right schema -> Right $ UnevaluatedItemsData schema

-- | Validate unevaluatedItems using the pluggable keyword system
--
-- NOTE: This is a placeholder implementation. Full validation requires annotation
-- tracking from all other keywords. The actual validation is done via
-- validateUnevaluatedItemsPost which is called after other keywords.
validateUnevaluatedItemsKeyword :: LegacyValidateFunc UnevaluatedItemsData
validateUnevaluatedItemsKeyword _recursiveValidator (UnevaluatedItemsData _schema) _ctx _val =
  -- This keyword is validated separately after other keywords have been validated
  -- and their annotations collected. See validateUnevaluatedItemsPost.
  pure []

-- | Post-validation function for unevaluatedItems
-- This receives annotations from other keywords and validates unevaluated items
validateUnevaluatedItemsPost :: PostValidateFunc UnevaluatedItemsData
validateUnevaluatedItemsPost recursiveValidator (UnevaluatedItemsData unevalSchema) _ctx (Array arr) collectedAnnotations =
  -- Extract evaluated item indices from annotations
  let evaluatedIndices = extractEvaluatedItems collectedAnnotations
      -- All item indices
      allIndices = Set.fromList [0 .. V.length arr - 1]
      -- Unevaluated items are those not in the evaluated set
      unevaluatedIndices = Set.difference allIndices evaluatedIndices
  in case schemaCore unevalSchema of
    BooleanSchema False ->
      -- No unevaluated items allowed - fail if there are any unevaluated items
      if Set.null unevaluatedIndices
      then pure $ ValidationSuccess mempty
      else pure $ ValidationFailure $ ValidationErrors $ pure $
           validationError "unevaluatedItems is false, no unevaluated items allowed"
    BooleanSchema True ->
      -- All unevaluated items allowed - mark them as evaluated and succeed
      if Set.null unevaluatedIndices
      then pure $ ValidationSuccess mempty
      else pure $ ValidationSuccess $ annotateItems unevaluatedIndices
    _ ->
      -- Validate unevaluated items against the schema
      if Set.null unevaluatedIndices
      then pure $ ValidationSuccess mempty
      else
        let results = do
              idx <- Set.toList unevaluatedIndices
              guard (idx < V.length arr)  -- Safety check
              pure $ recursiveValidator unevalSchema (arr V.! idx)
            failures = do
              ValidationFailure errs <- results
              pure errs
            annotations = do
              ValidationSuccess anns <- results
              pure anns
        in case failures of
          [] -> pure $ ValidationSuccess $ annotateItems unevaluatedIndices <> mconcat annotations
          failures' -> pure $ ValidationFailure $ sconcat (NE.fromList failures')
validateUnevaluatedItemsPost _ _ _ _ _ = pure $ ValidationSuccess mempty  -- Only applies to arrays

-- | Validate unevaluatedItems with access to collected annotations
--
-- This function is called after other keywords have been validated and their
-- annotations collected. It extracts evaluated item indices from annotations
-- and validates unevaluated ones against the schema.
validateUnevaluatedItemsWithAnnotations
  :: (Schema -> Value -> ValidationResult)  -- ^ Recursive validator
  -> SchemaObject                            -- ^ Schema object containing unevaluatedItems
  -> V.Vector Value                          -- ^ Array items to validate
  -> ValidationAnnotations                   -- ^ Annotations collected from other keywords
  -> ValidationResult
validateUnevaluatedItemsWithAnnotations recursiveValidator obj arr collectedAnnotations =
  case validationUnevaluatedItems (schemaValidation obj) of
    Nothing -> ValidationSuccess mempty
    Just unevalSchema ->
      let -- Extract evaluated item indices from annotations
          evaluatedIndices = extractEvaluatedItems collectedAnnotations
          -- All item indices
          allIndices = Set.fromList [0 .. V.length arr - 1]
          -- Unevaluated items are those not in the evaluated set
          unevaluatedIndices = Set.difference allIndices evaluatedIndices
          -- Validate unevaluated items against the schema
          results = do
            idx <- Set.toList unevaluatedIndices
            guard (idx < V.length arr)  -- Safety check
            pure $ recursiveValidator unevalSchema (arr V.! idx)
          failures = do
            ValidationFailure errs <- results
            pure errs
          annotations = do
            ValidationSuccess anns <- results
            pure anns
      in case failures of
        [] -> ValidationSuccess $ annotateItems unevaluatedIndices <> mconcat annotations
        failures' -> ValidationFailure $ sconcat (NE.fromList failures')

-- | Extract evaluated items indices from collected annotations
-- Only considers annotations at the current instance location (empty pointer)
extractEvaluatedItems :: ValidationAnnotations -> Set Int
extractEvaluatedItems (ValidationAnnotations annMap) =
  -- Only look at annotations at the current location (empty pointer)
  case Map.lookup emptyPointer annMap of
    Nothing -> Set.empty
    Just innerMap -> case Map.lookup "items" innerMap of
      Just (Aeson.Array arr) -> Set.fromList
        [ idx
        | Aeson.Number n <- toList arr
        , Just idx <- [Sci.toBoundedInteger n]
        ]
      _ -> Set.empty

-- | Keyword definition for unevaluatedItems
unevaluatedItemsKeyword :: KeywordDefinition
unevaluatedItemsKeyword = KeywordDefinition
  { keywordName = "unevaluatedItems"
  , keywordCompile = compileUnevaluatedItems
  , keywordValidate = legacyValidate "unevaluatedItems" validateUnevaluatedItemsKeyword
  , keywordNavigation = SingleSchema $ \schema -> case schemaCore schema of
      ObjectSchema obj -> validationUnevaluatedItems (schemaValidation obj)
      _ -> Nothing
  , keywordPostValidate = Just validateUnevaluatedItemsPost
  }

