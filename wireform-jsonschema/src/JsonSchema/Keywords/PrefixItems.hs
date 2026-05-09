{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'prefixItems' keyword (Draft 2020-12+)
--
-- The prefixItems keyword validates array items positionally. Each schema
-- in the prefixItems array validates the corresponding item at that index.
-- Items beyond the prefixItems array length are validated by the 'items' keyword.
module JsonSchema.Keywords.PrefixItems
  ( prefixItemsKeyword
  , compilePrefixItems
  , PrefixItemsData(..)
  ) where

import Data.Aeson (Value(..))
import qualified Data.Vector as V
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Typeable (Typeable)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable (toList)
import Control.Monad (guard)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Semigroup (sconcat)

import JsonSchema.Types 
  ( Schema(..), SchemaCore(..), SchemaObject(..), ArrayItemsValidation(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , validationPrefixItems, validationItems, schemaValidation, schemaRawKeywords
  , JsonSchemaVersion(..), schemaVersion, schemaCore
  , ValidationErrors(..), validationError
  )
import qualified JsonSchema.Parser.Internal as ParserInternal
import JsonSchema.Parser.Internal (parseSchemaValue)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , KeywordNavigation(..)
  )
import JsonSchema.Parser.Internal (parseSchema)
import JsonSchema.Validator.Annotations
  ( annotateItems
  , arrayIndexPointer
  , shiftAnnotations
  )

-- | Compiled data for prefixItems keyword
data PrefixItemsData = PrefixItemsData
  { prefixSchemas :: NonEmpty Schema
  , prefixAdditional :: Maybe ArrayItemsValidation
  }
  deriving (Typeable)

-- | Compile the prefixItems keyword
compilePrefixItems :: CompileFunc PrefixItemsData
compilePrefixItems value schema _ctx = case value of
  Array arr | not (V.null arr) -> do
    parsedSchemas <- mapM parseSchemaElem (V.toList arr)
    case NE.nonEmpty parsedSchemas of
      Nothing -> Left "prefixItems must contain at least one schema"
      Just schemas' ->
        case schemaCore schema of
          BooleanSchema _ -> Left "prefixItems cannot appear in boolean schemas"
          ObjectSchema obj ->
            -- Parse items on-demand if not pre-parsed
            let itemsValidation = case validationItems (schemaValidation obj) of
                  Just items -> Just items
                  Nothing -> parseItemsFromRaw schema
            in Right $ PrefixItemsData schemas' itemsValidation
  _ -> Left "prefixItems must be a non-empty array of schemas"
  where
    parseSchemaElem v = case parseSchema v of
      Left err -> Left $ "Invalid schema in prefixItems: " <> T.pack (show err)
      Right s -> Right s
    
    parseItemsFromRaw :: Schema -> Maybe ArrayItemsValidation
    parseItemsFromRaw s = case Map.lookup "items" (schemaRawKeywords s) of
      Just (Array arr) -> do
        -- Tuple-style items
        let version = fromMaybe Draft202012 (schemaVersion s)
            schemas = do
              val <- toList arr
              Right schema <- pure $ parseSchemaValue version val
              pure schema
        case NE.nonEmpty schemas of
          Just ne -> Just (ItemsTuple ne Nothing)
          Nothing -> Nothing
      Just val -> do
        -- Single schema
        let version = fromMaybe Draft202012 (schemaVersion s)
        case parseSchemaValue version val of
          Right schema -> Just (ItemsSchema schema)
          Left _ -> Nothing
      _ -> Nothing

-- | Validate prefixItems using the pluggable keyword system
validatePrefixItemsKeyword :: ValidateFunc PrefixItemsData
validatePrefixItemsKeyword recursiveValidator (PrefixItemsData schemas additional) _ctx (Array arr) =
  let prefixEvaluations =
        [ (idx, recursiveValidator schema item)
        | (idx, (schema, item)) <- zip [0..] (zip (NE.toList schemas) (toList arr))
        ]
      prefixFailures = do
        (_, ValidationFailure errs) <- prefixEvaluations
        pure errs
      prefixIndices = Set.fromList $ map fst prefixEvaluations
      prefixAnnotations = do
        (idx, ValidationSuccess anns) <- prefixEvaluations
        pure $ shiftAnnotations (arrayIndexPointer idx) anns
      startIdx = length schemas
      remainingItems = drop startIdx (toList arr)
      (additionalFailures, additionalIndices, additionalAnnotations) =
        case additional of
          Just (ItemsSchema itemSchema) ->
            -- Check if additionalItems is false (no additional items allowed)
            case schemaCore itemSchema of
              BooleanSchema False ->
                -- No additional items allowed - fail validation if there are any additional items
                if null remainingItems
                then ([], Set.empty, [])
                else let errs = ValidationErrors $ pure $ validationError "additionalItems is false, no additional items allowed"
                     in ([errs], Set.empty, [])
              BooleanSchema True ->
                -- All additional items allowed - mark them as evaluated
                let additionalIndices = Set.fromList [startIdx .. startIdx + length remainingItems - 1]
                in ([], additionalIndices, [])
              _ ->
                -- Validate additional items against the schema
                let evals =
                      [ (idx, recursiveValidator itemSchema item)
                      | (idx, item) <- zip [startIdx..] remainingItems
                      ]
                    failures = do
                      (_, ValidationFailure errs) <- evals
                      pure errs
                    indices = if null evals then Set.empty else Set.fromList [startIdx .. startIdx + length evals - 1]
                    annotations = do
                      (idx, ValidationSuccess anns) <- evals
                      pure $ shiftAnnotations (arrayIndexPointer idx) anns
                in (failures, indices, annotations)
          Just (ItemsTuple tupleSchemas maybeAdditional) ->
            -- Treat tuple-style additional validation similar to legacy semantics
            let tupleEvals =
                  [ (idx, recursiveValidator schema' item)
                  | (idx, (schema', item)) <- zip [startIdx..] (zip (NE.toList tupleSchemas) remainingItems)
                  ]
                tupleFailures = do
                  (_, ValidationFailure errs) <- tupleEvals
                  pure errs
                tupleIndices = Set.fromList $ map fst tupleEvals
                tupleAnnotations = do
                  (idx, ValidationSuccess anns) <- tupleEvals
                  pure $ shiftAnnotations (arrayIndexPointer idx) anns
                extraItems = drop (length tupleSchemas) remainingItems
                extraStart = startIdx + length tupleSchemas
                (extraFailures, extraIndices, extraAnnotations) = case maybeAdditional of
                  Just addlSchema ->
                    -- Check if additionalItems is false
                    case schemaCore addlSchema of
                      BooleanSchema False ->
                        -- No additional items allowed - fail validation if there are any additional items
                        if null extraItems
                        then ([], Set.empty, [])
                        else let errs = ValidationErrors $ pure $ validationError "additionalItems is false, no additional items allowed"
                             in ([errs], Set.empty, [])
                      BooleanSchema True ->
                        -- All additional items allowed - no validation needed
                        ([], Set.empty, [])
                      _ ->
                        -- Validate additional items against the schema
                        let evals = do
                              (idx, item) <- zip [extraStart..] extraItems
                              pure (idx, recursiveValidator addlSchema item)
                            failures = do
                              (_, ValidationFailure errs) <- evals
                              pure errs
                            indices = if null evals then Set.empty else Set.fromList [extraStart .. extraStart + length evals - 1]
                            annotations = do
                              (idx, ValidationSuccess anns) <- evals
                              pure $ shiftAnnotations (arrayIndexPointer idx) anns
                        in (failures, indices, annotations)
                  Nothing -> ([], Set.empty, [])
            in (tupleFailures <> extraFailures, tupleIndices <> extraIndices, tupleAnnotations <> extraAnnotations)
          Nothing -> ([], Set.empty, [])
      allFailures = prefixFailures <> additionalFailures
      allIndices = prefixIndices <> additionalIndices
      allAnnotations = prefixAnnotations <> additionalAnnotations
  in pure $
       case allFailures of
         [] -> ValidationSuccess (annotateItems allIndices <> mconcat allAnnotations)
         failures' -> ValidationFailure $ sconcat (NE.fromList failures')

validatePrefixItemsKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to arrays

-- | Keyword definition for prefixItems
prefixItemsKeyword :: KeywordDefinition
prefixItemsKeyword = KeywordDefinition
  { keywordName = "prefixItems"
  , keywordCompile = compilePrefixItems
  , keywordValidate = validatePrefixItemsKeyword
  , keywordNavigation = SchemaArray $ \schema -> case schemaCore schema of
      ObjectSchema obj -> 
        -- Check pre-parsed first, then parse on-demand
        case validationPrefixItems (schemaValidation obj) of
          Just prefixSchemas -> Just (NE.toList prefixSchemas)
          Nothing -> parsePrefixItemsFromRaw schema
      _ -> Nothing
  , keywordPostValidate = Nothing
  }
  where
    parsePrefixItemsFromRaw :: Schema -> Maybe [Schema]
    parsePrefixItemsFromRaw s = case Map.lookup "prefixItems" (schemaRawKeywords s) of
      Just (Array arr) ->
        let version = fromMaybe Draft202012 (schemaVersion s)
            schemas = do
              val <- toList arr
              Right schema <- pure $ ParserInternal.parseSchemaValue version val
              pure schema
        in if null schemas && not (null arr)
           then Nothing  -- Had items but all failed to parse
           else Just schemas
      _ -> Nothing

