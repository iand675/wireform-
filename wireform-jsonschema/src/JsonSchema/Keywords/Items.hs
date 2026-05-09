{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'items' keyword
--
-- The items keyword validates array items. It has two modes:
-- 1. Schema mode: A single schema that applies to all array items
-- 2. Tuple mode (Draft-04 only): An array of schemas for positional validation
--
-- In Draft 2020-12+, tuple validation uses prefixItems instead of items.
module JsonSchema.Keywords.Items
  ( itemsKeyword
  , compileItems
  , ItemsData(..)
  ) where

import Data.Aeson (Value(..))
import qualified Data.Vector as V
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (sconcat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Foldable (toList)
import Control.Monad.Reader (ask)
import Data.Set (Set)
import qualified Data.Set as Set

import JsonSchema.Types 
  ( Schema(..), SchemaCore(..), SchemaObject(..)
  , JsonSchemaVersion(..), ValidationConfig(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , ArrayItemsValidation(..), validationItems, validationPrefixItems, schemaValidation, schemaCore
  , ValidationAnnotations(..), schemaRawKeywords, schemaVersion
  , ValidationErrors(..), validationError
  )
import qualified JsonSchema.Parser.Internal as ParserInternal
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Control.Applicative ((<|>))
import JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..)
  , CompilationContext(..), contextParseSubschema
  , combineValidationResults
  )
import JsonSchema.Validator.Annotations
  ( annotateItems
  , arrayIndexPointer
  , shiftAnnotations
  )

-- | Compiled data for items keyword
data ItemsData = ItemsData
  { itemsValidationData :: ArrayItemsValidation
  , itemsHasPrefix :: Bool
  , itemsPrefixCount :: Int  -- Number of items covered by prefixItems (for Draft202012+)
  }
  deriving (Typeable)

-- | Compile the items keyword
compileItems :: CompileFunc ItemsData
compileItems value schema ctx = 
  -- Extract the already-parsed items schemas from the parent SchemaObject
  -- If not pre-parsed, parse on-demand using the compilation context
  -- This preserves base URI context that was set during initial schema parsing
  case schemaCore schema of
    BooleanSchema _ -> 
      Left "items keyword cannot appear in boolean schema"
    ObjectSchema obj ->
      case validationItems (schemaValidation obj) of
        Just itemsValidation ->
          -- Already parsed - use it
          let hasPrefix = validationPrefixItems (schemaValidation obj) /= Nothing
              prefixCount = case validationPrefixItems (schemaValidation obj) of
                Just prefixSchemas -> length prefixSchemas
                Nothing -> 0
          in Right $ ItemsData itemsValidation hasPrefix prefixCount
        Nothing -> do
          -- Not pre-parsed - parse on-demand from raw keywords or value
          let itemsVal = Map.lookup "items" (schemaRawKeywords schema) <|> Just value
          case itemsVal of
            Just (Array arr) -> do
              -- Tuple-style items (array of schemas)
              schemas <- traverse (contextParseSubschema ctx) (toList arr)
              nonEmpty <- case NE.nonEmpty schemas of
                Just ne -> Right ne
                Nothing -> Left "items array must contain at least one schema"
              -- Parse additionalItems if present
              let additionalItemsVal = Map.lookup "additionalItems" (schemaRawKeywords schema)
              additionalItems' <- case additionalItemsVal of
                Just val -> case contextParseSubschema ctx val of
                  Right s -> Right (Just s)
                  Left err -> Left $ "Failed to parse additionalItems: " <> err
                Nothing -> Right Nothing
              let hasPrefix = validationPrefixItems (schemaValidation obj) /= Nothing
                  prefixCount = case validationPrefixItems (schemaValidation obj) of
                    Just prefixSchemas -> length prefixSchemas
                    Nothing -> 0
              Right $ ItemsData (ItemsTuple nonEmpty additionalItems') hasPrefix prefixCount
            Just val -> do
              -- Single schema for all items
              itemSchema <- contextParseSubschema ctx val
              let hasPrefix = validationPrefixItems (schemaValidation obj) /= Nothing
                  prefixCount = case validationPrefixItems (schemaValidation obj) of
                    Just prefixSchemas -> length prefixSchemas
                    Nothing -> 0
              Right $ ItemsData (ItemsSchema itemSchema) hasPrefix prefixCount
            Nothing -> Left "items keyword present but no parsed items found in schema"

-- | Validate items using the pluggable keyword system
validateItemsKeyword :: ValidateFunc ItemsData
validateItemsKeyword recursiveValidator (ItemsData itemsValidation hasPrefix prefixCount) _ctx (Array arr) = do
  config <- ask
  let version = validationVersion config
  if hasPrefix && version >= Draft202012
    then case itemsValidation of
      ItemsSchema itemSchema ->
        -- In Draft202012+, items only applies to items after prefixItems
        -- But we still need to annotate those items as evaluated
        let remainingItems = drop prefixCount (toList arr)
            remainingIndices = Set.fromList [prefixCount .. prefixCount + length remainingItems - 1]
            evaluations = [ (idx, recursiveValidator itemSchema item)
                          | (idx, item) <- zip [prefixCount..] remainingItems
                          ]
            failures = do
              (_, ValidationFailure errs) <- evaluations
              pure errs
            shiftedAnnotations = do
              (idx, ValidationSuccess anns) <- evaluations
              pure $ shiftAnnotations (arrayIndexPointer idx) anns
        in pure $ case failures of
          [] -> ValidationSuccess (annotateItems remainingIndices <> mconcat shiftedAnnotations)
          failures' -> ValidationFailure $ sconcat (NE.fromList failures')
      _ -> pure (ValidationSuccess mempty)
    else case itemsValidation of
      ItemsSchema itemSchema ->
        let evaluations =
              [ (idx, recursiveValidator itemSchema item)
              | (idx, item) <- zip [0..] (toList arr)
              ]
            indices = Set.fromList $ map fst evaluations
            failures = do
              (_, ValidationFailure errs) <- evaluations
              pure errs
            shiftedAnnotations = do
              (idx, ValidationSuccess anns) <- evaluations
              pure $ shiftAnnotations (arrayIndexPointer idx) anns
        in pure $
             case failures of
               [] -> ValidationSuccess (annotateItems indices <> mconcat shiftedAnnotations)
               failures' -> ValidationFailure $ sconcat (NE.fromList failures')
      ItemsTuple tupleSchemas maybeAdditional ->
        let tupleEvaluations =
              [ (idx, recursiveValidator schema item)
              | (idx, (schema, item)) <-
                  zip [0..] (zip (NE.toList tupleSchemas) (toList arr))
              ]
            tupleIndices = Set.fromList $ map fst tupleEvaluations
            additionalItems = drop (length tupleSchemas) (toList arr)
            (additionalEvaluations, additionalFailures, additionalIndices, additionalAnnotations) = case maybeAdditional of
              Just addlSchema ->
                -- Check if additionalItems is false
                case schemaCore addlSchema of
                  BooleanSchema False ->
                    -- No additional items allowed - fail validation if there are any additional items
                    if null additionalItems
                    then ([], [], Set.empty, [])
                    else let errs = ValidationErrors $ pure $ validationError "additionalItems is false, no additional items allowed"
                         in ([], [errs], Set.empty, [])
                  BooleanSchema True ->
                    -- All additional items allowed - mark them as evaluated
                    let additionalIndices = Set.fromList [length tupleSchemas .. length tupleSchemas + length additionalItems - 1]
                    in ([], [], additionalIndices, [])
                  _ ->
                    -- Validate additional items against the schema
                    let evals = [ (idx, recursiveValidator addlSchema item)
                                | (idx, item) <- zip [length tupleSchemas ..] additionalItems
                                ]
                        failures = do
                          (_, ValidationFailure errs) <- evals
                          pure errs
                        indices = Set.fromList $ map fst evals
                        annotations = do
                          (idx, ValidationSuccess anns) <- evals
                          pure $ shiftAnnotations (arrayIndexPointer idx) anns
                    in (evals, failures, indices, annotations)
              Nothing -> ([], [], Set.empty, [])
            failures =
              [ errs
              | (_, ValidationFailure errs) <- tupleEvaluations ++ additionalEvaluations
              ] <> additionalFailures
            shiftedAnnotations =
              [ shiftAnnotations (arrayIndexPointer idx) anns
              | (idx, ValidationSuccess anns) <- tupleEvaluations ++ additionalEvaluations
              ] <> additionalAnnotations
            allIndices = tupleIndices <> additionalIndices
        in pure $
             case failures of
               [] -> ValidationSuccess (annotateItems allIndices <> mconcat shiftedAnnotations)
               failures' -> ValidationFailure $ sconcat (NE.fromList failures')


validateItemsKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to arrays

-- | Keyword definition for items
itemsKeyword :: KeywordDefinition
itemsKeyword = KeywordDefinition
  { keywordName = "items"
  , keywordCompile = compileItems
  , keywordValidate = validateItemsKeyword
  , keywordNavigation = CustomNavigation $ \schema _seg rest -> case schemaCore schema of
      ObjectSchema obj ->
        case validationItems (schemaValidation obj) of
          Just (ItemsSchema itemSchema) ->
            -- Single schema for all items - return it with full rest path
            Just (itemSchema, rest)
          Just (ItemsTuple schemas _) ->
            -- Array of schemas - need index from rest
            case rest of
              (idx:remaining) ->
                case reads (T.unpack idx) :: [(Int, String)] of
                  [(n, "")] | n >= 0 && n < length schemas ->
                    Just (NE.toList schemas !! n, remaining)
                  _ -> Nothing
              [] -> Nothing  -- Need an index for tuple items
          Nothing -> parseItemsFromRaw schema rest
      _ -> Nothing
  , keywordPostValidate = Nothing
  }
  where
    parseItemsFromRaw :: Schema -> [Text] -> Maybe (Schema, [Text])
    parseItemsFromRaw s rest = case Map.lookup "items" (schemaRawKeywords s) of
      Just (Array arr) | not (null arr) -> do
        -- Tuple-style items (array of schemas)
        let version = fromMaybe Draft202012 (schemaVersion s)
            schemas = do
              val <- toList arr
              Right schema <- pure $ ParserInternal.parseSchemaValue version val
              pure schema
        -- Only proceed if we successfully parsed at least one schema
        case schemas of
          [] -> Nothing  -- Failed to parse all schemas
          _ -> case rest of
            (idx:remaining) ->
              case reads (T.unpack idx) :: [(Int, String)] of
                [(n, "")] | n >= 0 && n < length schemas ->
                  Just (schemas !! n, remaining)
                _ -> Nothing
            [] -> Nothing
      Just val -> do
        -- Single schema for all items
        let version = fromMaybe Draft202012 (schemaVersion s)
        case ParserInternal.parseSchemaValue version val of
          Right schema -> Just (schema, rest)
          Left _ -> Nothing
      _ -> Nothing

