{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Keyword definitions that provide navigation support for schema resolution
--
-- These keyword definitions are primarily for JSON Pointer resolution ($ref)
-- rather than validation. They declare how keywords contain and expose subschemas.
module JsonSchema.Keywords.Navigation
  ( propertiesKeyword
  , patternPropertiesKeyword
  , additionalPropertiesKeyword
  , itemsKeyword
  , prefixItemsKeyword
  , containsKeyword
  , allOfKeyword
  , anyOfKeyword
  , oneOfKeyword
  , notKeyword
  , ifKeyword
  , thenKeyword
  , elseKeyword
  , dependentSchemasKeyword
  , propertyNamesKeyword
  , unevaluatedPropertiesKeyword
  , defsKeyword
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (sconcat)
import qualified Text.Read as Read

import JsonSchema.Keyword (mkNavigableKeyword)
import JsonSchema.Keyword.Types 
  ( KeywordDefinition, KeywordNavigation(..)
  , CompilationContext(..), ValidateFunc, CompileFunc
  )
import JsonSchema.Types 
  ( Schema(..), SchemaCore(..), SchemaObject(..), ArrayItemsValidation(..), Regex(..)
  , schemaAllOf, schemaAnyOf, schemaOneOf, schemaNot
  , schemaIf, schemaThen, schemaElse, schemaDefs
  , schemaCore, schemaValidation, SchemaValidation(..)
  , schemaRawKeywords, schemaVersion
  , validationProperties, validationPatternProperties, validationAdditionalProperties
  , validationItems, validationPrefixItems, validationContains
  , validationDependentSchemas, validationPropertyNames, validationUnevaluatedProperties
  , ValidationAnnotations(..), pattern ValidationSuccess, pattern ValidationFailure
  , ValidationResult, ValidationErrors(..), emptyPointer
  , JsonSchemaVersion(..), schemaVersion, schemaRawKeywords
  )
import qualified JsonSchema.Validator.Result as VR
import JsonSchema.Parser.Internal (parseSchemaValue)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson (Value(..), object)
import Control.Monad.Reader (ask)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Typeable (Typeable)
import Data.Foldable (toList)

-- | Navigate into 'properties' keyword (SchemaMap)
propertiesKeyword :: KeywordDefinition
propertiesKeyword = mkNavigableKeyword
  "properties"
  (\_ _ _ -> Right ())  -- No compilation needed for navigation-only
  (\_ _ _ _ -> pure (ValidationSuccess mempty))      -- No validation needed for navigation-only
  (SchemaMap $ \schema -> case schemaCore schema of
    ObjectSchema obj -> 
      -- Check pre-parsed first, then parse on-demand
      case validationProperties (schemaValidation obj) of
        Just props -> Just props
        Nothing -> parsePropertiesFromRaw schema
    _ -> Nothing)
  where
    parsePropertiesFromRaw :: Schema -> Maybe (Map Text Schema)
    parsePropertiesFromRaw s = case Map.lookup "properties" (schemaRawKeywords s) of
      Just (Object propsObj) ->
        let version = fromMaybe Draft202012 (schemaVersion s)
            entries = KeyMap.toList propsObj
            parseEntry (k, v) = case parseSchemaValue version v of
              Right schema -> Just (Key.toText k, schema)
              Left _ -> Nothing
            parsedMap = Map.fromList $ mapMaybe parseEntry entries
        in if Map.null parsedMap && not (KeyMap.null propsObj)
           then Nothing  -- Had properties but all failed to parse
           else Just parsedMap  -- Either successfully parsed or was empty to begin with
      _ -> Nothing

-- | Navigate into 'patternProperties' keyword (SchemaMap with regex keys)
patternPropertiesKeyword :: KeywordDefinition
patternPropertiesKeyword = mkNavigableKeyword
  "patternProperties"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SchemaMap $ \schema -> case schemaCore schema of
    ObjectSchema obj ->
      -- Check pre-parsed first, then parse on-demand
      case validationPatternProperties (schemaValidation obj) of
        Just patterns ->
          -- Convert Regex keys to Text keys for navigation
          Just $ Map.fromList $ do
            (Regex pat, schema) <- Map.toList patterns
            pure (pat, schema)
        Nothing -> parsePatternPropertiesFromRaw schema
    _ -> Nothing)
  where
    parsePatternPropertiesFromRaw :: Schema -> Maybe (Map Text Schema)
    parsePatternPropertiesFromRaw s = case Map.lookup "patternProperties" (schemaRawKeywords s) of
      Just (Object patternsObj) ->
        let version = fromMaybe Draft202012 (schemaVersion s)
            entries = KeyMap.toList patternsObj
            parseEntry (k, v) = case parseSchemaValue version v of
              Right schema -> Just (Key.toText k, schema)
              Left _ -> Nothing
        in Just $ Map.fromList $ mapMaybe parseEntry entries
      _ -> Nothing

-- | Navigate into 'additionalProperties' keyword (SingleSchema)
additionalPropertiesKeyword :: KeywordDefinition
additionalPropertiesKeyword = mkNavigableKeyword
  "additionalProperties"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> 
      -- Check pre-parsed first, then parse on-demand
      case validationAdditionalProperties (schemaValidation obj) of
        Just addl -> Just addl
        Nothing -> parseAdditionalPropertiesFromRaw schema
    _ -> Nothing)
  where
    parseAdditionalPropertiesFromRaw :: Schema -> Maybe Schema
    parseAdditionalPropertiesFromRaw s = case Map.lookup "additionalProperties" (schemaRawKeywords s) of
      Just (Bool b) ->
        let version = fromMaybe Draft202012 (schemaVersion s)
        in case parseSchemaValue version (Bool b) of
          Right schema -> Just schema
          Left _ -> Nothing
      Just val ->
        let version = fromMaybe Draft202012 (schemaVersion s)
        in case parseSchemaValue version val of
          Right schema -> Just schema
          Left _ -> Nothing
      _ -> Nothing

-- | Navigate into 'items' keyword (can be SingleSchema or SchemaArray in Draft-04)
itemsKeyword :: KeywordDefinition
itemsKeyword = mkNavigableKeyword
  "items"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (CustomNavigation $ \schema _seg rest -> case schemaCore schema of
    ObjectSchema obj ->
      -- Check pre-parsed first, then parse on-demand
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
    _ -> Nothing)
  where
    parseItemsFromRaw :: Schema -> [Text] -> Maybe (Schema, [Text])
    parseItemsFromRaw s rest = case Map.lookup "items" (schemaRawKeywords s) of
      Just (Array arr) | not (null arr) -> do
        -- Tuple-style items (array of schemas)
        let version = fromMaybe Draft202012 (schemaVersion s)
            schemas = do
              val <- toList arr
              Right schema <- pure $ parseSchemaValue version val
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
        case parseSchemaValue version val of
          Right schema -> Just (schema, rest)
          Left _ -> Nothing
      _ -> Nothing

-- | Navigate into 'prefixItems' keyword (SchemaArray)
prefixItemsKeyword :: KeywordDefinition
prefixItemsKeyword = mkNavigableKeyword
  "prefixItems"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SchemaArray $ \schema -> case schemaCore schema of
    ObjectSchema obj -> 
      -- Check pre-parsed first, then parse on-demand
      case validationPrefixItems (schemaValidation obj) of
        Just prefixSchemas -> Just (NE.toList prefixSchemas)
        Nothing -> parsePrefixItemsFromRaw schema
    _ -> Nothing)
  where
    parsePrefixItemsFromRaw :: Schema -> Maybe [Schema]
    parsePrefixItemsFromRaw s = case Map.lookup "prefixItems" (schemaRawKeywords s) of
      Just (Array arr) ->
        let version = fromMaybe Draft202012 (schemaVersion s)
            schemas = do
              val <- toList arr
              Right schema <- pure $ parseSchemaValue version val
              pure schema
        in case schemas of
          [] -> Nothing
          _ -> Just schemas
      _ -> Nothing

-- | Navigate into 'contains' keyword (SingleSchema)
containsKeyword :: KeywordDefinition
containsKeyword = mkNavigableKeyword
  "contains"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> 
      -- Check pre-parsed first, then parse on-demand
      case validationContains (schemaValidation obj) of
        Just contains -> Just contains
        Nothing -> parseContainsFromRaw schema
    _ -> Nothing)
  where
    parseContainsFromRaw :: Schema -> Maybe Schema
    parseContainsFromRaw s = case Map.lookup "contains" (schemaRawKeywords s) of
      Just val ->
        let version = fromMaybe Draft202012 (schemaVersion s)
        in case parseSchemaValue version val of
          Right schema -> Just schema
          Left _ -> Nothing
      _ -> Nothing

-- | Navigate into 'allOf' keyword (SchemaArray)
allOfKeyword :: KeywordDefinition
allOfKeyword = mkNavigableKeyword
  "allOf"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SchemaArray $ \schema -> case schemaCore schema of
    ObjectSchema obj -> 
      -- Check pre-parsed first, then parse on-demand
      case schemaAllOf obj of
        Just allOfSchemas -> Just (NE.toList allOfSchemas)
        Nothing -> parseAllOfFromRaw schema
    _ -> Nothing)
  where
    parseAllOfFromRaw :: Schema -> Maybe [Schema]
    parseAllOfFromRaw s = case Map.lookup "allOf" (schemaRawKeywords s) of
      Just (Array arr) ->
        let version = fromMaybe Draft202012 (schemaVersion s)
            schemas = do
              val <- toList arr
              Right schema <- pure $ parseSchemaValue version val
              pure schema
        in if null schemas && not (null arr)
           then Nothing  -- Had items but all failed to parse
           else Just schemas
      _ -> Nothing

-- | Navigate into 'anyOf' keyword (SchemaArray)
anyOfKeyword :: KeywordDefinition
anyOfKeyword = mkNavigableKeyword
  "anyOf"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SchemaArray $ \schema -> case schemaCore schema of
    ObjectSchema obj -> 
      -- Check pre-parsed first, then parse on-demand
      case schemaAnyOf obj of
        Just anyOfSchemas -> Just (NE.toList anyOfSchemas)
        Nothing -> parseAnyOfFromRaw schema
    _ -> Nothing)
  where
    parseAnyOfFromRaw :: Schema -> Maybe [Schema]
    parseAnyOfFromRaw s = case Map.lookup "anyOf" (schemaRawKeywords s) of
      Just (Array arr) ->
        let version = fromMaybe Draft202012 (schemaVersion s)
            schemas = do
              val <- toList arr
              Right schema <- pure $ parseSchemaValue version val
              pure schema
        in if null schemas && not (null arr)
           then Nothing  -- Had items but all failed to parse
           else Just schemas
      _ -> Nothing

-- | Navigate into 'oneOf' keyword (SchemaArray)
oneOfKeyword :: KeywordDefinition
oneOfKeyword = mkNavigableKeyword
  "oneOf"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SchemaArray $ \schema -> case schemaCore schema of
    ObjectSchema obj -> 
      -- Check pre-parsed first, then parse on-demand
      case schemaOneOf obj of
        Just oneOfSchemas -> Just (NE.toList oneOfSchemas)
        Nothing -> parseOneOfFromRaw schema
    _ -> Nothing)
  where
    parseOneOfFromRaw :: Schema -> Maybe [Schema]
    parseOneOfFromRaw s = case Map.lookup "oneOf" (schemaRawKeywords s) of
      Just (Array arr) ->
        let version = fromMaybe Draft202012 (schemaVersion s)
            schemas = do
              val <- toList arr
              Right schema <- pure $ parseSchemaValue version val
              pure schema
        in if null schemas && not (null arr)
           then Nothing  -- Had items but all failed to parse
           else Just schemas
      _ -> Nothing

-- | Navigate into 'not' keyword (SingleSchema)
notKeyword :: KeywordDefinition
notKeyword = mkNavigableKeyword
  "not"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> 
      -- Check pre-parsed first, then parse on-demand
      case schemaNot obj of
        Just notSchema -> Just notSchema
        Nothing -> parseNotFromRaw schema
    _ -> Nothing)
  where
    parseNotFromRaw :: Schema -> Maybe Schema
    parseNotFromRaw s = case Map.lookup "not" (schemaRawKeywords s) of
      Just val ->
        let version = fromMaybe Draft202012 (schemaVersion s)
        in case parseSchemaValue version val of
          Right schema -> Just schema
          Left _ -> Nothing
      _ -> Nothing

-- | Navigate into 'if' keyword (SingleSchema)
ifKeyword :: KeywordDefinition
ifKeyword = mkNavigableKeyword
  "if"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> 
      -- Check pre-parsed first, then parse on-demand
      case schemaIf obj of
        Just ifSchema -> Just ifSchema
        Nothing -> parseIfFromRaw schema
    _ -> Nothing)
  where
    parseIfFromRaw :: Schema -> Maybe Schema
    parseIfFromRaw s = case Map.lookup "if" (schemaRawKeywords s) of
      Just val ->
        let version = fromMaybe Draft202012 (schemaVersion s)
        in case parseSchemaValue version val of
          Right schema -> Just schema
          Left _ -> Nothing
      _ -> Nothing

-- | Navigate into 'then' keyword (SingleSchema)
thenKeyword :: KeywordDefinition
thenKeyword = mkNavigableKeyword
  "then"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> 
      -- Check pre-parsed first, then parse on-demand
      case schemaThen obj of
        Just thenSchema -> Just thenSchema
        Nothing -> parseThenFromRaw schema
    _ -> Nothing)
  where
    parseThenFromRaw :: Schema -> Maybe Schema
    parseThenFromRaw s = case Map.lookup "then" (schemaRawKeywords s) of
      Just val ->
        let version = fromMaybe Draft202012 (schemaVersion s)
        in case parseSchemaValue version val of
          Right schema -> Just schema
          Left _ -> Nothing
      _ -> Nothing

-- | Navigate into 'else' keyword (SingleSchema)
elseKeyword :: KeywordDefinition
elseKeyword = mkNavigableKeyword
  "else"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> 
      -- Check pre-parsed first, then parse on-demand
      case schemaElse obj of
        Just elseSchema -> Just elseSchema
        Nothing -> parseElseFromRaw schema
    _ -> Nothing)
  where
    parseElseFromRaw :: Schema -> Maybe Schema
    parseElseFromRaw s = case Map.lookup "else" (schemaRawKeywords s) of
      Just val ->
        let version = fromMaybe Draft202012 (schemaVersion s)
        in case parseSchemaValue version val of
          Right schema -> Just schema
          Left _ -> Nothing
      _ -> Nothing

-- | Navigate into 'dependentSchemas' keyword (SchemaMap)
dependentSchemasKeyword :: KeywordDefinition
dependentSchemasKeyword = mkNavigableKeyword
  "dependentSchemas"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SchemaMap $ \schema -> case schemaCore schema of
    ObjectSchema obj -> 
      -- Check pre-parsed first, then parse on-demand
      case validationDependentSchemas (schemaValidation obj) of
        Just depSchemas -> Just depSchemas
        Nothing -> parseDependentSchemasFromRaw schema
    _ -> Nothing)
  where
    parseDependentSchemasFromRaw :: Schema -> Maybe (Map Text Schema)
    parseDependentSchemasFromRaw s = case Map.lookup "dependentSchemas" (schemaRawKeywords s) of
      Just (Object depSchemasObj) ->
        let version = fromMaybe Draft202012 (schemaVersion s)
            entries = KeyMap.toList depSchemasObj
            parseEntry (k, v) = case parseSchemaValue version v of
              Right schema -> Just (Key.toText k, schema)
              Left _ -> Nothing
        in Just $ Map.fromList $ mapMaybe parseEntry entries
      _ -> Nothing

-- | Navigate into 'propertyNames' keyword (SingleSchema)
propertyNamesKeyword :: KeywordDefinition
propertyNamesKeyword = mkNavigableKeyword
  "propertyNames"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> 
      -- Check pre-parsed first, then parse on-demand
      case validationPropertyNames (schemaValidation obj) of
        Just propNames -> Just propNames
        Nothing -> parsePropertyNamesFromRaw schema
    _ -> Nothing)
  where
    parsePropertyNamesFromRaw :: Schema -> Maybe Schema
    parsePropertyNamesFromRaw s = case Map.lookup "propertyNames" (schemaRawKeywords s) of
      Just val ->
        let version = fromMaybe Draft202012 (schemaVersion s)
        in case parseSchemaValue version val of
          Right schema -> Just schema
          Left _ -> Nothing
      _ -> Nothing

-- | Navigate into 'unevaluatedProperties' keyword (SingleSchema)
unevaluatedPropertiesKeyword :: KeywordDefinition
unevaluatedPropertiesKeyword = mkNavigableKeyword
  "unevaluatedProperties"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> 
      -- Check pre-parsed first, then parse on-demand
      case validationUnevaluatedProperties (schemaValidation obj) of
        Just unevalProps -> Just unevalProps
        Nothing -> parseUnevaluatedPropertiesFromRaw schema
    _ -> Nothing)
  where
    parseUnevaluatedPropertiesFromRaw :: Schema -> Maybe Schema
    parseUnevaluatedPropertiesFromRaw s = case Map.lookup "unevaluatedProperties" (schemaRawKeywords s) of
      Just val ->
        let version = fromMaybe Draft202012 (schemaVersion s)
        in case parseSchemaValue version val of
          Right schema -> Just schema
          Left _ -> Nothing
      _ -> Nothing

-- | Compiled data for $defs keyword
-- Stores the schema that validates each definition value (from metaschema's additionalProperties)
newtype DefsData = DefsData Schema
  deriving (Typeable)

-- | Compile $defs keyword
-- The metaschema defines $defs with additionalProperties: { "$recursiveRef": "#" }
-- So we need to compile that schema to validate each definition value.
compileDefsKeyword :: CompileFunc DefsData
compileDefsKeyword (Object _defsObj) schema ctx = do
  -- The metaschema defines $defs with additionalProperties: { "$recursiveRef": "#" }
  -- We need to create a schema that validates each value in $defs against the metaschema.
  -- Since we're compiling $defs from the schema, we need to get the metaschema's definition.
  -- For now, we'll create a schema with $recursiveRef: "#" which will resolve to the metaschema.
  -- But we need the current schema version to parse the $recursiveRef correctly.
  let version = fromMaybe Draft202012 (schemaVersion schema)
      -- Create a schema that validates each definition value against the metaschema
      -- For 2019-09: additionalProperties: { "$recursiveRef": "#" }
      -- For 2020-12: additionalProperties: { "$dynamicRef": "#meta" }
      refValue = case version of
        Draft201909 -> object [("$recursiveRef", String "#")]
        _ -> object [("$dynamicRef", String "#meta")]  -- 2020-12+
  case parseSchemaValue version refValue of
    Left err -> Left $ "Failed to compile $defs validation schema: " <> T.pack (show err)
    Right defsValidationSchema -> Right $ DefsData defsValidationSchema
compileDefsKeyword _ _ _ = Left "$defs must be an object"

-- | Validate $defs keyword
-- When validating instance data against a schema that references the metaschema,
-- each value in the instance's $defs must be a valid schema (validated against the metaschema).
validateDefsKeyword :: ValidateFunc DefsData
validateDefsKeyword recursiveValidator (DefsData defsValidationSchema) _ctx (Object objMap) = do
  -- Check for $defs or definitions in the instance
  let defsValue = KeyMap.lookup (Key.fromText "$defs") objMap
      defsValue' = case defsValue of
        Just v -> Just v
        Nothing -> KeyMap.lookup (Key.fromText "definitions") objMap
  case defsValue' of
    Just (Object defsObj) ->
      -- Validate each value in $defs against the validation schema
      -- The validation schema has $recursiveRef: "#" which resolves to the metaschema
      let results = 
            [ recursiveValidator defsValidationSchema defValue
            | (defName, defValue) <- KeyMap.toList defsObj
            ]
          failures = do
            ValidationFailure errs <- results
            pure errs
      in pure $ case failures of
        [] -> ValidationSuccess mempty
        failures' -> ValidationFailure $ sconcat (NE.fromList failures')
    Just _ -> 
      -- $defs is not an object
      pure $ ValidationFailure $ ValidationErrors $ pure $ VR.ValidationError
        { VR.errorKeyword = "$defs"
        , VR.errorSchemaPath = emptyPointer
        , VR.errorInstancePath = emptyPointer
        , VR.errorMessage = "$defs must be an object"
        }
    Nothing -> 
      -- No $defs in instance, that's fine
      pure $ ValidationSuccess mempty
validateDefsKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to objects

-- | Navigate into '$defs' or 'definitions' keyword (SchemaMap)
defsKeyword :: KeywordDefinition
defsKeyword = mkNavigableKeyword
  "$defs"
  compileDefsKeyword
  validateDefsKeyword
  (SchemaMap $ \schema -> case schemaCore schema of
    ObjectSchema obj ->
      let defs = schemaDefs obj
      in if Map.null defs then Nothing else Just defs
    _ -> Nothing)

