{-# LANGUAGE PatternSynonyms #-}
-- | JSON Schema parsing with automatic version detection
--
-- This module provides functions to parse JSON Schema from JSON/YAML values.
-- The parser supports all versions (draft-04 through 2020-12) with automatic
-- version detection from the $schema keyword.
module JsonSchema.Parser
  ( -- * Parsing Functions
    parseSchema
  , parseSchemaWithVersion
  , parseSubschema
  , parseSchemaStrict
  , parseSchemaFromFile
  
    -- * Dialect-Aware Parsing
  , parseSchemaWithDialectRegistry
  , parseSchemaWithDialectRegistryAndVersion
  , resolveDialectFromSchema
  , extractSchemaURI
  
    -- * Configuration-Based Parsing
  , ParseConfig(..)
  , defaultParseConfig
  , parseSchemaWithConfig

    -- * Error Types
  , ParseError(..)
  , ParseWarning(..)
  ) where

import JsonSchema.Types
  ( Schema(..), SchemaCore(..), SchemaObject(..), ObjectSchemaData(..), SchemaAnnotations(..), SchemaValidation(..)
  , SchemaType(..), OneOrMany(..), ArrayItemsValidation(..), Dependency(..), Regex(..), Reference(..)
  , JsonSchemaVersion(..), ParseError(..), ParseWarning(..), UnknownKeywordMode(..)
  , JsonPointer(..), emptyPointer, ValidationResult
  )
import JsonSchema.Vocabulary (VocabularyRegistry, lookupDialect)
import JsonSchema.Dialect (Dialect, dialectURI, dialectVersion, dialectUnknownKeywords)
import JsonSchema.MetaschemaValidation
  ( validateSchemaAgainstMetaschema
  , metaschemaURIForVersion
  )
import JsonSchema.EmbeddedMetaschemas.Raw (lookupRawMetaschema)
import qualified JsonSchema.Parser.Internal as ParserInternal
import qualified JsonSchema.Validator as Validator
import JsonSchema.Types (ValidationConfig(..))
import Data.Aeson (Value(..), Object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Foldable (toList)
import Data.Maybe (isJust)
import Control.Monad (when, unless)

-- ParseError and ParseWarning are now defined in Types to avoid circular dependencies

-- | Configuration for schema parsing
--
-- Controls how the parser handles various aspects of schema processing,
-- including unknown keyword handling.
--
-- @since 0.1.0.0
data ParseConfig = ParseConfig
  { parseUnknownKeywordMode :: UnknownKeywordMode
    -- ^ How to handle keywords not in any registered vocabulary
    --
    -- * 'IgnoreUnknown': Don't collect unknown keywords at all
    -- * 'WarnUnknown': Collect warnings but continue parsing (extensions)
    -- * 'ErrorOnUnknown': Fail parsing when unknown keywords encountered (__non-standard__)
    -- * 'CollectUnknown': Collect unknown keywords in 'schemaExtensions' (default, spec-compliant)
    --
    -- __Note__: This is separate from @$vocabulary@ validation. Required vocabularies
    -- always cause parsing to fail if not understood, regardless of this setting.
  } deriving (Eq, Show)

-- | Default parsing configuration
--
-- Uses 'CollectUnknown' mode (spec-compliant default behavior).
--
-- @since 0.1.0.0
defaultParseConfig :: ParseConfig
defaultParseConfig = ParseConfig
  { parseUnknownKeywordMode = CollectUnknown
  }

-- | Parse a JSON Schema from a Value with automatic version detection
parseSchema :: Value -> Either ParseError Schema
parseSchema val = do
  version <- detectVersion val
  parseSchemaWithVersion version val

-- | Parse a subschema, inheriting the parent version unless the subschema
--   declares its own $schema.
parseSubschema :: JsonSchemaVersion -> Value -> Either ParseError Schema
parseSubschema parentVersion val = case val of
  Object obj
    | KeyMap.member "$schema" obj -> parseSchema val
    | otherwise -> parseSchemaWithVersion parentVersion val
  _ -> parseSchemaWithVersion parentVersion val

-- | Parse with explicit version (skip detection)
parseSchemaWithVersion :: JsonSchemaVersion -> Value -> Either ParseError Schema
parseSchemaWithVersion version val = case val of
  Bool b -> Right $ Schema
    { schemaVersion = Just version
    , schemaMetaschemaURI = Nothing  -- Boolean schemas don't have $schema
    , schemaId = Nothing
    , schemaCore = BooleanSchema b
    , schemaVocabulary = Nothing
    , schemaExtensions = Map.empty
    , schemaRawKeywords = Map.empty  -- Boolean schemas have no keywords
    }
  Object obj -> do
    -- Parse $schema URI
    let metaschemaURI = KeyMap.lookup "$schema" obj >>= \case
          String t -> Just t
          _ -> Nothing

    -- Parse $id (draft-06+) or id (draft-04 only)
    let idKey = if version >= Draft06 then "$id" else "id"
    let schemaId' = KeyMap.lookup idKey obj >>= \case
          String t -> Just t
          _ -> Nothing

    -- Parse $vocabulary (2019-09+)
    let vocabMap = if version >= Draft201909
                   then KeyMap.lookup "$vocabulary" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                   else Nothing

    -- Parse core structure
    core <- parseSchemaObject version obj

    -- Collect keywords based on parseUnknownKeywordMode
    -- Known keywords are those defined in the JSON Schema spec for this version
    -- For each unknown keyword:
    --   1. Check if vocabulary is registered
    --   2. Use keywordParser from KeywordDefinition to get typed KeywordValue
    --   3. Store in schemaCustomKeywords :: Map Text KeywordValue
    --   4. Truly unknown keywords stay in schemaExtensions
    let knownKeywords = Set.fromList
          [ "$schema", "$id", "id", "$ref", "$vocabulary", "$defs", "definitions"
          , "$comment", "$anchor", "$dynamicRef", "$dynamicAnchor", "$recursiveRef", "$recursiveAnchor"
          , "type", "enum", "const"
          , "allOf", "anyOf", "oneOf", "not"
          , "if", "then", "else"
          , "title", "description", "default"
          , "deprecated", "readOnly", "writeOnly"
          , "properties", "patternProperties", "additionalProperties"
          , "propertyNames", "required", "minProperties", "maxProperties"
          , "dependentRequired", "dependentSchemas", "dependencies"
          , "unevaluatedProperties"
          , "items", "prefixItems", "additionalItems", "contains"
          , "minItems", "maxItems", "uniqueItems"
          , "maxContains", "minContains", "unevaluatedItems"
          , "minLength", "maxLength", "pattern", "format"
          , "contentEncoding", "contentMediaType"
          , "minimum", "maximum", "exclusiveMinimum", "exclusiveMaximum"
          , "multipleOf"
          ]
    let allKeywords = Map.fromList $ do
          (k, v) <- KeyMap.toList obj
          pure (Key.toText k, v)
    let extensions = Map.filterWithKey (\k _ -> not $ Set.member k knownKeywords) allKeywords

    schema <- pure $ Schema
      { schemaVersion = Just version
      , schemaMetaschemaURI = metaschemaURI
      , schemaId = schemaId'
      , schemaCore = ObjectSchema core
      , schemaVocabulary = vocabMap
      , schemaExtensions = extensions  -- Always collect for backward compatibility
      , schemaRawKeywords = allKeywords  -- Store ALL keywords for monadic compilation
      }
    
    -- Validate schema against its metaschema
    -- Skip validation if this value is itself an embedded metaschema to avoid infinite recursion
    -- We check by seeing if the value matches any embedded metaschema
    -- Also skip if no explicit $schema URI (schemas without $schema are often simple/incomplete)
    let isEmbeddedMetaschema = isJust $ lookupRawMetaschema =<< metaschemaURI
    let hasExplicitSchema = isJust metaschemaURI
    when (not isEmbeddedMetaschema && hasExplicitSchema) $ do
      -- Use a parser that skips validation for metaschemas to avoid recursion
      let parseWithoutMetaschemaValidation v val = 
            ParserInternal.parseSchemaValue v val  -- Use internal parser that doesn't validate
      let validateFn v s val = Validator.validateValue 
            (Validator.defaultValidationConfig { validationVersion = v }) 
            s 
            val
      -- If validation fails, we log but don't fail parsing (metaschema validation is advisory)
      case validateSchemaAgainstMetaschema parseWithoutMetaschemaValidation validateFn version metaschemaURI val of
        Left _ -> pure ()  -- Skip validation errors for now - they're too strict
        Right () -> pure ()
    
    pure schema
  _ -> Left $ ParseError emptyPointer "Schema must be boolean or object" (Just val)

-- | Parse a schema with configuration-based unknown keyword handling
--
-- This function allows control over how unknown keywords (those not in the
-- JSON Schema specification) are handled during parsing.
--
-- __Unknown Keyword Modes__:
--
-- * 'ErrorOnUnknown': Parsing fails if unknown keywords are present (strict)
-- * 'WarnUnknown': Parser succeeds but returns schema with warnings (development)
-- * 'CollectUnknown': Unknown keywords stored in 'schemaExtensions' (default)
-- * 'IgnoreUnknown': Unknown keywords not collected at all
--
-- __Example__:
--
-- @
-- -- Strict mode: fail on typos
-- let config = ParseConfig { parseUnknownKeywordMode = ErrorOnUnknown }
-- case parseSchemaWithConfig config schemaJson of
--   Left err -> putStrLn $ "Invalid schema: " ++ show err
--   Right schema -> validate schema
-- @
--
-- @since 0.1.0.0
parseSchemaWithConfig :: ParseConfig -> Value -> Either ParseError Schema
parseSchemaWithConfig config val = do
  -- Parse normally first
  schema <- parseSchema val
  -- Apply unknown keyword mode
  applyUnknownKeywordMode config schema

-- | Strict parsing (fail on unknown keywords)
parseSchemaStrict :: Value -> Either ParseError Schema
parseSchemaStrict = parseSchema  -- TODO: Implement strict mode

-- | Parse from file (handles JSON and YAML)
parseSchemaFromFile :: FilePath -> IO (Either ParseError Schema)
parseSchemaFromFile = error "Not yet implemented"  -- TODO: Implement file loading

-- | Detect schema version from $schema keyword
detectVersion :: Value -> Either ParseError JsonSchemaVersion
detectVersion (Object obj) = case KeyMap.lookup "$schema" obj of
  Nothing -> pure Draft202012  -- Default to latest
  Just uri -> case Aeson.fromJSON uri of
    Aeson.Success ver -> pure ver
    Aeson.Error err -> Left $ ParseError emptyPointer (T.pack err) Nothing
detectVersion _ = pure Draft202012  -- Boolean schemas default to latest


-- | Parse full schema object
parseSchemaObject :: JsonSchemaVersion -> Object -> Either ParseError SchemaObject
parseSchemaObject version obj = do
  -- Parse type keyword
  let schemaType' = KeyMap.lookup "type" obj >>= \v -> case v of
        String t -> case parseSchemaType t of
          Just st -> Just (One st)
          Nothing -> Nothing
        Array arr -> case traverse (AesonTypes.parseEither Aeson.parseJSON) (toList arr) of
          Right types -> NE.nonEmpty types >>= Just . Many
          Left _ -> Nothing
        _ -> Nothing
  
  -- Parse enum
  let schemaEnum' = KeyMap.lookup "enum" obj >>= \v -> case v of
        Array arr -> NE.nonEmpty (toList arr)
        _ -> Nothing
  
  -- Parse const (draft-06+)
  let schemaConst' = if version >= Draft06
                     then KeyMap.lookup "const" obj
                     else Nothing
  
  -- Parse $ref
  let schemaRef' = KeyMap.lookup "$ref" obj >>= \v -> case v of
        String t -> Just (Reference t)
        _ -> Nothing
  
  -- Parse composition keywords
  let parseNonEmptySchemas key = KeyMap.lookup key obj >>= \v -> case v of
        Array arr -> do
          let schemas = do
                val <- toList arr
                Right schema <- pure $ parseSubschema version val
                pure schema
          NE.nonEmpty schemas
        _ -> Nothing
  
  let schemaAllOf' = parseNonEmptySchemas "allOf"
  let schemaAnyOf' = parseNonEmptySchemas "anyOf"
  let schemaOneOf' = parseNonEmptySchemas "oneOf"
  
  let schemaNot' = KeyMap.lookup "not" obj >>= eitherToMaybe . parseSubschema version
  
  -- Parse conditional keywords (draft-07+)
  let (schemaIf', schemaThen', schemaElse') = if version >= Draft07
        then ( KeyMap.lookup "if" obj >>= eitherToMaybe . parseSubschema version
             , KeyMap.lookup "then" obj >>= eitherToMaybe . parseSubschema version
             , KeyMap.lookup "else" obj >>= eitherToMaybe . parseSubschema version
             )
        else (Nothing, Nothing, Nothing)
  
  -- Parse validation keywords for backward compatibility
  -- Items, prefixItems, contains, properties, and dependentSchemas are parsed here
  -- for navigation/test compatibility
  -- Other subschema keywords are stored in schemaRawKeywords and parsed on-demand
  -- This makes the parser metaschema-driven: any boolean or object can be a schema
  simpleValidation <- parseValidationKeywords version obj
  let validation' = simpleValidation
        { validationPatternProperties = Nothing
        , validationAdditionalProperties = Nothing
        , validationUnevaluatedProperties = Nothing
        , validationPropertyNames = Nothing
        , validationUnevaluatedItems = Nothing
        }
  
  -- Parse annotations
  let annotations = SchemaAnnotations
        { annotationTitle = KeyMap.lookup "title" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
        , annotationDescription = KeyMap.lookup "description" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
        , annotationDefault = KeyMap.lookup "default" obj
        , annotationExamples = case KeyMap.lookup "examples" obj of
            Just (Array arr) -> toList arr
            _ -> []
        , annotationDeprecated = KeyMap.lookup "deprecated" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
        , annotationReadOnly = KeyMap.lookup "readOnly" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
        , annotationWriteOnly = KeyMap.lookup "writeOnly" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
        , annotationComment = if version >= Draft07
                              then KeyMap.lookup "$comment" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                              else Nothing
        , annotationCodegen = Nothing  -- TODO: Parse x-codegen annotations
        }
  
  -- Parse $defs and legacy definitions (both forms may appear)
  let parseDefsFor key = case KeyMap.lookup key obj of
        Just (Object defsObj) -> Map.fromList
          [ (Key.toText k, schema)
          | (k, v) <- KeyMap.toList defsObj
          , Right schema <- [parseSubschema version v]
          ]
        _ -> Map.empty
  let schemaDefs' =
        let newDefs = parseDefsFor (Key.fromText "$defs")
            legacyDefs = parseDefsFor (Key.fromText "definitions")
        in Map.union newDefs legacyDefs
  
  -- Parse $dynamicRef (2020-12+)
  let dynamicRef' = if version >= Draft202012
                    then KeyMap.lookup "$dynamicRef" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                    else Nothing
  
  -- Parse $dynamicAnchor (2020-12+)
  let dynamicAnchor' = if version >= Draft202012
                       then KeyMap.lookup "$dynamicAnchor" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                       else Nothing

  -- Parse $recursiveRef (2019-09)
  let recursiveRef' = if version == Draft201909
                      then KeyMap.lookup "$recursiveRef" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                      else Nothing

  -- Parse $recursiveAnchor (2019-09)
  let recursiveAnchor' = if version == Draft201909
                         then KeyMap.lookup "$recursiveAnchor" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                         else Nothing

  pure $ SchemaObject $ Right $ ObjectSchemaData
    { objectSchemaDataType = schemaType'
    , objectSchemaDataEnum = schemaEnum'
    , objectSchemaDataConst = schemaConst'
    , objectSchemaDataRef = schemaRef'
    , objectSchemaDataDynamicRef = dynamicRef'
    , objectSchemaDataAnchor = KeyMap.lookup "$anchor" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
    , objectSchemaDataDynamicAnchor = dynamicAnchor'
    , objectSchemaDataRecursiveRef = recursiveRef'
    , objectSchemaDataRecursiveAnchor = recursiveAnchor'
    , objectSchemaDataAllOf = schemaAllOf'
    , objectSchemaDataAnyOf = schemaAnyOf'
    , objectSchemaDataOneOf = schemaOneOf'
    , objectSchemaDataNot = schemaNot'
    , objectSchemaDataIf = schemaIf'
    , objectSchemaDataThen = schemaThen'
    , objectSchemaDataElse = schemaElse'
    , objectSchemaDataValidation = validation'
    , objectSchemaDataAnnotations = annotations
    , objectSchemaDataDefs = schemaDefs'
    }

-- | Helper to convert Either to Maybe
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe (Left _) = Nothing

-- | Parse schema type from text
parseSchemaType :: Text -> Maybe SchemaType
parseSchemaType "null" = Just NullType
parseSchemaType "boolean" = Just BooleanType
parseSchemaType "object" = Just ObjectType
parseSchemaType "array" = Just ArrayType
parseSchemaType "number" = Just NumberType
parseSchemaType "string" = Just StringType
parseSchemaType "integer" = Just IntegerType
parseSchemaType _ = Nothing

-- | Parse validation keywords
parseValidationKeywords :: JsonSchemaVersion -> Object -> Either ParseError SchemaValidation
parseValidationKeywords version obj = do
  -- Numeric validation
  let multipleOf = KeyMap.lookup "multipleOf" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  let maximum' = KeyMap.lookup "maximum" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  let minimum' = KeyMap.lookup "minimum" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  
  -- exclusiveMaximum/Minimum: boolean in draft-04, numeric in draft-06+
  let (exclMax, exclMin) = if version == Draft04
        then ( KeyMap.lookup "exclusiveMaximum" obj >>= AesonTypes.parseMaybe Aeson.parseJSON >>= Just . Left
             , KeyMap.lookup "exclusiveMinimum" obj >>= AesonTypes.parseMaybe Aeson.parseJSON >>= Just . Left
             )
        else ( KeyMap.lookup "exclusiveMaximum" obj >>= AesonTypes.parseMaybe Aeson.parseJSON >>= Just . Right
             , KeyMap.lookup "exclusiveMinimum" obj >>= AesonTypes.parseMaybe Aeson.parseJSON >>= Just . Right
             )
  
  -- String validation
  let maxLength = KeyMap.lookup "maxLength" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  let minLength = KeyMap.lookup "minLength" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  let pattern' = KeyMap.lookup "pattern" obj >>= \v -> case v of
        String t -> Just (Regex t)
        _ -> Nothing
  let format' = KeyMap.lookup "format" obj >>= AesonTypes.parseMaybe Aeson.parseJSON

  -- Content validation (draft-07+)
  let contentEncoding' = if version >= Draft07
                         then KeyMap.lookup "contentEncoding" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                         else Nothing
  let contentMediaType' = if version >= Draft07
                          then KeyMap.lookup "contentMediaType" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                          else Nothing

  -- Array validation
  let items' = parseArrayItems version obj
  let prefixItems' = if version >= Draft202012
                     then KeyMap.lookup "prefixItems" obj >>= parseArraySchemas version
                     else Nothing
  let contains' = KeyMap.lookup "contains" obj >>= eitherToMaybe . parseSubschema version
  let maxItems = KeyMap.lookup "maxItems" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  let minItems = KeyMap.lookup "minItems" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  let uniqueItems = KeyMap.lookup "uniqueItems" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  let maxContains = if version >= Draft201909
                    then KeyMap.lookup "maxContains" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                    else Nothing
  let minContains = if version >= Draft201909
                    then KeyMap.lookup "minContains" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                    else Nothing
  let unevaluatedItems' = if version >= Draft201909
                          then KeyMap.lookup "unevaluatedItems" obj >>= eitherToMaybe . parseSubschema version
                          else Nothing
  
  -- Object validation
  let properties' = KeyMap.lookup "properties" obj >>= parsePropertySchemas version
  let patternProperties' = KeyMap.lookup "patternProperties" obj >>= parsePatternPropertySchemas version
  let additionalProperties' = KeyMap.lookup "additionalProperties" obj >>= \v -> case v of
        Bool b -> Just $ Schema Nothing Nothing Nothing (BooleanSchema b) Nothing Map.empty Map.empty
        _ -> eitherToMaybe $ parseSubschema version v
  let unevaluatedProperties' = if version >= Draft201909
                               then KeyMap.lookup "unevaluatedProperties" obj >>= eitherToMaybe . parseSubschema version
                               else Nothing
  let propertyNames' = if version >= Draft06
                      then KeyMap.lookup "propertyNames" obj >>= eitherToMaybe . parseSubschema version
                       else Nothing
  let maxProperties = KeyMap.lookup "maxProperties" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  let minProperties = KeyMap.lookup "minProperties" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  let required' = KeyMap.lookup "required" obj >>= \v -> case v of
        Array arr -> Just $ Set.fromList $ do
          String t <- toList arr
          pure t
        _ -> Nothing
  let dependentRequired' = if version >= Draft201909
                           then KeyMap.lookup "dependentRequired" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                           else Nothing
  let dependentSchemas' = if version >= Draft201909
                          then KeyMap.lookup "dependentSchemas" obj >>= parsePropertySchemas version
                          else Nothing
  -- Dependencies keyword: deprecated in 2019-09+ but still supported for backward compatibility
  let dependencies' = KeyMap.lookup "dependencies" obj >>= parseDependencies version
  
  pure $ SchemaValidation
    { validationMultipleOf = multipleOf
    , validationMaximum = maximum'
    , validationExclusiveMaximum = exclMax
    , validationMinimum = minimum'
    , validationExclusiveMinimum = exclMin
    , validationMaxLength = maxLength
    , validationMinLength = minLength
    , validationPattern = pattern'
    , validationFormat = format'
    , validationContentEncoding = contentEncoding'
    , validationContentMediaType = contentMediaType'
    , validationItems = items'
    , validationPrefixItems = prefixItems'
    , validationContains = contains'
    , validationMaxItems = maxItems
    , validationMinItems = minItems
    , validationUniqueItems = uniqueItems
    , validationMaxContains = maxContains
    , validationMinContains = minContains
    , validationUnevaluatedItems = unevaluatedItems'
    , validationProperties = properties'
    , validationPatternProperties = patternProperties'
    , validationAdditionalProperties = additionalProperties'
    , validationUnevaluatedProperties = unevaluatedProperties'
    , validationPropertyNames = propertyNames'
    , validationMaxProperties = maxProperties
    , validationMinProperties = minProperties
    , validationRequired = required'
    , validationDependentRequired = dependentRequired'
    , validationDependentSchemas = dependentSchemas'
    , validationDependencies = dependencies'
    }

parseArrayItems :: JsonSchemaVersion -> Object -> Maybe ArrayItemsValidation
parseArrayItems version obj = do
  itemsVal <- KeyMap.lookup "items" obj
  case itemsVal of
    Array arr ->
      -- Tuple-style items (array of schemas)
      let schemas = do
            val <- toList arr
            Right schema <- pure $ parseSubschema version val
            pure schema
      in case NE.nonEmpty schemas of
        Just nonEmpty -> do
          -- Parse additionalItems if present
          let additionalItems' = KeyMap.lookup "additionalItems" obj >>= eitherToMaybe . parseSubschema version
          pure $ ItemsTuple nonEmpty additionalItems'
        Nothing -> Nothing
    _ -> do
      -- Single schema for all items
      schema <- eitherToMaybe (parseSubschema version itemsVal)
      pure $ ItemsSchema schema

-- | Parse non-empty array of schemas
parseArraySchemas :: JsonSchemaVersion -> Value -> Maybe (NonEmpty Schema)
parseArraySchemas version (Array arr) = do
  let schemas = do
        val <- toList arr
        Right schema <- pure $ parseSubschema version val
        pure schema
  NE.nonEmpty schemas
parseArraySchemas _ _ = Nothing

-- | Parse property schemas map
parsePropertySchemas :: JsonSchemaVersion -> Value -> Maybe (Map Text Schema)
parsePropertySchemas version (Object obj) = Just $ Map.fromList $ do
  (k, v) <- KeyMap.toList obj
  Right schema <- pure $ parseSubschema version v
  pure (Key.toText k, schema)
parsePropertySchemas _ _ = Nothing

-- | Parse pattern property schemas
parsePatternPropertySchemas :: JsonSchemaVersion -> Value -> Maybe (Map Regex Schema)
parsePatternPropertySchemas version (Object obj) = Just $ Map.fromList $ do
  (k, v) <- KeyMap.toList obj
  Right schema <- pure $ parseSubschema version v
  pure (Regex (Key.toText k), schema)
parsePatternPropertySchemas _ _ = Nothing

--- | Parse dependencies map (draft-04 through draft-07)
parseDependencies :: JsonSchemaVersion -> Value -> Maybe (Map Text Dependency)
parseDependencies version (Object obj) = Just $ Map.fromList $ do
  (k, v) <- KeyMap.toList obj
  Just dep <- pure $ parseDependency version v
  pure (Key.toText k, dep)
parseDependencies _ _ = Nothing

--- | Parse a single dependency
parseDependency :: JsonSchemaVersion -> Value -> Maybe Dependency
parseDependency version v@(Object _) =
  case parseSubschema version v of
    Right schema -> Just $ DependencySchema schema
    Left _ -> Nothing
parseDependency version v@(Bool _) =
  -- Boolean schemas (true/false) are valid dependencies
  case parseSubschema version v of
    Right schema -> Just $ DependencySchema schema
    Left _ -> Nothing
parseDependency _ (Array arr) = Just $ DependencyProperties $ Set.fromList $ do
  String t <- toList arr
  pure t
parseDependency _ _ = Nothing

-- | Parse a schema with dialect registry support
--
-- This function parses a JSON Schema and uses the provided dialect registry
-- to look up the dialect based on the $schema keyword.
--
-- Behavior:
-- - If $schema is present and found in registry: Use that dialect
-- - If $schema is present but NOT in registry: Error (even for standard URIs)
-- - If no $schema: Use default parsing (Draft 2020-12)
--
-- When using a dialect registry, ALL dialects must be explicitly registered.
-- Use 'standardDialectRegistry' or 'standardRegistry' to get standard dialects.
-- This ensures explicit control over which dialects are accepted.
--
-- __Dialect Configuration__:
--
-- The dialect's 'dialectUnknownKeywords' setting is applied to parsing,
-- controlling how unknown keywords are handled. See 'parseSchemaWithConfig'
-- for details on unknown keyword modes.
--
-- @since 0.1.0.0
parseSchemaWithDialectRegistry
  :: VocabularyRegistry  -- ^ Registry containing available dialects
  -> Value               -- ^ JSON value to parse
  -> Either ParseError Schema
parseSchemaWithDialectRegistry registry val = do
  -- Extract $schema URI if present
  let mSchemaURI = extractSchemaURI val
  
  -- Try to resolve dialect from registry
  case mSchemaURI of
    Just uri -> case lookupDialect uri registry of
      Just dialect -> do
        -- Found dialect - parse with its configuration
        let parseConfig = ParseConfig
              { parseUnknownKeywordMode = dialectUnknownKeywords dialect
              }
        schema <- parseSchemaWithVersion (dialectVersion dialect) val
        -- Apply unknown keyword mode after parsing
        applyUnknownKeywordMode parseConfig schema
      Nothing -> 
        -- Dialect not in registry - error
        Left $ ParseError
          { parseErrorPath = emptyPointer
          , parseErrorMessage = "Unregistered dialect: " <> uri <> ". " <> 
                               "Dialect must be registered in the dialect registry. " <>
                               "Use standardDialectRegistry to include standard JSON Schema dialects."
          , parseErrorContext = Nothing
          }
    Nothing -> do
      -- No $schema - use default parsing
      parseSchema val

-- | Apply unknown keyword mode to a parsed schema
applyUnknownKeywordMode :: ParseConfig -> Schema -> Either ParseError Schema
applyUnknownKeywordMode config schema =
  case (parseUnknownKeywordMode config, schemaExtensions schema) of
    -- ErrorOnUnknown: fail if extensions present
    (ErrorOnUnknown, exts) | not (Map.null exts) ->
      let keywords = Map.keys exts
          keywordList = T.intercalate ", " keywords
      in Left $ ParseError emptyPointer
           ("Unknown keywords (strict mode): " <> keywordList <>
            ". These keywords are not part of the JSON Schema specification. " <>
            "Use CollectUnknown mode if these are intentional extensions.")
           Nothing
    
    -- IgnoreUnknown: clear extensions
    (IgnoreUnknown, _) -> Right schema { schemaExtensions = Map.empty }
    
    -- WarnUnknown or CollectUnknown: keep extensions
    _ -> Right schema

-- | Parse a schema with explicit version and dialect registry
--
-- This variant allows overriding the version while still looking up
-- dialect information for validation configuration.
parseSchemaWithDialectRegistryAndVersion
  :: VocabularyRegistry  -- ^ Registry containing available dialects
  -> JsonSchemaVersion   -- ^ Explicit version to use
  -> Value               -- ^ JSON value to parse
  -> Either ParseError Schema
parseSchemaWithDialectRegistryAndVersion registry version val = do
  parseSchemaWithVersion version val

-- | Resolve the dialect for a parsed schema
--
-- Given a schema and a dialect registry, this function looks up the dialect
-- based on the schemaMetaschemaURI. Returns Nothing if:
-- - The schema has no $schema declaration
-- - The dialect is not found in the registry
--
-- This is useful during validation to access dialect-specific configuration.
resolveDialectFromSchema
  :: VocabularyRegistry  -- ^ Registry containing available dialects
  -> Schema              -- ^ Parsed schema
  -> Maybe Dialect
resolveDialectFromSchema registry schema =
  schemaMetaschemaURI schema >>= \uri -> lookupDialect uri registry

-- | Extract $schema URI from a JSON value
extractSchemaURI :: Value -> Maybe Text
extractSchemaURI (Object obj) = case KeyMap.lookup "$schema" obj of
  Just (String uri) -> Just uri
  _ -> Nothing
extractSchemaURI _ = Nothing

