{-# LANGUAGE PatternSynonyms #-}
-- | Internal parser functions shared between Parser and Validator
--
-- This module contains the core parsing logic that both Parser and Validator
-- need, breaking the circular dependency between them.
--
-- NOTE: This is an internal module and should not be imported directly by user code.
-- It is exposed only to break circular dependencies between Parser and Validator.
module JsonSchema.Parser.Internal
  ( parseSchemaValue
  , parseSchema
  ) where

import JsonSchema.Types
  ( Schema(..), SchemaCore(..), SchemaObject(..), ObjectSchemaData(..), SchemaAnnotations(..), SchemaValidation(..)
  , SchemaType(..), OneOrMany(..), ArrayItemsValidation(..), Dependency(..), Regex(..), Reference(..)
  , JsonSchemaVersion(..), ParseError(..), JsonPointer(..), emptyPointer
  )
import Data.Aeson (Value(..), Object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, mapMaybe)

-- | Parse a schema value with explicit version
--
-- This is a minimal parser that handles the core structure needed by Validator
-- when navigating through schema extensions. It doesn't do full validation or
-- metaschema checking - that's handled by the full Parser module.
parseSchemaValue :: JsonSchemaVersion -> Value -> Either ParseError Schema
parseSchemaValue version val = case val of
  Bool b -> Right $ Schema
    { schemaVersion = Just version
    , schemaMetaschemaURI = Nothing
    , schemaId = Nothing
    , schemaCore = BooleanSchema b
    , schemaVocabulary = Nothing
    , schemaExtensions = Map.empty
    , schemaRawKeywords = Map.empty
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

    -- Parse core structure (minimal - just enough for navigation)
    core <- parseSchemaObjectMinimal version obj

    -- Collect all keywords for raw storage
    let allKeywords = Map.fromList $ do
          (k, v) <- KeyMap.toList obj
          pure (Key.toText k, v)

    pure $ Schema
      { schemaVersion = Just version
      , schemaMetaschemaURI = metaschemaURI
      , schemaId = schemaId'
      , schemaCore = ObjectSchema core
      , schemaVocabulary = vocabMap
      , schemaExtensions = Map.empty  -- Extensions handled by full parser
      , schemaRawKeywords = allKeywords
      }
  _ -> Left $ ParseError emptyPointer "Schema must be boolean or object" (Just val)

-- | Parse schema object minimally (just enough for navigation)
parseSchemaObjectMinimal :: JsonSchemaVersion -> Object -> Either ParseError SchemaObject
parseSchemaObjectMinimal version obj = do
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
  
  -- Parse $defs and legacy definitions (both forms may appear)
  -- Check both keys and merge them, just like the full parser
  let parseDefsFor key = case KeyMap.lookup key obj of
        Just (Object defsObj) -> 
          let defsList = KeyMap.toList defsObj
              parseDef (k, v) = case parseSchemaValue version v of
                Right s -> Just (Key.toText k, s)
                Left _ -> Nothing
          in Map.fromList $ mapMaybe parseDef defsList
        _ -> Map.empty
  let schemaDefs' = 
        let newDefs = parseDefsFor (Key.fromText "$defs")
            legacyDefs = parseDefsFor (Key.fromText "definitions")
        in Just $ Map.union newDefs legacyDefs
  
  -- Parse $dynamicRef (2020-12+)
  let schemaDynamicRef' = if version >= Draft202012
                          then KeyMap.lookup "$dynamicRef" obj >>= \v -> case v of
                            String t -> Just (Reference t)
                            _ -> Nothing
                          else Nothing
  
  -- Parse $recursiveRef (2019-09 only, replaced by $dynamicRef in 2020-12)
  let schemaRecursiveRef' = if version == Draft201909
                            then KeyMap.lookup "$recursiveRef" obj >>= \v -> case v of
                              String t -> Just (Reference t)
                              _ -> Nothing
                            else Nothing
  
  -- Parse $anchor (2019-09+)
  let schemaAnchor' = if version >= Draft201909
                      then KeyMap.lookup "$anchor" obj >>= \v -> case v of
                        String t -> Just t
                        _ -> Nothing
                      else Nothing
  
  -- Parse $dynamicAnchor (2020-12+)
  let schemaDynamicAnchor' = if version >= Draft202012
                             then KeyMap.lookup "$dynamicAnchor" obj >>= \v -> case v of
                               String t -> Just t
                               _ -> Nothing
                             else Nothing
  
  -- Parse $recursiveAnchor (2019-09 only, replaced by $dynamicAnchor in 2020-12)
  let schemaRecursiveAnchor' = if version == Draft201909
                               then KeyMap.lookup "$recursiveAnchor" obj >>= \v -> case v of
                                 Bool b -> Just b
                                 _ -> Nothing
                               else Nothing
  
  -- Minimal parsing - just create empty structures for composition and conditional keywords
  -- These are not needed for basic navigation, so we leave them as Nothing
  pure $ SchemaObject $ Right $ ObjectSchemaData
    { objectSchemaDataType = schemaType'
    , objectSchemaDataEnum = schemaEnum'
    , objectSchemaDataConst = schemaConst'
    , objectSchemaDataRef = schemaRef'
    , objectSchemaDataDynamicRef = schemaDynamicRef'
    , objectSchemaDataAnchor = schemaAnchor'
    , objectSchemaDataDynamicAnchor = schemaDynamicAnchor'
    , objectSchemaDataRecursiveRef = schemaRecursiveRef'
    , objectSchemaDataRecursiveAnchor = schemaRecursiveAnchor'
    , objectSchemaDataAllOf = Nothing
    , objectSchemaDataAnyOf = Nothing
    , objectSchemaDataOneOf = Nothing
    , objectSchemaDataNot = Nothing
    , objectSchemaDataIf = Nothing
    , objectSchemaDataThen = Nothing
    , objectSchemaDataElse = Nothing
    , objectSchemaDataDefs = fromMaybe Map.empty schemaDefs'
    , objectSchemaDataValidation = SchemaValidation
        { validationMultipleOf = Nothing
        , validationMaximum = Nothing
        , validationExclusiveMaximum = Nothing
        , validationMinimum = Nothing
        , validationExclusiveMinimum = Nothing
        , validationMaxLength = Nothing
        , validationMinLength = Nothing
        , validationPattern = Nothing
        , validationFormat = Nothing
        , validationContentEncoding = Nothing
        , validationContentMediaType = Nothing
        , validationItems = Nothing
        , validationPrefixItems = Nothing
        , validationContains = Nothing
        , validationMaxItems = Nothing
        , validationMinItems = Nothing
        , validationUniqueItems = Nothing
        , validationMaxContains = Nothing
        , validationMinContains = Nothing
        , validationUnevaluatedItems = Nothing
        , validationProperties = Nothing
        , validationPatternProperties = Nothing
        , validationAdditionalProperties = Nothing
        , validationUnevaluatedProperties = Nothing
        , validationPropertyNames = Nothing
        , validationMaxProperties = Nothing
        , validationMinProperties = Nothing
        , validationRequired = Nothing
        , validationDependentRequired = Nothing
        , validationDependentSchemas = Nothing
        , validationDependencies = Nothing
        }
    , objectSchemaDataAnnotations = SchemaAnnotations
        { annotationTitle = Nothing
        , annotationDescription = Nothing
        , annotationDefault = Nothing
        , annotationDeprecated = Nothing
        , annotationReadOnly = Nothing
        , annotationWriteOnly = Nothing
        , annotationExamples = []
        , annotationComment = Nothing
        , annotationCodegen = Nothing
        }
    }

-- | Parse schema type string
parseSchemaType :: Text -> Maybe SchemaType
parseSchemaType "string" = Just StringType
parseSchemaType "number" = Just NumberType
parseSchemaType "integer" = Just IntegerType
parseSchemaType "boolean" = Just BooleanType
parseSchemaType "null" = Just NullType
parseSchemaType "array" = Just ArrayType
parseSchemaType "object" = Just ObjectType
parseSchemaType _ = Nothing

-- | Parse a schema with automatic version detection
--
-- This is a minimal version that doesn't do full validation or metaschema checking.
-- For full parsing with validation, use the Parser module.
parseSchema :: Value -> Either ParseError Schema
parseSchema val = do
  version <- detectVersion val
  parseSchemaValue version val

-- | Detect schema version from $schema keyword
detectVersion :: Value -> Either ParseError JsonSchemaVersion
detectVersion (Object obj) = case KeyMap.lookup "$schema" obj of
  Nothing -> pure Draft202012  -- Default to latest
  Just uri -> case Aeson.fromJSON uri of
    Aeson.Success ver -> pure ver
    Aeson.Error err -> Left $ ParseError emptyPointer (T.pack err) Nothing
detectVersion _ = pure Draft202012  -- Boolean schemas default to latest

