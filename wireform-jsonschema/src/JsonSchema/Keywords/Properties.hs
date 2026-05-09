{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'properties' keyword
--
-- The properties keyword validates specific object properties against
-- their corresponding schemas. Each property name maps to a schema that
-- validates values for that property.
module JsonSchema.Keywords.Properties
  ( propertiesKeyword
  , compileProperties
  , PropertiesData(..)
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (sconcat)

import JsonSchema.Types 
  ( Schema(..), SchemaCore(..), SchemaObject(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , validationProperties, schemaValidation, ValidationAnnotations(..)
  , schemaRawKeywords, schemaVersion, JsonSchemaVersion(..)
  )
import JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..)
  , CompilationContext(..)
  )
import JsonSchema.Parser.Internal (parseSchema)
import qualified JsonSchema.Parser.Internal as ParserInternal
import Data.Maybe (fromMaybe, mapMaybe)
import JsonSchema.Validator.Annotations
  ( annotateProperties
  , propertyPointer
  , shiftAnnotations
  )

-- | Compiled data for properties keyword
newtype PropertiesData = PropertiesData (Map Text Schema)
  deriving (Typeable)

-- | Compile the properties keyword
compileProperties :: CompileFunc PropertiesData
compileProperties (Object obj) _schema ctx = do
  -- Parse each property value as a schema
  let entries = KeyMap.toList obj
  schemas <- mapM parseEntry entries
  Right $ PropertiesData $ Map.fromList schemas
  where
    parseEntry (k, v) = case (contextParseSubschema ctx) v of
      Left err -> Left $ "Invalid schema for property '" <> Key.toText k <> "': " <> T.pack (show err)
      Right schema -> Right (Key.toText k, schema)

compileProperties _ _ _ = Left "properties must be an object"

-- | Validate properties using the pluggable keyword system
validatePropertiesKeyword :: ValidateFunc PropertiesData
validatePropertiesKeyword recursiveValidator (PropertiesData propSchemas) _ctx (Object objMap) =
  let evaluations = do
        (propName, propSchema) <- Map.toList propSchemas
        Just propValue <- pure $ KeyMap.lookup (Key.fromText propName) objMap
        pure (propName, recursiveValidator propSchema propValue)
      evaluatedProps = Set.fromList $ map fst evaluations
      failures = do
        (_, ValidationFailure errs) <- evaluations
        pure errs
      shiftedAnnotations = do
        (propName, ValidationSuccess anns) <- evaluations
        pure $ shiftAnnotations (propertyPointer propName) anns
  in pure $
       case failures of
         [] -> ValidationSuccess (annotateProperties evaluatedProps <> mconcat shiftedAnnotations)
         failures' -> ValidationFailure $ sconcat (NE.fromList failures')

validatePropertiesKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to objects

-- | Keyword definition for properties
propertiesKeyword :: KeywordDefinition
propertiesKeyword = KeywordDefinition
  { keywordName = "properties"
  , keywordCompile = compileProperties
  , keywordValidate = validatePropertiesKeyword
  , keywordNavigation = SchemaMap $ \schema -> case schemaCore schema of
      ObjectSchema obj -> 
        -- Check pre-parsed first, then parse on-demand
        case validationProperties (schemaValidation obj) of
          Just props -> Just props
          Nothing -> parsePropertiesFromRaw schema
      _ -> Nothing
  , keywordPostValidate = Nothing
  }
  where
    parsePropertiesFromRaw :: Schema -> Maybe (Map Text Schema)
    parsePropertiesFromRaw s = case Map.lookup "properties" (schemaRawKeywords s) of
      Just (Object propsObj) ->
        let version = fromMaybe Draft202012 (schemaVersion s)
            entries = KeyMap.toList propsObj
            parseEntry (k, v) = case ParserInternal.parseSchemaValue version v of
              Right schema -> Just (Key.toText k, schema)
              Left _ -> Nothing
            parsedMap = Map.fromList $ mapMaybe parseEntry entries
        in if Map.null parsedMap && not (KeyMap.null propsObj)
           then Nothing  -- Had properties but all failed to parse
           else Just parsedMap  -- Either successfully parsed or was empty to begin with
      _ -> Nothing

