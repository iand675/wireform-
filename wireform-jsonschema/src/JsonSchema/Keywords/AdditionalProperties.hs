{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'additionalProperties' keyword
--
-- The additionalProperties keyword validates object properties that are NOT
-- covered by the 'properties' or 'patternProperties' keywords. It applies
-- its schema to all "additional" properties.
module JsonSchema.Keywords.AdditionalProperties
  ( additionalPropertiesKeyword
  , compileAdditionalProperties
  , AdditionalPropertiesData(..)
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

import JsonSchema.Types 
  ( Schema(..), SchemaCore(..), SchemaObject(..), Regex(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , validationAdditionalProperties, validationProperties, validationPatternProperties
  , schemaValidation, ValidationAnnotations(..), schemaRawKeywords
  , schemaVersion, JsonSchemaVersion(..)
  )
import JsonSchema.Keywords.Common (extractPropertyNames)
import qualified JsonSchema.Parser.Internal as ParserInternal
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (sconcat)
import JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..)
  , CompilationContext(..), contextParseSubschema
  )
import JsonSchema.Parser.Internal (parseSchemaValue)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import Data.Maybe (fromMaybe)
import Data.Foldable (toList)
import qualified JsonSchema.Regex as RegexModule
import JsonSchema.Validator.Annotations
  ( annotateProperties
  , propertyPointer
  , shiftAnnotations
  )

-- | Compiled data for additionalProperties keyword
data AdditionalPropertiesData = AdditionalPropertiesData 
  { addlPropsSchema :: Schema
  , addlPropsDefinedProps :: Set Text  -- Properties covered by 'properties'
  , addlPropsPatterns :: [Regex]       -- Patterns from 'patternProperties'
  }
  deriving (Typeable)

-- | Compile the additionalProperties keyword
compileAdditionalProperties :: CompileFunc AdditionalPropertiesData
compileAdditionalProperties value schema ctx = do
  -- Parse the additionalProperties value as a schema
  addlSchema <- case contextParseSubschema ctx value of
    Left err -> Left $ "Invalid schema in additionalProperties: " <> err
    Right s -> Right s
  
  -- Read adjacent 'properties' and 'patternProperties' keywords from schema
  -- Parse on-demand from raw keywords if not pre-parsed
  (definedProps, patterns) <- case schemaCore schema of
        ObjectSchema obj ->
          let validation = schemaValidation obj
              props = case validationProperties validation of
                Just p -> Map.keysSet p
                Nothing -> parsePropertiesKeysFromRaw schema
              pats = case validationPatternProperties validation of
                Just pp -> map fst (Map.toList pp)
                Nothing -> parsePatternPropertiesKeysFromRaw schema
          in Right (props, pats)
        _ -> Left "additionalProperties can only appear in object schemas"
  
  Right $ AdditionalPropertiesData addlSchema definedProps patterns
  where
    parsePropertiesKeysFromRaw :: Schema -> Set Text
    parsePropertiesKeysFromRaw s = case Map.lookup "properties" (schemaRawKeywords s) of
      Just (Object propsObj) -> extractPropertyNames propsObj
      _ -> Set.empty
    
    parsePatternPropertiesKeysFromRaw :: Schema -> [Regex]
    parsePatternPropertiesKeysFromRaw s = case Map.lookup "patternProperties" (schemaRawKeywords s) of
      Just (Object patternsObj) -> do
        k <- KeyMap.keys patternsObj
        pure $ Regex (Key.toText k)
      _ -> []

-- | Check if a property name matches any pattern
matchesAnyPattern :: Text -> [Regex] -> Bool
matchesAnyPattern propName patterns = any matchesPattern patterns
  where
    matchesPattern (Regex patternText) = case RegexModule.compileRegex patternText of
      Right regex -> RegexModule.matchRegex regex propName
      Left _ -> False

-- | Validate additionalProperties using the pluggable keyword system
validateAdditionalPropertiesKeyword :: ValidateFunc AdditionalPropertiesData
validateAdditionalPropertiesKeyword recursiveValidator (AdditionalPropertiesData addlSchema definedProps patterns) _ctx (Object objMap) =
  let additionalProps =
        [ (Key.toText k, v)
        | (k, v) <- KeyMap.toList objMap
        , let propName = Key.toText k
        , not (Set.member propName definedProps)
        , not (matchesAnyPattern propName patterns)
        ]
      evaluations =
        [ (propName, recursiveValidator addlSchema propValue)
        | (propName, propValue) <- additionalProps
        ]
      additionalPropNames = Set.fromList $ map fst evaluations
      failures = do
        (_, ValidationFailure errs) <- evaluations
        pure errs
      shiftedAnnotations = do
        (propName, ValidationSuccess anns) <- evaluations
        pure $ shiftAnnotations (propertyPointer propName) anns
  in pure $
       case failures of
         [] -> ValidationSuccess (annotateProperties additionalPropNames <> mconcat shiftedAnnotations)
         failures' -> ValidationFailure $ sconcat (NE.fromList failures')

validateAdditionalPropertiesKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to objects

-- | Keyword definition for additionalProperties
additionalPropertiesKeyword :: KeywordDefinition
additionalPropertiesKeyword = KeywordDefinition
  { keywordName = "additionalProperties"
  , keywordCompile = compileAdditionalProperties
  , keywordValidate = validateAdditionalPropertiesKeyword
  , keywordNavigation = SingleSchema $ \schema -> case schemaCore schema of
      ObjectSchema obj -> 
        -- Check pre-parsed first, then parse on-demand
        case validationAdditionalProperties (schemaValidation obj) of
          Just addl -> Just addl
          Nothing -> parseAdditionalPropertiesFromRaw schema
      _ -> Nothing
  , keywordPostValidate = Nothing
  }
  where
    parseAdditionalPropertiesFromRaw :: Schema -> Maybe Schema
    parseAdditionalPropertiesFromRaw s = case Map.lookup "additionalProperties" (schemaRawKeywords s) of
      Just (Bool b) ->
        let version = fromMaybe Draft202012 (schemaVersion s)
        in case ParserInternal.parseSchemaValue version (Bool b) of
          Right schema -> Just schema
          Left _ -> Nothing
      Just val ->
        let version = fromMaybe Draft202012 (schemaVersion s)
        in case ParserInternal.parseSchemaValue version val of
          Right schema -> Just schema
          Left _ -> Nothing
      _ -> Nothing

