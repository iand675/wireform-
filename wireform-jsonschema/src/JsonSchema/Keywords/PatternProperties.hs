{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'patternProperties' keyword
--
-- The patternProperties keyword validates object properties whose names
-- match specific regex patterns. Each pattern maps to a schema that
-- validates values for properties matching that pattern.
module JsonSchema.Keywords.PatternProperties
  ( patternPropertiesKeyword
  , compilePatternProperties
  , PatternPropertiesData(..)
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
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
  ( Schema(..), SchemaCore(..), SchemaObject(..), Regex(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , validationPatternProperties, schemaValidation
  , schemaRawKeywords, schemaVersion, JsonSchemaVersion(..)
  )
import JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..)
  )
import JsonSchema.Parser.Internal (parseSchema)
import qualified JsonSchema.Parser.Internal as ParserInternal
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified JsonSchema.Regex as RegexModule
import JsonSchema.Validator.Annotations
  ( annotateProperties
  , propertyPointer
  , shiftAnnotations
  )

-- | Compiled data for patternProperties keyword
newtype PatternPropertiesData = PatternPropertiesData (Map Regex Schema)
  deriving (Typeable)

-- | Compile the patternProperties keyword
compilePatternProperties :: CompileFunc PatternPropertiesData
compilePatternProperties (Object obj) _schema _ctx = do
  -- Parse each property value as a schema, keys are regex patterns
  let entries = KeyMap.toList obj
  schemas <- mapM parseEntry entries
  Right $ PatternPropertiesData $ Map.fromList schemas
  where
    parseEntry (k, v) = case parseSchema v of
      Left err -> Left $ "Invalid schema for pattern '" <> Key.toText k <> "': " <> T.pack (show err)
      Right schema -> Right (Regex (Key.toText k), schema)

compilePatternProperties _ _ _ = Left "patternProperties must be an object"

-- | Validate patternProperties using the pluggable keyword system
validatePatternPropertiesKeyword :: ValidateFunc PatternPropertiesData
validatePatternPropertiesKeyword recursiveValidator (PatternPropertiesData patternSchemas) _ctx (Object objMap) =
  let matches = do
        (k, propValue) <- KeyMap.toList objMap
        let propName = Key.toText k
        (Regex patternText, patternSchema) <- Map.toList patternSchemas
        case RegexModule.compileRegex patternText of
          Right regex | RegexModule.matchRegex regex propName ->
            pure (propName, recursiveValidator patternSchema propValue)
          _ -> []
      matchedProps = Set.fromList $ map fst matches
      failures = do
        (_, ValidationFailure errs) <- matches
        pure errs
      shiftedAnnotations = do
        (propName, ValidationSuccess anns) <- matches
        pure $ shiftAnnotations (propertyPointer propName) anns
  in pure $
       case failures of
         [] -> ValidationSuccess (annotateProperties matchedProps <> mconcat shiftedAnnotations)
         failures' -> ValidationFailure $ sconcat (NE.fromList failures')

validatePatternPropertiesKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to objects

-- | Keyword definition for patternProperties
patternPropertiesKeyword :: KeywordDefinition
patternPropertiesKeyword = KeywordDefinition
  { keywordName = "patternProperties"
  , keywordCompile = compilePatternProperties
  , keywordValidate = validatePatternPropertiesKeyword
  , keywordNavigation = SchemaMap $ \schema -> case schemaCore schema of
      ObjectSchema obj -> 
        -- Check pre-parsed first, then parse on-demand
        case validationPatternProperties (schemaValidation obj) of
          Just patterns ->
            -- Convert Regex keys to Text keys for navigation
            Just $ Map.fromList $ do
              (Regex pat, schema') <- Map.toList patterns
              pure (pat, schema')
          Nothing -> parsePatternPropertiesFromRaw schema
      _ -> Nothing
  , keywordPostValidate = Nothing
  }
  where
    parsePatternPropertiesFromRaw :: Schema -> Maybe (Map Text Schema)
    parsePatternPropertiesFromRaw s = case Map.lookup "patternProperties" (schemaRawKeywords s) of
      Just (Object patternsObj) ->
        let version = fromMaybe Draft202012 (schemaVersion s)
            entries = KeyMap.toList patternsObj
            parseEntry (k, v) = case ParserInternal.parseSchemaValue version v of
              Right schema -> Just (Key.toText k, schema)
              Left _ -> Nothing
        in Just $ Map.fromList $ mapMaybe parseEntry entries
      _ -> Nothing

