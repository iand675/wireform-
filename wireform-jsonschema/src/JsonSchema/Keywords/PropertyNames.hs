{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'propertyNames' keyword
--
-- The propertyNames keyword validates that all property names in an object
-- match a given schema. Property names are validated as strings.
module JsonSchema.Keywords.PropertyNames
  ( propertyNamesKeyword
  , compilePropertyNames
  , PropertyNamesData(..)
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)

import JsonSchema.Types 
  ( Schema(..), SchemaCore(..), SchemaObject(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , validationPropertyNames, schemaValidation
  , schemaRawKeywords, schemaVersion, JsonSchemaVersion(..)
  )
import JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..)
  , combineValidationResults
  )
import JsonSchema.Parser.Internal (parseSchema)
import qualified JsonSchema.Parser.Internal as ParserInternal
import Data.Maybe (fromMaybe)

-- | Compiled data for propertyNames keyword
newtype PropertyNamesData = PropertyNamesData Schema
  deriving (Typeable)

-- | Compile the propertyNames keyword
compilePropertyNames :: CompileFunc PropertyNamesData
compilePropertyNames value _schema _ctx = case parseSchema value of
  Left err -> Left $ "Invalid schema in propertyNames: " <> T.pack (show err)
  Right schema -> Right $ PropertyNamesData schema

-- | Validate propertyNames using the pluggable keyword system
validatePropertyNamesKeyword :: ValidateFunc PropertyNamesData
validatePropertyNamesKeyword recursiveValidator (PropertyNamesData nameSchema) _ctx (Object objMap) =
  -- Validate each property name (as a string) against the schema
  let propNames = [Key.toText k | k <- KeyMap.keys objMap]
      results = [recursiveValidator nameSchema (String propName) | propName <- propNames]
  in pure $ combineValidationResults results

validatePropertyNamesKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to objects

-- | Keyword definition for propertyNames
propertyNamesKeyword :: KeywordDefinition
propertyNamesKeyword = KeywordDefinition
  { keywordName = "propertyNames"
  , keywordCompile = compilePropertyNames
  , keywordValidate = validatePropertyNamesKeyword
  , keywordNavigation = SingleSchema $ \schema -> case schemaCore schema of
      ObjectSchema obj -> 
        -- Check pre-parsed first, then parse on-demand
        case validationPropertyNames (schemaValidation obj) of
          Just propNames -> Just propNames
          Nothing -> parsePropertyNamesFromRaw schema
      _ -> Nothing
  , keywordPostValidate = Nothing
  }
  where
    parsePropertyNamesFromRaw :: Schema -> Maybe Schema
    parsePropertyNamesFromRaw s = case Map.lookup "propertyNames" (schemaRawKeywords s) of
      Just val ->
        let version = fromMaybe Draft202012 (schemaVersion s)
        in case ParserInternal.parseSchemaValue version val of
          Right schema -> Just schema
          Left _ -> Nothing
      _ -> Nothing

