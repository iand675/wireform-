{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'not' composition keyword
--
-- The not keyword requires that the instance does NOT validate against the
-- provided schema. This is an applicator keyword that recursively validates
-- a subschema and inverts the result.
module JsonSchema.Keywords.Not
  ( validateNot
  , notKeyword
  , NotData(..)
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
import qualified Data.Text as T

import JsonSchema.Types (Schema, SchemaCore(..), ValidationResult, pattern ValidationSuccess, pattern ValidationFailure, ValidationAnnotations, validationFailure, schemaCore, schemaNot, schemaRawKeywords, schemaVersion, JsonSchemaVersion(..))
import JsonSchema.Parser.Internal (parseSchema)
import qualified JsonSchema.Parser.Internal as ParserInternal
import JsonSchema.Types (ParseError)
import JsonSchema.Keyword.Types (KeywordDefinition(..), CompileFunc, ValidateFunc, KeywordNavigation(..), CompilationContext(..), ValidationContext')
import JsonSchema.Keyword (mkKeywordDefinition)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

-- | Compiled data for 'not' keyword
newtype NotData = NotData Schema

-- | Compile function for 'not' keyword
compileNot :: CompileFunc NotData
compileNot value _schema _ctx = case parseSchema value of
  Left err -> Left $ "Invalid schema in not: " <> T.pack (show err)
  Right schema -> Right $ NotData schema

-- | Validate that a value does NOT satisfy the schema in 'not'
--
-- Parameters:
-- - recursiveValidator: Recursive validation function for subschemas
-- - NotData schema: The compiled schema that must NOT validate
-- - ctx: Validation context (unused for 'not')
-- - value: The instance value to validate
--
-- Returns:
-- - ValidationSuccess with no annotations if the schema does NOT validate
-- - ValidationFailure if the schema DOES validate
validateNot
  :: (Schema -> Value -> ValidationResult)  -- ^ Recursive validator
  -> Schema                                  -- ^ Schema that must NOT validate
  -> Value                                   -- ^ Value to validate
  -> ValidationResult
validateNot validateSchema schema value =
  case validateSchema schema value of
    ValidationSuccess _ -> validationFailure "not" "Value matches schema in 'not'"
    ValidationFailure _ -> ValidationSuccess mempty

-- | ValidateFunc for 'not' keyword (pluggable system)
validateNotKeyword :: ValidateFunc NotData
validateNotKeyword recursiveValidator (NotData schema) _ctx value =
  pure $
    case recursiveValidator schema value of
      ValidationSuccess _ -> validationFailure "not" "Value matches schema in 'not'"
      ValidationFailure _ -> ValidationSuccess mempty

-- | Keyword definition for 'not'
notKeyword :: KeywordDefinition
notKeyword = KeywordDefinition
  { keywordName = "not"
  , keywordCompile = compileNot
  , keywordValidate = validateNotKeyword
  , keywordNavigation = SingleSchema $ \schema -> case schemaCore schema of
      ObjectSchema obj -> 
        -- Check pre-parsed first, then parse on-demand
        case schemaNot obj of
          Just notSchema -> Just notSchema
          Nothing -> parseNotFromRaw schema
      _ -> Nothing
  , keywordPostValidate = Nothing
  }
  where
    parseNotFromRaw :: Schema -> Maybe Schema
    parseNotFromRaw s = case Map.lookup "not" (schemaRawKeywords s) of
      Just val ->
        let version = fromMaybe Draft202012 (schemaVersion s)
        in case ParserInternal.parseSchemaValue version val of
          Right schema -> Just schema
          Left _ -> Nothing
      _ -> Nothing

