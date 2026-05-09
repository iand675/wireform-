{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of the 'type' keyword
--
-- The type keyword restricts a value to a specific JSON type or union of types.
-- For arrays of types, validation succeeds if the value matches any type in the array.
module JsonSchema.Keywords.Type
  ( typeKeyword
    -- * Backward compatibility
  , validateTypeConstraint
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Foldable (toList)
import qualified Data.Scientific as Sci
import qualified Data.List.NonEmpty as NE

import JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordNavigation(..), CompileFunc, ValidateFunc)
import JsonSchema.Types (Schema, SchemaType(..), SchemaObject(..), OneOrMany(..), ValidationResult, pattern ValidationSuccess, pattern ValidationFailure, validationFailure, schemaType)

-- | Compiled data for the 'type' keyword
data TypeData = TypeData
  { typeExpectedTypes :: [SchemaType]
  } deriving (Show, Eq, Typeable)

-- | Compile function for 'type' keyword
compileType :: CompileFunc TypeData
compileType value _schema _ctx = case value of
  String typeStr -> case parseSchemaType typeStr of
    Just t -> Right $ TypeData { typeExpectedTypes = [t] }
    Nothing -> Left $ "Unknown type: " <> typeStr
  Array arr -> do
    types <- mapM parseTypeFromValue (toList arr)
    Right $ TypeData { typeExpectedTypes = types }
  _ -> Left "type must be a string or array of strings"
  where
    parseTypeFromValue (String s) = case parseSchemaType s of
      Just t -> Right t
      Nothing -> Left $ "Unknown type: " <> s
    parseTypeFromValue _ = Left "type array must contain only strings"

    parseSchemaType :: Text -> Maybe SchemaType
    parseSchemaType "null" = Just NullType
    parseSchemaType "boolean" = Just BooleanType
    parseSchemaType "string" = Just StringType
    parseSchemaType "number" = Just NumberType
    parseSchemaType "integer" = Just IntegerType
    parseSchemaType "object" = Just ObjectType
    parseSchemaType "array" = Just ArrayType
    parseSchemaType _ = Nothing

-- | Validate function for 'type' keyword
validateType :: ValidateFunc TypeData
validateType _recursiveValidator (TypeData expectedTypes) _ctx actual =
  if any (matchesType actual) expectedTypes
    then pure (ValidationSuccess mempty)
    else pure (validationFailure "type" $
                "Expected one of: " <> T.intercalate ", " (map (T.pack . show) expectedTypes))
  where
    matchesType :: Value -> SchemaType -> Bool
    matchesType Null NullType = True
    matchesType (Bool _) BooleanType = True
    matchesType (String _) StringType = True
    matchesType (Number _) NumberType = True
    matchesType (Number n) IntegerType = Sci.isInteger n
    matchesType (Object _) ObjectType = True
    matchesType (Array _) ArrayType = True
    matchesType _ _ = False

-- | The 'type' keyword definition
typeKeyword :: KeywordDefinition
typeKeyword = KeywordDefinition
  { keywordName = "type"
  , keywordCompile = compileType
  , keywordValidate = validateType
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

-- | Backward compatibility: validate type constraint from SchemaObject
validateTypeConstraint :: SchemaObject -> Value -> ValidationResult
validateTypeConstraint obj val = case schemaType obj of
  Nothing -> ValidationSuccess mempty
  Just (One expectedType) -> validateType expectedType val
  Just (Many types) -> validateTypeUnion (NE.toList types) val
  where
    validateType :: SchemaType -> Value -> ValidationResult
    validateType NullType Null = ValidationSuccess mempty
    validateType NullType _ = validationFailure "type" "Expected null"
    validateType BooleanType (Bool _) = ValidationSuccess mempty
    validateType BooleanType _ = validationFailure "type" "Expected boolean"
    validateType StringType (String _) = ValidationSuccess mempty
    validateType StringType _ = validationFailure "type" "Expected string"
    validateType NumberType (Number _) = ValidationSuccess mempty
    validateType NumberType _ = validationFailure "type" "Expected number"
    validateType IntegerType (Number n) =
      if Sci.isInteger n
        then ValidationSuccess mempty
        else validationFailure "type" "Expected integer"
    validateType IntegerType _ = validationFailure "type" "Expected integer"
    validateType ObjectType (Object _) = ValidationSuccess mempty
    validateType ObjectType _ = validationFailure "type" "Expected object"
    validateType ArrayType (Array _) = ValidationSuccess mempty
    validateType ArrayType _ = validationFailure "type" "Expected array"

    validateTypeUnion :: [SchemaType] -> Value -> ValidationResult
    validateTypeUnion types v =
      if any (\t -> isSuccess $ validateType t v) types
        then ValidationSuccess mempty
        else validationFailure "type" $ "Expected one of: " <> T.intercalate ", " (map (T.pack . show) types)

    isSuccess :: ValidationResult -> Bool
    isSuccess (ValidationSuccess _) = True
    isSuccess (ValidationFailure _) = False
