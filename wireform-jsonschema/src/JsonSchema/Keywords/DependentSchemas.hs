{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'dependentSchemas' keyword (Draft 2019-09+)
--
-- The dependentSchemas keyword specifies schema dependencies:
-- if a property exists in an object, then the entire object must validate against
-- the dependent schema.
module JsonSchema.Keywords.DependentSchemas
  ( dependentSchemasKeyword
  , compileDependentSchemas
  , DependentSchemasData(..)
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
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
  , validationDependentSchemas, schemaValidation
  )
import JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..)
  , combineValidationResults
  )
import JsonSchema.Parser.Internal (parseSchema)

-- | Compiled data for dependentSchemas keyword
-- Maps property names to schemas
newtype DependentSchemasData = DependentSchemasData (Map Text Schema)
  deriving (Typeable)

-- | Compile the dependentSchemas keyword
compileDependentSchemas :: CompileFunc DependentSchemasData
compileDependentSchemas (Object obj) _schema _ctx = do
  -- Each property maps to a schema
  entries <- mapM parseEntry (KeyMap.toList obj)
  Right $ DependentSchemasData $ Map.fromList entries
  where
    parseEntry (k, v) = case parseSchema v of
      Left err -> Left $ "Invalid schema for dependentSchemas property '" <> Key.toText k <> "': " <> T.pack (show err)
      Right schema -> Right (Key.toText k, schema)

compileDependentSchemas _ _ _ = Left "dependentSchemas must be an object"

-- | Validate dependentSchemas using the pluggable keyword system
validateDependentSchemasKeyword :: ValidateFunc DependentSchemasData
validateDependentSchemasKeyword recursiveValidator (DependentSchemasData depSchemaMap) _ctx val@(Object objMap) =
  let results =
        [ recursiveValidator depSchema val
        | (propName, depSchema) <- Map.toList depSchemaMap
        , KeyMap.member (Key.fromText propName) objMap  -- Property is present
        ]
  in pure $ combineValidationResults results

validateDependentSchemasKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to objects

-- | Keyword definition for dependentSchemas
dependentSchemasKeyword :: KeywordDefinition
dependentSchemasKeyword = KeywordDefinition
  { keywordName = "dependentSchemas"
  , keywordCompile = compileDependentSchemas
  , keywordValidate = validateDependentSchemasKeyword
  , keywordNavigation = SchemaMap $ \schema -> case schemaCore schema of
      ObjectSchema obj -> validationDependentSchemas (schemaValidation obj)
      _ -> Nothing
  , keywordPostValidate = Nothing
  }

