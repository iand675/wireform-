{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the legacy 'dependencies' keyword (Draft-04 through Draft-07)
module JsonSchema.Keywords.Dependencies
  ( dependenciesKeyword
  , compileDependencies
  , DependenciesData(..)
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
import Data.Foldable (toList)

import JsonSchema.Types
  ( Schema(..), SchemaCore(..), SchemaObject(..), Dependency(..)
  , ValidationResult, pattern ValidationSuccess, validationFailure
  , schemaValidation, validationDependencies, schemaRawKeywords
  , JsonSchemaVersion(..)
  )
import JsonSchema.Keyword.Types
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..)
  , CompilationContext(..), contextParseSubschema
  , combineValidationResults
  )
import JsonSchema.Keywords.Common (extractPropertyNames)
import JsonSchema.Parser.Internal (parseSchemaValue)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))

-- | Compiled data for dependencies keyword
newtype DependenciesData = DependenciesData (Map Text Dependency)
  deriving (Typeable)

-- | Compile the dependencies keyword
-- Parses on-demand from raw keywords if not pre-parsed
compileDependencies :: CompileFunc DependenciesData
compileDependencies value schema ctx =
  case schemaCore schema of
    BooleanSchema _ -> Left "dependencies cannot appear in boolean schemas"
    ObjectSchema obj ->
      case validationDependencies (schemaValidation obj) of
        Just deps -> Right $ DependenciesData deps  -- Already parsed
        Nothing -> do
          -- Parse on-demand from raw keywords
          let depsVal = Map.lookup "dependencies" (schemaRawKeywords schema) <|> Just value
          case depsVal of
            Just (Object depsObj) -> do
              let version = fromMaybe Draft07 (schemaVersion schema)
              deps <- Map.fromList <$> mapM (parseDependencyEntry version ctx) (KeyMap.toList depsObj)
              Right $ DependenciesData deps
            _ -> Left "dependencies keyword missing parsed data"
  where
    parseDependencyEntry :: JsonSchemaVersion -> CompilationContext -> (Key.Key, Value) -> Either Text (Text, Dependency)
    parseDependencyEntry version ctx (k, v) = do
      let propName = Key.toText k
      case v of
        Array arr -> do
          -- Array of required property names
          let required = Set.fromList $ do
                String t <- toList arr
                pure t
          Right (propName, DependencyProperties required)
        Bool b -> do
          -- Boolean schema dependency (true/false)
          let version = fromMaybe Draft07 (schemaVersion schema)
          schema' <- case parseSchemaValue version v of
            Left err -> Left $ "Invalid boolean schema for dependency '" <> propName <> "': " <> T.pack (show err)
            Right s -> Right s
          Right (propName, DependencySchema schema')
        Object _ -> do
          -- Schema dependency
          schema' <- case contextParseSubschema ctx v of
            Left err -> Left $ "Invalid schema for dependency '" <> propName <> "': " <> err
            Right s -> Right s
          Right (propName, DependencySchema schema')
        _ -> Left $ "Invalid dependency value for '" <> propName <> "': must be array, object, or boolean"
    
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) = \x y -> case x of
      Just _ -> x
      Nothing -> y

-- | Validate dependencies: property and schema dependencies (Draft-04/06/07)
validateDependenciesKeyword :: ValidateFunc DependenciesData
validateDependenciesKeyword recursiveValidator (DependenciesData deps) _ctx (Object objMap) =
  let presentProps = extractPropertyNames objMap
      results = map (validateDependency presentProps) (Map.toList deps)
  in pure $ combineValidationResults results
  where
    validateDependency present (propName, dep) =
      case KeyMap.lookup (Key.fromText propName) objMap of
        Nothing -> ValidationSuccess mempty
        Just _ ->
          case dep of
            DependencyProperties required ->
              let missing = Set.difference required present
              in if Set.null missing
                   then ValidationSuccess mempty
                   else validationFailure "dependencies" $
                        "Property '" <> propName <> "' requires these properties: " <>
                          T.intercalate ", " (Set.toList missing)
            DependencySchema depSchema ->
              recursiveValidator depSchema (Object objMap)
validateDependenciesKeyword _ _ _ _ = pure (ValidationSuccess mempty)

-- | Keyword definition for dependencies
dependenciesKeyword :: KeywordDefinition
dependenciesKeyword = KeywordDefinition
  { keywordName = "dependencies"
  , keywordCompile = compileDependencies
  , keywordValidate = validateDependenciesKeyword
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

