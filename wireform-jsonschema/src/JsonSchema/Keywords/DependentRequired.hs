{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'dependentRequired' keyword (Draft 2019-09+)
--
-- The dependentRequired keyword specifies property dependencies:
-- if a property exists in an object, then certain other properties must also exist.
module JsonSchema.Keywords.DependentRequired
  ( dependentRequiredKeyword
  , compileDependentRequired
  , DependentRequiredData(..)
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
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
import Data.Foldable (toList)

import JsonSchema.Types 
  ( Schema, SchemaCore(..), SchemaObject(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , ValidationErrors(..), emptyPointer, ValidationAnnotations(..)
  )
import qualified JsonSchema.Validator.Result as VR
import JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..)
  )
import JsonSchema.Keywords.Common (extractPropertyNames)

-- | Compiled data for dependentRequired keyword
-- Maps property names to sets of required properties
newtype DependentRequiredData = DependentRequiredData (Map Text (Set Text))
  deriving (Typeable)

-- | Compile the dependentRequired keyword
compileDependentRequired :: CompileFunc DependentRequiredData
compileDependentRequired (Object obj) _schema _ctx = do
  -- Each property maps to an array of required property names
  entries <- mapM parseEntry (KeyMap.toList obj)
  Right $ DependentRequiredData $ Map.fromList entries
  where
    parseEntry (k, v) = case v of
      Array arr -> do
        propNames <- mapM extractString (toList arr)
        Right (Key.toText k, Set.fromList propNames)
      _ -> Left $ "dependentRequired value for '" <> Key.toText k <> "' must be an array"
    
    extractString (String s) = Right s
    extractString _ = Left "dependentRequired array must contain only strings"

compileDependentRequired _ _ _ = Left "dependentRequired must be an object"

-- | Validate dependentRequired using the pluggable keyword system
validateDependentRequiredKeyword :: ValidateFunc DependentRequiredData
validateDependentRequiredKeyword _recursiveValidator (DependentRequiredData depReqMap) _ctx (Object objMap) =
  let presentProps = extractPropertyNames objMap
      errors =
        [ VR.ValidationError
            { VR.errorKeyword = "dependentRequired"
            , VR.errorSchemaPath = emptyPointer
            , VR.errorInstancePath = emptyPointer
            , VR.errorMessage = "Property '" <> propName <> "' requires these properties: " <>
                               T.intercalate ", " (Set.toList missingDeps)
            }
        | (propName, requiredDeps) <- Map.toList depReqMap
        , KeyMap.member (Key.fromText propName) objMap  -- Property is present
        , let missingDeps = Set.difference requiredDeps presentProps
        , not (Set.null missingDeps)  -- Has missing dependencies
        ]
  in case errors of
    [] -> pure (ValidationSuccess mempty)
    (e:es) -> pure (ValidationFailure $ ValidationErrors $ e NE.:| es)

validateDependentRequiredKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to objects

-- | Keyword definition for dependentRequired
dependentRequiredKeyword :: KeywordDefinition
dependentRequiredKeyword = KeywordDefinition
  { keywordName = "dependentRequired"
  , keywordCompile = compileDependentRequired
  , keywordValidate = validateDependentRequiredKeyword
  , keywordNavigation = NoNavigation  -- No subschemas
  , keywordPostValidate = Nothing
  }

