{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'required' keyword
--
-- The required keyword specifies an array of property names that must
-- be present in an object instance.
module JsonSchema.Keywords.Required
  ( requiredKeyword
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Foldable (toList)
import qualified Data.Set as Set
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key

import JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordNavigation(..), CompileFunc, ValidateFunc)
import JsonSchema.Types (Schema, validationFailure, ValidationAnnotations(..), ValidationResult, pattern ValidationSuccess)
import JsonSchema.Keywords.Common (extractPropertyNames)

-- | Compiled data for the 'required' keyword
newtype RequiredData = RequiredData (Set.Set Text)
  deriving (Show, Eq, Typeable)

-- | Compile function for 'required' keyword
compileRequired :: CompileFunc RequiredData
compileRequired value _schema _ctx = case value of
  Array arr -> do
    let items = toList arr
    -- All items must be strings
    strings <- mapM extractString items
    Right $ RequiredData (Set.fromList strings)
  _ -> Left "required must be an array of strings"
  where
    extractString (String s) = Right s
    extractString _ = Left "required array must contain only strings"

-- | Validate function for 'required' keyword
validateRequired :: ValidateFunc RequiredData
validateRequired _recursiveValidator (RequiredData requiredProps) _ctx (Object objMap) =
  let presentProps = extractPropertyNames objMap
      missingProps = Set.difference requiredProps presentProps
  in if Set.null missingProps
     then pure (ValidationSuccess mempty)
     else pure (validationFailure "required" $
                "Missing required properties: " <> T.intercalate ", " (Set.toList missingProps))
validateRequired _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to objects

-- | The 'required' keyword definition
requiredKeyword :: KeywordDefinition
requiredKeyword = KeywordDefinition
  { keywordName = "required"
  , keywordCompile = compileRequired
  , keywordValidate = validateRequired
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

