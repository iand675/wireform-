{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'exclusiveMinimum' keyword (Draft-06+ standalone numeric)
--
-- The exclusiveMinimum keyword requires that a numeric value is strictly
-- greater than the specified minimum value.
module JsonSchema.Keywords.ExclusiveMinimum
  ( exclusiveMinimumKeyword
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Scientific as Sci

import JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordNavigation(..), CompileFunc, ValidateFunc)
import JsonSchema.Types (Schema, validationFailure, ValidationAnnotations(..), ValidationResult, pattern ValidationSuccess)

-- | Compiled data for the 'exclusiveMinimum' keyword (Draft-06+ standalone numeric)
newtype ExclusiveMinimumData = ExclusiveMinimumData Sci.Scientific
  deriving (Show, Eq, Typeable)

-- | Compile function for 'exclusiveMinimum' keyword
compileExclusiveMinimum :: CompileFunc ExclusiveMinimumData
compileExclusiveMinimum value _schema _ctx = case value of
  Number n -> Right $ ExclusiveMinimumData n
  _ -> Left "exclusiveMinimum must be a number"

-- | Validate function for 'exclusiveMinimum' keyword
validateExclusiveMinimum :: ValidateFunc ExclusiveMinimumData
validateExclusiveMinimum _recursiveValidator (ExclusiveMinimumData minVal) _ctx (Number n) =
  if n > minVal
    then pure (ValidationSuccess mempty)
    else pure (validationFailure "exclusiveMinimum"
                ("Value " <> T.pack (show n) <> " must be greater than exclusiveMinimum " <> T.pack (show minVal)))
validateExclusiveMinimum _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to numbers

-- | The 'exclusiveMinimum' keyword definition (Draft-06+ style)
exclusiveMinimumKeyword :: KeywordDefinition
exclusiveMinimumKeyword = KeywordDefinition
  { keywordName = "exclusiveMinimum"
  , keywordCompile = compileExclusiveMinimum
  , keywordValidate = validateExclusiveMinimum
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

