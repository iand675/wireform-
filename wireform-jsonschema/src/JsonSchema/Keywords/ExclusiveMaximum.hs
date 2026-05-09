{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'exclusiveMaximum' keyword (Draft-06+ standalone numeric)
--
-- The exclusiveMaximum keyword requires that a numeric value is strictly
-- less than the specified maximum value.
module JsonSchema.Keywords.ExclusiveMaximum
  ( exclusiveMaximumKeyword
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Scientific as Sci

import JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordNavigation(..), CompileFunc, ValidateFunc)
import JsonSchema.Types (Schema, validationFailure, ValidationAnnotations(..), ValidationResult, pattern ValidationSuccess)

-- | Compiled data for the 'exclusiveMaximum' keyword (Draft-06+ standalone numeric)
newtype ExclusiveMaximumData = ExclusiveMaximumData Sci.Scientific
  deriving (Show, Eq, Typeable)

-- | Compile function for 'exclusiveMaximum' keyword
compileExclusiveMaximum :: CompileFunc ExclusiveMaximumData
compileExclusiveMaximum value _schema _ctx = case value of
  Number n -> Right $ ExclusiveMaximumData n
  _ -> Left "exclusiveMaximum must be a number"

-- | Validate function for 'exclusiveMaximum' keyword
validateExclusiveMaximum :: ValidateFunc ExclusiveMaximumData
validateExclusiveMaximum _recursiveValidator (ExclusiveMaximumData maxVal) _ctx (Number n) =
  if n < maxVal
    then pure (ValidationSuccess mempty)
    else pure (validationFailure "exclusiveMaximum"
                ("Value " <> T.pack (show n) <> " must be less than exclusiveMaximum " <> T.pack (show maxVal)))
validateExclusiveMaximum _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to numbers

-- | The 'exclusiveMaximum' keyword definition (Draft-06+ style)
exclusiveMaximumKeyword :: KeywordDefinition
exclusiveMaximumKeyword = KeywordDefinition
  { keywordName = "exclusiveMaximum"
  , keywordCompile = compileExclusiveMaximum
  , keywordValidate = validateExclusiveMaximum
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

