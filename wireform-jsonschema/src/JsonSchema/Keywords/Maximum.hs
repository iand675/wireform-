{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'maximum' keyword
--
-- The maximum keyword requires that a numeric value is less than or
-- equal to the specified maximum value.
module JsonSchema.Keywords.Maximum
  ( maximumKeyword
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Scientific as Sci

import JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordNavigation(..), CompileFunc, ValidateFunc)
import JsonSchema.Types (Schema, validationFailure, ValidationAnnotations(..), ValidationResult, pattern ValidationSuccess)

-- | Compiled data for the 'maximum' keyword
newtype MaximumData = MaximumData Sci.Scientific
  deriving (Show, Eq, Typeable)

-- | Compile function for 'maximum' keyword
compileMaximum :: CompileFunc MaximumData
compileMaximum value _schema _ctx = case value of
  Number n -> Right $ MaximumData n
  _ -> Left "maximum must be a number"

-- | Validate function for 'maximum' keyword
validateMaximum :: ValidateFunc MaximumData
validateMaximum _recursiveValidator (MaximumData maxVal) _ctx (Number n) =
  if n <= maxVal
    then pure (ValidationSuccess mempty)
    else pure (validationFailure "maximum" $
                "Value " <> T.pack (show n) <> " exceeds maximum " <> T.pack (show maxVal))
validateMaximum _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to numbers

-- | The 'maximum' keyword definition
maximumKeyword :: KeywordDefinition
maximumKeyword = KeywordDefinition
  { keywordName = "maximum"
  , keywordCompile = compileMaximum
  , keywordValidate = validateMaximum
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

