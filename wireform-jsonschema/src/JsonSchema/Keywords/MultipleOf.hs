{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'multipleOf' keyword
--
-- The multipleOf keyword requires that a numeric value is a multiple
-- of the specified divisor.
module JsonSchema.Keywords.MultipleOf
  ( multipleOfKeyword
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Scientific as Sci

import JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordNavigation(..), CompileFunc, ValidateFunc)
import JsonSchema.Types (Schema, validationFailure, ValidationResult, ValidationAnnotations(..), pattern ValidationSuccess)

-- | Compiled data for the 'multipleOf' keyword
newtype MultipleOfData = MultipleOfData Sci.Scientific
  deriving (Show, Eq, Typeable)

-- | Compile function for 'multipleOf' keyword
compileMultipleOf :: CompileFunc MultipleOfData
compileMultipleOf value _schema _ctx = case value of
  Number n | n > 0 -> Right $ MultipleOfData n
  _ -> Left "multipleOf must be a number greater than 0"

-- | Validate function for 'multipleOf' keyword
--
-- Handles float overflow cases: when checking if a large integer is a multiple
-- of a small divisor (e.g., 1e308 / 0.5), we avoid division overflow by checking
-- if the divisor's reciprocal is an integer. If so, any integer is a multiple.
validateMultipleOf :: ValidateFunc MultipleOfData
validateMultipleOf _recursiveValidator (MultipleOfData divisor) _ctx (Number n) =
  let numDouble = Sci.toRealFloat n :: Double
      divisorDouble = Sci.toRealFloat divisor :: Double

      successResult = ValidationSuccess mempty
      failureResult msg = validationFailure "multipleOf" msg

      -- Standard check: divide and check remainder
      checkStandard :: ValidationResult
      checkStandard =
        let quotient = numDouble / divisorDouble
            remainder = quotient - fromIntegral (round quotient :: Integer)
            epsilon = 1e-10
        in if abs remainder < epsilon || abs (1 - remainder) < epsilon
             then successResult
             else failureResult ("Value is not a multiple of " <> T.pack (show divisor))

      -- Check if the number is an integer (represented as Scientific but actually integer)
      isIntegerValue = numDouble == fromIntegral (round numDouble :: Integer)

      -- Special case: if number is integer and divisor <= 1, check if 1/divisor is integer
      -- This handles overflow cases like 1e308 / 0.5 without actually dividing
      -- e.g., 0.5 -> 1/0.5 = 2 (integer), so any integer is a multiple of 0.5
      -- e.g., 0.25 -> 1/0.25 = 4 (integer), so any integer is a multiple of 0.25
      result =
        if isIntegerValue && divisorDouble > 0 && divisorDouble <= 1
          then
            let reciprocal = 1 / divisorDouble
                isValidDivisor = reciprocal == fromIntegral (round reciprocal :: Integer)
            in if isValidDivisor
                 then successResult  -- Any integer is a multiple
                 else checkStandard  -- Fall back to standard check
          else checkStandard
  in pure result
validateMultipleOf _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to numbers

-- | The 'multipleOf' keyword definition
multipleOfKeyword :: KeywordDefinition
multipleOfKeyword = KeywordDefinition
  { keywordName = "multipleOf"
  , keywordCompile = compileMultipleOf
  , keywordValidate = validateMultipleOf
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

