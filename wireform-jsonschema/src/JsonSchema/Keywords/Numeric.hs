{-# LANGUAGE OverloadedStrings #-}
-- | Numeric keyword validation
--
-- Validates numeric constraint keywords: multipleOf, minimum, maximum,
-- exclusiveMinimum, and exclusiveMaximum. Handles both draft-04 style
-- (boolean exclusive) and draft-06+ style (numeric exclusive).
module JsonSchema.Keywords.Numeric
  ( validateNumericConstraints
  ) where

import JsonSchema.Types
import JsonSchema.Keyword.Types (combineValidationResults)
import Data.Aeson (Value(..))
import qualified Data.Text as T
import qualified Data.Scientific as Sci

-- | Validate numeric constraints
--
-- Validates multipleOf, minimum, maximum, and their exclusive variants.
-- Returns success for non-numeric values (other validators handle type checking).
validateNumericConstraints :: SchemaObject -> Value -> ValidationResult
validateNumericConstraints obj (Number n) =
  let validation = schemaValidation obj
      results =
        [ maybe (ValidationSuccess mempty) (checkMultipleOf n) (validationMultipleOf validation)
        , checkMaximumWithExclusive n validation
        , checkMinimumWithExclusive n validation
        ]
  in combineValidationResults results
  where
    checkMultipleOf :: Sci.Scientific -> Sci.Scientific -> ValidationResult
    checkMultipleOf num divisor =
      -- Special case: if the number is an integer and divisor <= 1, any integer is a multiple
      -- This handles overflow cases like 1e308 / 0.5
      let numDouble = Sci.toRealFloat num :: Double
          divisorDouble = Sci.toRealFloat divisor :: Double
          isIntegerValue = numDouble == fromIntegral (round numDouble :: Integer)
      in if isIntegerValue && divisorDouble > 0 && divisorDouble <= 1
        then
          -- For integers with divisor <= 1, check if 1/divisor is an integer
          -- e.g., 0.5 -> 2, 0.25 -> 4, etc.
          let reciprocal = 1 / divisorDouble
              isValidDivisor = reciprocal == fromIntegral (round reciprocal :: Integer)
          in if isValidDivisor
             then ValidationSuccess mempty
             else
               -- Fall back to standard check
               let remainder = numDouble - (fromIntegral (floor (numDouble / divisorDouble) :: Integer) * divisorDouble)
                   epsilon = 1e-10
               in if abs remainder < epsilon || abs (remainder - divisorDouble) < epsilon
                  then ValidationSuccess mempty
                  else validationFailure "multipleOf" $ "Value is not a multiple of " <> T.pack (show divisor)
        else
          -- Standard check for non-integer or divisor > 1
          let remainder = numDouble - (fromIntegral (floor (numDouble / divisorDouble) :: Integer) * divisorDouble)
              epsilon = 1e-10
          in if abs remainder < epsilon || abs (remainder - divisorDouble) < epsilon
            then ValidationSuccess mempty
            else validationFailure "multipleOf" $ "Value is not a multiple of " <> T.pack (show divisor)

    -- Check maximum with exclusiveMaximum handling (draft-04 vs draft-06+)
    checkMaximumWithExclusive :: Sci.Scientific -> SchemaValidation -> ValidationResult
    checkMaximumWithExclusive num validation' =
      case (validationMaximum validation', validationExclusiveMaximum validation') of
        (Just max', Just (Left True)) ->
          -- Draft-04: exclusiveMaximum is boolean, modifies maximum behavior
          if num < max'
            then ValidationSuccess mempty
            else validationFailure "exclusiveMaximum" $ "Value " <> T.pack (show num) <> " must be less than " <> T.pack (show max')
        (Just max', _) ->
          -- No exclusive or exclusiveMaximum = False
          if num <= max'
            then ValidationSuccess mempty
            else validationFailure "maximum" $ "Value " <> T.pack (show num) <> " exceeds maximum " <> T.pack (show max')
        (Nothing, Just (Right exclusiveMax)) ->
          -- Draft-06+: exclusiveMaximum is numeric, standalone
          if num < exclusiveMax
            then ValidationSuccess mempty
            else validationFailure "exclusiveMaximum" $ "Value " <> T.pack (show num) <> " must be less than " <> T.pack (show exclusiveMax)
        _ -> ValidationSuccess mempty

    -- Check minimum with exclusiveMinimum handling (draft-04 vs draft-06+)
    checkMinimumWithExclusive :: Sci.Scientific -> SchemaValidation -> ValidationResult
    checkMinimumWithExclusive num validation' =
      case (validationMinimum validation', validationExclusiveMinimum validation') of
        (Just min', Just (Left True)) ->
          -- Draft-04: exclusiveMinimum is boolean, modifies minimum behavior
          if num > min'
            then ValidationSuccess mempty
            else validationFailure "exclusiveMinimum" $ "Value " <> T.pack (show num) <> " must be greater than " <> T.pack (show min')
        (Just min', _) ->
          -- No exclusive or exclusiveMinimum = False
          if num >= min'
            then ValidationSuccess mempty
            else validationFailure "minimum" $ "Value " <> T.pack (show num) <> " is less than minimum " <> T.pack (show min')
        (Nothing, Just (Right exclusiveMin)) ->
          -- Draft-06+: exclusiveMinimum is numeric, standalone
          if num > exclusiveMin
            then ValidationSuccess mempty
            else validationFailure "exclusiveMinimum" $ "Value " <> T.pack (show num) <> " must be greater than " <> T.pack (show exclusiveMin)
        _ -> ValidationSuccess mempty

validateNumericConstraints _ _ = ValidationSuccess mempty
