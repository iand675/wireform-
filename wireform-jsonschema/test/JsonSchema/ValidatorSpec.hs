module JsonSchema.ValidatorSpec (spec) where

import Test.Hspec
import qualified Test.Hspec.Hedgehog as HH
import qualified Hedgehog as H
import JsonSchema.Types
import JsonSchema.Validator
import Data.Aeson (Value(..))

-- | Smoke-test the public 'validateValue' surface using the helper
-- builders from "JsonSchema.Types". Property-style monotonicity
-- coverage lives in the Hedgehog block below.
spec :: Spec
spec = do
  describe "validateValue" $ do
    it "validates against boolean schema true" $ do
      let schema = (booleanSchema True)
            { schemaVersion = Just Draft202012 }
      isSuccess (validateValue defaultValidationConfig schema Null)
        `shouldBe` True

    it "fails against boolean schema false" $ do
      let schema = (booleanSchema False)
            { schemaVersion = Just Draft202012 }
      isFailure (validateValue defaultValidationConfig schema Null)
        `shouldBe` True

  describe "validation monotonicity property" $ do
    it "minimum constraint preserves validity for in-range values" $ HH.hedgehog $ do
      let schema1 = (objectSchema
            (defaultObjectSchemaData
              { objectSchemaDataType = Just (One NumberType) }))
            { schemaVersion = Just Draft202012 }
          schema2 = (objectSchema
            (defaultObjectSchemaData
              { objectSchemaDataType = Just (One NumberType)
              , objectSchemaDataValidation = defaultSchemaValidation
                  { validationMinimum = Just 0 }
              }))
            { schemaVersion = Just Draft202012 }

          validValue = Number 10
          r1 = validateValue defaultValidationConfig schema1 validValue
          r2 = validateValue defaultValidationConfig schema2 validValue

      case (r1, r2) of
        (ValidationSuccess _, ValidationSuccess _) -> H.success
        _ -> H.annotate
               "monotonicity failed: more restrictive must imply less restrictive"
             >> H.failure
