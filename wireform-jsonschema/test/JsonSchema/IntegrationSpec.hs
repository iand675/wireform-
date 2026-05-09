module JsonSchema.IntegrationSpec (spec) where

import Test.Hspec
import JsonSchema
import Data.Aeson (Value(..), object, (.=), toJSON)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

-- | These tests exercise the runtime validator end-to-end via
-- programmatically constructed schemas (the schemas a user might build
-- when shipping `HasJsonSchema` instances by hand). They use the
-- helper builders 'objectSchema', 'defaultObjectSchemaData' and
-- 'defaultSchemaValidation' so the noise of clearing every Maybe
-- keyword stays out of the spec.
spec :: Spec
spec = describe "JSON Schema Integration Tests" $ do
  describe "Object validation with constraints" $ do
    it "validates object with required properties" $ do
      let personSchema = (objectSchema
            (defaultObjectSchemaData
              { objectSchemaDataType = Just (One ObjectType)
              , objectSchemaDataValidation = defaultSchemaValidation
                  { validationRequired = Just $ Set.fromList ["name"] }
              , objectSchemaDataAnnotations = defaultSchemaAnnotations
                  { annotationTitle = Just "Person"
                  , annotationDescription = Just "A person object"
                  }
              }))
            { schemaVersion = Just Draft07
            , schemaRawKeywords = Map.fromList
                [ ("type", String "object")
                , ("required", toJSON ["name" :: T.Text])
                ]
            }

          validPerson = object ["name" .= ("Alice" :: T.Text)]
          invalidPerson = object ["age" .= (30 :: Int)]

      isSuccess (validateValue defaultValidationConfig personSchema validPerson)
        `shouldBe` True
      isFailure (validateValue defaultValidationConfig personSchema invalidPerson)
        `shouldBe` True

  describe "Numeric validation" $ do
    it "validates minimum / maximum constraints" $ do
      let ageSchema = (objectSchema
            (defaultObjectSchemaData
              { objectSchemaDataType = Just (One NumberType)
              , objectSchemaDataValidation = defaultSchemaValidation
                  { validationMinimum = Just 0
                  , validationMaximum = Just 150
                  }
              }))
            { schemaVersion = Just Draft07
            , schemaRawKeywords = Map.fromList
                [ ("type",    String "number")
                , ("minimum", Number 0)
                , ("maximum", Number 150)
                ]
            }

      isSuccess (validateValue defaultValidationConfig ageSchema (Number 30))
        `shouldBe` True
      isFailure (validateValue defaultValidationConfig ageSchema (Number (-5)))
        `shouldBe` True
      isFailure (validateValue defaultValidationConfig ageSchema (Number 200))
        `shouldBe` True
