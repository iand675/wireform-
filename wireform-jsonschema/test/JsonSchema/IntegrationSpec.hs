module JsonSchema.IntegrationSpec (spec) where

import Test.Hspec
import JsonSchema
import Data.Aeson (Value(..), object, (.=), toJSON)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = describe "JSON Schema Integration Tests" $ do
  describe "Object validation with constraints" $ do
    it "validates object with required properties" $ do
      -- Create a person schema programmatically
      let personSchema = Schema
            { schemaVersion = Just Draft07
            , schemaMetaschemaURI = Nothing
            , schemaId = Nothing
            , schemaCore = ObjectSchema $ SchemaObject
                { schemaType = Just (One ObjectType)
                , schemaEnum = Nothing
                , schemaConst = Nothing
                , schemaRef = Nothing
                , schemaDynamicRef = Nothing
                , schemaAnchor = Nothing
                , schemaDynamicAnchor = Nothing
                , schemaRecursiveRef = Nothing
                , schemaRecursiveAnchor = Nothing
                , schemaAllOf = Nothing
                , schemaAnyOf = Nothing
                , schemaOneOf = Nothing
                , schemaNot = Nothing
                , schemaIf = Nothing
                , schemaThen = Nothing
                , schemaElse = Nothing
                , schemaValidation = SchemaValidation
                    { validationMultipleOf = Nothing
                    , validationMaximum = Nothing
                    , validationExclusiveMaximum = Nothing
                    , validationMinimum = Nothing
                    , validationExclusiveMinimum = Nothing
                    , validationMaxLength = Nothing
                    , validationMinLength = Nothing
                    , validationPattern = Nothing
                    , validationFormat = Nothing
                    , validationItems = Nothing
                    , validationPrefixItems = Nothing
                    , validationContains = Nothing
                    , validationMaxItems = Nothing
                    , validationMinItems = Nothing
                    , validationUniqueItems = Nothing
                    , validationMaxContains = Nothing
                    , validationMinContains = Nothing
                    , validationProperties = Nothing
                    , validationPatternProperties = Nothing
                    , validationAdditionalProperties = Nothing
                    , validationUnevaluatedProperties = Nothing
                    , validationPropertyNames = Nothing
                    , validationMaxProperties = Nothing
                    , validationMinProperties = Nothing
                    , validationRequired = Just $ Set.fromList ["name"]
                    , validationDependentRequired = Nothing
                    , validationDependentSchemas = Nothing
                    , validationDependencies = Nothing
                    }
                , schemaAnnotations = SchemaAnnotations
                    { annotationTitle = Just "Person"
                    , annotationDescription = Just "A person object"
                    , annotationDefault = Nothing
                    , annotationExamples = []
                    , annotationDeprecated = Nothing
                    , annotationReadOnly = Nothing
                    , annotationWriteOnly = Nothing
                    , annotationComment = Nothing
                    , annotationCodegen = Nothing
                    }
                , schemaDefs = Map.empty
                }
            , schemaVocabulary = Nothing
            , schemaExtensions = Map.empty
            , schemaRawKeywords = Map.fromList
                [ ("type", String "object")
                , ("required", toJSON ["name" :: T.Text])
                ]
            }
      
      let validPerson = object ["name" .= ("Alice" :: T.Text)]
      let invalidPerson = object ["age" .= (30 :: Int)]
      
      -- Valid person should pass
      isSuccess (validateValue defaultValidationConfig personSchema validPerson) `shouldBe` True
      
      -- Invalid person should fail
      isFailure (validateValue defaultValidationConfig personSchema invalidPerson) `shouldBe` True
  
  describe "Numeric validation" $ do
    it "validates minimum constraint" $ do
      let ageSchema = Schema
            { schemaVersion = Just Draft07
            , schemaMetaschemaURI = Nothing
            , schemaId = Nothing
            , schemaCore = ObjectSchema $ SchemaObject
                { schemaType = Just (One NumberType)
                , schemaEnum = Nothing
                , schemaConst = Nothing
                , schemaRef = Nothing
                , schemaDynamicRef = Nothing
                , schemaAnchor = Nothing
                , schemaDynamicAnchor = Nothing
                , schemaRecursiveRef = Nothing
                , schemaRecursiveAnchor = Nothing
                , schemaAllOf = Nothing
                , schemaAnyOf = Nothing
                , schemaOneOf = Nothing
                , schemaNot = Nothing
                , schemaIf = Nothing
                , schemaThen = Nothing
                , schemaElse = Nothing
                , schemaValidation = SchemaValidation
                    { validationMultipleOf = Nothing
                    , validationMaximum = Just 150
                    , validationExclusiveMaximum = Nothing
                    , validationMinimum = Just 0
                    , validationExclusiveMinimum = Nothing
                    , validationMaxLength = Nothing
                    , validationMinLength = Nothing
                    , validationPattern = Nothing
                    , validationFormat = Nothing
                    , validationItems = Nothing
                    , validationPrefixItems = Nothing
                    , validationContains = Nothing
                    , validationMaxItems = Nothing
                    , validationMinItems = Nothing
                    , validationUniqueItems = Nothing
                    , validationMaxContains = Nothing
                    , validationMinContains = Nothing
                    , validationProperties = Nothing
                    , validationPatternProperties = Nothing
                    , validationAdditionalProperties = Nothing
                    , validationUnevaluatedProperties = Nothing
                    , validationPropertyNames = Nothing
                    , validationMaxProperties = Nothing
                    , validationMinProperties = Nothing
                    , validationRequired = Nothing
                    , validationDependentRequired = Nothing
                    , validationDependentSchemas = Nothing
                    , validationDependencies = Nothing
                    }
                , schemaAnnotations = SchemaAnnotations
                    { annotationTitle = Nothing
                    , annotationDescription = Nothing
                    , annotationDefault = Nothing
                    , annotationExamples = []
                    , annotationDeprecated = Nothing
                    , annotationReadOnly = Nothing
                    , annotationWriteOnly = Nothing
                    , annotationComment = Nothing
                    , annotationCodegen = Nothing
                    }
                , schemaDefs = Map.empty
                }
            , schemaVocabulary = Nothing
            , schemaExtensions = Map.empty
            , schemaRawKeywords = Map.fromList
                [ ("type", String "number")
                , ("minimum", Number 0)
                , ("maximum", Number 150)
                ]
            }
      
      -- Valid age
      isSuccess (validateValue defaultValidationConfig ageSchema (Number 30)) `shouldBe` True
      
      -- Too young
      isFailure (validateValue defaultValidationConfig ageSchema (Number (-5))) `shouldBe` True
      
      -- Too old
      isFailure (validateValue defaultValidationConfig ageSchema (Number 200)) `shouldBe` True

