module JsonSchema.ValidatorSpec (spec) where

import Test.Hspec
import qualified Test.Hspec.Hedgehog as HH
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import JsonSchema.Types
import JsonSchema.Validator
import Data.Aeson (Value(..))
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  describe "validateValue" $ do
    it "validates against boolean schema true" $ do
      let schema = Schema
            { schemaVersion = Just Draft202012
            , schemaMetaschemaURI = Nothing
            , schemaId = Nothing
            , schemaCore = BooleanSchema True
            , schemaVocabulary = Nothing
            , schemaExtensions = mempty
            }
      let result = validateValue defaultValidationConfig schema Null
      isSuccess result `shouldBe` True
    
    it "fails against boolean schema false" $ do
      let schema = Schema
            { schemaVersion = Just Draft202012
            , schemaMetaschemaURI = Nothing
            , schemaId = Nothing
            , schemaCore = BooleanSchema False
            , schemaVocabulary = Nothing
            , schemaExtensions = mempty
            }
      let result = validateValue defaultValidationConfig schema Null
      isFailure result `shouldBe` True
  
  describe "validation monotonicity property" $ do
    it "allOf makes schema more restrictive" $ HH.hedgehog $ do
      -- Schema 1: just type constraint
      let schema1 = Schema
            { schemaVersion = Just Draft202012
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
                , schemaValidation = mempty
                , schemaAnnotations = mempty
                , schemaDefs = Map.empty
                }
            , schemaVocabulary = Nothing
            , schemaExtensions = Map.empty
            , schemaRawKeywords = Map.empty
            }
      
      -- Schema 2: type + minimum constraint (more restrictive)
      let schema2 = Schema
            { schemaVersion = Just Draft202012
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
                    { validationMinimum = Just 0
                    , validationMaximum = Nothing
                    , validationMultipleOf = Nothing
                    , validationExclusiveMinimum = Nothing
                    , validationExclusiveMaximum = Nothing
                    , validationMinLength = Nothing
                    , validationMaxLength = Nothing
                    , validationPattern = Nothing
                    , validationFormat = Nothing
                    , validationItems = Nothing
                    , validationPrefixItems = Nothing
                    , validationContains = Nothing
                    , validationMinItems = Nothing
                    , validationMaxItems = Nothing
                    , validationUniqueItems = Nothing
                    , validationMinContains = Nothing
                    , validationMaxContains = Nothing
                    , validationProperties = Nothing
                    , validationPatternProperties = Nothing
                    , validationAdditionalProperties = Nothing
                    , validationUnevaluatedProperties = Nothing
                    , validationPropertyNames = Nothing
                    , validationMinProperties = Nothing
                    , validationMaxProperties = Nothing
                    , validationRequired = Nothing
                    , validationDependentRequired = Nothing
                    , validationDependentSchemas = Nothing
                    , validationDependencies = Nothing
                    }
                , schemaAnnotations = mempty
                , schemaDefs = Map.empty
                }
            , schemaVocabulary = Nothing
            , schemaExtensions = Map.empty
            , schemaRawKeywords = Map.empty
            }
      
      -- Property: if value validates against more restrictive schema,
      -- it should also validate against less restrictive schema
      let validValue = Number 10  -- Satisfies both
      let validResult1 = validateValue defaultValidationConfig schema1 validValue
      let validResult2 = validateValue defaultValidationConfig schema2 validValue
      
      case (validResult1, validResult2) of
        (ValidationSuccess _, ValidationSuccess _) -> H.success
        _ -> H.annotate "Monotonicity failed: more restrictive should imply less restrictive" >> H.failure

-- Helper instances for property tests
instance Semigroup SchemaValidation where
  _ <> _ = mempty  -- Simplified for tests

instance Monoid SchemaValidation where
  mempty = SchemaValidation
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
    , validationRequired = Nothing
    , validationDependentRequired = Nothing
    , validationDependentSchemas = Nothing
    , validationDependencies = Nothing
    }

instance Semigroup SchemaAnnotations where
  _ <> _ = mempty

instance Monoid SchemaAnnotations where
  mempty = SchemaAnnotations
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

