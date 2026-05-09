module JsonSchema.VocabularySpec (spec) where

import Test.Hspec
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import JsonSchema.Vocabulary
import JsonSchema.Types
import JsonSchema.Dialect
import JsonSchema (parseSchema, validateValue, defaultValidationConfig, isSuccess)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson

-- Helper function to create validation errors
mkValidationError :: Text -> ValidationError
mkValidationError msg = validationError msg

spec :: Spec
spec = describe "Vocabulary System" $ do
  
  describe "Custom Keyword Validation" $ do
    it "executes custom validators for extension keywords" $ do
      -- Schema with custom keyword
      let schemaJson = Aeson.object
            [ "type" Aeson..= String "string"
            , "x-even-length" Aeson..= Bool True  -- Custom keyword
            ]
      
      case Aeson.fromJSON schemaJson of
        Aeson.Error err -> expectationFailure $ "Parse error: " <> err
        Aeson.Success val -> do
          case parseSchema val of
            Left err -> expectationFailure $ "Schema parse error: " <> show err
            Right schema -> do
              -- Custom validator: string must have even length
              let evenLengthValidator (String s) =
                    if T.length s `mod` 2 == 0
                      then Right ()
                      else Left $ mkValidationError "String must have even length"
                  evenLengthValidator _ = Left $ mkValidationError "Must be a string"
              
              let config = defaultValidationConfig
                    { validationCustomValidators = Map.fromList [("x-even-length", evenLengthValidator)]
                    }
              
              -- Test valid data (even length string)
              let validResult = validateValue config schema (String "ab")
              isSuccess validResult `shouldBe` True
              
              -- Test invalid data (odd length string)
              let invalidResult = validateValue config schema (String "abc")
              isSuccess invalidResult `shouldBe` False
    
    it "treats unregistered custom keywords as annotations only" $ do
      -- Schema with custom keyword but no validator
      let schemaJson = Aeson.object
            [ "type" Aeson..= String "string"
            , "x-custom-annotation" Aeson..= String "some value"
            ]
      
      case Aeson.fromJSON schemaJson of
        Aeson.Error err -> expectationFailure $ "Parse error: " <> err
        Aeson.Success val -> do
          case parseSchema val of
            Left err -> expectationFailure $ "Schema parse error: " <> show err
            Right schema -> do
              let config = defaultValidationConfig
                    { validationCustomValidators = Map.empty  -- No validators
                    }
              
              -- Should succeed (no validator = annotation only)
              let result = validateValue config schema (String "test")
              isSuccess result `shouldBe` True
    
    it "can validate multiple custom keywords" $ do
      let schemaJson = Aeson.object
            [ "type" Aeson..= String "number"
            , "x-positive" Aeson..= Bool True
            , "x-even" Aeson..= Bool True
            ]
      
      case Aeson.fromJSON schemaJson of
        Aeson.Error err -> expectationFailure $ "Parse error: " <> err
        Aeson.Success val -> do
          case parseSchema val of
            Left err -> expectationFailure $ "Schema parse error: " <> show err
            Right schema -> do
              let positiveValidator (Number n) =
                    if n > 0 then Right () else Left $ mkValidationError "Must be positive"
                  positiveValidator _ = Left $ mkValidationError "Must be a number"
                  
                  evenValidator (Number n) =
                    if round n `mod` (2 :: Integer) == 0
                      then Right ()
                      else Left $ mkValidationError "Must be even"
                  evenValidator _ = Left $ mkValidationError "Must be a number"
              
              let config = defaultValidationConfig
                    { validationCustomValidators = Map.fromList
                        [ ("x-positive", positiveValidator)
                        , ("x-even", evenValidator)
                        ]
                    }
              
              -- Valid: positive and even
              isSuccess (validateValue config schema (Number 4)) `shouldBe` True
              
              -- Invalid: negative
              isSuccess (validateValue config schema (Number (-2))) `shouldBe` False
              
              -- Invalid: odd
              isSuccess (validateValue config schema (Number 3)) `shouldBe` False
  
  describe "KeywordValue type safety" $ do
    it "roundtrips values correctly" $ do
      let intValue = KeywordValue (42 :: Int)
      extractKeywordValue intValue `shouldBe` Just (42 :: Int)
    
    it "returns Nothing for wrong type" $ do
      let intValue = KeywordValue (42 :: Int)
      (extractKeywordValue intValue :: Maybe String) `shouldBe` Nothing
    
    it "handles Text values" $ do
      let textValue = KeywordValue ("hello" :: Text)
      extractKeywordValue textValue `shouldBe` Just ("hello" :: Text)
    
    it "handles Bool values" $ do
      let boolValue = KeywordValue True
      extractKeywordValue boolValue `shouldBe` Just True
  
  describe "Vocabulary registration and lookup" $ do
    it "registers and looks up vocabulary" $ do
      let customVocab = Vocabulary
            { vocabularyURI = "https://example.com/custom/v1"
            , vocabularyRequired = False
            , vocabularyKeywords = Set.fromList
                [ "x-test"
                ]
            }
          registry = registerVocabulary customVocab emptyVocabularyRegistry
      
      case lookupVocabulary "https://example.com/custom/v1" registry of
        Nothing -> expectationFailure "Vocabulary not found"
        Just vocab -> vocabularyURI vocab `shouldBe` "https://example.com/custom/v1"
    
    it "returns Nothing for unregistered vocabulary" $ do
      lookupVocabulary "https://example.com/nonexistent" emptyVocabularyRegistry
        `shouldBe` Nothing
  
  describe "Dialect registration and lookup" $ do
    it "registers and looks up dialect" $ do
      let registry = registerDialect draft07Dialect emptyVocabularyRegistry
          uri = dialectURI draft07Dialect
      
      case lookupDialect uri registry of
        Nothing -> expectationFailure "Dialect not found"
        Just dialect -> dialectName dialect `shouldBe` "JSON Schema Draft-07"
    
    it "returns Nothing for unregistered dialect" $ do
      lookupDialect "https://example.com/nonexistent" emptyVocabularyRegistry
        `shouldBe` Nothing
  
  describe "Keyword conflict detection" $ do
    it "detects no conflicts when vocabularies don't overlap" $ do
      let vocab1 = Vocabulary
            { vocabularyURI = "https://example.com/vocab1"
            , vocabularyRequired = False
            , vocabularyKeywords = Set.fromList
                [ "x-keyword1"
                ]
            }
          vocab2 = Vocabulary
            { vocabularyURI = "https://example.com/vocab2"
            , vocabularyRequired = False
            , vocabularyKeywords = Set.fromList
                [ "x-keyword2"
                ]
            }
      detectKeywordConflicts [vocab1, vocab2] `shouldBe` []
    
    it "detects conflicts when keywords overlap" $ do
      let vocab1 = Vocabulary
            { vocabularyURI = "https://example.com/vocab1"
            , vocabularyRequired = False
            , vocabularyKeywords = Set.fromList
                [ "type"
                ]
            }
          vocab2 = Vocabulary
            { vocabularyURI = "https://example.com/vocab2"
            , vocabularyRequired = False
            , vocabularyKeywords = Set.fromList
                [ "type"
                ]
            }
      detectKeywordConflicts [vocab1, vocab2] `shouldBe` ["type"]
  
  describe "Dialect composition" $ do
    it "composes dialect from registered vocabularies" $ do
      let registry = standardRegistry
      
      case composeDialect
            "Test Dialect"
            Draft202012
            [ ("https://json-schema.org/draft/2020-12/vocab/core", True)
            , ("https://json-schema.org/draft/2020-12/vocab/validation", True)
            ]
            FormatAnnotation
            CollectUnknown
            registry of
        Left err -> expectationFailure $ "Failed to compose: " ++ T.unpack err
        Right dialect -> do
          dialectName dialect `shouldBe` "Test Dialect"
          dialectVersion dialect `shouldBe` Draft202012
          Map.size (dialectVocabularies dialect) `shouldBe` 2
    
    it "fails when required vocabulary not found" $ do
      let registry = emptyVocabularyRegistry
      
      case composeDialect
            "Test Dialect"
            Draft202012
            [ ("https://example.com/nonexistent", True)
            ]
            FormatAnnotation
            CollectUnknown
            registry of
        Left err -> err `shouldSatisfy` T.isInfixOf "not found"
        Right _ -> expectationFailure "Should have failed with missing vocabulary"
    
    it "detects conflicts during composition" $ do
      let conflictingVocab = Vocabulary
            { vocabularyURI = "https://example.com/conflict"
            , vocabularyRequired = False
            , vocabularyKeywords = Set.fromList
                [ "type"
                ]
            }
          registry = registerVocabulary conflictingVocab 
                   $ registerVocabulary validationVocabulary emptyVocabularyRegistry
      
      case composeDialect
            "Conflicting Dialect"
            Draft202012
            [ ("https://json-schema.org/draft/2020-12/vocab/validation", True)
            , ("https://example.com/conflict", False)
            ]
            FormatAnnotation
            CollectUnknown
            registry of
        Left err -> err `shouldSatisfy` T.isInfixOf "conflict"
        Right _ -> expectationFailure "Should have detected keyword conflict"
  
  describe "Standard vocabularies" $ do
    it "includes core vocabulary in standard registry" $ do
      case lookupVocabulary "https://json-schema.org/draft/2020-12/vocab/core" standardRegistry of
        Nothing -> expectationFailure "Core vocabulary not found"
        Just vocab -> vocabularyRequired vocab `shouldBe` True
    
    it "includes validation vocabulary in standard registry" $ do
      case lookupVocabulary "https://json-schema.org/draft/2020-12/vocab/validation" standardRegistry of
        Nothing -> expectationFailure "Validation vocabulary not found"
        Just vocab -> vocabularyRequired vocab `shouldBe` True
    
    it "includes metadata vocabulary in standard registry" $ do
      case lookupVocabulary "https://json-schema.org/draft/2020-12/vocab/meta-data" standardRegistry of
        Nothing -> expectationFailure "Metadata vocabulary not found"
        Just vocab -> vocabularyRequired vocab `shouldBe` False
  
  describe "Standard dialects" $ do
    it "includes Draft-07 in standard registry" $ do
      let uri = dialectURI draft07Dialect
      case lookupDialect uri standardRegistry of
        Nothing -> expectationFailure "Draft-07 not found"
        Just dialect -> dialectVersion dialect `shouldBe` Draft07
    
    it "includes Draft 2020-12 in standard registry" $ do
      let uri = dialectURI draft202012Dialect
      case lookupDialect uri standardRegistry of
        Nothing -> expectationFailure "Draft 2020-12 not found"
        Just dialect -> dialectVersion dialect `shouldBe` Draft202012

