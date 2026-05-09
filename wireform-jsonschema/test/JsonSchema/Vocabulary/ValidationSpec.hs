{-# LANGUAGE PatternSynonyms #-}
module JsonSchema.Vocabulary.ValidationSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Typeable (Typeable)

import JsonSchema.Vocabulary.Types
import JsonSchema.Vocabulary.Registry
import JsonSchema.Vocabulary.Validation
import JsonSchema.Types (Schema(..))
import JsonSchema.Parser (parseSchema)
import JsonSchema.Keyword.Types
import JsonSchema.Types (pattern ValidationSuccess)

-- Test keyword definitions
testCompile :: CompileFunc ()
testCompile _ _ _ = Right ()

testValidate :: ValidateFunc ()
testValidate _ _ _ _ = pure (ValidationSuccess mempty)

mkTestKeyword :: Text -> KeywordDefinition
mkTestKeyword name = KeywordDefinition
  { keywordName = name
  , keywordCompile = testCompile
  , keywordValidate = testValidate
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

spec :: Spec
spec = describe "Vocabulary Validation" $ do

  describe "Required Vocabulary Checking" $ do
    it "passes when all required vocabularies are registered" $ do
      let vocab = Vocabulary
            { vocabularyURI = "https://example.com/vocab/v1"
            , vocabularyRequired = True
            , vocabularyKeywords = Set.fromList ["x-test"]
            }
          registry = registerVocabulary vocab emptyVocabularyRegistry

          schemaJson = Aeson.object
            [ "$vocabulary" Aeson..= Aeson.object
                [ "https://example.com/vocab/v1" Aeson..= Bool True
                ]
            ]

      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right schema -> do
          case validateSchemaVocabularies schema registry of
            Left errs -> expectationFailure $ "Validation failed: " ++ show errs
            Right () -> return ()

    it "fails when required vocabulary is not registered" $ do
      let registry = emptyVocabularyRegistry

          schemaJson = Aeson.object
            [ "$vocabulary" Aeson..= Aeson.object
                [ "https://example.com/unknown/v1" Aeson..= Bool True
                ]
            ]

      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right schema -> do
          case validateSchemaVocabularies schema registry of
            Left errs -> do
              length errs `shouldBe` 1
              case head errs of
                UnknownRequiredVocabulary uri -> uri `shouldBe` "https://example.com/unknown/v1"
                _ -> expectationFailure "Wrong error type"
            Right () -> expectationFailure "Should have failed with unknown vocabulary"

    it "passes when optional vocabulary is not registered" $ do
      let registry = emptyVocabularyRegistry

          schemaJson = Aeson.object
            [ "$vocabulary" Aeson..= Aeson.object
                [ "https://example.com/unknown/v1" Aeson..= Bool False
                ]
            ]

      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right schema -> do
          case validateSchemaVocabularies schema registry of
            Left errs -> expectationFailure $ "Should have passed: " ++ show errs
            Right () -> return ()

    it "passes when schema has no $vocabulary" $ do
      let registry = emptyVocabularyRegistry
          schemaJson = Aeson.object [ "type" Aeson..= String "string" ]

      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right schema -> do
          case validateSchemaVocabularies schema registry of
            Left errs -> expectationFailure $ "Should have passed: " ++ show errs
            Right () -> return ()

    it "checks multiple required vocabularies" $ do
      let vocab1 = Vocabulary
            { vocabularyURI = "https://example.com/vocab1"
            , vocabularyRequired = True
            , vocabularyKeywords = Set.fromList ["x-test1"]
            }
          vocab2 = Vocabulary
            { vocabularyURI = "https://example.com/vocab2"
            , vocabularyRequired = True
            , vocabularyKeywords = Set.fromList ["x-test2"]
            }
          registry = registerVocabulary vocab2 $ registerVocabulary vocab1 emptyVocabularyRegistry

          schemaJson = Aeson.object
            [ "$vocabulary" Aeson..= Aeson.object
                [ "https://example.com/vocab1" Aeson..= Bool True
                , "https://example.com/vocab2" Aeson..= Bool True
                , "https://example.com/vocab3" Aeson..= Bool True  -- Missing!
                ]
            ]

      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right schema -> do
          case validateSchemaVocabularies schema registry of
            Left errs -> do
              length errs `shouldBe` 1
              case head errs of
                UnknownRequiredVocabulary uri -> uri `shouldBe` "https://example.com/vocab3"
                _ -> expectationFailure "Wrong error type"
            Right () -> expectationFailure "Should have failed"

  describe "Vocabulary Requirement Queries" $ do
    it "gets required vocabularies from schema" $ do
      let schemaJson = Aeson.object
            [ "$vocabulary" Aeson..= Aeson.object
                [ "https://example.com/vocab1" Aeson..= Bool True
                , "https://example.com/vocab2" Aeson..= Bool False
                , "https://example.com/vocab3" Aeson..= Bool True
                ]
            ]

      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right schema -> do
          let required = getRequiredVocabularies schema
          length required `shouldBe` 2
          "https://example.com/vocab1" `elem` required `shouldBe` True
          "https://example.com/vocab3" `elem` required `shouldBe` True

    it "gets optional vocabularies from schema" $ do
      let schemaJson = Aeson.object
            [ "$vocabulary" Aeson..= Aeson.object
                [ "https://example.com/vocab1" Aeson..= Bool True
                , "https://example.com/vocab2" Aeson..= Bool False
                , "https://example.com/vocab3" Aeson..= Bool True
                ]
            ]

      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right schema -> do
          let optional = getOptionalVocabularies schema
          length optional `shouldBe` 1
          head optional `shouldBe` "https://example.com/vocab2"

    it "checks if specific vocabulary is required" $ do
      let schemaJson = Aeson.object
            [ "$vocabulary" Aeson..= Aeson.object
                [ "https://example.com/vocab1" Aeson..= Bool True
                , "https://example.com/vocab2" Aeson..= Bool False
                ]
            ]

      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right schema -> do
          isVocabularyRequired "https://example.com/vocab1" schema `shouldBe` True
          isVocabularyRequired "https://example.com/vocab2" schema `shouldBe` False
          isVocabularyRequired "https://example.com/vocab3" schema `shouldBe` False

    it "returns empty lists when schema has no $vocabulary" $ do
      let schemaJson = Aeson.object [ "type" Aeson..= String "string" ]

      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right schema -> do
          getRequiredVocabularies schema `shouldBe` []
          getOptionalVocabularies schema `shouldBe` []
          isVocabularyRequired "https://example.com/any" schema `shouldBe` False
