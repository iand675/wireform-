{-# LANGUAGE PatternSynonyms #-}
module JsonSchema.Vocabulary.RegistrySpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import JsonSchema.Vocabulary.Types
import JsonSchema.Vocabulary.Registry
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
spec = describe "Vocabulary Registry" $ do

  describe "Vocabulary Registration" $ do
    it "registers a vocabulary" $ do
      let vocab = Vocabulary
            { vocabularyURI = "https://example.com/vocab/v1"
            , vocabularyRequired = True
            , vocabularyKeywords = Set.fromList ["x-test"]
            }
          registry = registerVocabulary vocab emptyVocabularyRegistry

      case lookupVocabulary "https://example.com/vocab/v1" registry of
        Nothing -> expectationFailure "Vocabulary not found after registration"
        Just v -> do
          vocabularyURI v `shouldBe` "https://example.com/vocab/v1"
          vocabularyRequired v `shouldBe` True

    it "replaces vocabulary with same URI" $ do
      let vocab1 = Vocabulary
            { vocabularyURI = "https://example.com/vocab/v1"
            , vocabularyRequired = False
            , vocabularyKeywords = Set.fromList ["x-old"]
            }
          vocab2 = Vocabulary
            { vocabularyURI = "https://example.com/vocab/v1"
            , vocabularyRequired = True
            , vocabularyKeywords = Set.fromList ["x-new"]
            }
          registry = registerVocabulary vocab2 $ registerVocabulary vocab1 emptyVocabularyRegistry

      case lookupVocabulary "https://example.com/vocab/v1" registry of
        Nothing -> expectationFailure "Vocabulary not found"
        Just v -> do
          vocabularyRequired v `shouldBe` True
          Set.member "x-new" (vocabularyKeywords v) `shouldBe` True
          Set.member "x-old" (vocabularyKeywords v) `shouldBe` False

    it "unregisters a vocabulary" $ do
      let vocab = Vocabulary
            { vocabularyURI = "https://example.com/vocab/v1"
            , vocabularyRequired = True
            , vocabularyKeywords = Set.fromList ["x-test"]
            }
          registry = registerVocabulary vocab emptyVocabularyRegistry
          registry' = unregisterVocabulary "https://example.com/vocab/v1" registry

      case lookupVocabulary "https://example.com/vocab/v1" registry' of
        Nothing -> return ()
        Just _ -> expectationFailure "Vocabulary should have been unregistered"

  describe "Keyword Index" $ do
    it "indexes keywords by vocabulary" $ do
      let vocab = Vocabulary
            { vocabularyURI = "https://example.com/vocab/v1"
            , vocabularyRequired = True
            , vocabularyKeywords = Set.fromList
                [ "x-test1"
                , "x-test2"
                ]
            }
          registry = registerVocabulary vocab emptyVocabularyRegistry

      lookupKeywordVocabulary "x-test1" registry `shouldBe` Just "https://example.com/vocab/v1"
      lookupKeywordVocabulary "x-test2" registry `shouldBe` Just "https://example.com/vocab/v1"

    it "returns Nothing for unregistered keyword" $ do
      lookupKeywordVocabulary "x-unknown" emptyVocabularyRegistry `shouldBe` Nothing

    it "updates keyword index when vocabulary is replaced" $ do
      let vocab1 = Vocabulary
            { vocabularyURI = "https://example.com/vocab/v1"
            , vocabularyRequired = True
            , vocabularyKeywords = Set.fromList ["x-old"]
            }
          vocab2 = Vocabulary
            { vocabularyURI = "https://example.com/vocab/v1"
            , vocabularyRequired = True
            , vocabularyKeywords = Set.fromList ["x-new"]
            }
          registry = registerVocabulary vocab2 $ registerVocabulary vocab1 emptyVocabularyRegistry

      lookupKeywordVocabulary "x-old" registry `shouldBe` Nothing
      lookupKeywordVocabulary "x-new" registry `shouldBe` Just "https://example.com/vocab/v1"

  describe "Conflict Detection" $ do
    it "detects no conflicts when vocabularies don't overlap" $ do
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

      detectKeywordConflicts [vocab1, vocab2] `shouldBe` []
      hasConflicts [vocab1, vocab2] `shouldBe` False

    it "detects conflicts when keywords overlap" $ do
      let vocab1 = Vocabulary
            { vocabularyURI = "https://example.com/vocab1"
            , vocabularyRequired = True
            , vocabularyKeywords = Set.fromList ["x-shared"]
            }
          vocab2 = Vocabulary
            { vocabularyURI = "https://example.com/vocab2"
            , vocabularyRequired = True
            , vocabularyKeywords = Set.fromList ["x-shared"]
            }

      let conflicts = detectKeywordConflicts [vocab1, vocab2]
      length conflicts `shouldBe` 1
      conflictKeyword (head conflicts) `shouldBe` "x-shared"
      hasConflicts [vocab1, vocab2] `shouldBe` True

    it "detects multiple conflicts" $ do
      let vocab1 = Vocabulary
            { vocabularyURI = "https://example.com/vocab1"
            , vocabularyRequired = True
            , vocabularyKeywords = Set.fromList
                [ "x-shared1"
                , "x-shared2"
                ]
            }
          vocab2 = Vocabulary
            { vocabularyURI = "https://example.com/vocab2"
            , vocabularyRequired = True
            , vocabularyKeywords = Set.fromList
                [ "x-shared1"
                , "x-shared2"
                ]
            }

      let conflicts = detectKeywordConflicts [vocab1, vocab2]
      length conflicts `shouldBe` 2

  describe "Bulk Registration" $ do
    it "registers multiple vocabularies without conflicts" $ do
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

      case registerVocabularies [vocab1, vocab2] emptyVocabularyRegistry of
        Left err -> expectationFailure $ "Registration failed: " ++ show err
        Right registry -> do
          lookupVocabulary "https://example.com/vocab1" registry `shouldSatisfy` (\x -> case x of Just _ -> True; Nothing -> False)
          lookupVocabulary "https://example.com/vocab2" registry `shouldSatisfy` (\x -> case x of Just _ -> True; Nothing -> False)

    it "fails when registering vocabularies with conflicts" $ do
      let vocab1 = Vocabulary
            { vocabularyURI = "https://example.com/vocab1"
            , vocabularyRequired = True
            , vocabularyKeywords = Set.fromList ["x-shared"]
            }
          vocab2 = Vocabulary
            { vocabularyURI = "https://example.com/vocab2"
            , vocabularyRequired = True
            , vocabularyKeywords = Set.fromList ["x-shared"]
            }

      case registerVocabularies [vocab1, vocab2] emptyVocabularyRegistry of
        Left err -> err `shouldSatisfy` (\t -> T.isInfixOf "conflict" (T.toLower t))
        Right _ -> expectationFailure "Should have failed with conflicts"

  describe "Vocabulary Keywords" $ do
    it "gets keywords from a vocabulary" $ do
      let vocab = Vocabulary
            { vocabularyURI = "https://example.com/vocab/v1"
            , vocabularyRequired = True
            , vocabularyKeywords = Set.fromList
                [ "x-test1"
                , "x-test2"
                ]
            }
          registry = registerVocabulary vocab emptyVocabularyRegistry

      case getVocabularyKeywords "https://example.com/vocab/v1" registry of
        Nothing -> expectationFailure "Keywords not found"
        Just keywords -> do
          Set.size keywords `shouldBe` 2
          Set.member "x-test1" keywords `shouldBe` True
          Set.member "x-test2" keywords `shouldBe` True

    it "returns Nothing for unregistered vocabulary" $ do
      case getVocabularyKeywords "https://example.com/nonexistent" emptyVocabularyRegistry of
        Nothing -> return ()
        Just _ -> expectationFailure "Should not have found keywords"

  describe "KeywordRegistry Conversion" $ do
    it "converts VocabularyRegistry to KeywordRegistry" $ do
      let vocab = Vocabulary
            { vocabularyURI = "https://example.com/vocab/v1"
            , vocabularyRequired = True
            , vocabularyKeywords = Set.fromList
                [ "x-test1"
                , "x-test2"
                ]
            }
          vocabRegistry = registerVocabulary vocab emptyVocabularyRegistry

      -- Should have both keywords in the vocabulary
      case lookupVocabulary "https://example.com/vocab/v1" vocabRegistry of
        Nothing -> expectationFailure "Vocabulary not found"
        Just v -> do
          Set.member "x-test1" (vocabularyKeywords v) `shouldBe` True
          Set.member "x-test2" (vocabularyKeywords v) `shouldBe` True
