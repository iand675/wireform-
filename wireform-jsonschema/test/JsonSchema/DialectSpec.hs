{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module JsonSchema.DialectSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort)
import Data.Either (isLeft)

import JsonSchema.Dialect
import JsonSchema.Types (JsonSchemaVersion(..), pattern ValidationSuccess)
import JsonSchema.Vocabulary.Types
import JsonSchema.Vocabulary.Registry
import qualified JsonSchema.Vocabulary as Vocab
import JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordNavigation(..))

spec :: Spec
spec = describe "Dialect" $ do
  
  describe "Predefined Dialects" $ do
    it "draft04Dialect has correct URI" $ do
      dialectURI draft04Dialect `shouldBe` "http://json-schema.org/draft-04/schema#"
    
    it "draft06Dialect has correct URI" $ do
      dialectURI draft06Dialect `shouldBe` "http://json-schema.org/draft-06/schema#"
    
    it "draft07Dialect has correct URI" $ do
      dialectURI draft07Dialect `shouldBe` "http://json-schema.org/draft-07/schema#"
    
    it "draft201909Dialect has correct URI" $ do
      dialectURI draft201909Dialect `shouldBe` "https://json-schema.org/draft/2019-09/schema"
    
    it "draft202012Dialect has correct URI" $ do
      dialectURI draft202012Dialect `shouldBe` "https://json-schema.org/draft/2020-12/schema"
    
    it "draft201909Dialect declares vocabularies" $ do
      dialectVocabularies draft201909Dialect `shouldSatisfy` (not . Map.null)
    
    it "draft202012Dialect declares vocabularies" $ do
      dialectVocabularies draft202012Dialect `shouldSatisfy` (not . Map.null)

  describe "validateDialectURI" $ do
    it "accepts http:// URIs" $ do
      validateDialectURI "http://example.com/schema" `shouldBe` Right ()
    
    it "accepts https:// URIs" $ do
      validateDialectURI "https://example.com/schema" `shouldBe` Right ()
    
    it "accepts file:// URIs" $ do
      validateDialectURI "file:///path/to/dialect.json" `shouldBe` Right ()
    
    it "accepts urn: URIs" $ do
      validateDialectURI "urn:example:dialect:v1" `shouldBe` Right ()
    
    it "accepts custom schemes" $ do
      validateDialectURI "x-custom://example.com/schema" `shouldBe` Right ()
    
    it "rejects empty URIs" $ do
      validateDialectURI "" `shouldSatisfy` isLeft
    
    it "rejects relative URIs" $ do
      validateDialectURI "/relative/path" `shouldSatisfy` isLeft
    
    it "rejects URIs without scheme" $ do
      validateDialectURI "example.com/schema" `shouldSatisfy` isLeft
    
    it "rejects URIs with invalid scheme" $ do
      validateDialectURI "123://invalid" `shouldSatisfy` isLeft

  describe "validateDialect" $ do
    let emptyRegistry = emptyVocabularyRegistry
    
    it "accepts dialect with valid absolute URI" $ do
      let dialect = draft04Dialect
      validateDialect emptyRegistry dialect `shouldBe` Right ()
    
    it "rejects dialect with invalid URI" $ do
      let dialect = draft04Dialect { dialectURI = "not-absolute" }
      validateDialect emptyRegistry dialect `shouldSatisfy` isLeft
    
    it "rejects dialect with unresolvable vocabulary" $ do
      let dialect = Dialect
            { dialectURI = "https://example.com/dialect"
            , dialectVersion = Draft202012
            , dialectName = "Test Dialect"
            , dialectVocabularies = Map.singleton "https://example.com/unknown-vocab" True
            , dialectDefaultFormat = FormatAnnotation
            , dialectUnknownKeywords = CollectUnknown
            }
      case validateDialect emptyRegistry dialect of
        Left (UnresolvableVocabulary _) -> pure ()
        _ -> expectationFailure "Expected UnresolvableVocabulary error"
    
    it "detects keyword conflicts between vocabularies" $ do
      -- Create two vocabularies with conflicting keywords
      let keyword1 = KeywordDefinition
            { keywordName = "conflicting"
            , keywordCompile = \_ _ _ -> Left "Not implemented" :: Either Text ()
            , keywordValidate = \_ (_ :: ()) _ _ -> pure (ValidationSuccess mempty)
            , keywordNavigation = NoNavigation
            , keywordPostValidate = Nothing
            }
          vocab1 = Vocabulary
            { vocabularyURI = "https://example.com/vocab1"
            , vocabularyRequired = True
            , vocabularyKeywords = Set.singleton "conflicting"
            }
          vocab2 = Vocabulary
            { vocabularyURI = "https://example.com/vocab2"
            , vocabularyRequired = True
            , vocabularyKeywords = Set.singleton "conflicting"
            }
          registry = registerVocabulary vocab2 $ registerVocabulary vocab1 emptyRegistry
          dialect = Dialect
            { dialectURI = "https://example.com/dialect"
            , dialectVersion = Draft202012
            , dialectName = "Conflicting Dialect"
            , dialectVocabularies = Map.fromList
                [ ("https://example.com/vocab1", True)
                , ("https://example.com/vocab2", True)
                ]
            , dialectDefaultFormat = FormatAnnotation
            , dialectUnknownKeywords = CollectUnknown
            }
      
      case validateDialect registry dialect of
        Left (KeywordConflicts conflicts) -> do
          length conflicts `shouldBe` 1
          fst (head conflicts) `shouldBe` "conflicting"
        _ -> expectationFailure "Expected KeywordConflicts error"

  describe "FormatBehavior" $ do
    it "has FormatAssertion variant" $ do
      show FormatAssertion `shouldContain` "Assertion"
    
    it "has FormatAnnotation variant" $ do
      show FormatAnnotation `shouldContain` "Annotation"

  describe "UnknownKeywordMode" $ do
    it "has all variants" $ do
      let modes = [IgnoreUnknown, WarnUnknown, ErrorOnUnknown, CollectUnknown]
      length modes `shouldBe` 4

  describe "defaultDialectURI" $ do
    it "returns correct URI for Draft04" $ do
      defaultDialectURI Draft04 `shouldBe` "http://json-schema.org/draft-04/schema#"
    
    it "returns correct URI for Draft06" $ do
      defaultDialectURI Draft06 `shouldBe` "http://json-schema.org/draft-06/schema#"
    
    it "returns correct URI for Draft07" $ do
      defaultDialectURI Draft07 `shouldBe` "http://json-schema.org/draft-07/schema#"
    
    it "returns correct URI for Draft201909" $ do
      defaultDialectURI Draft201909 `shouldBe` "https://json-schema.org/draft/2019-09/schema"
    
    it "returns correct URI for Draft202012" $ do
      defaultDialectURI Draft202012 `shouldBe` "https://json-schema.org/draft/2020-12/schema"

  describe "Dialect composition" $ do
    it "composes dialect with valid vocabularies" $ do
      let vocab = Vocabulary
            { vocabularyURI = "https://example.com/vocab"
            , vocabularyRequired = False
            , vocabularyKeywords = Set.empty
            }
          registry = registerVocabulary vocab emptyVocabularyRegistry
          dialect = Dialect
            { dialectURI = "https://example.com/dialect"
            , dialectVersion = Draft202012
            , dialectName = "Example Dialect"
            , dialectVocabularies = Map.singleton "https://example.com/vocab" False
            , dialectDefaultFormat = FormatAnnotation
            , dialectUnknownKeywords = CollectUnknown
            }
      
      validateDialect registry dialect `shouldBe` Right ()
    
    it "preserves vocabulary requirements" $ do
      let dialect = draft201909Dialect
          vocabs = dialectVocabularies dialect
      
      -- Core vocabulary should be required
      Map.lookup "https://json-schema.org/draft/2019-09/vocab/core" vocabs `shouldBe` Just True
      
      -- Format vocabulary should be optional
      Map.lookup "https://json-schema.org/draft/2019-09/vocab/format" vocabs `shouldBe` Just False

  describe "Dialect Registration API" $ do
    describe "registerDialect and lookupDialect" $ do
      it "registers and looks up a dialect" $ do
        let dialect = draft04Dialect
            registry = Vocab.registerDialect dialect Vocab.emptyVocabularyRegistry
        Vocab.lookupDialect (dialectURI dialect) registry `shouldBe` Just dialect
      
      it "returns Nothing for unregistered dialect" $ do
        let registry = Vocab.emptyVocabularyRegistry
        Vocab.lookupDialect "https://unknown.com/dialect" registry `shouldBe` Nothing
      
      it "overwrites existing dialect with same URI" $ do
        let dialect1 = draft04Dialect
            dialect2 = draft04Dialect { dialectName = "Modified Draft-04" }
            registry = Vocab.registerDialect dialect2 $ Vocab.registerDialect dialect1 Vocab.emptyVocabularyRegistry
        case Vocab.lookupDialect (dialectURI dialect1) registry of
          Just d -> dialectName d `shouldBe` "Modified Draft-04"
          Nothing -> expectationFailure "Dialect not found"
    
    describe "unregisterDialect" $ do
      it "removes a dialect from registry" $ do
        let dialect = draft04Dialect
            registry = Vocab.registerDialect dialect Vocab.emptyVocabularyRegistry
            registry' = Vocab.unregisterDialect (dialectURI dialect) registry
        Vocab.lookupDialect (dialectURI dialect) registry' `shouldBe` Nothing
      
      it "is a no-op for unregistered dialect" $ do
        let registry = Vocab.emptyVocabularyRegistry
            registry' = Vocab.unregisterDialect "https://unknown.com/dialect" registry
        Vocab.listDialects registry' `shouldBe` []
    
    describe "hasDialect" $ do
      it "returns True for registered dialect" $ do
        let dialect = draft04Dialect
            registry = Vocab.registerDialect dialect Vocab.emptyVocabularyRegistry
        Vocab.hasDialect (dialectURI dialect) registry `shouldBe` True
      
      it "returns False for unregistered dialect" $ do
        let registry = Vocab.emptyVocabularyRegistry
        Vocab.hasDialect "https://unknown.com/dialect" registry `shouldBe` False
    
    describe "listDialects" $ do
      it "returns empty list for empty registry" $ do
        Vocab.listDialects Vocab.emptyVocabularyRegistry `shouldBe` []
      
      it "returns all registered dialects" $ do
        let registry = Vocab.registerDialect draft06Dialect $
                      Vocab.registerDialect draft04Dialect Vocab.emptyVocabularyRegistry
            dialects = Vocab.listDialects registry
        length dialects `shouldBe` 2
        map dialectVersion dialects `shouldMatchList` [Draft04, Draft06]
    
    describe "getDialectByVersion" $ do
      it "finds dialect by version" $ do
        let registry = Vocab.registerDialect draft04Dialect Vocab.emptyVocabularyRegistry
        case Vocab.getDialectByVersion Draft04 registry of
          Just d -> dialectVersion d `shouldBe` Draft04
          Nothing -> expectationFailure "Dialect not found"
      
      it "returns Nothing for unregistered version" $ do
        let registry = Vocab.emptyVocabularyRegistry
        Vocab.getDialectByVersion Draft04 registry `shouldBe` Nothing
      
      it "returns first dialect when multiple exist for version" $ do
        let dialect1 = draft04Dialect
            dialect2 = draft04Dialect { dialectURI = "https://custom.com/draft-04", dialectName = "Custom Draft-04" }
            registry = Vocab.registerDialect dialect2 $ Vocab.registerDialect dialect1 Vocab.emptyVocabularyRegistry
        case Vocab.getDialectByVersion Draft04 registry of
          Just d -> dialectVersion d `shouldBe` Draft04
          Nothing -> expectationFailure "Dialect not found"
    
    describe "registerDialects" $ do
      it "registers multiple dialects" $ do
        let dialects = [draft04Dialect, draft06Dialect, draft07Dialect]
        case Vocab.registerDialects dialects Vocab.emptyVocabularyRegistry of
          Right registry -> do
            length (Vocab.listDialects registry) `shouldBe` 3
            Vocab.hasDialect (dialectURI draft04Dialect) registry `shouldBe` True
            Vocab.hasDialect (dialectURI draft06Dialect) registry `shouldBe` True
            Vocab.hasDialect (dialectURI draft07Dialect) registry `shouldBe` True
          Left err -> expectationFailure $ "Registration failed: " ++ show err
      
      it "detects duplicate URIs in input" $ do
        let dialects = [draft04Dialect, draft04Dialect]
        case Vocab.registerDialects dialects Vocab.emptyVocabularyRegistry of
          Left err -> err `shouldSatisfy` T.isInfixOf "Duplicate"
          Right _ -> expectationFailure "Should have detected duplicate"
    
    describe "standardDialectRegistry" $ do
      it "contains all standard dialects" $ do
        let registry = Vocab.standardDialectRegistry
            dialects = Vocab.listDialects registry
        length dialects `shouldBe` 5
      
      it "contains Draft-04 through 2020-12" $ do
        let registry = Vocab.standardDialectRegistry
        Vocab.hasDialect (dialectURI draft04Dialect) registry `shouldBe` True
        Vocab.hasDialect (dialectURI draft06Dialect) registry `shouldBe` True
        Vocab.hasDialect (dialectURI draft07Dialect) registry `shouldBe` True
        Vocab.hasDialect (dialectURI draft201909Dialect) registry `shouldBe` True
        Vocab.hasDialect (dialectURI draft202012Dialect) registry `shouldBe` True
      
      it "allows lookup by version" $ do
        let registry = Vocab.standardDialectRegistry
        Vocab.getDialectByVersion Draft04 registry `shouldSatisfy` (/= Nothing)
        Vocab.getDialectByVersion Draft202012 registry `shouldSatisfy` (/= Nothing)
