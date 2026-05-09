{-# LANGUAGE OverloadedStrings #-}
module JsonSchema.UnknownKeywordSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import JsonSchema.Types
import JsonSchema.Parser
import JsonSchema.Dialect
import JsonSchema.Vocabulary (standardDialectRegistry, registerDialect)

spec :: Spec
spec = do
  describe "Unknown Keyword Handling Modes" $ do
    let schemaWithUnknown = object
          [ "type" .= ("string" :: T.Text)
          , "x-custom" .= ("value" :: T.Text)  -- Unknown keyword
          , "x-another" .= (123 :: Int)        -- Another unknown keyword
          ]
    
    let validSchema = object
          [ "type" .= ("string" :: T.Text)
          , "minLength" .= (1 :: Int)
          ]
    
    describe "CollectUnknown (default, spec-compliant)" $ do
      it "collects unknown keywords in schemaExtensions" $ do
        let config = defaultParseConfig
        case parseSchemaWithConfig config schemaWithUnknown of
          Right schema -> do
            Map.size (schemaExtensions schema) `shouldBe` 2
            Map.member "x-custom" (schemaExtensions schema) `shouldBe` True
            Map.member "x-another" (schemaExtensions schema) `shouldBe` True
          Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      it "succeeds with no unknown keywords" $ do
        let config = defaultParseConfig
        case parseSchemaWithConfig config validSchema of
          Right schema -> do
            Map.size (schemaExtensions schema) `shouldBe` 0
          Left err -> expectationFailure $ "Parse failed: " ++ show err
    
    describe "ErrorOnUnknown (strict mode, non-standard)" $ do
      it "fails when unknown keywords present" $ do
        let config = ParseConfig { parseUnknownKeywordMode = ErrorOnUnknown }
        case parseSchemaWithConfig config schemaWithUnknown of
          Left err -> do
            parseErrorMessage err `shouldSatisfy` T.isInfixOf "Unknown keywords"
            parseErrorMessage err `shouldSatisfy` T.isInfixOf "x-custom"
            parseErrorMessage err `shouldSatisfy` T.isInfixOf "x-another"
          Right _ -> expectationFailure "Should have failed with unknown keywords"
      
      it "succeeds with no unknown keywords" $ do
        let config = ParseConfig { parseUnknownKeywordMode = ErrorOnUnknown }
        case parseSchemaWithConfig config validSchema of
          Right schema -> do
            Map.size (schemaExtensions schema) `shouldBe` 0
          Left err -> expectationFailure $ "Parse failed: " ++ show err
    
    describe "WarnUnknown (development mode, non-standard)" $ do
      it "succeeds and collects unknown keywords" $ do
        let config = ParseConfig { parseUnknownKeywordMode = WarnUnknown }
        case parseSchemaWithConfig config schemaWithUnknown of
          Right schema -> do
            -- WarnUnknown still collects for inspection
            Map.size (schemaExtensions schema) `shouldBe` 2
            Map.member "x-custom" (schemaExtensions schema) `shouldBe` True
          Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      it "succeeds with no unknown keywords" $ do
        let config = ParseConfig { parseUnknownKeywordMode = WarnUnknown }
        case parseSchemaWithConfig config validSchema of
          Right schema -> do
            Map.size (schemaExtensions schema) `shouldBe` 0
          Left err -> expectationFailure $ "Parse failed: " ++ show err
    
    describe "IgnoreUnknown (permissive mode, spec-compliant)" $ do
      it "succeeds and discards unknown keywords" $ do
        let config = ParseConfig { parseUnknownKeywordMode = IgnoreUnknown }
        case parseSchemaWithConfig config schemaWithUnknown of
          Right schema -> do
            -- IgnoreUnknown clears extensions
            Map.size (schemaExtensions schema) `shouldBe` 0
            Map.member "x-custom" (schemaExtensions schema) `shouldBe` False
          Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      it "succeeds with no unknown keywords" $ do
        let config = ParseConfig { parseUnknownKeywordMode = IgnoreUnknown }
        case parseSchemaWithConfig config validSchema of
          Right schema -> do
            Map.size (schemaExtensions schema) `shouldBe` 0
          Left err -> expectationFailure $ "Parse failed: " ++ show err
  
  describe "Dialect-based unknown keyword handling" $ do
    let customDialect = draft202012Dialect
          { dialectURI = "https://example.com/strict-schema"
          , dialectUnknownKeywords = ErrorOnUnknown
          }
    
    let permissiveDialect = draft202012Dialect
          { dialectURI = "https://example.com/permissive-schema"
          , dialectUnknownKeywords = IgnoreUnknown
          }
    
    it "applies ErrorOnUnknown from dialect" $ do
      let schemaJson = object
            [ "$schema" .= dialectURI customDialect
            , "type" .= ("string" :: T.Text)
            , "x-custom" .= ("value" :: T.Text)
            ]
      -- Register custom dialect
      let registry = registerDialect customDialect standardDialectRegistry
      case parseSchemaWithDialectRegistry registry schemaJson of
        Left err -> do
          parseErrorMessage err `shouldSatisfy` T.isInfixOf "Unknown keywords"
        Right _ -> expectationFailure "Should have failed with ErrorOnUnknown"
    
    it "applies IgnoreUnknown from dialect" $ do
      let schemaJson = object
            [ "$schema" .= dialectURI permissiveDialect
            , "type" .= ("string" :: T.Text)
            , "x-custom" .= ("value" :: T.Text)
            ]
      -- Register custom dialect
      let registry = registerDialect permissiveDialect standardDialectRegistry
      case parseSchemaWithDialectRegistry registry schemaJson of
        Right schema -> do
          Map.size (schemaExtensions schema) `shouldBe` 0
        Left err -> expectationFailure $ "Parse failed: " ++ show err
    
    it "standard dialects use CollectUnknown by default" $ do
      let schemaJson = object
            [ "$schema" .= ("https://json-schema.org/draft/2020-12/schema" :: T.Text)
            , "type" .= ("string" :: T.Text)
            , "x-custom" .= ("value" :: T.Text)
            ]
      case parseSchemaWithDialectRegistry standardDialectRegistry schemaJson of
        Right schema -> do
          Map.size (schemaExtensions schema) `shouldBe` 1
          Map.member "x-custom" (schemaExtensions schema) `shouldBe` True
        Left err -> expectationFailure $ "Parse failed: " ++ show err
  
  describe "Known vs Unknown keywords" $ do
    it "standard keywords are not considered unknown" $ do
      let schema = object
            [ "type" .= ("object" :: T.Text)
            , "properties" .= object []
            , "required" .= ([] :: [T.Text])
            , "additionalProperties" .= False
            ]
      let config = ParseConfig { parseUnknownKeywordMode = ErrorOnUnknown }
      case parseSchemaWithConfig config schema of
        Right parsed -> do
          Map.size (schemaExtensions parsed) `shouldBe` 0
        Left err -> expectationFailure $ "Parse failed: " ++ show err
    
    it "x- prefixed keywords are unknown" $ do
      let schema = object
            [ "type" .= ("string" :: T.Text)
            , "x-internal-id" .= ("abc" :: T.Text)
            ]
      case parseSchema schema of
        Right parsed -> do
          Map.member "x-internal-id" (schemaExtensions parsed) `shouldBe` True
        Left err -> expectationFailure $ "Parse failed: " ++ show err
    
    it "typos are caught in ErrorOnUnknown mode" $ do
      let schema = object
            [ "typ" .= ("string" :: T.Text)  -- Typo: should be "type"
            ]
      let config = ParseConfig { parseUnknownKeywordMode = ErrorOnUnknown }
      case parseSchemaWithConfig config schema of
        Left err -> do
          parseErrorMessage err `shouldSatisfy` T.isInfixOf "typ"
        Right _ -> expectationFailure "Should have caught typo"

