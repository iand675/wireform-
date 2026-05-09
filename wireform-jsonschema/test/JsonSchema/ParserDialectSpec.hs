{-# LANGUAGE OverloadedStrings #-}
module JsonSchema.ParserDialectSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T

import JsonSchema.Parser
import JsonSchema.Types
import JsonSchema.Vocabulary (VocabularyRegistry, standardDialectRegistry, emptyVocabularyRegistry)
import JsonSchema.Dialect
  ( draft04Dialect
  , draft06Dialect
  , draft07Dialect
  , draft201909Dialect
  , draft202012Dialect
  , dialectURI
  , dialectVersion
  )

spec :: Spec
spec = do
  describe "parseSchemaWithDialectRegistry" $ do
    describe "with standard dialects" $ do
      it "parses Draft-04 schema" $ do
        let schema = object
              [ "$schema" .= ("http://json-schema.org/draft-04/schema#" :: T.Text)
              , "type" .= ("string" :: T.Text)
              ]
        case parseSchemaWithDialectRegistry standardDialectRegistry schema of
          Right parsed -> do
            schemaVersion parsed `shouldBe` Just Draft04
            schemaMetaschemaURI parsed `shouldBe` Just "http://json-schema.org/draft-04/schema#"
          Left err -> expectationFailure $ "Failed to parse: " ++ show err
      
      it "parses Draft-06 schema" $ do
        let schema = object
              [ "$schema" .= ("http://json-schema.org/draft-06/schema#" :: T.Text)
              , "type" .= ("number" :: T.Text)
              ]
        case parseSchemaWithDialectRegistry standardDialectRegistry schema of
          Right parsed -> do
            schemaVersion parsed `shouldBe` Just Draft06
            schemaMetaschemaURI parsed `shouldBe` Just "http://json-schema.org/draft-06/schema#"
          Left err -> expectationFailure $ "Failed to parse: " ++ show err
      
      it "parses Draft-07 schema" $ do
        let schema = object
              [ "$schema" .= ("http://json-schema.org/draft-07/schema#" :: T.Text)
              , "type" .= ("object" :: T.Text)
              ]
        case parseSchemaWithDialectRegistry standardDialectRegistry schema of
          Right parsed -> do
            schemaVersion parsed `shouldBe` Just Draft07
            schemaMetaschemaURI parsed `shouldBe` Just "http://json-schema.org/draft-07/schema#"
          Left err -> expectationFailure $ "Failed to parse: " ++ show err
      
      it "parses 2019-09 schema" $ do
        let schema = object
              [ "$schema" .= ("https://json-schema.org/draft/2019-09/schema" :: T.Text)
              , "type" .= ("array" :: T.Text)
              ]
        case parseSchemaWithDialectRegistry standardDialectRegistry schema of
          Right parsed -> do
            schemaVersion parsed `shouldBe` Just Draft201909
            schemaMetaschemaURI parsed `shouldBe` Just "https://json-schema.org/draft/2019-09/schema"
          Left err -> expectationFailure $ "Failed to parse: " ++ show err
      
      it "parses 2020-12 schema" $ do
        let schema = object
              [ "$schema" .= ("https://json-schema.org/draft/2020-12/schema" :: T.Text)
              , "type" .= ("boolean" :: T.Text)
              ]
        case parseSchemaWithDialectRegistry standardDialectRegistry schema of
          Right parsed -> do
            schemaVersion parsed `shouldBe` Just Draft202012
            schemaMetaschemaURI parsed `shouldBe` Just "https://json-schema.org/draft/2020-12/schema"
          Left err -> expectationFailure $ "Failed to parse: " ++ show err
    
    describe "error handling" $ do
      it "errors on unregistered custom dialect" $ do
        let schema = object
              [ "$schema" .= ("https://custom.example.com/schema" :: T.Text)
              , "type" .= ("string" :: T.Text)
              ]
        case parseSchemaWithDialectRegistry standardDialectRegistry schema of
          Left err -> do
            parseErrorMessage err `shouldSatisfy` T.isInfixOf "Unregistered dialect"
            parseErrorMessage err `shouldSatisfy` T.isInfixOf "https://custom.example.com/schema"
          Right _ -> expectationFailure "Should have failed with unregistered dialect error"
      
      it "errors on standard URI when not in registry" $ do
        -- If using an empty registry, even standard URIs should fail
        let schema = object
              [ "$schema" .= ("http://json-schema.org/draft-04/schema#" :: T.Text)
              , "type" .= ("string" :: T.Text)
              ]
        case parseSchemaWithDialectRegistry emptyVocabularyRegistry schema of
          Left err -> do
            parseErrorMessage err `shouldSatisfy` T.isInfixOf "Unregistered dialect"
            parseErrorMessage err `shouldSatisfy` T.isInfixOf "http://json-schema.org/draft-04/schema#"
          Right _ -> expectationFailure "Should have failed with unregistered dialect error"
      
      it "parses schema without $schema keyword" $ do
        let schema = object
              [ "type" .= ("number" :: T.Text)
              , "minimum" .= (0 :: Int)
              ]
        case parseSchemaWithDialectRegistry standardDialectRegistry schema of
          Right parsed -> do
            -- Should default to latest version
            schemaVersion parsed `shouldBe` Just Draft202012
            schemaMetaschemaURI parsed `shouldBe` Nothing
          Left err -> expectationFailure $ "Failed to parse: " ++ show err
      
      it "parses boolean schema" $ do
        let schema = Aeson.Bool True
        case parseSchemaWithDialectRegistry standardDialectRegistry schema of
          Right parsed -> do
            schemaVersion parsed `shouldBe` Just Draft202012
            schemaMetaschemaURI parsed `shouldBe` Nothing
          Left err -> expectationFailure $ "Failed to parse: " ++ show err

  describe "resolveDialectFromSchema" $ do
    it "resolves Draft-04 dialect from parsed schema" $ do
      let schema = object
            [ "$schema" .= ("http://json-schema.org/draft-04/schema#" :: T.Text)
            , "type" .= ("string" :: T.Text)
            ]
      case parseSchemaWithDialectRegistry standardDialectRegistry schema of
        Right parsed -> do
          case resolveDialectFromSchema standardDialectRegistry parsed of
            Just dialect -> do
              dialectURI dialect `shouldBe` "http://json-schema.org/draft-04/schema#"
              dialectVersion dialect `shouldBe` Draft04
            Nothing -> expectationFailure "Dialect not found"
        Left err -> expectationFailure $ "Failed to parse: " ++ show err
    
    it "returns Nothing for schema without $schema" $ do
      let schema = object [ "type" .= ("string" :: T.Text) ]
      case parseSchemaWithDialectRegistry standardDialectRegistry schema of
        Right parsed -> do
          resolveDialectFromSchema standardDialectRegistry parsed `shouldBe` Nothing
        Left err -> expectationFailure $ "Failed to parse: " ++ show err

  describe "extractSchemaURI" $ do
    it "extracts $schema from object" $ do
      let schema = object
            [ "$schema" .= ("http://json-schema.org/draft-04/schema#" :: T.Text)
            , "type" .= ("string" :: T.Text)
            ]
      extractSchemaURI schema `shouldBe` Just "http://json-schema.org/draft-04/schema#"
    
    it "returns Nothing for object without $schema" $ do
      let schema = object [ "type" .= ("string" :: T.Text) ]
      extractSchemaURI schema `shouldBe` Nothing
    
    it "returns Nothing for boolean schema" $ do
      extractSchemaURI (Aeson.Bool True) `shouldBe` Nothing
    
    it "returns Nothing for non-string $schema" $ do
      let schema = object [ "$schema" .= (123 :: Int) ]
      extractSchemaURI schema `shouldBe` Nothing

