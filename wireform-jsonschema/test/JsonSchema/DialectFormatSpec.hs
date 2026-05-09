{-# LANGUAGE OverloadedStrings #-}
module JsonSchema.DialectFormatSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Text as T

import JsonSchema.Types
import JsonSchema.Validator
import JsonSchema.Dialect
import JsonSchema.Parser

spec :: Spec
spec = do
  describe "Format Assertion vs Annotation Mode" $ do
    let emailSchema = object
          [ "type" .= ("string" :: T.Text)
          , "format" .= ("email" :: T.Text)
          ]
    
    describe "validationFormatAssertion flag (backward compatibility)" $ do
      it "format as annotation (default): invalid format succeeds" $ do
        let config = defaultValidationConfig
            parsed = parseSchema emailSchema
        case parsed of
          Right schema -> do
            let result = validateValue config schema (String "not-an-email")
            isSuccess result `shouldBe` True
          Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      it "format as assertion: invalid format fails" $ do
        let config = defaultValidationConfig { validationFormatAssertion = True }
            parsed = parseSchema emailSchema
        case parsed of
          Right schema -> do
            let result = validateValue config schema (String "not-an-email")
            isFailure result `shouldBe` True
          Left err -> expectationFailure $ "Parse failed: " ++ show err
    
    describe "validationDialectFormatBehavior (dialect-specific)" $ do
      it "FormatAnnotation: invalid format succeeds" $ do
        let config = defaultValidationConfig
              { validationDialectFormatBehavior = Just FormatAnnotation
              }
            parsed = parseSchema emailSchema
        case parsed of
          Right schema -> do
            let result = validateValue config schema (String "not-an-email")
            isSuccess result `shouldBe` True
          Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      it "FormatAssertion: invalid format fails" $ do
        let config = defaultValidationConfig
              { validationDialectFormatBehavior = Just FormatAssertion
              }
            parsed = parseSchema emailSchema
        case parsed of
          Right schema -> do
            let result = validateValue config schema (String "not-an-email")
            isFailure result `shouldBe` True
          Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      it "dialect behavior overrides boolean flag (FormatAnnotation)" $ do
        let config = defaultValidationConfig
              { validationFormatAssertion = True  -- Would assert
              , validationDialectFormatBehavior = Just FormatAnnotation  -- But dialect says annotation
              }
            parsed = parseSchema emailSchema
        case parsed of
          Right schema -> do
            let result = validateValue config schema (String "not-an-email")
            isSuccess result `shouldBe` True  -- Annotation wins
          Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      it "dialect behavior overrides boolean flag (FormatAssertion)" $ do
        let config = defaultValidationConfig
              { validationFormatAssertion = False  -- Would annotate
              , validationDialectFormatBehavior = Just FormatAssertion  -- But dialect says assertion
              }
            parsed = parseSchema emailSchema
        case parsed of
          Right schema -> do
            let result = validateValue config schema (String "not-an-email")
            isFailure result `shouldBe` True  -- Assertion wins
          Left err -> expectationFailure $ "Parse failed: " ++ show err
    
    describe "applyDialectToConfig" $ do
      it "applies FormatAssertion from dialect" $ do
        let dialect = draft04Dialect { dialectDefaultFormat = FormatAssertion }
            config = applyDialectToConfig dialect defaultValidationConfig
        validationDialectFormatBehavior config `shouldBe` Just FormatAssertion
      
      it "applies FormatAnnotation from dialect" $ do
        let dialect = draft202012Dialect  -- Has FormatAnnotation by default
            config = applyDialectToConfig dialect defaultValidationConfig
        validationDialectFormatBehavior config `shouldBe` Just FormatAnnotation
      
      it "applies dialect version" $ do
        let dialect = draft04Dialect
            config = applyDialectToConfig dialect defaultValidationConfig
        validationVersion config `shouldBe` Draft04
    
    describe "strictValidationConfig" $ do
      it "has FormatAssertion set" $ do
        validationDialectFormatBehavior strictValidationConfig `shouldBe` Just FormatAssertion
      
      it "causes format validation to fail on invalid format" $ do
        let parsed = parseSchema emailSchema
        case parsed of
          Right schema -> do
            let result = validateValue strictValidationConfig schema (String "not-an-email")
            isFailure result `shouldBe` True
          Left err -> expectationFailure $ "Parse failed: " ++ show err
    
    describe "valid format values" $ do
      it "FormatAssertion: valid email passes" $ do
        let config = defaultValidationConfig { validationDialectFormatBehavior = Just FormatAssertion }
            parsed = parseSchema emailSchema
        case parsed of
          Right schema -> do
            let result = validateValue config schema (String "user@example.com")
            isSuccess result `shouldBe` True
          Left err -> expectationFailure $ "Parse failed: " ++ show err
      
      it "FormatAnnotation: valid email passes" $ do
        let config = defaultValidationConfig { validationDialectFormatBehavior = Just FormatAnnotation }
            parsed = parseSchema emailSchema
        case parsed of
          Right schema -> do
            let result = validateValue config schema (String "user@example.com")
            isSuccess result `shouldBe` True
          Left err -> expectationFailure $ "Parse failed: " ++ show err

