{-# LANGUAGE PatternSynonyms #-}
module JsonSchema.Validator.ExtendedSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)

import JsonSchema.Validator.Extended
import JsonSchema.Parser.Extended
import JsonSchema.Keyword
import JsonSchema.Keyword.Types
import JsonSchema.Types
  ( ValidationConfig(..)
  , validationFailure
  , pattern ValidationSuccess
  )
import JsonSchema.Validator (defaultValidationConfig)

-- Test keyword: range validation
data RangeData = RangeData { rangeMin :: Double, rangeMax :: Double }
  deriving (Show, Typeable)

compileRange :: CompileFunc RangeData
compileRange value _schema _ctx = do
  case value of
    Aeson.Object obj -> do
      minVal <- case KeyMap.lookup "min" obj of
                  Just (Number n) -> Right (realToFrac n)
                  _ -> Left "x-range.min must be a number"
      maxVal <- case KeyMap.lookup "max" obj of
                  Just (Number n) -> Right (realToFrac n)
                  _ -> Left "x-range.max must be a number"
      if minVal <= maxVal
        then Right $ RangeData minVal maxVal
        else Left "x-range: min must be <= max"
    _ -> Left "x-range must be an object with min and max"

validateRange :: ValidateFunc RangeData
validateRange _recursiveValidator (RangeData minVal maxVal) _ctx (Number n) =
  let val = realToFrac n
  in if val >= minVal && val <= maxVal
       then pure (ValidationSuccess mempty)
       else pure $
         validationFailure "x-range" $
           "Value " <> T.pack (show val)
           <> " is not in range [" <> T.pack (show minVal) <> ", " <> T.pack (show maxVal) <> "]"
validateRange _ _ _ _ =
  pure $ validationFailure "x-range" "x-range can only validate numbers"

-- Test keyword: string prefix
data PrefixData = PrefixData Text
  deriving (Show, Typeable)

compilePrefix :: CompileFunc PrefixData
compilePrefix (String s) _schema _ctx = Right $ PrefixData s
compilePrefix _ _schema _ctx = Left "x-prefix must be a string"

validatePrefix :: ValidateFunc PrefixData
validatePrefix _recursiveValidator (PrefixData prefix) _ctx (String s) =
  if prefix `T.isPrefixOf` s
    then pure (ValidationSuccess mempty)
    else pure $ validationFailure "x-prefix" ("String does not start with prefix: " <> prefix)
validatePrefix _ _ _ _ =
  pure $ validationFailure "x-prefix" "x-prefix can only validate strings"

spec :: Spec
spec = describe "Extended Validator" $ do

  describe "Standard Validation Only" $ do
    it "validates standard schema without custom keywords" $ do
      let schemaJson = Aeson.object
            [ "type" Aeson..= String "string"
            , "minLength" Aeson..= Number 3
            ]
          registry = emptyKeywordRegistry

      case parseSchemaWithRegistry registry Map.empty schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right extSchema -> do
          -- Valid string
          let validResult = validateExtended extSchema (String "hello")
          extendedIsValid validResult `shouldBe` True
          extendedCustomErrors validResult `shouldBe` []

          -- Invalid: too short
          let invalidResult = validateExtended extSchema (String "ab")
          extendedIsValid invalidResult `shouldBe` False
          extendedCustomErrors invalidResult `shouldBe` []  -- No custom errors

  describe "Custom Keyword Validation" $ do
    it "validates with single custom keyword" $ do
      let def = mkKeywordDefinition "x-prefix"  compilePrefix validatePrefix
          registry = registerKeyword def emptyKeywordRegistry
          schemaJson = Aeson.object
            [ "type" Aeson..= String "string"
            , "x-prefix" Aeson..= String "test"
            ]

      case parseSchemaWithRegistry registry Map.empty schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right extSchema -> do
          -- Valid: starts with "test"
          let validResult = validateExtended extSchema (String "testing")
          extendedIsValid validResult `shouldBe` True
          extendedCustomErrors validResult `shouldBe` []

          -- Invalid: doesn't start with "test"
          let invalidResult = validateExtended extSchema (String "hello")
          extendedIsValid invalidResult `shouldBe` False
          length (extendedCustomErrors invalidResult) `shouldBe` 1
          head (extendedCustomErrors invalidResult) `shouldSatisfy` T.isInfixOf "does not start with prefix"

    it "validates with multiple custom keywords" $ do
      let prefixDef = mkKeywordDefinition "x-prefix"  compilePrefix validatePrefix
          rangeDef = mkKeywordDefinition "x-range"  compileRange validateRange
          registry = registerKeyword prefixDef $ registerKeyword rangeDef emptyKeywordRegistry
          schemaJson = Aeson.object
            [ "x-prefix" Aeson..= String "val"
            , "x-range" Aeson..= Aeson.object
                [ "min" Aeson..= Number 0
                , "max" Aeson..= Number 100
                ]
            ]

      case parseSchemaWithRegistry registry Map.empty schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right extSchema -> do
          -- Test string (x-prefix passes, x-range fails for strings)
          let strResult = validateExtended extSchema (String "value")
          extendedIsValid strResult `shouldBe` False
          length (extendedCustomErrors strResult) `shouldBe` 1
          head (extendedCustomErrors strResult) `shouldSatisfy` T.isInfixOf "can only validate numbers"

          -- Test number (x-range passes, x-prefix fails for numbers)
          let numResult = validateExtended extSchema (Number 50)
          extendedIsValid numResult `shouldBe` False
          length (extendedCustomErrors numResult) `shouldBe` 1
          head (extendedCustomErrors numResult) `shouldSatisfy` T.isInfixOf "can only validate strings"

  describe "Combined Standard and Custom Validation" $ do
    it "requires both standard and custom to pass" $ do
      let def = mkKeywordDefinition "x-prefix"  compilePrefix validatePrefix
          registry = registerKeyword def emptyKeywordRegistry
          schemaJson = Aeson.object
            [ "type" Aeson..= String "string"
            , "minLength" Aeson..= Number 10
            , "x-prefix" Aeson..= String "test"
            ]

      case parseSchemaWithRegistry registry Map.empty schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right extSchema -> do
          -- Both pass (length >= 10, starts with "test")
          let bothValidResult = validateExtended extSchema (String "test123456")
          extendedIsValid bothValidResult `shouldBe` True

          -- Standard fails, custom passes (length < 10, starts with "test")
          let standardFailsResult = validateExtended extSchema (String "test")
          extendedIsValid standardFailsResult `shouldBe` False
          extendedCustomErrors standardFailsResult `shouldBe` []  -- Custom passed

          -- Standard passes, custom fails (length >= 10, doesn't start with "test")
          let customFailsResult = validateExtended extSchema (String "hello12345")
          extendedIsValid customFailsResult `shouldBe` False
          length (extendedCustomErrors customFailsResult) `shouldBe` 1

          -- Both fail (length < 10, doesn't start with "test")
          let bothFailResult = validateExtended extSchema (String "hi")
          extendedIsValid bothFailResult `shouldBe` False
          length (extendedCustomErrors bothFailResult) `shouldBe` 1

  describe "ExtendedValidationResult" $ do
    it "converts standard ValidationResult to ExtendedValidationResult" $ do
      let schemaJson = Aeson.object [ "type" Aeson..= String "number" ]
          registry = emptyKeywordRegistry

      case parseSchemaWithRegistry registry Map.empty schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right extSchema -> do
          let result = validateExtended extSchema (Number 42)
              converted = toExtendedValidationResult (extendedStandardResult result)

          extendedIsValid converted `shouldBe` True
          extendedCustomErrors converted `shouldBe` []

  describe "Validation with Config" $ do
    it "uses custom validation config" $ do
      let def = mkKeywordDefinition "x-prefix"  compilePrefix validatePrefix
          registry = registerKeyword def emptyKeywordRegistry
          schemaJson = Aeson.object
            [ "type" Aeson..= String "string"
            , "x-prefix" Aeson..= String "test"
            ]
          config = defaultValidationConfig  -- Could customize this

      case parseSchemaWithRegistry registry Map.empty schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right extSchema -> do
          let result = validateExtendedWithConfig config extSchema (String "testing")
          extendedIsValid result `shouldBe` True

  describe "Error Collection" $ do
    it "collects all custom keyword errors" $ do
      let prefixDef = mkKeywordDefinition "x-prefix"  compilePrefix validatePrefix
          rangeDef = mkKeywordDefinition "x-range"  compileRange validateRange
          registry = registerKeyword prefixDef $ registerKeyword rangeDef emptyKeywordRegistry
          schemaJson = Aeson.object
            [ "x-prefix" Aeson..= String "test"
            , "x-range" Aeson..= Aeson.object
                [ "min" Aeson..= Number 10
                , "max" Aeson..= Number 20
                ]
            ]

      case parseSchemaWithRegistry registry Map.empty schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right extSchema -> do
          -- Both keywords should fail for this value
          let result = validateExtended extSchema (Number 5)
          extendedIsValid result `shouldBe` False
          -- Should have errors from both keywords
          length (extendedCustomErrors result) `shouldBe` 2
