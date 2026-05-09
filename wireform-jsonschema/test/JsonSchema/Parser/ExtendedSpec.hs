{-# LANGUAGE PatternSynonyms #-}
module JsonSchema.Parser.ExtendedSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)

import JsonSchema.Parser.Extended
import JsonSchema.Parser (parseSchema, ParseError(..))
import JsonSchema.Keyword
import JsonSchema.Keyword.Compile
import JsonSchema.Types
  ( Schema
  , JsonSchemaVersion(..)
  , validationFailure
  , pattern ValidationSuccess
  )

-- Test keyword implementations
data MaxValueData = MaxValueData Double
  deriving (Show, Typeable)

compileMaxValue :: CompileFunc MaxValueData
compileMaxValue (Number n) _schema _ctx = Right $ MaxValueData (realToFrac n)
compileMaxValue _ _schema _ctx = Left "x-max-value must be a number"

validateMaxValue :: ValidateFunc MaxValueData
validateMaxValue _recursiveValidator (MaxValueData maxVal) _ctx (Number n) =
  if realToFrac n <= maxVal
    then pure (ValidationSuccess mempty)
    else pure $ validationFailure "x-max-value" "Value exceeds maximum"
validateMaxValue _ _ _ _ =
  pure $ validationFailure "x-max-value" "x-max-value can only validate numbers"

spec :: Spec
spec = describe "Extended Parser" $ do

  describe "Basic Schema Parsing" $ do
    it "parses standard schema without custom keywords" $ do
      let schemaJson = Aeson.object
            [ "type" Aeson..= String "string"
            , "minLength" Aeson..= Number 5
            ]
          registry = emptyKeywordRegistry

      case parseSchemaWithRegistry registry Map.empty schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right extSchema -> do
          -- Should have no compiled keywords
          let (CompiledKeywords compiled) = extendedCompiledKeywords extSchema
          Map.size compiled `shouldBe` 0

    it "converts standard Schema to ExtendedSchema" $ do
      let schemaJson = Aeson.object [ "type" Aeson..= String "number" ]
          stdResult = case parseSchema schemaJson of
                        Right s -> Just s
                        Left _ -> Nothing

      case stdResult of
        Nothing -> expectationFailure "Failed to parse standard schema"
        Just schema -> do
          let extSchema = toExtendedSchema schema
          fromExtendedSchema extSchema `shouldBe` schema
          let (CompiledKeywords compiled) = extendedCompiledKeywords extSchema
          Map.size compiled `shouldBe` 0

  describe "Custom Keyword Parsing" $ do
    it "parses schema with registered custom keyword" $ do
      let def = mkKeywordDefinition "x-max-value" compileMaxValue validateMaxValue
          registry = registerKeyword def emptyKeywordRegistry
          schemaJson = Aeson.object
            [ "type" Aeson..= String "number"
            , "x-max-value" Aeson..= Number 100
            ]

      case parseSchemaWithRegistry registry Map.empty schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right extSchema -> do
          -- Should have one compiled keyword
          let (CompiledKeywords compiled) = extendedCompiledKeywords extSchema
          Map.size compiled `shouldBe` 1

          -- Should have custom keyword value stored
          let customValues = extendedCustomKeywordValues extSchema
          Map.lookup "x-max-value" customValues `shouldBe` Just (Number 100)

    it "ignores unregistered custom keywords" $ do
      let registry = emptyKeywordRegistry  -- No keywords registered
          schemaJson = Aeson.object
            [ "type" Aeson..= String "string"
            , "x-unknown" Aeson..= String "value"
            ]

      case parseSchemaWithRegistry registry Map.empty schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right extSchema -> do
          -- Should have no compiled keywords (x-unknown not registered)
          let (CompiledKeywords compiled) = extendedCompiledKeywords extSchema
          Map.size compiled `shouldBe` 0

          -- Custom keyword values should also be empty (only tracks registered keywords)
          let customValues = extendedCustomKeywordValues extSchema
          Map.size customValues `shouldBe` 0

    it "parses schema with multiple custom keywords" $ do
      let maxDef = mkKeywordDefinition "x-max" compileMaxValue validateMaxValue
          minDef = mkKeywordDefinition "x-min" compileMaxValue validateMaxValue  -- Reuse same implementation
          registry = registerKeyword maxDef $ registerKeyword minDef emptyKeywordRegistry
          schemaJson = Aeson.object
            [ "x-max" Aeson..= Number 100
            , "x-min" Aeson..= Number 1
            ]

      case parseSchemaWithRegistry registry Map.empty schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right extSchema -> do
          let (CompiledKeywords compiled) = extendedCompiledKeywords extSchema
          Map.size compiled `shouldBe` 2

          let customValues = extendedCustomKeywordValues extSchema
          Map.size customValues `shouldBe` 2

  describe "Compilation Errors" $ do
    it "fails when custom keyword has invalid value" $ do
      let def = mkKeywordDefinition "x-max-value" compileMaxValue validateMaxValue
          registry = registerKeyword def emptyKeywordRegistry
          schemaJson = Aeson.object
            [ "x-max-value" Aeson..= String "not a number"  -- Wrong type
            ]

      case parseSchemaWithRegistry registry Map.empty schemaJson of
        Left err -> parseErrorMessage err `shouldSatisfy` T.isInfixOf "x-max-value must be a number"
        Right _ -> expectationFailure "Should have failed with invalid keyword value"

  describe "Schema Registry Integration" $ do
    it "provides schema registry to compilation context" $ do
      -- This test verifies the registry is passed through, but we can't easily
      -- test $ref resolution without a more complex setup. This is a placeholder
      -- that verifies the API accepts a schema registry.
      let registry = emptyKeywordRegistry
          schemaRegistry = Map.empty :: Map Text Schema
          schemaJson = Aeson.object [ "type" Aeson..= String "string" ]

      case parseSchemaWithRegistry registry schemaRegistry schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right _ -> return ()  -- Success is sufficient

  describe "Version-Specific Parsing" $ do
    it "parses with explicit version" $ do
      let def = mkKeywordDefinition "x-test" compileMaxValue validateMaxValue
          registry = registerKeyword def emptyKeywordRegistry
          schemaJson = Aeson.object
            [ "x-test" Aeson..= Number 42
            ]

      case parseSchemaWithRegistryAndVersion registry Map.empty Draft202012 schemaJson of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right extSchema -> do
          let (CompiledKeywords compiled) = extendedCompiledKeywords extSchema
          Map.size compiled `shouldBe` 1
