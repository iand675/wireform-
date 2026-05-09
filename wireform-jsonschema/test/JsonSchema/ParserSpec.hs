module JsonSchema.ParserSpec (spec) where

import Test.Hspec
import qualified Test.Hspec.Hedgehog as HH
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import JsonSchema.Types
import JsonSchema.Parser
import JsonSchema.Renderer
import Data.Aeson (Value(..))

spec :: Spec
spec = do
  describe "parseSchema" $ do
    it "parses boolean schema true" $ do
      let result = parseSchema (Bool True)
      case result of
        Right schema -> schemaCore schema `shouldBe` BooleanSchema True
        Left err -> expectationFailure $ "Parse failed: " <> show err
    
    it "parses boolean schema false" $ do
      let result = parseSchema (Bool False)
      case result of
        Right schema -> schemaCore schema `shouldBe` BooleanSchema False
        Left err -> expectationFailure $ "Parse failed: " <> show err
  
  describe "detectVersion" $ do
    it "defaults to Draft202012 when $schema missing" $ do
      let result = parseSchema (Bool True)
      case result of
        Right schema -> schemaVersion schema `shouldBe` Just Draft202012
        Left err -> expectationFailure $ "Parse failed: " <> show err
  
  describe "roundtrip property" $ do
    it "parseSchema . renderSchema ≡ id for boolean schemas" $ HH.hedgehog $ do
      bool <- H.forAll $ Gen.bool
      let schema = Schema
            { schemaVersion = Just Draft202012
            , schemaId = Nothing
            , schemaCore = BooleanSchema bool
            , schemaVocabulary = Nothing
            , schemaExtensions = mempty
            }
      let rendered = renderSchema schema
      let reparsed = parseSchema rendered
      case reparsed of
        Right schema' -> schemaCore schema' H.=== schemaCore schema
        Left err -> H.annotate (show err) >> H.failure

