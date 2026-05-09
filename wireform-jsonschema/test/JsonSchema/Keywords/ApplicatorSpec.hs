{-# LANGUAGE PatternSynonyms #-}
module JsonSchema.Keywords.ApplicatorSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..), toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T

import JsonSchema.Keywords.AllOf (validateAllOf, allOfKeyword, compileAllOf)
import JsonSchema.Keywords.AnyOf (validateAnyOf, anyOfKeyword, compileAnyOf)
import JsonSchema.Keywords.OneOf (validateOneOf, oneOfKeyword, compileOneOf)
import JsonSchema.Types (Schema(..), SchemaCore(..), ValidationResult, pattern ValidationSuccess, pattern ValidationFailure, ValidationAnnotations)
import JsonSchema.Parser (parseSchema)
import JsonSchema.Validator (validateValueWithRegistry, defaultValidationConfig)
import JsonSchema.Types (emptyRegistry)

spec :: Spec
spec = describe "Applicator Keywords" $ do
  describe "allOf" $ do
    it "validates when ALL schemas pass" $ do
      let schemaJson1 = Aeson.object [("type", toJSON ("number" :: Text))]
          schemaJson2 = Aeson.object [("minimum", toJSON (10 :: Int))]
          Right schema1 = parseSchema schemaJson1
          Right schema2 = parseSchema schemaJson2
          schemas = NE.fromList [schema1, schema2]
          value = toJSON (15 :: Int)
          recursiveValidator schema val = validateValueWithRegistry defaultValidationConfig emptyRegistry schema val

      case validateAllOf recursiveValidator schemas value of
        ValidationSuccess _ -> pure ()
        ValidationFailure errs -> expectationFailure $ "Expected success but got: " ++ show errs

    it "fails when ANY schema fails" $ do
      let schemaJson1 = Aeson.object [("type", toJSON ("number" :: Text))]
          schemaJson2 = Aeson.object [("minimum", toJSON (10 :: Int))]
          Right schema1 = parseSchema schemaJson1
          Right schema2 = parseSchema schemaJson2
          schemas = NE.fromList [schema1, schema2]
          value = toJSON (5 :: Int)  -- Fails schema2 (minimum 10)
          recursiveValidator schema val = validateValueWithRegistry defaultValidationConfig emptyRegistry schema val

      case validateAllOf recursiveValidator schemas value of
        ValidationFailure _ -> pure ()
        ValidationSuccess _ -> expectationFailure "Expected failure but validation passed"

    it "merges annotations from ALL passing schemas" $ do
      let schemaJson1 = Aeson.object [("type", toJSON ("number" :: Text))]
          schemaJson2 = Aeson.object [("minimum", toJSON (10 :: Int))]
          Right schema1 = parseSchema schemaJson1
          Right schema2 = parseSchema schemaJson2
          schemas = NE.fromList [schema1, schema2]
          value = toJSON (15 :: Int)
          recursiveValidator schema val = validateValueWithRegistry defaultValidationConfig emptyRegistry schema val

      case validateAllOf recursiveValidator schemas value of
        ValidationSuccess anns -> anns `shouldBe` mempty  -- Both return empty, mconcat is empty
        ValidationFailure errs -> expectationFailure $ "Expected success: " ++ show errs

  describe "anyOf" $ do
    it "validates when AT LEAST ONE schema passes" $ do
      let schemaJson1 = Aeson.object [("type", toJSON ("string" :: Text))]
          schemaJson2 = Aeson.object [("type", toJSON ("number" :: Text))]
          Right schema1 = parseSchema schemaJson1
          Right schema2 = parseSchema schemaJson2
          schemas = NE.fromList [schema1, schema2]
          value = toJSON (15 :: Int)  -- Passes schema2
          recursiveValidator schema val = validateValueWithRegistry defaultValidationConfig emptyRegistry schema val

      case validateAnyOf recursiveValidator schemas value of
        ValidationSuccess _ -> pure ()
        ValidationFailure errs -> expectationFailure $ "Expected success but got: " ++ show errs

    it "fails when NO schemas pass" $ do
      let schemaJson1 = Aeson.object [("type", toJSON ("string" :: Text))]
          schemaJson2 = Aeson.object [("type", toJSON ("boolean" :: Text))]
          Right schema1 = parseSchema schemaJson1
          Right schema2 = parseSchema schemaJson2
          schemas = NE.fromList [schema1, schema2]
          value = toJSON (15 :: Int)  -- Fails both (not string, not boolean)
          recursiveValidator schema val = validateValueWithRegistry defaultValidationConfig emptyRegistry schema val

      case validateAnyOf recursiveValidator schemas value of
        ValidationFailure _ -> pure ()
        ValidationSuccess _ -> expectationFailure "Expected failure but validation passed"

    it "merges annotations from ALL passing schemas" $ do
      let schemaJson1 = Aeson.object [("type", toJSON ("number" :: Text))]
          schemaJson2 = Aeson.object [("minimum", toJSON (0 :: Int))]
          Right schema1 = parseSchema schemaJson1
          Right schema2 = parseSchema schemaJson2
          schemas = NE.fromList [schema1, schema2]
          value = toJSON (15 :: Int)  -- Passes both
          recursiveValidator schema val = validateValueWithRegistry defaultValidationConfig emptyRegistry schema val

      case validateAnyOf recursiveValidator schemas value of
        ValidationSuccess anns -> anns `shouldBe` mempty
        ValidationFailure errs -> expectationFailure $ "Expected success: " ++ show errs

  describe "oneOf" $ do
    it "validates when EXACTLY ONE schema passes" $ do
      let schemaJson1 = Aeson.object [("type", toJSON ("string" :: Text))]
          schemaJson2 = Aeson.object [("type", toJSON ("number" :: Text))]
          Right schema1 = parseSchema schemaJson1
          Right schema2 = parseSchema schemaJson2
          schemas = NE.fromList [schema1, schema2]
          value = toJSON (15 :: Int)  -- Passes schema2 only
          recursiveValidator schema val = validateValueWithRegistry defaultValidationConfig emptyRegistry schema val

      case validateOneOf recursiveValidator schemas value of
        ValidationSuccess _ -> pure ()
        ValidationFailure errs -> expectationFailure $ "Expected success but got: " ++ show errs

    it "fails when ZERO schemas pass" $ do
      let schemaJson1 = Aeson.object [("type", toJSON ("string" :: Text))]
          schemaJson2 = Aeson.object [("type", toJSON ("boolean" :: Text))]
          Right schema1 = parseSchema schemaJson1
          Right schema2 = parseSchema schemaJson2
          schemas = NE.fromList [schema1, schema2]
          value = toJSON (15 :: Int)  -- Fails both
          recursiveValidator schema val = validateValueWithRegistry defaultValidationConfig emptyRegistry schema val

      case validateOneOf recursiveValidator schemas value of
        ValidationFailure _ -> pure ()
        ValidationSuccess _ -> expectationFailure "Expected failure but validation passed"

    it "fails when MORE THAN ONE schema passes" $ do
      let schemaJson1 = Aeson.object [("type", toJSON ("number" :: Text)), ("minimum", toJSON (0 :: Int))]
          schemaJson2 = Aeson.object [("type", toJSON ("number" :: Text)), ("maximum", toJSON (100 :: Int))]
          Right schema1 = parseSchema schemaJson1
          Right schema2 = parseSchema schemaJson2
          schemas = NE.fromList [schema1, schema2]
          value = toJSON (15 :: Int)  -- Passes both (>= 0 AND <= 100)
          recursiveValidator schema val = validateValueWithRegistry defaultValidationConfig emptyRegistry schema val

      case validateOneOf recursiveValidator schemas value of
        ValidationFailure _ -> pure ()
        ValidationSuccess _ -> expectationFailure "Expected failure but validation passed"

    it "returns annotations from the single passing schema" $ do
      let schemaJson1 = Aeson.object [("type", toJSON ("string" :: Text))]
          schemaJson2 = Aeson.object [("type", toJSON ("number" :: Text)), ("minimum", toJSON (0 :: Int))]
          Right schema1 = parseSchema schemaJson1
          Right schema2 = parseSchema schemaJson2
          schemas = NE.fromList [schema1, schema2]
          value = toJSON (15 :: Int)  -- Passes schema2 only (number >= 0)
          recursiveValidator schema val = validateValueWithRegistry defaultValidationConfig emptyRegistry schema val

      case validateOneOf recursiveValidator schemas value of
        ValidationSuccess anns -> anns `shouldBe` mempty  -- Schema2 returns empty annotations
        ValidationFailure errs -> expectationFailure $ "Expected success: " ++ show errs

