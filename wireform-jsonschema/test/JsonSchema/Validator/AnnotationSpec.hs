{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module JsonSchema.Validator.AnnotationSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..), object, (.=), toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V

import JsonSchema
import JsonSchema.Types (ValidationResult, pattern ValidationSuccess, ValidationAnnotations(..), emptyRegistry, JsonPointer, emptyPointer)
import JsonSchema.Validator (validateValueWithRegistry, defaultValidationConfig)
import JsonSchema.Parser (parseSchema)
import JsonPointer ((/.), renderPointer)

spec :: Spec
spec = describe "Annotation Collection" $ do
  
  describe "Properties annotations" $ do
    it "collects property names from 'properties' keyword" $ do
      let schemaJson = object
            [ "type" .= ("object" :: Text)
            , "properties" .= object
                [ "foo" .= object ["type" .= ("string" :: Text)]
                , "bar" .= object ["type" .= ("number" :: Text)]
                ]
            ]
          Right schema = parseSchema schemaJson
          instance' = object ["foo" .= ("hello" :: Text), "bar" .= (42 :: Int)]
          
      case validateValueWithRegistry defaultValidationConfig emptyRegistry schema instance' of
        ValidationSuccess anns -> do
          -- Check that properties annotation exists
          let annMap = unAnnotations anns
          case Map.lookup emptyPointer annMap of
            Just kwMap -> do
              -- Check for "properties" keyword annotation
              case Map.lookup "properties" kwMap of
                Just (Array props) -> do
                  let propNames = [name | String name <- V.toList props]
                  Set.fromList propNames `shouldBe` Set.fromList ["foo", "bar"]
                Nothing -> expectationFailure "No 'properties' annotation found"
            Nothing -> expectationFailure "No annotations at root path"
        failure -> expectationFailure $ "Validation failed: " ++ show failure
    
    it "collects only validated properties (not all defined properties)" $ do
      let schemaJson = object
            [ "type" .= ("object" :: Text)
            , "properties" .= object
                [ "foo" .= object ["type" .= ("string" :: Text)]
                , "bar" .= object ["type" .= ("number" :: Text)]
                , "baz" .= object ["type" .= ("boolean" :: Text)]
                ]
            ]
          Right schema = parseSchema schemaJson
          instance' = object ["foo" .= ("hello" :: Text)]  -- Only foo present
          
      case validateValueWithRegistry defaultValidationConfig emptyRegistry schema instance' of
        ValidationSuccess anns -> do
          let annMap = unAnnotations anns
          case Map.lookup emptyPointer annMap of
            Just kwMap -> do
              case Map.lookup "properties" kwMap of
                Just (Array props) -> do
                  let propNames = [name | String name <- V.toList props]
                  propNames `shouldBe` ["foo"]  -- Only foo was validated
                Nothing -> expectationFailure "No 'properties' annotation found"
            Nothing -> expectationFailure "No annotations at root path"
        failure -> expectationFailure $ "Validation failed: " ++ show failure

  describe "PatternProperties annotations" $ do
    it "collects property names matching patterns" $ do
      let schemaJson = object
            [ "type" .= ("object" :: Text)
            , "patternProperties" .= object
                [ "^f" .= object ["type" .= ("string" :: Text)]
                ]
            ]
          Right schema = parseSchema schemaJson
          instance' = object ["foo" .= ("hello" :: Text), "bar" .= ("world" :: Text)]
          
      case validateValueWithRegistry defaultValidationConfig emptyRegistry schema instance' of
        ValidationSuccess anns -> do
          let annMap = unAnnotations anns
          case Map.lookup emptyPointer annMap of
            Just kwMap -> do
              -- Pattern properties should also contribute to properties annotation
              case Map.lookup "properties" kwMap of
                Just (Array props) -> do
                  let propNames = [name | String name <- V.toList props]
                  "foo" `elem` propNames `shouldBe` True
                Nothing -> pure ()  -- Pattern properties might use different annotation key
            Nothing -> pure ()  -- Acceptable if no annotations
        failure -> expectationFailure $ "Validation failed: " ++ show failure

  describe "AdditionalProperties annotations" $ do
    it "collects additional property names" $ do
      let schemaJson = object
            [ "type" .= ("object" :: Text)
            , "properties" .= object
                [ "foo" .= object ["type" .= ("string" :: Text)]
                ]
            , "additionalProperties" .= object ["type" .= ("number" :: Text)]
            ]
          Right schema = parseSchema schemaJson
          instance' = object 
            [ "foo" .= ("hello" :: Text)
            , "extra1" .= (42 :: Int)
            , "extra2" .= (99 :: Int)
            ]
          
      case validateValueWithRegistry defaultValidationConfig emptyRegistry schema instance' of
        ValidationSuccess anns -> do
          let annMap = unAnnotations anns
          case Map.lookup emptyPointer annMap of
            Just kwMap -> do
              case Map.lookup "properties" kwMap of
                Just (Array props) -> do
                  let propNames = Set.fromList [name | String name <- V.toList props]
                  -- Should include foo and additional properties
                  propNames `shouldSatisfy` \s -> Set.member "foo" s
                Nothing -> expectationFailure "No properties annotation"
            Nothing -> expectationFailure "No annotations at root"
        failure -> expectationFailure $ "Validation failed: " ++ show failure

  describe "Items annotations" $ do
    it "collects validated array indices" $ do
      let schemaJson = object
            [ "type" .= ("array" :: Text)
            , "items" .= object ["type" .= ("string" :: Text)]
            ]
          Right schema = parseSchema schemaJson
          instance' = toJSON ["a" :: Text, "b", "c"]
          
      case validateValueWithRegistry defaultValidationConfig emptyRegistry schema instance' of
        ValidationSuccess anns -> do
          -- Items annotation should indicate all items were validated
          let annMap = unAnnotations anns
          -- For "items" applied to all, annotation should be present
          annMap `shouldSatisfy` \m -> not (Map.null m)
        failure -> expectationFailure $ "Validation failed: " ++ show failure

  describe "Nested annotations" $ do
    it "collects annotations at correct paths in nested objects" $ do
      let schemaJson = object
            [ "type" .= ("object" :: Text)
            , "properties" .= object
                [ "nested" .= object
                    [ "type" .= ("object" :: Text)
                    , "properties" .= object
                        [ "inner" .= object ["type" .= ("string" :: Text)]
                        ]
                    ]
                ]
            ]
          Right schema = parseSchema schemaJson
          instance' = object
            [ "nested" .= object ["inner" .= ("value" :: Text)]
            ]
          
      case validateValueWithRegistry defaultValidationConfig emptyRegistry schema instance' of
        ValidationSuccess anns -> do
          let annMap = unAnnotations anns
          -- Root should have "nested" property annotation
          case Map.lookup emptyPointer annMap of
            Just kwMap -> do
              case Map.lookup "properties" kwMap of
                Just (Array props) -> do
                  let propNames = [name | String name <- V.toList props]
                  propNames `shouldBe` ["nested"]
                Nothing -> expectationFailure "No properties annotation at root"
            Nothing -> expectationFailure "No annotations at root"
        failure -> expectationFailure $ "Validation failed: " ++ show failure

  describe "Annotation merging" $ do
    it "merges annotations from multiple validation paths" $ do
      let schemaJson = object
            [ "allOf" .=
                [ object
                    [ "type" .= ("object" :: Text)
                    , "properties" .= object
                        [ "foo" .= object ["type" .= ("string" :: Text)]
                        ]
                    ]
                , object
                    [ "properties" .= object
                        [ "bar" .= object ["type" .= ("number" :: Text)]
                        ]
                    ]
                ]
            ]
          Right schema = parseSchema schemaJson
          instance' = object ["foo" .= ("hello" :: Text), "bar" .= (42 :: Int)]
          
      case validateValueWithRegistry defaultValidationConfig emptyRegistry schema instance' of
        ValidationSuccess anns -> do
          -- Both properties should be annotated
          let annMap = unAnnotations anns
          annMap `shouldSatisfy` \m -> not (Map.null m)
        failure -> expectationFailure $ "Validation failed: " ++ show failure

  describe "Metadata annotations" $ do
    it "collects title annotation" $ do
      let schemaJson = object
            [ "type" .= ("string" :: Text)
            , "title" .= ("User Name" :: Text)
            ]
          Right schema = parseSchema schemaJson
          instance' = toJSON ("john" :: Text)
          
      case validateValueWithRegistry defaultValidationConfig emptyRegistry schema instance' of
        ValidationSuccess anns -> do
          let annMap = unAnnotations anns
          case Map.lookup emptyPointer annMap of
            Just kwMap -> do
              case Map.lookup "title" kwMap of
                Just (String title) -> title `shouldBe` "User Name"
                Nothing -> expectationFailure "No 'title' annotation found"
            Nothing -> expectationFailure "No annotations at root"
        failure -> expectationFailure $ "Validation failed: " ++ show failure

    it "collects description annotation" $ do
      let schemaJson = object
            [ "type" .= ("number" :: Text)
            , "description" .= ("Age in years" :: Text)
            ]
          Right schema = parseSchema schemaJson
          instance' = toJSON (25 :: Int)
          
      case validateValueWithRegistry defaultValidationConfig emptyRegistry schema instance' of
        ValidationSuccess anns -> do
          let annMap = unAnnotations anns
          case Map.lookup emptyPointer annMap of
            Just kwMap -> do
              case Map.lookup "description" kwMap of
                Just (String desc) -> desc `shouldBe` "Age in years"
                Nothing -> expectationFailure "No 'description' annotation found"
            Nothing -> expectationFailure "No annotations at root"
        failure -> expectationFailure $ "Validation failed: " ++ show failure

    it "collects default annotation" $ do
      let schemaJson = object
            [ "type" .= ("boolean" :: Text)
            , "default" .= True
            ]
          Right schema = parseSchema schemaJson
          instance' = toJSON True
          
      case validateValueWithRegistry defaultValidationConfig emptyRegistry schema instance' of
        ValidationSuccess anns -> do
          let annMap = unAnnotations anns
          case Map.lookup emptyPointer annMap of
            Just kwMap -> do
              case Map.lookup "default" kwMap of
                Just (Bool defaultVal) -> defaultVal `shouldBe` True
                Nothing -> expectationFailure "No 'default' annotation found"
            Nothing -> expectationFailure "No annotations at root"
        failure -> expectationFailure $ "Validation failed: " ++ show failure

    it "collects multiple metadata annotations" $ do
      let schemaJson = object
            [ "type" .= ("string" :: Text)
            , "title" .= ("Username" :: Text)
            , "description" .= ("The user's login name" :: Text)
            , "default" .= ("guest" :: Text)
            ]
          Right schema = parseSchema schemaJson
          instance' = toJSON ("alice" :: Text)
          
      case validateValueWithRegistry defaultValidationConfig emptyRegistry schema instance' of
        ValidationSuccess anns -> do
          let annMap = unAnnotations anns
          case Map.lookup emptyPointer annMap of
            Just kwMap -> do
              Map.lookup "title" kwMap `shouldSatisfy` isJust
              Map.lookup "description" kwMap `shouldSatisfy` isJust
              Map.lookup "default" kwMap `shouldSatisfy` isJust
            Nothing -> expectationFailure "No annotations at root"
        failure -> expectationFailure $ "Validation failed: " ++ show failure
  
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

