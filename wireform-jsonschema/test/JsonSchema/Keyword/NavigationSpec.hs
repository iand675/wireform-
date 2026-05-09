{-# LANGUAGE OverloadedStrings #-}

module JsonSchema.Keyword.NavigationSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import qualified Data.List.NonEmpty as NE

import JsonSchema
import JsonSchema.Types
import JsonSchema.Keyword.Types
import JsonSchema.Keyword
import JsonSchema.Parser (parseSchema)

spec :: Spec
spec = describe "Keyword Navigation" $ do
  
  describe "NoNavigation" $ do
    it "is the default for simple keywords" $ do
      -- NoNavigation is the default for keywords that don't contain subschemas
      -- Examples: type, enum, const, minimum, maximum, etc.
      -- These keywords should never be navigated into via JSON Pointer
      let schemaJson = object
            [ "type" .= ("string" :: String)
            , "minLength" .= (5 :: Int)
            , "maxLength" .= (10 :: Int)
            ]
      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " <> show err
        Right schema ->
          case schemaCore schema of
            ObjectSchema obj -> do
              -- These keywords exist but don't contain subschemas
              schemaType obj `shouldBe` Just (One StringType)
              validationMinLength (schemaValidation obj) `shouldBe` Just 5
              validationMaxLength (schemaValidation obj) `shouldBe` Just 10
            _ -> expectationFailure "Expected ObjectSchema"
  
  describe "SingleSchema navigation" $ do
    it "navigates into 'not' keyword" $ do
      let schemaJson = object
            [ "not" .= object ["type" .= ("string" :: String)]
            ]
      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " <> show err
        Right schema ->
          case schemaCore schema of
            ObjectSchema obj ->
              case schemaNot obj of
                Just notSchema -> do
                  -- Should be able to extract the not schema
                  case schemaCore notSchema of
                    ObjectSchema notObj ->
                      schemaType notObj `shouldBe` Just (One StringType)
                    _ -> expectationFailure "Expected ObjectSchema in not"
                Nothing -> expectationFailure "Expected not keyword"
            _ -> expectationFailure "Expected ObjectSchema"
    
    it "navigates into 'contains' keyword" $ do
      let schemaJson = object
            [ "contains" .= object ["type" .= ("number" :: String)]
            ]
      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " <> show err
        Right schema ->
          case schemaCore schema of
            ObjectSchema obj ->
              case validationContains (schemaValidation obj) of
                Just containsSchema ->
                  case schemaCore containsSchema of
                    ObjectSchema containsObj ->
                      schemaType containsObj `shouldBe` Just (One NumberType)
                    _ -> expectationFailure "Expected ObjectSchema in contains"
                Nothing -> expectationFailure "Expected contains keyword"
            _ -> expectationFailure "Expected ObjectSchema"
  
  describe "SchemaMap navigation" $ do
    it "navigates into 'properties' with key lookup" $ do
      let schemaJson = object
            [ "properties" .= object
                [ "name" .= object ["type" .= ("string" :: String)]
                , "age" .= object ["type" .= ("integer" :: String)]
                ]
            ]
      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " <> show err
        Right schema ->
          case schemaCore schema of
            ObjectSchema obj ->
              case validationProperties (schemaValidation obj) of
                Just props -> do
                  -- Should have 2 properties
                  Map.size props `shouldBe` 2
                  
                  -- Should be able to lookup by key
                  case Map.lookup "name" props of
                    Just nameSchema ->
                      case schemaCore nameSchema of
                        ObjectSchema nameObj ->
                          schemaType nameObj `shouldBe` Just (One StringType)
                        _ -> expectationFailure "Expected ObjectSchema for name"
                    Nothing -> expectationFailure "Expected 'name' property"
                  
                  case Map.lookup "age" props of
                    Just ageSchema ->
                      case schemaCore ageSchema of
                        ObjectSchema ageObj ->
                          schemaType ageObj `shouldBe` Just (One IntegerType)
                        _ -> expectationFailure "Expected ObjectSchema for age"
                    Nothing -> expectationFailure "Expected 'age' property"
                
                Nothing -> expectationFailure "Expected properties keyword"
            _ -> expectationFailure "Expected ObjectSchema"
    
    it "navigates into 'dependentSchemas' with key lookup" $ do
      let schemaJson = object
            [ "dependentSchemas" .= object
                [ "foo" .= object ["required" .= (["bar"] :: [String])]
                ]
            ]
      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " <> show err
        Right schema ->
          case schemaCore schema of
            ObjectSchema obj ->
              case validationDependentSchemas (schemaValidation obj) of
                Just depSchemas -> do
                  Map.size depSchemas `shouldBe` 1
                  isJust (Map.lookup "foo" depSchemas) `shouldBe` True
                Nothing -> expectationFailure "Expected dependentSchemas keyword"
            _ -> expectationFailure "Expected ObjectSchema"
  
  describe "SchemaArray navigation" $ do
    it "navigates into 'allOf' with numeric index" $ do
      let schemaJson = object
            [ "allOf" .=
                [ object ["type" .= ("string" :: String)]
                , object ["minLength" .= (5 :: Int)]
                ]
            ]
      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " <> show err
        Right schema ->
          case schemaCore schema of
            ObjectSchema obj ->
              case schemaAllOf obj of
                Just allOfSchemas -> do
                  let schemaList = NE.toList allOfSchemas
                  length schemaList `shouldBe` 2
                  
                  -- First schema should have type
                  case schemaCore (schemaList !! 0) of
                    ObjectSchema s1 ->
                      schemaType s1 `shouldBe` Just (One StringType)
                    _ -> expectationFailure "Expected ObjectSchema at index 0"
                  
                  -- Second schema should have minLength
                  case schemaCore (schemaList !! 1) of
                    ObjectSchema s2 ->
                      isJust (validationMinLength (schemaValidation s2)) `shouldBe` True
                    _ -> expectationFailure "Expected ObjectSchema at index 1"
                
                Nothing -> expectationFailure "Expected allOf keyword"
            _ -> expectationFailure "Expected ObjectSchema"
    
    it "navigates into 'anyOf' with numeric index" $ do
      let schemaJson = object
            [ "anyOf" .=
                [ object ["type" .= ("string" :: String)]
                , object ["type" .= ("number" :: String)]
                ]
            ]
      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " <> show err
        Right schema ->
          case schemaCore schema of
            ObjectSchema obj ->
              case schemaAnyOf obj of
                Just anyOfSchemas -> do
                  length (NE.toList anyOfSchemas) `shouldBe` 2
                Nothing -> expectationFailure "Expected anyOf keyword"
            _ -> expectationFailure "Expected ObjectSchema"
    
    it "navigates into 'oneOf' with numeric index" $ do
      let schemaJson = object
            [ "oneOf" .=
                [ object ["type" .= ("string" :: String)]
                , object ["type" .= ("integer" :: String)]
                ]
            ]
      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " <> show err
        Right schema ->
          case schemaCore schema of
            ObjectSchema obj ->
              case schemaOneOf obj of
                Just oneOfSchemas -> do
                  length (NE.toList oneOfSchemas) `shouldBe` 2
                Nothing -> expectationFailure "Expected oneOf keyword"
            _ -> expectationFailure "Expected ObjectSchema"
    
    it "navigates into 'prefixItems' with numeric index" $ do
      let schemaJson = object
            [ "prefixItems" .=
                [ object ["type" .= ("string" :: String)]
                , object ["type" .= ("number" :: String)]
                ]
            ]
      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " <> show err
        Right schema ->
          case schemaCore schema of
            ObjectSchema obj ->
              case validationPrefixItems (schemaValidation obj) of
                Just prefixSchemas -> do
                  length (NE.toList prefixSchemas) `shouldBe` 2
                Nothing -> expectationFailure "Expected prefixItems keyword"
            _ -> expectationFailure "Expected ObjectSchema"
  
  describe "CustomNavigation for items keyword" $ do
    it "navigates into single items schema" $ do
      let schemaJson = object
            [ "items" .= object ["type" .= ("string" :: String)]
            ]
      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " <> show err
        Right schema ->
          case schemaCore schema of
            ObjectSchema obj ->
              case validationItems (schemaValidation obj) of
                Just (ItemsSchema itemsSchema) ->
                  case schemaCore itemsSchema of
                    ObjectSchema itemsObj ->
                      schemaType itemsObj `shouldBe` Just (One StringType)
                    _ -> expectationFailure "Expected ObjectSchema in items"
                _ -> expectationFailure "Expected single items schema"
            _ -> expectationFailure "Expected ObjectSchema"
    
    it "navigates into tuple items with index" $ do
      let schemaJson = object
            [ "items" .=
                [ object ["type" .= ("string" :: String)]
                , object ["type" .= ("number" :: String)]
                ]
            ]
      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Parse failed: " <> show err
        Right schema ->
          case schemaCore schema of
            ObjectSchema obj ->
              case validationItems (schemaValidation obj) of
                Just (ItemsTuple tupleSchemas _) -> do
                  length (NE.toList tupleSchemas) `shouldBe` 2
                _ -> expectationFailure "Expected tuple items"
            _ -> expectationFailure "Expected ObjectSchema"

