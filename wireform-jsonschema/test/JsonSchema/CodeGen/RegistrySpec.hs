{-# LANGUAGE OverloadedStrings #-}

-- | Coverage for the 'JsonSchema.CodeGen.Registry' matcher algebra
-- and the codegen integration.
module JsonSchema.CodeGen.RegistrySpec (spec) where

import Test.Hspec
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T

import JsonSchema (parseSchema)
import JsonSchema.CodeGen
  ( generateJsonSchemaTypesWith
  , renderGeneratedModule
  )
import JsonSchema.CodeGen.Registry

import JsonSchema.Types (Schema, SchemaType (..))

spec :: Spec
spec = do
  describe "Matcher" $ do
    describe "matchFormat" $ do
      it "matches when the format keyword is the registered value" $
        runMatcher (matchFormat "uuid") (parse formatUuid) `shouldBe` True
      it "does not match a different format" $
        runMatcher (matchFormat "uuid") (parse formatDateTime) `shouldBe` False
      it "does not match when the schema has no format" $
        runMatcher (matchFormat "uuid") (parse plainString) `shouldBe` False

    describe "matchPattern" $ do
      it "matches the pattern verbatim" $
        runMatcher (matchPattern "^[0-9]+$") (parse pat) `shouldBe` True
      it "does not match a different pattern string" $
        runMatcher (matchPattern "^[a-z]+$") (parse pat) `shouldBe` False

    describe "matchPatternRegex" $ do
      it "matches when the schema's pattern regex-matches the given regex" $
        runMatcher (matchPatternRegex "^\\^\\[0-9\\]")
                   (parse pat) `shouldBe` True
      it "does not match when the schema has no pattern" $
        runMatcher (matchPatternRegex ".") (parse plainString) `shouldBe` False

    describe "matchRef" $ do
      it "matches the registered $ref URI" $
        runMatcher (matchRef "https://acme.example/schemas/User.json")
                   (parse refSchema) `shouldBe` True
      it "does not match a different URI" $
        runMatcher (matchRef "https://acme.example/schemas/Other.json")
                   (parse refSchema) `shouldBe` False

    describe "matchType" $ do
      it "matches a single declared type" $
        runMatcher (matchType StringType) (parse plainString) `shouldBe` True
      it "matches a type contained in a union" $
        runMatcher (matchType StringType) (parse unionStringNull) `shouldBe` True
      it "does not match a different declared type" $
        runMatcher (matchType IntegerType) (parse plainString) `shouldBe` False

    describe "matchHasProperty" $ do
      it "matches when the property is declared" $
        runMatcher (matchHasProperty "tag" Nothing) (parse taggedObject)
          `shouldBe` True
      it "matches when the property's subschema satisfies the inner matcher" $
        runMatcher (matchHasProperty "tag" (Just (matchType StringType)))
                   (parse taggedObject) `shouldBe` True

    describe "composition" $ do
      it "matchAll requires every sub-matcher to fire" $
        runMatcher (matchAll [matchType StringType, matchFormat "uuid"])
                   (parse formatUuid) `shouldBe` True
      it "matchAll fails if any sub-matcher fails" $
        runMatcher (matchAll [matchType StringType, matchFormat "date-time"])
                   (parse formatUuid) `shouldBe` False
      it "matchAny requires at least one sub-matcher" $
        runMatcher (matchAny [matchFormat "uuid", matchFormat "uri"])
                   (parse formatUuid) `shouldBe` True
      it "matchNot inverts the inner matcher" $
        runMatcher (matchNot (matchFormat "uri"))
                   (parse formatUuid) `shouldBe` True

  describe "MappingRegistry / lookupMapping" $ do
    let reg =
            register (matchFormat "uuid")
                     (typeMapping "UUID" ["Data.UUID (UUID)"])
          $ register (matchRef "https://acme.example/schemas/User.json")
                     (typeMapping "User" ["Acme.User (User)"])
          $ register matchAnything
                     (typeMapping "Aeson.Value" [])
          $ emptyMappingRegistry

    it "returns the format mapping" $ do
      tmHaskellType <$> lookupMapping reg (parse formatUuid)
        `shouldBe` Just "UUID"

    it "returns the $ref mapping" $ do
      tmHaskellType <$> lookupMapping reg (parse refSchema)
        `shouldBe` Just "User"

    it "falls through to the catch-all" $ do
      tmHaskellType <$> lookupMapping reg (parse plainString)
        `shouldBe` Just "Aeson.Value"

  describe "generateJsonSchemaTypesWith" $ do
    let reg =
            register (matchFormat "uuid")
                     (typeMapping "UUID" ["Data.UUID (UUID)"])
          $ register (matchFormat "date-time")
                     (typeMapping "UTCTime" ["Data.Time (UTCTime)"])
          $ emptyMappingRegistry
        s = parse personLikeSchema
        out = renderGeneratedModule
                (generateJsonSchemaTypesWith reg "Person" s)

    it "uses the registered Haskell type for the matched field" $ do
      out `shouldSatisfy` ("personId :: Maybe UUID"          `T.isInfixOf`)
      out `shouldSatisfy` ("personCreatedAt :: Maybe UTCTime"`T.isInfixOf`)

    it "emits an import for each registered type that fires" $ do
      out `shouldSatisfy` ("import Data.UUID (UUID)" `T.isInfixOf`)
      out `shouldSatisfy` ("import Data.Time (UTCTime)" `T.isInfixOf`)

    it "does not emit imports for registered types that did not fire" $ do
      out `shouldNotSatisfy`
        ("import Data.Time.Calendar (Day)" `T.isInfixOf`)

-- ---------------------------------------------------------------------------
-- Fixture schemas
-- ---------------------------------------------------------------------------

parse :: Aeson.Value -> Schema
parse v = fromJust $ either (const Nothing) Just (parseSchema v)

formatUuid, formatDateTime, plainString, pat, refSchema,
  unionStringNull, taggedObject, personLikeSchema :: Aeson.Value

formatUuid = Aeson.object
  [ "type"   Aeson..= ("string" :: Text)
  , "format" Aeson..= ("uuid" :: Text)
  ]

formatDateTime = Aeson.object
  [ "type"   Aeson..= ("string" :: Text)
  , "format" Aeson..= ("date-time" :: Text)
  ]

plainString = Aeson.object [ "type" Aeson..= ("string" :: Text) ]

pat = Aeson.object
  [ "type"    Aeson..= ("string" :: Text)
  , "pattern" Aeson..= ("^[0-9]+$" :: Text)
  ]

refSchema = Aeson.object
  [ "$ref" Aeson..= ("https://acme.example/schemas/User.json" :: Text) ]

unionStringNull = Aeson.object
  [ "type" Aeson..= (["string", "null"] :: [Text]) ]

taggedObject = Aeson.object
  [ "type" Aeson..= ("object" :: Text)
  , "properties" Aeson..= Aeson.object
      [ "tag" Aeson..= Aeson.object
          [ "type" Aeson..= ("string" :: Text) ]
      ]
  ]

personLikeSchema = Aeson.object
  [ "type" Aeson..= ("object" :: Text)
  , "required" Aeson..= (["name"] :: [Text])
  , "properties" Aeson..= Aeson.object
      [ "name" Aeson..= Aeson.object
          [ "type" Aeson..= ("string" :: Text) ]
      , "id" Aeson..= Aeson.object
          [ "type"   Aeson..= ("string" :: Text)
          , "format" Aeson..= ("uuid" :: Text)
          ]
      , "createdAt" Aeson..= Aeson.object
          [ "type"   Aeson..= ("string" :: Text)
          , "format" Aeson..= ("date-time" :: Text)
          ]
      ]
  ]
