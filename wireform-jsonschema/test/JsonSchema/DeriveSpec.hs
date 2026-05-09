{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

-- | End-to-end coverage for the annotation-driven JSON-Schema TH
-- deriver. The assertions here use the runtime validator rather than
-- the JSON renderer because the renderer is still a stub for many
-- keywords (see @JsonSchema.Renderer@) — once that is fleshed out we
-- can also assert against 'jsonSchemaJSON'.
module JsonSchema.DeriveSpec (spec) where

import Test.Hspec
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import JsonSchema.Class (HasJsonSchema (..), validateAgainstSchema)
import JsonSchema.Derive (deriveHasJsonSchema)
import JsonSchema.Types
  ( ObjectSchemaData (..)
  , OneOrMany (..)
  , Schema (..)
  , SchemaCore (..)
  , SchemaObject (..)
  , SchemaType (..)
  , SchemaValidation (..)
  , isFailure
  , isSuccess
  )

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

data Person = Person
  { personName :: !Text
  , personAge  :: !(Maybe Int)
  } deriving (Eq, Show, Generic)

deriveHasJsonSchema ''Person

data Color = Red | Green | Blue
  deriving (Eq, Show, Generic)

deriveHasJsonSchema ''Color

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "deriveHasJsonSchema (record)" $ do
    let schema = toJsonSchema (Proxy @Person)

    it "produces an object schema" $
      coreType schema `shouldBe` Just (One ObjectType)

    it "lists each record field under properties" $ do
      fmap Map.keys (coreProperties schema)
        `shouldBe` Just ["personAge", "personName"]

    it "marks non-Maybe fields as required" $
      coreRequired schema `shouldBe` Just (Set.fromList ["personName"])

    it "validates a well-formed instance" $ do
      let v = Aeson.object
            [ Key.fromString "personName" Aeson..= ("Alice" :: Text)
            , Key.fromString "personAge"  Aeson..= (30 :: Int)
            ]
      isSuccess (validateAgainstSchema (Proxy @Person) v)
        `shouldBe` True

    it "rejects an instance missing the required field" $ do
      let v = Aeson.object
            [ Key.fromString "personAge" Aeson..= (30 :: Int) ]
      isFailure (validateAgainstSchema (Proxy @Person) v)
        `shouldBe` True

  describe "deriveHasJsonSchema (enum)" $ do
    let schema = toJsonSchema (Proxy @Color)

    it "is encoded as a string-typed schema" $
      coreType schema `shouldBe` Just (One StringType)

    it "lists every constructor under enum" $
      fmap NE.toList (coreEnum schema)
        `shouldBe`
          Just [Aeson.String "Red", Aeson.String "Green", Aeson.String "Blue"]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

withObjectData :: Schema -> (ObjectSchemaData -> Maybe a) -> Maybe a
withObjectData s f = case schemaCore s of
  ObjectSchema (SchemaObject (Right o)) -> f o
  _ -> Nothing

coreType :: Schema -> Maybe (OneOrMany SchemaType)
coreType s = withObjectData s objectSchemaDataType

coreEnum :: Schema -> Maybe (NonEmpty Aeson.Value)
coreEnum s = withObjectData s objectSchemaDataEnum

coreProperties :: Schema -> Maybe (Map.Map Text ())
coreProperties s = withObjectData s $ \o ->
  fmap (fmap (const ())) (validationProperties (objectSchemaDataValidation o))

coreRequired :: Schema -> Maybe (Set.Set Text)
coreRequired s = withObjectData s $ \o ->
  validationRequired (objectSchemaDataValidation o)
