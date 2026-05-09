{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Walk-through of the two codegen surfaces in @wireform-jsonschema@:
--
-- * 'JsonSchema.CodeGen' — schema-driven code generation. You feed it
--   a 'JsonSchema.Types.Schema' (parsed from a JSON document, say)
--   and get back a Haskell module shell as 'Text'.
--
-- * 'JsonSchema.Derive' — annotation-driven Template Haskell deriver.
--   You declare the Haskell type and 'deriveHasJsonSchema' synthesises
--   a 'HasJsonSchema' instance, which 'jsonSchemaJSON' then renders
--   back to a JSON Schema document.
--
-- Run this with:
--
-- > cabal --project-file=cabal.project.jsonschema run \\
-- >   wireform-jsonschema-codegen-demo
module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonP
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

import JsonSchema (parseSchema)
import JsonSchema.Class (HasJsonSchema, jsonSchemaJSON)
import JsonSchema.CodeGen
  ( generateJsonSchemaTypesWithName
  , renderGeneratedModule
  )
import JsonSchema.Derive (deriveHasJsonSchema)

-- ---------------------------------------------------------------------------
-- Example 1 — schema-driven codegen on a record schema
-- ---------------------------------------------------------------------------

personSchemaJSON :: Aeson.Value
personSchemaJSON = either error id $ Aeson.eitherDecode $ BL.pack
  "{ \"$schema\": \"http://json-schema.org/draft-07/schema#\"\n\
  \, \"title\": \"Person\"\n\
  \, \"type\": \"object\"\n\
  \, \"required\": [\"name\"]\n\
  \, \"properties\":\n\
  \    { \"name\":  { \"type\": \"string\"  }\n\
  \    , \"age\":   { \"type\": \"integer\" }\n\
  \    , \"email\": { \"type\": \"string\", \"format\": \"email\" }\n\
  \    }\n\
  \}"

-- ---------------------------------------------------------------------------
-- Example 2 — schema-driven codegen on an enum schema
-- ---------------------------------------------------------------------------

colorSchemaJSON :: Aeson.Value
colorSchemaJSON = either error id $ Aeson.eitherDecode $ BL.pack
  "{ \"$schema\": \"http://json-schema.org/draft-07/schema#\"\n\
  \, \"title\":   \"Color\"\n\
  \, \"type\":    \"string\"\n\
  \, \"enum\":    [\"red\", \"green\", \"blue\"]\n\
  \}"

-- ---------------------------------------------------------------------------
-- Example 3 — annotation-driven deriver on Haskell records / sums
-- ---------------------------------------------------------------------------

data Address = Address
  { addressStreet  :: !Text
  , addressCity    :: !Text
  , addressZipCode :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

data Customer = Customer
  { customerId      :: !Int
  , customerName    :: !Text
  , customerAddress :: !Address
  , customerTags    :: ![Text]
  } deriving (Eq, Show, Generic)

data Status
  = StatusActive
  | StatusSuspended
  | StatusClosed
  deriving (Eq, Show, Generic)

deriveHasJsonSchema ''Address
deriveHasJsonSchema ''Customer
deriveHasJsonSchema ''Status

-- ---------------------------------------------------------------------------
-- Driver
-- ---------------------------------------------------------------------------

prettyJSON :: Aeson.ToJSON a => a -> Text
prettyJSON = T.pack . BL.unpack . AesonP.encodePretty

heading :: Text -> IO ()
heading t = do
  T.putStrLn ""
  T.putStrLn (T.replicate 72 "=")
  T.putStrLn t
  T.putStrLn (T.replicate 72 "=")

subHeading :: Text -> IO ()
subHeading t = do
  T.putStrLn ""
  T.putStrLn (T.replicate 60 "-")
  T.putStrLn t
  T.putStrLn (T.replicate 60 "-")

runSchemaCodegen :: Text -> Aeson.Value -> IO ()
runSchemaCodegen rootName json = do
  subHeading ("Input schema (" <> rootName <> ")")
  T.putStrLn (prettyJSON json)
  case parseSchema json of
    Left err ->
      T.putStrLn $ "parseSchema failed: " <> T.pack (show err)
    Right s -> do
      subHeading ("Generated Haskell (" <> rootName <> ")")
      T.putStrLn (renderGeneratedModule
                    (generateJsonSchemaTypesWithName rootName s))

runDerive
  :: forall a. HasJsonSchema a
  => Proxy a
  -> Text
  -> IO ()
runDerive _ name = do
  subHeading (name <> " :: jsonSchemaJSON")
  T.putStrLn (prettyJSON (jsonSchemaJSON @a))

main :: IO ()
main = do
  heading "JsonSchema.CodeGen — schema-driven generation"
  runSchemaCodegen "Person" personSchemaJSON
  runSchemaCodegen "Color"  colorSchemaJSON

  heading "JsonSchema.Derive — annotation-driven deriver"
  runDerive (Proxy :: Proxy Address)  "Address"
  runDerive (Proxy :: Proxy Customer) "Customer"
  runDerive (Proxy :: Proxy Status)   "Status"
