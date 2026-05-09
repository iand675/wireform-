module JsonSchema.EmbeddedMetaschemasSpec (spec) where

import Test.Hspec
import JsonSchema.EmbeddedMetaschemas
import JsonSchema.EmbeddedMetaschemas.Raw (embeddedMetaschemaValues)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

spec :: Spec
spec = describe "Embedded Metaschemas" $ do
  it "has embedded files" $ do
    let count = Map.size embeddedMetaschemaValues
    putStrLn $ "\nNumber of embedded metaschemas: " ++ show count
    putStrLn "Embedded metaschema URIs:"
    mapM_ (putStrLn . ("  - " ++) . T.unpack) (Map.keys embeddedMetaschemaValues)
    embeddedMetaschemaValues `shouldSatisfy` (not . Map.null)

  it "embeds at least one metaschema" $ do
    embeddedMetaschemaURIs `shouldSatisfy` (not . null)

  it "prints all embedded URIs for debugging" $ do
    putStrLn "\nEmbedded metaschema URIs:"
    mapM_ (putStrLn . ("  - " ++) . T.unpack) embeddedMetaschemaURIs
    True `shouldBe` True

  it "can look up 2020-12 core metaschema" $ do
    let uri = "https://json-schema.org/draft/2020-12/meta/core"
    lookupEmbeddedMetaschema uri `shouldSatisfy` maybe False (const True)

  it "can look up 2020-12 applicator metaschema" $ do
    let uri = "https://json-schema.org/draft/2020-12/meta/applicator"
    lookupEmbeddedMetaschema uri `shouldSatisfy` maybe False (const True)

  it "can look up 2019-09 core metaschema" $ do
    let uri = "https://json-schema.org/draft/2019-09/meta/core"
    lookupEmbeddedMetaschema uri `shouldSatisfy` maybe False (const True)

  it "can look up 2019-09 applicator metaschema" $ do
    let uri = "https://json-schema.org/draft/2019-09/meta/applicator"
    lookupEmbeddedMetaschema uri `shouldSatisfy` maybe False (const True)
