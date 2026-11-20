module Test.HTML.Entities (tests) where

import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Primitive.SmallArray (indexSmallArray, sizeofSmallArray)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import HTML.Parse (parseEntityRef, parseHTML)
import HTML.Value
import Paths_wireform_html (getDataFileName)
import Test.Syd


newtype Entity = Entity Text


instance A.FromJSON Entity where
  parseJSON = A.withObject "entity" (\o -> Entity <$> o A..: "characters")


tests :: Spec
tests = describe "WHATWG character references" $ do
  it "resolves every canonical and legacy entry from the pinned official table" $ do
    path <- getDataFileName "data/entities.json"
    bytes <- BS.readFile path
    entries <- either fail pure (A.eitherDecodeStrict' bytes :: Either String (Map.Map Text Entity))
    Map.size entries `shouldBe` 2231
    mapM_ (\(name, Entity chars) -> parseEntityRef (T.unpack (T.drop 1 name) <> "!") `shouldBe` (T.unpack chars, "!")) (Map.toList entries)
    -- Exercise the byte tokenizer and attribute context as well as the helper.
    let canonical = filter (T.isSuffixOf ";" . fst) (Map.toList entries)
        input = T.concat (map (\(name, _) -> "<p data-x='" <> name <> "'>" <> name <> "</p>") canonical)
        document = parseHTML (TE.encodeUtf8 input)
        expected = map (\(_, Entity chars) -> chars) canonical
    textContent (htmlRoot document) `shouldBe` T.concat expected
    attributes (htmlRoot document) `shouldBe` expected
  it "uses longest match and preserves the unmatched suffix" $ do
    parseEntityRef "notin;!" `shouldBe` ("\x2209", "!")
    parseEntityRef "notit;" `shouldBe` ("\xac", "it;")
    parseEntityRef "bogus;" `shouldBe` ("&", "bogus;")
    parseEntityRef "CounterClockwiseContourIntegral;" `shouldBe` ("\x2233", "")
  it "blocks legacy matches followed by ASCII alphanumeric or equals in attributes" $ do
    let document = parseHTML "<p data-x='&notit; &copy= &copy! &NotEqualTilde;'>"
    attributes (htmlRoot document) `shouldBe` ["&notit; &copy= \xa9! \x2242\x338"]
  it "keeps numeric replacement rules" $ do
    parseEntityRef "#x80;" `shouldBe` ("\x20ac", "")
    parseEntityRef "#0;" `shouldBe` ("\xfffd", "")
    parseEntityRef "#xD800;" `shouldBe` ("\xfffd", "")


attributes :: HTMLNode -> [Text]
attributes node@(HTMLElement _ _ children) = maybe [] pure (getAttr "data-x" node) <> concatMap (attributes . indexSmallArray children) (enumFromTo 0 (sizeofSmallArray children - 1))
attributes _ = []
