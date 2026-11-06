module Test.XML.FastEntities (tests) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Vector qualified as V
import Test.Syd
import XML.FastDOM
import XML.Value


tests :: Spec
tests = describe "FastDOM internal entities" $ do
  it "expands nested general, predefined, and numeric references in text and attributes" $ do
    rootOf "<!DOCTYPE r [<!ENTITY a 'hello'><!ENTITY b '&a; &amp; &#x1F600;'>]><r x='&b;'>before &b; after</r>"
      `shouldBe` Right (Element (simpleName "r") (V.singleton (Attribute (simpleName "x") "hello & \x1F600")) (V.singleton (Text "before hello & \x1F600 after")))
  it "honours first declaration, comments, and brackets inside quoted values" $ do
    rootOf "<!DOCTYPE r [<!-- ] > --><!ENTITY a '] >'><!ENTITY a 'wrong'>]><r>&a;</r>"
      `shouldBe` Right (elementText "] >")
  it "leaves CDATA and comment contents literal" $ do
    rootOf "<!DOCTYPE r [<!ENTITY a 'hello'>]><r><![CDATA[&a;]]><!--&a;-->&a;</r>"
      `shouldBe` Right (Element (simpleName "r") V.empty (V.fromList [CData "&a;", Comment "&a;", Text "hello"]))
  it "retains source spans and the no-DTD constructor" $ do
    let input = "<!DOCTYPE r [<!ENTITY a 'hello'>]><r>&a;</r>"
    case parseFast input of
      Left err -> expectationFailure err
      Right doc -> do
        fdSource doc `shouldBe` input
        case fdRoot doc of
          FElement _ _ children -> case V.toList children of
            [FText (Span offset size)] -> BS.take size (BS.drop offset input) `shouldBe` "&a;"
            _ -> expectationFailure "expected raw text span"
          _ -> expectationFailure "expected root"
    rootOf "<r>x &amp; y</r>" `shouldBe` Right (elementText "x & y")
  it "rejects cycles, undeclared refs, unsupported external/parameter/markup entities and invalid scalars" $ do
    mapM_
      (\input -> isLeft (parseFast input) `shouldBe` True)
      [ "<!DOCTYPE r [<!ENTITY a '&b;'><!ENTITY b '&a;'>]><r>&a;</r>"
      , "<!DOCTYPE r [<!ENTITY a '&missing;'>]><r/>"
      , "<!DOCTYPE r [<!ENTITY a 'x'>]><r>&missing;</r>"
      , "<!DOCTYPE r [<!ENTITY a SYSTEM 'file:///etc/passwd'>]><r>&a;</r>"
      , "<!DOCTYPE r [<!ENTITY % a 'x'>]><r/>"
      , "<!DOCTYPE r [<!ENTITY a '<b/>'>]><r>&a;</r>"
      , "<!DOCTYPE r [<!ENTITY a '&#xD800;'>]><r>&a;</r>"
      , "<!DOCTYPE r [<!ENTITY a '&#18446744073709551616;'>]><r>&a;</r>"
      , "<!DOCTYPE r [<!ENTITY a '&#0;'>]><r>&a;</r>"
      ]
  it "bounds exponential expansion before materializing it" $ do
    let declaration i = "<!ENTITY e" <> BS.pack (show i) <> " '" <> (if i == 0 then "x" else BS.concat (replicate 10 ("&e" <> BS.pack (show (i - 1)) <> ";"))) <> "'>"
        input = "<!DOCTYPE r [" <> BS.concat (map declaration (enumFromTo (0 :: Int) 8)) <> "]><r>&e8;</r>"
    isLeft (parseFast input) `shouldBe` True
  it "does not fetch an external subset" $ do
    rootOf "<!DOCTYPE r SYSTEM 'https://example.invalid/never-fetch'><r>ok</r>" `shouldBe` Right (elementText "ok")

  it "distinguishes declaration-time numeric references and inclusion-time general references" $ do
    let check value attribute text =
          rootOf ("<!DOCTYPE r [<!ENTITY a '" <> value <> "'>]><r x='&a;'>&a;</r>")
            `shouldBe` Right (Element (simpleName "r") (V.singleton (Attribute (simpleName "x") attribute)) (V.singleton (Text text)))
    check "&#38;lt;" "<" "<"
    check "&amp;lt;" "&lt;" "&lt;"
    check "&#10;" " " "\n"
    check "&#38;#10;" "\n" "\n"
    check "&amp;#10;" "&#10;" "&#10;"
    rootOf "<!DOCTYPE r [<!ENTITY a 'x\r\ny'>]><r x='&a;'>&a;</r>"
      `shouldBe` Right (Element (simpleName "r") (V.singleton (Attribute (simpleName "x") "x y")) (V.singleton (Text "x\ny")))
    isLeft (parseFast "<!DOCTYPE r [<!ENTITY a '&#60;b/>'>]><r>&a;</r>") `shouldBe` True


rootOf :: ByteString -> Either String Node
rootOf input = docRoot . toDocument <$> parseFast input


elementText :: Text -> Node
elementText value = Element (simpleName "r") V.empty (V.singleton (Text value))
