{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.YAML.Regression (tests) where

import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Int (Int64, Int8)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word64, Word8)
import Hedgehog (forAll, property, tripping)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Numeric.Natural (Natural)
import Test.Syd
import Test.Syd.Hedgehog ()
import YAML.Annotated qualified as AN
import YAML.Class qualified as C
import YAML.Decode qualified as D
import YAML.Decode.Annotated qualified as AD
import YAML.Encode qualified as E
import YAML.JSON qualified as J
import YAML.Pretty qualified as P
import YAML.Value


tests :: Spec
tests = describe "regressions" $ do
  it "decodes escapes after Unicode characters and Unicode flow siblings" $
    mapM_
      (\(src, expected) -> D.decode src `shouldBe` Right expected)
      [ ("\"é\\n\"", YString "é\n")
      , ("[é, \"😀\\t\\u0061\"]", seq_ [YString "é", YString "😀\ta"])
      , ("\"é\\x41\\U0001F600\"", YString "éA😀")
      ]
  it "rejects invalid Unicode scalar escapes through Either" $
    mapM_
      (\src -> D.decode src `shouldSatisfy` isLeft)
      ["\"\\U00110000\"", "\"\\UFFFFFFFF\"", "\"\\uD800\"", "[\"\\uDFFF\"]"]
  it "does not mistake quoted strings for aliases" $ do
    D.decode "[\"\\0alias\\0missing\"]" `shouldBe` Right (seq_ [YString "\0alias\0missing"])
    case D.decode "[&a secret, \"\\0alias\\0a\"]" of
      Right (YSeq xs) -> xs V.! 1 `shouldBe` YString "\0alias\0a"
      other -> expectationFailure (show other)
  it "bounds both flow and mixed block/flow nesting" $ do
    let nested = T.replicate 2000 "[" <> "0" <> T.replicate 2000 "]"
    D.decode nested `shouldSatisfy` isLeft
    D.decode ("items: " <> nested) `shouldSatisfy` isLeft
    let block = T.unlines ((\i -> T.replicate (2 * i) " " <> "a:") <$> [0 .. 899])
    D.decode (block <> T.replicate 1800 " " <> T.replicate 200 "[" <> "0" <> T.replicate 200 "]") `shouldSatisfy` isLeft
    D.decode (T.replicate 200 "[" <> "0" <> T.replicate 200 "]") `shouldSatisfy` either (const False) (const True)
  it "preserves arbitrary precision integers in parsing, typed codecs, and JSON" $ do
    let n = 18446744073709551616
    D.decode "18446744073709551616" `shouldBe` Right (YInt n)
    D.decode "-9223372036854775809" `shouldBe` Right (YInt (-9223372036854775809))
    C.decodeYAML (C.encodeYAML n) `shouldBe` Right (n :: Integer)
    C.decodeYAML (C.encodeYAML (maxBound :: Word64)) `shouldBe` Right (maxBound :: Word64)
    C.decodeYAML (C.encodeYAML (fromInteger n :: Natural)) `shouldBe` Right (fromInteger n :: Natural)
    J.jsonToYAML (A.Number (fromInteger n)) `shouldBe` YInt n
  it "rejects bounded overflow, negative unsigned values, and fractional integers" $ do
    (C.decodeYAML "256" :: Either String Word8) `shouldSatisfy` isLeft
    (C.decodeYAML "-1" :: Either String Word8) `shouldSatisfy` isLeft
    (C.decodeYAML "128" :: Either String Int8) `shouldSatisfy` isLeft
    (C.decodeYAML "9223372036854775808" :: Either String Int64) `shouldSatisfy` isLeft
    (C.decodeYAML "1.9" :: Either String Int) `shouldSatisfy` isLeft
    (C.decodeYAML ".inf" :: Either String Int) `shouldSatisfy` isLeft
  it "round-trips large integers across signs" $ property $ do
    n <- forAll (Gen.integral (Range.linear (negate (2 ^ (256 :: Int))) (2 ^ (256 :: Int))))
    tripping (YInt n) E.encode D.decode
  it "quotes structural string boundaries" $
    mapM_
      (\str -> D.decode (E.encode (YString str)) `shouldBe` Right (YString str))
      ["foo:", "a:\tvalue", ":", "---", "...", " #", "é\n", "\0alias\0x"]
  it "frames constructed streams without merging documents" $ do
    let docs = V.fromList [Document False False (YInt 1), Document False False (YInt 2)]
    fmap (V.map docBody . unStream) (D.decodeStream (E.encodeStream (Stream docs))) `shouldBe` Right (V.fromList [YInt 1, YInt 2])
  it "ignores stream end markers that do not terminate a document" $ do
    D.decodeStream "# comment\n...\n" `shouldBe` Right (Stream V.empty)
    fmap (V.map docBody . unStream) (D.decodeStream "a\n...\n...\nb\n")
      `shouldBe` Right (V.fromList [YString "a", YString "b"])
  it "keeps document-start comments out of scalar content" $
    D.decode "--- # comment\nscalar\n" `shouldBe` Right (YString "scalar")
  it "resolves overridden secondary tag handles in block and flow nodes" $ do
    fmap J.yamlToJSON (D.decode "%TAG !! tag:example.com,2000:app/\n---\n!!int 1 - 3\n")
      `shouldBe` Right (A.String "1 - 3")
    fmap J.yamlToJSON (D.decode "%TAG !! tag:example.com,2000:app/\n---\n[!!int 1 - 3]\n")
      `shouldBe` Right (A.Array (V.singleton (A.String "1 - 3")))
  it "preserves tags and anchors at the root and inside collections" $ do
    let tagged = YTagged (Tag "!secret") (YString "x")
        customURI = YTagged (Tag "urn:example:secret") (YString "x")
    mapM_
      (\v -> D.decode (E.encode v) `shouldBe` Right v)
      [YAnchored (Anchor "node") (YInt 1), tagged, customURI, seq_ [tagged], mapping [(YString "x", tagged)], YTagged (Tag "!list") (seq_ [YInt 1]), mapping [(tagged, YInt 1), (customURI, YInt 2)]]
    let source = "[&a [1], *a]"
    case D.decode source of
      Left err -> expectationFailure err
      Right v -> fmap J.yamlToJSON (D.decode (E.encode v)) `shouldBe` Right (J.yamlToJSON v)
  it "resolves explicitly tagged scalars before typed decoding" $ do
    (C.decodeYAML "!!str 001" :: Either String Text) `shouldBe` Right "001"
    (C.decodeYAML "!!str null" :: Either String Text) `shouldBe` Right "null"
    (C.decodeYAML "!!bool \"false\"" :: Either String Bool) `shouldBe` Right False
    (C.decodeYAML "!!int \"001\"" :: Either String Int) `shouldBe` Right 1
    fmap J.yamlToJSON (D.decode "[!!str 001, !!bool \"false\"]") `shouldBe` Right (A.Array (V.fromList [A.String "001", A.Bool False]))
  it "aliases inherit a tag applied before their anchor" $
    mapM_
      (\src -> fmap J.yamlToJSON (D.decode src) `shouldBe` Right (A.Array (V.fromList [A.Bool False, A.Bool False])))
      ["[!!bool &a false, *a]", "- !!bool &a false\n- *a\n"]
  it "does not duplicate trailing comments when adding document markers" $ do
    let src = "a: 1\n# tail\n"
    case AD.decodeAnnotated src of
      Left err -> expectationFailure err
      Right (doc, original) ->
        P.renderAnnotatedDocument (P.defaultOptions {P.roDocStartMarker = True}) doc (Just original)
          `shouldBe` "---\na: 1\n# tail\n"
  it "implements core-schema numeric spellings" $
    mapM_
      (\(src, expected) -> D.decode src `shouldBe` Right expected)
      [("01", YInt 1), (".5", YFloat 0.5), ("1.", YFloat 1), ("-.5", YFloat (-0.5)), ("1.e2", YFloat 100), ("0XFF", YString "0XFF"), ("+0xFF", YString "+0xFF")]
  it "projects non-finite JSON values to null" $
    mapM_ (\d -> J.yamlToJSON (YFloat d) `shouldBe` A.Null) [0 / 0, 1 / 0, -1 / 0]
  it "rejects malformed UTF-8 in both byte entry points" $ do
    D.decodeBS (BS.pack [0xff]) `shouldSatisfy` isLeft
    D.decodeStreamBS (BS.pack [0xc0, 0x80]) `shouldSatisfy` isLeft
  it "accepts CR and CRLF line endings" $
    mapM_
      (\src -> D.decode src `shouldBe` Right (mapping [(YString "a", YInt 1), (YString "b", YInt 2)]))
      ["a: 1\rb: 2\r", "a: 1\r\nb: 2\r\n"]
  it "round-trips annotated document framing and multi-document sources exactly" $
    mapM_
      annotatedRoundtrip
      [ "---\na: 1\n...\n"
      , "---\na: 1\n...\n---\nb: 2\n"
      , "# lead\n%YAML 1.2\n---\na: 1\n...\n# next\n%YAML 1.2\n---\nb: 2\n# tail\n"
      , "a: 1\n...\nb: 2\n"
      , "---\n---\n"
      ]
  it "preserves trailing comments after edits" $ do
    let src = "a: 'quoted'\nb: 2\n# trailing\n"
    case AD.decodeAnnotated src of
      Left err -> expectationFailure err
      Right (doc, original) ->
        P.renderAnnotatedDocument P.defaultOptions (doc {AN.adBody = AN.setKey "b" (AN.aInt 3) (AN.adBody doc)}) (Just original)
          `shouldBe` "a: 'quoted'\nb: 3\n# trailing\n"
  it "edits one document without copying another document's source" $ do
    let src = "---\na: 1\n---\nb: 2\n"
    case AD.decodeAnnotatedStream src of
      Left err -> expectationFailure err
      Right (AN.AStream docs, original) -> do
        let changed = V.imap (\i doc -> if i == 1 then doc {AN.adBody = AN.setKey "b" (AN.aInt 3) (AN.adBody doc)} else doc) docs
        P.renderAnnotatedStream P.defaultOptions (AN.AStream changed) (Just original) `shouldBe` "---\na: 1\n---\nb: 3\n"
  it "does not duplicate literal-block content that looks like a comment" $ do
    let src = "a: 1\ncode: |\n  # scalar content\n"
    case AD.decodeAnnotated src of
      Left err -> expectationFailure err
      Right (doc, original) ->
        P.renderAnnotatedDocument P.defaultOptions (doc {AN.adBody = AN.setKey "a" (AN.aInt 2) (AN.adBody doc)}) (Just original)
          `shouldBe` "a: 2\ncode: |\n  # scalar content\n"


annotatedRoundtrip :: Text -> IO ()
annotatedRoundtrip src = case AD.decodeAnnotatedStream src of
  Left err -> expectationFailure err
  Right (docs, original) -> P.renderAnnotatedStream P.defaultOptions docs (Just original) `shouldBe` src
