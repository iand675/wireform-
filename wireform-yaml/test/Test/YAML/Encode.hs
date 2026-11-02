{-# LANGUAGE OverloadedStrings #-}

module Test.YAML.Encode (tests) where

import Data.Either (isLeft)
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Syd
import Test.YAML.Helpers (assertRenderRoundtrip)
import YAML.Decode qualified as D
import YAML.Encode qualified as E
import YAML.JSON qualified as J
import YAML.Pretty qualified as P
import YAML.Value


tests :: Spec
tests =
  describe "encode" $
    sequence_
      [ scalarOutput
      , collectionOutput
      , quotingTests
      , structuralRoundtripTests
      ]


scalarOutput :: Spec
scalarOutput =
  describe "scalars" $
    sequence_
      [ it "null" $ E.encode YNull `shouldBe` "null\n"
      , it "true" $ E.encode (YBool True) `shouldBe` "true\n"
      , it "false" $ E.encode (YBool False) `shouldBe` "false\n"
      , it "int" $ E.encode (YInt 42) `shouldBe` "42\n"
      , it "float" $ (T.last (E.encode (YFloat 3.14)) == '\n') `shouldBe` True
      ]


collectionOutput :: Spec
collectionOutput =
  describe "collections" $
    sequence_
      [ it "empty seq" $ E.encode (YSeq V.empty) `shouldBe` "[]\n"
      , it "empty map" $ E.encode (YMap V.empty) `shouldBe` "{}\n"
      , it "block map" $ do
          let v = YMap (V.fromList [(YString "a", YInt 1), (YString "b", YInt 2)])
          E.encode v `shouldBe` "a: 1\nb: 2\n"
      , it "nested map" $ do
          let v =
                YMap
                  ( V.fromList
                      [
                        ( YString "outer"
                        , YMap (V.fromList [(YString "x", YInt 1)])
                        )
                      ]
                  )
          E.encode v `shouldBe` "outer:\n  x: 1\n"
      , it "block seq" $ do
          let v = YSeq (V.fromList [YInt 1, YInt 2])
          E.encode v `shouldBe` "- 1\n- 2\n"
      ]


quotingTests :: Spec
quotingTests =
  describe "quoting" $
    sequence_
      [ it "string that looks like int is quoted" $ do
          let v = YString "42"
          let out = E.encode v
          case D.decode out of
            Right (YString "42") -> pure ()
            r -> expectationFailure ("did not roundtrip: " ++ show r ++ " from " ++ T.unpack out)
      , it "string that looks like bool is quoted" $ do
          let v = YString "true"
          let out = E.encode v
          case D.decode out of
            Right (YString "true") -> pure ()
            r -> expectationFailure ("did not roundtrip: " ++ show r ++ " from " ++ T.unpack out)
      , it "string with newline" $ do
          let v = YString "a\nb"
          let out = E.encode v
          case D.decode out of
            Right (YString "a\nb") -> pure ()
            r -> expectationFailure (show r)
      ]


structuralRoundtripTests :: Spec
structuralRoundtripTests = describe "structural round trips" $ do
  it "quotes structural string boundaries" $
    mapM_
      (\str -> D.decode (E.encode (YString str)) `shouldBe` Right (YString str))
      ["foo:", "a:\tvalue", ":", "---", "...", " #", "é\n", "\0alias\0x"]
  it "frames constructed streams without merging documents" $ do
    let docs = V.fromList [Document False False (YInt 1), Document False False (YInt 2)]
    fmap (V.map docBody . unStream) (D.decodeStream (E.encodeStream (Stream docs))) `shouldBe` Right (V.fromList [YInt 1, YInt 2])
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
  it "preserves non-string block keys through encode and decode" $
    mapM_
      (\value -> D.decode (E.encode value) `shouldBe` Right value)
      [mapping [(YInt 1, YString "integer"), (YBool True, YString "bool"), (YNull, YString "null"), (YFloat 1.5, YString "float"), (YString "1", YString "string")]]
  it "allows escaped controls and JSON-compatible non-C0 quoted content" $
    mapM_ (\text -> D.decode (E.encode (YString text)) `shouldBe` Right (YString text)) ["x\0y", "x\x7fy", "x\x9fy", "x\xfffey", "x\xffffy", "x\xfeffy"]
  it "uses verbatim syntax for local tags containing handle or flow punctuation" $
    mapM_
      ( \tag ->
          let value = YTagged (Tag tag) (YString "x")
          in do
               D.decode (E.encode value) `shouldBe` Right value
               assertRenderRoundtrip P.defaultOptions value
      )
      ["!local!suffix", "!local,comma", "!local[brackets]", "!"]
  it "uses explicit notation for long and collection-valued keys" $ do
    let value = mapping [(YString (T.replicate 1100 "a"), seq_ [YInt 1]), (seq_ [YInt 1], YString "collection")]
    D.decode (E.encode value) `shouldBe` Right value
    mapM_ (\opts -> assertRenderRoundtrip opts value) [P.defaultOptions, P.compactOptions]
    D.decode (T.replicate 1025 "a" <> ": value") `shouldSatisfy` isLeft
    D.decode ("[" <> T.replicate 1025 "a" <> "]: value") `shouldSatisfy` isLeft
