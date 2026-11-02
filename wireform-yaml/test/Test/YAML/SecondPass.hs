{-# LANGUAGE OverloadedStrings #-}

module Test.YAML.SecondPass (tests) where

import Data.Aeson qualified as J
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as BI
import Data.Vector qualified as V
import Test.Syd
import YAML.Annotated qualified as A
import YAML.Decode qualified as D
import YAML.Decode.Annotated qualified as AD
import YAML.Encode qualified as E
import YAML.JSON qualified as YJ
import YAML.Pretty qualified as P
import YAML.Value


tests :: Spec
tests = describe "second-pass regressions" $ do
  it "rejects alias amplification after legal name rebinding" $ do
    let layer i =
          let (old, new) = if odd i then ("a", "b") else ("b", "a")
          in "&" <> new <> " [" <> T.intercalate ", " (replicate 9 ("*" <> old) <> ["&" <> old <> " 0"]) <> "]"
        source = "[" <> T.intercalate ", " ("&a [0]" : (layer <$> [1 .. 10 :: Int])) <> "]"
    D.decode source `shouldSatisfy` isLeft
  it "keeps earlier alias bindings when a source name is reused" $
    fmap YJ.yamlToJSON (D.decode "[&a [1], &b [*a, &a 2], *a, *b]")
      `shouldBe` Right (J.Array (V.fromList [J.Array (V.singleton (J.Number 1)), J.Array (V.fromList [J.Array (V.singleton (J.Number 1)), J.Number 2]), J.Number 2, J.Array (V.fromList [J.Array (V.singleton (J.Number 1)), J.Number 2])]))
  it "bounds total document expansion as well as individual anchors" $ do
    let layer (i, prior) = "&a" <> numberText i <> " [" <> T.intercalate "," (replicate 9 ("*a" <> numberText prior)) <> "]"
        anchors = "&a0 [0]" : (layer <$> zip [1 .. 6 :: Int] [0 .. 5])
        source = "[" <> T.intercalate "," (anchors <> replicate 100 "*a6") <> "]"
    D.decode source `shouldSatisfy` isLeft
  it "pretty-prints nested mappings and sequences without changing their shape" $
    mapM_
      prettyRoundtrip
      [ mapping [(YString "outer", mapping [(YString "x", YInt 1)])]
      , mapping [(YString "outer", seq_ [seq_ [YInt 1, YInt 2], mapping [(YString "x", seq_ [YInt 3])]])]
      , seq_ [seq_ [seq_ [YInt 1]], mapping [(YString "x", mapping [(YString "y", YInt 2)])]]
      ]
  it "preserves tagged and anchored containers in pretty output" $
    mapM_ prettyRoundtrip [seq_ [YTagged (Tag "!custom") (mapping [(YString "x", YInt 1)])], mapping [(YTagged (Tag "!key") (YString "x"), YInt 1)]]
  it "preserves non-string block keys through encode and decode" $
    mapM_
      (\value -> D.decode (E.encode value) `shouldBe` Right value)
      [mapping [(YInt 1, YString "integer"), (YBool True, YString "bool"), (YNull, YString "null"), (YFloat 1.5, YString "float"), (YString "1", YString "string")]]
  it "decodes all YAML escapes in quoted mapping keys" $
    D.decode "\"\\u0061\\x62\\U0001F600\": 1\n\"é\\n\": 2\n"
      `shouldBe` Right (mapping [(YString "ab😀", YInt 1), (YString "é\n", YInt 2)])
  it "rejects malformed escapes in mapping keys" $
    mapM_
      (\source -> D.decode source `shouldSatisfy` isLeft)
      ["\"\\q\": 1", "\"\\uD800\": 1", "x: 0\n\"\\U00110000\": 1", "\"unterminated: 1"]
  it "preserves exact string content in pretty output" $
    mapM_
      (prettyRoundtrip . YString)
      [" leading", "trailing ", " a ", "a\nb", "a\n", "a\n\n", "\n", "\n\n", "\r", "a\r\nb", "\nleading\nlast\n\n"]
  it "indents literal scalars at their actual nesting level" $
    prettyRoundtrip (mapping [(YString "outer", mapping [(YString "text", YString "a\nb\n\n")])])
  it "quotes whitespace and punctuation in flow collections" $
    mapM_
      (\value -> D.decode (P.render P.compactOptions value) `shouldBe` Right value)
      [seq_ (YString <$> ["a,b", "a[b]", " leading", "trailing ", "a\nb"]), mapping [(YString "a,b", YString "x{y}")]]
  it "keeps flow punctuation quoted when ambiguous scalar coercion is allowed" $ do
    let value = seq_ [YString "a,b", YString " leading"]
        options = P.compactOptions {P.roQuoteAmbiguousScalars = False}
    D.decode (P.render options value) `shouldBe` Right value
  it "uses eight-digit escapes for supplementary non-printing characters" $ do
    let value = YString "x\x1D173y"
    D.decode (P.render (P.defaultOptions {P.roStringStyleHint = P.HintAlwaysDouble}) value) `shouldBe` Right value
  it "handles URI punctuation inside verbatim flow tags" $ do
    let value = seq_ [YTagged (Tag "urn:comma,a") (YString "x")]
    D.decode "[!<urn:comma,a> x]" `shouldBe` Right value
    D.decode (P.render P.compactOptions value) `shouldBe` Right value
  it "expands each tag handle exactly once" $
    D.decode "%TAG ! !prefix-\n---\n!foo key: value\n"
      `shouldBe` Right (mapping [(YTagged (Tag "!prefix-foo") (YString "key"), YString "value")])
  it "rejects duplicate keys after scalar and tag resolution" $
    mapM_
      (\source -> D.decode source `shouldSatisfy` isLeft)
      ["a: 1\na: 2", "{a: 1, a: 2}", "01: a\n1: b", "!!str a: 1\na: 2", "? [1,2]\n: a\n? [1,2]\n: b", ".nan: a\n.NaN: b", "-0.0: a\n0.0: b"]
  it "rejects stray flow commas and empty anchor names" $
    mapM_
      (\source -> D.decode source `shouldSatisfy` isLeft)
      ["{,a: 1}", "{a: 1,, b: 2}", "{,,}", "[& x]", "& value", "[* ]"]
  it "supports merge mappings with explicit overrides and earlier-map precedence" $
    fmap YJ.yamlToJSON (D.decode "a: &a {x: 1, y: 2}\nb: &b {x: 3, z: 4}\nvalue:\n  <<: [*a, *b]\n  y: 9\n")
      `shouldBe` Right (J.object ["a" J..= J.object ["x" J..= (1 :: Int), "y" J..= (2 :: Int)], "b" J..= J.object ["x" J..= (3 :: Int), "z" J..= (4 :: Int)], "value" J..= J.object ["x" J..= (1 :: Int), "y" J..= (9 :: Int), "z" J..= (4 :: Int)]])
  it "keeps quoted merge-looking keys literal and rejects invalid merge sources" $ do
    let literal = mapping [(YString "<<", YInt 1)]
    D.decode "\"<<\": 1" `shouldBe` Right literal
    D.decode (E.encode literal) `shouldBe` Right literal
    D.decode (P.render P.defaultOptions literal) `shouldBe` Right literal
    D.decode (E.encode (YString "<<")) `shouldBe` Right (YString "<<")
    mapM_ (\source -> D.decode source `shouldSatisfy` isLeft) ["<<: 1", "<<: [{x: 1}, 2]"]
  it "preserves TAG directives when another entry is edited" $
    editAndCheck
      "%TAG !e! tag:example.com,2020:\n---\nkeep: !e!thing value\nchange: 1\n"
      ["change"]
      (A.aInt 9)
      "%TAG !e! tag:example.com,2020:\n---\nkeep: !e!thing value\nchange: 9\n"
  it "preserves comment-only annotated streams" $
    mapM_
      ( \source -> case AD.decodeAnnotatedStream source of
          Left err -> expectationFailure err
          Right (stream, original) -> P.renderAnnotatedStream P.defaultOptions stream (Just original) `shouldBe` source
      )
      ["# just a comment\n", "\n# comment\n...\n"]
  it "keeps nested comments and untouched scalar styles when one leaf changes" $
    editAndCheck
      "outer:\n  # group\n  x: 1 # inline\n  y: 'old'\nother: 2\n"
      ["outer", "x"]
      (A.aInt 9)
      "outer:\n  # group\n  x: 9  # inline\n  y: 'old'\nother: 2\n"
  it "preserves the existing quoting style when changing a nested string" $
    editAndCheck
      "outer:\n  name: 'old'\n  other: \"keep\"\n"
      ["outer", "name"]
      (A.aString "new")
      "outer:\n  name: 'new'\n  other: \"keep\"\n"
  it "keeps nested blank lines and trailing comments" $
    editAndCheck
      "outer:\n  x: 1\n\n  # group\n  y: 'old'\n  # tail\n"
      ["outer", "x"]
      (A.aInt 9)
      "outer:\n  x: 9\n\n  # group\n  y: 'old'\n  # tail\n"
  it "keeps nested sequence-of-mapping annotations during edits" $ do
    let source = "items:\n  - name: 'first'\n    value: 1 # note\n  - name: 'second'\n    value: 2\n"
    case AD.decodeAnnotated source of
      Left err -> expectationFailure err
      Right (doc, original) -> do
        let changeItems value = case A.avBody value of
              A.ASeq items _ -> A.setIndex 0 (A.setKey "value" (A.aInt 9) (items V.! 0)) value
              _ -> value
            changed = doc {A.adBody = A.updateKey "items" (Just . changeItems) (A.adBody doc)}
            output = P.renderAnnotatedDocument P.defaultOptions changed (Just original)
        output `shouldBe` "items:\n  - name: 'first'\n    value: 9  # note\n  - name: 'second'\n    value: 2\n"
        D.decode output `shouldSatisfy` either (const False) (const True)


prettyRoundtrip :: Value -> IO ()
prettyRoundtrip value = D.decode (P.render P.defaultOptions value) `shouldBe` Right value


editAndCheck :: Text -> [Text] -> A.AValue -> Text -> IO ()
editAndCheck source path value expected = case AD.decodeAnnotated source of
  Left err -> expectationFailure err
  Right (doc, original) -> do
    let changed = doc {A.adBody = A.updateAt path value (A.adBody doc)}
        output = P.renderAnnotatedDocument P.defaultOptions changed (Just original)
    output `shouldBe` expected
    D.decode output `shouldBe` D.decode expected


numberText :: Int -> Text
numberText = TL.toStrict . TB.toLazyText . BI.decimal
