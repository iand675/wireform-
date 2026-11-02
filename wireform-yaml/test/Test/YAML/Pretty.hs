{-# LANGUAGE OverloadedStrings #-}

module Test.YAML.Pretty (tests) where

import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Syd
import Test.YAML.Helpers (assertDefaultRenderRoundtrip, assertRenderRoundtrip)
import YAML.Annotated qualified as A
import YAML.Decode qualified as D
import YAML.Pretty qualified as P
import YAML.Value


tests :: Spec
tests = describe "pretty" $ do
  prettyOptionsRespected
  prettyMaxLineWidth
  it "pretty-prints nested mappings and sequences without changing their shape" $
    mapM_
      assertDefaultRenderRoundtrip
      [ mapping [(YString "outer", mapping [(YString "x", YInt 1)])]
      , mapping [(YString "outer", seq_ [seq_ [YInt 1, YInt 2], mapping [(YString "x", seq_ [YInt 3])]])]
      , seq_ [seq_ [seq_ [YInt 1]], mapping [(YString "x", mapping [(YString "y", YInt 2)])]]
      ]
  it "preserves tagged and anchored containers in pretty output" $
    mapM_ assertDefaultRenderRoundtrip [seq_ [YTagged (Tag "!custom") (mapping [(YString "x", YInt 1)])], mapping [(YTagged (Tag "!key") (YString "x"), YInt 1)]]
  it "preserves exact string content in pretty output" $
    mapM_
      (assertDefaultRenderRoundtrip . YString)
      [" leading", "trailing ", " a ", "a\nb", "a\n", "a\n\n", "\n", "\n\n", "\r", "a\r\nb", "\nleading\nlast\n\n"]
  it "indents literal scalars at their actual nesting level" $
    assertDefaultRenderRoundtrip (mapping [(YString "outer", mapping [(YString "text", YString "a\nb\n\n")])])
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
  it "keeps null values in mappings" $
    assertRenderRoundtrip P.defaultOptions (mapping [(YString "key", YNull)])
  it "preserves leading whitespace in literal strings at every nesting level" $
    mapM_
      (\text -> mapM_ (assertRenderRoundtrip P.defaultOptions) [YString text, mapping [(YString "text", YString text)], seq_ [YString text]])
      ["  first\nnext", "\tfirst\nnext", " \nnext", "\n  first\nnext", "  first\nnext\n\n", "\n\n"]
  it "round-trips nested containers without compact sequence mappings" $
    assertRenderRoundtrip (P.defaultOptions {P.roCompactSeqOfMap = False}) (seq_ [mapping [(YString "x", seq_ [YInt 1])], seq_ [YInt 2]])
  it "honours fresh trailing-newline preferences" $ do
    P.render P.compactOptions (YInt 1) `shouldBe` "1"
    assertRenderRoundtrip P.compactOptions (YString "a\nb\n\n")
  it "preserves document framing flags" $
    P.renderDocument P.defaultOptions (Document True True (YInt 1)) `shouldBe` "---\n1\n...\n"
  it "emits each requested stream marker exactly once" $ do
    let docs = Stream (V.fromList [Document True True (YInt 1), Document False False (YInt 2)])
        opts = P.defaultOptions {P.roDocStartMarker = True, P.roDocEndMarker = True}
        out = P.renderStream opts docs
    out `shouldBe` "---\n1\n...\n---\n2\n...\n"
    fmap (fmap docBody . V.toList . unStream) (D.decodeStream out) `shouldBe` Right [YInt 1, YInt 2]
  it "indents wrapped flow collections within block mappings" $ do
    let flow = YTagged (Tag "!collection") (seq_ [seq_ (YInt <$> [1 .. 20])])
        value = mapping [(YString "outer", mapping [(YString "nested", flow)])]
    assertRenderRoundtrip (P.defaultOptions {P.roMaxLineWidth = 20}) value
  it "wraps complex explicit keys without dedenting their flow content" $ do
    let value = mapping [(seq_ (YInt <$> [1 .. 20]), YInt 9)]
    assertRenderRoundtrip (P.defaultOptions {P.roMaxLineWidth = 20}) value
  it "applies forced quote styles to fresh mapping keys" $
    P.render (P.defaultOptions {P.roStringStyleHint = P.HintAlwaysSingle}) (mapping [(YString "key", YString "value")]) `shouldBe` "'key': 'value'\n"
  it "uses a positive indentation even when zero or negative is requested" $
    mapM_ (\indent -> assertRenderRoundtrip (P.defaultOptions {P.roIndent = indent}) (mapping [(YString "outer", mapping [(YString "x", YInt 1)])])) [0, -2]
  it "honours flow mapping style for block-sequence items" $ do
    let options = P.defaultOptions {P.roDefaultMapStyle = A.MapFlow}
        value = seq_ [mapping [(YString "x", YInt 1)]]
    T.isInfixOf "{" (P.render options value) `shouldBe` True
    assertRenderRoundtrip options value
  it "preserves whitespace-only literal content with an explicit indentation hint" $
    mapM_ (\text -> mapM_ (assertRenderRoundtrip P.defaultOptions) [YString text, seq_ [seq_ [seq_ [YString text]]]]) [" \n", "  \n\n", " \n "]


prettyOptionsRespected :: Spec
prettyOptionsRespected = it "pretty options affect output" $ do
  let v =
        YMap
          ( V.fromList
              [ (YString "key", YString "value")
              ,
                ( YString "items"
                , YSeq
                    ( V.fromList
                        [YInt 1, YInt 2, YInt 3]
                    )
                )
              ]
          )
      compact = P.render P.compactOptions v
  (if ("{" `T.isInfixOf` compact) then pure () else expectationFailure ("compact emits flow {}: " <> T.unpack compact))


prettyMaxLineWidth :: Spec
prettyMaxLineWidth = it "max line width wraps long flows" $ do
  let manyItems = YSeq (V.fromList (YInt <$> [1 .. 30]))
      narrow = (P.compactOptions {P.roMaxLineWidth = 20})
      out = P.render narrow manyItems
  -- Should contain at least one newline inside the brackets
  -- because the items don't fit on one line of 20 columns.
  (if (T.count "\n" out > 1) then pure () else expectationFailure ("expected wrapping: " <> T.unpack out))
