{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.YAML.ThirdPass (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Either (isLeft, isRight)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Array qualified as TA
import Data.Text.Encoding qualified as TE
import Data.Text.Internal qualified as TI
import Data.Vector qualified as V
import GHC.Exts (Int (..), sizeofByteArray#)
import Test.Syd
import YAML.Annotated qualified as A
import YAML.Class qualified as C
import YAML.Decode qualified as D
import YAML.Decode.Annotated qualified as AD
import YAML.Encode qualified as E
import YAML.JSON qualified as J
import YAML.Pretty qualified as P
import YAML.Value


tests :: Spec
tests = describe "third-pass regressions" $ do
  it "does not retain discarded literal blank lines in the decoded text buffer" $
    mapM_
      ( \header -> case D.decode (header <> "\n  text\n" <> T.replicate 10000 "\n") of
          Right (YString text@(TI.Text (TA.ByteArray bytes#) _ _)) -> do
            T.stripEnd text `shouldBe` "text"
            I# (sizeofByteArray# bytes#) `shouldSatisfy` (<= 128)
          result -> expectationFailure (show result)
      )
      ["|-", "|"]
  it "validates controls at every byte alignment through the ASCII fast path" $
    mapM_
      ( \offset -> do
          let prefix = "key: " <> T.replicate offset "x"
          mapM_ (\char -> D.decode (prefix <> T.singleton char) `shouldSatisfy` isLeft) ['\0', '\v', '\x7f', '\x80', '\xFFFF']
          D.decode (prefix <> "\r\nnext: valid\r\n") `shouldBe` D.decode (prefix <> "\nnext: valid\n")
      )
      [0 .. 15]
  it "keeps non-ASCII quoted content on the lexical validation path" $
    mapM_
      ( \offset -> do
          let content = T.replicate offset "x" <> "\x7f\x80\xFFFF"
          D.decode ("\"" <> content <> "\"") `shouldBe` Right (YString content)
      )
      [0 .. 15]
  it "copies literal UTF-8, relative indentation, and chomping without changing content" $ do
    D.decode "|-\n  é\n    🧰\n\n\n" `shouldBe` Right (YString "é\n  🧰")
    D.decode "|\n  é\n    🧰\n\n\n" `shouldBe` Right (YString "é\n  🧰\n")
    D.decode "|+\n  é\n    🧰\n\n\n" `shouldBe` Right (YString "é\n  🧰\n\n\n")
    D.decode "|2-\n   a\n    b\n\n" `shouldBe` Right (YString " a\n  b")
  it "keeps null values in mappings" $
    roundtrip P.defaultOptions (mapping [(YString "key", YNull)])
  it "rounds decimal floats correctly and bounds extreme exponents" $ do
    mapM_
      (\number -> D.decode (E.encode (YFloat number)) `shouldBe` Right (YFloat number))
      [1.1930203655234305e70, -2.3686045112702234e69, 5e-324, 2.2250738585072014e-308, 1.7976931348623157e308]
    D.decode "1e999999999999999999999999" `shouldBe` Right (YFloat (1 / 0))
    D.decode "1e-999999999999999999999999" `shouldBe` Right (YFloat 0)
    D.decode "0e999999999999999999999999" `shouldBe` Right (YFloat 0)
  it "preserves leading whitespace in literal strings at every nesting level" $
    mapM_
      (\text -> mapM_ (roundtrip P.defaultOptions) [YString text, mapping [(YString "text", YString text)], seq_ [YString text]])
      ["  first\nnext", "\tfirst\nnext", " \nnext", "\n  first\nnext", "  first\nnext\n\n", "\n\n"]
  it "round-trips nested containers without compact sequence mappings" $
    roundtrip (P.defaultOptions {P.roCompactSeqOfMap = False}) (seq_ [mapping [(YString "x", seq_ [YInt 1])], seq_ [YInt 2]])
  it "honours fresh trailing-newline preferences" $ do
    P.render P.compactOptions (YInt 1) `shouldBe` "1"
    roundtrip P.compactOptions (YString "a\nb\n\n")
  it "preserves document framing flags" $
    P.renderDocument P.defaultOptions (Document True True (YInt 1)) `shouldBe` "---\n1\n...\n"
  it "emits each requested stream marker exactly once" $ do
    let docs = Stream (V.fromList [Document True True (YInt 1), Document False False (YInt 2)])
        opts = P.defaultOptions {P.roDocStartMarker = True, P.roDocEndMarker = True}
        out = P.renderStream opts docs
    out `shouldBe` "---\n1\n...\n---\n2\n...\n"
    fmap (fmap docBody . V.toList . unStream) (D.decodeStream out) `shouldBe` Right [YInt 1, YInt 2]
  it "rejects raw C0 controls even inside quoted strings and comments" $
    mapM_ (\source -> D.decode source `shouldSatisfy` isLeft) ["x\0y", "\"x\0y\"", "'x\1y'", "# comment\2\n"]
  it "allows escaped controls and JSON-compatible non-C0 quoted content" $
    mapM_ (\text -> D.decode (E.encode (YString text)) `shouldBe` Right (YString text)) ["x\0y", "x\x7fy", "x\x9fy", "x\xfffey", "x\xffffy", "x\xfeffy"]
  it "rejects non-printable characters in plain and block scalars" $
    mapM_ (\source -> D.decode source `shouldSatisfy` isLeft) ["x\x7fy", "x\x9fy", "x\xfffey", "|\n  x\xffffy\n", "x\xfeffy"]
  it "accepts byte order marks at document prefixes" $ do
    D.decode "\xfeffkey: value" `shouldBe` Right (mapping [(YString "key", YString "value")])
    fmap (fmap docBody . V.toList . unStream) (D.decodeStream "\xfeff---\n1\n...\n\xfeff---\n2\n") `shouldBe` Right [YInt 1, YInt 2]
    D.decode "- one\n\xfeff\n- two\n" `shouldSatisfy` isLeft
  it "detects UTF-16 and UTF-32 with and without a BOM" $ do
    let source = "key: \"hello 😀\"\n"
        expected = D.decode source
        encoders = [TE.encodeUtf16LE, TE.encodeUtf16BE, TE.encodeUtf32LE, TE.encodeUtf32BE]
    mapM_ (\encode -> mapM_ (\text -> D.decodeBS (encode text) `shouldBe` expected) [source, "\xfeff" <> source]) encoders
  it "rejects malformed wide encodings instead of throwing or replacing characters" $
    mapM_
      (\bytes -> D.decodeBS (BS.pack bytes) `shouldSatisfy` isLeft)
      [[255, 254, 0], [254, 255, 216, 0], [255, 254, 0, 220], [0, 0, 254, 255, 0, 17, 0, 0], [255, 254, 0, 0, 0, 216, 0, 0]]
  it "validates directive versions, handles, and repeated handle declarations" $
    mapM_
      (\source -> D.decode source `shouldSatisfy` isLeft)
      ["%YAML 2.0\n---\nx", "%TAG bad tag:example.com,2020:\n---\nx", "%TAG !bad_! !prefix\n---\nx", "%TAG !e! !a\n%TAG !e! !b\n---\nx"]
  it "rejects repeated properties on one node in both block and flow contexts" $
    mapM_
      (\source -> D.decode source `shouldSatisfy` isLeft)
      ["!foo !bar x", "&a &b x", "[!foo !bar x]", "[&a &b x]", "!foo &a !bar x", "&a !foo &b x"]
  it "validates tag URI spelling without expanding percent escapes" $ do
    D.decode "!<tag:example.com,%41> x" `shouldBe` Right (YTagged (Tag "tag:example.com,%41") (YString "x"))
    mapM_
      (\source -> D.decode source `shouldSatisfy` isLeft)
      ["!<urn:bad%> x", "[!<urn:bad%GG> x]", "!<> x", "!<!> x", "!<$:?> x", "%TAG !e! urn:x\n---\n!e! x", "!<urn:x>value"]
  it "uses verbatim syntax for local tags containing handle or flow punctuation" $
    mapM_
      ( \tag ->
          let value = YTagged (Tag tag) (YString "x")
          in do
               D.decode (E.encode value) `shouldBe` Right value
               roundtrip P.defaultOptions value
      )
      ["!local!suffix", "!local,comma", "!local[brackets]", "!"]
  it "resolves a non-specific tag on an empty node as an empty string" $
    fmap unwrap (D.decode "!") `shouldBe` Right (YString "")
  it "allows a root anchor before a same-column mapping" $
    D.decode "&a\nx: 1\ny: 2\n" `shouldBe` Right (YAnchored (Anchor "a") (mapping [(YString "x", YInt 1), (YString "y", YInt 2)]))
  it "edits mappings transparently through node properties" $
    mapM_
      (\source -> checkEdit source (A.setKey "x" (A.aInt 9)) (\value -> lookupKey "x" value `shouldBe` Just (YInt 9)))
      ["!foo {x: 1, y: 2}\n", "&a\nx: 1\ny: 2\n"]
  it "retains anchor definitions when their values are replaced" $
    checkEdit "x: &a 1\ny: *a\n" (A.setKey "x" (A.aInt 9)) (\value -> fmap unwrap (lookupKey "y" value) `shouldBe` Just (YInt 9))
  it "keeps sequence item comments and styles after replacement" $
    checkEdit "- 'old' # note\n- keep\n" (A.setIndex 0 (A.aString "new")) (\value -> unwrap value `shouldBe` seq_ [YString "new", YString "keep"])
  it "preserves sequence item comment text exactly once after replacement" $
    case AD.decodeAnnotated "# lead\n- 'old' # note\n- keep\n" of
      Left err -> expectationFailure err
      Right (doc, original) ->
        P.renderAnnotatedDocument P.defaultOptions (doc {A.adBody = A.setIndex 0 (A.aString "new") (A.adBody doc)}) (Just original)
          `shouldBe` "# lead\n- 'new'  # note\n- keep\n"
  it "registers properties on later mapping keys before parsing their values" $
    mapM_
      (\source -> D.decode source `shouldSatisfy` either (const False) (const True))
      ["first: 1\n&a second: *a\n", "? first\n: 1\n&a second: *a\n", "[1]: value\n&a second: *a\n"]

  it "decodes quoted keys followed by quoted values as mappings" $
    mapM_
      (\source -> D.decode source `shouldBe` Right (mapping [(YString "", YString "")]))
      ["\"\": \"\"", "'': ''"]
  it "uses null for empty implicit keys and honours explicit flow-key markers" $ do
    D.decode ":" `shouldBe` Right (mapping [(YNull, YNull)])
    D.decode "{? foo :, : bar}" `shouldBe` Right (mapping [(YString "foo", YNull), (YNull, YString "bar")])
    D.decode "{?}" `shouldBe` Right (mapping [(YNull, YNull)])
  it "keeps compact mapping keys inside explicit keys" $
    D.decode "? []: x\n" `shouldBe` Right (mapping [(mapping [(seq_ [], YString "x")], YNull)])
  it "does not absorb sibling sequence entries into empty anchored values or keys" $ do
    D.decode "- first: &a\n- second\n" `shouldBe` Right (seq_ [mapping [(YString "first", YAnchored (Anchor "a") YNull)], YString "second"])
    D.decode "- ? &a\n- second\n" `shouldBe` Right (seq_ [mapping [(YAnchored (Anchor "a") YNull, YNull)], YString "second"])
  it "preserves comment-looking block scalar content at its baseline" $
    D.decode "text: |\n  first\n  # content\n  last\n" `shouldBe` Right (mapping [(YString "text", YString "first\n# content\nlast\n")])
  it "rejects reserved indicators in plain scalars at roots and in flow values" $
    mapM_ (\source -> D.decode source `shouldSatisfy` isLeft) ["@foo", "`foo", "[%foo]", "[|foo]", "{a: :}", "key: @foo", "key: `foo"]
  it "rejects multiple documents through single-document entry points" $ do
    D.decode "1\n---\n2" `shouldSatisfy` isLeft
    D.decodeBS "1\n---\n2" `shouldSatisfy` isLeft
  it "uses explicit notation for long and collection-valued keys" $ do
    let value = mapping [(YString (T.replicate 1100 "a"), seq_ [YInt 1]), (seq_ [YInt 1], YString "collection")]
    D.decode (E.encode value) `shouldBe` Right value
    mapM_ (\opts -> roundtrip opts value) [P.defaultOptions, P.compactOptions]
    D.decode (T.replicate 1025 "a" <> ": value") `shouldSatisfy` isLeft
    D.decode ("[" <> T.replicate 1025 "a" <> "]: value") `shouldSatisfy` isLeft
  it "treats empty custom-tag scalars as empty strings" $
    D.decode "!custom" `shouldBe` Right (YTagged (Tag "!custom") (YString ""))
  it "round-trips arbitrary strict and lazy byte payloads without UTF-8 exceptions" $ do
    let bytes = BS.pack [255, 0, 128, 65]
    (C.decodeYAML (C.encodeYAML bytes) :: Either String BS.ByteString) `shouldBe` Right bytes
    let lazyBytes = BSL.fromStrict bytes
    (C.decodeYAML (C.encodeYAML lazyBytes) :: Either String BSL.ByteString) `shouldBe` Right lazyBytes
    (C.decodeYAML "[256]" :: Either String BS.ByteString) `shouldSatisfy` isLeft
  it "preserves Value metadata through the typeclass identity instance" $ do
    let value = YAnchored (Anchor "a") (YTagged (Tag "!custom") (YString "value"))
    C.fromYAML (C.toYAML value) `shouldBe` Right value

  it "indents wrapped flow collections within block mappings" $ do
    let flow = YTagged (Tag "!collection") (seq_ [seq_ (YInt <$> [1 .. 20])])
        value = mapping [(YString "outer", mapping [(YString "nested", flow)])]
    roundtrip (P.defaultOptions {P.roMaxLineWidth = 20}) value
  it "wraps complex explicit keys without dedenting their flow content" $ do
    let value = mapping [(seq_ (YInt <$> [1 .. 20]), YInt 9)]
    roundtrip (P.defaultOptions {P.roMaxLineWidth = 20}) value
  it "applies forced quote styles to fresh mapping keys" $
    P.render (P.defaultOptions {P.roStringStyleHint = P.HintAlwaysSingle}) (mapping [(YString "key", YString "value")]) `shouldBe` "'key': 'value'\n"
  it "honours newline preferences when normalizing annotated documents" $ do
    case AD.decodeAnnotated "text: |+\n  a\n\n" of
      Left err -> expectationFailure err
      Right (doc, original) -> do
        let options = P.compactOptions {P.roPreserveOriginalText = False}
            text = P.renderAnnotatedDocument options doc (Just original)
        T.isSuffixOf "\n" text `shouldBe` False
        D.decode text `shouldBe` D.decode original

  it "distinguishes raw non-C0 quoted content from non-printable comments" $ do
    mapM_ (\source -> fmap unwrap (D.decode source) `shouldBe` Right (YString "\x9f")) ["\"\x9f\"", "'\x9f'", "--- \"\x9f\"", "\xfeff\"\x9f\""]
    mapM_ (\source -> D.decode source `shouldSatisfy` isLeft) ["# \"\x9f\"\n", "key: value # \xfffe", "'key': \"value\" # \x7f"]

  it "normalizes edited anchors while retaining live alias relationships" $ do
    let source = "base: &a {x: 1}\ncopy: *a\n"
    assertEdited (P.defaultOptions {P.roPreserveOriginalText = False}) source (A.updateAt ["base", "x"] (A.aInt 9)) "base: {x: 9}\ncopy: {x: 9}\n"
  it "materializes aliases when their anchor definition is deleted" $
    assertEdited P.defaultOptions "base: &a {x: 1}\ncopy: *a\n" (A.deleteKey "base") "copy: {x: 1}\n"
  it "distinguishes deleted bindings after legal anchor-name rebinding" $
    mapM_
      ( \options -> do
          let source = "old: &a 1\nold-copy: *a\nnew: &a 2\nnew-copy: *a\n"
          assertEdited options source (A.deleteKey "new") "old: 1\nold-copy: 1\nnew-copy: 2\n"
          assertEdited options source (A.deleteKey "old") "old-copy: 1\nnew: 2\nnew-copy: 2\n"
      )
      [P.defaultOptions, P.defaultOptions {P.roPreserveOriginalText = False}]
  it "materializes deleted anchor references inside flow collections" $
    assertEdited P.defaultOptions "base: &a 1\nrest: [*a, {x: *a}]\n" (A.deleteKey "base") "rest: [1, {x: 1}]\n"
  it "edits an alias target as an independent copy" $
    assertEdited P.defaultOptions "base: &a {x: 1}\ncopy: *a\nother: *a\n" (A.updateAt ["copy", "x"] (A.aInt 9)) "base: {x: 1}\ncopy: {x: 9}\nother: {x: 1}\n"
  it "can replace an anchored sequence item with its own resolved reference" $
    assertEdited
      P.defaultOptions
      "- &a 1\n- *a\n"
      ( \value -> case A.avBody value of
          A.ASeq items _ -> A.setIndex 0 (items V.! 1) value
          _ -> value
      )
      "[1, 1]"
  it "materializes an alias moved before its definition" $
    assertEdited
      P.defaultOptions
      "- &a 1\n- *a\n"
      ( \value -> case A.avBody value of
          A.ASeq items style -> A.markDirty value {A.avBody = A.ASeq (V.reverse items) style}
          _ -> value
      )
      "[1, 1]"
  it "preserves comments inside a root property-wrapped mapping" $ do
    case AD.decodeAnnotated "&root\n# note\nx: 1\ny: 'old'\n" of
      Left err -> expectationFailure err
      Right (doc, original) -> do
        let out = P.renderAnnotatedDocument P.defaultOptions (doc {A.adBody = A.setKey "x" (A.aInt 9) (A.adBody doc)}) (Just original)
        T.count "# note" out `shouldBe` 1
        T.isInfixOf "y: 'old'" out `shouldBe` True
        fmap J.yamlToJSON (D.decode out) `shouldBe` fmap J.yamlToJSON (D.decode "x: 9\ny: old\n")

  it "uses a positive indentation even when zero or negative is requested" $
    mapM_ (\indent -> roundtrip (P.defaultOptions {P.roIndent = indent}) (mapping [(YString "outer", mapping [(YString "x", YInt 1)])])) [0, -2]
  it "honours flow mapping style for block-sequence items" $ do
    let options = P.defaultOptions {P.roDefaultMapStyle = A.MapFlow}
        value = seq_ [mapping [(YString "x", YInt 1)]]
    T.isInfixOf "{" (P.render options value) `shouldBe` True
    roundtrip options value

  it "preserves comments, quoting, and uniform CR/CRLF endings during edits" $
    mapM_
      ( \ending -> do
          let source = T.replace "\n" ending "outer:\n  # note\n  x: 1\n  y: 'old'\n"
              expected = T.replace "\n" ending "outer:\n  # note\n  x: 9\n  y: 'old'\n"
          case AD.decodeAnnotated source of
            Left err -> expectationFailure err
            Right (doc, original) -> P.renderAnnotatedDocument P.defaultOptions (doc {A.adBody = A.updateAt ["outer", "x"] (A.aInt 9) (A.adBody doc)}) (Just original) `shouldBe` expected
      )
      ["\r", "\r\n"]
  it "round-trips empty annotated documents verbatim" $
    case AD.decodeAnnotated "" of
      Left err -> expectationFailure err
      Right (doc, original) -> P.renderAnnotatedDocument P.defaultOptions doc (Just original) `shouldBe` ""
  it "renders an annotated scalar subtree without leaking its mapping header" $
    case AD.decodeAnnotated "key: 'value'\n" of
      Left err -> expectationFailure err
      Right (doc, original) -> case A.avBody (A.adBody doc) of
        A.AMap entries _ -> D.decode (P.renderAnnotated P.defaultOptions (A.amValue (V.head entries)) (Just original)) `shouldBe` Right (YString "value")
        _ -> expectationFailure "expected mapping"
  it "reindents a rendered annotated mapping subtree" $
    case AD.decodeAnnotated "outer:\n  a: 1\n  nested:\n    b: 'two'\n" of
      Left err -> expectationFailure err
      Right (doc, original) -> case A.avBody (A.adBody doc) of
        A.AMap entries _ ->
          let out = P.renderAnnotated P.defaultOptions (A.amValue (V.head entries)) (Just original)
          in D.decode out `shouldBe` D.decode "a: 1\nnested:\n  b: two\n"
        _ -> expectationFailure "expected mapping"
  it "can reuse an annotated mapping under a new key at a deeper indentation" $
    assertEdited
      P.defaultOptions
      "outer:\n  a: 1\n  nested:\n    b: 'two'\n"
      ( \root -> case A.avBody root of
          A.AMap entries _ -> A.appendKey "new" (A.aMap [(A.aString "copy", A.amValue (V.head entries))]) root
          _ -> root
      )
      "outer: {a: 1, nested: {b: two}}\nnew: {copy: {a: 1, nested: {b: two}}}\n"
  it "can reuse an annotated mapping value as a sequence item" $
    case AD.decodeAnnotated "source: {x: 1}\n" of
      Left err -> expectationFailure err
      Right (doc, original) -> case A.avBody (A.adBody doc) of
        A.AMap entries _ -> D.decode (P.renderAnnotated P.defaultOptions (A.aSeq [A.amValue (V.head entries)]) (Just original)) `shouldBe` D.decode "[{x: 1}]"
        _ -> expectationFailure "expected mapping"
  it "separates forced end markers from a copied final entry without a newline" $
    case AD.decodeAnnotated "a: 1\nb: 2" of
      Left err -> expectationFailure err
      Right (doc, original) ->
        D.decodeStream (P.renderAnnotatedDocument (P.defaultOptions {P.roDocEndMarker = True}) doc (Just original))
          `shouldBe` D.decodeStream "a: 1\nb: 2\n...\n"

  it "preserves Unicode spaces and non-ASCII line separators as scalar content" $
    mapM_
      ( \char -> do
          let space = T.singleton char
              strings = [space, "left" <> space, space <> "right", "left" <> space <> "right"]
          mapM_
            ( \text -> do
                D.decode text `shouldBe` Right (YString text)
                D.decode (E.encode (YString text)) `shouldBe` Right (YString text)
                mapM_ (\options -> roundtrip options (mapping [(YString ("key" <> space), YString text)])) [P.defaultOptions, P.compactOptions]
            )
            strings
      )
      ['\xA0', '\x85', '\x2009', '\x2028', '\x2029']
  it "keeps non-breaking spaces next to comment indicators and continuation prefixes" $ do
    let nbsp = T.singleton '\xA0'
    D.decode ("key: " <> nbsp <> "#content") `shouldBe` Right (mapping [(YString "key", YString (nbsp <> "#content"))])
    D.decode ("first\n " <> nbsp <> "second") `shouldBe` Right (YString ("first " <> nbsp <> "second"))
    D.decode ("\"first\n " <> nbsp <> "second\"") `shouldBe` Right (YString ("first " <> nbsp <> "second"))
  it "requires ASCII directive separation and float tag spellings" $ do
    D.decode ("%YAML " <> T.singleton '\xA0' <> "1.2\n---\nx") `shouldSatisfy` isLeft
    D.decode "!!float 0x10" `shouldSatisfy` isLeft
    case D.decode "!!float -0" of
      Right value -> case unwrap value of YFloat number -> isNegativeZero number `shouldBe` True; _ -> expectationFailure "expected float"
      Left err -> expectationFailure err

  it "preserves whitespace-only literal content with an explicit indentation hint" $
    mapM_ (\text -> mapM_ (roundtrip P.defaultOptions) [YString text, seq_ [seq_ [seq_ [YString text]]]]) [" \n", "  \n\n", " \n "]
  it "accepts numeric YAML versions with leading zeroes" $
    D.decode "%YAML 01.02\n---\nx" `shouldBe` Right (YString "x")

  it "rejects native map key collisions introduced by dropping tag metadata" $ do
    let source = "!a key: 1\n!b key: 2\n"
    (C.decodeYAML source :: Either String (Map.Map Text Int)) `shouldSatisfy` isLeft
    (C.decodeYAML source :: Either String Value) `shouldSatisfy` isRight
    (C.fromYAML (mapping [(YInt 1, YInt 1), (YInt 1, YInt 2)]) :: Either String (IntMap.IntMap Int)) `shouldSatisfy` isLeft
  it "preserves non-finite Float values and signed zero through native conversion" $ do
    let nan = C.decodeYAML ".nan" :: Either String Float
    either (const False) isNaN nan `shouldBe` True
    (C.decodeYAML ".inf" :: Either String Float) `shouldBe` Right (1 / 0)
    (C.decodeYAML "-.inf" :: Either String Float) `shouldBe` Right (-1 / 0)
    either (const False) isNegativeZero (C.decodeYAML (C.encodeYAML (negate 0 :: Float)) :: Either String Float) `shouldBe` True

  it "rejects indentation that cannot fit the parser state instead of wrapping it" $
    D.decode ("outer:\n" <> T.replicate 1048575 " " <> "key: value\n") `shouldSatisfy` isLeft

  it "accepts indented comments throughout multiline flow collections" $ do
    D.decode "{\n  # key comment\n  key: 1\n  # before close\n}\n" `shouldBe` D.decode "{key: 1}"
    D.decode "[\n  # item comment\n  1, # inline\n  # next comment\n  2\n]\n" `shouldBe` D.decode "[1, 2]"

  it "decodes expanding Unicode escapes into their UTF-8 scalars" $ do
    let source = "\"é" <> T.replicate 500 "\\L\\P\\u00a0\\U0010ffff" <> "\\nlast\""
        expected = YString ("é" <> T.replicate 500 "\x2028\x2029\xA0\x10FFFF" <> "\nlast")
    D.decode source `shouldBe` Right expected
  it "distinguishes escaped quotes after odd and even backslash runs" $
    mapM_
      (\text -> roundtrip (P.defaultOptions {P.roStringStyleHint = P.HintAlwaysDouble}) (YString text))
      ["\\", "\\\"", "\\\\", "\\\\\"", "a\"b\\c", "é\\\\\"last"]
  it "rejects incomplete and out-of-range escapes at the end of a quoted buffer" $
    mapM_ (\source -> D.decode source `shouldSatisfy` isLeft) ["\"trailing\\", "[\"\\u\"]", "\"\\Uffffffff\"", "\"\\U00110000\"", "\"\\uD800\""]

  it "keeps untouched flow-map tokens and comments when replacing a sibling" $
    assertFlowPresentation
      "# prologue\n{\"name\": 'a''b', # keep this\n count: 0x0A, changed: 1, # ending\n}\n"
      (A.setKey "changed" (A.aInt 2))
      ["# prologue", "\"name\"", "'a''b'", "0x0A", "# keep this", "# ending"]
      "{name: a'b, count: 10, changed: 2}"
  it "keeps flow-sequence comments and quote styles after structural edits" $
    assertFlowPresentation
      "['first', # before second\n \"second\", # before third\n third]\n"
      (A.appendItem (A.aString "fourth") . A.setIndex 1 (A.aString "changed"))
      ["'first'", "\"changed\"", "# before second", "# before third"]
      "[first, changed, third, fourth]"
  it "retains nested flow presentation through block-entry source relocation" $
    assertFlowPresentation
      "outer:\r\n  flow: {keep: 'é', # inner\r\n    nested: [\"old\", 0x10]}\r\n"
      (A.updateKey "outer" (Just . A.updateKey "flow" (Just . A.updateKey "nested" (Just . A.setIndex 0 (A.aString "new")))))
      ["'é'", "# inner", "\"new\"", "0x10"]
      "outer: {flow: {keep: é, nested: [new, 16]}}"
  it "can delete and append commented flow entries" $
    assertFlowPresentation
      "{remove: 1, # keep comment\n keep: 'value', # last\n}\n"
      (A.appendKey "added" (A.aBool True) . A.deleteKey "remove")
      ["# keep comment", "'value'", "# last"]
      "{keep: value, added: true}"
  it "preserves comments between flow properties and the collection" $
    assertFlowPresentation
      "!tag # properties\n{key: ':colon', empty: !!str, other: 1}\n"
      (A.setKey "other" (A.aInt 2))
      ["# properties", "':colon'", "!!str"]
      "!tag {key: ':colon', empty: !!str, other: 2}"
  it "safely renders a moved multiline flow subtree at a deeper indentation" $
    assertEdited
      P.defaultOptions
      "source: {nested: [\n 'one',\n 'two'\n ]}\n"
      ( \root -> case A.avBody root of
          A.AMap entries _ -> A.appendKey "outer" (A.aMap [(A.aString "deeper", A.amValue (V.head entries))]) root
          _ -> root
      )
      "source: {nested: [one, two]}\nouter: {deeper: {nested: [one, two]}}\n"
  it "preserves a root flow collection's final inline comment" $
    assertFlowPresentation
      "{a: 'value', b: 1} # final comment\n"
      (A.setKey "b" (A.aInt 2))
      ["# final comment", "'value'"]
      "{a: value, b: 2}"
  it "retains an implicit empty-key flow pair when replacing a sibling" $
    assertFlowPresentation
      "[? : , other]\n"
      (A.setIndex 1 (A.aString "changed"))
      ["?"]
      "[{null: null}, changed]"
  it "keeps property comments from swallowing the next flow separator" $
    assertFlowPresentation
      "{empty: !!str # note\n, other: 1}\n"
      (A.setKey "other" (A.aInt 2))
      ["# note"]
      "{empty: !!str, other: 2}"
  it "keeps a copied root's inline comment when inserting it into a flow collection" $
    case AD.decodeAnnotated "{key: 'value'} # copied comment\n" of
      Left err -> expectationFailure err
      Right (doc, original) -> do
        let root = (A.aSeq [A.adBody doc, A.aInt 2]) {A.avBody = A.ASeq (V.fromList [A.adBody doc, A.aInt 2]) A.SeqFlow}
            out = P.renderAnnotated P.defaultOptions root (Just original)
        out `shouldSatisfy` T.isInfixOf "# copied comment"
        D.decode out `shouldBe` D.decode "[{key: value}, 2]"
  it "retains the original numeric token after a leading flow comment" $
    assertFlowPresentation
      "[ # before\n 0x0A, 1]\n"
      (A.setIndex 1 (A.aInt 2))
      ["# before", "0x0A"]
      "[10, 2]"
  it "normalizes commented flow collections without dropping comments or quoting" $
    case AD.decodeAnnotated "{a: 'one', # comment\n b: [\"two\"]}\n" of
      Left err -> expectationFailure err
      Right (doc, source) -> do
        let out = P.renderAnnotatedDocument (P.defaultOptions {P.roPreserveOriginalText = False}) doc (Just source)
        mapM_ (\part -> out `shouldSatisfy` T.isInfixOf part) ["'one'", "\"two\"", "# comment"]
        D.decode out `shouldBe` D.decode source
  it "does not introduce an alias cycle when a copied subtree contains its new anchor" $
    assertEdited
      P.defaultOptions
      "base: &a 1\nnew: {nested: [*a]}\ncopy: *a\n"
      ( \root -> case A.avBody root of
          A.AMap entries _ -> A.setKey "base" (A.amValue (entries V.! 1)) root
          _ -> root
      )
      "base: {nested: [1]}\nnew: {nested: [{nested: [1]}]}\ncopy: {nested: [1]}\n"


roundtrip :: P.RenderOptions -> Value -> IO ()
roundtrip opts value = D.decode (P.render opts value) `shouldBe` Right value


checkEdit :: Text -> (A.AValue -> A.AValue) -> (Value -> IO ()) -> IO ()
checkEdit source change verify = case AD.decodeAnnotated source of
  Left err -> expectationFailure err
  Right (doc, original) -> case D.decode (P.renderAnnotatedDocument P.defaultOptions (doc {A.adBody = change (A.adBody doc)}) (Just original)) of
    Left err -> expectationFailure err
    Right value -> verify value


assertEdited :: P.RenderOptions -> Text -> (A.AValue -> A.AValue) -> Text -> IO ()
assertEdited options source change expected = case AD.decodeAnnotated source of
  Left err -> expectationFailure err
  Right (doc, original) ->
    let output = P.renderAnnotatedDocument options (doc {A.adBody = change (A.adBody doc)}) (Just original)
    in fmap J.yamlToJSON (D.decode output) `shouldBe` fmap J.yamlToJSON (D.decode expected)


assertFlowPresentation :: Text -> (A.AValue -> A.AValue) -> [Text] -> Text -> IO ()
assertFlowPresentation source change retained expected = case AD.decodeAnnotated source of
  Left err -> expectationFailure err
  Right (doc, original) -> do
    let output = P.renderAnnotatedDocument P.defaultOptions (doc {A.adBody = change (A.adBody doc)}) (Just original)
    mapM_ (\text -> output `shouldSatisfy` T.isInfixOf text) retained
    fmap J.yamlToJSON (D.decode output) `shouldBe` fmap J.yamlToJSON (D.decode expected)
