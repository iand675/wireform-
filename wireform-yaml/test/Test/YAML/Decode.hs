{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.YAML.Decode (tests) where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Text qualified as T
import Data.Text.Array qualified as TA
import Data.Text.Encoding qualified as TE
import Data.Text.Internal qualified as TI
import Data.Vector qualified as V
import GHC.Exts (Int (..), sizeofByteArray#)
import Test.Syd
import Test.YAML.Helpers (assertRenderRoundtrip)
import YAML.Decode qualified as D
import YAML.Encode qualified as E
import YAML.JSON qualified as J
import YAML.Pretty qualified as P
import YAML.Value


tests :: Spec
tests =
  describe "decode" $
    sequence_
      [ scalarTests
      , collectionTests
      , flowTests
      , stringTests
      , blockScalarTests
      , anchorTests
      , streamTests
      , validationTests
      ]


scalarTests :: Spec
scalarTests =
  describe "scalars" $
    sequence_
      [ it "null" $ D.decode "null" `shouldBe` Right YNull
      , it "tilde" $ D.decode "~" `shouldBe` Right YNull
      , it "true" $ D.decode "true" `shouldBe` Right (YBool True)
      , it "false" $ D.decode "false" `shouldBe` Right (YBool False)
      , it "int" $ D.decode "42" `shouldBe` Right (YInt 42)
      , it "neg" $ D.decode "-17" `shouldBe` Right (YInt (-17))
      , it "hex" $ D.decode "0x2A" `shouldBe` Right (YInt 42)
      , it "oct" $ D.decode "0o52" `shouldBe` Right (YInt 42)
      , it "float" $ case D.decode "3.14" of
          Right (YFloat d) -> (abs (d - 3.14) < 1e-9) `shouldBe` True
          r -> expectationFailure (show r)
      , it ".inf" $ case D.decode ".inf" of
          Right (YFloat d) -> (isInfinite d && d > 0) `shouldBe` True
          r -> expectationFailure (show r)
      , it "-.inf" $ case D.decode "-.inf" of
          Right (YFloat d) -> (isInfinite d && d < 0) `shouldBe` True
          r -> expectationFailure (show r)
      , it ".nan" $ case D.decode ".nan" of
          Right (YFloat d) -> (isNaN d) `shouldBe` True
          r -> expectationFailure (show r)
      , it "unquoted string" $ D.decode "hello" `shouldBe` Right (YString "hello")
      ]


collectionTests :: Spec
collectionTests =
  describe "block style" $
    sequence_
      [ it "block mapping" $ do
          let src =
                T.unlines
                  [ "name: alice"
                  , "age: 30"
                  ]
          case D.decode src of
            Right v -> do
              lookupKey "name" v `shouldBe` Just (YString "alice")
              lookupKey "age" v `shouldBe` Just (YInt 30)
            Left e -> expectationFailure e
      , it "block sequence" $ do
          let src =
                T.unlines
                  [ "- 1"
                  , "- 2"
                  , "- 3"
                  ]
          case D.decode src of
            Right (YSeq xs) -> V.toList xs `shouldBe` [YInt 1, YInt 2, YInt 3]
            r -> expectationFailure (show r)
      , it "nested mapping" $ do
          let src =
                T.unlines
                  [ "server:"
                  , "  host: localhost"
                  , "  port: 8080"
                  ]
          case D.decode src of
            Right v ->
              case lookupKey "server" v of
                Just inner -> do
                  lookupKey "host" inner `shouldBe` Just (YString "localhost")
                  lookupKey "port" inner `shouldBe` Just (YInt 8080)
                _ -> expectationFailure "no server"
            Left e -> expectationFailure e
      , it "sequence of mappings" $ do
          let src =
                T.unlines
                  [ "- name: a"
                  , "  v: 1"
                  , "- name: b"
                  , "  v: 2"
                  ]
          case D.decode src of
            Right (YSeq xs) -> do
              V.length xs `shouldBe` 2
              let m0 = xs V.! 0
              lookupKey "name" m0 `shouldBe` Just (YString "a")
              lookupKey "v" m0 `shouldBe` Just (YInt 1)
            r -> expectationFailure (show r)
      , it "comment + blank lines" $ do
          let src =
                T.unlines
                  [ "# header"
                  , ""
                  , "foo: bar  # trailing"
                  ]
          case D.decode src of
            Right v -> lookupKey "foo" v `shouldBe` Just (YString "bar")
            Left e -> expectationFailure e
      ]


flowTests :: Spec
flowTests =
  describe "flow style" $
    sequence_
      [ it "flow seq" $
          D.decode "[1, 2, 3]"
            `shouldBe` Right (YSeq (V.fromList [YInt 1, YInt 2, YInt 3]))
      , it "flow map" $ case D.decode "{a: 1, b: 2}" of
          Right (YMap kvs) -> do
            V.length kvs `shouldBe` 2
          r -> expectationFailure (show r)
      , it "nested flow" $
          case D.decode "[[1, 2], [3, 4]]" of
            Right (YSeq xs) -> V.length xs `shouldBe` 2
            r -> expectationFailure (show r)
      ]


stringTests :: Spec
stringTests =
  describe "strings" $
    sequence_
      [ it "double quoted" $
          D.decode "\"hello world\"" `shouldBe` Right (YString "hello world")
      , it "single quoted" $
          D.decode "'it''s ok'" `shouldBe` Right (YString "it's ok")
      , it "escape \\n" $
          D.decode "\"a\\nb\"" `shouldBe` Right (YString "a\nb")
      , it "escape \\u" $
          D.decode "\"\\u00E9\"" `shouldBe` Right (YString "\233")
      , it "quoted in mapping" $ do
          case D.decode "msg: \"hello\\tworld\"" of
            Right v -> lookupKey "msg" v `shouldBe` Just (YString "hello\tworld")
            Left e -> expectationFailure e
      ]


blockScalarTests :: Spec
blockScalarTests =
  describe "block scalars" $
    sequence_
      [ it "literal" $ do
          let src =
                T.unlines
                  [ "txt: |"
                  , "  one"
                  , "  two"
                  ]
          case D.decode src of
            Right v -> lookupKey "txt" v `shouldBe` Just (YString "one\ntwo\n")
            Left e -> expectationFailure e
      , it "literal strip" $ do
          let src =
                T.unlines
                  [ "txt: |-"
                  , "  one"
                  , "  two"
                  ]
          case D.decode src of
            Right v -> lookupKey "txt" v `shouldBe` Just (YString "one\ntwo")
            Left e -> expectationFailure e
      , it "folded" $ do
          let src =
                T.unlines
                  [ "txt: >"
                  , "  one"
                  , "  two"
                  ]
          case D.decode src of
            Right v -> lookupKey "txt" v `shouldBe` Just (YString "one two\n")
            Left e -> expectationFailure e
      ]


anchorTests :: Spec
anchorTests =
  describe "anchors" $
    sequence_
      [ it "anchor + alias scalar" $ do
          let src =
                T.unlines
                  [ "a: &x 1"
                  , "b: *x"
                  ]
          case D.decode src of
            Right v -> do
              -- Aliases come back wrapped in 'YAnchored' so the
              -- size-of walk can identify shared subtrees by name;
              -- 'unwrap' strips the wrapper for value comparison.
              fmap unwrap (lookupKey "a" v) `shouldBe` Just (YInt 1)
              fmap unwrap (lookupKey "b" v) `shouldBe` Just (YInt 1)
            Left e -> expectationFailure e
      ]


streamTests :: Spec
streamTests =
  describe "stream" $
    sequence_
      [ it "single doc, no markers" $
          case D.decodeStream "k: 1" of
            Right (Stream xs) -> V.length xs `shouldBe` 1
            Left e -> expectationFailure e
      , it "two docs" $ do
          let src =
                T.unlines
                  [ "---"
                  , "a: 1"
                  , "..."
                  , "---"
                  , "b: 2"
                  ]
          case D.decodeStream src of
            Right (Stream xs) -> V.length xs `shouldBe` 2
            Left e -> expectationFailure e
      ]


validationTests :: Spec
validationTests = describe "validation" $ do
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
  it "ignores stream end markers that do not terminate a document" $ do
    D.decodeStream "# comment\n...\n" `shouldBe` Right (Stream V.empty)
    fmap (V.map docBody . unStream) (D.decodeStream "a\n...\n...\nb\n")
      `shouldBe` Right (V.fromList [YString "a", YString "b"])
  it "keeps document-start comments out of scalar content" $
    D.decode "--- # comment\nscalar\n" `shouldBe` Right (YString "scalar")
  it "resolves overridden secondary tag handles in block and flow nodes" $ do
    fmap J.yamlToJSON (D.decode "%TAG !! tag:example.com,2000:app/\n---\n!!int 1 - 3\n")
      `shouldBe` Right (Aeson.String "1 - 3")
    fmap J.yamlToJSON (D.decode "%TAG !! tag:example.com,2000:app/\n---\n[!!int 1 - 3]\n")
      `shouldBe` Right (Aeson.Array (V.singleton (Aeson.String "1 - 3")))
  it "aliases inherit a tag applied before their anchor" $
    mapM_
      (\src -> fmap J.yamlToJSON (D.decode src) `shouldBe` Right (Aeson.Array (V.fromList [Aeson.Bool False, Aeson.Bool False])))
      ["[!!bool &a false, *a]", "- !!bool &a false\n- *a\n"]
  it "implements core-schema numeric spellings" $
    mapM_
      (\(src, expected) -> D.decode src `shouldBe` Right expected)
      [("01", YInt 1), (".5", YFloat 0.5), ("1.", YFloat 1), ("-.5", YFloat (-0.5)), ("1.e2", YFloat 100), ("0XFF", YString "0XFF"), ("+0xFF", YString "+0xFF")]
  it "rejects malformed UTF-8 in both byte entry points" $ do
    D.decodeBS (BS.pack [0xff]) `shouldSatisfy` isLeft
    D.decodeStreamBS (BS.pack [0xc0, 0x80]) `shouldSatisfy` isLeft
  it "accepts CR and CRLF line endings" $
    mapM_
      (\src -> D.decode src `shouldBe` Right (mapping [(YString "a", YInt 1), (YString "b", YInt 2)]))
      ["a: 1\rb: 2\r", "a: 1\r\nb: 2\r\n"]
  it "keeps earlier alias bindings when a source name is reused" $
    fmap J.yamlToJSON (D.decode "[&a [1], &b [*a, &a 2], *a, *b]")
      `shouldBe` Right (Aeson.Array (V.fromList [Aeson.Array (V.singleton (Aeson.Number 1)), Aeson.Array (V.fromList [Aeson.Array (V.singleton (Aeson.Number 1)), Aeson.Number 2]), Aeson.Number 2, Aeson.Array (V.fromList [Aeson.Array (V.singleton (Aeson.Number 1)), Aeson.Number 2])]))
  it "decodes all YAML escapes in quoted mapping keys" $
    D.decode "\"\\u0061\\x62\\U0001F600\": 1\n\"é\\n\": 2\n"
      `shouldBe` Right (mapping [(YString "ab😀", YInt 1), (YString "é\n", YInt 2)])
  it "rejects malformed escapes in mapping keys" $
    mapM_
      (\source -> D.decode source `shouldSatisfy` isLeft)
      ["\"\\q\": 1", "\"\\uD800\": 1", "x: 0\n\"\\U00110000\": 1", "\"unterminated: 1"]
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
    fmap J.yamlToJSON (D.decode "a: &a {x: 1, y: 2}\nb: &b {x: 3, z: 4}\nvalue:\n  <<: [*a, *b]\n  y: 9\n")
      `shouldBe` Right (Aeson.object ["a" Aeson..= Aeson.object ["x" Aeson..= (1 :: Int), "y" Aeson..= (2 :: Int)], "b" Aeson..= Aeson.object ["x" Aeson..= (3 :: Int), "z" Aeson..= (4 :: Int)], "value" Aeson..= Aeson.object ["x" Aeson..= (1 :: Int), "y" Aeson..= (9 :: Int), "z" Aeson..= (4 :: Int)]])
  it "keeps quoted merge-looking keys literal and rejects invalid merge sources" $ do
    let literal = mapping [(YString "<<", YInt 1)]
    D.decode "\"<<\": 1" `shouldBe` Right literal
    D.decode (E.encode literal) `shouldBe` Right literal
    D.decode (P.render P.defaultOptions literal) `shouldBe` Right literal
    D.decode (E.encode (YString "<<")) `shouldBe` Right (YString "<<")
    mapM_ (\source -> D.decode source `shouldSatisfy` isLeft) ["<<: 1", "<<: [{x: 1}, 2]"]
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
  it "rounds decimal floats correctly and bounds extreme exponents" $ do
    mapM_
      (\number -> D.decode (E.encode (YFloat number)) `shouldBe` Right (YFloat number))
      [1.1930203655234305e70, -2.3686045112702234e69, 5e-324, 2.2250738585072014e-308, 1.7976931348623157e308]
    D.decode "1e999999999999999999999999" `shouldBe` Right (YFloat (1 / 0))
    D.decode "1e-999999999999999999999999" `shouldBe` Right (YFloat 0)
    D.decode "0e999999999999999999999999" `shouldBe` Right (YFloat 0)
  it "rejects raw C0 controls even inside quoted strings and comments" $
    mapM_ (\source -> D.decode source `shouldSatisfy` isLeft) ["x\0y", "\"x\0y\"", "'x\1y'", "# comment\2\n"]
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
  it "resolves a non-specific tag on an empty node as an empty string" $
    fmap unwrap (D.decode "!") `shouldBe` Right (YString "")
  it "allows a root anchor before a same-column mapping" $
    D.decode "&a\nx: 1\ny: 2\n" `shouldBe` Right (YAnchored (Anchor "a") (mapping [(YString "x", YInt 1), (YString "y", YInt 2)]))
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
  it "treats empty custom-tag scalars as empty strings" $
    D.decode "!custom" `shouldBe` Right (YTagged (Tag "!custom") (YString ""))
  it "distinguishes raw non-C0 quoted content from non-printable comments" $ do
    mapM_ (\source -> fmap unwrap (D.decode source) `shouldBe` Right (YString "\x9f")) ["\"\x9f\"", "'\x9f'", "--- \"\x9f\"", "\xfeff\"\x9f\""]
    mapM_ (\source -> D.decode source `shouldSatisfy` isLeft) ["# \"\x9f\"\n", "key: value # \xfffe", "'key': \"value\" # \x7f"]
  it "preserves Unicode spaces and non-ASCII line separators as scalar content" $
    mapM_
      ( \char -> do
          let space = T.singleton char
              strings = [space, "left" <> space, space <> "right", "left" <> space <> "right"]
          mapM_
            ( \text -> do
                D.decode text `shouldBe` Right (YString text)
                D.decode (E.encode (YString text)) `shouldBe` Right (YString text)
                mapM_ (\options -> assertRenderRoundtrip options (mapping [(YString ("key" <> space), YString text)])) [P.defaultOptions, P.compactOptions]
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
  it "accepts numeric YAML versions with leading zeroes" $
    D.decode "%YAML 01.02\n---\nx" `shouldBe` Right (YString "x")
  it "accepts indented comments throughout multiline flow collections" $ do
    D.decode "{\n  # key comment\n  key: 1\n  # before close\n}\n" `shouldBe` D.decode "{key: 1}"
    D.decode "[\n  # item comment\n  1, # inline\n  # next comment\n  2\n]\n" `shouldBe` D.decode "[1, 2]"
  it "decodes expanding Unicode escapes into their UTF-8 scalars" $ do
    let source = "\"é" <> T.replicate 500 "\\L\\P\\u00a0\\U0010ffff" <> "\\nlast\""
        expected = YString ("é" <> T.replicate 500 "\x2028\x2029\xA0\x10FFFF" <> "\nlast")
    D.decode source `shouldBe` Right expected
  it "distinguishes escaped quotes after odd and even backslash runs" $
    mapM_
      (\text -> assertRenderRoundtrip (P.defaultOptions {P.roStringStyleHint = P.HintAlwaysDouble}) (YString text))
      ["\\", "\\\"", "\\\\", "\\\\\"", "a\"b\\c", "é\\\\\"last"]
  it "rejects incomplete and out-of-range escapes at the end of a quoted buffer" $
    mapM_ (\source -> D.decode source `shouldSatisfy` isLeft) ["\"trailing\\", "[\"\\u\"]", "\"\\Uffffffff\"", "\"\\U00110000\"", "\"\\uD800\""]
