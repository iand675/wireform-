{-# LANGUAGE OverloadedStrings #-}

module Test.YAML.Annotated (tests) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Syd
import YAML.Annotated
import YAML.Annotated qualified as A
import YAML.Decode qualified as D
import YAML.Decode.Annotated (decodeAnnotated)
import YAML.Decode.Annotated qualified as AD
import YAML.JSON qualified as J
import YAML.Pretty (defaultOptions, renderAnnotatedDocument)
import YAML.Pretty qualified as P
import YAML.Value


tests :: Spec
tests =
  describe "Annotated" $
    sequence_
      [ roundTripExact
      , roundTripWithComments
      , roundTripSeq
      , modifyPreservesUnmodified
      , modifyPreservesComments
      , addKeyAppears
      , deleteKeyDisappears
      , presentationTests
      ]


{- | Parse and re-render an unmodified document; the result must
be byte-identical to the input.
-}
roundTripExact :: Spec
roundTripExact = it "round-trip preserves source verbatim" $ do
  let src =
        T.unlines
          [ "# top comment"
          , "name: alice"
          , "age: 30"
          , ""
          , "# group comment"
          , "address:"
          , "  street: 1 main st"
          , "  city:   springfield"
          , "tags: [a, b, c]"
          ]
  case decodeAnnotated src of
    Left err -> (if (False) then pure () else expectationFailure (err))
    Right (adoc, srcText) -> do
      let out = renderAnnotatedDocument defaultOptions adoc (Just srcText)
      out `shouldBe` src


{- | Modifying one mapping key only changes that key's value; the
surrounding entries — including comments and the unrelated
entries' formatting — stay byte-identical.
-}
modifyPreservesUnmodified :: Spec
modifyPreservesUnmodified =
  it "single-key modification preserves rest" $ do
    let src =
          T.unlines
            [ "# top comment"
            , "name: alice"
            , "age: 30"
            , "city: springfield"
            ]
    case decodeAnnotated src of
      Left err -> (if (False) then pure () else expectationFailure (err))
      Right (adoc, srcText) -> do
        let body = adBody adoc
            body' = setKey "age" (aInt 31) body
            adoc' = adoc {adBody = body'}
            out = renderAnnotatedDocument defaultOptions adoc' (Just srcText)
        (if ("31" `T.isInfixOf` out) then pure () else expectationFailure ("must mention 31, got: " <> T.unpack out))
        (if ("name: alice" `T.isInfixOf` out) then pure () else expectationFailure ("must keep `name: alice`: " <> T.unpack out))
        (if ("city: springfield" `T.isInfixOf` out) then pure () else expectationFailure ("must keep `city: springfield`: " <> T.unpack out))
        (if (not ("age: 30" `T.isInfixOf` out)) then pure () else expectationFailure ("must drop original `age: 30`: " <> T.unpack out))


-- | Appending a new key produces output that contains it.
addKeyAppears :: Spec
addKeyAppears = it "appendKey adds the new entry" $ do
  let src = T.unlines ["name: alice", "age: 30"]
  case decodeAnnotated src of
    Left err -> (if (False) then pure () else expectationFailure (err))
    Right (adoc, srcText) -> do
      let body = adBody adoc
          body' = appendKey "city" (aString "springfield") body
          adoc' = adoc {adBody = body'}
          out = renderAnnotatedDocument defaultOptions adoc' (Just srcText)
      (if ("city" `T.isInfixOf` out) then pure () else expectationFailure ("expected city: " <> T.unpack out))
      (if ("name: alice" `T.isInfixOf` out) then pure () else expectationFailure ("must keep `name: alice` byte-equal: " <> T.unpack out))
      (if ("age: 30" `T.isInfixOf` out) then pure () else expectationFailure ("must keep `age: 30` byte-equal: " <> T.unpack out))


-- | Round-trip with comments scattered throughout.
roundTripWithComments :: Spec
roundTripWithComments =
  it "round-trip preserves comments verbatim" $ do
    let src =
          T.unlines
            [ "# header comment"
            , "name: alice  # eol comment on name"
            , ""
            , "# group comment"
            , "age: 30"
            , ""
            , "# trailing comment"
            ]
    case decodeAnnotated src of
      Left err -> (if (False) then pure () else expectationFailure (err))
      Right (adoc, srcText) -> do
        let out = renderAnnotatedDocument defaultOptions adoc (Just srcText)
        out `shouldBe` src


-- | Round-trip a top-level sequence with item comments.
roundTripSeq :: Spec
roundTripSeq = it "round-trip preserves top-level seq" $ do
  let src =
        T.unlines
          [ "- alice"
          , "- bob"
          , "- carol"
          ]
  case decodeAnnotated src of
    Left err -> (if (False) then pure () else expectationFailure (err))
    Right (adoc, srcText) -> do
      let out = renderAnnotatedDocument defaultOptions adoc (Just srcText)
      out `shouldBe` src


{- | Modifying one entry preserves the *exact bytes* (including
end-of-line comments and surrounding blank lines) of all the
other entries.
-}
modifyPreservesComments :: Spec
modifyPreservesComments =
  it "modify preserves untouched-entry comments" $ do
    let src =
          T.unlines
            [ "# header"
            , "name: alice  # eol on name"
            , ""
            , "age: 30"
            , ""
            , "# group"
            , "city: springfield"
            ]
    case decodeAnnotated src of
      Left err -> (if (False) then pure () else expectationFailure (err))
      Right (adoc, srcText) -> do
        let body = adBody adoc
            body' = setKey "age" (aInt 31) body
            adoc' = adoc {adBody = body'}
            out = renderAnnotatedDocument defaultOptions adoc' (Just srcText)
        (if ("# eol on name" `T.isInfixOf` out) then pure () else expectationFailure ("must keep eol comment on name: " <> T.unpack out))
        (if ("# group" `T.isInfixOf` out) then pure () else expectationFailure ("must keep `# group` comment: " <> T.unpack out))
        (if ("city: springfield" `T.isInfixOf` out) then pure () else expectationFailure ("must keep `city: springfield`: " <> T.unpack out))
        (if ("31" `T.isInfixOf` out) then pure () else expectationFailure ("must mention 31: " <> T.unpack out))


-- | Deleting a key removes it.
deleteKeyDisappears :: Spec
deleteKeyDisappears = it "deleteKey removes the entry" $ do
  let src = T.unlines ["a: 1", "b: 2", "c: 3"]
  case decodeAnnotated src of
    Left err -> (if (False) then pure () else expectationFailure (err))
    Right (adoc, srcText) -> do
      let body = adBody adoc
          body' = deleteKey "b" body
          adoc' = adoc {adBody = body'}
          out = renderAnnotatedDocument defaultOptions adoc' (Just srcText)
      (if (not ("b: 2" `T.isInfixOf` out)) then pure () else expectationFailure ("must remove b entirely: " <> T.unpack out))
      (if ("a: 1" `T.isInfixOf` out) then pure () else expectationFailure ("must keep a: " <> T.unpack out))
      (if ("c: 3" `T.isInfixOf` out) then pure () else expectationFailure ("must keep c: " <> T.unpack out))


presentationTests :: Spec
presentationTests = describe "source preservation" $ do
  it "does not duplicate trailing comments when adding document markers" $ do
    let src = "a: 1\n# tail\n"
    case AD.decodeAnnotated src of
      Left err -> expectationFailure err
      Right (doc, original) ->
        P.renderAnnotatedDocument (P.defaultOptions {P.roDocStartMarker = True}) doc (Just original)
          `shouldBe` "---\na: 1\n# tail\n"
  it "round-trips annotated document framing and multi-document sources exactly" $
    mapM_
      assertVerbatimRoundtrip
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
        P.renderAnnotatedDocument P.defaultOptions (doc {A.adBody = A.setKey "b" (A.aInt 3) (A.adBody doc)}) (Just original)
          `shouldBe` "a: 'quoted'\nb: 3\n# trailing\n"
  it "edits one document without copying another document's source" $ do
    let src = "---\na: 1\n---\nb: 2\n"
    case AD.decodeAnnotatedStream src of
      Left err -> expectationFailure err
      Right (A.AStream docs, original) -> do
        let changed = V.imap (\i doc -> if i == 1 then doc {A.adBody = A.setKey "b" (A.aInt 3) (A.adBody doc)} else doc) docs
        P.renderAnnotatedStream P.defaultOptions (A.AStream changed) (Just original) `shouldBe` "---\na: 1\n---\nb: 3\n"
  it "does not duplicate literal-block content that looks like a comment" $ do
    let src = "a: 1\ncode: |\n  # scalar content\n"
    case AD.decodeAnnotated src of
      Left err -> expectationFailure err
      Right (doc, original) ->
        P.renderAnnotatedDocument P.defaultOptions (doc {A.adBody = A.setKey "a" (A.aInt 2) (A.adBody doc)}) (Just original)
          `shouldBe` "a: 2\ncode: |\n  # scalar content\n"
  it "preserves TAG directives when another entry is edited" $
    assertEditedDocument
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
    assertEditedDocument
      "outer:\n  # group\n  x: 1 # inline\n  y: 'old'\nother: 2\n"
      ["outer", "x"]
      (A.aInt 9)
      "outer:\n  # group\n  x: 9  # inline\n  y: 'old'\nother: 2\n"
  it "preserves the existing quoting style when changing a nested string" $
    assertEditedDocument
      "outer:\n  name: 'old'\n  other: \"keep\"\n"
      ["outer", "name"]
      (A.aString "new")
      "outer:\n  name: 'new'\n  other: \"keep\"\n"
  it "keeps nested blank lines and trailing comments" $
    assertEditedDocument
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
  it "edits mappings transparently through node properties" $
    mapM_
      (\source -> checkEditedValue source (A.setKey "x" (A.aInt 9)) (\value -> lookupKey "x" value `shouldBe` Just (YInt 9)))
      ["!foo {x: 1, y: 2}\n", "&a\nx: 1\ny: 2\n"]
  it "retains anchor definitions when their values are replaced" $
    checkEditedValue "x: &a 1\ny: *a\n" (A.setKey "x" (A.aInt 9)) (\value -> fmap unwrap (lookupKey "y" value) `shouldBe` Just (YInt 9))
  it "keeps sequence item comments and styles after replacement" $
    checkEditedValue "- 'old' # note\n- keep\n" (A.setIndex 0 (A.aString "new")) (\value -> unwrap value `shouldBe` seq_ [YString "new", YString "keep"])
  it "preserves sequence item comment text exactly once after replacement" $
    case AD.decodeAnnotated "# lead\n- 'old' # note\n- keep\n" of
      Left err -> expectationFailure err
      Right (doc, original) ->
        P.renderAnnotatedDocument P.defaultOptions (doc {A.adBody = A.setIndex 0 (A.aString "new") (A.adBody doc)}) (Just original)
          `shouldBe` "# lead\n- 'new'  # note\n- keep\n"
  it "honours newline preferences when normalizing annotated documents" $ do
    case AD.decodeAnnotated "text: |+\n  a\n\n" of
      Left err -> expectationFailure err
      Right (doc, original) -> do
        let options = P.compactOptions {P.roPreserveOriginalText = False}
            text = P.renderAnnotatedDocument options doc (Just original)
        T.isSuffixOf "\n" text `shouldBe` False
        D.decode text `shouldBe` D.decode original
  it "normalizes edited anchors while retaining live alias relationships" $ do
    let source = "base: &a {x: 1}\ncopy: *a\n"
    assertEditedValue (P.defaultOptions {P.roPreserveOriginalText = False}) source (A.updateAt ["base", "x"] (A.aInt 9)) "base: {x: 9}\ncopy: {x: 9}\n"
  it "materializes aliases when their anchor definition is deleted" $
    assertEditedValue P.defaultOptions "base: &a {x: 1}\ncopy: *a\n" (A.deleteKey "base") "copy: {x: 1}\n"
  it "distinguishes deleted bindings after legal anchor-name rebinding" $
    mapM_
      ( \options -> do
          let source = "old: &a 1\nold-copy: *a\nnew: &a 2\nnew-copy: *a\n"
          assertEditedValue options source (A.deleteKey "new") "old: 1\nold-copy: 1\nnew-copy: 2\n"
          assertEditedValue options source (A.deleteKey "old") "old-copy: 1\nnew: 2\nnew-copy: 2\n"
      )
      [P.defaultOptions, P.defaultOptions {P.roPreserveOriginalText = False}]
  it "materializes deleted anchor references inside flow collections" $
    assertEditedValue P.defaultOptions "base: &a 1\nrest: [*a, {x: *a}]\n" (A.deleteKey "base") "rest: [1, {x: 1}]\n"
  it "edits an alias target as an independent copy" $
    assertEditedValue P.defaultOptions "base: &a {x: 1}\ncopy: *a\nother: *a\n" (A.updateAt ["copy", "x"] (A.aInt 9)) "base: {x: 1}\ncopy: {x: 9}\nother: {x: 1}\n"
  it "can replace an anchored sequence item with its own resolved reference" $
    assertEditedValue
      P.defaultOptions
      "- &a 1\n- *a\n"
      ( \value -> case A.avBody value of
          A.ASeq items _ -> A.setIndex 0 (items V.! 1) value
          _ -> value
      )
      "[1, 1]"
  it "materializes an alias moved before its definition" $
    assertEditedValue
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
    assertEditedValue
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
    assertEditedValue
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
    assertEditedValue
      P.defaultOptions
      "base: &a 1\nnew: {nested: [*a]}\ncopy: *a\n"
      ( \root -> case A.avBody root of
          A.AMap entries _ -> A.setKey "base" (A.amValue (entries V.! 1)) root
          _ -> root
      )
      "base: {nested: [1]}\nnew: {nested: [{nested: [1]}]}\ncopy: {nested: [1]}\n"


assertVerbatimRoundtrip :: Text -> IO ()
assertVerbatimRoundtrip src = case AD.decodeAnnotatedStream src of
  Left err -> expectationFailure err
  Right (docs, original) -> P.renderAnnotatedStream P.defaultOptions docs (Just original) `shouldBe` src


assertEditedDocument :: Text -> [Text] -> A.AValue -> Text -> IO ()
assertEditedDocument source path value expected = case AD.decodeAnnotated source of
  Left err -> expectationFailure err
  Right (doc, original) -> do
    let changed = doc {A.adBody = A.updateAt path value (A.adBody doc)}
        output = P.renderAnnotatedDocument P.defaultOptions changed (Just original)
    output `shouldBe` expected
    D.decode output `shouldBe` D.decode expected


checkEditedValue :: Text -> (A.AValue -> A.AValue) -> (Value -> IO ()) -> IO ()
checkEditedValue source change verify = case AD.decodeAnnotated source of
  Left err -> expectationFailure err
  Right (doc, original) -> case D.decode (P.renderAnnotatedDocument P.defaultOptions (doc {A.adBody = change (A.adBody doc)}) (Just original)) of
    Left err -> expectationFailure err
    Right value -> verify value


assertEditedValue :: P.RenderOptions -> Text -> (A.AValue -> A.AValue) -> Text -> IO ()
assertEditedValue options source change expected = case AD.decodeAnnotated source of
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
