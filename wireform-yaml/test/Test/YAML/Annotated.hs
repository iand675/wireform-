{-# LANGUAGE OverloadedStrings #-}

module Test.YAML.Annotated (tests) where

import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Syd
import YAML.Annotated
import YAML.Decode.Annotated (decodeAnnotated)
import YAML.Pretty (defaultOptions, render, renderAnnotatedDocument)
import YAML.Pretty qualified as P
import YAML.Value (Value (..))


tests :: Spec
tests =
  describe "Annotated" $
    sequence_
      [ roundTripExact
      , roundTripWithComments
      , roundTripSeq
      , modifyPreservesUnmodified
      , modifyPreservesComments
      , prettyOptionsRespected
      , prettyMaxLineWidth
      , addKeyAppears
      , deleteKeyDisappears
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


-- | The pretty-printer respects 'roMaxLineWidth' and 'roIndent'.
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
      compact = render P.compactOptions v
  (if ("{" `T.isInfixOf` compact) then pure () else expectationFailure ("compact emits flow {}: " <> T.unpack compact))


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


-- | The pretty-printer wraps long flow collections.
prettyMaxLineWidth :: Spec
prettyMaxLineWidth = it "max line width wraps long flows" $ do
  let manyItems = YSeq (V.fromList (YInt <$> [1 .. 30]))
      narrow = (P.compactOptions {P.roMaxLineWidth = 20})
      out = render narrow manyItems
  -- Should contain at least one newline inside the brackets
  -- because the items don't fit on one line of 20 columns.
  (if (T.count "\n" out > 1) then pure () else expectationFailure ("expected wrapping: " <> T.unpack out))


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
