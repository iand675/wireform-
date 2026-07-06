{- | Verb bindings (spec §11.7/§11.8), schema level: the verbs fixture
elaborates into the pinned model shapes, canonical IDL round-trips and is
pinned as a golden, and every elaboration check rejects with an error
naming the offending mutation.

The wire-level half — routing, preconditions, merge-patch decoding,
batches over HTTP — lives in "Test.Lattice.VerbsE2E".
-}
module Test.Lattice.Verbs (tests) where

import Data.Either (isRight)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Lattice.IDL.Parser (SchemaError (..), parseSchema)
import Lattice.IDL.Print (canonicalIdl)
import Lattice.Schema (
  BatchPolicy (..),
  BindVerb (..),
  MutationDef (..),
  VerbBinding (..),
  schemaMutations,
 )
import Lattice.Types (MutationName, TypeName)
import Test.Lattice.Fixtures (verbsSchema, verbsText)
import Test.Syd


tests :: Spec
tests =
  describe "Verb bindings (§11.7/§11.8), schema level" $ do
    describe "the verbs fixture elaborates" $ do
      it "verbs.lattice parses and elaborates" $
        parseSchema verbsText `shouldSatisfy` isRight

      it "canonical IDL round-trips and is a fixpoint" $ do
        parseSchema (canonicalIdl verbsSchema) `shouldBe` Right verbsSchema
        fmap canonicalIdl (parseSchema (canonicalIdl verbsSchema))
          `shouldBe` Right (canonicalIdl verbsSchema)

      it "canonical IDL golden (cross-implementation pin)" $
        pureGoldenTextFile
          "test/fixtures/golden/verbs.canonical.lattice"
          (canonicalIdl verbsSchema)

    describe "elaborated binding shapes (§11.7 model pins)" $ do
      it "PUT /e/Note/{note} elaborates keyed, non-lww" $
        bindingOf "replaceNote"
          `shouldBe` Just (VerbBinding BindPut "Note" (Just "note") False)

      it "PATCH /e/Note/{note} elaborates keyed with a bound PATCH batch" $ do
        bindingOf "editNote"
          `shouldBe` Just (VerbBinding BindPatch "Note" (Just "note") False)
        batchBoundOf "editNote" `shouldBe` Just (BindPatch, "Note")

      it "DELETE /e/Note/{note} elaborates keyed with a bound DELETE batch" $ do
        bindingOf "deleteNote"
          `shouldBe` Just (VerbBinding BindDelete "Note" (Just "note") False)
        batchBoundOf "deleteNote" `shouldBe` Just (BindDelete, "Note")

      it "POST /e/Note elaborates as the collection (creation) form" $ do
        bindingOf "createNote"
          `shouldBe` Just (VerbBinding BindCreate "Note" Nothing False)
        batchBoundOf "createNote" `shouldBe` Just (BindCreate, "Note")

      it "last-writer-wins elaborates on the keyed PATCH" $
        bindingOf "setFlag"
          `shouldBe` Just (VerbBinding BindPatch "Flag" (Just "flag") True)

    describe "elaboration rejections name the offender (§11.7 checks)" $ do
      it "PUT binding on a non-natural effect names the mutation" $
        rejectsMentioning "replaceThing" (mutationDoc putTransactional)

      it "DELETE binding on a non-natural effect names the mutation" $
        rejectsMentioning "dropThing" (mutationDoc deleteTransactional)

      it "PATCH binding whose input record has a required field names the field" $ do
        let src = mutationDoc patchRequiredField
        rejectsMentioning "editThing" src
        rejectsMentioning "label" src

      it "PATCH binding with a non-record input names the mutation" $
        rejectsMentioning "toggleThing" (mutationDoc patchNonRecord)

      it "a key segment naming no argument names the segment" $
        rejectsMentioning "wrong" (mutationDoc keyArgMissing)

      it "a key segment whose type is not the target key type is rejected" $
        rejectsMentioning "count" (mutationDoc keyArgWrongType)

      it "a binding target that is not the returns type is rejected" $
        rejectsMentioning "replaceOther" (mutationDoc targetNotReturns)

      it "a POST binding with a key segment is rejected (creation binds the collection URL)" $
        rejectsMentioning "makeThing" (mutationDoc postWithKey)

      it "last-writer-wins on a POST binding is rejected" $
        rejectsMentioning "makeLww" (mutationDoc postLww)

      it "a POST binding with a second argument is rejected (the body is one creation record)" $
        rejectsMentioning "makeTwoArg" (mutationDoc postTwoArgs)

      it "a POST binding whose argument is not a record names the argument" $ do
        let src = mutationDoc postNonRecord
        rejectsMentioning "makeFromBool" src
        rejectsMentioning "on" src

      it "two mutations binding one (verb, URL shape) are rejected" $ do
        let src = mutationDoc duplicateShape
        rejectsMentioning "editThing" src
        rejectsMentioning "editAgain" src

      it "a PUT batch binding is rejected (PUT never batches)" $
        rejectsMentioning "replaceBatch" (mutationDoc putBatch)

      it "a bound batch without a matching singular binding is rejected" $
        rejectsMentioning "editUnbound" (mutationDoc batchWithoutSingular)


-- ---------------------------------------------------------------------------
-- Model accessors
-- ---------------------------------------------------------------------------

mutationOf :: MutationName -> Maybe MutationDef
mutationOf n = Map.lookup n (schemaMutations verbsSchema)


bindingOf :: MutationName -> Maybe VerbBinding
bindingOf n = mutationOf n >>= mutBinding

batchBoundOf :: MutationName -> Maybe (BindVerb, TypeName)
batchBoundOf n = mutationOf n >>= mutBatch >>= bpBound


-- ---------------------------------------------------------------------------
-- Rejection documents
-- ---------------------------------------------------------------------------

{- | A minimal schema around one Thing entity (plus an unrelated Other),
with the mutation declarations under test appended.
-}
mutationDoc :: [Text] -> Text
mutationDoc decls =
  T.unlines
    ( [ "schema t.example"
      , ""
      , "newtype ThingId = Text"
      , "newtype OtherId = Text"
      , ""
      , "data ThingInput {"
      , "  label: Text"
      , "}"
      , ""
      , "data ThingPatch {"
      , "  label: Text?"
      , "}"
      , ""
      , "data BadPatch {"
      , "  label: Text"
      , "  note:  Text?"
      , "}"
      , ""
      , "entity Thing by id {"
      , "  visible to all by default"
      , "  id:    ThingId"
      , "  label: Text"
      , "  fetch by id: public"
      , "}"
      , ""
      , "entity Other by id {"
      , "  visible to all by default"
      , "  id:    OtherId"
      , "  fetch by id: public"
      , "}"
      , ""
      ]
        <> decls
    )


-- | The body every rejection mutation shares.
mutBody :: Text -> [Text]
mutBody effect =
  [ "  allow       public"
  , "  writes      Thing(new)"
  , "  invalidates writes"
  , "  effect      " <> effect
  ]


putTransactional :: [Text]
putTransactional =
  ["mutation replaceThing(thing: ThingId, input: ThingInput) returns Thing {"]
    <> mutBody "transactional"
    <> ["  as PUT /e/Thing/{thing}", "}"]


deleteTransactional :: [Text]
deleteTransactional =
  ["mutation dropThing(thing: ThingId) returns Thing {"]
    <> mutBody "transactional"
    <> ["  as DELETE /e/Thing/{thing}", "}"]


patchRequiredField :: [Text]
patchRequiredField =
  ["mutation editThing(thing: ThingId, patch: BadPatch) returns Thing {"]
    <> mutBody "transactional"
    <> ["  as PATCH /e/Thing/{thing}", "}"]


patchNonRecord :: [Text]
patchNonRecord =
  ["mutation toggleThing(thing: ThingId, on: Bool) returns Thing {"]
    <> mutBody "transactional"
    <> ["  as PATCH /e/Thing/{thing}", "}"]


keyArgMissing :: [Text]
keyArgMissing =
  ["mutation replaceThing(thing: ThingId, input: ThingInput) returns Thing {"]
    <> mutBody "natural"
    <> ["  as PUT /e/Thing/{wrong}", "}"]


keyArgWrongType :: [Text]
keyArgWrongType =
  ["mutation replaceThing(count: I32, input: ThingInput) returns Thing {"]
    <> mutBody "natural"
    <> ["  as PUT /e/Thing/{count}", "}"]


targetNotReturns :: [Text]
targetNotReturns =
  ["mutation replaceOther(thing: ThingId, input: ThingInput) returns Thing {"]
    <> mutBody "natural"
    <> ["  as PUT /e/Other/{thing}", "}"]


postWithKey :: [Text]
postWithKey =
  ["mutation makeThing(thing: ThingId, input: ThingInput) returns Thing {"]
    <> mutBody "transactional"
    <> ["  as POST /e/Thing/{thing}", "}"]


postLww :: [Text]
postLww =
  ["mutation makeLww(input: ThingInput) returns Thing {"]
    <> mutBody "transactional"
    <> ["  as POST /e/Thing last-writer-wins", "}"]


postTwoArgs :: [Text]
postTwoArgs =
  ["mutation makeTwoArg(input: ThingInput, extra: Bool) returns Thing {"]
    <> mutBody "transactional"
    <> ["  as POST /e/Thing", "}"]


postNonRecord :: [Text]
postNonRecord =
  ["mutation makeFromBool(on: Bool) returns Thing {"]
    <> mutBody "transactional"
    <> ["  as POST /e/Thing", "}"]


duplicateShape :: [Text]
duplicateShape =
  ["mutation editThing(thing: ThingId, patch: ThingPatch) returns Thing {"]
    <> mutBody "transactional"
    <> ["  as PATCH /e/Thing/{thing}", "}", ""]
    <> ["mutation editAgain(thing: ThingId, patch: ThingPatch) returns Thing {"]
    <> mutBody "transactional"
    <> ["  as PATCH /e/Thing/{thing}", "}"]


putBatch :: [Text]
putBatch =
  ["mutation replaceBatch(thing: ThingId, input: ThingInput) returns Thing {"]
    <> mutBody "natural"
    <> ["  as PUT /e/Thing/{thing}", "  batch best-effort max 10 as PUT /e/Thing", "}"]


batchWithoutSingular :: [Text]
batchWithoutSingular =
  ["mutation editUnbound(thing: ThingId, patch: ThingPatch) returns Thing {"]
    <> mutBody "transactional"
    <> ["  batch best-effort max 10 as PATCH /e/Thing", "}"]


{- | Elaboration must fail, and at least one 'SchemaError' must mention the
offending name (mirrors the IDL suite's convention).
-}
rejectsMentioning :: Text -> Text -> IO ()
rejectsMentioning offender src = case parseSchema src of
  Right _ -> expectationFailure ("expected elaboration to reject, mentioning: " <> T.unpack offender)
  Left errs -> map seMessage errs `shouldSatisfy` any (T.isInfixOf offender)
