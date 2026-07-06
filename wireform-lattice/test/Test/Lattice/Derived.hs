{- | Derived fields (spec §3.7), all four blocks:

* __IDL & elaboration__ — the pinned clause order parses, prints
  canonically (golden), and elaborates into the 'Derivation' model;
  information flow (§8.1 dominance unless @\@declassify@), chaining, and
  key-field rejections each name the offender.
* __Planning__ — hidden dep traversals count fully against the plan
  budgets.
* __Validators__ — a response touching @on read@ derived fields
  validates with @hash(ver, witness)@: the etag moves when a dep changes
  while the base row's @ver@ does not, and a held @If-None-Match@ stops
  matching. The read set mints the deps' surrogate keys.
* __Materialization__ — @maintained@ values are stored, recomputed by
  the invalidation-bus relay (never on read), and recomputation
  converges: a value-identical recompute writes nothing.

Everything is synchronized on the bus ('subscribeInvalidations' under a
loud 'io' timeout) or on 'maintainedRecompute''s return value — no
sleeps.
-}
module Test.Lattice.Derived (tests) where

import Control.Concurrent.STM (STM, atomically)
import Control.Exception (bracket)
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BS8
import Data.Either (isRight)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Lattice.Backend
import Lattice.Backend.Memory
import Lattice.IDL.Parser (SchemaError (..), parseSchema)
import Lattice.IDL.Print (canonicalIdl)
import Lattice.Plan (Plan, planQuery)
import Lattice.Query.Validate (CompileError (..))
import Lattice.Schema
import Lattice.Server (
  InvalEvent (..),
  maintainedRecompute,
  startMaintainedRelay,
  subscribeInvalidations,
 )
import Lattice.Types
import Lattice.Wire (EntityRecord (..), Record (..), SurrogateKey, hSurrogateKey, queryMediaType)
import Network.HTTP.Types.Header (HeaderName, hETag, hIfNoneMatch)
import Network.HTTP.Types.Method (Method (..))
import Test.Lattice.Fixtures
import Test.Lattice.Loop
import Test.Syd


tests :: Spec
tests =
  describe "Derived fields (§3.7)" $ do
    describe "IDL and elaboration" $ do
      it "derived.lattice parses and elaborates" $
        parseSchema derivedText `shouldSatisfy` isRight

      it "canonical IDL golden (cross-implementation pin)" $
        pureGoldenTextFile
          "test/fixtures/golden/derived.canonical.lattice"
          (canonicalIdl derivedSchema)

      it "canonical IDL round-trips and is a fixpoint" $ do
        parseSchema (canonicalIdl derivedSchema) `shouldBe` Right derivedSchema
        fmap canonicalIdl (parseSchema (canonicalIdl derivedSchema))
          `shouldBe` Right (canonicalIdl derivedSchema)

      it "the three declarations elaborate into the pinned model shapes" $ do
        derivationOf "commentCount"
          `shouldBe` Just (Derivation (ViaCollection "comments" AggCount :| []) OnRead Nothing)
        derivationOf "authorLine"
          `shouldBe` Just (Derivation (ViaEdge "author" "AuthorByline" :| []) OnRead Nothing)
        derivationOf "commentTotal"
          `shouldBe` Just (Derivation (ViaCollection "comments" AggCount :| []) Maintained Nothing)

      describe "rejections name the offender" $ do
        it "a public derived field reading a private dep violates information flow (§8.1 dominance)" $ do
          let src = rejectDoc ["  leak: Text derived reads own(secret) on read"]
          rejectsMentioning "leak" src
          rejectsMentioning "information flow" src

        it "@declassify(approved: …) declassifies deliberately, and is carried on the model" $ do
          let src = rejectDoc ["  leak: Text derived reads own(secret) @declassify(approved: \"LEDGER-99\") on read"]
          schema <- requireRight (parseSchema src)
          (lookupEntity schema "Thing" >>= (`lookupEntityField` "leak") >>= fieldDerivation)
            `shouldSatisfy` maybe False ((== Just "LEDGER-99") . derivDeclassify)

        it "derivation chaining is rejected naming both fields" $ do
          let src =
                rejectDoc
                  [ "  half: W32 derived reads own(label) on read"
                  , "  twice: W32 derived reads own(half) on read"
                  ]
          rejectsMentioning "twice" src
          rejectsMentioning "half" src

        it "a key field cannot be derived" $
          rejectsMentioning "id" (rejectDoc ["  id: ThingId derived reads own(label) on read"])

    describe "planning: hidden traversals are budgeted (§3.7 Planning)" $ do
      it "an edge dep counts fully against maxDepth" $ do
        c <- mustCompileWith derivedSchema "query { post(id: \"p1\") { authorLine } }"
        planQuery derivedSchema defaultBudgets {maxDepth = 1} c
          `rejectsNaming` "maxDepth budget 1"

      it "control: the same selection without the derived field plans clean" $ do
        c <- mustCompileWith derivedSchema "query { post(id: \"p1\") { title } }"
        planQuery derivedSchema defaultBudgets {maxDepth = 1} c `shouldSatisfy` isRight

    describe "on read over loopback (§3.7 Planning + Invalidation)" $ do
      it "aggregate and edge deps compute through the query path; the read set mints the dep surrogate keys" $
        withDerived $ \loop -> do
          r <- oneshotPost loop "query Q { post(id: \"p1\") { title commentCount authorLine } }"
          rawStatus r `shouldBe` 200
          case entityRecordsOf r of
            [er] -> do
              erId er `shouldBe` Ref "Post" "p1"
              Map.lookup "commentCount" (erFields er) `shouldBe` Just (A.Number 2)
              Map.lookup "authorLine" (erFields er) `shouldBe` Just (A.String "by Ada")
            other -> expectationFailure ("expected exactly the Post record, got: " <> show other)
          keys <- surrogatesOf r
          keys `shouldSatisfy` elem "Post:p1"
          -- ViaCollection contributes the collection tag at the grouping
          -- key; ViaEdge contributes the dep entity's key (§3.7
          -- Invalidation) — existing write sets purge derived values.
          keys `shouldSatisfy` elem "Post.comments:p1"
          keys `shouldSatisfy` elem "Author:a1"

      it "a point fetch touching an on-read derived field validates with hash(ver, witness)" $
        withDerived $ \loop -> do
          bare <- httpRaw loop GET "/e/Post/p1?f=title" [] Nothing
          rawHeader hETag bare `shouldBe` Just "\"e1\""
          r <- httpRaw loop GET "/e/Post/p1?f=commentCount" [] Nothing
          w <- requireHeader hETag r
          w `shouldSatisfy` BS8.isPrefixOf "\"w:"
          held <- httpRaw loop GET "/e/Post/p1?f=commentCount" [(hIfNoneMatch, w)] Nothing
          rawStatus held `shouldBe` 304

      it "the witness etag moves when an aggregate dep changes while the base ver does not" $
        withDerived $ \loop -> do
          r1 <- httpRaw loop GET "/e/Post/p1?f=commentCount" [] Nothing
          w1 <- requireHeader hETag r1
          -- A third comment lands out of band: the aggregate moves, the
          -- Post row itself is untouched.
          atomically (putRow (loopDb loop) "Comment" "c8" (commentFields "c8" "p1" "2024-01-03T00:00:00Z"))
          r2 <- httpRaw loop GET "/e/Post/p1?f=commentCount" [] Nothing
          w2 <- requireHeader hETag r2
          w2 `shouldNotBe` w1
          valueOf "commentCount" r2 `shouldBe` Just (A.Number 3)
          -- The base row still validates as e1 …
          base <- httpRaw loop GET "/e/Post/p1?f=title" [] Nothing
          rawHeader hETag base `shouldBe` Just "\"e1\""
          -- … and the held validator stops matching (no stale 304).
          stale <- httpRaw loop GET "/e/Post/p1?f=commentCount" [(hIfNoneMatch, w1)] Nothing
          rawStatus stale `shouldBe` 200

      it "an edge dep's (id, ver) witnesses the response: bumping the dep entity moves the etag" $
        withDerived $ \loop -> do
          r1 <- httpRaw loop GET "/e/Post/p1?f=authorLine" [] Nothing
          w1 <- requireHeader hETag r1
          valueOf "authorLine" r1 `shouldBe` Just (A.String "by Ada")
          atomically $
            putRow (loopDb loop) "Author" "a1" $
              Map.fromList [("id", A.String "a1"), ("name", A.String "Grace")]
          r2 <- httpRaw loop GET "/e/Post/p1?f=authorLine" [] Nothing
          w2 <- requireHeader hETag r2
          w2 `shouldNotBe` w1
          valueOf "authorLine" r2 `shouldBe` Just (A.String "by Grace")

    describe "maintained via the invalidation bus (§3.7 Materialization)" $ do
      it "stored, not computed on read: before any event the unmasked fetch computes commentCount but carries no commentTotal" $
        withDerived $ \loop -> do
          r <- httpRaw loop GET "/e/Post/p1" [] Nothing
          rawStatus r `shouldBe` 200
          -- The on-read sibling computes; the maintained value is read
          -- off the row, and no relay has ever written it.
          valueOf "commentCount" r `shouldBe` Just (A.Number 2)
          valueOf "commentTotal" r `shouldBe` Nothing

      it "the relay recomputes on a mutation's purge, bumps ver, publishes; recomputation converges" $
        withDerived $ \loop -> do
          let origin = loopOrigin loop
          next <- subscribeInvalidations origin
          bracket (startMaintainedRelay origin) id $ \_relay -> do
            m <- addComment loop "c9" "p1" "a third comment"
            rawStatus m `shouldBe` 200
            -- Block (loudly bounded) on the recompute's own purge event:
            -- the relay wrote Post p1 and published its entity key.
            awaitEventNaming next "Post:p1"
            r <- httpRaw loop GET "/e/Post/p1?f=commentTotal" [] Nothing
            valueOf "commentTotal" r `shouldBe` Just (A.Number 3)
            -- The write-back was an ordinary ver bump.
            base <- httpRaw loop GET "/e/Post/p1?f=title" [] Nothing
            rawHeader hETag base `shouldBe` Just "\"e2\""
            -- Convergence guard: recomputing the same read-set key again
            -- is value-identical and writes (and publishes) nothing.
            maintainedRecompute origin ["Post.comments:p1"] >>= (`shouldBe` [])


-- ---------------------------------------------------------------------------
-- Fixture: posts with an author edge and a comments collection
-- ---------------------------------------------------------------------------

withDerived :: (Loop -> IO a) -> IO a
withDerived =
  withLoop
    (loopSpec derivedSchema)
      { lsHooks = derivedHooks
      , lsRows = derivedRows
      }


derivedRows :: [(TypeName, Map FieldName A.Value)]
derivedRows =
  [ ("Author", Map.fromList [("id", A.String "a1"), ("name", A.String "Ada")])
  , ("Post", Map.fromList [("id", A.String "p1"), ("title", A.String "First"), ("authorId", A.String "a1")])
  , ("Comment", commentFields "c1" "p1" "2024-01-01T00:00:00Z")
  , ("Comment", commentFields "c2" "p1" "2024-01-02T00:00:00Z")
  ]


commentFields :: Text -> Text -> Text -> Map FieldName A.Value
commentFields cid post ts =
  Map.fromList
    [ ("id", A.String cid)
    , ("postId", A.String post)
    , ("body", A.String ("comment " <> cid))
    , ("createdAt", A.String ts)
    ]


{- | @commentCount@ and @commentTotal@ need no compute hook (their read
set is a single aggregate; the memory backend's fallback returns it);
@authorLine@ renders its edge dep's fragment fields.
-}
derivedHooks :: MemoryHooks
derivedHooks =
  defaultHooks
    { mhGetRoots = Map.fromList [("post", byIdRoot "Post")]
    , mhMutations = Map.fromList [("addComment", addCommentEffect)]
    , mhDerives = Map.fromList [(("Post", "authorLine"), authorLineOf)]
    }
  where
    byIdRoot ty _db args = pure $ case Map.lookup "id" args of
      Just (A.String k) -> Just (Ref ty k)
      _ -> Nothing
    authorLineOf deps = case Map.lookup "author" (dvEdges deps) of
      Just (_, fields)
        | Just (A.String n) <- Map.lookup "name" fields -> Just (A.String ("by " <> n))
      _ -> Nothing


-- | @addComment(comment, post, body)@: insert the row and report the
-- write facts the schema's write set declares — the created entity and
-- the collection tag at the written comment's @postId@.
addCommentEffect :: MemoryDb -> Claims -> Map ArgName A.Value -> IO MutationOutcome
addCommentEffect db _claims args =
  case (Map.lookup "comment" args, Map.lookup "post" args, Map.lookup "body" args) of
    (Just (A.String cid), Just (A.String post), Just (A.String body)) -> atomically $ do
      putRow db "Comment" cid $
        Map.fromList
          [ ("id", A.String cid)
          , ("postId", A.String post)
          , ("body", A.String body)
          , ("createdAt", A.String "2024-06-01T00:00:00Z")
          ]
      tok <- snapshotToken db
      pure . MutationCommitted $
        CommitResult
          { crResult = [Ref "Comment" cid]
          , crWrites = [WroteEntity (Ref "Comment" cid), WroteCollection "Post.comments" [post]]
          , crSnapshot = tok
          }
    _ -> pure (MutationFailed (internalError (Just "addComment: comment, post, and body arguments required")))


-- ---------------------------------------------------------------------------
-- Requests and assertions
-- ---------------------------------------------------------------------------

oneshotPost :: Loop -> Text -> IO RawResp
oneshotPost loop body =
  httpRaw
    loop
    POST
    "/q?intent=oneshot&slice=pub"
    [("Content-Type", queryMediaType)]
    (Just (encodeUtf8 body))


addComment :: Loop -> Text -> Text -> Text -> IO RawResp
addComment loop cid post body =
  httpRaw
    loop
    POST
    "/m/addComment"
    [("Content-Type", "application/json")]
    ( Just . BS8.toStrict . A.encode $
        A.object [("comment", A.String cid), ("post", A.String post), ("body", A.String body)]
    )


entityRecordsOf :: RawResp -> [EntityRecord]
entityRecordsOf r = mapMaybe pick (rawRecords r)
  where
    pick = \case
      REntity er -> Just er
      _ -> Nothing


-- | The named field of the single entity record of a response.
valueOf :: Text -> RawResp -> Maybe A.Value
valueOf field r = case entityRecordsOf r of
  [er] -> Map.lookup field (erFields er)
  _ -> Nothing


-- | The response's space-separated @Surrogate-Key@ tokens.
surrogatesOf :: RawResp -> IO [Text]
surrogatesOf r = T.words . decodeUtf8 <$> requireHeader hSurrogateKey r


requireHeader :: HeaderName -> RawResp -> IO BS8.ByteString
requireHeader name r =
  maybe (expectationFailure ("response lacks header: " <> show name)) pure (rawHeader name r)


-- | Block (bounded by 'io'-style loud timeout) until the bus delivers an
-- event naming the key.
awaitEventNaming :: STM InvalEvent -> SurrogateKey -> IO ()
awaitEventNaming next key = io ("an invalidation event naming " <> show key) go
  where
    go = do
      ev <- atomically next
      if key `elem` ieKeys ev then pure () else go


-- ---------------------------------------------------------------------------
-- Rejection documents
-- ---------------------------------------------------------------------------

{- | A minimal schema around one Thing entity (public by default, one
private stored field) with the field declarations under test spliced in.
-}
rejectDoc :: [Text] -> Text
rejectDoc fieldDecls =
  T.unlines
    ( [ "schema reject.example.com"
      , ""
      , "newtype ThingId = Text"
      , ""
      , "entity Thing by id {"
      , "  visible to all by default"
      , ""
      , "  id:     ThingId"
      , "  label:  Text"
      , "  secret: Text?  private"
      ]
        <> fieldDecls
        <> [ ""
           , "  fetch by id: public"
           , "}"
           , ""
           , "get thing(id: ThingId) of Thing public"
           ]
    )


-- | Elaboration must fail, and at least one 'SchemaError' must mention
-- the offender (the suite-wide convention).
rejectsMentioning :: Text -> Text -> IO ()
rejectsMentioning offender src = case parseSchema src of
  Right _ -> expectationFailure ("expected elaboration to reject, mentioning: " <> T.unpack offender)
  Left errs -> map seMessage errs `shouldSatisfy` any (T.isInfixOf offender)


-- | The plan must be rejected with a diagnostic naming the violated bound.
rejectsNaming :: Either CompileError Plan -> Text -> IO ()
rejectsNaming result needle = case result of
  Right _ -> expectationFailure ("plan unexpectedly compiled; wanted " <> T.unpack needle)
  Left ce -> ceDiagnostics ce `shouldSatisfy` any (T.isInfixOf needle)


derivationOf :: FieldName -> Maybe Derivation
derivationOf f =
  lookupEntity derivedSchema "Post" >>= (`lookupEntityField` f) >>= fieldDerivation
