{- | Cross-slice consistency (spec §10.2 validity floors, §13.2
guarantees 2/3/7, §6.5 the strict tier, §12 page subscriptions).

Transport: everything runs in-process against 'latticeHandler' (no
sockets). Header-level assertions drive the handler directly;
client-behavior assertions build a 'latticeClientOver' client whose send
function journals every request, which also lets a test interpose a
mid-assembly write between two slice fetches of one 'query': the §13.2
guarantee 3 interleaving, fully deterministic (the send function runs on
the query's own thread). Live tests synchronize on a 'TQueue' fed by the
subscription callback under the suite's loud 'io' timeouts; no sleeps.

Fixtures:

* The §10.4 digest fixture ('digestSchema': one @Doc@ with a public
  @title@ and a private @secret@) drives the floor, convergence, and
  one-shot page tests. Its @doc(id:){title secret}@ plan splits into a
  pub and a priv slice whose read sets share @Doc:d1@, which is exactly
  the shape guarantee 3 detects: a write landing between the two fetches
  raises the second slice's floor past the first slice's token.
* An inline notes/memos schema (same shape as the digest fixture, but
  two distinctly named roots over two entities) drives the page
  subscription test: the pub section alone carries the Note record and
  the priv section alone carries the Memo record, so per-(slice, entity)
  delta granularity is observable.
-}
module Test.Lattice.Consistency (tests) where

import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (when)
import Data.Aeson qualified as A
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Either (rights)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Lattice.Backend.Memory (MemoryDb, defaultHooks, MemoryHooks (..), putRow)
import Lattice.Canonical (Compiled (..))
import Lattice.Client
import Lattice.Schema (Schema)
import Lattice.Server (LiveConfig (..), OriginConfig (..), defaultLiveConfig, latticeHandler, publishPurge)
import Lattice.Types
import Lattice.Wire (
  EndRecord (..),
  EntityRecord (..),
  Manifest (..),
  Record (..),
  compareSnapshotTokens,
  decodeRecords,
  hLatticeSnapshot,
  hLatticeSnapshotFloor,
  parseSnapshotVector,
  queryMediaType,
  renderSnapshotVector,
 )
import Network.HTTP.Message (Request (..), Response (..), Scheme (..))
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Types.Header (HeaderName, Headers, hCacheControl, lookupHeader)
import Network.HTTP.Types.Method (Method (..))
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Version qualified as V
import Test.Lattice.Fixtures
import Test.Lattice.Loop
import Test.Syd


tests :: Spec
tests =
  describe "Consistency across slices (§10.2, §13.2)" $ do
    describe "validity floors ride every data-slice response (§10.2)" $ do
      it "floor <= token always; a non-intersecting write moves the token but never the floor; an intersecting one moves both" $
        withDocs $ \loop -> do
          hash <- introduceQ loop digestSchema docBothQ
          r1 <- rawOrigin loop GET (pubTarget hash) [] Nothing
          rStatus r1 `shouldBe` 200
          (flr1, tok1) <- interval r1
          compareSnapshotTokens flr1 tok1 `shouldNotBe` GT
          -- A write to an entity outside the query's read set: the next
          -- response's token is strictly newer, its floor untouched
          -- (floors certify "nothing I depend on changed since", so
          -- unrelated churn must not widen anyone's staleness window).
          setDoc loop "d2" "Title d2" "Secret noise"
          publishPurge (loopOrigin loop) ["Doc:d2"]
          r2 <- rawOrigin loop GET (pubTarget hash) [(hCacheControl, "no-cache")] Nothing
          rStatus r2 `shouldBe` 200
          (flr2, tok2) <- interval r2
          compareSnapshotTokens tok1 tok2 `shouldBe` LT
          flr2 `shouldBe` flr1
          -- A write to an entity inside the read set: the floor advances
          -- to the invalidation's token (and stays <= the response token).
          setDoc loop "d1" "Title d1" "Secret v2"
          publishPurge (loopOrigin loop) ["Doc:d1"]
          r3 <- rawOrigin loop GET (pubTarget hash) [(hCacheControl, "no-cache")] Nothing
          rStatus r3 `shouldBe` 200
          (flr3, tok3) <- interval r3
          compareSnapshotTokens flr1 flr3 `shouldBe` LT
          compareSnapshotTokens flr3 tok3 `shouldNotBe` GT

      it "a 304 revalidation still carries the full validity interval" $
        withDocs $ \loop -> do
          hash <- introduceQ loop digestSchema docBothQ
          r1 <- rawOrigin loop GET (pubTarget hash) [] Nothing
          rStatus r1 `shouldBe` 200
          etag <-
            maybe (expectationFailure "the data slice carried no ETag") pure $
              lookupHeader "ETag" (rHeaders r1)
          r2 <- rawOrigin loop GET (pubTarget hash) [("If-None-Match", etag)] Nothing
          rStatus r2 `shouldBe` 304
          rBody r2 `shouldBe` ""
          -- The revalidation is a fresh origin observation: a cache
          -- refreshing its copy must learn the current interval, or the
          -- convergence loop could never terminate through a 304.
          (flr, tok) <- interval r2
          compareSnapshotTokens flr tok `shouldNotBe` GT

    describe "the client's convergence loop (§13.2 guarantee 3)" $ do
      it "a mid-assembly intersecting write is detected and repaired with one no-cache refetch of the stale slice" $
        withDocs $ \loop -> do
          _ <- introduceQ loop digestSchema docBothQ
          (r, reqs) <- trappedQuery loop id (intersectingWrite loop)
          qrConsistent r `shouldBe` True
          -- The repaired page renders the post-write facts whole.
          rootValue "doc" r >>= textField "secret" >>= (`shouldBe` "Secret v2")
          rootValue "doc" r >>= textField "title" >>= (`shouldBe` "Title d1")
          let pubs = sliceGets "pub" reqs
              privs = sliceGets "priv" reqs
          length pubs `shouldBe` 2
          length privs `shouldBe` 1
          -- The refetch (and only the refetch) revalidates through the
          -- shared cache: Cache-Control: no-cache on the wire.
          map noCacheOf pubs `shouldBe` [False, True]
          map noCacheOf privs `shouldBe` [False]

      it "a mid-assembly write outside every fetched read set generates no convergence traffic" $
        withDocs $ \loop -> do
          _ <- introduceQ loop digestSchema docBothQ
          let noise = do
                setDoc loop "d2" "Title d2" "Secret noise"
                publishPurge (loopOrigin loop) ["Doc:d2"]
          (r, reqs) <- trappedQuery loop id noise
          qrConsistent r `shouldBe` True
          length (sliceGets "pub" reqs) `shouldBe` 1
          length (sliceGets "priv" reqs) `shouldBe` 1
          reqs `shouldSatisfy` all (not . noCacheOf)

      it "K exhaustion (ccConvergeRetries = 0) surfaces qrConsistent False on a newest-wins result, never an error" $
        withDocs $ \loop -> do
          _ <- introduceQ loop digestSchema docBothQ
          (r, reqs) <-
            trappedQuery
              loop
              (\c -> c {ccConvergeRetries = 0})
              (intersectingWrite loop)
          qrConsistent r `shouldBe` False
          -- Rendering is newest-wins per entity: the priv slice ran after
          -- the write, so its facts are present even though the page is
          -- not a consistent cut.
          rootValue "doc" r >>= textField "secret" >>= (`shouldBe` "Secret v2")
          -- Repair disabled: exactly the initial slice set was fetched.
          length (sliceGets "pub" reqs) `shouldBe` 1
          length (sliceGets "priv" reqs) `shouldBe` 1
          reqs `shouldSatisfy` all (not . noCacheOf)

    describe "the strict tier: one-shot slice=page (§6.5, §13.2 guarantee 7)" $ do
      it "queryPage answers slice-ordered sections, each manifest carrying its slice and etag, one etag-less end, no-store" $
        withDocs $ \loop -> do
          capV <- newTVarIO Nothing
          let send req = do
                resp <- latticeHandler (loopOrigin loop) req
                when ("intent=oneshot" `BS8.isInfixOf` requestTarget req) $ do
                  body <- case responseBody resp of
                    BodyBytes bs -> pure bs
                    _ -> pure ""
                  atomically (writeTVar capV (Just (responseHeaders resp, body)))
                pure resp
          lc <- latticeClientOver docCfg send
          r <- io "the page query" (queryPage lc docBothQ Map.empty) >>= requireRight
          -- Composed under one snapshot: consistent by construction.
          qrConsistent r `shouldBe` True
          rootValue "doc" r >>= textField "title" >>= (`shouldBe` "Title d1")
          rootValue "doc" r >>= textField "secret" >>= (`shouldBe` "Secret d1")
          (hdrs, body) <-
            readTVarIO capV
              >>= maybe (expectationFailure "no oneshot response was captured") pure
          -- The strict tier trades every shared-cache benefit away.
          lookupHeader hCacheControl hdrs `shouldBe` Just "no-store"
          let recs = rights (decodeRecords body)
          case recs of
            RManifest m : _ -> mSlice m `shouldBe` Just SlicePub
            other -> expectationFailure ("expected a manifest first, got: " <> show (take 1 other))
          manifestSlicesOf recs `shouldBe` [Just SlicePub, Just SlicePriv]
          manifestEtagsOf recs `shouldSatisfy` all (not . T.null)
          -- Each section carries its own rendering of the shared entity.
          entityRefsOf recs `shouldBe` [Ref "Doc" "d1", Ref "Doc" "d1"]
          case reverse recs of
            REnd e : _ -> endEtag e `shouldBe` Nothing
            other -> expectationFailure ("expected the end record last, got: " <> show (take 1 other))
          length (endRecordsOf recs) `shouldBe` 1

      it "strict admission: a nonempty priv slice without Authorization refuses the whole page with priv's ordinary 401" $
        withDocs $ \loop -> do
          lc <-
            latticeClientOver
              defaultClientConfig {ccSchema = Just digestSchema}
              (latticeHandler (loopOrigin loop))
          expectProblem 401 "proof-expired" (queryPage lc docBothQ Map.empty)

      it "slice=page on a hash-form GET is a 400 naming the offender (§6.6)" $
        withDocs $ \loop -> do
          hash <- introduceQ loop digestSchema docBothQ
          r <- rawOrigin loop GET ("/q/" <> encodeUtf8 hash <> "?slice=page") [] Nothing
          rStatus r `shouldBe` 400
          case A.decodeStrict (rBody r) of
            Just v -> problemTexts v `shouldSatisfy` any (T.isInfixOf "slice=page")
            Nothing -> expectationFailure "the 400 carried no JSON problem body"

    describe "multiplexed page subscriptions (§12)" $ do
      it "the snapshot is sectioned in slice order; deltas re-emit per (slice, entity) and a cross-slice write is one burst" $
        withNotes $ \loop -> do
          q <- newTQueueIO
          lc <- latticeClientOver notesCfg (latticeHandler (loopOrigin loop))
          sub <-
            io
              "the page subscription"
              ( subscribeQuery
                  lc
                  pageWatchQ
                  Map.empty
                  defaultSubscribeOptions {soTarget = SubscribePage}
                  (atomically . writeTQueue q)
              )
              >>= requireRight
          let nextEv = io "a live event" (atomically (readTQueue q))
          flip finally (subscriptionCancel sub) $ do
            snap <- nextEv >>= expectSnapshot
            manifestSlicesOf snap `shouldBe` [Just SlicePub, Just SlicePriv]
            entityRefsOf snap `shouldBe` [Ref "Note" "n1", Ref "Memo" "m1"]
            endLast snap
            -- A write touching only the priv section's entity: the delta
            -- carries that record alone, and no manifest (root membership
            -- held in both sections).
            setMemo loop "Memo v2"
            publishPurge (loopOrigin loop) ["Memo:m1"]
            d1 <- nextEv >>= expectDelta
            manifestSlicesOf d1 `shouldBe` []
            entityRefsOf d1 `shouldBe` [Ref "Memo" "m1"]
            endLast d1
            -- One write intersecting both sections: ONE burst carries both
            -- re-emissions, in slice order. This also proves the previous
            -- write produced exactly one delta: a spurious second burst
            -- would arrive here first (SSE is ordered) and fail the shape.
            setNote loop "Note v2"
            setMemo loop "Memo v3"
            publishPurge (loopOrigin loop) ["Note:n1", "Memo:m1"]
            d2 <- nextEv >>= expectDelta
            manifestSlicesOf d2 `shouldBe` []
            entityRefsOf d2 `shouldBe` [Ref "Note" "n1", Ref "Memo" "m1"]
            endLast d2

    describe "snapshot vector plumbing (§13.1, §10.2)" $ do
      it "parseSnapshotVector inverts renderSnapshotVector on one- and two-domain vectors, spaces tolerated" $ do
        let one = [("mem", "mem:41")]
            two = [("mem", "mem:41"), ("pg", "lsn:0/16B3740")]
        parseSnapshotVector (renderSnapshotVector one) `shouldBe` one
        parseSnapshotVector (renderSnapshotVector two) `shouldBe` two
        parseSnapshotVector " mem=\"mem:41\" ,  pg=\"lsn:9\"" `shouldBe` [("mem", "mem:41"), ("pg", "lsn:9")]

      it "compareSnapshotTokens: decimal tails compare numerically, otherwise length-then-lexicographic" $ do
        -- Numerically, not lexicographically: "mem:41" < "mem:9" as text.
        compareSnapshotTokens "mem:9" "mem:41" `shouldBe` LT
        compareSnapshotTokens "mem:41" "mem:9" `shouldBe` GT
        compareSnapshotTokens "mem:41" "mem:41" `shouldBe` EQ
        -- No decimal tail: shorter first (counter-like tokens grow) ...
        compareSnapshotTokens "zz" "aaa" `shouldBe` LT
        -- ... then lexicographic at equal length.
        compareSnapshotTokens "abc" "abd" `shouldBe` LT


-- ---------------------------------------------------------------------------
-- Fixture: the §10.4 digest docs (public title, private secret)
-- ---------------------------------------------------------------------------

withDocs :: (Loop -> IO a) -> IO a
withDocs =
  withLoop
    (loopSpec digestSchema)
      { lsHooks = defaultHooks {mhGetRoots = Map.singleton "doc" (byIdRoot "Doc")}
      , lsRows = [docRow "d1", docRow "d2"]
      }


docRow :: Text -> (TypeName, Map FieldName A.Value)
docRow k =
  ( "Doc"
  , Map.fromList
      [ ("id", A.String k)
      , ("title", A.String ("Title " <> k))
      , ("secret", A.String ("Secret " <> k))
      ]
  )


byIdRoot :: TypeName -> MemoryDb -> Map ArgName A.Value -> IO (Maybe Ref)
byIdRoot ty _db args = pure $ case Map.lookup "id" args of
  Just (A.String k) -> Just (Ref ty k)
  _ -> Nothing


setDoc :: Loop -> Text -> Text -> Text -> IO ()
setDoc loop k title secret =
  atomically $
    putRow (loopDb loop) "Doc" k $
      Map.fromList
        [ ("id", A.String k)
        , ("title", A.String title)
        , ("secret", A.String secret)
        ]


-- | Selects both audiences of one entity: the pub and priv read sets
-- share @Doc:d1@, so an intersecting write raises both floors.
docBothQ :: Text
docBothQ = "query DocBoth { doc(id: \"d1\") { title secret } }"


-- | The guarantee 3 trigger: a write to the shared entity, published
-- between the pub fetch and the priv fetch by 'trappedQuery'.
intersectingWrite :: Loop -> IO ()
intersectingWrite loop = do
  setDoc loop "d1" "Title d1" "Secret v2"
  publishPurge (loopOrigin loop) ["Doc:d1"]


docCfg :: ClientConfig
docCfg = defaultClientConfig {ccSchema = Just digestSchema, ccAuthorization = Just "Bearer w1"}


-- ---------------------------------------------------------------------------
-- Fixture: notes/memos (disjoint entities across the two sections)
-- ---------------------------------------------------------------------------

pageIdl :: Text
pageIdl =
  T.unlines
    [ "schema consistency.example.com"
    , ""
    , "-- Two roots whose entity records are disjoint across slices: `note`"
    , "-- is fully public; `memo` selects only a private field, so the priv"
    , "-- section alone carries the Memo entity record (the pub slice carries"
    , "-- the root ref, §8.1). Per-(slice, entity) delta granularity is"
    , "-- observable only on this shape."
    , ""
    , "newtype NoteId = Text"
    , "newtype MemoId = Text"
    , ""
    , "entity Note by id {"
    , "  visible to all by default"
    , ""
    , "  id:    NoteId"
    , "  title: Text"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "entity Memo by id {"
    , "  visible to all by default"
    , ""
    , "  id:   MemoId"
    , "  body: Text   private"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "get note(id: NoteId) of Note public"
    , "get memo(id: MemoId) of Memo public"
    ]


pageSchema :: Schema
pageSchema = mustParseSchema pageIdl
{-# NOINLINE pageSchema #-}


pageWatchQ :: Text
pageWatchQ = "query PageWatch { note(id: \"n1\") { title } memo(id: \"m1\") { body } }"


withNotes :: (Loop -> IO a) -> IO a
withNotes =
  withLoop
    (loopSpec pageSchema)
      { lsHooks =
          defaultHooks
            { mhGetRoots = Map.fromList [("note", byIdRoot "Note"), ("memo", byIdRoot "Memo")]
            }
      , lsRows =
          [ ("Note", Map.fromList [("id", A.String "n1"), ("title", A.String "Note v1")])
          , ("Memo", Map.fromList [("id", A.String "m1"), ("body", A.String "Memo v1")])
          ]
      , -- Deterministic streams: no keep-alive ping loop.
        lsTweak = \c -> c {ocLive = defaultLiveConfig {livePingMicros = 0}}
      }


setNote :: Loop -> Text -> IO ()
setNote loop title =
  atomically $
    putRow (loopDb loop) "Note" "n1" $
      Map.fromList [("id", A.String "n1"), ("title", A.String title)]


setMemo :: Loop -> Text -> IO ()
setMemo loop body =
  atomically $
    putRow (loopDb loop) "Memo" "m1" $
      Map.fromList [("id", A.String "m1"), ("body", A.String body)]


notesCfg :: ClientConfig
notesCfg = defaultClientConfig {ccSchema = Just pageSchema, ccAuthorization = Just "Bearer w1"}


-- ---------------------------------------------------------------------------
-- In-process requests
-- ---------------------------------------------------------------------------

-- | A drained in-process response.
data RawR = RawR
  { rStatus :: Int
  , rHeaders :: Headers
  , rBody :: ByteString
  }


mkOriginReq :: Method -> ByteString -> [(HeaderName, ByteString)] -> Maybe ByteString -> Request
mkOriginReq method target headers mBody =
  Request
    { requestMethod = method
    , requestTarget = target
    , requestAuthority = Just "consistency.test"
    , requestScheme = SchemeHttp
    , requestHeaders = headers
    , requestBody = maybe BodyEmpty BodyBytes mBody
    , requestVersion = V.HTTP1_1
    , requestTrailers = pure []
    }


-- | One request straight into 'latticeHandler': the header-assertion
-- seam (the client never exposes response headers).
rawOrigin :: Loop -> Method -> ByteString -> [(HeaderName, ByteString)] -> Maybe ByteString -> IO RawR
rawOrigin loop method target headers mBody = do
  resp <- io "an origin response" (latticeHandler (loopOrigin loop) (mkOriginReq method target headers mBody))
  body <- case responseBody resp of
    BodyEmpty -> pure ""
    BodyBytes bs -> pure bs
    BodyStream pop -> drainPop pop []
  pure
    RawR
      { rStatus = fromIntegral (statusCode (responseStatus resp))
      , rHeaders = responseHeaders resp
      , rBody = body
      }
  where
    drainPop pop acc =
      pop >>= \case
        Nothing -> pure (BS8.concat (reverse acc))
        Just c -> drainPop pop (c : acc)


-- | Memoize the query at the origin and return its hash, so a test
-- client's first slice request is already the hash-form GET.
introduceQ :: Loop -> Schema -> Text -> IO Text
introduceQ loop schema q = do
  c <- mustCompileWith schema q
  r <-
    rawOrigin
      loop
      POST
      "/q?intent=introduce&slice=pub"
      [("Content-Type", queryMediaType)]
      (Just (encodeUtf8 (compiledText c)))
  rStatus r `shouldBe` 200
  pure (compiledHash c)


pubTarget :: Text -> ByteString
pubTarget hash = "/q/" <> encodeUtf8 hash <> "?slice=pub"


-- ---------------------------------------------------------------------------
-- Validity intervals
-- ---------------------------------------------------------------------------

-- | The single @(domain, token)@ member of a snapshot-vector header.
vectorMember :: HeaderName -> RawR -> IO (Text, Text)
vectorMember name r = case lookupHeader name (rHeaders r) of
  Nothing -> expectationFailure ("the response carried no " <> show name)
  Just v -> case parseSnapshotVector (decodeUtf8 v) of
    [m] -> pure m
    other -> expectationFailure ("expected one vector member, got: " <> show other)


-- | The response's @(floor, token)@; both headers must be present and
-- agree on the domain.
interval :: RawR -> IO (Text, Text)
interval r = do
  (tokDom, tok) <- vectorMember hLatticeSnapshot r
  (flrDom, flr) <- vectorMember hLatticeSnapshotFloor r
  flrDom `shouldBe` tokDom
  pure (flr, tok)


-- ---------------------------------------------------------------------------
-- The trapped client (the §13.2 guarantee 3 interleaving)
-- ---------------------------------------------------------------------------

{- | Run 'query' over a send function that journals every request and
springs @trap@ exactly once: after the first pub-slice GET has been
served (its facts are frozen) and before its response reaches the
client, so the write lands between the two slice fetches of one page
assembly. Deterministic: the send function runs on the query's thread.
-}
trappedQuery :: Loop -> (ClientConfig -> ClientConfig) -> IO () -> IO (QueryResult, [Request])
trappedQuery loop f trap = do
  reqsV <- newTVarIO []
  armedV <- newTVarIO True
  let send req = do
        atomically (modifyTVar' reqsV (req :))
        resp <- latticeHandler (loopOrigin loop) req
        fire <- atomically $ do
          armed <- readTVar armedV
          let hit = armed && isGet req && hasSlice "pub" req
          when hit (writeTVar armedV False)
          pure hit
        when fire trap
        pure resp
  lc <- latticeClientOver (f docCfg) send
  r <- io "the trapped query" (query lc docBothQ Map.empty) >>= requireRight
  reqs <- reverse <$> readTVarIO reqsV
  pure (r, reqs)


isGet :: Request -> Bool
isGet req = case requestMethod req of
  GET -> True
  _ -> False


hasSlice :: ByteString -> Request -> Bool
hasSlice s req = ("slice=" <> s) `BS8.isInfixOf` requestTarget req


sliceGets :: ByteString -> [Request] -> [Request]
sliceGets s = filter (\r -> isGet r && hasSlice s r)


noCacheOf :: Request -> Bool
noCacheOf r = lookupHeader hCacheControl (requestHeaders r) == Just "no-cache"


-- ---------------------------------------------------------------------------
-- Record and event shapes
-- ---------------------------------------------------------------------------

manifestSlicesOf :: [Record] -> [Maybe SliceName]
manifestSlicesOf = mapMaybe $ \case
  RManifest m -> Just (mSlice m)
  _ -> Nothing


manifestEtagsOf :: [Record] -> [Text]
manifestEtagsOf = mapMaybe $ \case
  RManifest m -> Just (mEtag m)
  _ -> Nothing


entityRefsOf :: [Record] -> [Ref]
entityRefsOf = mapMaybe $ \case
  REntity er -> Just (erId er)
  _ -> Nothing


endRecordsOf :: [Record] -> [EndRecord]
endRecordsOf = mapMaybe $ \case
  REnd e -> Just e
  _ -> Nothing


endLast :: [Record] -> IO ()
endLast recs = case reverse recs of
  REnd _ : _ -> pure ()
  other -> expectationFailure ("expected the end record last, got: " <> show (take 1 other))


expectSnapshot :: LiveEvent -> IO [Record]
expectSnapshot = \case
  LiveSnapshotEvent rs -> pure rs
  other -> expectationFailure ("expected the snapshot burst, got: " <> show other)


expectDelta :: LiveEvent -> IO [Record]
expectDelta = \case
  LiveDeltaEvent rs -> pure rs
  other -> expectationFailure ("expected a delta burst, got: " <> show other)
