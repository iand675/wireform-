{- | Live queries (spec §12) over the in-process loopback handler.

Transport: tests call 'latticeHandler' directly (no sockets) so the SSE
body popper can be pulled frame-by-frame; frames are parsed with the
wireform-http SSE parser ('sseFramePopper' — the suite never hand-rolls
the wire grammar). Synchronization is entirely on the popper (which
blocks until the origin pushes) and on the invalidation bus, under the
suite's loud 'io' timeouts — no sleeps.

Coverage:

* Subscription surface: @live=sse@ on a memoized hash-form GET answers
  @200 text/event-stream@, @Cache-Control: no-store@, opens with the
  @: lattice live@ comment, then streams the snapshot burst
  (manifest → entities → end) with __no__ event ids.
* Deltas: an intersecting 'InvalEvent' re-executes and pushes changed
  @(id, ver)@ entities then @end@ with the new etag; every delta event
  carries @id: {outbox cursor}@. A fresh manifest rides __first__ iff
  membership changed (the §12 ordering pin).
* Single-flight: two subscribers of one (hash, vars, claims) triple
  share one re-execution whose output fans out to both.
* Reconnect with @Last-Event-ID@ is answered with a fresh full
  snapshot (the conforming baseline; the id is advisory).
* Reauth: a presented proof with expiry bounds the connection — at
  expiry the origin sends @{"kind":"reauth"}@ and (grace 0) terminates.
* The per-origin subscriber cap answers 503 over capacity.
-}
module Test.Lattice.Live (tests) where

import Control.Concurrent.STM
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Either (rights)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Lattice.Backend (CommitResult (..), MutationOutcome (..), WriteFact (..), internalError)
import Lattice.Backend.Memory (MemoryDb, MemoryHooks (..), defaultHooks, putRow, snapshotToken)
import Lattice.Canonical (Compiled (..))
import Lattice.Server (
  LiveConfig (..),
  OriginConfig (..),
  defaultLiveConfig,
  latticeHandler,
  publishPurge,
 )
import Lattice.Server.Auth (encodeClaims, hmacProof, hmacVerifier)
import Lattice.Types
import Lattice.Wire (EndRecord (..), EntityRecord (..), Manifest (..), Record (..), decodeRecords, queryMediaType)
import Network.HTTP.Client.SSE (ServerSentEvent (..), SseFrame (..), parseEventStream, sseFramePopper)
import Network.HTTP.Message (Request (..), Response (..), Scheme (..))
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Types.Header (HeaderName, hCacheControl, hContentType, lookupHeader)
import Network.HTTP.Types.Method (Method (..))
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Version qualified as V
import Test.Lattice.Fixtures
import Test.Lattice.Loop
import Test.Syd


tests :: Spec
tests =
  describe "Live queries (§12)" $ do
    describe "subscription and the initial snapshot" $ do
      it "live=sse on a memoized hash answers an event stream: headers, opening comment, snapshot burst, no ids" $
        withLive $ \loop -> do
          hash <- introduceLive loop livePageQ
          withSub loop (liveTarget hash) [] $ \sub -> do
            subStatus sub `shouldBe` 200
            subHeader hContentType sub `shouldSatisfy` maybe False (BS8.isPrefixOf "text/event-stream")
            subHeader hCacheControl sub `shouldBe` Just "no-store"
            first <- nextFrame sub
            commentText first `shouldBe` Just "lattice live"
            burst <- readBurst sub
            -- Snapshot events carry no id (§12: only deltas name a cursor).
            map fst burst `shouldBe` map (const Nothing) burst
            kindsOf burst `shouldBe` ["manifest", "entity", "entity", "entity", "end"]
            case recordsOf burst of
              RManifest m : _ -> Map.lookup "post" (mRoot m) `shouldBe` Just [Ref "Post" "p1"]
              other -> expectationFailure ("expected a manifest first, got: " <> show other)

      it "an unknown live= value is rejected 400; an unknown hash stays the ordinary 404" $
        withLive $ \loop -> do
          hash <- introduceLive loop livePageQ
          bad <- rawLive loop ("/q/" <> encodeUtf8 hash <> "?slice=pub&live=carrier-pigeon") []
          respStatus bad `shouldBe` 400
          missing <- rawLive loop "/q/deadbeef?slice=pub&live=sse" []
          respStatus missing `shouldBe` 404

    describe "deltas ride the invalidation bus" $ do
      it "a changed (id, ver) pushes entity then end with the new etag; no manifest when membership held; ids carry the cursor" $
        withLive $ \loop -> do
          hash <- introduceLive loop liveTitleQ
          withSub loop (liveTarget hash) [] $ \sub -> do
            _ <- nextFrame sub -- opening comment
            snapshot <- readBurst sub
            let etag0 = endEtagOf snapshot
            atomically $
              putRow (loopDb loop) "Post" "p1" $
                Map.fromList
                  [ ("id", A.String "p1")
                  , ("title", A.String "Retitled")
                  , ("authorId", A.String "a1")
                  ]
            publishPurge (loopOrigin loop) ["Post:p1"]
            delta <- readBurst sub
            kindsOf delta `shouldBe` ["entity", "end"]
            -- Every delta event names the triggering outbox cursor, and
            -- one push shares one cursor.
            let ids = map fst delta
            ids `shouldSatisfy` all (maybe False (not . BS8.null))
            length (dedup ids) `shouldBe` 1
            case recordsOf delta of
              [REntity er, REnd e] -> do
                erId er `shouldBe` Ref "Post" "p1"
                Map.lookup "title" (erFields er) `shouldBe` Just (A.String "Retitled")
                endEtag e `shouldSatisfy` (/= etag0)
              other -> expectationFailure ("expected entity+end, got: " <> show other)

      it "page membership grows WITHOUT a fresh manifest (root map held): owner re-emits with the new page, then end (§12 pin)" $
        withLive $ \loop -> do
          hash <- introduceLive loop livePageQ
          withSub loop (liveTarget hash) [] $ \sub -> do
            _ <- nextFrame sub
            snapshot <- readBurst sub
            let etag0 = endEtagOf snapshot
            m <- addComment loop "c9" "p1" "a third comment"
            respStatus m `shouldBe` 200
            delta <- readBurst sub
            map fst delta `shouldSatisfy` all (maybe False (not . BS8.null))
            case recordsOf delta of
              -- The manifest ROOT map is unchanged (`post` still names
              -- Post:p1), so no manifest leads; the grown comments page
              -- rides in the re-emitted owner (byte diff catches the
              -- page change even though Post's ver held still).
              recs@(first' : _) -> do
                case first' of
                  RManifest _ -> expectationFailure "no manifest may lead a delta whose root membership held"
                  _ -> pure ()
                let entityIds = [erId er | REntity er <- recs]
                entityIds `shouldSatisfy` elem (Ref "Comment" "c9")
                entityIds `shouldSatisfy` elem (Ref "Post" "p1")
                case reverse recs of
                  REnd e : _ -> endEtag e `shouldSatisfy` (\e' -> e' /= etag0)
                  other -> expectationFailure ("expected end last, got: " <> show other)
              other -> expectationFailure ("expected a non-empty delta, got: " <> show other)

      it "two subscribers of one (hash, vars, claims) triple share ONE re-execution (single-flight)" $ do
        execs <- newTVarIO (0 :: Int)
        withLiveCounting execs $ \loop -> do
          hash <- introduceLive loop liveTitleQ
          withSub loop (liveTarget hash) [] $ \subA ->
            withSub loop (liveTarget hash) [] $ \subB -> do
              _ <- nextFrame subA
              _ <- readBurst subA
              _ <- nextFrame subB
              _ <- readBurst subB
              atomically (writeTVar execs 0)
              atomically $
                putRow (loopDb loop) "Post" "p1" $
                  Map.fromList
                    [ ("id", A.String "p1")
                    , ("title", A.String "Shared push")
                    , ("authorId", A.String "a1")
                    ]
              publishPurge (loopOrigin loop) ["Post:p1"]
              -- Both connections observe the delta …
              deltaA <- readBurst subA
              deltaB <- readBurst subB
              kindsOf deltaA `shouldBe` ["entity", "end"]
              kindsOf deltaB `shouldBe` ["entity", "end"]
              -- … from one plan execution: concurrent triggers coalesce.
              readTVarIO execs >>= (`shouldBe` 1)

    describe "reconnect (§12: the conforming baseline answer is a fresh snapshot)" $ do
      it "a reconnect presenting Last-Event-ID gets the full snapshot again" $
        withLive $ \loop -> do
          hash <- introduceLive loop liveTitleQ
          cursor <- withSub loop (liveTarget hash) [] $ \sub -> do
            _ <- nextFrame sub
            _ <- readBurst sub
            atomically $
              putRow (loopDb loop) "Post" "p1" $
                Map.fromList
                  [ ("id", A.String "p1")
                  , ("title", A.String "Moved on")
                  , ("authorId", A.String "a1")
                  ]
            publishPurge (loopOrigin loop) ["Post:p1"]
            delta <- readBurst sub
            case mapMaybe fst delta of
              c : _ -> pure c
              [] -> expectationFailure "delta events carried no cursor id"
          withSub loop (liveTarget hash) [("Last-Event-ID", cursor)] $ \sub -> do
            _ <- nextFrame sub
            burst <- readBurst sub
            -- Fresh snapshot: full record set, snapshot events carry no id.
            kindsOf burst `shouldBe` ["manifest", "entity", "end"]
            map fst burst `shouldBe` map (const Nothing) burst

    describe "origin governance" $ do
      it "the subscriber cap answers 503 lattice:live-over-capacity" $
        withLoop
          liveSpec {lsTweak = \c -> c {ocLive = defaultLiveConfig {liveMaxSubscribers = 1}}}
          $ \loop -> do
            hash <- introduceLive loop liveTitleQ
            withSub loop (liveTarget hash) [] $ \sub -> do
              subStatus sub `shouldBe` 200
              over <- rawLive loop (liveTarget hash) []
              respStatus over `shouldBe` 503
              problemTypeOf over `shouldSatisfy` maybe False (T.isSuffixOf "live-over-capacity")

    describe "reauth (§12: a connection MUST NOT outlive its proof)" $ do
      it "at proof expiry the origin sends {\"kind\":\"reauth\"} and terminates after the grace" $ do
        clock <- newTVarIO 1700000000
        let verifier = hmacVerifier liveSecret (readTVarIO clock)
        withLoop
          ( liveSpec
              { lsVerifier = Just verifier
              , lsTweak = \c ->
                  c
                    { ocNow = readTVarIO clock
                    , ocLive = defaultLiveConfig {liveReauthGraceMicros = 0}
                    }
              }
          )
          $ \loop -> do
            hash <- introduceLive loop liveTitleQ
            now <- readTVarIO clock
            -- exp == now admits the request but the lifetime is already
            -- spent: the reauth cycle fires deterministically, no timers.
            let payload = encodeClaims liveClaims
                proof = hmacProof liveSecret payload (floor now)
                target =
                  liveTarget hash
                    <> "&vc="
                    <> encodeUtf8 payload
            withSub loop target [("X-Vc-Auth", encodeUtf8 proof)] $ \sub -> do
              subStatus sub `shouldBe` 200
              _ <- nextFrame sub
              snapshot <- readBurst sub
              kindsOf snapshot `shouldBe` ["manifest", "entity", "end"]
              reauth <- nextEvent sub
              eventRecord reauth >>= (`shouldBe` RReauth)
              -- Grace 0, no fresh proof: the stream ends.
              io "stream termination after reauth" (subNext sub) >>= (`shouldBe` Nothing)


-- ---------------------------------------------------------------------------
-- Fixture: the derived blog (posts, comments, addComment mutation)
-- ---------------------------------------------------------------------------

{- | The §3.7 derived fixture doubles as the live fixture: @addComment@
declares @writes Comment(new), Post.comments(Comment.postId)@, which is
exactly the intersecting write a live subscription re-executes on.
-}
liveSpec :: LoopSpec
liveSpec = (loopSpec derivedSchema) {lsHooks = liveHooks Nothing, lsRows = liveRows}


withLive :: (Loop -> IO a) -> IO a
withLive = withLoop liveSpec


-- | Same fixture with an execution counter on the @post@ root loader:
-- one root-loader call per plan execution.
withLiveCounting :: TVar Int -> (Loop -> IO a) -> IO a
withLiveCounting execs =
  withLoop (loopSpec derivedSchema) {lsHooks = liveHooks (Just execs), lsRows = liveRows}


liveRows :: [(TypeName, Map FieldName A.Value)]
liveRows =
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


liveHooks :: Maybe (TVar Int) -> MemoryHooks
liveHooks mCounter =
  defaultHooks
    { mhGetRoots = Map.fromList [("post", postRoot)]
    , mhMutations = Map.fromList [("addComment", addCommentEffect)]
    }
  where
    postRoot _db args = do
      case mCounter of
        Just c -> atomically (modifyTVar' c (+ 1))
        Nothing -> pure ()
      pure $ case Map.lookup "id" args of
        Just (A.String k) -> Just (Ref "Post" k)
        _ -> Nothing


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


-- | Selects the paginated collection: membership grows on addComment.
livePageQ :: Text
livePageQ = "query LiveFeed { post(id: \"p1\") { title comments(first: 10) { body } } }"


-- | Selects the post alone: membership never changes.
liveTitleQ :: Text
liveTitleQ = "query LiveTitle { post(id: \"p1\") { title } }"


liveSecret :: ByteString
liveSecret = "live-secret"


liveClaims :: Claims
liveClaims = Map.singleton "org" (A.String "org-1")


-- ---------------------------------------------------------------------------
-- In-process SSE plumbing
-- ---------------------------------------------------------------------------

-- | An open live subscription: status, headers, and a frame-at-a-time
-- popper over the response body (wireform-http's SSE parser).
data Sub = Sub
  { subStatus :: Int
  , subHeaders :: [(HeaderName, ByteString)]
  , subNext :: IO (Maybe SseFrame)
  }


-- | A drained non-streaming answer (the rejection paths).
data LiveResp = LiveResp
  { respStatus :: Int
  , respBody :: ByteString
  }


mkLiveReq :: ByteString -> [(HeaderName, ByteString)] -> Request
mkLiveReq target headers =
  Request
    { requestMethod = GET
    , requestTarget = target
    , requestAuthority = Just "live.test"
    , requestScheme = SchemeHttp
    , requestHeaders = ("Accept", "text/event-stream") : headers
    , requestBody = BodyEmpty
    , requestVersion = V.HTTP1_1
    , requestTrailers = pure []
    }


{- | Subscribe in-process: call 'latticeHandler' directly and hand the
body popper to the SSE parser. The callback owns the connection; tests
pull frames with 'nextFrame'\/'readBurst' under loud timeouts.
-}
withSub :: Loop -> ByteString -> [(HeaderName, ByteString)] -> (Sub -> IO a) -> IO a
withSub loop target headers k = do
  resp <- io "live subscription head" (latticeHandler (loopOrigin loop) (mkLiveReq target headers))
  next <- case responseBody resp of
    -- The server-side body popper signals EOF with 'Nothing'; the client
    -- 'Popper' the SSE parser consumes signals it with the empty chunk.
    BodyStream pop -> sseFramePopper (fromMaybe BS8.empty <$> pop)
    BodyBytes bs -> do
      -- A non-streaming answer still surfaces as frames so error-path
      -- tests can assert through one shape.
      ref <- newTVarIO (parseEventStream bs)
      pure $ atomically $ do
        frames <- readTVar ref
        case frames of
          [] -> pure Nothing
          f : fs -> writeTVar ref fs >> pure (Just f)
    BodyEmpty -> pure (pure Nothing)
  k
    Sub
      { subStatus = fromIntegral (statusCode (responseStatus resp))
      , subHeaders = responseHeaders resp
      , subNext = next
      }


-- | One-shot request for the rejection paths (400/404/503): status + body.
rawLive :: Loop -> ByteString -> [(HeaderName, ByteString)] -> IO LiveResp
rawLive loop target headers = do
  resp <- io "live rejection" (latticeHandler (loopOrigin loop) (mkLiveReq target headers))
  body <- case responseBody resp of
    BodyEmpty -> pure BS8.empty
    BodyBytes bs -> pure bs
    BodyStream pop -> drain pop []
  pure LiveResp {respStatus = fromIntegral (statusCode (responseStatus resp)), respBody = body}
  where
    drain pop acc =
      pop >>= \case
        Nothing -> pure (BS8.concat (reverse acc))
        Just c -> drain pop (c : acc)


subHeader :: HeaderName -> Sub -> Maybe ByteString
subHeader name sub = lookupHeader name (subHeaders sub)


-- | The RFC 9457 @type@ of a problem body, when it decodes.
problemTypeOf :: LiveResp -> Maybe Text
problemTypeOf r = case A.decodeStrict (respBody r) of
  Just (A.Object o) | Just (A.String t) <- KM.lookup "type" o -> Just t
  _ -> Nothing


-- | The next frame, loudly bounded.
nextFrame :: Sub -> IO SseFrame
nextFrame sub =
  io "an SSE frame" (subNext sub)
    >>= maybe (expectationFailure "the event stream ended unexpectedly") pure


-- | The next dispatched event, skipping keep-alive comments.
nextEvent :: Sub -> IO ServerSentEvent
nextEvent sub =
  nextFrame sub >>= \case
    SseDispatch ev -> pure ev
    _ -> nextEvent sub


-- | Decode the single NDJSON record riding one event.
eventRecord :: ServerSentEvent -> IO Record
eventRecord ev = case rights (decodeRecords (sseData ev)) of
  [r] -> pure r
  other -> expectationFailure ("expected one record per event, got: " <> show other)


{- | Pull dispatched events (skipping comments) up to and including the
next @end@ record: one snapshot burst or one delta push, as
@(event id, record)@ pairs in stream order.
-}
readBurst :: Sub -> IO [(Maybe ByteString, Record)]
readBurst sub = go []
  where
    go acc = do
      ev <- nextEvent sub
      r <- eventRecord ev
      let acc' = (sseEventId ev, r) : acc
      case r of
        REnd _ -> pure (reverse acc')
        _ -> go acc'


commentText :: SseFrame -> Maybe Text
commentText = \case
  SseComment c -> Just (T.strip (T.pack (BS8.unpack c)))
  _ -> Nothing


recordsOf :: [(Maybe ByteString, Record)] -> [Record]
recordsOf = map snd


kindsOf :: [(Maybe ByteString, Record)] -> [Text]
kindsOf = map (kindOf . snd)


kindOf :: Record -> Text
kindOf = \case
  RManifest {} -> "manifest"
  REntity {} -> "entity"
  RTombstone {} -> "tombstone"
  RUnchanged {} -> "unchanged"
  REnd {} -> "end"
  RReauth -> "reauth"
  _ -> "other"


endEtagOf :: [(Maybe ByteString, Record)] -> Maybe Text
endEtagOf burst = case [e | REnd e <- recordsOf burst] of
  [e] -> endEtag e
  _ -> Nothing


dedup :: (Eq a) => [a] -> [a]
dedup = foldr (\x acc -> if x `elem` acc then acc else x : acc) []


-- ---------------------------------------------------------------------------
-- Requests
-- ---------------------------------------------------------------------------

-- | Memoize the query (ordinary introduce POST over the socket) and
-- return its hash.
introduceLive :: Loop -> Text -> IO Text
introduceLive loop q = do
  c <- mustCompileWith derivedSchema q
  r <-
    httpRaw
      loop
      POST
      "/q?intent=introduce&slice=pub"
      [("Content-Type", queryMediaType)]
      (Just (encodeUtf8 (compiledText c)))
  rawStatus r `shouldBe` 200
  pure (compiledHash c)


liveTarget :: Text -> ByteString
liveTarget hash = "/q/" <> encodeUtf8 hash <> "?slice=pub&live=sse"


addComment :: Loop -> Text -> Text -> Text -> IO LiveResp
addComment loop cid post body = do
  r <-
    httpRaw
      loop
      POST
      "/m/addComment"
      [("Content-Type", "application/json")]
      ( Just . BS8.toStrict . A.encode $
          A.object [("comment", A.String cid), ("post", A.String post), ("body", A.String body)]
      )
  pure LiveResp {respStatus = rawStatus r, respBody = rawBody r}
