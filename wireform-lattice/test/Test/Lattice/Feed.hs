{- | The federation invalidation feed (spec §18.6):
@GET \/invalidations?since={cursor}&live=sse@ over the in-process
loopback handler.

Transport mirrors "Test.Lattice.Live": tests call 'latticeHandler'
directly (no sockets) so the SSE body popper can be pulled
frame-by-frame with the wireform-http SSE parser; rejection paths ride
'httpRaw' over the real loopback socket. Synchronization is entirely on
the popper and the invalidation bus under the suite's loud 'io'
timeouts — no sleeps. Ordering is made deterministic with the sentinel
pattern: after the head of a subscription is answered the registration
is live, so a 'publishPurge' sentinel bounds "nothing else arrived".

Coverage:

* Surface: @live=sse@ answers @200 text\/event-stream@ with
  @Cache-Control: no-store@; the polling form (no @live=sse@) is the
  pinned 400 (out of scope per §18.6).
* Events: a mutation through the ordinary path arrives as ONE event —
  a JSON object @{"cursor": n, "keys": [...]}@ whose SSE @id:@ equals
  the cursor; keys are the write set's surrogate keys; cursors are
  monotone across mutations.
* Replay: @since={c}@ replays exactly the retained events with cursor
  @> c@ before the live tail; @since@ at the newest cursor replays
  nothing; @since@ absent starts at the live tail.
* Outrun (§18.6 pin): a consumer whose first replayed cursor exceeds
  @since + 1@ knows the retention window was outrun — driven with a
  synthetic 'publishPurge' flood past the log bound.
-}
module Test.Lattice.Feed (tests) where

import Control.Concurrent.STM
import Control.Monad (forM_)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Lattice.Backend (CommitResult (..), MutationOutcome (..), WriteFact (..), internalError)
import Lattice.Backend.Memory (MemoryDb, MemoryHooks (..), defaultHooks, putRow, snapshotToken)
import Lattice.Schema (Schema)
import Lattice.Server (latticeHandler, publishPurge)
import Lattice.Types
import Network.HTTP.Client.SSE (ServerSentEvent (..), SseFrame (..), parseEventStream, sseFramePopper)
import Network.HTTP.Message (Request (..), Response (..), Scheme (..))
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Types.Header (HeaderName, hCacheControl, hContentType, lookupHeader)
import Network.HTTP.Types.Method (Method (..))
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Version qualified as V
import Test.Lattice.Fixtures (mustParseSchema)
import Test.Lattice.Loop
import Test.Syd


tests :: Spec
tests =
  describe "The invalidation feed (§18.6)" $ do
    describe "subscription surface" $ do
      it "live=sse answers 200 text/event-stream, Cache-Control: no-store" $
        withFeed $ \loop ->
          withFeedSub loop "/invalidations?live=sse" $ \sub -> do
            fsStatus sub `shouldBe` 200
            fsHeader hContentType sub
              `shouldSatisfy` maybe False ("text/event-stream" `BS8.isPrefixOf`)
            fsHeader hCacheControl sub `shouldBe` Just "no-store"

      it "without live=sse the polling form is the pinned 400" $
        withFeed $ \loop -> do
          r <- httpRaw loop GET "/invalidations" [] Nothing
          rawStatus r `shouldBe` 400
          rawProblemType r
            `shouldSatisfy` maybe False ("lattice.dev/problems/" `T.isInfixOf`)

      it "without live=sse a since parameter alone still does not subscribe" $
        withFeed $ \loop -> do
          r <- httpRaw loop GET "/invalidations?since=0" [] Nothing
          rawStatus r `shouldBe` 400

    describe "the live tail" $ do
      it "a mutation through the ordinary path is one event: write-set keys, id = cursor" $
        withFeed $ \loop ->
          withFeedSub loop "/invalidations?live=sse" $ \sub -> do
            renameNote loop "n1" "Renamed"
            ev <- nextFeedEvent sub
            feKeys ev `shouldSatisfy` elem "Note:n1"
            feId ev `shouldBe` Just (BS8.pack (show (feCursor ev)))

      it "cursors are monotone across consecutive mutations" $
        withFeed $ \loop ->
          withFeedSub loop "/invalidations?live=sse" $ \sub -> do
            renameNote loop "n1" "One"
            e1 <- nextFeedEvent sub
            renameNote loop "n1" "Two"
            e2 <- nextFeedEvent sub
            feCursor e2 `shouldBe` feCursor e1 + 1

      it "absent since starts at the live tail: earlier events never replay" $
        withFeed $ \loop -> do
          renameNote loop "n1" "Before"
          withFeedSub loop "/invalidations?live=sse" $ \sub -> do
            publishPurge (loopOrigin loop) ["sentinel:tail"]
            ev <- nextFeedEvent sub
            feKeys ev `shouldBe` ["sentinel:tail"]

    describe "since replay (§18.6)" $ do
      it "since={c} replays exactly the events after c, then continues live" $
        withFeed $ \loop -> do
          c1 <- withFeedSub loop "/invalidations?live=sse" $ \sub -> do
            renameNote loop "n1" "One"
            e1 <- nextFeedEvent sub
            renameNote loop "n1" "Two"
            e2 <- nextFeedEvent sub
            feCursor e2 `shouldBe` feCursor e1 + 1
            pure (feCursor e1)
          withFeedSub loop (sinceTarget c1) $ \sub -> do
            replayed <- nextFeedEvent sub
            feCursor replayed `shouldBe` c1 + 1
            feKeys replayed `shouldSatisfy` elem "Note:n1"
            -- The sentinel arrives NEXT: nothing rode between the end of
            -- the replay and the live tail.
            publishPurge (loopOrigin loop) ["sentinel:replay"]
            tailEv <- nextFeedEvent sub
            feKeys tailEv `shouldBe` ["sentinel:replay"]

      it "since at the newest cursor replays nothing" $
        withFeed $ \loop -> do
          newest <- withFeedSub loop "/invalidations?live=sse" $ \sub -> do
            renameNote loop "n1" "Only"
            feCursor <$> nextFeedEvent sub
          withFeedSub loop (sinceTarget newest) $ \sub -> do
            publishPurge (loopOrigin loop) ["sentinel:empty"]
            ev <- nextFeedEvent sub
            feKeys ev `shouldBe` ["sentinel:empty"]

    describe "the outrun rule (§18.6 pin)" $ do
      it "a first replayed cursor exceeding since+1 shows the window was outrun" $
        withFeed $ \loop -> do
          -- Overrun the retention window with a synthetic flood: cursors
          -- 1..floodSize are published, the oldest fall off the log.
          forM_ [1 .. floodSize] $ \i ->
            publishPurge (loopOrigin loop) ["flood:" <> T.pack (show (i :: Int))]
          withFeedSub loop (sinceTarget 0) $ \sub -> do
            first <- nextFeedEvent sub
            -- The consumer's rule: first replayed cursor > since + 1 =>
            -- the gap is untrustworthy, resync from scratch.
            feCursor first `shouldSatisfy` (> 1)
            -- The replay itself is contiguous from that point: the window
            -- holds exactly the newest events up to the flood's cursor.
            rest <- drainReplay sub (feCursor first) (fromIntegral floodSize)
            rest `shouldBe` [feCursor first .. fromIntegral floodSize]


-- | Comfortably past the origin's 4096-event retention bound.
floodSize :: Int
floodSize = 4200


-- ---------------------------------------------------------------------------
-- Fixture: one entity, one mutation writing it
-- ---------------------------------------------------------------------------

feedText :: Text
feedText =
  T.unlines
    [ "schema feed.example.com"
    , ""
    , "newtype NoteId = Text"
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
    , "get note(id: NoteId) of Note public"
    , ""
    , "mutation renameNote(note: NoteId, title: Text) returns Note {"
    , "  allow       public"
    , "  writes      Note(note)"
    , "  invalidates writes"
    , "  effect      transactional"
    , "}"
    ]


feedSchema :: Schema
feedSchema = mustParseSchema feedText
{-# NOINLINE feedSchema #-}


withFeed :: (Loop -> IO a) -> IO a
withFeed =
  withLoop
    (loopSpec feedSchema)
      { lsHooks = feedHooks
      , lsRows = [("Note", Map.fromList [("id", A.String "n1"), ("title", A.String "First")])]
      }


feedHooks :: MemoryHooks
feedHooks = defaultHooks {mhMutations = Map.fromList [("renameNote", renameEffect)]}


renameEffect :: MemoryDb -> Claims -> Map ArgName A.Value -> IO MutationOutcome
renameEffect db _claims args =
  case (Map.lookup "note" args, Map.lookup "title" args) of
    (Just (A.String nid), Just title@(A.String _)) -> atomically $ do
      putRow db "Note" nid (Map.fromList [("id", A.String nid), ("title", title)])
      tok <- snapshotToken db
      pure . MutationCommitted $
        CommitResult
          { crResult = [Ref "Note" nid]
          , crWrites = [WroteEntity (Ref "Note" nid)]
          , crSnapshot = tok
          }
    _ -> pure (MutationFailed (internalError (Just "renameNote: note and title arguments required")))


-- | The mutation, through the ordinary HTTP path.
renameNote :: Loop -> Text -> Text -> IO ()
renameNote loop nid title = do
  r <-
    httpRaw
      loop
      POST
      "/m/renameNote"
      [("Content-Type", "application/json")]
      ( Just . BS8.toStrict . A.encode $
          A.object [("note", A.String nid), ("title", A.String title)]
      )
  rawStatus r `shouldBe` 200


-- ---------------------------------------------------------------------------
-- In-process SSE plumbing (the Test.Lattice.Live pattern)
-- ---------------------------------------------------------------------------

-- | An open feed subscription: status, headers, and a frame-at-a-time
-- popper over the response body (wireform-http's SSE parser).
data FeedSub = FeedSub
  { fsStatus :: Int
  , fsHeaders :: [(HeaderName, ByteString)]
  , fsNext :: IO (Maybe SseFrame)
  }


-- | One decoded feed event: @{"cursor": n, "keys": [...]}@ plus the SSE
-- event id it rode with.
data FeedEvent = FeedEvent
  { feCursor :: Word64
  , feKeys :: [Text]
  , feId :: Maybe ByteString
  }
  deriving stock (Show)


mkFeedReq :: ByteString -> Request
mkFeedReq target =
  Request
    { requestMethod = GET
    , requestTarget = target
    , requestAuthority = Just "feed.test"
    , requestScheme = SchemeHttp
    , requestHeaders = [("Accept", "text/event-stream")]
    , requestBody = BodyEmpty
    , requestVersion = V.HTTP1_1
    , requestTrailers = pure []
    }


{- | Subscribe in-process: call 'latticeHandler' directly and hand the
body popper to the SSE parser. Once the head is answered the
registration is live, so subsequent 'publishPurge' calls are observable.
-}
withFeedSub :: Loop -> ByteString -> (FeedSub -> IO a) -> IO a
withFeedSub loop target k = do
  resp <- io "feed subscription head" (latticeHandler (loopOrigin loop) (mkFeedReq target))
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
    FeedSub
      { fsStatus = fromIntegral (statusCode (responseStatus resp))
      , fsHeaders = responseHeaders resp
      , fsNext = next
      }


fsHeader :: HeaderName -> FeedSub -> Maybe ByteString
fsHeader name sub = lookupHeader name (fsHeaders sub)


sinceTarget :: Word64 -> ByteString
sinceTarget c = "/invalidations?since=" <> BS8.pack (show c) <> "&live=sse"


-- | The next dispatched event, skipping keep-alive comments, loudly
-- bounded.
nextFeedEvent :: FeedSub -> IO FeedEvent
nextFeedEvent sub = io "a feed event" go
  where
    go =
      fsNext sub >>= \case
        Nothing -> expectationFailure "the feed stream ended unexpectedly"
        Just (SseDispatch ev) -> decodeFeedEvent ev
        Just _ -> go


-- | Decode the pinned §18.6 event shape.
decodeFeedEvent :: ServerSentEvent -> IO FeedEvent
decodeFeedEvent ev = case A.decodeStrict (sseData ev) of
  Just (A.Object o)
    | Just cursor <- KM.lookup "cursor" o >>= asWord
    , Just (A.Array ks) <- KM.lookup "keys" o
    , Just keys <- traverse asString (toList ks) ->
        pure FeedEvent {feCursor = cursor, feKeys = keys, feId = sseEventId ev}
  _ -> expectationFailure ("not a §18.6 feed event: " <> show ev)
  where
    asWord = \case
      A.Number n | n >= 0 -> Just (round n)
      _ -> Nothing
    asString = \case
      A.String t -> Just t
      _ -> Nothing


-- | Drain a replay: cursors from the first replayed one through the
-- expected end, in arrival order.
drainReplay :: FeedSub -> Word64 -> Word64 -> IO [Word64]
drainReplay sub start end = go [start]
  where
    go acc@(newest : _)
      | newest >= end = pure (reverse acc)
      | otherwise = do
          ev <- nextFeedEvent sub
          go (feCursor ev : acc)
    go [] = pure []
