{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Tests for the reconnecting EventSource client added to
"Network.HTTP.Client.SSE": 'withReconnectingSSE',
'ReconnectPolicy', and the per-attempt 'Last-Event-ID' header.

The harness is a mock 'Transport' that returns a different
canned response per call so we can simulate "first attempt drops
mid-stream, second attempt resumes from the recorded id".
-}
module Test.SSEReconnect (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
import Network.HTTP.Client.BodyStream qualified as BSm
import Network.HTTP.Client.Protocol (ProtocolInfo (..))
import Network.HTTP.Client.Request (Request, get)
import Network.HTTP.Client.Request qualified as WReq
import Network.HTTP.Client.Response qualified as Resp
import Network.HTTP.Client.SSE
import Network.HTTP.Client.Send (prepareRequest)
import Network.HTTP.Client.Transport
import Network.HTTP.Client.URI qualified as WURI
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Status qualified as S
import Test.Syd


-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

mkRequest :: IO (Request BSm.BodyStream)
mkRequest = case WURI.parseTemplate "http://example.com/sse" of
  Left e -> error (show e)
  Right t -> prepareRequest [] (get t)


eventStreamHeaders :: [(H.HeaderName, H.HeaderValue)]
eventStreamHeaders = [(H.hContentType, "text/event-stream")]


{- | A canned-responses transport that also records every
request's @Last-Event-ID@ header (if any) for assertions.
-}
mkTransport
  :: [(S.Status, [(H.HeaderName, H.HeaderValue)], ByteString)]
  -> IO (Transport IO, IORef [Maybe ByteString])
mkTransport script = do
  scriptRef <- newIORef script
  idsRef <- newIORef ([] :: [Maybe ByteString])
  let t = Transport $ \req -> do
        atomicModifyIORef' idsRef $ \xs ->
          (H.lookupHeader H.hLastEventID (WReq.headers req) : xs, ())
        next <- atomicModifyIORef' scriptRef $ \s -> case s of
          (x : rest) -> (rest, x)
          [] -> ([], (S.status204, eventStreamHeaders, ""))
        let (st, hdrs, body) = next
        popper <- BSm.popperFromStrict body
        pure
          Resp.RawResponse
            { Resp.statusCode = st
            , Resp.headers = hdrs
            , Resp.bodyPopper = popper
            , Resp.protocolInfo = HTTP1_1
            }
  pure (t, idsRef)


drainAt :: IO (Maybe ServerSentEvent) -> Int -> IO [ServerSentEvent]
drainAt pop n = go (0 :: Int) []
  where
    go i acc
      | i >= n = pure (reverse acc)
      | otherwise = do
          mev <- pop
          case mev of
            Nothing -> pure (reverse acc)
            Just ev -> go (i + 1) (ev : acc)


-- ---------------------------------------------------------------------------
-- The reconnect loop
-- ---------------------------------------------------------------------------

{- | First connection delivers 2 events with ids \"1\" and \"2\",
then EOF; second connection delivers 1 more event with id
\"3\" then EOF; third connection returns @204 No Content@
which terminates the reconnect loop.
-}
sseScript :: [(S.Status, [(H.HeaderName, H.HeaderValue)], ByteString)]
sseScript =
  [
    ( S.status200
    , eventStreamHeaders
    , "id: 1\ndata: hello\n\nid: 2\ndata: world\n\n"
    )
  ,
    ( S.status200
    , eventStreamHeaders
    , "id: 3\ndata: again\n\n"
    )
  , (S.status204, eventStreamHeaders, "")
  ]


unit_resumes_with_last_event_id :: Spec
unit_resumes_with_last_event_id = it
  "reconnect attaches Last-Event-ID from the most recent id"
  $ do
    (t, idsRef) <- mkTransport sseScript
    req <- mkRequest
    let policy = defaultReconnectPolicy {rpInitialRetryMs = 1}
    evs <- withReconnectingSSE t req policy $ \pop -> drainAt pop 5
    (length evs) `shouldBe` 3
    (sseEventId (evs !! 0)) `shouldBe` (Just "1")
    (sseEventId (evs !! 1)) `shouldBe` (Just "2")
    (sseEventId (evs !! 2)) `shouldBe` (Just "3")
    -- Three connection attempts: first carries no Last-Event-ID;
    -- second carries "2" (the most recent id from connection 1);
    -- third carries "3".
    ids <- reverse <$> readIORef idsRef
    (length ids) `shouldBe` 3
    (ids !! 0) `shouldBe` Nothing
    (ids !! 1) `shouldBe` (Just "2")
    (ids !! 2) `shouldBe` (Just "3")


unit_commits_id_only_blocks :: Spec
unit_commits_id_only_blocks = it "carries ID-only updates across connections and discards an unfinished ID" $ do
  (t, idsRef) <- mkTransport
    [ (S.status200, eventStreamHeaders, "id: ready\r\n\r\nid: unfinished\r\n")
    , (S.status200, eventStreamHeaders, "data: next\n\n")
    , (S.status204, [], "")
    ]
  req <- mkRequest
  events <- withReconnectingSSE t req defaultReconnectPolicy{rpInitialRetryMs = 0} $ \pop -> drainAt pop 5
  map sseEventId events `shouldBe` [Just "ready"]
  reverse <$> readIORef idsRef `shouldReturn` [Nothing, Just "ready", Just "ready"]


unit_empty_id_clears_seed :: Spec
unit_empty_id_clears_seed = it "removes Last-Event-ID after a blank-line-terminated empty ID" $ do
  (t, idsRef) <- mkTransport
    [ (S.status200, eventStreamHeaders, "data: seeded\n\n")
    , (S.status200, eventStreamHeaders, "id:\n\nid: unfinished\n")
    , (S.status200, eventStreamHeaders, "data: reset\n\n")
    , (S.status204, [], "")
    ]
  req <- WReq.setHeader H.hLastEventID "seed" <$> mkRequest
  events <- withReconnectingSSE t req defaultReconnectPolicy{rpInitialRetryMs = 0} $ \pop -> drainAt pop 5
  map sseEventId events `shouldBe` [Just "seed", Just ""]
  reverse <$> readIORef idsRef `shouldReturn` [Just "seed", Just "seed", Nothing, Nothing]


unit_repairs_utf8_in_cursor :: Spec
unit_repairs_utf8_in_cursor = it "replays the decoded UTF-8 ID and ignores a later ID containing NUL" $ do
  let raw = "id: " <> BS.pack [0xF0, 0x9F] <> "\n\nid: invalid\0id\ndata: event\n\n"
      replacement = BS.pack [0xEF, 0xBF, 0xBD]
  (t, idsRef) <- mkTransport [(S.status200, eventStreamHeaders, raw), (S.status204, [], "")]
  req <- mkRequest
  events <- withReconnectingSSE t req defaultReconnectPolicy{rpInitialRetryMs = 0} $ \pop -> drainAt pop 5
  map sseEventId events `shouldBe` [Just replacement]
  reverse <$> readIORef idsRef `shouldReturn` [Nothing, Just replacement]


unit_ignores_unusable_resume_seed :: Spec
unit_ignores_unusable_resume_seed = it "omits malformed, repeated or connection-scoped caller cursors" $ do
  req <- mkRequest
  let seeds =
        [ ([(H.hLastEventID, BS.singleton 0xFF)], Just "learned")
        , ([(H.hLastEventID, "bad\x0b\&cursor")], Just "learned")
        , ([(H.hLastEventID, "first"), (H.hLastEventID, "second")], Just "learned")
        , ([(H.hLastEventID, "local"), (H.hConnection, "last-event-id")], Nothing)
        ]
  mapM_ (\(fields, replay) -> do
    (t, idsRef) <- mkTransport
      [ (S.status200, eventStreamHeaders, "id: learned\n\n")
      , (S.status204, [], "")
      ]
    _ <- withReconnectingSSE t req{WReq.headers = fields}
      defaultReconnectPolicy{rpInitialRetryMs = 0} $ \pop -> drainAt pop 1
    reverse <$> readIORef idsRef `shouldReturn` [Nothing, replay]) seeds


unit_204_stops :: Spec
unit_204_stops = it
  "204 No Content from the first attempt stops reconnecting"
  $ do
    (t, idsRef) <-
      mkTransport
        [(S.status204, eventStreamHeaders, "")]
    req <- mkRequest
    let policy = defaultReconnectPolicy {rpInitialRetryMs = 1}
    _ <- withReconnectingSSE t req policy $ \pop -> drainAt pop 5
    ids <- readIORef idsRef
    -- We don't try a reconnect after a 204.
    (length ids) `shouldBe` 1


unit_max_attempts :: Spec
unit_max_attempts = it
  "rpMaxAttempts stops the loop after N consecutive empty attempts"
  $ do
    -- Every attempt returns an immediate-EOF stream (no events),
    -- so failCount climbs by 1 per attempt.
    (t, idsRef) <-
      mkTransport
        [(S.status200, eventStreamHeaders, "") | _ <- [(1 :: Int) .. 10]]
    req <- mkRequest
    let policy =
          defaultReconnectPolicy
            { rpInitialRetryMs = 1
            , rpMaxAttempts = Just 3
            }
    _ <- withReconnectingSSE t req policy $ \pop -> drainAt pop 1
    -- The consumer's popper only sees Nothing once the worker
    -- signals stop — by which time the worker has already
    -- incremented idsRef for every attempt it made.
    ids <- readIORef idsRef
    (if (length ids <= 3) then pure () else expectationFailure ("attempts " <> show (length ids) <> " <= 3"))


-- ---------------------------------------------------------------------------
-- Top-level
-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "Network.HTTP.Client.SSE.Reconnect" $
    sequence_
      [ unit_resumes_with_last_event_id
      , unit_commits_id_only_blocks
      , unit_empty_id_clears_seed
      , unit_repairs_utf8_in_cursor
      , unit_ignores_unusable_resume_seed
      , unit_204_stops
      , unit_max_attempts
      ]
