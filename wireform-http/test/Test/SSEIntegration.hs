{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | End-to-end SSE tests: a real HTTP\/1.1 server using
-- 'sseResponseBody' + 'SseChannel' on one side, the high-level
-- 'withSSE' \/ 'withSSEFrames' client over
-- 'streamedTransport' on the other.
--
-- These exist because the unit + property tests in "Test.SSE" only
-- exercise the parser and the in-process mocks. The whole point of
-- the server-side push API is that one popper call ends up as one
-- chunked-transfer chunk on the wire, gets flushed promptly, and
-- the client's parser dispatches it as a single event — none of
-- which is exercised without a real server, a real socket, and the
-- real HTTP\/1.1 chunked encoder \/ decoder in between.
module Test.SSEIntegration (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
  (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, finally, fromException, try, SomeException)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import Data.IORef
import Control.Monad (forM, forM_, replicateM_)
import qualified Network.Socket as NS

import Test.Tasty
import Test.Tasty.HUnit

import qualified Network.HTTP.Types.Header  as H
import qualified Network.HTTP.Types.Status  as S
import qualified Network.HTTP.Types.Version as V
import qualified Network.HTTP.Message       as Msg
import qualified Network.HTTP.Types.Body    as TB
import Network.HTTP.Server
import Network.HTTP.VersionRange (VersionRange, http1Only, http2Only)

import Network.HTTP.Client

tests :: TestTree
tests = testGroup "SSE end-to-end (HTTP/1.1)"
  [ basicRoundTrip
  , heartbeatsAndEvents
  , manyEventsOnOneConnection
  , largeDataPayload
  , veryLongStream
  , mixedSizeEvents
  , concurrentClients
  , earlyCancellation
  , tinyChunkStream
  , backToBackRequests
  , extremelyLongStream
  , highConcurrency
  , serverError5xx
  , serverWrongContentType
  , sseOverHttp2
  , producerCrashGraceful
  , extremeBackpressure
  ]

-- ---------------------------------------------------------------------------
-- Stress tests
-- ---------------------------------------------------------------------------

-- | Push a /lot/ of events through one connection. Catches O(n²)
-- behaviour in the per-event encoder \/ decoder, slow ring
-- starvation, and any leak in the streamed-transport worker that
-- only shows up over a long stream.
veryLongStream :: TestTree
veryLongStream = testCase "50,000 events on one connection" $ do
  let n   = 50000
      evs = map mkEvent [0 .. n - 1]
      mkEvent i = defaultSseEvent
        { sseEventId = Just (BS8.pack (show i))
        , sseData    = BS8.pack ("payload-" <> show i)
        }
  withSseServer (pushAllAndClose evs) $ \port -> do
    received <- newIORef ([] :: [ServerSentEvent])
    let req       = get (compileURL port "/events")
        transport = streamedTransport http1Only
    withSSE transport req $ \nextEvent ->
      drainEvents nextEvent received
    got <- reverse <$> readIORef received
    length got @?= n
    -- Don't @?= the full list (the assertion failure would be
    -- unreadable); spot-check the boundaries + a quartile-grid
    -- of interior indices instead, which catches drift /
    -- off-by-one / id-reordering.
    head got               @?= head evs
    last got               @?= last evs
    (got !! (n `div` 4))   @?= (evs !! (n `div` 4))
    (got !! (n `div` 2))   @?= (evs !! (n `div` 2))
    (got !! (3 * n `div` 4)) @?= (evs !! (3 * n `div` 4))

-- | Three event sizes interleaved (10 B, 500 B, 10 KiB) to
-- exercise the chunked-decoder's per-call 16 KiB read boundary
-- and the SSE parser's @data:@ line accumulator across very
-- different chunk lengths.
mixedSizeEvents :: TestTree
mixedSizeEvents = testCase "mixed-size events round-trip in order" $ do
  let evs = concatMap one [0 .. 19 :: Int]
      one i =
        [ defaultSseEvent { sseData = "s" <> BS8.pack (show i) }
        , defaultSseEvent { sseData = BS.replicate 500   0x41 <> BS8.pack (show i) }
        , defaultSseEvent { sseData = BS.replicate 10000 0x42 <> BS8.pack (show i) }
        ]
  withSseServer (pushAllAndClose evs) $ \port -> do
    received <- newIORef ([] :: [ServerSentEvent])
    let req       = get (compileURL port "/events")
        transport = streamedTransport http1Only
    withSSE transport req $ \nextEvent ->
      drainEvents nextEvent received
    got <- reverse <$> readIORef received
    got @?= evs

-- | Five clients dialing the same handler in parallel; each one
-- should receive the full event sequence in order. The handler
-- builds a fresh 'SseChannel' + producer for every request, so
-- nothing is shared across clients; this is a check that the
-- streamed-transport worker, the HTTP\/1.x server, and the SSE
-- parser don't have any cross-connection state pollution.
concurrentClients :: TestTree
concurrentClients = testCase "20 concurrent SSE clients each receive the full stream" $ do
  let perClient   = 500
      numClients  = 20 :: Int
      evs         = map (\i -> defaultSseEvent
                                  { sseData = BS8.pack ("e" <> show i) })
                        [0 .. perClient - 1]
  withSseServer (pushAllAndClose evs) $ \port -> do
    slots <- mapM (\_ -> newEmptyMVar) [1 .. numClients]
    forM_ slots $ \mv -> forkIO $ do
      received <- newIORef ([] :: [ServerSentEvent])
      let req       = get (compileURL port "/events")
          transport = streamedTransport http1Only
      withSSE transport req $ \nextEvent ->
        drainEvents nextEvent received
      got <- reverse <$> readIORef received
      putMVar mv got
    results <- mapM takeMVar slots
    forM_ (zip [0 :: Int ..] results) $ \(i, got) -> do
      assertEqual ("client " <> show i <> " event count")
        perClient (length got)
      assertEqual ("client " <> show i <> " full sequence")
        evs got

-- | The 'withSSE' callback returns after reading just a handful of
-- events. The streamedTransport's worker should observe that EOF
-- propagates back through the popper (because we cancelled the
-- response on our way out) and exit cleanly — no hang, no leak.
-- The test passes if it terminates with the correct partial count.
earlyCancellation :: TestTree
earlyCancellation = testCase "early return from withSSE doesn't hang" $ do
  let evs = map (\i -> defaultSseEvent
                          { sseData = BS8.pack ("e" <> show i) })
                [0 .. 999 :: Int]
  withSseServer (pushAllAndClose evs) $ \port -> do
    received <- newIORef ([] :: [ServerSentEvent])
    let req       = get (compileURL port "/events")
        transport = streamedTransport http1Only
    withSSE transport req $ \nextEvent ->
      replicateM_ 5 $ do
        mev <- nextEvent
        case mev of
          Just ev -> modifyIORef' received (ev :)
          Nothing -> pure ()
    got <- reverse <$> readIORef received
    length got @?= 5

-- | 5,000 events whose @data:@ payload is a single byte each. Worst
-- case for the chunk-framing overhead ratio (event is ~12 wire bytes
-- with @id:@, body is 1) and a hard exercise of the chunk-size-line
-- reader (~5,000 size lines back-to-back on one connection).
tinyChunkStream :: TestTree
tinyChunkStream = testCase "5,000 single-byte-data events" $ do
  let n   = 5000
      evs = map mkEvent [0 .. n - 1]
      mkEvent i = defaultSseEvent
        { sseEventId = Just (BS8.pack (show i))
        , sseData    = BS.singleton (fromIntegral (0x21 + (i `mod` 94)))
                       -- printable ASCII, varying so a byte-for-byte
                       -- shift would show up.
        }
  withSseServer (pushAllAndClose evs) $ \port -> do
    received <- newIORef ([] :: [ServerSentEvent])
    let req       = get (compileURL port "/events")
        transport = streamedTransport http1Only
    withSSE transport req $ \nextEvent ->
      drainEvents nextEvent received
    got <- reverse <$> readIORef received
    length got @?= n
    head got        @?= head evs
    last got        @?= last evs
    (got !! (n `div` 2)) @?= (evs !! (n `div` 2))

-- | Three SSE responses one after the other against the same
-- localhost server. Each request gets a fresh 'streamedTransport'
-- (because that's the lifetime model — each request owns its own
-- connection), but the server's accept loop is shared. Catches any
-- state pollution in the server's per-connection handler when a
-- streaming response goes through.
backToBackRequests :: TestTree
backToBackRequests = testCase "3 sequential SSE requests against the same server" $ do
  let runs =
        [ map (\i -> defaultSseEvent { sseData = BS8.pack ("a" <> show i) })
              [0 .. 99  :: Int]
        , map (\i -> defaultSseEvent { sseData = BS8.pack ("b" <> show i) })
              [0 .. 199 :: Int]
        , map (\i -> defaultSseEvent { sseData = BS8.pack ("c" <> show i) })
              [0 .. 299 :: Int]
        ]
  -- We can't easily reconfigure the producer between client
  -- connections (the handler closure is fixed); instead the
  -- handler always pushes whichever batch corresponds to the
  -- current request count.
  counter <- newIORef (0 :: Int)
  let producerFor ch = do
        idx <- atomicModifyIORef' counter (\n -> (n + 1, n))
        let evs = runs !! min idx (length runs - 1)
        pushAllAndClose evs ch
  withSseServer producerFor $ \port -> do
    gotAll <- forM runs $ \_expected -> do
      received <- newIORef ([] :: [ServerSentEvent])
      let req       = get (compileURL port "/events")
          transport = streamedTransport http1Only
      withSSE transport req $ \nextEvent ->
        drainEvents nextEvent received
      reverse <$> readIORef received
    gotAll @?= runs

-- | Push the envelope: 100,000 events on a single connection.
-- At ~25 bytes per event ≈ 2.5 MB of streaming response, ~100k
-- chunk-size lines, ~100k SSE-parser dispatches. If anything in
-- the pipeline is O(n²) on the event count, this will be where
-- it finally shows.
extremelyLongStream :: TestTree
extremelyLongStream = testCase "100,000 events on one connection" $ do
  let n   = 100000
      evs = map mkEvent [0 .. n - 1]
      mkEvent i = defaultSseEvent
        { sseEventId = Just (BS8.pack (show i))
        , sseData    = BS8.pack ("payload-" <> show i)
        }
  withSseServer (pushAllAndClose evs) $ \port -> do
    received <- newIORef ([] :: [ServerSentEvent])
    let req       = get (compileURL port "/events")
        transport = streamedTransport http1Only
    withSSE transport req $ \nextEvent ->
      drainEvents nextEvent received
    got <- reverse <$> readIORef received
    length got @?= n
    head got                     @?= head evs
    last got                     @?= last evs
    (got !! (n `div` 2))         @?= (evs !! (n `div` 2))
    (got !! (3 * n `div` 4))     @?= (evs !! (3 * n `div` 4))

-- | 50 concurrent clients each receiving a 1000-event stream
-- (50,000 events total flowing simultaneously). Catches any
-- contention or fairness bug in the server's accept loop, the
-- streamed-transport worker pool, or the per-connection
-- send-ring publishing.
highConcurrency :: TestTree
highConcurrency = testCase "50 concurrent clients x 1000 events each" $ do
  let perClient  = 1000
      numClients = 50 :: Int
      evs        = map (\i -> defaultSseEvent
                                { sseData = BS8.pack ("e" <> show i) })
                       [0 .. perClient - 1]
  withSseServer (pushAllAndClose evs) $ \port -> do
    slots <- mapM (\_ -> newEmptyMVar) [1 .. numClients]
    forM_ slots $ \mv -> forkIO $ do
      received <- newIORef ([] :: [ServerSentEvent])
      let req       = get (compileURL port "/events")
          transport = streamedTransport http1Only
      withSSE transport req $ \nextEvent ->
        drainEvents nextEvent received
      got <- reverse <$> readIORef received
      putMVar mv got
    results <- mapM takeMVar slots
    forM_ (zip [0 :: Int ..] results) $ \(i, got) -> do
      assertEqual ("client " <> show i <> " event count")
        perClient (length got)
      assertEqual ("client " <> show i <> " first event")
        (head evs) (head got)
      assertEqual ("client " <> show i <> " last event")
        (last evs) (last got)

-- | A handler that returns 500 should surface as
-- 'SseUnexpectedStatus' on the client — over a real wire, not
-- just against a mock.
serverError5xx :: TestTree
serverError5xx = testCase "server 500 surfaces as SseUnexpectedStatus" $ do
  let handler _ = pure Msg.Response
        { Msg.responseStatus     = S.status500
        , Msg.responseVersion    = V.HTTP1_1
        , Msg.responseHeaders    = [(H.hContentType, "text/event-stream")]
        , Msg.responseBody       = TB.BodyEmpty
        , Msg.responseTrailers   = pure []
        , Msg.responseH2StreamId = 0
        , Msg.responsePushPromises = pure []
        , Msg.responseCancel     = pure ()
        }
  withTestServer http1Only handler $ \port -> do
    let req       = get (compileURL port "/events")
        transport = streamedTransport http1Only
    result <- try $ withSSE transport req $ \_ -> pure ()
    case (result :: Either SomeException ()) of
      Left e -> case fromException e of
        Just (SseUnexpectedStatus s) -> S.statusCode s @?= 500
        _ -> assertFailure ("wrong exception: " <> show e)
      Right _ -> assertFailure "expected SseUnexpectedStatus"

-- | A handler whose @Content-Type@ is not @text/event-stream@
-- should surface as 'SseUnexpectedContentType' on the client.
serverWrongContentType :: TestTree
serverWrongContentType =
  testCase "server text/plain surfaces as SseUnexpectedContentType" $ do
  let handler _ = pure Msg.Response
        { Msg.responseStatus     = S.status200
        , Msg.responseVersion    = V.HTTP1_1
        , Msg.responseHeaders    = [(H.hContentType, "text/plain")]
        , Msg.responseBody       = TB.BodyBytes "not an event stream"
        , Msg.responseTrailers   = pure []
        , Msg.responseH2StreamId = 0
        , Msg.responsePushPromises = pure []
        , Msg.responseCancel     = pure ()
        }
  withTestServer http1Only handler $ \port -> do
    let req       = get (compileURL port "/events")
        transport = streamedTransport http1Only
    result <- try $ withSSE transport req $ \_ -> pure ()
    case (result :: Either SomeException ()) of
      Left e -> case fromException e of
        Just (SseUnexpectedContentType mt) -> do
          mtType mt    @?= "text"
          mtSubType mt @?= "plain"
        _ -> assertFailure ("wrong exception: " <> show e)
      Right _ -> assertFailure "expected SseUnexpectedContentType"

-- | Additional regression: a single event whose data payload is large.
-- Exercises the 'Wireform.Builder' accumulator in 'spDataBuf' across
-- many @data:@ field appends, and the chunked-body reader at the
-- per-chunk 16 KiB read boundary.
largeDataPayload :: TestTree
largeDataPayload = testCase "single event with 64 KiB data round-trips" $ do
  let bigLines = map (\i -> BS8.pack ("line-" <> show (i :: Int) <> "-"
                                  <> replicate 56 'x'))
                     [0 .. 999]
      payload  = BS.intercalate "\n" bigLines
      ev       = defaultSseEvent { sseData = payload }
  withSseServer (pushAllAndClose [ev]) $ \port -> do
    received <- newIORef ([] :: [ServerSentEvent])
    let req       = get (compileURL port "/events")
        transport = streamedTransport http1Only
    withSSE transport req $ \nextEvent ->
      drainEvents nextEvent received
    got <- reverse <$> readIORef received
    got @?= [ev]

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

basicRoundTrip :: TestTree
basicRoundTrip = testCase "events server -> client round-trip" $ do
  let evs =
        [ defaultSseEvent { sseEventType = Just "tick", sseEventId = Just "1", sseData = "first"  }
        , defaultSseEvent { sseEventType = Just "tick", sseEventId = Just "2", sseData = "second" }
        , defaultSseEvent { sseEventType = Just "done", sseEventId = Just "3", sseData = "bye"    }
        ]
  withSseServer (pushAllAndClose evs) $ \port -> do
    received <- newIORef ([] :: [ServerSentEvent])
    let req       = get (compileURL port "/events")
        transport = streamedTransport http1Only
    withSSE transport req $ \nextEvent ->
      drainEvents nextEvent received
    got <- reverse <$> readIORef received
    got @?= evs

heartbeatsAndEvents :: TestTree
heartbeatsAndEvents = testCase "withSSEFrames sees comments + retry + events" $ do
  let frames =
        [ SseComment " keepalive"
        , SseRetry 2500
        , SseDispatch (defaultSseEvent { sseData = "a" })
        , SseComment " still-alive"
        , SseDispatch (defaultSseEvent { sseData = "b" })
        ]
  -- Server must use the frame-aware popper, otherwise
  -- 'awaitSseEvent' would silently drop comments + retry on the
  -- way out.
  withSseFrameServer (pushFramesAndClose frames) $ \port -> do
    received <- newIORef ([] :: [SseFrame])
    let req       = get (compileURL port "/events")
        transport = streamedTransport http1Only
    withSSEFrames transport req $ \nextFrame ->
      drainFrames nextFrame received
    got <- reverse <$> readIORef received
    got @?= frames

manyEventsOnOneConnection :: TestTree
manyEventsOnOneConnection =
  testCase "1000 events stay in order across the wire" $ do
  -- 1000 is comfortably above every internal queue cap we thread
  -- through (streamedTransport: 32; SseChannel: 64; defaultChunkLineCap
  -- after enough small chunks pile up: 4 KiB). The third one was a real
  -- bug — see the commit fixing 'readChunkSizeLineFrom' to
  -- compare the cap against the size-line length itself rather
  -- than against ring occupancy. Pre-fix this test truncated
  -- non-deterministically around the 25–200 event mark.
  let evs = map mkEvent [0 .. 999 :: Int]
      mkEvent i = defaultSseEvent
        { sseEventId = Just (BS8.pack (show i))
        , sseData    = BS8.pack ("payload-" <> show i)
        }
  withSseServer (pushAllAndClose evs) $ \port -> do
    received <- newIORef ([] :: [ServerSentEvent])
    let req       = get (compileURL port "/events")
        transport = streamedTransport http1Only
    withSSE transport req $ \nextEvent ->
      drainEvents nextEvent received
    got <- reverse <$> readIORef received
    length got @?= length evs
    got @?= evs

-- ---------------------------------------------------------------------------
-- Server-side producers
-- ---------------------------------------------------------------------------

pushAllAndClose :: [ServerSentEvent] -> SseChannel -> IO ()
pushAllAndClose evs ch = do
  forM_ evs (sendSseEvent ch)
  closeSseChannel ch

pushFramesAndClose :: [SseFrame] -> SseChannel -> IO ()
pushFramesAndClose fs ch = do
  forM_ fs (sendSseFrame ch)
  closeSseChannel ch

-- ---------------------------------------------------------------------------
-- Consumer drains
-- ---------------------------------------------------------------------------

drainEvents :: IO (Maybe ServerSentEvent) -> IORef [ServerSentEvent] -> IO ()
drainEvents nextEvent ref = loop
  where
    loop = nextEvent >>= \case
      Nothing -> pure ()
      Just ev -> do
        modifyIORef' ref (ev :)
        loop

drainFrames :: IO (Maybe SseFrame) -> IORef [SseFrame] -> IO ()
drainFrames nextFrame ref = loop
  where
    loop = nextFrame >>= \case
      Nothing -> pure ()
      Just f  -> do
        modifyIORef' ref (f :)
        loop

-- ---------------------------------------------------------------------------
-- Server plumbing: spawn a localhost HTTP/1.1 server whose handler
-- spins up an 'SseChannel', forks the caller-supplied producer onto
-- it, and returns an SSE response body that drains the channel.
-- ---------------------------------------------------------------------------

withSseServer
  :: (SseChannel -> IO ())
  -> (String -> IO a)
  -> IO a
withSseServer = withSseServerOn http1Only 64 (sseResponseBody . awaitSseEvent)

withSseFrameServer
  :: (SseChannel -> IO ())
  -> (String -> IO a)
  -> IO a
withSseFrameServer =
  withSseServerOn http1Only 64 (sseResponseBodyFrames . awaitSseFrame)

withSseServerCap
  :: Int
  -> (SseChannel -> IO ())
  -> (String -> IO a)
  -> IO a
withSseServerCap cap =
  withSseServerOn http1Only cap (sseResponseBody . awaitSseEvent)

withSseServerVersion
  :: VersionRange
  -> (SseChannel -> IO ())
  -> (String -> IO a)
  -> IO a
withSseServerVersion ver =
  withSseServerOn ver 64 (sseResponseBody . awaitSseEvent)

withSseServerOn
  :: VersionRange
  -> Int                          -- ^ channel backlog cap
  -> (SseChannel -> TB.Body)
  -> (SseChannel -> IO ())
  -> (String -> IO a)
  -> IO a
withSseServerOn ver cap mkBody producer = withTestServer ver handler
  where
    handler _req = do
      ch <- newSseChannel cap
      _  <- forkIO (producer ch)
      pure Msg.Response
        { Msg.responseStatus     = S.status200
          -- The HTTP/1.x server mirrors the request version onto
          -- the response; the HTTP/2 server ignores this field and
          -- uses its own framing. So HTTP1_1 is fine across both
          -- backends.
        , Msg.responseVersion    = V.HTTP1_1
        , Msg.responseHeaders    =
            [ (H.hContentType,  "text/event-stream")
            , (H.hCacheControl, "no-store")
            ]
        , Msg.responseBody       = mkBody ch
        , Msg.responseTrailers   = pure []
        , Msg.responseH2StreamId = 0
        , Msg.responsePushPromises = pure []
        , Msg.responseCancel     = pure ()
        }

-- ---------------------------------------------------------------------------
-- HTTP/2, producer-crash, max-backpressure
-- ---------------------------------------------------------------------------

-- | Same basic SSE round-trip as 'basicRoundTrip', but over h2c
-- (HTTP\/2 cleartext with prior knowledge). The SSE pipeline
-- doesn't care which version is on the wire — events flow
-- through 'Body' chunks the same way regardless of whether the
-- HTTP layer encodes them as chunked-transfer or DATA frames —
-- but this is the proof point that the high-level SSE API is
-- in fact version-agnostic.
sseOverHttp2 :: TestTree
sseOverHttp2 = testCase "events round-trip over HTTP/2 (h2c)" $ do
  let evs =
        [ defaultSseEvent { sseEventType = Just "tick", sseData = "1" }
        , defaultSseEvent { sseEventType = Just "tick", sseData = "2" }
        , defaultSseEvent { sseEventType = Just "done", sseData = "ok" }
        ]
  withSseServerVersion http2Only (pushAllAndClose evs) $ \port -> do
    received <- newIORef ([] :: [ServerSentEvent])
    let req       = get (compileURL port "/events")
        transport = streamedTransport http2Only
    withSSE transport req $ \nextEvent ->
      drainEvents nextEvent received
    got <- reverse <$> readIORef received
    got @?= evs

-- | If a producer throws mid-stream but follows the recommended
-- pattern of @'finally' 'closeSseChannel'@, the consumer should
-- see every event that landed before the crash and then a clean
-- EOF (rather than hanging on @awaitSseEvent@ forever).
--
-- This codifies the recommended producer shape — closing the
-- channel via @finally@ is the documented way to surface
-- end-of-stream regardless of how the producer exits.
producerCrashGraceful :: TestTree
producerCrashGraceful =
  testCase "producer crash with `finally close` delivers prefix + EOF" $ do
  let goodEvs = map (\i -> defaultSseEvent { sseData = BS8.pack ("e" <> show i) })
                    [0 .. 9 :: Int]
      producer ch =
        (do forM_ goodEvs (sendSseEvent ch)
            error "producer goes boom mid-stream")
        `finally` closeSseChannel ch
  withSseServer producer $ \port -> do
    received <- newIORef ([] :: [ServerSentEvent])
    let req       = get (compileURL port "/events")
        transport = streamedTransport http1Only
    withSSE transport req $ \nextEvent ->
      drainEvents nextEvent received
    got <- reverse <$> readIORef received
    got @?= goodEvs

-- | Bounded channel with the smallest possible cap (1): the
-- producer can only stage a single event at a time, so every send
-- past the first one blocks until the consumer takes the previous
-- one off. Catches any deadlock or lost-wakeup in the
-- producer\/consumer STM dance under maximum backpressure.
--
-- Pushing 500 events through a cap-1 queue makes every send round-trip
-- through the consumer.
extremeBackpressure :: TestTree
extremeBackpressure = testCase "channel cap = 1 still delivers all events in order" $ do
  let evs = map (\i -> defaultSseEvent { sseData = BS8.pack ("e" <> show i) })
                [0 .. 499 :: Int]
  withSseServerCap 1 (pushAllAndClose evs) $ \port -> do
    received <- newIORef ([] :: [ServerSentEvent])
    let req       = get (compileURL port "/events")
        transport = streamedTransport http1Only
    withSSE transport req $ \nextEvent ->
      drainEvents nextEvent received
    got <- reverse <$> readIORef received
    got @?= evs

-- ---------------------------------------------------------------------------
-- Plumbing
-- ---------------------------------------------------------------------------

withTestServer
  :: VersionRange
  -> Handler
  -> (String -> IO a)
  -> IO a
withTestServer range handler action = do
  readyVar <- newEmptyMVar
  let hints = NS.defaultHints
        { NS.addrFlags      = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> assertFailure "no addr available for test bind" >> error "unreachable"
    (addr : _) -> bracket
      (NS.openSocket addr)
      NS.close
      $ \listenSock -> do
        NS.setSocketOption listenSock NS.ReuseAddr 1
        NS.bind listenSock (NS.addrAddress addr)
        NS.listen listenSock 128
        bound <- NS.getSocketName listenSock
        let portStr = case bound of
              NS.SockAddrInet p _ -> show (fromIntegral p :: Int)
              _                   -> "0"
            cfg = defaultServerConfig
              { serverHost         = "127.0.0.1"
              , serverPort         = portStr
              , serverVersionRange = range
              , serverHandler      = handler
              }
        tid <- forkIO $ do
          putMVar readyVar ()
          runServerOnListener cfg listenSock
        takeMVar readyVar
        action portStr `finally` killThread tid

compileURL :: String -> ByteString -> UriTemplate
compileURL port path =
  let bs = "http://127.0.0.1:" <> BS8.pack port <> path
  in case parseTemplate (BS8.unpack bs) of
       Right t  -> t
       Left err -> error ("compileURL: " <> show err)
