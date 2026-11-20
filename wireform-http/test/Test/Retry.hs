{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Tests for the retry / circuit-breaker middleware added to
"Network.HTTP.Client.Middleware":

* @Retry-After@ parsing (delta-seconds and HTTP-date),
* idempotency-aware retry (@retrySafeMethodsOnly@),
* circuit-breaker open / half-open transitions.

The retry tests use a counter-backed mock transport to assert
exactly how many upstream calls happened. They keep retry delays
in the low-millisecond range so the test suite stays fast.
-}
module Test.Retry (tests) where

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Network.HTTP.Client.BodyStream qualified as BSm
import Network.HTTP.Client.Middleware
import Network.HTTP.Client.Protocol (ProtocolInfo (..))
import Network.HTTP.Client.Request (Request, get)
import Network.HTTP.Client.Request qualified as WReq
import Network.HTTP.Client.Response qualified as Resp
import Network.HTTP.Client.Send (prepareRequest)
import Network.HTTP.Client.Transport
import Network.HTTP.Client.URI qualified as WURI
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as M
import Network.HTTP.Types.Status qualified as S
import Test.Syd


-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

mkTransport
  :: [(S.Status, [(H.HeaderName, H.HeaderValue)], ByteString)]
  -> IO (Transport IO, IORef Int)
mkTransport canned = do
  ref <- newIORef canned
  calls <- newIORef 0
  let t = Transport $ \_req -> do
        atomicModifyIORef' calls (\n -> (n + 1, ()))
        next <- atomicModifyIORef' ref $ \rs -> case rs of
          (r : rest) -> (rest, r)
          [] -> ([], (S.status500, [], "no more canned"))
        let (st, hdrs, body) = next
        popper <- BSm.popperFromStrict body
        pure
          Resp.RawResponse
            { Resp.statusCode = st
            , Resp.headers = hdrs
            , Resp.bodyPopper = popper
            , Resp.protocolInfo = HTTP1_1
            }
  pure (t, calls)


makeGet :: IO (Request BSm.BodyStream)
makeGet = case WURI.parseTemplate "http://example.com/" of
  Left e -> error (show e)
  Right t -> prepareRequest [] (get t)


{- | Build a request whose 'method' is @POST@. The body is empty
to keep the harness simple.
-}
makePost :: IO (Request BSm.BodyStream)
makePost = do
  r <- makeGet
  pure r {WReq.method = M.mPost}


-- ---------------------------------------------------------------------------
-- Idempotency
-- ---------------------------------------------------------------------------

unit_retries_get :: Spec
unit_retries_get = it "default policy retries GET on 503" $ do
  (t, calls) <-
    mkTransport
      [ (S.status503, [], "")
      , (S.status503, [], "")
      , (S.status200, [], "ok")
      ]
  let cfg =
        defaultRetryPolicy
          { initialDelay = millis 1
          , maxDelay = millis 5
          , maxAttempts = 3
          }
  req <- makeGet
  _ <- sendRaw (withRetry cfg t) req
  n <- readIORef calls
  n `shouldBe` 3


unit_does_not_retry_post_by_default :: Spec
unit_does_not_retry_post_by_default = it "default policy does NOT retry POST" $ do
  (t, calls) <-
    mkTransport
      [ (S.status503, [], "")
      , (S.status503, [], "")
      , (S.status200, [], "ok")
      ]
  let cfg =
        defaultRetryPolicy
          { initialDelay = millis 1
          , maxDelay = millis 5
          , maxAttempts = 3
          }
  req <- makePost
  _ <- sendRaw (withRetry cfg t) req
  n <- readIORef calls
  n `shouldBe` 1


unit_retries_post_when_opted_in :: Spec
unit_retries_post_when_opted_in = it
  "retrySafeMethodsOnly=False lets POST retry"
  $ do
    (t, calls) <-
      mkTransport
        [ (S.status503, [], "")
        , (S.status503, [], "")
        , (S.status200, [], "ok")
        ]
    let cfg =
          defaultRetryPolicy
            { initialDelay = millis 1
            , maxDelay = millis 5
            , maxAttempts = 3
            , retrySafeMethodsOnly = False
            }
    req <- makePost
    _ <- sendRaw (withRetry cfg t) req
    n <- readIORef calls
    n `shouldBe` 3


unit_429_is_retried :: Spec
unit_429_is_retried = it "429 is retried by default" $ do
  (t, calls) <-
    mkTransport
      [ (S.status429, [], "")
      , (S.status200, [], "ok")
      ]
  let cfg =
        defaultRetryPolicy
          { initialDelay = millis 1
          , maxDelay = millis 5
          , maxAttempts = 2
          }
  req <- makeGet
  _ <- sendRaw (withRetry cfg t) req
  n <- readIORef calls
  n `shouldBe` 2


-- ---------------------------------------------------------------------------
-- Retry-After
-- ---------------------------------------------------------------------------

retryAfterHeader :: Spec
retryAfterHeader = describe "Retry-After header" $ do
  it "honors delta-seconds with field whitespace on 429" $
    check defaultPolicy S.status429 (field " \t1 \t") (seconds 1) 0
  it "allows a server delay right at the policy limit" $
    check defaultPolicy S.status503 (field "2") (seconds 2) 0
  it "returns an unread response when a numeric delay exceeds the policy limit" $
    mapM_ (\wire -> checkStopped S.status429 (field wire) 0)
      ["3", BS8.pack (show (maxBound :: Word)), BS8.replicate 4096 '9']
  it "falls back to backoff for signed, malformed, repeated, and connection-scoped fields" $
    mapM_ (\fields -> check defaultPolicy S.status503 fields (millis 100) 0)
      [ field "+1"
      , field "1junk"
      , field "1844674407370955161600000000000junk"
      , field (BS8.replicate 4096 '9' <> "junk")
      , field "1" <> field "1"
      , field "1" <> [(H.hConnection, "keep-alive, RETRY-AFTER")]
      ]
  it "honors the HTTP-date without rounding the wait down" $
    check defaultPolicy S.status503 (field "Thu, 01 Jan 2026 00:00:01 GMT") (millis 250) 1
  it "uses the live century window for an obsolete HTTP-date" $
    checkStopped S.status503 (field "Tuesday, 01-Jan-75 00:00:00 GMT") 1
  it "returns an unread response when a date exceeds the policy limit" $
    checkStopped S.status503 (field "Thu, 01 Jan 2026 00:00:03 GMT") 1
  it "uses client backoff for a past HTTP-date" $
    check defaultPolicy S.status503 (field "Sun, 06 Nov 1994 08:49:37 GMT") (millis 100) 1
  it "does not read the clock for an absent field or an opted-out policy" $ do
    check defaultPolicy S.status503 [] (millis 100) 0
    check defaultPolicy{honorRetryAfter = False} S.status503
      (field "Thu, 01 Jan 2026 00:00:01 GMT") (millis 100) 0
    check defaultPolicy{honorRetryAfter = False} S.status429
      (field (BS8.replicate 4096 '9')) (millis 100) 0
  where
    defaultPolicy = defaultRetryPolicy{initialDelay = millis 100, maxDelay = seconds 2, maxAttempts = 2}
    field value = [(H.hRetryAfter, value)]
    testTime = UTCTime (fromGregorian 2026 1 1) 0.75000075
    check policy status fields expected expectedClockReads = do
      (inner, calls) <- mkTransport
        [ (status, fields, "discard")
        , (S.status200, [], "ok")
        ]
      waits <- newIORef []
      clockReads <- newIORef (0 :: Int)
      let clock = modifyIORef' clockReads (+ 1) >> pure testTime
          wait duration = modifyIORef' waits (duration :)
      request <- makeGet
      response <- sendRaw (withRetryUsing clock wait policy inner) request
      BSm.popperBytes (Resp.bodyPopper response) `shouldReturn` "ok"
      readIORef calls `shouldReturn` 2
      readIORef waits `shouldReturn` [expected]
      readIORef clockReads `shouldReturn` expectedClockReads
    checkStopped status fields expectedClockReads = do
      (inner, calls) <- mkTransport [(status, fields, "retry later"), (S.status200, [], "unexpected retry")]
      waits <- newIORef []
      clockReads <- newIORef (0 :: Int)
      bodyReads <- newIORef (0 :: Int)
      let transport = Transport $ \request -> do
            reply <- sendRaw inner request
            pure reply{Resp.bodyPopper = modifyIORef' bodyReads (+ 1) >> Resp.bodyPopper reply}
          clock = modifyIORef' clockReads (+ 1) >> pure testTime
          wait duration = modifyIORef' waits (duration :)
      request <- makeGet
      response <- sendRaw (withRetryUsing clock wait defaultPolicy transport) request
      Resp.statusCode response `shouldBe` status
      Resp.headers response `shouldBe` fields
      readIORef bodyReads `shouldReturn` 0
      BSm.popperBytes (Resp.bodyPopper response) `shouldReturn` "retry later"
      readIORef calls `shouldReturn` 1
      readIORef waits `shouldReturn` []
      readIORef clockReads `shouldReturn` expectedClockReads


unit_drains_before_backoff :: Spec
unit_drains_before_backoff = it "drains the intermediate body before waiting and leaves the final body readable" $ do
  (inner, _) <- mkTransport
    [ (S.status503, [], "discard")
    , (S.status503, [], "last response")
    ]
  drained <- newIORef False
  let transport = Transport $ \request -> do
        response <- sendRaw inner request
        pure response{Resp.bodyPopper = do
          chunk <- Resp.bodyPopper response
          if BS.null chunk then writeIORef drained True else pure ()
          pure chunk}
      wait _ = readIORef drained `shouldReturn` True
      policy = defaultRetryPolicy{maxAttempts = 2}
      clock = pure (UTCTime (fromGregorian 2026 1 1) 0)
  request <- makeGet
  response <- sendRaw (withRetryUsing clock wait policy transport) request
  BSm.popperBytes (Resp.bodyPopper response) `shouldReturn` "last response"


unit_caps_backoff_before_rounding :: Spec
unit_caps_backoff_before_rounding = it "caps the initial and scaled backoff before scheduling" $ do
  (inner, _) <- mkTransport
    [ (S.status503, [], "")
    , (S.status503, [], "")
    , (S.status200, [], "ok")
    ]
  waits <- newIORef []
  let policy = defaultRetryPolicy{initialDelay = seconds 10, backoffFactor = 1e30, maxDelay = seconds 2}
      wait duration = modifyIORef' waits (duration :)
      clock = pure (UTCTime (fromGregorian 2026 1 1) 0)
  request <- makeGet
  response <- sendRaw (withRetryUsing clock wait policy inner) request
  BSm.popperBytes (Resp.bodyPopper response) `shouldReturn` "ok"
  readIORef waits `shouldReturn` [seconds 2, seconds 2]


-- ---------------------------------------------------------------------------
-- Circuit breaker
-- ---------------------------------------------------------------------------

unit_breaker_opens_after_threshold :: Spec
unit_breaker_opens_after_threshold = it
  "breaker opens after consecutive failures"
  $ do
    -- A 5xx response counts as a failure (per cbFailureOn=5xx).
    -- After 3 consecutive failures the breaker opens; the next
    -- call throws CircuitBreakerOpen.
    (t, calls) <- mkTransport (replicate 10 (S.status500, [], "boom"))
    let cfg = defaultCircuitBreakerConfig {cbFailureThreshold = 3}
    cb <- newCircuitBreaker cfg
    let trans = withCircuitBreaker cb t
    -- Three calls that all return 500 — they pass through and we
    -- discard the body.
    req <- makeGet
    mapM_ (\_ -> sendRaw trans req >>= drain) [(1 :: Int) .. 3]
    -- Fourth call should throw CircuitBreakerOpen.
    r <- try (sendRaw trans req)
    case r of
      Left (_ :: SomeException) -> pure ()
      Right _ -> error "expected breaker to open"
    -- Underlying transport saw exactly 3 calls (the open breaker
    -- short-circuits the 4th).
    n <- readIORef calls
    n `shouldBe` 3
  where
    drain :: Resp.RawResponse -> IO ByteString
    drain = BSm.popperBytes . Resp.bodyPopper


unit_breaker_passes_when_healthy :: Spec
unit_breaker_passes_when_healthy = it
  "breaker does not interfere on successful calls"
  $ do
    (t, calls) <- mkTransport (replicate 5 (S.status200, [], "ok"))
    cb <- newCircuitBreaker defaultCircuitBreakerConfig
    let trans = withCircuitBreaker cb t
    req <- makeGet
    mapM_
      (\_ -> sendRaw trans req >>= (BSm.popperBytes . Resp.bodyPopper))
      [(1 :: Int) .. 5]
    n <- readIORef calls
    n `shouldBe` 5


-- ---------------------------------------------------------------------------
-- Top-level
-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "Network.HTTP.Client.Middleware.Retry" $
    sequence_
      [ unit_retries_get
      , unit_does_not_retry_post_by_default
      , unit_retries_post_when_opted_in
      , unit_429_is_retried
      , retryAfterHeader
      , unit_drains_before_backoff
      , unit_caps_backoff_before_rounding
      , unit_breaker_opens_after_threshold
      , unit_breaker_passes_when_healthy
      ]
