{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Built-in middleware combinators.

Every middleware here is @'Transport' m -> 'Transport' m@. Composition
is just function composition, so a stack reads top-down:

@
stack =
    withLogging logger
  . withRetry retryPolicy
  . withTimeout (seconds 30)
  . withAuth (Bearer \"abc\")
  $ baseTransport
@

The middleware in this module is deliberately mechanical — no
state-management, no clever scheduling. Stateful behaviour (rate
limiting, cookie jar) lives in dedicated modules so the state
allocation is explicit at the call site.
-}
module Network.HTTP.Client.Middleware (
  -- * Re-exports
  Middleware,

  -- * Logging
  Logger (..),
  stdoutLogger,
  noopLogger,
  withLogging,

  -- * Auth
  AuthScheme (..),
  withAuth,

  -- * Timing
  Duration,
  millis,
  seconds,
  toMicros,
  withTimeout,

  -- * Retry
  RetryPolicy (..),
  defaultRetryPolicy,
  exponentialBackoff,
  withRetry,
  withRetryUsing,

  -- * Circuit breaker
  CircuitBreaker,
  CircuitBreakerConfig (..),
  defaultCircuitBreakerConfig,
  newCircuitBreaker,
  withCircuitBreaker,
  CircuitBreakerOpen (..),

  -- * Header validation
  withHeaderValidation,
  HeaderValidationFailed (..),

  -- * Expect: 100-continue
  ExpectContinuePolicy (..),
  defaultExpectContinuePolicy,
  withExpectContinue,

  -- * URI rewriting
  withBaseURL,

  -- * Rate limiting
  RateLimit,
  newRateLimit,
  withRateLimit,

  -- * Fault injection (test fixtures)
  withLatency,
  withFailureRate,
  failFirstN,
  withSlowBody,
  newFailFirstN,
  newFailureRate,
  withJitter,
  withConnectionReset,
  withTruncation,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (Exception, SomeException, throwIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BS8
import Data.CaseInsensitive qualified as CI
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import Network.HTTP.Client.BodyStream
import Network.HTTP.Client.Request
import Network.HTTP.Client.Response
import Network.HTTP.Client.Transport
import Network.HTTP.Client.URI
import Network.HTTP.Internal.RetryAfter (RetryDelay (..), retryAfterWithin)
import Network.HTTP.Internal.Validation qualified as V
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as M
import Network.HTTP.Types.Status qualified as S
import System.Timeout qualified
import UnliftIO.Exception qualified as U


-- ---------------------------------------------------------------------------
-- Logging
-- ---------------------------------------------------------------------------

newtype Logger = Logger {logLine :: Text -> IO ()}


stdoutLogger :: Logger
stdoutLogger = Logger (\t -> putStrLn ("[wireform-http] " <> T.unpack t))


noopLogger :: Logger
noopLogger = Logger (\_ -> pure ())


{- | Log the request method and URI before sending, and the status
after. Wraps any 'IO'-based transport.
-}
withLogging :: Logger -> Middleware IO
withLogging logger inner = Transport $ \req -> do
  logLine logger $ "-> " <> T.pack (show (method req)) <> " " <> requestURIToText (requestURI req)
  raw <- sendRaw inner req
  logLine logger $ "<- " <> T.pack (show (statusCode raw))
  pure raw


-- ---------------------------------------------------------------------------
-- Auth
-- ---------------------------------------------------------------------------

data AuthScheme
  = Bearer !ByteString
  | Basic !ByteString !ByteString
  | RawAuth !ByteString -- exact header value
  deriving stock (Show, Eq)


withAuth :: AuthScheme -> Middleware m
withAuth scheme inner = Transport $ \req ->
  let val = case scheme of
        Bearer t -> "Bearer " <> t
        Basic u p -> "Basic " <> B64.encode (u <> ":" <> p)
        RawAuth raw -> raw
  in sendRaw
       inner
       req
         { Network.HTTP.Client.Request.headers =
             H.insertHeader H.hAuthorization val (Network.HTTP.Client.Request.headers req)
         }


-- ---------------------------------------------------------------------------
-- Duration / Timeout
-- ---------------------------------------------------------------------------

-- | A duration in microseconds.
newtype Duration
  = -- | microseconds
    Duration {unDuration :: Int}
  deriving stock (Eq, Ord, Show)


millis :: Int -> Duration
millis n = Duration (n * 1000)


seconds :: Int -> Duration
seconds n = Duration (n * 1_000_000)


toMicros :: Duration -> Int
toMicros = unDuration


data TimeoutException = RequestTimeout
  deriving stock (Show)


instance Exception TimeoutException


withTimeout :: MonadUnliftIO m => Duration -> Middleware m
withTimeout (Duration us) inner = Transport $ \req -> withRunInIO $ \run -> do
  result <- System.Timeout.timeout us (run (sendRaw inner req))
  case result of
    Just r -> pure r
    Nothing -> throwIO RequestTimeout


-- ---------------------------------------------------------------------------
-- Retry
-- ---------------------------------------------------------------------------

data RetryPolicy = RetryPolicy
  { maxAttempts :: !Int
  , initialDelay :: !Duration
  , maxDelay :: !Duration
  , backoffFactor :: !Double
  , retryOn :: !(S.Status -> Bool)
  , retrySafeMethodsOnly :: !Bool
  {- ^ When 'True', retry only requests whose method is idempotent
  per RFC 9110 §9.2.2 (GET, HEAD, OPTIONS, TRACE, PUT, DELETE
  — POST and PATCH are excluded). 'True' by default. Set to
  'False' for systems whose POSTs are explicitly opted into
  replay safety (e.g. server-side @Idempotency-Key@).
  -}
  , honorRetryAfter :: !Bool
  {- ^ Use a valid singleton @Retry-After@ header (RFC 9110 §10.2.3)
  on a response selected by 'retryOn'. Return the response if the server
  delay exceeds 'maxDelay'. Otherwise wait at least the server delay and
  the client's capped backoff. 'True' by default.
  -}
  }


defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy =
  RetryPolicy
    { maxAttempts = 3
    , initialDelay = millis 100
    , maxDelay = seconds 5
    , backoffFactor = 2.0
    , retryOn =
        \s -> let c = S.statusCode s in c == 429 || (c >= 500 && c < 600)
    , retrySafeMethodsOnly = True
    , honorRetryAfter = True
    }


exponentialBackoff :: Int -> RetryPolicy
exponentialBackoff n = defaultRetryPolicy {maxAttempts = n}


{- | Retry middleware. Buffers the request body upfront so the body
can be replayed across attempts — this is a deliberate choice;
callers who want to retry without buffering should compose retry
/below/ a layer that handles their streaming semantics.
-}
withRetry :: RetryPolicy -> Middleware IO
withRetry = withRetryUsing getCurrentTime (threadDelay . toMicros)


-- | Retry middleware with a supplied wall clock and delay operation.
withRetryUsing :: IO UTCTime -> (Duration -> IO ()) -> RetryPolicy -> Middleware IO
withRetryUsing getTime wait policy inner = Transport $ \req -> do
  buffered <- bodyStreamBytes (body req)
  let cap = max 0 (toMicros (maxDelay policy))
      initial = max (Duration 0) (min (Duration cap) (initialDelay policy))
      methodIsSafe = M.isIdempotent (method req)
      attempt n delay = do
        bs <- streamFromStrict buffered
        let attemptReq = req {Network.HTTP.Client.Request.body = bs}
        raw <- sendRaw inner attemptReq
        let shouldRetry =
              retryOn policy (statusCode raw)
                && n < maxAttempts policy
                && (not (retrySafeMethodsOnly policy) || methodIsSafe)
        if shouldRetry
          then do
            let drain = drainPopper (bodyPopper raw)
                retry waitFor = do
                  drain
                  wait waitFor
                  attempt (n + 1) (scaleDuration delay (backoffFactor policy) (Duration cap))
            hinted <- if honorRetryAfter policy
              then retryAfterWithin getTime cap (Network.HTTP.Client.Response.headers raw)
                `U.onException` drain
              else pure NoRetryDelay
            case hinted of
              NoRetryDelay -> retry delay
              RetryDelay micros -> retry (max delay (Duration micros))
              RetryDelayAboveLimit -> pure raw
          else pure raw
  attempt 1 initial


scaleDuration :: Duration -> Double -> Duration -> Duration
scaleDuration (Duration us) factor (Duration cap) =
  let scaled = fromIntegral us * factor
  in Duration $
       if scaled >= fromIntegral cap then cap
       else if scaled > 0 then round scaled else 0


-- ---------------------------------------------------------------------------
-- Circuit breaker
-- ---------------------------------------------------------------------------

{- | A simple closed/open/half-open circuit breaker. The closed
state lets every request through; once @cbFailureThreshold@
consecutive failures accumulate, the breaker opens and rejects
further calls with 'CircuitBreakerOpen' until @cbResetAfter@
seconds have elapsed, at which point it goes half-open and lets
one trial request through. A successful trial closes the breaker
again; a failure re-opens it.
-}
data CircuitBreaker = CircuitBreaker
  { cbState :: !(MVar CircuitState)
  , cbConfig :: !CircuitBreakerConfig
  }


data CircuitBreakerConfig = CircuitBreakerConfig
  { cbFailureThreshold :: !Int
  -- ^ Consecutive failures that flip the breaker open. Default 5.
  , cbResetAfter :: !NominalDiffTime
  {- ^ Time the breaker stays open before going half-open and
  admitting a trial request. Default 30 seconds.
  -}
  , cbFailureOn :: !(S.Status -> Bool)
  {- ^ Status predicate that counts as a failure for breaker
  accounting. Default: 5xx.
  -}
  }


defaultCircuitBreakerConfig :: CircuitBreakerConfig
defaultCircuitBreakerConfig =
  CircuitBreakerConfig
    { cbFailureThreshold = 5
    , cbResetAfter = 30
    , cbFailureOn = S.statusIsServerError
    }


data CircuitState
  = -- | consecutive failure count
    CircuitClosed !Int
  | -- | open until this point in time
    CircuitOpen !UTCTime
  | CircuitHalfOpen


{- | Thrown by 'withCircuitBreaker' when the breaker is open. Wraps
the time at which it'll go half-open so callers can surface a
useful retry hint.
-}
data CircuitBreakerOpen = CircuitBreakerOpen {cboOpenUntil :: !UTCTime}
  deriving stock (Show)


instance Exception CircuitBreakerOpen


newCircuitBreaker :: CircuitBreakerConfig -> IO CircuitBreaker
newCircuitBreaker cfg = do
  m <- newMVar (CircuitClosed 0)
  pure CircuitBreaker {cbState = m, cbConfig = cfg}


{- | Wrap a transport in a circuit breaker. Failures (per
'cbFailureOn') and IO exceptions count toward the threshold; a
successful response in either the closed or half-open state
resets the failure counter.
-}
withCircuitBreaker :: CircuitBreaker -> Middleware IO
withCircuitBreaker cb inner = Transport $ \req -> do
  admit <- modifyMVar (cbState cb) $ \st -> do
    now <- getCurrentTime
    case st of
      CircuitClosed _ -> pure (st, Right ())
      CircuitOpen until_
        | now >= until_ -> pure (CircuitHalfOpen, Right ())
        | otherwise -> pure (st, Left until_)
      CircuitHalfOpen -> pure (st, Right ())
  case admit of
    Left until_ -> throwIO (CircuitBreakerOpen until_)
    Right () ->
      U.try (sendRaw inner req) >>= \case
        Left (e :: SomeException) -> do
          recordFailure
          U.throwIO e
        Right raw -> do
          if cbFailureOn (cbConfig cb) (statusCode raw)
            then recordFailure
            else recordSuccess
          pure raw
  where
    recordSuccess =
      modifyMVar_ (cbState cb) $ \_ -> pure (CircuitClosed 0)
    recordFailure =
      modifyMVar_ (cbState cb) $ \st -> do
        now <- getCurrentTime
        let cfg = cbConfig cb
            openAt = addUTCTime (cbResetAfter cfg) now
            tripped = CircuitOpen openAt
        case st of
          CircuitClosed n
            | n + 1 >= cbFailureThreshold cfg -> pure tripped
            | otherwise -> pure (CircuitClosed (n + 1))
          CircuitHalfOpen -> pure tripped
          CircuitOpen {} -> pure st


-- ---------------------------------------------------------------------------
-- Header validation
-- ---------------------------------------------------------------------------

{- | Thrown by 'withHeaderValidation' when an outgoing request
carries a header that doesn't satisfy the RFC 9110 field-name
(token) or field-value (no CR \/ LF \/ NUL) grammar. Carries
the offending name + value so the caller can route the
exception sensibly (and decide whether to retry, error out, or
log).
-}
data HeaderValidationFailed = HeaderValidationFailed
  { hvName :: !H.HeaderName
  , hvValue :: !H.HeaderValue
  , hvKind :: !V.HeaderError
  }
  deriving stock (Show)


instance Exception HeaderValidationFailed


{- | Middleware that asserts the RFC 9110 grammar on every
outgoing request header.  The fast-path header constructors
('insertHeader', 'addHeader', etc.) intentionally skip
validation because the cost is hot-path-sensitive and the
shipped middleware uses only constants that are valid by
construction.  When external input might land in the header
list (URL-derived names, user-provided headers, …), compose
'withHeaderValidation' near the outermost edge of your stack
so any malformed entry fails fast with
'HeaderValidationFailed' instead of going to the wire.
-}
withHeaderValidation :: Middleware IO
withHeaderValidation inner = Transport $ \req -> do
  let hs = Network.HTTP.Client.Request.headers req
  mapM_ check hs
  sendRaw inner req
  where
    check (name, value) = do
      let nameBytes = CI.original name
      case V.validateHeaderName nameBytes of
        Left err ->
          throwIO
            HeaderValidationFailed
              { hvName = name
              , hvValue = value
              , hvKind = err
              }
        Right _ -> pure ()
      case V.validateHeaderValue value of
        Left err ->
          throwIO
            HeaderValidationFailed
              { hvName = name
              , hvValue = value
              , hvKind = err
              }
        Right _ -> pure ()


-- ---------------------------------------------------------------------------
-- Expect: 100-continue
-- ---------------------------------------------------------------------------

{- | When to attach @Expect: 100-continue@ to an outgoing
request.  The point of the header is to let the server reject
a body (auth failure, 413, …) before the client has streamed
the bytes; it only makes sense if the body is large enough
that paying the round-trip is cheaper than uploading and
discarding.
-}
data ExpectContinuePolicy = ExpectContinuePolicy
  { ecMinBodyBytes :: !(Maybe Int)
  {- ^ Skip Expect on requests whose 'Content-Length' is below
  this many bytes.  Defaults to 1 MiB.  'Nothing' means
  always attach (or, equivalently, threshold 0).
  -}
  , ecSkipMethods :: ![M.Method]
  {- ^ Methods that are always exempt.  Defaults to GET / HEAD
  / DELETE (which by RFC 9110 §9.3 don't carry a body
  that the server can usefully reject pre-upload).
  -}
  }


defaultExpectContinuePolicy :: ExpectContinuePolicy
defaultExpectContinuePolicy =
  ExpectContinuePolicy
    { ecMinBodyBytes = Just (1024 * 1024)
    , ecSkipMethods = [M.mGet, M.mHead, M.mDelete]
    }


{- | Attach @Expect: 100-continue@ to outgoing requests when the
policy says it is worthwhile. The HTTP/1.1 and HTTP/2 clients wait for
@100 Continue@, a final response, or its continue timeout before
deciding whether to send the body.
-}
withExpectContinue :: ExpectContinuePolicy -> Middleware IO
withExpectContinue policy inner = Transport $ \req -> do
  let req' = if shouldAttach req then attach req else req
  sendRaw inner req'
  where
    shouldAttach req =
      let meth = Network.HTTP.Client.Request.method req
          hdrs = Network.HTTP.Client.Request.headers req
          alreadyHas = H.hasHeader H.hExpect hdrs
          contentLen = parseCL =<< H.lookupHeader H.hContentLength hdrs
      in not alreadyHas
           && meth `notElem` ecSkipMethods policy
           && knownSize (Network.HTTP.Client.Request.body req) /= Just 0
           && contentLen /= Just 0
           && passesThreshold contentLen

    passesThreshold len = case (ecMinBodyBytes policy, len) of
      (Nothing, _) -> True
      (Just thr, Just n) -> n >= thr
      (Just _, Nothing) -> True
    -- \^ Unknown body length is conservative: assume it's
    --   large enough to warrant the round-trip.

    attach req =
      let hdrs = Network.HTTP.Client.Request.headers req
          hdrs' = H.insertHeader H.hExpect "100-continue" hdrs
      in req {Network.HTTP.Client.Request.headers = hdrs'}

    parseCL bs = case BS8.readInt bs of
      Just (n, leftover) | BS.null leftover && n >= 0 -> Just n
      _ -> Nothing


-- ---------------------------------------------------------------------------
-- URI rewriting / base URL
-- ---------------------------------------------------------------------------

{- | Re-root requests at a 'BaseURL'. Absolute request URIs are passed
through untouched; relative paths are joined to the base.
-}
withBaseURL :: BaseURL -> Middleware m
withBaseURL base inner = Transport $ \req ->
  let rewritten = req {requestURI = resolveTemplate base (requestURI req)}
  in sendRaw inner rewritten


resolveTemplate :: BaseURL -> RequestURI -> RequestURI
resolveTemplate base ru =
  let reqText = requestURIToText ru
  in if isAbsolute reqText
       then ru
       else
         let baseText = TE.decodeUtf8 (renderBaseURL base)
             joined =
               case T.uncons reqText of
                 Nothing -> baseText
                 Just ('/', _) -> dropTrailingSlash baseText <> reqText
                 _ -> dropTrailingSlash baseText <> "/" <> reqText
         in staticURI joined
  where
    isAbsolute t =
      "http://" `T.isPrefixOf` t
        || "https://" `T.isPrefixOf` t
    dropTrailingSlash t
      | "/" `T.isSuffixOf` t = T.dropEnd 1 t
      | otherwise = t


-- ---------------------------------------------------------------------------
-- Rate limiting
-- ---------------------------------------------------------------------------

{- | A token-bucket-ish rate limiter. The simplest possible
implementation: an 'MVar' counter that admits at most @rate@
requests per @window@ duration. New permits arrive lazily, one at
a time, as the window slides.
-}
data RateLimit = RateLimit
  { rlState :: !(MVar (UTCTime, Int))
  , rlRate :: !Int
  , rlWindow :: !NominalDiffTime
  }


newRateLimit
  :: Int
  -- ^ requests per window
  -> NominalDiffTime
  -- ^ window duration
  -> IO RateLimit
newRateLimit r w = do
  now <- getCurrentTime
  m <- newMVar (now, r)
  pure RateLimit {rlState = m, rlRate = r, rlWindow = w}


withRateLimit :: RateLimit -> Middleware IO
withRateLimit rl inner = Transport $ \req -> do
  awaitPermit rl
  sendRaw inner req


awaitPermit :: RateLimit -> IO ()
awaitPermit rl = do
  modifyMVar_ (rlState rl) $ \(windowStart, tokens) -> do
    now <- getCurrentTime
    let elapsed = diffUTCTime now windowStart
        (winStart', tokens')
          | elapsed >= rlWindow rl = (addUTCTime (rlWindow rl) windowStart, rlRate rl)
          | otherwise = (windowStart, tokens)
    if tokens' > 0
      then pure (winStart', tokens' - 1)
      else do
        -- Sleep until the next window boundary and refill.
        let sleepFor = rlWindow rl - elapsed
            us = max 0 (round (1_000_000 * realToFrac sleepFor :: Double))
        threadDelay us
        let nextStart = addUTCTime (rlWindow rl) windowStart
        pure (nextStart, rlRate rl - 1)


-- ---------------------------------------------------------------------------
-- Fault injection (intended for tests)
-- ---------------------------------------------------------------------------

withLatency :: Duration -> Middleware IO
withLatency (Duration us) inner = Transport $ \req -> do
  threadDelay us
  sendRaw inner req


{- | Allocate a state-bearing middleware that fails the first @n@
requests with @canned@ and then passes through. The spec
presented this as a pure @Int -> RawResponse -> Middleware IO@,
but the per-call counter has to live somewhere — exposing the
'IO' wrap-up explicitly is more honest than hiding an
'unsafePerformIO'.
-}
newFailFirstN :: Int -> RawResponse -> IO (Middleware IO)
newFailFirstN n canned = do
  ref <- newIORef n
  pure $ \inner -> Transport $ \req -> do
    keepFailing <- atomicModifyIORef' ref $ \k ->
      if k > 0 then (k - 1, True) else (0, False)
    if keepFailing then pure canned else sendRaw inner req


{- | Backwards-compatible alias: builds the middleware inside 'IO' and
immediately applies it. Useful when wiring stacks together
imperatively.
-}
failFirstN :: Int -> RawResponse -> Transport IO -> IO (Transport IO)
failFirstN n canned inner = do
  m <- newFailFirstN n canned
  pure (m inner)


{- | Allocate a randomised failure-injector. Returns @canned@ with
probability @p@; otherwise forwards.
-}
newFailureRate :: Double -> RawResponse -> IO (Middleware IO)
newFailureRate p canned = do
  seedRef <- newIORef =<< initialSeed
  pure $ \inner -> Transport $ \req -> do
    roll <- atomicModifyIORef' seedRef $ \s ->
      let s' = stepLcg s
          d = fromIntegral (s' `div` 2 :: Word64) / fromIntegral (maxBound :: Word64) :: Double
      in (s', d)
    if roll < p then pure canned else sendRaw inner req


{- | Convenience wrapper: same as 'newFailureRate' but applies
immediately. Mostly for symmetry with 'failFirstN'.
-}
withFailureRate :: Double -> RawResponse -> Transport IO -> IO (Transport IO)
withFailureRate p canned inner = do
  m <- newFailureRate p canned
  pure (m inner)


{- | Add latency between response body chunks. The transport returns
normally; the popper sleeps before yielding each chunk.
-}
withSlowBody :: Duration -> Middleware IO
withSlowBody (Duration us) inner = Transport $ \req -> do
  raw <- sendRaw inner req
  let slow = do
        threadDelay us
        bodyPopper raw
  pure raw {bodyPopper = slow}


{- | Sleep for a uniform-random duration in the closed range before
each request. Useful for shaking out race conditions in code that
expects responses to arrive faster than they actually do.

The RNG is an internally-owned LCG seeded from the current time;
it's good enough for jitter but not for anything that needs real
randomness.
-}
withJitter :: (Duration, Duration) -> IO (Middleware IO)
withJitter (Duration lo, Duration hi)
  | hi < lo = pure (\inner -> inner)
  | hi == lo = pure (withLatency (Duration lo))
  | otherwise = do
      seedRef <- newIORef =<< initialSeed
      pure $ \inner -> Transport $ \req -> do
        d <- atomicModifyIORef' seedRef $ \s ->
          let s' = stepLcg s
              fraction = fromIntegral (s' `mod` 1000000) / 1000000 :: Double
              jittered = lo + round (fraction * fromIntegral (hi - lo) :: Double)
          in (s', jittered)
        threadDelay d
        sendRaw inner req


{- | Throw an 'IOException' simulating a closed peer connection with
probability @p@; otherwise forward the request. The probability is
evaluated independently per request.
-}
withConnectionReset :: Double -> IO (Middleware IO)
withConnectionReset p = do
  seedRef <- newIORef =<< initialSeed
  pure $ \inner -> Transport $ \req -> do
    roll <- atomicModifyIORef' seedRef $ \s ->
      let s' = stepLcg s
          d = fromIntegral (s' `mod` 1000000) / 1000000 :: Double
      in (s', d)
    if roll < p
      then throwIO (userError "withConnectionReset: peer closed connection")
      else sendRaw inner req


{- | Truncate response bodies after @n@ bytes (simulates a mid-body
network drop). Stops yielding chunks once @n@ bytes have been
delivered.
-}
withTruncation :: Int -> Middleware IO
withTruncation n inner = Transport $ \req -> do
  raw <- sendRaw inner req
  remainingRef <- newIORef n
  let truncated = do
        remaining <- readIORef remainingRef
        if remaining <= 0
          then pure BS.empty
          else do
            chunk <- bodyPopper raw
            if BS.null chunk
              then pure BS.empty
              else do
                let take_ = min remaining (BS.length chunk)
                writeIORef remainingRef (remaining - take_)
                pure (BS.take take_ chunk)
  pure raw {bodyPopper = truncated}


-- ---------------------------------------------------------------------------
-- Shared LCG (cheap, time-seeded; suitable for tests, not crypto)
-- ---------------------------------------------------------------------------

initialSeed :: IO Word64
initialSeed = do
  now <- getCurrentTime
  pure $!
    fromIntegral
      ( round
          ( realToFrac
              (diffUTCTime now (read "1970-01-01 00:00:00 UTC"))
              * 1000000
              :: Double
          )
          :: Integer
      )


stepLcg :: Word64 -> Word64
stepLcg s = s * 6364136223846793005 + 1442695040888963407
