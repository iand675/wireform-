{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Server-Sent Events (the WHATWG @EventSource@ wire format) for
the wireform HTTP client and server.

The on-wire format is text-based and stream-oriented: lines
terminated by CR, LF, or CRLF; @field: value@ pairs; blank lines
dispatch the accumulated fields as a single event. See
<https://html.spec.whatwg.org/multipage/server-sent-events.html>
for the normative grammar. This module implements that grammar as
an incremental parser that can be fed arbitrary chunk boundaries
(line splits, mid-CRLF, mid-field-name) without losing or
duplicating events.

== Shapes

* 'ServerSentEvent' is a dispatched event — what the spec calls a
  @MessageEvent@: a payload, an optional event type, and the
  @lastEventId@ that was in scope at dispatch time.
* 'SseFrame' adds the two side-channel directives the parser also
  surfaces: 'SseRetry' (the @retry:@ field's reconnection-time
  hint) and 'SseComment' (lines starting with @:@).
* 'SseParser' is the incremental parser state — opaque so the
  carry shape can change.

== Client side

The two consumer popper helpers, 'sseFramePopper' and
'sseEventPopper', adapt a wire-level 'Popper' to a stream of
frames or just dispatched events respectively. They return
'Nothing' on EOF and never re-dispatch.

'withSSE' \/ 'withSSEFrames' are the high-level entrypoints: they
inject the @Accept@ and @Cache-Control@ headers the EventSource
spec calls for, run the request, and hand the caller a
ready-to-drain event popper. The connection lifecycle is bound to
the callback by the underlying 'Transport' — pair with
'Network.HTTP.Client.Streaming.streamedTransport' when you want
connection-per-request lifetime (the recommended posture for SSE).

== Server side

For pushing events from a server handler, 'sseResponseBody' wraps
an event source (any @IO (Maybe ServerSentEvent)@) into the
'BodyStream' that 'Network.HTTP.Message.Response' expects. A
typical handler pairs that with an 'SseChannel' so the worker
threads producing events are decoupled from the HTTP body popper
draining them.

=== Flush semantics

Server-Sent Events is real-time: every event the server emits has
to make it to the wire \"now\", not whenever a buffer happens to
fill. There's no separate @flush@ primitive in this module
(or in the underlying 'BodyStream' \/ 'Body' types) because the
popper contract already gives us that:

* /One popper call returns the bytes for one event./ This is what
  'sseBodyPopper' (and therefore 'sseResponseBody') guarantee — the
  caller's event source is invoked once per popper call, the
  resulting event is rendered, and the bytes are returned as a
  single chunk.
* /One returned chunk becomes one wire frame./ The HTTP\/1.1
  server's 'streamChunked' encoder emits a single chunked-transfer
  chunk per popper call; the HTTP\/2 server emits a single
  @DATA@ frame. Both layers stage the chunk into the send ring
  and the connection's send loop drains it immediately, so each
  event hits the network as soon as its source action returns.

The upshot: if your event source blocks until an event is
available, the HTTP layer will sit idle (no spurious flushes,
no busy-loop); when it returns an event, that one event flushes
on its own. No explicit flush call, no event coalescing.

The 'EventStream' tag plugs SSE into the existing media-type
machinery: callers can decode a one-shot body with
@as \@EventStream \@[ServerSentEvent]@, or encode a list of
events as a server-side response body. For the (vastly more
common) streaming case, use 'withSSE' on the client side and
'sseResponseBody' on the server side.
-}
module Network.HTTP.Client.SSE (
  -- * Events and frames
  ServerSentEvent (..),
  SseFrame (..),

  -- * Incremental parser
  SseParser,
  newSseParser,
  feedSseParser,
  endSseParser,

  -- * One-shot parsing
  parseEventStream,
  parseEventStreamEvents,

  -- * Popper integration
  sseFramePopper,
  sseEventPopper,

  -- * High-level client entry point
  withSSE,
  withSSEFrames,

  -- * Reconnecting EventSource client
  ReconnectPolicy (..),
  defaultReconnectPolicy,
  withReconnectingSSE,
  withReconnectingSSEFrames,
  SseError (..),
  SseStopReason (..),

  -- * Server-side encoder
  renderSseFrame,
  renderServerSentEvent,
  buildSseFrame,
  buildServerSentEvent,
  defaultSseEvent,

  -- * Server-side body
  -- $serverBody
  sseBodyPopper,
  sseBodyPopperFrames,
  sseResponseBody,
  sseResponseBodyFrames,

  -- * Server-side push channel
  -- $serverChannel
  SseChannel,
  newSseChannel,
  sendSseEvent,
  sendSseFrame,
  sendSseRetry,
  sendSseComment,
  closeSseChannel,
  isSseChannelClosed,
  awaitSseFrame,
  awaitSseEvent,

  -- * Content-type tag
  EventStream,
) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (
  STM,
  TBQueue,
  TVar,
  atomically,
  isEmptyTBQueue,
  newTBQueueIO,
  newTVarIO,
  readTBQueue,
  readTVar,
  retry,
  writeTBQueue,
  writeTVar,
 )
import Control.Exception (Exception, SomeException, bracket, throwIO, try)
import Control.Exception qualified
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (mapMaybe)
import Network.HTTP.Client.Body (Body)
import Network.HTTP.Client.BodyStream (Popper)
import Network.HTTP.Client.Media (
  Decode (..),
  Encode (..),
  HasMediaType (..),
  MediaType (..),
  Quality,
  contentTypeOf,
  maxQuality,
 )
import Network.HTTP.Client.Request (Request, removeHeader, setHeader)
import Network.HTTP.Client.Request qualified as Request
import Network.HTTP.Client.Response (
  RawResponse (bodyPopper, statusCode),
  headers,
 )
import Network.HTTP.Client.Send (withResponse)
import Network.HTTP.Client.Transport (Transport)
import Network.HTTP.Headers qualified as Typed
import Network.HTTP.Headers.LastEventID (LastEventID)
import Network.HTTP.Types.Body qualified as B
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Status qualified as S
import Wireform.Builder (Builder)
import Wireform.Builder qualified as WB


-- ---------------------------------------------------------------------------
-- Public types
-- ---------------------------------------------------------------------------

{- | A dispatched server-sent event.

The @sseEventType@ field is 'Nothing' when no @event:@ line preceded
the dispatch — per the spec the implicit event type is then
@\"message\"@. We surface the explicit absence so callers can
distinguish \"the server told us this is a heartbeat\" from \"the
server said nothing\".

@sseEventId@ carries the last value set by an @id:@ field. The
EventSource spec keeps the id across events until explicitly
changed, so two consecutive dispatches with no intervening @id:@
line will see the same value here. This is also the value clients
echo back as @Last-Event-ID@ on reconnection.
-}
data ServerSentEvent = ServerSentEvent
  { sseEventType :: !(Maybe ByteString)
  , sseEventId :: !(Maybe ByteString)
  , sseData :: !ByteString
  }
  deriving stock (Eq, Show)


{- | A single output of the parser. Most callers only care about
'SseDispatch' and use 'sseEventPopper' to filter the rest out.

* 'SseDispatch' — a complete event ready for the application.
* 'SseRetry' — the server's @retry:@ directive in milliseconds.
  Clients honoring reconnection backoff use this value as their
  delay.
* 'SseComment' — a line that started with a literal @:@. Most
  servers use this for keep-alives; surfacing it lets diagnostics
  and proxies see the heartbeat without having to peek at the
  wire.
-}
data SseFrame
  = SseDispatch !ServerSentEvent
  | SseRetry !Int
  | SseComment !ByteString
  deriving stock (Eq, Show)


-- ---------------------------------------------------------------------------
-- Parser state
-- ---------------------------------------------------------------------------

{- | The incremental parser. Opaque on purpose: the carry, BOM state,
and per-event buffers are implementation details we want freedom
to change.
-}
data SseParser = SseParser
  { spCarry :: !ByteString
  {- ^ Bytes seen since the last line terminator. Held as a
  strict 'ByteString' because lines in SSE are typically
  short; the worst case is one giant @data:@ line that we
  never finish, which is the caller's problem.
  -}
  , spJustSawCR :: !Bool
  {- ^ Previous byte was @CR@. If the very next byte is @LF@ we
  fold the pair into one terminator (CRLF). This is the
  only piece of state that crosses chunk boundaries.
  -}
  , spEventType :: !(Maybe ByteString)
  , spDataBuf :: !Builder
  {- ^ Accumulated @data:@ values. Each appended chunk is
  @value <> \"\\n\"@. We materialise once at dispatch via
  'WB.toStrictByteString', which is O(total bytes) — using
  'Wireform.Builder' here keeps repeated @data:@ lines in
  one event from going quadratic the way @BS.append@ would.
  -}
  , spDataNonEmpty :: !Bool
  {- ^ Whether any @data:@ field at all was seen. The spec
  suppresses dispatch if the data buffer is empty; we track
  the predicate directly because @data:@ with an empty value
  still appends @\"\\n\"@ (so the bytes-buffer is non-empty
  even when functionally empty).
  -}
  , spLastEventId :: !(Maybe ByteString)
  -- ^ The ID buffer, including any ID in the unfinished block.
  , spCommittedEventId :: !(Maybe ByteString)
  -- ^ Resume cursor from the most recent complete block.
  , spBomChecked :: !Bool
  -- ^ Whether the optional leading UTF-8 BOM has been consumed.
  }


{- | Initial parser state. No carry, no buffered fields, BOM not yet
considered.
-}
newSseParser :: SseParser
newSseParser =
  SseParser
    { spCarry = BS.empty
    , spJustSawCR = False
    , spEventType = Nothing
    , spDataBuf = mempty
    , spDataNonEmpty = False
    , spLastEventId = Nothing
    , spCommittedEventId = Nothing
    , spBomChecked = False
    }


{- | Feed a chunk through the parser. Returns the updated state and
any frames the chunk completed (in stream order).

The function is total and never throws — even malformed lines just
get ignored per spec.
-}
feedSseParser :: SseParser -> ByteString -> (SseParser, [SseFrame])
feedSseParser p0 chunk0 =
  let (p1, chunk) = stripBomIfNeeded p0 chunk0
      (lines_, carry', sawCR') = splitLines (spCarry p1) (spJustSawCR p1) chunk
      pAfter = p1 {spCarry = carry', spJustSawCR = sawCR'}
      (pFinal, framesRev) = foldLines pAfter [] lines_
  in (pFinal, reverse framesRev)


{- | Signal EOF to the parser. Per the EventSource spec, a partial
event without its terminating blank line is /not/ dispatched at
EOF; this function returns the parser with its in-flight buffers
intact and no frames. It's exposed so callers building their own
popper wrapper have a place to plumb \"the stream ended\" through.
-}
endSseParser :: SseParser -> (SseParser, [SseFrame])
endSseParser p = (p, [])


-- ---------------------------------------------------------------------------
-- One-shot helpers
-- ---------------------------------------------------------------------------

{- | Parse a complete event-stream body into frames in one shot.

Convenience wrapper for tests and recorded fixtures. For live
streams use 'sseFramePopper' (the parser is incremental and
chunk-safe; the one-shot form just feeds the whole buffer at
once).
-}
parseEventStream :: ByteString -> [SseFrame]
parseEventStream bs = snd (feedSseParser newSseParser bs)


-- | 'parseEventStream' filtered to dispatched events only.
parseEventStreamEvents :: ByteString -> [ServerSentEvent]
parseEventStreamEvents = mapMaybe pickDispatch . parseEventStream
  where
    pickDispatch (SseDispatch ev) = Just ev
    pickDispatch _ = Nothing


-- ---------------------------------------------------------------------------
-- Popper integration
-- ---------------------------------------------------------------------------

{- | Wrap a wire-level 'Popper' as a frame popper.

The returned action pulls additional bytes from the underlying
popper as needed to surface the next frame. It returns
'Nothing' on EOF and continues to return 'Nothing' on every
subsequent call.
-}
sseFramePopper :: Popper -> IO (IO (Maybe SseFrame))
sseFramePopper = sseFramePopperWith newSseParser (const (pure ()))


sseFramePopperWith :: SseParser -> (Maybe ByteString -> IO ()) -> Popper -> IO (IO (Maybe SseFrame))
sseFramePopperWith initial commit popper = do
  -- State: parser + a small queue of frames already extracted but
  -- not yet returned. We need the queue because one chunk can
  -- produce more than one frame (consider a server flushing many
  -- buffered events into a single TCP segment).
  ref <- newIORef (initial, [] :: [SseFrame], False)
  let loop = do
        (p, queued, eofSeen) <- readIORef ref
        case queued of
          (f : rest) -> do
            writeIORef ref (p, rest, eofSeen)
            pure (Just f)
          [] ->
            if eofSeen
              then pure Nothing
              else do
                chunk <- popper
                if BS.null chunk
                  then do
                    writeIORef ref (p, [], True)
                    pure Nothing
                  else do
                    let (p', newFrames) = feedSseParser p chunk
                    commit (spCommittedEventId p')
                    case newFrames of
                      (f : rest) -> do
                        writeIORef ref (p', rest, False)
                        pure (Just f)
                      [] -> do
                        writeIORef ref (p', [], False)
                        loop
  pure loop
{-# INLINE sseFramePopperWith #-}


{- | Like 'sseFramePopper' but filters to dispatched events,
silently swallowing comments and retry directives. This is what
most application code wants; reach for the frame popper if you
need to honour reconnection backoff or trace heartbeats.
-}
sseEventPopper :: Popper -> IO (IO (Maybe ServerSentEvent))
sseEventPopper popper = do
  framePopper <- sseFramePopper popper
  let loop = do
        mf <- framePopper
        case mf of
          Nothing -> pure Nothing
          Just (SseDispatch ev) -> pure (Just ev)
          Just _ -> loop
  pure loop


-- ---------------------------------------------------------------------------
-- High-level client helpers
-- ---------------------------------------------------------------------------

{- | Open an SSE stream against a transport.

Adds @Accept: text\/event-stream@ and @Cache-Control: no-store@
to the outgoing request (both per the EventSource spec). The
callback receives a popper of dispatched events; the underlying
connection is owned for the lifetime of the callback by whatever
'Transport' was passed in (use 'Network.HTTP.Client.Streaming.streamedTransport'
for connection-per-request lifetime).

Throws 'SseError' if the response status is non-2xx or the
response @Content-Type@ isn't @text\/event-stream@.
-}
withSSE
  :: forall m body a
   . (MonadUnliftIO m, Body body)
  => Transport IO
  -> Request body
  -> (IO (Maybe ServerSentEvent) -> m a)
  -> m a
withSSE t req k =
  withSSEFrames t req $ \framePopper -> k (filterFrames framePopper)
  where
    filterFrames fp = do
      mf <- fp
      case mf of
        Nothing -> pure Nothing
        Just (SseDispatch ev) -> pure (Just ev)
        Just _ -> filterFrames fp


{- | Same as 'withSSE' but the callback sees every frame the parser
surfaces, including 'SseRetry' and 'SseComment'. Use this if you
need to react to server-controlled reconnection delays or want
to observe keep-alive heartbeats explicitly.
-}
withSSEFrames
  :: forall m body a
   . (MonadUnliftIO m, Body body)
  => Transport IO
  -> Request body
  -> (IO (Maybe SseFrame) -> m a)
  -> m a
withSSEFrames t req k =
  withSSEBody t req $ \popper -> do
    framePopper <- liftIO (sseFramePopper popper)
    k framePopper


withSSEBody
  :: (MonadUnliftIO m, Body body)
  => Transport IO
  -> Request body
  -> (Popper -> m a)
  -> m a
withSSEBody t req k =
  withResponse t (sseRequestHeaders req) acceptList $ \raw -> do
    liftIO (assertSseResponse raw)
    k (bodyPopper raw)
  where
    acceptList :: [(MediaType, Quality)]
    acceptList = [(mediaType @EventStream, maxQuality)]


sseRequestHeaders :: Request body -> Request body
sseRequestHeaders =
  setHeader H.hAccept "text/event-stream"
    . setHeader H.hCacheControl "no-store"


-- | Surface of failure modes for 'withSSE' \/ 'withSSEFrames'.
data SseError
  = SseUnexpectedStatus !S.Status
  | SseUnexpectedContentType !MediaType
  deriving stock (Show)


instance Exception SseError


assertSseResponse :: RawResponse -> IO ()
assertSseResponse raw = do
  let s = statusCode raw
      code = S.statusCode s
  -- Per the WHATWG EventSource spec, 204 No Content is the
  -- server's signal to stop reconnecting. For a non-reconnecting
  -- 'withSSE' caller that's a "no events; give up" outcome —
  -- the same surface as any other non-success status. The
  -- reconnecting client ('withReconnectingSSE') catches this
  -- specific case via the 'SseStopServerEnded' branch.
  unless (code >= 200 && code < 300 && code /= 204) $
    throwIO (SseUnexpectedStatus s)
  let ct = contentTypeOf (headers raw)
  unless (mtType ct == "text" && mtSubType ct == "event-stream") $
    throwIO (SseUnexpectedContentType ct)


-- ---------------------------------------------------------------------------
-- Reconnecting EventSource client
-- ---------------------------------------------------------------------------

{- | Reconnection knobs for 'withReconnectingSSE'. Mirrors the
behaviour of the WHATWG EventSource interface: on disconnect
(or non-fatal status), sleep and reissue the request with a
@Last-Event-ID@ header with the cursor from the last complete block.
-}
data ReconnectPolicy = ReconnectPolicy
  { rpInitialRetryMs :: !Int
  {- ^ Default reconnection delay in milliseconds, used until
  the server overrides it via an @SseRetry@ directive.
  Default 3000 ms (the EventSource spec's recommendation).
  -}
  , rpMaxRetryMs :: !Int
  {- ^ Clamp on retry intervals (both initial and
  server-supplied) so a buggy or malicious server can't
  pin a client for hours. Default 60 000 ms.
  -}
  , rpMaxAttempts :: !(Maybe Int)
  {- ^ When 'Just', give up after this many consecutive failed
  attempts (a connection that returns at least one event
  resets the counter). 'Nothing' = retry indefinitely.
  Default 'Nothing'.
  -}
  , rpRetryOnException :: !Bool
  {- ^ When 'True' (default), transport-layer exceptions
  (DNS, TLS, RST, etc.) also trigger a reconnect; when
  'False' they surface immediately as an exception out of
  'withReconnectingSSE'.
  -}
  }
  deriving stock (Eq, Show)


defaultReconnectPolicy :: ReconnectPolicy
defaultReconnectPolicy =
  ReconnectPolicy
    { rpInitialRetryMs = 3000
    , rpMaxRetryMs = 60000
    , rpMaxAttempts = Nothing
    , rpRetryOnException = True
    }


{- | Why the reconnecting loop stopped. Carried inside the queue's
terminal sentinel so the consumer popper can distinguish
normal end-of-stream from giving-up-after-N-failures.
-}
data SseStopReason
  = -- | Server returned @204 No Content@ ("don't reconnect").
    SseStopServerEnded
  | -- | 'rpMaxAttempts' exhausted without a successful event.
    SseStopMaxAttempts
  | {- | A transport exception escaped (only when
    'rpRetryOnException' is 'False').
    -}
    SseStopException !SomeException
  deriving stock (Show)


{- | Reconnecting EventSource client. Behaves like 'withSSE' but
reissues the request when the server hangs up, sending
@Last-Event-ID@ with the cursor from the last complete block and
honoring the server's @retry:@ directives. An empty ID clears the
resume header. A valid caller-supplied header seeds the cursor.

The callback's popper returns 'Nothing' only when the loop has
given up (server sent @204 No Content@, max attempts hit, or a
fatal exception); ordinary disconnects are transparent.
-}
withReconnectingSSE
  :: forall m body a
   . (MonadUnliftIO m, Body body)
  => Transport IO
  -> Request body
  -> ReconnectPolicy
  -> (IO (Maybe ServerSentEvent) -> m a)
  -> m a
withReconnectingSSE t req policy k =
  withReconnectingSSEFrames t req policy $ \fp ->
    k (filterToEvents fp)
  where
    filterToEvents fp = do
      mf <- fp
      case mf of
        Nothing -> pure Nothing
        Just (SseDispatch ev) -> pure (Just ev)
        Just _ -> filterToEvents fp


{- | Like 'withReconnectingSSE' but the callback sees every parsed
frame, including 'SseRetry' and 'SseComment'. Heartbeats and
the server-supplied reconnect interval are observable here.
-}
withReconnectingSSEFrames
  :: forall m body a
   . (MonadUnliftIO m, Body body)
  => Transport IO
  -> Request body
  -> ReconnectPolicy
  -> (IO (Maybe SseFrame) -> m a)
  -> m a
withReconnectingSSEFrames transport req0 policy k = withRunInIO $ \runInIO -> do
  -- Bounded queue between the reconnect worker and the consumer's
  -- popper. The size is small on purpose: SSE is real-time so a
  -- big buffer adds latency without adding value, and the worker
  -- is happy to block when the consumer is slow.
  q <- newTBQueueIO 16
  lastId <- newIORef (if maySendCursor then resumeSeed req0 else Nothing)
  retryMs <- newIORef (clampRetry (rpInitialRetryMs policy))
  stopRef <- newIORef (Nothing :: Maybe SseStopReason)
  -- A flag the consumer flips when its callback exits so the
  -- worker stops trying to reconnect.
  done <- newIORef False

  let consumer = do
        d <- readIORef done
        if d
          then pure Nothing
          else do
            item <- atomically (readTBQueue q)
            case item of
              Just f -> pure (Just f)
              Nothing -> do
                writeIORef done True
                pure Nothing

  bracket
    (forkIO (reconnectLoop q lastId retryMs stopRef done))
    ( \tid -> do
        writeIORef done True
        killThread tid
    )
    (\_ -> runInIO (k consumer))
  where
    clampRetry n = max 0 (min n (rpMaxRetryMs policy))
    baseRequest = removeHeader H.hLastEventID req0
    baseFields = Request.headers baseRequest
    maySendCursor =
      not (H.hasHeader H.hConnection baseFields)
        || H.hasHeader H.hLastEventID (H.stripHopByHop ((H.hLastEventID, BS.empty) : baseFields))

    -- Reconnect loop runs in the worker thread.
    reconnectLoop q lastId retryMs stopRef done = go 0
      where
        go failCount = do
          d <- readIORef done
          if d
            then pure ()
            else do
              -- Build the per-attempt request, injecting
              -- Last-Event-ID if we have one.
              mLast <- readIORef lastId
              let req
                    | maySendCursor, Just lid <- mLast >>= resumeHeader =
                        setHeader H.hLastEventID lid baseRequest
                    | otherwise = baseRequest
                  initial = newSseParser{spLastEventId = mLast, spCommittedEventId = mLast}
              -- Reset the failure counter on a connection that
              -- dispatches at least one event.
              succeededRef <- newIORef False
              outcome :: Either SomeException () <-
                try $
                  withSSEBody transport req $ \popper -> do
                    fp <- sseFramePopperWith initial (writeIORef lastId) popper
                    pumpFrames fp q retryMs succeededRef done
              succeeded <- readIORef succeededRef
              let newFailCount = if succeeded then 0 else failCount + 1
              case outcome of
                Right () ->
                  case rpMaxAttempts policy of
                    Just n | newFailCount >= n -> stopWith SseStopMaxAttempts
                    _ -> sleepThenRetry newFailCount
                Left err ->
                  case Control.Exception.fromException err :: Maybe SseError of
                    Just (SseUnexpectedStatus s)
                      | S.statusCode s == 204 ->
                          stopWith SseStopServerEnded
                      | otherwise ->
                          -- Any other non-2xx is a permanent
                          -- failure per the EventSource spec.
                          stopWith (SseStopException err)
                    Just _ -> stopWith (SseStopException err)
                    Nothing
                      | rpRetryOnException policy ->
                          case rpMaxAttempts policy of
                            Just n | newFailCount >= n -> stopWith SseStopMaxAttempts
                            _ -> sleepThenRetry newFailCount
                      | otherwise ->
                          stopWith (SseStopException err)
          where
            sleepThenRetry n = do
              d <- readIORef done
              if d
                then pure ()
                else do
                  ms <- readIORef retryMs
                  threadDelay (ms * 1000)
                  go n
            stopWith reason = do
              writeIORef stopRef (Just reason)
              atomically (writeTBQueue q Nothing)

    -- Pump frames from the popper into the queue, updating retry
    -- accounting as they arrive.
    pumpFrames fp q retryMs succeededRef done = loop
      where
        loop = do
          mf <- fp
          case mf of
            Nothing -> pure ()
            Just f -> do
              case f of
                SseDispatch _ -> writeIORef succeededRef True
                SseRetry ms -> do
                  let clamped = max 0 (min ms (rpMaxRetryMs policy))
                  writeIORef retryMs clamped
                SseComment _ -> pure ()
              atomically (writeTBQueue q (Just f))
              -- Bail early if the consumer signalled done.
              d <- readIORef done
              when (not d) loop


resumeSeed :: Request body -> Maybe ByteString
resumeSeed request = do
  field <- either (const Nothing) id (H.lookupSingletonHeader H.hLastEventID (Request.headers request))
  resumeHeader field


resumeHeader :: ByteString -> Maybe ByteString
resumeHeader field
  | BS.null field = Nothing
  | otherwise =
      case Typed.parseFromHeaders @LastEventID Typed.defaultHeaderSettings (field :| []) of
        Right cursor -> Just (Typed.renderToHeaders Typed.defaultHeaderSettings cursor)
        Left _ -> Nothing


-- ---------------------------------------------------------------------------
-- Content-type tag and instances
-- ---------------------------------------------------------------------------

{- | Phantom tag for @text\/event-stream@. Provides
'HasMediaType' so it slots into the generic media negotiation
pipeline, plus 'Encode' \/ 'Decode' for the one-shot list form
(useful when fixtures pre-record a full stream and want to feed
it through 'send').
-}
data EventStream


instance HasMediaType EventStream where
  mediaType = "text/event-stream"


instance Decode EventStream [ServerSentEvent] where
  decode = Right . parseEventStreamEvents


instance Decode EventStream [SseFrame] where
  decode = Right . parseEventStream


instance Encode EventStream [ServerSentEvent] where
  encode = WB.toStrictByteString . foldMap buildServerSentEvent


instance Encode EventStream [SseFrame] where
  encode = WB.toStrictByteString . foldMap buildSseFrame


-- ---------------------------------------------------------------------------
-- Server-side rendering
-- ---------------------------------------------------------------------------

{- | An empty 'ServerSentEvent' with default values. Useful as a base
for record-update syntax.
-}
defaultSseEvent :: ServerSentEvent
defaultSseEvent =
  ServerSentEvent
    { sseEventType = Nothing
    , sseEventId = Nothing
    , sseData = BS.empty
    }


{- | Render a single dispatched event to the wire. Each line of
'sseData' becomes a separate @data:@ line; the final blank line
terminating the event is included. The data payload must not
contain @CR@ (the spec disallows it inside data lines and a
naive render would produce two events at the consumer);
this function does /not/ check.
-}
renderServerSentEvent :: ServerSentEvent -> ByteString
renderServerSentEvent = WB.toStrictByteString . buildServerSentEvent


{- | Render any single frame. 'SseDispatch' delegates to
'renderServerSentEvent'; 'SseRetry' becomes a single @retry:@
line followed by the trailing blank; 'SseComment' becomes a
@:@-prefixed line followed by the trailing blank.
-}
renderSseFrame :: SseFrame -> ByteString
renderSseFrame = WB.toStrictByteString . buildSseFrame


{- | 'Wireform.Builder' versions of the renderers. The byte-returning
'renderServerSentEvent' \/ 'renderSseFrame' just materialise these,
but composing many frames stays O(n) when callers stay in
@Builder@-land and only collapse with 'WB.toStrictByteString' (or
'WB.hPutBuilder') once at the end.
-}
buildServerSentEvent :: ServerSentEvent -> Builder
buildServerSentEvent ev =
  optField "event: " (sseEventType ev)
    <> optField "id: " (sseEventId ev)
    <> buildDataPayload (sseData ev)
    <> WB.word8 0x0A
  where
    optField _ Nothing = mempty
    optField name (Just v) = WB.byteString name <> WB.byteString v <> WB.word8 0x0A


buildSseFrame :: SseFrame -> Builder
buildSseFrame (SseDispatch ev) = buildServerSentEvent ev
buildSseFrame (SseRetry n) =
  WB.byteString "retry: " <> WB.intDec n <> WB.byteString "\n\n"
buildSseFrame (SseComment c) =
  WB.word8 0x3A <> WB.byteString c <> WB.byteString "\n\n"


{- | One @data: <line>\\n@ per LF-separated chunk in @d@. We split
/once/ here and feed the resulting slices straight into the
builder; each slice is O(1) (strict ByteString slices share the
underlying ForeignPtr), so no per-line copy happens.
-}
buildDataPayload :: ByteString -> Builder
buildDataPayload d
  | BS.null d = mempty
  | otherwise = foldMap oneLine (BS.split 0x0A d)
  where
    oneLine l = WB.byteString "data: " <> WB.byteString l <> WB.word8 0x0A


-- ---------------------------------------------------------------------------
-- Server-side body
-- ---------------------------------------------------------------------------

{- $serverBody
The body popper expected by 'Network.HTTP.Types.Body.BodyStream' is
@IO (Maybe ByteString)@: produce a chunk, or 'Nothing' for EOF.
'sseBodyPopper' adapts an event source — any
@IO (Maybe ServerSentEvent)@ — into one of those, materialising
exactly one event per popper call so the underlying HTTP encoder
emits one wire chunk per event (see the module's \"Flush
semantics\" section).

A typical handler looks like:

> handler :: Request -> IO Response
> handler _req = do
>   ch <- newSseChannel 64
>   _  <- forkIO (produce ch)         -- producer pushes events
>   pure Response
>     { responseStatus  = status200
>     , responseHeaders = [ (hContentType,  "text/event-stream")
>                         , (hCacheControl, "no-store")
>                         ]
>     , responseBody    = sseResponseBody (awaitSseEvent ch)
>     , ...
>     }
-}


{- | Adapt an event source into the chunk popper expected by
'Network.HTTP.Types.Body.BodyStream'. Each popper call pulls
exactly one event from the source and renders it; the encoder
(HTTP\/1.1 chunked or HTTP\/2 DATA) then flushes that single
chunk to the wire.
-}
sseBodyPopper :: IO (Maybe ServerSentEvent) -> IO (Maybe ByteString)
sseBodyPopper source = fmap (fmap renderServerSentEvent) source


{- | 'sseBodyPopper' for sources that also want to surface retry
directives and comments (typical for keep-alive heartbeats).
-}
sseBodyPopperFrames :: IO (Maybe SseFrame) -> IO (Maybe ByteString)
sseBodyPopperFrames source = fmap (fmap renderSseFrame) source


{- | Build a streaming SSE response body. The 'Content-Type' and
'Cache-Control' headers still need to be set on the 'Response'
record by the handler — this helper only constructs the body.
-}
sseResponseBody :: IO (Maybe ServerSentEvent) -> B.Body
sseResponseBody = B.BodyStream . sseBodyPopper


-- | Frame-aware variant of 'sseResponseBody'.
sseResponseBodyFrames :: IO (Maybe SseFrame) -> B.Body
sseResponseBodyFrames = B.BodyStream . sseBodyPopperFrames


-- ---------------------------------------------------------------------------
-- Server-side push channel
-- ---------------------------------------------------------------------------

{- $serverChannel
A multi-producer single-consumer event queue with a bounded
backlog. Producers push frames with 'sendSseEvent' \/
'sendSseFrame'; the HTTP body popper (typically built via
'awaitSseEvent') drains them in FIFO order. The bound provides
back-pressure: when the queue fills, producers block until the
consumer drains a slot, so a slow client can't OOM the producer.

'closeSseChannel' is non-blocking and idempotent. The consumer
finishes draining whatever is still in the queue and then
returns 'Nothing'; subsequent sends are silently dropped. This
shape lets a handler signal end-of-stream from any thread
without worrying about queue fullness, races with the
consumer, or running over the cap.
-}


-- | Multi-producer, single-consumer event channel.
data SseChannel = SseChannel
  { sseChannelQueue :: !(TBQueue SseFrame)
  , sseChannelClosed :: !(TVar Bool)
  }


{- | Allocate an empty channel with a fixed backlog cap. Producers
block in 'sendSseFrame' \/ 'sendSseEvent' when the queue is full.
-}
newSseChannel
  :: Int
  -- ^ backlog cap (in frames; @64@ is a reasonable default)
  -> IO SseChannel
newSseChannel cap = do
  q <- newTBQueueIO (fromIntegral (max 1 cap))
  c <- newTVarIO False
  pure
    SseChannel
      { sseChannelQueue = q
      , sseChannelClosed = c
      }


{- | Push a frame onto the channel. Blocks if the channel is full;
silently drops if the channel has already been closed (which
lets producers fire-and-forget after a 'closeSseChannel').
-}
sendSseFrame :: SseChannel -> SseFrame -> IO ()
sendSseFrame ch f = atomically $ do
  closed <- readTVar (sseChannelClosed ch)
  unless closed (writeTBQueue (sseChannelQueue ch) f)


-- | 'sendSseFrame' for a dispatched event.
sendSseEvent :: SseChannel -> ServerSentEvent -> IO ()
sendSseEvent ch = sendSseFrame ch . SseDispatch


-- | Push a @retry:@ reconnection-time directive.
sendSseRetry :: SseChannel -> Int -> IO ()
sendSseRetry ch = sendSseFrame ch . SseRetry


{- | Push a @:@-prefixed comment line. The conventional use is a
periodic keep-alive heartbeat against load balancers that idle
out quiet connections.
-}
sendSseComment :: SseChannel -> ByteString -> IO ()
sendSseComment ch = sendSseFrame ch . SseComment


{- | Close the channel. Non-blocking, idempotent. The consumer
continues to see any already-queued frames and then receives
'Nothing'; subsequent producer sends are silently dropped.
-}
closeSseChannel :: SseChannel -> IO ()
closeSseChannel ch =
  atomically (writeTVar (sseChannelClosed ch) True)


{- | Snapshot of the channel's closed bit. Useful for producers
that want to skip work that's about to be dropped.
-}
isSseChannelClosed :: SseChannel -> IO Bool
isSseChannelClosed ch = atomically (readTVar (sseChannelClosed ch))


{- | Block until the next frame is available; return 'Nothing' if
the channel is closed /and/ empty.

The semantics — drain then signal close — match how SSE bodies
want to behave on the wire: any event the handler queued before
shutting down still reaches the client.
-}
awaitSseFrame :: SseChannel -> IO (Maybe SseFrame)
awaitSseFrame ch = atomically (awaitFrameSTM ch)


{- | STM-level variant of 'awaitSseFrame' for callers composing
larger 'orElse' choices.
-}
awaitFrameSTM :: SseChannel -> STM (Maybe SseFrame)
awaitFrameSTM ch = do
  empty <- isEmptyTBQueue (sseChannelQueue ch)
  if not empty
    then Just <$> readTBQueue (sseChannelQueue ch)
    else do
      closed <- readTVar (sseChannelClosed ch)
      if closed then pure Nothing else retry


{- | 'awaitSseFrame' filtered to dispatched events; comments and
retry directives are passed through to the wire (when used with
'sseBodyPopperFrames') but skipped here.
-}
awaitSseEvent :: SseChannel -> IO (Maybe ServerSentEvent)
awaitSseEvent ch = loop
  where
    loop = do
      m <- awaitSseFrame ch
      case m of
        Nothing -> pure Nothing
        Just (SseDispatch ev) -> pure (Just ev)
        Just _ -> loop


-- ---------------------------------------------------------------------------
-- Internal: BOM handling
-- ---------------------------------------------------------------------------

{- | If the first three bytes of the very first chunk are the UTF-8
BOM (EF BB BF), drop them. The BOM check fires once per parser;
a partial BOM that arrives split across chunks still works,
because we only mark the BOM as handled once at least one
non-BOM byte has been seen (carrying any partial prefix
forward).
-}
stripBomIfNeeded :: SseParser -> ByteString -> (SseParser, ByteString)
stripBomIfNeeded p chunk
  | spBomChecked p = (p, chunk)
  | otherwise =
      let combined = spCarry p <> chunk
          len = BS.length combined
      in if len < 3 && combined `BS.isPrefixOf` bom
           then -- entirely a BOM prefix so far: keep it in carry, do
           -- not flip the BOM flag yet.
             (p {spCarry = combined}, BS.empty)
           else
             if bom `BS.isPrefixOf` combined
               then
                 ( p {spBomChecked = True, spCarry = BS.empty}
                 , BS.drop 3 combined
                 )
               else
                 ( p {spBomChecked = True, spCarry = BS.empty}
                 , combined
                 )
  where
    bom :: ByteString
    bom = BS.pack [0xEF, 0xBB, 0xBF]


-- ---------------------------------------------------------------------------
-- Internal: line splitting
-- ---------------------------------------------------------------------------

{- | Split a chunk into completed lines + leftover carry, honouring
CR \/ LF \/ CRLF line terminators and the CRLF-folding state
carried in from the previous chunk.

The returned line list is in stream order. The boolean is the
new \"just saw CR\" flag.
-}
splitLines
  :: ByteString
  -- ^ carry from previous chunk
  -> Bool
  -- ^ whether the previous chunk ended on a bare CR
  -> ByteString
  -- ^ new chunk
  -> ([ByteString], ByteString, Bool)
splitLines carry0 sawCR0 input
  | BS.null input = ([], carry0, sawCR0)
  | otherwise = go [] carry0 sawCR0 0
  where
    n = BS.length input
    -- Find the next index >= i that is CR or LF, plus whether it
    -- was a CR.
    nextTerm i = case BS.findIndex isTerm (BS.drop i input) of
      Nothing -> Nothing
      Just j ->
        let idx = i + j
            isCr = BS.index input idx == 0x0D
        in Just (idx, isCr)
    isTerm b = b == 0x0D || b == 0x0A
    go framesRev carry sawCR i
      | i >= n = (reverse framesRev, carry, sawCR)
      | sawCR && BS.index input i == 0x0A =
          -- LF after CR is the second half of a CRLF; skip it
          -- without emitting a (second) empty line.
          go framesRev carry False (i + 1)
      | otherwise = case nextTerm i of
          Nothing ->
            let tail_ = BS.drop i input
                carry' = if BS.null carry then tail_ else carry <> tail_
            in (reverse framesRev, carry', False)
          Just (termIdx, isCR) ->
            let pre = BS.take (termIdx - i) (BS.drop i input)
                line = if BS.null carry then pre else carry <> pre
            in go (line : framesRev) BS.empty isCR (termIdx + 1)


-- ---------------------------------------------------------------------------
-- Internal: line processing
-- ---------------------------------------------------------------------------

{- | Fold a batch of completed lines through the parser, accumulating
emitted frames in reverse order.
-}
foldLines :: SseParser -> [SseFrame] -> [ByteString] -> (SseParser, [SseFrame])
foldLines p acc [] = (p, acc)
foldLines p acc (l : ls) =
  let (p', emitted) = processLine p l
      acc' = prependReverse emitted acc
  in foldLines p' acc' ls


{- | @prependReverse xs acc@ pushes @xs@ onto @acc@ keeping the
per-line emission order. We push @x1, x2, x3@ as
@x3 : x2 : x1 : acc@ so the final 'reverse' gives the right
order.
-}
prependReverse :: [a] -> [a] -> [a]
prependReverse [] acc = acc
prependReverse (x : xs) acc = prependReverse xs (x : acc)


{- | Classify a completed line and apply it to the parser state,
returning any frames that result.
-}
processLine :: SseParser -> ByteString -> (SseParser, [SseFrame])
processLine p line
  -- Blank line: dispatch any in-flight event.
  | BS.null line = dispatch p
  -- Comment line (literal ':' first). Spec doesn't strip a
  -- leading space; we mirror that exactly so callers see the raw
  -- payload.
  | BS.head line == 0x3A =
      (p, [SseComment (BS.tail line)])
  | otherwise = case BS.elemIndex 0x3A line of
      Nothing ->
        -- No colon: per spec the whole line is the field name
        -- and the value is the empty string.
        applyField p line BS.empty
      Just i ->
        let name = BS.take i line
            after = BS.drop (i + 1) line
            value = stripOneLeadingSpace after
        in applyField p name value


stripOneLeadingSpace :: ByteString -> ByteString
stripOneLeadingSpace bs = case BS.uncons bs of
  Just (0x20, rest) -> rest
  _ -> bs


applyField
  :: SseParser
  -> ByteString
  -- ^ field name (case-sensitive per spec)
  -> ByteString
  {- ^ field value (already left-trimmed of a
  single space)
  -}
  -> (SseParser, [SseFrame])
applyField p name value = case name of
  "event" ->
    let v = if BS.null value then Nothing else Just value
    in (p {spEventType = v}, [])
  "data" ->
    ( p
        { spDataBuf = spDataBuf p <> WB.byteString value <> WB.word8 0x0A
        , spDataNonEmpty = True
        }
    , []
    )
  "id" ->
    -- Spec: ignore the field entirely if the value contains a NUL.
    if 0x00 `BS.elem` value
      then (p, [])
      else (p {spLastEventId = Just (eventIdUtf8 value)}, [])
  "retry" -> case parseAsciiDigits value of
    Just n -> (p, [SseRetry n])
    Nothing -> (p, [])
  _ -> (p, [])


-- SSE reads IDs as UTF-8. Copy valid runs only when an ill-formed
-- sequence needs replacement, consuming its maximal valid prefix.
eventIdUtf8 :: ByteString -> ByteString
eventIdUtf8 bytes
  | BS.isValidUtf8 bytes = bytes
  | otherwise = repairEventIdUtf8 bytes
{-# INLINE eventIdUtf8 #-}


repairEventIdUtf8 :: ByteString -> ByteString
repairEventIdUtf8 bytes = WB.toStrictByteString (scan 0 0 mempty)
  where
    size = BS.length bytes
    at = BS.index bytes

    scan start i !out
      | i >= size = out <> WB.byteString (BS.drop start bytes)
      | lead < 0x80 = scan start (i + 1) out
      | lead < 0xC2 || lead > 0xF4 = replace start i (i + 1) out
      | j < size && at j >= secondMin && at j <= secondMax =
          continue start i (j + 1) (width - 2) out
      | otherwise = replace start i j out
      where
        lead = at i
        j = i + 1
        width
          | lead < 0xE0 = 2
          | lead < 0xF0 = 3
          | otherwise = 4
        secondMin
          | lead == 0xE0 = 0xA0
          | lead == 0xF0 = 0x90
          | otherwise = 0x80
        secondMax
          | lead == 0xED = 0x9F
          | lead == 0xF4 = 0x8F
          | otherwise = 0xBF

    continue start i j (remaining :: Int) out
      | remaining == 0 = scan start j out
      | j < size && at j >= 0x80 && at j <= 0xBF =
          continue start i (j + 1) (remaining - 1) out
      | otherwise = replace start i j out

    replace start i resume out =
      let prefix = BS.take (i - start) (BS.drop start bytes)
      in scan resume resume (out <> WB.byteString prefix <> WB.byteString "\xEF\xBF\xBD")


dispatch :: SseParser -> (SseParser, [SseFrame])
dispatch p
  -- No @data:@ field at all: the spec says to drop the in-flight
  -- event /and/ clear the event-type buffer, but preserve
  -- lastEventId.
  | not (spDataNonEmpty p) =
      ( p
          { spEventType = Nothing
          , spDataBuf = mempty
          , spCommittedEventId = spLastEventId p
          }
      , []
      )
  | otherwise =
      let raw = WB.toStrictByteString (spDataBuf p)
          payload = stripTrailingLF raw
          ev =
            ServerSentEvent
              { sseEventType = spEventType p
              , sseEventId = spLastEventId p
              , sseData = payload
              }
          p' =
            p
              { spEventType = Nothing
              , spDataBuf = mempty
              , spDataNonEmpty = False
              , spCommittedEventId = spLastEventId p
              }
      in (p', [SseDispatch ev])


stripTrailingLF :: ByteString -> ByteString
stripTrailingLF bs = case BS.unsnoc bs of
  Just (rest, 0x0A) -> rest
  _ -> bs


{- | Parse a byte string of ASCII digits (0..9) into a non-negative
'Int'. Returns 'Nothing' for an empty string or any non-digit
byte. The EventSource spec demands this exact predicate for the
@retry:@ field.
-}
parseAsciiDigits :: ByteString -> Maybe Int
parseAsciiDigits bs
  | BS.null bs = Nothing
  | otherwise = go 0 0
  where
    n = BS.length bs
    go !acc !i
      | i >= n = Just acc
      | otherwise =
          let !b = BS.index bs i
          in if b >= 0x30 && b <= 0x39
               then go (acc * 10 + fromIntegral (b - 0x30)) (i + 1)
               else Nothing
