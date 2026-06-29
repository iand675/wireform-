module Network.HTTP2.Client
  ( ClientConfig (..)
  , defaultClientConfig
  , ClientRequest (..)
  , ClientResponse (..)
  , RequestBody (..)
    -- * Connection
  , ClientHandle
  , clientHandleConnection
  , withConnection
  , withConnectionOnTransport
    -- * Requests
  , sendRequest
  , sendRequestStreamId
  , withResponse
  , drainResponseBody
    -- * Errors
  , ClientStreamError (..)
    -- * Push promises (RFC 9113 §6.6)
  , PushPromiseReceived (..)
  , awaitPushResponse
    -- * Low-level pieces, re-used by TLS bring-up
  , sendClientPreface
  , clientRecvLoop
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
  (Exception, SomeException, bracket, catch, evaluate, finally, mask, throwIO, try)
import Control.Monad (when)
import Data.Bits ((.|.), (.&.), shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Internal as BSI
import Data.IORef
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import qualified Network.Socket as NS

import Network.HTTP2.Connection
import Wireform.Transport.Send (sendByteStringMany)
import Network.HTTP2.Frame
import Network.HTTP2.Frame.Types (decodeGoAway, decodeSettings)
import Network.HTTP2.HPACK
import Network.HTTP2.RateLimit (RateCounter, newRateCounter, tickRate)
import Network.HTTP2.Types
import qualified Network.HTTP2.Types as H2Types
import qualified Debug.Trace

data ClientConfig = ClientConfig
  { clientSettings :: !Settings
  , clientHost :: !String
  , clientPort :: !String
  }

defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig
  { clientSettings = defaultSettings
  , clientHost = "127.0.0.1"
  , clientPort = "80"
  }

data ClientRequest = ClientRequest
  { crMethod :: !ByteString
  , crPath :: !ByteString
  , crScheme :: !ByteString
  , crAuthority :: !ByteString
  , crHeaders :: ![(ByteString, ByteString)]
  , crBody :: !RequestBody
  }

-- | Outbound request body.
--
-- @ReqBodyStream@ is a pull producer that yields chunks until it
-- returns 'Nothing'. The client splits each chunk to the peer's
-- @SETTINGS_MAX_FRAME_SIZE@ and blocks on the connection-level
-- send flow-control window (HTTP\/2 §6.9) before pushing each
-- DATA frame; back-pressure on the peer back-propagates to the
-- producer naturally.
data RequestBody
  = ReqBodyNone
  | ReqBodyBytes !ByteString
  | ReqBodyStream !(IO (Maybe ByteString))

data ClientResponse = ClientResponse
  { crStreamId :: !StreamId
    -- ^ The stream id the request and response are running on.
    -- Useful for tracing and for upper-layer integrations that
    -- want to correlate frames with @h2c@-level diagnostics.
  , crStatus :: !Int
  , crResponseHeaders :: ![(ByteString, ByteString)]
  , crResponseBody :: !(IO (Maybe ByteString))
    -- ^ Pull producer for the response body.  Yields successive DATA
    -- frame payloads until the stream is half-closed by the peer, at
    -- which point subsequent calls return 'Nothing'.  The producer
    -- throws 'ClientStreamError' if the peer resets the stream or the
    -- connection drops before the body completes.
    --
    -- The producer is only valid for the lifetime of the underlying
    -- 'ClientHandle'; consume it before the surrounding
    -- 'withConnection' bracket exits.  'drainResponseBody' offers a
    -- convenience that materialises the whole body if you don't care
    -- about streaming.
  , crResponseTrailers :: !(IO [(ByteString, ByteString)])
    -- ^ Block on the peer's trailer block.  Returns @[]@ when the
    -- response had no trailers (the END_STREAM came in on DATA or on
    -- the initial HEADERS frame).  Must be called after the body has
    -- been fully drained — pulling trailers before observing the end
    -- of the body will block.
  , crCancel :: !(IO ())
    -- ^ Best-effort stream cancellation: emits @RST_STREAM(CANCEL)@
    -- to the peer and tears down the local inbox. Idempotent (the
    -- recv loop is also free to remove the inbox first). For
    -- streams started by 'sendRequest' this is fire-and-forget; for
    -- 'withResponse' the bracket already calls it on exit.
  , crPushPromises :: !(IO [PushPromiseReceived])
    -- ^ Push promises the server sent on this stream so far.
    -- Returns the list in arrival order; may be called multiple
    -- times; new items appear once 'crResponseBody' has been
    -- partially or fully consumed (the recv loop delivers
    -- PUSH_PROMISE frames interleaved with DATA frames).
  }

-- | Per-stream inbox the recv loop pushes into.
data StreamInbox = StreamInbox
  { siHeaders :: !(MVar (Either ClientStreamError ResponseHead))
    -- ^ Filled exactly once when the response @HEADERS@ block arrives
    -- (or when the stream is reset before any headers).
  , siBody :: !(TBQueue BodyItem)
    -- ^ Chunks streamed in by DATA frames, terminated by 'BodyEnd' or
    -- 'BodyError'.
  , siTrailers :: !(MVar [(ByteString, ByteString)])
    -- ^ Filled exactly once when END_STREAM has been observed: with
    -- the decoded trailer block (a HEADERS frame /after/ the body),
    -- or with @[]@ when END_STREAM came in on DATA \/ on the initial
    -- HEADERS frame.  Stream resets also fill this with @[]@ so that
    -- waiting readers wake up.
  , siSendWindow :: !FlowControl
    -- ^ Per-stream HTTP\/2 send window (RFC 9113 § 6.9.1).
    -- Initialised from the peer's @SETTINGS_INITIAL_WINDOW_SIZE@ and
    -- adjusted by per-stream @WINDOW_UPDATE@ frames; an outgoing DATA
    -- frame must reserve from both this window and the
    -- connection-level @connSendFlowControl@.
  , siRecvUnacked :: !(IORef Int)
    -- ^ Bytes received on this stream that the user has consumed but
    -- the recv loop hasn't yet acknowledged via a @WINDOW_UPDATE@.
    -- Refunded to the peer when it crosses 'recvWindowAckThreshold'.
  , siRecvWindow :: !(IORef Int)
    -- ^ Per-stream recv window remaining (RFC 9113 §6.9.1).
    -- Decremented by DATA bytes the peer sends; replenished when we
    -- emit a @WINDOW_UPDATE@.  Goes negative ⇒ peer overflowed our
    -- advertised window ⇒ stream-level @FLOW_CONTROL_ERROR@.
  , siPushPromises :: !(TVar [PushPromiseReceived])
    -- ^ Push promises received on this stream (RFC 9113 §6.6).
    -- Accumulated in LIFO order by the recv loop as PUSH_PROMISE
    -- frames arrive; exposed via 'crPushPromises'.  Reversed on
    -- read so callers see promises in arrival order.
  }

data ResponseHead = ResponseHead
  { rhStatus :: !Int
  , rhHeaders :: ![(ByteString, ByteString)]
  }

data BodyItem
  = BodyChunk !ByteString
  | BodyEnd
  | BodyError !ClientStreamError

-- | Errors that can be observed on a single response stream.
data ClientStreamError
  = ClientStreamReset !ErrorCode
    -- ^ The peer sent @RST_STREAM@.
  | ClientStreamProtocolError !ByteString
    -- ^ The peer violated the HTTP\/2 wire protocol on this stream.
  | ClientStreamConnectionClosed
    -- ^ The connection went away before the stream completed.
  | ClientStreamRefusedAfterGoAway !StreamId
    -- ^ The peer sent @GOAWAY@ with the carried @lastStreamId@
    -- before we opened this stream.  Per RFC 9113 § 6.8 the peer
    -- will refuse it; we don't bother sending the request.
  deriving stock (Eq, Show)

instance Exception ClientStreamError

-- | A push promise received from the server (RFC 9113 §6.6).  The
-- server may announce these on any open client-initiated stream to
-- indicate that it will proactively push the named resource.
--
-- @pprHeaders@ contains the HPACK-decoded pseudo-headers and regular
-- headers that describe the pushed request (same shape as a normal
-- request HEADERS block).  The wireform HTTP/2 client does not
-- currently fulfil push responses end-to-end; see
-- 'Network.HTTP.Client.Protocol.PushPromise' for the higher-level
-- wrapper.
data PushPromiseReceived = PushPromiseReceived
  { pprPromisedStreamId :: !StreamId
    -- ^ The server-assigned stream ID that will carry the pushed
    --   response DATA / HEADERS frames.
  , pprHeaders          :: ![(ByteString, ByteString)]
    -- ^ Decoded push-promise request headers.
  , pprFulfil           :: !(IO ClientResponse)
    -- ^ Block until the server sends the response headers for the
    --   promised stream.  The returned 'ClientResponse' has a live
    --   streaming body and must be consumed before the surrounding
    --   'withConnection' bracket exits.  Calling this more than once
    --   on the same promise is a programming error; the underlying
    --   MVar blocks forever on the second call.
  }

-- | A live client connection plus the bookkeeping needed to collect
-- per-stream responses. Acquire with 'withConnection' or
-- 'withConnectionOnTransport'.
data ClientHandle = ClientHandle
  { chConnection :: !Connection
  , chStreams :: !(IORef (Map.Map StreamId StreamInbox))
  , chPending :: !(IORef (Maybe Continuation))
  , chRecvUnacked :: !(IORef Int)
    -- ^ Connection-level recv-window debt (bytes received and
    -- consumed since the last connection-level @WINDOW_UPDATE@).
  , chActiveStreams :: !(TVar Int)
    -- ^ Count of streams the application has opened that haven't
    -- yet reached @END_STREAM@ \/ @RST_STREAM@ \/ connection close.
    -- 'registerAndSend' blocks here when the count is at the peer's
    -- @SETTINGS_MAX_CONCURRENT_STREAMS@.
  , chGoAway :: !(TVar (Maybe StreamId))
    -- ^ @Just lastId@ once the peer has sent a @GOAWAY@ frame with
    -- that @lastStreamId@.  'registerAndSend' refuses to open
    -- streams whose ID would exceed it; in-flight streams below the
    -- threshold complete normally.
  , chPingRate :: !RateCounter
    -- ^ PING frames received this window (RFC 9113 § 6.7 abuse).
  , chSettingsRate :: !RateCounter
    -- ^ SETTINGS frames received this window.
  , chRstRate :: !RateCounter
    -- ^ RST_STREAM frames received this window.
  , chEmptyDataRate :: !RateCounter
    -- ^ Empty DATA frames received this window (CPU-soak vector).
  , chMaxHeaderListSize :: !(Maybe Word32)
    -- ^ Our advertised @SETTINGS_MAX_HEADER_LIST_SIZE@.  We refuse
    -- ('ClientStreamError' on the inbox) any incoming HEADERS \/
    -- trailer block whose RFC 7541 §4.1 size exceeds this.  'Nothing'
    -- means unbounded (the spec default).
  , chRecvWindow :: !(IORef Int)
    -- ^ Connection-level recv window remaining.  Decremented by every
    -- incoming DATA frame's full payload (incl. padding) and
    -- replenished when we emit a connection-level @WINDOW_UPDATE@.
    -- Goes negative ⇒ peer overflowed our advertised window ⇒
    -- connection-level @FLOW_CONTROL_ERROR@.
  }

-- | Per-second caps for the control-plane rate counters.  These
-- numbers are deliberately generous for legitimate peers (a
-- well-behaved gRPC client sends one keep-alive PING every
-- 30s; even an aggressive one is well under 10\/s) and are
-- only intended to bound the damage a hostile peer can inflict
-- before we tear the connection down.
defaultPingPerSec, defaultSettingsPerSec, defaultRstPerSec, defaultEmptyPerSec :: Int
defaultPingPerSec     = 10
defaultSettingsPerSec = 5
defaultRstPerSec      = 100
defaultEmptyPerSec    = 50

-- | When per-stream or connection-level unacked bytes cross this
-- threshold the recv path emits a @WINDOW_UPDATE@ to replenish.
-- Half the default @SETTINGS_INITIAL_WINDOW_SIZE@ — large enough
-- that we don't spam updates, small enough that we don't stall the
-- peer.
recvWindowAckThreshold :: Int
recvWindowAckThreshold = 32768

clientHandleConnection :: ClientHandle -> Connection
clientHandleConnection = chConnection

-- | Pending header-block continuation state.
data Continuation = Continuation
  { contStreamId :: !StreamId
  , contInitialFlags :: !FrameFlags
  , contBuffer :: !ByteString
  }

withConnection :: ClientConfig -> (ClientHandle -> IO a) -> IO a
withConnection cfg action = do
  let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
  addrs <- NS.getAddrInfo (Just hints) (Just (clientHost cfg)) (Just (clientPort cfg))
  case addrs of
    [] -> error "No address found"
    (addr:_) -> bracket
      (NS.openSocket addr)
      NS.close
      $ \sock -> do
        NS.connect sock (NS.addrAddress addr)
        NS.setSocketOption sock NS.NoDelay 1
        let transport = socketTransport sock
        withConnectionOnTransport cfg transport (Just sock) action

-- | Run the client over an already-prepared 'Transport'. Useful for
-- HTTP/2-over-TLS bring-up where the caller has already done the TLS
-- handshake / ALPN negotiation.
--
-- The optional 'NS.Socket' is the original socket (if any), retained on
-- the 'Connection' so higher-level code can inspect peer addresses.
withConnectionOnTransport
  :: ClientConfig
  -> Transport
  -> Maybe NS.Socket
  -> (ClientHandle -> IO a)
  -> IO a
withConnectionOnTransport cfg transport mSock action = do
  conn <- newConnection ConnectionConfig
    { ccRole = RoleClient
    , ccSettings = clientSettings cfg
    , ccSocket = mSock
    , ccTransport = Just transport
    , ccOnGoAway = \_ _ _ -> pure ()
    }
  sendClientPreface conn (clientSettings cfg)
  streamsRef <- newIORef Map.empty
  pendingRef <- newIORef Nothing
  recvUnackedRef <- newIORef 0
  activeStreamsTv <- atomically (newTVar 0)
  goAwayTv <- atomically (newTVar Nothing)
  pingRate <- newRateCounter
  settingsRate <- newRateCounter
  rstRate <- newRateCounter
  emptyDataRate <- newRateCounter
  recvWindowRef <- newIORef (65535 :: Int)
  let handle = ClientHandle conn streamsRef pendingRef recvUnackedRef activeStreamsTv goAwayTv
                            pingRate settingsRate rstRate emptyDataRate
                            (settingsMaxHeaderListSize (clientSettings cfg))
                            recvWindowRef
  recvTid <- forkIO $ clientRecvLoop handle `finally` failOutstanding handle
  -- Tear-down order matters: stop the recv loop *before* the
  -- outer bracket closes the socket, otherwise the recv loop's
  -- in-flight @threadWaitRead@ sees the fd disappear and prints
  -- "threadWait: invalid argument (Bad file descriptor)" to
  -- stderr.
  action handle
    `finally` closeConnection conn NoError ""
    `finally` killThread recvTid

sendClientPreface :: Connection -> Settings -> IO ()
sendClientPreface conn settings = do
  let preface = connectionPreface
      params = encodeSettings settings
      settingsFrame = Frame
        (FrameHeader (fromIntegral (length params * 6)) FrameSettings 0 0)
        (SettingsFrame params)
  sendByteStringMany (connSendTransport conn) [preface, encodeFrame settingsFrame]

------------------------------------------------------------------------
-- Recv loop
------------------------------------------------------------------------

-- | Frame receive loop. Runs in its own thread (forked by
-- 'withConnectionOnTransport'). Dispatch is on the 'FrameType' carried
-- in the header — the 'FramePayload' pattern synonyms all match
-- 'FramePayloadRaw' regardless of frame type, so dispatching by
-- pattern would conflate unrelated frame shapes.
clientRecvLoop :: ClientHandle -> IO ()
clientRecvLoop handle = loop
  where
    conn = chConnection handle
    loop = do
      result <- recvFrame conn
      case result of
        Left _ -> pure ()
        Right frame -> do
          ok <- handleClientFrame handle frame
          if ok then loop else pure ()

handleClientFrame :: ClientHandle -> Frame -> IO Bool
handleClientFrame handle (Frame hdr (FramePayloadRaw body)) = case fhType hdr of
  FrameSettings
    | testFlag (fhFlags hdr) flagAck -> pure True
    | otherwise -> do
        n <- tickRate (chSettingsRate handle)
        if n > defaultSettingsPerSec
          then do
            closeConnection (chConnection handle) EnhanceYourCalm "SETTINGS flood"
            pure False
          else case decodeSettings body of
            Nothing -> do
              closeConnection (chConnection handle) FrameSizeError "malformed SETTINGS payload"
              pure False
            Just params -> do
              old <- readIORef (connRemoteSettings (chConnection handle))
              case applySettingsParams old params of
                Left _ -> do
                  closeConnection (chConnection handle) ProtocolError "invalid settings"
                  pure False
                Right newSettings -> do
                  atomicModifyIORef' (connRemoteSettings (chConnection handle)) (\_ -> (newSettings, ()))
                  -- Per-stream send windows follow
                  -- SETTINGS_INITIAL_WINDOW_SIZE changes (RFC 9113
                  -- § 6.9.2): each open stream's send window is
                  -- shifted by the delta of old vs new initial size.
                  adjustStreamWindowsForInitialChange
                    handle
                    (settingsInitialWindowSize old)
                    (settingsInitialWindowSize newSettings)
                  -- Peer's SETTINGS_HEADER_TABLE_SIZE caps the
                  -- dynamic table we may use when encoding HEADERS
                  -- for them.  Shrink our encoder's table to match;
                  -- the eviction inside 'setMaxSize' guarantees we
                  -- won't reference entries the peer no longer has.
                  -- RFC 7541 § 6.3 also wants us to emit a "Dynamic
                  -- Table Size Update" on the next header block; we
                  -- don't yet (TODO), so this is best-effort against
                  -- lenient peers.
                  when (settingsHeaderTableSize old /= settingsHeaderTableSize newSettings) $
                    withMVar (connHpackEncoder (chConnection handle)) $ \enc ->
                      setMaxSize enc (fromIntegral (settingsHeaderTableSize newSettings))
                  let ack = Frame (FrameHeader 0 FrameSettings flagAck 0) (SettingsFrame [])
                  sendFrame (chConnection handle) ack
                  pure True

  FramePing
    | testFlag (fhFlags hdr) flagAck -> pure True
    | otherwise -> do
        n <- tickRate (chPingRate handle)
        if n > defaultPingPerSec
          then do
            closeConnection (chConnection handle) EnhanceYourCalm "PING flood"
            pure False
          else do
            let pong = Frame (FrameHeader 8 FramePing flagAck 0) (PingFrame body)
            sendFrame (chConnection handle) pong
            pure True

  FrameWindowUpdate
    | BS.length body /= 4 -> pure True
    | otherwise -> do
        let inc = readWord32 body .&. 0x7FFFFFFF
            sid = fhStreamId hdr
        if sid == 0
          then atomically $ do
            _ <- releaseWindow (connSendFlowControl (chConnection handle))
                                (fromIntegral inc)
            pure ()
          else do
            inboxes <- readIORef (chStreams handle)
            case Map.lookup sid inboxes of
              Just inbox -> atomically $ do
                _ <- releaseWindow (siSendWindow inbox) (fromIntegral inc)
                pure ()
              Nothing -> pure ()
        pure True

  FrameGoAway -> do
    case decodeGoAway body of
      Just (lastId, code, debug) -> do
        -- Record the cutoff so 'registerAndSend' refuses future
        -- streams beyond it; keep the lower of any prior GOAWAY's
        -- lastId, because the peer may walk it down (RFC 9113 § 6.8).
        atomically $ modifyTVar' (chGoAway handle) $ \prev ->
          Just $ maybe lastId (\p -> min p lastId) prev
        connOnGoAway (chConnection handle) lastId code debug
      Nothing -> pure ()
    pure True

  FrameRSTStream
    | BS.length body /= 4 -> pure True
    | otherwise -> do
        n <- tickRate (chRstRate handle)
        if n > defaultRstPerSec
          then do
            closeConnection (chConnection handle) EnhanceYourCalm "RST_STREAM flood"
            pure False
          else do
            let sid = fhStreamId hdr
                code = word32ToErrorCodeLocal (readWord32 body)
            failStream handle sid (ClientStreamReset code)
            pure True

  FrameHeaders -> do
    let sid = fhStreamId hdr
        flags = fhFlags hdr
    if not (testFlag flags flagEndHeaders)
      then do
        -- 'body' is a slice of the recv ring buffer; force-copy
        -- before buffering across frames so the CONTINUATION
        -- assembly doesn't see overwritten bytes.  See note on
        -- 'forceCopyBS' below.
        copiedBody <- forceCopyBS body
        writeIORef (chPending handle) (Just Continuation
          { contStreamId = sid
          , contInitialFlags = flags
          , contBuffer = copiedBody
          })
        pure True
      else case stripHeaderPadding flags body of
        Nothing -> do
          failStream handle sid (ClientStreamProtocolError "malformed HEADERS frame")
          pure True
        Just block -> do
          -- HPACK's 'decodeString' can hand back literal-string
          -- slices of its input.  Our input here is itself a slice
          -- of the recv ring buffer, which the next 'recvBufferRead'
          -- will overwrite — so the slices' bytes turn to garbage
          -- by the time the user code looks at them.  Eagerly copy
          -- once at the boundary so every downstream slice is
          -- backed by stable memory.
          copied <- forceCopyBS block
          deliverHeaders handle sid flags copied

  FrameContinuation -> do
    pending <- readIORef (chPending handle)
    case pending of
      Nothing -> pure True
      Just c
        | contStreamId c /= fhStreamId hdr -> pure True
        | testFlag (fhFlags hdr) flagEndHeaders -> do
            writeIORef (chPending handle) Nothing
            let assembled = contBuffer c <> body
                origFlags = contInitialFlags c
            case stripHeaderPadding origFlags assembled of
              Nothing -> do
                failStream handle (fhStreamId hdr)
                  (ClientStreamProtocolError "malformed HEADERS continuation")
                pure True
              Just block -> do
                copied <- forceCopyBS block
                deliverHeaders handle (fhStreamId hdr) origFlags copied
        | otherwise -> do
            -- See note on CONTINUATION above: copy 'body' before
            -- splicing into the cross-frame buffer.
            copiedBody <- forceCopyBS body
            writeIORef (chPending handle)
              (Just c { contBuffer = contBuffer c <> copiedBody })
            pure True

  FrameData -> do
    let sid = fhStreamId hdr
        endStream = testFlag (fhFlags hdr) flagEndStream
    -- Empty-DATA flood guard: each empty DATA frame is a "free"
    -- frame from the peer's POV (no flow-control debit) but still
    -- forces us through the recv loop's per-frame work.  Cap.
    overFlood <-
      if BS.null body || (BS.length body == 1 && testFlag (fhFlags hdr) flagPadded)
        then do
          n <- tickRate (chEmptyDataRate handle)
          pure (n > defaultEmptyPerSec)
        else pure False
    if overFlood
      then do
        closeConnection (chConnection handle) EnhanceYourCalm "empty-DATA flood"
        pure False
      else case stripDataPadding (fhFlags hdr) body of
        Nothing -> do
          failStream handle sid (ClientStreamProtocolError "malformed DATA frame")
          pure True
        Just payload -> do
          inboxes <- readIORef (chStreams handle)
          case Map.lookup sid inboxes of
            Nothing -> pure True
            Just inbox -> do
              -- Recv-window accounting (RFC 9113 §6.9.1): the
              -- whole frame payload (including padding) is
              -- charged, not the post-padding bytes.
              let frameBytes = BS.length body
              streamWin <- atomicModifyIORef' (siRecvWindow inbox) $ \w ->
                let w' = w - frameBytes in (w', w')
              connWin <- atomicModifyIORef' (chRecvWindow handle) $ \w ->
                let w' = w - frameBytes in (w', w')
              if streamWin < 0
                then do
                  sendRstStream handle sid FlowControlError
                  failStream handle sid (ClientStreamReset FlowControlError)
                  pure True
                else if connWin < 0
                  then do
                    closeConnection (chConnection handle) FlowControlError
                      "connection recv window underflow"
                    pure False
                  else do
                    -- The recv loop just enqueues here; WINDOW_UPDATE
                    -- frames are emitted lazily by 'nextBodyChunk' once
                    -- the user has actually consumed the bytes -- so a
                    -- slow reader throttles the peer naturally.
                    -- 'forceCopyBS' (not 'BS.copy'): the payload is a
                    -- zero-copy slice of the recv ring buffer, and
                    -- 'BS.copy' is too lazy under inlining to make the
                    -- memcpy happen before the buffer is overwritten.
                    copied <- forceCopyBS payload
                    atomically $ writeTBQueue (siBody inbox) (BodyChunk copied)
                    when endStream $ do
                      _ <- tryPutMVar (siTrailers inbox) []
                      atomically $ writeTBQueue (siBody inbox) BodyEnd
                      closeStream handle sid
                    pure True

  FramePushPromise -> do
    -- RFC 9113 §6.6: PUSH_PROMISE carries the promised stream ID
    -- and a HPACK-encoded request header block in the payload.
    --
    -- We:
    -- 1. HPACK-decode the header block.
    -- 2. Allocate and register a StreamInbox for the *promised* stream
    --    ID so the recv loop can route HEADERS/DATA to it.
    -- 3. Record the promise (with a live pprFulfil action) on the
    --    requesting stream's inbox.
    let sid = fhStreamId hdr
    case stripPushPromisePayload (fhFlags hdr) body of
      Nothing -> pure True  -- malformed: ignore
      Just (promisedSid, block) -> do
        copied <- forceCopyBS block
        res <- withMVar (connHpackDecoder (chConnection handle)) $ \decoder ->
          decodeHeaderBlock decoder copied
        case res of
          Left _ -> do
            closeConnection (chConnection handle) CompressionError
              "HPACK decode failed in PUSH_PROMISE"
            pure False
          Right headers -> do
            hdrs <- forceCopyHeaders headers
            inboxes <- readIORef (chStreams handle)
            case Map.lookup sid inboxes of
              Nothing -> pure True
              Just reqInbox -> do
                -- Create an inbox for the promised stream so the recv loop
                -- can deliver its HEADERS and DATA frames.
                ourInitial <- settingsInitialWindowSize <$>
                  readIORef (connLocalSettings (chConnection handle))
                promisedInbox <- newPromisedStreamInbox (fromIntegral ourInitial)
                atomicModifyIORef' (chStreams handle)
                  (\m -> (Map.insert promisedSid promisedInbox m, ()))
                let pp = PushPromiseReceived
                      { pprPromisedStreamId = promisedSid
                      , pprHeaders = hdrs
                      , pprFulfil  = awaitPushResponse handle promisedSid promisedInbox
                      }
                atomically $ modifyTVar' (siPushPromises reqInbox) (pp :)
                pure True

  _ -> pure True

-- | Decode a HEADERS block and route it as either the initial
-- response headers or a trailer block.  Closes the stream on
-- END_STREAM.
deliverHeaders
  :: ClientHandle -> StreamId -> FrameFlags -> ByteString -> IO Bool
deliverHeaders handle sid flags block = do
  res0 <- withMVar (connHpackDecoder (chConnection handle)) $ \decoder ->
    decodeHeaderBlock decoder block
  -- 'forceCopyHeaders' rebuilds each name\/value into a fresh
  -- malloc'd buffer.  HPACK literals + huffman-decoded strings can
  -- otherwise stay as lazy thunks that read from @block@ (a recv
  -- ring-buffer slice) when the user finally inspects them — by
  -- which point the ring buffer has been recycled.
  res <- case res0 of
    Right hs -> Right <$> forceCopyHeaders hs
    Left e   -> pure (Left e)
  case res of
    Left _ -> do
      closeConnection (chConnection handle) CompressionError "HPACK decode failed"
      pure False
    Right headers | overClientHeaderLimit handle headers -> do
      -- Server sent us more headers than we said we'd accept.
      -- Cancel just this stream rather than tearing the connection.
      sendRstStream handle sid EnhanceYourCalm
      failStream handle sid (ClientStreamReset EnhanceYourCalm)
      pure True
    Right headers -> do
      inboxes <- readIORef (chStreams handle)
      case Map.lookup sid inboxes of
        Nothing -> pure True
        Just inbox -> do
          let (status, rest) = splitStatus headers
              endStream = testFlag flags flagEndStream
          -- 'tryPutMVar' is the discriminator: if the headers MVar
          -- was empty, this was the initial HEADERS; otherwise this
          -- is a trailer block.
          wasInitial <- tryPutMVar (siHeaders inbox) (Right ResponseHead
            { rhStatus = status
            , rhHeaders = rest
            })
          if wasInitial
            then do
              when endStream $ do
                -- Bodyless / trailer-less response.  Fill the
                -- trailers MVar so a reader doesn't block, push the
                -- body terminator, and release the stream.
                _ <- tryPutMVar (siTrailers inbox) []
                atomically $ writeTBQueue (siBody inbox) BodyEnd
                closeStream handle sid
            else do
              -- Trailer block: RFC 9113 §8.1 forbids pseudo-headers
              -- here, but a strict reject would conflict with too
              -- many real peers; we just drop pseudo-headers and
              -- deliver the rest.
              let cleaned = filter
                    (\(n, _) -> BS.null n || BS.head n /= 0x3A) headers
              _ <- tryPutMVar (siTrailers inbox) cleaned
              -- Trailer block always carries END_STREAM (else the
              -- peer is malformed).  Either way, push the body
              -- terminator and release the stream.
              atomically $ writeTBQueue (siBody inbox) BodyEnd
              closeStream handle sid
          pure True

-- | RFC 7541 §4.1 sizing: 32 + name + value per header.  Used to
-- enforce our advertised @SETTINGS_MAX_HEADER_LIST_SIZE@ on response
-- header / trailer blocks the peer sends.
-- | Walk a decoded header list and replace every name\/value with an
-- eagerly-copied @ByteString@.  See the long comment in
-- @Network.HTTP2.Server.forceCopyHeaders@ for the recv-ring-buffer
-- aliasing hazard this protects against.
forceCopyHeaders
  :: [(ByteString, ByteString)] -> IO [(ByteString, ByteString)]
forceCopyHeaders = mapM $ \(n, v) -> do
  n' <- forceCopyBS n
  v' <- forceCopyBS v
  pure (n', v')

overClientHeaderLimit
  :: ClientHandle -> [(ByteString, ByteString)] -> Bool
overClientHeaderLimit handle hs = case chMaxHeaderListSize handle of
  Nothing  -> False
  Just lim ->
    let total = foldr (\(n, v) acc -> acc + 32 + BS.length n + BS.length v) 0 hs
    in total > fromIntegral lim

splitStatus
  :: [(ByteString, ByteString)] -> (Int, [(ByteString, ByteString)])
splitStatus = go 0 []
  where
    go acc rest [] = (acc, reverse rest)
    go acc rest ((n, v) : xs)
      | n == ":status" = case BS8.readInt v of
          Just (k, _) -> go k rest xs
          Nothing     -> go acc rest xs
      | not (BS.null n) && BS.head n == 0x3A {- ':' -} = go acc rest xs
      | otherwise = go acc ((n, v) : rest) xs

-- | Notify any open inbox that the stream failed. Idempotent.
failStream :: ClientHandle -> StreamId -> ClientStreamError -> IO ()
failStream handle sid err = do
  inboxes <- readIORef (chStreams handle)
  case Map.lookup sid inboxes of
    Nothing -> pure ()
    Just inbox -> do
      _ <- tryPutMVar (siHeaders inbox) (Left err)
      _ <- tryPutMVar (siTrailers inbox) []
      atomically $ writeTBQueue (siBody inbox) (BodyError err)
      closeStream handle sid

-- | When the connection terminates, mark every outstanding stream as
-- 'ClientStreamConnectionClosed' so that blocked callers wake up.
-- The active-stream counter is reset to zero so any 'registerAndSend'
-- waiters wake (they will then observe the closed connection and
-- fail on their own).
failOutstanding :: ClientHandle -> IO ()
failOutstanding handle = do
  inboxes <- atomicModifyIORef' (chStreams handle) (\m -> (Map.empty, m))
  mapM_ failOne (Map.toList inboxes)
  atomically $ writeTVar (chActiveStreams handle) 0
  where
    failOne (_, inbox) = do
      _ <- tryPutMVar (siHeaders inbox) (Left ClientStreamConnectionClosed)
      _ <- tryPutMVar (siTrailers inbox) []
      atomically $ writeTBQueue (siBody inbox) (BodyError ClientStreamConnectionClosed)

------------------------------------------------------------------------
-- Sending
------------------------------------------------------------------------

-- | Send a request and wait for the response headers.
--
-- Returns as soon as the peer's response HEADERS frame has been
-- decoded; the response body streams in lazily via
-- @'crResponseBody'@.  Throws 'ClientStreamError' if the stream is
-- reset (or the connection drops) before the headers arrive.
--
-- 'sendRequest' is fire-and-forget on the request side: if the
-- caller is interrupted before the response is fully consumed, the
-- stream is left half-open and the peer keeps streaming until the
-- connection closes.  Use 'withResponse' to get RST_STREAM-on-abort
-- cancellation semantics.
sendRequest :: ClientHandle -> ClientRequest -> IO ClientResponse
sendRequest handle req = do
  (sid, inbox) <- registerAndSend handle req
  -- 'readMVar' rather than 'takeMVar': leaving the headers in the
  -- MVar is how 'deliverHeaders' distinguishes the initial HEADERS
  -- block from the trailer block (via @tryPutMVar siHeaders@,
  -- which only succeeds while the MVar is empty).  'takeMVar'
  -- would empty the slot and the next HEADERS frame on the
  -- stream would be silently re-classified as initial.
  headersResult <- readMVar (siHeaders inbox)
  case headersResult of
    Left err -> throwIO err
    Right rh -> pure ClientResponse
      { crStreamId = sid
      , crStatus = rhStatus rh
      , crResponseHeaders = rhHeaders rh
      , crResponseBody = nextBodyChunk handle sid inbox
      , crResponseTrailers = readMVar (siTrailers inbox)
      , crCancel = sendRstStream handle sid Cancel
                     `catch` (\(_ :: SomeException) -> pure ())
      , crPushPromises = reverse <$> readTVarIO (siPushPromises inbox)
      }

-- | Bracket-style request that cancels the stream if the action is
-- interrupted by an async (or sync) exception.
--
-- On exception we emit @RST_STREAM(CANCEL)@ to the peer and remove
-- the stream's inbox.  Use this whenever the caller might bail
-- mid-request (timeouts, racing several requests, cooperative
-- cancellation) — the unified 'sendRequest' will leave streams
-- dangling if interrupted.
withResponse
  :: ClientHandle
  -> ClientRequest
  -> (ClientResponse -> IO a)
  -> IO a
withResponse handle req action = mask $ \restore -> do
  (sid, inbox) <- registerAndSend handle req
  -- Cancel the stream if (a) the action raises an exception, or
  -- (b) the action returns without fully draining the body.  Both
  -- leave the stream in 'chStreams' (the recv loop only removes it
  -- on END_STREAM / RST_STREAM); 'closeStream' is idempotent.
  let cancelIfOpen reason = do
        streams <- readIORef (chStreams handle)
        when (Map.member sid streams) $ do
          sendRstStream handle sid Cancel
            `catch` (\(_ :: SomeException) -> pure ())
          _ <- tryPutMVar (siHeaders inbox) (Left reason)
          _ <- tryPutMVar (siTrailers inbox) []
          atomically $ writeTBQueue (siBody inbox) (BodyError reason)
          closeStream handle sid
  result <- try @SomeException $ restore $ do
    -- See note on 'readMVar' in 'sendRequest': 'takeMVar' would
    -- empty the slot and break trailer-block detection.
    headersResult <- readMVar (siHeaders inbox)
    case headersResult of
      Left err -> throwIO err
      Right rh -> action ClientResponse
        { crStreamId = sid
        , crStatus = rhStatus rh
        , crResponseHeaders = rhHeaders rh
        , crResponseBody = nextBodyChunk handle sid inbox
        , crResponseTrailers = readMVar (siTrailers inbox)
        , crCancel = cancelIfOpen ClientStreamConnectionClosed
        , crPushPromises = reverse <$> readTVarIO (siPushPromises inbox)
        }
  cancelIfOpen ClientStreamConnectionClosed
  case result of
    Left e -> throwIO e
    Right r -> pure r

-- | Eager byte-for-byte copy of a 'ByteString'.  Used to defang
-- zero-copy slices that share memory with the recv ring buffer
-- before they're stashed in long-lived per-stream queues.
--
-- This explicitly walks every byte so the underlying memcpy
-- happens /now/ rather than being deferred behind a lazy
-- 'unsafeDupablePerformIO' wrapper.  Bytestring's 'BS.copy' is
-- not safe here: under inlining the byte fetch can be hoisted
-- past the buffer-overwrite that motivated the copy in the
-- first place.
forceCopyBS :: ByteString -> IO ByteString
forceCopyBS (BSI.PS srcFp srcOff srcLen) = do
  dstFp <- BSI.mallocByteString srcLen
  withForeignPtr srcFp $ \src ->
    withForeignPtr dstFp $ \dst ->
      BSI.memcpy dst (src `plusPtr` srcOff) srcLen
  let !result = BSI.PS dstFp 0 srcLen
  -- Walk every byte once to materialise the memcpy synchronously.
  BS.foldr' (\b acc -> b `seq` acc) () result `seq` pure result

-- | Send an @RST_STREAM@ frame for the given stream ID.
sendRstStream :: ClientHandle -> StreamId -> ErrorCode -> IO ()
sendRstStream handle sid code =
  sendFrame (chConnection handle) $ Frame
    (FrameHeader 4 FrameRSTStream 0 sid)
    (RSTStreamFrame code)

-- | Remove a stream from the active map and (if it was present)
-- decrement the active-stream count so the next 'registerAndSend'
-- can proceed.  Idempotent.
closeStream :: ClientHandle -> StreamId -> IO ()
closeStream handle sid = do
  wasPresent <- atomicModifyIORef' (chStreams handle) $ \m ->
    if Map.member sid m
      then (Map.delete sid m, True)
      else (m, False)
  when wasPresent $
    atomically $ modifyTVar' (chActiveStreams handle) (\n -> n - 1)

-- | Wait for the server to deliver response HEADERS on a
-- server-pushed stream (RFC 9113 §8.4).  This is the fulfil action
-- stored in 'pprFulfil' of 'PushPromiseReceived'.
--
-- The call blocks until:
-- * The recv loop fills 'siHeaders' with the pushed response head
--   (normal case), or
-- * The stream is reset by the peer or the connection drops
--   ('ClientStreamError' is thrown).
awaitPushResponse :: ClientHandle -> StreamId -> StreamInbox -> IO ClientResponse
awaitPushResponse handle sid inbox = do
  headersResult <- readMVar (siHeaders inbox)
  case headersResult of
    Left err -> throwIO err
    Right rh -> pure ClientResponse
      { crStreamId         = sid
      , crStatus           = rhStatus rh
      , crResponseHeaders  = rhHeaders rh
      , crResponseBody     = nextBodyChunk handle sid inbox
      , crResponseTrailers = readMVar (siTrailers inbox)
      , crCancel           = sendRstStream handle sid Cancel
                               `catch` (\(_ :: SomeException) -> pure ())
      , crPushPromises     = reverse <$> readTVarIO (siPushPromises inbox)
      }

-- | Allocate a 'StreamInbox' for a server-initiated (pushed) stream.
-- Pushed streams are receive-only: the client never sends DATA on
-- them, so the send-flow-control window is initialised to 0.
newPromisedStreamInbox :: Int -> IO StreamInbox
newPromisedStreamInbox ourInitialRecv = do
  h  <- newEmptyMVar
  q  <- atomically (newTBQueue 64)
  t  <- newEmptyMVar
  sw <- atomically $ newFlowControl 0    -- send window: unused for pushed streams
  ru <- newIORef 0
  rw <- newIORef ourInitialRecv
  pp <- atomically (newTVar [])
  pure $ StreamInbox h q t sw ru rw pp

-- | Pull one chunk of the response body and replenish the recv
-- windows that the chunk consumed.
--
-- Returns 'Nothing' once the stream has been half-closed by the
-- peer; the end marker is re-queued so subsequent calls keep
-- returning 'Nothing' instead of blocking.  Throws on stream reset.
--
-- @WINDOW_UPDATE@ frames are coalesced: a per-stream or
-- connection-level @IORef@ accumulates byte counts and a single
-- update is sent when the count crosses 'recvWindowAckThreshold'.
-- Stream-level acks stop once the stream is half-closed (the peer
-- doesn't care about its window any more), but connection-level
-- acks keep flowing because the credit is shared across all
-- streams.
nextBodyChunk
  :: ClientHandle -> StreamId -> StreamInbox -> IO (Maybe ByteString)
nextBodyChunk handle sid inbox = do
  item <- atomically $ readTBQueue (siBody inbox)
  case item of
    BodyChunk bs -> do
      let n = BS.length bs
      ackConnectionRecv handle n
      ackStreamRecv handle sid inbox n
      pure (Just bs)
    BodyEnd -> do
      atomically $ writeTBQueue (siBody inbox) BodyEnd
      pure Nothing
    BodyError e -> throwIO e

-- | Tally @n@ bytes against the connection-level recv-window debt
-- and flush a @WINDOW_UPDATE@ on stream 0 if we've crossed the
-- threshold.
ackConnectionRecv :: ClientHandle -> Int -> IO ()
ackConnectionRecv handle n = do
  unacked <- atomicModifyIORef' (chRecvUnacked handle) $ \u ->
    let u' = u + n
    in if u' >= recvWindowAckThreshold
         then (0, u')
         else (u', 0)
  when (unacked > 0) $ do
    atomicModifyIORef' (chRecvWindow handle) (\w -> (w + unacked, ()))
    sendFrame (chConnection handle) $ Frame
      (FrameHeader 4 FrameWindowUpdate 0 0)
      (WindowUpdateFrame (fromIntegral unacked))

-- | Tally @n@ bytes against the per-stream recv-window debt and
-- flush a per-stream @WINDOW_UPDATE@ if we've crossed the
-- threshold.  Suppressed once the stream has been removed from
-- 'chStreams' (so we don't refund a closed stream's window).
ackStreamRecv :: ClientHandle -> StreamId -> StreamInbox -> Int -> IO ()
ackStreamRecv handle sid inbox n = do
  unacked <- atomicModifyIORef' (siRecvUnacked inbox) $ \u ->
    let u' = u + n
    in if u' >= recvWindowAckThreshold
         then (0, u')
         else (u', 0)
  when (unacked > 0) $ do
    -- Only emit if the stream is still open at our end.
    streams <- readIORef (chStreams handle)
    when (Map.member sid streams) $ do
      atomicModifyIORef' (siRecvWindow inbox) (\w -> (w + unacked, ()))
      sendFrame (chConnection handle) $ Frame
        (FrameHeader 4 FrameWindowUpdate 0 sid)
        (WindowUpdateFrame (fromIntegral unacked))

-- | Materialise an entire response body into a single 'ByteString'.
-- Convenience wrapper for callers that don't care about streaming;
-- equivalent to looping over 'crResponseBody' until it returns
-- 'Nothing' and concatenating the chunks.
drainResponseBody :: ClientResponse -> IO ByteString
drainResponseBody resp = go []
  where
    go acc = do
      mc <- crResponseBody resp
      case mc of
        Nothing -> pure $ BS.concat (reverse acc)
        Just bs -> go (bs : acc)

-- | Lower-level send that returns only the 'StreamId' (the response
-- arrives via the recv loop). Provided for callers that want
-- fire-and-forget behaviour; most users should prefer 'sendRequest'.
sendRequestStreamId :: ClientHandle -> ClientRequest -> IO StreamId
sendRequestStreamId handle req = do
  (sid, _inbox) <- registerAndSend handle req
  pure sid

registerAndSend
  :: ClientHandle -> ClientRequest -> IO (StreamId, StreamInbox)
registerAndSend handle req = do
  let conn = chConnection handle
  -- Honour the peer's SETTINGS_MAX_CONCURRENT_STREAMS by waiting
  -- (via STM retry) until the active-stream count drops below the
  -- limit.  Snapshot the limit out of the IORef first because we
  -- can't do IO inside STM.
  remoteMax <- settingsMaxConcurrentStreams <$> readIORef (connRemoteSettings conn)
  outcome <- atomically $ do
    -- A prior GOAWAY caps the IDs we may use.  We compute the next
    -- ID inside the same transaction so the cap check is consistent
    -- with what we'd allocate.
    nextSid <- readTVar (stNextStreamId (connStreamTable conn))
    goAway <- readTVar (chGoAway handle)
    case goAway of
      Just lastId | nextSid > lastId ->
        pure (Left (ClientStreamRefusedAfterGoAway lastId))
      _ -> do
        case remoteMax of
          Just m -> do
            active <- readTVar (chActiveStreams handle)
            if active >= fromIntegral m
              then retry
              else pure ()
          Nothing -> pure ()
        modifyTVar' (chActiveStreams handle) (+ 1)
        writeTVar (stNextStreamId (connStreamTable conn)) (nextSid + 2)
        pure (Right nextSid)
  sid <- case outcome of
    Left err -> throwIO err
    Right s  -> pure s
  remoteInitial <- settingsInitialWindowSize <$> readIORef (connRemoteSettings conn)
  ourInitial <- settingsInitialWindowSize <$> readIORef (connLocalSettings conn)
  inbox <- do
    h <- newEmptyMVar
    q <- atomically (newTBQueue 64)
    t <- newEmptyMVar
    sw <- atomically $ newFlowControl (fromIntegral remoteInitial)
    ru <- newIORef 0
    rw <- newIORef (fromIntegral ourInitial)
    pp <- atomically (newTVar [])
    pure $ StreamInbox h q t sw ru rw pp
  atomicModifyIORef' (chStreams handle) (\m -> (Map.insert sid inbox m, ()))
  let pseudoHeaders =
        [ (":method", crMethod req)
        , (":path", crPath req)
        , (":scheme", crScheme req)
        , (":authority", crAuthority req)
        ]
      allHeaders = pseudoHeaders <> crHeaders req
  headerBlock <- withMVar (connHpackEncoder conn) $ \encoder ->
    encodeHeaderBlock defaultEncodeStrategy encoder allHeaders
  let bodyKind = crBody req
      endStreamOnHeaders = case bodyKind of
        ReqBodyNone -> True
        ReqBodyBytes bs -> BS.null bs
        ReqBodyStream _ -> False
  maxFrame <- peerMaxFrameSize conn
  sendHeaderBlock conn sid endStreamOnHeaders 0 headerBlock maxFrame
  case bodyKind of
    ReqBodyNone -> pure ()
    ReqBodyBytes b
      | BS.null b -> pure ()
      | otherwise -> sendBodyOneShot conn inbox sid b
    -- Stream the request body on a separate thread so this function can
    -- return after the HEADERS frame; the caller then reads response
    -- headers/body concurrently with the request body still in flight.
    -- This is what makes full-duplex (bidirectional streaming) possible:
    -- an inline send would block until the whole request body had been
    -- written, deadlocking any peer that interleaves request and response.
    -- A failing producer aborts the stream with RST_STREAM(CANCEL).
    ReqBodyStream producer ->
      () <$ forkIO
        ( sendBodyStream conn inbox sid producer
            `catch` \(_ :: SomeException) ->
              sendRstStream handle sid Cancel
                `catch` \(_ :: SomeException) -> pure ()
        )
  pure (sid, inbox)

-- | Send a known-length body chunked to the peer's MAX_FRAME_SIZE,
-- terminating with END_STREAM.  Both connection- and stream-level
-- flow windows are respected.
sendBodyOneShot :: Connection -> StreamInbox -> StreamId -> ByteString -> IO ()
sendBodyOneShot conn inbox sid b = do
  maxFrame <- peerMaxFrameSize conn
  loop b maxFrame
  where
    loop bs maxFrame
      | BS.length bs <= maxFrame = sendDataFrame conn inbox sid True bs
      | otherwise = do
          let (chunk, rest) = BS.splitAt maxFrame bs
          sendDataFrame conn inbox sid False chunk
          loop rest maxFrame

-- | Pump a streaming-body producer onto the wire as a sequence of DATA
-- frames, terminated by an empty DATA + END_STREAM.  Each chunk is
-- split to the peer's MAX_FRAME_SIZE and gated on both the
-- connection-level and per-stream send flow windows.
sendBodyStream
  :: Connection -> StreamInbox -> StreamId -> IO (Maybe ByteString) -> IO ()
sendBodyStream conn inbox sid producer = loop
  where
    loop = do
      mChunk <- producer
      case mChunk of
        Nothing -> sendDataFrame conn inbox sid True BS.empty
        Just bs
          | BS.null bs -> loop
          | otherwise -> do
              maxFrame <- peerMaxFrameSize conn
              sendChunkChopped conn inbox sid bs maxFrame
              loop

sendChunkChopped
  :: Connection -> StreamInbox -> StreamId -> ByteString -> Int -> IO ()
sendChunkChopped conn inbox sid bs maxFrame
  | BS.null bs = pure ()
  | BS.length bs <= maxFrame = sendDataFrame conn inbox sid False bs
  | otherwise = do
      let (chunk, rest) = BS.splitAt maxFrame bs
      sendDataFrame conn inbox sid False chunk
      sendChunkChopped conn inbox sid rest maxFrame

-- | Send one DATA frame, blocking until /both/ the connection-level
-- and per-stream send flow windows have room (HTTP\/2 §6.9 — peers
-- enforce flow control on both layers and a sender that ignores
-- either is a protocol error).
sendDataFrame :: Connection -> StreamInbox -> StreamId -> Bool -> ByteString -> IO ()
sendDataFrame conn inbox sid endStream bs = do
  let n = BS.length bs
  -- An empty terminator DATA frame doesn't consume flow window.
  if n == 0
    then sendIt
    else do
      atomically $ do
        cw <- availableWindow (connSendFlowControl conn)
        sw <- availableWindow (siSendWindow inbox)
        if cw >= fromIntegral n && sw >= fromIntegral n
          then do
            _ <- consumeWindow (connSendFlowControl conn) (fromIntegral n)
            _ <- consumeWindow (siSendWindow inbox) (fromIntegral n)
            pure ()
          else retry
      sendIt
  where
    sendIt = sendFrame conn $ Frame
      (FrameHeader (fromIntegral (BS.length bs)) FrameData
        (if endStream then flagEndStream else 0) sid)
      (DataFrame bs)

-- | Walk every open stream and shift its send window by the delta of
-- @oldInit@ vs @newInit@ (RFC 9113 § 6.9.2).  The peer's
-- @SETTINGS_INITIAL_WINDOW_SIZE@ governs the /baseline/ from which
-- per-stream @WINDOW_UPDATE@ frames accumulate, so a settings change
-- mid-connection has to fan out to every stream's window.
--
-- Overflow ('Left' result from 'updateInitialWindowSize') is dropped
-- silently here: the spec asks for @FLOW_CONTROL_ERROR@ but we can't
-- escalate cleanly from a recv-loop side-effect.  In practice the
-- only way to overflow is a peer that sent an absurd new initial
-- size; degrading to \"window unchanged\" still keeps the connection
-- alive.
adjustStreamWindowsForInitialChange
  :: ClientHandle -> Word32 -> Word32 -> IO ()
adjustStreamWindowsForInitialChange handle oldInit newInit
  | oldInit == newInit = pure ()
  | otherwise = do
      inboxes <- readIORef (chStreams handle)
      atomically $ mapM_ adjust (Map.elems inboxes)
  where
    adjust inbox = do
      _ <- updateInitialWindowSize
             (siSendWindow inbox)
             (fromIntegral oldInit)
             (fromIntegral newInit)
      pure ()

peerMaxFrameSize :: Connection -> IO Int
peerMaxFrameSize conn = do
  s <- readIORef (connRemoteSettings conn)
  -- The peer-advertised value is a Word32; we clamp to Int range
  -- (the protocol minimum / maximum are both well below 2^31).
  pure (fromIntegral (settingsMaxFrameSize s))


------------------------------------------------------------------------
-- Frame helpers
------------------------------------------------------------------------

-- | Strip the @Padded@ prefix/suffix from a DATA / HEADERS payload.
-- Returns 'Nothing' when the pad length byte claims more padding than
-- the frame contains.
stripDataPadding :: FrameFlags -> ByteString -> Maybe ByteString
stripDataPadding flags bs
  | not (testFlag flags flagPadded) = Just bs
  | BS.null bs = Nothing
  | otherwise =
      let padLen = fromIntegral (BS.head bs)
          total  = BS.length bs
      in if padLen + 1 > total
           then Nothing
           else Just (BS.take (total - padLen - 1) (BS.drop 1 bs))

-- | Strip the optional padding /and/ PRIORITY (flag 'flagPriority')
-- prefixes from a HEADERS payload.
stripHeaderPadding :: FrameFlags -> ByteString -> Maybe ByteString
stripHeaderPadding flags bs0 = do
  bs1 <- stripDataPadding flags bs0
  if testFlag flags flagPriority
    then if BS.length bs1 >= 5 then Just (BS.drop 5 bs1) else Nothing
    else Just bs1

-- | Decode a PUSH_PROMISE payload into (promisedStreamId, headerBlock).
-- Strips optional padding (RFC 9113 §6.6).
stripPushPromisePayload :: FrameFlags -> ByteString -> Maybe (StreamId, ByteString)
stripPushPromisePayload flags bs0 = do
  bs1 <- stripDataPadding flags bs0
  if BS.length bs1 < 4
    then Nothing
    else Just ( readWord32 bs1 .&. 0x7FFFFFFF
              , BS.drop 4 bs1
              )

readWord32 :: ByteString -> Word32
readWord32 bs =
  (fromIntegral (BS.index bs 0) `shiftL` 24)
    .|. (fromIntegral (BS.index bs 1) `shiftL` 16)
    .|. (fromIntegral (BS.index bs 2) `shiftL` 8)
    .|. fromIntegral (BS.index bs 3)

word32ToErrorCodeLocal :: Word32 -> ErrorCode
word32ToErrorCodeLocal w = case w of
  0x0 -> NoError
  0x1 -> ProtocolError
  0x2 -> InternalError
  0x3 -> FlowControlError
  0x4 -> SettingsTimeout
  0x5 -> H2Types.StreamClosed
  0x6 -> FrameSizeError
  0x7 -> RefusedStream
  0x8 -> Cancel
  0x9 -> CompressionError
  0xa -> ConnectError
  0xb -> EnhanceYourCalm
  0xc -> InadequateSecurity
  0xd -> HTTP11Required
  other -> UnknownError other
