module Network.HTTP2.Server
  ( ServerConfig (..)
  , defaultServerConfig
  , Request (..)
  , Response (..)
  , ResponseBody (..)
  , defaultResponse
  , runServer
  , runServerOnSocket
  , runServerOnTransport
  ) where

import Control.Concurrent (forkIO, killThread, myThreadId, threadDelay, throwTo, ThreadId)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (Exception, bracket, catch, handle, SomeException, finally, onException, throwIO)
import Control.Monad (unless, when)
import GHC.Clock (getMonotonicTime)
import System.Timeout (timeout)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Network.Socket (Socket)
import qualified Network.Socket as NS

import qualified Data.ByteString.Internal as BSI

import Data.Bits ((.|.), (.&.), shiftL)
import qualified Data.Map.Strict as Map
import Data.Word (Word32, Word64)

import Network.HTTP2.Connection
import Network.HTTP2.Frame
import Network.HTTP2.Frame.Types (decodeGoAway, decodeSettings)
import Network.HTTP2.HPACK
import Network.HTTP2.RateLimit (RateCounter, newRateCounter, tickRate)
import Network.HTTP2.Types hiding (StreamClosed)
import qualified Network.HTTP2.Types as Types

data ServerConfig = ServerConfig
  { serverSettings :: !Settings
  , serverHost :: !String
  , serverPort :: !String
  , serverHandler :: Request -> (Response -> IO ()) -> IO ()
  , serverForkConnection :: IO () -> IO ThreadId
    -- ^ How to fork a new thread for each accepted connection.
    -- Default: 'forkIO'.
    -- Use 'forkOn n' for pinned-core scheduling,
    -- or 'forkOS' for bound OS threads.
  , serverForkStream :: IO () -> IO ThreadId
    -- ^ How to fork a new thread for each concurrent stream handler.
    -- Default: 'forkIO'.
  , serverIdleTimeoutSeconds :: !(Maybe Double)
    -- ^ Close a connection if no frame has arrived from the peer for
    -- this many seconds (send GOAWAY first).  'Nothing' disables the
    -- check.  Default: @Just 120@ — matches @nginx@'s
    -- @keepalive_timeout@ ballpark.
  , serverHeaderBlockTimeoutSeconds :: !(Maybe Double)
    -- ^ Maximum time the peer may take between the initial @HEADERS@
    -- and the @END_HEADERS@ on the final @CONTINUATION@.  Caps slow
    -- header-block reads.  'Nothing' disables.  Default: @Just 10@.
  , serverPrefaceTimeoutSeconds :: !(Maybe Double)
    -- ^ Maximum time the peer may take to send the connection preface
    -- after the TCP\/TLS handshake completes.  'Nothing' disables.
    -- Default: @Just 5@.
  }

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { serverSettings = defaultSettings
  , serverHost = "0.0.0.0"
  , serverPort = "8080"
  , serverHandler = \_ respond -> respond defaultResponse
  , serverForkConnection = forkIO
  , serverForkStream = forkIO
  , serverIdleTimeoutSeconds = Just 120
  , serverHeaderBlockTimeoutSeconds = Just 10
  , serverPrefaceTimeoutSeconds = Just 5
  }

data Request = Request
  { requestMethod :: !ByteString
  , requestPath :: !ByteString
  , requestScheme :: !ByteString
  , requestAuthority :: !ByteString
  , requestHeaders :: ![(ByteString, ByteString)]
  , requestBody :: !(IO ByteString)
  , requestStreamId :: !StreamId
  , requestTrailers :: !(IO [(ByteString, ByteString)])
    -- ^ Block on the peer's trailer block.  Returns @[]@ when the
    -- request had no trailers (the END_STREAM came in on DATA or on
    -- the initial HEADERS frame).  Must be called after the body
    -- has been fully drained. A malformed trailer block raises
    -- 'ConnectionError' for this stream.
  }

data Response = Response
  { responseStatus :: !Int
  , responseHeaders :: ![(ByteString, ByteString)]
  , responseBody :: !ResponseBody
  , responseTrailers :: ![(ByteString, ByteString)]
    -- ^ Static trailers, sent before any deferred fields in the
    -- terminating field block.
  }

data ResponseBody
  = ResponseBodyEmpty
  | ResponseBodyBS !ByteString
  | ResponseBodyStream (IO (Maybe ByteString))
  | ResponseBodyStreamWithTrailers !(IO (Maybe ByteString)) !(IO [(ByteString, ByteString)])
    -- ^ Evaluate the trailer action when the body producer returns 'Nothing'.
  | ResponseBodyEmptyWithTrailers !(IO [(ByteString, ByteString)])
    -- ^ Evaluate the trailer action after sending the initial headers.
  | ResponseBodyBSWithTrailers !ByteString !(IO [(ByteString, ByteString)])
    -- ^ Evaluate the trailer action after sending the buffered content.

defaultResponse :: Response
defaultResponse = Response
  { responseStatus = 200
  , responseHeaders = []
  , responseBody = ResponseBodyEmpty
  , responseTrailers = []
  }

runServer :: ServerConfig -> IO ()
runServer cfg = do
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just (serverHost cfg)) (Just (serverPort cfg))
  case addrs of
    [] -> error "No address found"
    (addr:_) -> bracket
      (NS.openSocket addr)
      NS.close
      $ \sock -> do
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.setSocketOption sock NS.NoDelay 1
        NS.bind sock (NS.addrAddress addr)
        NS.listen sock 128
        acceptLoop cfg sock

acceptLoop :: ServerConfig -> Socket -> IO ()
acceptLoop cfg listenSock = do
  (clientSock, _) <- NS.accept listenSock
  NS.setSocketOption clientSock NS.NoDelay 1
  _ <- serverForkConnection cfg $
    handleClient cfg clientSock `catch` (\(_ :: SomeException) -> NS.close clientSock)
  acceptLoop cfg listenSock

runServerOnSocket :: ServerConfig -> Socket -> IO ()
runServerOnSocket cfg sock = handleClient cfg sock

-- | Run the HTTP/2 server over an arbitrary 'Transport' (e.g. a TLS
-- context). The transport must already be live: the caller is
-- responsible for the TLS handshake / ALPN negotiation.
runServerOnTransport :: ServerConfig -> Transport -> IO ()
runServerOnTransport cfg transport = handleTransport cfg transport

handleClient :: ServerConfig -> Socket -> IO ()
handleClient cfg sock = do
  let transport = socketTransport sock
  handleTransport cfg transport `finally` gracefulClose sock

-- | Half-close the write side first (so any pending GOAWAY / RST
-- frames make it out), then close the socket.
gracefulClose :: Socket -> IO ()
gracefulClose sock = do
  (NS.shutdown sock NS.ShutdownSend) `catch` (\(_ :: SomeException) -> pure ())
  NS.close sock

handleTransport :: ServerConfig -> Transport -> IO ()
handleTransport cfg transport = do
  -- Slowloris guard #1: cap how long the peer can sit on the socket
  -- without sending the connection preface.  A misbehaving peer that
  -- never sends bytes would otherwise pin a server thread forever.
  mPreface <- withMaybeTimeout (serverPrefaceTimeoutSeconds cfg) $
    recvExactTransport transport (BS.length connectionPreface)
  case mPreface of
    Nothing -> pure ()
    Just preface | preface /= connectionPreface -> pure ()
    Just _ -> do
      conn <- newConnectionFromTransport
                RoleServer
                (serverSettings cfg)
                (\_ _ _ -> pure ())
                transport
      sendServerPreface conn (serverSettings cfg)
      streamsRef <- newIORef Map.empty
      lastPeerStreamRef <- newIORef 0
      connRecvUnackedRef <- newIORef 0
      -- Our advertised connection-level recv window starts at the
      -- RFC default of 65535 (the connection window is never affected
      -- by SETTINGS_INITIAL_WINDOW_SIZE, RFC 9113 §6.9.2).  We
      -- decrement it as DATA arrives and replenish when we send
      -- connection-level @WINDOW_UPDATE@; a negative value means the
      -- peer has overflowed what we advertised.
      connRecvWindowRef <- newIORef (65535 :: Int)
      rl <- ServerRateLimiters
        <$> newRateCounter <*> newRateCounter
        <*> newRateCounter <*> newRateCounter
      -- Activity timestamp updated by the recv loop on every frame.
      -- The idle / continuation watchdog reads it and shuts us down
      -- if too much time has passed.
      now0 <- getMonotonicTime
      lastActivityRef <- newIORef now0
      -- continuationStart: when the in-flight header block began
      -- arriving (HEADERS without END_HEADERS).  Reset to Nothing
      -- whenever END_HEADERS lands.  Slowloris guard #2.
      continuationStartRef <- newIORef Nothing
      loopTid <- myThreadId
      watchdog <- forkIO $ idleWatchdog cfg conn loopTid
                             lastActivityRef continuationStartRef
      -- The loop decodes frames from zero-copy slices of the receive
      -- ring, so the ring is released only here, after the loop (the
      -- only reader) has stopped and the watchdog can no longer wake it.
      -- A wake that was queued while the finalizer ran lands in the
      -- outer handler.
      handle (\IdleWatchdogFired -> pure ()) $
        ( connectionLoop cfg conn streamsRef lastPeerStreamRef
                         connRecvUnackedRef connRecvWindowRef rl
                         lastActivityRef continuationStartRef
            `catch` (\IdleWatchdogFired -> pure ())
            `catch` (\(_ :: ConnectionError) -> pure ())
        )
          `finally` (killThread watchdog >> releaseConnection conn)

-- | Thrown by 'idleWatchdog' to the connection-loop thread after it has
-- closed the connection, so a loop parked in @recv@ wakes up and exits.
data IdleWatchdogFired = IdleWatchdogFired
  deriving stock (Show)

instance Exception IdleWatchdogFired

-- | Per-stream state we track at the server boundary.
data StreamRec = StreamRec
  { srState :: !ServerStreamState
  , srRemainingLength :: !(Maybe Word64)
    -- ^ Bytes still owed against the declared content length, excluding padding.
  , srBodyQueue :: !(TBQueue BodyChunkItem)
    -- ^ Per-stream inbox for the request body.  Each DATA frame's
    -- payload becomes a 'BodyChunkBytes'; 'BodyChunkEnd' marks the
    -- final END_STREAM and is re-queued by the reader so subsequent
    -- 'requestBody' calls keep returning the empty 'ByteString'.
  , srSendWindow :: !FlowControl
    -- ^ Per-stream HTTP\/2 send window (RFC 9113 § 6.9.1).
    -- Initialised from the peer's @SETTINGS_INITIAL_WINDOW_SIZE@ and
    -- replenished by per-stream @WINDOW_UPDATE@ frames; an outgoing
    -- DATA frame must reserve from both this window and
    -- 'connSendFlowControl'.
  , srRecvUnacked :: !(IORef Int)
    -- ^ Bytes received on this stream that the handler has consumed
    -- but the server hasn't yet acknowledged via a @WINDOW_UPDATE@.
  , srRecvWindow :: !(IORef Int)
    -- ^ Per-stream recv window remaining (RFC 9113 §6.9.1).
    -- Initialised from our own @SETTINGS_INITIAL_WINDOW_SIZE@,
    -- decremented on every DATA frame, replenished on every
    -- @WINDOW_UPDATE@ we emit.  When this goes negative the peer
    -- has overflowed our advertised window: stream-level
    -- @FLOW_CONTROL_ERROR@ via @RST_STREAM@.
  , srTrailers :: !(MVar (Either ConnectionError [(ByteString, ByteString)]))
    -- ^ Filled exactly once when END_STREAM has been observed:
    -- with the decoded trailer block from a final HEADERS frame, or
    -- with @[]@ when END_STREAM came in on DATA \/ on the initial
    -- HEADERS frame. A rejected trailer block carries its stream error.
  }

data BodyChunkItem
  = BodyChunkBytes !ByteString
  | BodyChunkEnd
  | BodyChunkError !ConnectionError

data ServerStreamState
  = StOpen
  | StHalfClosedRemote
  | StClosedSrv
  deriving stock (Eq, Show)

freshStream
  :: Word32          -- ^ peer's @SETTINGS_INITIAL_WINDOW_SIZE@
  -> Word32          -- ^ our @SETTINGS_INITIAL_WINDOW_SIZE@
  -> Maybe Word64
  -> ServerStreamState
  -> IO StreamRec
freshStream peerInit ourInit el st = do
  q <- atomically $ newTBQueue 64
  sw <- atomically $ newFlowControl (fromIntegral peerInit)
  ru <- newIORef 0
  rw <- newIORef (fromIntegral ourInit)
  trailers <- newEmptyMVar
  pure $ StreamRec st el q sw ru rw trailers

-- | Threshold for coalesced @WINDOW_UPDATE@ frames.  Half the
-- default @SETTINGS_INITIAL_WINDOW_SIZE@ — large enough that we
-- don't spam updates, small enough that we don't stall the peer.
recvWindowAckThreshold :: Int
recvWindowAckThreshold = 32768

-- | Pull the next request-body chunk from a stream's body queue,
-- replenishing the recv windows that the chunk consumed.
--
-- Returns 'BS.empty' once END_STREAM has been delivered; subsequent
-- calls keep returning empty (we re-queue the end marker so
-- defensive readers don't deadlock).
--
-- @WINDOW_UPDATE@ frames are emitted lazily here rather than eagerly
-- when DATA arrives, so a slow handler back-pressures the peer via
-- the recv window.
makeRequestBody
  :: Connection
  -> IORef Int       -- ^ connection-level recv-unacked counter
  -> IORef Int       -- ^ connection-level recv-window mirror
  -> StreamId
  -> StreamRec
  -> IO ByteString
makeRequestBody conn connUnackedRef connWindowRef sid sr = do
  item <- atomically $ readTBQueue (srBodyQueue sr)
  case item of
    BodyChunkBytes bs -> do
      let n = BS.length bs
      ackConnRecv conn connUnackedRef connWindowRef n
      ackStreamRecv conn sid (srRecvUnacked sr) (srRecvWindow sr) n
      pure bs
    BodyChunkEnd -> do
      atomically $ writeTBQueue (srBodyQueue sr) BodyChunkEnd
      pure BS.empty
    BodyChunkError err -> do
      atomically $ writeTBQueue (srBodyQueue sr) item
      throwIO err

-- | Connection-level recv-window ack (stream 0).  When the unacked
-- counter crosses 'recvWindowAckThreshold', emit a @WINDOW_UPDATE@
-- and replenish the recv-window mirror so we can detect overflow on
-- the next DATA frame.
ackConnRecv :: Connection -> IORef Int -> IORef Int -> Int -> IO ()
ackConnRecv conn ref windowRef n = do
  unacked <- atomicModifyIORef' ref $ \u ->
    let u' = u + n
    in if u' >= recvWindowAckThreshold
         then (0, u')
         else (u', 0)
  when (unacked > 0) $ do
    atomicModifyIORef' windowRef (\w -> (w + unacked, ()))
    sendFrame conn $ Frame
      (FrameHeader 4 FrameWindowUpdate 0 0)
      (WindowUpdateFrame (fromIntegral unacked))

-- | Per-stream recv-window ack.
ackStreamRecv :: Connection -> StreamId -> IORef Int -> IORef Int -> Int -> IO ()
ackStreamRecv conn sid ref windowRef n = do
  unacked <- atomicModifyIORef' ref $ \u ->
    let u' = u + n
    in if u' >= recvWindowAckThreshold
         then (0, u')
         else (u', 0)
  when (unacked > 0) $ do
    atomicModifyIORef' windowRef (\w -> (w + unacked, ()))
    sendFrame conn $ Frame
      (FrameHeader 4 FrameWindowUpdate 0 sid)
      (WindowUpdateFrame (fromIntegral unacked))

-- | Receive exactly @n@ bytes from a 'Transport'. Used only to read the
-- connection preface; the per-connection 'RecvBuffer' takes over after.
recvExactTransport :: Transport -> Int -> IO ByteString
recvExactTransport t n = go n []
  where
    go 0 acc = pure (BS.concat (reverse acc))
    go remaining acc = do
      chunk <- chunkFromTransport t (min remaining 4096)
      if BS.null chunk
        then pure (BS.concat (reverse acc))
        else go (remaining - BS.length chunk) (chunk : acc)

-- | Pull a chunk of up to @n@ bytes via the transport's @recvBuf@.
chunkFromTransport :: Transport -> Int -> IO ByteString
chunkFromTransport t n = BSI.createUptoN n $ \ptr -> tRecvBuf t ptr n

sendServerPreface :: Connection -> Settings -> IO ()
sendServerPreface conn settings = do
  let params = encodeSettings settings
      frame = Frame
        (FrameHeader (fromIntegral (length params * 6)) FrameSettings 0 0)
        (SettingsFrame params)
  sendFrame conn frame

-- | Per-connection rate limiters that protect the server's
-- recv loop from hostile control-plane floods.
data ServerRateLimiters = ServerRateLimiters
  { srlPing     :: !RateCounter
  , srlSettings :: !RateCounter
  , srlRst      :: !RateCounter
  , srlEmpty    :: !RateCounter
  }

-- Per-second caps; see Network.HTTP2.Client for the
-- corresponding constants on the client side.
srlPingCap, srlSettingsCap, srlRstCap, srlEmptyCap :: Int
-- Per-second caps tuned to bound genuine floods (CVE-2019-9512/9515/9518),
-- which sustain tens of thousands of frames per second, while tolerating
-- the bursts a legitimate high-throughput peer produces — e.g. a client
-- driving hundreds of rapid RPCs on one connection, with per-RPC keepalive
-- PINGs. Caps that are too tight (a handful per second) GOAWAY such peers.
srlPingCap     = 1000
srlSettingsCap = 100
srlRstCap      = 1000
srlEmptyCap    = 1000

connectionLoop
  :: ServerConfig
  -> Connection
  -> IORef (Map.Map StreamId StreamRec)
  -> IORef StreamId
  -> IORef Int    -- ^ connection-level recv-unacked counter
  -> IORef Int    -- ^ connection-level recv-window remaining
  -> ServerRateLimiters
  -> IORef Double           -- ^ last-activity timestamp
  -> IORef (Maybe Double)   -- ^ header-block start timestamp
  -> IO ()
connectionLoop cfg conn streamsRef lastPeerStreamRef
               connRecvUnackedRef connRecvWindowRef rl
               lastActivityRef continuationStartRef = loop
  where
    maxFrame = settingsMaxFrameSize (serverSettings cfg)
    receive = do
      result <- recvFrame conn
      -- Refresh the activity timestamp before doing any work: any
      -- byte we just pulled off the wire counts as activity.
      now <- getMonotonicTime
      writeIORef lastActivityRef now
      case result of
        Left err -> abortConnection conn (decodeErrorToCode err) (decodeErrorMessage err)
        Right frame
          | fhLength (frameHeader frame) > maxFrame ->
              abortConnection conn FrameSizeError "frame exceeds SETTINGS_MAX_FRAME_SIZE"
          | otherwise -> pure frame
    loop = do
      frame <- receive
      ok <- handleFrame' cfg conn streamsRef lastPeerStreamRef
                         connRecvUnackedRef connRecvWindowRef rl
                         continuationStartRef (Just <$> receive) frame
      when ok loop

-- | Map a 'FrameDecodeError' to the HTTP\/2 error code dictated by
-- RFC 9113. Most malformed-payload conditions are
-- @FRAME_SIZE_ERROR@; the rest fall back to @PROTOCOL_ERROR@.
decodeErrorToCode :: FrameDecodeError -> ErrorCode
decodeErrorToCode FrameTooShort                = FrameSizeError
decodeErrorToCode PayloadTooShort              = FrameSizeError
decodeErrorToCode InvalidSettingsLength        = FrameSizeError
decodeErrorToCode InvalidWindowUpdateIncrement = ProtocolError
decodeErrorToCode InvalidPadding               = ProtocolError
decodeErrorToCode InvalidStreamId              = ProtocolError

decodeErrorMessage :: FrameDecodeError -> ByteString
decodeErrorMessage = BS.pack . map (fromIntegral . fromEnum) . show

-- | Process a single frame. Returns 'True' to keep reading,
-- 'False' to tear the connection down (a GOAWAY was already sent).
--
-- We dispatch on 'fhType' rather than on the 'FramePayload' pattern
-- synonyms because the latter all match @FramePayloadRaw bs@
-- regardless of frame type — they're decode helpers, not a type-
-- tagged sum. Dispatching on the header's actual type avoids the
-- crossover where (e.g.) a HEADERS body coincidentally matches the
-- @PingFrame@ view pattern.
handleFrame'
  :: ServerConfig
  -> Connection
  -> IORef (Map.Map StreamId StreamRec)
  -> IORef StreamId
  -> IORef Int      -- ^ connection-level recv-unacked counter
  -> IORef Int      -- ^ connection-level recv-window remaining
  -> ServerRateLimiters
  -> IORef (Maybe Double)   -- ^ header-block start timestamp (slowloris guard)
  -> IO (Maybe Frame)       -- ^ read another fragment on this connection
  -> Frame
  -> IO Bool
handleFrame' cfg conn streamsRef lastPeerStreamRef
             connRecvUnackedRef connRecvWindowRef rl
             continuationStartRef receive (Frame hdr (FramePayloadRaw body)) = case fhType hdr of
  FrameSettings
    | testFlag (fhFlags hdr) flagAck ->
        if BS.null body
          then pure True
          else do
            closeConnection conn FrameSizeError "SETTINGS ACK with non-empty payload"
            pure False
    | fhStreamId hdr /= 0 -> do
        closeConnection conn ProtocolError "SETTINGS on non-zero stream"
        pure False
    | BS.length body `mod` 6 /= 0 -> do
        closeConnection conn FrameSizeError "SETTINGS payload not multiple of 6"
        pure False
    | otherwise -> do
        n <- tickRate (srlSettings rl)
        if n > srlSettingsCap
          then do
            closeConnection conn EnhanceYourCalm "SETTINGS flood"
            pure False
          else case decodeSettings body of
            Nothing -> do
              closeConnection conn FrameSizeError "malformed SETTINGS payload"
              pure False
            Just params -> do
              old <- readIORef (connRemoteSettings conn)
              case applySettingsParams old params of
                Left _ -> do
                  closeConnection conn ProtocolError "invalid settings"
                  pure False
                Right newSettings -> do
                  atomicModifyIORef' (connRemoteSettings conn) (\_ -> (newSettings, ()))
                  -- Adjust every open stream's send window by the
                  -- delta of the peer's
                  -- SETTINGS_INITIAL_WINDOW_SIZE (RFC 9113 §6.9.2).
                  adjustServerStreamWindows
                    streamsRef
                    (settingsInitialWindowSize old)
                    (settingsInitialWindowSize newSettings)
                  -- Peer's SETTINGS_HEADER_TABLE_SIZE caps our HPACK
                  -- encoder's dynamic table size.  See note in
                  -- Network.HTTP2.Client; we shrink the table but
                  -- don't yet emit the spec-required "Dynamic Table
                  -- Size Update" instruction on the next header
                  -- block.
                  when (settingsHeaderTableSize old /= settingsHeaderTableSize newSettings) $
                    withMVar (connHpackEncoder conn) $ \enc ->
                      setMaxSize enc (fromIntegral (settingsHeaderTableSize newSettings))
                  let ack = Frame (FrameHeader 0 FrameSettings flagAck 0) (SettingsFrame [])
                  sendFrame conn ack
                  pure True

  FramePing
    | fhStreamId hdr /= 0 -> do
        closeConnection conn ProtocolError "PING on non-zero stream"
        pure False
    | BS.length body /= 8 -> do
        closeConnection conn FrameSizeError "PING payload length must be 8"
        pure False
    | testFlag (fhFlags hdr) flagAck -> pure True
    | otherwise -> do
        n <- tickRate (srlPing rl)
        if n > srlPingCap
          then do
            closeConnection conn EnhanceYourCalm "PING flood"
            pure False
          else do
            let pong = Frame (FrameHeader 8 FramePing flagAck 0) (PingFrame body)
            sendFrame conn pong
            pure True

  FrameWindowUpdate
    | BS.length body /= 4 -> do
        closeConnection conn FrameSizeError "WINDOW_UPDATE length must be 4"
        pure False
    | otherwise ->
        let increment = (fromIntegral (BS.index body 0) `shiftL` 24)
                    .|. (fromIntegral (BS.index body 1) `shiftL` 16)
                    .|. (fromIntegral (BS.index body 2) `shiftL` 8)
                    .|. fromIntegral (BS.index body 3) :: Word32
            inc = increment .&. 0x7FFFFFFF
            sid = fhStreamId hdr
        in if inc == 0
             then if sid == 0
                    then do
                      closeConnection conn ProtocolError "WINDOW_UPDATE with zero increment on stream 0"
                      pure False
                    else do
                      sendRstStream conn sid ProtocolError
                      pure True
             else if sid == 0
                    then do
                      r <- atomically $
                        releaseWindow (connSendFlowControl conn) (fromIntegral inc)
                      case r of
                        Left _ -> do
                          -- Window overflow > 2^31-1 → FLOW_CONTROL_ERROR
                          -- connection error.
                          closeConnection conn FlowControlError "connection flow control overflow"
                          pure False
                        Right () -> pure True
                    else do
                      streams <- readIORef streamsRef
                      lastPeer <- readIORef lastPeerStreamRef
                      case Map.lookup sid streams of
                        Nothing | odd sid && sid > lastPeer -> do
                          closeConnection conn ProtocolError "WINDOW_UPDATE on idle stream"
                          pure False
                        Just sr -> do
                          r <- atomically $
                            releaseWindow (srSendWindow sr) (fromIntegral inc)
                          case r of
                            Left _ -> do
                              -- Per-stream window overflow → RST_STREAM
                              -- with FLOW_CONTROL_ERROR (RFC 9113 §6.9.1).
                              sendRstStream conn sid FlowControlError
                              _ <- tryPutMVar (srTrailers sr) (Right [])
                              atomically $
                                writeTBQueue (srBodyQueue sr) BodyChunkEnd
                              atomicModifyIORef' streamsRef
                                (\m -> (Map.adjust (\s -> s { srState = StClosedSrv }) sid m, ()))
                              pure True
                            Right () -> pure True
                        Nothing -> pure True  -- closed stream; harmless

  FrameGoAway
    | fhStreamId hdr /= 0 -> do
        closeConnection conn ProtocolError "GOAWAY on non-zero stream"
        pure False
    | otherwise -> case decodeGoAway body of
        Just (lastId, code, debug) -> do
          connOnGoAway conn lastId code debug
          pure True
        Nothing -> do
          closeConnection conn FrameSizeError "malformed GOAWAY"
          pure False

  FrameHeaders
    | fhStreamId hdr == 0 -> do
        closeConnection conn ProtocolError "HEADERS on stream 0"
        pure False
    | otherwise -> do
        let sid = fhStreamId hdr
            !selfDependency = headersSelfDependency sid (fhFlags hdr) body
        block <- readBlock
        streams <- readIORef streamsRef
        case Map.lookup sid streams of
          Just sr | srState sr == StClosedSrv -> do
            closeConnection conn Types.StreamClosed "HEADERS on closed stream"
            pure False
          Just sr | srState sr == StHalfClosedRemote -> do
            closeConnection conn Types.StreamClosed "HEADERS on half-closed (remote) stream"
            pure False
          Just sr -> do
            -- Decode first to keep the HPACK table in sync even when
            -- the result violates trailer semantics.
            res <- withMVar (connHpackDecoder conn) $ \decoder ->
              decodeHeaderBlock decoder block
            case res of
              Left _ -> do
                closeConnection conn CompressionError "HPACK decode failed in trailers"
                pure False
              Right trailers
                | overHeaderListLimit' (serverSettings cfg) trailers ->
                    rejectRequestBody conn streamsRef sid sr EnhanceYourCalm "trailer field list too large"
                | Just reason <- validateTrailerBlock (fhFlags hdr) trailers ->
                    rejectRequestBody conn streamsRef sid sr ProtocolError reason
                | Just remaining <- srRemainingLength sr, remaining /= 0 ->
                    rejectRequestBody conn streamsRef sid sr ProtocolError "request content-length mismatch"
                | otherwise -> do
                    fields <- forceCopyHeaders trailers
                    atomicModifyStreams_ streamsRef $
                      Map.adjust (\current -> case srState current of
                        StOpen -> current { srState = StHalfClosedRemote }
                        _ -> current) sid
                    finishRequestBody sr fields
                    pure True
          Nothing -> do
            lastPeer <- readIORef lastPeerStreamRef
            handleFreshStream lastPeer sid (fhFlags hdr) selfDependency block
        where
          readBlock
            | testFlag (fhFlags hdr) flagEndHeaders = assemble
            | otherwise = do
                now <- getMonotonicTime
                writeIORef continuationStartRef (Just now)
                assemble `finally` writeIORef continuationStartRef Nothing
            where
              assemble = recvHeaderBlockWith copyReceivedBytes conn receive hdr body

          connLastSid = connLastStreamId conn
          handleFreshStream lp sid' flags selfDependency block
            | even sid' || sid' <= lp = do
                closeConnection conn ProtocolError "invalid stream id"
                pure False
            | selfDependency = do
                closeConnection conn ProtocolError "HEADERS depends on itself"
                pure False
            | otherwise = do
                result <- withMVar (connHpackDecoder conn) $ \decoder ->
                  decodeHeaderBlock decoder block
                -- Give the handler compact copies of the fields it keeps.
                result' <- case result of
                  Right hs -> Right <$> forceCopyHeaders hs
                  Left e   -> pure (Left e)
                case result' of
                  Left _ -> do
                    closeConnection conn CompressionError "HPACK decode failed"
                    pure False
                  Right headers
                    | overHeaderListLimit' (serverSettings cfg) headers ->
                        refuseFreshStream sid' RefusedStream
                    | otherwise -> case parseRequestHead headers of
                        Left _ -> refuseFreshStream sid' ProtocolError
                        Right requestHead -> case readContentLength (requestHeadFields requestHead) of
                          Left _ -> refuseFreshStream sid' ProtocolError
                          Right remaining
                            | testFlag flags flagEndStream, Just n <- remaining, n /= 0 ->
                                refuseFreshStream sid' ProtocolError
                            | otherwise -> acceptFreshStream sid' flags requestHead remaining

          refuseFreshStream sid code = do
            writeIORef lastPeerStreamRef sid
            peerInit <- settingsInitialWindowSize <$> readIORef (connRemoteSettings conn)
            refused <- freshStream peerInit ourInit Nothing StClosedSrv
            atomicModifyStreams_ streamsRef (Map.insert sid refused)
            sendRstStream conn sid code
            pure True

          ourInit = settingsInitialWindowSize (serverSettings cfg)

          acceptFreshStream sid flags requestHead remaining = do
            streamsNow <- readIORef streamsRef
            let active = Map.size (Map.filter ((/= StClosedSrv) . srState) streamsNow)
            case settingsMaxConcurrentStreams (serverSettings cfg) of
              Just n | active >= fromIntegral n -> refuseFreshStream sid RefusedStream
              _ -> do
                writeIORef connLastSid sid
                writeIORef lastPeerStreamRef sid
                peerInit <- settingsInitialWindowSize <$> readIORef (connRemoteSettings conn)
                let endStream = testFlag flags flagEndStream
                    newState = if endStream then StHalfClosedRemote else StOpen
                rec <- freshStream peerInit ourInit remaining newState
                when endStream $ finishRequestBody rec []
                atomicModifyStreams_ streamsRef (Map.insert sid rec)
                unless endStream $ sendContinueIfExpected conn sid remaining requestHead
                let req = buildRequest conn connRecvUnackedRef connRecvWindowRef sid requestHead rec
                _ <- serverForkStream cfg $
                  ( (serverHandler cfg req $ \resp ->
                        sendResponse conn sid (srSendWindow rec) (requestMethod req) resp
                          `onException` sendRstStream conn sid InternalError
                    )
                    `finally` atomicModifyStreams_ streamsRef
                      (Map.adjust (\sr -> sr { srState = StClosedSrv }) sid)
                  )
                  -- A worker can still be writing when its connection closes.
                  `catch` (\(_ :: SomeException) -> pure ())
                pure True

  FrameData
    | fhStreamId hdr == 0 -> do
        closeConnection conn ProtocolError "DATA on stream 0"
        pure False
    | otherwise -> do
        -- Empty-DATA frames cost the peer nothing flow-control-wise
        -- but force us through the full per-frame work.  Cap them —
        -- but only frames that make no progress: an empty DATA frame
        -- carrying END_STREAM is the normal stream terminator (one per
        -- request stream), so it must never count toward the flood limit
        -- (CVE-2019-9518 is about empty frames that keep a stream open).
        let isEmpty = ( BS.null body
                          || (BS.length body == 1 && testFlag (fhFlags hdr) flagPadded)
                      )
                   && not (testFlag (fhFlags hdr) flagEndStream)
        overEmpty <-
          if isEmpty
            then do
              n <- tickRate (srlEmpty rl)
              pure (n > srlEmptyCap)
            else pure False
        if overEmpty
          then do
            closeConnection conn EnhanceYourCalm "empty-DATA flood"
            pure False
          else do
            let sid = fhStreamId hdr
            streams <- readIORef streamsRef
            case Map.lookup sid streams of
              Nothing -> do
                closeConnection conn Types.StreamClosed "DATA on idle stream"
                pure False
              Just sr | srState sr == StClosedSrv ->
                -- Half-closed (local): the server has finished sending its
                -- response, but the peer may still send DATA until it
                -- half-closes (RFC 9113 §5.1 — a "half-closed (local)" stream
                -- can receive any frame). The handler is gone, so discard the
                -- payload — immediately returning its connection-level
                -- flow-control credit so the peer isn't stalled — and finalize
                -- on END_STREAM, rather than tearing down the whole connection.
                case stripFramePadding (fhFlags hdr) body of
                  Nothing -> do
                    closeConnection conn ProtocolError "malformed DATA frame"
                    pure False
                  Just _payload -> do
                    let frameBytes = BS.length body
                    when (frameBytes > 0) $
                      sendFrame conn $ Frame
                        (FrameHeader 4 FrameWindowUpdate 0 0)
                        (WindowUpdateFrame (fromIntegral frameBytes))
                    when (testFlag (fhFlags hdr) flagEndStream) $ do
                      _ <- tryPutMVar (srTrailers sr) (Right [])
                      atomically $ writeTBQueue (srBodyQueue sr) BodyChunkEnd
                    pure True
              Just sr | srState sr == StHalfClosedRemote -> do
                closeConnection conn Types.StreamClosed "DATA on half-closed (remote) stream"
                pure False
              Just sr -> case stripFramePadding (fhFlags hdr) body of
                Nothing -> do
                  closeConnection conn ProtocolError "malformed DATA frame"
                  pure False
                Just payload -> do
                  -- Flow control counts the full DATA frame payload
                  -- (including padding) against the recv window
                  -- (RFC 9113 §6.9.1).  Decrement both windows; if
                  -- either goes negative the peer has overflowed
                  -- what we advertised.  Stream-level overflow is
                  -- recoverable via RST_STREAM; connection-level
                  -- overflow tears the connection.
                  let frameBytes = BS.length body
                  streamWin <- atomicModifyIORef' (srRecvWindow sr) $ \w ->
                    let w' = w - frameBytes in (w', w')
                  connWin <- atomicModifyIORef' connRecvWindowRef $ \w ->
                    let w' = w - frameBytes in (w', w')
                  if streamWin < 0
                    then do
                      sendRstStream conn sid FlowControlError
                      atomicModifyStreams_ streamsRef
                        (Map.adjust (\s -> s { srState = StClosedSrv }) sid)
                      pure True
                    else if connWin < 0
                      then do
                        closeConnection conn FlowControlError "connection recv window underflow"
                        pure False
                      else do
                        let bytes = BS.length payload
                            endStream = testFlag (fhFlags hdr) flagEndStream
                            afterPayload remaining = remainingContentLength remaining bytes endStream
                        case traverse afterPayload (srRemainingLength sr) of
                          Nothing -> do
                            ackConnRecv conn connRecvUnackedRef connRecvWindowRef frameBytes
                            rejectRequestBody conn streamsRef sid sr ProtocolError "request content-length mismatch"
                          Just remaining -> do
                            let newState = if endStream then StHalfClosedRemote else StOpen
                            when (endStream || remaining /= Nothing) $
                              atomicModifyStreams_ streamsRef $
                                Map.adjust (\current -> case srState current of
                                  StOpen -> current { srState = newState, srRemainingLength = remaining }
                                  _ -> current) sid
                            let paddingBytes = frameBytes - bytes
                            when (paddingBytes > 0) $ do
                              ackConnRecv conn connRecvUnackedRef connRecvWindowRef paddingBytes
                              when (not endStream) $
                                ackStreamRecv conn sid (srRecvUnacked sr) (srRecvWindow sr) paddingBytes
                            -- Copy before reuse of the ring. The reader refunds
                            -- content bytes when it consumes each chunk.
                            when (bytes /= 0) $ do
                              copied <- copyReceivedBytes payload
                              atomically $ writeTBQueue (srBodyQueue sr) (BodyChunkBytes copied)
                            when endStream $ finishRequestBody sr []
                            pure True

  FrameRSTStream
    | fhStreamId hdr == 0 -> do
        closeConnection conn ProtocolError "RST_STREAM on stream 0"
        pure False
    | BS.length body /= 4 -> do
        closeConnection conn FrameSizeError "RST_STREAM length must be 4"
        pure False
    | otherwise -> do
        n <- tickRate (srlRst rl)
        if n > srlRstCap
          then do
            closeConnection conn EnhanceYourCalm "RST_STREAM flood"
            pure False
          else do
            let sid = fhStreamId hdr
            streams <- readIORef streamsRef
            case Map.lookup sid streams of
              Nothing -> do
                closeConnection conn ProtocolError "RST_STREAM on idle stream"
                pure False
              Just sr -> do
                -- Wake any blocked body / trailer readers so the
                -- handler unblocks (the wireform unified Body sees
                -- this as end-of-stream; callers that need to
                -- distinguish abort vs clean end can poll the
                -- connection state).
                _ <- tryPutMVar (srTrailers sr) (Right [])
                atomically $ writeTBQueue (srBodyQueue sr) BodyChunkEnd
                atomicModifyStreams_ streamsRef
                  (Map.adjust (\s -> s { srState = StClosedSrv }) sid)
                pure True

  FramePriority
    | fhStreamId hdr == 0 -> do
        closeConnection conn ProtocolError "PRIORITY on stream 0"
        pure False
    | BS.length body /= 5 -> do
        sendRstStream conn (fhStreamId hdr) FrameSizeError
        pure True
    | otherwise -> do
        let depRaw = (fromIntegral (BS.index body 0) `shiftL` 24)
                 .|. (fromIntegral (BS.index body 1) `shiftL` 16)
                 .|. (fromIntegral (BS.index body 2) `shiftL` 8)
                 .|. fromIntegral (BS.index body 3) :: Word32
            dep = depRaw .&. 0x7FFFFFFF
        if dep == fhStreamId hdr
          then do
            -- RFC 9113 §5.3.1: stream cannot depend on itself.
            sendRstStream conn (fhStreamId hdr) ProtocolError
            pure True
          else pure True

  FrameContinuation ->
    abortConnection conn ProtocolError "unexpected CONTINUATION"

  FramePushPromise -> do
    -- Clients are not supposed to send PUSH_PROMISE.
    closeConnection conn ProtocolError "client sent PUSH_PROMISE"
    pure False

  FrameUnknown _ -> pure True  -- RFC 9113 §4.1: unknown frame types MUST be ignored

finishRequestBody :: StreamRec -> [Header] -> IO ()
finishRequestBody sr trailers = do
  _ <- tryPutMVar (srTrailers sr) (Right trailers)
  atomically $ writeTBQueue (srBodyQueue sr) BodyChunkEnd

rejectRequestBody :: Connection -> IORef (Map.Map StreamId StreamRec) -> StreamId -> StreamRec -> ErrorCode -> ByteString -> IO Bool
rejectRequestBody conn streamsRef sid sr code reason = do
  let err = ConnectionError code reason sid
  sendRstStream conn sid code
  atomicModifyStreams_ streamsRef (Map.adjust (\current -> current { srState = StClosedSrv }) sid)
  _ <- tryPutMVar (srTrailers sr) (Left err)
  atomically $ writeTBQueue (srBodyQueue sr) (BodyChunkError err)
  pure True

-- | Send a stream-scoped RST_STREAM with the given error code.
sendRstStream :: Connection -> StreamId -> ErrorCode -> IO ()
sendRstStream conn sid code =
  sendFrame conn $ Frame
    (FrameHeader 4 FrameRSTStream 0 sid)
    (RSTStreamFrame code)

-- | True if the HEADERS frame carries PRIORITY-flag info that names
-- the same stream as its dependency (RFC 9113 §5.3.1: a stream
-- cannot depend on itself).
headersSelfDependency :: StreamId -> FrameFlags -> ByteString -> Bool
headersSelfDependency sid flags body0
  | not (testFlag flags flagPriority) = False
  | otherwise =
      -- The PRIORITY block sits after the optional padding byte.
      let body = if testFlag flags flagPadded
                   then BS.drop 1 body0
                   else body0
      in BS.length body >= 5 &&
         let depRaw = (fromIntegral (BS.index body 0) `shiftL` 24)
                  .|. (fromIntegral (BS.index body 1) `shiftL` 16)
                  .|. (fromIntegral (BS.index body 2) `shiftL` 8)
                  .|. fromIntegral (BS.index body 3) :: Word32
             dep = depRaw .&. 0x7FFFFFFF
         in dep == sid

-- | RFC 7541 §4.1 sizing: 32 octets overhead per field, plus the
-- byte lengths of name and value.  Used to enforce the peer's
-- advertised SETTINGS_MAX_HEADER_LIST_SIZE — exceeding it lets the
-- peer wedge us with absurdly large header blocks even though HPACK
-- decoded them fine.
headerListSize :: [(ByteString, ByteString)] -> Int
headerListSize = foldr (\(n, v) acc -> acc + 32 + BS.length n + BS.length v) 0

-- | Walk a decoded header list and replace every name\/value with an
-- eagerly-copied @ByteString@.  HPACK's literal\/huffman decoders
-- return bytestrings whose backing memory ultimately came from the
-- recv ring buffer; once the recv loop advances, that memory is
-- recycled.  This pass detaches every bytestring with a fresh
-- @memcpy@ so downstream code (handler threads, HPACK table) holds
-- only stable memory.
forceCopyHeaders
  :: [(ByteString, ByteString)] -> IO [(ByteString, ByteString)]
forceCopyHeaders = mapM $ \(n, v) -> do
  n' <- copyReceivedBytes n
  v' <- copyReceivedBytes v
  pure (n', v')

-- | Run @io@ but abandon it (and return 'Nothing') if it takes longer
-- than the configured number of seconds.  'Nothing' for the timeout
-- means "no cap", so the action runs to completion and is wrapped in
-- 'Just'.
withMaybeTimeout :: Maybe Double -> IO a -> IO (Maybe a)
withMaybeTimeout Nothing io = Just <$> io
withMaybeTimeout (Just secs) io = timeout (toMicros secs) io

toMicros :: Double -> Int
toMicros s
  | s <= 0    = 0
  | otherwise = round (s * 1e6)

-- | Background watchdog that closes the connection when it's been
-- idle (no incoming frame) for longer than
-- 'serverIdleTimeoutSeconds', or when a header block has been
-- mid-flight (HEADERS without END_HEADERS) longer than
-- 'serverHeaderBlockTimeoutSeconds'.
--
-- We poll once a second rather than wiring a precise timer per
-- connection — close enough for the use case (DoS mitigation), and
-- avoids a thread-per-deadline.
idleWatchdog
  :: ServerConfig
  -> Connection
  -> ThreadId               -- ^ the connection-loop thread to wake
  -> IORef Double           -- ^ monotonic time of last incoming frame
  -> IORef (Maybe Double)   -- ^ monotonic time when current header block began
  -> IO ()
idleWatchdog cfg conn loopTid lastActivityRef continuationStartRef =
  case (serverIdleTimeoutSeconds cfg, serverHeaderBlockTimeoutSeconds cfg) of
    (Nothing, Nothing) -> pure ()
    _ -> loop
  where
    loop = do
      threadDelay 1000000  -- 1s tick
      now <- getMonotonicTime
      idleTrip <- case serverIdleTimeoutSeconds cfg of
        Just secs -> do
          last' <- readIORef lastActivityRef
          pure (now - last' > secs)
        Nothing -> pure False
      contTrip <- case serverHeaderBlockTimeoutSeconds cfg of
        Just secs -> do
          mStart <- readIORef continuationStartRef
          case mStart of
            Just s -> pure (now - s > secs)
            Nothing -> pure False
        Nothing -> pure False
      if idleTrip || contTrip
        then do
          -- Closing never unmaps the receive ring, so this is safe while
          -- the loop is mid-frame; the loop's owner releases it.
          closeConnection conn EnhanceYourCalm
            (if idleTrip then "idle timeout" else "slow header block")
            `catch` (\(_ :: SomeException) -> pure ())
          throwTo loopTid IdleWatchdogFired
        else loop

-- | True when @hs@ exceeds the server's advertised
-- @SETTINGS_MAX_HEADER_LIST_SIZE@.  Returns False when no limit was
-- advertised.
overHeaderListLimit' :: Settings -> [(ByteString, ByteString)] -> Bool
overHeaderListLimit' s hs = case settingsMaxHeaderListSize s of
  Nothing  -> False
  Just lim -> headerListSize hs > fromIntegral lim

buildRequest
  :: Connection
  -> IORef Int      -- ^ connection-level recv-unacked counter
  -> IORef Int      -- ^ connection-level recv-window mirror
  -> StreamId
  -> RequestHead
  -> StreamRec
  -> Request
buildRequest conn connUnackedRef connWindowRef sid requestHead sr =
  Request
    { requestMethod = requestHeadMethod requestHead
    , requestPath = requestHeadPath requestHead
    , requestScheme = requestHeadScheme requestHead
    , requestAuthority = requestHeadAuthority requestHead
    , requestHeaders = requestHeadFields requestHead
    , requestBody = makeRequestBody conn connUnackedRef connWindowRef sid sr
    , requestStreamId = sid
    , requestTrailers = readMVar (srTrailers sr) >>= either throwIO pure
    }

sendResponse :: Connection -> StreamId -> FlowControl -> ByteString -> Response -> IO ()
sendResponse conn sid sendWin method resp = do
  let status = responseStatus resp
      informational = status >= 100 && status < 200
      statusBS = BS.pack (map (fromIntegral . fromEnum) (show status))
      statusHdr = (":status", statusBS)
      fields
        | informational || status == 204 = filter ((/= "content-length") . fst) (responseHeaders resp)
        | otherwise = responseHeaders resp
      allHeaders = statusHdr : fields
      trailers = responseTrailers resp
      hasTrailers = not (null trailers)
  maxFrame <- peerMaxFrameSize conn
  -- HPACK-encode the response headers and emit the HEADERS block in ONE
  -- send-lock critical section ('encodeAndSendHeaderBlock'), so the
  -- encoder's dynamic-table mutation order matches the wire order even when
  -- many stream handlers send responses concurrently. Encoding under the
  -- HPACK lock and then sending under the send lock separately would let
  -- two responses encode in one order and reach the wire in the other,
  -- desyncing the peer's decoder.
  case responseBody resp of
    _ | informational -> encodeAndSendHeaderBlock conn sid False 0 allHeaders maxFrame
      | method == "HEAD" || status == 204 || status == 205 || status == 304 ->
          encodeAndSendHeaderBlock conn sid True 0 allHeaders maxFrame
    ResponseBodyEmpty
      | hasTrailers -> do
          checkResponseTrailers trailers
          encodeAndSendHeaderBlock conn sid False 0 allHeaders maxFrame
          sendTrailers conn sid trailers
      | otherwise ->
          encodeAndSendHeaderBlock conn sid True 0 allHeaders maxFrame
    ResponseBodyBS body -> do
      checkResponseTrailers trailers
      encodeAndSendHeaderBlock conn sid False 0 allHeaders maxFrame
      when (not hasTrailers || not (BS.null body)) $
        sendBytes conn sendWin sid (not hasTrailers) body maxFrame
      if hasTrailers
        then sendTrailers conn sid trailers
        else pure ()
    ResponseBodyStream producer -> do
      encodeAndSendHeaderBlock conn sid False 0 allHeaders maxFrame
      streamBody conn sendWin sid producer (pure trailers)
    ResponseBodyStreamWithTrailers producer produceTrailers -> do
      encodeAndSendHeaderBlock conn sid False 0 allHeaders maxFrame
      streamBody conn sendWin sid producer ((trailers <>) <$> produceTrailers)
    ResponseBodyEmptyWithTrailers produceTrailers -> do
      encodeAndSendHeaderBlock conn sid False 0 allHeaders maxFrame
      finishResponseTrailers conn sendWin sid ((trailers <>) <$> produceTrailers)
    ResponseBodyBSWithTrailers body produceTrailers -> do
      encodeAndSendHeaderBlock conn sid False 0 allHeaders maxFrame
      unless (BS.null body) $ sendBytes conn sendWin sid False body maxFrame
      finishResponseTrailers conn sendWin sid ((trailers <>) <$> produceTrailers)

-- | Send a contiguous DATA payload chunked to MAX_FRAME_SIZE, with the
-- final frame carrying @END_STREAM@ when @endStream@ is 'True'.
sendBytes
  :: Connection
  -> FlowControl       -- ^ per-stream send window
  -> StreamId
  -> Bool              -- ^ set END_STREAM on the last frame
  -> ByteString
  -> Int               -- ^ peer MAX_FRAME_SIZE
  -> IO ()
sendBytes conn sendWin sid endStream bs maxFrame
  | BS.length bs <= maxFrame = sendDataFrame conn sendWin sid endStream bs
  | otherwise = do
      let (chunk, rest) = BS.splitAt maxFrame bs
      sendDataFrame conn sendWin sid False chunk
      sendBytes conn sendWin sid endStream rest maxFrame

-- | Walk every open stream and shift its send window by the delta of
-- @oldInit@ vs @newInit@ (RFC 9113 § 6.9.2).
adjustServerStreamWindows
  :: IORef (Map.Map StreamId StreamRec) -> Word32 -> Word32 -> IO ()
adjustServerStreamWindows ref oldInit newInit
  | oldInit == newInit = pure ()
  | otherwise = do
      m <- readIORef ref
      atomically $ mapM_ adjust (Map.elems m)
  where
    adjust sr = do
      _ <- updateInitialWindowSize
             (srSendWindow sr)
             (fromIntegral oldInit)
             (fromIntegral newInit)
      pure ()

peerMaxFrameSize :: Connection -> IO Int
peerMaxFrameSize conn = do
  s <- readIORef (connRemoteSettings conn)
  pure (fromIntegral (settingsMaxFrameSize s))

-- | Send one DATA frame, blocking until /both/ the connection-level
-- and per-stream send flow windows have room (RFC 9113 § 6.9).
sendDataFrame
  :: Connection
  -> FlowControl
  -> StreamId
  -> Bool       -- ^ END_STREAM
  -> ByteString
  -> IO ()
sendDataFrame conn sendWin sid endStream bs = do
  let n = BS.length bs
  if n == 0
    then sendIt
    else do
      atomically $ do
        cw <- availableWindow (connSendFlowControl conn)
        sw <- availableWindow sendWin
        if cw >= fromIntegral n && sw >= fromIntegral n
          then do
            _ <- consumeWindow (connSendFlowControl conn) (fromIntegral n)
            _ <- consumeWindow sendWin (fromIntegral n)
            pure ()
          else retry
      sendIt
  where
    sendIt = sendFrame conn $ Frame
      (FrameHeader (fromIntegral (BS.length bs)) FrameData
        (if endStream then flagEndStream else 0) sid)
      (DataFrame bs)

-- | Emit the trailer block as a final HEADERS frame with END_STREAM
-- (chunked over CONTINUATION frames if necessary). The HPACK encode and
-- the send are one send-lock critical section ('encodeAndSendHeaderBlock')
-- so trailer encoding can't desync the peer relative to concurrent streams.
sendTrailers :: Connection -> StreamId -> [(ByteString, ByteString)] -> IO ()
sendTrailers conn sid trailers = do
  maxFrame <- peerMaxFrameSize conn
  encodeAndSendHeaderBlock conn sid True 0 trailers maxFrame

checkResponseTrailers :: [(ByteString, ByteString)] -> IO ()
checkResponseTrailers trailers =
  case validateOutboundTrailers trailers of
    Just reason -> fail (BS8.unpack reason)
    Nothing -> pure ()
{-# INLINE checkResponseTrailers #-}

finishResponseTrailers :: Connection -> FlowControl -> StreamId -> IO [Header] -> IO ()
finishResponseTrailers conn sendWin sid produceTrailers = do
  trailers <- produceTrailers
  case trailers of
    [] -> sendDataFrame conn sendWin sid True BS.empty
    _ -> do
      checkResponseTrailers trailers
      sendTrailers conn sid trailers

streamBody
  :: Connection
  -> FlowControl
  -> StreamId
  -> IO (Maybe ByteString)
  -> IO [(ByteString, ByteString)]
  -> IO ()
streamBody conn sendWin sid producer produceTrailers = do
  maxFrame <- peerMaxFrameSize conn
  loop maxFrame
  where
    -- Send each chunk immediately as it is produced — never hold a frame
    -- waiting for the next one, which would deadlock full-duplex bidi
    -- streaming (the peer won't send the next request, hence the next
    -- response chunk, until it receives the one we'd be holding).
    loop maxFrame = do
      mChunk <- producer
      case mChunk of
        Nothing -> finishResponseTrailers conn sendWin sid produceTrailers
        Just chunk
          | BS.null chunk -> loop maxFrame
          | otherwise -> do
              sendBytes conn sendWin sid False chunk maxFrame
              loop maxFrame

-- | Atomically modify a streams map IORef. Wraps the common
-- pattern of adjusting or inserting into the per-connection stream map
-- from potentially concurrent threads (recv loop + stream handlers).
atomicModifyStreams_ :: IORef (Map.Map StreamId a) -> (Map.Map StreamId a -> Map.Map StreamId a) -> IO ()
atomicModifyStreams_ ref f = atomicModifyIORef' ref (\m -> (f m, ()))

