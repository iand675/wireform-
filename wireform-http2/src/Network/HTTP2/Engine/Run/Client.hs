{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Client-side frame I\/O loop for the wireform-http2 gRPC engine.

Drives 'Network.HTTP2.Engine.Client.run'. Like the server runtime,
this is intentionally narrower than @http2@ 5.3.x: handles HEADERS
+ DATA + trailing HEADERS, half-close in both directions, basic
WINDOW_UPDATE / PING / SETTINGS bookkeeping. No connection
preserving, no PUSH_PROMISE, no concurrent-stream rate limiting.
-}
module Network.HTTP2.Engine.Run.Client (
  RunEnv (..),
  EngineAux (..),
  runClient,
) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (
  SomeException,
  catch,
  finally,
  onException,
  throwIO,
  toException,
 )
import Control.Monad (unless, void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Word (Word32, Word64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr, plusPtr)
import Network.HTTP2.Client (ClientStreamError (..))
import Network.HTTP2.Connection (
  Connection,
  connHpackDecoder,
  newConnectionFromTransport,
  releaseConnection,
  sendFrame,
  sendNewStreamHeaders,
 )
import Network.HTTP2.Connection qualified as Conn
import Network.HTTP2.Connection.Settings (encodeSettings)
import Network.HTTP2.Engine.Run.Output
import Network.HTTP2.Engine.Types
import Network.HTTP2.Frame
import Network.HTTP2.HPACK
import Network.HTTP2.Transport (SendFn, Transport (..))
import Network.HTTP2.Types (FrameType (..))
import Network.HTTP2.Types qualified as H2
import Network.HTTP2.Types qualified as Wire


-- | Connection-scoped configuration for the client engine.
data RunEnv = RunEnv
  { envAuthority :: !Authority
  , envSendFn :: !SendFn
  , envReadN :: !(Int -> IO ByteString)
  , envConnectionWindow :: !Int
  , envInitialWindowSize :: !Int
  , envMaxConcurrentStreams :: !(Maybe Word32)
  , envScheme :: !Scheme
  }


-- | Mirror of 'Aux' from Engine.Client.
data EngineAux = EngineAux
  { engineAuxPossibleStreams :: !(IO Int)
  }


{- | Drive a client connection. The supplied callback receives a
'SendRequest'-shaped helper and an 'EngineAux'; the returned
value bubbles out.
-}
runClient
  :: RunEnv
  -> ((forall r. OutObj -> (InpObj -> IO r) -> IO r) -> EngineAux -> IO a)
  -> IO a
runClient env client = do
  let transport = engineTransport env
  conn <-
    newConnectionFromTransport
      Conn.RoleClient
      wireSettings
      (\_ _ _ -> pure ())
      transport
  sendAllRaw (envSendFn env) connectionPreface
  sendClientPrefaceSettings conn wireSettings
  streamsRef <- newIORef Map.empty
  nextSidRef <- newIORef 1
  recvTid <-
    forkIO $
      recvLoop env conn streamsRef
        `catch` ( \(_ :: SomeException) -> do
                    streams <- readIORef streamsRef
                    let err = toException ClientStreamConnectionClosed
                    mapM_ (failStream err) (Map.elems streams)
                )
  let sendRequest req k = doSendRequest env conn streamsRef nextSidRef req k
      aux = EngineAux {engineAuxPossibleStreams = pure 100}
  client sendRequest aux `finally` do
    releaseConnection conn
    killThread recvTid
  where
    wireSettings =
      H2.defaultSettings
        { H2.settingsInitialWindowSize = fromIntegral (envInitialWindowSize env)
        , H2.settingsMaxConcurrentStreams = envMaxConcurrentStreams env
        }


engineTransport :: RunEnv -> Transport
engineTransport env =
  Transport
    { tSendFn = envSendFn env
    , tRecvBuf = \ptr n -> do
        bs <- envReadN env n
        if BS.null bs
          then pure 0
          else do
            let (fp, off, len) = BSI.toForeignPtr bs
            withForeignPtr fp $ \src ->
              BSI.memcpy ptr (src `plusPtr` off) len
            pure len
    , tShutdownWrite = pure ()
    , tClose = pure ()
    }


sendAllRaw :: SendFn -> ByteString -> IO ()
sendAllRaw sf bs
  | BS.null bs = pure ()
  | otherwise = BSU.unsafeUseAsCStringLen bs $ \(src, len) ->
      drainLoop (castPtr src) len
  where
    drainLoop _ 0 = pure ()
    drainLoop p n = do
      k <- sf p n
      if k <= 0
        then ioError (userError "sendAllRaw: send returned 0")
        else drainLoop (p `plusPtr` k) (n - k)


sendClientPrefaceSettings :: Connection -> H2.Settings -> IO ()
sendClientPrefaceSettings conn s = do
  let params = encodeSettings s
      frame =
        Frame
          (FrameHeader (fromIntegral (length params * 6)) FrameSettings 0 0)
          (SettingsFrame params)
  sendFrame conn frame


-- | Per-stream worker mailbox for inbound (response side) data.
data StreamMb = StreamMb
  { smHeadersVar :: !(MVar (Either SomeException ResponseHead))
    -- ^ The final response head, or an error that ended the stream first.
  , smRequestMethod :: !ByteString
  , smResponseState :: !(IORef ResponseState)
  , smInputQueue :: !(TBQueue InputItem)
  , smTrailerSlot :: !(IORef (Maybe TokenHeaderTable))
  , smContinueGate :: !Conn.ContinueGate
  }

data ResponseHead = ResponseHead
  { responseFields :: ![Wire.Header]
  , responseSize :: !(Maybe Int)
  }

data ResponseState
  = AwaitingResponseHead
  | ReceivingUnboundedBody
  | ReceivingBodylessResponse
  | ReceivingDeclaredBody {-# UNPACK #-} !Word64
  | ResponseEnded


data InputItem
  = InputChunk !ByteString
  | InputFinal !ByteString
  | InputEnd
  | InputError !SomeException

-- Failure delivery must not block the connection's sole reader behind
-- a full body queue belonging to a consumer that has already stopped.
failStream :: SomeException -> StreamMb -> IO ()
failStream err mb = do
  Conn.stopContinue (smContinueGate mb)
  writeIORef (smResponseState mb) ResponseEnded
  _ <- tryPutMVar (smHeadersVar mb) (Left err)
  atomically $ do
    full <- isFullTBQueue (smInputQueue mb)
    when full (void (flushTBQueue (smInputQueue mb)))
    writeTBQueue (smInputQueue mb) (InputError err)


{- | Open a new stream, send the request headers/body, and call @k@
with the inbound response object once headers arrive.
-}
doSendRequest
  :: RunEnv
  -> Connection
  -> IORef (Map.Map H2.StreamId StreamMb)
  -> IORef H2.StreamId
  -> OutObj
  -> (InpObj -> IO r)
  -> IO r
doSendRequest env conn streamsRef nextSidRef outgoing k = do
  let hdrs = outObjHeaders outgoing
      body = outObjBody outgoing
      trailerMaker = outObjTrailerMaker outgoing
  headersVar <- newEmptyMVar
  responseState <- newIORef AwaitingResponseHead
  inputQ <- atomically (newTBQueue 64)
  trailerRef <- newIORef Nothing
  earlyTrailers <- case body of
    OutBodyNone -> maybe (pure []) finishTrailers trailerMaker
    _ -> pure []
  -- Inject :scheme and :authority before the user-supplied headers
  -- (these are required pseudo-headers on the request side).
  let augmented =
        (":scheme", envScheme env)
          : (":authority", BS.empty) -- placeholder; will be set below
          : ciHeadersToRaw hdrs
      finalHeaders = injectAuthority (envAuthority env) augmented
      requestMethod = maybe BS.empty id (lookup ":method" finalHeaders)
      carriesContent = case body of
        OutBodyNone -> False
        _ -> True
  continueGate <- Conn.newContinueGate carriesContent requestMethod finalHeaders
  let mb = StreamMb headersVar requestMethod responseState inputQ trailerRef continueGate
      openStream = do
        sid <- atomicModifyIORef' nextSidRef $ \s -> (s + 2, s)
        atomicModifyIORef' streamsRef (\m -> (Map.insert sid mb m, ()))
        pure sid
      endStreamOnHeaders = case body of
        OutBodyNone -> null earlyTrailers
        _           -> False
      streamBody sid producer =
        void $ forkIO $
          runStreamingBody conn sid trailerMaker (const Wire.Cancel) (outputFailed sid) producer
      outputFailed sid err = do
        sendRstStream conn sid Wire.InternalError
        state <- readIORef responseState
        case state of
          ResponseEnded -> pure ()
          _ -> failStream err mb
      forget sid = atomicModifyIORef' streamsRef (\m -> (Map.delete sid m, ()))
      release sid = case continueGate of
        Conn.SendImmediately -> forget sid
        Conn.WaitForContinue _ -> do
          suppressed <- Conn.finishContinueResponse continueGate
          when suppressed $ cancelUpload conn sid
          forget sid
      sendBody sid = case body of
        OutBodyNone -> unless endStreamOnHeaders (sendFinalBody conn sid BS.empty earlyTrailers)
        OutBodyBuilder b ->
          sendFixedBody conn sid b trailerMaker `onException` sendRstStream conn sid Wire.InternalError
        OutBodyStreaming f ->
          streamBody sid (\iface -> f (outBodyPush iface) (outBodyFlush iface))
        OutBodyStreamingIface f -> streamBody sid f
        OutBodyFile _ ->
          error "Network.HTTP2.Engine.Run.Client: OutBodyFile not supported"

  -- Allocate the stream id, register it, and send the request HEADERS
  -- atomically (under the connection send lock) so HEADERS reach the wire
  -- in stream-id order and the HPACK encode stays in sync with them. See
  -- 'sendNewStreamHeaders' for why both must share one critical section.
  sid <- sendNewStreamHeaders conn openStream endStreamOnHeaders finalHeaders

  flip finally (release sid) $ do
    case continueGate of
      Conn.SendImmediately -> sendBody sid
      Conn.WaitForContinue _ -> do
        allowed <- Conn.awaitContinue 1_000_000 continueGate `onException` do
          Conn.stopContinue continueGate
          cancelUpload conn sid
        when allowed $ sendBody sid

    respHead <- either throwIO pure =<< takeMVar headersVar
    k InpObj
      { inpObjHeaders = tokeniseHeaders (responseFields respHead)
      , inpObjBodySize = responseSize respHead
      , inpObjBody = do
          item <- atomically $ readTBQueue inputQ
          case item of
            InputChunk bs -> pure (bs, False)
            InputFinal bs -> pure (bs, True)
            InputEnd -> pure (BS.empty, True)
            InputError exc -> throwIO exc
      , inpObjTrailers = trailerRef
      }


injectAuthority :: Authority -> [(ByteString, ByteString)] -> [(ByteString, ByteString)]
injectAuthority auth = map fix
  where
    fix (":authority", _) = (":authority", BSI.packChars auth)
    fix kv = kv


-- | Response frame dispatch loop.
recvLoop
  :: RunEnv
  -> Connection
  -> IORef (Map.Map H2.StreamId StreamMb)
  -> IO ()
recvLoop env conn streamsRef = loop
  where
    receive = Conn.recvExactFrame conn (envReadN env) (H2.settingsMaxFrameSize H2.defaultSettings)
    loop = do
      mFrame <- receive
      case mFrame of
        Nothing -> do
          streams <- readIORef streamsRef
          let err = toException ClientStreamConnectionClosed
          mapM_ (failStream err) (Map.elems streams)
        Just (Frame hdr payload) -> do
          handleClientFrame receive conn streamsRef hdr payload
          loop


handleClientFrame
  :: IO (Maybe Frame)
  -> Connection
  -> IORef (Map.Map H2.StreamId StreamMb)
  -> FrameHeader
  -> FramePayload
  -> IO ()
handleClientFrame receive conn streamsRef hdr payload = case fhType hdr of
  FrameSettings
    | fhStreamId hdr /= 0 -> Conn.abortConnection conn Wire.ProtocolError "SETTINGS on nonzero stream"
    | testFlag (fhFlags hdr) flagAck -> pure ()
    | SettingsFrame params <- payload -> Conn.acknowledgePeerSettings conn params
    | otherwise -> Conn.abortConnection conn Wire.FrameSizeError "invalid SETTINGS payload"
  FramePing
    | testFlag (fhFlags hdr) flagAck -> pure ()
    | otherwise -> case payload of
        PingFrame opaque ->
          sendFrame conn $
            Frame
              (FrameHeader 8 FramePing flagAck 0)
              (PingFrame opaque)
        _ -> pure ()
  FrameWindowUpdate -> pure ()
  FrameGoAway -> pure ()
  FrameHeaders
    | fhStreamId hdr == 0 -> Conn.abortConnection conn Wire.ProtocolError "HEADERS on stream zero"
    | otherwise -> do
        block <- Conn.recvHeaderBlock conn receive hdr payload
        result <- withMVar (connHpackDecoder conn) $ \decoder ->
          decodeHeaderBlock decoder block
        case result of
          Left _ -> Conn.abortConnection conn Wire.CompressionError "hpack decode"
          Right headers -> do
            streams <- readIORef streamsRef
            mapM_ (acceptResponseHeaders conn hdr headers) (Map.lookup (fhStreamId hdr) streams)
  FrameContinuation -> Conn.abortConnection conn Wire.ProtocolError "unexpected CONTINUATION"
  FrameData
    | fhStreamId hdr == 0 -> Conn.abortConnection conn Wire.ProtocolError "DATA on stream zero"
    | Just body <- Conn.stripFramePadding (fhFlags hdr) (rawPayload payload) -> do
        streams <- readIORef streamsRef
        let sid = fhStreamId hdr
        updateStream <- case Map.lookup sid streams of
          Just mb -> acceptResponseData conn hdr mb body
          Nothing -> pure False
        Conn.returnFrameCredit conn sid (fhLength hdr) updateStream
    | otherwise -> Conn.abortConnection conn Wire.ProtocolError "invalid DATA padding"
  FrameRSTStream -> do
    streams <- readIORef streamsRef
    case Map.lookup (fhStreamId hdr) streams of
      Just mb -> do
        -- A reset can arrive before any response HEADERS (the request
        -- is still waiting in 'doSendRequest's takeMVar), or after them
        -- (the body reader is waiting on the queue). Fail both.
        state <- readIORef (smResponseState mb)
        case (state, payload) of
          (ResponseEnded, RSTStreamFrame Wire.NoError) -> pure ()
          (_, RSTStreamFrame code) -> failStream (toException (ClientStreamReset code)) mb
          _ -> failStream (toException ClientStreamConnectionClosed) mb
      Nothing -> pure ()
  _ -> pure ()

acceptResponseHeaders :: Connection -> FrameHeader -> [Wire.Header] -> StreamMb -> IO ()
acceptResponseHeaders conn hdr headers mb = do
  state <- readIORef (smResponseState mb)
  case state of
    AwaitingResponseHead -> case headers of
      (":status", value) : fields | Just status <- Conn.parseResponseStatus value ->
        case Conn.validateResponseFields fields of
          Just reason -> reject reason
          Nothing -> case Conn.readResponseContentLength (smRequestMethod mb) status fields of
            Left reason -> reject reason
            Right rule
              | status < 200 ->
                  if endStream
                    then reject "informational response with END_STREAM"
                    else Conn.noteContinueResponse (smContinueGate mb) status
              | endStream, Conn.ContentExpected n <- rule, n /= 0 -> reject lengthMismatch
              | otherwise -> do
                  Conn.noteContinueResponse (smContinueGate mb) status
                  -- gRPC trailers-only responses carry metadata in the
                  -- terminating initial field block.
                  if endStream
                    then endResponse conn (fhStreamId hdr) mb headers InputEnd
                    else writeIORef (smResponseState mb) $ case rule of
                      Conn.ContentUnbounded -> ReceivingUnboundedBody
                      Conn.ContentForbidden -> ReceivingBodylessResponse
                      Conn.ContentExpected n -> ReceivingDeclaredBody n
                  let size = case rule of
                        Conn.ContentUnbounded -> Nothing
                        Conn.ContentForbidden -> Just 0
                        Conn.ContentExpected n -> Conn.contentLengthAsInt n
                  putMVar (smHeadersVar mb) (Right (ResponseHead headers size))
      _ -> reject "invalid HTTP/2 response head"
    ResponseEnded -> pure ()
    _ -> case Conn.validateTrailerBlock (fhFlags hdr) headers of
      Just reason -> reject reason
      Nothing | ReceivingBodylessResponse <- state -> reject "trailers on a response without content"
      Nothing | ReceivingDeclaredBody n <- state, n /= 0 -> reject lengthMismatch
      Nothing -> endResponse conn (fhStreamId hdr) mb headers InputEnd
  where
    endStream = testFlag (fhFlags hdr) flagEndStream
    reject = rejectResponse conn (fhStreamId hdr) mb

acceptResponseData :: Connection -> FrameHeader -> StreamMb -> ByteString -> IO Bool
acceptResponseData conn hdr mb body = do
  state <- readIORef (smResponseState mb)
  case state of
    AwaitingResponseHead -> reject "DATA received before final response headers"
    ReceivingUnboundedBody -> deliver
    ReceivingBodylessResponse
      | BS.null body -> deliver
      | otherwise -> reject lengthMismatch
    ReceivingDeclaredBody remaining ->
      case Conn.remainingContentLength remaining (BS.length body) endStream of
        Nothing -> reject lengthMismatch
        Just next -> do
          when (not endStream && next /= remaining) $
            writeIORef (smResponseState mb) (ReceivingDeclaredBody next)
          deliver
    ResponseEnded -> pure False
  where
    endStream = testFlag (fhFlags hdr) flagEndStream
    reject reason = rejectResponse conn (fhStreamId hdr) mb reason >> pure False
    deliver = do
      if endStream
        then endResponse conn (fhStreamId hdr) mb [] (if BS.null body then InputEnd else InputFinal body)
        else unless (BS.null body) $
          atomically $ writeTBQueue (smInputQueue mb) (InputChunk body)
      pure (not endStream)

lengthMismatch :: ByteString
lengthMismatch = "response content-length mismatch"

endResponse :: Connection -> H2.StreamId -> StreamMb -> [Wire.Header] -> InputItem -> IO ()
endResponse conn sid mb trailers item = do
  suppressed <- Conn.finishContinueResponse (smContinueGate mb)
  when suppressed $ cancelUpload conn sid
  writeIORef (smTrailerSlot mb) (Just (tokeniseHeaders trailers))
  writeIORef (smResponseState mb) ResponseEnded
  atomically $ writeTBQueue (smInputQueue mb) item

cancelUpload :: Connection -> H2.StreamId -> IO ()
cancelUpload conn sid = sendRstStream conn sid Wire.Cancel
  `catch` (\(_ :: SomeException) -> pure ())

rejectResponse :: Connection -> H2.StreamId -> StreamMb -> ByteString -> IO ()
rejectResponse conn sid mb reason = do
  sendRstStream conn sid Wire.ProtocolError
  failStream (toException (ClientStreamProtocolError reason)) mb
