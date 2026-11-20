{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Server-side frame I\/O loop for the wireform-http2 gRPC engine.

The runtime that backs 'Network.HTTP2.Engine.Server.run'. Designed
for gRPC's request shape:

  * HEADERS with optional CONTINUATION frames (request headers)
  * DATA frames (request body) optionally followed by HEADERS
    [END_HEADERS, END_STREAM] for inbound trailers
  * The handler invokes 'respond' with a 'Response' that the engine
    translates into HEADERS / DATA / trailing HEADERS frames.

The implementation is intentionally narrower than @http2@ 5.3.x:
no PUSH_PROMISE, no responseFile, and no priority enforcement. Flow control is implemented in
aggregate (auto WINDOW_UPDATE) rather than per-stream-careful.
-}
module Network.HTTP2.Engine.Run.Server (
  RunEnv (..),
  EngineAux (..),
  EnginePushPromise (..),
  runServer,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket_, catch, finally, throwIO, toException)
import Control.Monad (unless, void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import Network.HTTP2.Client (ClientStreamError (..))
import Network.HTTP2.Connection (
  Connection,
  closeConnection,
  connHpackDecoder,
  sendEncodedHeaders,
  newConnectionFromTransport,
  releaseConnection,
  sendFrame,
 )
import Network.HTTP2.Connection qualified as Conn
import Network.HTTP2.Connection.Settings (encodeSettings)
import Network.HTTP2.Engine.Run.Output
import Network.HTTP2.Engine.Settings (Settings (..))
import Network.HTTP2.Engine.Types
import Network.HTTP2.Frame
import Network.HTTP2.HPACK
import Network.HTTP2.Transport (SendFn, Transport (..))
import Network.HTTP2.Types (FrameType (..))
import Network.HTTP2.Types qualified as H2
import Network.HTTP2.Types qualified as Wire
import Network.Socket (SockAddr)
import System.TimeManager qualified as TM


-- | Connection-scoped configuration the engine needs.
data RunEnv = RunEnv
  { envSettings :: !Settings
  , envConnectionWindow :: !Int
  , envSendFn :: !SendFn
  , envReadN :: !(Int -> IO ByteString)
  , envTimeoutManager :: !TM.Manager
  , envMySockAddr :: !SockAddr
  , envPeerSockAddr :: !SockAddr
  }


{- | Mirror of 'Aux' from Engine.Server, lifted here so this module
doesn't depend on Engine.Server.
-}
data EngineAux = EngineAux
  { envAuxTimeHandle :: !TM.Handle
  , envAuxMySockAddr :: !SockAddr
  , envAuxPeerSockAddr :: !SockAddr
  }


{- | Mirror of 'PushPromise' from Engine.Server (not used; gRPC
doesn't emit push promises).
-}
data EnginePushPromise = EnginePushPromise !ByteString !OutObj


-- | Per-stream worker mailbox.
data StreamMb = StreamMb
  { smInputQueue :: !(TBQueue InputItem)
  , smTrailerSlot :: !(IORef (Maybe TokenHeaderTable))
  , smCancel :: !(IORef (Maybe (Maybe SomeException -> IO ())))
  , smRequestState :: !(IORef RequestState)
  }

data RequestState
  = RequestOpen
  | RequestWithLength {-# UNPACK #-} !Word64
  | RequestEnded
  | RequestFailed
  deriving stock (Eq)


data InputItem
  = InputChunk !ByteString
  | InputFinal !ByteString
  | InputEnd
  | InputError !SomeException


-- | Drive the server connection.
runServer
  :: RunEnv
  -> (InpObj -> EngineAux -> (OutObj -> [EnginePushPromise] -> IO ()) -> IO ())
  -> IO ()
runServer env handler = do
  let transport = engineTransport env
  conn <-
    newConnectionFromTransport
      Conn.RoleServer
      (engineSettingsToWire (envSettings env))
      (\_ _ _ -> pure ())
      transport
  preface <- envReadN env (BS.length connectionPreface)
  if preface /= connectionPreface
    then closeConnection conn Wire.ProtocolError "bad preface" >> releaseConnection conn
    else do
      sendServerPrefaceFrames conn (envSettings env)
      streamsRef <- newIORef Map.empty
      connLoop env conn handler streamsRef
        `finally` releaseConnection conn


-- | Transport adapter over the env's send/recv callbacks.
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


engineSettingsToWire :: Settings -> H2.Settings
engineSettingsToWire s =
  H2.defaultSettings
    { H2.settingsHeaderTableSize = fromIntegral (headerTableSize s)
    , H2.settingsEnablePush = enablePush s
    , H2.settingsMaxConcurrentStreams = maxConcurrentStreams s
    , H2.settingsInitialWindowSize = fromIntegral (initialWindowSize s)
    , H2.settingsMaxFrameSize = fromIntegral (maxFrameSize s)
    , H2.settingsMaxHeaderListSize = maxHeaderListSize s
    }


sendServerPrefaceFrames :: Connection -> Settings -> IO ()
sendServerPrefaceFrames conn s = do
  let params = encodeSettings (engineSettingsToWire s)
      frame =
        Frame
          (FrameHeader (fromIntegral (length params * 6)) FrameSettings 0 0)
          (SettingsFrame params)
  sendFrame conn frame


-- | Frame dispatch loop.
connLoop
  :: RunEnv
  -> Connection
  -> (InpObj -> EngineAux -> (OutObj -> [EnginePushPromise] -> IO ()) -> IO ())
  -> IORef (Map.Map H2.StreamId StreamMb)
  -> IO ()
connLoop env conn handler streamsRef = loop `finally` failOutstanding
  where
    receive = Conn.recvExactFrame conn (envReadN env) (fromIntegral (maxFrameSize (envSettings env)))
    loop = do
      mFrame <- receive
      case mFrame of
        Nothing -> pure ()
        Just (Frame hdr payload) -> do
          handleFrame env receive conn handler streamsRef hdr payload
          loop
    failOutstanding = do
      streams <- readIORef streamsRef
      mapM_ (\mb -> failRequest mb (toException ClientStreamConnectionClosed)) streams


handleFrame
  :: RunEnv
  -> IO (Maybe Frame)
  -> Connection
  -> (InpObj -> EngineAux -> (OutObj -> [EnginePushPromise] -> IO ()) -> IO ())
  -> IORef (Map.Map H2.StreamId StreamMb)
  -> FrameHeader
  -> FramePayload
  -> IO ()
handleFrame env receive conn handler streamsRef hdr payload = case fhType hdr of
  FrameSettings
    | sid /= 0 -> Conn.abortConnection conn Wire.ProtocolError "SETTINGS on nonzero stream"
    | testFlag (fhFlags hdr) flagAck -> pure ()
    | SettingsFrame params <- payload -> Conn.acknowledgePeerSettings conn params
    | otherwise -> Conn.abortConnection conn Wire.FrameSizeError "invalid SETTINGS payload"
  FramePing
    | testFlag (fhFlags hdr) flagAck -> pure ()
    | otherwise -> case payload of
        PingFrame opaque -> do
          let pong = Frame (FrameHeader 8 FramePing flagAck 0) (PingFrame opaque)
          sendFrame conn pong
        _ -> pure ()
  FrameWindowUpdate -> pure ()
  FrameGoAway -> pure ()
  FrameHeaders
    | sid == 0 || even sid -> Conn.abortConnection conn Wire.ProtocolError "invalid request stream ID"
    | otherwise -> do
        block <- Conn.recvHeaderBlock conn receive hdr payload
        result <- withMVar (connHpackDecoder conn) $ \decoder ->
          decodeHeaderBlock decoder block
        case result of
          Left _ -> Conn.abortConnection conn Wire.CompressionError "hpack decode"
          Right headers -> do
            streams <- readIORef streamsRef
            case Map.lookup sid streams of
              Just mb -> do
                state <- readIORef (smRequestState mb)
                case state of
                  RequestEnded -> rejectRequest conn sid mb Wire.StreamClosed "headers after request end"
                  RequestFailed -> pure ()
                  _ -> case Conn.validateTrailerBlock (fhFlags hdr) headers of
                    Just reason -> rejectRequest conn sid mb Wire.ProtocolError reason
                    Nothing | RequestWithLength n <- state, n /= 0 ->
                      rejectRequest conn sid mb Wire.ProtocolError lengthMismatch
                    Nothing -> endRequest mb headers InputEnd
              Nothing -> do
                lastSid <- readIORef (Conn.connLastStreamId conn)
                if sid <= lastSid
                  then sendRstStream conn sid Wire.StreamClosed
                  else do
                    writeIORef (Conn.connLastStreamId conn) sid
                    case Conn.parseRequestHead headers of
                      Left _ -> sendRstStream conn sid Wire.ProtocolError
                      Right requestHead -> case Conn.readContentLength (Conn.requestHeadFields requestHead) of
                        Left _ -> sendRstStream conn sid Wire.ProtocolError
                        Right (Just n) | testFlag (fhFlags hdr) flagEndStream, n /= 0 ->
                          sendRstStream conn sid Wire.ProtocolError
                        Right declared -> do
                          unless (testFlag (fhFlags hdr) flagEndStream) $
                            Conn.sendContinueIfExpected conn sid declared requestHead
                          startStream env conn handler streamsRef hdr requestHead declared
  FrameContinuation -> Conn.abortConnection conn Wire.ProtocolError "unexpected CONTINUATION"
  FrameData
    | sid == 0 -> Conn.abortConnection conn Wire.ProtocolError "DATA on stream zero"
    | Just body <- Conn.stripFramePadding (fhFlags hdr) (rawPayload payload) -> do
        streams <- readIORef streamsRef
        updateStream <- case Map.lookup sid streams of
          Just mb -> acceptRequestData conn hdr mb body
          Nothing -> pure False
        Conn.returnFrameCredit conn sid (fhLength hdr) updateStream
    | otherwise -> Conn.abortConnection conn Wire.ProtocolError "invalid DATA padding"
  FrameRSTStream -> do
    streams <- readIORef streamsRef
    case Map.lookup (fhStreamId hdr) streams of
      Nothing -> pure ()
      Just mb -> do
        failRequest mb (toException ClientStreamConnectionClosed)
        mCancel <- readIORef (smCancel mb)
        case mCancel of
          Just c -> c (Just (toException ClientStreamConnectionClosed))
          Nothing -> pure ()
  _ -> pure ()
  where
    sid = fhStreamId hdr

acceptRequestData :: Connection -> FrameHeader -> StreamMb -> ByteString -> IO Bool
acceptRequestData conn hdr mb body = do
  state <- readIORef (smRequestState mb)
  case state of
    RequestOpen -> deliver
    RequestWithLength remaining ->
      case Conn.remainingContentLength remaining (BS.length body) endStream of
        Nothing -> do
          rejectRequest conn (fhStreamId hdr) mb Wire.ProtocolError lengthMismatch
          pure False
        Just next -> do
          when (not endStream && next /= remaining) $
            writeIORef (smRequestState mb) (RequestWithLength next)
          deliver
    _ -> pure False
  where
    endStream = testFlag (fhFlags hdr) flagEndStream
    deliver = do
      if endStream
        then endRequest mb [] (if BS.null body then InputEnd else InputFinal body)
        else unless (BS.null body) $
          atomically $ writeTBQueue (smInputQueue mb) (InputChunk body)
      pure (not endStream)

lengthMismatch :: ByteString
lengthMismatch = "request content-length mismatch"

endRequest :: StreamMb -> [Wire.Header] -> InputItem -> IO ()
endRequest mb trailers item = do
  writeIORef (smTrailerSlot mb) (Just (tokeniseHeaders trailers))
  writeIORef (smRequestState mb) RequestEnded
  atomically $ writeTBQueue (smInputQueue mb) item

failRequest :: StreamMb -> SomeException -> IO ()
failRequest mb err = do
  writeIORef (smRequestState mb) RequestFailed
  let queue = smInputQueue mb
  atomically $ do
    full <- isFullTBQueue queue
    when full (void (flushTBQueue queue))
    writeTBQueue queue (InputError err)

rejectRequest :: Connection -> H2.StreamId -> StreamMb -> Wire.ErrorCode -> ByteString -> IO ()
rejectRequest conn sid mb code reason = do
  sendRstStream conn sid code
  failRequest mb (toException (Conn.ConnectionError code reason sid))


-- | Start a worker for a fresh stream.
startStream
  :: RunEnv
  -> Connection
  -> (InpObj -> EngineAux -> (OutObj -> [EnginePushPromise] -> IO ()) -> IO ())
  -> IORef (Map.Map H2.StreamId StreamMb)
  -> FrameHeader
  -> Conn.RequestHead
  -> Maybe Word64
  -> IO ()
startStream env conn handler streamsRef hdr requestHead declared = do
  inputQ <- atomically (newTBQueue 64)
  trailerRef <- newIORef Nothing
  cancelRef <- newIORef Nothing
  stateRef <- newIORef (maybe RequestOpen RequestWithLength declared)
  let mb = StreamMb inputQ trailerRef cancelRef stateRef
      sid = fhStreamId hdr
  atomicModifyIORef' streamsRef (\m -> (Map.insert sid mb m, ()))

  when (testFlag (fhFlags hdr) flagEndStream) $ do
    endRequest mb [] InputEnd

  let readChunk :: IO (ByteString, Bool)
      readChunk = do
        item <- atomically $ readTBQueue inputQ
        case item of
          InputChunk bs -> pure (bs, False)
          InputFinal bs -> pure (bs, True)
          InputEnd -> pure (BS.empty, True)
          InputError exc -> throwIO exc
      inpObj =
        InpObj
          { inpObjHeaders = tokeniseHeaders (requestHeaderList requestHead)
          , inpObjBodySize = declared >>= Conn.contentLengthAsInt
          , inpObjBody = readChunk
          , inpObjTrailers = trailerRef
          }

  timeHandle <- TM.register (envTimeoutManager env) (pure ())
  let aux =
        EngineAux
          { envAuxTimeHandle = timeHandle
          , envAuxMySockAddr = envMySockAddr env
          , envAuxPeerSockAddr = envPeerSockAddr env
          }

  _ <-
    forkIO $
      ( handler inpObj aux $ \resp _pushes -> do
          state <- readIORef stateRef
          unless (state == RequestFailed) $ sendOutObj conn cancelRef sid resp
      )
        `catch` (\(_ :: SomeException) -> do
          state <- readIORef stateRef
          unless (state == RequestFailed) $ sendRstStream conn sid Wire.InternalError)
        `finally` do
          atomicModifyIORef' streamsRef (\m -> (Map.delete sid m, ()))
          TM.cancel timeHandle
  pure ()

requestHeaderList :: Conn.RequestHead -> [Wire.Header]
requestHeaderList headFields =
  (":method", Conn.requestHeadMethod headFields)
    : optional ":scheme" (Conn.requestHeadScheme headFields)
    <> optional ":path" (Conn.requestHeadPath headFields)
    <> optional ":authority" (Conn.requestHeadAuthority headFields)
    <> Conn.requestHeadFields headFields
  where
    optional name value
      | BS.null value = []
      | otherwise = [(name, value)]

-- | Send a complete 'OutObj' onto the wire.
sendOutObj
  :: Connection
  -> IORef (Maybe (Maybe SomeException -> IO ()))
  -> H2.StreamId
  -> OutObj
  -> IO ()
sendOutObj conn cancelRef sid outgoing =
  case outObjBody outgoing of
    OutBodyNone -> do
      trailers <- maybe (pure []) finishTrailers trailerMaker
      sendHead (null trailers)
      unless (null trailers) (sendFinalBody conn sid BS.empty trailers)
    OutBodyBuilder b -> do
      sendHead False
      sendFixedBody conn sid b trailerMaker
    OutBodyStreaming producer ->
      streamBody (\iface -> producer (outBodyPush iface) (outBodyFlush iface))
    OutBodyStreamingIface producer -> streamBody producer
    OutBodyFile _ ->
      error "Network.HTTP2.Engine.Run.Server: OutBodyFile not supported"
  where
    trailerMaker = outObjTrailerMaker outgoing
    sendHead endStream = sendEncodedHeaders conn sid endStream (ciHeadersToRaw (outObjHeaders outgoing))
    streamBody producer = do
      sendHead False
      runStreamingBody conn sid trailerMaker cancelCode throwIO $ \iface ->
        bracket_
          (writeIORef cancelRef (Just (outBodyCancel iface)))
          (writeIORef cancelRef Nothing)
          (producer iface)
    cancelCode = maybe Wire.Cancel (const Wire.InternalError)
