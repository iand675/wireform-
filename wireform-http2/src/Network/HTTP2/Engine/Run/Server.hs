{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Server-side frame I\/O loop for the wireform-http2 gRPC engine.

The runtime that backs 'Network.HTTP2.Engine.Server.run'. Designed
for gRPC's request shape:

  * HEADERS [END_HEADERS] (request headers)
  * DATA frames (request body) optionally followed by HEADERS
    [END_HEADERS, END_STREAM] for inbound trailers
  * The handler invokes 'respond' with a 'Response' that the engine
    translates into HEADERS / DATA / trailing HEADERS frames.

The implementation is intentionally narrower than @http2@ 5.3.x:
no PUSH_PROMISE, no responseFile, no CONTINUATION-fragmented
HEADERS, no priority enforcement. Flow control is implemented in
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
import Control.Exception (Exception, SomeException, catch, finally, throwIO, toException, try)
import Control.Monad (forever, unless, void, when)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Word (Word32)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import Network.HTTP.Header (Header)
import Network.HTTP2.Client (ClientStreamError (..))
import Network.HTTP2.Connection (
  Connection,
  closeConnection,
  connHpackDecoder,
  sendEncodedHeaders,
  newConnectionFromTransport,
  sendFrame,
 )
import Network.HTTP2.Connection qualified as Conn
import Network.HTTP2.Connection.Settings (encodeSettings)
import Network.HTTP2.Connection.Settings qualified as ConnSettings
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
  , smClosedRemote :: !(IORef Bool)
  }


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
    then closeConnection conn Wire.ProtocolError "bad preface"
    else do
      sendServerPrefaceFrames conn (envSettings env)
      streamsRef <- newIORef Map.empty
      connLoop env conn handler streamsRef
        `finally` closeConnection conn Wire.NoError ""


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
connLoop env conn handler streamsRef = loop
  where
    loop = do
      mFrame <- pumpFrame env
      case mFrame of
        Nothing -> do
          streams <- readIORef streamsRef
          let err = toException ClientStreamConnectionClosed
          mapM_
            ( \mb ->
                atomically $
                  writeTBQueue
                    (smInputQueue mb)
                    (InputError err)
            )
            (Map.elems streams)
        Just (Frame hdr payload) -> do
          handleFrame env conn handler streamsRef hdr payload
          loop


pumpFrame :: RunEnv -> IO (Maybe Frame)
pumpFrame env = do
  hdrBs <- envReadN env frameHeaderLength
  if BS.length hdrBs < frameHeaderLength
    then pure Nothing
    else case decodeFrameHeader hdrBs of
      Left _ -> pure Nothing
      Right hdr -> do
        let plen = fromIntegral (fhLength hdr)
        payloadBs <- if plen == 0 then pure BS.empty else envReadN env plen
        case decodeFramePayload hdr payloadBs of
          Left _ -> pure Nothing
          Right p -> pure (Just (Frame hdr p))


handleFrame
  :: RunEnv
  -> Connection
  -> (InpObj -> EngineAux -> (OutObj -> [EnginePushPromise] -> IO ()) -> IO ())
  -> IORef (Map.Map H2.StreamId StreamMb)
  -> FrameHeader
  -> FramePayload
  -> IO ()
handleFrame env conn handler streamsRef hdr payload = case fhType hdr of
  FrameSettings
    | testFlag (fhFlags hdr) flagAck -> pure ()
    | otherwise -> do
        let ack = Frame (FrameHeader 0 FrameSettings flagAck 0) (SettingsFrame [])
        sendFrame conn ack
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
    | testFlag (fhFlags hdr) flagEndHeaders -> case payload of
        HeadersFrame _ block -> do
          result <- withMVar (connHpackDecoder conn) $ \decoder ->
            decodeHeaderBlock decoder block
          case result of
            Left _ -> closeConnection conn Wire.CompressionError "hpack decode"
            Right headers -> do
              streams <- readIORef streamsRef
              case Map.lookup (fhStreamId hdr) streams of
                Just mb -> do
                  writeIORef (smTrailerSlot mb) (Just (tokeniseHeaders headers))
                  writeIORef (smClosedRemote mb) True
                  atomically $ writeTBQueue (smInputQueue mb) InputEnd
                Nothing ->
                  startStream env conn handler streamsRef hdr headers
        _ -> pure ()
    | otherwise ->
        closeConnection conn Wire.ProtocolError "fragmented HEADERS unsupported"
  FrameData -> case payload of
    DataFrame body -> do
      streams <- readIORef streamsRef
      case Map.lookup (fhStreamId hdr) streams of
        Nothing -> pure ()
        Just mb -> do
          let isEnd = testFlag (fhFlags hdr) flagEndStream
          case (BS.null body, isEnd) of
            (True, True) -> do
              alreadyClosed <- readIORef (smClosedRemote mb)
              unless alreadyClosed $ do
                writeIORef (smTrailerSlot mb) (Just (tokeniseHeaders []))
                writeIORef (smClosedRemote mb) True
                atomically $ writeTBQueue (smInputQueue mb) InputEnd
            (False, True) -> do
              writeIORef (smTrailerSlot mb) (Just (tokeniseHeaders []))
              writeIORef (smClosedRemote mb) True
              atomically $ writeTBQueue (smInputQueue mb) (InputFinal body)
            (False, False) ->
              atomically $ writeTBQueue (smInputQueue mb) (InputChunk body)
            (True, False) -> pure ()
          let len = fhLength hdr
          when (len > 0) $
            void $
              trySendWindowUpdates conn (fhStreamId hdr) len
    _ -> pure ()
  FrameRSTStream -> do
    streams <- readIORef streamsRef
    case Map.lookup (fhStreamId hdr) streams of
      Nothing -> pure ()
      Just mb -> do
        writeIORef (smClosedRemote mb) True
        atomically $
          writeTBQueue
            (smInputQueue mb)
            (InputError (toException ClientStreamConnectionClosed))
        mCancel <- readIORef (smCancel mb)
        case mCancel of
          Just c -> c (Just (toException ClientStreamConnectionClosed))
          Nothing -> pure ()
  _ -> pure ()


-- | Start a worker for a fresh stream.
startStream
  :: RunEnv
  -> Connection
  -> (InpObj -> EngineAux -> (OutObj -> [EnginePushPromise] -> IO ()) -> IO ())
  -> IORef (Map.Map H2.StreamId StreamMb)
  -> FrameHeader
  -> [(ByteString, ByteString)]
  -> IO ()
startStream env conn handler streamsRef hdr headers = do
  inputQ <- atomically (newTBQueue 64)
  trailerRef <- newIORef Nothing
  cancelRef <- newIORef Nothing
  closedRemoteRef <- newIORef False
  let mb = StreamMb inputQ trailerRef cancelRef closedRemoteRef
      sid = fhStreamId hdr
  atomicModifyIORef' streamsRef (\m -> (Map.insert sid mb m, ()))

  -- Trailers-Only request: HEADERS w/ END_STREAM set on the
  -- initial frame.
  when (testFlag (fhFlags hdr) flagEndStream) $ do
    writeIORef trailerRef (Just (tokeniseHeaders []))
    writeIORef closedRemoteRef True
    atomically $ writeTBQueue inputQ InputEnd

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
          { inpObjHeaders = tokeniseHeaders headers
          , inpObjBodySize = Nothing
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
      ( handler inpObj aux $ \resp _pushes ->
          sendOutObj conn cancelRef sid resp
      )
        `catch` (\(_ :: SomeException) -> sendRstStream conn sid Wire.InternalError)
        `finally` do
          atomicModifyIORef' streamsRef (\m -> (Map.delete sid m, ()))
          TM.cancel timeHandle
  pure ()


-- | Send a complete 'OutObj' onto the wire.
sendOutObj
  :: Connection
  -> IORef (Maybe (Maybe SomeException -> IO ()))
  -> H2.StreamId
  -> OutObj
  -> IO ()
sendOutObj conn cancelRef sid (OutObj hdrs body trailerMaker) = do
  let rawHdrs = ciHeadersToRaw hdrs
  case body of
    OutBodyNone -> do
      Trailers tr <- trailerMaker Nothing
      if null tr
        then sendEncodedHeaders conn sid True rawHdrs
        else do
          sendEncodedHeaders conn sid False rawHdrs
          sendTrailerBlock conn sid (ciHeadersToRaw tr)
    OutBodyBuilder b -> do
      sendEncodedHeaders conn sid False rawHdrs
      let bs = LBS.toStrict (BSB.toLazyByteString b)
      _ <- trailerMaker (Just bs)
      Trailers tr <- trailerMaker Nothing
      if null tr
        then sendData conn sid bs True
        else do
          sendData conn sid bs False
          sendTrailerBlock conn sid (ciHeadersToRaw tr)
    OutBodyStreaming f -> do
      sendEncodedHeaders conn sid False rawHdrs
      runStreamingPushFlush conn cancelRef sid trailerMaker f
    OutBodyStreamingIface f -> do
      sendEncodedHeaders conn sid False rawHdrs
      runStreamingIface conn cancelRef sid trailerMaker f
    OutBodyFile _ ->
      error "Network.HTTP2.Engine.Run.Server: OutBodyFile not supported"


runStreamingPushFlush
  :: Connection
  -> IORef (Maybe (Maybe SomeException -> IO ()))
  -> H2.StreamId
  -> TrailersMaker
  -> ((BSB.Builder -> IO ()) -> IO () -> IO ())
  -> IO ()
runStreamingPushFlush conn cancelRef sid tm body =
  runStreamingIface conn cancelRef sid tm $ \iface ->
    body (outBodyPush iface) (outBodyFlush iface)


runStreamingIface
  :: Connection
  -> IORef (Maybe (Maybe SomeException -> IO ()))
  -> H2.StreamId
  -> TrailersMaker
  -> (OutBodyIface -> IO ())
  -> IO ()
runStreamingIface conn cancelRef sid trailerMakerInit body = do
  finalisedRef <- newIORef False
  trailerMakerRef <- newIORef trailerMakerInit
  let pushOne bs = do
        unless (BS.null bs) $ sendData conn sid bs False
        updateMaker trailerMakerRef (Just bs)
      pushFinal bs = do
        already <- readIORef finalisedRef
        unless already $ do
          unless (BS.null bs) $ sendData conn sid bs False
          updateMaker trailerMakerRef (Just bs)
      cancel mExc = do
        already <- readIORef finalisedRef
        unless already $ do
          writeIORef finalisedRef True
          let code = maybe Wire.Cancel (const Wire.InternalError) mExc
          sendRstStream conn sid code
      flushBody = pure ()
      finalise = do
        already <- readIORef finalisedRef
        unless already $ do
          writeIORef finalisedRef True
          tm <- readIORef trailerMakerRef
          Trailers tr <- tm Nothing
          if null tr
            then sendData conn sid BS.empty True
            else sendTrailerBlock conn sid (ciHeadersToRaw tr)
      iface =
        OutBodyIface
          { outBodyUnmask = id
          , outBodyPush = \b -> pushOne (LBS.toStrict (BSB.toLazyByteString b))
          , outBodyPushFinal = \b -> pushFinal (LBS.toStrict (BSB.toLazyByteString b))
          , outBodyCancel = cancel
          , outBodyFlush = flushBody
          }
  writeIORef cancelRef (Just cancel)
  r <- try (body iface)
  writeIORef cancelRef Nothing
  case r of
    Left (e :: SomeException) -> do
      already <- readIORef finalisedRef
      unless already $ do
        writeIORef finalisedRef True
        sendRstStream conn sid Wire.InternalError
      throwIO e
    Right () -> finalise


updateMaker :: IORef TrailersMaker -> Maybe ByteString -> IO ()
updateMaker ref mbs = do
  tm <- readIORef ref
  next <- tm mbs
  case next of
    NextTrailersMaker tm' -> writeIORef ref tm'
    Trailers _ -> pure ()


ciHeadersToRaw :: [Header] -> [(ByteString, ByteString)]
ciHeadersToRaw = map (\(k, v) -> (CI.original k, v))


sendTrailerBlock :: Connection -> H2.StreamId -> [(ByteString, ByteString)] -> IO ()
sendTrailerBlock conn sid hdrs =
  sendEncodedHeaders conn sid True hdrs


sendData :: Connection -> H2.StreamId -> ByteString -> Bool -> IO ()
sendData conn sid body endStream =
  let flags = if endStream then flagEndStream else 0
      frame =
        Frame
          (FrameHeader (fromIntegral (BS.length body)) FrameData flags sid)
          (DataFrame body)
  in sendFrame conn frame


sendRstStream :: Connection -> H2.StreamId -> Wire.ErrorCode -> IO ()
sendRstStream conn sid code =
  let frame =
        Frame
          (FrameHeader 4 FrameRSTStream 0 sid)
          (RSTStreamFrame code)
  in sendFrame conn frame `catch` (\(_ :: SomeException) -> pure ())


trySendWindowUpdates :: Connection -> H2.StreamId -> Word32 -> IO Bool
trySendWindowUpdates conn sid len =
  do
    let connWu = Frame (FrameHeader 4 FrameWindowUpdate 0 0) (WindowUpdateFrame len)
        streamWu = Frame (FrameHeader 4 FrameWindowUpdate 0 sid) (WindowUpdateFrame len)
    sendFrame conn connWu
    sendFrame conn streamWu
    pure True
    `catch` \(_ :: SomeException) -> pure False
