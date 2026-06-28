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
  throwIO,
  toException,
  try,
 )
import Control.Monad (forever, unless, void, when)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Unsafe qualified as BSU
import Data.CaseInsensitive qualified as CI
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Word (Word32)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr, plusPtr)
import Network.HTTP.Header (Header)
import Network.HTTP2.Client (ClientStreamError (..))
import Network.HTTP2.Connection (
  Connection,
  closeConnection,
  connHpackDecoder,
  connHpackEncoder,
  newConnectionFromTransport,
  sendFrame,
 )
import Network.HTTP2.Connection qualified as Conn
import Network.HTTP2.Connection.Settings (encodeSettings)
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
                    mapM_
                      ( \mb -> do
                          _ <- tryPutMVar (smHeadersVar mb) []
                          atomically $ writeTBQueue (smInputQueue mb) (InputError err)
                      )
                      (Map.elems streams)
                )
  let sendRequest req k = doSendRequest env conn streamsRef nextSidRef req k
      aux = EngineAux {engineAuxPossibleStreams = pure 100}
  client sendRequest aux `finally` do
    closeConnection conn Wire.NoError ""
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
  { smHeadersVar :: !(MVar [(ByteString, ByteString)])
  , smHeadersSent :: !(IORef Bool)
  , smInputQueue :: !(TBQueue InputItem)
  , smTrailerSlot :: !(IORef (Maybe TokenHeaderTable))
  }


data InputItem
  = InputChunk !ByteString
  | InputFinal !ByteString
  | InputEnd
  | InputError !SomeException


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
doSendRequest env conn streamsRef nextSidRef (OutObj hdrs body trailerMaker) k = do
  sid <- atomicModifyIORef' nextSidRef $ \s -> (s + 2, s)
  headersVar <- newEmptyMVar
  headersSentRef <- newIORef False
  inputQ <- atomically (newTBQueue 64)
  trailerRef <- newIORef Nothing
  let mb = StreamMb headersVar headersSentRef inputQ trailerRef
  atomicModifyIORef' streamsRef (\m -> (Map.insert sid mb m, ()))

  -- Inject :scheme and :authority before the user-supplied headers
  -- (these are required pseudo-headers on the request side).
  let augmented =
        (":scheme", envScheme env)
          : (":authority", BS.empty) -- placeholder; will be set below
          : ciHeadersToRaw hdrs
      finalHeaders = injectAuthority (envAuthority env) augmented

  block <- withMVar (connHpackEncoder conn) $ \encoder ->
    encodeHeaderBlock defaultEncodeStrategy encoder finalHeaders
  case body of
    OutBodyNone -> do
      sendHeadersFrame conn sid block True
    OutBodyBuilder b -> do
      sendHeadersFrame conn sid block False
      let bs = LBS.toStrict (BSB.toLazyByteString b)
      sendDataFrame conn sid bs True
    OutBodyStreaming f -> do
      sendHeadersFrame conn sid block False
      _ <-
        forkIO $
          runStreamingBody
            conn
            sid
            trailerMaker
            ( \iface ->
                f (outBodyPush iface) (outBodyFlush iface)
            )
            `catch` (\(_ :: SomeException) -> pure ())
      pure ()
    OutBodyStreamingIface f -> do
      sendHeadersFrame conn sid block False
      _ <-
        forkIO $
          runStreamingBody conn sid trailerMaker f
            `catch` (\(_ :: SomeException) -> pure ())
      pure ()
    OutBodyFile _ ->
      error "Network.HTTP2.Engine.Run.Client: OutBodyFile not supported"

  -- Wait for the response headers, then construct the inbound object
  -- and hand it to the caller.
  respHeaders <- takeMVar headersVar
  let inpObj =
        InpObj
          { inpObjHeaders = tokeniseHeaders respHeaders
          , inpObjBodySize = Nothing
          , inpObjBody = do
              item <- atomically $ readTBQueue inputQ
              case item of
                InputChunk bs -> pure (bs, False)
                InputFinal bs -> pure (bs, True)
                InputEnd -> pure (BS.empty, True)
                InputError exc -> throwIO exc
          , inpObjTrailers = trailerRef
          }
  k inpObj `finally` atomicModifyIORef' streamsRef (\m -> (Map.delete sid m, ()))


injectAuthority :: Authority -> [(ByteString, ByteString)] -> [(ByteString, ByteString)]
injectAuthority auth = map fix
  where
    fix (":authority", _) = (":authority", BSI.packChars auth)
    fix kv = kv


runStreamingBody
  :: Connection
  -> H2.StreamId
  -> TrailersMaker
  -> (OutBodyIface -> IO ())
  -> IO ()
runStreamingBody conn sid _tmInit body = do
  finalisedRef <- newIORef False
  let pushOne bs = unless (BS.null bs) $ sendDataFrame conn sid bs False
      pushFinal bs = do
        already <- readIORef finalisedRef
        unless already $ do
          writeIORef finalisedRef True
          sendDataFrame conn sid bs True
      cancel _ = do
        already <- readIORef finalisedRef
        unless already $ do
          writeIORef finalisedRef True
          sendRstStream conn sid Wire.Cancel
      iface =
        OutBodyIface
          { outBodyUnmask = id
          , outBodyPush = \b -> pushOne (LBS.toStrict (BSB.toLazyByteString b))
          , outBodyPushFinal = \b -> pushFinal (LBS.toStrict (BSB.toLazyByteString b))
          , outBodyCancel = cancel
          , outBodyFlush = pure ()
          }
  r <- try (body iface)
  case r of
    Left (e :: SomeException) -> do
      already <- readIORef finalisedRef
      unless already $ do
        writeIORef finalisedRef True
        sendRstStream conn sid Wire.InternalError
      throwIO e
    Right () -> do
      already <- readIORef finalisedRef
      unless already $ do
        writeIORef finalisedRef True
        sendDataFrame conn sid BS.empty True


-- | Response frame dispatch loop.
recvLoop
  :: RunEnv
  -> Connection
  -> IORef (Map.Map H2.StreamId StreamMb)
  -> IO ()
recvLoop env conn streamsRef = loop
  where
    loop = do
      mFrame <- pumpFrame env
      case mFrame of
        Nothing -> do
          streams <- readIORef streamsRef
          let err = toException ClientStreamConnectionClosed
          mapM_
            ( \mb -> do
                _ <- tryPutMVar (smHeadersVar mb) []
                atomically $ writeTBQueue (smInputQueue mb) (InputError err)
            )
            (Map.elems streams)
        Just (Frame hdr payload) -> do
          handleClientFrame env conn streamsRef hdr payload
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


handleClientFrame
  :: RunEnv
  -> Connection
  -> IORef (Map.Map H2.StreamId StreamMb)
  -> FrameHeader
  -> FramePayload
  -> IO ()
handleClientFrame _env conn streamsRef hdr payload = case fhType hdr of
  FrameSettings
    | testFlag (fhFlags hdr) flagAck -> pure ()
    | otherwise ->
        sendFrame conn $
          Frame
            (FrameHeader 0 FrameSettings flagAck 0)
            (SettingsFrame [])
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
                  alreadySent <- readIORef (smHeadersSent mb)
                  if not alreadySent
                    then do
                      writeIORef (smHeadersSent mb) True
                      putMVar (smHeadersVar mb) headers
                      when (testFlag (fhFlags hdr) flagEndStream) $ do
                        writeIORef (smTrailerSlot mb) (Just (tokeniseHeaders headers))
                        atomically $ writeTBQueue (smInputQueue mb) InputEnd
                    else do
                      writeIORef (smTrailerSlot mb) (Just (tokeniseHeaders headers))
                      atomically $ writeTBQueue (smInputQueue mb) InputEnd
                Nothing -> pure ()
        _ -> pure ()
    | otherwise -> closeConnection conn Wire.ProtocolError "fragmented HEADERS"
  FrameData -> case payload of
    DataFrame body -> do
      streams <- readIORef streamsRef
      case Map.lookup (fhStreamId hdr) streams of
        Just mb -> do
          let isEnd = testFlag (fhFlags hdr) flagEndStream
          case (BS.null body, isEnd) of
            (True, True) -> do
              writeIORef (smTrailerSlot mb) (Just (tokeniseHeaders []))
              atomically $ writeTBQueue (smInputQueue mb) InputEnd
            (False, True) -> do
              writeIORef (smTrailerSlot mb) (Just (tokeniseHeaders []))
              atomically $ writeTBQueue (smInputQueue mb) (InputFinal body)
            (False, False) ->
              atomically $ writeTBQueue (smInputQueue mb) (InputChunk body)
            (True, False) -> pure ()
          let len = fhLength hdr
          when (len > 0) $
            void $
              trySendWindowUpdates conn (fhStreamId hdr) len
        Nothing -> pure ()
  FrameRSTStream -> do
    streams <- readIORef streamsRef
    case Map.lookup (fhStreamId hdr) streams of
      Just mb ->
        atomically $
          writeTBQueue
            (smInputQueue mb)
            (InputError (toException ClientStreamConnectionClosed))
      Nothing -> pure ()
  _ -> pure ()


trySendWindowUpdates :: Connection -> H2.StreamId -> Word32 -> IO Bool
trySendWindowUpdates conn sid len =
  do
    let connWu = Frame (FrameHeader 4 FrameWindowUpdate 0 0) (WindowUpdateFrame len)
        streamWu = Frame (FrameHeader 4 FrameWindowUpdate 0 sid) (WindowUpdateFrame len)
    sendFrame conn connWu
    sendFrame conn streamWu
    pure True
    `catch` \(_ :: SomeException) -> pure False


ciHeadersToRaw :: [Header] -> [(ByteString, ByteString)]
ciHeadersToRaw = map (\(k, v) -> (CI.original k, v))


sendHeadersFrame :: Connection -> H2.StreamId -> ByteString -> Bool -> IO ()
sendHeadersFrame conn sid block endStream =
  let flags = flagEndHeaders .|. (if endStream then flagEndStream else 0)
      frame =
        Frame
          (FrameHeader (fromIntegral (BS.length block)) FrameHeaders flags sid)
          (HeadersFrame Nothing block)
  in sendFrame conn frame


sendDataFrame :: Connection -> H2.StreamId -> ByteString -> Bool -> IO ()
sendDataFrame conn sid body endStream =
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
