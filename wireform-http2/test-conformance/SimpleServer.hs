module Main (main) where

import Control.Concurrent (forkIO, threadDelay, yield)
import Control.Concurrent.MVar (readMVar, newMVar, modifyMVar_, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, bracket, finally)
import Data.Bits ((.|.), (.&.), testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word
import Network.Socket (Socket)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS
import System.IO (hPutStrLn, stderr, hFlush)

import Network.HTTP2.Connection hiding (StreamState(..), ClosedReason(..))
import Network.HTTP2.Connection.Settings
import Network.HTTP2.Frame
import Network.HTTP2.HPACK
import Network.HTTP2.Types (StreamId, FrameType(..), ErrorCode(..), Settings(..), defaultSettings, Priority(..))

data StreamInfo = StreamInfo
  { siState :: !StreamSt
  , siSendWindow :: !Int
  , siContentLength :: !(Maybe Int)  -- expected content-length, Nothing = not set
  , siReceivedBytes :: !Int          -- actual bytes received
  } deriving stock (Eq, Show)

data StreamSt
  = StIdle
  | StOpen
  | StHalfClosedRemote
  | StHalfClosedLocal
  | StClosed
  deriving stock (Eq, Show)

-- Pending DATA to send, respecting flow control
data PendingData = PendingData
  { pdStreamId :: !StreamId
  , pdData :: !ByteString
  , pdEndStream :: !Bool
  }

data ServerState = ServerState
  { ssStreams :: !(Map StreamId StreamInfo)
  , ssLastStreamId :: !StreamId
  , ssLocalSettings :: !Settings
  , ssRemoteSettings :: !Settings
  , ssExpectingContinuation :: !(Maybe StreamId)
  , ssContinuationBuffer :: !ByteString
  , ssGoAwaySent :: !Bool
  , ssConnSendWindow :: !Int
  , ssPendingSends :: ![PendingData]
  }

main :: IO ()
main = do
  let port = "9090"
  hPutStrLn stderr $ "Starting HTTP/2 conformance server on port " <> port
  hFlush stderr
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just port)
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
        hPutStrLn stderr "Listening..."
        hFlush stderr
        acceptLoop sock

acceptLoop :: Socket -> IO ()
acceptLoop listenSock = do
  (clientSock, _) <- NS.accept listenSock
  NS.setSocketOption clientSock NS.NoDelay 1
  _ <- forkIO $ handleClient clientSock
    `catch` (\(_ :: SomeException) -> NS.close clientSock)
  acceptLoop listenSock

handleClient :: Socket -> IO ()
handleClient sock = do
  preface <- recvExact sock 24
  if preface /= connectionPreface
    then do
      conn <- newConnection ConnectionConfig
        { ccRole = RoleServer
        , ccSettings = defaultSettings
        , ccSocket = Just sock
        , ccTransport = Nothing
        , ccOnGoAway = \_ _ _ -> pure ()
        }
      closeConnection conn ProtocolError ""
      releaseConnection conn
      NS.close sock
    else do
      conn <- newConnection ConnectionConfig
        { ccRole = RoleServer
        , ccSettings = defaultSettings
            { settingsMaxConcurrentStreams = Just 100
            }
        , ccSocket = Just sock
        , ccTransport = Nothing
        , ccOnGoAway = \_ _ _ -> pure ()
        }
      let initSettings = defaultSettings { settingsMaxConcurrentStreams = Just 100 }
      sendServerSettings conn initSettings
      stRef <- newIORef ServerState
        { ssStreams = Map.empty
        , ssLastStreamId = 0
        , ssLocalSettings = initSettings
        , ssRemoteSettings = defaultSettings
        , ssExpectingContinuation = Nothing
        , ssContinuationBuffer = BS.empty
        , ssGoAwaySent = False
        , ssConnSendWindow = 65535
        , ssPendingSends = []
        }
      serverLoop conn stRef `finally` (releaseConnection conn >> NS.close sock)

sendServerSettings :: Connection -> Settings -> IO ()
sendServerSettings conn settings = do
  let params = encodeSettings settings
      frame = Frame
        (FrameHeader (fromIntegral (length params * 6)) FrameSettings 0 0)
        (SettingsFrame params)
  sendFrame conn frame

serverLoop :: Connection -> IORef ServerState -> IO ()
serverLoop conn stRef = do
  result <- recvFrame conn
  case result of
    Left _ -> do
      flushPendingSends conn stRef
    Right frame -> do
      shouldContinue <- processFrame conn stRef frame
      if shouldContinue
        then do
          -- Always try to flush after processing a frame
          flushPendingSends conn stRef
          serverLoop conn stRef
        else pure ()

-- Flush any pending DATA frames that now fit in the flow control window
flushPendingSends :: Connection -> IORef ServerState -> IO ()
flushPendingSends conn stRef = do
  st <- readIORef stRef
  case ssPendingSends st of
    [] -> pure ()
    (pd:rest) -> do
      let sid = pdStreamId pd
          body = pdData pd
          bodyLen = BS.length body
          streamWindow = maybe 65535 siSendWindow (Map.lookup sid (ssStreams st))
          connWindow = ssConnSendWindow st
          maxSend = min streamWindow connWindow
      if maxSend <= 0
        then pure ()
        else do
          let sendLen = min bodyLen maxSend
              chunk = BS.take sendLen body
              remaining = BS.drop sendLen body
              isLast = BS.null remaining && pdEndStream pd
              flags = if isLast then flagEndStream else 0
              dataFrame = Frame
                (FrameHeader (fromIntegral sendLen) FrameData flags sid)
                (DataFrame chunk)
          sendFrame conn dataFrame
          let newConnWindow = connWindow - sendLen
              newStreamWindow = streamWindow - sendLen
          if isLast
            then writeIORef stRef st
              { ssPendingSends = rest
              , ssConnSendWindow = newConnWindow
              , ssStreams = Map.insert sid (StreamInfo StClosed 0 Nothing 0) (ssStreams st)
              }
            else do
              let newPd = pd { pdData = remaining }
              writeIORef stRef st
                { ssPendingSends = newPd : rest
                , ssConnSendWindow = newConnWindow
                , ssStreams = Map.adjust (\si -> si { siSendWindow = newStreamWindow }) sid (ssStreams st)
                }
              -- Try to flush more
              flushPendingSends conn stRef

processFrame :: Connection -> IORef ServerState -> Frame -> IO Bool
processFrame conn stRef (Frame hdr payload) = do
  st <- readIORef stRef
  let maxFrameSize = settingsMaxFrameSize (ssLocalSettings st)
  if fhLength hdr > maxFrameSize
    then do
      connError conn stRef FrameSizeError
      pure False
    else processFrame' conn stRef st hdr payload

processFrame' :: Connection -> IORef ServerState -> ServerState -> FrameHeader -> FramePayload -> IO Bool
processFrame' conn stRef st hdr payload =
  case ssExpectingContinuation st of
    Just expectedSid -> case payload of
      ContinuationFrame fragment
        | fhStreamId hdr == expectedSid -> do
            let buf = ssContinuationBuffer st <> fragment
            if testFlag (fhFlags hdr) flagEndHeaders
              then do
                writeIORef stRef st
                  { ssExpectingContinuation = Nothing
                  , ssContinuationBuffer = BS.empty
                  }
                processHeaders conn stRef (fhStreamId hdr) buf
              else do
                writeIORef stRef st { ssContinuationBuffer = buf }
                pure True
        | otherwise -> do
            connError conn stRef ProtocolError
            pure False
      _ -> do
        connError conn stRef ProtocolError
        pure False
    Nothing -> case payload of
      ContinuationFrame _ -> do
        connError conn stRef ProtocolError
        pure False
      _ -> processFramePayload conn stRef hdr payload

processFramePayload :: Connection -> IORef ServerState -> FrameHeader -> FramePayload -> IO Bool
processFramePayload conn stRef hdr payload = case payload of
  SettingsFrame params
    | testFlag (fhFlags hdr) flagAck -> pure True
    | fhStreamId hdr /= 0 -> do
        connError conn stRef ProtocolError
        pure False
    | otherwise -> do
        st <- readIORef stRef
        case validateAndApplySettings (ssRemoteSettings st) params of
          Left code -> do
            connError conn stRef code
            pure False
          Right newSettings -> do
            let oldInitWindow = fromIntegral (settingsInitialWindowSize (ssRemoteSettings st))
                newInitWindow = fromIntegral (settingsInitialWindowSize newSettings)
                diff = newInitWindow - oldInitWindow
            -- Adjust all existing stream windows
            let adjustedStreams = Map.map (\si -> si { siSendWindow = siSendWindow si + diff }) (ssStreams st)
            writeIORef stRef st
              { ssRemoteSettings = newSettings
              , ssStreams = adjustedStreams
              }
            let ack = Frame (FrameHeader 0 FrameSettings flagAck 0) (SettingsFrame [])
            sendFrame conn ack
            pure True

  PingFrame opaqueData
    | fhStreamId hdr /= 0 -> do
        connError conn stRef ProtocolError
        pure False
    | BS.length opaqueData /= 8 -> do
        connError conn stRef FrameSizeError
        pure False
    | testFlag (fhFlags hdr) flagAck -> pure True
    | otherwise -> do
        let pong = Frame (FrameHeader 8 FramePing flagAck 0) (PingFrame opaqueData)
        sendFrame conn pong
        pure True

  WindowUpdateFrame increment
    | fhStreamId hdr == 0 -> do
        if increment == 0
          then do
            connError conn stRef ProtocolError
            pure False
          else do
            st <- readIORef stRef
            let newWindow = ssConnSendWindow st + fromIntegral increment
            if newWindow > 2147483647
              then do
                connError conn stRef FlowControlError
                pure False
              else do
                writeIORef stRef st { ssConnSendWindow = newWindow }
                pure True
    | otherwise -> do
        st <- readIORef stRef
        let streamSt = getStreamState st (fhStreamId hdr)
        case streamSt of
          StIdle
            | fhStreamId hdr `mod` 2 == 0 -> do
                connError conn stRef ProtocolError
                pure False
            | fhStreamId hdr > ssLastStreamId st -> do
                connError conn stRef ProtocolError
                pure False
            | otherwise ->
                if increment == 0
                  then do
                    streamError conn (fhStreamId hdr) ProtocolError
                    pure True
                  else pure True
          StHalfClosedRemote ->
            if increment == 0
              then do
                streamError conn (fhStreamId hdr) ProtocolError
                pure True
              else do
                checkStreamWindowOverflow conn stRef (fhStreamId hdr) increment
          StHalfClosedLocal ->
            if increment == 0
              then do
                streamError conn (fhStreamId hdr) ProtocolError
                pure True
              else do
                checkStreamWindowOverflow conn stRef (fhStreamId hdr) increment
          StClosed ->
            if increment == 0
              then pure True
              else checkStreamWindowOverflow conn stRef (fhStreamId hdr) increment
          _ ->
            if increment == 0
              then do
                streamError conn (fhStreamId hdr) ProtocolError
                pure True
              else do
                checkStreamWindowOverflow conn stRef (fhStreamId hdr) increment

  GoAwayFrame _ _ _ -> do
    closeConnection conn NoError ""
    pure False

  HeadersFrame mpri headerBlock
    | fhStreamId hdr == 0 -> do
        connError conn stRef ProtocolError
        pure False
    | fhStreamId hdr `mod` 2 == 0 -> do
        connError conn stRef ProtocolError
        pure False
    | otherwise -> do
        st <- readIORef stRef
        -- Check self-dependency
        case mpri of
          Just pri | priorityDependency pri == fhStreamId hdr -> do
            streamError conn (fhStreamId hdr) ProtocolError
            pure True
          _ -> do
            -- Stream ID must be greater than last (unless it's an existing stream)
            let streamSt = getStreamState st (fhStreamId hdr)
            if fhStreamId hdr <= ssLastStreamId st && ssLastStreamId st /= 0 && streamSt == StIdle
              then do
                connError conn stRef ProtocolError
                pure False
              else do
                -- Check concurrent stream limit
                let maxConcurrent = maybe 100 id (settingsMaxConcurrentStreams (ssLocalSettings st))
                    openCount = Map.size (Map.filter (\si -> siState si == StOpen || siState si == StHalfClosedRemote || siState si == StHalfClosedLocal) (ssStreams st))
                if fromIntegral openCount >= maxConcurrent
                  then do
                    streamError conn (fhStreamId hdr) RefusedStream
                    modifyIORef' stRef $ \s -> s
                      { ssLastStreamId = fhStreamId hdr
                      , ssStreams = Map.insert (fhStreamId hdr) (StreamInfo StClosed 0 Nothing 0) (ssStreams s)
                      }
                    pure True
                  else do
                    let streamSt = getStreamState st (fhStreamId hdr)
                    case streamSt of
                      StHalfClosedRemote -> do
                        streamError conn (fhStreamId hdr) StreamClosed
                        pure True
                      StClosed -> do
                        connError conn stRef StreamClosed
                        pure False
                      StOpen ->
                        if not (testFlag (fhFlags hdr) flagEndStream)
                          then do
                            connError conn stRef ProtocolError
                            pure False
                          else do
                            if testFlag (fhFlags hdr) flagEndHeaders
                              then processTrailers conn stRef (fhStreamId hdr) headerBlock
                              else do
                                modifyIORef' stRef $ \s -> s
                                  { ssExpectingContinuation = Just (fhStreamId hdr)
                                  , ssContinuationBuffer = headerBlock
                                  }
                                pure True
                      _ -> do
                        writeIORef stRef st { ssLastStreamId = fhStreamId hdr }
                        let hasEndStream = testFlag (fhFlags hdr) flagEndStream
                        if testFlag (fhFlags hdr) flagEndHeaders
                          then if hasEndStream
                            then processHeaders conn stRef (fhStreamId hdr) headerBlock
                            else processHeadersNoEnd conn stRef (fhStreamId hdr) headerBlock
                          else do
                            modifyIORef' stRef $ \s -> s
                              { ssExpectingContinuation = Just (fhStreamId hdr)
                              , ssContinuationBuffer = headerBlock
                              }
                            pure True

  DataFrame _body
    | fhStreamId hdr == 0 -> do
        connError conn stRef ProtocolError
        pure False
    | otherwise -> do
        st <- readIORef stRef
        let streamSt = getStreamState st (fhStreamId hdr)
        case streamSt of
          StIdle -> do
            connError conn stRef ProtocolError
            pure False
          StHalfClosedRemote -> do
            streamError conn (fhStreamId hdr) StreamClosed
            pure True
          StClosed -> do
            connError conn stRef StreamClosed
            pure False
          _ -> do
            let len = fhLength hdr
                dataLen = fromIntegral len
            if len > 0
              then do
                let windowUpdate = Frame
                      (FrameHeader 4 FrameWindowUpdate 0 0)
                      (WindowUpdateFrame len)
                sendFrame conn windowUpdate
                let streamWU = Frame
                      (FrameHeader 4 FrameWindowUpdate 0 (fhStreamId hdr))
                      (WindowUpdateFrame len)
                sendFrame conn streamWU
              else pure ()
            -- Update received byte count
            modifyIORef' stRef $ \s -> s
              { ssStreams = Map.adjust (\si -> si { siReceivedBytes = siReceivedBytes si + dataLen }) (fhStreamId hdr) (ssStreams s)
              }
            if testFlag (fhFlags hdr) flagEndStream
              then do
                st2 <- readIORef stRef
                let mInfo = Map.lookup (fhStreamId hdr) (ssStreams st2)
                    received = maybe 0 siReceivedBytes mInfo
                    mExpected = mInfo >>= siContentLength
                case mExpected of
                  Just expected | expected /= received -> do
                    streamError conn (fhStreamId hdr) ProtocolError
                    modifyIORef' stRef $ \s -> s
                      { ssStreams = Map.insert (fhStreamId hdr) (StreamInfo StClosed 0 Nothing 0) (ssStreams s)
                      }
                  _ -> do
                    modifyIORef' stRef $ \s -> s
                      { ssStreams = Map.adjust (\si -> si { siState = StHalfClosedRemote }) (fhStreamId hdr) (ssStreams s)
                      }
                    queueResponse conn stRef (fhStreamId hdr)
              else pure ()
            pure True

  RSTStreamFrame _code
    | fhStreamId hdr == 0 -> do
        connError conn stRef ProtocolError
        pure False
    | otherwise -> do
        st <- readIORef stRef
        let streamSt = getStreamState st (fhStreamId hdr)
        case streamSt of
          StIdle -> do
            connError conn stRef ProtocolError
            pure False
          _ -> do
            modifyIORef' stRef $ \s -> s
              { ssStreams = Map.insert (fhStreamId hdr)
                  (StreamInfo StClosed 0 Nothing 0) (ssStreams s)
              , ssPendingSends = filter (\pd -> pdStreamId pd /= fhStreamId hdr) (ssPendingSends s)
              }
            pure True

  PriorityFrame pri
    | fhStreamId hdr == 0 -> do
        connError conn stRef ProtocolError
        pure False
    | priorityDependency pri == fhStreamId hdr -> do
        streamError conn (fhStreamId hdr) ProtocolError
        pure True
    | otherwise -> pure True

  PushPromiseFrame _ _ -> do
    connError conn stRef ProtocolError
    pure False

  UnknownFrame _ _ -> pure True

  _ -> pure True

processHeaders :: Connection -> IORef ServerState -> StreamId -> ByteString -> IO Bool
processHeaders conn stRef sid headerBlock = processHeadersEndStream conn stRef sid headerBlock True

processHeadersNoEnd :: Connection -> IORef ServerState -> StreamId -> ByteString -> IO Bool
processHeadersNoEnd conn stRef sid headerBlock = processHeadersEndStream conn stRef sid headerBlock False

processHeadersEndStream :: Connection -> IORef ServerState -> StreamId -> ByteString -> Bool -> IO Bool
processHeadersEndStream conn stRef sid headerBlock endStream = do
  decoder <- readMVar (connHpackDecoder conn)
  st <- readIORef stRef
  let tableSize = fromIntegral (settingsHeaderTableSize (ssLocalSettings st))
  result <- decodeHeaderBlockWithMaxSize decoder tableSize headerBlock
  case result of
    Left _ -> do
      connError conn stRef CompressionError
      pure False
    Right headers -> do
      case validateRequestHeaders headers of
        Left _ -> do
          streamError conn sid ProtocolError
          modifyIORef' stRef $ \s -> s
            { ssStreams = Map.insert sid (StreamInfo StClosed 0 Nothing 0) (ssStreams s)
            }
          pure True
        Right () -> do
          st2 <- readIORef stRef
          let initWin = fromIntegral (settingsInitialWindowSize (ssRemoteSettings st2))
              streamState = if endStream then StHalfClosedRemote else StOpen
              contentLen = case lookup "content-length" headers of
                Just v -> case parseDecimal v of
                  Just n -> Just n
                  Nothing -> Nothing
                Nothing -> Nothing
          modifyIORef' stRef $ \s -> s
            { ssStreams = Map.insert sid (StreamInfo streamState initWin contentLen 0) (ssStreams s)
            }
          if endStream
            then do
              -- Check content-length against 0 bytes received (no body)
              case contentLen of
                Just n | n /= 0 -> do
                  streamError conn sid ProtocolError
                  modifyIORef' stRef $ \s2 -> s2
                    { ssStreams = Map.insert sid (StreamInfo StClosed 0 Nothing 0) (ssStreams s2)
                    }
                _ -> queueResponse conn stRef sid
            else pure ()
          pure True

processTrailers :: Connection -> IORef ServerState -> StreamId -> ByteString -> IO Bool
processTrailers conn stRef sid headerBlock = do
  decoder <- readMVar (connHpackDecoder conn)
  st <- readIORef stRef
  let tableSize = fromIntegral (settingsHeaderTableSize (ssLocalSettings st))
  result <- decodeHeaderBlockWithMaxSize decoder tableSize headerBlock
  case result of
    Left _ -> do
      connError conn stRef CompressionError
      pure False
    Right headers -> do
      let hasPseudo = any (\(k, _) -> BS.isPrefixOf ":" k) headers
      if hasPseudo
        then do
          streamError conn sid ProtocolError
          modifyIORef' stRef $ \s -> s
            { ssStreams = Map.insert sid (StreamInfo StClosed 0 Nothing 0) (ssStreams s)
            }
          pure True
        else do
          modifyIORef' stRef $ \s -> s
            { ssStreams = Map.adjust (\si -> si { siState = StHalfClosedRemote }) sid (ssStreams s)
            }
          queueResponse conn stRef sid
          pure True

queueResponse :: Connection -> IORef ServerState -> StreamId -> IO ()
queueResponse conn stRef sid = do
  encoder <- readMVar (connHpackEncoder conn)
  let responseBody = "ok"
      bodyLen = BS.length responseBody
      headers = [(":status", "200"), ("content-type", "text/plain"),
                 ("content-length", BS.pack (map (fromIntegral . fromEnum) (show bodyLen)))]
  headerBlock <- encodeHeaderBlock defaultEncodeStrategy encoder headers
  -- Send HEADERS immediately (not flow-controlled)
  let headersFrame = Frame
        (FrameHeader (fromIntegral (BS.length headerBlock)) FrameHeaders
          flagEndHeaders sid)
        (HeadersFrame Nothing headerBlock)
  sendFrame conn headersFrame
  -- Queue DATA for flow-controlled sending
  modifyIORef' stRef $ \s -> s
    { ssPendingSends = ssPendingSends s <> [PendingData sid responseBody True]
    , ssStreams = Map.adjust (\si -> si { siState = StHalfClosedLocal }) sid (ssStreams s)
    }

connError :: Connection -> IORef ServerState -> ErrorCode -> IO ()
connError conn stRef code = do
  st <- readIORef stRef
  if ssGoAwaySent st
    then pure ()
    else do
      writeIORef stRef st { ssGoAwaySent = True }
      closeConnection conn code ""

streamError :: Connection -> StreamId -> ErrorCode -> IO ()
streamError conn sid code = do
  let rst = Frame
        (FrameHeader 4 FrameRSTStream 0 sid)
        (RSTStreamFrame code)
  sendFrame conn rst

checkStreamWindowOverflow :: Connection -> IORef ServerState -> StreamId -> Word32 -> IO Bool
checkStreamWindowOverflow conn stRef sid increment = do
  st <- readIORef stRef
  let mInfo = Map.lookup sid (ssStreams st)
      currentWindow = maybe (fromIntegral (settingsInitialWindowSize (ssRemoteSettings st))) siSendWindow mInfo
      newWindow = currentWindow + fromIntegral increment
  if newWindow > 2147483647
    then do
      streamError conn sid FlowControlError
      pure True
    else do
      modifyIORef' stRef $ \s -> s
        { ssStreams = Map.alter (Just . maybe (StreamInfo StOpen newWindow Nothing 0)
            (\i -> i { siSendWindow = newWindow })) sid (ssStreams s)
        }
      pure True

getStreamState :: ServerState -> StreamId -> StreamSt
getStreamState st sid = case Map.lookup sid (ssStreams st) of
  Just info -> siState info
  Nothing
    | sid <= ssLastStreamId st -> StClosed
    | otherwise -> StIdle

validateRequestHeaders :: [(ByteString, ByteString)] -> Either ByteString ()
validateRequestHeaders headers = do
  let (pseudos, regulars) = span (\(k, _) -> BS.isPrefixOf ":" k) headers
  let remainingPseudos = filter (\(k, _) -> BS.isPrefixOf ":" k) regulars
  if not (null remainingPseudos)
    then Left "pseudo-header after regular header"
    else pure ()
  let pseudoNames = map fst pseudos
  let hasMethod = ":method" `elem` pseudoNames
      hasScheme = ":scheme" `elem` pseudoNames
      hasPath = ":path" `elem` pseudoNames
  if not hasMethod then Left "missing :method"
  else if not hasScheme then Left "missing :scheme"
  else if not hasPath then Left "missing :path"
  else pure ()
  let checkDuplicates [] = Right ()
      checkDuplicates (x:xs)
        | x `elem` xs = Left "duplicated pseudo-header"
        | otherwise = checkDuplicates xs
  checkDuplicates pseudoNames
  case lookup ":path" pseudos of
    Just p | BS.null p -> Left "empty :path"
    _ -> pure ()
  let validPseudos = [":method", ":scheme", ":path", ":authority"]
  let unknownPseudos = filter (\n -> n `notElem` validPseudos) pseudoNames
  if not (null unknownPseudos)
    then Left "unknown pseudo-header"
    else pure ()
  if ":status" `elem` pseudoNames
    then Left "response pseudo-header in request"
    else pure ()
  let hasUppercase bs = BS.any (\w -> w >= 0x41 && w <= 0x5A) bs
  let uppercaseHeaders = filter (\(k, _) -> hasUppercase k) (pseudos <> regulars)
  if not (null uppercaseHeaders)
    then Left "uppercase header name"
    else pure ()
  let connectionHeaders = ["connection", "keep-alive", "proxy-connection", "transfer-encoding", "upgrade"]
  let hasConnectionHeader = any (\(k, _) -> k `elem` connectionHeaders) regulars
  if hasConnectionHeader
    then Left "connection-specific header"
    else pure ()
  case lookup "te" regulars of
    Just v | v /= "trailers" -> Left "TE with value other than trailers"
    _ -> pure ()
  pure ()

validateAndApplySettings :: Settings -> [(Word16, Word32)] -> Either ErrorCode Settings
validateAndApplySettings = go
  where
    go s [] = Right s
    go s ((ident, val):rest) = case ident of
      0x1 -> go s { settingsHeaderTableSize = val } rest
      0x2 -> if val > 1
               then Left ProtocolError
               else go s { settingsEnablePush = val /= 0 } rest
      0x3 -> go s { settingsMaxConcurrentStreams = Just val } rest
      0x4 -> if val > 2147483647
               then Left FlowControlError
               else go s { settingsInitialWindowSize = val } rest
      0x5 -> if val < 16384 || val > 16777215
               then Left ProtocolError
               else go s { settingsMaxFrameSize = val } rest
      0x6 -> go s { settingsMaxHeaderListSize = Just val } rest
      _   -> go s rest

parseDecimal :: ByteString -> Maybe Int
parseDecimal bs
  | BS.null bs = Nothing
  | otherwise = go 0 0
  where
    len = BS.length bs
    go !acc !i
      | i >= len = Just acc
      | otherwise =
          let b = BS.index bs i
          in if b >= 0x30 && b <= 0x39
               then go (acc * 10 + fromIntegral (b - 0x30)) (i + 1)
               else Nothing

recvExact :: Socket -> Int -> IO ByteString
recvExact sock n = go n []
  where
    go 0 acc = pure (BS.concat (reverse acc))
    go remaining acc = do
      chunk <- NBS.recv sock (min remaining 4096)
      if BS.null chunk
        then pure (BS.concat (reverse acc))
        else go (remaining - BS.length chunk) (chunk : acc)
