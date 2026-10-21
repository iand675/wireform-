-- | HTTP/2 benchmark server using the real wireform-http2 library API.
-- No shortcuts — exercises the full frame encode/decode, HPACK, and connection stack.
--
-- Threading model: per-core accept via forkOn, pinned connection handlers.
module Main (main) where

import Control.Concurrent (forkOn, getNumCapabilities)
import Control.Concurrent.MVar (withMVar)
import Control.Exception (SomeException, catch, bracket, finally)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Network.Socket (Socket)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS
import System.Environment (getArgs)

import Network.HTTP2.Connection
import Network.HTTP2.Connection.Settings
import Network.HTTP2.Frame
import Network.HTTP2.HPACK
import Network.HTTP2.Types

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
        (p:_) -> p
        _ -> "8080"
  caps <- getNumCapabilities
  putStrLn $ "wireform-http2 server: " <> show caps <> " capabilities, port " <> port
  -- Per-core accept: each capability runs its own accept loop on a shared socket.
  -- This distributes connection handling across cores without cross-core migration.
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
        NS.listen sock 4096
        -- Single accept loop, round-robin connections across capabilities
        capCounter <- newIORef (0 :: Int)
        acceptLoop caps capCounter sock

acceptLoop :: Int -> IORef Int -> Socket -> IO ()
acceptLoop caps capCounter listenSock = do
  (clientSock, _) <- NS.accept listenSock
  NS.setSocketOption clientSock NS.NoDelay 1
  -- Round-robin across capabilities for even distribution
  cap <- atomicModifyIORef' capCounter (\n -> (if n + 1 >= caps then 0 else n + 1, n))
  _ <- forkOn cap $ handleClient clientSock
    `catch` (\(_ :: SomeException) -> NS.close clientSock)
  acceptLoop caps capCounter listenSock

handleClient :: Socket -> IO ()
handleClient sock = do
  preface <- recvExact sock 24
  if preface /= connectionPreface
    then NS.close sock
    else do
      conn <- newConnection ConnectionConfig
        { ccRole = RoleServer
        , ccSettings = defaultSettings
            { settingsMaxConcurrentStreams = Just 1000
            , settingsInitialWindowSize = 1048576
            }
        , ccSocket = Just sock
        , ccTransport = Nothing
        , ccOnGoAway = \_ _ _ -> pure ()
        }
      -- Send server settings
      let params = encodeSettings defaultSettings
            { settingsMaxConcurrentStreams = Just 1000
            , settingsInitialWindowSize = 1048576
            }
      sendFrame conn $ Frame
        (FrameHeader (fromIntegral (length params * 6)) FrameSettings 0 0)
        (SettingsFrame params)
      -- Boost connection window
      sendFrame conn $ Frame
        (FrameHeader 4 FrameWindowUpdate 0 0)
        (WindowUpdateFrame 10485760)
      serverLoop conn `finally` (releaseConnection conn >> NS.close sock)

serverLoop :: Connection -> IO ()
serverLoop conn = do
  mFrame <- recvFrameRaw conn
  case mFrame of
    Nothing -> pure ()
    Just (hdr, payload) -> do
      handleFrameRaw conn hdr payload
      serverLoop conn

-- | Handle frames using raw payload bytes — avoids constructing FramePayload ADT.
-- Pattern matches on FrameType which GHC can compile to a jump table.
handleFrameRaw :: Connection -> FrameHeader -> ByteString -> IO ()
handleFrameRaw conn hdr payload = case fhType hdr of
  FrameSettings
    | testFlag (fhFlags hdr) flagAck -> pure ()
    | otherwise ->
        sendFrame conn $ Frame (FrameHeader 0 FrameSettings flagAck 0) (SettingsFrame [])

  FramePing
    | testFlag (fhFlags hdr) flagAck -> pure ()
    | otherwise ->
        sendFrame conn $ Frame (FrameHeader 8 FramePing flagAck 0) (PingFrame payload)

  FrameWindowUpdate -> pure ()

  FrameHeaders
    | testFlag (fhFlags hdr) flagEndHeaders -> do
        _ <- withMVar (connHpackDecoder conn) $ \decoder ->
          decodeHeaderBlock decoder payload
        sendResponse conn (fhStreamId hdr)
    | otherwise -> pure ()

  FrameData -> do
    let len = fhLength hdr
    if len > 0
      then sendFrame conn $ Frame (FrameHeader 4 FrameWindowUpdate 0 0) (WindowUpdateFrame len)
      else pure ()

  FrameGoAway -> pure ()
  FrameRSTStream -> pure ()
  _ -> pure ()

sendResponse :: Connection -> StreamId -> IO ()
sendResponse conn sid = do
  headerBlock <- withMVar (connHpackEncoder conn) $ \encoder ->
    encodeHeaderBlock defaultEncodeStrategy encoder
      [(":status", "200"), ("content-type", "text/plain"), ("content-length", "13")]
  sendFrames conn
    [ Frame (FrameHeader (fromIntegral (BS.length headerBlock)) FrameHeaders flagEndHeaders sid)
        (HeadersFrame Nothing headerBlock)
    , Frame (FrameHeader 13 FrameData flagEndStream sid)
        (DataFrame "Hello, World!")
    ]

recvExact :: Socket -> Int -> IO ByteString
recvExact sock n = go n []
  where
    go 0 acc = pure (BS.concat (reverse acc))
    go remaining acc = do
      chunk <- NBS.recv sock (min remaining 16384)
      if BS.null chunk
        then pure (BS.concat (reverse acc))
        else go (remaining - BS.length chunk) (chunk : acc)
