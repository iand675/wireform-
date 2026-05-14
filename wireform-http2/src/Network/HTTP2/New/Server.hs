{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- | HTTP/2 server entry point.
module Network.HTTP2.New.Server
    ( ServerConfig(..)
    , defaultServerConfig
    , run
    , runWith
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (bracket, handle, catch, SomeException)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.IntMap.Strict as IntMap
import Foreign.Marshal.Alloc (mallocBytes, free)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import System.IO (hPutStrLn, stderr)

import Network.HTTP2.Frame
import qualified Network.HPACK as HPACK

import Network.HTTP2.New.HPACK
import Network.HTTP2.New.Receiver (frameReceiver)
import Network.HTTP2.New.Sender (frameSender)
import Network.HTTP2.New.Types

----------------------------------------------------------------

data ServerConfig = ServerConfig
    { serverBufferSize     :: !Int
    , serverInitWindowSize :: !WindowSize
    , serverMaxConcurrent  :: !Int
    , serverHpackTableSize :: !Int
    }

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
    { serverBufferSize     = 32768
    , serverInitWindowSize = 65535
    , serverMaxConcurrent  = 100
    , serverHpackTableSize = 4096
    }

----------------------------------------------------------------

-- | Accept and handle connections in a loop.
run :: ServerConfig -> Socket -> (Request -> IO Response) -> IO ()
run scfg listenSock handler = loop
  where
    loop = do
        (conn, peerAddr) <- accept listenSock
        myAddr <- getSocketName conn
        _ <- forkIO $
            handle (\(_ :: SomeException) -> drainAndClose conn) $ do
                runWith scfg conn myAddr peerAddr handler
                drainAndClose conn
        loop

-- | Gracefully close a socket:
-- 1. Shutdown the send direction (sends FIN to peer).
-- 2. Drain any remaining incoming data until peer closes.
-- 3. Close the socket.
-- This avoids TCP RST, which happens when close() is called with unread
-- data in the receive buffer.
drainAndClose :: Socket -> IO ()
drainAndClose sock = do
    -- FIN: signal we are done sending.
    shutdown sock ShutdownSend `catch` (\(_ :: SomeException) -> return ())
    -- Drain until peer closes (EOF) or error.
    let drain = do
            bs <- recv sock 4096
            unless (BS.null bs) drain
    drain `catch` (\(_ :: SomeException) -> return ())
    close sock

-- | Handle a single accepted connection.
runWith
    :: ServerConfig
    -> Socket
    -> SockAddr
    -> SockAddr
    -> (Request -> IO Response)
    -> IO ()
runWith ServerConfig{..} sock myAddr peerAddr handler =
    bracket (mallocBytes serverBufferSize) free $ \writeBuf -> do

        -- 1. Read exactly 24 bytes of the client connection preface.
        preface <- recvExact sock 24
        if preface /= http2Preface
            then return ()   -- not HTTP/2; close silently
            else do
                -- 2. Build the shared Config and Context.
                let cfg = Config
                        { cfgWriteBuffer  = writeBuf
                        , cfgBufferSize   = serverBufferSize
                        , cfgSendAll      = sendAll sock
                        , cfgReadN        = recvExact sock
                        , cfgMySockAddr   = myAddr
                        , cfgPeerSockAddr = peerAddr
                        }

                ctx <- newContext myAddr peerAddr serverInitWindowSize serverHpackTableSize
                            serverMaxConcurrent

                -- 3. Enqueue our initial SETTINGS (sent by the sender thread).
                let initSettings =
                        [ (SettingsInitialWindowSize,   serverInitWindowSize)
                        , (SettingsMaxConcurrentStreams, serverMaxConcurrent)
                        , (SettingsMaxFrameSize,         serverBufferSize)
                        ]
                atomically $ writeTQueue (ctxControlQ ctx)
                    (OControl (CSettings initSettings))

                -- 4. Sender thread: drains the output queues and writes to socket.
                _ <- forkIO (frameSender cfg ctx)

                -- 5. Receiver loop (this thread): reads frames, dispatches.
                frameReceiver cfg ctx handler

----------------------------------------------------------------
-- Context construction

newContext
    :: SockAddr
    -> SockAddr
    -> WindowSize
    -> Int          -- ^ hpack table size
    -> Int          -- ^ max concurrent streams
    -> IO Context
newContext myAddr peerAddr initWin hpackSz maxConc = do
    connTxWin    <- newTxWindow (65535 * 256)
    connRxWin    <- newIORef 0
    enc          <- newEncoder hpackSz
    dec          <- newDecoder hpackSz
    hpackLock    <- newMVar ()
    streams       <- newIORef IntMap.empty
    closedStreams  <- newIORef IntMap.empty
    nextStreamId  <- newIORef 1
    peerSettings <- newIORef defaultSettings
    outputQ      <- newTQueueIO
    controlQ     <- newTQueueIO
    return Context
        { ctxRole         = Server
        , ctxConnTxWin    = connTxWin
        , ctxConnRxWin    = connRxWin
        , ctxHpackEnc     = enc
        , ctxHpackDec     = dec
        , ctxHpackLock    = hpackLock
        , ctxStreams       = streams
        , ctxClosedStreams = closedStreams
        , ctxNextStreamId = nextStreamId
        , ctxPeerSettings = peerSettings
        , ctxMySettings   = serverMySettings initWin maxConc
        , ctxOutputQ      = outputQ
        , ctxControlQ     = controlQ
        , ctxMySockAddr   = myAddr
        , ctxPeerSockAddr = peerAddr
        }

serverMySettings :: WindowSize -> Int -> Settings
serverMySettings win maxConc = defaultSettings
    { initialWindowSize    = win
    , maxConcurrentStreams = Just maxConc
    }

----------------------------------------------------------------
-- Socket helpers

-- | Read exactly @n@ bytes, blocking until all arrive.
recvExact :: Socket -> Int -> IO ByteString
recvExact _    0 = return BS.empty
recvExact sock n = go n []
  where
    go 0   acc = return $! BS.concat (reverse acc)
    go rem acc = do
        chunk <- recv sock (min rem 4096)
        if BS.null chunk
            then ioError (userError "connection closed by peer")
            else go (rem - BS.length chunk) (chunk : acc)

-- | The 24-byte HTTP/2 client connection preface (RFC 9113 §3.4).
http2Preface :: ByteString
http2Preface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
