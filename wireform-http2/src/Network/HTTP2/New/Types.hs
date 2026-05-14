{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
module Network.HTTP2.New.Types where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Network.Socket (SockAddr)

import qualified Network.HPACK as HPACK
import Network.HTTP2.Frame (StreamId, WindowSize, ErrorCode(..), SettingsList)

----------------------------------------------------------------
-- Basic HTTP/2 primitives

----------------------------------------------------------------
-- Flow control: IORef for the counter, MVar for blocking.
-- When txAvailable drops to ≤ 0 the sender calls takeMVar txSig.
-- WINDOW_UPDATE increments txAvailable and signals txSig.

-- | TX flow-control window backed by STM.
-- STM guarantees atomicity and correct blocking/signalling in all scenarios,
-- including multi-increment updates (e.g. 0→1→5 via two WINDOW_UPDATEs).
data TxWindow = TxWindow
    { txAvailable :: !(TVar Int)
    }

newTxWindow :: WindowSize -> IO TxWindow
newTxWindow n = TxWindow <$> newTVarIO n

-- | Consume @n@ bytes from the window.  Blocks (via STM retry) until
-- the window holds at least @n@ bytes.
waitTxWindow :: TxWindow -> Int -> IO ()
waitTxWindow TxWindow{..} n = atomically $ do
    avail <- readTVar txAvailable
    check (avail >= n)
    writeTVar txAvailable (avail - n)

-- | Add @n@ bytes to the window (from a WINDOW_UPDATE frame).
addTxWindow :: TxWindow -> Int -> IO ()
addTxWindow TxWindow{..} n =
    atomically $ modifyTVar' txAvailable (+ n)

-- | Adjust the TX window by @delta@ (may be negative) for
-- SETTINGS_INITIAL_WINDOW_SIZE changes per RFC 9113 §6.9.2.
adjustStreamTxWindow :: TxWindow -> Int -> IO ()
adjustStreamTxWindow TxWindow{..} delta =
    atomically $ modifyTVar' txAvailable (+ delta)

----------------------------------------------------------------
-- Stream state: two independent booleans.
-- This avoids the complex ADT of the original library where
-- open/half-closed/closed state is a single enum that must be
-- transitioned atomically with body-queue operations.

data Stream = Stream
    { streamId     :: !StreamId
    , streamTxDone :: !(IORef Bool)   -- we are done sending
    , streamRxDone :: !(IORef Bool)   -- peer is done sending
    , streamBody   :: !BodyChannel
    , streamTxWin  :: !TxWindow       -- per-stream send window
    }

data BodyChannel
    = UnaryBody  !(MVar (Either SomeException ByteString))
      -- ^ Single-shot: receiver putMVars the full body; handler takeMVars it.
    | StreamBody !(MVar (Either SomeException (ByteString, Bool)))
      -- ^ Multi-shot: receiver putMVars chunks; handler loops until EOS.

newStream :: StreamId -> WindowSize -> IO Stream
newStream sid win =
    Stream sid
        <$> newIORef False
        <*> newIORef False
        <*> (UnaryBody <$> newEmptyMVar)
        <*> newTxWindow win

----------------------------------------------------------------
-- Response types

data Request = Request
    { reqStreamId  :: !StreamId
    , reqHeaders   :: !HPACK.TokenHeaderTable
    , reqBody      :: !(IO (ByteString, Bool))  -- pull-style body reader
    }

data Response
    = ResponseUnary
        { rspStatus  :: !Int
        , rspHeaders :: ![(ByteString, ByteString)]
        , rspBody    :: !ByteString
        , rspTrailers :: ![(ByteString, ByteString)]
        }
    | ResponseStreaming
        { rspStatus   :: !Int
        , rspHeaders  :: ![(ByteString, ByteString)]
        , rspProducer :: !(((ByteString, Bool) -> IO ()) -> IO ())
        , rspTrailers :: ![(ByteString, ByteString)]
        }

----------------------------------------------------------------
-- Output queue items.
-- OUnary is the key design win: HEADERS + DATA + trailing HEADERS
-- are processed in a single sender pass without re-enqueuing.

data Output
    = OUnary
        !Stream
        ![(ByteString, ByteString)]  -- response headers
        !ByteString                  -- response body
        ![(ByteString, ByteString)]  -- trailing headers
        !(IO ())                     -- completion callback
    | OStreaming
        !Stream
        ![(ByteString, ByteString)]  -- response headers
        !(((ByteString, Bool) -> IO ()) -> IO ())  -- body producer
        ![(ByteString, ByteString)]  -- trailing headers
        !(IO ())
    | OControl !ControlFrame

data ControlFrame
    = CSettings !SettingsList
    | CSettingsAck
    | CPing !ByteString       -- ping payload to echo
    | CGoAway !StreamId !ErrorCode !ByteString
    | CRstStream !StreamId !ErrorCode
    | CWindowUpdate !StreamId !WindowSize

----------------------------------------------------------------
-- Settings record (parsed form of HTTP/2 SETTINGS frames)

data Settings = Settings
    { headerTableSize      :: !Int
    , enablePush           :: !Bool
    , maxConcurrentStreams :: !(Maybe Int)
    , initialWindowSize    :: !WindowSize
    , maxFrameSize         :: !Int
    , maxHeaderListSize    :: !(Maybe Int)
    } deriving (Show, Eq)

defaultSettings :: Settings
defaultSettings = Settings
    { headerTableSize      = 4096
    , enablePush           = True
    , maxConcurrentStreams = Nothing
    , initialWindowSize    = 65535
    , maxFrameSize         = 16384
    , maxHeaderListSize    = Nothing
    }

----------------------------------------------------------------
-- Connection context

data Context = Context
    { ctxRole         :: !Role
    , ctxConnTxWin    :: !TxWindow
    , ctxConnRxWin    :: !(IORef WindowSize)   -- bytes we've consumed
    , ctxHpackEnc     :: !HPACK.DynamicTable
    , ctxHpackDec     :: !HPACK.DynamicTable
    , ctxHpackLock    :: !(MVar ())            -- serialise HPACK encode
    , ctxStreams      :: !(IORef (IntMap Stream))
    , ctxClosedStreams :: !(IORef (IntMap ()))
    -- ^ Streams that have been fully closed (half-closed-remote or closed).
    -- Used to send STREAM_CLOSED errors for frames on closed streams,
    -- rather than treating them as idle streams.
    , ctxNextStreamId :: !(IORef StreamId)     -- client: next odd ID
    , ctxPeerSettings :: !(IORef Settings)
    , ctxMySettings   :: !Settings
    , ctxOutputQ      :: !(TQueue Output)   -- data frames (OUnary, OStreaming)
    , ctxControlQ     :: !(TQueue Output)   -- control frames (higher priority)
    , ctxMySockAddr   :: !SockAddr
    , ctxPeerSockAddr :: !SockAddr
    }

data Role = Client | Server deriving (Eq, Show)

----------------------------------------------------------------
-- Config

data Config = Config
    { cfgWriteBuffer :: !HPACK.Buffer
    , cfgBufferSize  :: !HPACK.BufferSize
    , cfgSendAll     :: !(ByteString -> IO ())
    , cfgReadN       :: !(Int -> IO ByteString)
    , cfgMySockAddr  :: !SockAddr
    , cfgPeerSockAddr :: !SockAddr
    }

----------------------------------------------------------------
-- Errors

data HTTP2Error
    = ConnectionError !ErrorCode !ByteString
    | StreamError     !StreamId !ErrorCode !ByteString
    deriving (Show)
    deriving anyclass Exception

connError :: ErrorCode -> ByteString -> HTTP2Error
connError = ConnectionError

streamError :: StreamId -> ErrorCode -> ByteString -> HTTP2Error
streamError = StreamError
