{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | gRPC-friendly HTTP\/2 client engine.
--
-- The @http-semantics@-shaped client API in wireform-http2's
-- namespace. The runtime ('run') is currently a stub; see the
-- "TODO" note on 'run' below.
module Network.HTTP2.Engine.Client
  ( -- * The client callback
    Client
  , SendRequest
    -- * Request
  , Request (..)
  , requestNoBody
  , requestFile
  , requestStreaming
  , requestStreamingUnmask
  , requestBuilder
  , requestStreamingIface
  , setRequestTrailersMaker
    -- * Response
  , Response (..)
  , responseStatus
  , responseHeaders
  , responseBodySize
  , getResponseBodyChunk
  , getResponseBodyChunk'
  , getResponseTrailers
    -- * Auxiliary information
  , Aux (..)
    -- * Streaming body interface
  , OutBodyIface (..)
    -- * Trailers
  , TrailersMaker
  , NextTrailersMaker (..)
  , defaultTrailersMaker
    -- * Settings
  , ClientConfig (..)
  , defaultClientConfig
  , Settings (..)
  , defaultSettings
  , Config (..)
    -- * Type aliases
  , Path
  , Authority
  , Scheme
  , FileSpec (..)
  , FileOffset
  , ByteCount
  , BufferSize
    -- * Simple config helpers
  , allocSimpleConfig
  , allocConfigForTransport
  , freeSimpleConfig
    -- * Engine entry point
  , run
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (readIORef)
import Data.Word (Word32)
import Network.HTTP.Header (RequestHeaders)
import Network.HTTP.Status (StatusCode (..))
import Network.Socket (Socket)
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as NBS
import qualified System.TimeManager as TM

import Network.HTTP2.Transport (SendFn)
import qualified Network.HTTP2.Engine.Run.Client as RunClient
import Network.HTTP2.Engine.Types

-- | Per-call send hook. The continuation is invoked with the received
-- 'Response' once headers have arrived.
type SendRequest = forall r. Request -> (Response -> IO r) -> IO r

-- | The client callback: given a 'SendRequest' and 'Aux', produce a result.
type Client a = SendRequest -> Aux -> IO a

-- | Per-client auxiliary information.
data Aux = Aux
  { auxPossibleClientStreams :: !(IO Int)
  }

newtype Request = Request OutObj
  deriving stock (Show)

newtype Response = Response InpObj
  deriving stock (Show)

addHeaders :: ByteString -> Path -> RequestHeaders -> RequestHeaders
addHeaders m p hs = (":method", m) : (":path", p) : hs

requestNoBody :: ByteString -> Path -> RequestHeaders -> Request
requestNoBody m p hdr =
  Request (OutObj (addHeaders m p hdr) OutBodyNone defaultTrailersMaker)

requestFile :: ByteString -> Path -> RequestHeaders -> FileSpec -> Request
requestFile m p hdr fs =
  Request (OutObj (addHeaders m p hdr) (OutBodyFile fs) defaultTrailersMaker)

requestBuilder :: ByteString -> Path -> RequestHeaders -> BSB.Builder -> Request
requestBuilder m p hdr b =
  Request (OutObj (addHeaders m p hdr) (OutBodyBuilder b) defaultTrailersMaker)

requestStreaming
  :: ByteString
  -> Path
  -> RequestHeaders
  -> ((BSB.Builder -> IO ()) -> IO () -> IO ())
  -> Request
requestStreaming m p hdr body =
  Request (OutObj (addHeaders m p hdr) (OutBodyStreaming body) defaultTrailersMaker)

requestStreamingUnmask
  :: ByteString
  -> Path
  -> RequestHeaders
  -> ((forall x. IO x -> IO x) -> (BSB.Builder -> IO ()) -> IO () -> IO ())
  -> Request
requestStreamingUnmask m p hdr body =
  requestStreamingIface m p hdr $ \iface ->
    body (outBodyUnmask iface) (outBodyPush iface) (outBodyFlush iface)

requestStreamingIface
  :: ByteString
  -> Path
  -> RequestHeaders
  -> (OutBodyIface -> IO ())
  -> Request
requestStreamingIface m p hdr body =
  Request (OutObj (addHeaders m p hdr) (OutBodyStreamingIface body) defaultTrailersMaker)

setRequestTrailersMaker :: Request -> TrailersMaker -> Request
setRequestTrailersMaker (Request o) tm = Request o { outObjTrailers = tm }

responseStatus :: Response -> Maybe StatusCode
responseStatus (Response r) = do
  bs <- lookupToken ":status" (inpObjHeaders r)
  case reads (BS8.unpack bs) of
    [(n, "")] -> Just (StatusCode (fromIntegral (n :: Int)))
    _         -> Nothing

responseHeaders :: Response -> TokenHeaderTable
responseHeaders (Response r) = inpObjHeaders r

responseBodySize :: Response -> Maybe Int
responseBodySize (Response r) = inpObjBodySize r

getResponseBodyChunk :: Response -> IO ByteString
getResponseBodyChunk = fmap fst . getResponseBodyChunk'

getResponseBodyChunk' :: Response -> IO (ByteString, Bool)
getResponseBodyChunk' (Response r) = inpObjBody r

getResponseTrailers :: Response -> IO (Maybe TokenHeaderTable)
getResponseTrailers (Response r) = readIORef (inpObjTrailers r)

data ClientConfig = ClientConfig
  { authority :: !Authority
  , settings :: !Settings
  , connectionWindowSize :: !Int
  }

defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig
  { authority = "localhost"
  , settings = defaultSettings
  , connectionWindowSize = 16777216
  }

data Settings = Settings
  { headerTableSize :: !Int
  , enablePush :: !Bool
  , maxConcurrentStreams :: !(Maybe Word32)
  , initialWindowSize :: !Int
  , maxFrameSize :: !Int
  , maxHeaderListSize :: !(Maybe Word32)
  , pingRateLimit :: !Int
  , emptyFrameRateLimit :: !Int
  , settingsRateLimit :: !Int
  , rstRateLimit :: !Int
  }
  deriving stock (Eq, Show)

defaultSettings :: Settings
defaultSettings = Settings
  { headerTableSize = 4096
  , enablePush = True
  , maxConcurrentStreams = Just 64
  , initialWindowSize = 262144
  , maxFrameSize = 16384
  , maxHeaderListSize = Nothing
  , pingRateLimit = 10
  , emptyFrameRateLimit = 4
  , settingsRateLimit = 4
  , rstRateLimit = 4
  }

data Config = Config
  { confSendFn :: !SendFn
  , confReadN :: !(Int -> IO ByteString)
  , confPositionReadMaker :: !PositionReadMaker
  , confTimeoutManager :: !TM.Manager
  }

-- | Allocate a 'Config' that talks to the given 'Socket'.
--
-- A minimal time manager is created internally; 'freeSimpleConfig'
-- kills it. The position-read maker is the placeholder
-- 'defaultPositionReadMaker' since gRPC doesn't serve files.
allocSimpleConfig :: Socket -> BufferSize -> IO Config
allocSimpleConfig sock _bufSize = do
  mgr <- TM.initialize (30 * 1000 * 1000)  -- 30s default
  pure Config
    { confSendFn = S.sendBuf sock
    , confReadN = recvExactN sock
    , confPositionReadMaker = defaultPositionReadMaker
    , confTimeoutManager = mgr
    }

-- | Allocate a 'Config' from raw transport callbacks rather than a 'Socket'.
-- The @readN@ callback MUST return exactly the requested number of bytes
-- (blocking until they arrive), returning fewer only at end-of-input — the
-- same contract as 'recvExactN'. Used to run the engine (and thus gRPC /
-- Connect) over any byte transport, e.g. the @wireform-dst-net@ fault link.
-- 'freeSimpleConfig' releases the internally-created time manager.
allocConfigForTransport :: SendFn -> (Int -> IO ByteString) -> IO Config
allocConfigForTransport sendFn readN = do
  mgr <- TM.initialize (30 * 1000 * 1000)
  pure Config
    { confSendFn = sendFn
    , confReadN = readN
    , confPositionReadMaker = defaultPositionReadMaker
    , confTimeoutManager = mgr
    }

recvExactN :: Socket -> Int -> IO ByteString
recvExactN sock n = go n []
  where
    go 0 acc = pure (BS.concat (reverse acc))
    go remaining acc = do
      chunk <- NBS.recv sock (min remaining 65536)
      if BS.null chunk
        then pure (BS.concat (reverse acc))
        else go (remaining - BS.length chunk) (chunk : acc)

freeSimpleConfig :: Config -> IO ()
freeSimpleConfig cfg =
  TM.killManager (confTimeoutManager cfg)

-- | Run an HTTP\/2 client over the supplied I\/O plumbing.
--
-- Handles the gRPC happy path: HEADERS + DATA + trailer dispatch,
-- per-stream send/recv queues, half-close in both directions.
run :: ClientConfig -> Config -> Client a -> IO a
run cc cfg client =
  RunClient.runClient
    RunClient.RunEnv
      { RunClient.envAuthority = authority cc
      , RunClient.envSendFn = confSendFn cfg
      , RunClient.envReadN = confReadN cfg
      , RunClient.envConnectionWindow = connectionWindowSize cc
      , RunClient.envInitialWindowSize = initialWindowSize (settings cc)
      , RunClient.envMaxConcurrentStreams = maxConcurrentStreams (settings cc)
      , RunClient.envScheme = "http"
      }
    (\sendRequest engineAux ->
       client (\(Request oo) k -> sendRequest oo (\inp -> k (Response inp)))
              (Aux { auxPossibleClientStreams = RunClient.engineAuxPossibleStreams engineAux }))
