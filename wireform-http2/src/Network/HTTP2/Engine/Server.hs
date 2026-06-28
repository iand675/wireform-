{-# LANGUAGE OverloadedStrings #-}
-- | gRPC-friendly HTTP\/2 server engine.
--
-- This module exposes an @http-semantics@-shaped API in the
-- wireform-http2 namespace so wireform-grpc can use it without
-- depending on the upstream @http-semantics@, @http2@, or
-- @http2-tls@ packages.
--
-- The runtime ('run') lives in "Network.HTTP2.Engine.Run.Server"; this
-- module owns the public types and accessors.
module Network.HTTP2.Engine.Server
  ( -- * The server callback
    Server
    -- * Request
  , Request (..)
  , requestMethod
  , requestPath
  , requestAuthority
  , requestScheme
  , requestHeaders
  , requestBodySize
  , getRequestBodyChunk
  , getRequestBodyChunk'
  , getRequestTrailers
    -- * Response
  , Response (..)
  , responseNoBody
  , responseFile
  , responseBuilder
  , responseStreaming
  , responseStreamingIface
  , responseBodySize
  , setResponseTrailersMaker
    -- * Trailers (re-exported from Types)
  , TrailersMaker
  , NextTrailersMaker (..)
  , defaultTrailersMaker
    -- * Auxiliary information
  , Aux (..)
    -- * Push promises (placeholder; never used by gRPC)
  , PushPromise (..)
  , pushPromise
    -- * Streaming body interface
  , OutBodyIface (..)
    -- * Settings / config
  , module Network.HTTP2.Engine.Settings
  , Config (..)
    -- * Type aliases
  , Path
  , Authority
  , Scheme
  , FileSpec (..)
  , FileOffset
  , ByteCount
  , BufferSize
  , PositionReadMaker
  , PositionRead
  , Sentinel (..)
  , defaultPositionReadMaker
    -- * Engine entry point
  , run
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.UTF8 as UTF8
import Data.IORef (readIORef)
import Network.HTTP.Header (ResponseHeaders)
import Network.HTTP.Status (StatusCode, statusCode)
import Network.Socket (SockAddr)
import qualified System.TimeManager as TM

import Network.HTTP2.Transport (SendFn)
import qualified Network.HTTP2.Engine.Run.Server as RunServer
import Network.HTTP2.Engine.Settings
import Network.HTTP2.Engine.Types

-- | The handler signature: receive 'Request' + 'Aux', call the
-- continuation with a 'Response' + any 'PushPromise's (always @[]@
-- for gRPC). The continuation must be called exactly once.
type Server = Request -> Aux -> (Response -> [PushPromise] -> IO ()) -> IO ()

-- | Per-request auxiliary information attached at handler invocation.
data Aux = Aux
  { auxTimeHandle :: !TM.Handle
  , auxMySockAddr :: !SockAddr
  , auxPeerSockAddr :: !SockAddr
  }

newtype Request = Request InpObj
  deriving stock (Show)

newtype Response = Response OutObj
  deriving stock (Show)

requestMethod :: Request -> Maybe ByteString
requestMethod (Request req) = lookupToken ":method" (inpObjHeaders req)

requestPath :: Request -> Maybe Path
requestPath (Request req) = lookupToken ":path" (inpObjHeaders req)

requestAuthority :: Request -> Maybe Authority
requestAuthority (Request req) =
  UTF8.toString <$> lookupToken ":authority" (inpObjHeaders req)

requestScheme :: Request -> Maybe Scheme
requestScheme (Request req) = lookupToken ":scheme" (inpObjHeaders req)

requestHeaders :: Request -> TokenHeaderTable
requestHeaders (Request req) = inpObjHeaders req

requestBodySize :: Request -> Maybe Int
requestBodySize (Request req) = inpObjBodySize req

getRequestBodyChunk :: Request -> IO ByteString
getRequestBodyChunk = fmap fst . getRequestBodyChunk'

getRequestBodyChunk' :: Request -> IO (ByteString, Bool)
getRequestBodyChunk' (Request req) = inpObjBody req

getRequestTrailers :: Request -> IO (Maybe TokenHeaderTable)
getRequestTrailers (Request req) = readIORef (inpObjTrailers req)

responseNoBody :: StatusCode -> ResponseHeaders -> Response
responseNoBody st hdr =
  Response (OutObj (setStatus st hdr) OutBodyNone defaultTrailersMaker)

responseFile :: StatusCode -> ResponseHeaders -> FileSpec -> Response
responseFile st hdr fs =
  Response (OutObj (setStatus st hdr) (OutBodyFile fs) defaultTrailersMaker)

responseBuilder :: StatusCode -> ResponseHeaders -> BSB.Builder -> Response
responseBuilder st hdr b =
  Response (OutObj (setStatus st hdr) (OutBodyBuilder b) defaultTrailersMaker)

responseStreaming
  :: StatusCode
  -> ResponseHeaders
  -> ((BSB.Builder -> IO ()) -> IO () -> IO ())
  -> Response
responseStreaming st hdr body =
  Response (OutObj (setStatus st hdr) (OutBodyStreaming body) defaultTrailersMaker)

responseStreamingIface
  :: StatusCode
  -> ResponseHeaders
  -> (OutBodyIface -> IO ())
  -> Response
responseStreamingIface st hdr body =
  Response (OutObj (setStatus st hdr) (OutBodyStreamingIface body) defaultTrailersMaker)

responseBodySize :: Response -> Maybe Int
responseBodySize (Response (OutObj _ (OutBodyFile (FileSpec _ _ len)) _)) =
  Just (fromIntegral len)
responseBodySize _ = Nothing

setResponseTrailersMaker :: Response -> TrailersMaker -> Response
setResponseTrailersMaker (Response o) tm = Response o { outObjTrailers = tm }

setStatus :: StatusCode -> ResponseHeaders -> ResponseHeaders
setStatus st hs = (":status", statusBS) : hs
  where statusBS = UTF8.fromString (show (statusCode st))

data PushPromise = PushPromise
  { promiseRequestPath :: !ByteString
  , promiseResponse :: !Response
  }

pushPromise :: ByteString -> Response -> Int -> PushPromise
pushPromise path r _ = PushPromise path r

-- | HTTP\/2 server-side per-connection plumbing.
data Config = Config
  { confSendFn :: !SendFn
  , confReadN :: !(Int -> IO ByteString)
  , confPositionReadMaker :: !PositionReadMaker
  , confTimeoutManager :: !TM.Manager
  , confMySockAddr :: !SockAddr
  , confPeerSockAddr :: !SockAddr
  }

-- | Run an HTTP\/2 server over the supplied I\/O plumbing.
--
-- Reads frames from @confReadN@, dispatches HEADERS to fresh
-- per-stream worker threads that invoke @server@, multiplexes
-- response frames out via @confSendAll@, and runs until the
-- connection drops or the peer sends GOAWAY.
--
-- The implementation handles the subset of RFC 9113 gRPC relies on:
-- HEADERS + DATA + trailing HEADERS, half-close in both directions,
-- per-stream input/output queues, RST_STREAM cancellation, SETTINGS
-- and PING bookkeeping. Things like server push (PUSH_PROMISE),
-- responseFile, and the deprecated 'numberOfWorkers' thread pool are
-- intentionally absent.
run :: ServerConfig -> Config -> Server -> IO ()
run sc cfg server =
  RunServer.runServer
    RunServer.RunEnv
      { RunServer.envSettings = settings sc
      , RunServer.envConnectionWindow = connectionWindowSize sc
      , RunServer.envSendFn = confSendFn cfg
      , RunServer.envReadN = confReadN cfg
      , RunServer.envTimeoutManager = confTimeoutManager cfg
      , RunServer.envMySockAddr = confMySockAddr cfg
      , RunServer.envPeerSockAddr = confPeerSockAddr cfg
      }
    (\envReq aux respond ->
       server (Request envReq) (engineAuxToAux aux) (\(Response oo) ps ->
         respond oo (map promiseToEngine ps)))
  where
    engineAuxToAux :: RunServer.EngineAux -> Aux
    engineAuxToAux a = Aux
      { auxTimeHandle = RunServer.envAuxTimeHandle a
      , auxMySockAddr = RunServer.envAuxMySockAddr a
      , auxPeerSockAddr = RunServer.envAuxPeerSockAddr a
      }

    promiseToEngine :: PushPromise -> RunServer.EnginePushPromise
    promiseToEngine (PushPromise p (Response oo)) =
      RunServer.EnginePushPromise p oo
