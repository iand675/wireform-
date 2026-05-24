{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{- | Apples-to-apples benchmark: wireform-http1 vs Warp.

Two benchmark groups:

1. __Keep-alive__ — persistent connection, measures steady-state
   request\/response throughput. Both sides reuse connections.

2. __Connection churn__ — new TCP connection per iteration, measures
   connection setup cost. Compares: wireform without ring pool,
   wireform with ring pool, and Warp.
-}
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (bracket, finally)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (mk)
import qualified "http-types" Network.HTTP.Types as WAIHttp
import qualified "http-client" Network.HTTP.Client as HC
import qualified Network.Socket as NS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Criterion.Main

import qualified Network.HTTP.Message as U
import qualified "wireform-http" Network.HTTP.Types.Body as U
import qualified "wireform-http" Network.HTTP.Types.Header as U
import qualified "wireform-http" Network.HTTP.Types.Method as U
import qualified "wireform-http" Network.HTTP.Types.Version as U
import Network.HTTP.Connection
  (Connection, ConnectionConfig(..), defaultConnectionConfig,
   openConnection, closeConnection, sendOn, withConnection)
import Network.HTTP.Server
  (ServerConfig(..), defaultServerConfig, runServerOnListener)
import Network.HTTP.VersionRange (http1Only)
import Network.HTTP.WAI (waiToHandler)
import Wireform.Ring.Pool
  (RingPool, newRingPool, defaultRingPoolConfig)

main :: IO ()
main = do
  mgr <- HC.newManager HC.defaultManagerSettings
  -- Manager that doesn't reuse connections (for churn comparison)
  mgrNoReuse <- HC.newManager HC.defaultManagerSettings
    { HC.managerIdleConnectionCount = 0 }
  ringPool <- newRingPool defaultRingPoolConfig

  -- Keep-alive servers
  wfHello   <- setupWireform Nothing helloApp
  wfHeaders <- setupWireform Nothing headersApp
  warpHelloUrl   <- startWarp helloApp
  warpHeadersUrl <- startWarp headersApp

  -- Connection-churn servers (one without pool, one with pool)
  churnNoPoolPort <- startWireformServer Nothing helloApp
  churnPoolPort   <- startWireformServer (Just ringPool) helloApp
  warpChurnUrl    <- startWarp helloApp

  churnNoPoolPortH <- startWireformServer Nothing headersApp
  churnPoolPortH   <- startWireformServer (Just ringPool) headersApp
  warpChurnUrlH    <- startWarp headersApp

  defaultMain
    [ bgroup "keep-alive: hello (GET → 5 B)"
        [ bench "wireform-http1" $ nfIO (wfGet wfHello)
        , bench "warp"           $ nfIO (warpGet mgr warpHelloUrl)
        ]
    , bgroup "keep-alive: headers (GET → 20 hdrs)"
        [ bench "wireform-http1" $ nfIO (wfGet wfHeaders)
        , bench "warp"           $ nfIO (warpGet mgr warpHeadersUrl)
        ]
    , bgroup "connection churn: hello (GET → 5 B)"
        [ bench "wireform (no pool)" $ nfIO (churnGet churnNoPoolPort)
        , bench "wireform (pooled)"  $ nfIO (churnGet churnPoolPort)
        , bench "warp"               $ nfIO (warpGet mgrNoReuse warpChurnUrl)
        ]
    , bgroup "connection churn: headers (GET → 20 hdrs)"
        [ bench "wireform (no pool)" $ nfIO (churnGet churnNoPoolPortH)
        , bench "wireform (pooled)"  $ nfIO (churnGet churnPoolPortH)
        , bench "warp"               $ nfIO (warpGet mgrNoReuse warpChurnUrlH)
        ]
    ]

------------------------------------------------------------------------
-- Payloads
------------------------------------------------------------------------

payload1k :: BS.ByteString
payload1k = BS.replicate 1024 0x61

------------------------------------------------------------------------
-- WAI apps
------------------------------------------------------------------------

helloApp :: Wai.Application
helloApp _req respond =
  respond $ Wai.responseLBS WAIHttp.status200
    [("Content-Type", "text/plain")] "hello"

headersApp :: Wai.Application
headersApp _req respond =
  respond $ Wai.responseLBS WAIHttp.status200 prebuiltHeaders "ok"

prebuiltHeaders :: [(CI.CI BS.ByteString, BS.ByteString)]
prebuiltHeaders =
  [ (mk (BS8.pack ("X-Header-" <> show i)), BS8.pack ("value-" <> show i))
  | i <- [1..20 :: Int]
  ] <> [("Content-Type", "text/plain")]

------------------------------------------------------------------------
-- wireform keep-alive path (persistent connection)
------------------------------------------------------------------------

data WfEnv = WfEnv
  { wfConn :: !Connection
  , wfPort :: !String
  }

setupWireform :: Maybe RingPool -> Wai.Application -> IO WfEnv
setupWireform mPool app = do
  portStr <- startWireformServer mPool app
  let connCfg = defaultConnectionConfig
        { connectionHost = "127.0.0.1"
        , connectionPort = portStr
        , connectionVersionRange = http1Only
        , connectionTls = Nothing
        }
  eConn <- openConnection connCfg
  case eConn of
    Left err -> error ("openConnection failed: " <> err)
    Right conn -> pure WfEnv { wfConn = conn, wfPort = portStr }

startWireformServer :: Maybe RingPool -> Wai.Application -> IO String
startWireformServer mPool app = do
  readyVar <- newEmptyMVar
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> error "no addr"
    (addr:_) -> do
      listenSock <- NS.openSocket addr
      NS.setSocketOption listenSock NS.ReuseAddr 1
      NS.bind listenSock (NS.addrAddress addr)
      NS.listen listenSock 128
      bound <- NS.getSocketName listenSock
      let portStr = case bound of
            NS.SockAddrInet p _ -> show (fromIntegral p :: Int)
            _ -> "0"
          handler = waiToHandler app
          cfg = defaultServerConfig
            { serverHost = "127.0.0.1"
            , serverPort = portStr
            , serverVersionRange = http1Only
            , serverHandler = handler
            , serverRingPool = mPool
            }
      _ <- forkIO $ do
        putMVar readyVar ()
        runServerOnListener cfg listenSock `finally` NS.close listenSock
      takeMVar readyVar
      pure portStr

wfGet :: WfEnv -> IO BS.ByteString
wfGet env = do
  let port = wfPort env
  r <- sendOn (wfConn env) U.Request
    { U.requestMethod    = U.methodFromBytes "GET"
    , U.requestTarget    = "/"
    , U.requestAuthority = Just (BS8.pack ("127.0.0.1:" <> port))
    , U.requestScheme    = U.SchemeHttp
    , U.requestHeaders   = [(U.hHost, BS8.pack ("127.0.0.1:" <> port))]
    , U.requestBody      = U.BodyEmpty
    , U.requestVersion   = U.HTTP1_1
    , U.requestTrailers  = pure []
    }
  drainBody (U.responseBody r)

------------------------------------------------------------------------
-- wireform connection-churn path (new connection per request)
------------------------------------------------------------------------

churnGet :: String -> IO BS.ByteString
churnGet port = do
  let connCfg = defaultConnectionConfig
        { connectionHost = "127.0.0.1"
        , connectionPort = port
        , connectionVersionRange = http1Only
        , connectionTls = Nothing
        }
  withConnection connCfg $ \conn -> do
    r <- sendOn conn U.Request
      { U.requestMethod    = U.methodFromBytes "GET"
      , U.requestTarget    = "/"
      , U.requestAuthority = Just (BS8.pack ("127.0.0.1:" <> port))
      , U.requestScheme    = U.SchemeHttp
      , U.requestHeaders   =
          [ (U.hHost, BS8.pack ("127.0.0.1:" <> port))
          , (U.hConnection, "close")
          ]
      , U.requestBody      = U.BodyEmpty
      , U.requestVersion   = U.HTTP1_1
      , U.requestTrailers  = pure []
      }
    drainBody (U.responseBody r)

drainBody :: U.Body -> IO BS.ByteString
drainBody U.BodyEmpty = pure BS.empty
drainBody (U.BodyBytes bs) = pure bs
drainBody (U.BodyStream p) = BS.concat <$> go
  where
    go = do
      mc <- p
      case mc of
        Nothing -> pure []
        Just c  -> (c :) <$> go

------------------------------------------------------------------------
-- Warp
------------------------------------------------------------------------

startWarp :: Wai.Application -> IO String
startWarp app = do
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> error "no addr"
    (addr:_) -> do
      sock <- NS.openSocket addr
      NS.setSocketOption sock NS.ReuseAddr 1
      NS.bind sock (NS.addrAddress addr)
      NS.listen sock 128
      bound <- NS.getSocketName sock
      let port = case bound of
            NS.SockAddrInet p _ -> fromIntegral p :: Int
            _ -> 0
          settings = Warp.setPort port
                   $ Warp.setHost "127.0.0.1"
                   $ Warp.defaultSettings
      _ <- forkIO $ Warp.runSettingsSocket settings sock app
      threadDelay 50000
      pure ("http://127.0.0.1:" <> show port)

warpGet :: HC.Manager -> String -> IO BS.ByteString
warpGet mgr url = do
  req <- HC.parseRequest url
  resp <- HC.httpLbs req mgr
  pure $! LBS.toStrict (HC.responseBody resp)
