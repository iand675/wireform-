{-# LANGUAGE OverloadedStrings #-}
{- | Micro-benchmarks for the wireform HTTP unified layer.

Measures the overhead of the conversion layer and the unified
client/server on top of the HTTP/1 and HTTP/2 runtimes.
-}
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (finally)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import qualified Network.Socket as NS

import Criterion.Main

import Network.HTTP
import Network.HTTP.Connection
import Network.HTTP.Server
import qualified Network.HTTP.Types.Status as S
import qualified Network.HTTP.Types.Version as V

main :: IO ()
main = defaultMain
  [ bgroup "HTTP/1.x"
      [ withSetup http1Only "hello-world GET" helloHandler $ \port ->
          bench "request/response" $ nfIO (doGet http1Only port V.HTTP1_1)
      , withSetup http1Only "64KiB POST echo" echoHandler $ \port ->
          bench "large body" $ nfIO (doPost http1Only port V.HTTP1_1 payload64k)
      , withSetup http1Only "streaming 64KiB" (streamHandler 16) $ \port ->
          bench "streaming response" $ nfIO (doGet http1Only port V.HTTP1_1)
      ]
  , bgroup "HTTP/2"
      [ withSetup http2Only "hello-world GET" helloHandler $ \port ->
          bench "request/response" $ nfIO (doGet http2Only port V.HTTP2)
      , withSetup http2Only "64KiB POST echo" echoHandler $ \port ->
          bench "large body" $ nfIO (doPost http2Only port V.HTTP2 payload64k)
      , withSetup http2Only "streaming 64KiB" (streamHandler 16) $ \port ->
          bench "streaming response" $ nfIO (doGet http2Only port V.HTTP2)
      ]
  ]

payload64k :: BS.ByteString
payload64k = BS.replicate (64 * 1024) 0x61

helloHandler :: Handler
helloHandler _ = pure Response
  { responseStatus  = S.status200
  , responseVersion = V.HTTP1_1
  , responseHeaders = []
  , responseBody    = BodyBytes "hello"
  , responseTrailers = pure []
  , responseH2StreamId = 0
  , responsePushPromises = pure []
  , responseCancel = pure ()
  }

echoHandler :: Handler
echoHandler req = do
  body <- drainBody (requestBody req)
  pure Response
    { responseStatus  = S.status200
    , responseVersion = V.HTTP1_1
    , responseHeaders = []
    , responseBody    = BodyBytes body
    , responseTrailers = pure []
    , responseH2StreamId = 0
    , responsePushPromises = pure []
    , responseCancel = pure ()
    }

streamHandler :: Int -> Handler
streamHandler nChunks _ = do
  let chunk = BS.replicate 4096 0x78
  ref <- newIORef nChunks
  pure Response
    { responseStatus  = S.status200
    , responseVersion = V.HTTP1_1
    , responseHeaders = []
    , responseBody    = BodyStream $ do
        remaining <- readIORef ref
        if remaining <= 0
          then pure Nothing
          else do
            writeIORef ref (remaining - 1)
            pure (Just chunk)
    , responseTrailers = pure []
    , responseH2StreamId = 0
    , responsePushPromises = pure []
    , responseCancel = pure ()
    }

doGet :: VersionRange -> String -> V.Version -> IO BS.ByteString
doGet range port ver = do
  let cfg = defaultConnectionConfig
        { connectionHost = "127.0.0.1"
        , connectionPort = port
        , connectionVersionRange = range
        , connectionTls = Nothing
        }
  withConnection cfg $ \c -> do
    r <- sendOn c Request
      { requestMethod   = methodFromBytes "GET"
      , requestTarget   = "/"
      , requestAuthority = Just (BS8.pack ("127.0.0.1:" <> port))
      , requestScheme   = SchemeHttp
      , requestHeaders  = []
      , requestBody     = BodyEmpty
      , requestVersion  = ver
      , requestTrailers = pure []
      }
    drainBody (responseBody r)

doPost :: VersionRange -> String -> V.Version -> BS.ByteString -> IO BS.ByteString
doPost range port ver payload = do
  let cfg = defaultConnectionConfig
        { connectionHost = "127.0.0.1"
        , connectionPort = port
        , connectionVersionRange = range
        , connectionTls = Nothing
        }
  withConnection cfg $ \c -> do
    r <- sendOn c Request
      { requestMethod   = methodFromBytes "POST"
      , requestTarget   = "/"
      , requestAuthority = Just (BS8.pack ("127.0.0.1:" <> port))
      , requestScheme   = SchemeHttp
      , requestHeaders  = []
      , requestBody     = BodyBytes payload
      , requestVersion  = ver
      , requestTrailers = pure []
      }
    drainBody (responseBody r)

withSetup :: VersionRange -> String -> Handler -> (String -> Benchmark) -> Benchmark
withSetup range name handler mkBench =
  envWithCleanup (startServer range handler) stopServer $ \port ->
    bgroup name [mkBench port]

startServer :: VersionRange -> Handler -> IO String
startServer range handler = do
  readyVar <- newEmptyMVar
  tidVar <- newEmptyMVar
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
          cfg = defaultServerConfig
            { serverHost = "127.0.0.1"
            , serverPort = portStr
            , serverVersionRange = range
            , serverHandler = handler
            }
      tid <- forkIO $ do
        putMVar readyVar ()
        runServerOnListener cfg listenSock `finally` NS.close listenSock
      putMVar tidVar tid
      takeMVar readyVar
      pure portStr

stopServer :: String -> IO ()
stopServer _ = pure ()

drainBody :: Body -> IO BS.ByteString
drainBody BodyEmpty = pure ""
drainBody (BodyBytes bs) = pure bs
drainBody (BodyStream p) = BS.concat <$> go
  where
    go = do
      mc <- p
      case mc of
        Nothing -> pure []
        Just c -> (c :) <$> go
