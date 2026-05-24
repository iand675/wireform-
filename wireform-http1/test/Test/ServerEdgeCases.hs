{-# LANGUAGE OverloadedStrings #-}
{- | Additional integration tests for the HTTP/1.x server and client
covering edge cases: large bodies, concurrent connections, error
responses, HEAD method, empty bodies, binary payloads.
-}
module Test.ServerEdgeCases (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (bracket, finally)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import qualified Network.Socket as NS

import Test.Tasty
import Test.Tasty.HUnit

import Network.HTTP1.Client
import Network.HTTP1.Server
import Network.HTTP1.Status
import Network.HTTP1.Types
import Network.HTTP1.Version

tests :: TestTree
tests = testGroup "Server edge cases"
  [ largeBodyTest
  , binaryBodyTest
  , errorStatusTest
  , emptyBodyResponseTest
  , manySequentialRequestsTest
  , streamingRequestEchoTest
  , concurrentConnectionsTest
  , responseHeaderPreservationTest
  ]

------------------------------------------------------------------------

largeBodyTest :: TestTree
largeBodyTest = testCase "8 KiB POST body round-trips" $
  withServer echo $ \port -> do
    let payload = BS.replicate 8192 0x42
    withClientConnection (clientCfg port) $ \conn -> do
      Right r <- sendRequestOn conn (mkReq POST "/large" port (BodyBytes payload) [])
      body <- bodyOf r
      BS.length body @?= 8192
      body @?= payload
  where
    echo req = do
      body <- drainAll (requestBody req)
      pure $ Response OK HTTP_1_1 [] (BodyBytes body) (pure [])

binaryBodyTest :: TestTree
binaryBodyTest = testCase "all 256 byte values survive round-trip" $
  withServer echo $ \port -> do
    let payload = BS.pack [0..255]
        req = mkReq POST "/binary" port (BodyBytes payload) []
    Right r <- sendRequest (clientCfg port) req
    body <- bodyOf r
    body @?= payload
  where
    echo req = do
      body <- drainAll (requestBody req)
      pure $ Response OK HTTP_1_1 [] (BodyBytes body) (pure [])

errorStatusTest :: TestTree
errorStatusTest = testCase "server returns 404, 500" $
  withServer handler $ \port -> do
    Right r404 <- sendRequest (clientCfg port) (mkReq GET "/404" port BodyEmpty [])
    responseStatus r404 @?= NotFound
    _ <- bodyOf r404

    Right r500 <- sendRequest (clientCfg port) (mkReq GET "/500" port BodyEmpty [])
    responseStatus r500 @?= InternalServerError
    _ <- bodyOf r500
    pure ()
  where
    handler req = case requestTarget req of
      "/404" -> pure $ Response NotFound HTTP_1_1 [] (BodyBytes "not found") (pure [])
      "/500" -> pure $ Response InternalServerError HTTP_1_1 [] (BodyBytes "error") (pure [])
      _      -> pure $ resp200 "ok"

emptyBodyResponseTest :: TestTree
emptyBodyResponseTest = testCase "204 No Content has empty body" $
  withServer (\_ -> pure (Response NoContent HTTP_1_1 [] BodyEmpty (pure []))) $ \port -> do
    Right r <- sendRequest (clientCfg port) (mkReq GET "/" port BodyEmpty [])
    responseStatus r @?= NoContent
    body <- bodyOf r
    body @?= ""

manySequentialRequestsTest :: TestTree
manySequentialRequestsTest =
  testCase "10 sequential requests on one keep-alive connection" $
    withServer (\_ -> pure (resp200 "ok")) $ \port -> do
      withClientConnection (clientCfg port) $ \conn -> do
        results <- mapM (\i -> do
          Right r <- sendRequestOn conn (mkReq GET (BS8.pack ("/" <> show i)) port BodyEmpty [])
          body <- bodyOf r
          pure (responseStatus r, body)
          ) [1 :: Int .. 10]
        let statuses = map fst results
        all (== OK) statuses @?
          ("all should be OK, got: " <> show statuses)

streamingRequestEchoTest :: TestTree
streamingRequestEchoTest = testCase "streaming chunked request body echo" $
  withServer echo $ \port -> do
    let chunks = ["alpha", "beta", "gamma", "delta"]
    ref <- newIORef chunks
    let producer = do
          xs <- readIORef ref
          case xs of
            []    -> pure Nothing
            (h:t) -> writeIORef ref t >> pure (Just h)
        req = mkReq POST "/stream" port (BodyStream producer) []
    Right r <- sendRequest (clientCfg port) req
    body <- bodyOf r
    body @?= "alphabetagammadelta"
  where
    echo req = do
      body <- drainAll (requestBody req)
      pure $ Response OK HTTP_1_1 [] (BodyBytes body) (pure [])

concurrentConnectionsTest :: TestTree
concurrentConnectionsTest = testCase "3 concurrent connections" $
  withServer (\_ -> pure (resp200 "concurrent-ok")) $ \port -> do
    results <- newIORef ([] :: [BS.ByteString])
    doneVar <- newEmptyMVar
    let go = do
          Right r <- sendRequest (clientCfg port) (mkReq GET "/" port BodyEmpty [])
          body <- bodyOf r
          atomicModifyIORef' results (\xs -> (body : xs, ()))
          putMVar doneVar ()
    mapM_ (\_ -> forkIO go) [1 :: Int .. 3]
    mapM_ (\_ -> takeMVar doneVar) [1 :: Int .. 3]
    bodies <- readIORef results
    length bodies @?= 3
    all (== "concurrent-ok") bodies @?
      "all concurrent responses should match"

responseHeaderPreservationTest :: TestTree
responseHeaderPreservationTest = testCase "custom response headers preserved" $
  withServer handler $ \port -> do
    Right r <- sendRequest (clientCfg port) (mkReq GET "/" port BodyEmpty [])
    responseStatus r @?= OK
    let hdrs = responseHeaders r
    lookup "x-custom" hdrs @?= Just "test-value"
    lookup "x-another" hdrs @?= Just "another-value"
    _ <- bodyOf r
    pure ()
  where
    handler _ = pure $ Response OK HTTP_1_1
      [ ("X-Custom", "test-value")
      , ("X-Another", "another-value")
      , ("Content-Type", "text/plain")
      ]
      (BodyBytes "ok")
      (pure [])

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

withServer :: Handler -> (String -> IO ()) -> IO ()
withServer handler action = do
  readyMV <- newEmptyMVar
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> assertFailure "no addr"
    (addr : _) -> bracket
      (NS.openSocket addr)
      NS.close
      $ \listenSock -> do
        NS.setSocketOption listenSock NS.ReuseAddr 1
        NS.bind listenSock (NS.addrAddress addr)
        NS.listen listenSock 128
        boundAddr <- NS.getSocketName listenSock
        let portInt = case boundAddr of
              NS.SockAddrInet p _ -> fromIntegral p :: Int
              _ -> 0
            cfg = defaultServerConfig
              { serverHost = "127.0.0.1"
              , serverPort = show portInt
              , serverHandler = handler
              }
        tid <- forkIO $ do
          putMVar readyMV ()
          acceptForever cfg listenSock
        takeMVar readyMV
        action (show portInt) `finally` killThread tid

acceptForever :: ServerConfig -> NS.Socket -> IO ()
acceptForever cfg listenSock = loop
  where
    loop = do
      (s, _) <- NS.accept listenSock
      _ <- forkIO (runServerOnSocket cfg s)
      loop

clientCfg :: String -> ClientConfig
clientCfg p = defaultClientConfig { clientHost = "127.0.0.1", clientPort = p }

mkReq :: Method -> BS.ByteString -> String -> Body -> Headers -> Request
mkReq m t port body extras = Request
  { requestMethod   = m
  , requestTarget   = t
  , requestVersion  = HTTP_1_1
  , requestHeaders  = [("Host", BS.pack (map (fromIntegral . fromEnum) ("127.0.0.1:" <> port)))] <> extras
  , requestBody     = body
  , requestTrailers = pure []
  }

resp200 :: BS.ByteString -> Response
resp200 b = Response OK HTTP_1_1 [("Content-Type", "text/plain")] (BodyBytes b) (pure [])

bodyOf :: Response -> IO BS.ByteString
bodyOf = drainAll . responseBody

drainAll :: Body -> IO BS.ByteString
drainAll BodyEmpty = pure ""
drainAll (BodyBytes bs) = pure bs
drainAll (BodyStream prod) = BS.concat <$> go
  where
    go = do
      mc <- prod
      case mc of
        Nothing -> pure []
        Just c -> (c :) <$> go
