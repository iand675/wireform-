{-# LANGUAGE OverloadedStrings #-}
{- | Edge-case and stress tests for HTTP/2 through the unified API.

Covers: large bodies, concurrent multiplexed streams, error status
codes, HEAD semantics over h2, binary payloads, empty bodies,
and streaming request bodies.
-}
module Test.Http2EdgeCases (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (bracket, finally)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Data.IORef
import qualified Network.Socket as NS

import Test.Tasty
import Test.Tasty.HUnit

import Network.HTTP
import Network.HTTP.Connection
import Network.HTTP.Server
import qualified Network.HTTP.Types.Status as S
import qualified Network.HTTP.Types.Version as V

tests :: TestTree
tests = testGroup "HTTP/2 edge cases"
  [ emptyBody
  , errorStatusCodes
  , largeBodyRoundTrip
  , streamingLargeBody
  , binaryPayload
  , manyHeaders
  , concurrentManyStreams
  , sequentialRequestReuse
  , streamingRequestLargeBody
  , emptyStreamingResponse
  ]

------------------------------------------------------------------------

emptyBody :: TestTree
emptyBody = testCase "GET returning empty body" $
  withTestServer http2Only (\_ -> pure (resp S.status200 "")) $ \port -> do
    body <- runClient http2Only port $ \c -> do
      r <- sendOn c (mkRequest "GET" "/" port BodyEmpty [])
      drainBody (responseBody r)
    body @?= ""

errorStatusCodes :: TestTree
errorStatusCodes = testCase "various error status codes" $
  withTestServer http2Only handler $ \port -> do
    runClient http2Only port $ \c -> do
      r400 <- sendOn c (mkRequest "GET" "/400" port BodyEmpty [])
      responseStatus r400 @?= S.status400
      _ <- drainBody (responseBody r400)

      r404 <- sendOn c (mkRequest "GET" "/404" port BodyEmpty [])
      responseStatus r404 @?= S.status404
      _ <- drainBody (responseBody r404)

      r500 <- sendOn c (mkRequest "GET" "/500" port BodyEmpty [])
      responseStatus r500 @?= S.status500
      _ <- drainBody (responseBody r500)
      pure ()
  where
    handler req = case requestTarget req of
      "/400" -> pure (resp S.status400 "bad request")
      "/404" -> pure (resp S.status404 "not found")
      "/500" -> pure (resp S.status500 "internal error")
      _      -> pure (resp S.status200 "ok")

largeBodyRoundTrip :: TestTree
largeBodyRoundTrip = testCase "64 KiB body round-trips" $
  withTestServer http2Only echo $ \port -> do
    let payload = BS.replicate 65536 0x42
    (status, body) <- runClient http2Only port $ \c -> do
      r <- sendOn c (mkRequest "POST" "/" port (BodyBytes payload) [])
      b <- drainBody (responseBody r)
      pure (responseStatus r, b)
    status @?= S.status200
    BS.length body @?= 65536
    body @?= payload
  where
    echo req = do
      body <- drainBody (requestBody req)
      pure (resp S.status200 body)

streamingLargeBody :: TestTree
streamingLargeBody = testCase "streaming 256 KiB response body" $
  withTestServer http2Only handler $ \port -> do
    body <- runClient http2Only port $ \c -> do
      r <- sendOn c (mkRequest "GET" "/" port BodyEmpty [])
      drainBody (responseBody r)
    BS.length body @?= 256 * 1024
  where
    handler _ = do
      let chunkSize = 4096
          totalChunks = 64 :: Int
          chunk = BS.replicate chunkSize 0x58
      ref <- newIORef totalChunks
      pure Response
        { responseStatus  = S.status200
        , responseVersion = V.HTTP2
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

binaryPayload :: TestTree
binaryPayload = testCase "binary body with all byte values" $
  withTestServer http2Only echo $ \port -> do
    let payload = BS.pack [0..255]
    body <- runClient http2Only port $ \c -> do
      r <- sendOn c (mkRequest "POST" "/" port (BodyBytes payload) [])
      drainBody (responseBody r)
    body @?= payload
  where
    echo req = do
      body <- drainBody (requestBody req)
      pure (resp S.status200 body)

manyHeaders :: TestTree
manyHeaders = testCase "request with 50 custom headers" $
  withTestServer http2Only counter $ \port -> do
    let hdrs = [ (CI.mk (BS8.pack ("x-custom-" <> show n)), BS8.pack ("val-" <> show n))
               | n <- [1 :: Int .. 50]
               ]
    body <- runClient http2Only port $ \c -> do
      r <- sendOn c (mkRequest "GET" "/" port BodyEmpty hdrs)
      drainBody (responseBody r)
    let count = read (BS8.unpack body) :: Int
    assertBool "server saw at least 50 custom headers" (count >= 50)
  where
    counter req = do
      let customCount = length
            [ ()
            | (n, _) <- requestHeaders req
            , "x-custom-" `BS.isPrefixOf` CI.foldedCase n
            ]
      pure (resp S.status200 (BS8.pack (show customCount)))

concurrentManyStreams :: TestTree
concurrentManyStreams =
  testCase "10 concurrent streams on one connection" $
    withTestServer http2Only handler $ \port -> do
      runClient http2Only port $ \c -> do
        responses <- mapM (\i -> do
          sendOn c (mkRequest "GET" (BS8.pack ("/" <> show i)) port BodyEmpty [])
          ) [1 :: Int .. 10]
        bodies <- mapM (drainBody . responseBody) responses
        let statuses = map responseStatus responses
        all (== S.status200) statuses @?
          ("all statuses should be 200, got: " <> show statuses)
        length bodies @?= 10
  where
    handler req = do
      let target = requestTarget req
      pure (resp S.status200 target)

sequentialRequestReuse :: TestTree
sequentialRequestReuse =
  testCase "5 sequential requests on one h2 connection" $
    withTestServer http2Only (\_ -> pure (resp S.status200 "ok")) $ \port -> do
      runClient http2Only port $ \c -> do
        results <- mapM (\i -> do
          r <- sendOn c (mkRequest "GET" (BS8.pack ("/" <> show i)) port BodyEmpty [])
          b <- drainBody (responseBody r)
          pure (responseStatus r, b)
          ) [1 :: Int .. 5]
        let statuses = map fst results
        all (== S.status200) statuses @?
          ("all statuses should be 200, got: " <> show statuses)

streamingRequestLargeBody :: TestTree
streamingRequestLargeBody =
  testCase "streaming 128 KiB request body" $
    withTestServer http2Only echo $ \port -> do
      let chunkSize = 4096
          totalChunks = 32 :: Int
          chunk = BS.replicate chunkSize 0x61
      ref <- newIORef totalChunks
      let producer = do
            remaining <- readIORef ref
            if remaining <= 0
              then pure Nothing
              else do
                writeIORef ref (remaining - 1)
                pure (Just chunk)
      (status, body) <- runClient http2Only port $ \c -> do
        r <- sendOn c (mkRequest "POST" "/" port (BodyStream producer) [])
        b <- drainBody (responseBody r)
        pure (responseStatus r, b)
      status @?= S.status200
      BS.length body @?= chunkSize * totalChunks
  where
    echo req = do
      body <- drainBody (requestBody req)
      pure (resp S.status200 body)

emptyStreamingResponse :: TestTree
emptyStreamingResponse = testCase "streaming body that immediately returns Nothing" $
  withTestServer http2Only handler $ \port -> do
    body <- runClient http2Only port $ \c -> do
      r <- sendOn c (mkRequest "GET" "/" port BodyEmpty [])
      drainBody (responseBody r)
    body @?= ""
  where
    handler _ = pure Response
      { responseStatus  = S.status200
      , responseVersion = V.HTTP2
      , responseHeaders = []
      , responseBody    = BodyStream (pure Nothing)
      , responseTrailers = pure []
      , responseH2StreamId = 0
      , responsePushPromises = pure []
      , responseCancel = pure ()
      }

------------------------------------------------------------------------
-- Plumbing
------------------------------------------------------------------------

withTestServer
  :: VersionRange
  -> Handler
  -> (String -> IO a)
  -> IO a
withTestServer range handler action = do
  readyVar <- newEmptyMVar
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> assertFailure "no addr available for test bind"
    (addr:_) -> bracket
      (NS.openSocket addr)
      NS.close
      $ \listenSock -> do
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
          runServerOnListener cfg listenSock
        takeMVar readyVar
        action portStr `finally` killThread tid

runClient :: VersionRange -> String -> (Connection -> IO a) -> IO a
runClient range port action = do
  let cfg = defaultConnectionConfig
        { connectionHost = "127.0.0.1"
        , connectionPort = port
        , connectionVersionRange = range
        , connectionTls = Nothing
        }
  withConnection cfg action

mkRequest
  :: BS.ByteString
  -> BS.ByteString
  -> String
  -> Body
  -> Headers
  -> Request
mkRequest method target port body extras = Request
  { requestMethod    = methodFromBytes method
  , requestTarget    = target
  , requestAuthority = Just (BS8.pack ("127.0.0.1:" <> port))
  , requestScheme    = SchemeHttp
  , requestHeaders   = extras
  , requestBody      = body
  , requestVersion   = V.HTTP2
  , requestTrailers  = pure []
  }

resp :: S.Status -> BS.ByteString -> Response
resp status body = Response
  { responseStatus   = status
  , responseVersion  = V.HTTP2
  , responseHeaders  = []
  , responseBody     = if BS.null body then BodyEmpty else BodyBytes body
  , responseTrailers = pure []
  , responseH2StreamId = 0
  , responsePushPromises = pure []
  , responseCancel = pure ()
  }

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
