{-# LANGUAGE OverloadedStrings #-}
{- | Edge-case and stress tests for HTTP/1.x through the unified API.

Covers scenarios that the happy-path integration tests skip:
large bodies, empty bodies, many headers, keep-alive reuse under
multiple sequential requests, error status codes, HEAD method
semantics, and 204/304 no-body framing.
-}
module Test.Http1EdgeCases (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (bracket, finally, try, SomeException)
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
tests = testGroup "HTTP/1.x edge cases"
  [ emptyGetBody
  , errorStatusCodes
  , headMethod
  , noContent204
  , largeBodyRoundTrip
  , manyHeaders
  , keepAliveSequentialRequests
  , streamingLargeBody
  , binaryBodyContent
  , requestWithQueryString
  , oversizedHeaderBlockRejected
  ]

------------------------------------------------------------------------

emptyGetBody :: TestTree
emptyGetBody = testCase "GET with empty body returns BodyEmpty or empty bytes" $
  withTestServer http1Only (\_ -> pure (resp S.status200 "")) $ \port -> do
    body <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "GET" "/" port BodyEmpty [])
      drainBody (responseBody r)
    body @?= ""

errorStatusCodes :: TestTree
errorStatusCodes = testCase "server returns various error status codes" $
  withTestServer http1Only handler $ \port -> do
    runClient http1Only port $ \c -> do
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

headMethod :: TestTree
headMethod = testCase "HEAD returns headers but no body" $
  withTestServer http1Only (\_ -> pure (resp S.status200 "should-not-appear")) $ \port -> do
    (status, body) <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "HEAD" "/" port BodyEmpty [])
      b <- drainBody (responseBody r)
      pure (responseStatus r, b)
    status @?= S.status200
    body @?= ""

noContent204 :: TestTree
noContent204 = testCase "204 No Content has no body" $
  withTestServer http1Only (\_ -> pure resp204) $ \port -> do
    (status, body) <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "GET" "/" port BodyEmpty [])
      b <- drainBody (responseBody r)
      pure (responseStatus r, b)
    status @?= S.status204
    body @?= ""
  where
    resp204 = Response
      { responseStatus  = S.status204
      , responseVersion = V.HTTP1_1
      , responseHeaders = []
      , responseBody    = BodyEmpty
      , responseTrailers = pure []
      , responseH2StreamId = 0
      , responsePushPromises = pure []
      , responseCancel = pure ()
      }

largeBodyRoundTrip :: TestTree
largeBodyRoundTrip = testCase "64 KiB body round-trips correctly" $
  withTestServer http1Only echo $ \port -> do
    let payload = BS.replicate 65536 0x42
    (status, body) <- runClient http1Only port $ \c -> do
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

manyHeaders :: TestTree
manyHeaders = testCase "request with 50 custom headers" $
  withTestServer http1Only counter $ \port -> do
    let hdrs = [ (CI.mk (BS8.pack ("X-Custom-" <> show n)), BS8.pack ("val-" <> show n))
               | n <- [1 :: Int .. 50]
               ]
    body <- runClient http1Only port $ \c -> do
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

keepAliveSequentialRequests :: TestTree
keepAliveSequentialRequests =
  testCase "5 sequential requests on one keep-alive connection" $
    withTestServer http1Only counter $ \port -> do
      runClient http1Only port $ \c -> do
        results <- mapM (\i -> do
          r <- sendOn c (mkRequest "GET" (BS8.pack ("/" <> show i)) port BodyEmpty [])
          b <- drainBody (responseBody r)
          pure (responseStatus r, b)
          ) [1 :: Int .. 5]
        let statuses = map fst results
        all (== S.status200) statuses @?
          ("all statuses should be 200, got: " <> show statuses)
  where
    counter _ = pure (resp S.status200 "ok")

streamingLargeBody :: TestTree
streamingLargeBody = testCase "streaming 5-chunk response body" $
  withTestServer http1Only handler $ \port -> do
    body <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "GET" "/" port BodyEmpty [])
      drainBody (responseBody r)
    body @?= "chunk1chunk2chunk3chunk4chunk5"
  where
    handler _ = do
      ref <- newIORef ["chunk1", "chunk2", "chunk3", "chunk4", "chunk5"]
      pure Response
        { responseStatus  = S.status200
        , responseVersion = V.HTTP1_1
        , responseHeaders = []
        , responseBody    = BodyStream $ do
            xs <- readIORef ref
            case xs of
              []    -> pure Nothing
              (h:t) -> writeIORef ref t >> pure (Just h)
        , responseTrailers = pure []
        , responseH2StreamId = 0
        , responsePushPromises = pure []
        , responseCancel = pure ()
        }

binaryBodyContent :: TestTree
binaryBodyContent = testCase "binary body with all byte values 0x00-0xFF" $
  withTestServer http1Only echo $ \port -> do
    let payload = BS.pack [0..255]
    body <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "POST" "/" port (BodyBytes payload) [])
      drainBody (responseBody r)
    body @?= payload
  where
    echo req = do
      body <- drainBody (requestBody req)
      pure (resp S.status200 body)

requestWithQueryString :: TestTree
requestWithQueryString = testCase "request target with query string preserved" $
  withTestServer http1Only echoTarget $ \port -> do
    body <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "GET" "/path?key=value&foo=bar" port BodyEmpty [])
      drainBody (responseBody r)
    body @?= "/path?key=value&foo=bar"
  where
    echoTarget req = pure (resp S.status200 (requestTarget req))

-- | Companion to 'largeBodyRoundTrip'. The header-block cap fix in
-- 'Network.HTTP1.StreamingReader.readHeaderBlockFrom' was about
-- not /spuriously/ tripping when the body inflates the ring; this
-- test confirms the cap still triggers for a header block that is
-- genuinely too large. The default cap is 32 KiB; a single
-- header value at ~100 KiB sails well past it and the server must
-- answer 431 (and close).
oversizedHeaderBlockRejected :: TestTree
oversizedHeaderBlockRejected =
  testCase "request with a 100 KiB header value gets 431" $
    withTestServer http1Only (\_ -> pure (resp S.status200 "")) $ \port -> do
      let bigHdr = (CI.mk "X-Huge", BS.replicate (100 * 1024) (asciiByte 'A'))
      status <- runClient http1Only port $ \c -> do
        r <- sendOn c (mkRequest "GET" "/" port BodyEmpty [bigHdr])
        _ <- drainBody (responseBody r)
        pure (responseStatus r)
      status @?= S.status431
  where
    asciiByte = fromIntegral . fromEnum

------------------------------------------------------------------------
-- Plumbing (shared with other integration modules)
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
  , requestVersion   = V.HTTP1_1
  , requestTrailers  = pure []
  }

resp :: S.Status -> BS.ByteString -> Response
resp status body = Response
  { responseStatus   = status
  , responseVersion  = V.HTTP1_1
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
