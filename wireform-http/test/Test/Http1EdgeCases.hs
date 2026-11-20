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
import Control.Exception (SomeException, bracket, finally, try)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.CaseInsensitive qualified as CI
import Data.IORef
import Data.Text qualified as T
import Network.HTTP
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.Cache qualified as Cache
import Network.HTTP.Client.Response qualified as Raw
import Network.HTTP.Connection
import Network.HTTP.Server
import Network.HTTP.Types.Status qualified as S
import Network.HTTP.Types.Version qualified as V
import Network.Socket qualified as NS
import Test.Syd


tests :: Spec
tests =
  describe "HTTP/1.x edge cases" $
    sequence_
      [ emptyGetBody
      , errorStatusCodes
      , headMethod
      , noContent204
      , largeBodyRoundTrip
      , manyHeaders
      , keepAliveSequentialRequests
      , streamingLargeBody
      , cacheChunkedResponse
      , binaryBodyContent
      , requestWithQueryString
      , oversizedHeaderBlockRejected
      ]


------------------------------------------------------------------------

emptyGetBody :: Spec
emptyGetBody = it "GET with empty body returns BodyEmpty or empty bytes" $
  withTestServer http1Only (\_ -> pure (resp S.status200 "")) $ \port -> do
    body <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "GET" "/" port BodyEmpty [])
      drainBody (responseBody r)
    body `shouldBe` ""


errorStatusCodes :: Spec
errorStatusCodes = it "server returns various error status codes" $
  withTestServer http1Only handler $ \port -> do
    runClient http1Only port $ \c -> do
      r400 <- sendOn c (mkRequest "GET" "/400" port BodyEmpty [])
      responseStatus r400 `shouldBe` S.status400
      _ <- drainBody (responseBody r400)

      r404 <- sendOn c (mkRequest "GET" "/404" port BodyEmpty [])
      responseStatus r404 `shouldBe` S.status404
      _ <- drainBody (responseBody r404)

      r500 <- sendOn c (mkRequest "GET" "/500" port BodyEmpty [])
      responseStatus r500 `shouldBe` S.status500
      _ <- drainBody (responseBody r500)
      pure ()
  where
    handler req = case requestTarget req of
      "/400" -> pure (resp S.status400 "bad request")
      "/404" -> pure (resp S.status404 "not found")
      "/500" -> pure (resp S.status500 "internal error")
      _ -> pure (resp S.status200 "ok")


headMethod :: Spec
headMethod = it "HEAD returns headers but no body" $
  withTestServer http1Only (\_ -> pure (resp S.status200 "should-not-appear")) $ \port -> do
    (status, body) <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "HEAD" "/" port BodyEmpty [])
      b <- drainBody (responseBody r)
      pure (responseStatus r, b)
    status `shouldBe` S.status200
    body `shouldBe` ""


noContent204 :: Spec
noContent204 = it "204 No Content has no body" $
  withTestServer http1Only (\_ -> pure resp204) $ \port -> do
    (status, body) <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "GET" "/" port BodyEmpty [])
      b <- drainBody (responseBody r)
      pure (responseStatus r, b)
    status `shouldBe` S.status204
    body `shouldBe` ""
  where
    resp204 =
      Response
        { responseStatus = S.status204
        , responseVersion = V.HTTP1_1
        , responseHeaders = []
        , responseBody = BodyEmpty
        , responseTrailers = pure []
        , responseH2StreamId = 0
        , responseCancel = pure ()
        , responsePushPromises = pure []
        }


largeBodyRoundTrip :: Spec
largeBodyRoundTrip = it "64 KiB body round-trips correctly" $
  withTestServer http1Only echo $ \port -> do
    let payload = BS.replicate 65536 0x42
    (status, body) <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "POST" "/" port (BodyBytes payload) [])
      b <- drainBody (responseBody r)
      pure (responseStatus r, b)
    status `shouldBe` S.status200
    BS.length body `shouldBe` 65536
    body `shouldBe` payload
  where
    echo req = do
      body <- drainBody (requestBody req)
      pure (resp S.status200 body)


manyHeaders :: Spec
manyHeaders = it "request with 50 custom headers" $
  withTestServer http1Only counter $ \port -> do
    let hdrs =
          [ (CI.mk (BS8.pack ("X-Custom-" <> show n)), BS8.pack ("val-" <> show n))
          | n <- [1 :: Int .. 50]
          ]
    body <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "GET" "/" port BodyEmpty hdrs)
      drainBody (responseBody r)
    let count = read (BS8.unpack body) :: Int
    (count >= 50) `shouldBe` True
  where
    counter req = do
      let customCount =
            length
              [ ()
              | (n, _) <- requestHeaders req
              , "x-custom-" `BS.isPrefixOf` CI.foldedCase n
              ]
      pure (resp S.status200 (BS8.pack (show customCount)))


keepAliveSequentialRequests :: Spec
keepAliveSequentialRequests =
  it "5 sequential requests on one keep-alive connection" $
    withTestServer http1Only counter $ \port -> do
      runClient http1Only port $ \c -> do
        results <-
          mapM
            ( \i -> do
                r <- sendOn c (mkRequest "GET" (BS8.pack ("/" <> show i)) port BodyEmpty [])
                b <- drainBody (responseBody r)
                pure (responseStatus r, b)
            )
            [1 :: Int .. 5]
        let statuses = map fst results
        all (== S.status200) statuses `shouldBe` True
  where
    counter _ = pure (resp S.status200 "ok")


streamingLargeBody :: Spec
streamingLargeBody = it "streaming 5-chunk response body" $
  withTestServer http1Only handler $ \port -> do
    body <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "GET" "/" port BodyEmpty [])
      drainBody (responseBody r)
    body `shouldBe` "chunk1chunk2chunk3chunk4chunk5"
  where
    handler _ = do
      ref <- newIORef ["chunk1", "chunk2", "chunk3", "chunk4", "chunk5"]
      pure
        Response
          { responseStatus = S.status200
          , responseVersion = V.HTTP1_1
          , responseHeaders = []
          , responseBody = BodyStream $ do
              xs <- readIORef ref
              case xs of
                [] -> pure Nothing
                (h : t) -> writeIORef ref t >> pure (Just h)
          , responseTrailers = pure []
          , responseH2StreamId = 0
          , responseCancel = pure ()
          , responsePushPromises = pure []
          }


cacheChunkedResponse :: Spec
cacheChunkedResponse = it "replays a chunked response from cache with its end-to-end headers" $ do
  calls <- newIORef (0 :: Int)
  withTestServer http1Only (handler calls) $ \port -> do
    cache <- Cache.newCache Cache.defaultCacheConfig
    let uri = Client.staticURI (T.pack ("http://127.0.0.1:" <> port <> "/"))
        req = Client.request (methodFromBytes "GET") uri ()
        transport = Cache.withCache cache (Client.baseTransport http1Only)
    Client.withResponse transport req [] $ \response -> do
      lookup "Transfer-Encoding" (Raw.headers response) `shouldBe` Just "chunked"
      lookup "X-Hop" (Raw.headers response) `shouldBe` Just "local"
      Client.rawResponseBytes response `shouldReturn` "firstsecond"
    Client.withResponse transport req [] $ \response -> do
      mapM_ (\name -> lookup name (Raw.headers response) `shouldBe` Nothing)
        ["Connection", "Transfer-Encoding", "X-Hop"]
      lookup "X-Representation" (Raw.headers response) `shouldBe` Just "stored"
      Client.rawResponseBytes response `shouldReturn` "firstsecond"
    readIORef calls `shouldReturn` 1
  where
    handler calls _ = do
      atomicModifyIORef' calls (\count -> (count + 1, ()))
      chunks <- newIORef ["first", "second"]
      let next = atomicModifyIORef' chunks $ \xs -> case xs of
            [] -> ([], Nothing)
            x : rest -> (rest, Just x)
      pure
        (resp S.status200 "")
          { responseHeaders =
              [ ("Cache-Control", "max-age=60")
              , ("Connection", "close, X-Hop")
              , ("X-Hop", "local")
              , ("X-Representation", "stored")
              ]
          , responseBody = BodyStream next
          }


binaryBodyContent :: Spec
binaryBodyContent = it "binary body with all byte values 0x00-0xFF" $
  withTestServer http1Only echo $ \port -> do
    let payload = BS.pack [0 .. 255]
    body <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "POST" "/" port (BodyBytes payload) [])
      drainBody (responseBody r)
    body `shouldBe` payload
  where
    echo req = do
      body <- drainBody (requestBody req)
      pure (resp S.status200 body)


requestWithQueryString :: Spec
requestWithQueryString = it "request target with query string preserved" $
  withTestServer http1Only echoTarget $ \port -> do
    body <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "GET" "/path?key=value&foo=bar" port BodyEmpty [])
      drainBody (responseBody r)
    body `shouldBe` "/path?key=value&foo=bar"
  where
    echoTarget req = pure (resp S.status200 (requestTarget req))


{- | Companion to 'largeBodyRoundTrip'. The header-block cap fix in
'Network.HTTP1.StreamingReader.readHeaderBlockFrom' was about
not /spuriously/ tripping when the body inflates the ring; this
test confirms the cap still triggers for a header block that is
genuinely too large. The default cap is 32 KiB; a single
header value at ~100 KiB sails well past it and the server must
answer 431 (and close).
-}
oversizedHeaderBlockRejected :: Spec
oversizedHeaderBlockRejected =
  it "request with a 100 KiB header value gets 431" $
    withTestServer http1Only (\_ -> pure (resp S.status200 "")) $ \port -> do
      let bigHdr = (CI.mk "X-Huge", BS.replicate (100 * 1024) (asciiByte 'A'))
      status <- runClient http1Only port $ \c -> do
        r <- sendOn c (mkRequest "GET" "/" port BodyEmpty [bigHdr])
        _ <- drainBody (responseBody r)
        pure (responseStatus r)
      status `shouldBe` S.status431
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
  let hints =
        NS.defaultHints
          { NS.addrFlags = [NS.AI_PASSIVE]
          , NS.addrSocketType = NS.Stream
          }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> expectationFailure "no addr available for test bind"
    (addr : _) -> bracket
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
            cfg =
              defaultServerConfig
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
  let cfg =
        defaultConnectionConfig
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
mkRequest method target port body extras =
  Request
    { requestMethod = methodFromBytes method
    , requestTarget = target
    , requestAuthority = Just (BS8.pack ("127.0.0.1:" <> port))
    , requestScheme = SchemeHttp
    , requestHeaders = extras
    , requestBody = body
    , requestVersion = V.HTTP1_1
    , requestTrailers = pure []
    }


resp :: S.Status -> BS.ByteString -> Response
resp status body =
  Response
    { responseStatus = status
    , responseVersion = V.HTTP1_1
    , responseHeaders = []
    , responseBody = if BS.null body then BodyEmpty else BodyBytes body
    , responseTrailers = pure []
    , responseH2StreamId = 0
    , responseCancel = pure ()
    , responsePushPromises = pure []
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
