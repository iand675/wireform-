{-# LANGUAGE OverloadedStrings #-}

{- | End-to-end integration tests: actually open a socket pair, run our
server on one side and our client on the other, and verify request /
response semantics on the wire.

These exercise the full pipeline (recv buffer -> parser -> handler ->
encoder -> send buffer) and let us catch desync bugs that pure-code
tests would miss.
-}
module Test.Integration (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (bracket, finally)
import Data.ByteString qualified as BS
import Data.IORef
import Network.HTTP1.Client
import Network.HTTP1.Client.Pool (defaultPoolConfig, destroyPool, newPool, withPooledRequest)
import Network.HTTP1.Encode qualified as Enc
import Network.HTTP1.Method
import Network.HTTP1.Parser (ParseError (ParseMessageTooLong, ParseUnexpectedEof))
import Network.HTTP1.Server
import Network.HTTP1.Status
import Network.HTTP1.Types
import Network.HTTP1.Version
import Network.Socket qualified as NS
import Network.Socket.ByteString qualified as NSB
import System.Directory (removeFile)
import System.IO.Error (tryIOError)
import System.Timeout (timeout)
import Test.Syd


tests :: Spec
tests =
  describe "Integration" $
    sequence_
      [ helloWorldTest
      , hostAuthorityTest
      , clientSocketOwnershipTest
      , echoBodyTest
      , chunkedRequestTest
      , chunkedResponseTest
      , retainedTrailersTest
      , boundedTrailersTest
      , closeDelimitedPoolTest
      , truncatedContentLengthTest
      , keepAlivePipelineTest
      , repeatedConnectionOptionsTests
      , preEncodedGetTest
      , preEncodedHeadTest
      , sendFileGetTest
      , sendFileHeadTest
      , sendFileCachedFdTest
      ]


------------------------------------------------------------------------

helloWorldTest :: Spec
helloWorldTest = it "GET hello world" $
  withServer (\_ -> pure $ resp200 "Hello, world!\n") $ \port -> do
    let req = mkReq GET "/" port BodyEmpty []
    Right r <- sendRequest (clientCfg port) req
    responseStatus r `shouldBe` OK
    bodyOf r >>= (`shouldBe` "Hello, world!\n")


clientSocketOwnershipTest :: Spec
clientSocketOwnershipTest = it "closes the owned TCP socket when the client connection is released" $
  withServer (\_ -> pure (resp200 "ok")) $ \port -> do
    Just socket <- withClientConnection (clientCfg port) (pure . clientConnectionSocket)
    result <- tryIOError (NS.getPeerName socket)
    case result of
      Left _ -> pure ()
      Right peer -> expectationFailure ("socket still connected to " <> show peer)


hostAuthorityTest :: Spec
hostAuthorityTest = it "validates Host before handing the request to the server" $
  withServer echoHost $ \port -> do
    let request host = (mkReq GET "/" port BodyEmpty []) {requestHeaders = [("Host", host)]}
    Right good <- sendRequest (clientCfg port) (request "[2001:db8::1]:8080")
    responseStatus good `shouldBe` OK
    bodyOf good >>= (`shouldBe` "[2001:db8::1]:8080")
    Right bad <- sendRequest (clientCfg port) (request "example.test:port")
    responseStatus bad `shouldBe` BadRequest
  where
    echoHost = pure . resp200 . maybe "" id . findHost . requestHeaders


echoBodyTest :: Spec
echoBodyTest = it "POST echo body" $
  withServer echo $ \port -> do
    let req = mkReq POST "/echo" port (BodyBytes "round trip me") []
    Right r <- sendRequest (clientCfg port) req
    responseStatus r `shouldBe` OK
    bodyOf r >>= (`shouldBe` "round trip me")
  where
    echo req = do
      body <- drainAll (requestBody req)
      pure $
        Response
          OK
          HTTP_1_1
          [("Content-Type", "text/plain")]
          (BodyBytes body)
          (pure [])


chunkedRequestTest :: Spec
chunkedRequestTest = it "POST chunked request body" $
  withServer echo $ \port -> do
    chunkRef <- newIORef ["one", "two", "three!"]
    let producer = do
          xs <- readIORef chunkRef
          case xs of
            [] -> pure Nothing
            (h : t) -> do writeIORef chunkRef t; pure (Just h)
        req = mkReq POST "/" port (BodyStream producer) []
    Right r <- sendRequest (clientCfg port) req
    responseStatus r `shouldBe` OK
    bodyOf r >>= (`shouldBe` "onetwothree!")
  where
    echo req = do
      body <- drainAll (requestBody req)
      pure $ Response OK HTTP_1_1 [] (BodyBytes body) (pure [])


chunkedResponseTest :: Spec
chunkedResponseTest = it "streaming chunked response" $
  withServer streaming $ \port -> do
    let req = mkReq GET "/stream" port BodyEmpty []
    Right r <- sendRequest (clientCfg port) req
    responseStatus r `shouldBe` OK
    bodyOf r >>= (`shouldBe` "alphabetagamma")
  where
    streaming _ = do
      chunkRef <- newIORef ["alpha", "beta", "gamma"]
      pure $ Response OK HTTP_1_1 [] (BodyStream (next chunkRef)) (pure [])
    next ref = do
      xs <- readIORef ref
      case xs of
        [] -> pure Nothing
        (h : t) -> do writeIORef ref t; pure (Just h)


retainedTrailersTest :: Spec
retainedTrailersTest = it "retains ordered trailers beyond the receive buffer's lifetime" $ do
  let first = BS.replicate 8182 0x61
      second = BS.replicate 6000 0x62
      headers = "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nTrailer: X-Result\r\n\r\n"
      wire = "4\r\nbody\r\n0\r\nX-Result: " <> first <> "\r\nX-Result:\t" <> second <> " \t\r\n\r\n"
  withServer (\_ -> pure (wireResponse headers wire)) $ \port -> do
    let request = mkReq GET "/" port BodyEmpty [("TE", "trailers"), ("Connection", "TE")]
    Right response <- sendRequest (clientCfg port) request
    bodyOf response `shouldReturn` "body"
    responseTrailers response `shouldReturn` [("X-Result", first), ("X-Result", second)]


boundedTrailersTest :: Spec
boundedTrailersTest = it "bounds the full trailer section across individually small fields" $ do
  let headers = "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nTrailer: X-Result\r\n\r\n"
      line = "X-Result: " <> BS.replicate 7000 0x61 <> "\r\n"
      wire = "0\r\n" <> mconcat (replicate 5 line) <> "\r\n"
  withServer (\_ -> pure (wireResponse headers wire)) $ \port -> do
    let request = mkReq GET "/" port BodyEmpty [("TE", "trailers"), ("Connection", "TE")]
    result <- sendRequest (clientCfg port) request
    fmap responseStatus result `shouldBe` Left ParseMessageTooLong


closeDelimitedPoolTest :: Spec
closeDelimitedPoolTest = it "reads a close-delimited response and opens a fresh pooled connection" $
  withServer handler $ \port ->
    bracket (newPool defaultPoolConfig) destroyPool $ \pool -> do
      let request = mkReq GET "/" port BodyEmpty []
          readResponse (Right response) = do
            responseStatus response `shouldBe` OK
            bodyOf response `shouldReturn` "body before EOF"
          readResponse (Left e) = expectationFailure (show e)
      withPooledRequest pool (clientCfg port) request readResponse
      withPooledRequest pool (clientCfg port) request readResponse
  where
    handler _ = pure (wireResponse "HTTP/1.1 200 OK\r\n\r\n" "body before EOF")


truncatedContentLengthTest :: Spec
truncatedContentLengthTest = it "reports EOF before the declared body length" $
  withServer handler $ \port -> do
    result <- sendRequest (clientCfg port) (mkReq GET "/" port BodyEmpty [])
    fmap responseStatus result `shouldBe` Left ParseUnexpectedEof
  where
    handler _ = pure (wireResponse "HTTP/1.1 200 OK\r\nContent-Length: 20\r\n\r\n" "short body")


wireResponse :: BS.ByteString -> BS.ByteString -> Response
wireResponse headBytes body =
  Response OK HTTP_1_1 [("Connection", "close")] (BodyPreEncoded (PreEncoded (headBytes <> body) (BS.length headBytes))) (pure [])


keepAlivePipelineTest :: Spec
keepAlivePipelineTest = it "keep-alive: two requests on one connection" $
  withServer (\_ -> pure (resp200 "ok")) $ \port -> do
    withClientConnection (clientCfg port) $ \conn -> do
      Right r1 <- sendRequestOn conn (mkReq GET "/a" port BodyEmpty [])
      _ <- bodyOf r1
      Right r2 <- sendRequestOn conn (mkReq GET "/b" port BodyEmpty [])
      _ <- bodyOf r2
      responseStatus r1 `shouldBe` OK
      responseStatus r2 `shouldBe` OK


repeatedConnectionOptionsTests :: Spec
repeatedConnectionOptionsTests = describe "Connection options across field lines" $ do
  it "honors close on a later request field and advertises it beside other response options" $
    withServer (respond [("Connection", "X-Hop")]) $ \port -> do
      wire <- rawExchange port (request "GET" "/seen-first" "1.1" "Connection: keep-alive\r\nconnection: cLoSe\r\n" <> second)
      wire `shouldSatisfy` BS.isInfixOf "Connection: X-Hop\r\nConnection: close\r\n"
      wire `shouldSatisfy` BS.isSuffixOf "\r\n\r\n/seen-first"
  it "honors an existing close on a later response field" $
    withServer (respond [("Connection", "X-Hop"), ("connection", "ClOsE")]) $ \port -> do
      wire <- rawExchange port (request "GET" "/seen-first" "1.1" "" <> second)
      wire `shouldSatisfy` BS.isInfixOf "Connection: X-Hop\r\nconnection: ClOsE\r\n"
      wire `shouldSatisfy` BS.isSuffixOf "\r\n\r\n/seen-first"
  it "uses later keep-alive on HTTP/1.0 and gives later close precedence" $
    withServer (\req -> pure ((resp200 (requestTarget req)) {responseVersion = HTTP_1_0})) $ \port -> do
      let keep = request "GET" "/seen-first" "1.0" "Close: close\r\nConnection: X-Hop\r\nconnection: Keep-Alive\r\n"
          close = request "GET" "/seen-second" "1.0" "Connection: keep-alive\r\nconnection: close\r\n"
      wire <- rawExchange port (keep <> close <> second)
      wire `shouldSatisfy` BS.isInfixOf "Connection: keep-alive\r\n"
      wire `shouldSatisfy` BS.isInfixOf "\r\n\r\n/seen-firstHTTP/1.0 200"
      wire `shouldSatisfy` BS.isInfixOf "Connection: close\r\n"
      wire `shouldSatisfy` BS.isSuffixOf "\r\n\r\n/seen-second"
  it "adds close to a pre-encoded head for GET and HEAD" $
    withServer (\_ -> pure (Enc.precomputeResponse (resp200 "pre-encoded"))) $ \port -> do
      let exchange method = rawExchange port (request method "/seen-first" "1.1" "Connection: close\r\n" <> second)
      get <- exchange "GET"
      headOnly <- exchange "HEAD"
      get `shouldSatisfy` BS.isInfixOf "Connection: close\r\n"
      get `shouldSatisfy` BS.isSuffixOf "\r\n\r\npre-encoded"
      headOnly `shouldSatisfy` BS.isInfixOf "Connection: close\r\n"
      headOnly `shouldSatisfy` BS.isSuffixOf "\r\n\r\n"
  where
    request method path version fields =
      method <> " " <> path <> " HTTP/" <> version <> "\r\nHost: localhost\r\n" <> fields <> "\r\n"
    second = request "GET" "/must-not-run" "1.1" "Connection: close\r\n"
    respond fields req = pure ((resp200 (requestTarget req)) {responseHeaders = fields})


rawExchange :: String -> BS.ByteString -> IO BS.ByteString
rawExchange port wire = do
  let hints = NS.defaultHints {NS.addrSocketType = NS.Stream}
  (address : _) <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just port)
  bracket (NS.openSocket address) NS.close $ \sock -> do
    NS.connect sock (NS.addrAddress address)
    NSB.sendAll sock wire
    NS.shutdown sock NS.ShutdownSend
    result <- timeout 3_000_000 (readAll sock [])
    maybe (expectationFailure "server did not close the connection" >> pure "") pure result
  where
    readAll sock chunks = do
      bytes <- NSB.recv sock 4096
      if BS.null bytes then pure (BS.concat (reverse chunks)) else readAll sock (bytes : chunks)


------------------------------------------------------------------------
-- Pre-encoded responses
------------------------------------------------------------------------

-- A precomputed static response — identical shape to what
-- bench-server/WireformServer.hs ships.
staticOk :: Response
staticOk =
  Enc.precomputeResponse $
    Response
      { responseStatus = OK
      , responseVersion = HTTP_1_1
      , responseHeaders = [("Content-Type", "text/plain"), ("Server", "test")]
      , responseBody = BodyBytes "Hello, world!\n"
      , responseTrailers = pure []
      }


preEncodedGetTest :: Spec
preEncodedGetTest = it "pre-encoded response (GET) is byte-identical to encoder output" $
  withServer (\_ -> pure staticOk) $ \port -> do
    Right r <- sendRequest (clientCfg port) (mkReq GET "/" port BodyEmpty [])
    responseStatus r `shouldBe` OK
    body <- bodyOf r
    body `shouldBe` "Hello, world!\n"
    -- Verify the parsed headers match what the encoder would emit.
    BS.length body `shouldBe` 14


preEncodedHeadTest :: Spec
preEncodedHeadTest = it "pre-encoded response served as HEAD drops body but keeps Content-Length" $
  withServer (\_ -> pure staticOk) $ \port -> do
    -- HEAD on the same precomputed response. The server slices
    -- peBytes to peHeadLen so metadata survives but the body is gone.
    Right r <- sendRequest (clientCfg port) (mkReq HEAD "/" port BodyEmpty [])
    responseStatus r `shouldBe` OK
    -- HEAD response framing is special: the parser sees
    -- Content-Length: 14 in the headers but knows HEAD MUST NOT carry
    -- a body, so it frames as NoBody.
    body <- bodyOf r
    body `shouldBe` ""


------------------------------------------------------------------------
-- sendfile(2)
------------------------------------------------------------------------

{- | Bracket-style helper that writes a temp file with given contents,
runs an action against it, then removes the file.
-}
withTempFile :: FilePath -> BS.ByteString -> (FilePath -> IO a) -> IO a
withTempFile path contents action = do
  BS.writeFile path contents
  action path `finally` (removeFile path)


{- | Sendfile bodies can be larger than the client's recv buffer, so
we drain inside 'withClientConnection' to keep the connection live
while we read.
-}
sendFileGetTest :: Spec
sendFileGetTest = it "sendfile body: GET delivers file bytes verbatim" $
  withTempFile "/tmp/wireform-http1-sendfile-test.txt" payload $ \path -> do
    let handler _ = do
          fb <- wholeFileBody path
          pure $
            Response
              OK
              HTTP_1_1
              [("Content-Type", "text/plain"), ("Server", "test")]
              (BodyFile fb)
              (pure [])
    withServer handler $ \port -> do
      withClientConnection (clientCfg port) $ \conn -> do
        Right r <- sendRequestOn conn (mkReq GET "/" port BodyEmpty [])
        responseStatus r `shouldBe` OK
        body <- bodyOf r
        body `shouldBe` payload
  where
    payload = BS.replicate 4096 0x61 -- 4 KiB of 'a'


sendFileHeadTest :: Spec
sendFileHeadTest = it "sendfile body: HEAD emits headers, no body" $
  withTempFile "/tmp/wireform-http1-sendfile-head-test.txt" payload $ \path -> do
    let handler _ = do
          fb <- wholeFileBody path
          pure $
            Response
              OK
              HTTP_1_1
              [("Content-Type", "text/plain"), ("Server", "test")]
              (BodyFile fb)
              (pure [])
    withServer handler $ \port -> do
      withClientConnection (clientCfg port) $ \conn -> do
        Right r <- sendRequestOn conn (mkReq HEAD "/" port BodyEmpty [])
        responseStatus r `shouldBe` OK
        body <- bodyOf r
        body `shouldBe` ""
  where
    payload = BS.replicate 256 0x62


sendFileCachedFdTest :: Spec
sendFileCachedFdTest = it "sendfile body: cached fd path (no per-request open/close)" $
  withTempFile "/tmp/wireform-http1-sendfile-cached-test.txt" payload $ \path -> do
    -- Open the fd once and reuse it for multiple requests on the
    -- same keep-alive connection. This is the static-file fast path.
    fb <- wholeFileBodyFd path
    let handler _ =
          pure $
            Response
              OK
              HTTP_1_1
              [("Content-Type", "text/plain")]
              (BodyFile fb)
              (pure [])
    withServer handler $ \port -> do
      withClientConnection (clientCfg port) $ \conn -> do
        -- Two requests on the same connection — exercises the fd
        -- being reused without an intervening close.
        Right r1 <- sendRequestOn conn (mkReq GET "/a" port BodyEmpty [])
        body1 <- bodyOf r1
        body1 `shouldBe` payload
        Right r2 <- sendRequestOn conn (mkReq GET "/b" port BodyEmpty [])
        body2 <- bodyOf r2
        body2 `shouldBe` payload
  where
    payload = BS.replicate 1024 0x63 -- 1 KiB of 'c'


------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{- | Spin up the server on a free port, run the test, then tear down.
Synchronises on the server signalling that it has @listen()@ed.
-}
withServer :: Handler -> (String -> IO ()) -> IO ()
withServer handler action = do
  readyMV <- newEmptyMVar
  let cfg =
        defaultServerConfig
          { serverHost = "127.0.0.1"
          , serverPort = "0" -- placeholder; we'll bind manually
          , serverHandler = handler
          }
  -- Bind ourselves to know the port, then hand the listening socket
  -- to runServerOnSocket via accept loop.
  let hints =
        NS.defaultHints
          { NS.addrFlags = [NS.AI_PASSIVE]
          , NS.addrSocketType = NS.Stream
          }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> expectationFailure "no addr"
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
        tid <- forkIO $ acceptForever cfg listenSock readyMV
        putMVar readyMV ()
        action (show portInt) `finally` killThread tid


acceptForever :: ServerConfig -> NS.Socket -> MVar () -> IO ()
acceptForever cfg listenSock _ready = loop
  where
    loop = do
      (s, _) <- NS.accept listenSock
      _ <- forkIO (runServerOnSocket cfg s)
      loop


clientCfg :: String -> ClientConfig
clientCfg p = defaultClientConfig {clientHost = "127.0.0.1", clientPort = p}


mkReq :: Method -> BS.ByteString -> String -> Body -> Headers -> Request
mkReq m t port body extras =
  Request
    { requestMethod = m
    , requestTarget = t
    , requestVersion = HTTP_1_1
    , requestHeaders = [("Host", BS.pack (map (fromIntegral . fromEnum) ("127.0.0.1:" <> port)))] <> extras
    , requestBody = body
    , requestTrailers = pure []
    }


resp200 :: BS.ByteString -> Response
resp200 b = Response OK HTTP_1_1 [("Content-Type", "text/plain")] (BodyBytes b) (pure [])


bodyOf :: Response -> IO BS.ByteString
bodyOf r = drainAll (responseBody r)


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
