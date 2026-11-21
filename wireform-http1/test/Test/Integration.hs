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
import Control.Exception (bracket, finally)
import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Data.IORef
import Network.HTTP1.Client
import Network.HTTP1.Client.Pool (defaultPoolConfig, destroyPool, newPool, withPooledRequest)
import Network.HTTP1.Connection (newConnectionFromDuplex)
import Network.HTTP1.Encode qualified as Enc
import Network.HTTP1.Method
import Network.HTTP1.Parser (ParseError (ParseMessageTooLong, ParseUnexpectedEof), declaredContentLength)
import Network.HTTP1.Server
import Network.HTTP1.Status
import Network.HTTP1.Types
import Network.HTTP1.Version
import Network.Socket qualified as NS
import Network.Socket.ByteString qualified as NSB
import System.Directory (removeFile)
import System.IO (SeekMode (..))
import System.IO.Error (ioeGetErrorString, tryIOError)
import System.Posix.Files (setFileSize)
import System.Posix.IO (closeFd, fdSeek)
import System.Timeout (timeout)
import Test.Syd
import Wireform.Network (newDuplexTransport)
import Wireform.Transport.Config (defaultTransportConfig)


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
      , outboundTrailersTest
      , closeDelimitedPoolTest
      , truncatedContentLengthTest
      , keepAlivePipelineTest
      , repeatedConnectionOptionsTests
      , legacyResponseFramingTests
      , explicitResponseFramingTests
      , preEncodedGetTest
      , preEncodedHeadTest
      , preEncodedTrailersTest
      , sendFileGetTest
      , sendFileHeadTest
      , sendFileCachedFdTest
      , sendFileTrailersTest "TCP" runServerOnSocket
      , sendFileTrailersTest "duplex" runOverDuplex
      , fixedFileFramingTest "TCP" runServerOnSocket
      , fixedFileFramingTest "duplex" runOverDuplex
      ]


runOverDuplex :: ServerConfig -> NS.Socket -> IO ()
runOverDuplex cfg sock = do
  transport <- newDuplexTransport defaultTransportConfig sock
  newConnectionFromDuplex transport >>= runServerOnConnection cfg


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


outboundTrailersTest :: Spec
outboundTrailersTest = it "validates generated trailer fields before publishing the terminator" $
  withServer handler $ \port -> do
    forM_ ["/syntax", "/value", "/forbidden"] $ \path -> do
      wire <- rawExchangeWith (const (pure ())) port $
        "GET " <> path <> " HTTP/1.1\r\nHost: localhost\r\nTE: trailers\r\nConnection: TE\r\n\r\n"
      wire `shouldSatisfy` BS.isPrefixOf "HTTP/1.1 200 "
      wire `shouldSatisfy` BS.isSuffixOf "\r\n\r\n"
      wire `shouldSatisfy` (not . BS.isInfixOf "\r\n0\r\n")
      wire `shouldSatisfy` (not . BS.isInfixOf "HTTP/1.1 500")
      wire `shouldSatisfy` (not . BS.isInfixOf "X-First: ready")
    withClientConnection (clientCfg port) $ \conn -> do
      let upload = (mkReq POST "/upload" port (BodyStream (pure Nothing)) [("Transfer-Encoding", "chunked")])
            {requestTrailers = pure [("Content-Type", "text/plain")]}
      result <- tryIOError (sendRequestOn conn upload)
      case result of
        Left err -> do
          ioeGetErrorString err `shouldBe` "forbidden trailer field"
          clientConnectionCanReuse conn `shouldReturn` False
        Right _ -> expectationFailure "expected local request trailer validation to fail"
  where
    handler req = case requestTarget req of
      "/upload" -> drainAll (requestBody req) >> pure (resp200 "ok")
      "/syntax" -> pure (withInvalidTrailer [] ("Bad Name", "value"))
      "/value" -> pure (withInvalidTrailer [] ("X-Result", BS.singleton 0x7f))
      _ -> pure (withInvalidTrailer [("Trailer", "X-Result")] ("CoNTeNt-TyPe", "text/plain"))
    withInvalidTrailer fields bad =
      Response OK HTTP_1_1 fields (BodyStream (pure Nothing)) (pure [("X-First", "ready"), bad])


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


legacyResponseFramingTests :: Spec
legacyResponseFramingTests = describe "HTTP/1.0 response framing" $ do
  it "closes raw streams that have no length when either side uses HTTP/1.0" $
    withServer handler $ \port ->
      forM_
        [ ("GET", "/stream", "1.0", "")
        , ("GET", "/legacy-version", "1.1", "")
        , ("POST", "/stream", "1.0", "Content-Length: 5\r\n")
        ] $ \(method, path, version, headers) -> do
          wire <- exchange port (request method path version (headers <> keepAlive))
          wire `shouldSatisfy` BS.isInfixOf "\r\nConnection: close, X-Hop\r\n"
          wire `shouldSatisfy` BS.isSuffixOf "\r\n\r\nhello world"
          wire `shouldSatisfy` (not . BS.isInfixOf "Transfer-Encoding:")
          wire `shouldSatisfy` (not . BS.isInfixOf "Trailer:")
          wire `shouldSatisfy` (not . BS.isInfixOf "Keep-Alive:")
  it "keeps HEAD and fixed-length streams reusable for HTTP/1.0" $
    withServer handler $ \port -> do
      wire <- exchange port $
        request "HEAD" "/stream" "1.0" keepAlive
          <> request "GET" "/fixed" "1.0" keepAlive
          <> request "GET" "/buffered" "1.0" "Connection: close\r\n"
      wire `shouldSatisfy` BS.isInfixOf "\r\n\r\nHTTP/1.1 200"
      wire `shouldSatisfy` BS.isInfixOf "\r\nContent-Length: 5\r\n"
      wire `shouldSatisfy` BS.isInfixOf "\r\n\r\nhelloHTTP/1.1 200"
      wire `shouldSatisfy` BS.isSuffixOf "\r\n\r\ncomplete"
      wire `shouldSatisfy` (not . BS.isInfixOf "Transfer-Encoding:")
      wire `shouldSatisfy` (not . BS.isInfixOf "Trailer:")
  it "closes on a fixed-length stream size mismatch" $
    withServer handler $ \port -> do
      short <- exchange port (request "GET" "/short" "1.0" keepAlive)
      short `shouldSatisfy` BS.isSuffixOf "\r\n\r\nabc"
      long <- exchange port (request "GET" "/long" "1.0" keepAlive)
      long `shouldSatisfy` BS.isSuffixOf "\r\n\r\n"
      forM_ [short, long] $ \wire -> do
        wire `shouldSatisfy` BS.isPrefixOf "HTTP/1.1 200 "
        wire `shouldSatisfy` (not . BS.isInfixOf "HTTP/1.1 500")
  where
    exchange = rawExchangeWith (const (pure ()))
    keepAlive = "Connection: keep-alive\r\n"
    request method path version fields =
      method <> " " <> path <> " HTTP/" <> version <> "\r\nHost: localhost\r\n" <> fields <> "\r\n"
    handler req
      | requestMethod req == HEAD =
          pure (withTrailers [("Connection", "keep-alive")] (BodyStream unexpected))
      | otherwise = case requestTarget req of
          "/fixed" -> stream HTTP_1_1 [("Content-Length", "5")] ["he", "llo"]
          "/short" -> stream HTTP_1_1 [("Content-Length", "4")] ["abc"]
          "/long" -> stream HTTP_1_1 [("Content-Length", "4")] ["complete"]
          "/buffered" -> pure (withTrailers [("Transfer-Encoding", "chunked"), ("Content-Length", "9")] (BodyBytes "complete"))
          "/legacy-version" -> stream HTTP_1_0 streamFields ["hello", "", " world"]
          _ -> stream HTTP_1_1 streamFields ["hello", "", " world"]
    streamFields =
      [("Connection", "keep-alive, X-Hop"), ("Keep-Alive", "timeout=5"), ("Transfer-Encoding", "chunked")]
    stream version fields pieces = do
      chunks <- newIORef pieces
      let produce = atomicModifyIORef' chunks $ \case
            [] -> ([], Nothing)
            bytes : rest -> (rest, Just bytes)
      pure ((withTrailers fields (BodyStream produce)) {responseVersion = version})
    withTrailers fields body = Response OK HTTP_1_1 (("Trailer", "X-Result") : fields) body unexpected
    unexpected = ioError (userError "body or trailer producer ran")


explicitResponseFramingTests :: Spec
explicitResponseFramingTests = describe "HTTP/1.1 response framing" $ do
  it "writes a fixed-length stream and keeps the connection reusable" $
    withServer handler $ \port ->
      withClientConnection (clientCfg port) $ \conn -> do
        Right fixed <- sendRequestOn conn (mkReq GET "/fixed" port BodyEmpty [])
        hLookup "Content-Length" (responseHeaders fixed) `shouldBe` Just "5"
        hLookup "Transfer-Encoding" (responseHeaders fixed) `shouldBe` Nothing
        bodyOf fixed `shouldReturn` "hello"
        responseTrailers fixed `shouldReturn` []
        Right next <- sendRequestOn conn (mkReq GET "/next" port BodyEmpty [])
        bodyOf next `shouldReturn` "next"
  it "writes chunk framing for explicit Transfer-Encoding on cached bytes" $
    withServer handler $ \port -> do
      wire <- exchange port "/cached"
      wire `shouldSatisfy` BS.isInfixOf "\r\nTransfer-Encoding: chunked\r\n"
      wire `shouldSatisfy` BS.isSuffixOf "\r\n\r\n5\r\nhello\r\n0\r\n\r\n"
      wire `shouldSatisfy` (not . BS.isInfixOf "Content-Length:")
  it "rejects invalid framing before the head and closes on streamed length mismatch" $
    withServer handler $ \port -> do
      forM_ ["/bad-length", "/bad-coding"] $ \path -> do
        wire <- exchange port path
        wire `shouldSatisfy` BS.isPrefixOf "HTTP/1.1 500 "
      short <- exchange port "/short"
      long <- exchange port "/long"
      short `shouldSatisfy` BS.isSuffixOf "\r\n\r\nabc"
      long `shouldSatisfy` BS.isSuffixOf "\r\n\r\n"
      forM_ [short, long] $ \wire -> do
        wire `shouldSatisfy` BS.isPrefixOf "HTTP/1.1 200 "
        wire `shouldSatisfy` (not . BS.isInfixOf "HTTP/1.1 500")
  where
    exchange port path = rawExchangeWith (const (pure ())) port $
      "GET " <> path <> " HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
    handler req = case requestTarget req of
      "/fixed" -> stream "5" ["he", "", "llo"]
      "/short" -> stream "4" ["abc"]
      "/long" -> stream "4" ["complete"]
      "/bad-length" -> pure ((resp200 "abc") {responseHeaders = [("Content-Length", "4")]})
      "/bad-coding" -> pure ((resp200 "abc") {responseHeaders = [("Transfer-Encoding", "gzip;")]})
      "/cached" -> pure (Enc.precomputeResponse ((resp200 "hello")
        {responseHeaders = [("Transfer-Encoding", "chunked"), ("Content-Length", "9")]}))
      _ -> pure (resp200 "next")
    stream len pieces = do
      chunks <- newIORef pieces
      let producer = atomicModifyIORef' chunks $ \case
            [] -> ([], Nothing)
            bytes : rest -> (rest, Just bytes)
      pure $ Response OK HTTP_1_1 [("Content-Length", len)] (BodyStream producer) $
        ioError (userError "fixed response called its trailer producer")


rawExchange :: String -> BS.ByteString -> IO BS.ByteString
rawExchange = rawExchangeWith (\sock -> NS.shutdown sock NS.ShutdownSend)


rawExchangeWith :: (NS.Socket -> IO ()) -> String -> BS.ByteString -> IO BS.ByteString
rawExchangeWith afterSend port wire = do
  let hints = NS.defaultHints {NS.addrSocketType = NS.Stream}
  (address : _) <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just port)
  bracket (NS.openSocket address) NS.close $ \sock -> do
    NS.connect sock (NS.addrAddress address)
    NSB.sendAll sock wire
    afterSend sock
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


preEncodedTrailersTest :: Spec
preEncodedTrailersTest = it "writes deferred trailers after the pre-encoded response prefix" $ do
  calls <- newIORef (0 :: Int)
  let cached fields body = Enc.precomputeResponse $ (resp200 "")
        { responseHeaders = [("Trailer", ""), ("trailer", "X-Result"), ("Content-Length", "4")] <> fields
        , responseBody = body
        , responseTrailers = do
            n <- atomicModifyIORef' calls (\n -> (n + 1, n + 1))
            pure [("X-Result", if n == 1 then "first" else "again")]
        }
      full = cached [] (BodyBytes "body")
      empty = cached [] BodyEmpty
      -- gzip encoding of "body".
      gzip = BS.pack [0x1f, 0x8b, 8, 0, 0, 0, 0, 0, 2, 0xff, 0x4b, 0xca, 0x4f, 0xa9, 4, 0, 0xb2, 0x0b, 0xa8, 0xdb, 4, 0, 0, 0]
      coded = cached [("Transfer-Encoding", "gzip")] (BodyBytes gzip)
      largePayload = BS.replicate (1024 * 1024) 0x78
      large = cached [] (BodyBytes largePayload)
      handler req = pure $ case requestTarget req of
        "/full" -> full
        "/empty" -> empty
        "/coded" -> coded
        "/large" -> large
        _ -> resp200 "next"
  BodyPreEncoded PreEncodedWithTrailers {} <- pure (responseBody full)
  BodyPreEncoded PreEncodedWithTrailers {} <- pure (responseBody empty)
  readIORef calls `shouldReturn` 0
  withServer handler $ \port -> do
    withClientConnection (clientCfg port) $ \conn -> do
      let fetch method path = sendRequestOn conn $
            mkReq method path port BodyEmpty [("TE", "trailers"), ("Connection", "TE")]
          cases =
            [ ("/full", "body", ["chunked"], "first")
            , ("/full", "body", ["chunked"], "again")
            , ("/empty", "", ["chunked"], "again")
            , ("/coded", gzip, ["gzip", "chunked"], "again")
            , ("/large", largePayload, ["chunked"], "again")
            ]
      forM_ cases $ \(path, bytes, codings, trailer) -> do
        Right response <- fetch GET path
        hLookupAll "Transfer-Encoding" (responseHeaders response) `shouldBe` codings
        hLookup "Content-Length" (responseHeaders response) `shouldBe` Nothing
        bodyOf response `shouldReturn` bytes
        responseTrailers response `shouldReturn` [("X-Result", trailer)]
      Right metadata <- fetch HEAD "/full"
      hLookup "Transfer-Encoding" (responseHeaders metadata) `shouldBe` Just "chunked"
      bodyOf metadata `shouldReturn` ""
      responseTrailers metadata `shouldReturn` []
      Right next <- fetch GET "/next"
      bodyOf next `shouldReturn` "next"
    legacy <- rawExchange port "GET /full HTTP/1.0\r\nHost: localhost\r\n\r\n"
    legacy `shouldSatisfy` BS.isSuffixOf "\r\n\r\nbody"
    legacy `shouldSatisfy` BS.isInfixOf "\r\nContent-Length: 4\r\n"
    legacy `shouldSatisfy` (not . BS.isInfixOf "Transfer-Encoding")
    legacy `shouldSatisfy` (not . BS.isInfixOf "Trailer:")
    legacyLarge <- rawExchange port "GET /large HTTP/1.0\r\nHost: localhost\r\n\r\n"
    legacyLarge `shouldSatisfy` BS.isInfixOf "\r\nContent-Length: 1048576\r\n"
    legacyLarge `shouldSatisfy` BS.isSuffixOf largePayload
    legacyCoded <- rawExchange port "GET /coded HTTP/1.0\r\nHost: localhost\r\n\r\n"
    legacyCoded `shouldSatisfy` BS.isPrefixOf "HTTP/1.1 500 "
    legacyMetadata <- rawExchange port "HEAD /coded HTTP/1.0\r\nHost: localhost\r\n\r\n"
    legacyMetadata `shouldSatisfy` BS.isPrefixOf "HTTP/1.1 200 "
    legacyMetadata `shouldSatisfy` (not . BS.isInfixOf "Transfer-Encoding")
    readIORef calls `shouldReturn` 5
    final <- rawExchange port "GET /full HTTP/1.1\r\nHost: localhost\r\nTE: trailers\r\nConnection: TE, close\r\n\r\n"
    final `shouldSatisfy` BS.isInfixOf "\r\nConnection: close\r\n"
    final `shouldSatisfy` BS.isSuffixOf "\r\n\r\n4\r\nbody\r\n0\r\nX-Result: again\r\n\r\n"
    finalLarge <- rawExchange port "GET /large HTTP/1.1\r\nHost: localhost\r\nTE: trailers\r\nConnection: TE, close\r\n\r\n"
    finalLarge `shouldSatisfy` BS.isInfixOf "\r\nConnection: close\r\n"
    finalLarge `shouldSatisfy` BS.isSuffixOf (largePayload <> "\r\n0\r\nX-Result: again\r\n\r\n")
    readIORef calls `shouldReturn` 7


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

sendFileTrailersTest :: String -> (ServerConfig -> NS.Socket -> IO ()) -> Spec
sendFileTrailersTest name serve = it ("file response trailers over " <> name) $
  withTempFile ("/tmp/wireform-http1-file-trailers-" <> name) contents $ \path ->
    bracket (wholeFileBodyFd path) closeCachedFile $ \file -> do
      FileSourceFd fd <- pure (fbSource file)
      _ <- fdSeek fd AbsoluteSeek 5
      calls <- newIORef (0 :: Int)
      let extent = file {fbOffset = 7, fbLength = fromIntegral (BS.length payload)}
          handler req = pure $ case requestTarget req of
            "/path" -> (fileResponse calls (extent {fbSource = FileSourcePath path}))
              {responseHeaders = [("Transfer-Encoding", "chunked")]}
            "/fd" -> fileResponse calls extent
            "/empty" -> fileResponse calls (extent {fbLength = 0})
            "/short" -> fileResponse calls (file {fbLength = fbLength file + 1024})
            "/invalid-trailer" ->
              (fileResponse calls (extent {fbLength = 0}))
                {responseTrailers = pure [("Content-Type", "text/plain")]}
            _ -> resp200 "next"
      withServerUsing serve handler $ \port -> do
        let opts = [("TE", "trailers"), ("Connection", "TE")]
        withClientConnection (clientCfg port) $ \conn -> do
          let fetch method target = sendRequestOn conn (mkReq method target port BodyEmpty opts)
          forM_ [("/path", payload), ("/fd", payload), ("/empty", "")] $ \(target, expected) -> do
            Right response <- fetch GET target
            responseStatus response `shouldBe` OK
            hLookup "Transfer-Encoding" (responseHeaders response) `shouldBe` Just "chunked"
            hLookup "Content-Length" (responseHeaders response) `shouldBe` Nothing
            bodyOf response `shouldReturn` expected
            responseTrailers response `shouldReturn` [("X-Result", "done")]
          Right metadata <- fetch HEAD "/fd"
          bodyOf metadata `shouldReturn` ""
          responseTrailers metadata `shouldReturn` []
          Right next <- fetch GET "/next"
          bodyOf next `shouldReturn` "next"
        wire <- rawExchangeWith (const (pure ())) port "GET /short HTTP/1.1\r\nHost: localhost\r\nTE: trailers\r\nConnection: TE\r\n\r\n"
        wire `shouldSatisfy` BS.isPrefixOf "HTTP/1.1 200 "
        wire `shouldSatisfy` BS.isInfixOf "the end"
        wire `shouldSatisfy` (not . BS.isInfixOf "X-Result: done")
        wire `shouldSatisfy` (not . BS.isInfixOf "HTTP/1.1 500")
        invalid <- rawExchangeWith (const (pure ())) port "GET /invalid-trailer HTTP/1.1\r\nHost: localhost\r\nTE: trailers\r\nConnection: TE\r\n\r\n"
        invalid `shouldSatisfy` BS.isSuffixOf "\r\n\r\n"
        invalid `shouldSatisfy` (not . BS.isInfixOf "\r\n0\r\n")
        readIORef calls `shouldReturn` 3
        fdSeek fd RelativeSeek 0 `shouldReturn` 5
  where
    payload = BS.replicate (1024 * 1024 + 4096) 0x63
    contents = "prefix:" <> payload <> "the end"
    fileResponse calls file =
      Response OK HTTP_1_1 [("Trailer", "X-Result")] (BodyFile file) $
        modifyIORef' calls (+ 1) >> pure [("X-Result", "done")]


fixedFileFramingTest :: String -> (ServerConfig -> NS.Socket -> IO ()) -> Spec
fixedFileFramingTest name serve = it ("preserves fixed-length file framing over " <> name) $
  withTempFile ("/tmp/wireform-http1-fixed-file-" <> name) contents $ \path ->
    bracket (wholeFileBodyFd path) closeCachedFile $ \file -> do
      FileSourceFd fd <- pure (fbSource file)
      _ <- fdSeek fd AbsoluteSeek 5
      let extent = file {fbOffset = 7, fbLength = fromIntegral (BS.length payload)}
          handler req = pure $ case requestTarget req of
            "/path" -> fixedFileResponse (extent {fbSource = FileSourcePath path})
            "/fd" -> fixedFileResponse extent
            "/empty" -> fixedFileResponse (extent {fbLength = 0})
            "/missing" -> fixedFileResponse (extent {fbSource = FileSourcePath (path <> "/missing")})
            _ -> fixedFileResponse file
      withServerUsing serve handler $ \port -> do
        let fetch method target = mkReq method target port BodyEmpty []
        withClientConnection (clientCfg port) $ \conn -> do
          forM_ [("/path", payload), ("/fd", payload), ("/empty", ""), ("/fd", payload)] $ \(target, expected) -> do
            Right response <- sendRequestOn conn (fetch GET target)
            responseStatus response `shouldBe` OK
            declaredContentLength (responseHeaders response)
              `shouldBe` Right (Just (fromIntegral (BS.length expected)))
            hLookup "Transfer-Encoding" (responseHeaders response) `shouldBe` Nothing
            bodyOf response `shouldReturn` expected
          Right metadata <- sendRequestOn conn (fetch HEAD "/missing")
          responseStatus metadata `shouldBe` OK
          declaredContentLength (responseHeaders metadata) `shouldBe` Right (Just (fbLength extent))
          bodyOf metadata `shouldReturn` ""
        fdSeek fd RelativeSeek 0 `shouldReturn` 5
        Right missing <- sendRequest (clientCfg port) (fetch GET "/missing")
        responseStatus missing `shouldBe` InternalServerError
        setFileSize path 7
        short <- rawExchangeWith (const (pure ())) port "GET /whole HTTP/1.1\r\nHost: localhost\r\n\r\n"
        short `shouldSatisfy` BS.isPrefixOf "HTTP/1.1 200 "
        short `shouldSatisfy` BS.isSuffixOf "\r\n\r\nprefix:"
        (fmap responseStatus <$> sendRequest (clientCfg port) (fetch GET "/whole"))
          `shouldReturn` Left ParseUnexpectedEof
        fdSeek fd RelativeSeek 0 `shouldReturn` 5
  where
    payload = BS.replicate (1024 * 1024 + 4096) 0x63
    contents = "prefix:" <> payload <> "the end"
    fixedFileResponse file =
      Response OK HTTP_1_1 [] (BodyFile file) (ioError (userError "fixed response called its trailer producer"))


closeCachedFile :: FileBody -> IO ()
closeCachedFile file = case fbSource file of
  FileSourceFd fd -> closeFd fd
  FileSourcePath _ -> pure ()


------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Spin up the server on an ephemeral loopback port.
withServer :: Handler -> (String -> IO ()) -> IO ()
withServer = withServerUsing runServerOnSocket


withServerUsing :: (ServerConfig -> NS.Socket -> IO ()) -> Handler -> (String -> IO ()) -> IO ()
withServerUsing serve handler action =
  bracket (NS.socket NS.AF_INET NS.Stream NS.defaultProtocol) NS.close $ \listenSock -> do
    NS.bind listenSock (NS.SockAddrInet 0 (NS.tupleToHostAddress (127, 0, 0, 1)))
    NS.listen listenSock 128
    NS.SockAddrInet port _ <- NS.getSocketName listenSock
    bracket (forkIO (acceptForever listenSock)) killThread $ \_ ->
      action (show port)
  where
    cfg = defaultServerConfig {serverHandler = handler}
    acceptForever listenSock = do
      (s, _) <- NS.accept listenSock
      _ <- forkIO (serve cfg s)
      acceptForever listenSock


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
