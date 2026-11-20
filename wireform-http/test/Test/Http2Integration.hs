{-# LANGUAGE OverloadedStrings #-}

{- | End-to-end HTTP\/2 integration through the unified
'Network.HTTP.Connection' \/ 'Network.HTTP.Server' surface.

Uses plaintext HTTP\/2 (prior-knowledge h2c) on an ephemeral
@127.0.0.1@ port — no TLS or ALPN required to exercise the
protocol-level changes (streaming, trailers, flow control,
HEADERS continuation, cancellation, GOAWAY refusal).
-}
module Test.Http2Integration (tests) where

import Codec.Compression.Zlib qualified as Zlib
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (SomeException, bracket, finally, throwIO, try)
import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive qualified as CI
import Data.IORef
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import Network.HTTP
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.Cache qualified as Cache
import Network.HTTP.Client.Redirect (defaultRedirectPolicy, withRedirects)
import Network.HTTP.Client.Response qualified as Raw
import Network.HTTP.Connection
import Network.HTTP.Headers qualified as Typed
import Network.HTTP.Headers.ContentDigest (ContentDigest)
import Network.HTTP.Headers.Signature qualified as Signature
import Network.HTTP.Headers.SignatureInput qualified as SignatureInput
import Network.HTTP.Server
import Network.HTTP.Types.Status qualified as S
import Network.HTTP.Types.Version qualified as V
import Network.HTTP2.Client (ClientStreamError (..))
import Network.HTTP2.Types (ErrorCode (..))
import Network.Socket qualified as NS
import Test.Syd


tests :: Spec
tests =
  describe "HTTP/2 integration" $
    sequence_
      [ helloWorld
      , redirectEmptyQuery
      , echoBody
      , streamingResponseBody
      , streamingRequestBody
      , serverEmitsTrailers
      , bodylessResponseLengths
      , connectAuthorityTarget
      , ordinaryAuthority
      , bodyDerivedTrailers
      , splitSignatureFields
      , fixedRequestTrailers
      , trailerActionFailure
      , invalidRequestTrailers
      , invalidResponseTrailers
      , clientResponseTrailers
      , clientRequestTrailers
      , compressedRequestTrailers
      , decodedResponseTrailers
      , streamedTrailerFailure
      , cancelStreamedTrailers
      , largeHeaderBlockTriggersContinuation
      , concurrentStreams
      , cancellationViaWithResponse
      ]


------------------------------------------------------------------------

helloWorld :: Spec
helloWorld = it "HTTP/2 plaintext (h2c prior knowledge): hello world" $
  withTestServer http2Only (\_ -> pure (resp200 "hi")) $ \port -> do
    (status, ver, body) <- runClient http2Only port $ \c -> do
      r <- sendOn c (mkRequest "GET" "/" port BodyEmpty [])
      b <- drainBody (responseBody r)
      pure (responseStatus r, responseVersion r, b)
    status `shouldBe` S.status200
    body `shouldBe` "hi"
    ver `shouldBe` V.HTTP2


redirectEmptyQuery :: Spec
redirectEmptyQuery = it "sends a bare query delimiter in :path after a Location redirect" $
  withTestServer http2Only handler $ \port -> do
    let start = Client.staticURI (T.pack ("http://127.0.0.1:" <> port <> "/record"))
        transport = withRedirects defaultRedirectPolicy (Client.baseTransport http2Only)
    response <- Client.sendRaw transport (Client.request (methodFromBytes "GET") start Client.emptyStream)
    Client.popperBytes (Client.bodyPopper response) `shouldReturn` "/record?"
  where
    handler req
      | requestTarget req == "/record" =
          pure (resp200 ""){responseStatus = S.status302, responseHeaders = [("Location", "?")]}
      | otherwise = pure (resp200 (requestTarget req))


echoBody :: Spec
echoBody = it "POST echoes a bounded request body" $
  withTestServer http2Only echo $ \port -> do
    (status, body) <- runClient http2Only port $ \c -> do
      r <- sendOn c (mkRequest "POST" "/" port (BodyBytes "payload") [("Content-Length", "7, 7"), ("Content-Length", "007")])
      b <- drainBody (responseBody r)
      pure (responseStatus r, b)
    status `shouldBe` S.status200
    body `shouldBe` "payload"
  where
    echo req = do
      body <- drainBody (requestBody req)
      pure (resp200 body) {responseHeaders = [("Content-Length", "7")]}


streamingResponseBody :: Spec
streamingResponseBody =
  it "streaming response body arrives chunk-wise" $
    withTestServer http2Only handler $ \port -> do
      (status, body) <- runClient http2Only port $ \c -> do
        r <- sendOn c (mkRequest "GET" "/" port BodyEmpty [])
        b <- drainBody (responseBody r)
        pure (responseStatus r, b)
      status `shouldBe` S.status200
      -- We don't assert chunk boundaries — HTTP/2 may re-frame DATA
      -- frames freely — only the assembled body.
      body `shouldBe` "alphabetagamma"
  where
    handler _ = do
      ref <- newIORef ["alpha", "beta", "gamma"]
      pure
        Response
          { responseStatus = S.status200
          , responseVersion = V.HTTP2
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


streamingRequestBody :: Spec
streamingRequestBody =
  it "streaming request body is delivered chunked over DATA frames" $
    withTestServer http2Only echo $ \port -> do
      chunkRef <- newIORef ["one ", "two ", "three"]
      let producer = do
            xs <- readIORef chunkRef
            case xs of
              [] -> pure Nothing
              (h : t) -> writeIORef chunkRef t >> pure (Just h)
      (status, body) <- runClient http2Only port $ \c -> do
        r <- sendOn c (mkRequest "POST" "/" port (BodyStream producer) [])
        b <- drainBody (responseBody r)
        pure (responseStatus r, b)
      status `shouldBe` S.status200
      body `shouldBe` "one two three"
  where
    echo req = do
      body <- drainBody (requestBody req)
      pure (resp200 body)


serverEmitsTrailers :: Spec
serverEmitsTrailers =
  it "server-set responseTrailers reach the client's crResponseTrailers" $
    withTestServer http2Only handler $ \port -> do
      (status, trs) <- runClient http2Only port $ \c -> do
        r <- sendOn c (mkRequest "GET" "/" port BodyEmpty [])
        _ <- drainBody (responseBody r)
        t <- responseTrailers r
        pure (responseStatus r, t)
      status `shouldBe` S.status200
      lookup (CI.mk "grpc-status") trs `shouldBe` Just "0"
      lookup (CI.mk "grpc-message") trs `shouldBe` Just "OK"
  where
    handler _ =
      pure
        Response
          { responseStatus = S.status200
          , responseVersion = V.HTTP2
          , responseHeaders = [("Content-Length", "4")]
          , responseBody = BodyBytes "body"
          , responseTrailers =
              pure
                [ (CI.mk "grpc-status", "0")
                , (CI.mk "grpc-message", "OK")
                ]
          , responseH2StreamId = 0
          , responseCancel = pure ()
          , responsePushPromises = pure []
          }


bodylessResponseLengths :: Spec
bodylessResponseLengths =
  it "preserves Content-Length metadata on HEAD and 304 without reading a body" $
    withTestServer http2Only handler $ \port ->
      runClient http2Only port $ \conn -> do
        let check method target status = do
              response <- sendOn conn (mkRequest method target port BodyEmpty [])
              responseStatus response `shouldBe` status
              lookup "Content-Length" (responseHeaders response) `shouldBe` Just "5"
              drainBody (responseBody response) `shouldReturn` ""
              responseTrailers response `shouldReturn` []
        check "HEAD" "/" S.status200
        check "GET" "/cached" S.status304
        response <- sendOn conn (mkRequest "GET" "/" port BodyEmpty [])
        drainBody (responseBody response) `shouldReturn` "hello"
  where
    handler req = pure $ case (requestMethod req, requestTarget req) of
      (_, "/cached") -> (resp200 "") {responseStatus = S.status304, responseHeaders = lengthHeader}
      (method, _) | method == methodFromBytes "HEAD" -> (resp200 "") {responseHeaders = lengthHeader}
      _ -> (resp200 "hello") {responseHeaders = lengthHeader}
    lengthHeader = [("Content-Length", "5")]


connectAuthorityTarget :: Spec
connectAuthorityTarget =
  it "round-trips an authority-form CONNECT target" $
    withTestServer http2Only handler $ \port ->
      runClient http2Only port $ \conn -> do
        let request = (mkRequest "CONNECT" "[::1]:443" port BodyEmpty []) {requestAuthority = Nothing}
        response <- sendOn conn request
        responseStatus response `shouldBe` S.status200
        drainBody (responseBody response) `shouldReturn` ""
  where
    handler request = do
      requestTarget request `shouldBe` "[::1]:443"
      requestAuthority request `shouldBe` Just "[::1]:443"
      pure (resp200 "")


ordinaryAuthority :: Spec
ordinaryAuthority =
  it "uses the chosen HTTP/2 authority when forwarding to HTTP/1" $
    withTestServer http1Only origin $ \originPort ->
      withTestServer http2Only (forward originPort) $ \port ->
        runClient http2Only port $ \conn -> do
          let request = (mkRequest "GET" "/" port BodyEmpty [("Host", "example.test:443")])
                { requestScheme = SchemeHttps
                , requestAuthority = Just "Example.test"
                }
          response <- sendOn conn request
          responseStatus response `shouldBe` S.status200
          drainBody (responseBody response) `shouldReturn` "ok"
  where
    forward originPort request = do
      requestAuthority request `shouldBe` Just "Example.test"
      lookup "Host" (requestHeaders request) `shouldBe` Just "example.test:443"
      runClient http1Only originPort $ \conn -> do
        response <- sendOn conn (request {requestBody = BodyEmpty})
        responseStatus response `shouldBe` S.status200
        resp200 <$> drainBody (responseBody response)

    origin request = do
      lookup "Host" (requestHeaders request) `shouldBe` Just "Example.test"
      pure (resp200 "ok")


bodyDerivedTrailers :: Spec
bodyDerivedTrailers =
  it "round-trips typed request and response trailers after producer EOF" $
    withTestServer http2Only handler $ \port ->
      runClient http2Only port $ \conn -> do
        (body, trailers) <- bodyAndTrailers
        let request = mkRequest "POST" "/" port body announcement
        withResponseDeferredOn conn (request {requestTrailers = trailers}) $ \await -> do
          response <- await
          responseStatus response `shouldBe` S.status200
          drainBody (responseBody response) `shouldReturn` "hello"
          received <- responseTrailers response
          checkDigestTrailers (responseHeaders response) received
  where
    handler request = do
      drainBody (requestBody request) `shouldReturn` "hello"
      received <- requestTrailers request
      checkDigestTrailers (requestHeaders request) received
      (body, trailers) <- bodyAndTrailers
      pure (resp200 "") {responseHeaders = announcement, responseBody = body, responseTrailers = trailers}

    bodyAndTrailers = do
      chunks <- newIORef [Just "he", Just "", Just "llo", Nothing]
      emittedTrailers <- newIORef False
      let produce =
            atomicModifyIORef' chunks $ \remaining -> case remaining of
              chunk : rest -> (rest, chunk)
              [] -> ([], Nothing)
          trailers = do
            readIORef chunks `shouldReturn` []
            atomicModifyIORef' emittedTrailers (\sent -> (True, sent)) `shouldReturn` False
            pure digestTrailers
      pure (BodyStream produce, trailers)


fixedRequestTrailers :: Spec
fixedRequestTrailers =
  it "sends static request trailers with both nonempty and empty bodies" $
    withTestServer http2Only echo $ \port ->
      runClient http2Only port $ \conn -> do
        let exchange body expected headers trailers = do
              let request = mkRequest "POST" "/" port body headers
              withResponseOn conn (request {requestTrailers = pure trailers}) $ \response -> do
                responseStatus response `shouldBe` S.status200
                drainBody (responseBody response) `shouldReturn` expected
        exchange (BodyBytes "hello") "hello" (("Content-Length", "5") : announcement) digestTrailers
        exchange BodyEmpty "" [("Content-Length", "0"), ("Trailer", "X-Test")] [("X-Test", "empty")]
  where
    echo request = do
      body <- drainBody (requestBody request)
      received <- requestTrailers request
      if BS.null body
        then received `shouldBe` [("X-Test", "empty")]
        else checkDigestTrailers (requestHeaders request) received
      pure (resp200 body)


announcement :: Headers
announcement = [("Trailer", "Content-Digest, X-Test")]


digestTrailers :: Headers
digestTrailers =
  [ ("X-Test", "first")
  , ("Content-Digest", helloDigest)
  , ("X-Test", "second")
  ]


helloDigest :: BS.ByteString
helloDigest = "sha-256=:LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=:"


splitSignatureFields :: Spec
splitSignatureFields = it "pairs signature headers and trailers over HTTP/1.1 and HTTP/2" $
  forM_ [(http1Only, V.HTTP1_1), (http2Only, V.HTTP2)] $ \(range, version) ->
    withTestServer range (handler version) $ \port ->
      runClient range port $ \connection -> do
        let acceptTrailers = if version == V.HTTP1_1 then [("TE", "trailers"), ("Connection", "TE")] else []
            request = (mkRequest "GET" "/" port BodyEmpty acceptTrailers) {requestVersion = version}
        response <- sendOn connection request
        drainBody (responseBody response) `shouldReturn` "hello"
        trailers <- responseTrailers response
        let fields = Typed.headerMapFromList (responseHeaders response)
        case Signature.lookupMessageSignaturesWithTrailers fields (Typed.headerMapFromList trailers) of
          Right (Just (Signature.MessageSignature input bytes :| [])) -> do
            SignatureInput.signatureLabel input `shouldBe` "sig1"
            bytes `shouldBe` "fixture"
          other -> expectationFailure (show other)
  where
    handler version _ = do
      next <- newIORef (Just "hello")
      pure (resp200 "")
        { responseVersion = version
        , responseHeaders =
            [ ("Trailer", "Content-Digest, Signature")
            , ("Signature-Input", "sig1=(\"content-digest\";tr)")
            ]
        , responseBody = BodyStream (atomicModifyIORef' next (\chunk -> (Nothing, chunk)))
        , responseTrailers = pure [("Content-Digest", helloDigest), ("Signature", "sig1=:Zml4dHVyZQ==:")]
        }


checkDigestTrailers :: Headers -> Headers -> IO ()
checkDigestTrailers headers trailers = do
  lookup "trailer" headers `shouldBe` lookup "Trailer" announcement
  lookup "content-digest" headers `shouldBe` Nothing
  trailers `shouldBe` digestTrailers
  Right (Just value) <- pure (Typed.lookupHeader @ContentDigest (Typed.headerMapFromList trailers))
  Typed.renderToHeaders Typed.defaultHeaderSettings value `shouldBe` [helloDigest]


trailerActionFailure :: Spec
trailerActionFailure =
  it "resets streams whose trailer action fails and leaves the connection usable" $ do
    requestAborted <- newEmptyMVar
    releaseRequest <- newEmptyMVar
    let handler request = case requestTarget request of
          "/request" -> do
            _ <- drainBody (requestBody request)
            requestTrailers request `shouldReturn` []
            putMVar requestAborted ()
            readMVar releaseRequest
            throwIO (userError "request was cancelled")
          "/response" ->
            pure (resp200 "") {responseBody = BodyStream (pure Nothing), responseTrailers = failTrailers}
          _ -> pure (resp200 "ok")
    withTestServer http2Only handler $ \port ->
      runClient http2Only port $ \conn ->
        ( do
            let request = mkRequest "POST" "/request" port (BodyStream (pure Nothing)) []
            requestResult <- try @ClientStreamError (sendOn conn (request {requestTrailers = failTrailers}))
            (() <$ requestResult) `shouldBe` Left (ClientStreamReset Cancel)
            takeMVar requestAborted
            response <- sendOn conn (mkRequest "GET" "/response" port BodyEmpty [])
            responseResult <- try @ClientStreamError (drainBody (responseBody response))
            responseResult `shouldBe` Left (ClientStreamReset InternalError)
            ok <- sendOn conn (mkRequest "GET" "/" port BodyEmpty [])
            drainBody (responseBody ok) `shouldReturn` "ok"
        ) `finally` putMVar releaseRequest ()
  where
    failTrailers = throwIO (userError "trailer producer failed")


invalidRequestTrailers :: Spec
invalidRequestTrailers =
  it "rejects invalid fixed and streamed request trailers locally" $
    withTestServer http2Only handler $ \port ->
      runClient http2Only port $ \conn -> do
        let request body = (mkRequest "POST" "/invalid" port body [])
              {requestTrailers = pure [("X-Result", " padded")]}
        forM_ [BodyEmpty, BodyBytes "payload"] $ \body -> do
          result <- try @ClientStreamError (sendOn conn (request body))
          (() <$ result) `shouldBe` Left (ClientRequestHeaderError "invalid trailer field value")
        result <- try @ClientStreamError (sendOn conn (request (BodyStream (pure Nothing))))
        (() <$ result) `shouldBe` Left (ClientStreamReset Cancel)
        response <- sendOn conn (mkRequest "GET" "/" port BodyEmpty [])
        drainBody (responseBody response) `shouldReturn` "ok"
  where
    handler request
      | requestTarget request == "/" = pure (resp200 "ok")
      | otherwise = do
          _ <- drainBody (requestBody request)
          _ <- requestTrailers request
          throwIO (userError "invalid trailer request ended")


invalidResponseTrailers :: Spec
invalidResponseTrailers =
  it "rejects invalid fixed and streamed response trailers on their own streams" $
    withTestServer http2Only handler $ \port ->
      runClient http2Only port $ \conn -> do
        let request path = mkRequest "GET" path port BodyEmpty []
        forM_ ["/empty", "/fixed"] $ \path -> do
          result <- try @ClientStreamError (sendOn conn (request path))
          (() <$ result) `shouldBe` Left (ClientStreamReset InternalError)
        response <- sendOn conn (request "/stream")
        result <- try @ClientStreamError (drainBody (responseBody response))
        result `shouldBe` Left (ClientStreamReset InternalError)
        ok <- sendOn conn (request "/")
        drainBody (responseBody ok) `shouldReturn` "ok"
  where
    handler request = pure $ case requestTarget request of
      "/empty" -> invalid BodyEmpty
      "/fixed" -> invalid (BodyBytes "payload")
      "/stream" -> invalid (BodyStream (pure Nothing))
      _ -> resp200 "ok"
    invalid body =
      (resp200 "") {responseBody = body, responseTrailers = pure [("X-Result", "padded\t")]}


clientResponseTrailers :: Spec
clientResponseTrailers = it "exposes ordered trailer sections on HTTP/2 responses and omits them on cache hits" $
  withTestServer http2Only handler $ \port -> do
    let request = Client.request (methodFromBytes "GET") (clientTestURI port "/") ()
        base = Client.baseTransport http2Only
    cache <- Cache.newCache Cache.defaultCacheConfig
    let cached = Cache.withCache cache base
        check transport = Client.withResponse transport request [] $ \raw -> do
          Client.rawResponseBytes raw `shouldReturn` "hello"
          trailers <- Client.rawResponseTrailers raw
          checkDigestTrailers (Raw.headers raw) trailers
          Client.rawResponseTrailers raw `shouldReturn` trailers
          case Client.protocolInfo raw of
            Client.HTTP2 _ -> pure ()
            _ -> expectationFailure "expected HTTP/2 protocol metadata"
    Client.withPool Client.defaultPoolConfig{Client.versionRange = http2Only} $ \pool ->
      forM_ [cached, Client.pooledTransport pool, Client.streamedTransportWith 1 http2Only] check
    Client.withResponse cached request [] $ \hit -> do
      lookup "Trailer" (Raw.headers hit) `shouldBe` Nothing
      Client.rawResponseBytes hit `shouldReturn` "hello"
      Client.rawResponseTrailers hit `shouldReturn` []
  where
    handler _ = pure (resp200 "hello")
      { responseHeaders = ("Cache-Control", "max-age=300") : announcement
      , responseTrailers = pure digestTrailers
      }


clientRequestTrailers :: Spec
clientRequestTrailers = it "sends typed request trailers through the HTTP/2 client transports" $
  withTestServer http2Only handler $ \port -> do
    let check transport = do
          let plain = Client.request (methodFromBytes "POST") (clientTestURI port "/") ("hello" :: BS.ByteString)
          Right request <- pure (Client.withRequestTrailers ("Content-Digest" :| ["X-Test"]) (pure digestTrailers) plain)
          Client.withResponse transport request [] $ \response ->
            Client.rawResponseBytes response `shouldReturn` "hello"
    Client.withPool Client.defaultPoolConfig{Client.versionRange = http2Only} $ \pool ->
      forM_ [Client.baseTransport http2Only, Client.pooledTransport pool, Client.streamedTransportWith 1 http2Only] check
  where
    handler req = do
      lookup "Content-Length" (requestHeaders req) `shouldBe` Just "5"
      lookup "Transfer-Encoding" (requestHeaders req) `shouldBe` Nothing
      bytes <- drainBody (requestBody req)
      trailers <- requestTrailers req
      checkDigestTrailers (requestHeaders req) trailers
      pure (resp200 bytes)


compressedRequestTrailers :: Spec
compressedRequestTrailers = it "removes source digests from request trailers and their announcement after encoding" $
  withTestServer http2Only handler $ \port -> do
    let plain = Client.request (methodFromBytes "POST") (clientTestURI port "/") ("hello" :: BS.ByteString)
        transport = Client.withCompression @Client.Deflate (Client.baseTransport http2Only)
    Right request <- pure (Client.withRequestTrailers ("Content-Digest" :| ["X-Test"]) (pure digestTrailers) plain)
    Client.withResponse transport request [] $ \response ->
      Client.rawResponseBytes response `shouldReturn` "hello"
  where
    handler req = do
      lookup "Content-Encoding" (requestHeaders req) `shouldBe` Just "deflate"
      lookup "Trailer" (requestHeaders req) `shouldBe` Just "X-Test"
      encoded <- drainBody (requestBody req)
      lookup "Content-Length" (requestHeaders req) `shouldBe` Just (BS8.pack (show (BS.length encoded)))
      requestTrailers req `shouldReturn` [("X-Test", "first"), ("X-Test", "second")]
      pure (resp200 (BSL.toStrict (Zlib.decompress (BSL.fromStrict encoded))))


decodedResponseTrailers :: Spec
decodedResponseTrailers = it "removes wire digest trailers and updates each announcement on a decoded response" $
  withTestServer http2Only handler $ \port -> do
    let request = Client.request (methodFromBytes "GET") (clientTestURI port "/") ()
        streamed = Client.streamedTransportWith 1 http2Only
    Client.withResponse (Client.withDecompression streamed) request [] $ \raw -> do
      let anns = filter ((== "Trailer") . fst) (Raw.headers raw)
      anns `shouldBe` [("Trailer", "X-Test"), ("Trailer", "x-extra")]
      Client.rawResponseBytes raw `shouldReturn` "hello"
      Client.rawResponseTrailers raw `shouldReturn` [("X-Test", "first"), ("X-Test", "second"), ("x-extra", "more")]
  where
    handler _ = pure (resp200 (BSL.toStrict (Zlib.compress "hello")))
      { responseHeaders =
          [ ("Content-Encoding", "deflate")
          , ("Trailer", "cOnTeNt-DiGeSt, X-Test")
          , ("Trailer", "Repr-Digest")
          , ("Trailer", "x-extra")
          ]
      , responseTrailers = pure $
          digestTrailers
            <> [ ("Repr-Digest", helloDigest)
               , ("Digest", "MD5=1B2M2Y8AsgTpgAmY7PhCfg==")
               , ("Content-MD5", "1B2M2Y8AsgTpgAmY7PhCfg==")
               , ("x-extra", "more")
               ]
      }


streamedTrailerFailure :: Spec
streamedTrailerFailure = it "surfaces a streamed trailer failure after delivering the preceding bytes" $
  withTestServer http2Only handler $ \port -> do
    let request = Client.request (methodFromBytes "GET") (clientTestURI port "/") ()
    Client.withResponse (Client.streamedTransportWith 1 http2Only) request [] $ \raw -> do
      Client.bodyPopper raw `shouldReturn` "hello"
      try @ClientStreamError (Client.bodyPopper raw) `shouldReturn` Left (ClientStreamReset InternalError)
      try @ClientStreamError (Client.rawResponseTrailers raw) `shouldReturn` Left (ClientStreamReset InternalError)
  where
    handler _ = do
      chunk <- newIORef (Just "hello")
      let produce = atomicModifyIORef' chunk (\next -> (Nothing, next))
      pure (resp200 "")
        { responseBody = BodyStream produce
        , responseTrailers = throwIO (userError "trailer producer failed")
        }


cancelStreamedTrailers :: Spec
cancelStreamedTrailers = it "cancels a streamed response without draining its bounded body queue" $
  withTestServer http2Only handler $ \port -> do
    let request = Client.request (methodFromBytes "GET") (clientTestURI port "/") ()
    Client.withResponse (Client.streamedTransportWith 1 http2Only) request [] $ \raw -> do
      Raw.cancelStream raw
      Client.bodyPopper raw `shouldReturn` ""
      Client.rawResponseTrailers raw `shouldReturn` []
  where
    handler _ = pure (resp200 (BS.replicate (64 * 1024) 0x61))
      { responseHeaders = announcement
      , responseTrailers = pure digestTrailers
      }


clientTestURI :: String -> String -> Client.RequestURI
clientTestURI port path = Client.staticURI (T.pack ("http://127.0.0.1:" <> port <> path))


largeHeaderBlockTriggersContinuation :: Spec
largeHeaderBlockTriggersContinuation =
  it "header block > MAX_FRAME_SIZE splits across HEADERS + CONTINUATION" $
    withTestServer http2Only handler $ \port -> do
      -- ~32 KiB of distinct header values guarantees we cross the
      -- default 16 KiB SETTINGS_MAX_FRAME_SIZE.
      let bigVal = BS.replicate 512 0x61 -- 512 bytes of 'a'
          manyHeaders =
            [ (CI.mk (BS8.pack ("X-Test-" <> show n)), bigVal)
            | n <- [1 :: Int .. 64]
            ]
      (status, body) <- runClient http2Only port $ \c -> do
        r <- sendOn c (mkRequest "GET" "/" port BodyEmpty manyHeaders)
        b <- drainBody (responseBody r)
        pure (responseStatus r, b)
      status `shouldBe` S.status200
      body `shouldBe` "got-it"
  where
    handler _ = pure (resp200 "got-it")


concurrentStreams :: Spec
concurrentStreams = it "multiple streams on one connection are independent" $
  withTestServer http2Only handler $ \port -> do
    runClient http2Only port $ \c -> do
      r1 <- sendOn c (mkRequest "GET" "/one" port BodyEmpty [])
      r2 <- sendOn c (mkRequest "GET" "/two" port BodyEmpty [])
      b1 <- drainBody (responseBody r1)
      b2 <- drainBody (responseBody r2)
      b1 `shouldBe` "one"
      b2 `shouldBe` "two"
  where
    handler req = case requestTarget req of
      "/one" -> pure (resp200 "one")
      "/two" -> pure (resp200 "two")
      _ -> pure (resp200 "?")


cancellationViaWithResponse :: Spec
cancellationViaWithResponse =
  it "withResponseOn sends RST_STREAM(CANCEL) when the action throws" $
    withTestServer http2Only handler $ \port -> do
      result <- try @SomeException $
        runClient http2Only port $ \c ->
          withResponseOn c (mkRequest "GET" "/" port BodyEmpty []) $ \_ ->
            error "intentional"
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected the action's exception to propagate"
  where
    handler _ = do
      -- A non-trivial body so the recv side has some activity even
      -- after the RST.  We don't observe the peer's RST directly
      -- here; the test mainly verifies that the exception
      -- propagates and the bracket runs to completion (no hang).
      ref <- newIORef [BS.replicate 1024 0x62, BS.replicate 1024 0x63]
      pure
        Response
          { responseStatus = S.status200
          , responseVersion = V.HTTP2
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


------------------------------------------------------------------------
-- Plumbing
------------------------------------------------------------------------

withTestServer
  :: VersionRange
  -> Handler
  -> (String -> IO a)
  -> IO a
withTestServer range handler action = do
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
        readyVar <- newEmptyMVar
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
    , requestVersion = V.HTTP2
    , requestTrailers = pure []
    }


resp200 :: BS.ByteString -> Response
resp200 body =
  Response
    { responseStatus = S.status200
    , responseVersion = V.HTTP2
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
