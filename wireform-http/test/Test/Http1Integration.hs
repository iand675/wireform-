{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | End-to-end HTTP\/1.x integration through the unified
'Network.HTTP.Connection' \/ 'Network.HTTP.Server' surface.

Each test runs a real TCP listener on @127.0.0.1@ with an
ephemeral port (@bind 0@) and drives the unified accept loop via
'runServerOnListener', so the test exercises the whole
client → server roundtrip including the conversion layer.
TLS is covered separately because it needs cert fixtures.
-}
module Test.Http1Integration (tests) where

import Codec.Compression.GZip qualified as GZip
import Control.Concurrent (forkFinally, forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (bracket, finally, throwIO, try)
import Control.Monad (forM_, replicateM_)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive qualified as CI
import Data.IORef
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Network.HTTP
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.Base (baseTransport)
import Network.HTTP.Client.Conditional qualified as Conditional
import Network.HTTP.Client.Expect qualified as Expect
import Network.HTTP.Client.Middleware (millis)
import Network.HTTP.Client.Range qualified as Range
import Network.HTTP.Client.Redirect (defaultRedirectPolicy, withRedirects)
import Network.HTTP.Client.Response qualified as Raw
import Network.HTTP.Client.Streaming (streamedTransport)
import Network.HTTP.Client.Transport (Transport, sendRaw)
import Network.HTTP.Connection
import Network.HTTP.Headers qualified as Typed
import Network.HTTP.Headers.ContentDigest (ContentDigest)
import Network.HTTP.Headers.Trailer (Trailer (..))
import Network.HTTP.Server
import Network.HTTP.Types.Status qualified as S
import Network.HTTP.Types.Version qualified as V
import Network.HTTP1.Parser qualified as H1
import Network.Socket qualified as NS
import Network.Socket.ByteString qualified as NSB
import System.Timeout (timeout)
import Test.Syd


tests :: Spec
tests =
  describe "HTTP/1.x integration" $
    sequence_
      [ helloWorld
      , echoBody
      , streamingResponse
      , chunkedRequestWithEmptyTrailers
      , chunkedDigestTrailers
      , bufferedResponseTrailers
      , clientResponseTrailers
      , clientRequestTrailers
      , requestTrailersRequireFraming
      , earlyFinalSkipsRequestTrailers
      , redirectsWithRequestTrailers
      , expectContinue
      , serverContinueOnRepeatedFields
      , serverContinueOnlyForHttp11Content
      , expectEarlyFinal
      , expectTimeoutWithPartialHead
      , expectClientTransports
      , redirectEmptyQuery
      , serverPreconditions
      , conditionalRangeAuthoring
      , redirectsConditionalRange
      , compressedRangeResponse
      , expectLengthMismatch
      , pooledCloseDelimited
      ]


------------------------------------------------------------------------

serverPreconditions :: Spec
serverPreconditions = it "applies origin preconditions before modifying or returning the resource" $ do
  actions <- newIORef (0 :: Int)
  let modified = UTCTime (fromGregorian 1994 11 6) 31777
      validators = Just (RepresentationValidators (Just (Conditional.strongETag "v1")) (Just modified))
      currentDate = "Sun, 06 Nov 1994 08:49:37 GMT"
      oldDate = "Sun, 06 Nov 1994 08:49:36 GMT"
      evaluate method status representation fields =
        evaluatePreconditions modified status representation (mkRequest method "/" "80" BodyEmpty fields)
      allowed = Right PreconditionsProceed
      denied = Right PreconditionsFailed
      unchanged = Right PreconditionsNotModified
  evaluate "PUT" S.status200 validators [(hIfMatch, "W/\"v1\""), (hIfUnmodifiedSince, currentDate)] `shouldBe` denied
  evaluate "PUT" S.status200 validators [(hIfMatch, "\"v1\""), (hIfUnmodifiedSince, oldDate)] `shouldBe` allowed
  evaluate "PUT" S.status200 validators [(hIfUnmodifiedSince, oldDate)] `shouldBe` denied
  evaluate "PUT" S.status200 validators [(hIfUnmodifiedSince, oldDate), (hIfUnmodifiedSince, oldDate)] `shouldBe` allowed
  evaluate "GET" S.status200 validators [(hIfNoneMatch, "\"old\""), (hIfNoneMatch, "W/\"v1\"")] `shouldBe` unchanged
  evaluate "GET" S.status200 validators
    [ ("IF-UNMODIFIED-SINCE", oldDate)
    , ("iF-MaTcH", "\"old\"")
    , ("If-Modified-Since", oldDate)
    , ("X-Request-Id", "between validators")
    , (hIfMatch, "\"v1\"")
    , (hIfNoneMatch, "W/\"v1\"")
    ] `shouldBe` unchanged
  evaluate "GET" S.status200 validators [(hIfModifiedSince, currentDate), ("If-Range", "\"v1\""), (hIfModifiedSince, currentDate)]
    `shouldBe` allowed
  evaluate "HEAD" S.status200 validators [(hIfModifiedSince, currentDate)] `shouldBe` unchanged
  evaluate "GET" S.status200 validators [(hIfNoneMatch, "\"old\""), (hIfModifiedSince, currentDate)] `shouldBe` allowed
  evaluate "GET" S.status200 validators [(hIfModifiedSince, currentDate <> ", " <> currentDate)] `shouldBe` allowed
  evaluate "GET" S.status200 validators [(hIfModifiedSince, "not a date")] `shouldBe` allowed
  evaluate "GET" S.status200 validators [(hIfModifiedSince, "Sunday, 06-Nov-94 08:49:37 GMT")] `shouldBe` unchanged
  evaluate "PUT" S.status200 validators [(hIfModifiedSince, currentDate)] `shouldBe` allowed
  evaluate "OPTIONS" S.status200 validators [(hIfMatch, "\"old\"")] `shouldBe` allowed
  evaluate "GET" S.status404 validators [(hIfMatch, "\"old\"")] `shouldBe` allowed
  evaluate "PUT" S.status201 Nothing [(hIfNoneMatch, "*")] `shouldBe` allowed
  evaluate "PUT" S.status200 (Just (RepresentationValidators Nothing Nothing)) [(hIfNoneMatch, "*")] `shouldBe` denied
  let handler request = case evaluatePreconditions modified S.status200 validators request of
        Right PreconditionsProceed -> modifyIORef' actions (+ 1) >> pure (resp200 "current")
        Right PreconditionsNotModified -> pure ((resp200 "") {responseStatus = S.status304})
        Right PreconditionsFailed -> pure ((resp200 "") {responseStatus = S.status412})
        Left _ -> pure ((resp200 "") {responseStatus = S.status400})
  withTestServer http1Only handler $ \port -> do
    forM_
      [ ("GET", [(hIfModifiedSince, currentDate)], S.status304)
      , ("PUT", [(hIfUnmodifiedSince, oldDate)], S.status412)
      , ("PUT", [(hIfNoneMatch, "W/\"v1\"")], S.status412)
      , ("GET", [(hIfNoneMatch, "* , \"v1\"")], S.status400)
      , ("PUT", [(hIfMatch, "\"v1\""), (hIfUnmodifiedSince, oldDate)], S.status200)
      ] $ \(method, fields, expected) ->
        runClient http1Only port $ \connection -> do
          reply <- sendOn connection (mkRequest method "/" port BodyEmpty fields)
          responseStatus reply `shouldBe` expected
          drainBody (responseBody reply) `shouldReturn` (if expected == S.status200 then "current" else "")
  readIORef actions `shouldReturn` 1


helloWorld :: Spec
helloWorld = it "GET hello world (HTTP/1.1)" $
  withTestServer http1Only (\_ -> pure (resp200 "hi")) $ \port -> do
    (status, ver, body) <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "GET" "/" port BodyEmpty [])
      b <- drainBody (responseBody r)
      pure (responseStatus r, responseVersion r, b)
    status `shouldBe` S.status200
    body `shouldBe` "hi"
    ver `shouldBe` V.HTTP1_1


echoBody :: Spec
echoBody = it "POST echoes Content-Length body" $
  withTestServer http1Only echo $ \port -> do
    (status, body) <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "POST" "/" port (BodyBytes "rountrip") [])
      b <- drainBody (responseBody r)
      pure (responseStatus r, b)
    status `shouldBe` S.status200
    body `shouldBe` "rountrip"
  where
    echo req = do
      body <- drainBody (requestBody req)
      pure (resp200 body)


streamingResponse :: Spec
streamingResponse = it "streaming response body arrives chunk-wise" $
  withTestServer http1Only stream $ \port -> do
    body <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "GET" "/stream" port BodyEmpty [])
      drainBody (responseBody r)
    body `shouldBe` "alphabetagamma"
  where
    stream _ = do
      ref <- newIORef ["alpha", "beta", "gamma"]
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


chunkedRequestWithEmptyTrailers :: Spec
chunkedRequestWithEmptyTrailers =
  it "chunked request body: handler sees [] trailers when none sent" $
    withTestServer http1Only handler $ \port -> do
      chunkRef <- newIORef ["one ", "two ", "three!"]
      let producer = do
            xs <- readIORef chunkRef
            case xs of
              [] -> pure Nothing
              (h : t) -> writeIORef chunkRef t >> pure (Just h)
      (status, body) <- runClient http1Only port $ \c -> do
        r <- sendOn c (mkRequest "POST" "/" port (BodyStream producer) [])
        b <- drainBody (responseBody r)
        pure (responseStatus r, b)
      status `shouldBe` S.status200
      body `shouldBe` "one two three!"
  where
    handler req = do
      body <- drainBody (requestBody req)
      trs <- requestTrailers req
      trs `shouldBe` []
      pure (resp200 body)


expectContinue :: Spec
expectContinue =
  it "Expect: 100-continue: server emits 100, client absorbs it" $
    withTestServer http1Only echo $ \port -> do
      (status, body) <- runClient http1Only port $ \c -> do
        r <-
          sendOn
            c
            ( mkRequest
                "POST"
                "/"
                port
                (BodyBytes "payload")
                [(CI.mk "Expect", "100-continue")]
            )
        b <- drainBody (responseBody r)
        pure (responseStatus r, b)
      -- The final response we observe is the handler's 200, not the
      -- interim 100; the underlying HTTP/1 client transparently
      -- skips informational responses.
      status `shouldBe` S.status200
      body `shouldBe` "payload"
  where
    echo req = do
      body <- drainBody (requestBody req)
      pure (resp200 body)


serverContinueOnRepeatedFields :: Spec
serverContinueOnRepeatedFields =
  it "Expect: server sends 100 for an expectation on a later line" $
    withTestServer http1Only echo $ \port ->
      withRawClient port $ \peer -> do
        NSB.sendAll peer "POST / HTTP/1.1\r\nHost: localhost\r\nContent-Length: 7\r\nExpect: extension=ready\r\nExpect: , 100-Continue\r\n\r\n"
        receiveUntil peer (BS.isInfixOf "\r\n\r\n") `shouldReturn` "HTTP/1.1 100 Continue\r\n\r\n"
        NSB.sendAll peer "payload"
        response <- receiveUntil peer (BS.isInfixOf "\r\n\r\npayload")
        response `shouldSatisfy` BS.isPrefixOf "HTTP/1.1 200 "
  where
    echo req = resp200 <$> drainBody (requestBody req)


serverContinueOnlyForHttp11Content :: Spec
serverContinueOnlyForHttp11Content =
  it "Expect: server replies directly to HTTP/1.0 and empty HTTP/1.1 requests" $
    withTestServer http1Only echo $ \port -> do
      withRawClient port $ \peer -> do
        NSB.sendAll peer "POST / HTTP/1.0\r\nContent-Length: 7\r\nExpect: 100-continue\r\n\r\npayload"
        response <- receiveUntil peer (BS.isInfixOf "\r\n\r\npayload")
        response `shouldSatisfy` BS.isPrefixOf "HTTP/1.0 200 "
      withRawClient port $ \peer -> do
        NSB.sendAll peer "POST / HTTP/1.1\r\nHost: localhost\r\nContent-Length: 0\r\nExpect: 100-continue\r\n\r\n"
        response <- receiveUntil peer (BS.isInfixOf "\r\n\r\n")
        response `shouldSatisfy` BS.isPrefixOf "HTTP/1.1 200 "
  where
    echo req = resp200 <$> drainBody (requestBody req)


expectEarlyFinal :: Spec
expectEarlyFinal =
  it "Expect: 103 followed by a final response leaves the producer untouched" $
    withRawPeer peer $ \port -> do
      bodyChunk <- newIORef (Just "payload")
      let produce = atomicModifyIORef' bodyChunk (\next -> (Nothing, next))
          request = mkRequest "POST" "/" port (BodyStream produce) [("Expect", "extension=ready"), ("Expect", "100-Continue")]
      runClient http1Only port $ \conn -> do
        withResponseOn conn request $ \response -> do
          responseStatus response `shouldBe` S.status417
          drainBody (responseBody response) `shouldReturn` BS.empty
        readIORef bodyChunk `shouldReturn` Just "payload"
        canReuseHttp1Connection conn `shouldReturn` False
  where
    peer sock = do
      _ <- receiveUntil sock (BS.isInfixOf "\r\n\r\n")
      NSB.sendAll sock "HTTP/1.1 103 Early Hints\r\nLink: </style.css>; rel=preload\r\n\r\nHTTP/1.1 417 Expectation Failed\r\nContent-Length: 0\r\n\r\n"
      NSB.recv sock 4096 `shouldReturn` BS.empty


expectTimeoutWithPartialHead :: Spec
expectTimeoutWithPartialHead =
  it "Expect: timeout sends the body and preserves a partial response head" $
    withRawPeer peer $ \port ->
      runClient http1Only port $ \conn -> do
        let request = mkRequest "POST" "/" port (BodyBytes "payload") [("Expect", "100-continue")]
        response <- sendOn conn request
        responseStatus response `shouldBe` S.status200
        drainBody (responseBody response) `shouldReturn` BS.empty
  where
    peer sock = do
      headBytes <- receiveUntil sock (BS.isInfixOf "\r\n\r\n")
      headBytes `shouldSatisfy` BS.isSuffixOf "\r\n\r\n"
      NSB.sendAll sock "HTTP/1.1 103 Early Hints\r\n\r\nHTTP/1.1 2"
      receiveUntil sock ((>= 7) . BS.length) `shouldReturn` "payload"
      NSB.sendAll sock "00 OK\r\nContent-Length: 0\r\n\r\n"


redirectEmptyQuery :: Spec
redirectEmptyQuery = it "sends a bare query delimiter after a Location redirect" $
  withTestServer http1Only handler $ \port -> do
    let start = Client.staticURI (T.pack ("http://127.0.0.1:" <> port <> "/record?old=1"))
        transport = withRedirects defaultRedirectPolicy (baseTransport http1Only)
    response <- sendRaw transport (Client.request (methodFromBytes "GET") start Client.emptyStream)
    Client.popperBytes (Raw.bodyPopper response) `shouldReturn` "/record?"
  where
    handler req
      | requestTarget req == "/record?old=1" =
          pure (resp200 ""){responseStatus = S.status302, responseHeaders = [("Location", "?")]}
      | otherwise = pure (resp200 (requestTarget req))


conditionalRangeAuthoring :: Spec
conditionalRangeAuthoring = it "sends a conditional range only with a usable validator" $
  withTestServer http1Only handler $ \port -> do
    let uri = Client.staticURI (T.pack ("http://127.0.0.1:" <> port <> "/download"))
        original = Client.request (methodFromBytes "GET") uri Client.emptyStream
        withBytes = Range.withRange [Range.byteRangeFrom 10]
        withTag = Conditional.ifRange . Conditional.IfRangeETag
        strong = withTag (Conditional.strongETag "v1")
        send req = do
          response <- sendRaw (baseTransport http1Only) req
          Client.popperBytes (Raw.bodyPopper response)
    send (strong (withBytes original)) `shouldReturn` "bytes=10-|\"v1\""
    send (withBytes (strong original)) `shouldReturn` "bytes=10-|\"v1\""
    send (withBytes (withTag (Conditional.weakETag "v1") original)) `shouldReturn` "|"
    send (withTag (Conditional.strongETag "v\"1") (withBytes original)) `shouldReturn` "|"
    send (strong original) `shouldReturn` "|"
    send (strong (Client.setHeader "Range" "bytes=10-2" original)) `shouldReturn` "|"
    send (Client.addHeader "Range" "bytes=4-" (strong (withBytes original))) `shouldReturn` "|"
    send (Client.addHeader "If-Range" "\"v2\"" (strong (withBytes original))) `shouldReturn` "|"
    send (withBytes original) `shouldReturn` "bytes=10-|"
  where
    handler req =
      let field name = maybe "" id (lookup name (requestHeaders req))
      in pure (resp200 (field "range" <> "|" <> field "if-range"))


redirectsConditionalRange :: Spec
redirectsConditionalRange = it "refetches a redirected resource even when its entity tag collides" $
  withTestServer http1Only handler $ \port -> do
    let uri = Client.staticURI (T.pack ("http://127.0.0.1:" <> port <> "/old"))
        validator = Conditional.IfRangeETag (Conditional.strongETag "revision-1")
        req = Conditional.ifRange validator
          (Range.withRange [Range.byteRangeFrom 4] (Client.request (methodFromBytes "GET") uri Client.emptyStream))
    response <- sendRaw (withRedirects defaultRedirectPolicy (baseTransport http1Only)) req
    Raw.statusCode response `shouldBe` S.status200
    Client.popperBytes (Raw.bodyPopper response) `shouldReturn` "replacement body"
  where
    handler req
      | requestTarget req == "/old" =
          pure (resp200 ""){responseStatus = S.status307, responseHeaders = [("Location", "/different")]}
      | lookup "range" (requestHeaders req) == Just "bytes=4-"
      , lookup "if-range" (requestHeaders req) == Just "\"revision-1\"" =
          pure (resp200 "acement body")
            { responseStatus = S.status206
            , responseHeaders = [("Content-Range", "bytes 4-15/16"), ("ETag", "\"revision-1\"")]
            }
      | otherwise = pure (resp200 "replacement body"){responseHeaders = [("ETag", "\"revision-1\"")]}


compressedRangeResponse :: Spec
compressedRangeResponse = it "keeps a coded byte range intact and prefers identity by default" $
  withTestServer http1Only handler $ \port -> do
    let uri = Client.staticURI (T.pack ("http://127.0.0.1:" <> port <> "/download"))
        request = Range.withRange [Range.byteRangeFrom 5] (Client.request (methodFromBytes "GET") uri Client.emptyStream)
        transport = Client.withDecompression (baseTransport http1Only)
    coded <- sendRaw transport (Client.setHeader "Accept-Encoding" "gzip" request)
    Raw.statusCode coded `shouldBe` S.status206
    lookup "Content-Encoding" (Raw.headers coded) `shouldBe` Just "gzip"
    lookup "Content-Range" (Raw.headers coded) `shouldBe` Just codedRange
    Client.popperBytes (Raw.bodyPopper coded) `shouldReturn` codedPart
    plain <- sendRaw transport request
    Raw.statusCode plain `shouldBe` S.status206
    lookup "Content-Range" (Raw.headers plain) `shouldBe` Just "bytes 5-7/8"
    Client.popperBytes (Raw.bodyPopper plain) `shouldReturn` "fgh"
  where
    wire = BSL.toStrict (GZip.compress "abcdefgh")
    codedPart = BS.drop 5 wire
    codedRange = "bytes 5-" <> BS8.pack (show (BS.length wire - 1)) <> "/" <> BS8.pack (show (BS.length wire))
    partial bytes fields = (resp200 bytes){responseStatus = S.status206, responseHeaders = fields}
    handler req = pure $ case lookup "Accept-Encoding" (requestHeaders req) of
      Just "gzip" -> partial codedPart [("Content-Encoding", "gzip"), ("Content-Range", codedRange)]
      Just "identity" -> partial "fgh" [("Content-Range", "bytes 5-7/8")]
      _ -> (resp200 ""){responseStatus = S.status406}


expectClientTransports :: Spec
expectClientTransports = describe "Expect middleware through HTTP/1.1 transports" $ do
  cases "base" (\test -> test (baseTransport http1Only))
  cases "pooled" (\test -> Client.withPool Client.defaultPoolConfig (test . Client.pooledTransport))
  cases "streamed" (\test -> test (streamedTransport http1Only))
  where
    cases name withTransport = describe name $ do
      it "keeps a known-length upload unread after an early final response" $
        withTransport $ \inner ->
          withRawPeer rejectBody $ \port -> do
            chunks <- newIORef ["pay", "load"]
            response <- sendWithExpect inner Expect.defaultExpectConfig port chunks
            Raw.statusCode response `shouldBe` S.status417
            Raw.bodyPopper response `shouldReturn` ""
            readIORef chunks `shouldReturn` ["pay", "load"]
      it "uses the configured timeout and sends fixed-length bytes" $
        withTransport $ \inner ->
          withRawPeer receiveBody $ \port -> do
            chunks <- newIORef ["pay", "load"]
            let config = Expect.defaultExpectConfig {Expect.expectTimeout = millis 25}
            response <- sendWithExpect inner config port chunks
            Raw.statusCode response `shouldBe` S.status200
            Raw.bodyPopper response `shouldReturn` ""
            readIORef chunks `shouldReturn` []

    sendWithExpect :: Transport IO -> Expect.ExpectConfig -> String -> IORef [BS.ByteString] -> IO Raw.RawResponse
    sendWithExpect inner config port chunks =
      let uri = Client.staticURI (T.pack ("http://127.0.0.1:" <> port <> "/"))
          produce = atomicModifyIORef' chunks $ \case
            [] -> ([], "")
            chunk : rest -> (rest, chunk)
          request = Client.request (methodFromBytes "POST") uri (Client.BodyStream produce (Just 7))
      in sendRaw (Expect.withExpectContinue config {Expect.expectMinBodyBytes = 1} inner) request

    readHead sock = do
      wire <- receiveUntil sock (BS.isInfixOf "\r\n\r\n")
      let (headBytes, rest) = BS.breakSubstring "\r\n\r\n" wire
      headBytes `shouldSatisfy` BS.isInfixOf "\r\nContent-Length: 7"
      headBytes `shouldSatisfy` BS.isInfixOf "\r\nExpect: 100-continue"
      pure (BS.drop 4 rest)

    rejectBody sock = do
      readHead sock `shouldReturn` ""
      NSB.sendAll sock "HTTP/1.1 417 Expectation Failed\r\nContent-Length: 0\r\n\r\n"
      NSB.recv sock 4096 `shouldReturn` ""

    receiveBody sock = do
      received <- readHead sock
      let remaining = 7 - BS.length received
      tailBytes <- if remaining <= 0
        then pure (Just "")
        else timeout 650000 (receiveUntil sock ((>= remaining) . BS.length))
      fmap (received <>) tailBytes `shouldBe` Just "payload"
      NSB.sendAll sock "HTTP/1.1 200 OK\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"


expectLengthMismatch :: Spec
expectLengthMismatch = it "retires an expected fixed-length upload before sending a surplus chunk" $
  withRawPeer peer $ \port -> runClient http1Only port $ \conn -> do
    let request = mkRequest "POST" "/" port (BodyStream (pure (Just "payload")))
          [("Expect", "100-continue"), ("Content-Length", "4")]
    sent <- try (sendOn conn request)
    case sent of
      Left (ConnectionParseError H1.ParseInvalidLength) -> pure ()
      Left err -> expectationFailure (show err)
      Right _ -> expectationFailure "expected the fixed-length upload to fail"
    canReuseHttp1Connection conn `shouldReturn` False
  where
    peer sock = do
      _ <- receiveUntil sock (BS.isInfixOf "\r\n\r\n")
      NSB.sendAll sock "HTTP/1.1 100 Continue\r\n\r\n"
      NSB.recv sock 4096 `shouldReturn` ""


withRawPeer :: (NS.Socket -> IO ()) -> (String -> IO a) -> IO a
withRawPeer peer action = withLoopbackListener $ \listener port -> do
  result <- newEmptyMVar
  let serve = bracket (fst <$> NS.accept listener) NS.close peer
  bracket (forkFinally serve (putMVar result)) killThread $ \_ -> do
    value <- action port
    takeMVar result >>= either throwIO pure
    pure value


withRawClient :: String -> (NS.Socket -> IO a) -> IO a
withRawClient port action =
  bracket (NS.socket NS.AF_INET NS.Stream NS.defaultProtocol) NS.close $ \peer -> do
    NS.connect peer (NS.SockAddrInet (fromIntegral (read port :: Int)) (NS.tupleToHostAddress (127, 0, 0, 1)))
    action peer


receiveUntil :: NS.Socket -> (BS.ByteString -> Bool) -> IO BS.ByteString
receiveUntil sock complete = go BS.empty
  where
    go received
      | complete received = pure received
      | otherwise = do
          chunk <- NSB.recv sock 4096
          if BS.null chunk
            then fail "peer closed before sending the expected bytes"
            else go (received <> chunk)


chunkedDigestTrailers :: Spec
chunkedDigestTrailers = it "round-trips typed body-derived trailers in both directions" $
  withTestServer http1Only handler $ \port -> do
    (produce, trailers) <- bodyAndDigest
    let request = mkRequest "POST" "/" port (BodyStream produce) [("Trailer", "Content-Digest"), ("TE", "trailers"), ("Connection", "TE")]
    (announcement, receivedTrailers) <- runClient http1Only port $ \conn -> do
      response <- sendOn conn (request {requestTrailers = trailers})
      drainBody (responseBody response) `shouldReturn` "hello"
      received <- responseTrailers response
      pure (responseHeaders response, received)
    checkTrailer announcement receivedTrailers
  where
    digest = "sha-256=:LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=:"

    bodyAndDigest = do
      state <- newIORef (Just "hello")
      ended <- newIORef False
      let produce = do
            chunk <- atomicModifyIORef' state (\next -> (Nothing, next))
            case chunk of
              Nothing -> writeIORef ended True
              Just _ -> pure ()
            pure chunk
          trailers = do
            readIORef ended `shouldReturn` True
            pure [("Content-Digest", digest)]
      pure (produce, trailers)

    handler req = do
      drainBody (requestBody req) `shouldReturn` "hello"
      received <- requestTrailers req
      checkTrailer (requestHeaders req) received
      (produce, trailers) <- bodyAndDigest
      pure $ (resp200 "") {responseHeaders = [("Trailer", "Content-Digest")], responseBody = BodyStream produce, responseTrailers = trailers}

    checkTrailer announcement trailers = do
      Typed.lookupHeader @Trailer (toTypedHeaders announcement) `shouldBe` Right (Just (Trailer ("Content-Digest" :| [])))
      lookup "content-digest" trailers `shouldBe` Just digest
      Right (Just value) <- pure (Typed.lookupHeader @ContentDigest (toTypedHeaders trailers))
      Typed.renderToHeaders Typed.defaultHeaderSettings value `shouldBe` [digest]

    toTypedHeaders = Typed.headerMapFromList


bufferedResponseTrailers :: Spec
bufferedResponseTrailers = it "uses chunked framing for declared trailers on buffered responses" $ do
  produced <- newIORef (0 :: Int)
  let encoded = BSL.toStrict (GZip.compress "buffered")
      handler request
        | requestTarget request == "/plain" = pure (resp200 "plain")
        | otherwise = do
            let payload = case requestTarget request of
                  "/empty" -> ""
                  "/coding" -> encoded
                  _ -> "buffered"
                outgoing = if BS.null payload then BodyEmpty else BodyBytes payload
                trailers = modifyIORef' produced (+ 1) >> pure [("X-Result", "done")]
                framing = case requestTarget request of
                  "/explicit" -> [("Transfer-Encoding", "chunked")]
                  "/coding" -> [("Transfer-Encoding", "gzip")]
                  _ -> [("trailer", ""), ("Trailer", "X-Result")]
            pure $
              (resp200 payload)
                { responseHeaders =
                    ("Content-Length", BS8.pack (show (BS.length payload))) : framing
                , responseBody = outgoing
                , responseTrailers = trailers
                }
  withTestServer http1Only handler $ \port ->
    runClient http1Only port $ \conn -> do
      let fetch method path = sendOn conn (mkRequest method path port BodyEmpty [("TE", "trailers"), ("Connection", "TE")])
          cases =
            [ ("/buffered", "buffered", ["chunked"])
            , ("/explicit", "buffered", ["chunked"])
            , ("/coding", encoded, ["gzip", "chunked"])
            , ("/empty", "", ["chunked"])
            ]
      forM_ cases $ \(path, payload, codings) -> do
        response <- fetch "GET" path
        responseStatus response `shouldBe` S.status200
        map snd (filter ((== "Transfer-Encoding") . fst) (responseHeaders response)) `shouldBe` codings
        lookup "Content-Length" (responseHeaders response) `shouldBe` Nothing
        drainBody (responseBody response) `shouldReturn` payload
        responseTrailers response `shouldReturn` [("X-Result", "done")]
      headResponse <- fetch "HEAD" "/buffered"
      drainBody (responseBody headResponse) `shouldReturn` ""
      responseTrailers headResponse `shouldReturn` []
      (fetch "GET" "/plain" >>= drainBody . responseBody) `shouldReturn` "plain"
      readIORef produced `shouldReturn` 4


clientResponseTrailers :: Spec
clientResponseTrailers = it "exposes response trailers through base, pooled, and streamed clients" $
  withTestServer http1Only handler $ \port -> do
    let uri = Client.staticURI (T.pack ("http://127.0.0.1:" <> port <> "/"))
        request = Client.addHeader "TE" "trailers" $
          Client.addHeader "Connection" "TE" $
            Client.request (methodFromBytes "GET") uri ()
        check transport = do
          response <- Client.sendIO transport request (Client.as @Client.PlainText @Text)
          Client.responseBody response `shouldBe` "hello"
          Client.responseTrailers response `shouldReturn` trailers
    Client.withPool Client.defaultPoolConfig $ \pool ->
      forM_ [baseTransport http1Only, Client.pooledTransport pool, Client.streamedTransportWith 1 http1Only] check
  where
    digest = "sha-256=:LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=:"
    trailers = [("Content-Digest", digest), ("X-Result", "done")]
    handler _ = do
      pending <- newIORef [Just "he", Just "llo"]
      let produce = atomicModifyIORef' pending $ \chunks -> case chunks of
            [] -> ([], Nothing)
            first : rest -> (rest, first)
      pure (resp200 "")
        { responseHeaders = [("Content-Type", "text/plain"), ("Trailer", "Content-Digest, X-Result")]
        , responseBody = BodyStream produce
        , responseTrailers = pure trailers
        }


clientRequestTrailers :: Spec
clientRequestTrailers = it "sends empty and nonempty trailer-bearing uploads through HTTP/1.1 transports" $
  withTestServer http1Only handler $ \port -> do
    let uri = Client.staticURI (T.pack ("http://127.0.0.1:" <> port <> "/"))
        check transport = do
          forM_ [[], ["he", "llo"]] $ \chunks -> do
            producer <- Client.popperFromList chunks
            atEOF <- newIORef False
            calls <- newIORef (0 :: Int)
            let payload = BS.concat chunks
                pull = do
                  chunk <- producer
                  if BS.null chunk then writeIORef atEOF True else pure ()
                  pure chunk
                stream = Client.BodyStream pull (Just (fromIntegral (BS.length payload)))
                trailers = do
                  readIORef atEOF `shouldReturn` True
                  modifyIORef' calls (+ 1)
                  pure [("X-Result", "one"), ("X-Result", "two")]
                plain = Client.request (methodFromBytes "POST") uri stream
            Right request <- pure (Client.withRequestTrailers ("X-Result" :| []) trailers plain)
            Client.withResponse transport request [] $ \response ->
              Client.rawResponseBytes response `shouldReturn` payload
            readIORef calls `shouldReturn` 1
    Client.withPool Client.defaultPoolConfig $ \pool ->
      forM_ [baseTransport http1Only, Client.pooledTransport pool, Client.streamedTransportWith 1 http1Only] check
  where
    handler req = do
      let fields = requestHeaders req
      lookup "Trailer" fields `shouldBe` Just "X-Result"
      lookup "Transfer-Encoding" fields `shouldBe` Just "chunked"
      lookup "Content-Length" fields `shouldBe` Nothing
      bytes <- drainBody (requestBody req)
      requestTrailers req `shouldReturn` [("X-Result", "one"), ("X-Result", "two")]
      pure (resp200 bytes)


requestTrailersRequireFraming :: Spec
requestTrailersRequireFraming = it "rejects trailer uploads on HTTP/1.0 before writing the request" $
  withRawPeer (\sock -> NSB.recv sock 4096 `shouldReturn` "") $ \port -> do
    let uri = Client.staticURI (T.pack ("http://127.0.0.1:" <> port <> "/"))
        plain = Client.request (methodFromBytes "POST") uri ()
        produce = throwIO (userError "unexpected trailer production")
    Right request <- pure (Client.withRequestTrailers ("X-Result" :| []) produce plain)
    result <- try @Client.RequestTrailerError (Client.sendRawIO (baseTransport (versionRange (V.HTTP1_0 :| []))) request [])
    (() <$ result) `shouldBe` Left (Client.RequestTrailersUnsupportedVersion V.HTTP1_0)


earlyFinalSkipsRequestTrailers :: Spec
earlyFinalSkipsRequestTrailers = it "leaves the upload and its trailers unread after an early final response" $
  withRawPeer peer $ \port -> do
    let uri = Client.staticURI (T.pack ("http://127.0.0.1:" <> port <> "/"))
        source = Client.BodyStream (throwIO (userError "unexpected body pull")) (Just 5)
        plain = Client.setHeader "Expect" "100-continue" (Client.request (methodFromBytes "POST") uri source)
        produce = throwIO (userError "unexpected trailer production")
    Right request <- pure (Client.withRequestTrailers ("X-Result" :| []) produce plain)
    response <- Client.sendRawIO (baseTransport http1Only) request []
    Raw.statusCode response `shouldBe` S.status417
  where
    peer sock = do
      wire <- receiveUntil sock (BS.isInfixOf "\r\n\r\n")
      wire `shouldSatisfy` BS.isInfixOf "\r\nTrailer: X-Result\r\n"
      wire `shouldSatisfy` BS.isInfixOf "\r\nTransfer-Encoding: chunked\r\n"
      BS.drop 4 (snd (BS.breakSubstring "\r\n\r\n" wire)) `shouldBe` ""
      NSB.sendAll sock "HTTP/1.1 417 Expectation Failed\r\nContent-Length: 0\r\n\r\n"
      NSB.recv sock 4096 `shouldReturn` ""


redirectsWithRequestTrailers :: Spec
redirectsWithRequestTrailers = it "resends trailers on a 307 and clears them on a 303 rewrite" $
  withTestServer http1Only handler $ \port ->
    forM_ ["/keep", "/rewrite"] $ \path -> do
      let uri = Client.staticURI (T.pack ("http://127.0.0.1:" <> port <> path))
          plain = Client.request (methodFromBytes "POST") uri ("hello" :: BS.ByteString)
          transport = withRedirects defaultRedirectPolicy (baseTransport http1Only)
      calls <- newIORef (0 :: Int)
      Right request <- pure (Client.withRequestTrailers ("X-Result" :| []) (modifyIORef' calls (+ 1) >> pure fields) plain)
      response <- Client.sendRawIO transport request []
      Client.rawResponseBytes response `shouldReturn` (if path == "/keep" then "POST" else "GET")
      readIORef calls `shouldReturn` (if path == "/keep" then 2 else 1)
  where
    fields = [("X-Result", "done")]
    handler req = do
      bytes <- drainBody (requestBody req)
      let redirected code = (resp200 "") {responseStatus = S.Status code, responseHeaders = [("Location", "/last")]}
          requestMethodBytes = fromMethod (requestMethod req)
      if requestMethodBytes == "POST"
        then do
          bytes `shouldBe` "hello"
          lookup "Trailer" (requestHeaders req) `shouldBe` Just "X-Result"
          requestTrailers req `shouldReturn` fields
        else do
          bytes `shouldBe` ""
          lookup "Trailer" (requestHeaders req) `shouldBe` Nothing
          requestTrailers req `shouldReturn` []
      pure $ case requestTarget req of
        "/keep" -> redirected 307
        "/rewrite" -> redirected 303
        _ -> resp200 requestMethodBytes


------------------------------------------------------------------------
-- Plumbing
------------------------------------------------------------------------

pooledCloseDelimited :: Spec
pooledCloseDelimited = describe "aggregate pool persistence" $ do
  it "retires close-delimited connections" $
    poolRetiresAfter ""
  it "honors close on a later Connection field line" $
    poolRetiresAfter "Content-Length: 15\r\nConnection: keep-alive\r\nConnection: close\r\n"


poolRetiresAfter :: BS.ByteString -> IO ()
poolRetiresAfter extraHeaders =
  withLoopbackListener $ \listener port -> do
    peerRead <- newEmptyMVar
    let serve = replicateM_ 2 $
          bracket (fst <$> NS.accept listener) NS.close $ \peer -> do
            readRequestHead peer BS.empty
            NSB.sendAll peer ("HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n" <> extraHeaders <> "\r\nbody before EOF")
            NS.shutdown peer NS.ShutdownSend
            NSB.recv peer 4096 >>= putMVar peerRead
        uri = Client.staticURI (T.pack ("http://127.0.0.1:" <> port <> "/"))
        request = Client.request (methodFromBytes "GET") uri ()
    bracket (forkIO serve) killThread $ \_ ->
      Client.withPool Client.defaultPoolConfig $ \pool ->
        replicateM_ 2 $ do
          response <- Client.sendIO (Client.pooledTransport pool) request (Client.as @Client.PlainText @Text)
          Client.responseBody response `shouldBe` "body before EOF"
          timeout 2000000 (takeMVar peerRead) `shouldReturn` Just BS.empty
  where
    readRequestHead peer received
      | "\r\n\r\n" `BS.isInfixOf` received = pure ()
      | otherwise = do
          chunk <- NSB.recv peer 4096
          if BS.null chunk
            then expectationFailure "client closed before sending the request head"
            else readRequestHead peer (received <> chunk)


{- | Bind an ephemeral TCP port, spawn the unified accept loop on
it, and pass the resolved port string to the action.  The
listener is torn down on exit.
-}
withTestServer
  :: VersionRange
  -> Handler
  -> (String -> IO a)
  -> IO a
withTestServer range handler action = withLoopbackListener $ \listenSock portStr -> do
  let cfg =
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


withLoopbackListener :: (NS.Socket -> String -> IO a) -> IO a
withLoopbackListener action = do
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
        action listenSock portStr


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


resp200 :: BS.ByteString -> Response
resp200 body =
  Response
    { responseStatus = S.status200
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
