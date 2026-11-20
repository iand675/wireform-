{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Loopback (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception (SomeException, bracket, finally, try)
import Control.Monad (forM, forM_, when)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.Connect.Client
import Network.Connect.Compression (ContentCoding (..))
import Network.Connect.Compression qualified as Compression
import Network.Connect.Envelope (EndStreamResponse (..), EnvelopeFlags (..), FrameReader, buildFrameLazy, decodeEndStream, newFrameReader, readFrame)
import Network.Connect.Error (ConnectError (..), ConnectException (..), decodeConnectError, throwConnect)
import Network.Connect.Protocol (Codec (..), hConnectAcceptEncoding, hConnectContentEncoding, hConnectProtocolVersion, hConnectTimeoutMs)
import Network.Connect.Server
import Network.GRPC.Spec
  ( CustomMetadata (..)
  , GrpcError (..)
  , HeaderName (..)
  , Proto (..)
  )
import Network.HTTP.Connection (sendOn)
import Network.HTTP.Message (Request (..), Response (..), Scheme (..))
import Network.HTTP.Server (Handler, ServerConfig (..), defaultServerConfig, runServerOnListener)
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Types.Header (Headers, hAcceptEncoding, hContentEncoding, hContentType, hVary, lookupHeader)
import Network.HTTP.Types.Method (mGet, mPost)
import Network.HTTP.Types.Status (Status, status200, status201, status400, status406, status415, status501, status503, status504)
import Network.HTTP.Types.Version (pattern HTTP1_1)
import Network.HTTP.VersionRange (VersionRange, http1Only, http2Only)
import Network.Socket qualified as NS
import System.Timeout (timeout)
import Test.Syd

import Connect.TestProto

------------------------------------------------------------------------
-- Service implementation
------------------------------------------------------------------------

elizaService :: Service ElizaService ConnectServerM
elizaService =
  service
    ( method @Say sayH
        :& method @Aggregate aggregateH
        :& method @Converse converseH
        :& method @Introduce introduceH
        :& Done
    )

elizaHandlers :: [MethodHandler]
elizaHandlers = connectHandlers elizaService

sayH :: Proto SayRequest -> ConnectServerM (Proto SayResponse)
sayH (Proto req) =
  let s = sayRequestSentence req
   in if s == "boom"
        then liftIO (throwConnect GrpcUnimplemented "intentional boom")
        else do
          md <- getRequestMetadata
          pure (Proto defaultSayResponse{sayResponseSentence = "echo:" <> s <> foldMap echoVal md})

echoVal :: CustomMetadata -> Text
echoVal (CustomMetadata (AsciiHeader "x-echo") v) = TE.decodeUtf8 v
echoVal _ = ""

introduceH :: Proto IntroduceRequest -> (Proto IntroduceResponse -> ConnectServerM ()) -> ConnectServerM ()
introduceH (Proto req) send = mapM_ one ([1, 2, 3] :: [Int])
  where
    nm = introduceRequestName req
    one i = send (Proto defaultIntroduceResponse{introduceResponseSentence = nm <> T.pack (show i)})

aggregateH :: ConnectServerM (Maybe (Proto SayRequest)) -> ConnectServerM (Proto SayResponse)
aggregateH recv = go []
  where
    go acc = do
      m <- recv
      case m of
        Nothing -> pure (Proto defaultSayResponse{sayResponseSentence = T.intercalate "," (reverse acc)})
        Just (Proto r) -> go (sayRequestSentence r : acc)

converseH :: ConnectServerM (Maybe (Proto ConverseRequest)) -> (Proto ConverseResponse -> ConnectServerM ()) -> ConnectServerM ()
converseH recv send = loop
  where
    loop = do
      m <- recv
      case m of
        Nothing -> pure ()
        Just (Proto r) -> do
          send (Proto defaultConverseResponse{converseResponseSentence = "re:" <> converseRequestSentence r})
          loop

------------------------------------------------------------------------
-- Message helpers
------------------------------------------------------------------------

mkSay :: Text -> Proto SayRequest
mkSay s = Proto defaultSayRequest{sayRequestSentence = s}

saidSentence :: Proto SayResponse -> Text
saidSentence (Proto r) = sayResponseSentence r

mkIntro :: Text -> Proto IntroduceRequest
mkIntro n = Proto defaultIntroduceRequest{introduceRequestName = n}

introSentence :: Proto IntroduceResponse -> Text
introSentence (Proto r) = introduceResponseSentence r

mkConv :: Text -> Proto ConverseRequest
mkConv s = Proto defaultConverseRequest{converseRequestSentence = s}

convSentence :: Proto ConverseResponse -> Text
convSentence (Proto r) = converseResponseSentence r

------------------------------------------------------------------------
-- Server / client harness
------------------------------------------------------------------------

-- The loopback server advertises every coding so request br/zstd is accepted.
serverCfg :: ConnectServerConfig
serverCfg = defaultConnectServerConfig{cscSupportedCompression = [Identity, Gzip, Br, Zstd]}

withLoopback :: VersionRange -> ConnectClientConfig -> (ConnectClient -> IO a) -> IO a
withLoopback = withLoopbackHandlers elizaHandlers

withLoopbackHandlers :: [MethodHandler] -> VersionRange -> ConnectClientConfig -> (ConnectClient -> IO a) -> IO a
withLoopbackHandlers handlers = withLoopbackServer (connectApplication serverCfg handlers)

withLoopbackServer :: Handler -> VersionRange -> ConnectClientConfig -> (ConnectClient -> IO a) -> IO a
withLoopbackServer handler range ccfg action =
  withServerSocket $ \sock port -> do
    let scfg =
          defaultServerConfig
            { serverHost = "127.0.0.1"
            , serverPort = show port
            , serverVersionRange = range
            , serverHandler = handler
            }
    readyVar <- newEmptyMVar
    tid <- forkIO (putMVar readyVar () >> runServerOnListener scfg sock)
    takeMVar readyVar
    let conncfg =
          defaultConnectionConfig
            { connectionHost = "127.0.0.1"
            , connectionPort = show port
            , connectionVersionRange = range
            }
    withConnectClient ccfg conncfg action `finally` killThread tid

withServerSocket :: (NS.Socket -> Int -> IO a) -> IO a
withServerSocket k = do
  let hints = NS.defaultHints{NS.addrFlags = [NS.AI_PASSIVE], NS.addrSocketType = NS.Stream}
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> error "no addr available for test bind"
    (addr : _) ->
      bracket (NS.openSocket addr) NS.close $ \sock -> do
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.bind sock (NS.addrAddress addr)
        NS.listen sock 128
        bound <- NS.getSocketName sock
        let port = case bound of
              NS.SockAddrInet p _ -> fromIntegral p
              _ -> 0 :: Int
        k sock port

clientCfg :: Codec -> ConnectClientConfig
clientCfg codec = defaultConnectClientConfig{cccCodec = codec}

-- Drain a server-stream recv action into a list, projecting each element.
drainAll :: (a -> b) -> IO (Maybe a) -> IO [b]
drainAll proj recv = go []
  where
    go acc = do
      m <- recv
      case m of
        Nothing -> pure (reverse acc)
        Just x -> go (proj x : acc)

-- Read a full response body into one strict ByteString.
drainBody :: Body -> IO ByteString
drainBody BodyEmpty = pure ""
drainBody (BodyBytes bs) = pure bs
drainBody (BodyStream p) = go []
  where
    go acc = p >>= maybe (pure (BS.concat (reverse acc))) (\c -> go (c : acc))

------------------------------------------------------------------------
-- Test matrix
------------------------------------------------------------------------

transports :: [(String, VersionRange)]
transports = [("http1", http1Only), ("http2", http2Only)]

codecs :: [(String, Codec)]
codecs = [("proto", CodecProto), ("json", CodecJSON)]

tests :: Spec
tests =
  describe "Loopback" $ do
    describe "unary Say echo" $
      forM_ transports $ \(tn, range) ->
        forM_ codecs $ \(cn, codec) ->
          it (tn <> "/" <> cn) (unaryEchoT range codec)
    describe "concurrent unary over one http2 connection" $
      forM_ codecs $ \(cn, codec) ->
        it cn (concurrentUnaryT codec)
    describe "unary GET Say" $
      forM_ transports $ \(tn, range) ->
        forM_ codecs $ \(cn, codec) ->
          it (tn <> "/" <> cn) (unaryGetT range codec)
    describe "server streaming Introduce" $
      forM_ transports $ \(tn, range) ->
        forM_ codecs $ \(cn, codec) ->
          it (tn <> "/" <> cn) (serverStreamT range codec)
    describe "client streaming Aggregate" $
      forM_ transports $ \(tn, range) ->
        forM_ codecs $ \(cn, codec) ->
          it (tn <> "/" <> cn) (clientStreamT range codec)
    describe "bidi Converse (http2)" $
      forM_ codecs $ \(cn, codec) ->
        it cn (bidiT codec)
    describe "request compression round-trips (http1)" $
      forM_ [("gzip", Gzip), ("brotli", Br), ("zstd", Zstd)] $ \(cn, coding) ->
        it cn (reqCompressionT coding)
    describe "unary response compression" $
      forM_ transports $ \(tn, range) ->
        it tn (unaryCompressionNegotiationT range)
    describe "unary Content-Encoding" $
      forM_ transports $ \(tn, range) -> do
        it (tn <> " request coding") (unaryRequestCodingT range)
        it (tn <> " response coding") (unaryResponseCodingT range)
    describe "Content-Type validation" $
      forM_ transports $ \(tn, range) -> do
        it (tn <> " repeated request fields") (repeatedMediaTypeT range)
        it (tn <> " response grammar and charset") (responseMediaTypeT range)
    describe "unary error response headers" $
      forM_ transports $ \(tn, range) -> do
        it (tn <> " requires a JSON error media type and status 200 for success") (unaryErrorMediaT range)
        it (tn <> " uses the advertised or implicit response coding") (unaryResponseAdvertisementT range)
    describe "streaming response headers" $ do
      forM_ transports $ \(tn, range) -> do
        it (tn <> " rejects invalid headers before decoding server streams") (streamingResponseMediaT range)
        it (tn <> " rejects an invalid response head without waiting for its body") (streamingRejectedBodyT range)
        it (tn <> " decodes frames using the advertised coding") (streamingResponseCodingT range)
        it (tn <> " validates client-streaming responses") (clientStreamingResponseMediaT range)
        it (tn <> " validates streaming request coding") (streamingRequestCodingT range)
      it "http2 validates bidi response headers on the first receive" bidiResponseMediaT
    describe "streaming Accept-Encoding negotiation" $
      forM_ transports $ \(tn, range) -> do
        it (tn <> " respects omission and ordered field lines") (streamingNegotiationT range)
        it (tn <> " accepts implicit but rejects unadvertised response coding") (streamingImplicitCodingT range)
    it "error path surfaces ConnectException" errorT
    it "error path returns HTTP 501 on the wire" rawErrorStatusT
    it "leading metadata reaches the handler" metadataT
    it "accepts supported Content-Type parameters on unary and streaming requests" mediaTypeT
    it "rejects malformed media types and incompatible JSON charsets" invalidMediaTypeT
    describe "control headers" $
      forM_ transports $ \(tn, range) -> do
        it (tn <> " rejects malformed, repeated, and expired deadlines") (timeoutHeaderT range)
        it (tn <> " validates the optional protocol version") (protocolVersionT range)
        it (tn <> " emits bounded deadlines and the protocol marker on the wire") (clientControlHeadersT range)
    it "ends a blocked stream when its positive deadline expires" streamingDeadlineT

unaryEchoT :: VersionRange -> Codec -> IO ()
unaryEchoT range codec = do
  out <- withLoopback range (clientCfg codec) $ \cl ->
    nonStreaming cl (Proxy @Say) (mkSay "hi")
  saidSentence out `shouldBe` "echo:hi"

unaryGetT :: VersionRange -> Codec -> IO ()
unaryGetT range codec = do
  out <- withLoopback range (clientCfg codec) $ \cl ->
    nonStreamingGet cl (Proxy @Say) (mkSay "hi")
  saidSentence out `shouldBe` "echo:hi"

-- | Many unary RPCs multiplexed concurrently over ONE HTTP/2 connection.
-- This is the scenario the HPACK send-ordering fix targets: concurrent
-- streams must keep the encoder's dynamic-table mutation order in lockstep
-- with the wire order (and allocate monotonically-increasing stream ids),
-- or the peer's decoder desyncs / the server rejects an out-of-order id.
-- Each call carries a unique payload; every reply must be that call's own
-- echo.
concurrentUnaryT :: Codec -> IO ()
concurrentUnaryT codec = do
  let inputs = map (T.pack . show) [1 .. 50 :: Int]
  results <- withLoopback http2Only (clientCfg codec) $ \cl -> do
    vars <- forM inputs $ \s -> do
      v <- newEmptyMVar
      _ <- forkIO $ do
        r <-
          try (nonStreaming cl (Proxy @Say) (mkSay s))
            :: IO (Either SomeException (Proto SayResponse))
        putMVar v (s, r)
      pure v
    mapM takeMVar vars
  forM_ results $ \(s, r) -> case r of
    Right out -> saidSentence out `shouldBe` ("echo:" <> s)
    Left e ->
      expectationFailure ("concurrent call " <> T.unpack s <> " failed: " <> show e)

serverStreamT :: VersionRange -> Codec -> IO ()
serverStreamT range codec = do
  rs <- withLoopback range (clientCfg codec) $ \cl ->
    serverStreaming cl (Proxy @Introduce) (mkIntro "X") (drainAll introSentence)
  rs `shouldBe` ["X1", "X2", "X3"]

clientStreamT :: VersionRange -> Codec -> IO ()
clientStreamT range codec = do
  out <- withLoopback range (clientCfg codec) $ \cl ->
    clientStreaming cl (Proxy @Aggregate) (\send -> mapM_ (send . mkSay) ["a", "b", "c"])
  saidSentence out `shouldBe` "a,b,c"

bidiT :: Codec -> IO ()
bidiT codec = do
  rs <- withLoopback http2Only (clientCfg codec) $ \cl ->
    biDiStreaming cl (Proxy @Converse) $ \send recv -> do
      send (mkConv "a")
      r1 <- recv
      send (mkConv "b")
      r2 <- recv
      pure (mapMaybe (fmap convSentence) [r1, r2])
  rs `shouldBe` ["re:a", "re:b"]

reqCompressionT :: ContentCoding -> IO ()
reqCompressionT coding = do
  out <- withLoopback http1Only (clientCfg CodecProto){cccRequestCompression = coding} $ \cl ->
    nonStreaming cl (Proxy @Say) (mkSay "zzz")
  saidSentence out `shouldBe` "echo:zzz"

unaryCompressionNegotiationT :: VersionRange -> IO ()
unaryCompressionNegotiationT range = withLoopback range (clientCfg CodecJSON) $ \cl -> do
  let request = testRequest False "application/json" []
      json = "{\"sentence\":\"hi\"}"
      compressedRequest =
        request
          { requestHeaders = (hContentEncoding, "gzip") : requestHeaders request
          , requestBody = BodyBytes (Compression.compress Gzip json)
          }
      advertise values req =
        req {requestHeaders = requestHeaders req <> map (\value -> (hAcceptEncoding, value)) values}
      expectCoding coding req = do
        response <- sendRaw cl req
        responseStatus response `shouldBe` status200
        lookupHeader hContentEncoding (responseHeaders response)
          `shouldBe` if coding == Identity then Nothing else Just (Compression.codingName coding)
        when (requestMethod req == mGet) $
          lookupHeader hVary (responseHeaders response) `shouldBe` Just "Accept-Encoding"
        body <- drainBody (responseBody response)
        plain <- Compression.decompress coding body
        fmap saidSentence (plain >>= Aeson.eitherDecodeStrict) `shouldBe` Right "echo:hi"
  expectCoding Gzip compressedRequest
  expectCoding Identity (advertise [""] compressedRequest)
  expectCoding Br (advertise ["gzip;q=0.2", "BR;q=0.8"] request)
  expectCoding Br (advertise ["br, gzip"] request)
  expectCoding Br (advertise ["*;q=0.5, gzip;q=0, zstd;q=0"] request)
  expectCoding Identity (advertise ["br;q=0, gzip;q=0, zstd;q=0"] request)
  response <- sendRaw cl (advertise ["*;q=0"] request)
  responseStatus response `shouldBe` status406
  expectCoding Gzip $
    request
      { requestMethod = mGet
      , requestTarget = requestTarget request <> "?encoding=json&compression=gzip&base64=1&message="
          <> B64.encodeUnpadded (Compression.compress Gzip json)
      , requestHeaders = []
      , requestBody = BodyEmpty
      }

unaryRequestCodingT :: VersionRange -> IO ()
unaryRequestCodingT range = withLoopback range (clientCfg CodecJSON) $ \cl -> do
  let request encodings =
        (testRequest False "application/json" (map (\value -> (hContentEncoding, value)) encodings))
          {requestBody = BodyBytes (Compression.compress Gzip "{\"sentence\":\"hi\"}")}
  accepted <- sendRaw cl (request ["X-GZip"])
  responseStatus accepted `shouldBe` status200
  lookupHeader hContentEncoding (responseHeaders accepted) `shouldBe` Just "gzip"
  body <- drainBody (responseBody accepted) >>= Compression.decompress Gzip
  fmap saidSentence (body >>= Aeson.eitherDecodeStrict) `shouldBe` Right "echo:hi"
  forM_ [["gzip", "identity"], ["gzip, br"], [""]] $ \encodings -> do
    rejected <- sendRaw cl (request encodings)
    responseStatus rejected `shouldBe` status501
    _ <- drainBody (responseBody rejected)
    pure ()

unaryResponseCodingT :: VersionRange -> IO ()
unaryResponseCodingT range = do
  let plain = "{\"sentence\":\"echo:hi\"}"
      makeResponse status codings =
        testResponse status ((hContentType, "application/json") : map (\value -> (hContentEncoding, value)) codings)
      good = makeResponse status200 ["X-GZip"] (Compression.compress Gzip plain)
      cfg = (clientCfg CodecJSON) {cccAcceptCompression = [Br, Gzip]}
      remoteError = "{\"code\":\"invalid_argument\",\"message\":\"remote error\"}"
  reply <- newIORef good
  receivedAccept <- newIORef Nothing
  let handler request = do
        writeIORef receivedAccept (lookupHeader hAcceptEncoding (requestHeaders request))
        readIORef reply
  withLoopbackServer handler range cfg $ \cl -> do
    saidSentence <$> nonStreaming cl (Proxy @Say) (mkSay "hi") `shouldReturn` "echo:hi"
    saidSentence <$> nonStreamingGet cl (Proxy @Say) (mkSay "hi") `shouldReturn` "echo:hi"
    readIORef receivedAccept `shouldReturn` Just "br, gzip"
    resolvedGet <- nonStreamingGetResolved cl (Proxy @Say) (mkSay "hi")
    map saidSentence (roPayloads resolvedGet) `shouldBe` ["echo:hi"]
    readIORef receivedAccept `shouldReturn` Just "br, gzip"
    forM_ [ (["gzip", "gzip"], Compression.compress Gzip plain)
          , (["unknown"], plain)
          , (["gzip;q=0"], plain)
          , (["gzip"], plain)
          ] $ \(codings, body) -> do
      writeIORef reply (makeResponse status200 codings body)
      thrown <- try (nonStreaming cl (Proxy @Say) (mkSay "hi"))
      case thrown of
        Left (ConnectException err) -> ceCode err `shouldBe` GrpcInternal
        Right value -> expectationFailure ("Accepted response: " <> show (saidSentence value))
      resolved <- nonStreamingResolved cl (Proxy @Say) (mkSay "hi")
      fmap ceCode (roError resolved) `shouldBe` Just GrpcInternal
      null (roPayloads resolved) `shouldBe` True
    writeIORef reply (makeResponse status400 ["X-GZip"] (Compression.compress Gzip remoteError))
    errorResponse <- nonStreamingResolved cl (Proxy @Say) (mkSay "hi")
    fmap ceMessage (roError errorResponse) `shouldBe` Just (Just "remote error")
    writeIORef reply (makeResponse status501 ["gzip"] remoteError)
    badError <- nonStreamingResolved cl (Proxy @Say) (mkSay "hi")
    fmap ceCode (roError badError) `shouldBe` Just GrpcUnknown
    fmap ceMessage (roError badError) `shouldBe` Just (Just "connect: server error")

testResponse :: Status -> Headers -> ByteString -> Response
testResponse status fields body =
  Response
    { responseStatus = status
    , responseVersion = HTTP1_1
    , responseHeaders = fields
    , responseBody = BodyBytes body
    , responseTrailers = pure []
    , responseH2StreamId = 0
    , responseCancel = pure ()
    , responsePushPromises = pure []
    }

repeatedMediaTypeT :: VersionRange -> IO ()
repeatedMediaTypeT range = withLoopback range (clientCfg CodecJSON) $ \cl -> do
  let check streaming wire =
        forM_ [wire, "text/plain"] $ \second -> do
          response <- sendRaw cl (testRequest streaming wire [("CONTENT-TYPE", second)])
          responseStatus response `shouldBe` status415
          drainBody (responseBody response) `shouldReturn` ""
  check False "application/json"
  check True "application/connect+json"

responseMediaTypeT :: VersionRange -> IO ()
responseMediaTypeT range = do
  let valid = [(hContentType, "APPLICATION/JSON; CHARSET=\"UTF-8\"; version=\"a;b\"")]
      body = "{\"sentence\":\"echo:hi\"}"
  reply <- newIORef valid
  let handler _ = do
        fields <- readIORef reply
        pure (testResponse status200 fields body)
  withLoopbackServer handler range (clientCfg CodecJSON) $ \cl -> do
    saidSentence <$> nonStreaming cl (Proxy @Say) (mkSay "hi") `shouldReturn` "echo:hi"
    let expectError fields code = do
          writeIORef reply fields
          resolved <- nonStreamingResolved cl (Proxy @Say) (mkSay "hi")
          fmap ceCode (roError resolved) `shouldBe` Just code
          null (roPayloads resolved) `shouldBe` True
    expectError [(hContentType, "application/json; charset=latin1")] GrpcInternal
    expectError [(hContentType, "application/json; charset=utf-8; charset=latin1")] GrpcInternal
    expectError [(hContentType, "application/json; param=\"unterminated")] GrpcInternal
    expectError [(hContentType, "application/json"), (hContentType, "text/plain")] GrpcInternal
    expectError [(hContentType, "application/proto")] GrpcInternal
    expectError [(hContentType, "application/connect+json")] GrpcInternal
    expectError [(hContentType, "text/plain")] GrpcUnknown
    expectError [] GrpcUnknown

unaryErrorMediaT :: VersionRange -> IO ()
unaryErrorMediaT range = do
  let json = [(hContentType, "application/json")]
      errorBody = "{\"code\":\"invalid_argument\",\"message\":\"remote error\"}"
      valid = testResponse status503
        [(hContentType, "Application/JSON; Charset=\"UTF-8\"; profile=\"a;b\""), ("trailer-x-end", "done")]
        errorBody
  reply <- newIORef valid
  withLoopbackServer (const (readIORef reply)) range (clientCfg CodecProto) $ \cl -> do
    let call = nonStreamingResolved cl (Proxy @Say) (mkSay "hi")
        callGet = nonStreamingGetResolved cl (Proxy @Say) (mkSay "hi")
        check code out = do
          fmap ceCode (roError out) `shouldBe` Just code
          null (roPayloads out) `shouldBe` True
    good <- call
    check GrpcInvalidArgument good
    fmap ceMessage (roError good) `shouldBe` Just (Just "remote error")
    roTrailers good `shouldBe` [CustomMetadata (AsciiHeader "x-end") "done"]
    forM_
      [ []
      , [(hContentType, "text/plain")]
      , [(hContentType, "application/proto")]
      , [(hContentType, "application/connect+json")]
      , [(hContentType, "application/json; charset=latin1")]
      , [(hContentType, "application/json; profile=\"broken")]
      , json <> json
      , json <> [(hContentType, "text/plain")]
      ] $ \fields -> do
        writeIORef reply (testResponse status503 fields errorBody)
        call >>= check GrpcUnavailable
        callGet >>= check GrpcUnavailable
        expectConnectException GrpcUnavailable (nonStreaming cl (Proxy @Say) (mkSay "hi"))
        expectConnectException GrpcUnavailable (nonStreamingGet cl (Proxy @Say) (mkSay "hi"))
    forM_ ["", "{", "[1]"] $ \body -> do
      writeIORef reply (testResponse status503 json body)
      call >>= check GrpcUnavailable

  withLoopbackServer (const (pure (testResponse status201 json "{\"sentence\":\"ok\"}"))) range (clientCfg CodecJSON) $ \cl -> do
    nonStreamingResolved cl (Proxy @Say) (mkSay "hi") >>= \out -> do
      fmap ceCode (roError out) `shouldBe` Just GrpcUnknown
      null (roPayloads out) `shouldBe` True
    expectConnectException GrpcUnknown (nonStreaming cl (Proxy @Say) (mkSay "hi"))

unaryResponseAdvertisementT :: VersionRange -> IO ()
unaryResponseAdvertisementT range = do
  let success = "{\"sentence\":\"echo:hi\"}"
      rejectedBody = "{\"code\":\"invalid_argument\"}"
      reply status coding body =
        testResponse status [(hContentType, "application/json"), (hContentEncoding, Compression.codingName coding)]
          (Compression.compress coding body)
      cfg = (clientCfg CodecJSON){cccRequestCompression = Gzip, cccAcceptCompression = []}
  response <- newIORef (reply status200 Gzip success)
  let handler _ = readIORef response
  withLoopbackServer handler range cfg $ \cl -> do
    saidSentence <$> nonStreaming cl (Proxy @Say) (mkSay "hi") `shouldReturn` "echo:hi"
    saidSentence <$> nonStreamingGet cl (Proxy @Say) (mkSay "hi") `shouldReturn` "echo:hi"
    writeIORef response (reply status200 Zstd success)
    expectConnectException GrpcInternal (nonStreamingGet cl (Proxy @Say) (mkSay "hi"))
    writeIORef response (reply status503 Zstd rejectedBody)
    expectConnectException GrpcUnavailable (nonStreaming cl (Proxy @Say) (mkSay "hi"))
    writeIORef response (reply status503 Gzip rejectedBody)
    expectConnectException GrpcInvalidArgument (nonStreaming cl (Proxy @Say) (mkSay "hi"))
  writeIORef response (reply status200 Gzip success)
  withLoopbackServer handler range cfg{cccAcceptCompression = [Br]} $ \cl ->
    expectConnectException GrpcInternal (nonStreaming cl (Proxy @Say) (mkSay "hi"))

streamingResponseMediaT :: VersionRange -> IO ()
streamingResponseMediaT range = do
  let valid = streamingMedia "APPLICATION/CONNECT+JSON; charset=\"UTF-8\"; version=\"a;b\""
      body = streamingBody Identity
      withReply status fields =
        withLoopbackServer (const (pure (testResponse status fields body))) range (clientCfg CodecJSON)
  withReply status200 valid $ \cl -> do
    out <- serverStreamingResolved cl (Proxy @Introduce) (mkIntro "hi")
    map introSentence (roPayloads out) `shouldBe` ["stream"]
    roError out `shouldBe` Nothing
  forM_
    [ (status200, [], GrpcUnknown)
    , (status200, streamingMedia "application/json", GrpcUnknown)
    , (status200, streamingMedia "application/connect+proto", GrpcInternal)
    , (status200, streamingMedia "application/connect+junk", GrpcInternal)
    , (status200, streamingMedia "application/connect+json; charset=latin1", GrpcInternal)
    , (status200, streamingMedia "application/connect+json; profile=\"broken", GrpcInternal)
    , (status200, valid <> streamingMedia "application/connect+json", GrpcInternal)
    , (status201, valid, GrpcUnknown)
    , (status503, valid, GrpcUnavailable)
    ] $ \(status, fields, expected) -> do
      withReply status fields $ \cl ->
        serverStreamingResolved cl (Proxy @Introduce) (mkIntro "hi") >>= expectStreamOutcomeError expected
      withReply status fields $ \cl ->
        expectConnectException expected $
          serverStreaming cl (Proxy @Introduce) (mkIntro "hi") (drainAll introSentence)

streamingRejectedBodyT :: VersionRange -> IO ()
streamingRejectedBodyT range = do
  release <- newEmptyMVar
  first <- newIORef True
  let valid = testResponse status200 (streamingMedia "application/connect+json") (streamingBody Identity)
      rejected =
        (testResponse status200 (streamingMedia "text/plain") "")
          {responseBody = BodyStream (readMVar release >> pure Nothing)}
      handler _ = do
        isFirst <- atomicModifyIORef' first (\current -> (False, current))
        pure (if isFirst then rejected else valid)
  (withLoopbackServer handler range (clientCfg CodecJSON) $ \cl -> do
      result <- timeout 2000000 (serverStreamingResolved cl (Proxy @Introduce) (mkIntro "hi"))
      case result of
        Nothing -> expectationFailure "client waited for the body after an invalid response head"
        Just outcome -> expectStreamOutcomeError GrpcUnknown outcome
      when (range == http2Only) $
        serverStreaming cl (Proxy @Introduce) (mkIntro "hi") (drainAll introSentence) `shouldReturn` ["stream"])
    `finally` putMVar release ()

streamingResponseCodingT :: VersionRange -> IO ()
streamingResponseCodingT range = do
  let cfg = (clientCfg CodecJSON){cccAcceptCompression = [Br, Gzip]}
      media = streamingMedia "application/connect+json"
      withReply codings body =
        withLoopbackServer (const (pure (testResponse status200 (media <> codingFields codings) body))) range cfg
      checkAccepted codings body = do
        withReply codings body $ \cl ->
          serverStreaming cl (Proxy @Introduce) (mkIntro "hi") (drainAll introSentence) `shouldReturn` ["stream"]
        withReply codings body $ \cl -> do
          out <- serverStreamingResolved cl (Proxy @Introduce) (mkIntro "hi")
          map introSentence (roPayloads out) `shouldBe` ["stream"]
          roError out `shouldBe` Nothing
  checkAccepted ["X-GZip"] (streamingBody Gzip)
  checkAccepted ["gzip"] (streamingBody Identity)
  let compressedEnd = BL.toStrict $
        buildFrameLazy (EnvelopeFlags True True) (Compression.compress Gzip "{\"error\":{\"code\":\"permission_denied\"}}")
  withReply ["gzip"] compressedEnd $ \cl ->
    serverStreamingResolved cl (Proxy @Introduce) (mkIntro "hi") >>= expectStreamOutcomeError GrpcPermissionDenied
  forM_
    [ ([], streamingBody Gzip)
    , (["identity"], streamingBody Gzip)
    , (["gzip", "gzip"], streamingBody Gzip)
    , (["gzip, br"], streamingBody Gzip)
    , (["gzip,"], streamingBody Gzip)
    , ([""], streamingBody Identity)
    , (["unknown"], streamingBody Identity)
    , (["zstd"], streamingBody Identity)
    , (["gzip"], BL.toStrict (buildFrameLazy (EnvelopeFlags True False) "{\"sentence\":\"stream\"}"))
    ] $ \(codings, body) -> do
      withReply codings body $ \cl ->
        serverStreamingResolved cl (Proxy @Introduce) (mkIntro "hi") >>= expectStreamOutcomeError GrpcInternal
      withReply codings body $ \cl ->
        expectConnectException GrpcInternal $
          serverStreaming cl (Proxy @Introduce) (mkIntro "hi") (drainAll introSentence)

clientStreamingResponseMediaT :: VersionRange -> IO ()
clientStreamingResponseMediaT range = do
  let fields = streamingMedia "application/connect+proto"
      withReply = withLoopbackServer (const (pure (testResponse status200 fields (streamingBody Identity)))) range (clientCfg CodecJSON)
  withReply $ \cl ->
    expectConnectException GrpcInternal $
      clientStreaming cl (Proxy @Aggregate) (\send -> send (mkSay "hi"))
  withReply $ \cl ->
    clientStreamingResolved cl (Proxy @Aggregate) [mkSay "hi"] >>= expectStreamOutcomeError GrpcInternal

bidiResponseMediaT :: IO ()
bidiResponseMediaT = do
  let fields = streamingMedia "application/json"
      withReply = withLoopbackServer (const (pure (testResponse status200 fields (streamingBody Identity)))) http2Only (clientCfg CodecJSON)
  withReply $ \cl ->
    biDiStreaming cl (Proxy @Converse) $ \send recv -> do
      send (mkConv "hi")
      expectConnectException GrpcUnknown recv
      expectConnectException GrpcUnknown recv
  withReply $ \cl ->
    biDiStreamingResolved cl (Proxy @Converse) [mkConv "hi"] >>= expectStreamOutcomeError GrpcUnknown

streamingRequestCodingT :: VersionRange -> IO ()
streamingRequestCodingT range = withLoopback range (clientCfg CodecJSON) $ \cl -> do
  let request codings =
        testRequest True "application/connect+json" ((hConnectAcceptEncoding, "identity") : codingFields codings)
  forM_ [["dont-know"], ["identity", "gzip"], ["gzip, br"]] $ \codings -> do
    resp <- sendRaw cl (request codings)
    lookupHeader hContentType (responseHeaders resp) `shouldBe` Just "application/connect+json"
    lookupHeader hConnectAcceptEncoding (responseHeaders resp) `shouldBe` Just "identity, gzip, br, zstd"
    expectStreamError GrpcUnimplemented resp
  let compressed =
        BL.toStrict (buildFrameLazy (EnvelopeFlags True False) (Compression.compress Gzip "{\"sentence\":\"hi\"}"))
  accepted <- sendRaw cl (request ["X-GZip"]){requestBody = BodyBytes compressed}
  responseStatus accepted `shouldBe` status200
  reader <- readResponseFrames accepted
  readFrame reader >>= \case
    Just (EnvelopeFlags False False, payload) ->
      fmap saidSentence (Aeson.eitherDecodeStrict payload) `shouldBe` Right "hi"
    _ -> expectationFailure "expected a message decoded from the request's gzip frame"
  readFrame reader `shouldReturn` Just (EnvelopeFlags False True, "{}")
  readFrame reader `shouldReturn` Nothing

streamingNegotiationT :: VersionRange -> IO ()
streamingNegotiationT range = withLoopback range (clientCfg CodecJSON) $ \cl -> do
  let check accepted expected = do
        let fields = (hConnectContentEncoding, "gzip") : map (\value -> (hConnectAcceptEncoding, value)) accepted
            request = (testRequest True "application/connect+json" fields)
              {requestBody = BodyBytes (BL.toStrict (buildFrameLazy (EnvelopeFlags True False) compressed))}
            compressed = Compression.compress Gzip "{\"sentence\":\"hi\"}"
        response <- sendRaw cl request
        responseStatus response `shouldBe` status200
        lookupHeader hConnectContentEncoding (responseHeaders response)
          `shouldBe` if expected == Identity then Nothing else Just (Compression.codingName expected)
        reader <- readResponseFrames response
        readFrame reader >>= \case
          Just (EnvelopeFlags compressedFlag False, payload) -> do
            compressedFlag `shouldBe` (expected /= Identity)
            decoded <- Compression.decompress expected payload
            fmap saidSentence (decoded >>= Aeson.eitherDecodeStrict) `shouldBe` Right "hi"
          _ -> expectationFailure "expected a response message"
        readFrame reader `shouldReturn` Just (EnvelopeFlags False True, "{}")
        readFrame reader `shouldReturn` Nothing
  check [] Gzip
  check [""] Identity
  check ["identity, gzip"] Identity
  check ["unsupported", "BR, gzip"] Br
  check ["br, gzip", "zstd"] Br
  check ["gzip;q=1, *, invalid name", "X-GZip"] Gzip
  check ["gzip;q=1, *"] Identity
  check [", ,", "unknown,\t ZSTD \t, br"] Zstd

streamingImplicitCodingT :: VersionRange -> IO ()
streamingImplicitCodingT range = do
  observed <- newIORef Nothing
  let response = testResponse status200 (streamingMedia "application/connect+json" <> codingFields ["gzip"]) (streamingBody Gzip)
      handler request = do
        writeIORef observed (lookupHeader hConnectAcceptEncoding (requestHeaders request))
        pure response
      cfg = (clientCfg CodecJSON){cccRequestCompression = Gzip, cccAcceptCompression = []}
  withLoopbackServer handler range cfg $ \cl -> do
    serverStreaming cl (Proxy @Introduce) (mkIntro "hi") (drainAll introSentence) `shouldReturn` ["stream"]
    readIORef observed `shouldReturn` Nothing
    out <- serverStreamingResolved cl (Proxy @Introduce) (mkIntro "hi")
    map introSentence (roPayloads out) `shouldBe` ["stream"]
    roError out `shouldBe` Nothing
  withLoopbackServer handler range cfg{cccAcceptCompression = [Identity]} $ \cl -> do
    serverStreamingResolved cl (Proxy @Introduce) (mkIntro "hi") >>= expectStreamOutcomeError GrpcInternal
    readIORef observed `shouldReturn` Just "identity"

streamingMedia :: ByteString -> Headers
streamingMedia value = [(hContentType, value)]

codingFields :: [ByteString] -> Headers
codingFields = map (\value -> (hConnectContentEncoding, value))

streamingBody :: ContentCoding -> ByteString
streamingBody coding =
  BL.toStrict $
    buildFrameLazy (EnvelopeFlags (coding /= Identity) False) (Compression.compress coding "{\"sentence\":\"stream\"}")
      <> buildFrameLazy (EnvelopeFlags False True) "{}"

expectStreamOutcomeError :: GrpcError -> RpcOutcome a -> IO ()
expectStreamOutcomeError expected out = do
  fmap ceCode (roError out) `shouldBe` Just expected
  null (roPayloads out) `shouldBe` True

expectConnectException :: GrpcError -> IO a -> IO ()
expectConnectException expected run = do
  outcome <- try run
  case outcome of
    Left (ConnectException err) -> ceCode err `shouldBe` expected
    Right _ -> expectationFailure "expected a ConnectException"

errorT :: IO ()
errorT = do
  res <- withLoopback http1Only (clientCfg CodecProto) $ \cl ->
    try (nonStreaming cl (Proxy @Say) (mkSay "boom"))
  case (res :: Either ConnectException (Proto SayResponse)) of
    Left (ConnectException ce) -> ceCode ce `shouldBe` GrpcUnimplemented
    Right _ -> expectationFailure "expected ConnectException, got a response"

-- The high-level client derives the code from the JSON error body; this
-- additionally pins the on-the-wire HTTP status the server emits.
rawErrorStatusT :: IO ()
rawErrorStatusT = withLoopback http1Only (clientCfg CodecJSON) $ \cl -> do
  resp <- rawSayJSON cl "boom"
  responseStatus resp `shouldBe` status501
  bs <- drainBody (responseBody resp)
  case Aeson.eitherDecodeStrict bs >>= decodeConnectError of
    Right ce -> ceCode ce `shouldBe` GrpcUnimplemented
    Left e -> expectationFailure ("bad error envelope on the wire: " <> e)

rawSayJSON :: ConnectClient -> Text -> IO Response
rawSayJSON cl sentence = sendOn (clConn cl) req
  where
    body = BL.toStrict (Aeson.encode (Proto defaultSayRequest{sayRequestSentence = sentence}))
    req =
      Request
        { requestMethod = mPost
        , requestTarget = "/connectrpc.eliza.v1.ElizaService/Say"
        , requestAuthority = Just (clAuthority cl)
        , requestScheme = clScheme cl
        , requestHeaders = [(hContentType, "application/json")]
        , requestBody = BodyBytes body
        , requestVersion = HTTP1_1
        , requestTrailers = pure []
        }

metadataT :: IO ()
metadataT = do
  let cfg = (clientCfg CodecProto){cccMetadata = [CustomMetadata (AsciiHeader "x-echo") "!"]}
  out <- withLoopback http1Only cfg $ \cl ->
    nonStreaming cl (Proxy @Say) (mkSay "hi")
  saidSentence out `shouldBe` "echo:hi!"

mediaTypeT :: IO ()
mediaTypeT = withLoopback http1Only (clientCfg CodecJSON) $ \cl -> do
  let call = sendRaw cl
  unary <- call (testRequest False "application/json; charset=\"UTF-8\"; version=1" [])
  responseStatus unary `shouldBe` status200
  unaryBody <- drainBody (responseBody unary)
  fmap saidSentence (Aeson.eitherDecodeStrict unaryBody) `shouldBe` Right "echo:hi"
  streaming <- call (testRequest True "application/connect+json; charset=utf-8" [])
  responseStatus streaming `shouldBe` status200
  body <- drainBody (responseBody streaming)
  BS.take 1 body `shouldBe` "\0"

invalidMediaTypeT :: IO ()
invalidMediaTypeT = withLoopback http1Only (clientCfg CodecJSON) $ \cl -> do
  let call = sendRaw cl
  forM_ ["application", "application/json; charset=latin1"] $ \ct -> do
    response <- call (testRequest False ct [])
    responseStatus response `shouldBe` status415
  response <- call (testRequest True "application/connect+json; charset=utf-16" [])
  responseStatus response `shouldBe` status415

timeoutHeaderT :: VersionRange -> IO ()
timeoutHeaderT range = do
  called <- newIORef False
  let handlers =
        connectHandlers $
          service @ElizaService
            ( method @Say (\input -> liftIO (writeIORef called True) >> sayH input)
                :& method @Aggregate (\recv -> liftIO (writeIORef called True) >> aggregateH recv)
                :& method @Converse converseH
                :& method @Introduce introduceH
                :& Done
            )
  withLoopbackHandlers handlers range (clientCfg CodecJSON) $ \cl -> do
    forM_
      [ (["0"], status504, GrpcDeadlineExceeded)
      , (["-1"], status400, GrpcInvalidArgument)
      , (["0x1"], status400, GrpcInvalidArgument)
      , (["10000000000"], status400, GrpcInvalidArgument)
      , (["5000, 0"], status400, GrpcInvalidArgument)
      , (["5000", "5000"], status400, GrpcInvalidArgument)
      , (["5000", "0"], status400, GrpcInvalidArgument)
      , (["0", "5000"], status400, GrpcInvalidArgument)
      ] $ \(values, status, code) -> do
        let fields = map (\value -> (hConnectTimeoutMs, value)) values
        sendRaw cl (testRequest False "application/json" fields) >>= expectUnaryError status code
        sendRaw cl (testGetRequest "connect=v1" fields) >>= expectUnaryError status code
        sendRaw cl (testRequest True "application/connect+json" fields) >>= expectStreamError code
  readIORef called >>= (`shouldBe` False)

protocolVersionT :: VersionRange -> IO ()
protocolVersionT range = withLoopback range (clientCfg CodecJSON) $ \cl -> do
  forM_ [[], [(hConnectProtocolVersion, "1")]] $ \fields -> do
    sendRaw cl (testRequest False "application/json" fields) >>= expectOK
    sendRaw cl (testRequest True "application/connect+json" fields) >>= expectOK
  forM_ ["", "connect=v1"] $ \version ->
    sendRaw cl (testGetRequest version []) >>= expectOK
  forM_ [[""], ["2"], ["01"], ["1, 1"], ["1", "1"], ["1", "2"]] $ \values -> do
    let fields = map (\value -> (hConnectProtocolVersion, value)) values
    sendRaw cl (testRequest False "application/json" fields) >>= expectUnaryError status400 GrpcInvalidArgument
    sendRaw cl (testRequest True "application/connect+json" fields) >>= expectStreamError GrpcInvalidArgument
  forM_ ["connect=", "connect=v2", "connect=v1&connect=v1", "connect=v1&connect=v2"] $ \version ->
    sendRaw cl (testGetRequest version []) >>= expectUnaryError status400 GrpcInvalidArgument
  where
    expectOK resp = do
      responseStatus resp `shouldBe` status200
      body <- drainBody (responseBody resp)
      BS.null body `shouldBe` False

clientControlHeadersT :: VersionRange -> IO ()
clientControlHeadersT range = do
  observed <- newIORef Nothing
  let cfg = (clientCfg CodecJSON){cccTimeoutMs = Just 5000}
      handler req = do
        writeIORef observed (Just (requestHeaders req))
        connectApplication serverCfg elizaHandlers req
      checkHeaders deadline version = do
        Just fields <- readIORef observed
        lookupHeader hConnectTimeoutMs fields `shouldBe` Just deadline
        lookupHeader hConnectProtocolVersion fields `shouldBe` version
  withLoopbackServer handler range cfg $ \cl -> do
    saidSentence <$> nonStreamingGet cl (Proxy @Say) (mkSay "hi") `shouldReturn` "echo:hi"
    checkHeaders "5000" Nothing
    out <- nonStreamingGetResolved cl (Proxy @Say) (mkSay "hi")
    map saidSentence (roPayloads out) `shouldBe` ["echo:hi"]
    checkHeaders "5000" Nothing
    saidSentence <$> nonStreaming cl (Proxy @Say) (mkSay "hi") `shouldReturn` "echo:hi"
    checkHeaders "5000" (Just "1")
    serverStreaming cl (Proxy @Introduce) (mkIntro "hi") (drainAll introSentence) `shouldReturn` ["hi1", "hi2", "hi3"]
    checkHeaders "5000" (Just "1")
  withLoopbackServer handler range cfg{cccTimeoutMs = Just maxBound, cccSendProtocolVersion = False} $ \cl -> do
    out <- nonStreamingResolved cl (Proxy @Say) (mkSay "hi")
    map saidSentence (roPayloads out) `shouldBe` ["echo:hi"]
    let maxWireMillis = min 9999999999 (toInteger (maxBound `quot` 1000 :: Int))
    checkHeaders (BL.toStrict (BB.toLazyByteString (BB.integerDec maxWireMillis))) Nothing
  withLoopbackServer handler range cfg{cccTimeoutMs = Just (-1)} $ \cl -> do
    out <- nonStreamingResolved cl (Proxy @Say) (mkSay "hi")
    fmap ceCode (roError out) `shouldBe` Just GrpcDeadlineExceeded
    checkHeaders "0" (Just "1")

expectUnaryError :: Status -> GrpcError -> Response -> IO ()
expectUnaryError status code resp = do
  responseStatus resp `shouldBe` status
  body <- drainBody (responseBody resp)
  fmap ceCode (Aeson.eitherDecodeStrict body >>= decodeConnectError) `shouldBe` Right code

testGetRequest :: ByteString -> Headers -> Request
testGetRequest version fields =
  let base = testRequest False "application/json" fields
      suffix = if BS.null version then "" else "&" <> version
   in base
        { requestMethod = mGet
        , requestTarget = requestTarget base <> "?encoding=json&message=%7B%7D" <> suffix
        , requestBody = BodyEmpty
        }

streamingDeadlineT :: IO ()
streamingDeadlineT = do
  blocker <- newEmptyMVar
  let handlers =
        connectHandlers $
          service @ElizaService
            ( method @Say sayH
                :& method @Aggregate (\_ -> liftIO (takeMVar blocker))
                :& method @Converse converseH
                :& method @Introduce introduceH
                :& Done
            )
      request = testRequest True "application/connect+json" [(hConnectTimeoutMs, "10")]
  result <- timeout 5000000 $
    withLoopbackHandlers handlers http1Only (clientCfg CodecJSON) $ \cl ->
      sendRaw cl request >>= expectStreamError GrpcDeadlineExceeded
  result `shouldBe` Just ()

expectStreamError :: GrpcError -> Response -> IO ()
expectStreamError code response = do
  responseStatus response `shouldBe` status200
  reader <- readResponseFrames response
  readFrame reader >>= \case
    Just (flags, payload) -> do
      flags `shouldBe` EnvelopeFlags False True
      fmap (fmap ceCode . esError) (decodeEndStream payload) `shouldBe` Right (Just code)
    Nothing -> expectationFailure "expected an end-stream error frame"
  readFrame reader >>= (`shouldBe` Nothing)

readResponseFrames :: Response -> IO FrameReader
readResponseFrames response = do
  frame <- newIORef . Just =<< drainBody (responseBody response)
  newFrameReader (atomicModifyIORef' frame (\current -> (Nothing, current)))

sendRaw :: ConnectClient -> Request -> IO Response
sendRaw cl req =
  sendOn (clConn cl) req{requestAuthority = Just (clAuthority cl), requestScheme = clScheme cl}

testRequest :: Bool -> ByteString -> Headers -> Request
testRequest streaming ct headers =
  Request
    { requestMethod = mPost
    , requestTarget = "/connectrpc.eliza.v1.ElizaService/" <> if streaming then "Aggregate" else "Say"
    , requestAuthority = Nothing
    , requestScheme = SchemeHttp
    , requestHeaders = (hContentType, ct) : headers
    , requestBody = BodyBytes body
    , requestVersion = HTTP1_1
    , requestTrailers = pure []
    }
  where
    json = "{\"sentence\":\"hi\"}"
    body
      | streaming = BL.toStrict (buildFrameLazy (EnvelopeFlags False False) json)
      | otherwise = json
