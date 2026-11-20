module Test.Connection (tests) where

import Control.Concurrent (forkFinally, forkIO, killThread)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, throwIO, try)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket (Socket)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS
import System.Timeout (timeout)
import Test.Syd

import qualified Network.HTTP2.Client as Client
import Network.HTTP2.Connection
import Network.HTTP2.Frame
import Network.HTTP2.HPACK (DynamicTable, decodeHeaderBlock, defaultEncodeStrategy, encodeHeaderBlock, newDynamicTable)
import qualified Network.HTTP2.Server as Server
import Network.HTTP2.Types

tests :: Spec
tests = describe "Connection" $ sequence_
  [ describe "Settings" $ sequence_
      [ it "apply valid settings" $ do
          let params = [(0x1, 8192), (0x3, 200), (0x4, 131072), (0x5, 32768)]
          case applySettingsParams defaultSettings params of
            Left err -> expectationFailure (show err)
            Right s -> do
              settingsHeaderTableSize s `shouldBe` 8192
              settingsMaxConcurrentStreams s `shouldBe` Just 200
              settingsInitialWindowSize s `shouldBe` 131072
              settingsMaxFrameSize s `shouldBe` 32768
      , it "reject invalid max frame size (too small)" $ do
          let params = [(0x5, 100)]
          case applySettingsParams defaultSettings params of
            Left (InvalidMaxFrameSize _) -> pure ()
            _ -> expectationFailure "Expected InvalidMaxFrameSize"
      , it "reject invalid max frame size (too large)" $ do
          let params = [(0x5, 20000000)]
          case applySettingsParams defaultSettings params of
            Left (InvalidMaxFrameSize _) -> pure ()
            _ -> expectationFailure "Expected InvalidMaxFrameSize"
      , it "reject invalid initial window size" $ do
          let params = [(0x4, 0x80000000)]
          case applySettingsParams defaultSettings params of
            Left (InvalidInitialWindowSize _) -> pure ()
            _ -> expectationFailure "Expected InvalidInitialWindowSize"
      , it "reject invalid enable push" $ do
          let params = [(0x2, 2)]
          case applySettingsParams defaultSettings params of
            Left (InvalidEnablePush _) -> pure ()
            _ -> expectationFailure "Expected InvalidEnablePush"
      , it "unknown settings ignored" $ do
          let params = [(0xFF, 42)]
          case applySettingsParams defaultSettings params of
            Left err -> expectationFailure (show err)
            Right s -> s `shouldBe` defaultSettings
      ]
  , describe "Initial field validation" $ sequence_
      [ it "accepts request TE trailers and preserves the regular field grammar" $ do
          let fields = [("x-test", ""), ("x-test", BS.pack [0x61, 0x09, 0xFF])]
          validateRequestHead (getHead <> (("te", "Trailers") : fields)) `shouldBe` Nothing
          validateResponseFields fields `shouldBe` Nothing
      , it "rejects invalid field names and values in both directions" $ do
          let badFields =
                [ ("X-Test", "value")
                , ("x test", "value")
                , ("x-test", " padded")
                , ("x-test", BS.singleton 0)
                ]
          mapM_ (\field -> do
            validateRequestHead (getHead <> [field]) `shouldSatisfy` isJust
            validateResponseFields [field] `shouldSatisfy` isJust) badFields
      , it "enforces the section rules for TE and connection-specific fields" $ do
          validateRequestHead (getHead <> [("te", "gzip")]) `shouldBe`
            Just "TE request field must be trailers"
          validateResponseFields [("te", "trailers")] `shouldBe` Just "TE in a response"
          validateResponseFields [("connection", "close")] `shouldBe`
            Just "connection-specific header field"
      , it "requires singleton request pseudo-headers before ordinary fields" $ do
          validateRequestHead (getHead <> [(":path", "/second")]) `shouldBe`
            Just "duplicate request pseudo-header"
          validateRequestHead (getHead <> [("x-test", "value"), (":authority", "example.com")]) `shouldSatisfy` isJust
          validateRequestHead [(":method", "GET"), (":path", "/")] `shouldBe` Just "missing :scheme"
          validateResponseFields [(":status", "200")] `shouldSatisfy` isJust
      , it "validates the classic CONNECT field layout" $ do
          let connect = [(":method", "CONNECT"), (":authority", "[::1]:443")]
          validateRequestHead connect `shouldBe` Nothing
          validateRequestHead (connect <> [(":path", "/")]) `shouldBe`
            Just "CONNECT must omit :scheme and :path"
          validateRequestHead [(":method", "CONNECT"), (":authority", "example.com")] `shouldSatisfy` isJust
      , it "exchanges classic CONNECT headers over HTTP/2" $
          withClientServer connectServer $ \client -> do
            let request = Client.ClientRequest "CONNECT" "/" "https" "[::1]:443" [] Client.ReqBodyNone
            response <- Client.sendRequest client request
            Client.crStatus response `shouldBe` 200
            Client.crResponseHeaders response `shouldBe` [("x-authority", "[::1]:443")]
            Client.crResponseBody response `shouldReturn` Nothing
      ]
  , describe "Request authority" $ sequence_
      [ it "uses Host when :authority is absent" $
          (requestHeadAuthority <$> parseRequestHead (getHead <> [("host", "Example.test")]))
            `shouldBe` Right "Example.test"
      , it "compares normalized host spellings and keeps the original authority" $ do
          let matches authority host =
                (requestHeadAuthority <$> parseRequestHead (getHead <> [(":authority", authority), ("host", host)]))
                  `shouldBe` Right authority
          matches "Example.test:0443" "example.test"
          matches "%65xample.test" "EXAMPLE.test:"
          matches "[2001:db8:0:0:0:0:0:1]:443" "[2001:DB8::1]"
          matches "a%2Fb.test" "A%2fb.test"
      , it "requires distinct authorities to stay distinct" $ do
          let mismatch authority host =
                validateRequestHead (getHead <> [(":authority", authority), ("host", host)])
                  `shouldBe` Just "Host differs from :authority"
          mismatch "example.test:8443" "example.test"
          mismatch "a%21b.test" "a!b.test"
          mismatch "example.test" "other.test"
      , it "validates each authority field and enforces singleton Host" $ do
          mapM_ (\bad -> validateRequestHead (getHead <> [(":authority", bad)]) `shouldSatisfy` isJust)
            ["", "..:65536", "[::1", "example.test/%20", "user@example.test"]
          validateRequestHead (getHead <> [("host", "example.test:abc")]) `shouldBe` Just "invalid Host"
          validateRequestHead (getHead <> [("host", "example.test"), ("host", "example.test")])
            `shouldBe` Just "duplicate Host"
      , it "rejects conflicting outbound fields before allocating a stream" $
          withClientServer echoAuthorityServer $ \client -> do
            let request = Client.ClientRequest "GET" "/" "https" "example.test"
                  [("host", "other.test")] Client.ReqBodyNone
            result <- try @Client.ClientStreamError (Client.sendRequest client request)
            (() <$ result) `shouldBe` Left (Client.ClientRequestHeaderError "Host differs from :authority")
            response <- Client.sendRequest client
              (request {Client.crAuthority = "", Client.crHeaders = [("host", "Example.test:443")]})
            Client.crStreamId response `shouldBe` 1
            Client.crResponseHeaders response `shouldBe` [("x-authority", "Example.test:443")]
            Client.crResponseBody response `shouldReturn` Nothing
      ]
  , describe "Content-Length validation" $ sequence_
      [ it "parses an optional, bounded decimal length" $ do
          readContentLength [] `shouldBe` Right Nothing
          readContentLength [("content-length", "0")] `shouldBe` Right (Just 0)
          readContentLength [("content-length", "18446744073709551615")] `shouldBe` Right (Just maxBound)
      , it "requires all repeated lengths to agree" $ do
          readContentLength [("content-length", "05, 5"), ("content-length", "5")] `shouldBe` Right (Just 5)
          readContentLength [("content-length", "5"), ("content-length", "8")] `shouldBe` Left "conflicting content-length values"
      , it "rejects malformed or overflowing decimals" $
          mapM_ (\value -> readContentLength [("content-length", value)] `shouldBe` Left "invalid content-length")
            ["", "+5", "5x", "5,", " 5,5", "5,5 ", "18446744073709551616"]
      , it "requires the content byte count to match at END_STREAM" $ do
          remainingContentLength 5 2 False `shouldBe` Just 3
          remainingContentLength 3 3 True `shouldBe` Just 0
          remainingContentLength 3 4 False `shouldBe` Nothing
          remainingContentLength 3 2 True `shouldBe` Nothing
          remainingContentLength 3 0 True `shouldBe` Nothing
      ]
  , describe "Trailer validation" $ sequence_
      [ it "accepts empty fields, repeated names, and opaque value bytes" $
          validateTrailerBlock flagEndStream
            [ ("x-test", "")
            , ("x-test", BS.pack [0x61, 0x09, 0xFF])
            , ("custom_name-8", "opaque")
            ] `shouldBe` Nothing
      , it "requires a terminating field block" $
          validateTrailerBlock flagEndHeaders [] `shouldBe`
            Just "trailers without END_STREAM"
      , it "rejects pseudo-headers and invalid field names" $ do
          validateTrailerBlock flagEndStream [(":status", "200")] `shouldBe`
            Just "pseudo-header in trailers"
          mapM_ (\name -> validateTrailerBlock flagEndStream [(name, "")] `shouldBe`
            Just "invalid trailer field name") ["", "Mixed-Case", "two words", "name:part"]
      , it "rejects connection-specific fields" $
          validateTrailerBlock flagEndStream [("connection", "close")] `shouldBe`
            Just "connection-specific trailer field"
      , it "matches the lowercase token alphabet for all octets" $
          mapM_ (\byte ->
            let name = BS.snoc "x-" byte
                allowed = BS.elem byte "0123456789abcdefghijklmnopqrstuvwxyz!#$%&'*+-.^_\x60|~"
            in validateTrailerBlock flagEndStream [(name, "")] `shouldBe`
              if allowed then Nothing else Just "invalid trailer field name") [minBound .. maxBound]
      , it "rejects forbidden value bytes and surrounding whitespace" $
          mapM_ (\value -> validateTrailerBlock flagEndStream [("x-test", value)] `shouldBe`
            Just "invalid trailer field value") [" padded", "padded\t", BS.singleton 0]
      , it "receives a final response and trailers after Early Hints" $
          withClientServer earlyHintsServer $ \client -> do
            let request = Client.ClientRequest "GET" "/" "http" "example.com" [("te", "Trailers")] Client.ReqBodyNone
            response <- Client.sendRequest client request
            Client.crStatus response `shouldBe` 200
            Client.crResponseHeaders response `shouldBe` [("x-result", "final")]
            Client.crResponseBody response `shouldReturn` Just "body"
            Client.crResponseBody response `shouldReturn` Nothing
            Client.crResponseTrailers response `shouldReturn` [("x-result", "trailer")]
      ]
  , describe "FlowControl" $ sequence_
      [ it "consume and release" $ do
          fc <- atomically $ newFlowControl 65535
          consumed <- atomically $ consumeWindow fc 1000
          consumed `shouldBe` True
          avail <- atomically $ availableWindow fc
          avail `shouldBe` 64535
          result <- atomically $ releaseWindow fc 1000
          case result of
            Right () -> pure ()
            Left _ -> expectationFailure "Should not overflow"
          avail2 <- atomically $ availableWindow fc
          avail2 `shouldBe` 65535
      , it "refuse over-consume" $ do
          fc <- atomically $ newFlowControl 100
          consumed <- atomically $ consumeWindow fc 200
          not consumed `shouldBe` True
      , it "detect overflow" $ do
          fc <- atomically $ newFlowControl 2147483647
          result <- atomically $ releaseWindow fc 1
          case result of
            Left _ -> pure ()
            Right () -> expectationFailure "Should overflow"
      ]
  , describe "Client/Server handshake" $ sequence_
      [ it "basic preface exchange" $ do
          (clientSock, serverSock) <- socketPair
          serverDone <- newEmptyMVar
          _ <- forkIO $ do
            preface <- recvExact' serverSock 24
            preface `shouldBe` connectionPreface
            let settingsAck = encodeFrame $ Frame
                  (FrameHeader 0 FrameSettings flagAck 0)
                  (SettingsFrame [])
                serverSettings = encodeFrame $ Frame
                  (FrameHeader 0 FrameSettings 0 0)
                  (SettingsFrame [])
            NBS.sendMany serverSock [serverSettings, settingsAck]
            putMVar serverDone ()
          NBS.sendMany clientSock [connectionPreface]
          let clientSettings = encodeFrame $ Frame
                (FrameHeader 0 FrameSettings 0 0)
                (SettingsFrame [])
          NBS.sendMany clientSock [clientSettings]
          takeMVar serverDone
          NS.close clientSock
          NS.close serverSock
      ]
  , describe "Fragmented fields" $ do
      it "receives padded request headers and terminating trailers across frames" receiveFragmentedRequest
      it "receives padded response headers, a push promise, and trailers across frames" receiveFragmentedResponse
  , it "acknowledges repeated Expect on an open request with content" receiveContinue
  , it "starts an HTTP/2 upload after 100 Continue" (sendContinueBody True Nothing)
  , it "falls back after waiting for 100 Continue despite Early Hints" (sendContinueBody False Nothing)
  , it "uses the HTTP/2 request's continue timeout" (sendContinueBody False (Just 25000))
  , it "skips an expected upload, drains the early response, and reuses the connection" skipContinueBody
  , it "cancels a deferred upload while waiting for Continue" cancelContinueBody
  ]

getHead :: [Header]
getHead = [(":method", "GET"), (":scheme", "https"), (":path", "/")]

withClientServer :: Server.ServerConfig -> (Client.ClientHandle -> IO a) -> IO a
withClientServer config action =
  bracket socketPair (\(clientSock, serverSock) -> NS.close clientSock >> NS.close serverSock) $
    \(clientSock, serverSock) ->
      bracket (forkIO (Server.runServerOnSocket config serverSock)) killThread $ \_ ->
        Client.withConnectionOnTransport Client.defaultClientConfig (socketTransport clientSock) (Just clientSock) action

connectServer :: Server.ServerConfig
connectServer = Server.defaultServerConfig
  { Server.serverHandler = \request respond -> do
      Server.requestMethod request `shouldBe` "CONNECT"
      Server.requestPath request `shouldBe` ""
      Server.requestScheme request `shouldBe` ""
      respond (Server.defaultResponse { Server.responseHeaders = [("x-authority", Server.requestAuthority request)] })
  }

echoAuthorityServer :: Server.ServerConfig
echoAuthorityServer = Server.defaultServerConfig
  { Server.serverHandler = \request respond ->
      respond (Server.defaultResponse { Server.responseHeaders = [("x-authority", Server.requestAuthority request)] })
  }

earlyHintsServer :: Server.ServerConfig
earlyHintsServer = Server.defaultServerConfig
  { Server.serverHandler = \_ respond -> do
      respond (Server.defaultResponse
        { Server.responseStatus = 103
        , Server.responseHeaders = [("link", "</style.css>; rel=preload; as=style")]
        })
      respond (Server.defaultResponse
        { Server.responseHeaders = [("x-result", "final")]
        , Server.responseBody = Server.ResponseBodyBS "body"
        , Server.responseTrailers = [("x-result", "trailer")]
        })
  }

sendContinueBody :: Bool -> Maybe Int -> IO ()
sendContinueBody sendContinue timeoutUs = withContinuePeer peer $ \client -> do
  let request = continueRequest (Client.ReqBodyBytes "body")
      send = maybe Client.sendRequest Client.sendRequestWithContinueTimeout timeoutUs
  response <- send client request
  Client.crStatus response `shouldBe` 204
  Client.drainResponseBody response `shouldReturn` ""
  where
    peer sock = do
      sid <- awaitRequest sock
      table <- newDynamicTable 4096
      sendTestHead sock table sid False [(":status", "103")]
      start <- getMonotonicTimeNSec
      if sendContinue
        then do
          -- Early Hints alone leaves the request body waiting.
          premature <- timeout 100000 (recvTestStreamFrame sock sid)
          isJust premature `shouldBe` False
          sendTestHead sock table sid False [(":status", "100")]
        else pure ()
      Frame hdr payload <- recvTestStreamFrame sock sid
      fhType hdr `shouldBe` FrameData
      testFlag (fhFlags hdr) flagEndStream `shouldBe` True
      rawPayload payload `shouldBe` "body"
      finish <- getMonotonicTimeNSec
      case timeoutUs of
        Just _ -> (finish - start) `shouldSatisfy` (< 700000000)
        Nothing
          | sendContinue -> (finish - start) `shouldSatisfy` (< 850000000)
          | otherwise -> (finish - start) `shouldSatisfy` (>= 700000000)
      sendTestHead sock table sid True [(":status", "204")]

skipContinueBody :: IO ()
skipContinueBody = withContinuePeer peer $ \client -> do
  waiting <- newIORef (Just "body")
  let produce = atomicModifyIORef' waiting (\next -> (Nothing, next))
  Client.withResponse client (continueRequest (Client.ReqBodyStream produce)) $ \response -> do
    Client.crStatus response `shouldBe` 417
    Client.drainResponseBody response `shouldReturn` "denied"
    Client.crResponseTrailers response `shouldReturn` [("x-result", "complete")]
  readIORef waiting `shouldReturn` Just "body"
  let next = Client.ClientRequest "GET" "/" "http" "localhost" [] Client.ReqBodyNone
  response <- Client.sendRequest client next
  Client.crStatus response `shouldBe` 204
  Client.drainResponseBody response `shouldReturn` ""
  where
    peer sock = do
      sid <- awaitRequest sock
      table <- newDynamicTable 4096
      sendTestHead sock table sid False [(":status", "103")]
      sendTestHead sock table sid False [(":status", "417"), ("content-length", "6")]
      let dataFrame = Frame (FrameHeader 6 FrameData 0 sid) (DataFrame "denied")
      NBS.sendAll sock (encodeFrame dataFrame)
      sendTestHead sock table sid True [("x-result", "complete")]
      NBS.sendAll sock (encodeFrame (Frame (FrameHeader 4 FrameRSTStream 0 sid) (RSTStreamFrame NoError)))
      Frame hdr payload <- recvTestStreamFrame sock sid
      fhType hdr `shouldBe` FrameRSTStream
      rawPayload payload `shouldBe` BS.pack [0, 0, 0, 8]
      nextSid <- awaitRequest sock
      nextSid `shouldBe` sid + 2
      sendTestHead sock table nextSid True [(":status", "204")]

cancelContinueBody :: IO ()
cancelContinueBody = withContinuePeer peer $ \client -> do
  bodyChunk <- newIORef (Just "body")
  let produce = atomicModifyIORef' bodyChunk (\next -> (Nothing, next))
  Client.withResponseDeferred client (continueRequest (Client.ReqBodyStream produce)) (const (pure ()))
  readIORef bodyChunk `shouldReturn` Just "body"
  where
    peer sock = do
      sid <- awaitRequest sock
      Frame hdr payload <- recvTestStreamFrame sock sid
      fhType hdr `shouldBe` FrameRSTStream
      rawPayload payload `shouldBe` BS.pack [0, 0, 0, 8]

continueRequest :: Client.RequestBody -> Client.ClientRequest
continueRequest = Client.ClientRequest "POST" "/" "http" "localhost"
  [("content-length", "4"), ("expect", "extension=ready"), ("expect", "100-Continue")]

withContinuePeer :: (Socket -> IO ()) -> (Client.ClientHandle -> IO a) -> IO a
withContinuePeer peer action =
  bracket socketPair (\(client, server) -> NS.close client >> NS.close server) $ \(client, server) -> do
    verified <- newEmptyMVar
    let serve = do
          recvExact' server (BS.length connectionPreface) `shouldReturn` connectionPreface
          hdr <- recvTestFrame server
          fhType hdr `shouldBe` FrameSettings
          NBS.sendMany server (map encodeFrame [settingsFrame 0, settingsFrame flagAck])
          peer server
    bracket (forkFinally serve (putMVar verified)) killThread $ \_ -> do
      value <- Client.withConnectionOnTransport Client.defaultClientConfig (socketTransport client) (Just client) action
      either throwIO pure =<< takeMVar verified
      pure value

sendTestHead :: Socket -> DynamicTable -> StreamId -> Bool -> [Header] -> IO ()
sendTestHead sock table sid ended fields = do
  block <- encodeHeaderBlock defaultEncodeStrategy table fields
  NBS.sendMany sock (map encodeFrame (headerBlockFrames sid ended 0 block 16384))

recvTestStreamFrame :: Socket -> StreamId -> IO Frame
recvTestStreamFrame sock sid = do
  frame <- recvRawTestFrame sock
  if fhStreamId (frameHeader frame) == sid then pure frame else recvTestStreamFrame sock sid

receiveContinue :: IO ()
receiveContinue =
  bracket socketPair (\(client, server) -> NS.close client >> NS.close server) $ \(client, server) ->
    bracket (forkIO (Server.runServerOnSocket config server)) killThread $ \_ -> do
      encoder <- newDynamicTable 4096
      decoder <- newDynamicTable 4096
      let start sid ended extra = do
            block <- encodeHeaderBlock defaultEncodeStrategy encoder
              ([(":method", "POST"), (":scheme", "http"), (":path", "/"), (":authority", "localhost")] <> extra)
            NBS.sendMany client (map encodeFrame (headerBlockFrames sid ended 0 block 16384))
          finish sid body =
            NBS.sendAll client (encodeFrame (Frame (FrameHeader (fromIntegral (BS.length body)) FrameData flagEndStream sid) (DataFrame body)))
          checkHead sid ended expected = do
            Frame hdr payload <- recvTestHeaders client
            fhStreamId hdr `shouldBe` sid
            testFlag (fhFlags hdr) flagEndStream `shouldBe` ended
            decodeHeaderBlock decoder (rawPayload payload) `shouldReturn` Right expected
          expectation = [("expect", "100-continue")]
      NBS.sendMany client [connectionPreface, encodeFrame (settingsFrame 0), encodeFrame (settingsFrame flagAck)]
      start 1 False [("content-length", "4"), ("expect", "extension=ready"), ("expect", ", 100-Continue")]
      checkHead 1 False [(":status", "100")]
      finish 1 "body"
      checkHead 1 True [(":status", "204"), ("x-received", "body")]
      start 3 True expectation
      checkHead 3 True [(":status", "204"), ("x-received", "")]
      start 5 False (("content-length", "0") : expectation)
      finish 5 ""
      checkHead 5 True [(":status", "204"), ("x-received", "")]
      start 7 False [("content-length", "4"), ("expect", "extension=\"100-continue\"")]
      finish 7 "body"
      checkHead 7 True [(":status", "204"), ("x-received", "body")]
  where
    config = Server.defaultServerConfig
      { Server.serverHandler = \request respond -> do
          body <- Server.requestBody request
          respond Server.defaultResponse
            { Server.responseStatus = 204
            , Server.responseHeaders = [("x-received", body)]
            }
      }

    recvTestHeaders sock = do
      frame@(Frame hdr _) <- recvRawTestFrame sock
      if fhType hdr == FrameHeaders
        then pure frame
        else recvTestHeaders sock

receiveFragmentedRequest :: IO ()
receiveFragmentedRequest = do
  table <- newDynamicTable 4096
  let encode = encodeHeaderBlock defaultEncodeStrategy table
  initial <- encode
    [ (":method", "POST"), (":scheme", "https"), (":path", "/echo")
    , (":authority", "example.com"), ("x-metadata", "request value")
    , ("content-length", "4")
    ]
  trailers <- encode [("x-metadata", "request value"), ("x-result", "complete")]
  checked <- newEmptyMVar
  let config = Server.defaultServerConfig
        { Server.serverHandler = \request respond -> do
            result <- try @SomeException $ do
              Server.requestPath request `shouldBe` "/echo"
              lookup "x-metadata" (Server.requestHeaders request) `shouldBe` Just "request value"
              Server.requestBody request `shouldReturn` "body"
              Server.requestBody request `shouldReturn` BS.empty
              Server.requestTrailers request `shouldReturn`
                [("x-metadata", "request value"), ("x-result", "complete")]
              respond Server.defaultResponse
            putMVar checked result
        }
      frames = settingsFrame flagAck : paddedHeaders 1 False initial
        <> [Frame (FrameHeader 4 FrameData 0 1) (DataFrame "body")]
        <> paddedHeaders 1 True trailers
  bracket socketPair (\(client, server) -> NS.close client >> NS.close server) $ \(client, server) ->
    bracket (forkIO (Server.runServerOnSocket config server)) killThread $ \_ -> do
      NBS.sendMany client [connectionPreface, encodeFrame (settingsFrame 0)]
      hello <- recvTestFrame client
      fhType hello `shouldBe` FrameSettings
      NBS.sendMany client (map encodeFrame frames)
      either throwIO pure =<< takeMVar checked

receiveFragmentedResponse :: IO ()
receiveFragmentedResponse = do
  table <- newDynamicTable 4096
  let encode = encodeHeaderBlock defaultEncodeStrategy table
      promisedHeaders = [(":method", "GET"), (":scheme", "https"), (":path", "/style.css")
        , (":authority", "example.com"), ("x-metadata", "pushed value")]
  promise <- encode promisedHeaders
  pushed <- encode [(":status", "200"), ("x-metadata", "pushed value")]
  initial <- encode [(":status", "200"), ("x-metadata", "response value"), ("content-length", "4")]
  trailers <- encode [("x-metadata", "response value"), ("x-result", "complete")]
  let reply server = do
        _ <- recvExact' server (BS.length connectionPreface)
        _ <- recvTestFrame server
        NBS.sendMany server (map encodeFrame [settingsFrame 0, settingsFrame flagAck])
        sid <- awaitRequest server
        let frames = paddedPromise sid 2 promise <> paddedHeaders 2 True pushed
              <> paddedHeaders sid False initial
              <> [Frame (FrameHeader 4 FrameData 0 sid) (DataFrame "body")]
              <> paddedHeaders sid True trailers
        NBS.sendMany server (map encodeFrame frames)
      config = Client.defaultClientConfig
        { Client.clientSettings = defaultSettings {settingsEnablePush = True} }
      request = Client.ClientRequest "GET" "/" "https" "example.com" [] Client.ReqBodyNone
  bracket socketPair (\(client, server) -> NS.close client >> NS.close server) $ \(client, server) ->
    bracket (forkIO (reply server)) killThread $ \_ ->
      Client.withConnectionOnTransport config (socketTransport client) (Just client) $ \conn -> do
        response <- Client.sendRequest conn request
        Client.crStatus response `shouldBe` 200
        lookup "x-metadata" (Client.crResponseHeaders response) `shouldBe` Just "response value"
        Client.drainResponseBody response `shouldReturn` "body"
        Client.crResponseTrailers response `shouldReturn`
          [("x-metadata", "response value"), ("x-result", "complete")]
        [pushedResponse] <- Client.crPushPromises response
        Client.pprPromisedStreamId pushedResponse `shouldBe` 2
        Client.pprHeaders pushedResponse `shouldBe` promisedHeaders
        fulfilled <- Client.pprFulfil pushedResponse
        Client.crResponseHeaders fulfilled `shouldBe` [("x-metadata", "pushed value")]
        Client.crResponseBody fulfilled `shouldReturn` Nothing

paddedHeaders :: StreamId -> Bool -> ByteString -> [Frame]
paddedHeaders sid end block =
  case headerBlockFrames sid end 0 block 4 of
    Frame hdr first : rest ->
      padFrame (hdr {fhFlags = fhFlags hdr .|. flagPriority})
        (HeadersFrame (Just (Priority False 0 16)) (rawPayload first)) : rest
    [] -> error "expected initial HEADERS"

paddedPromise :: StreamId -> StreamId -> ByteString -> [Frame]
paddedPromise sid promised block =
  case headerBlockFrames sid False 0 block 4 of
    Frame hdr first : rest ->
      padFrame (hdr {fhType = FramePushPromise}) (PushPromiseFrame promised (rawPayload first)) : rest
    [] -> error "expected initial PUSH_PROMISE"

padFrame :: FrameHeader -> FramePayload -> Frame
padFrame hdr payload =
  let padded = BS.singleton 2 <> rawPayload payload <> BS.replicate 2 0
  in Frame (hdr {fhLength = fromIntegral (BS.length padded), fhFlags = fhFlags hdr .|. flagPadded})
       (FramePayloadRaw padded)

settingsFrame :: FrameFlags -> Frame
settingsFrame flags = Frame (FrameHeader 0 FrameSettings flags 0) (SettingsFrame [])

awaitRequest :: Socket -> IO StreamId
awaitRequest sock = do
  hdr <- recvTestFrame sock
  if fhType hdr == FrameHeaders then pure (fhStreamId hdr) else awaitRequest sock

recvTestFrame :: Socket -> IO FrameHeader
recvTestFrame sock = frameHeader <$> recvRawTestFrame sock

recvRawTestFrame :: Socket -> IO Frame
recvRawTestFrame sock = do
  bytes <- recvExact' sock frameHeaderLength
  hdr <- either (fail . show) pure (decodeFrameHeader bytes)
  body <- recvExact' sock (fromIntegral (fhLength hdr))
  pure (Frame hdr (FramePayloadRaw body))

socketPair :: IO (Socket, Socket)
socketPair = do
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> error "No address"
    (addr:_) -> do
      listenSock <- NS.openSocket addr
      NS.setSocketOption listenSock NS.ReuseAddr 1
      NS.bind listenSock (NS.addrAddress addr)
      NS.listen listenSock 1
      boundAddr <- NS.getSocketName listenSock
      clientSock <- NS.openSocket addr
      NS.connect clientSock boundAddr
      (serverSock, _) <- NS.accept listenSock
      NS.close listenSock
      pure (clientSock, serverSock)

recvExact' :: Socket -> Int -> IO ByteString
recvExact' sock n = go n []
  where
    go 0 acc = pure (BS.concat (reverse acc))
    go remaining acc = do
      chunk <- NBS.recv sock (min remaining 4096)
      if BS.null chunk
        then pure (BS.concat (reverse acc))
        else go (remaining - BS.length chunk) (chunk : acc)
