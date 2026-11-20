{-# LANGUAGE RankNTypes #-}
module Test.EngineClient (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, throwIO, try)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.Bits ((.|.))
import Data.IORef (newIORef, readIORef, writeIORef)
import Foreign.Ptr (castPtr)
import Network.HTTP2.Client (ClientStreamError (..))
import Network.HTTP2.Connection (headerBlockFrames)
import Network.HTTP2.Engine.Client qualified as Engine
import Network.HTTP2.Engine.Run.Client
import Network.HTTP2.Engine.Run.Server qualified as Server
import Network.HTTP2.Engine.Settings qualified as Server
import Network.HTTP2.Engine.Types
import Network.HTTP2.Frame
import Network.HTTP2.HPACK (decodeHeaderBlock, defaultEncodeStrategy, encodeHeaderBlock, newDynamicTable)
import Network.HTTP2.Transport (SendFn)
import Network.HTTP2.Types
import Network.Socket (SockAddr (..))
import System.IO.Error (ioeGetErrorString)
import System.Timeout (timeout)
import Test.Syd

tests :: Spec
tests = do
  failures
  responseHeaders
  continueUploads
  requestAuthorityAndTrailers
  requestContinue
  largeFieldBlocks
  outboundTrailerMakers

failures :: Spec
failures = describe "Engine client failures" $ do
  it "wakes a request reset before response headers" $
    withinDeadline $ withPeer (\sid -> [reset sid]) $ \send -> do
      result <- try (send request (const (pure ())))
      expectReset result
  it "wakes header waiters after a fatal HPACK error" $
    withinDeadline $ withPeer (\sid -> [headers sid (BS.singleton 0x80)]) $ \send -> do
      -- Indexed representation with index zero is invalid (RFC 7541 6.1).
      result <- try (send request (const (pure ())))
      case result of
        Left ClientStreamConnectionClosed -> pure ()
        other -> expectationFailure ("Expected connection failure, got " ++ show (other :: Either ClientStreamError ()))
  it "does not block other streams when resetting an abandoned full body queue" $
    withinDeadline $ withPeer responses $ \send ->
      send request $ \first -> do
        -- The first body remains unread while the next request waits for
        -- headers. Dispatching stream 1's reset must not block on its queue.
        result <- try (send request (const (pure ())))
        expectReset result
        firstResult <- try (inpObjBody first >> pure ())
        expectReset firstResult
  where
    responses sid
      | sid == 1 = headers sid (BS.singleton 0x88)
          : replicate 64 (Frame (FrameHeader 1 FrameData 0 sid) (DataFrame "x"))
          ++ [reset sid]
      | otherwise = [reset sid]

responseHeaders :: Spec
responseHeaders = describe "Engine client response headers" $ do
  it "waits for the final response and delivers its separate gRPC trailers" $
    withinDeadline $ do
      table <- newDynamicTable 4096
      let encode = encodeHeaderBlock defaultEncodeStrategy table
      hints <- encode [(":status", "103"), ("link", "</schema>; rel=preload")]
      final <- encode [(":status", "200"), ("content-type", "application/grpc"), ("content-length", "4")]
      trailers <- encode [("grpc-status", "0"), ("x-result", "done")]
      let frames sid =
            headerBlockFrames sid False 0 hints 16384
              <> paddedFirstFragment (headerBlockFrames sid False 0 final 7)
              <> [paddedData sid "bo", paddedData sid "dy"]
              <> headerBlockFrames sid True 0 trailers 5
      withPeer frames $ \send -> send request $ \response -> do
        lookupToken ":status" (inpObjHeaders response) `shouldBe` Just "200"
        lookupToken "link" (inpObjHeaders response) `shouldBe` Nothing
        inpObjBodySize response `shouldBe` Just 4
        drainInput response `shouldReturn` "body"
        trailersTable <- readIORef (inpObjTrailers response)
        (trailersTable >>= lookupToken "grpc-status") `shouldBe` Just "0"
        (trailersTable >>= lookupToken "x-result") `shouldBe` Just "done"
  it "makes trailers-only status and metadata available with the response head" $
    withinDeadline $ do
      table <- newDynamicTable 4096
      block <- encodeHeaderBlock defaultEncodeStrategy table
        [ (":status", "200"), ("content-type", "application/grpc"), ("content-length", "0")
        , ("grpc-status", "7"), ("grpc-message", "denied")
        ]
      withPeer (\sid -> headerBlockFrames sid True 0 block 7) $ \send -> send request $ \response -> do
        lookupToken "grpc-status" (inpObjHeaders response) `shouldBe` Just "7"
        trailersTable <- readIORef (inpObjTrailers response)
        (trailersTable >>= lookupToken "grpc-message") `shouldBe` Just "denied"
        inpObjBodySize response `shouldBe` Just 0
        inpObjBody response `shouldReturn` ("", True)
  it "preserves representation length for HEAD and 304 responses with no body" $
    withinDeadline $ do
      let checkResponse method status = do
            table <- newDynamicTable 4096
            block <- encodeHeaderBlock defaultEncodeStrategy table
              [(":status", status), ("content-length", "128")]
            let outgoing = OutObj [(":method", method), (":path", "/")] OutBodyNone defaultTrailersMaker
            withPeer (\sid -> headerBlockFrames sid True 0 block 16384) $ \send -> send outgoing $ \response -> do
              lookupToken "content-length" (inpObjHeaders response) `shouldBe` Just "128"
              inpObjBodySize response `shouldBe` Just 0
              inpObjBody response `shouldReturn` ("", True)
      checkResponse "HEAD" "200"
      checkResponse "GET" "304"

continueUploads :: Spec
continueUploads = describe "Engine client Expect" $ do
  it "skips a streaming upload and keeps the complete response after a NO_ERROR reset" $
    withinDeadline $ do
      table <- newDynamicTable 4096
      let encode = encodeHeaderBlock defaultEncodeStrategy table
      hints <- encode [(":status", "103")]
      final <- encode [(":status", "417"), ("content-length", "6")]
      trailers <- encode [("x-result", "complete")]
      producerCalled <- newIORef False
      sent <- newTVarIO []
      let response sid =
            headerBlockFrames sid False 0 hints 16384
            <> headerBlockFrames sid False 0 final 16384
            <> [Frame (FrameHeader 6 FrameData 0 sid) (DataFrame "denied")]
            <> headerBlockFrames sid True 0 trailers 16384
            <> [Frame (FrameHeader 4 FrameRSTStream 0 sid) (RSTStreamFrame NoError)]
          outgoing = OutObj
            [(":method", "POST"), (":path", "/"), ("content-length", "4"), ("expect", "100-continue")]
            (OutBodyStreamingIface $ \iface -> do
              writeIORef producerCalled True
              outBodyPushFinal iface (BSB.byteString "body"))
            defaultTrailersMaker
          observe hdr = atomically $ modifyTVar' sent (fhType hdr :)
      withObservedPeer observe response $ \send -> send outgoing $ \incoming -> do
        lookupToken ":status" (inpObjHeaders incoming) `shouldBe` Just "417"
        drainInput incoming `shouldReturn` "denied"
        result <- readIORef (inpObjTrailers incoming)
        (result >>= lookupToken "x-result") `shouldBe` Just "complete"
      readIORef producerCalled `shouldReturn` False
      frames <- readTVarIO sent
      frames `shouldSatisfy` elem FrameRSTStream
      frames `shouldSatisfy` notElem FrameData

  it "uploads a fixed body to an engine server after continue" $
    withinDeadline $ do
      let handler incoming _ respond = do
            body <- drainInput incoming
            respond (OutObj [(":status", "204"), ("x-received", body)] OutBodyNone defaultTrailersMaker) []
          outgoing = OutObj
            [(":method", "POST"), (":path", "/"), ("expect", "extension=ready"), ("expect", "100-Continue")]
            (OutBodyBuilder (BSB.byteString "body"))
            defaultTrailersMaker
      withEnginePair handler $ \send -> send outgoing $ \incoming -> do
        lookupToken ":status" (inpObjHeaders incoming) `shouldBe` Just "204"
        lookupToken "x-received" (inpObjHeaders incoming) `shouldBe` Just "body"
        drainInput incoming `shouldReturn` ""

requestAuthorityAndTrailers :: Spec
requestAuthorityAndTrailers =
  it "delivers Host-derived authority and terminating request metadata to the engine server" $
    withinDeadline $ do
      table <- newDynamicTable 4096
      let encode = encodeHeaderBlock defaultEncodeStrategy table
      initial <- encode
        [ (":method", "POST"), (":scheme", "https"), (":path", "/echo")
        , ("host", "Example.test"), ("content-length", "4")
        ]
      trailers <- encode [("x-request-end", "done")]
      let frames = headerBlockFrames 1 False 0 initial 7
            <> [paddedData 1 "bo", paddedData 1 "dy"]
            <> headerBlockFrames 1 True 0 trailers 5
      input <- newTVarIO (connectionPreface <> BS.concat (map encodeFrame frames))
      verified <- newEmptyTMVarIO
      let send _ len = pure len
          readN = readInput input
          handler incoming _ _ = do
            result <- try @SomeException $ do
              lookupToken ":authority" (inpObjHeaders incoming) `shouldBe` Just "Example.test"
              inpObjBodySize incoming `shouldBe` Just 4
              drainInput incoming `shouldReturn` "body"
              received <- readIORef (inpObjTrailers incoming)
              (received >>= lookupToken "x-request-end") `shouldBe` Just "done"
            atomically $ putTMVar verified result
      withEngineServer send readN handler $
        either throwIO pure =<< atomically (takeTMVar verified)

requestContinue :: Spec
requestContinue =
  it "acknowledges Expect before the engine handler waits for request content" $
    withinDeadline $ do
      encoder <- newDynamicTable 4096
      decoder <- newDynamicTable 4096
      initial <- encodeHeaderBlock defaultEncodeStrategy encoder
        [ (":method", "POST"), (":scheme", "https"), (":path", "/echo"), (":authority", "example.com")
        , ("expect", "extension=ready"), ("expect", "100-Continue")
        ]
      let settings = Frame (FrameHeader 0 FrameSettings 0 0) (SettingsFrame [])
          requestFrames = settings : headerBlockFrames 1 False 0 initial 16384
      input <- newTVarIO (connectionPreface <> BS.concat (map encodeFrame requestFrames))
      output <- newTVarIO BS.empty
      let handler incoming _ respond = do
            body <- drainInput incoming
            respond (OutObj [(":status", "204"), ("x-received", body)] OutBodyNone defaultTrailersMaker) []
          receiveHead = do
            bytes <- readInput output frameHeaderLength
            hdr <- either (fail . show) pure (decodeFrameHeader bytes)
            payload <- readInput output (fromIntegral (fhLength hdr))
            if fhType hdr == FrameHeaders then pure (Frame hdr (FramePayloadRaw payload)) else receiveHead
          checkHead ended expected = do
            Frame hdr payload <- receiveHead
            fhStreamId hdr `shouldBe` 1
            testFlag (fhFlags hdr) flagEndStream `shouldBe` ended
            decodeHeaderBlock decoder (rawPayload payload) `shouldReturn` Right expected
      withEngineServer (writeInput output) (readInput input) handler $ do
        checkHead False [(":status", "100")]
        atomically $ modifyTVar' input (<> encodeFrame (Frame (FrameHeader 4 FrameData flagEndStream 1) (DataFrame "body")))
        checkHead True [(":status", "204"), ("x-received", "body")]

largeFieldBlocks :: Spec
largeFieldBlocks =
  it "exchanges large request headers, response headers, trailers, and DATA between engines" $
    withinDeadline $ do
      let metadata = BS.replicate 30000 0x61
          body = BS.replicate 48000 0x62
          requestWithMetadata = OutObj
            [(":method", "POST"), (":path", "/echo"), ("x-request", metadata), ("content-length", "48000")]
            (OutBodyBuilder (BSB.byteString body))
            (makeTrailers "x-request-end")
          makeTrailers name =
            pure . maybe (Trailers [(name, metadata)]) (const (NextTrailersMaker (makeTrailers name)))
          handler incoming _ respond = do
            lookupToken "x-request" (inpObjHeaders incoming) `shouldBe` Just metadata
            inpObjBodySize incoming `shouldBe` Just (BS.length body)
            drainInput incoming `shouldReturn` body
            trailers <- readIORef (inpObjTrailers incoming)
            (trailers >>= lookupToken "x-request-end") `shouldBe` Just metadata
            respond (OutObj [(":status", "200"), ("x-response", metadata), ("content-length", "48000")]
              (OutBodyBuilder (BSB.byteString body)) (makeTrailers "x-response-end")) []
      withEnginePair handler $ \send -> send requestWithMetadata $ \response -> do
        lookupToken "x-response" (inpObjHeaders response) `shouldBe` Just metadata
        inpObjBodySize response `shouldBe` Just (BS.length body)
        drainInput response `shouldReturn` body
        trailers <- readIORef (inpObjTrailers response)
        (trailers >>= lookupToken "x-response-end") `shouldBe` Just metadata

outboundTrailerMakers :: Spec
outboundTrailerMakers = describe "Engine outbound trailers" $ do
  it "ends a request without a trailer maker on its final DATA chunk" $
    withinDeadline $ do
      let Engine.Request outgoing = Engine.requestStreamingIface "POST" "/" [] $ \iface ->
            outBodyPushFinal iface (BSB.byteString "body")
          handler incoming _ respond = do
            inpObjBody incoming `shouldReturn` ("body", True)
            trailers <- readIORef (inpObjTrailers incoming)
            fmap detokeniseHeaders trailers `shouldBe` Just []
            respond emptyResponse []
      withEnginePair handler $ \send -> send outgoing $ \response ->
        drainInput response `shouldReturn` ""
  it "sends trailers for a message without a body" $
    roundTripTrailers True [] (const OutBodyNone)
  it "threads the successor maker through a fixed body" $
    roundTripTrailers True ["body"] (const (OutBodyBuilder (BSB.byteString "body")))
  it "finishes trailers after a streaming producer returns" $
    roundTripTrailers False ["first", "last"] $ \ready ->
      OutBodyStreaming $ \push flush -> do
        push (BSB.byteString "first")
        flush
        push (BSB.byteString "last")
        ready
  it "defers trailers until after final push and keeps cancellation a no-op" $
    roundTripTrailers False ["first", "last"] $ \ready ->
      OutBodyStreamingIface $ \iface -> do
        outBodyPush iface (BSB.byteString "first")
        outBodyPushFinal iface (BSB.byteString "last")
        outBodyCancel iface Nothing
        ready
  it "reports a failed request trailer producer and allows the next request" $
    withinDeadline $ do
      let handler incoming _ respond = do
            _ <- drainInput incoming
            respond emptyResponse []
          outgoing = OutObj [(":method", "POST"), (":path", "/fail")]
            (OutBodyStreamingIface (\iface -> outBodyPushFinal iface (BSB.byteString "body")))
            failingTrailers
      withEnginePair handler $ \send -> do
        result <- try @IOError (send outgoing drainInput)
        case result of
          Left err -> ioeGetErrorString err `shouldBe` "trailer producer failed"
          Right _ -> expectationFailure "expected the trailer producer error"
        send request $ \response -> drainInput response `shouldReturn` ""
  it "resets a response whose trailer producer fails and allows the next request" $
    withinDeadline $ do
      let handler incoming _ respond = do
            _ <- drainInput incoming
            let response = if lookupToken ":path" (inpObjHeaders incoming) == Just "/fail"
                  then OutObj [(":status", "200")] (OutBodyBuilder "body") failingTrailers
                  else emptyResponse
            respond response []
          outgoing = OutObj [(":method", "GET"), (":path", "/fail")] OutBodyNone defaultTrailersMaker
      withEnginePair handler $ \send -> do
        result <- try @ClientStreamError (send outgoing drainInput)
        result `shouldBe` Left (ClientStreamReset InternalError)
        send request $ \response -> drainInput response `shouldReturn` ""
  where
    emptyResponse = OutObj [(":status", "200")] OutBodyNone defaultTrailersMaker
    failingTrailers Nothing = throwIO (userError "trailer producer failed")
    failingTrailers (Just _) = pure (NextTrailersMaker failingTrailers)

roundTripTrailers :: Bool -> [BS.ByteString] -> (IO () -> OutBody) -> IO ()
roundTripTrailers initiallyReady chunks makeBody = withinDeadline $ do
  requestReady <- newIORef initiallyReady
  let baseRequest = Engine.Request $ outObjWithNoTrailers [(":method", "POST"), (":path", "/")]
        (makeBody (writeIORef requestReady True))
      Engine.Request outgoing =
        Engine.setRequestTrailersMaker baseRequest (observingTrailers (readIORef requestReady))
      handler incoming _ respond = do
        checkMessage incoming
        responseReady <- newIORef initiallyReady
        respond (OutObj [(":status", "200")]
          (makeBody (writeIORef responseReady True))
          (observingTrailers (readIORef responseReady))) []
      checkMessage incoming = do
        drainInput incoming `shouldReturn` BS.concat chunks
        trailers <- readIORef (inpObjTrailers incoming)
        fmap detokeniseHeaders trailers `shouldBe`
          Just (map ("x-chunk",) (chunks <> ["complete"]))
  withEnginePair handler $ \send -> send outgoing checkMessage

observingTrailers :: IO Bool -> TrailersMaker
observingTrailers ready = collect []
  where
    collect chunks (Just bytes) = pure (NextTrailersMaker (collect (bytes : chunks)))
    collect chunks Nothing = do
      ready `shouldReturn` True
      pure (Trailers (map ("X-Chunk",) (reverse chunks <> ["complete"])))

withEnginePair
  :: (InpObj -> Server.EngineAux -> (OutObj -> [Server.EnginePushPromise] -> IO ()) -> IO ())
  -> ((forall r. OutObj -> (InpObj -> IO r) -> IO r) -> IO a)
  -> IO a
withEnginePair handler action = do
  serverInput <- newTVarIO BS.empty
  clientInput <- newTVarIO BS.empty
  let env = RunEnv "localhost" (writeInput serverInput) (readInput clientInput) 65535 65535 (Just 100) "http"
  withEngineServer (writeInput clientInput) (readInput serverInput) handler $
    runClient env $ \send _ -> action send

paddedFirstFragment :: [Frame] -> [Frame]
paddedFirstFragment (Frame hdr (HeadersFrame _ fragment) : rest) =
  let payload = BS.pack [2, 0, 0, 0, 0, 0] <> fragment <> BS.replicate 2 0
      flags = fhFlags hdr .|. flagPadded .|. flagPriority
  in Frame (hdr {fhFlags = flags, fhLength = fromIntegral (BS.length payload)}) (FramePayloadRaw payload) : rest
paddedFirstFragment frames = frames

paddedData :: StreamId -> BS.ByteString -> Frame
paddedData sid bytes =
  let body = BS.singleton 2 <> bytes <> BS.replicate 2 0
  in Frame (FrameHeader (fromIntegral (BS.length body)) FrameData flagPadded sid) (DataFrame body)

withEngineServer
  :: SendFn
  -> (Int -> IO BS.ByteString)
  -> (InpObj -> Server.EngineAux -> (OutObj -> [Server.EnginePushPromise] -> IO ()) -> IO ())
  -> IO a
  -> IO a
withEngineServer send readN handler action =
  bracket (Engine.allocConfigForTransport send readN) Engine.freeSimpleConfig $ \config -> do
    let addr = SockAddrInet 0 0
        env = Server.RunEnv Server.defaultSettings 65535 send readN (Engine.confTimeoutManager config) addr addr
    bracket (forkIO (Server.runServer env handler)) killThread (const action)

drainInput :: InpObj -> IO BS.ByteString
drainInput incoming = go []
  where
    go chunks = do
      (chunk, ended) <- inpObjBody incoming
      let received = chunk : chunks
      if ended then pure (BS.concat (reverse received)) else go received

writeInput :: TVar BS.ByteString -> SendFn
writeInput input ptr n = do
  bytes <- BS.packCStringLen (castPtr ptr, n)
  atomically $ modifyTVar' input (<> bytes)
  pure n

withinDeadline :: IO () -> IO ()
withinDeadline action = do
  result <- timeout 5000000 action
  result `shouldBe` Just ()

expectReset :: Either ClientStreamError () -> IO ()
expectReset (Left (ClientStreamReset Cancel)) = pure ()
expectReset other = expectationFailure ("Expected CANCEL reset, got " ++ show other)

request :: OutObj
request = OutObj [(":method", "GET"), (":path", "/")] OutBodyNone defaultTrailersMaker

reset :: StreamId -> Frame
reset sid = Frame (FrameHeader 4 FrameRSTStream 0 sid) (RSTStreamFrame Cancel)

headers :: StreamId -> BS.ByteString -> Frame
headers sid block = Frame
  (FrameHeader (fromIntegral (BS.length block)) FrameHeaders flagEndHeaders sid)
  (HeadersFrame Nothing block)

-- Drive the real engine over a scripted byte peer. Replies become visible
-- only after request HEADERS have been published; STM provides all ordering.
withPeer
  :: (StreamId -> [Frame])
  -> ((forall r. OutObj -> (InpObj -> IO r) -> IO r) -> IO a)
  -> IO a
withPeer = withObservedPeer (const (pure ()))

withObservedPeer
  :: (FrameHeader -> IO ())
  -> (StreamId -> [Frame])
  -> ((forall r. OutObj -> (InpObj -> IO r) -> IO r) -> IO a)
  -> IO a
withObservedPeer observe response action = do
  input <- newTVarIO BS.empty
  let send ptr len = do
        bytes <- BS.packCStringLen (castPtr ptr, len)
        case decodeFrameHeader bytes of
          Right h -> do
            observe h
            if fhType h == FrameHeaders
              then atomically $ modifyTVar' input (<> BS.concat (map encodeFrame (response (fhStreamId h))))
              else pure ()
          _ -> pure ()
        pure len
      readN = readInput input
      env = RunEnv "localhost" send readN 65535 65535 (Just 100) "http"
  runClient env $ \sendRequest _ -> action sendRequest

readInput :: TVar BS.ByteString -> Int -> IO BS.ByteString
readInput input n = atomically $ do
  bytes <- readTVar input
  check (BS.length bytes >= n)
  let (prefix, rest) = BS.splitAt n bytes
  writeTVar input rest
  pure prefix
