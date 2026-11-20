{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.WAI (tests) where

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive (mk)
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Message qualified as U
import Network.HTTP.WAI
import Network.Wai qualified as Wai
import Network.Wai.Internal (ResponseReceived (ResponseReceived))
import Test.Syd
import "http-types" Network.HTTP.Types qualified as WAIHttp
import "wireform-http" Network.HTTP.Types.Body qualified as U
import "wireform-http" Network.HTTP.Types.Header qualified as U
import "wireform-http" Network.HTTP.Types.Method qualified as U
import "wireform-http" Network.HTTP.Types.Status qualified as U
import "wireform-http" Network.HTTP.Types.Version qualified as U


tests :: Spec
tests =
  describe "WAI adapter" $
    sequence_
      [ describe "Primitive conversions" $
          sequence_
            [ testStatusRoundtrip
            , testVersionRoundtrip
            , testMethodRoundtrip
            ]
      , describe "waiToHandler" $
          sequence_
            [ testWaiToHandlerBasic
            , testWaiToHandlerBody
            , testWaiToHandlerHeaders
            , testWaiToHandlerQueryString
            , testWaiToHandlerStreamingResponse
            , testWaiToHandlerStreamingRequest
            , testWaiToHandlerAppThrows
            ]
      , describe "handlerToWai" $
          sequence_
            [ testHandlerToWaiBasic
            , testHandlerToWaiStream
            ]
      , describe "Request conversions" $
          sequence_
            [ testFromWaiRequestPreservesFields
            , testToWaiRequestPreservesFields
            , testToWaiRequestHostFromAuthority
            , testToWaiRequestEmptyTarget
            ]
      , testWaiTransportEmptyQuery
      , describe "Error handling" $
          sequence_
            [ testWaiAppDidNotRespond
            ]
      ]


testWaiTransportEmptyQuery :: Spec
testWaiTransportEmptyQuery = it "passes a bare query delimiter to the WAI application" $ do
  let app req respond =
        respond (Wai.responseLBS WAIHttp.status200 [] (LBS.fromStrict (Wai.rawQueryString req)))
      clientReq = Client.request U.mGet (Client.staticURI "http://example.com/record?#") Client.emptyStream
  response <- Client.sendRaw (waiTransport app) clientReq
  Client.popperBytes (Client.bodyPopper response) `shouldReturn` "?"


------------------------------------------------------------------------
-- Primitive conversion tests
------------------------------------------------------------------------

testStatusRoundtrip :: Spec
testStatusRoundtrip = it "status roundtrip" $ do
  let statuses = [U.status200, U.status404, U.status500, U.Status 418]
  mapM_
    ( \s -> do
        let waiS = WAIHttp.mkStatus (fromIntegral (U.statusCode s)) ""
            back = U.Status (fromIntegral (WAIHttp.statusCode waiS))
        U.statusCode back `shouldBe` U.statusCode s
    )
    statuses


testVersionRoundtrip :: Spec
testVersionRoundtrip = it "version roundtrip" $ do
  let versions =
        [ (U.HTTP1_0, WAIHttp.HttpVersion 1 0)
        , (U.HTTP1_1, WAIHttp.HttpVersion 1 1)
        , (U.HTTP2, WAIHttp.HttpVersion 2 0)
        ]
  mapM_
    ( \(wf, wai) -> do
        let wai' =
              WAIHttp.HttpVersion
                (fromIntegral (U.versionMajor wf))
                (fromIntegral (U.versionMinor wf))
        wai' `shouldBe` wai
        let wf' =
              U.mkVersion
                (fromIntegral (WAIHttp.httpMajor wai))
                (fromIntegral (WAIHttp.httpMinor wai))
        wf' `shouldBe` wf
    )
    versions


testMethodRoundtrip :: Spec
testMethodRoundtrip = it "method roundtrip" $ do
  let methods = [U.mGet, U.mPost, U.mPut, U.mDelete, U.mPatch]
  mapM_
    ( \m -> do
        let wai = U.fromMethod m
            back = U.methodFromBytes wai
        back `shouldBe` m
    )
    methods


------------------------------------------------------------------------
-- waiToHandler tests
------------------------------------------------------------------------

echoApp :: Wai.Application
echoApp req respond = do
  body <- Wai.consumeRequestBodyStrict req
  respond $
    Wai.responseLBS
      WAIHttp.status200
      [ ("X-Method", Wai.requestMethod req)
      , ("X-Path", Wai.rawPathInfo req)
      , ("X-Query", Wai.rawQueryString req)
      , ("Content-Type", "application/octet-stream")
      ]
      body


testWaiToHandlerBasic :: Spec
testWaiToHandlerBasic = it "GET / returns 200" $ do
  let handler = waiToHandler echoApp
  resp <-
    handler
      U.Request
        { U.requestMethod = U.mGet
        , U.requestTarget = "/"
        , U.requestAuthority = Nothing
        , U.requestScheme = U.SchemeHttp
        , U.requestHeaders = []
        , U.requestBody = U.BodyEmpty
        , U.requestVersion = U.HTTP1_1
        , U.requestTrailers = pure []
        }
  U.responseStatus resp `shouldBe` U.status200
  assertHeaderEq (U.responseHeaders resp) "X-Method" "GET"
  assertHeaderEq (U.responseHeaders resp) "X-Path" "/"


testWaiToHandlerBody :: Spec
testWaiToHandlerBody = it "POST with body echoes it back" $ do
  let handler = waiToHandler echoApp
      payload = "hello wireform"
  resp <-
    handler
      U.Request
        { U.requestMethod = U.mPost
        , U.requestTarget = "/echo"
        , U.requestAuthority = Nothing
        , U.requestScheme = U.SchemeHttp
        , U.requestHeaders = [(U.hContentType, "text/plain")]
        , U.requestBody = U.BodyBytes payload
        , U.requestVersion = U.HTTP1_1
        , U.requestTrailers = pure []
        }
  U.responseStatus resp `shouldBe` U.status200
  case U.responseBody resp of
    U.BodyBytes bs -> bs `shouldBe` payload
    other -> (if (False) then pure () else expectationFailure ("expected BodyBytes, got " <> show other))


testWaiToHandlerHeaders :: Spec
testWaiToHandlerHeaders = it "request headers forwarded to WAI app" $ do
  let app req respond = do
        let ua = maybe "" id $ lookup "User-Agent" (Wai.requestHeaders req)
        respond $
          Wai.responseLBS
            WAIHttp.status200
            [("X-UA", ua)]
            ""
      handler = waiToHandler app
  resp <-
    handler
      U.Request
        { U.requestMethod = U.mGet
        , U.requestTarget = "/"
        , U.requestAuthority = Nothing
        , U.requestScheme = U.SchemeHttp
        , U.requestHeaders = [(U.hUserAgent, "wireform-test/1.0")]
        , U.requestBody = U.BodyEmpty
        , U.requestVersion = U.HTTP1_1
        , U.requestTrailers = pure []
        }
  assertHeaderEq (U.responseHeaders resp) "X-UA" "wireform-test/1.0"


testWaiToHandlerQueryString :: Spec
testWaiToHandlerQueryString = it "query string preserved" $ do
  let handler = waiToHandler echoApp
  resp <-
    handler
      U.Request
        { U.requestMethod = U.mGet
        , U.requestTarget = "/search?q=haskell&page=1"
        , U.requestAuthority = Nothing
        , U.requestScheme = U.SchemeHttp
        , U.requestHeaders = []
        , U.requestBody = U.BodyEmpty
        , U.requestVersion = U.HTTP1_1
        , U.requestTrailers = pure []
        }
  assertHeaderEq (U.responseHeaders resp) "X-Path" "/search"
  assertHeaderEq (U.responseHeaders resp) "X-Query" "?q=haskell&page=1"


testWaiToHandlerStreamingResponse :: Spec
testWaiToHandlerStreamingResponse = it "streaming WAI response materialised" $ do
  let app _req respond =
        respond
          $ Wai.responseStream
            WAIHttp.status200
            [("Content-Type", "text/plain")]
          $ \write flush -> do
            write (byteString "chunk1")
            write (byteString "chunk2")
            flush
      handler = waiToHandler app
  resp <-
    handler
      U.Request
        { U.requestMethod = U.mGet
        , U.requestTarget = "/"
        , U.requestAuthority = Nothing
        , U.requestScheme = U.SchemeHttp
        , U.requestHeaders = []
        , U.requestBody = U.BodyEmpty
        , U.requestVersion = U.HTTP1_1
        , U.requestTrailers = pure []
        }
  U.responseStatus resp `shouldBe` U.status200
  case U.responseBody resp of
    U.BodyBytes bs -> bs `shouldBe` "chunk1chunk2"
    other -> (if (False) then pure () else expectationFailure ("expected BodyBytes, got " <> show other))


testWaiToHandlerStreamingRequest :: Spec
testWaiToHandlerStreamingRequest = it "streaming request body consumed by WAI app" $ do
  chunksRef <- newIORef ["part1" :: ByteString, "part2", "part3"]
  let bodyProducer = do
        chunks <- readIORef chunksRef
        case chunks of
          [] -> pure Nothing
          (c : cs) -> writeIORef chunksRef cs >> pure (Just c)
      handler = waiToHandler echoApp
  resp <-
    handler
      U.Request
        { U.requestMethod = U.mPost
        , U.requestTarget = "/"
        , U.requestAuthority = Nothing
        , U.requestScheme = U.SchemeHttp
        , U.requestHeaders = [(U.hContentType, "text/plain")]
        , U.requestBody = U.BodyStream bodyProducer
        , U.requestVersion = U.HTTP1_1
        , U.requestTrailers = pure []
        }
  case U.responseBody resp of
    U.BodyBytes bs -> bs `shouldBe` "part1part2part3"
    other -> (if (False) then pure () else expectationFailure ("expected BodyBytes, got " <> show other))


testWaiToHandlerAppThrows :: Spec
testWaiToHandlerAppThrows = it "WAI app exception propagates" $ do
  let app :: Wai.Application
      app _req _respond = error "deliberate test failure"
      handler = waiToHandler app
  result <-
    try @SomeException $
      handler
        U.Request
          { U.requestMethod = U.mGet
          , U.requestTarget = "/"
          , U.requestAuthority = Nothing
          , U.requestScheme = U.SchemeHttp
          , U.requestHeaders = []
          , U.requestBody = U.BodyEmpty
          , U.requestVersion = U.HTTP1_1
          , U.requestTrailers = pure []
          }
  case result of
    Left _ -> pure ()
    Right _ -> (False) `shouldBe` True


------------------------------------------------------------------------
-- handlerToWai tests
------------------------------------------------------------------------

testHandlerToWaiBasic :: Spec
testHandlerToWaiBasic = it "200 response round-trips" $ do
  let handler _req =
        pure
          U.Response
            { U.responseStatus = U.status200
            , U.responseVersion = U.HTTP1_1
            , U.responseHeaders = [(mk "X-Custom", "value")]
            , U.responseBody = U.BodyBytes "ok"
            , U.responseTrailers = pure []
            , U.responseH2StreamId = 0
            , U.responseCancel = pure ()
            , U.responsePushPromises = pure []
            }
      app = handlerToWai handler
  resultRef <- newIORef Nothing
  _ <- app Wai.defaultRequest $ \resp -> do
    writeIORef resultRef (Just resp)
    pure ResponseReceived
  mResp <- readIORef resultRef
  case mResp of
    Nothing -> (False) `shouldBe` True
    Just resp -> do
      WAIHttp.statusCode (Wai.responseStatus resp) `shouldBe` 200
      (lookup "X-Custom" (Wai.responseHeaders resp) == Just "value") `shouldBe` True


testHandlerToWaiStream :: Spec
testHandlerToWaiStream = it "streaming body round-trips" $ do
  chunksRef <- newIORef ["chunk1" :: ByteString, "chunk2", "chunk3"]
  let handler _req =
        pure
          U.Response
            { U.responseStatus = U.status200
            , U.responseVersion = U.HTTP1_1
            , U.responseHeaders = []
            , U.responseBody = U.BodyStream $ do
                chunks <- readIORef chunksRef
                case chunks of
                  [] -> pure Nothing
                  (c : cs) -> do
                    writeIORef chunksRef cs
                    pure (Just c)
            , U.responseTrailers = pure []
            , U.responseH2StreamId = 0
            , U.responseCancel = pure ()
            , U.responsePushPromises = pure []
            }
      app = handlerToWai handler
  resultRef <- newIORef Nothing
  _ <- app Wai.defaultRequest $ \resp -> do
    let (status, _hdrs, withBody) = Wai.responseToStream resp
    body <- withBody $ \streamBody -> do
      collectedRef <- newIORef []
      streamBody
        ( \builder -> do
            let bs = LBS.toStrict (toLazyByteString builder)
            collected <- readIORef collectedRef
            writeIORef collectedRef (collected <> [bs])
        )
        (pure ())
      collected <- readIORef collectedRef
      pure (BS.concat collected)
    writeIORef resultRef (Just (WAIHttp.statusCode status, body))
    pure ResponseReceived
  mResult <- readIORef resultRef
  case mResult of
    Nothing -> (False) `shouldBe` True
    Just (code, body) -> do
      code `shouldBe` 200
      body `shouldBe` "chunk1chunk2chunk3"


------------------------------------------------------------------------
-- Request conversion tests
------------------------------------------------------------------------

testFromWaiRequestPreservesFields :: Spec
testFromWaiRequestPreservesFields = it "fromWaiRequest preserves fields" $ do
  let waiReq =
        Wai.defaultRequest
          { Wai.requestMethod = "POST"
          , Wai.rawPathInfo = "/api/users"
          , Wai.rawQueryString = "?active=true"
          , Wai.httpVersion = WAIHttp.http11
          , Wai.requestHeaders = [("Content-Type", "application/json"), ("Host", "example.com")]
          , Wai.isSecure = True
          }
      req = fromWaiRequest waiReq
  U.requestMethod req `shouldBe` U.mPost
  U.requestTarget req `shouldBe` "/api/users?active=true"
  U.requestScheme req `shouldBe` U.SchemeHttps
  U.requestVersion req `shouldBe` U.HTTP1_1
  (U.lookupHeader U.hContentType (U.requestHeaders req) == Just "application/json") `shouldBe` True


testToWaiRequestPreservesFields :: Spec
testToWaiRequestPreservesFields = it "toWaiRequest preserves fields" $ do
  waiReq <-
    toWaiRequest
      U.Request
        { U.requestMethod = U.mPut
        , U.requestTarget = "/items/42?format=json"
        , U.requestAuthority = Just "api.example.com"
        , U.requestScheme = U.SchemeHttps
        , U.requestHeaders = [(U.hContentType, "application/json")]
        , U.requestBody = U.BodyBytes "{\"name\":\"test\"}"
        , U.requestVersion = U.HTTP1_1
        , U.requestTrailers = pure []
        }
  Wai.requestMethod waiReq `shouldBe` "PUT"
  Wai.rawPathInfo waiReq `shouldBe` "/items/42"
  Wai.rawQueryString waiReq `shouldBe` "?format=json"
  Wai.isSecure waiReq `shouldBe` True
  (lookup "Content-Type" (Wai.requestHeaders waiReq) == Just "application/json") `shouldBe` True
  (Wai.requestHeaderHost waiReq == Just "api.example.com") `shouldBe` True
  case Wai.requestBodyLength waiReq of
    Wai.KnownLength n -> n `shouldBe` fromIntegral (BS.length "{\"name\":\"test\"}")
    Wai.ChunkedBody -> (False) `shouldBe` True
  bodyChunk <- Wai.getRequestBodyChunk waiReq
  bodyChunk `shouldBe` "{\"name\":\"test\"}"
  eof <- Wai.getRequestBodyChunk waiReq
  eof `shouldBe` BS.empty


testToWaiRequestHostFromAuthority :: Spec
testToWaiRequestHostFromAuthority = it "Host synthesised from authority" $ do
  waiReq <-
    toWaiRequest
      U.Request
        { U.requestMethod = U.mGet
        , U.requestTarget = "/"
        , U.requestAuthority = Just "example.com"
        , U.requestScheme = U.SchemeHttp
        , U.requestHeaders = []
        , U.requestBody = U.BodyEmpty
        , U.requestVersion = U.HTTP1_1
        , U.requestTrailers = pure []
        }
  (lookup "Host" (Wai.requestHeaders waiReq) == Just "example.com") `shouldBe` True
  Wai.requestHeaderHost waiReq `shouldBe` Just "example.com"


testToWaiRequestEmptyTarget :: Spec
testToWaiRequestEmptyTarget = it "empty target → rawPathInfo empty" $ do
  waiReq <-
    toWaiRequest
      U.Request
        { U.requestMethod = U.mGet
        , U.requestTarget = ""
        , U.requestAuthority = Nothing
        , U.requestScheme = U.SchemeHttp
        , U.requestHeaders = []
        , U.requestBody = U.BodyEmpty
        , U.requestVersion = U.HTTP1_1
        , U.requestTrailers = pure []
        }
  Wai.rawPathInfo waiReq `shouldBe` ""
  Wai.rawQueryString waiReq `shouldBe` ""


------------------------------------------------------------------------
-- Error handling tests
------------------------------------------------------------------------

testWaiAppDidNotRespond :: Spec
testWaiAppDidNotRespond = it "WaiAppDidNotRespond on non-responding app" $ do
  let app :: Wai.Application
      app _req _respond = pure ResponseReceived
      handler = waiToHandler app
  result <-
    try @WaiAdapterError $
      handler
        U.Request
          { U.requestMethod = U.mGet
          , U.requestTarget = "/"
          , U.requestAuthority = Nothing
          , U.requestScheme = U.SchemeHttp
          , U.requestHeaders = []
          , U.requestBody = U.BodyEmpty
          , U.requestVersion = U.HTTP1_1
          , U.requestTrailers = pure []
          }
  case result of
    Left WaiAppDidNotRespond -> pure ()
    Left e -> (if (False) then pure () else expectationFailure ("wrong error: " <> show e))
    Right _ -> (False) `shouldBe` True


------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

assertHeaderEq :: U.Headers -> BS.ByteString -> BS.ByteString -> IO ()
assertHeaderEq hdrs name expected =
  case U.lookupHeader (mk name) hdrs of
    Just v -> v `shouldBe` expected
    Nothing -> (if (False) then pure () else expectationFailure ("missing header: " <> BS8.unpack name))
