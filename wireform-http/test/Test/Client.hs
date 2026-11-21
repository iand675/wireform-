{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Tests for the high-level wireform HTTP client (@Network.HTTP.Client.*@).

These exercise the pieces that are testable without spinning up a
live server: media-type matching, request encoding, the assertion
library against a mock transport, VCR record/replay, and the
middleware combinators.
-}
module Test.Client (tests) where

import Codec.Compression.Brotli qualified as Brotli
import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Zlib qualified as Zlib
import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Either (isLeft)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.Compression qualified as Compression
import Network.HTTP.Client.Negotiation qualified as Negotiation
import Network.HTTP.Client.Request (Request (headers))
import Network.HTTP.Client.Request qualified as WReq
import Network.HTTP.Client.Response (RawResponse (headers))
import Network.HTTP.ContentCoding qualified as Coding
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as M
import Network.HTTP.Types.Status qualified as S
import System.IO.Temp (withSystemTempDirectory)
import Test.Syd


-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

data User = User
  { userId :: !Int
  , userName :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)


tests :: Spec
tests =
  describe "Network.HTTP.Client" $
    sequence_
      [ mediaTypeTests
      , requestBuildingTests
      , sendTests
      , middlewareTests
      , vcrTests
      , mockAPITests
      , resourceTests
      , expectationTests
      , stateMachineTests
      , decoderTests
      , faultTests
      , tracingTests
      , compressionTests
      , poolTests
      , streamingVcrTests
      , decoderAltTests
      , pathMatcherTests
      ]


-- ---------------------------------------------------------------------------
-- Media type parsing + matching
-- ---------------------------------------------------------------------------

mediaTypeTests :: Spec
mediaTypeTests =
  describe "MediaType" $
    sequence_
      [ it "parses type/subtype" $ do
          case parseMediaType "application/json" of
            Right m -> do
              mtType m `shouldBe` "application"
              mtSubType m `shouldBe` "json"
            Left err -> expectationFailure err
      , it "parses parameters and lowercases the name" $ do
          case parseMediaType "Application/JSON; charset=utf-8" of
            Right m -> do
              mtType m `shouldBe` "application"
              mtSubType m `shouldBe` "json"
              lookup "charset" (mtParameters m) `shouldBe` Just "utf-8"
            Left err -> expectationFailure err
      , it "projects media names within the field's byte bounds" $ do
          let wire = " \tX-New/Vnd.Example+JSON\t "
              field = BS.take (BS.length wire) (BS.drop 4 ("bad/" <> wire <> "/bad"))
              expected = MediaType "x-new" "vnd.example+json" []
          parseMediaType field `shouldBe` Right expected
          contentTypeOf [(H.hContentType, field)] `shouldBe` expected
      , it "validates the entire field and retains its parse diagnostics" $ do
          map (isLeft . parseMediaType) ["text /plain", "text/plain, image/png", "text/pl*ain", "text/plain\0"]
            `shouldBe` replicate 4 True
          parseMediaType " \ttext/plain junk" `shouldBe` Left "trailing input after Content-Type: \" junk\""
          parseMediaType " \ttext/" `shouldBe` Left "malformed Content-Type: \" \\ttext/\""
      , it "wildcard matches" $ do
          matches "application/json" "*/*" `shouldBe` True
          matches "application/json" "application/*" `shouldBe` True
          matches "application/json" "text/*" `shouldBe` False
          matches "text/plain" "text/plain" `shouldBe` True
      , it "distinguishes concrete content types from media ranges" $ do
          map (isLeft . parseMediaType) ["*/*", "text/*", "*/json", "application/*+json"] `shouldBe` replicate 4 True
          parseMediaRange "TeXT/*; charset=utf-8" `shouldBe` Right (MediaType "text" "*" [("charset", "utf-8")])
          contentTypeOf [(H.hContentType, "*/*")] `shouldBe` MediaType "application" "octet-stream" []
      , it "uses the default for repeated Content-Type field lines" $
          map
            (\value -> contentTypeOf [(H.hContentType, "application/json"), (H.hAge, "0"), ("CONTENT-TYPE", value)])
            ["application/json", "text/plain"]
            `shouldBe` replicate 2 (MediaType "application" "octet-stream" [])
      , it "round-trips quoted parameter delimiters and opaque bytes" $ do
          let opaque = BS.pack [0x80, 0xff]
              parsed = MediaType "application" "example" [("profile", "a,b"), ("opaque", opaque), ("quoted", "\"\\")]
              canonical = "application/example; profile=\"a,b\"; opaque=\"" <> opaque <> "\"; quoted=\"\\\"\\\\\""
          parseMediaType canonical `shouldBe` Right parsed
          renderMediaType parsed `shouldBe` canonical
      , it "Accept header rendering omits q=1" $ do
          acceptHeaderValue
            [ ("application/json", maxQuality)
            , ("text/plain", Quality 0.5)
            ]
            `shouldBe` "application/json, text/plain; q=0.5"
      , it "Accept renders rounded and out-of-range qualities as valid qvalues" $ do
          let authored = [0.0004, 0.001, 0.825, 0.99949, 0.9995, 1 / 0, -1 / 0, 0 / 0]
              expected = [0, 0.001, 0.825, 0.999, 1, 1, 0, 0]
              field = acceptHeaderValue (map (\q -> ("text/plain", Quality q)) authored)
          fmap (map Negotiation.mediaWeight . Negotiation.accept) (Negotiation.parseAccept field) `shouldBe` Just expected
      , it "normalizes authored qualities for the other negotiation fields" $ do
          let excluded = Negotiation.mkQuality (0 / 0)
              preferred = Negotiation.mkQuality (1 / 0)
          Negotiation.unQuality excluded `shouldBe` 0
          Negotiation.unQuality preferred `shouldBe` 1
          Negotiation.acceptLanguageValue [("en-GB", excluded), ("en", preferred)] `shouldBe` "en-GB;q=0, en"
          Negotiation.acceptCharsetValue [("utf-8", preferred), ("*", excluded)] `shouldBe` "utf-8, *;q=0"
          Negotiation.acceptEncodingValue [(Just Coding.GZip, preferred), (Nothing, excluded)] `shouldBe` "gzip, *;q=0"
      ]


-- ---------------------------------------------------------------------------
-- Request building / bindVar
-- ---------------------------------------------------------------------------

requestBuildingTests :: Spec
requestBuildingTests =
  describe "Request building" $
    sequence_
      [ it "bindVar substitutes a path variable" $ do
          tpl <- case parseTemplate "/users/{userId}" of
            Right t -> pure t
            Left err -> expectationFailure (show err) >> error "unreachable"
          let req = bindVar' "userId" (42 :: Int) (request M.mGet (templateURI tpl) ())
          requestURIToText (requestURI req) `shouldBe` "/users/42"
      , it "withBody @JSON sets Content-Type" $ do
          let req :: Request BS.ByteString
              req = withBody @JSON (User 1 "alice") (post (compileTemplate "/users"))
              Request {headers = hs} = req
          H.lookupHeader H.hContentType hs `shouldBe` Just "application/json; charset=utf-8"
      , it "setHeader replaces previous header values" $ do
          let req :: Request ()
              req =
                setHeader H.hContentType "text/plain"
                  . setHeader H.hContentType "application/xml"
                  $ get (compileTemplate "/x")
              Request {headers = hs} = req
          H.lookupHeader H.hContentType hs `shouldBe` Just "text/plain"
      ]


compileTemplate :: String -> UriTemplate
compileTemplate s = case parseTemplate s of
  Right t -> t
  Left err -> error ("compileTemplate: " <> show err)


-- Helpers ----------------------------------------------------------------

bindVar' :: Text -> Int -> Request a -> Request a
bindVar' n v r = r {requestURI = bindVar n v (requestURI r)}


-- ---------------------------------------------------------------------------
-- send + mock transport
-- ---------------------------------------------------------------------------

sendTests :: Spec
sendTests =
  describe "send / mocks" $
    sequence_
      [ it "stubJSON decodes a response" $ do
          let transport = stubJSON S.status200 (User 7 "alice")
          Response {responseBody = u} <-
            sendIO transport (get (compileTemplate "/users/7")) (as @JSON @User)
          u `shouldBe` User 7 "alice"
      , it "request log captures method + uri" $ do
          let inner = stubJSON S.status200 (User 1 "bob")
          (t, log_) <- withRequestLog inner
          _ <- sendIO t (get (compileTemplate "/users/1")) (as @JSON @User)
          assertLog log_ (requestCount 1)
          assertLog log_ (anyRequest (hasMethod M.mGet <> hasURI "/users/1"))
          assertLog log_ (anyRequest (hasURIPath "/users/1"))
      , it "sends the correct Accept preference at the upper rounding boundary" $ do
          (transport, log_) <- withRequestLog (stubStatus S.status200)
          _ <-
            sendRawIO transport (get (compileTemplate "/x"))
              [("application/json", Quality 0.9995), ("text/plain", Quality 0.8)]
          assertLog log_ (anyRequest (hasHeaderEq H.hAccept "application/json; q=1, text/plain; q=0.8"))
      , it "decode failure throws DecodeFailure" $ do
          let transport = stub S.status200 "not json at all"
          result <-
            try (sendIO transport (get (compileTemplate "/x")) (as @JSON @User))
              :: IO (Either SomeException (Response User))
          case result of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected DecodeFailure"
      ]


-- ---------------------------------------------------------------------------
-- Middleware
-- ---------------------------------------------------------------------------

middlewareTests :: Spec
middlewareTests =
  describe "middleware" $
    sequence_
      [ it "withAuth adds Authorization header" $ do
          -- Place the log innermost so it observes the request that the
          -- base transport sees (i.e. after the auth middleware ran).
          (logged, log_) <- withRequestLog (stubStatus S.status200)
          let transport = withAuth (Bearer "tok123") logged
          _ <-
            try (sendIO transport (get (compileTemplate "/x")) (as @PlainText @Text))
              :: IO (Either SomeException (Response Text))
          assertLog log_ (anyRequest (hasHeaderEq H.hAuthorization "Bearer tok123"))
      , it "failFirstN retries to success" $ do
          let canned = errorResp
          retried <- failFirstN 2 canned (stubJSON S.status200 (User 9 "ok"))
          let withRet = withRetry defaultRetryPolicy retried
          Response {responseBody = u} <-
            sendIO withRet (get (compileTemplate "/x")) (as @JSON @User)
          u `shouldBe` User 9 "ok"
      ]


errorResp :: RawResponse
errorResp =
  RawResponse
    { statusCode = S.status503
    , headers = []
    , bodyPopper = pure BS.empty
    , protocolInfo = HTTP1_1
    }


-- ---------------------------------------------------------------------------
-- VCR
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- MockAPI declarative routing
-- ---------------------------------------------------------------------------

mockAPITests :: Spec
mockAPITests =
  describe "MockAPI" $
    sequence_
      [ it "first matching route wins" $ do
          let api =
                mockAPI
                  MockAPI
                    { routes =
                        [ on
                            (hasMethod M.mGet <> hasURIPath "/health")
                            (\_ _ -> ok200 "ok")
                        , on
                            (hasMethod M.mGet <> hasURIPathPrefix "/users/")
                            (\_ _ -> json200 (User 1 "alice"))
                        ]
                    , fallback = throwUnexpected
                    }
          Response {responseBody = u} <-
            sendIO api (get (compileTemplate "/users/1")) (as @JSON @User)
          u `shouldBe` User 1 "alice"
      , it "fallback runs when nothing matches" $ do
          let api =
                mockAPI
                  MockAPI
                    { routes =
                        [ on
                            (hasMethod M.mGet <> hasURIPath "/health")
                            (\_ _ -> ok200 "ok")
                        ]
                    , fallback = \_ _ -> rawResponse S.status418 [] "i'm a teapot"
                    }
          raw <- sendRaw api =<< prep (get (compileTemplate "/unknown"))
          statusCode raw `shouldBe` S.status418
      , it "hasQueryParam matches" $ do
          let api =
                mockAPI
                  MockAPI
                    { routes =
                        [ on
                            ( hasMethod M.mGet
                                <> hasURIPath "/search"
                                <> hasQueryParam "q" "hello"
                            )
                            (\_ _ -> ok200 "found")
                        ]
                    , fallback = \_ _ -> rawResponse S.status404 [] ""
                    }
          raw <- sendRaw api =<< prep (get (compileTemplate "/search?q=hello"))
          statusCode raw `shouldBe` S.status200
      , it "hasJSONBody matches on shape" $ do
          let target = User 1 "alice"
              api =
                mockAPI
                  MockAPI
                    { routes =
                        [ on
                            (hasMethod M.mPost <> hasJSONBody target)
                            (\_ _ -> ok200 "match")
                        ]
                    , fallback = \_ _ -> rawResponse S.status404 [] ""
                    }
          raw <-
            sendRaw api
              =<< prep
                (withBody @JSON target (post (compileTemplate "/users")))
          statusCode raw `shouldBe` S.status200
      ]


-- ---------------------------------------------------------------------------
-- Resource mocks
-- ---------------------------------------------------------------------------

resourceTests :: Spec
resourceTests =
  describe "resource" $
    sequence_
      [ it "POST then GET, then DELETE then GET" $ do
          idVar <- newIORef (0 :: Int)
          let nextId = atomicModifyIORef' idVar (\n -> (n + 1, T.pack (show (n + 1))))
          userRoutes <-
            resource
              ResourceConfig
                { basePath = "/users"
                , idField = \(User i _) -> T.pack (show i)
                , generateId = nextId
                }
          let api = mockAPI MockAPI {routes = userRoutes, fallback = throwUnexpected}

          created <-
            sendIO
              api
              (withBody @JSON (User 0 "alice") (post (compileTemplate "/users")))
              (as @JSON @User)
          responseStatus created `shouldBe` S.status201

          Response {responseBody = u} <-
            sendIO
              api
              (get (compileTemplate "/users/0"))
              (as @JSON @User)
          userName u `shouldBe` "alice"

          delRaw <- sendRaw api =<< prep (delete (compileTemplate "/users/0"))
          statusCode delRaw `shouldBe` S.status204

          notFound <- sendRaw api =<< prep (get (compileTemplate "/users/0"))
          statusCode notFound `shouldBe` S.status404
      ]


-- ---------------------------------------------------------------------------
-- Expectations
-- ---------------------------------------------------------------------------

expectationTests :: Spec
expectationTests =
  describe "withExpectations" $
    sequence_
      [ it "expected counts pass" $ do
          withExpectations
            [ expect_
                (hasMethod M.mGet <> hasURIPath "/users")
                (Exactly 1)
                (json200 (User 1 "alice"))
            ]
            $ \t -> do
              _ <- sendIO t (get (compileTemplate "/users")) (as @JSON @User)
              pure ()
      , it "violated count throws ExpectationNotMet" $ do
          result <- try
            $ withExpectations
              [ expect_
                  (hasMethod M.mGet <> hasURIPath "/users")
                  (Exactly 2)
                  (json200 (User 1 "alice"))
              ]
            $ \t -> do
              _ <- sendIO t (get (compileTemplate "/users")) (as @JSON @User)
              pure ()
          case (result :: Either ExpectationNotMet ()) of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected ExpectationNotMet"
      , it "unexpected request throws UnexpectedRequest" $ do
          result <- try
            $ withExpectations
              [ expect_
                  (hasMethod M.mGet <> hasURIPath "/users")
                  AnyTimes
                  (json200 (User 1 "alice"))
              ]
            $ \t -> do
              _ <- sendIO t (get (compileTemplate "/admin")) (as @JSON @User)
              pure ()
          case (result :: Either SomeException ()) of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected an exception"
      ]


-- ---------------------------------------------------------------------------
-- State machines
-- ---------------------------------------------------------------------------

stateMachineTests :: Spec
stateMachineTests =
  describe "stateMachine" $
    sequence_
      [ it "polling: pending -> pending -> complete" $ do
          transport <-
            stateMachine
              StateMachine
                { initialState = 0 :: Int
                , transition = \n _ _ ->
                    let body_ =
                          if n < 2
                            then "{\"status\":\"pending\"}"
                            else "{\"status\":\"complete\"}"
                    in (,) (n + 1)
                         <$> rawResponse
                           S.status200
                           [(H.hContentType, "application/json")]
                           body_
                }
          let req = get (compileTemplate "/jobs/1")
          r1 <- sendRaw transport =<< prep req
          r1Body <- rawResponseBytes r1
          BS.isInfixOf "pending" r1Body `shouldBe` True
          r2 <- sendRaw transport =<< prep req
          r2Body <- rawResponseBytes r2
          BS.isInfixOf "pending" r2Body `shouldBe` True
          r3 <- sendRaw transport =<< prep req
          r3Body <- rawResponseBytes r3
          BS.isInfixOf "complete" r3Body `shouldBe` True
      ]


-- ---------------------------------------------------------------------------
-- Decoders
-- ---------------------------------------------------------------------------

decoderTests :: Spec
decoderTests =
  describe "Decoder" $
    sequence_
      [ it "asEither @JSON @JSON returns Right on 2xx" $ do
          let transport = stubJSON S.status200 (User 1 "alice")
          Response {responseBody = r} <-
            sendIO
              transport
              (get (compileTemplate "/users/1"))
              (asEither @JSON @ErrorPayload @JSON @User)
          r `shouldBe` Right (User 1 "alice")
      , it "asEither returns Left on non-2xx" $ do
          let transport = stubJSON S.status404 (ErrorPayload "not found")
          Response {responseBody = r} <-
            sendIO
              transport
              (get (compileTemplate "/users/999"))
              (asEither @JSON @ErrorPayload @JSON @User)
          r `shouldBe` Left (ErrorPayload "not found")
      ]


data ErrorPayload = ErrorPayload {errMsg :: !Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)


-- ---------------------------------------------------------------------------
-- Fault injection
-- ---------------------------------------------------------------------------

faultTests :: Spec
faultTests =
  describe "Faults" $
    sequence_
      [ it "withTruncation cuts the body" $ do
          let inner = stub S.status200 "abcdefghij"
              transport = withTruncation 4 inner
          raw <- sendRaw transport =<< prep (get (compileTemplate "/x"))
          bs <- rawResponseBytes raw
          bs `shouldBe` "abcd"
      ]


-- ---------------------------------------------------------------------------
-- Tracing (smoke test: middleware composes without error)
-- ---------------------------------------------------------------------------

tracingTests :: Spec
tracingTests =
  describe "withTracing" $
    sequence_
      [ it "no-op tracing middleware passes the response through" $ do
          let inner = stubJSON S.status200 (User 1 "alice")
              transport = withTracing defaultTracingConfig inner
          Response {responseBody = u} <-
            sendIO transport (get (compileTemplate "/users/1")) (as @JSON @User)
          u `shouldBe` User 1 "alice"
      , it "TracingDisabled short-circuits to the inner transport" $ do
          let inner = stubJSON S.status200 (User 2 "bob")
              transport = withTracing TracingDisabled inner
          Response {responseBody = u} <-
            sendIO transport (get (compileTemplate "/users/2")) (as @JSON @User)
          u `shouldBe` User 2 "bob"
      , it "span lifetime extends until the popper hits EOF" $ do
          -- Smoke test: the popper should still produce its chunk
          -- /after/ sendRaw returns, since the span (and the popper
          -- wrapping it) live past that boundary.
          let inner = stub S.status200 "streamed body"
              transport = withTracing defaultTracingConfig inner
          raw <- sendRaw transport =<< prep (get (compileTemplate "/x"))
          first <- bodyPopper raw
          first `shouldBe` "streamed body"
          eof <- bodyPopper raw
          eof `shouldBe` BS.empty
      ]


-- Build a Request BodyStream from any Body-bearing Request for use
-- with sendRaw.
prep :: Body body => Request body -> IO (Request BodyStream)
prep r = prepareRequest [] r


-- ---------------------------------------------------------------------------
-- Streaming VCR + Alt + path matcher
-- ---------------------------------------------------------------------------

streamingVcrTests :: Spec
streamingVcrTests =
  describe "Streaming VCR" $
    sequence_
      [ it "withChunkedRecording preserves chunk boundaries on replay" $ do
          let chunks = ["alpha", "beta", "gamma"] :: [BS.ByteString]
          innerRaw <- do
            p <- popperFromList chunks
            pure $ mockTransport $ \_ ->
              pure
                RawResponse
                  { statusCode = S.status200
                  , Network.HTTP.Client.Response.headers = []
                  , bodyPopper = p
                  , protocolInfo = HTTP1_1
                  }
          withSystemTempDirectory "wire-vcr-stream" $ \dir -> do
            let path = dir <> "/stream.yaml"
            _ <- recordSessionChunked innerRaw path $ \t -> do
              raw <- sendRaw t =<< prep (get (compileTemplate "/x"))
              -- materialise to ensure recording runs
              _ <- rawResponseBytes raw
              pure ()
            cassette <- loadCassette path
            replay <- replayTransport cassette byMethodAndURI
            raw <- sendRaw replay =<< prep (get (compileTemplate "/x"))
            replayed <- pullChunks (bodyPopper raw)
            replayed `shouldBe` chunks
      ]


pullChunks :: IO BS.ByteString -> IO [BS.ByteString]
pullChunks p = go []
  where
    go acc = do
      c <- p
      if BS.null c then pure (reverse acc) else go (c : acc)


decoderAltTests :: Spec
decoderAltTests =
  describe "ResponseDecoder Alt" $
    sequence_
      [ it "<!> defers to the second decoder when the first can't decode" $ do
          let transport =
                stubBytes
                  S.status200
                  [(H.hContentType, "text/plain; charset=utf-8")]
                  "plain bytes"
              decoder :: ResponseDecoder Text
              decoder = as @JSON @Text <!> as @PlainText @Text
          Response {responseBody = t} <-
            sendIO transport (get (compileTemplate "/x")) decoder
          t `shouldBe` "plain bytes"
      , it "routes a prepared bare decoder by both names and ignores supplied parameters" $ do
          let offers = [("application/json", minQuality)]
              primary = ResponseDecoder offers (\_ _ _ -> Right ("primary" :: Text))
              fallback = ResponseDecoder [("*/*", maxQuality)] (\_ _ _ -> Right "fallback")
              combined = primary <!> fallback
              choose actual = either (const "decode error") id (decodeBody combined S.status200 actual "")
          acceptable combined `shouldBe` (offers <> acceptable fallback)
          forM_
            [ ("Application/JSON; charset=utf-8; profile=\"a,b\"", "primary")
            , ("application/xml", "fallback")
            , ("text/json", "fallback")
            ]
            (\(actual, expected) -> choose actual `shouldBe` expected)
      , it "dispatches between profiles of the same Content-Type" $ do
          let profiled range result = ResponseDecoder [(range, maxQuality)] (\_ _ _ -> Right (result :: Text))
              v1 = profiled "application/example; profile=\"urn:Example,v1\"" "v1"
              v2 = profiled "application/example; profile=\"urn:Example,v2\"; charset=utf-8" "v2"
              fallback = profiled "*/*" "fallback"
              req = get (compileTemplate "/x")
              cases =
                [ ("application/example; profile=\"urn:Example,v1\"", "v1")
                , ("Application/Example; PROFILE=\"urn:Example,v2\"; CHARSET=\"UTF-8\"; version=2", "v2")
                , ("application/example; profile=\"urn:example,v1\"", "fallback")
                , ("application/example; profile=\"urn:Example,v2\"; charset=latin1", "fallback")
                , ("application/example", "fallback")
                ]
          forM_ cases $ \(field, expected) -> do
            let transport = stubBytes S.status200 [(H.hContentType, field)] "body"
            Response {responseBody = result} <- sendIO transport req ((v1 <!> v2) <!> fallback)
            result `shouldBe` expected
      , it "keeps charset-less JSON and plain text eligible for their UTF-8 decoders" $ do
          let fallback = ResponseDecoder [("*/*", maxQuality)] (\_ _ _ -> Right ("fallback" :: Text))
              req = get (compileTemplate "/x")
              examples =
                [ ("application/json", as @JSON @Text, "\"json\"", "json")
                , ("text/plain", as @PlainText @Text, "plain", "plain")
                , ("text/plain; charset=latin1", as @PlainText @Text, "plain", "fallback")
                ]
          forM_ examples $ \(field, decoder, body, expected) -> do
            let transport = stubBytes S.status200 [(H.hContentType, field)] body
            Response {responseBody = result} <- sendIO transport req (decoder <!> fallback)
            result `shouldBe` expected
      , it "compares parameter slices through each short comparison width" $ do
          let slice prefix suffix value = BS.take (BS.length value) (BS.drop (BS.length prefix) (prefix <> value <> suffix))
              media value = MediaType "application" "example" [("profile", value)]
              choose authored received =
                let primary = ResponseDecoder [(media authored, maxQuality)] (\_ _ _ -> Right True)
                    fallback = ResponseDecoder [] (\_ _ _ -> Right False)
                in either (const False) id (decodeBody (primary <!> fallback) S.status200 (media received) "")
          forM_ [0 .. 32] $ \width -> do
            let value = BS.take width (BS.pack [128 .. 159])
                required = slice "p" "q" value
                supplied = slice "rs" "tu" value
            choose required supplied `shouldBe` True
            forM_ [0 .. width - 1] $ \offset -> do
              let changed = BS.take offset supplied <> "X" <> BS.drop (offset + 1) supplied
              choose required changed `shouldBe` False
      ]


pathMatcherTests :: Spec
pathMatcherTests =
  describe "hasPathMatches" $
    sequence_
      [ it "captures simple :id segment" $ do
          let api =
                mockAPI
                  MockAPI
                    { routes =
                        [ on
                            (hasMethod M.mGet <> hasPathMatches "/users/:id")
                            (\_ _ -> ok200 "match")
                        ]
                    , fallback = \_ _ -> rawResponse S.status404 [] ""
                    }
          raw <- sendRaw api =<< prep (get (compileTemplate "/users/42"))
          statusCode raw `shouldBe` S.status200
      , it "rejects extra segments" $ do
          let api =
                mockAPI
                  MockAPI
                    { routes =
                        [ on
                            (hasMethod M.mGet <> hasPathMatches "/users/:id")
                            (\_ _ -> ok200 "match")
                        ]
                    , fallback = \_ _ -> rawResponse S.status404 [] ""
                    }
          raw <- sendRaw api =<< prep (get (compileTemplate "/users/42/posts"))
          statusCode raw `shouldBe` S.status404
      ]


-- ---------------------------------------------------------------------------
-- Compression
-- ---------------------------------------------------------------------------

poolTests :: Spec
poolTests =
  describe "ConnectionPool" $
    sequence_
      [ it "newPool / closePool round-trip is clean" $ do
          pool <- newPool defaultPoolConfig
          closePool pool
      , it "withPool runs the action and tears down" $ do
          ran <- newIORef False
          withPool defaultPoolConfig $ \_ -> writeIORef ran True
          readIORef ran >>= (`shouldBe` True)
      , it "ClientConfig.ccPoolConfig defaults to Just" $
          case ccPoolConfig defaultClientConfig of
            Just _ -> pure ()
            Nothing -> expectationFailure "expected a default PoolConfig"
      ]


compressionTests :: Spec
compressionTests =
  describe "withDecompression" $
    sequence_
      [ it "decodes gzip-encoded response" $ do
          let payload = "the quick brown fox jumps over the lazy dog" :: BS.ByteString
              compressed = BSL.toStrict $ GZip.compress (BSL.fromStrict payload)
              inner = mockTransport $ \_ ->
                rawResponse
                  S.status200
                  [(H.hContentType, "text/plain"), ("Content-Encoding", "gzip")]
                  compressed
              transport = withDecompression inner
          Response {responseBody = t} <-
            sendIO transport (get (compileTemplate "/x")) (as @PlainText @Text)
          TE.encodeUtf8 t `shouldBe` payload
      , it "decodes deflate (zlib) response" $ do
          let payload = "deflate payload bytes here" :: BS.ByteString
              compressed = BSL.toStrict $ Zlib.compress (BSL.fromStrict payload)
              inner = mockTransport $ \_ ->
                rawResponse
                  S.status200
                  [(H.hContentType, "text/plain"), ("Content-Encoding", "deflate")]
                  compressed
              transport = withDecompression inner
          Response {responseBody = t} <-
            sendIO transport (get (compileTemplate "/x")) (as @PlainText @Text)
          TE.encodeUtf8 t `shouldBe` payload
      , it "decodes brotli response" $ do
          let payload =
                "brotli payload that is reasonably long for the encoder"
                  :: BS.ByteString
              compressed = BSL.toStrict $ Brotli.compress (BSL.fromStrict payload)
              inner = mockTransport $ \_ ->
                rawResponse
                  S.status200
                  [(H.hContentType, "text/plain"), ("Content-Encoding", "br")]
                  compressed
              transport = withDecompression inner
          Response {responseBody = t} <-
            sendIO transport (get (compileTemplate "/x")) (as @PlainText @Text)
          TE.encodeUtf8 t `shouldBe` payload
      , it "adds Accept-Encoding when absent" $ do
          (logged, log_) <- withRequestLog (stubStatus S.status200)
          let transport = withDecompression logged
          _ <- sendIO transport (get (compileTemplate "/x")) (as @PlainText @Text)
          assertLog log_ (anyRequest (hasHeaderEq H.hAcceptEncoding "br, gzip, deflate"))
      , it "asks for identity on ranged requests and preserves explicit preferences" $ do
          let ranged = setHeader H.hRange "bytes=5-" (get (compileTemplate "/x"))
          forM_ [(ranged, "identity"), (setHeader H.hAcceptEncoding "gzip" ranged, "gzip"), (setHeader H.hAcceptEncoding "" ranged, "")] $ \(req, expected) -> do
            (logged, log_) <- withRequestLog (stubStatus S.status200)
            _ <- sendRaw (withDecompression logged) =<< prep req
            assertLog log_ (anyRequest (hasHeaderEq H.hAcceptEncoding expected))
      , it "preserves encoded partial bytes and the fields that describe them" $ do
          let coded = BSL.toStrict (GZip.compress "range payload more than five bytes")
              part = BS.drop 5 coded
              lastByte = BS8.pack (show (BS.length coded - 1))
              total = BS8.pack (show (BS.length coded))
              fields = [(H.hContentEncoding, "gzip"), (H.hContentLength, BS8.pack (show (BS.length part))), (H.hContentRange, "bytes 5-" <> lastByte <> "/" <> total)] <> bodyDigests
              inner = mockTransport $ \_ -> rawResponse S.status206 fields part
              req = setHeader H.hAcceptEncoding "gzip" (setHeader H.hRange "bytes=5-" (get (compileTemplate "/x")))
          raw <- sendRaw (withDecompression inner) =<< prep req
          let RawResponse{headers = respFields} = raw
          respFields `shouldBe` fields
          rawResponseBytes raw `shouldReturn` part
      , it "decodes a full replacement response to a range request" $ do
          let coded = BSL.toStrict (GZip.compress "new representation")
              inner = mockTransport $ \_ -> rawResponse S.status200 [(H.hContentEncoding, "gzip")] coded
              req = setHeader H.hAcceptEncoding "gzip" (setHeader H.hRange "bytes=5-" (get (compileTemplate "/x")))
          raw <- sendRaw (withDecompression inner) =<< prep req
          rawResponseBytes raw `shouldReturn` "new representation"
      , it "preserves coding and length metadata on HEAD and 304" $ do
          let fields = [(H.hContentEncoding, "gzip"), (H.hContentLength, "42")]
          forM_ [(M.mHead, S.status200), (M.mGet, S.status304)] $ \(method, status) -> do
            let req = WReq.setMethod method (get (compileTemplate "/x"))
                inner = mockTransport $ \_ -> rawResponse status fields ""
            raw <- sendRaw (withDecompression inner) =<< prep req
            let RawResponse{headers = respFields} = raw
            respFields `shouldBe` fields
            rawResponseBytes raw `shouldReturn` ""
      , it "does not rebuild a response when its only coding is identity" $ do
          let fields = [(H.hContentEncoding, "identity"), (H.hContentLength, "3")]
              inner = mockTransport $ \_ -> rawResponse S.status200 fields "abc"
          forM_ [withDecompression, withDecompressionPolicy []] $ \middleware -> do
            raw <- sendRaw (middleware inner) =<< prep (get (compileTemplate "/x"))
            let RawResponse{headers = respFields} = raw
            respFields `shouldBe` fields
            rawResponseBytes raw `shouldReturn` "abc"
      , it "leaves unknown Content-Encoding alone" $ do
          let payload = "raw bytes" :: BS.ByteString
              inner = mockTransport $ \_ ->
                rawResponse
                  S.status200
                  [("Content-Encoding", "x-magic")]
                  payload
              transport = withDecompression inner
          raw <- sendRaw transport =<< prep (get (compileTemplate "/x"))
          bs <- rawResponseBytes raw
          bs `shouldBe` payload
      , it "decodes Content-Encoding across repeated field lines in reverse application order" $ do
          let payload = "gzip inside brotli" :: BS.ByteString
              compressed = BSL.toStrict (Brotli.compress (GZip.compress (BSL.fromStrict payload)))
              inner = mockTransport $ \_ ->
                rawResponse S.status200 [(H.hContentEncoding, "x-GZip"), (H.hContentEncoding, "BR")] compressed
          raw <- sendRaw (withDecompression inner) =<< prep (get (compileTemplate "/x"))
          rawResponseBytes raw `shouldReturn` payload
          let RawResponse {headers = respHdrs} = raw
          H.lookupHeaders H.hContentEncoding respHdrs `shouldBe` []
      , it "leaves the whole response intact when any Content-Encoding line is unsupported or malformed" $ do
          let compressed = BSL.toStrict (GZip.compress "a gzip payload")
          mapM_ (\later -> do
            let fields = [(H.hContentEncoding, "gzip"), (H.hContentEncoding, later)]
                inner = mockTransport $ \_ -> rawResponse S.status200 fields compressed
            raw <- sendRaw (withDecompression inner) =<< prep (get (compileTemplate "/x"))
            let RawResponse {headers = respHdrs} = raw
            H.lookupHeaders H.hContentEncoding respHdrs `shouldBe` ["gzip", later]
            rawResponseBytes raw `shouldReturn` compressed)
            ["unknown-coding", "br;q=0"]
      , it "matches registered custom handlers independently of coding name case" $ do
          let payload = "custom gzip"
              compressed = BSL.toStrict (GZip.compress "custom gzip")
              handler = (asDecompressor @Gzip)
                {ehCoding = Coding.Custom "x-test-gzip"}
              inner = mockTransport $ \_ ->
                rawResponse S.status200 [(H.hContentEncoding, "X-Test-GZip")] compressed
          raw <- sendRaw (withDecompressionPolicy [handler] inner) =<< prep (get (compileTemplate "/x"))
          rawResponseBytes raw `shouldReturn` payload
      , it "strips Content-Encoding + Content-Length after decompression" $ do
          let payload = "hello" :: BS.ByteString
              compressed = BSL.toStrict $ GZip.compress (BSL.fromStrict payload)
              inner = mockTransport $ \_ ->
                rawResponse
                  S.status200
                  [ ("Content-Encoding", "gzip")
                  ,
                    ( H.hContentLength
                    , BS8.pack (show (BS.length compressed))
                    )
                  ]
                  compressed
              transport = withDecompression inner
          raw <- sendRaw transport =<< prep (get (compileTemplate "/x"))
          let RawResponse {headers = respHdrs} = raw
          H.lookupHeader "Content-Encoding" respHdrs `shouldBe` Nothing
          H.lookupHeader H.hContentLength respHdrs `shouldBe` Nothing
      , it "removes wire digests after decoding and preserves validators" $ do
          let payload = "a coded body"
              encoded = BSL.toStrict (GZip.compress (BSL.fromStrict payload))
              validator = (H.hETag, "\"gzip-revision\"")
              fields = (H.hContentEncoding, "gzip") : validator : bodyDigests
              inner = mockTransport $ \_ -> rawResponse S.status200 fields encoded
          raw <- sendRaw (withDecompression inner) =<< prep (get (compileTemplate "/x"))
          let RawResponse{headers = respFields} = raw
          respFields `shouldBe` [validator]
          rawResponseBytes raw `shouldReturn` payload
      , it "removes original digests when compressing a request and keeps digest preferences" $ do
          let payload = "body before compression"
              preference = ("Want-Repr-Digest", "sha-256=10")
              inner = mockTransport $ \req -> do
                let Request{headers = fields} = req
                H.lookupHeader H.hContentEncoding fields `shouldBe` Just "gzip"
                H.lookupHeader (fst preference) fields `shouldBe` Just (snd preference)
                mapM_ (\(name, _) -> H.lookupHeader name fields `shouldBe` Nothing) bodyDigests
                compressed <- bodyStreamBytes (WReq.body req)
                H.lookupHeader H.hContentLength fields `shouldBe` Just (BS8.pack (show (BS.length compressed)))
                GZip.decompress (BSL.fromStrict compressed) `shouldBe` BSL.fromStrict payload
                rawResponse S.status200 [] ""
          stream <- streamFromStrict payload
          req <- prep (post (compileTemplate "/uploads"))
          _ <- sendRaw (withCompression @Gzip inner) req{WReq.body = stream, WReq.headers = preference : bodyDigests}
          pure ()
      , it "keeps digest fields on an identity-coded request" $ do
          let payload = "plain body"
              inner = mockTransport $ \req -> do
                let Request{headers = fields} = req
                fields `shouldBe` bodyDigests
                bodyStreamBytes (WReq.body req) `shouldReturn` payload
                rawResponse S.status200 [] ""
          stream <- streamFromStrict payload
          req <- prep (post (compileTemplate "/uploads"))
          _ <- sendRaw (withCompression @Compression.Identity inner) req{WReq.body = stream, WReq.headers = bodyDigests}
          pure ()
      ]
  where
    bodyDigests =
      [ ("Content-Digest", "sha-256=:47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=:")
      , ("content-digest", "md5=:1B2M2Y8AsgTpgAmY7PhCfg==:")
      , ("Repr-Digest", "sha-256=:47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=:")
      , ("Digest", "md5=1B2M2Y8AsgTpgAmY7PhCfg==")
      , ("Content-MD5", "1B2M2Y8AsgTpgAmY7PhCfg==")
      ]


vcrTests :: Spec
vcrTests =
  describe "VCR" $
    sequence_
      [ it "record then replay reproduces the response" $ do
          withSystemTempDirectory "wire-vcr" $ \dir -> do
            let cassettePath = dir <> "/login.yaml"
                real = stubJSON S.status201 (User 1 "alice")
                postUsers = post (compileTemplate "/users")
            Response {responseBody = u} <-
              recordSession real cassettePath $ \t ->
                sendIO t postUsers (as @JSON @User)
            u `shouldBe` User 1 "alice"

            cassette <- loadCassette cassettePath
            transport <- replayTransport cassette byMethodAndURI
            Response {responseBody = u2} <-
              sendIO transport postUsers (as @JSON @User)
            u2 `shouldBe` User 1 "alice"
      , it "keeps trailers on the live response and omits the announcement on replay" $
          withSystemTempDirectory "wire-vcr" $ \dir -> do
            let cassettePath = dir <> "/trailers.yaml"
                trailers = [("X-Result", "done")]
                real = mockTransport $ \_ -> do
                  raw <- rawResponse S.status200 [("Trailer", "X-Result")] "body"
                  pure raw {protocolInfo = withBufferedTrailers trailers (protocolInfo raw)}
                req = get (compileTemplate "/body")
            recordSession real cassettePath $ \transport ->
              withResponse transport req [] $ \live -> do
                rawResponseBytes live `shouldReturn` "body"
                rawResponseTrailers live `shouldReturn` trailers
            cassette <- loadCassette cassettePath
            transport <- replayTransport cassette byMethodAndURI
            withResponse transport req [] $ \replayed -> do
              rawResponseBytes replayed `shouldReturn` "body"
              rawResponseTrailers replayed `shouldReturn` []
              let RawResponse {headers = fields} = replayed
              H.lookupHeader H.hTrailer fields `shouldBe` Nothing
      ]
