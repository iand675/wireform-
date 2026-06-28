{-# LANGUAGE OverloadedStrings #-}

module Test.Handshake (tests) where

import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Network.HTTP.Message
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as M
import Network.HTTP.Types.Version qualified as V
import Network.WebSocket.Handshake
import Test.Syd


tests :: Spec
tests =
  describe "Handshake" $
    sequence_
      [ rfcAcceptVector
      , acceptsValidRequest
      , rejectsMissingKey
      , clientHandshakeRoundTrip
      ]


-- The canonical RFC 6455 sec 1.3 / sec 4.2.2 vector:
--   key    = "dGhlIHNhbXBsZSBub25jZQ=="
--   accept = "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
rfcAcceptVector :: Spec
rfcAcceptVector =
  it "RFC 6455 sec 4.2.2 Sec-WebSocket-Accept vector" $
    computeAccept "dGhlIHNhbXBsZSBub25jZQ=="
      `shouldBe` "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="


acceptsValidRequest :: Spec
acceptsValidRequest = it "parses a valid handshake request" $ do
  let req =
        mkReq
          [ (H.hHost, "example.com")
          , (H.hUpgrade, "websocket")
          , (H.hConnection, "Upgrade")
          , (CI.mk "Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ==")
          , (CI.mk "Sec-WebSocket-Version", "13")
          ]
  case parseWebSocketRequest req of
    Right ws -> wsReqKey ws `shouldBe` "dGhlIHNhbXBsZSBub25jZQ=="
    Left e -> expectationFailure ("rejected valid request: " <> show e)


rejectsMissingKey :: Spec
rejectsMissingKey = it "rejects missing Sec-WebSocket-Key" $ do
  let req =
        mkReq
          [ (H.hHost, "example.com")
          , (H.hUpgrade, "websocket")
          , (H.hConnection, "Upgrade")
          , (CI.mk "Sec-WebSocket-Version", "13")
          ]
  case parseWebSocketRequest req of
    Left _ -> pure ()
    Right _ -> expectationFailure "accepted a request without Sec-WebSocket-Key"


clientHandshakeRoundTrip :: Spec
clientHandshakeRoundTrip = it "client handshake -> server verifies" $ do
  let opts =
        (defaultWebSocketHandshakeOpts "/chat" "example.com")
          { wsOptProtocols = ["chat"]
          }
  (reqBytes, key) <- buildClientHandshake opts
  -- Sanity check: GET line is correct.
  (BS.isPrefixOf "GET /chat HTTP/1.1" reqBytes) `shouldBe` True
  -- Server replies with computed accept; client verifies.
  let acceptVal = computeAccept key
      hdrs =
        [ (H.hUpgrade, "websocket")
        , (H.hConnection, "Upgrade")
        , (CI.mk "Sec-WebSocket-Accept", acceptVal)
        ]
  case verifyServerHandshake key 101 hdrs of
    Right () -> pure ()
    Left e -> expectationFailure ("client verify failed: " <> show e)

  -- Negative case: wrong accept value rejected.
  let hdrsBad =
        [ (H.hUpgrade, "websocket")
        , (H.hConnection, "Upgrade")
        , (CI.mk "Sec-WebSocket-Accept", "wrong=")
        ]
  case verifyServerHandshake key 101 hdrsBad of
    Left _ -> pure ()
    Right _ -> expectationFailure "accepted a bogus Sec-WebSocket-Accept"


mkReq :: H.Headers -> Request
mkReq hdrs =
  Request
    { requestMethod = M.mGet
    , requestTarget = "/chat"
    , requestAuthority = lookup H.hHost hdrs
    , requestScheme = SchemeHttp
    , requestHeaders = hdrs
    , requestBody = BodyEmpty
    , requestVersion = V.HTTP1_1
    , requestTrailers = pure []
    }
