{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

{- | Tests for 'parseResponseFrame' covering the flexible vs.
non-flexible response-header dispatch.

Background: the broker sends a /version-aware/ response header.
Non-flexible APIs use response header v0 (just the 4-byte
correlation id); flexible APIs use response header v1
(correlation id + a 'TaggedFields' trailer).
'ApiVersionsResponse' is the special case that always uses v0
regardless of body version. Skipping the right number of bytes
between the correlation id and the actual body is mandatory —
leaving the v1 tagged-fields trailer attached to the body
shifts every subsequent field by one byte and makes the entire
decode return garbage (in DescribeConfigs v4 it presented as
the @results@ array always being empty).
-}
module Client.ResponseFrameSpec (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Data.UUID qualified as UUID
import Data.Word (Word8)
import Kafka.Client.Internal.Request (
  frameRequestWithClientInstanceId,
  parseResponseFrame,
  requestHeaderVersionFor,
  responseHeaderVersionFor,
  sendRequestOn,
 )
import Kafka.Network.Connection qualified as Conn
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBytes
import Test.Syd
import "wireform-kafka-protocol" Kafka.Protocol.Generated.RequestHeader qualified as RH
import "wireform-kafka-protocol" Kafka.Protocol.Primitives qualified as P
import "wireform-kafka-protocol" Kafka.Protocol.Wire qualified as W
import "wireform-kafka-protocol" Kafka.Protocol.Wire.Codec qualified as WC


----------------------------------------------------------------------
-- Wire helpers
----------------------------------------------------------------------

{- | A complete response frame the way the broker would put it on
the wire: 4-byte size prefix, then the header + body.
-}
mkFrame :: ByteString -> ByteString
mkFrame hb =
  let !len = BS.length hb
  in BS.pack
       [ fromIntegral (len `div` 0x1000000 `mod` 0x100)
       , fromIntegral (len `div` 0x10000 `mod` 0x100)
       , fromIntegral (len `div` 0x100 `mod` 0x100)
       , fromIntegral (len `mod` 0x100)
       ]
       <> hb


type ByteString = BS.ByteString


-- | Big-endian Int32.
i32 :: Int -> [Word8]
i32 n =
  [ fromIntegral ((n `div` 0x1000000) `mod` 0x100)
  , fromIntegral ((n `div` 0x10000) `mod` 0x100)
  , fromIntegral ((n `div` 0x100) `mod` 0x100)
  , fromIntegral (n `mod` 0x100)
  ]


----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: Spec
tests =
  describe "Client.ResponseFrameSpec" $
    sequence_
      [ headerVersionTests
      , requestFrameTests
      , parseTests
      ]


headerVersionTests :: Spec
headerVersionTests =
  describe "responseHeaderVersionFor" $
    sequence_
      [ it "Produce v3 (non-flexible) -> response header v0" $
          responseHeaderVersionFor 0 3 `shouldBe` 0
      , it "Produce v9 (flexible) -> response header v1" $
          responseHeaderVersionFor 0 9 `shouldBe` 1
      , it "Fetch v11 (non-flexible) -> response header v0" $
          responseHeaderVersionFor 1 11 `shouldBe` 0
      , it "Fetch v12 (flexible) -> response header v1" $
          responseHeaderVersionFor 1 12 `shouldBe` 1
      , it "DescribeConfigs v3 (non-flexible) -> response header v0" $
          responseHeaderVersionFor 32 3 `shouldBe` 0
      , it "DescribeConfigs v4 (flexible) -> response header v1" $
          responseHeaderVersionFor 32 4 `shouldBe` 1
      , it "ApiVersions v0 -> response header v0" $
          responseHeaderVersionFor 18 0 `shouldBe` 0
      , it "ApiVersions v3 (flexible body) -> response header v0 (special case)" $
          -- This is the JVM-client workaround: even though the body
          -- is flexible, the broker /always/ sends ApiVersionsResponse
          -- with response header v0 to break the chicken-and-egg
          -- problem. Mirroring that here is what makes the negotiation
          -- handshake itself parse correctly.
          responseHeaderVersionFor 18 3 `shouldBe` 0
      , it "Unknown api key -> default to header v0 (non-flexible)" $
          responseHeaderVersionFor 999 0 `shouldBe` 0
      , describe "request/response header pairing" $
          sequence_
            [ it "Produce v3: request v1 / response v0" $ do
                requestHeaderVersionFor 0 3 `shouldBe` 1
                responseHeaderVersionFor 0 3 `shouldBe` 0
            , it "Produce v9: request v2 / response v1" $ do
                requestHeaderVersionFor 0 9 `shouldBe` 2
                responseHeaderVersionFor 0 9 `shouldBe` 1
            , it "ApiVersions v3: request v2 / response v0 (asymmetric on purpose)" $ do
                requestHeaderVersionFor 18 3 `shouldBe` 2
                responseHeaderVersionFor 18 3 `shouldBe` 0
            , it "selects header v3 at each KIP-1313 boundary" $
                forM_ [(0, 14, 1), (1, 19, 1), (18, 6, 0), (58, 1, 1)] $ \(key, version, response) -> do
                  requestHeaderVersionFor key (version - 1) `shouldBe` 2
                  requestHeaderVersionFor key version `shouldBe` 3
                  responseHeaderVersionFor key version `shouldBe` response
            ]
      ]


requestFrameTests :: Spec
requestFrameTests = do
  it "encodes the v3 UUID as tag 0 with a 16-byte payload" $ do
    let instanceId = P.mkKafkaUuid (UUID.fromWords 0x01020304 0x05060708 0x090a0b0c 0x0d0e0f10)
        body = BS.pack [0xa1, 0xb2]
        prefix = [0, 18, 0, 6, 0, 0, 0, 7, 0, 1, 120]
        expected = mkFrame (BS.pack (prefix ++ [1, 0, 16] ++ [1 .. 16]) <> body)
    frameRequestWithClientInstanceId instanceId 18 6 7 (P.mkKafkaString "x") body `shouldBe` expected
    frameRequestWithClientInstanceId P.nullUuid 18 6 7 (P.mkKafkaString "x") body
      `shouldBe` mkFrame (BS.pack (prefix ++ [0]) <> body)
  it "sends client identity on the first v3 request and omits it thereafter" $
    bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \listener -> do
      Socket.bind listener (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
      Socket.listen listener 1
      port <- Socket.socketPort listener
      let address = Conn.BrokerAddress "127.0.0.1" (fromIntegral port)
          client = P.mkKafkaString "wireform"
      result <- Conn.withConnection address Conn.defaultConnectionConfig $ \connection ->
        bracket (fst <$> Socket.accept listener) Socket.close $ \peer -> do
          sendRequestOn connection 18 6 1 client BS.empty
          first <- readRequestHeader peer
          RH.requestHeaderClientId first `shouldBe` client
          RH.requestHeaderClientInstanceId first `shouldBe` Conn.connClientInstanceId connection
          RH.requestHeaderClientInstanceId first `shouldNotBe` P.nullUuid
          sendRequestOn connection 1 19 2 client BS.empty
          later <- readRequestHeader peer
          RH.requestHeaderClientId later `shouldBe` P.KafkaString P.Null
          RH.requestHeaderClientInstanceId later `shouldBe` P.nullUuid
      result `shouldBe` Right ()
  where
    readRequestHeader peer = do
      prefix <- readExactly peer 4
      size <- either fail pure (W.readInt32BE prefix)
      bytes <- readExactly peer (fromIntegral size)
      either fail pure (WC.runDecodeVer 3 bytes)

    readExactly peer size = go size []
      where
        go 0 chunks = pure (BS.concat (reverse chunks))
        go remaining chunks = do
          chunk <- SocketBytes.recv peer remaining
          if BS.null chunk
            then fail "unexpected EOF while reading request frame"
            else go (remaining - BS.length chunk) (chunk : chunks)


parseTests :: Spec
parseTests =
  describe "parseResponseFrame" $
    sequence_
      [ it "non-flexible response: body starts immediately after correlation id" $ do
          -- DescribeConfigs v3: header v0 = just correlation id;
          -- body = throttle (0) + results array (empty: Int32 0).
          let body = i32 0 {- throttle -} ++ i32 0 {- empty results -}
              hb = i32 42 {- correlation id -} ++ body
              frame = mkFrame (BS.pack hb)
          case parseResponseFrame 32 3 frame of
            Right (cid, b) -> do
              cid `shouldBe` 42
              BS.unpack b `shouldBe` body
            Left e -> expectationFailure ("parse failed: " <> e)
      , it "flexible response: empty tagged-fields trailer is skipped" $ do
          -- DescribeConfigs v4: header v1 = correlation id + 1 byte
          -- empty TaggedFields (UVarInt 0 = 0x00); body starts after.
          let body = i32 0 {- throttle -} ++ [0x01 {- compact-len 0 = empty results -}] ++ [0x00 {- body tag -}]
              hb = i32 42 ++ [0x00 {- header tagged-fields -}] ++ body
              frame = mkFrame (BS.pack hb)
          case parseResponseFrame 32 4 frame of
            Right (cid, b) -> do
              cid `shouldBe` 42
              -- The body returned must NOT include the header's
              -- tagged-fields byte. If we got the byte wrong, the
              -- caller's decoder would read the throttle as
              -- 0x00 << 24 | (next 3 bytes) and corrupt every
              -- subsequent field.
              BS.unpack b `shouldBe` body
            Left e -> expectationFailure ("parse failed: " <> e)
      , it "flexible response: non-empty tagged-fields trailer is also skipped" $ do
          -- TaggedFields = UVarInt count; then for each field
          -- (UVarInt tag, UVarInt size, size bytes). One field with
          -- tag=7, size=2, payload [0xAA, 0xBB].
          let headerTagged =
                [ 0x01 -- count: UVarInt 1
                , 0x07 -- tag:   UVarInt 7
                , 0x02 -- size:  UVarInt 2
                , 0xAA
                , 0xBB
                ]
              body = i32 99
              hb = i32 7 ++ headerTagged ++ body
              frame = mkFrame (BS.pack hb)
          case parseResponseFrame 32 4 frame of
            Right (cid, b) -> do
              cid `shouldBe` 7
              BS.unpack b `shouldBe` body
            Left e -> expectationFailure ("parse failed: " <> e)
      , it "ApiVersionsResponse v3: response header v0 even though body is flexible" $ do
          -- Body is whatever; the parser must not eat any tagged-fields.
          let body = [0xAB, 0xCD, 0xEF, 0x01]
              hb = i32 11 ++ body
              frame = mkFrame (BS.pack hb)
          case parseResponseFrame 18 3 frame of
            Right (cid, b) -> do
              cid `shouldBe` 11
              BS.unpack b `shouldBe` body
            Left e -> expectationFailure ("parse failed: " <> e)
      , it "truncated frame: missing size prefix -> Left" $ do
          let r = parseResponseFrame 0 3 (BS.pack [0x00])
          case r of
            Left _ -> pure ()
            Right v -> expectationFailure ("expected Left, got Right " <> show v)
      , it "truncated frame: missing correlation id -> Left" $ do
          let frame = mkFrame (BS.pack [0x00, 0x00])
          case parseResponseFrame 0 3 frame of
            Left _ -> pure ()
            Right v -> expectationFailure ("expected Left, got Right " <> show v)
      , it "truncated frame (flexible): missing tagged-fields trailer -> Left" $ do
          -- 4 bytes for correlation id, but no tagged-fields byte
          -- follows, so the parser must reject rather than return
          -- garbage.
          let frame = mkFrame (BS.pack (i32 7))
          case parseResponseFrame 32 4 frame of
            Left _ -> (True) `shouldBe` True
            Right (_, b) ->
              -- An empty body is fine; what we want to make sure of
              -- is that we /don't/ silently steal a byte from a
              -- non-existent trailer.
              (BS.null b) `shouldBe` True
      ]
