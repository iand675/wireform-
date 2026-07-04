{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

-- | __Exploration campaign against the real Kafka client__ (@wireform-kafka@)
-- running over the @wireform-dst@ fault link. Unlike gRPC / Connect — which
-- have in-process servers — Kafka is client-only and rides a magic-ring
-- 'DuplexTransport'. We drive the /real/ client wire path: a 'Connection' built
-- over the SimLink's 'leDuplex' (the same magic-ring transport a live broker
-- socket produces, just fed by the fault link instead of a kernel socket) plus
-- a dummy unconnected socket that only exists so 'connectionClose' has
-- something to close. The client runs the genuine 'negotiateVersions' handshake
-- (ApiVersions v3: frame + request-header encode, blocking frame read, flexible
-- response decode); a minimal raw-fn mock broker on the other end reads the
-- request frame, extracts the correlation id, and replies with a real
-- codec-encoded 'ApiVersionsResponse'.
--
-- This proves the SimLink's __duplex__ path (concurrent client send + receive
-- over one connection) carries a real request/response protocol under faults —
-- the seam HTTP/1 and WebSocket also use. An isolation repro
-- (@dst-net-duplex-repro@) separately confirms the duplex itself is memory-safe
-- under concurrent bidi + faults; the earlier "duplex misbehaves" note was
-- HTTP/1-layer-specific, not the SimLink.
--
-- Fault fairness mirrors the other explore suites: latency / partition-heal are
-- full liveness (the handshake must complete); cut is a clean failure (no crash,
-- no wrong data); byte corruption / drop are crash-only (a mangled or truncated
-- frame legitimately fails to decode or stalls, but must never crash).
module Main (main) where

import Control.Concurrent.Async (withAsync)
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.IORef (newIORef)
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Data.Word (Word8)
import Foreign.Ptr (castPtr)
import Test.Syd

import Kafka.Network.Connection (BrokerAddress (..))
import Kafka.Network.Connection.Internal (Connection (..))
import Kafka.Protocol.ApiVersions (createVersionCache, negotiateVersions)
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ApiVersionsResponse qualified as AVResp
import "wireform-kafka-protocol" Kafka.Protocol.Primitives qualified as P
import "wireform-kafka-protocol" Kafka.Protocol.Wire.Codec qualified as WC
import Network.Socket qualified as NS

import Sim.Net.Explore
import Sim.Net.Link

-- ---------------------------------------------------------------------------
-- Wire helpers
-- ---------------------------------------------------------------------------

int32BE :: Int -> ByteString
int32BE n = BS.pack [byte 24, byte 16, byte 8, byte 0]
  where
    byte s = fromIntegral (n `shiftR` s) :: Word8

be32At :: ByteString -> Int -> Int32
be32At bs o =
  (b 0 `shiftL` 24) .|. (b 1 `shiftL` 16) .|. (b 2 `shiftL` 8) .|. b 3
  where
    b i = fromIntegral (BS.index bs (o + i)) :: Int32

-- Send a whole ByteString over a LinkEnd's raw pointer 'SendFn'.
leSendBS :: LinkEnd -> ByteString -> IO ()
leSendBS end bs =
  BSU.unsafeUseAsCStringLen bs $ \(p, n) -> do
    _ <- leSendFn end (castPtr p) n
    pure ()

-- ---------------------------------------------------------------------------
-- Minimal mock broker: one ApiVersions round-trip over the raw fault-fns.
-- ---------------------------------------------------------------------------

-- A canned, real ApiVersionsResponse (v3) advertising a few APIs.
apiVersionsBody :: ByteString
apiVersionsBody =
  WC.runEncodeVer @AVResp.ApiVersionsResponse 3 $
    AVResp.ApiVersionsResponse
      { AVResp.apiVersionsResponseErrorCode = 0
      , AVResp.apiVersionsResponseApiKeys =
          P.mkKafkaArray
            ( V.fromList
                [ AVResp.ApiVersion 18 0 3
                , AVResp.ApiVersion 3 0 12
                , AVResp.ApiVersion 0 0 9
                ]
            )
      , AVResp.apiVersionsResponseThrottleTimeMs = 0
      , AVResp.apiVersionsResponseSupportedFeatures = P.mkKafkaArray V.empty
      , AVResp.apiVersionsResponseFinalizedFeaturesEpoch = -1
      , AVResp.apiVersionsResponseFinalizedFeatures = P.mkKafkaArray V.empty
      , AVResp.apiVersionsResponseZkMigrationReady = False
      }

-- Serve exactly one request on the server end, then return. Defensive against
-- short / corrupted / truncated frames (fault link): any anomaly just returns.
mockBroker :: SimLink -> IO ()
mockBroker l = do
  let srv = slServer l
  sizeB <- leReadExactN srv 4
  when (BS.length sizeB == 4) $ do
    let sz = fromIntegral (be32At sizeB 0) :: Int
    when (sz > 0 && sz <= 10_000_000) $ do
      reqFrame <- leReadExactN srv sz
      -- correlation id is at offset 4 (after apiKey:int16 + apiVersion:int16),
      -- regardless of request-header flexibility.
      when (BS.length reqFrame >= 8) $ do
        let cid = be32At reqFrame 4
            respFrame = int32BE (fromIntegral cid) <> apiVersionsBody
            full = int32BE (BS.length respFrame) <> respFrame
        leSendBS srv full

-- ---------------------------------------------------------------------------
-- Client workload: real negotiateVersions over a SimLink-backed Connection.
-- ---------------------------------------------------------------------------

kafkaWorkload :: SimLink -> IO Bool
kafkaWorkload l =
  withAsync (mockBroker l) $ \_ -> do
    -- Dummy socket: never used for I/O (the duplex is the fault link); only
    -- held so the Connection is well-formed and closable.
    dummy <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
    cursor <- newIORef 0
    closed <- newIORef False
    let conn =
          Connection
            { connDuplex = leDuplex (slClient l)
            , connSocket = dummy
            , connSslConn = Nothing
            , connCtx = Nothing
            , connCursor = cursor
            , connClosed = closed
            }
    cache <- createVersionCache
    result <- negotiateVersions conn (BrokerAddress "sim" 9092) cache 1
    NS.close dummy
    case result of
      -- A clean handshake failure (cut / desync) is a clean error, not a
      -- silent-correctness bug: re-raise so the oracle files it under crErrored.
      Left err -> throwIO (userError err)
      Right versionMap -> pure (not (Map.null versionMap))

correct :: Bool -> Bool
correct = id

seedsFrom :: Int -> Int -> [Seed]
seedsFrom base n = map (Seed . fromIntegral) [base .. base + n - 1]

-- ---------------------------------------------------------------------------
-- Campaigns
-- ---------------------------------------------------------------------------

main :: IO ()
main =
  sydTest $
    describe "Kafka client — randomized fault exploration over the sim link (duplex)" $ do
      it "latency (0-4ms both dirs): all handshakes complete (40 trials)" $ do
        rep <- runCampaign 5000000 (seedsFrom 1000 40) latencySchedule correct kafkaWorkload
        crInternal rep `shouldBe` []
        crWrong rep `shouldBe` []
        crTimeouts rep `shouldBe` []
        crOK rep `shouldBe` 40

      it "cut at random moment: no crash, no wrong data (40 trials)" $ do
        rep <- runCampaign 4000000 (seedsFrom 2000 40) (cutSchedule 20000) correct kafkaWorkload
        crInternal rep `shouldBe` []
        crWrong rep `shouldBe` []

      it "transient partition + heal: liveness preserved (30 trials)" $ do
        rep <- runCampaign 5000000 (seedsFrom 3000 30) partitionHealSchedule correct kafkaWorkload
        crInternal rep `shouldBe` []
        crWrong rep `shouldBe` []
        crTimeouts rep `shouldBe` []
        crOK rep `shouldBe` 30

      it "byte corruption: Kafka client never crashes on malformed input (30 trials)" $ do
        rep <- runCampaign 3000000 (seedsFrom 4000 30) corruptSchedule correct kafkaWorkload
        crInternal rep `shouldBe` []

      it "byte drop then reset: Kafka client never crashes (30 trials)" $ do
        rep <- runCampaign 3000000 (seedsFrom 5000 30) dropSchedule correct kafkaWorkload
        crInternal rep `shouldBe` []
