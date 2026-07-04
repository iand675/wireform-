{-# LANGUAGE OverloadedStrings #-}

-- | __Exploration campaign__ against the real vendored HTTP/2 engine over the
-- @wireform-dst@ fault link. Unlike @http2-fault-test@ (hand-designed cases
-- that pass by construction), this /generates/ fault schedules from a range of
-- seeds and folds the outcomes through an invariant oracle — a randomized bug
-- hunt that runs in CI, not a manually-fed REPL session.
--
-- Invariants, and why each fault class checks what it does:
--
--   * __latency__ (delay both directions): the engine must always complete
--     with the correct response — pure liveness/correctness stressor.
--   * __cut__ (TCP reset / peer death at a random moment): must complete or
--     raise a /clean/ error within the timeout — never hang, never crash,
--     never deliver wrong data.
--   * __partition + heal__ (transient outage): must complete correctly once
--     the link heals — liveness under recoverable partitions.
--   * __corruption / drop__ at the byte layer: NOT fair safety/liveness tests
--     for cleartext h2c (HTTP/2 doesn't checksum DATA, so a flipped byte is
--     legitimately delivered and a dropped chunk legitimately desyncs framing
--     → stall). So these hunt only for __engine crashes__: malformed input must
--     surface as a clean protocol error or a stall, never an @ErrorCall@ /
--     pattern-match failure / arithmetic or array exception.
--
-- A failing campaign prints the offending seed(s) so the schedule reproduces.
module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Test.Syd

import Network.HTTP2.Client
import Network.HTTP2.Server
import Network.HTTP2.Transport (Transport (..))
import Sim.Net.Explore
import Sim.Net.Link

-- ---------------------------------------------------------------------------
-- HTTP/2 workload driver (one request/response over a SimLink)
-- ---------------------------------------------------------------------------

linkTransport :: LinkEnd -> Transport
linkTransport end =
  Transport
    { tSendFn = leSendFn end
    , tRecvBuf = leReceiveFn end
    , tShutdownWrite = leShutdown end
    , tClose = pure ()
    }

serverCfg :: ServerConfig
serverCfg =
  defaultServerConfig
    { serverHandler = \_req respond ->
        respond
          defaultResponse
            { responseStatus = 200
            , responseHeaders = [("content-type", "text/plain")]
            , responseBody = ResponseBodyBS "pong"
            }
    }

pingReq :: ClientRequest
pingReq =
  ClientRequest
    { crMethod = "GET"
    , crPath = "/ping"
    , crScheme = "http"
    , crAuthority = "sim"
    , crHeaders = []
    , crBody = ReqBodyNone
    }

-- Fork the real server + client over one fault link and do a single request.
http2Workload :: SimLink -> IO (Int, ByteString)
http2Workload l =
  bracket
    (forkIO $ runServerOnTransport serverCfg (linkTransport (slServer l)))
    killThread
    ( \_ ->
        withConnectionOnTransport
          defaultClientConfig
          (linkTransport (slClient l))
          Nothing
          ( \h -> do
              resp <- sendRequest h pingReq
              body <- drainResponseBody resp
              pure (crStatus resp, body)
          )
    )

correct :: (Int, ByteString) -> Bool
correct = (== (200, "pong"))

seedsFrom :: Int -> Int -> [Seed]
seedsFrom base n = map (Seed . fromIntegral) [base .. base + n - 1]

-- ---------------------------------------------------------------------------
-- Campaigns
-- ---------------------------------------------------------------------------

main :: IO ()
main =
  sydTest $
    describe "HTTP/2 engine — randomized fault exploration over the sim link" $ do
      it "latency (0-4ms both dirs): all 60 trials complete correctly" $ do
        rep <- runCampaign 3000000 (seedsFrom 1000 60) latencySchedule correct http2Workload
        crInternal rep `shouldBe` []
        crWrong rep `shouldBe` []
        crTimeouts rep `shouldBe` []
        crOK rep `shouldBe` 60

      it "cut at random moment: no hang, no crash, no wrong data (60 trials)" $ do
        rep <- runCampaign 2000000 (seedsFrom 2000 60) (cutSchedule 30000) correct http2Workload
        crInternal rep `shouldBe` []
        crWrong rep `shouldBe` []
        crTimeouts rep `shouldBe` []
        -- every trial either completed cleanly or raised a clean error
        (crOK rep + crErrored rep) `shouldBe` 60

      it "transient partition + heal: liveness preserved (40 trials)" $ do
        rep <- runCampaign 3000000 (seedsFrom 3000 40) partitionHealSchedule correct http2Workload
        crInternal rep `shouldBe` []
        crWrong rep `shouldBe` []
        crTimeouts rep `shouldBe` []
        crOK rep `shouldBe` 40

      it "byte corruption: engine never crashes on malformed input (40 trials)" $ do
        rep <- runCampaign 1500000 (seedsFrom 4000 40) corruptSchedule correct http2Workload
        -- safety/liveness not asserted (h2c has no DATA integrity); crash-only
        crInternal rep `shouldBe` []

      it "byte drop then reset: no crash, no hang (40 trials)" $ do
        rep <- runCampaign 1000000 (seedsFrom 5000 40) dropSchedule correct http2Workload
        -- Silently-wrong / incomplete data is tolerated (cleartext h2c has no
        -- DATA integrity), but the engine must never crash and must never
        -- hang: the trailing reset always unsticks it. This campaign
        -- originally found a real wireform-http2 liveness bug — DATA arriving
        -- before response HEADERS (loss dropped the HEADERS frame) orphaned
        -- the response-headers MVar, hanging 'sendRequest' forever. Fixed by
        -- rejecting DATA-before-HEADERS as a protocol error; liveness is now
        -- asserted to guard against regressions.
        crInternal rep `shouldBe` []
        crTimeouts rep `shouldBe` []
