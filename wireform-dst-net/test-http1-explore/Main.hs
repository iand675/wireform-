{-# LANGUAGE OverloadedStrings #-}

-- | __Exploration campaign against the real HTTP/1.1 stack__ (@wireform-http1@
-- client + server) over the @wireform-dst@ fault link. HTTP/1 rides the
-- magic-ring 'DuplexTransport' (client 'newConnectionFromDuplex' +
-- 'sendRequestOn', server 'runServerOnConnection'), i.e. the SimLink's
-- 'leDuplex'.
--
-- Building this suite surfaced (and fixed) a real @wireform-network@ bug: the
-- magic-ring 'closeDuplexTransport' was not actually idempotent (despite its
-- haddock) — 'destroyMagicRing' @munmap@s its base pointer unconditionally, so
-- a re-entrant / concurrent second close would @munmap@ an address a fresh ring
-- had since been mapped at → heap corruption / SIGSEGV. Under the concurrent
-- fault campaigns (each trial opens + closes a fresh pair of connections, with
-- latency widening the teardown window) this crashed hard. Fixed by making the
-- ring teardown fire at most once, atomically ('mkDestroyOnce' in
-- @Wireform.Network.Transport.Duplex@).
--
-- Fault fairness mirrors the other explore suites: latency / partition-heal are
-- full liveness (request completes with the right status + body); cut is a clean
-- failure (a 'ParseError', never a crash, never wrong data); byte corruption /
-- drop are crash-only.
module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Test.Syd

import Network.HTTP1.Client (ClientConnection (..), closeClientConnection, sendRequestOn)
import Network.HTTP1.Connection (newConnectionFromDuplex)
import Network.HTTP1.Method
import Network.HTTP1.Server (ServerConfig (..), defaultServerConfig, runServerOnConnection)
import Network.HTTP1.Status
import Network.HTTP1.Types
import Network.HTTP1.Version

import Sim.Net.Explore hiding (OK)
import Sim.Net.Link

serverCfg :: ServerConfig
serverCfg =
  defaultServerConfig
    { serverHandler = \_req ->
        pure
          Response
            { responseStatus = OK
            , responseVersion = HTTP_1_1
            , responseHeaders = [("Content-Type", "text/plain")]
            , responseBody = BodyBytes "pong"
            , responseTrailers = pure []
            }
    }

pingReq :: Request
pingReq =
  Request
    { requestMethod = GET
    , requestTarget = "/ping"
    , requestVersion = HTTP_1_1
    , requestHeaders = [("Host", "sim")]
    , requestBody = BodyEmpty
    , requestTrailers = pure []
    }

drainAll :: Body -> IO ByteString
drainAll BodyEmpty = pure ""
drainAll (BodyBytes bs) = pure bs
drainAll (BodyPreEncoded _) = pure ""
drainAll (BodyFile _) = pure ""
drainAll (BodyStream prod) = BS.concat <$> go
  where
    go = do
      mc <- prod
      case mc of
        Nothing -> pure []
        Just c -> (c :) <$> go

-- Fork the real HTTP/1 server + issue one request over the fault link. Both
-- connections are closed deterministically (bracket) — a leaked magic-ring
-- connection whose finalizer munmaps later races the next trial's rings.
http1Workload :: SimLink -> IO (Status, ByteString)
http1Workload l =
  bracket
    ( forkIO $ do
        sConn <- newConnectionFromDuplex (leDuplex (slServer l))
        runServerOnConnection serverCfg sConn
    )
    killThread
    ( \_ ->
        bracket
          (ClientConnection <$> newConnectionFromDuplex (leDuplex (slClient l)))
          closeClientConnection
          ( \cc -> do
              r <- sendRequestOn cc pingReq
              case r of
                -- A clean parse failure (cut / desync) is a clean error, not a
                -- silent-correctness bug: re-raise so the oracle files it under crErrored.
                Left err -> throwIO (userError (show err))
                Right resp -> do
                  body <- drainAll (responseBody resp)
                  pure (responseStatus resp, body)
          )
    )

correct :: (Status, ByteString) -> Bool
correct = (== (OK, "pong"))

seedsFrom :: Int -> Int -> [Seed]
seedsFrom base n = map (Seed . fromIntegral) [base .. base + n - 1]

main :: IO ()
main =
  sydTest $
    describe "HTTP/1.1 stack — randomized fault exploration over the sim link (duplex)" $ do
      it "latency (0-4ms both dirs): all requests complete OK (40 trials)" $ do
        rep <- runCampaign 5000000 (seedsFrom 1000 40) latencySchedule correct http1Workload
        crInternal rep `shouldBe` []
        crWrong rep `shouldBe` []
        crTimeouts rep `shouldBe` []
        crOK rep `shouldBe` 40

      it "cut at random moment: no crash, no wrong data (40 trials)" $ do
        rep <- runCampaign 4000000 (seedsFrom 2000 40) (cutSchedule 20000) correct http1Workload
        crInternal rep `shouldBe` []
        crWrong rep `shouldBe` []

      it "transient partition + heal: liveness preserved (30 trials)" $ do
        rep <- runCampaign 5000000 (seedsFrom 3000 30) partitionHealSchedule correct http1Workload
        crInternal rep `shouldBe` []
        crWrong rep `shouldBe` []
        crTimeouts rep `shouldBe` []
        crOK rep `shouldBe` 30

      it "byte corruption: HTTP/1 stack never crashes on malformed input (30 trials)" $ do
        rep <- runCampaign 3000000 (seedsFrom 4000 30) corruptSchedule correct http1Workload
        crInternal rep `shouldBe` []

      it "byte drop then reset: HTTP/1 stack never crashes (30 trials)" $ do
        rep <- runCampaign 3000000 (seedsFrom 5000 30) dropSchedule correct http1Workload
        crInternal rep `shouldBe` []
