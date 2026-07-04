{-# LANGUAGE OverloadedStrings #-}

-- | __Exploration campaign against the real WebSocket frame layer__
-- (@wireform-websocket@) over the @wireform-dst@ fault link. WebSocket rides
-- the magic-ring 'DuplexTransport': a 'Connection' is built directly from a
-- transport via 'newConnection' (role-aware masking), so we drive both ends
-- over the SimLink's 'leDuplex' and exercise the real RFC 6455 frame codec
-- (client-masked / server-unmasked frames, fragmentation reassembly, UTF-8
-- validation, control-frame handling) under faults.
--
-- The opening HTTP\/1.1 @Upgrade@ handshake is /not/ re-tested here — it is an
-- HTTP\/1 exchange already covered by @http1-explore@ (and shares the same
-- 'leDuplex' seam). This suite targets the WebSocket-specific message layer,
-- which begins once 'newConnection' has taken over the transport.
--
-- Like the sibling suites, this also relies on the @wireform-network@ duplex
-- double-@munmap@ fix (see @http1-explore@): every trial opens + closes a fresh
-- pair of magic-ring connections concurrently.
--
-- Fault fairness: latency / partition-heal are full liveness (the echo
-- round-trips); cut is a clean failure (a connection exception, never a crash,
-- never wrong data); byte corruption / drop are crash-only.
module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Test.Syd

import Network.WebSocket.Connection (closeConnection, newConnection)
import Network.WebSocket.Connection.Role (Role (..))
import Network.WebSocket.Frame (defaultPayloadLimit)
import Network.WebSocket.Message
  ( Message (..)
  , defaultMessageLimit
  , receiveMessage
  , sendBinaryMessage
  , sendTextMessage
  )

import Sim.Net.Explore
import Sim.Net.Link

-- Fork the real WebSocket server end (echo one message) + drive the client end
-- over the fault link. Both connections are closed deterministically.
wsWorkload :: SimLink -> IO Message
wsWorkload l =
  bracket
    ( forkIO $
        bracket
          (newConnection Server defaultPayloadLimit (leDuplex (slServer l)))
          closeConnection
          ( \sc -> do
              m <- receiveMessage sc defaultMessageLimit
              case m of
                TextMessage t -> sendTextMessage sc t
                BinaryMessage b -> sendBinaryMessage sc b
          )
    )
    killThread
    ( \_ ->
        bracket
          (newConnection Client defaultPayloadLimit (leDuplex (slClient l)))
          closeConnection
          ( \cc -> do
              sendTextMessage cc "ping"
              receiveMessage cc defaultMessageLimit
          )
    )

correct :: Message -> Bool
correct = (== TextMessage "ping")

seedsFrom :: Int -> Int -> [Seed]
seedsFrom base n = map (Seed . fromIntegral) [base .. base + n - 1]

main :: IO ()
main =
  sydTest $
    describe "WebSocket frame layer — randomized fault exploration over the sim link (duplex)" $ do
      it "latency (0-4ms both dirs): all echoes round-trip (40 trials)" $ do
        rep <- runCampaign 5000000 (seedsFrom 1000 40) latencySchedule correct wsWorkload
        crInternal rep `shouldBe` []
        crWrong rep `shouldBe` []
        crTimeouts rep `shouldBe` []
        crOK rep `shouldBe` 40

      it "cut at random moment: no crash, no wrong data (40 trials)" $ do
        rep <- runCampaign 4000000 (seedsFrom 2000 40) (cutSchedule 20000) correct wsWorkload
        crInternal rep `shouldBe` []
        crWrong rep `shouldBe` []

      it "transient partition + heal: liveness preserved (30 trials)" $ do
        rep <- runCampaign 5000000 (seedsFrom 3000 30) partitionHealSchedule correct wsWorkload
        crInternal rep `shouldBe` []
        crWrong rep `shouldBe` []
        crTimeouts rep `shouldBe` []
        crOK rep `shouldBe` 30

      it "byte corruption: WebSocket frame layer never crashes on malformed input (30 trials)" $ do
        rep <- runCampaign 3000000 (seedsFrom 4000 30) corruptSchedule correct wsWorkload
        crInternal rep `shouldBe` []

      it "byte drop then reset: WebSocket frame layer never crashes (30 trials)" $ do
        rep <- runCampaign 3000000 (seedsFrom 5000 30) dropSchedule correct wsWorkload
        crInternal rep `shouldBe` []
