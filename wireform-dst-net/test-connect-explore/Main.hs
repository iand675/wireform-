{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | __Exploration campaign against the real Connect stack__ (the
-- @wireform-connect@ client /and/ server) running over the @wireform-dst@
-- fault link. Connect is served over the link's HTTP\/2 __h2c__ transport
-- (server 'runConnectServerOnTransport', client 'withConnectClientOnTransport')
-- — the raw-fn engine seam that @http2-explore@ and @grpc-explore@ already
-- exercise. This deliberately avoids Connect's HTTP\/1 path (which rides the
-- @wireform-http@ duplex-over-link seam that still misbehaves under concurrent
-- bidirectional use). A minimal proto echo service ('Connect.EchoProto') is the
-- workload.
--
-- Connect layers its own envelope framing, unary content-type negotiation, and
-- the @connect.Error@ model on top of HTTP\/2, so this exercises encode\/decode
-- paths neither @http2-explore@ nor @grpc-explore@ touch.
--
-- Fault fairness mirrors @http2-explore@ / @grpc-explore@:
--
--   * __latency__: must always complete with the echoed body (liveness).
--   * __cut__ (peer death mid-call): a clean Connect\/connection error or
--     completion — never a crash, never wrong data.
--   * __partition + heal__: completes once the link heals (liveness).
--   * __corruption \/ drop__ on cleartext h2c: no DATA integrity, so these are
--     __crash-only__ oracles — the Connect stack must never raise an internal
--     exception, though a mangled\/dropped frame may legitimately stall or
--     error the call.
module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Test.Syd

import Network.Connect.Client
  ( ConnectionConfig (..)
  , defaultConnectClientConfig
  , defaultConnectionConfig
  , nonStreaming
  , withConnectClientOnTransport
  )
import Network.Connect.Server
  ( Handlers (..)
  , MethodHandler
  , Service
  , connectHandlers
  , defaultConnectServerConfig
  , method
  , runConnectServerOnTransport
  , service
  )
import Network.Connect.Server qualified as CS (ConnectServerM)
import Network.GRPC.Spec (Proto (..))
import Network.HTTP.Server (ServerConfig, defaultServerConfig)
import Network.HTTP2.Transport (Transport (..))

import Connect.EchoProto
import Sim.Net.Explore
import Sim.Net.Link

-- ---------------------------------------------------------------------------
-- Echo service (one unary method, echoes the request back)
-- ---------------------------------------------------------------------------

echoService :: Service EchoService CS.ConnectServerM
echoService = service (method @Echo echoH :& Done)

echoH :: Proto EchoMessage -> CS.ConnectServerM (Proto EchoMessage)
echoH = pure

echoHandlers :: [MethodHandler]
echoHandlers = connectHandlers echoService

mkEcho :: Text -> Proto EchoMessage
mkEcho t = Proto defaultEchoMessage {echoMessageText = t}

echoedText :: Proto EchoMessage -> Text
echoedText (Proto m) = echoMessageText m

-- ---------------------------------------------------------------------------
-- Connect workload driver (one unary echo over a SimLink, h2c)
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
serverCfg = defaultServerConfig

connCfg :: ConnectionConfig
connCfg = defaultConnectionConfig {connectionHost = "sim"}

connectWorkload :: SimLink -> IO Text
connectWorkload l =
  bracket
    ( forkIO $
        runConnectServerOnTransport
          defaultConnectServerConfig
          serverCfg
          echoHandlers
          (linkTransport (slServer l))
    )
    killThread
    ( \_ ->
        withConnectClientOnTransport
          defaultConnectClientConfig
          connCfg
          (linkTransport (slClient l))
          (\cl -> echoedText <$> nonStreaming cl (Proxy @Echo) (mkEcho "ping"))
    )

correct :: Text -> Bool
correct = (== "ping")

seedsFrom :: Int -> Int -> [Seed]
seedsFrom base n = map (Seed . fromIntegral) [base .. base + n - 1]

-- ---------------------------------------------------------------------------
-- Campaigns
-- ---------------------------------------------------------------------------

main :: IO ()
main =
  sydTest $
    describe "Connect stack — randomized fault exploration over the sim link (h2c)" $ do
      it "latency (0-4ms both dirs): all trials complete with echoed body" $ do
        rep <- runCampaign 5000000 (seedsFrom 1000 40) latencySchedule correct connectWorkload
        crInternal rep `shouldBe` []
        crWrong rep `shouldBe` []
        crTimeouts rep `shouldBe` []
        crOK rep `shouldBe` 40

      it "cut at random moment: no crash, no wrong data (40 trials)" $ do
        rep <- runCampaign 4000000 (seedsFrom 2000 40) (cutSchedule 30000) correct connectWorkload
        crInternal rep `shouldBe` []
        crWrong rep `shouldBe` []

      it "transient partition + heal: liveness preserved (30 trials)" $ do
        rep <- runCampaign 5000000 (seedsFrom 3000 30) partitionHealSchedule correct connectWorkload
        crInternal rep `shouldBe` []
        crWrong rep `shouldBe` []
        crTimeouts rep `shouldBe` []
        crOK rep `shouldBe` 30

      it "byte corruption: Connect stack never crashes on malformed input (30 trials)" $ do
        rep <- runCampaign 3000000 (seedsFrom 4000 30) corruptSchedule correct connectWorkload
        -- crash-only (h2c has no DATA integrity): no internal exceptions
        crInternal rep `shouldBe` []

      it "byte drop then reset: Connect stack never crashes (30 trials)" $ do
        rep <- runCampaign 3000000 (seedsFrom 5000 30) dropSchedule correct connectWorkload
        -- crash-only: silent loss / desync is tolerated, but never an internal
        -- exception.
        crInternal rep `shouldBe` []
