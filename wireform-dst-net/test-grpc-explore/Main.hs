{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | __Exploration campaign against the real gRPC stack__ (the @wireform-grpc@
-- grapesy fork — client /and/ server) running over the @wireform-dst@ fault
-- link. The gRPC engine 'Config' is built directly from the link's byte
-- primitives (@leSendFn@ + the exactly-N @leReadExactN@), with no socket: the
-- client via 'withConnectionVia' and the server via 'runServerOverConfig'. A
-- codegen-free 'RawRpc' echo (raw @ByteString@ in/out, no protobuf) is the
-- workload.
--
-- This is the gRPC analogue of @http2-explore@. Its extra value: gRPC is a
-- whole session layer (HPACK, stream lifecycle, the gRPC framing + status
-- trailers) /on top of/ the HTTP\/2 engine, so it exercises paths the raw
-- @http2-explore@ never touches. Note in particular that the DATA-before-HEADERS
-- liveness fix landed in @Network.HTTP2.Client@ does __not__ cover gRPC (grapesy
-- drives the engine through its own session, not @Network.HTTP2.Client@) — so
-- the drop\/corrupt campaigns here are the first fault coverage of that path.
--
-- Fault fairness mirrors @http2-explore@:
--
--   * __latency__: must always complete with the echoed body (liveness).
--   * __cut__ (peer death mid-call): a clean gRPC\/connection error or
--     completion — never a crash, never wrong data.
--   * __partition + heal__: completes once the link heals (liveness).
--   * __corruption \/ drop__ on cleartext h2c: no DATA integrity, so these are
--     __crash-only__ oracles — the gRPC stack must never raise an internal
--     exception (ErrorCall \/ pattern-match \/ arithmetic), even though a
--     mangled\/dropped frame may legitimately stall or error the call.
module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Data.ByteString.Lazy (ByteString)
import Data.Default (def)
import Test.Syd

import Network.GRPC.Client (rpc, withConnectionVia)
import Network.GRPC.Client.StreamType.IO (nonStreaming)
import Network.GRPC.Common
  ( NoMetadata (..)
  , RequestMetadata
  , ResponseInitialMetadata
  , ResponseTrailingMetadata
  )
import Network.GRPC.Server
  ( SomeRpcHandler
  , mkGrpcServer
  , mkRpcHandler
  , recvFinalInput
  , sendFinalOutput
  , someRpcHandler
  )
import Network.GRPC.Server.Run (runServerOverConfig)
import Network.GRPC.Spec (RawRpc)
import Network.HTTP2.Engine.Client (allocConfigForTransport)

import Sim.Net.Explore
import Sim.Net.Link

-- ---------------------------------------------------------------------------
-- A codegen-free unary echo RPC: /sim/echo, raw ByteString in and out
-- ---------------------------------------------------------------------------

type Echo = RawRpc "sim" "echo"

-- RawRpc ships no default metadata, so bind all three to 'NoMetadata'.
type instance RequestMetadata Echo = NoMetadata
type instance ResponseInitialMetadata Echo = NoMetadata
type instance ResponseTrailingMetadata Echo = NoMetadata

echoHandler :: SomeRpcHandler IO
echoHandler =
  someRpcHandler $
    mkRpcHandler @Echo $ \call -> do
      inp <- recvFinalInput call
      sendFinalOutput call (inp, NoMetadata)

-- ---------------------------------------------------------------------------
-- gRPC workload driver (one unary echo over a SimLink)
-- ---------------------------------------------------------------------------

-- Fork the real gRPC server + issue one unary call over the fault link, at the
-- engine-'Config' seam (no socket).
grpcWorkload :: SimLink -> IO ByteString
grpcWorkload l = do
  server <- mkGrpcServer def [echoHandler]
  bracket
    ( forkIO $
        runServerOverConfig
          def
          server
          (leSendFn (slServer l))
          (leReadExactN (slServer l))
    )
    killThread
    ( \_ ->
        withConnectionVia
          def
          "sim"
          (allocConfigForTransport (leSendFn (slClient l)) (leReadExactN (slClient l)))
          (\conn -> nonStreaming conn (rpc @Echo) "ping")
    )

correct :: ByteString -> Bool
correct = (== "ping")

seedsFrom :: Int -> Int -> [Seed]
seedsFrom base n = map (Seed . fromIntegral) [base .. base + n - 1]

-- ---------------------------------------------------------------------------
-- Campaigns
-- ---------------------------------------------------------------------------

main :: IO ()
main =
  sydTest $
    describe "gRPC stack — randomized fault exploration over the sim link" $ do
      it "latency (0-4ms both dirs): all trials complete with echoed body" $ do
        rep <- runCampaign 5000000 (seedsFrom 1000 40) latencySchedule correct grpcWorkload
        crInternal rep `shouldBe` []
        crWrong rep `shouldBe` []
        crTimeouts rep `shouldBe` []
        crOK rep `shouldBe` 40

      it "cut at random moment: no crash, no wrong data (40 trials)" $ do
        rep <- runCampaign 4000000 (seedsFrom 2000 40) (cutSchedule 30000) correct grpcWorkload
        crInternal rep `shouldBe` []
        crWrong rep `shouldBe` []

      it "transient partition + heal: liveness preserved (30 trials)" $ do
        rep <- runCampaign 5000000 (seedsFrom 3000 30) partitionHealSchedule correct grpcWorkload
        crInternal rep `shouldBe` []
        crWrong rep `shouldBe` []
        crTimeouts rep `shouldBe` []
        crOK rep `shouldBe` 30

      it "byte corruption: gRPC stack never crashes on malformed input (30 trials)" $ do
        rep <- runCampaign 3000000 (seedsFrom 4000 30) corruptSchedule correct grpcWorkload
        -- crash-only (h2c has no DATA integrity): no internal exceptions
        crInternal rep `shouldBe` []

      it "byte drop then reset: gRPC stack never crashes (30 trials)" $ do
        rep <- runCampaign 3000000 (seedsFrom 5000 30) dropSchedule correct grpcWorkload
        -- crash-only: silent loss / desync is tolerated, but never an internal
        -- exception. (Liveness is not asserted: unlike Network.HTTP2.Client,
        -- grapesy's session layer has no DATA-before-HEADERS guard, so a hang
        -- under dropped HEADERS would be a genuine finding, not a test bug.)
        crInternal rep `shouldBe` []
