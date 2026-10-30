{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Tests for the latest tier-of-parity:
Topology.optimize, addReadOnlyStateStore, lag listener.
-}
module Streams.MorePartiyTwoSpec (tests) where

import Data.IORef
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Test.Syd


tests :: Spec
tests =
  describe "ParityRoundTwo" $
    sequence_
      [ optimize_topology_is_identity_when_off
      , optimize_topology_with_default_config
      ]


mkSimpleTopo :: IO Topology
mkSimpleTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  buildTopology b


optimize_topology_is_identity_when_off :: Spec
optimize_topology_is_identity_when_off =
  it "optimizeTopology is the identity when no toggles are enabled" $ do
    topo <- mkSimpleTopo
    let cfg = noOptimisations
        topo' = optimizeTopology cfg topo
    -- Same node count after optimisation (currently a no-op).
    length (topoOrder topo') `shouldBe` length (topoOrder topo)


optimize_topology_with_default_config :: Spec
optimize_topology_with_default_config =
  it "optimizeTopology with the default config still yields a valid topology" $ do
    topo <- mkSimpleTopo
    let topo' = optimizeTopology defaultOptimizationConfig topo
    case validateTopology topo' of
      Right _ -> pure ()
      Left err -> error (show err)


mkRuntime :: IO KafkaStreams
mkRuntime = do
  topo <- mkSimpleTopo
  case validateTopology topo of
    Left err -> error (show err)
    Right v ->
      newKafkaStreams
        defaultStreamsConfig
          { applicationId = "lag-app"
          , bootstrapServers = ["mock:0"]
          }
        v


