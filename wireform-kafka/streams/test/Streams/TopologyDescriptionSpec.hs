{-# LANGUAGE OverloadedStrings #-}

module Streams.TopologyDescriptionSpec (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Test.Syd


tests :: Spec
tests =
  describe "TopologyDescription" $
    sequence_
      [ describe_simple_passthrough
      , describe_lists_stores
      , pretty_includes_arrows_and_topics
      , two_sub_topologies_via_through
      , through_topic_round_trips_in_driver
      ]


simpleTopo :: IO Topology
simpleTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  s' <- mapValues T.toUpper s
  toTopic (topicName "out") (produced textSerde textSerde) s'
  buildTopology b


describe_simple_passthrough :: Spec
describe_simple_passthrough =
  it "describeTopology returns one sub-topology with the right node count" $ do
    topo <- simpleTopo
    let td = describeTopology topo
    length (tdSubtopologies td) `shouldBe` 1
    case tdSubtopologies td of
      [st] -> length (stNodes st) `shouldBe` 3 -- source + map + sink
      _ -> error "expected exactly one sub-topology"


describe_lists_stores :: Spec
describe_lists_stores =
  it "tdStores enumerates every declared state store" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    let g = grouped textSerde textSerde
        kg = groupByKey g src
    _ <- countStream (materializedAs (storeName "my-counter")) kg
    topo <- buildTopology b
    let td = describeTopology topo
    map unStoreName (tdStores td) `shouldBe` ["my-counter"]


pretty_includes_arrows_and_topics :: Spec
pretty_includes_arrows_and_topics =
  it "pretty rendering includes node arrows + topic names" $ do
    topo <- simpleTopo
    let txt = pretty (describeTopology topo)
    -- Spot-check a handful of expected substrings.
    mapM_
      (\needle -> (if (needle `T.isInfixOf` txt) then pure () else expectationFailure (T.unpack needle <> " missing in:\n" <> T.unpack txt)))
      [ "Topologies:"
      , "Sub-topology: 0"
      , "Source:"
      , "topics: [in]"
      , "Sink:"
      , "topic: out"
      , "-->"
      , "<--"
      ]


----------------------------------------------------------------------
-- Sub-topology splitting + driver auto-feedback
----------------------------------------------------------------------

twoSubTopologyTopo :: IO Topology
twoSubTopologyTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  s' <- mapValues T.toUpper s
  s'' <- throughTopic (topicName "internal-loop") (produced textSerde textSerde) s'
  toTopic (topicName "out") (produced textSerde textSerde) s''
  buildTopology b


two_sub_topologies_via_through :: Spec
two_sub_topologies_via_through =
  it "throughTopic boundary splits the topology into two sub-topologies" $ do
    topo <- twoSubTopologyTopo
    let td = describeTopology topo
    length (tdSubtopologies td) `shouldBe` 2


through_topic_round_trips_in_driver :: Spec
through_topic_round_trips_in_driver =
  it "driver auto-feedback delivers records across the through-topic loop" $ do
    topo <- twoSubTopologyTopo
    driver <- newDriver topo "tt-app"
    pipeInput driver (topicName "in") Nothing (BSC.pack "hello") (Timestamp 0) 0
    pipeInput driver (topicName "in") Nothing (BSC.pack "world") (Timestamp 0) 0
    out <- readOutput driver (topicName "out")
    map crValueBytes out `shouldBe` [BSC.pack "HELLO", BSC.pack "WORLD"]
    closeDriver driver
