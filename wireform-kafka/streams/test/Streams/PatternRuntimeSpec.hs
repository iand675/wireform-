{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Pattern sources at runtime, on the in-memory broker: the pattern
is resolved against the cluster's topics when the instance starts, and
again every @metadata.max.age.ms@; when what it matches changes, the
instance re-plans in place and its state carries over.
-}
module Streams.PatternRuntimeSpec (tests) where

import Control.Exception (finally)
import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Kafka.Streams.Imperative
import Kafka.Streams.Runtime.MockBroker
import Kafka.Streams.Runtime.Plan (resolvePatternTopics)
import Kafka.Streams.Topology qualified as Topo
import Streams.RuntimeHarness
import Test.Syd


tests :: Spec
tests =
  describe "Pattern sources (runtime)" $ do
    it "matches whole names, leaving out Kafka's and the app's own topics" resolution
    it "reads every matching topic and nothing else" reads_matching
    it "picks up a new matching topic, keeping its state" new_topic_replans
    it "starts with nothing matched, and runs once a topic appears (two instances)" starts_empty


-- | Counts per key over every topic matching @tenant-.*@, to @out@.
tenantCount :: IO TopologyValid
tenantCount = do
  b <- newStreamsBuilder
  s <- streamFromPattern b "tenant-.*" (consumed textSerde textSerde)
  counts <- countStream (materializedAs (storeName "counts")) (groupByKey (grouped textSerde textSerde) s)
  out <- toKStreamFromKTable counts
  toTopic (topicName "out") (produced textSerde int64Serde) out
  buildTopology b >>= either (error . show) pure . validateTopology


produceTo :: MockBroker -> Text -> Int -> IO ()
produceTo broker topic rounds =
  forM_ [1 .. rounds] $ \r ->
    forM_ keys $ \k -> mockProduce broker topic (Just (textBytes k)) (Just "x") (fromIntegral r)


cfgFor :: Text -> FilePath -> StreamsConfig
cfgFor app d = (testConfig app d) {metadataMaxAgeMs = 20}


resolution :: IO ()
resolution = do
  topo <- Topo.topologyValidGraph <$> tenantCount
  let [src] = Map.keys (Topo.topoSources topo)
      topics = ["tenant-a", "tenant-b", "xtenant-c", "tenant", "__consumer_offsets", "app-tenant-x", "other"]
  resolvePatternTopics "app" topo topics `shouldBe` Right (Map.singleton src ["tenant-a", "tenant-b"])
  -- a topic also read by name would reach two sources
  b <- newStreamsBuilder
  _ <- streamFromPattern b "t-.*" (consumed textSerde textSerde)
  _ <- streamFromTopic b (topicName "t-1") (consumed textSerde textSerde)
  both <- buildTopology b
  resolvePatternTopics "app" both ["t-1", "t-2"] `shouldSatisfy` isLeft
  -- so would one matched by two patterns
  b2 <- newStreamsBuilder
  _ <- streamFromPattern b2 "t-.*" (consumed textSerde textSerde)
  _ <- streamFromPattern b2 ".*-1" (consumed textSerde textSerde)
  twice <- buildTopology b2
  resolvePatternTopics "app" twice ["t-1"] `shouldSatisfy` isLeft
  resolvePatternTopics "app" twice ["t-2", "u-1"] `shouldSatisfy` either (const False) ((== [["t-2"], ["u-1"]]) . Map.elems)


reads_matching :: IO ()
reads_matching = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mapM_ (\t -> mockCreateTopic broker t 2) ["tenant-a", "tenant-b", "other"]
  topo <- tenantCount
  a <- startOn broker (cfgFor "pat" d) topo
  ( do
      produceTo broker "tenant-a" 2
      produceTo broker "other" 5
      produceTo broker "tenant-b" 1
      eventually [a] "counted to 3" (countsReach broker 3)
      _ <- awaitTicks a 30
      -- nothing from "other"
      countsReach broker 3 `shouldReturn` True
    )
    `finally` closeAll [a]


new_topic_replans :: IO ()
new_topic_replans = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "tenant-a" 2
  topo <- tenantCount
  a <- startOn broker (cfgFor "patnew" d) topo
  ( do
      produceTo broker "tenant-a" 3
      eventually [a] "counted to 3" (countsReach broker 3)
      mockCreateTopic broker "tenant-b" 2
      mockCreateTopic broker "xtenant-b" 2
      produceTo broker "tenant-b" 2
      produceTo broker "xtenant-b" 4
      -- the counts carry on from 3: the state survived the re-plan
      eventually [a] "counted to 5" (countsReach broker 5)
      produceTo broker "tenant-a" 1
      eventually [a] "counted to 6" (countsReach broker 6)
      _ <- awaitTicks a 30
      countsReach broker 6 `shouldReturn` True
    )
    `finally` closeAll [a]


starts_empty :: IO ()
starts_empty = withStateDirs 2 $ \[d1, d2] -> do
  broker <- newMockBroker (Just 1)
  topo <- tenantCount
  a <- startOn broker (cfgFor "patempty" d1) topo
  b <- startOn broker (cfgFor "patempty" d2) topo
  ( do
      _ <- awaitTicks a 20
      -- nothing to do yet, and no error for it
      mapM streamsStatus [a, b] >>= (`shouldSatisfy` all (`notElem` [StreamsClosed, StreamsClosing]))
      mapM streamsStatus [a, b] >>= (`shouldSatisfy` all (\st -> case st of StreamsError _ -> False; _ -> True))
      mockCreateTopic broker "tenant-z" 4
      produceTo broker "tenant-z" 2
      eventually [a, b] "counted to 2" (countsReach broker 2)
      eventually [a, b] "both run tasks" $ do
        owned <- mapM ownedPartitions [a, b]
        pure (all (not . null) owned)
    )
    `finally` closeAll [a, b]
