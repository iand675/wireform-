{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.WorkerPoolSpec (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Int qualified as Int
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Kafka.Streams.Imperative
import Kafka.Streams.Internal.RecordCollector (collectorTake)
import Kafka.Streams.Runtime.WorkerPool
import Test.Syd


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack


t :: Integer -> Timestamp
t = Timestamp . fromIntegral


owned :: [(Text, Int)] -> HashSet (TopicName, Int.Int32)
owned = HashSet.fromList . map (\(tp, p) -> (topicName tp, fromIntegral p))


tests :: Spec
tests =
  describe "WorkerPool" $
    sequence_
      [ pool_routes_to_owner
      , pool_per_worker_state_isolation
      , pool_count_processed
      ]


passthroughTopo :: IO TopologyValid
passthroughTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v -> pure v


pool_routes_to_owner :: Spec
pool_routes_to_owner =
  it "submitRecord delivers to the worker that owns the partition" $ do
    topo <- passthroughTopo
    let perWorker =
          [ owned [("in", 0), ("in", 1)]
          , owned [("in", 2), ("in", 3)]
          ]
    pool <- newWorkerPool topo "wp-app" perWorker

    submitRecord pool (topicName "in") (Just (bytes "k")) (bytes "p0") (t 0) 0
    submitRecord pool (topicName "in") (Just (bytes "k")) (bytes "p1") (t 0) 1
    submitRecord pool (topicName "in") (Just (bytes "k")) (bytes "p2") (t 0) 2
    submitRecord pool (topicName "in") (Just (bytes "k")) (bytes "p3") (t 0) 3

    waitForQuiescence pool
    -- Allow a short interval for engine processing to settle.

    case V.toList (poolWorkers pool) of
      [w0, w1] -> do
        out0 <- collectorTake (workerCollector w0) (topicName "out")
        out1 <- collectorTake (workerCollector w1) (topicName "out")
        Set.fromList (map (unbytes . crValueBytes) out0)
          `shouldBe` Set.fromList ["p0", "p1"]
        Set.fromList (map (unbytes . crValueBytes) out1)
          `shouldBe` Set.fromList ["p2", "p3"]
      _ -> error "expected 2 workers"
    closeWorkerPool pool


pool_per_worker_state_isolation :: Spec
pool_per_worker_state_isolation =
  it "each worker has its own engine state" $ do
    topo <- passthroughTopo
    let perWorker =
          [ owned [("in", 0)]
          , owned [("in", 1)]
          ]
    pool <- newWorkerPool topo "wp-app" perWorker

    -- Send 3 records to partition 0 and 1 record to partition 1.
    submitRecord pool (topicName "in") Nothing (bytes "a") (t 0) 0
    submitRecord pool (topicName "in") Nothing (bytes "b") (t 1) 0
    submitRecord pool (topicName "in") Nothing (bytes "c") (t 2) 0
    submitRecord pool (topicName "in") Nothing (bytes "d") (t 3) 1

    waitForQuiescence pool

    case V.toList (poolWorkers pool) of
      [w0, w1] -> do
        c0 <- workerProcessedCount w0
        c1 <- workerProcessedCount w1
        c0 `shouldBe` 3
        c1 `shouldBe` 1
      _ -> error "expected 2 workers"
    closeWorkerPool pool


pool_count_processed :: Spec
pool_count_processed =
  it "workerProcessedCount tracks records consumed" $ do
    topo <- passthroughTopo
    pool <- newWorkerPool topo "wp-app" [owned [("in", 0)]]
    mapM_
      (\v -> submitRecord pool (topicName "in") Nothing (bytes v) (t 0) 0)
      ["v1", "v2", "v3", "v4", "v5"]
    waitForQuiescence pool
    case V.toList (poolWorkers pool) of
      [w] -> workerProcessedCount w >>= (`shouldBe` 5)
      _ -> error "expected 1 worker"
    closeWorkerPool pool
