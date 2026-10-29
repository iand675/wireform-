{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.MultiTaskSpec (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int32)
import Data.Int qualified as Int
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Kafka.Streams.Internal.RecordCollector (
  collectorTake,
  inMemoryCollector,
 )
import Kafka.Streams.Runtime.Task
import Test.Syd


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack


t :: Integer -> Timestamp
t = Timestamp . fromIntegral


ownsParts :: Set Int32 -> (Int32 -> Bool)
ownsParts s p = Set.member p s


tests :: Spec
tests =
  describe "MultiTask" $
    sequence_
      [ multi_task_routes_records_to_owners
      , multi_task_separate_state_per_task
      , multi_task_ignores_records_for_unowned_partition
      ]


-- Build a simple source -> sink topology.
buildPassthroughTopo :: IO TopologyValid
buildPassthroughTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v -> pure v


-- Build a topology that aggregates per key into a state store.
buildCounterTopo :: IO (TopologyValid, StoreName)
buildCounterTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  let g = grouped textSerde textSerde
      kgs = groupByKey g s
  table <-
    countStream
      (materializedAs (storeName "counter"))
      kgs
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v -> pure (v, ktableStore table)


multi_task_routes_records_to_owners :: Spec
multi_task_routes_records_to_owners =
  it "TaskManager routes records to the task owning the partition" $ do
    topo <- buildPassthroughTopo
    tm <- newTaskManager
    -- Two tasks: task 0 owns partitions {0, 1}; task 1 owns {2, 3}.
    coll0 <- inMemoryCollector
    coll1 <- inMemoryCollector
    t0 <-
      newTask
        topo
        (TaskId 0 0)
        "mt-app"
        coll0
        logAndContinue
        (ownsParts (Set.fromList [0, 1]))
    t1 <-
      newTask
        topo
        (TaskId 0 1)
        "mt-app"
        coll1
        logAndContinue
        (ownsParts (Set.fromList [2, 3]))
    addTask tm t0 [(topicName "in", 0), (topicName "in", 1)]
    addTask tm t1 [(topicName "in", 2), (topicName "in", 3)]

    pipeInputAcrossPartitions
      tm
      [ (topicName "in", Just (bytes "k"), bytes "v0", t 0, 0, 0)
      , (topicName "in", Just (bytes "k"), bytes "v1", t 0, 1, 0)
      , (topicName "in", Just (bytes "k"), bytes "v2", t 0, 2, 0)
      , (topicName "in", Just (bytes "k"), bytes "v3", t 0, 3, 0)
      ]

    -- Drain each task's collector independently.
    out0 <- collectorTake coll0 (topicName "out")
    out1 <- collectorTake coll1 (topicName "out")
    map (unbytes . crValueBytes) out0 `shouldBe` ["v0", "v1"]
    map (unbytes . crValueBytes) out1 `shouldBe` ["v2", "v3"]

    closeAllTasks tm


multi_task_separate_state_per_task :: Spec
multi_task_separate_state_per_task =
  it "each task has its own state store" $ do
    (topo, storeNm) <- buildCounterTopo
    tm <- newTaskManager
    coll0 <- inMemoryCollector
    coll1 <- inMemoryCollector
    t0 <-
      newTask
        topo
        (TaskId 0 0)
        "mt-app"
        coll0
        logAndContinue
        (ownsParts (Set.fromList [0]))
    t1 <-
      newTask
        topo
        (TaskId 0 1)
        "mt-app"
        coll1
        logAndContinue
        (ownsParts (Set.fromList [1]))
    addTask tm t0 [(topicName "in", 0)]
    addTask tm t1 [(topicName "in", 1)]

    -- Send the SAME key to both partitions; each task should count
    -- independently.
    pipeInputAcrossPartitions
      tm
      [ (topicName "in", Just (bytes "k"), bytes "x", t 0, 0, 0)
      , (topicName "in", Just (bytes "k"), bytes "y", t 1, 0, 0)
      , (topicName "in", Just (bytes "k"), bytes "z", t 2, 1, 0)
      ]

    -- Read each task's store via interactive queries.
    Just ro0 <- queryEngineStore @Text @Int.Int64 (taskEngine t0) storeNm
    Just ro1 <- queryEngineStore @Text @Int.Int64 (taskEngine t1) storeNm
    ro0.roKvGet "k" >>= (`shouldBe` Just 2)
    ro1.roKvGet "k" >>= (`shouldBe` Just 1)

    closeAllTasks tm


multi_task_ignores_records_for_unowned_partition :: Spec
multi_task_ignores_records_for_unowned_partition =
  it "feedTask drops records on partitions not owned by the task" $ do
    topo <- buildPassthroughTopo
    coll <- inMemoryCollector
    t0 <-
      newTask
        topo
        (TaskId 0 0)
        "mt-app"
        coll
        logAndContinue
        (ownsParts (Set.fromList [0]))
    -- Feed partition 0 (owned) and partition 1 (not owned) directly
    -- through the task — the task should silently drop the second.
    feedTask t0 (topicName "in") (Just (bytes "k")) (bytes "owned") (t 0) 0 0
    feedTask t0 (topicName "in") (Just (bytes "k")) (bytes "stranger") (t 0) 1 0
    out <- collectorTake coll (topicName "out")
    map (unbytes . crValueBytes) out `shouldBe` ["owned"]
    closeTask t0
