{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Standby-task replay scaffolding.

Verifies that 'standbyReplay' faithfully folds a changelog
batch into the local state store, reports the resulting
lag, and that the runtime's warmup-lag map picks it up so
the KIP-441 probing rebalance can promote the standby.
-}
module Streams.StandbyTaskSpec (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime.StandbyTask
import Kafka.Streams.State.KeyValue.InMemory qualified as KVInMem
import Test.Syd


tests :: Spec
tests =
  describe "Standby task replay" $
    sequence_
      [ replay_applies_to_store
      , replay_reports_lag
      , manager_round_trip
      , replay_feeds_warmup_lag_map
      ]


----------------------------------------------------------------------
-- 1. Replay applies records to the underlying KV store
----------------------------------------------------------------------

replay_applies_to_store :: Spec
replay_applies_to_store =
  it "standbyReplay: each ChangelogRecord lands in the store" $ do
    st <-
      newStandbyTask
        (TaskId 0 0)
        "changelog"
        0
        (storeName "store-1")
    kvs <-
      KVInMem.inMemoryKeyValueStore @BSC.ByteString @(Maybe BSC.ByteString)
        (storeName "store-1")
    _ <-
      standbyReplay
        st
        kvs
        [ ChangelogRecord 0 "k1" (Just "v1") 3
        , ChangelogRecord 1 "k2" (Just "v2") 3
        , ChangelogRecord 2 "k1" Nothing 3
        -- tombstone for k1
        ]
    kvsGet kvs "k1" >>= (`shouldBe` Just Nothing) -- tombstoned
    kvsGet kvs "k2" >>= (`shouldBe` Just (Just "v2"))


----------------------------------------------------------------------
-- 2. Replay's lag computation
----------------------------------------------------------------------

replay_reports_lag :: Spec
replay_reports_lag =
  it "standbyReplay: returned lag is end-offset minus next-replay" $ do
    st <-
      newStandbyTask
        (TaskId 0 0)
        "cl"
        0
        (storeName "lag-store")
    kvs <-
      KVInMem.inMemoryKeyValueStore @BSC.ByteString @(Maybe BSC.ByteString)
        (storeName "lag-store")
    -- 2 records consumed, end-of-log = 5. Lag should be 5 - 2 = 3.
    lag <-
      standbyReplay
        st
        kvs
        [ ChangelogRecord 0 "k" (Just "v") 5
        , ChangelogRecord 1 "k" (Just "v") 5
        ]
    lag `shouldBe` 3
    -- Empty replay: lag stays the same.
    lag2 <- standbyReplay st kvs []
    lag2 `shouldBe` 3


----------------------------------------------------------------------
-- 3. Manager add / list / remove
----------------------------------------------------------------------

manager_round_trip :: Spec
manager_round_trip =
  it "StandbyManager: add/list/remove round-trip" $ do
    mgr <- newStandbyManager
    s1 <- newStandbyTask (TaskId 0 0) "cl-a" 0 (storeName "store-a")
    s2 <- newStandbyTask (TaskId 0 1) "cl-b" 0 (storeName "store-b")
    addStandbyTask mgr s1
    addStandbyTask mgr s2
    listStandbyTasks mgr >>= \xs ->
      length xs `shouldBe` 2
    removeStandbyTask mgr (TaskId 0 0) "cl-a" 0
    listStandbyTasks mgr >>= \xs ->
      length xs `shouldBe` 1


----------------------------------------------------------------------
-- 4. End-to-end: replay reports lag through reportWarmupLag
--    so the runtime's warmupSnapshot picks it up.
----------------------------------------------------------------------

replay_feeds_warmup_lag_map :: Spec
replay_feeds_warmup_lag_map =
  it "standbyReplay drives reportWarmupLag end-to-end" $ do
    b <- newStreamsBuilder
    _ <-
      streamFromTopic
        b
        (topicName "in")
        (consumed textSerde textSerde)
    topo <- buildTopology b
    let topo' = case validateTopology topo of
          Left e -> error (show e)
          Right v -> v
    ks <-
      newKafkaStreams
        ( defaultStreamsConfig
            { applicationId = "standby-test"
            , bootstrapServers = ["mock:0"]
            , numStreamThreads = 1
            , pollMs = 0
            }
        )
        topo'

    let tid = TaskId 0 0
    st <- newStandbyTask tid "cl" 0 (storeName "standby-store")

    kvs <-
      KVInMem.inMemoryKeyValueStore @BSC.ByteString @(Maybe BSC.ByteString)
        (storeName "standby-store")

    -- Replay 3 records with end-of-log at 10. Lag = 10 - 3 = 7.
    lag <-
      standbyReplay
        st
        kvs
        [ ChangelogRecord 0 "a" (Just "1") 10
        , ChangelogRecord 1 "b" (Just "2") 10
        , ChangelogRecord 2 "c" (Just "3") 10
        ]
    lag `shouldBe` 7

    -- Plumb it into the warmup-lag map.
    reportWarmupLag ks tid lag
    snap <- warmupSnapshot ks
    Map.lookup tid snap `shouldBe` Just 7

    -- After catching up: lag = 0 -> the probing-rebalance
    -- machinery would now promote.
    lag2 <-
      standbyReplay
        st
        kvs
        [ ChangelogRecord 3 "d" (Just "4") 4
        , ChangelogRecord 4 "e" (Just "5") 5
        ]
    -- End-offset moved from 10 down to 5 -> max(end - next) -> 0.
    (if (lag2 == 0) then pure () else expectationFailure ("expected lag 0; got " <> show lag2))
    reportWarmupLag ks tid lag2
    snap2 <- warmupSnapshot ks
    Map.lookup tid snap2 `shouldBe` Just 0
