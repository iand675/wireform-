{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | What the streams assignor buys at runtime, on the in-memory broker:

  * a standby keeps a task's state current, so the instance that takes
    the task over restores (almost) nothing, where without one it
    replays the changelog;
  * a task whose balanced owner has no state stays put while a warmup
    restores it there, and moves at a probing rebalance once caught up;
  * a KIP-924 task assignor deals the tasks, and an invalid one falls
    back to the built-in assignor;
  * every instance learns who hosts which partitions and stores
    (@application.server@), and where a key lives.
-}
module Streams.StandbyRuntimeSpec (tests) where

import Control.Exception (finally)
import Data.IORef
import Data.Int (Int64)
import Data.Text (Text)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Kafka.Client.Consumer qualified as KC
import Kafka.Client.Internal.Murmur2 qualified as Murmur2
import Kafka.Streams.Discovery (HostInfo (..), KeyQueryMetadata (..), StreamsMetadata (..))
import Kafka.Streams.Imperative
import Kafka.Streams.InteractiveQueries qualified as IQ
import Kafka.Streams.Metrics (restoredRecordsTotal, standbyRecordsTotal)
import Kafka.Streams.Processor.Assignment qualified as PA
import Kafka.Streams.Runtime.Driver (DriverRecord (..))
import Kafka.Streams.Runtime.GroupAssignor (StreamsAssignment (..))
import Kafka.Streams.Runtime.MockBroker
import Streams.RuntimeHarness
import Test.Syd


tests :: Spec
tests =
  describe "Standbys, warmups and discovery" $ do
    it "a standby takes over warm; without one the changelog is replayed" standby_failover
    it "under exactly-once a standby never applies aborted writes" standby_eos
    it "a warmup restores before its task moves, at a probing rebalance" warmup_then_probe
    it "a KIP-924 assignor deals the tasks; an invalid one falls back" user_assignor
    it "instances learn the hosts of partitions, stores and keys" discovery


actives :: KafkaStreams -> IO Int
actives ks = maybe 0 (Set.size . saActive) <$> currentAssignment ks


-- | Restored by the survivor when the other instance leaves, with @standbys@ replicas.
failoverRestore :: Int -> IO Int64
failoverRestore standbys = withStateDirs 2 $ \[d1, d2] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 2
  topo <- wordCount
  -- uncached: the restore counts below are per update
  let cfg d = (testConfig "sb" d) {numStandbyReplicas = standbys, statestoreCacheMaxBytes = 0}
  a <- startOn broker (cfg d1) topo
  b <- startOn broker (cfg d2) topo
  ( do
      eventually [a, b] "one task each" ((\x y -> x == 1 && y == 1) <$> actives a <*> actives b)
      produceWords broker 30
      eventually [a, b] "counted to 30" (countsReach broker 30)
      if standbys > 0
        then do
          -- a's standby has applied all of b's changelog partition
          Just sa <- currentAssignment b
          let p = (Set.findMin (saActive sa)).taskPartition
          eventually [a, b] "the standby caught up" $ do
            logged <- length . filter ((== p) . (.drPartition)) <$> visible broker "sb-counts-changelog"
            (>= fromIntegral logged) <$> counter a standbyRecordsTotal
          -- and says so: the standby's lag is 0, a running task's always is
          lags <- either (error . show) id <$> localTaskLags a
          map (\l -> (l.lagStandby, l.lagEndOffset - l.lagCurrentOffset)) lags `shouldMatchList` [(False, 0), (True, 0)]
          -- stale queries read the standby's copy; others don't see it
          [k] <- pure (take 1 (filter (\key -> Murmur2.partitionForKey (textBytes key) 2 == p) keys))
          let params = (IQ.storeQueryParameters (storeName "counts")) {IQ.partition = Just (fromIntegral p)}
          Just stale <- IQ.queryKVStoreWithParameters @Text @Int64 a params {IQ.staleStoresEnabled = True}
          stale.roKvGet k `shouldReturn` Just 30
          fresh <- IQ.queryKVStoreWithParameters @Text @Int64 a params
          maybe (pure Nothing) (\s -> s.roKvGet k) fresh `shouldReturn` Nothing
        else counter a standbyRecordsTotal >>= (`shouldBe` 0)
      before <- counter a restoredRecordsTotal
      closeKafkaStreams b >> awaitState b StreamsClosed
      eventually [a] "a runs both tasks" ((== 2) <$> actives a)
      produceWords broker 10
      eventually [a] "counts carry on" (countsReach broker 40)
      subtract before <$> counter a restoredRecordsTotal
    )
    `finally` closeAll [a, b]


standby_failover :: IO ()
standby_failover = do
  warm <- failoverRestore 1
  cold <- failoverRestore 0
  -- about 150 changelog records per partition (10 keys x 30 rounds over 2)
  warm `shouldSatisfy` (< 5)
  cold `shouldSatisfy` (> 100)


standby_eos :: IO ()
standby_eos = withStateDirs 2 $ \[d1, d2] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 2
  topo <- wordCount
  let cfg d = (testConfig "sbe" d) {numStandbyReplicas = 1, processingGuarantee = ExactlyOnceV2, statestoreCacheMaxBytes = 0}
  a <- startOn broker (cfg d1) topo
  b <- startOn broker (cfg d2) topo
  ( do
      eventually [a, b] "one task each" ((\x y -> x == 1 && y == 1) <$> actives a <*> actives b)
      produceWords broker 5
      eventually [a, b] "counted to 5" (countsReach broker 5)
      -- aborted transactions put changelog writes in the log that a
      -- read_committed standby must skip
      mockFailTransactionCommits broker 3
      produceWords broker 3
      eventually [a, b] "counted to 8" (countsReach broker 8)
      Just sa <- currentAssignment b
      let p = (Set.findMin (saActive sa)).taskPartition
      eventually [a, b] "the standby read b's changelog to its end" $ do
        everything <- filter ((== p) . (.drPartition)) <$> mockRecords broker "sbe-counts-changelog"
        committed <- filter ((== p) . (.drPartition)) <$> visible broker "sbe-counts-changelog"
        applied <- counter a standbyRecordsTotal
        pure (length everything > length committed && applied >= fromIntegral (length committed))
      closeKafkaStreams b >> awaitState b StreamsClosed
      eventually [a] "a runs both tasks" ((== 2) <$> actives a)
      produceWords broker 2
      eventually [a] "counted to 10" (countsReach broker 10)
      -- every key counted 1..10 exactly once: nothing aborted came back
      outs <- visible broker "out"
      mapM_
        ( \k ->
            map (fmap decodeI64 . (.drValue)) (filter ((== Just (textBytes k)) . (.drKey)) outs)
              `shouldBe` map Just [1 .. 10]
        )
        keys
    )
    `finally` closeAll [a, b]


warmup_then_probe :: IO ()
warmup_then_probe = withStateDirs 2 $ \[d1, d2] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 2
  topo <- wordCount
  let cfg d = (testConfig "wu" d) {acceptableRecoveryLag = 5, probingRebalanceIntervalMs = 100, statestoreCacheMaxBytes = 0}
  a <- startOn broker (cfg d1) topo
  ( do
      produceWords broker 30
      eventually [a] "a counted to 30" (countsReach broker 30)
      b <- startOn broker (cfg d2) topo
      ( do
          -- b has no state: a keeps both tasks, b warms one up, and the
          -- probing rebalance hands it over once b is caught up
          eventually [a, b] "b runs a task" ((== 1) <$> actives b)
          counter b restoredRecordsTotal >>= (`shouldSatisfy` (< 10))
          counter b standbyRecordsTotal >>= (`shouldSatisfy` (> 100))
          produceWords broker 5
          eventually [a, b] "counted to 35" (countsReach broker 35)
        )
        `finally` closeAll [b]
    )
    `finally` closeAll [a]


user_assignor :: IO ()
user_assignor = do
  -- every task on the lowest process id
  outcomes <- newIORef []
  let onComputed _ e = atomicModifyIORef' outcomes (\es -> (e : es, ()))
      allToOne =
        PA.TaskAssignor
          { PA.taAssign = \app -> do
              let pids = Map.keys (PA.asKafkaStreamsStates app)
                  allTasks = Map.keysSet (PA.asAllTasks app)
              pure
                PA.TaskAssignment
                  { PA.taAssignments =
                      Map.fromList
                        ( zipWith
                            (\i pid -> (pid, PA.KafkaStreamsAssignment (if i == (0 :: Int) then allTasks else Set.empty) Set.empty Nothing))
                            [0 ..]
                            pids
                        )
                  }
          , PA.taOnAssignmentComputed = onComputed
          , PA.taConfigure = \_ -> pure ()
          }
      -- loses every task
      dropsAll = allToOne {PA.taAssign = \app -> pure (PA.TaskAssignment (Map.map (const (PA.KafkaStreamsAssignment Set.empty Set.empty Nothing)) (PA.asKafkaStreamsStates app)))}
      run app assignor = withStateDirs 2 $ \[d1, d2] -> do
        broker <- newMockBroker (Just 1)
        mockCreateTopic broker "in" 4
        topo <- wordCount
        let cfg d = (testConfig app d) {taskAssignor = Just assignor}
        a <- startOn broker (cfg d1) topo
        b <- startOn broker (cfg d2) topo
        ( do
            eventually [a, b] "all four tasks dealt" ((== 4) <$> ((+) <$> actives a <*> actives b))
            produceWords broker 3
            eventually [a, b] "counted to 3" (countsReach broker 3)
            sort <$> mapM actives [a, b]
          )
          `finally` closeAll [a, b]
  run "ua-good" allToOne `shouldReturn` [0, 4]
  readIORef outcomes >>= (`shouldSatisfy` all (== PA.AssignmentErrorNone))
  writeIORef outcomes []
  run "ua-bad" dropsAll `shouldReturn` [2, 2]
  readIORef outcomes >>= (`shouldSatisfy` elem PA.MissingActiveTask)


discovery :: IO ()
discovery = withStateDirs 2 $ \[d1, d2] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 2
  topo <- wordCount
  let hostA = HostInfo "a.local" 7001
      hostB = HostInfo "b.local" 7002
      cfg d h = (testConfig "dq" d) {applicationServer = Just h, numStandbyReplicas = 1}
  a <- startOn broker (cfg d1 hostA) topo
  b <- startOn broker (cfg d2 hostB) topo
  ( do
      eventually [a, b] "one task each" ((\x y -> x == 1 && y == 1) <$> actives a <*> actives b)
      -- both instances see the same picture
      metaA <- streamsMetadata a
      metaB <- streamsMetadata b
      metaA `shouldBe` metaB
      map (.host) metaA `shouldBe` [hostA, hostB]
      Set.unions (map (.topicPartitions) metaA) `shouldBe` Set.fromList [KC.TopicPartition "in" 0, KC.TopicPartition "in" 1]
      mapM_ (\m -> m.stateStores `shouldBe` Set.singleton "counts") metaA
      -- with one standby each, each instance is the other's standby
      mapM_ (\m -> Set.size m.standbyPartitions `shouldBe` 1) metaA
      Just km <- queryMetadataForKey a (storeName "counts") (textBytes "k3")
      let p = Murmur2.partitionForKey (textBytes "k3") 2
      km.partition `shouldBe` p
      [owner] <- pure (filter (Set.member (KC.TopicPartition "in" p) . (.topicPartitions)) metaA)
      km.activeHost `shouldBe` owner.host
      km.standbyHosts `shouldBe` filter (/= owner.host) [hostA, hostB]
      queryMetadataForKey b (storeName "counts") (textBytes "k3") `shouldReturn` Just km
      queryMetadataForKey a (storeName "no-such-store") (textBytes "k3") `shouldReturn` Nothing
    )
    `finally` closeAll [a, b]
