{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.Mock.StreamsDriver
Description : Streams engine wired against a 'MockCluster'

'MockStreamsDriver' is the higher-level integration: a streams
'Engine' fed by a 'MockConsumer' and emitting via a
'MockProducer'. Tests can exercise the full lifecycle (poll →
engine.feedSource → forward → mock-producer.append → consumer
offset commit) including failure modes that 'TopologyTestDriver'
can't reach because it bypasses the producer/consumer halves.

The driver is /step-driven/: tests call 'tickDriver' to run one
poll/process/commit cycle. This keeps assertions deterministic
without 'threadDelay'.
-}
module Kafka.Streams.Mock.StreamsDriver (
  MockStreamsDriver,
  DriverMode (..),
  newMockStreamsDriver,
  newMockStreamsDriverEOS,
  driverCluster,
  driverProducer,
  driverConsumer,
  driverEngine,
  driverPartitions,
  tickDriver,
  runUntilQuiet,
  closeMockDriver,

  -- * Convenience producers (from outside the topology)
  externalSend,
  externalSendH,
) where

import Control.Concurrent.STM
import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified
import Data.IORef
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Kafka.Headers qualified as KH
import Kafka.Streams.Errors (logAndContinue)
import Kafka.Streams.Internal.Engine (
  Engine,
  buildEngine,
  closeEngine,
  commitEngine,
  engineCollector,
  feedSourceH,
  setPartitionCountLookup,
  takeCommitRequested,
 )
import Kafka.Streams.Internal.RecordCollector (
  CollectedRecord (..),
  collectorTake,
  inMemoryCollector,
 )
import Kafka.Streams.Mock.Cluster (
  GroupId (..),
  MockCluster,
  StoredRecord (..),
  TxnId (..),
  appendToPartition,
  createTopic,
 )
import Kafka.Streams.Mock.Consumer (
  IsolationLevel (..),
  MockConsumer,
  PollResult (..),
  commitOffsetsMC,
  currentPosition,
  newMockConsumer,
  pollMC,
  subscribeMC,
 )
import Kafka.Streams.Mock.Consumer qualified
import Kafka.Streams.Mock.Fault (FaultPolicy)
import Kafka.Streams.Mock.Fault qualified as F
import Kafka.Streams.Mock.Producer (
  MockProduceResult (..),
  MockProducer,
  abortTxnMP,
  beginTxnMP,
  commitTxnMP,
  flushMock,
  newMockProducer,
  sendMockH,
 )
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Time (Timestamp (..))
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Types (TopicName, topicName, unTopicName)


----------------------------------------------------------------------
-- Driver
----------------------------------------------------------------------

-- | At-least-once vs exactly-once-V2 wiring.
data DriverMode
  = AtLeastOnceMode
  | ExactlyOnceMode !TxnId
  deriving (Eq, Show)


data MockStreamsDriver = MockStreamsDriver
  { driverCluster :: !MockCluster
  , driverFaults :: !FaultPolicy
  , driverProducer :: !MockProducer
  , driverConsumer :: !MockConsumer
  , driverEngine :: !Engine
  , driverGroup :: !GroupId
  , driverPartCount :: !(IORef (Map.Map TopicName Int))
  , driverMode :: !DriverMode
  }


{- | Build a driver. The cluster + faults are caller-supplied so
tests can pre-load failure scenarios. The driver:

  * Creates every source topic listed in the topology with the
    given partition count (default 1) if it doesn't exist yet.
  * Subscribes the consumer to every source topic.
  * Spawns no threads; the test drives ticks manually.
| Build an at-least-once driver. The output partition count is
the same as the input partition count for every (source ∪ sink)
topic.
-}
newMockStreamsDriver
  :: MockCluster
  -> FaultPolicy
  -> Topo.TopologyValid
  -> Text
  -- ^ application id (group id)
  -> Int
  -- ^ partitions per topic
  -> IO MockStreamsDriver
newMockStreamsDriver = newMockStreamsDriver' AtLeastOnceMode


{- | Build an EOS-V2 driver. Each tick wraps its emissions in a
transaction; a fault on the txn commit aborts and re-tries on
the next tick. The caller supplies the @TxnId@ — typically
@applicationId <> "-txn"@ to mirror the JVM client.
-}
newMockStreamsDriverEOS
  :: MockCluster
  -> FaultPolicy
  -> Topo.TopologyValid
  -> Text
  -- ^ application id (group id)
  -> TxnId
  -- ^ transactional id
  -> Int
  -- ^ partitions per topic
  -> IO MockStreamsDriver
newMockStreamsDriverEOS c fp topo appId tid n =
  newMockStreamsDriver' (ExactlyOnceMode tid) c fp topo appId n


newMockStreamsDriver'
  :: DriverMode
  -> MockCluster
  -> FaultPolicy
  -> Topo.TopologyValid
  -> Text
  -> Int
  -> IO MockStreamsDriver
newMockStreamsDriver' mode cluster faults topo appId partsPerTopic = do
  let topo' = Topo.topologyValidGraph topo
      sourceTopics =
        Set.toList . Set.fromList $
          concatMap
            Topo.sourceTopics
            (Map.elems (Topo.topoSources topo'))
      sinkTopics =
        Set.toList . Set.fromList $
          map Topo.sinkTopic (Map.elems (Topo.topoSinks topo'))
  forM_ (sourceTopics ++ sinkTopics) $ \tp ->
    createTopic cluster tp partsPerTopic
  partRef <-
    newIORef
      (Map.fromList [(tp, partsPerTopic) | tp <- sourceTopics ++ sinkTopics])
  let txnId = case mode of
        AtLeastOnceMode -> Nothing
        ExactlyOnceMode tid -> Just tid
  prod <- newMockProducer cluster faults txnId
  let !grp = GroupId appId
  cons <-
    newMockConsumer
      cluster
      faults
      grp
      ( case mode of
          AtLeastOnceMode -> ReadUncommitted
          ExactlyOnceMode _ -> ReadCommitted
      )
      100
  subscribeMC cons sourceTopics
  collector <- inMemoryCollector
  engine <- buildEngine topo (TaskId 0 0) appId collector logAndContinue
  -- Sinks with a StreamPartitioner see the cluster's partition counts.
  setPartitionCountLookup engine $ \t -> do
    m <- readIORef partRef
    pure (Right (Map.findWithDefault 1 (topicName t) m))
  pure
    MockStreamsDriver
      { driverCluster = cluster
      , driverFaults = faults
      , driverProducer = prod
      , driverConsumer = cons
      , driverEngine = engine
      , driverGroup = grp
      , driverPartCount = partRef
      , driverMode = mode
      }


{- | Read-only accessor for the driver's per-topic partition count
(so tests can avoid hard-coding the value they passed at
construction).
-}
driverPartitions :: MockStreamsDriver -> TopicName -> IO Int
driverPartitions = partitionCountFor


----------------------------------------------------------------------
-- Tick: one poll → process → drain → commit cycle
----------------------------------------------------------------------

{- | Run one full poll/process/drain/commit cycle.

AtLeastOnce mode:
  * poll → feedSource for each record → drain collector → flush sink writes
  * commit consumer offsets to the group coordinator

ExactlyOnce mode:
  * begin a transaction (faults short-circuit; the commit cycle
    leaves offsets uncommitted and surfaces the fault on the
    producer side via MPFault)
  * poll → feedSource → drain collector through the transactional
    producer (records get stamped with the txn id + epoch)
  * commit transaction → commit consumer offsets. If the txn
    commit faults, abort and surface the fault to the caller
    (the consumer's positions don't advance; the next tick
    starts from the same offset).

Returns 'True' if any record was consumed.
-}
tickDriver :: MockStreamsDriver -> IO Bool
tickDriver d = case driverMode d of
  AtLeastOnceMode -> tickAtLeastOnce d
  ExactlyOnceMode _ -> tickExactlyOnce d


tickAtLeastOnce :: MockStreamsDriver -> IO Bool
tickAtLeastOnce d = do
  PollResult {prRecords = recs, prErrors = errs} <- pollMC (driverConsumer d)
  when (not (null errs)) $ pure ()
  forM_ recs $ \(tp, p, sr) ->
    feedSourceH
      (driverEngine d)
      tp
      (srKey sr)
      (srValue sr)
      (KH.fromList (srHeaders sr))
      (srTimestamp sr)
      (fromIntegral p)
      (srOffset sr)
  drainCollectedToMock d
  _ <- takeCommitRequested (driverEngine d)
  commitEngine (driverEngine d)
  asg <- assignedPartitionsOnConsumer (driverConsumer d)
  offs <-
    mapM
      ( \(tp, p) -> do
          mPos <- currentPosition (driverConsumer d) tp p
          pure (tp, p, maybe 0 id mPos)
      )
      asg
  _ <- commitOffsetsMC (driverConsumer d) offs
  pure (not (null recs))


-- | EOS tick: open a txn, feed + emit, commit-or-abort.
tickExactlyOnce :: MockStreamsDriver -> IO Bool
tickExactlyOnce d = do
  beginR <- beginTxnMP (driverProducer d)
  case beginR of
    Left _ -> pure False -- couldn't even begin; skip this tick
    Right () -> do
      PollResult {prRecords = recs} <- pollMC (driverConsumer d)
      forM_ recs $ \(tp, p, sr) ->
        feedSourceH
          (driverEngine d)
          tp
          (srKey sr)
          (srValue sr)
          (KH.fromList (srHeaders sr))
          (srTimestamp sr)
          (fromIntegral p)
          (srOffset sr)
      drainCollectedToMock d
      _ <- takeCommitRequested (driverEngine d)
      commitEngine (driverEngine d)
      commitR <- commitTxnMP (driverProducer d)
      case commitR of
        Left _ -> do
          -- Abort the txn (records become read-committed-invisible)
          _ <- abortTxnMP (driverProducer d)
          pure (not (null recs))
        Right () -> do
          asg <- assignedPartitionsOnConsumer (driverConsumer d)
          offs <-
            mapM
              ( \(tp, p) -> do
                  mPos <- currentPosition (driverConsumer d) tp p
                  pure (tp, p, maybe 0 id mPos)
              )
              asg
          _ <- commitOffsetsMC (driverConsumer d) offs
          pure (not (null recs))


-- The Consumer module already exports 'assignedPartitions' under
-- that name; alias to avoid the small naming collision below.
assignedPartitionsOnConsumer :: MockConsumer -> IO [(TopicName, Int32)]
assignedPartitionsOnConsumer = Kafka.Streams.Mock.Consumer.assignedPartitions


----------------------------------------------------------------------
-- Drain collector into the mock cluster
----------------------------------------------------------------------

drainCollectedToMock :: MockStreamsDriver -> IO ()
drainCollectedToMock d = do
  -- We don't know which topics the topology might write to in
  -- advance, so iterate over every topic the cluster knows about
  -- and pull anything sitting in the collector for it.
  parts <- readIORef (driverPartCount d)
  forM_ (Map.toAscList parts) $ \(topic, _n) -> do
    rs <- collectorTake (engineCollector (driverEngine d)) topic
    forM_ rs $ \cr -> do
      n <- partitionCountFor d (crTopic cr)
      let !p = case crPartition cr of
            Just explicit -> fromIntegral explicit `mod` n
            Nothing -> hashKeyToPartition (crKey cr) n
      _ <-
        sendMockH
          (driverProducer d)
          (crTopic cr)
          (fromIntegral p)
          (crKey cr)
          (crValue cr)
          (crTimestamp cr)
          (KH.toList (crHeaders cr))
      pure ()


partitionCountFor :: MockStreamsDriver -> TopicName -> IO Int
partitionCountFor d t = do
  m <- readIORef (driverPartCount d)
  pure (Map.findWithDefault 1 t m)


hashKeyToPartition :: Maybe ByteString -> Int -> Int
hashKeyToPartition Nothing _ = 0
hashKeyToPartition (Just b) n =
  let !h = byteStringHash b
  in (h `mod` n)


byteStringHash :: ByteString -> Int
byteStringHash =
  -- Stand-in for Kafka's Murmur2; we only need a deterministic
  -- non-zero hash, not exact compat with the JVM client.
  Data.ByteString.foldl' (\acc w -> acc * 31 + fromEnum w) 0
  where
    foldl' = Data.ByteString.foldl'


----------------------------------------------------------------------
-- Run-until-quiet helper
----------------------------------------------------------------------

{- | Tick until two consecutive ticks return 'False' (no fresh
records consumed). Bounded to defend against pathological loops.
-}
runUntilQuiet :: MockStreamsDriver -> IO ()
runUntilQuiet d = go (32 :: Int)
  where
    go 0 = pure ()
    go n = do
      r <- tickDriver d
      if r then go (n - 1) else pure ()


closeMockDriver :: MockStreamsDriver -> IO ()
closeMockDriver d = do
  flushMock (driverProducer d)
  closeEngine (driverEngine d)


----------------------------------------------------------------------
-- External producer (for tests injecting input from outside the
-- topology, e.g. simulating an upstream service)
----------------------------------------------------------------------

{- | Write a record directly to the mock cluster as if some
/external/ producer had done so. Useful for setting up a topic
with seed data the topology will later consume.
-}
externalSend
  :: MockStreamsDriver
  -> TopicName
  -> Int32
  -> Maybe ByteString
  -> ByteString
  -> Timestamp
  -> IO (Either String Int64)
externalSend d topic part k v ts =
  appendToPartition (driverCluster d) topic part k v ts [] Nothing


-- | 'externalSend' with explicit headers.
externalSendH
  :: MockStreamsDriver
  -> TopicName
  -> Int32
  -> Maybe ByteString
  -> ByteString
  -> Timestamp
  -> [(Text, ByteString)]
  -> IO (Either String Int64)
externalSendH d topic part k v ts hdrs =
  appendToPartition (driverCluster d) topic part k v ts hdrs Nothing
