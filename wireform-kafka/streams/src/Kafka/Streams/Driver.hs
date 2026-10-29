{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.Driver
Description : Synchronous, broker-less topology driver

@
TopologyTestDriver
@

is the in-process driver Kafka Streams ships for unit-testing; we
mirror its surface here. Records pushed via 'pipeInput' are routed
through the topology synchronously; records produced by sinks
accumulate in the in-memory collector and are read out via
'readOutput'.

== Differences from the JVM

  * 'pipeInput' is synchronous and exception-passing; there is no
    producer thread or batching layer.
  * Wall-clock time is /entirely/ user-controlled — call
    'advanceWallClockTime' to step it.
  * Stream time is advanced automatically by the timestamp
    extractor on every 'pipeInput'.
  * State stores are realised eagerly when the driver starts; they
    stay open until 'closeDriver'.

== Common usage

@
driver <- newDriver topology "test-app"
pipeInput driver "input-topic" (Just "k") "v" (Timestamp 0) 0 0
out <- readOutput driver "output-topic"
closeDriver driver
@
-}
module Kafka.Streams.Driver (
  TopologyTestDriver,
  newDriver,
  newDriverWith,
  pipeInput,
  pipeInputH,
  pipeInputs,
  readOutput,
  readOutputAll,
  advanceWallClockTime,
  advanceDriverStreamTime,
  currentStreamTime,
  currentWallClockTime,
  commitDriver,
  closeDriver,
  driverEngine,
  getKeyValueStore,
  getWindowStore,
  getSessionStore,
  getTimestampedKeyValueStore,
  getTimestampedWindowStore,

  -- * Output decoding helpers
  OutputRecord (..),
  decodeOutput,

  -- * Re-export of the raw collected record shape
  CollectedRecord (..),
  crValueBytes,

  -- * Typed input / output topic helpers (streams-test-utils)
  TestInputTopic,
  TestOutputTopic,
  createInputTopic,
  createOutputTopic,
  pipeKV,
  pipeKVAt,
  pipeTombstone,
  pipeValue,
  pipeAll,
  readKV,
  readValuesToList,
  readKeyValuesToList,
  isOutputEmpty,

  -- * TestRecord
  TestRecord (..),
  toTestRecord,
  kvFromTestRecord,
) where

import Control.Monad (forM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text (Text)
import Kafka.Streams.Errors (DeserializationHandler, logAndContinue)
import Kafka.Streams.Internal.Engine (
  Engine,
  advanceStreamTimeTo,
  advanceWallClock,
  buildEngine,
  closeEngine,
  commitEngine,
  engineCollector,
  engineTopology,
  feedSource,
  feedSourceH,
  storeByName,
  storeEntryAny,
  streamTimeOfEngine,
  wallClockTimeOfEngine,
 )
import Kafka.Streams.Internal.RecordCollector (
  CollectedRecord (..),
  crValueBytes,
  RecordCollector,
  collectorPeek,
  collectorTake,
  inMemoryCollector,
 )
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Serde (Serde, deserialize)
import Kafka.Streams.Serde qualified
import Kafka.Streams.State.KeyValue.Timestamped qualified
import Kafka.Streams.State.Store (
  AnyStateStore (..),
  KeyValueStore,
  SessionStore,
  StoreName,
  WindowStore,
 )
import Kafka.Streams.Time (Timestamp (..))
import Kafka.Streams.Topology qualified
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Types (
  Headers,
  TopicName,
  emptyHeaders,
 )
import Kafka.Streams.Types qualified
import Unsafe.Coerce qualified as Unsafe


----------------------------------------------------------------------
-- Driver
----------------------------------------------------------------------

data TopologyTestDriver = TopologyTestDriver
  { driverEngine :: !Engine
  , driverNextOff :: !(IORef Int64)
  }


{- | Build a driver with default settings:

  * 'TaskId' = @TaskId 0 0@
  * Deserialisation handler = 'logAndContinue'
  * Wall-clock time starts at the current system time
-}
newDriver :: Topo.Topology -> Text -> IO TopologyTestDriver
newDriver topo appId =
  case Topo.validateTopology topo of
    Left err -> error $ "TopologyTestDriver: invalid topology: " <> show err
    Right v -> newDriverWith v appId logAndContinue


{- | Like 'newDriver' but takes an explicit handler and a validated
topology.
-}
newDriverWith
  :: Topo.TopologyValid
  -> Text
  -> DeserializationHandler
  -> IO TopologyTestDriver
newDriverWith validated appId deserHandler = do
  collector <- inMemoryCollector
  engine <- buildEngine validated (TaskId 0 0) appId collector deserHandler
  off <- newIORef 0
  pure
    TopologyTestDriver
      { driverEngine = engine
      , driverNextOff = off
      }


{- | Pipe one record through the topology, blocking until all
downstream effects (state-store writes, sink emissions, stream-time
punctuators, /and/ any internal repartition / through-topic loops)
complete.

Internal-topic routing: if the topology has a sink that produces
to a topic which is also subscribed by a source — i.e. a
repartition or 'throughTopic' boundary — the driver drains those
records out of the collector and re-feeds them to the source
side. This iterates until quiescence so multi-stage internal
loops complete in a single 'pipeInput' call.
-}
pipeInput
  :: TopologyTestDriver
  -> TopicName
  -> Maybe ByteString
  -- ^ key bytes (or 'Nothing' for a tombstone)
  -> ByteString
  -- ^ value bytes
  -> Timestamp
  -> Int
  -- ^ partition (advisory)
  -> IO ()
pipeInput d topic key value ts part =
  pipeInputH d topic key (Just value) emptyHeaders ts part


{- | Like 'pipeInput' but carries record headers into the topology.
Headers are visible to processors via the 'ProcessorContext' and
propagate through repartition / through-topic hops.
-}
pipeInputH
  :: TopologyTestDriver
  -> TopicName
  -> Maybe ByteString
  -- ^ key bytes ('Nothing' for a keyless record)
  -> Maybe ByteString
  -- ^ value bytes ('Nothing' for a null value, a tombstone)
  -> Headers
  -- ^ record headers
  -> Timestamp
  -> Int
  -- ^ partition (advisory)
  -> IO ()
pipeInputH d topic key value hdrs ts part = do
  off <- atomicModifyIORef' (driverNextOff d) (\n -> (n + 1, n))
  feedSourceH (driverEngine d) topic key value hdrs ts part off
  drainInternalLoop d


pipeInputs
  :: TopologyTestDriver
  -> [(TopicName, Maybe ByteString, ByteString, Timestamp, Int)]
  -> IO ()
pipeInputs d = mapM_ go
  where
    go (t, k, v, ts, p) = pipeInput d t k v ts p


{- | Read the next batch of records produced for the given topic. The
records are returned in the order they were emitted; the queue is
/drained/ — subsequent calls return only newly produced records.
-}
readOutput
  :: TopologyTestDriver
  -> TopicName
  -> IO [CollectedRecord]
readOutput d topic = do
  let collector = engineCollectorOf d
  collectorTake collector topic


-- | Read every output record currently buffered, grouped by topic.
readOutputAll
  :: TopologyTestDriver
  -> IO [(TopicName, [CollectedRecord])]
readOutputAll d = do
  m <- engineCollectorPeekOf d
  let tops = Map.keys m
  forM tops $ \t -> do
    rs <- collectorTake (engineCollectorOf d) t
    pure (t, rs)


{- | Step wall-clock time forward by @deltaMs@ and fire any due
wall-clock punctuators.
-}
advanceWallClockTime
  :: TopologyTestDriver
  -> Int64
  -- ^ delta in milliseconds
  -> IO ()
advanceWallClockTime d delta = advanceWallClock (driverEngine d) delta


{- | Advance stream-time to the supplied timestamp (no record is fed
in). Triggers stream-time punctuators that come due.
-}
advanceDriverStreamTime
  :: TopologyTestDriver
  -> Timestamp
  -> IO ()
advanceDriverStreamTime d ts = advanceStreamTimeTo (driverEngine d) ts


currentStreamTime :: TopologyTestDriver -> IO Timestamp
currentStreamTime = streamTimeOfEngine . driverEngine


currentWallClockTime :: TopologyTestDriver -> IO Timestamp
currentWallClockTime = wallClockTimeOfEngine . driverEngine


{- | Flush every store, drain the collector. After this returns the
driver is in the same state it would be after a successful commit
on a real runtime.
-}
commitDriver :: TopologyTestDriver -> IO ()
commitDriver = commitEngine . driverEngine


closeDriver :: TopologyTestDriver -> IO ()
closeDriver = closeEngine . driverEngine


----------------------------------------------------------------------
-- Store helpers
----------------------------------------------------------------------

{- | Look up a typed key-value store. The user is responsible for the
key/value types matching what the topology declared (the same way
the JVM driver requires the right @Class@ at @getKeyValueStore@).
-}
getKeyValueStore
  :: TopologyTestDriver
  -> StoreName
  -> IO (Maybe (KeyValueStore k v))
getKeyValueStore d sn = do
  m <- storeByName (driverEngine d) sn
  pure $ case m of
    Just se -> case storeEntryAny se of
      AnyKeyValueStore kvs -> Just (Unsafe.unsafeCoerce kvs)
      _ -> Nothing
    Nothing -> Nothing


getWindowStore
  :: TopologyTestDriver
  -> StoreName
  -> IO (Maybe (WindowStore k v))
getWindowStore d sn = do
  m <- storeByName (driverEngine d) sn
  pure $ case m of
    Just se -> case storeEntryAny se of
      AnyWindowStore ws -> Just (Unsafe.unsafeCoerce ws)
      _ -> Nothing
    Nothing -> Nothing


getSessionStore
  :: TopologyTestDriver
  -> StoreName
  -> IO (Maybe (SessionStore k v))
getSessionStore d sn = do
  m <- storeByName (driverEngine d) sn
  pure $ case m of
    Just se -> case storeEntryAny se of
      AnySessionStore ss -> Just (Unsafe.unsafeCoerce ss)
      _ -> Nothing
    Nothing -> Nothing


{- | Like 'getKeyValueStore' but for stores parameterised by
'ValueAndTimestamp'. Mirrors Java's
@TopologyTestDriver.getTimestampedKeyValueStore@.
-}
getTimestampedKeyValueStore
  :: TopologyTestDriver
  -> StoreName
  -> IO (Maybe (KeyValueStore k (Kafka.Streams.State.KeyValue.Timestamped.ValueAndTimestamp v)))
getTimestampedKeyValueStore = getKeyValueStore


{- | Like 'getWindowStore' but for windowed stores
parameterised by 'ValueAndTimestamp'. Mirrors Java's
@TopologyTestDriver.getTimestampedWindowStore@.
-}
getTimestampedWindowStore
  :: TopologyTestDriver
  -> StoreName
  -> IO (Maybe (WindowStore k (Kafka.Streams.State.KeyValue.Timestamped.ValueAndTimestamp v)))
getTimestampedWindowStore = getWindowStore


----------------------------------------------------------------------
-- Internal helpers
----------------------------------------------------------------------

engineCollectorOf :: TopologyTestDriver -> RecordCollector
engineCollectorOf d = engineCollector (driverEngine d)


{- | Detect every topic the topology /both/ produces to (via a sink)
and consumes from (via a source) — these are the "internal"
topics that repartition / through-topic boundaries create.
-}
internalTopics :: TopologyTestDriver -> IO [TopicName]
internalTopics d = do
  let topo = engineTopology (driverEngine d)
      sinkTopics =
        [ Kafka.Streams.Topology.sinkTopic spec
        | spec <- Map.elems (Kafka.Streams.Topology.topoSinks topo)
        ]
      sourceTopics =
        Set.fromList $
          concatMap Kafka.Streams.Topology.sourceTopics $
            Map.elems (Kafka.Streams.Topology.topoSources topo)
  pure (filter (`Set.member` sourceTopics) sinkTopics)


{- | Drain any records sitting in the collector whose topic is
internal (sink+source pair) and re-feed them through the source
side. Repeats until quiescence; bounded by a small iteration
count to defend against pathological cycles.
-}
drainInternalLoop :: TopologyTestDriver -> IO ()
drainInternalLoop d = go (8 :: Int)
  where
    go 0 = pure ()
    go n = do
      itopics <- internalTopics d
      delivered <- mapM drainOne itopics
      if any id delivered then go (n - 1) else pure ()

    drainOne :: TopicName -> IO Bool
    drainOne tp = do
      rs <- collectorTake (engineCollectorOf d) tp
      case rs of
        [] -> pure False
        _ -> do
          mapM_ feedOne rs
          pure True

    feedOne cr = do
      off <- atomicModifyIORef' (driverNextOff d) (\m -> (m + 1, m))
      feedSourceH
        (driverEngine d)
        (crTopic cr)
        (crKey cr)
        (crValue cr)
        (crHeaders cr)
        (crTimestamp cr)
        0
        off


engineCollectorPeekOf
  :: TopologyTestDriver
  -> IO (Map TopicName (Seq.Seq CollectedRecord))
engineCollectorPeekOf d =
  collectorPeek (engineCollector (driverEngine d))


----------------------------------------------------------------------
-- Output decoding
----------------------------------------------------------------------

-- | An output record decoded back through the user-supplied serdes.
data OutputRecord k v = OutputRecord
  { orKey :: !(Maybe k)
  , orValue :: !v
  , orTimestamp :: !Timestamp
  , orHeaders :: !Headers
  }


-- | Decode a 'CollectedRecord' through a typed key/value 'Serde' pair.
decodeOutput
  :: Serde k
  -> Serde v
  -> CollectedRecord
  -> Either Text (OutputRecord k v)
decodeOutput ks vs cr = do
  k <- maybe (Right Nothing) (fmap Just . deserialize ks) (crKey cr)
  v <- deserialize vs (crValueBytes cr)
  pure
    OutputRecord
      { orKey = k
      , orValue = v
      , orTimestamp = crTimestamp cr
      , orHeaders = crHeaders cr
      }


----------------------------------------------------------------------
-- TestInputTopic / TestOutputTopic
----------------------------------------------------------------------

{- | Typed input topic. Mirrors @TestInputTopic<K, V>@ from Java's
@streams-test-utils@: bind once, then 'pipeKV' / 'pipeValue'
without re-supplying the topic name or serdes.
-}
data TestInputTopic k v = TestInputTopic
  { titDriver :: !TopologyTestDriver
  , titTopic :: !TopicName
  , titKeySerde :: !(Serde k)
  , titValSerde :: !(Serde v)
  }


-- | Typed output topic. Mirrors @TestOutputTopic<K, V>@.
data TestOutputTopic k v = TestOutputTopic
  { totDriver :: !TopologyTestDriver
  , totTopic :: !TopicName
  , totKeySerde :: !(Serde k)
  , totValSerde :: !(Serde v)
  }


createInputTopic
  :: TopologyTestDriver
  -> TopicName
  -> Serde k
  -> Serde v
  -> TestInputTopic k v
createInputTopic d t ks vs = TestInputTopic d t ks vs


createOutputTopic
  :: TopologyTestDriver
  -> TopicName
  -> Serde k
  -> Serde v
  -> TestOutputTopic k v
createOutputTopic d t ks vs = TestOutputTopic d t ks vs


{- | Pipe a single (key, value) record using the topic's bound
serdes. Timestamps default to @Timestamp 0@; use 'pipeKVAt' to
override.
-}
pipeKV
  :: TestInputTopic k v
  -> Maybe k
  -> v
  -> IO ()
pipeKV tit mk v = pipeKVAt tit mk v (Timestamp 0) 0


-- | 'pipeKV' with an explicit timestamp + partition.
pipeKVAt
  :: TestInputTopic k v
  -> Maybe k
  -> v
  -> Timestamp
  -> Int
  -> IO ()
pipeKVAt TestInputTopic {..} mk v ts part = do
  let kBytes = fmap (Kafka.Streams.Serde.serialize titKeySerde) mk
      vBytes = Kafka.Streams.Serde.serialize titValSerde v
  pipeInput titDriver titTopic kBytes vBytes ts part


{- | Pipe a tombstone (a null value) for @k@: a table source deletes
the key; a stream source drops the record.
-}
pipeTombstone :: TestInputTopic k v -> k -> IO ()
pipeTombstone TestInputTopic {..} k =
  pipeInputH
    titDriver
    titTopic
    (Just (Kafka.Streams.Serde.serialize titKeySerde k))
    Nothing
    emptyHeaders
    (Timestamp 0)
    0


-- | Pipe a single value with no key.
pipeValue :: TestInputTopic k v -> v -> IO ()
pipeValue tit v = pipeKV tit Nothing v


-- | Pipe a sequence of (key, value, timestamp) tuples in order.
pipeAll
  :: TestInputTopic k v
  -> [(Maybe k, v, Timestamp)]
  -> IO ()
pipeAll tit = mapM_ (\(k, v, ts) -> pipeKVAt tit k v ts 0)


{- | Drain every record currently on the output topic and decode
each one through the bound serdes. Caller decides how to consume.
-}
readKeyValuesToList
  :: TestOutputTopic k v -> IO [Either Text (Maybe k, v)]
readKeyValuesToList TestOutputTopic {..} = do
  rs <- readOutput totDriver totTopic
  pure (map decodePair rs)
  where
    decodePair cr = do
      mk <- case crKey cr of
        Nothing -> Right Nothing
        Just kb -> Just <$> Kafka.Streams.Serde.deserialize totKeySerde kb
      v <- Kafka.Streams.Serde.deserialize totValSerde (crValueBytes cr)
      Right (mk, v)


-- | Drain only the values, skipping any decode errors silently.
readValuesToList :: TestOutputTopic k v -> IO [v]
readValuesToList tot = do
  pairs <- readKeyValuesToList tot
  pure [v | Right (_, v) <- pairs]


{- | Read one (key, value) pair, decoded. Returns @Nothing@ if the
output topic is empty.
-}
readKV :: TestOutputTopic k v -> IO (Maybe (Either Text (Maybe k, v)))
readKV tot = do
  pairs <- readKeyValuesToList tot
  case pairs of
    [] -> pure Nothing
    (p : _) -> pure (Just p)


-- | True iff the output topic has no records currently buffered.
isOutputEmpty :: TestOutputTopic k v -> IO Bool
isOutputEmpty tot = null <$> readKeyValuesToList tot


----------------------------------------------------------------------
-- TestRecord
----------------------------------------------------------------------

{- | A typed test record. Mirrors Java's @TestRecord<K, V>@ —
carries key / value / timestamp / headers in a single value
so test assertions can compare them with '==' or pattern match
structurally.
-}
data TestRecord k v = TestRecord
  { trKey :: !(Maybe k)
  , trValue :: !v
  , trTimestamp :: !Timestamp
  , trHeaders :: !Kafka.Streams.Types.Headers
  }
  deriving (Eq, Show)


toTestRecord
  :: Serde k
  -> Serde v
  -> CollectedRecord
  -> Either Text (TestRecord k v)
toTestRecord ks vs cr = do
  k <- maybe (Right Nothing) (fmap Just . deserialize ks) (crKey cr)
  v <- deserialize vs (crValueBytes cr)
  pure
    TestRecord
      { trKey = k
      , trValue = v
      , trTimestamp = crTimestamp cr
      , trHeaders = crHeaders cr
      }


kvFromTestRecord :: TestRecord k v -> (Maybe k, v)
kvFromTestRecord r = (trKey r, trValue r)
