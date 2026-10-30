{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Streams.KStream
Description : The KStream DSL surface

@
KStream k v
@

is a never-ending sequence of @Record k v@ values flowing along a
single edge of the topology graph. Operations on a 'KStream'
register a new processor as a child of the upstream node and
return a 'KStream' pinned to that new node.

The DSL mirrors the Java methods on
@org.apache.kafka.streams.kstream.KStream<K,V>@:

  * 'streamFromTopic'         — Java @StreamsBuilder.stream@
  * 'filterStream'            — @KStream.filter@
  * 'filterNotStream'         — @KStream.filterNot@
  * 'mapValues'               — @KStream.mapValues@
  * 'mapKeyValue'             — @KStream.map@
  * 'concatMapValues'           — @KStream.concatMapValues@
  * 'concatMapKeyValue'         — @KStream.flatMap@
  * 'foreachStream'           — @KStream.foreach@
  * 'peekStream'              — @KStream.peek@
  * 'selectKey'               — @KStream.selectKey@
  * 'mergeStreams'            — @KStream.merge@
  * 'branchStream'            — @KStream.branch@
  * 'toTopic'                 — @KStream.to@
  * 'throughTopic'            — @KStream.through@
  * 'groupByKey'              — @KStream.groupByKey@
  * 'groupByStream'           — @KStream.groupBy@
  * 'transformValuesStream'   — @KStream.transformValues@
-}
module Kafka.Streams.KStream (
  KStream (..),

  -- * Sources
  streamFromTopic,
  streamFromPattern,

  -- * Stateless transforms
  filterStream,
  filterStreamNamed,
  filterNotStream,
  filterNotStreamNamed,
  mapValues,
  mapValuesWith,
  mapValuesNamed,
  mapValuesM,
  mapValuesMWith,
  mapValuesMNamed,
  mapKeyValue,
  mapKeyValueWith,
  mapKeyValueNamed,
  mapKeyValueM,
  mapKeyValueMWith,
  mapKeyValueMNamed,
  mapRecord,
  mapRecordWith,
  mapRecordNamed,
  mapRecordM,
  mapRecordMWith,
  mapRecordMNamed,
  concatMapValues,
  concatMapValuesWith,
  concatMapValuesNamed,
  concatMapKeyValue,
  concatMapKeyValueWith,
  concatMapKeyValueNamed,
  peekStream,
  peekStreamNamed,
  foreachStream,
  foreachStreamAsync,
  printStream,
  printToHandle,
  valuesStream,
  valuesStreamNamed,
  selectKey,
  selectKeyWith,
  selectKeyNamed,

  -- * Composition
  mergeStreams,
  mergeStreamsNamed,
  mergeStreamsN,
  mkPassThrough,
  branchStream,

  -- * Sinks
  toTopic,
  toTopicNullable,
  toTopicNamed,
  toExtracted,
  TopicNameExtractor (..),
  throughTopic,

  -- * Conversions
  toTable,
  repartition,
  repartitionNamed,
  repartitionWith,
  repartitionThrough,
  repartitionIfKeyChanged,
  sharedRepartition,
  toKStreamFromKTable,
  toChangelogStream,

  -- * Joins
  joinKStreamKTable,
  leftJoinKStreamKTable,
  joinKStreamKStream,
  leftJoinKStreamKStream,
  outerJoinKStreamKStream,

  -- * Branching
  splitStream,
  Branched (..),
  branchedFrom,
  withFunction,
  withConsumer,

  -- * Low-level access
  transformValuesStream,
  processStream,
  processValuesStream,
  concatTransformValues,

  -- * Extension hooks

  --
  -- 'attachProcessor' is the primitive every stateless and stateful
  -- transform on this module is built on. It is exported so the
  -- 'Kafka.Streams.AsyncIO' family (and any future
  -- 'wireform-kafka-streams' Riffle extension) can plug in fresh
  -- processor primitives without duplicating the
  -- "fresh node name + addProcessor + return a KStream" boilerplate.
  attachProcessor,
  attachRekeyingProcessor,
) where

import Control.Concurrent.Async qualified
import Data.IORef
import Data.Int (Int64)
import Data.Maybe (isJust)
import Control.Exception (throwIO)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Kafka.Streams.Consumed (
  Consumed (..),
  consumed,
 )
import Kafka.Streams.Internal.Serdes (eitherSerde, listSerde)
import Kafka.Streams.Internal.WindowJoin qualified as WJ
import Kafka.Streams.Joined (
  JoinWindows (..),
  Joined (..),
 )
import Kafka.Streams.KTable (
  KTable (..),
  ktableNode,
  ktableStore,
 )
import Kafka.Streams.KTable qualified
import Kafka.Streams.Materialized (
  Materialized (..),
  materializedKV,
 )
import Kafka.Streams.Named qualified
import Kafka.Streams.Processor (
  Processor (..),
  ProcessorContext (..),
  forwardRecord,
  forwardTo,
  getStateStore,
  processorName,
 )
import Kafka.Streams.Processor qualified
import Kafka.Streams.Produced (
  Produced (..),
  StreamPartitioner,
 )
import Kafka.Streams.Repartitioned qualified as KafkaStreamsRepartitioned
import Kafka.Streams.Serde (HasSerde (..), Serde, serialize, serializeHeaders)
import Kafka.Streams.State.KeyValue.InMemory (
  inMemoryKeyValueStoreBuilder,
 )
import Kafka.Streams.State.Store (
  withStoreSerdesKV,
  withStoreSerdesW,
  AnyStateStore (..),
  KeyValueStore (..),
  StoreBuilderW,
  StoreName,
 )
import Kafka.Streams.State.Store qualified
import Kafka.Streams.State.Window.InMemory (
  inMemoryWindowStoreBuilder,
 )
import Kafka.Streams.Errors (TopologyException (..))
import Kafka.Streams.StreamsBuilder (
  StreamsBuilder,
  freshNodeName,
  freshStoreName,
  sharedRepartitionNode,
  withTopology_,
 )
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Types (
  Record (..),
  TopicName,
 )
import Kafka.Streams.Types qualified
import Unsafe.Coerce qualified as Unsafe


----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

{- | A handle to a stream node in the topology being built.

Note: the serde fields are intentionally lazy. After a 'mapValues'
the downstream value serde is unknown until the user attaches a
'Produced' / 'Materialized' downstream — we encode that as a
thunk that errors out only if forced.
-}
data KStream k v = KStream
  { kstreamBuilder :: !StreamsBuilder
  , kstreamParent :: !Topo.NodeName
  , kstreamKeySerde :: ~(Serde k)
  , kstreamValueSerde :: ~(Serde v)
  , kstreamKeyChanged :: !Bool
  {- ^ Whether an upstream operator may have changed the key since
  the stream was last partitioned by it (a source, a repartition,
  a join). Grouping or joining such a stream first repartitions it
  through an internal topic, as on the JVM.
  -}
  }


----------------------------------------------------------------------
-- Sources
----------------------------------------------------------------------

{- | Build a 'KStream' that subscribes to records from the
given topic. The 'Consumed' record carries the key / value
serdes and an optional explicit source node name.

/JVM equivalent:/ @StreamsBuilder.stream(topic, Consumed)@.
-}
streamFromTopic
  :: StreamsBuilder
  -> TopicName
  -> Consumed k v
  -> IO (KStream k v)
streamFromTopic b topic c = do
  nm <-
    maybe
      (freshNodeName b "KSTREAM-SOURCE")
      (pure . Topo.NodeName)
      (consumedNodeName c)
  withTopology_ b $
    Topo.addSourceWith
      Topo.SourceSpec
        { Topo.sourceName = nm
        , Topo.sourceTopics = [topic]
        , Topo.sourceKeySerde = Topo.AnySerde (consumedKeySerde c)
        , Topo.sourceValueSerde = Topo.AnySerde (consumedValueSerde c)
        , Topo.sourceExtractor = Topo.AnyTimestampExtractor (consumedExtractor c)
        , Topo.sourceOffsetReset = consumedOffsetReset c
        , Topo.sourcePattern = Nothing
        , Topo.sourceNullValues = False
        , Topo.sourceWatermarkStrategy = consumedWatermark c
        }
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = nm
      , kstreamKeyChanged = False
      , kstreamKeySerde = consumedKeySerde c
      , kstreamValueSerde = consumedValueSerde c
      }


{- | Subscribe to every broker topic matching the supplied
regex pattern (JVM @StreamsBuilder.stream(Pattern)@).

The @Text@ is interpreted as a Java-style regex on the
broker. The in-process 'TopologyTestDriver' doesn't enumerate
broker topics, so within the driver this is equivalent to a
source with /no/ explicit topics — pipe in records via
'pipeInput' with topics that match downstream consumers'
expectations. Against a real broker, the runtime resolves
topics matching the pattern at subscribe time.
-}
streamFromPattern
  :: StreamsBuilder
  -> T.Text
  -> Consumed k v
  -> IO (KStream k v)
streamFromPattern b pattern c = do
  nm <-
    maybe
      (freshNodeName b "KSTREAM-SOURCE-PATTERN")
      (pure . Topo.NodeName)
      (consumedNodeName c)
  withTopology_ b $
    Topo.addSourceWith
      Topo.SourceSpec
        { Topo.sourceName = nm
        , Topo.sourceTopics = []
        , Topo.sourceKeySerde = Topo.AnySerde (consumedKeySerde c)
        , Topo.sourceValueSerde = Topo.AnySerde (consumedValueSerde c)
        , Topo.sourceExtractor = Topo.AnyTimestampExtractor (consumedExtractor c)
        , Topo.sourceOffsetReset = consumedOffsetReset c
        , Topo.sourcePattern = Just pattern
        , Topo.sourceNullValues = False
        , Topo.sourceWatermarkStrategy = consumedWatermark c
        }
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = nm
      , kstreamKeyChanged = False
      , kstreamKeySerde = consumedKeySerde c
      , kstreamValueSerde = consumedValueSerde c
      }


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

{- | Internal: register a new processor whose supplier creates a
'Processor' whose /input/ matches @parent@. The new 'KStream'
carries the /output/ types declared by the supplied serdes.

The processor is responsible for forwarding records of the right
shape via 'forwardRecord' / 'ctxForward' — the engine itself is
type-erased.
-}
attachProcessor
  :: KStream k v
  -> T.Text
  -> IO (Processor k v)
  -> Serde k'
  -> Serde v'
  -> IO (KStream k' v')
attachProcessor parent prefix supplier ks' vs' =
  attachProcessorKeyed (kstreamKeyChanged parent) parent prefix supplier ks' vs'


{- | 'attachProcessor' for a processor that may change the key
('selectKey', 'map', 'flatMap', ...): a later grouping or join
repartitions the result.
-}
attachRekeyingProcessor
  :: KStream k v
  -> T.Text
  -> IO (Processor k v)
  -> Serde k'
  -> Serde v'
  -> IO (KStream k' v')
attachRekeyingProcessor = attachProcessorKeyed True


attachProcessorKeyed
  :: Bool
  -> KStream k v
  -> T.Text
  -> IO (Processor k v)
  -> Serde k'
  -> Serde v'
  -> IO (KStream k' v')
attachProcessorKeyed keyChanged parent prefix supplier ks' vs' = do
  let b = kstreamBuilder parent
  nm <- freshNodeName b prefix
  withTopology_ b $
    Topo.addProcessor nm [kstreamParent parent] supplier
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = nm
      , kstreamKeyChanged = keyChanged
      , kstreamKeySerde = ks'
      , kstreamValueSerde = vs'
      }


----------------------------------------------------------------------
-- Stateless transforms
----------------------------------------------------------------------

{- | Keep records for which the predicate returns 'True'.

/JVM equivalent:/ @KStream.filter(Predicate)@.
-}
filterStream :: (Record k v -> Bool) -> KStream k v -> IO (KStream k v)
filterStream pred_ s =
  attachProcessor
    s
    "KSTREAM-FILTER"
    (filterProcessor "KSTREAM-FILTER" pred_)
    (kstreamKeySerde s)
    (kstreamValueSerde s)


{- | Drop records for which the predicate returns 'True'; the
inverse of 'filterStream'.

/JVM equivalent:/ @KStream.filterNot(Predicate)@.
-}
filterNotStream :: (Record k v -> Bool) -> KStream k v -> IO (KStream k v)
filterNotStream pred_ = filterStream (not . pred_)


filterProcessor
  :: T.Text -> (Record k v -> Bool) -> IO (Processor k v)
filterProcessor nm p = do
  ctxRef <- newIORef Nothing
  pure
    Processor
      { procName = processorName nm
      , procInit = \ctx -> writeIORef ctxRef (Just ctx)
      , procClose = pure ()
      , procProcess = \r -> do
          mctx <- readIORef ctxRef
          case mctx of
            Nothing -> pure ()
            Just ctx -> if p r then forwardRecord ctx r else pure ()
      }


{- | Apply a pure function to every record's value.

/JVM equivalent:/ @KStream.mapValues(ValueMapper)@.
-}
mapValues
  :: forall k v v'
   . HasSerde v'
  => (v -> v') -> KStream k v -> IO (KStream k v')
mapValues f s = mapValuesMWith serde (pure . f) s


-- | 'mapValues' with an explicit downstream value 'Serde'.
mapValuesWith
  :: forall k v v'
   . Serde v'
  -> (v -> v')
  -> KStream k v
  -> IO (KStream k v')
mapValuesWith vs' f s = mapValuesMWith vs' (pure . f) s


{- | Apply an effectful function to every record's value. The
'IO' action runs once per record on the stream thread; it
mustn't block on external systems for long, since that
stalls the partition.

/JVM equivalent:/ no direct match — closest is the
@ValueMapper@ + a separate side-effect channel. We model
the @IO@ in the type signature so the cost is visible.
-}
mapValuesM
  :: forall k v v'
   . HasSerde v'
  => (v -> IO v') -> KStream k v -> IO (KStream k v')
mapValuesM = mapValuesMWith serde


-- | 'mapValuesM' with an explicit downstream value 'Serde'.
mapValuesMWith
  :: forall k v v'
   . Serde v' -> (v -> IO v') -> KStream k v -> IO (KStream k v')
mapValuesMWith vs' f s =
  attachProcessor
    s
    "KSTREAM-MAPVALUES"
    (mapValuesProc @k @v @v' f)
    (kstreamKeySerde s)
    vs'


-- Processor whose input is @Record k v@ and that forwards a
-- @Record k v'@ via 'ctxForward'. The forward type is universally
-- quantified inside 'ProcessorContext' so it always type-checks.
mapValuesProc
  :: forall k v v'
   . (v -> IO v') -> IO (Processor k v)
mapValuesProc f = do
  ctxRef <- newIORef Nothing
  pure
    Processor
      { procName = processorName "KSTREAM-MAPVALUES"
      , procInit = \ctx -> writeIORef ctxRef (Just ctx)
      , procClose = pure ()
      , procProcess = \r -> do
          mctx <- readIORef ctxRef
          case mctx of
            Nothing -> pure ()
            Just ctx -> do
              !v' <- f (recordValue r)
              let !out =
                    Record
                      { recordKey = recordKey r
                      , recordValue = v'
                      , recordTimestamp = recordTimestamp r
                      , recordHeaders = recordHeaders r
                      }
                      :: Record k v'
              forwardRecord ctx out
      }


{- | Re-key + re-value every record with a pure function. The
new key feeds into downstream partitioning, so if you intend
to consume the result with a stateful op you usually want to
'repartition' afterwards.

/JVM equivalent:/ @KStream.map(KeyValueMapper)@.
-}
mapKeyValue
  :: forall k v k' v'
   . (HasSerde k', HasSerde v')
  => (k -> v -> (k', v'))
  -> KStream k v
  -> IO (KStream k' v')
mapKeyValue f = mapKeyValueM (\k v -> pure (f k v))


-- | 'mapKeyValue' with explicit downstream key + value 'Serde's.
mapKeyValueWith
  :: forall k v k' v'
   . Serde k'
  -> Serde v'
  -> (k -> v -> (k', v'))
  -> KStream k v
  -> IO (KStream k' v')
mapKeyValueWith ks' vs' f =
  mapKeyValueMWith ks' vs' (\k v -> pure (f k v))


-- | Effectful version of 'mapKeyValue'.
mapKeyValueM
  :: forall k v k' v'
   . (HasSerde k', HasSerde v')
  => (k -> v -> IO (k', v'))
  -> KStream k v
  -> IO (KStream k' v')
mapKeyValueM = mapKeyValueMWith serde serde


-- | 'mapKeyValueM' with explicit downstream key + value serdes.
mapKeyValueMWith
  :: forall k v k' v'
   . Serde k'
  -> Serde v'
  -> (k -> v -> IO (k', v'))
  -> KStream k v
  -> IO (KStream k' v')
mapKeyValueMWith ks' vs' f s =
  attachRekeyingProcessor
    s
    "KSTREAM-MAP"
    (mapKVProc @k @v @k' @v' f)
    ks'
    vs'


mapKVProc
  :: forall k v k' v'
   . (k -> v -> IO (k', v')) -> IO (Processor k v)
mapKVProc f = do
  ctxRef <- newIORef Nothing
  pure
    Processor
      { procName = processorName "KSTREAM-MAP"
      , procInit = \ctx -> writeIORef ctxRef (Just ctx)
      , procClose = pure ()
      , procProcess = \r -> do
          mctx <- readIORef ctxRef
          case (mctx, recordKey r) of
            (Just ctx, Just k) -> do
              (!k', !v') <- f k (recordValue r)
              let !out =
                    Record
                      { recordKey = Just k'
                      , recordValue = v'
                      , recordTimestamp = recordTimestamp r
                      , recordHeaders = recordHeaders r
                      }
                      :: Record k' v'
              forwardRecord ctx out
            _ -> pure ()
      }


{- | Apply a pure transform to the entire 'Record' — key, value,
timestamp, and headers all in scope. Useful when you need to
mutate metadata (e.g. propagate a trace header) alongside the
key/value transform that 'mapKeyValue' covers.

/JVM equivalent:/ no single direct match — closest is
@KStream.transform@ with a stateless transformer. The Java
DSL forces callers through the Processor API; we expose the
common stateless shape as a smart constructor.
-}
mapRecord
  :: forall k v k' v'
   . (HasSerde k', HasSerde v')
  => (Record k v -> Record k' v')
  -> KStream k v
  -> IO (KStream k' v')
mapRecord f = mapRecordM (pure . f)


-- | 'mapRecord' with explicit downstream key + value 'Serde's.
mapRecordWith
  :: forall k v k' v'
   . Serde k'
  -> Serde v'
  -> (Record k v -> Record k' v')
  -> KStream k v
  -> IO (KStream k' v')
mapRecordWith ks' vs' f = mapRecordMWith ks' vs' (pure . f)


{- | Effectful 'mapRecord'. The 'IO' runs once per record on the
stream thread — same backpressure caveat as 'mapValuesM'.
-}
mapRecordM
  :: forall k v k' v'
   . (HasSerde k', HasSerde v')
  => (Record k v -> IO (Record k' v'))
  -> KStream k v
  -> IO (KStream k' v')
mapRecordM = mapRecordMWith serde serde


-- | 'mapRecordM' with explicit downstream key + value serdes.
mapRecordMWith
  :: forall k v k' v'
   . Serde k'
  -> Serde v'
  -> (Record k v -> IO (Record k' v'))
  -> KStream k v
  -> IO (KStream k' v')
mapRecordMWith ks' vs' f s =
  attachRekeyingProcessor
    s
    "KSTREAM-MAPRECORD"
    (mapRecordProc f)
    ks'
    vs'


mapRecordProc
  :: forall k v k' v'
   . (Record k v -> IO (Record k' v'))
  -> IO (Processor k v)
mapRecordProc f = do
  ctxRef <- newIORef Nothing
  pure
    Processor
      { procName = processorName "KSTREAM-MAPRECORD"
      , procInit = \ctx -> writeIORef ctxRef (Just ctx)
      , procClose = pure ()
      , procProcess = \r -> do
          mctx <- readIORef ctxRef
          case mctx of
            Nothing -> pure ()
            Just ctx -> do
              !r' <- f r
              forwardRecord ctx (r' :: Record k' v')
      }


{- | Expand each record into zero or more output records,
changing only the value.

/JVM equivalent:/ @KStream.concatMapValues(ValueMapper)@.
-}
concatMapValues
  :: forall k v v'
   . HasSerde v'
  => (v -> [v'])
  -> KStream k v
  -> IO (KStream k v')
concatMapValues = concatMapValuesWith serde


-- | 'concatMapValues' with an explicit downstream value 'Serde'.
concatMapValuesWith
  :: forall k v v'
   . Serde v'
  -> (v -> [v'])
  -> KStream k v
  -> IO (KStream k v')
concatMapValuesWith vs' f s =
  attachProcessor
    s
    "KSTREAM-FLATMAPVALUES"
    (concatMapValuesProc @k @v @v' f)
    (kstreamKeySerde s)
    vs'


concatMapValuesProc
  :: forall k v v'
   . (v -> [v']) -> IO (Processor k v)
concatMapValuesProc f = do
  ctxRef <- newIORef Nothing
  pure
    Processor
      { procName = processorName "KSTREAM-FLATMAPVALUES"
      , procInit = \ctx -> writeIORef ctxRef (Just ctx)
      , procClose = pure ()
      , procProcess = \r -> do
          mctx <- readIORef ctxRef
          case mctx of
            Nothing -> pure ()
            Just ctx ->
              mapM_
                ( \v' ->
                    let !out =
                          Record
                            { recordKey = recordKey r
                            , recordValue = v'
                            , recordTimestamp = recordTimestamp r
                            , recordHeaders = recordHeaders r
                            }
                            :: Record k v'
                    in forwardRecord ctx out
                )
                (f (recordValue r))
      }


{- | Expand each record into zero or more output records,
changing both key and value. As with 'mapKeyValue', the new
key feeds downstream partitioning.

/JVM equivalent:/ @KStream.flatMap(KeyValueMapper)@.
-}
concatMapKeyValue
  :: forall k v k' v'
   . (HasSerde k', HasSerde v')
  => (k -> v -> [(k', v')])
  -> KStream k v
  -> IO (KStream k' v')
concatMapKeyValue = concatMapKeyValueWith serde serde


-- | 'concatMapKeyValue' with explicit downstream key + value serdes.
concatMapKeyValueWith
  :: forall k v k' v'
   . Serde k'
  -> Serde v'
  -> (k -> v -> [(k', v')])
  -> KStream k v
  -> IO (KStream k' v')
concatMapKeyValueWith ks' vs' f s =
  attachRekeyingProcessor
    s
    "KSTREAM-FLATMAP"
    (concatMapKVProc @k @v @k' @v' f)
    ks'
    vs'


concatMapKVProc
  :: forall k v k' v'
   . (k -> v -> [(k', v')])
  -> IO (Processor k v)
concatMapKVProc f = do
  ctxRef <- newIORef Nothing
  pure
    Processor
      { procName = processorName "KSTREAM-FLATMAP"
      , procInit = \ctx -> writeIORef ctxRef (Just ctx)
      , procClose = pure ()
      , procProcess = \r -> do
          mctx <- readIORef ctxRef
          case (mctx, recordKey r) of
            (Just ctx, Just k) ->
              mapM_
                ( \(k', v') ->
                    let !out =
                          Record
                            { recordKey = Just k'
                            , recordValue = v'
                            , recordTimestamp = recordTimestamp r
                            , recordHeaders = recordHeaders r
                            }
                            :: Record k' v'
                    in forwardRecord ctx out
                )
                (f k (recordValue r))
            _ -> pure ()
      }


{- | Run a side-effecting action on every record without
changing the stream. Useful for metrics / logging / debug
output.

/JVM equivalent:/ @KStream.peek(ForeachAction)@.
-}
peekStream
  :: (Record k v -> IO ())
  -> KStream k v
  -> IO (KStream k v)
peekStream act s =
  attachProcessor
    s
    "KSTREAM-PEEK"
    ( do
        ctxRef <- newIORef Nothing
        pure
          Processor
            { procName = processorName "KSTREAM-PEEK"
            , procInit = \ctx -> writeIORef ctxRef (Just ctx)
            , procClose = pure ()
            , procProcess = \r -> do
                act r
                mctx <- readIORef ctxRef
                case mctx of
                  Nothing -> pure ()
                  Just ctx -> forwardRecord ctx r
            }
    )
    (kstreamKeySerde s)
    (kstreamValueSerde s)


{- | 'foreachStream' with each callback run concurrently, so a slow
one doesn't hold the task up (no order between records). Every commit
first waits for the callbacks started since the last one, so a commit
never covers a record whose side effect hasn't happened (at least
once: after a crash the unfinished ones run again); a callback that
throws fails the commit, and the stream thread then follows the
uncaught-exception handler.

For ordered effects use 'foreachStream' instead.
-}
foreachStreamAsync
  :: (Record k v -> IO ())
  -> KStream k v
  -> IO ()
foreachStreamAsync act s = do
  let b = kstreamBuilder s
  nm <- freshNodeName b "KSTREAM-FOREACH-ASYNC"
  withTopology_ b $
    Topo.addProcessor nm [kstreamParent s] $ do
      pending <- newIORef ([] :: [Control.Concurrent.Async.Async ()])
      let takeAll = atomicModifyIORef' pending (\as -> ([], reverse as))
      pure
        Processor
          { procName = processorName "KSTREAM-FOREACH-ASYNC"
          , procInit = \ctx -> ctxRegisterPreCommitDrain ctx (takeAll >>= mapM_ Control.Concurrent.Async.wait)
          , procClose = takeAll >>= mapM_ Control.Concurrent.Async.waitCatch
          , procProcess = \r -> do
              a <- Control.Concurrent.Async.async (act r)
              atomicModifyIORef' pending (\as -> (a : as, ()))
          }


{- | Terminal sink for side effects: every record runs through
the supplied action and is then dropped (no downstream
stream).

/JVM equivalent:/ @KStream.foreach(ForeachAction)@.
-}
foreachStream :: (Record k v -> IO ()) -> KStream k v -> IO ()
foreachStream act s = do
  let b = kstreamBuilder s
  nm <- freshNodeName b "KSTREAM-FOREACH"
  withTopology_ b $
    Topo.addProcessor nm [kstreamParent s] $ do
      pure
        Processor
          { procName = processorName "KSTREAM-FOREACH"
          , procInit = \_ -> pure ()
          , procClose = pure ()
          , procProcess = act
          }


{- | Re-key the stream by deriving a new key from the record
(key, value, timestamp, headers). Use 'repartition' after
this if a stateful op consumes the result, otherwise the
new key won't drive partitioning correctly.

/JVM equivalent:/ @KStream.selectKey(KeyValueMapper)@.
-}
selectKey
  :: forall k v k'
   . HasSerde k'
  => (Record k v -> k')
  -> KStream k v
  -> IO (KStream k' v)
selectKey = selectKeyWith serde


-- | 'selectKey' with an explicit downstream key 'Serde'.
selectKeyWith
  :: forall k v k'
   . Serde k'
  -> (Record k v -> k')
  -> KStream k v
  -> IO (KStream k' v)
selectKeyWith ks' f s =
  attachRekeyingProcessor
    s
    "KSTREAM-SELECTKEY"
    (selectKeyProc @k @v @k' f)
    ks'
    (kstreamValueSerde s)


selectKeyProc
  :: forall k v k'
   . (Record k v -> k') -> IO (Processor k v)
selectKeyProc f = do
  ctxRef <- newIORef Nothing
  pure
    Processor
      { procName = processorName "KSTREAM-SELECTKEY"
      , procInit = \ctx -> writeIORef ctxRef (Just ctx)
      , procClose = pure ()
      , procProcess = \r -> do
          mctx <- readIORef ctxRef
          case mctx of
            Nothing -> pure ()
            Just ctx ->
              let !out =
                    Record
                      { recordKey = Just (f r)
                      , recordValue = recordValue r
                      , recordTimestamp = recordTimestamp r
                      , recordHeaders = recordHeaders r
                      }
                      :: Record k' v
              in forwardRecord ctx out
      }


----------------------------------------------------------------------
-- Composition
----------------------------------------------------------------------

mergeStreams :: KStream k v -> KStream k v -> IO (KStream k v)
mergeStreams a b = do
  let bld = kstreamBuilder a
  nm <- freshNodeName bld "KSTREAM-MERGE"
  withTopology_ bld $ \t ->
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName = nm
        , Topo.processorSpecParents = [kstreamParent a, kstreamParent b]
        , Topo.processorSpecSupplier = Topo.AnyProcessor (mkPassThrough "KSTREAM-MERGE")
        , Topo.processorSpecStores = []
        }
      t
  pure
    KStream
      { kstreamBuilder = bld
      , kstreamParent = nm
      , kstreamKeyChanged = kstreamKeyChanged a || kstreamKeyChanged b
      , kstreamKeySerde = kstreamKeySerde a
      , kstreamValueSerde = kstreamValueSerde a
      }


mkPassThrough :: T.Text -> IO (Processor k v)
mkPassThrough nm = do
  ctxRef <- newIORef Nothing
  pure
    Processor
      { procName = processorName nm
      , procInit = \ctx -> writeIORef ctxRef (Just ctx)
      , procClose = pure ()
      , procProcess = \r -> do
          mctx <- readIORef ctxRef
          case mctx of
            Nothing -> pure ()
            Just ctx -> forwardRecord ctx r
      }


{- | Branch a stream into N substreams using the supplied
predicates. Records are routed to the first matching
predicate; non-matching records are dropped.

/JVM equivalent:/ @KStream.split().branch(Predicate, ...)@.
-}
branchStream
  :: [(Record k v -> Bool)]
  -> KStream k v
  -> IO [KStream k v]
branchStream preds s = do
  let b = kstreamBuilder s
  -- Create a router processor and one downstream pass-through per
  -- branch. Children nodes attach to the router; the router uses
  -- 'forwardTo' to dispatch.
  router <- freshNodeName b "KSTREAM-BRANCH"
  branches <-
    mapM
      ( \_ ->
          freshNodeName b "KSTREAM-BRANCH-CHILD"
      )
      preds
  withTopology_ b $ \t ->
    let !t1 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = router
              , Topo.processorSpecParents = [kstreamParent s]
              , Topo.processorSpecSupplier =
                  Topo.AnyProcessor (mkRouter preds branches)
              , Topo.processorSpecStores = []
              }
            t
        !t2 =
          foldl
            ( \acc bn ->
                Topo.addProcessorWith
                  Topo.ProcessorSpec
                    { Topo.processorSpecName = bn
                    , Topo.processorSpecParents = [router]
                    , Topo.processorSpecSupplier =
                        Topo.AnyProcessor
                          (mkPassThrough "KSTREAM-BRANCH-CHILD")
                    , Topo.processorSpecStores = []
                    }
                  acc
            )
            t1
            branches
    in t2
  pure
    [ KStream
        { kstreamBuilder = b
        , kstreamParent = bn
        , kstreamKeyChanged = kstreamKeyChanged s
        , kstreamKeySerde = kstreamKeySerde s
        , kstreamValueSerde = kstreamValueSerde s
        }
    | bn <- branches
    ]


mkRouter
  :: [Record k v -> Bool]
  -> [Topo.NodeName]
  -> IO (Processor k v)
mkRouter preds branches = do
  ctxRef <- newIORef Nothing
  pure
    Processor
      { procName = processorName "KSTREAM-BRANCH"
      , procInit = \ctx -> writeIORef ctxRef (Just ctx)
      , procClose = pure ()
      , procProcess = \r -> do
          mctx <- readIORef ctxRef
          case mctx of
            Nothing -> pure ()
            Just ctx -> dispatch ctx (zip preds branches) r
      }
  where
    dispatch _ [] _ = pure ()
    dispatch ctx ((p, target) : rest) r =
      if p r
        then forwardTo ctx target r
        else dispatch ctx rest r


----------------------------------------------------------------------
-- Sinks
----------------------------------------------------------------------

{- | Publish every record on the stream to the given topic.
The 'Produced' record carries the key / value serdes and an
optional explicit sink node name.

/JVM equivalent:/ @KStream.to(topic, Produced)@.
-}
toTopic
  :: TopicName
  -> Produced k v
  -> KStream k v
  -> IO ()
toTopic topic p s = do
  let b = kstreamBuilder s
  nm <-
    maybe
      (freshNodeName b "KSTREAM-SINK")
      (pure . Topo.NodeName)
      (producedName p)
  withTopology_ b $ producedSink nm topic p [kstreamParent s]


{- | Write a stream of @Maybe v@ to a topic, 'Nothing' as a null value
(a tombstone): what a 'toChangelogStream' of a table needs so a
compacted output topic sees its deletes. The 'Produced' partitioner,
if any, is not consulted (records go by key).
-}
toTopicNullable
  :: TopicName
  -> Produced k v
  -> KStream k (Maybe v)
  -> IO ()
toTopicNullable topic p s = do
  let b = kstreamBuilder s
  nm <- maybe (freshNodeName b "KSTREAM-SINK") (pure . Topo.NodeName) (producedName p)
  withTopology_ b $ Topo.addSinkNullable nm topic (producedKeySerde p) (producedValueSerde p) [kstreamParent s]


-- | Add the sink node a 'Produced' describes, partitioner included.
producedSink
  :: Topo.NodeName
  -> TopicName
  -> Produced k v
  -> [Topo.NodeName]
  -> Topo.Topology
  -> Topo.Topology
producedSink nm topic p parents = case producedPartitioner p of
  Nothing ->
    Topo.addSink nm topic (producedKeySerde p) (producedValueSerde p) parents
  Just sp ->
    Topo.addSinkPartitioned
      nm
      topic
      (producedKeySerde p)
      (producedValueSerde p)
      sp
      parents


{- | @through@ is a sink + a fresh source on the same topic. The
runtime handles the loopback through the broker.
-}
throughTopic
  :: TopicName
  -> Produced k v
  -> KStream k v
  -> IO (KStream k v)
throughTopic topic p s = do
  toTopic topic p s
  let b = kstreamBuilder s
  streamFromTopic
    b
    topic
    (consumed (producedKeySerde p) (producedValueSerde p))


----------------------------------------------------------------------
-- Low-level
----------------------------------------------------------------------

----------------------------------------------------------------------
-- KStream-KTable join
----------------------------------------------------------------------

{- | Inner join: for each stream record whose key matches an
entry in the table, emit @joiner v_stream v_table@. Stream
records that find no match are dropped.

/JVM equivalent:/ @KStream.join(KTable, ValueJoiner)@.
-}
joinKStreamKTable
  :: forall k v vt v'
   . (Ord k, HasSerde v')
  => (v -> vt -> v')
  -> Joined k v vt
  -> KStream k v
  -> KTable k vt
  -> IO (KStream k v')
joinKStreamKTable joiner _j s0 tab = do
  -- A re-keyed stream must be co-partitioned with the table first.
  s <- repartitionIfKeyChanged Nothing s0
  let b = kstreamBuilder s
  nm <- freshNodeName b "KSTREAM-KTABLE-JOIN"
  -- Note on wiring: the join processor's only parent is the
  -- /stream/ side. The table side updates its store via the topology
  -- it was built with, /before/ any joiner evaluation, because the
  -- engine processes records FIFO across the whole task. The table
  -- store is read by the join processor via 'getStateStore'.
  withTopology_ b $ \t ->
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName = nm
        , Topo.processorSpecParents = [kstreamParent s]
        , Topo.processorSpecSupplier =
            Topo.AnyProcessor
              (joinKStreamKTableProc @k @v @vt @v' (ktableStore tab) joiner False)
        , Topo.processorSpecStores = [ktableStore tab]
        }
      t
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = nm
      , kstreamKeyChanged = False
      , kstreamKeySerde = kstreamKeySerde s
      , kstreamValueSerde = serde
      }


{- | Left join: stream records always emit, with @Nothing@ on
the right side when the table has no entry.

/JVM equivalent:/ @KStream.leftJoin(KTable, ValueJoiner)@.
-}
leftJoinKStreamKTable
  :: forall k v vt v'
   . (Ord k, HasSerde v')
  => (v -> Maybe vt -> v')
  -> Joined k v vt
  -> KStream k v
  -> KTable k vt
  -> IO (KStream k v')
leftJoinKStreamKTable joiner _j s0 tab = do
  s <- repartitionIfKeyChanged Nothing s0
  let b = kstreamBuilder s
  nm <- freshNodeName b "KSTREAM-KTABLE-LEFTJOIN"
  withTopology_ b $ \t ->
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName = nm
        , Topo.processorSpecParents = [kstreamParent s]
        , Topo.processorSpecSupplier =
            Topo.AnyProcessor
              (joinKStreamKTableProcL @k @v @vt @v' (ktableStore tab) joiner)
        , Topo.processorSpecStores = [ktableStore tab]
        }
      t
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = nm
      , kstreamKeyChanged = False
      , kstreamKeySerde = kstreamKeySerde s
      , kstreamValueSerde = serde
      }


{- | The join processor. The KTable side has already updated its store
before this processor sees a stream record (because the Topology
builder lists the table's update node as a parent — and KTable
updates flow before the join processor is invoked for the stream
side, courtesy of the engine's per-record FIFO).

Note: in the JVM impl, both sides are "co-partitioned" before
joining; here we operate within a single task so partitioning is
trivially equal. A real multi-task runtime would need to ensure
the upstream KTable store is reachable from the same task as the
KStream — which is exactly why the join is materialised against
the table's store name.
-}
joinKStreamKTableProc
  :: forall k v vt v'
   . Ord k
  => StoreName
  -> (v -> vt -> v')
  -> Bool -- left-join?
  -> IO (Processor k v)
joinKStreamKTableProc storeNm joiner _isLeft = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore k vt))
  pure
    Processor
      { procName = processorName "KSTREAM-KTABLE-JOIN"
      , procInit = \ctx -> do
          writeIORef ctxRef (Just ctx)
          st <- getStateStore ctx storeNm
          case st of
            Just (AnyKeyValueStore kvs) ->
              writeIORef storeRef (Just (Unsafe.unsafeCoerce kvs))
            _ -> error $ "join: store not found: " <> show storeNm
      , procClose = pure ()
      , procProcess = \r -> do
          mctx <- readIORef ctxRef
          mst <- readIORef storeRef
          case (mctx, mst, recordKey r) of
            (Just ctx, Just kvs, Just k) -> do
              mt <- kvsGet kvs k
              case mt of
                Just tv ->
                  let !v' = joiner (recordValue r) tv
                      !out =
                        Record
                          { recordKey = Just k
                          , recordValue = v'
                          , recordTimestamp = recordTimestamp r
                          , recordHeaders = recordHeaders r
                          }
                          :: Record k v'
                  in forwardRecord ctx out
                Nothing -> pure () -- inner-join drops
            _ -> pure ()
      }


joinKStreamKTableProcL
  :: forall k v vt v'
   . Ord k
  => StoreName
  -> (v -> Maybe vt -> v')
  -> IO (Processor k v)
joinKStreamKTableProcL storeNm joiner = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore k vt))
  pure
    Processor
      { procName = processorName "KSTREAM-KTABLE-LEFTJOIN"
      , procInit = \ctx -> do
          writeIORef ctxRef (Just ctx)
          st <- getStateStore ctx storeNm
          case st of
            Just (AnyKeyValueStore kvs) ->
              writeIORef storeRef (Just (Unsafe.unsafeCoerce kvs))
            _ -> error $ "leftJoin: store not found: " <> show storeNm
      , procClose = pure ()
      , procProcess = \r -> do
          mctx <- readIORef ctxRef
          mst <- readIORef storeRef
          case (mctx, mst, recordKey r) of
            (Just ctx, Just kvs, Just k) -> do
              mt <- kvsGet kvs k
              let !v' = joiner (recordValue r) mt
                  !out =
                    Record
                      { recordKey = Just k
                      , recordValue = v'
                      , recordTimestamp = recordTimestamp r
                      , recordHeaders = recordHeaders r
                      }
                      :: Record k v'
               in forwardRecord ctx out
            _ -> pure ()
      }


----------------------------------------------------------------------
-- KStream-KStream window join ("Kafka.Streams.Internal.WindowJoin")
----------------------------------------------------------------------

{- | Inner KStream-KStream window join. Mirrors
@KStream.join(other, ValueJoiner, JoinWindows)@: a left record at @t@
joins the right records in @[t - before, t + after]@ (and so a right
record joins the left ones in the mirror range); the result carries
the later timestamp. See "Kafka.Streams.Internal.WindowJoin".
-}
joinKStreamKStream
  :: forall k v1 v2 v'
   . Ord k
  => (v1 -> v2 -> v')
  -> JoinWindows
  -> Joined k v1 v2
  -> KStream k v1
  -> KStream k v2
  -> IO (KStream k v')
joinKStreamKStream joiner jw j sl sr =
  buildWindowJoin sl sr jw j "KSTREAM-WINDOWJOIN" (WindowJoinSpec joiner Nothing Nothing)


{- | Left KStream-KStream window join. Every left record is joined with
its matches, or, if none arrives before its window closes (plus
grace), emitted once with @Nothing@ then. Right records only
contribute matches.
-}
leftJoinKStreamKStream
  :: forall k v1 v2 v'
   . Ord k
  => (v1 -> Maybe v2 -> v')
  -> JoinWindows
  -> Joined k v1 v2
  -> KStream k v1
  -> KStream k v2
  -> IO (KStream k v')
leftJoinKStreamKStream joiner jw j sl sr =
  buildWindowJoin
    sl
    sr
    jw
    j
    "KSTREAM-WINDOWLEFTJOIN"
    (WindowJoinSpec (\v1 v2 -> joiner v1 (Just v2)) (Just (\v1 -> joiner v1 Nothing)) Nothing)


{- | Outer KStream-KStream window join: as 'leftJoinKStreamKStream' for
both sides.
-}
outerJoinKStreamKStream
  :: forall k v1 v2 v'
   . Ord k
  => (Maybe v1 -> Maybe v2 -> v')
  -> JoinWindows
  -> Joined k v1 v2
  -> KStream k v1
  -> KStream k v2
  -> IO (KStream k v')
outerJoinKStreamKStream joiner jw j sl sr =
  buildWindowJoin
    sl
    sr
    jw
    j
    "KSTREAM-WINDOWOUTERJOIN"
    ( WindowJoinSpec
        (\v1 v2 -> joiner (Just v1) (Just v2))
        (Just (\v1 -> joiner (Just v1) Nothing))
        (Just (\v2 -> joiner Nothing (Just v2)))
    )


-- | What a window join emits: matches, and (left / outer) unmatched records.
data WindowJoinSpec v1 v2 v' = WindowJoinSpec
  { wjsJoin :: v1 -> v2 -> v'
  , wjsLeftOnly :: Maybe (v1 -> v')
  , wjsRightOnly :: Maybe (v2 -> v')
  }


{- | The topology of a window join:

@
left  --> LEFT  (left store, reads right store) --\
                                                   +--> MERGE --> downstream
right --> RIGHT (right store, reads left store) --/
@

Left and outer joins add an outer store both sides share
("Kafka.Streams.Internal.WindowJoin"). The stores keep a record until
no record still in grace can join it: @before + after + 1 + grace@.
The 'Joined' serdes are the stores' serdes, and its name names the
nodes and stores (the JVM's @-this-join@ / @-other-join@ /
@-outer-shared-join-store@).
-}
buildWindowJoin
  :: forall k v1 v2 v'
   . Ord k
  => KStream k v1
  -> KStream k v2
  -> JoinWindows
  -> Joined k v1 v2
  -> T.Text
  -- ^ prefix
  -> WindowJoinSpec v1 v2 v'
  -> IO (KStream k v')
buildWindowJoin sl0 sr0 jw j prefix spec = do
  -- Both sides must be partitioned by the join key.
  sl <- repartitionIfKeyChanged Nothing sl0
  sr <- repartitionIfKeyChanged Nothing sr0
  let b = kstreamBuilder sl
      named suffix fresh = maybe fresh (\n -> pure (n <> suffix)) j.name
  leftStoreNm <- Kafka.Streams.State.Store.storeName <$> named "-this-join-store" (Kafka.Streams.State.Store.unStoreName <$> freshStoreName b (prefix <> "-LEFT-STORE"))
  rightStoreNm <- Kafka.Streams.State.Store.storeName <$> named "-other-join-store" (Kafka.Streams.State.Store.unStoreName <$> freshStoreName b (prefix <> "-RIGHT-STORE"))
  outerNm <- Kafka.Streams.State.Store.storeName <$> named "-outer-shared-join-store" (Kafka.Streams.State.Store.unStoreName <$> freshStoreName b (prefix <> "-OUTER-STORE"))
  leftNm <- Topo.NodeName <$> named "-this-join" (Topo.unNodeName <$> freshNodeName b (prefix <> "-LEFT"))
  rightNm <- Topo.NodeName <$> named "-other-join" (Topo.unNodeName <$> freshNodeName b (prefix <> "-RIGHT"))
  mergeNm <- Topo.NodeName <$> named "-merge" (Topo.unNodeName <$> freshNodeName b (prefix <> "-MERGE"))
  times <- WJ.newTimeTrackers
  let !sz = jw.beforeMs + jw.afterMs + 1
      !ret = sz + jw.gracePeriodMs
      hasOuter = isJust spec.wjsLeftOnly || isJust spec.wjsRightOnly
      outerStore = if hasOuter then Just outerNm else Nothing
      lsb = withStoreSerdesW j.keySerde (listSerde j.v1Serde) (inMemoryWindowStoreBuilder leftStoreNm sz ret) :: StoreBuilderW k [v1]
      rsb = withStoreSerdesW j.keySerde (listSerde j.v2Serde) (inMemoryWindowStoreBuilder rightStoreNm sz ret) :: StoreBuilderW k [v2]
      osb =
        withStoreSerdesKV
          (WJ.outerKeySerde j.keySerde)
          (listSerde (eitherSerde j.v1Serde j.v2Serde))
          (inMemoryKeyValueStoreBuilder outerNm)
      render = \case
        Left v1 -> ($ v1) <$> spec.wjsLeftOnly
        Right v2 -> ($ v2) <$> spec.wjsRightOnly
      side :: forall vs vo. WJ.Side -> Int64 -> Int64 -> StoreName -> StoreName -> (vs -> vo -> v') -> Maybe (vs -> v') -> (vs -> Either v1 v2) -> T.Text -> IO (Processor k vs)
      side s back ahead self_ other jn unmatched hold nm =
        WJ.sideProcessor
          WJ.SideConfig
            { WJ.scSide = s
            , WJ.scBack = back
            , WJ.scAhead = ahead
            , WJ.scBefore = jw.beforeMs
            , WJ.scAfter = jw.afterMs
            , WJ.scGrace = jw.gracePeriodMs
            , WJ.scJoin = jn
            , WJ.scUnmatched = unmatched
            , WJ.scHold = hold
            , WJ.scRender = render
            , WJ.scSelfStore = self_
            , WJ.scOtherStore = other
            , WJ.scOuterStore = outerStore
            , WJ.scTimes = times
            , WJ.scName = nm
            }
      stores = [leftStoreNm, rightStoreNm] <> maybe [] pure outerStore

  withTopology_ b $ \t ->
    let !t1 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = leftNm
              , Topo.processorSpecParents = [kstreamParent sl]
              , Topo.processorSpecSupplier =
                  Topo.AnyProcessor
                    (side WJ.LeftSide jw.beforeMs jw.afterMs leftStoreNm rightStoreNm spec.wjsJoin spec.wjsLeftOnly Left (prefix <> "-LEFT"))
              , Topo.processorSpecStores = stores
              }
            t
        !t2 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = rightNm
              , Topo.processorSpecParents = [kstreamParent sr]
              , Topo.processorSpecSupplier =
                  Topo.AnyProcessor
                    (side WJ.RightSide jw.afterMs jw.beforeMs rightStoreNm leftStoreNm (flip spec.wjsJoin) spec.wjsRightOnly Right (prefix <> "-RIGHT"))
              , Topo.processorSpecStores = stores
              }
            t1
        !t3 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = mergeNm
              , Topo.processorSpecParents = [leftNm, rightNm]
              , Topo.processorSpecSupplier =
                  Topo.AnyProcessor (mkPassThrough (prefix <> "-MERGE"))
              , Topo.processorSpecStores = []
              }
            t2
        !t4 = Topo.addStateStoreW lsb [leftNm, rightNm] t3
        !t5 = Topo.addStateStoreW rsb [leftNm, rightNm] t4
        -- an inner join of a stream with itself can run on one store
        selfJoin =
          Topo.SelfJoinRewrite
            { Topo.sjrRight = rightNm
            , Topo.sjrLeftStore = leftStoreNm
            , Topo.sjrRightStore = rightStoreNm
            , Topo.sjrSingle =
                Topo.AnyProcessor
                  ( WJ.selfJoinProcessor @k @v1
                      WJ.SelfJoinConfig
                        { WJ.sjBefore = jw.beforeMs
                        , WJ.sjAfter = jw.afterMs
                        , WJ.sjGrace = jw.gracePeriodMs
                        , -- both sides are one stream, so v2 is v1
                          WJ.sjJoin = \a b -> spec.wjsJoin a (Unsafe.unsafeCoerce b)
                        , WJ.sjStore = leftStoreNm
                        , WJ.sjTimes = times
                        , WJ.sjName = prefix <> "-SELF"
                        }
                  )
            }
    in if
         | hasOuter -> Topo.addStateStoreKV osb [leftNm, rightNm] t5
         | kstreamParent sl == kstreamParent sr -> Topo.addSelfJoinCandidate leftNm selfJoin t5
         | otherwise -> t5

  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = mergeNm
      , kstreamKeyChanged = False
      , kstreamKeySerde = j.keySerde
      , kstreamValueSerde =
          error
            "KStream-KStream join: downstream value Serde unset; supply via to/through"
      }


{- | Drop into the low-level Processor API for value-only state, with
access to the typed 'ProcessorContext'.
-}
transformValuesStream
  :: T.Text
  -- ^ Processor name prefix
  -> [Topo.NodeName]
  -- ^ Stores attached to this processor
  -> IO (Processor k v)
  -- ^ Processor supplier (already projects to v')
  -> Serde v'
  -- ^ Output value serde
  -> KStream k v
  -> IO (KStream k v')
transformValuesStream prefix _stores supplier vs s =
  attachProcessor s prefix supplier (kstreamKeySerde s) vs


----------------------------------------------------------------------
-- KStream -> KTable conversion
----------------------------------------------------------------------

{- | Convert a stream into a 'KTable' by materialising the latest
value per key into a state store. Mirrors @KStream.toTable@.

Tombstones (records with @recordValue = ...@ but explicitly null
in the wire form — represented in our model as a regular 'Record'
whose user code chose to encode nullness explicitly) are NOT
automatically interpreted as deletes here. If you need delete
semantics, route through 'mapValues' producing 'Maybe' and write
a tombstone-aware writer; the Java implementation is similarly
caller-aware.
-}
toTable
  :: forall k v
   . Ord k
  => Materialized k v
  -> KStream k v
  -> IO (KTable k v)
toTable m s = do
  let b = kstreamBuilder s
  storeNm <-
    maybe
      (freshStoreName b "KSTREAM-TOTABLE-STORE")
      pure
      (matName m)
  procNm <- freshNodeName b "KSTREAM-TOTABLE"
  let supplier =
        materializedKV m storeNm (kstreamKeySerde s) (kstreamValueSerde s)
          :: Kafka.Streams.State.Store.StoreBuilderKV k v
  withTopology_ b $ \t ->
    let !t1 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = procNm
              , Topo.processorSpecParents = [kstreamParent s]
              , Topo.processorSpecSupplier =
                  Topo.AnyProcessor (toTableProc @k @v storeNm)
              , Topo.processorSpecStores = []
              }
            t
        !t2 = Topo.addStateStoreKV supplier [procNm] t1
    in t2
  pure (mkKTableFromStream procNm storeNm b (kstreamKeySerde s) (kstreamValueSerde s))


mkKTableFromStream
  :: Topo.NodeName
  -> Kafka.Streams.State.Store.StoreName
  -> StreamsBuilder
  -> Serde k
  -> Serde v
  -> KTable k v
mkKTableFromStream nm sn b ks vs =
  -- KTable is a record with non-strict serde fields (we made them
  -- lazy so the builder can stitch deferred error placeholders).
  Kafka.Streams.KTable.KTable nm sn b ks vs


toTableProc
  :: forall k v
   . Kafka.Streams.State.Store.StoreName
  -> IO (Processor k v)
toTableProc sn = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore k v))
  pure
    Processor
      { procName = processorName "KSTREAM-TOTABLE"
      , procInit = \ctx -> do
          writeIORef ctxRef (Just ctx)
          getStateStore ctx sn >>= \case
            Just (AnyKeyValueStore kvs0) -> do
              let kvs = Unsafe.unsafeCoerce kvs0 :: KeyValueStore k v
              Kafka.Streams.KTable.bindTableCache ctx kvs
              writeIORef storeRef (Just kvs)
            _ -> error $ "toTable: store missing: " <> show sn
      , procClose = pure ()
      , procProcess = \r -> case recordKey r of
          Nothing -> pure ()
          Just k -> do
            mctx <- readIORef ctxRef
            mst <- readIORef storeRef
            case (mctx, mst) of
              (Just ctx, Just kvs) -> do
                old <- kvsGet kvs k
                Kafka.Streams.KTable.writeRow ctx kvs r k (Just (recordValue r)) old
              _ -> pure ()
      }


----------------------------------------------------------------------
-- Repartition
----------------------------------------------------------------------

{- | @KStream.repartition()@ with a topic name: route the stream
through an internal topic so every record is re-partitioned by its
current key. The topology gets a sink writing to
@\<name\>-repartition@ and a source reading it back; against a broker
the topic is @\<application.id\>-\<name\>-repartition@, created by the
runtime with as many partitions as the widest upstream source topic.
-}
repartition
  :: forall k v
   . T.Text
  -- ^ topic name (the @-repartition@ suffix is added)
  -> KStream k v
  -> IO (KStream k v)
repartition name s =
  repartitionThrough (Just name) Nothing (kstreamKeySerde s) (kstreamValueSerde s) Nothing s


{- | @KStream.repartition(Repartitioned)@: 'repartition' with the
'Repartitioned' overrides (topic name, partition count, serdes,
'StreamPartitioner'), all of which reach the broker runtime.
-}
repartitionWith
  :: KafkaStreamsRepartitioned.Repartitioned k v
  -> KStream k v
  -> IO (KStream k v)
repartitionWith cfg s =
  repartitionThrough
    cfg.name
    (fromIntegral <$> cfg.numberOfPartitions)
    (maybe (kstreamKeySerde s) id cfg.keySerde)
    (maybe (kstreamValueSerde s) id cfg.valueSerde)
    cfg.streamPartitioner
    s


{- | The repartition every grouping / join of a key-changed stream
needs: a sink to an internal topic and a source reading it back.
Anonymous repartitions take the fresh sink node's name.
-}
repartitionThrough
  :: Maybe T.Text
  -> Maybe Int
  -> Serde k
  -> Serde v
  -> Maybe (StreamPartitioner k v)
  -> KStream k v
  -> IO (KStream k v)
repartitionThrough mName nParts ks vs mPart s = do
  let b = kstreamBuilder s
  sinkNm <- freshNodeName b "KSTREAM-REPARTITION-SINK"
  srcNm <- freshNodeName b "KSTREAM-REPARTITION-SOURCE"
  let !base = maybe (Topo.unNodeName sinkNm) id mName
      !topic = Kafka.Streams.Types.topicName (base <> "-repartition")
  withTopology_ b $ \t ->
    let !t1 = Topo.addInternalTopic topic Topo.InternalTopicSpec {Topo.itsPartitions = nParts} t
        !t2 = case mPart of
          Nothing -> Topo.addSink sinkNm topic ks vs [kstreamParent s] t1
          Just sp -> Topo.addSinkPartitioned sinkNm topic ks vs sp [kstreamParent s] t1
        !t3 =
          Topo.addSource
            srcNm
            [topic]
            ks
            vs
            (consumedExtractor (consumed ks vs))
            t2
    in t3
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = srcNm
      , kstreamKeyChanged = False
      , kstreamKeySerde = ks
      , kstreamValueSerde = vs
      }


{- | Repartition @s@ first when an upstream operator changed its key
(what JVM Streams does before a grouping or a join); otherwise @s@
itself.
-}
repartitionIfKeyChanged :: Maybe T.Text -> KStream k v -> IO (KStream k v)
repartitionIfKeyChanged mName s
  | kstreamKeyChanged s = sharedRepartition mName s
  | otherwise = pure s


{- | @s@ through its repartition topic named @mName@, made once per
stream and name ('sharedRepartitionNode'): every later grouping or join
of @s@ reads the same topic.
-}
sharedRepartition :: Maybe T.Text -> KStream k v -> IO (KStream k v)
sharedRepartition mName s = do
  r <-
    sharedRepartitionNode (kstreamBuilder s) (kstreamParent s) mName $
      kstreamParent <$> repartitionThrough mName Nothing (kstreamKeySerde s) (kstreamValueSerde s) Nothing s
  case r of
    Left e -> throwIO (TopologyException e)
    Right src -> pure s {kstreamParent = src, kstreamKeyChanged = False}


----------------------------------------------------------------------
-- Split (KIP-418)
----------------------------------------------------------------------

{- | A named branch in a 'splitStream'. The runtime evaluates each
predicate in order; the first match routes the record to that
branch's named output. The 'Branched' carries an optional
continuation invoked /at build time/ on the resulting 'KStream'
so users can chain into per-branch sink/transform pipelines
without having to thread the result list manually.
-}
data Branched k v = Branched
  { branchedName :: !T.Text
  , branchedPred :: !(Record k v -> Bool)
  , branchedAct :: !(KStream k v -> IO ())
  }


{- | 'Branched' constructor with a pure predicate and no per-branch
continuation. Equivalent to Java's @Branched.as("name").withPredicate(p)@.
-}
branchedFrom :: T.Text -> (Record k v -> Bool) -> Branched k v
branchedFrom n p =
  Branched
    { branchedName = n
    , branchedPred = p
    , branchedAct = \_ -> pure ()
    }


{- | @Branched.withFunction(stream -> stream')@. Attach
a transform function that runs once on the branched stream;
the returned 'KStream' replaces the original in the result
map. Use this when each branch needs a different
downstream shape.
-}
withFunction
  :: T.Text
  -> (Record k v -> Bool)
  -> (KStream k v -> IO ())
  -> Branched k v
withFunction n p act =
  Branched
    { branchedName = n
    , branchedPred = p
    , branchedAct = act
    }


{- | @Branched.withConsumer(stream -> Unit)@. Consume
the branch's stream in place (for example: sink to a topic);
the branch doesn't appear in the result map. Same as
'withFunction' but the type spelling-out makes intent
explicit at the call site.
-}
withConsumer
  :: T.Text
  -> (Record k v -> Bool)
  -> (KStream k v -> IO ())
  -> Branched k v
withConsumer = withFunction


{- | Predicate-routed split: each input record is routed to the first
branch whose predicate matches. Records that match none are sent
to the optional default branch ('Nothing' drops them).

Returns a 'Map' from branch name to the resulting 'KStream', plus
(if requested) the default-branch stream.
-}
splitStream
  :: forall k v
   . [Branched k v]
  -- ^ branches in evaluation order
  -> Maybe T.Text
  -- ^ optional default-branch name
  -> KStream k v
  -> IO (Map.Map T.Text (KStream k v))
splitStream branches mDefault s = do
  let b = kstreamBuilder s
  router <- freshNodeName b "KSTREAM-SPLIT"
  let allBranches =
        branches
          ++ case mDefault of
            Just n -> [defaultBranch n]
            Nothing -> []
      defaultBranch n =
        Branched
          { branchedName = n
          , branchedPred = \_ -> True
          , branchedAct = \_ -> pure ()
          }
  childNodes <-
    mapM
      ( \br -> do
          n <- freshNodeName b ("KSTREAM-SPLIT-" <> branchedName br)
          pure (br, n)
      )
      allBranches
  withTopology_ b $ \t ->
    let !t1 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = router
              , Topo.processorSpecParents = [kstreamParent s]
              , Topo.processorSpecSupplier =
                  Topo.AnyProcessor
                    ( mkRouter
                        (map (branchedPred . fst) childNodes)
                        (map snd childNodes)
                    )
              , Topo.processorSpecStores = []
              }
            t
        !t2 =
          foldl
            ( \acc (_br, n) ->
                Topo.addProcessorWith
                  Topo.ProcessorSpec
                    { Topo.processorSpecName = n
                    , Topo.processorSpecParents = [router]
                    , Topo.processorSpecSupplier =
                        Topo.AnyProcessor (mkPassThrough "KSTREAM-SPLIT-CHILD")
                    , Topo.processorSpecStores = []
                    }
                  acc
            )
            t1
            childNodes
    in t2
  results <-
    mapM
      ( \(br, n) -> do
          let !sub =
                KStream
                  { kstreamBuilder = b
                  , kstreamParent = n
                  , kstreamKeyChanged = kstreamKeyChanged s
                  , kstreamKeySerde = kstreamKeySerde s
                  , kstreamValueSerde = kstreamValueSerde s
                  }
          branchedAct br sub
          pure (branchedName br, sub)
      )
      childNodes
  pure (Map.fromList results)


----------------------------------------------------------------------
-- Named variants (KIP-307)
----------------------------------------------------------------------

-- | 'filterStream' with an explicit topology node name.
filterStreamNamed
  :: forall k v
   . Kafka.Streams.Named.Named
  -> (Record k v -> Bool)
  -> KStream k v
  -> IO (KStream k v)
filterStreamNamed nm pred_ s = do
  let b = kstreamBuilder s
  nodeNm <- Kafka.Streams.Named.namedOr b nm "KSTREAM-FILTER"
  withTopology_ b $
    Topo.addProcessor
      nodeNm
      [kstreamParent s]
      (filterProcessor (Topo.unNodeName nodeNm) pred_)
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = nodeNm
      , kstreamKeyChanged = kstreamKeyChanged s
      , kstreamKeySerde = kstreamKeySerde s
      , kstreamValueSerde = kstreamValueSerde s
      }


-- | 'mapValues' with an explicit topology node name.
mapValuesNamed
  :: forall k v v'
   . HasSerde v'
  => Kafka.Streams.Named.Named
  -> (v -> v')
  -> KStream k v
  -> IO (KStream k v')
mapValuesNamed nm f s = do
  let b = kstreamBuilder s
  nodeNm <- Kafka.Streams.Named.namedOr b nm "KSTREAM-MAPVALUES"
  withTopology_ b $
    Topo.addProcessor nodeNm [kstreamParent s] (mapValuesProc (pure . f))
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = nodeNm
      , kstreamKeyChanged = kstreamKeyChanged s
      , kstreamKeySerde = kstreamKeySerde s
      , kstreamValueSerde = serde
      }


-- | 'mapKeyValue' with an explicit topology node name.
mapKeyValueNamed
  :: forall k v k' v'
   . (HasSerde k', HasSerde v')
  => Kafka.Streams.Named.Named
  -> (k -> v -> (k', v'))
  -> KStream k v
  -> IO (KStream k' v')
mapKeyValueNamed nm f s = do
  let b = kstreamBuilder s
  nodeNm <- Kafka.Streams.Named.namedOr b nm "KSTREAM-MAP"
  withTopology_ b $
    Topo.addProcessor
      nodeNm
      [kstreamParent s]
      (mapKVProc (\k v -> pure (f k v)))
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = nodeNm
      , kstreamKeyChanged = True
      , kstreamKeySerde = serde
      , kstreamValueSerde = serde
      }


-- | 'peekStream' with an explicit topology node name.
peekStreamNamed
  :: forall k v
   . Kafka.Streams.Named.Named
  -> (Record k v -> IO ())
  -> KStream k v
  -> IO (KStream k v)
peekStreamNamed nm act s = do
  let b = kstreamBuilder s
  nodeNm <- Kafka.Streams.Named.namedOr b nm "KSTREAM-PEEK"
  withTopology_ b $
    Topo.addProcessor nodeNm [kstreamParent s] $ do
      ctxRef <- newIORef Nothing
      pure
        Processor
          { procName = processorName "KSTREAM-PEEK"
          , procInit = \ctx -> writeIORef ctxRef (Just ctx)
          , procClose = pure ()
          , procProcess = \r -> do
              act r
              mctx <- readIORef ctxRef
              case mctx of
                Nothing -> pure ()
                Just ctx -> forwardRecord ctx r
          }
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = nodeNm
      , kstreamKeyChanged = kstreamKeyChanged s
      , kstreamKeySerde = kstreamKeySerde s
      , kstreamValueSerde = kstreamValueSerde s
      }


-- | 'selectKey' with an explicit topology node name.
selectKeyNamed
  :: forall k v k'
   . HasSerde k'
  => Kafka.Streams.Named.Named
  -> (Record k v -> k')
  -> KStream k v
  -> IO (KStream k' v)
selectKeyNamed nm f s = do
  let b = kstreamBuilder s
  nodeNm <- Kafka.Streams.Named.namedOr b nm "KSTREAM-SELECTKEY"
  withTopology_ b $
    Topo.addProcessor nodeNm [kstreamParent s] (selectKeyProc f)
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = nodeNm
      , kstreamKeyChanged = True
      , kstreamKeySerde = serde
      , kstreamValueSerde = kstreamValueSerde s
      }


-- | 'toTopic' with an explicit topology node name.
toTopicNamed
  :: forall k v
   . Kafka.Streams.Named.Named
  -> TopicName
  -> Produced k v
  -> KStream k v
  -> IO ()
toTopicNamed nm topic p s = do
  let b = kstreamBuilder s
  nodeNm <- Kafka.Streams.Named.namedOr b nm "KSTREAM-SINK"
  withTopology_ b $ producedSink nodeNm topic p [kstreamParent s]


----------------------------------------------------------------------
-- KIP-307 named variants — symmetric coverage
----------------------------------------------------------------------

-- | 'filterNotStream' with an explicit topology node name.
filterNotStreamNamed
  :: forall k v
   . Kafka.Streams.Named.Named
  -> (Record k v -> Bool)
  -> KStream k v
  -> IO (KStream k v)
filterNotStreamNamed nm pred_ = filterStreamNamed nm (not . pred_)


-- | 'mapValuesM' with an explicit topology node name.
mapValuesMNamed
  :: forall k v v'
   . HasSerde v'
  => Kafka.Streams.Named.Named
  -> (v -> IO v')
  -> KStream k v
  -> IO (KStream k v')
mapValuesMNamed nm f s = do
  let b = kstreamBuilder s
  nodeNm <- Kafka.Streams.Named.namedOr b nm "KSTREAM-MAPVALUES"
  withTopology_ b $
    Topo.addProcessor nodeNm [kstreamParent s] (mapValuesProc f)
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = nodeNm
      , kstreamKeyChanged = kstreamKeyChanged s
      , kstreamKeySerde = kstreamKeySerde s
      , kstreamValueSerde = serde
      }


-- | 'mapKeyValueM' with an explicit topology node name.
mapKeyValueMNamed
  :: forall k v k' v'
   . (HasSerde k', HasSerde v')
  => Kafka.Streams.Named.Named
  -> (k -> v -> IO (k', v'))
  -> KStream k v
  -> IO (KStream k' v')
mapKeyValueMNamed nm f s = do
  let b = kstreamBuilder s
  nodeNm <- Kafka.Streams.Named.namedOr b nm "KSTREAM-MAP"
  withTopology_ b $
    Topo.addProcessor nodeNm [kstreamParent s] (mapKVProc f)
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = nodeNm
      , kstreamKeyChanged = True
      , kstreamKeySerde = serde
      , kstreamValueSerde = serde
      }


-- | 'mapRecord' with an explicit topology node name.
mapRecordNamed
  :: forall k v k' v'
   . (HasSerde k', HasSerde v')
  => Kafka.Streams.Named.Named
  -> (Record k v -> Record k' v')
  -> KStream k v
  -> IO (KStream k' v')
mapRecordNamed nm f = mapRecordMNamed nm (pure . f)


-- | 'mapRecordM' with an explicit topology node name.
mapRecordMNamed
  :: forall k v k' v'
   . (HasSerde k', HasSerde v')
  => Kafka.Streams.Named.Named
  -> (Record k v -> IO (Record k' v'))
  -> KStream k v
  -> IO (KStream k' v')
mapRecordMNamed nm f s = do
  let b = kstreamBuilder s
  nodeNm <- Kafka.Streams.Named.namedOr b nm "KSTREAM-MAPRECORD"
  withTopology_ b $
    Topo.addProcessor nodeNm [kstreamParent s] (mapRecordProc f)
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = nodeNm
      , kstreamKeyChanged = True
      , kstreamKeySerde = serde
      , kstreamValueSerde = serde
      }


-- | 'concatMapValues' with an explicit topology node name.
concatMapValuesNamed
  :: forall k v v'
   . HasSerde v'
  => Kafka.Streams.Named.Named
  -> (v -> [v'])
  -> KStream k v
  -> IO (KStream k v')
concatMapValuesNamed nm f s = do
  let b = kstreamBuilder s
  nodeNm <- Kafka.Streams.Named.namedOr b nm "KSTREAM-FLATMAPVALUES"
  withTopology_ b $
    Topo.addProcessor nodeNm [kstreamParent s] (concatMapValuesProc f)
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = nodeNm
      , kstreamKeyChanged = kstreamKeyChanged s
      , kstreamKeySerde = kstreamKeySerde s
      , kstreamValueSerde = serde
      }


-- | 'concatMapKeyValue' with an explicit topology node name.
concatMapKeyValueNamed
  :: forall k v k' v'
   . (HasSerde k', HasSerde v')
  => Kafka.Streams.Named.Named
  -> (k -> v -> [(k', v')])
  -> KStream k v
  -> IO (KStream k' v')
concatMapKeyValueNamed nm f s = do
  let b = kstreamBuilder s
  nodeNm <- Kafka.Streams.Named.namedOr b nm "KSTREAM-FLATMAP"
  withTopology_ b $
    Topo.addProcessor nodeNm [kstreamParent s] (concatMapKVProc f)
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = nodeNm
      , kstreamKeyChanged = True
      , kstreamKeySerde = serde
      , kstreamValueSerde = serde
      }


-- | 'valuesStream' with an explicit topology node name.
valuesStreamNamed
  :: forall k v
   . Kafka.Streams.Named.Named
  -> KStream k v
  -> IO (KStream () v)
valuesStreamNamed nm s = do
  let b = kstreamBuilder s
  nodeNm <- Kafka.Streams.Named.namedOr b nm "KSTREAM-VALUES"
  withTopology_ b $
    Topo.addProcessor nodeNm [kstreamParent s] (selectKeyProc (const ()))
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = nodeNm
      , kstreamKeyChanged = True
      , kstreamKeySerde = serde
      , kstreamValueSerde = kstreamValueSerde s
      }


-- | 'mergeStreams' with an explicit topology node name.
mergeStreamsNamed
  :: Kafka.Streams.Named.Named
  -> KStream k v
  -> KStream k v
  -> IO (KStream k v)
mergeStreamsNamed nm a b = do
  let bld = kstreamBuilder a
  nodeNm <- Kafka.Streams.Named.namedOr bld nm "KSTREAM-MERGE"
  withTopology_ bld $ \t ->
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName = nodeNm
        , Topo.processorSpecParents = [kstreamParent a, kstreamParent b]
        , Topo.processorSpecSupplier =
            Topo.AnyProcessor (mkPassThrough "KSTREAM-MERGE")
        , Topo.processorSpecStores = []
        }
      t
  pure
    KStream
      { kstreamBuilder = bld
      , kstreamParent = nodeNm
      , kstreamKeyChanged = kstreamKeyChanged a || kstreamKeyChanged b
      , kstreamKeySerde = kstreamKeySerde a
      , kstreamValueSerde = kstreamValueSerde a
      }


{- | 'repartition' with an explicit topology node name. The name is
used both as the repartition-topic prefix and as the node name
of the synthetic repartition processor.
-}
repartitionNamed
  :: Kafka.Streams.Named.Named
  -> KStream k v
  -> IO (KStream k v)
repartitionNamed nm s =
  let prefix =
        case Kafka.Streams.Named.unNamed nm of
          Just n -> n
          Nothing -> "repartition"
  in repartition prefix s


----------------------------------------------------------------------
-- KIP-820 process / processValues
----------------------------------------------------------------------

{- | Drop into the low-level Processor API for /side-effecting/
processors that may or may not forward downstream. Mirrors
@KStream.process(ProcessorSupplier, stateStoreNames)@.

The supplied 'Processor' receives @Record k v@; if the user code
needs to forward records, it can do so via 'forwardRecord' on the
'ProcessorContext'. Returns 'Nothing'-typed wrapper since the
processor is a /sink/ from the DSL's perspective (just like
'foreach', but with state-store access).
-}
processStream
  :: forall k v
   . T.Text
  -- ^ node-name prefix
  -> [StoreName]
  -- ^ stores attached
  -> IO (Processor k v)
  -> KStream k v
  -> IO ()
processStream prefix stores supplier s = do
  let b = kstreamBuilder s
  nm <- freshNodeName b prefix
  withTopology_ b $
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName = nm
        , Topo.processorSpecParents = [kstreamParent s]
        , Topo.processorSpecSupplier = Topo.AnyProcessor supplier
        , Topo.processorSpecStores = stores
        }


{- | Like 'processStream' but the resulting node feeds a downstream
'KStream'. The supplied 'Processor' takes input @Record k v@ and
may forward records of type @Record k v'@ via 'ctxForward'; the
caller supplies the output value 'Serde'.
-}
processValuesStream
  :: forall k v v'
   . T.Text
  -> [StoreName]
  -> IO (Processor k v)
  -> Serde v'
  -> KStream k v
  -> IO (KStream k v')
processValuesStream prefix stores supplier vs s = do
  let b = kstreamBuilder s
  nm <- freshNodeName b prefix
  withTopology_ b $
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName = nm
        , Topo.processorSpecParents = [kstreamParent s]
        , Topo.processorSpecSupplier = Topo.AnyProcessor supplier
        , Topo.processorSpecStores = stores
        }
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = nm
      , kstreamKeyChanged = kstreamKeyChanged s
      , kstreamKeySerde = kstreamKeySerde s
      , kstreamValueSerde = vs
      }


----------------------------------------------------------------------
-- KTable -> KStream / KTable.groupBy
----------------------------------------------------------------------

{- | The table's updates as a stream: each row's new value (deletes
are not part of it; 'toChangelogStream' keeps them). Mirrors
@KTable.toStream@ for consumers that only want current values.
-}
toKStreamFromKTable
  :: forall k v
   . Kafka.Streams.KTable.KTable k v
  -> IO (KStream k v)
toKStreamFromKTable kt =
  tableChanges "KTABLE-TOSTREAM" kt (Kafka.Streams.KTable.ktableValueSerde kt) $ \ctx r -> case recordValue r of
    Kafka.Streams.KTable.Change (Just v) _ -> forwardRecord ctx r {recordValue = v}
    _ -> pure ()


{- | The table's changelog as a stream: 'Just' the new value, or
'Nothing' for a delete (JVM @KTable.toStream@ with its null values).
Write it with 'toTopicNullable' to keep deletes as tombstones.
-}
toChangelogStream
  :: forall k v
   . Kafka.Streams.KTable.KTable k v
  -> IO (KStream k (Maybe v))
toChangelogStream kt =
  tableChanges
    "KTABLE-TOCHANGELOG"
    kt
    (error "toChangelogStream: write it with toTopicNullable (its values are Maybe v)")
    $ \ctx r -> forwardRecord ctx r {recordValue = Kafka.Streams.KTable.changeNew (recordValue r :: Kafka.Streams.KTable.Change v)}


-- | A stream node fed a table's changes.
tableChanges
  :: forall k v w
   . T.Text
  -> Kafka.Streams.KTable.KTable k v
  -> Serde w
  -> (ProcessorContext -> Record k (Kafka.Streams.KTable.Change v) -> IO ())
  -> IO (KStream k w)
tableChanges label kt vs step = do
  let b = Kafka.Streams.KTable.ktableBuilder kt
  nm <- freshNodeName b label
  withTopology_ b $ \t ->
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName = nm
        , Topo.processorSpecParents = [Kafka.Streams.KTable.ktableNode kt]
        , Topo.processorSpecSupplier =
            Topo.AnyProcessor $ do
              ref <- newIORef Nothing
              pure
                Processor
                  { procName = processorName label
                  , procInit = writeIORef ref . Just
                  , procClose = pure ()
                  , procProcess = \r -> readIORef ref >>= mapM_ (`step` r)
                  }
        , Topo.processorSpecStores = []
        }
      t
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = nm
      , kstreamKeyChanged = False
      , kstreamKeySerde = Kafka.Streams.KTable.ktableKeySerde kt
      , kstreamValueSerde = vs
      }


----------------------------------------------------------------------
-- TopicNameExtractor (KIP-303)
----------------------------------------------------------------------

{- | Mirrors Java's @org.apache.kafka.streams.processor.TopicNameExtractor@:
decides the destination topic per-record. Receives the record plus the
source 'Kafka.Streams.Types.RecordMetadata' (when available via the
'ProcessorContext').
-}
newtype TopicNameExtractor k v = TopicNameExtractor
  { runTopicNameExtractor :: Record k v -> IO TopicName
  }


{- | Sink that routes each record to a topic determined by the
supplied 'TopicNameExtractor'. Mirrors @KStream.to(TopicNameExtractor, Produced)@.
-}
toExtracted
  :: forall k v
   . TopicNameExtractor k v
  -> Produced k v
  -> KStream k v
  -> IO ()
toExtracted ext p s = do
  let b = kstreamBuilder s
  nm <- freshNodeName b "KSTREAM-EXTRACTING-SINK"
  withTopology_ b $
    Topo.addProcessor nm [kstreamParent s] $ do
      ctxRef <- newIORef Nothing
      pure
        Processor
          { procName = processorName "KSTREAM-EXTRACTING-SINK"
          , procInit = \ctx -> writeIORef ctxRef (Just ctx)
          , procClose = pure ()
          , procProcess = \r -> do
              mctx <- readIORef ctxRef
              case mctx of
                Nothing -> pure ()
                Just ctx -> do
                  tp <- runTopicNameExtractor ext r
                  let kBytes =
                        fmap
                          (serialize (producedKeySerde p))
                          (recordKey r)
                      vBytes =
                        serialize
                          (producedValueSerde p)
                          (recordValue r)
                      -- Same header shape as a static sink: the
                      -- record's own headers, then the serdes'.
                      hdrs =
                        recordHeaders r
                          <> maybe
                            mempty
                            (serializeHeaders (producedKeySerde p))
                            (recordKey r)
                          <> serializeHeaders
                            (producedValueSerde p)
                            (recordValue r)
                  ctxEmitToTopic
                    ctx
                    Kafka.Streams.Processor.SinkEmit
                      { Kafka.Streams.Processor.seTopic =
                          Kafka.Streams.Types.unTopicName tp
                      , Kafka.Streams.Processor.seKey = kBytes
                      , Kafka.Streams.Processor.seValue = vBytes
                      , Kafka.Streams.Processor.seTimestamp = recordTimestamp r
                      , Kafka.Streams.Processor.seHeaders = hdrs
                      }
          }


----------------------------------------------------------------------
-- mergeStreamsN
----------------------------------------------------------------------

{- | Merge any number of streams into one. Equivalent to a left-fold
over 'mergeStreams' but emits a single merge node with all inputs
as parents — slightly cleaner than @foldr1 mergeStreams@ which
creates a chain.
-}
mergeStreamsN
  :: forall k v
   . [KStream k v]
  -> IO (KStream k v)
mergeStreamsN [] = error "mergeStreamsN: empty input"
mergeStreamsN [s] = pure s
mergeStreamsN (s : ss) = do
  let bld = kstreamBuilder s
  nm <- freshNodeName bld "KSTREAM-MERGE-N"
  withTopology_ bld $ \t ->
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName = nm
        , Topo.processorSpecParents = map kstreamParent (s : ss)
        , Topo.processorSpecSupplier =
            Topo.AnyProcessor (mkPassThrough "KSTREAM-MERGE-N")
        , Topo.processorSpecStores = []
        }
      t
  pure
    KStream
      { kstreamBuilder = bld
      , kstreamParent = nm
      , kstreamKeyChanged = any kstreamKeyChanged (s : ss)
      , kstreamKeySerde = kstreamKeySerde s
      , kstreamValueSerde = kstreamValueSerde s
      }


----------------------------------------------------------------------
-- Debugging sinks
----------------------------------------------------------------------

{- | Print every record to standard out using @show@. Mirrors
@KStream.print(Printed.toSysOut())@.
-}
printStream :: (Show k, Show v) => KStream k v -> IO ()
printStream = printToHandle "[stream]" putStrLn


{- | Print every record using a caller-supplied @putLine@ function.
Lets users redirect to a logger / file handle without touching
'System.IO'.
-}
printToHandle
  :: (Show k, Show v)
  => T.Text -- prefix label
  -> (String -> IO ()) -- write a line
  -> KStream k v
  -> IO ()
printToHandle label putLine s =
  foreachStream
    ( \r ->
        putLine $
          T.unpack label
            <> " key="
            <> show (recordKey r)
            <> " value="
            <> show (recordValue r)
    )
    s


----------------------------------------------------------------------
-- KStream.values: emit only the value side
----------------------------------------------------------------------

{- | Drop the key from every record. Mirrors @KStream.values()@ —
equivalent to @mapKeyValue (\_ v -> ((), v))@ followed by a
consumer that ignores the unit key.
-}
valuesStream
  :: forall k v
   . KStream k v
  -> IO (KStream () v)
valuesStream s = do
  let b = kstreamBuilder s
  nm <- freshNodeName b "KSTREAM-VALUES"
  withTopology_ b $
    Topo.addProcessor nm [kstreamParent s] (mapKVProc (\_ v -> pure ((), v)))
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = nm
      , kstreamKeyChanged = True
      , kstreamKeySerde = serde
      , kstreamValueSerde = kstreamValueSerde s
      }


----------------------------------------------------------------------
-- concatTransformValues
----------------------------------------------------------------------

{- | Stateful 0-to-many emit on the value side. The user-supplied
'Processor' is free to call 'forwardRecord' (or 'ctxForward')
zero, one, or many times per input record. State stores listed
in @stores@ are attached and accessible via 'getStateStore'.

Mirrors @KStream.concatTransformValues@. Identical to
'processValuesStream' (same plumbing); kept as a separate name
for users porting from Java.
-}
concatTransformValues
  :: forall k v v'
   . T.Text
  -> [StoreName]
  -> IO (Processor k v)
  -> Serde v'
  -> KStream k v
  -> IO (KStream k v')
concatTransformValues = processValuesStream
