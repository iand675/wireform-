{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.KStream
-- Description : The KStream DSL surface
--
-- @
-- KStream k v
-- @
--
-- is a never-ending sequence of @Record k v@ values flowing along a
-- single edge of the topology graph. Operations on a 'KStream'
-- register a new processor as a child of the upstream node and
-- return a 'KStream' pinned to that new node.
--
-- The DSL mirrors the Java methods on
-- @org.apache.kafka.streams.kstream.KStream<K,V>@:
--
--   * 'streamFromTopic'         — Java @StreamsBuilder.stream@
--   * 'filterStream'            — @KStream.filter@
--   * 'filterNotStream'         — @KStream.filterNot@
--   * 'mapValues'               — @KStream.mapValues@
--   * 'mapKeyValue'             — @KStream.map@
--   * 'concatMapValues'           — @KStream.concatMapValues@
--   * 'concatMapKeyValue'         — @KStream.flatMap@
--   * 'foreachStream'           — @KStream.foreach@
--   * 'peekStream'              — @KStream.peek@
--   * 'selectKey'               — @KStream.selectKey@
--   * 'mergeStreams'            — @KStream.merge@
--   * 'branchStream'            — @KStream.branch@
--   * 'toTopic'                 — @KStream.to@
--   * 'throughTopic'            — @KStream.through@
--   * 'groupByKey'              — @KStream.groupByKey@
--   * 'groupByStream'           — @KStream.groupBy@
--   * 'transformValuesStream'   — @KStream.transformValues@
module Kafka.Streams.KStream
  ( KStream (..)
  , kstreamParent
  , kstreamBuilder
  , kstreamKeySerde
  , kstreamValueSerde
    -- * Sources
  , streamFromTopic
  , streamFromPattern
    -- * Stateless transforms
  , filterStream
  , filterStreamNamed
  , filterNotStream
  , filterNotStreamNamed
  , mapValues
  , mapValuesWith
  , mapValuesNamed
  , mapValuesM
  , mapValuesMWith
  , mapValuesMNamed
  , mapKeyValue
  , mapKeyValueWith
  , mapKeyValueNamed
  , mapKeyValueM
  , mapKeyValueMWith
  , mapKeyValueMNamed
  , mapRecord
  , mapRecordWith
  , mapRecordNamed
  , mapRecordM
  , mapRecordMWith
  , mapRecordMNamed
  , concatMapValues
  , concatMapValuesWith
  , concatMapValuesNamed
  , concatMapKeyValue
  , concatMapKeyValueWith
  , concatMapKeyValueNamed
  , peekStream
  , peekStreamNamed
  , foreachStream
  , foreachStreamAsync
  , printStream
  , printToHandle
  , valuesStream
  , valuesStreamNamed
  , selectKey
  , selectKeyWith
  , selectKeyNamed
    -- * Composition
  , mergeStreams
  , mergeStreamsNamed
  , mergeStreamsN
  , branchStream
    -- * Sinks
  , toTopic
  , toTopicNamed
  , toExtracted
  , TopicNameExtractor (..)
  , throughTopic
    -- * Conversions
  , toTable
  , repartition
  , repartitionNamed
  , repartitionWith
  , toKStreamFromKTable
  , groupByKTable
    -- * Joins
  , joinKStreamKTable
  , leftJoinKStreamKTable
  , joinKStreamKStream
  , leftJoinKStreamKStream
  , outerJoinKStreamKStream
    -- * Branching
  , splitStream
  , Branched (..)
  , branchedFrom
  , withFunction
  , withConsumer
    -- * Low-level access
  , transformValuesStream
  , processStream
  , processValuesStream
  , concatTransformValues
    -- * Extension hooks
  --
  -- 'attachProcessor' is the primitive every stateless and stateful
  -- transform on this module is built on. It is exported so the
  -- 'Kafka.Streams.AsyncIO' family (and any future
  -- 'wireform-kafka-streams' Riffle extension) can plug in fresh
  -- processor primitives without duplicating the
  -- "fresh node name + addProcessor + return a KStream" boilerplate.
  , attachProcessor
  ) where

import qualified Control.Concurrent.Async
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Kafka.Streams.Consumed
  ( Consumed (..)
  , consumed
  )
import Kafka.Streams.Joined
  ( JoinWindows (..)
  , Joined (..)
  )
import qualified Kafka.Streams.Named
import Kafka.Streams.Produced
  ( Produced (..)
  , produced
  )
import qualified Kafka.Streams.Repartitioned as KafkaStreamsRepartitioned
import Kafka.Streams.StreamsBuilder
  ( StreamsBuilder
  , freshNodeName
  , freshStoreName
  , withTopology_
  )

import qualified Unsafe.Coerce as Unsafe

import qualified Kafka.Streams.KTable
import Kafka.Streams.KTable
  ( KTable (..)
  , ktableNode
  , ktableStore
  )
import Kafka.Streams.Materialized
  ( Materialized (..)
  )
import qualified Kafka.Streams.State.Store
import qualified Kafka.Streams.Processor
import Kafka.Streams.Processor
  ( Processor (..)
  , ProcessorContext (..)
  , forwardRecord
  , forwardTo
  , getStateStore
  , processorName
  )
import qualified Kafka.Streams.Types
import Kafka.Streams.Serde (HasSerde (..), Serde, serialize)
import Kafka.Streams.State.KeyValue.InMemory
  ( inMemoryKeyValueStoreBuilder
  )
import Kafka.Streams.State.Store
  ( AnyStateStore (..)
  , KeyValueStore (..)
  , StoreBuilderW
  , StoreName
  , WindowStore (..)
  , kvIteratorToList
  )
import Kafka.Streams.State.Window.InMemory
  ( inMemoryWindowStoreBuilder
  )
import Kafka.Streams.Time (Timestamp (..))
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Types
  ( Record (..)
  , TopicName
  , mapValue
  )

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

-- | A handle to a stream node in the topology being built.
--
-- Note: the serde fields are intentionally lazy. After a 'mapValues'
-- the downstream value serde is unknown until the user attaches a
-- 'Produced' / 'Materialized' downstream — we encode that as a
-- thunk that errors out only if forced.
data KStream k v = KStream
  { kstreamBuilder    :: !StreamsBuilder
  , kstreamParent     :: !Topo.NodeName
  , kstreamKeySerde   :: ~(Serde k)
  , kstreamValueSerde :: ~(Serde v)
  }

----------------------------------------------------------------------
-- Sources
----------------------------------------------------------------------

-- | Build a 'KStream' that subscribes to records from the
-- given topic. The 'Consumed' record carries the key / value
-- serdes and an optional explicit source node name.
--
-- /JVM equivalent:/ @StreamsBuilder.stream(topic, Consumed)@.
streamFromTopic
  :: StreamsBuilder
  -> TopicName
  -> Consumed k v
  -> IO (KStream k v)
streamFromTopic b topic c = do
  nm <- maybe (freshNodeName b "KSTREAM-SOURCE")
              (pure . Topo.NodeName)
              (consumedNodeName c)
  withTopology_ b $
    Topo.addSourceWith
      Topo.SourceSpec
        { Topo.sourceName        = nm
        , Topo.sourceTopics      = [topic]
        , Topo.sourceKeySerde    = Topo.AnySerde (consumedKeySerde c)
        , Topo.sourceValueSerde  = Topo.AnySerde (consumedValueSerde c)
        , Topo.sourceExtractor   = Topo.AnyTimestampExtractor (consumedExtractor c)
        , Topo.sourceOffsetReset = consumedOffsetReset c
        , Topo.sourcePattern     = Nothing
        , Topo.sourceWatermarkStrategy = consumedWatermark c
        }
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nm
    , kstreamKeySerde   = consumedKeySerde c
    , kstreamValueSerde = consumedValueSerde c
    }

-- | Subscribe to every broker topic matching the supplied
-- regex pattern (JVM @StreamsBuilder.stream(Pattern)@).
--
-- The @Text@ is interpreted as a Java-style regex on the
-- broker. The in-process 'TopologyTestDriver' doesn't enumerate
-- broker topics, so within the driver this is equivalent to a
-- source with /no/ explicit topics — pipe in records via
-- 'pipeInput' with topics that match downstream consumers'
-- expectations. Against a real broker, the runtime resolves
-- topics matching the pattern at subscribe time.
streamFromPattern
  :: StreamsBuilder
  -> T.Text
  -> Consumed k v
  -> IO (KStream k v)
streamFromPattern b pattern c = do
  nm <- maybe (freshNodeName b "KSTREAM-SOURCE-PATTERN")
              (pure . Topo.NodeName)
              (consumedNodeName c)
  withTopology_ b $
    Topo.addSourceWith
      Topo.SourceSpec
        { Topo.sourceName        = nm
        , Topo.sourceTopics      = []
        , Topo.sourceKeySerde    = Topo.AnySerde (consumedKeySerde c)
        , Topo.sourceValueSerde  = Topo.AnySerde (consumedValueSerde c)
        , Topo.sourceExtractor   = Topo.AnyTimestampExtractor (consumedExtractor c)
        , Topo.sourceOffsetReset = consumedOffsetReset c
        , Topo.sourcePattern     = Just pattern
        , Topo.sourceWatermarkStrategy = consumedWatermark c
        }
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nm
    , kstreamKeySerde   = consumedKeySerde c
    , kstreamValueSerde = consumedValueSerde c
    }

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

-- | Internal: register a new processor whose supplier creates a
-- 'Processor' whose /input/ matches @parent@. The new 'KStream'
-- carries the /output/ types declared by the supplied serdes.
--
-- The processor is responsible for forwarding records of the right
-- shape via 'forwardRecord' / 'ctxForward' — the engine itself is
-- type-erased.
attachProcessor
  :: KStream k v
  -> T.Text
  -> IO (Processor k v)
  -> Serde k'
  -> Serde v'
  -> IO (KStream k' v')
attachProcessor parent prefix supplier ks' vs' = do
  let b = kstreamBuilder parent
  nm <- freshNodeName b prefix
  withTopology_ b $
    Topo.addProcessor nm [kstreamParent parent] supplier
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nm
    , kstreamKeySerde   = ks'
    , kstreamValueSerde = vs'
    }

-- | Build a stateless 'Processor' that runs @f@ on each record and
-- forwards the result via 'ProcessorContext.forward'.
statelessProcessorM
  :: T.Text
  -> (Record k v -> Record k' v')
  -> IO (Processor k v)
statelessProcessorM nm f = do
  ctxRef <- newIORef Nothing
  pure Processor
    { procName    = processorName nm
    , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case mctx of
          Nothing  -> pure ()
          Just ctx -> forwardRecord ctx (f r)
    }

----------------------------------------------------------------------
-- Stateless transforms
----------------------------------------------------------------------

-- | Keep records for which the predicate returns 'True'.
--
-- /JVM equivalent:/ @KStream.filter(Predicate)@.
filterStream :: (Record k v -> Bool) -> KStream k v -> IO (KStream k v)
filterStream pred_ s =
  attachProcessor s "KSTREAM-FILTER"
    (filterProcessor "KSTREAM-FILTER" pred_)
    (kstreamKeySerde s) (kstreamValueSerde s)

-- | Drop records for which the predicate returns 'True'; the
-- inverse of 'filterStream'.
--
-- /JVM equivalent:/ @KStream.filterNot(Predicate)@.
filterNotStream :: (Record k v -> Bool) -> KStream k v -> IO (KStream k v)
filterNotStream pred_ = filterStream (not . pred_)

filterProcessor
  :: T.Text -> (Record k v -> Bool) -> IO (Processor k v)
filterProcessor nm p = do
  ctxRef <- newIORef Nothing
  pure Processor
    { procName    = processorName nm
    , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case mctx of
          Nothing  -> pure ()
          Just ctx -> if p r then forwardRecord ctx r else pure ()
    }

-- | Apply a pure function to every record's value.
--
-- /JVM equivalent:/ @KStream.mapValues(ValueMapper)@.
mapValues
  :: forall k v v'
   . HasSerde v'
  => (v -> v') -> KStream k v -> IO (KStream k v')
mapValues f s = mapValuesMWith serde (pure . f) s

-- | 'mapValues' with an explicit downstream value 'Serde'.
mapValuesWith
  :: forall k v v'
   . Serde v'
  -> (v -> v') -> KStream k v -> IO (KStream k v')
mapValuesWith vs' f s = mapValuesMWith vs' (pure . f) s

-- | Apply an effectful function to every record's value. The
-- 'IO' action runs once per record on the stream thread; it
-- mustn't block on external systems for long, since that
-- stalls the partition.
--
-- /JVM equivalent:/ no direct match — closest is the
-- @ValueMapper@ + a separate side-effect channel. We model
-- the @IO@ in the type signature so the cost is visible.
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
  attachProcessor s "KSTREAM-MAPVALUES"
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
  pure Processor
    { procName    = processorName "KSTREAM-MAPVALUES"
    , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case mctx of
          Nothing  -> pure ()
          Just ctx -> do
            !v' <- f (recordValue r)
            let !out = Record
                  { recordKey       = recordKey r
                  , recordValue     = v'
                  , recordTimestamp = recordTimestamp r
                  , recordHeaders   = recordHeaders r
                  } :: Record k v'
            forwardRecord ctx out
    }

-- | Re-key + re-value every record with a pure function. The
-- new key feeds into downstream partitioning, so if you intend
-- to consume the result with a stateful op you usually want to
-- 'repartition' afterwards.
--
-- /JVM equivalent:/ @KStream.map(KeyValueMapper)@.
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
  attachProcessor s "KSTREAM-MAP"
    (mapKVProc @k @v @k' @v' f)
    ks'
    vs'

mapKVProc
  :: forall k v k' v'
   . (k -> v -> IO (k', v')) -> IO (Processor k v)
mapKVProc f = do
  ctxRef <- newIORef Nothing
  pure Processor
    { procName    = processorName "KSTREAM-MAP"
    , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case (mctx, recordKey r) of
          (Just ctx, Just k) -> do
            (!k', !v') <- f k (recordValue r)
            let !out = Record
                  { recordKey       = Just k'
                  , recordValue     = v'
                  , recordTimestamp = recordTimestamp r
                  , recordHeaders   = recordHeaders r
                  } :: Record k' v'
            forwardRecord ctx out
          _ -> pure ()
    }

-- | Apply a pure transform to the entire 'Record' — key, value,
-- timestamp, and headers all in scope. Useful when you need to
-- mutate metadata (e.g. propagate a trace header) alongside the
-- key/value transform that 'mapKeyValue' covers.
--
-- /JVM equivalent:/ no single direct match — closest is
-- @KStream.transform@ with a stateless transformer. The Java
-- DSL forces callers through the Processor API; we expose the
-- common stateless shape as a smart constructor.
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

-- | Effectful 'mapRecord'. The 'IO' runs once per record on the
-- stream thread — same backpressure caveat as 'mapValuesM'.
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
  attachProcessor s "KSTREAM-MAPRECORD"
    (mapRecordProc f)
    ks'
    vs'

mapRecordProc
  :: forall k v k' v'
   . (Record k v -> IO (Record k' v'))
  -> IO (Processor k v)
mapRecordProc f = do
  ctxRef <- newIORef Nothing
  pure Processor
    { procName    = processorName "KSTREAM-MAPRECORD"
    , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case mctx of
          Nothing  -> pure ()
          Just ctx -> do
            !r' <- f r
            forwardRecord ctx (r' :: Record k' v')
    }

-- | Expand each record into zero or more output records,
-- changing only the value.
--
-- /JVM equivalent:/ @KStream.concatMapValues(ValueMapper)@.
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
  attachProcessor s "KSTREAM-FLATMAPVALUES"
    (concatMapValuesProc @k @v @v' f)
    (kstreamKeySerde s)
    vs'

concatMapValuesProc
  :: forall k v v'
   . (v -> [v']) -> IO (Processor k v)
concatMapValuesProc f = do
  ctxRef <- newIORef Nothing
  pure Processor
    { procName    = processorName "KSTREAM-FLATMAPVALUES"
    , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case mctx of
          Nothing  -> pure ()
          Just ctx ->
            mapM_
              (\v' ->
                 let !out = Record
                       { recordKey       = recordKey r
                       , recordValue     = v'
                       , recordTimestamp = recordTimestamp r
                       , recordHeaders   = recordHeaders r
                       } :: Record k v'
                  in forwardRecord ctx out)
              (f (recordValue r))
    }

-- | Expand each record into zero or more output records,
-- changing both key and value. As with 'mapKeyValue', the new
-- key feeds downstream partitioning.
--
-- /JVM equivalent:/ @KStream.flatMap(KeyValueMapper)@.
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
  attachProcessor s "KSTREAM-FLATMAP"
    (concatMapKVProc @k @v @k' @v' f)
    ks'
    vs'

concatMapKVProc
  :: forall k v k' v'
   . (k -> v -> [(k', v')])
  -> IO (Processor k v)
concatMapKVProc f = do
  ctxRef <- newIORef Nothing
  pure Processor
    { procName    = processorName "KSTREAM-FLATMAP"
    , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case (mctx, recordKey r) of
          (Just ctx, Just k) ->
            mapM_
              (\(k', v') ->
                 let !out = Record
                       { recordKey       = Just k'
                       , recordValue     = v'
                       , recordTimestamp = recordTimestamp r
                       , recordHeaders   = recordHeaders r
                       } :: Record k' v'
                  in forwardRecord ctx out)
              (f k (recordValue r))
          _ -> pure ()
    }

-- | Run a side-effecting action on every record without
-- changing the stream. Useful for metrics / logging / debug
-- output.
--
-- /JVM equivalent:/ @KStream.peek(ForeachAction)@.
peekStream
  :: (Record k v -> IO ())
  -> KStream k v
  -> IO (KStream k v)
peekStream act s =
  attachProcessor s "KSTREAM-PEEK"
    (do
      ctxRef <- newIORef Nothing
      pure Processor
        { procName    = processorName "KSTREAM-PEEK"
        , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
        , procClose   = pure ()
        , procProcess = \r -> do
            act r
            mctx <- readIORef ctxRef
            case mctx of
              Nothing  -> pure ()
              Just ctx -> forwardRecord ctx r
        })
    (kstreamKeySerde s) (kstreamValueSerde s)

-- | Non-blocking 'foreachStream': forks each callback into a
-- detached 'Control.Concurrent.async' so a slow handler can't
-- back-pressure the worker thread. Use when the side-effect
-- is best-effort (logging, metrics emission, etc.) and you can
-- tolerate ordering loss across records.
--
-- For ordered effects use 'foreachStream' instead.
foreachStreamAsync
  :: (Record k v -> IO ())
  -> KStream k v
  -> IO ()
foreachStreamAsync act s =
  foreachStream
    (\r -> do
       _ <- Control.Concurrent.Async.async (act r)
       pure ())
    s

-- | Terminal sink for side effects: every record runs through
-- the supplied action and is then dropped (no downstream
-- stream).
--
-- /JVM equivalent:/ @KStream.foreach(ForeachAction)@.
foreachStream :: (Record k v -> IO ()) -> KStream k v -> IO ()
foreachStream act s = do
  let b = kstreamBuilder s
  nm <- freshNodeName b "KSTREAM-FOREACH"
  withTopology_ b $
    Topo.addProcessor nm [kstreamParent s] $ do
      pure Processor
        { procName    = processorName "KSTREAM-FOREACH"
        , procInit    = \_ -> pure ()
        , procClose   = pure ()
        , procProcess = act
        }

-- | Re-key the stream by deriving a new key from the record
-- (key, value, timestamp, headers). Use 'repartition' after
-- this if a stateful op consumes the result, otherwise the
-- new key won't drive partitioning correctly.
--
-- /JVM equivalent:/ @KStream.selectKey(KeyValueMapper)@.
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
  attachProcessor s "KSTREAM-SELECTKEY"
    (selectKeyProc @k @v @k' f)
    ks'
    (kstreamValueSerde s)

selectKeyProc
  :: forall k v k'
   . (Record k v -> k') -> IO (Processor k v)
selectKeyProc f = do
  ctxRef <- newIORef Nothing
  pure Processor
    { procName    = processorName "KSTREAM-SELECTKEY"
    , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case mctx of
          Nothing  -> pure ()
          Just ctx ->
            let !out = Record
                  { recordKey       = Just (f r)
                  , recordValue     = recordValue r
                  , recordTimestamp = recordTimestamp r
                  , recordHeaders   = recordHeaders r
                  } :: Record k' v
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
        { Topo.processorSpecName     = nm
        , Topo.processorSpecParents  = [kstreamParent a, kstreamParent b]
        , Topo.processorSpecSupplier = Topo.AnyProcessor (mkPassThrough "KSTREAM-MERGE")
        , Topo.processorSpecStores   = []
        }
      t
  pure KStream
    { kstreamBuilder    = bld
    , kstreamParent     = nm
    , kstreamKeySerde   = kstreamKeySerde a
    , kstreamValueSerde = kstreamValueSerde a
    }

mkPassThrough :: T.Text -> IO (Processor k v)
mkPassThrough nm = do
  ctxRef <- newIORef Nothing
  pure Processor
    { procName    = processorName nm
    , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case mctx of
          Nothing  -> pure ()
          Just ctx -> forwardRecord ctx r
    }

-- | Branch a stream into N substreams using the supplied
-- predicates. Records are routed to the first matching
-- predicate; non-matching records are dropped.
--
-- /JVM equivalent:/ @KStream.split().branch(Predicate, ...)@.
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
  branches <- mapM (\_ ->
                      freshNodeName b "KSTREAM-BRANCH-CHILD") preds
  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = router
                  , Topo.processorSpecParents  = [kstreamParent s]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor (mkRouter preds branches)
                  , Topo.processorSpecStores   = []
                  } t
        !t2 = foldl
                (\acc bn ->
                  Topo.addProcessorWith
                    Topo.ProcessorSpec
                      { Topo.processorSpecName     = bn
                      , Topo.processorSpecParents  = [router]
                      , Topo.processorSpecSupplier =
                          Topo.AnyProcessor
                            (mkPassThrough "KSTREAM-BRANCH-CHILD")
                      , Topo.processorSpecStores   = []
                      } acc)
                t1 branches
     in t2
  pure
    [ KStream
        { kstreamBuilder    = b
        , kstreamParent     = bn
        , kstreamKeySerde   = kstreamKeySerde s
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
  pure Processor
    { procName = processorName "KSTREAM-BRANCH"
    , procInit = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case mctx of
          Nothing  -> pure ()
          Just ctx -> dispatch ctx (zip preds branches) r
    }
  where
    dispatch _   []                  _ = pure ()
    dispatch ctx ((p, target):rest) r =
      if p r
        then forwardTo ctx target r
        else dispatch ctx rest r

----------------------------------------------------------------------
-- Sinks
----------------------------------------------------------------------

-- | Publish every record on the stream to the given topic.
-- The 'Produced' record carries the key / value serdes and an
-- optional explicit sink node name.
--
-- /JVM equivalent:/ @KStream.to(topic, Produced)@.
toTopic
  :: TopicName
  -> Produced k v
  -> KStream k v
  -> IO ()
toTopic topic p s = do
  let b = kstreamBuilder s
  nm <- maybe (freshNodeName b "KSTREAM-SINK")
              (pure . Topo.NodeName)
              (producedName p)
  withTopology_ b $
    Topo.addSink nm topic
                 (producedKeySerde p) (producedValueSerde p)
                 [kstreamParent s]

-- | @through@ is a sink + a fresh source on the same topic. The
-- runtime handles the loopback through the broker.
throughTopic
  :: TopicName
  -> Produced k v
  -> KStream k v
  -> IO (KStream k v)
throughTopic topic p s = do
  toTopic topic p s
  let b = kstreamBuilder s
  streamFromTopic b topic
    (consumed (producedKeySerde p) (producedValueSerde p))

----------------------------------------------------------------------
-- Low-level
----------------------------------------------------------------------

----------------------------------------------------------------------
-- KStream-KTable join
----------------------------------------------------------------------

-- | Inner join: for each stream record whose key matches an
-- entry in the table, emit @joiner v_stream v_table@. Stream
-- records that find no match are dropped.
--
-- /JVM equivalent:/ @KStream.join(KTable, ValueJoiner)@.
joinKStreamKTable
  :: forall k v vt v'
   . (Ord k, HasSerde v')
  => (v -> vt -> v')
  -> Joined k v vt
  -> KStream k v
  -> KTable k vt
  -> IO (KStream k v')
joinKStreamKTable joiner _j s tab = do
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
        { Topo.processorSpecName     = nm
        , Topo.processorSpecParents  = [kstreamParent s]
        , Topo.processorSpecSupplier =
            Topo.AnyProcessor
              (joinKStreamKTableProc @k @v @vt @v' (ktableStore tab) joiner False)
        , Topo.processorSpecStores   = [ktableStore tab]
        }
      t
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nm
    , kstreamKeySerde   = kstreamKeySerde s
    , kstreamValueSerde = serde
    }

-- | Left join: stream records always emit, with @Nothing@ on
-- the right side when the table has no entry.
--
-- /JVM equivalent:/ @KStream.leftJoin(KTable, ValueJoiner)@.
leftJoinKStreamKTable
  :: forall k v vt v'
   . (Ord k, HasSerde v')
  => (v -> Maybe vt -> v')
  -> Joined k v vt
  -> KStream k v
  -> KTable k vt
  -> IO (KStream k v')
leftJoinKStreamKTable joiner _j s tab = do
  let b = kstreamBuilder s
  nm <- freshNodeName b "KSTREAM-KTABLE-LEFTJOIN"
  withTopology_ b $ \t ->
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName     = nm
        , Topo.processorSpecParents  = [kstreamParent s]
        , Topo.processorSpecSupplier =
            Topo.AnyProcessor
              (joinKStreamKTableProcL @k @v @vt @v' (ktableStore tab) joiner)
        , Topo.processorSpecStores   = [ktableStore tab]
        }
      t
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nm
    , kstreamKeySerde   = kstreamKeySerde s
    , kstreamValueSerde = serde
    }

-- | The join processor. The KTable side has already updated its store
-- before this processor sees a stream record (because the Topology
-- builder lists the table's update node as a parent — and KTable
-- updates flow before the join processor is invoked for the stream
-- side, courtesy of the engine's per-record FIFO).
--
-- Note: in the JVM impl, both sides are "co-partitioned" before
-- joining; here we operate within a single task so partitioning is
-- trivially equal. A real multi-task runtime would need to ensure
-- the upstream KTable store is reachable from the same task as the
-- KStream — which is exactly why the join is materialised against
-- the table's store name.
joinKStreamKTableProc
  :: forall k v vt v'
   . Ord k
  => StoreName
  -> (v -> vt -> v')
  -> Bool                                -- left-join?
  -> IO (Processor k v)
joinKStreamKTableProc storeNm joiner _isLeft = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore k vt))
  pure Processor
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
        mst  <- readIORef storeRef
        case (mctx, mst, recordKey r) of
          (Just ctx, Just kvs, Just k) -> do
            mt <- kvsGet kvs k
            case mt of
              Just tv ->
                let !v' = joiner (recordValue r) tv
                    !out = Record
                      { recordKey       = Just k
                      , recordValue     = v'
                      , recordTimestamp = recordTimestamp r
                      , recordHeaders   = recordHeaders r
                      } :: Record k v'
                 in forwardRecord ctx out
              Nothing -> pure ()  -- inner-join drops
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
  pure Processor
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
        mst  <- readIORef storeRef
        case (mctx, mst, recordKey r) of
          (Just ctx, Just kvs, Just k) -> do
            mt <- kvsGet kvs k
            let !v' = joiner (recordValue r) mt
                !out = Record
                  { recordKey       = Just k
                  , recordValue     = v'
                  , recordTimestamp = recordTimestamp r
                  , recordHeaders   = recordHeaders r
                  } :: Record k v'
             in forwardRecord ctx out
          _ -> pure ()
    }

----------------------------------------------------------------------
-- KStream-KStream window join
--
-- Architecture:
--
--   Stream-A --> JoinSideProc-A --\
--                                  +--> MergePass --> downstream
--   Stream-B --> JoinSideProc-B --/
--
-- Each side processor:
--   1. Stores its incoming record in its /own/ window store, keyed
--      by record key, indexed by record timestamp.
--   2. Range-scans the /other/ store over
--      @[ts - (\j -> j.beforeMs), ts + (\j -> j.afterMs)]@ for matches.
--   3. For each match, calls the joiner with both values in the
--      canonical (left, right) order and forwards the result to the
--      MergePass node.
--   4. For LEFT and OUTER joins, when /this/ side finds no matches
--      and the side's emission rule for "no match" is non-empty, it
--      emits a single record with @Nothing@ on the other side.
----------------------------------------------------------------------

-- | Inner KStream-KStream window join. Mirrors
-- @KStream.join(other, ValueJoiner, JoinWindows)@.
joinKStreamKStream
  :: forall k v1 v2 v'
   . Ord k
  => (v1 -> v2 -> v')
  -> JoinWindows
  -> Joined k v1 v2
  -> KStream k v1
  -> KStream k v2
  -> IO (KStream k v')
joinKStreamKStream joiner jw _j sl sr =
  buildWindowJoin sl sr jw "KSTREAM-WINDOWJOIN"
    (mkSideProc @k @v1 @v2 @v' jw (\v1 v2 -> joiner v1 v2) Inner)
    (mkSideProc @k @v2 @v1 @v' jw (\v2 v1 -> joiner v1 v2) Inner)

-- | Left KStream-KStream window join. Every left record emits at
-- least once: with @Just v2@ for each match, or with @Nothing@ if no
-- match. Right records only contribute matches.
leftJoinKStreamKStream
  :: forall k v1 v2 v'
   . Ord k
  => (v1 -> Maybe v2 -> v')
  -> JoinWindows
  -> Joined k v1 v2
  -> KStream k v1
  -> KStream k v2
  -> IO (KStream k v')
leftJoinKStreamKStream joiner jw _j sl sr =
  buildWindowJoin sl sr jw "KSTREAM-WINDOWLEFTJOIN"
    (mkSideProc @k @v1 @v2 @v' jw
       (\v1 v2 -> joiner v1 (Just v2))
       (LeftEmitNothing (\v1 -> joiner v1 Nothing)))
    (mkSideProc @k @v2 @v1 @v' jw
       (\v2 v1 -> joiner v1 (Just v2))
       Inner)

-- | Outer KStream-KStream window join. Both sides emit at least
-- once. The joiner takes Maybes on both sides.
outerJoinKStreamKStream
  :: forall k v1 v2 v'
   . Ord k
  => (Maybe v1 -> Maybe v2 -> v')
  -> JoinWindows
  -> Joined k v1 v2
  -> KStream k v1
  -> KStream k v2
  -> IO (KStream k v')
outerJoinKStreamKStream joiner jw _j sl sr =
  buildWindowJoin sl sr jw "KSTREAM-WINDOWOUTERJOIN"
    (mkSideProc @k @v1 @v2 @v' jw
       (\v1 v2 -> joiner (Just v1) (Just v2))
       (LeftEmitNothing (\v1 -> joiner (Just v1) Nothing)))
    (mkSideProc @k @v2 @v1 @v' jw
       (\v2 v1 -> joiner (Just v1) (Just v2))
       (LeftEmitNothing (\v2 -> joiner Nothing (Just v2))))

-- | The "what to do when this side finds no matches" mode.
data JoinMode vSelf vOut
  = Inner
  | LeftEmitNothing (vSelf -> vOut)

-- | Internal: build the topology for a window join given the two
-- pre-typed side processors.
buildWindowJoin
  :: forall k v1 v2 v'
   . Ord k
  => KStream k v1
  -> KStream k v2
  -> JoinWindows
  -> T.Text                              -- ^ prefix
  -> (StoreName -> StoreName -> Topo.NodeName -> IO (Processor k v1))
  -> (StoreName -> StoreName -> Topo.NodeName -> IO (Processor k v2))
  -> IO (KStream k v')
buildWindowJoin sl sr jw prefix mkLeftProc mkRightProc = do
  let b = kstreamBuilder sl
  leftStoreNm  <- freshStoreName b (prefix <> "-LEFT-STORE")
  rightStoreNm <- freshStoreName b (prefix <> "-RIGHT-STORE")
  leftNm       <- freshNodeName  b (prefix <> "-LEFT")
  rightNm      <- freshNodeName  b (prefix <> "-RIGHT")
  mergeNm      <- freshNodeName  b (prefix <> "-MERGE")

  let !sz  = jw.beforeMs + jw.afterMs + 1
      !ret = sz * 2
      lsb = inMemoryWindowStoreBuilder leftStoreNm  sz ret
              :: StoreBuilderW k v1
      rsb = inMemoryWindowStoreBuilder rightStoreNm sz ret
              :: StoreBuilderW k v2

  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = leftNm
                  , Topo.processorSpecParents  = [kstreamParent sl]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor
                        (mkLeftProc leftStoreNm rightStoreNm mergeNm)
                  , Topo.processorSpecStores   = [leftStoreNm, rightStoreNm]
                  } t
        !t2 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = rightNm
                  , Topo.processorSpecParents  = [kstreamParent sr]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor
                        (mkRightProc rightStoreNm leftStoreNm mergeNm)
                  , Topo.processorSpecStores   = [leftStoreNm, rightStoreNm]
                  } t1
        !t3 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = mergeNm
                  , Topo.processorSpecParents  = [leftNm, rightNm]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor (mkPassThrough (prefix <> "-MERGE"))
                  , Topo.processorSpecStores   = []
                  } t2
        !t4 = Topo.addStateStoreW lsb [leftNm, rightNm] t3
        !t5 = Topo.addStateStoreW rsb [leftNm, rightNm] t4
     in t5

  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = mergeNm
    , kstreamKeySerde   = kstreamKeySerde sl
    , kstreamValueSerde = error
        "KStream-KStream join: downstream value Serde unset; supply via to/through"
    }

-- | Build a single side's join processor.
--
-- @selfStore@: where this side puts its incoming records.
-- @otherStore@: where this side scans for matches.
-- @merge@: downstream node that aggregates both sides' emissions.
-- @joiner@: takes (this-side-value, other-side-value) and returns the
--   joined output value.
-- @mode@: what to do when this record finds /no/ match on the other
--   side. 'Inner' drops; 'LeftEmitNothing f' emits @f thisValue@.
-- @side@: only used to suppress unused warnings; the joiner is
--   already in canonical (left,right) order.
mkSideProc
  :: forall k vSelf vOther vOut
   . Ord k
  => JoinWindows
  -> (vSelf -> vOther -> vOut)
  -> JoinMode vSelf vOut
  -> StoreName -> StoreName -> Topo.NodeName
  -> IO (Processor k vSelf)
mkSideProc jw joiner mode selfStoreNm otherStoreNm mergeNm = do
  ctxRef    <- newIORef Nothing
  selfRef   <- newIORef (Nothing :: Maybe (WindowStore k vSelf))
  otherRef  <- newIORef (Nothing :: Maybe (WindowStore k vOther))
  -- Track the max timestamp observed across both join inputs so
  -- we can drop late records past the configured grace period.
  -- JVM semantics: a record at @t@ joins a window
  -- @[t - beforeMs, t + afterMs]@; it's considered too late
  -- once @streamTime > t + afterMs + gracePeriodMs@ and is
  -- dropped at the input.
  streamTime <- newIORef (Timestamp 0)
  pure Processor
    { procName = processorName "WINDOW-JOIN-SIDE"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        getStateStore ctx selfStoreNm >>= \case
          Just (AnyWindowStore ws) ->
            writeIORef selfRef (Just (Unsafe.unsafeCoerce ws))
          _ -> error $ "join: self store not found: " <> show selfStoreNm
        getStateStore ctx otherStoreNm >>= \case
          Just (AnyWindowStore ws) ->
            writeIORef otherRef (Just (Unsafe.unsafeCoerce ws))
          _ -> error $ "join: other store not found: " <> show otherStoreNm
    , procClose = pure ()
    , procProcess = \r ->
        case recordKey r of
          Nothing -> pure ()
          Just k  -> do
            mctx <- readIORef ctxRef
            mself <- readIORef selfRef
            mother <- readIORef otherRef
            case (mctx, mself, mother) of
              (Just ctx, Just self_, Just other_) -> do
                let !ts@(Timestamp tsMs) = recordTimestamp r
                -- Late-record drop (KIP-633 / JoinWindows.grace).
                -- If the record's window has already closed past
                -- the configured grace period, skip the join
                -- entirely — it doesn't contribute to any open
                -- window's matches.
                Timestamp streamMs <- readIORef streamTime
                let !grace         = jw.gracePeriodMs
                    !windowCloseMs = tsMs + jw.afterMs
                    !pastGrace     = streamMs > windowCloseMs + grace
                if pastGrace
                  then pure ()
                  else do
                    -- Advance stream-time monotonically.
                    if tsMs > streamMs
                      then writeIORef streamTime ts
                      else pure ()
                    wsPut self_ k (recordValue r) ts
                    let !lo = Timestamp (tsMs - jw.beforeMs)
                        !hi = Timestamp (tsMs + jw.afterMs)
                    it <- wsFetchRange other_ k lo hi
                    matches <- kvIteratorToList it
                    if null matches
                      then case mode of
                        Inner -> pure ()
                        LeftEmitNothing f ->
                          forwardTo ctx mergeNm
                            (Record
                              { recordKey       = Just k
                              , recordValue     = f (recordValue r)
                              , recordTimestamp = ts
                              , recordHeaders   = recordHeaders r
                              } :: Record k vOut)
                      else mapM_
                             (\(_otherTs, otherV) ->
                                forwardTo ctx mergeNm
                                  (Record
                                    { recordKey       = Just k
                                    , recordValue     = joiner (recordValue r) otherV
                                    , recordTimestamp = ts
                                    , recordHeaders   = recordHeaders r
                                    } :: Record k vOut))
                             matches
              _ -> pure ()
    }

-- | Drop into the low-level Processor API for value-only state, with
-- access to the typed 'ProcessorContext'.
transformValuesStream
  :: T.Text                              -- ^ Processor name prefix
  -> [Topo.NodeName]                     -- ^ Stores attached to this processor
  -> IO (Processor k v)                  -- ^ Processor supplier (already projects to v')
  -> Serde v'                            -- ^ Output value serde
  -> KStream k v
  -> IO (KStream k v')
transformValuesStream prefix _stores supplier vs s =
  attachProcessor s prefix supplier (kstreamKeySerde s) vs

----------------------------------------------------------------------
-- KStream -> KTable conversion
----------------------------------------------------------------------

-- | Convert a stream into a 'KTable' by materialising the latest
-- value per key into a state store. Mirrors @KStream.toTable@.
--
-- Tombstones (records with @recordValue = ...@ but explicitly null
-- in the wire form — represented in our model as a regular 'Record'
-- whose user code chose to encode nullness explicitly) are NOT
-- automatically interpreted as deletes here. If you need delete
-- semantics, route through 'mapValues' producing 'Maybe' and write
-- a tombstone-aware writer; the Java implementation is similarly
-- caller-aware.
toTable
  :: forall k v
   . Ord k
  => Materialized k v
  -> KStream k v
  -> IO (KTable k v)
toTable m s = do
  let b = kstreamBuilder s
  storeNm <- maybe
               (freshStoreName b "KSTREAM-TOTABLE-STORE")
               pure
               (matName m)
  procNm <- freshNodeName b "KSTREAM-TOTABLE"
  let supplier = inMemoryKeyValueStoreBuilder storeNm
                   :: Kafka.Streams.State.Store.StoreBuilderKV k v
  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = procNm
                  , Topo.processorSpecParents  = [kstreamParent s]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor (toTableProc @k @v storeNm)
                  , Topo.processorSpecStores   = []
                  }
                t
        !t2 = Topo.addStateStoreKV supplier [procNm] t1
     in t2
  pure (mkKTableFromStream procNm storeNm b (kstreamKeySerde s) (kstreamValueSerde s))

mkKTableFromStream
  :: Topo.NodeName
  -> Kafka.Streams.State.Store.StoreName
  -> StreamsBuilder
  -> Serde k -> Serde v
  -> KTable k v
mkKTableFromStream nm sn b ks vs =
  -- KTable is a record with non-strict serde fields (we made them
  -- lazy so the builder can stitch deferred error placeholders).
  Kafka.Streams.KTable.KTable nm sn b ks vs

toTableProc
  :: forall k v
   . Ord k
  => Kafka.Streams.State.Store.StoreName
  -> IO (Processor k v)
toTableProc sn = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore k v))
  pure Processor
    { procName = processorName "KSTREAM-TOTABLE"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        getStateStore ctx sn >>= \case
          Just (AnyKeyValueStore kvs) ->
            writeIORef storeRef (Just (Unsafe.unsafeCoerce kvs))
          _ -> error $ "toTable: store missing: " <> show sn
    , procClose = pure ()
    , procProcess = \r -> case recordKey r of
        Nothing -> pure ()
        Just k  -> do
          mctx <- readIORef ctxRef
          mst  <- readIORef storeRef
          case (mctx, mst) of
            (Just ctx, Just kvs) -> do
              kvsPut kvs k (recordValue r)
              forwardRecord ctx r
            _ -> pure ()
    }

----------------------------------------------------------------------
-- Repartition
----------------------------------------------------------------------

-- | Force a repartition. In a single-task driver this is a
-- no-op pass-through; in a multi-task / broker-backed runtime
-- the generated topology routes the records through an
-- internal repartition topic, which is what triggers the
-- actual partitioning.
--
-- /JVM equivalent:/ @KStream.repartition()@.
repartition
  :: forall k v
   . T.Text                              -- ^ topic name prefix
  -> KStream k v
  -> IO (KStream k v)
repartition topicPrefix s = do
  let b = kstreamBuilder s
  nm <- freshNodeName b ("KSTREAM-REPARTITION-" <> topicPrefix)
  withTopology_ b $ \t ->
    Topo.addProcessor nm [kstreamParent s] (mkPassThrough "KSTREAM-REPARTITION") t
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nm
    , kstreamKeySerde   = kstreamKeySerde s
    , kstreamValueSerde = kstreamValueSerde s
    }

-- | @KStream.repartition(Repartitioned)@. Wires every
-- override the 'Repartitioned' record carries: explicit
-- internal-topic name, partition count, key/value serdes, and
-- an optional 'StreamPartitioner'.
--
-- In the in-process driver only the name override has
-- visible effect (we don't emit real partitions); the rest
-- threads through the topology to the live runtime where the
-- internal repartition topic is created with the requested
-- partition count and the per-record routing honours the
-- 'StreamPartitioner'.
repartitionWith
  :: KafkaStreamsRepartitioned.Repartitioned k v
  -> KStream k v
  -> IO (KStream k v)
repartitionWith cfg s = do
  let b = kstreamBuilder s
      !prefix = case cfg.name of
        Just n  -> n
        Nothing -> "REPARTITION"
  nm <- freshNodeName b ("KSTREAM-REPARTITION-" <> prefix)
  withTopology_ b $ \t ->
    Topo.addProcessor nm [kstreamParent s]
      (mkPassThrough "KSTREAM-REPARTITION") t
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nm
    , kstreamKeySerde   =
        maybe (kstreamKeySerde s) id cfg.keySerde
    , kstreamValueSerde =
        maybe (kstreamValueSerde s) id cfg.valueSerde
    }

----------------------------------------------------------------------
-- Split (KIP-418)
----------------------------------------------------------------------

-- | A named branch in a 'splitStream'. The runtime evaluates each
-- predicate in order; the first match routes the record to that
-- branch's named output. The 'Branched' carries an optional
-- continuation invoked /at build time/ on the resulting 'KStream'
-- so users can chain into per-branch sink/transform pipelines
-- without having to thread the result list manually.
data Branched k v = Branched
  { branchedName :: !T.Text
  , branchedPred :: !(Record k v -> Bool)
  , branchedAct  :: !(KStream k v -> IO ())
  }

-- | 'Branched' constructor with a pure predicate and no per-branch
-- continuation. Equivalent to Java's @Branched.as("name").withPredicate(p)@.
branchedFrom :: T.Text -> (Record k v -> Bool) -> Branched k v
branchedFrom n p = Branched
  { branchedName = n
  , branchedPred = p
  , branchedAct  = \_ -> pure ()
  }

-- | @Branched.withFunction(stream -> stream')@. Attach
-- a transform function that runs once on the branched stream;
-- the returned 'KStream' replaces the original in the result
-- map. Use this when each branch needs a different
-- downstream shape.
withFunction
  :: T.Text
  -> (Record k v -> Bool)
  -> (KStream k v -> IO ())
  -> Branched k v
withFunction n p act = Branched
  { branchedName = n
  , branchedPred = p
  , branchedAct  = act
  }

-- | @Branched.withConsumer(stream -> Unit)@. Consume
-- the branch's stream in place (for example: sink to a topic);
-- the branch doesn't appear in the result map. Same as
-- 'withFunction' but the type spelling-out makes intent
-- explicit at the call site.
withConsumer
  :: T.Text
  -> (Record k v -> Bool)
  -> (KStream k v -> IO ())
  -> Branched k v
withConsumer = withFunction

-- | Predicate-routed split: each input record is routed to the first
-- branch whose predicate matches. Records that match none are sent
-- to the optional default branch ('Nothing' drops them).
--
-- Returns a 'Map' from branch name to the resulting 'KStream', plus
-- (if requested) the default-branch stream.
splitStream
  :: forall k v
   . [Branched k v]                      -- ^ branches in evaluation order
  -> Maybe T.Text                        -- ^ optional default-branch name
  -> KStream k v
  -> IO (Map.Map T.Text (KStream k v))
splitStream branches mDefault s = do
  let b = kstreamBuilder s
  router <- freshNodeName b "KSTREAM-SPLIT"
  let allBranches = branches
        ++ case mDefault of
             Just n  -> [defaultBranch n]
             Nothing -> []
      defaultBranch n = Branched
        { branchedName = n
        , branchedPred = \_ -> True
        , branchedAct  = \_ -> pure ()
        }
  childNodes <- mapM
    (\br -> do
        n <- freshNodeName b ("KSTREAM-SPLIT-" <> branchedName br)
        pure (br, n))
    allBranches
  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = router
                  , Topo.processorSpecParents  = [kstreamParent s]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor
                        (mkRouter (map (branchedPred . fst) childNodes)
                                  (map snd childNodes))
                  , Topo.processorSpecStores   = []
                  }
                t
        !t2 = foldl
                (\acc (_br, n) ->
                   Topo.addProcessorWith
                     Topo.ProcessorSpec
                       { Topo.processorSpecName     = n
                       , Topo.processorSpecParents  = [router]
                       , Topo.processorSpecSupplier =
                           Topo.AnyProcessor (mkPassThrough "KSTREAM-SPLIT-CHILD")
                       , Topo.processorSpecStores   = []
                       } acc)
                t1 childNodes
     in t2
  results <- mapM
    (\(br, n) -> do
        let !sub = KStream
              { kstreamBuilder    = b
              , kstreamParent     = n
              , kstreamKeySerde   = kstreamKeySerde s
              , kstreamValueSerde = kstreamValueSerde s
              }
        branchedAct br sub
        pure (branchedName br, sub))
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
    Topo.addProcessor nodeNm [kstreamParent s]
      (filterProcessor (Topo.unNodeName nodeNm) pred_)
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nodeNm
    , kstreamKeySerde   = kstreamKeySerde s
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
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nodeNm
    , kstreamKeySerde   = kstreamKeySerde s
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
    Topo.addProcessor nodeNm [kstreamParent s]
      (mapKVProc (\k v -> pure (f k v)))
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nodeNm
    , kstreamKeySerde   = serde
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
      pure Processor
        { procName    = processorName "KSTREAM-PEEK"
        , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
        , procClose   = pure ()
        , procProcess = \r -> do
            act r
            mctx <- readIORef ctxRef
            case mctx of
              Nothing  -> pure ()
              Just ctx -> forwardRecord ctx r
        }
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nodeNm
    , kstreamKeySerde   = kstreamKeySerde s
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
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nodeNm
    , kstreamKeySerde   = serde
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
  withTopology_ b $
    Topo.addSink nodeNm topic
                 (producedKeySerde p) (producedValueSerde p)
                 [kstreamParent s]

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
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nodeNm
    , kstreamKeySerde   = kstreamKeySerde s
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
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nodeNm
    , kstreamKeySerde   = serde
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
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nodeNm
    , kstreamKeySerde   = serde
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
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nodeNm
    , kstreamKeySerde   = kstreamKeySerde s
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
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nodeNm
    , kstreamKeySerde   = serde
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
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nodeNm
    , kstreamKeySerde   = serde
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
        { Topo.processorSpecName     = nodeNm
        , Topo.processorSpecParents  = [kstreamParent a, kstreamParent b]
        , Topo.processorSpecSupplier =
            Topo.AnyProcessor (mkPassThrough "KSTREAM-MERGE")
        , Topo.processorSpecStores   = []
        }
      t
  pure KStream
    { kstreamBuilder    = bld
    , kstreamParent     = nodeNm
    , kstreamKeySerde   = kstreamKeySerde a
    , kstreamValueSerde = kstreamValueSerde a
    }

-- | 'repartition' with an explicit topology node name. The name is
-- used both as the repartition-topic prefix and as the node name
-- of the synthetic repartition processor.
repartitionNamed
  :: Kafka.Streams.Named.Named
  -> KStream k v
  -> IO (KStream k v)
repartitionNamed nm s =
  let prefix =
        case Kafka.Streams.Named.unNamed nm of
          Just n  -> n
          Nothing -> "repartition"
   in repartition prefix s

----------------------------------------------------------------------
-- KIP-820 process / processValues
----------------------------------------------------------------------

-- | Drop into the low-level Processor API for /side-effecting/
-- processors that may or may not forward downstream. Mirrors
-- @KStream.process(ProcessorSupplier, stateStoreNames)@.
--
-- The supplied 'Processor' receives @Record k v@; if the user code
-- needs to forward records, it can do so via 'forwardRecord' on the
-- 'ProcessorContext'. Returns 'Nothing'-typed wrapper since the
-- processor is a /sink/ from the DSL's perspective (just like
-- 'foreach', but with state-store access).
processStream
  :: forall k v
   . T.Text                              -- ^ node-name prefix
  -> [StoreName]                         -- ^ stores attached
  -> IO (Processor k v)
  -> KStream k v
  -> IO ()
processStream prefix stores supplier s = do
  let b = kstreamBuilder s
  nm <- freshNodeName b prefix
  withTopology_ b $
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName     = nm
        , Topo.processorSpecParents  = [kstreamParent s]
        , Topo.processorSpecSupplier = Topo.AnyProcessor supplier
        , Topo.processorSpecStores   = stores
        }

-- | Like 'processStream' but the resulting node feeds a downstream
-- 'KStream'. The supplied 'Processor' takes input @Record k v@ and
-- may forward records of type @Record k v'@ via 'ctxForward'; the
-- caller supplies the output value 'Serde'.
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
        { Topo.processorSpecName     = nm
        , Topo.processorSpecParents  = [kstreamParent s]
        , Topo.processorSpecSupplier = Topo.AnyProcessor supplier
        , Topo.processorSpecStores   = stores
        }
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nm
    , kstreamKeySerde   = kstreamKeySerde s
    , kstreamValueSerde = vs
    }

----------------------------------------------------------------------
-- KTable -> KStream / KTable.groupBy
----------------------------------------------------------------------

-- | Convert a 'KTable' to a 'KStream' carrying every change event.
-- Mirrors @KTable.toStream@.
toKStreamFromKTable
  :: forall k v
   . Kafka.Streams.KTable.KTable k v
  -> IO (KStream k v)
toKStreamFromKTable kt = do
  let b = Kafka.Streams.KTable.ktableBuilder kt
  nm <- freshNodeName b "KTABLE-TOSTREAM"
  withTopology_ b $ \t ->
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName     = nm
        , Topo.processorSpecParents  = [Kafka.Streams.KTable.ktableNode kt]
        , Topo.processorSpecSupplier =
            Topo.AnyProcessor (mkPassThrough "KTABLE-TOSTREAM")
        , Topo.processorSpecStores   = []
        } t
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nm
    , kstreamKeySerde   = Kafka.Streams.KTable.ktableKeySerde kt
    , kstreamValueSerde = Kafka.Streams.KTable.ktableValueSerde kt
    }

-- | Re-key a 'KTable'. The result is a 'KStream' on the new key
-- (since re-keying conceptually requires a repartition); the user
-- can call 'groupByKey' / 'aggregate' to produce a re-grouped
-- KTable. Mirrors Java's @KTable.groupBy(KeyValueMapper)@ —
-- conceptually returns a 'KGroupedTable' that the caller can
-- aggregate; we lower it to a stream + groupByKey because the
-- single-task driver doesn't distinguish the two.
groupByKTable
  :: forall k v k'
   . (Ord k, Ord k', HasSerde k', HasSerde v)
  => (k -> v -> k')
  -> Kafka.Streams.KTable.KTable k v
  -> IO (KStream k' v)
groupByKTable keyMap kt = do
  s <- toKStreamFromKTable kt
  mapKeyValue (\k v -> (keyMap k v, v)) s

----------------------------------------------------------------------
-- TopicNameExtractor (KIP-303)
----------------------------------------------------------------------

-- | Mirrors Java's @org.apache.kafka.streams.processor.TopicNameExtractor@:
-- decides the destination topic per-record. Receives the record plus the
-- source 'Kafka.Streams.Types.RecordMetadata' (when available via the
-- 'ProcessorContext').
newtype TopicNameExtractor k v = TopicNameExtractor
  { runTopicNameExtractor :: Record k v -> IO TopicName
  }

-- | Sink that routes each record to a topic determined by the
-- supplied 'TopicNameExtractor'. Mirrors @KStream.to(TopicNameExtractor, Produced)@.
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
      pure Processor
        { procName    = processorName "KSTREAM-EXTRACTING-SINK"
        , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
        , procClose   = pure ()
        , procProcess = \r -> do
            mctx <- readIORef ctxRef
            case mctx of
              Nothing  -> pure ()
              Just ctx -> do
                tp <- runTopicNameExtractor ext r
                let kBytes = fmap (serialize (producedKeySerde p))
                                  (recordKey r)
                    vBytes = serialize (producedValueSerde p)
                                       (recordValue r)
                ctxEmitToTopic ctx Kafka.Streams.Processor.SinkEmit
                  { Kafka.Streams.Processor.seTopic     =
                      Kafka.Streams.Types.unTopicName tp
                  , Kafka.Streams.Processor.seKey       = kBytes
                  , Kafka.Streams.Processor.seValue     = vBytes
                  , Kafka.Streams.Processor.seTimestamp = recordTimestamp r
                  }
        }

----------------------------------------------------------------------
-- mergeStreamsN
----------------------------------------------------------------------

-- | Merge any number of streams into one. Equivalent to a left-fold
-- over 'mergeStreams' but emits a single merge node with all inputs
-- as parents — slightly cleaner than @foldr1 mergeStreams@ which
-- creates a chain.
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
        { Topo.processorSpecName     = nm
        , Topo.processorSpecParents  = map kstreamParent (s : ss)
        , Topo.processorSpecSupplier =
            Topo.AnyProcessor (mkPassThrough "KSTREAM-MERGE-N")
        , Topo.processorSpecStores   = []
        }
      t
  pure KStream
    { kstreamBuilder    = bld
    , kstreamParent     = nm
    , kstreamKeySerde   = kstreamKeySerde s
    , kstreamValueSerde = kstreamValueSerde s
    }

----------------------------------------------------------------------
-- Debugging sinks
----------------------------------------------------------------------

-- | Print every record to standard out using @show@. Mirrors
-- @KStream.print(Printed.toSysOut())@.
printStream :: (Show k, Show v) => KStream k v -> IO ()
printStream = printToHandle "[stream]" putStrLn

-- | Print every record using a caller-supplied @putLine@ function.
-- Lets users redirect to a logger / file handle without touching
-- 'System.IO'.
printToHandle
  :: (Show k, Show v)
  => T.Text                     -- prefix label
  -> (String -> IO ())          -- write a line
  -> KStream k v
  -> IO ()
printToHandle label putLine s =
  foreachStream
    (\r -> putLine $ T.unpack label
                      <> " key=" <> show (recordKey r)
                      <> " value=" <> show (recordValue r))
    s

----------------------------------------------------------------------
-- KStream.values: emit only the value side
----------------------------------------------------------------------

-- | Drop the key from every record. Mirrors @KStream.values()@ —
-- equivalent to @mapKeyValue (\_ v -> ((), v))@ followed by a
-- consumer that ignores the unit key.
valuesStream
  :: forall k v
   . KStream k v
  -> IO (KStream () v)
valuesStream s = do
  let b = kstreamBuilder s
  nm <- freshNodeName b "KSTREAM-VALUES"
  withTopology_ b $
    Topo.addProcessor nm [kstreamParent s] (mapKVProc (\_ v -> pure ((), v)))
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nm
    , kstreamKeySerde   = serde
    , kstreamValueSerde = kstreamValueSerde s
    }

----------------------------------------------------------------------
-- concatTransformValues
----------------------------------------------------------------------

-- | Stateful 0-to-many emit on the value side. The user-supplied
-- 'Processor' is free to call 'forwardRecord' (or 'ctxForward')
-- zero, one, or many times per input record. State stores listed
-- in @stores@ are attached and accessible via 'getStateStore'.
--
-- Mirrors @KStream.concatTransformValues@. Identical to
-- 'processValuesStream' (same plumbing); kept as a separate name
-- for users porting from Java.
concatTransformValues
  :: forall k v v'
   . T.Text
  -> [StoreName]
  -> IO (Processor k v)
  -> Serde v'
  -> KStream k v
  -> IO (KStream k v')
concatTransformValues = processValuesStream
