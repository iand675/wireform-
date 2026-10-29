{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Streams.KGroupedStream
Description : KGroupedStream + aggregation surface

@
KGroupedStream k v
@

represents a stream that has been re-keyed (or kept on its key) and
is ready for aggregation. The aggregation primitives (@count@,
@reduce@, @aggregate@) materialise their output into a 'KTable'
backed by a 'KeyValueStore'.

Mirrors @org.apache.kafka.streams.kstream.KGroupedStream<K,V>@.
-}
module Kafka.Streams.KGroupedStream (
  KGroupedStream,
  kgsParent,
  kgsRepartition,
  groupedInput,
  groupedInputOf,
  kgsKey,
  kgsValue,
  kgsBuilder,

  -- * From 'KStream'
  groupByKey,
  groupByStream,

  -- * Aggregations
  countStream,
  reduceStream,
  aggregateStream,
  aggregateSerde,

  -- * Windowed
  windowedByTime,
  windowedBySession,
  TimeWindowedKStream (..),
  SessionWindowedKStream (..),
) where

import Data.IORef
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Kafka.Streams.Grouped (Grouped (..))
import Kafka.Streams.KTable (Change (..), KTable (..))
import Data.Text qualified as T
import Kafka.Streams.KStream (
  KStream (..),
  repartitionThrough,
  selectKey,
  selectKeyWith,
 )
import Kafka.Streams.Materialized (
  Materialized (..),
  materializedKV,
 )
import Kafka.Streams.Processor (
  Processor (..),
  forwardRecord,
  getStateStore,
  processorName,
 )
import Kafka.Streams.Serde (Serde, int64Serde)
import Kafka.Streams.State.KeyValue.InMemory (
  inMemoryKeyValueStoreBuilder,
 )
import Kafka.Streams.State.Store (
  withStoreSerdesKV,
  withStoreSerdesW,
  AnyStateStore (..),
  KeyValueStore (..),
  StoreBuilderKV,
  StoreName,
 )
import Kafka.Streams.StreamsBuilder (
  StreamsBuilder,
  freshNodeName,
  freshStoreName,
  withTopology_,
 )
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Types (Record (..))
import Kafka.Streams.Window (SessionWindows (..), Windows (..))
import Unsafe.Coerce qualified as Unsafe


-- | A stream that has been grouped on a key.
data KGroupedStream k v = KGroupedStream
  { kgsParent :: !Topo.NodeName
  , kgsKey :: !(Serde k)
  , kgsValue :: !(Serde v)
  , kgsBuilder :: !StreamsBuilder
  , kgsRepartition :: !(Maybe (Maybe T.Text))
  {- ^ @Just name@ when the key may have changed since the stream was
  last partitioned by it: each aggregation then reads through a
  repartition topic (named after the 'Grouped' name when given).
  -}
  }


{- | Group records by their existing key. When the key may have
changed upstream (a 'Kafka.Streams.KStream.selectKey', a 'map'),
the aggregation reads through a repartition topic so each key is
aggregated by the task owning its partition.

/JVM equivalent:/ @KStream.groupByKey(Grouped)@.
-}
groupByKey :: Grouped k v -> KStream k v -> KGroupedStream k v
groupByKey g s =
  KGroupedStream
    { kgsParent = kstreamParent s
    , kgsKey = groupedKeySerde g
    , kgsValue = groupedValueSerde g
    , kgsBuilder = kstreamBuilder s
    , kgsRepartition =
        if kstreamKeyChanged s then Just (groupedName g) else Nothing
    }


{- | Group by a key derived from each record. Inserts a
'selectKey' processor first.

/JVM equivalent:/ @KStream.groupBy(KeyValueMapper, Grouped)@.
-}
groupByStream
  :: (Record k v -> k')
  -> Grouped k' v
  -> KStream k v
  -> IO (KGroupedStream k' v)
groupByStream f g s = do
  -- Use the supplied 'Grouped' as the explicit serde for the new
  -- key; this is the per-call override that doesn't depend on
  -- 'HasSerde' instances for the user's key type.
  s' <- selectKeyWith (groupedKeySerde g) f s
  pure
    KGroupedStream
      { kgsParent = kstreamParent s'
      , kgsKey = groupedKeySerde g
      , kgsValue = groupedValueSerde g
      , kgsBuilder = kstreamBuilder s'
      , kgsRepartition = Just (groupedName g)
      }


{- | The node an aggregation of @kgs@ reads from: its parent, or a
repartition of it when the key may have changed.
-}
groupedInput :: KGroupedStream k v -> IO Topo.NodeName
groupedInput kgs =
  groupedInputOf (kgsBuilder kgs) (kgsParent kgs) (kgsKey kgs) (kgsValue kgs) (kgsRepartition kgs)


-- | 'groupedInput' from the pieces (shared by the windowed streams).
groupedInputOf
  :: StreamsBuilder
  -> Topo.NodeName
  -> Serde k
  -> Serde v
  -> Maybe (Maybe T.Text)
  -> IO Topo.NodeName
groupedInputOf _ parent _ _ Nothing = pure parent
groupedInputOf b parent ks vs (Just mName) = do
  s' <-
    repartitionThrough
      mName
      Nothing
      ks
      vs
      Nothing
      KStream
        { kstreamBuilder = b
        , kstreamParent = parent
        , kstreamKeyChanged = True
        , kstreamKeySerde = ks
        , kstreamValueSerde = vs
        }
  pure (kstreamParent s')


----------------------------------------------------------------------
-- Aggregations (non-windowed → KTable)
----------------------------------------------------------------------

{- | Count the records per key.

/JVM equivalent:/ @KGroupedStream.count(Materialized)@.

The result lives in a 'KeyValueStore' the runtime addresses by
'matName' (synthesised if absent).
-}
countStream
  :: forall k v
   . Ord k
  => Materialized k Int64
  -> KGroupedStream k v
  -> IO (KTable k Int64)
countStream m kgs = do
  storeNm <-
    maybe
      (freshStoreName (kgsBuilder kgs) "KSTREAM-AGGREGATE-STORE")
      pure
      (matName m)
  let supplier =
        materializedKV m storeNm (kgsKey kgs) int64Serde
          :: StoreBuilderKV k Int64
  nodeNm <- freshNodeName (kgsBuilder kgs) "KSTREAM-AGGREGATE"
  input <- groupedInput kgs
  withTopology_ (kgsBuilder kgs) $ \t ->
    let !t1 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = nodeNm
              , Topo.processorSpecParents = [input]
              , Topo.processorSpecSupplier =
                  Topo.AnyProcessor (countProcessor @k @v @Int64 storeNm)
              , Topo.processorSpecStores = []
              }
            t
        !t2 = Topo.addStateStoreKV supplier [nodeNm] t1
    in t2
  pure (aggregateTable kgs nodeNm storeNm int64Serde)


{- | Combine values for the same key with a binary reducer.

/JVM equivalent:/ @KGroupedStream.reduce(Reducer, Materialized)@.
-}
reduceStream
  :: forall k v
   . Ord k
  => (v -> v -> v)
  -> Materialized k v
  -> KGroupedStream k v
  -> IO (KTable k v)
reduceStream combine m kgs = do
  storeNm <-
    maybe
      (freshStoreName (kgsBuilder kgs) "KSTREAM-REDUCE-STORE")
      pure
      (matName m)
  let supplier =
        materializedKV m storeNm (kgsKey kgs) (kgsValue kgs) :: StoreBuilderKV k v
  nodeNm <- freshNodeName (kgsBuilder kgs) "KSTREAM-REDUCE"
  input <- groupedInput kgs
  withTopology_ (kgsBuilder kgs) $ \t ->
    let !t1 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = nodeNm
              , Topo.processorSpecParents = [input]
              , Topo.processorSpecSupplier =
                  Topo.AnyProcessor (reduceProcessor @k @v storeNm combine)
              , Topo.processorSpecStores = []
              }
            t
        !t2 = Topo.addStateStoreKV supplier [nodeNm] t1
    in t2
  pure (aggregateTable kgs nodeNm storeNm (kgsValue kgs))


{- | Stateful fold: seed an accumulator and update it per
record using the supplied aggregator.

/JVM equivalent:/ @KGroupedStream.aggregate(Initializer, Aggregator, Materialized)@.
-}
aggregateStream
  :: forall k v a
   . Ord k
  => IO a
  -- ^ initialiser (Java @Initializer<A>@)
  -> (k -> v -> a -> a)
  -- ^ aggregator (Java @Aggregator<K,V,A>@)
  -> Materialized k a
  -> KGroupedStream k v
  -> IO (KTable k a)
aggregateStream initial agg m kgs = do
  storeNm <-
    maybe
      (freshStoreName (kgsBuilder kgs) "KSTREAM-AGGREGATE-STORE")
      pure
      (matName m)
  let supplier =
        materializedKV m storeNm (kgsKey kgs) (aggregateSerde m)
          :: StoreBuilderKV k a
  nodeNm <- freshNodeName (kgsBuilder kgs) "KSTREAM-AGGREGATE"
  input <- groupedInput kgs
  withTopology_ (kgsBuilder kgs) $ \t ->
    let !t1 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = nodeNm
              , Topo.processorSpecParents = [input]
              , Topo.processorSpecSupplier =
                  Topo.AnyProcessor
                    (aggregateProcessor @k @v @a storeNm initial agg)
              , Topo.processorSpecStores = []
              }
            t
        !t2 = Topo.addStateStoreKV supplier [nodeNm] t1
    in t2
  pure (aggregateTable kgs nodeNm storeNm (aggregateSerde m))


----------------------------------------------------------------------
-- Aggregation processors
----------------------------------------------------------------------

{- | The 'countProcessor' takes 3 type vars but the input @v@ is
ignored — the processor only counts.
-}
countProcessor
  :: forall k v out
   . (Ord k, out ~ Int64)
  => StoreName
  -> IO (Processor k v)
countProcessor sn = aggregateProcessor @k @v @out sn (pure 0) (\_ _ acc -> acc + 1)


reduceProcessor
  :: forall k v
   . Ord k
  => StoreName
  -> (v -> v -> v)
  -> IO (Processor k v)
reduceProcessor sn combine = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore k v))
  pure
    Processor
      { procName = processorName "KSTREAM-REDUCE"
      , procInit = \ctx -> do
          writeIORef ctxRef (Just ctx)
          st <- getStateStore ctx sn
          case st of
            Just (AnyKeyValueStore kvs) ->
              writeIORef
                storeRef
                (Just (unsafeCastKV kvs :: KeyValueStore k v))
            _ -> error $ "KGroupedStream.reduce: store not found: " <> show sn
      , procClose = pure ()
      , procProcess = \r -> do
          mctx <- readIORef ctxRef
          mst <- readIORef storeRef
          case (mctx, mst, recordKey r) of
            (Just ctx, Just kvs, Just k) -> do
              !mPrev <- kvsGet kvs k
              let !next = case mPrev of
                    Nothing -> recordValue r
                    Just p -> combine p (recordValue r)
              kvsPut kvs k next
              forwardRecord ctx r {recordValue = Change (Just next) mPrev}
            _ -> pure ()
      }


aggregateProcessor
  :: forall k v a
   . Ord k
  => StoreName
  -> IO a
  -> (k -> v -> a -> a)
  -> IO (Processor k v)
aggregateProcessor sn initial agg = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore k a))
  pure
    Processor
      { procName = processorName "KSTREAM-AGGREGATE"
      , procInit = \ctx -> do
          writeIORef ctxRef (Just ctx)
          st <- getStateStore ctx sn
          case st of
            Just (AnyKeyValueStore kvs) ->
              writeIORef storeRef (Just (unsafeCastKV kvs))
            _ -> error $ "aggregate: store not found: " <> show sn
      , procClose = pure ()
      , procProcess = \r -> do
          mctx <- readIORef ctxRef
          mst <- readIORef storeRef
          case (mctx, mst, recordKey r) of
            (Just ctx, Just kvs, Just k) -> do
              mPrev <- kvsGet kvs k
              !cur <- case mPrev of
                Nothing -> initial
                Just p -> pure p
              let !next = agg k (recordValue r) cur
              kvsPut kvs k next
              forwardRecord ctx r {recordValue = Change (Just next) mPrev}
            _ -> pure ()
      }


{- | Type-erased cast across the engine boundary. The DSL guarantees
that the store's actual key/value types match the processor's at
the call site (both are inserted by the same operation).
-}
unsafeCastKV :: KeyValueStore k v -> KeyValueStore k' v'
unsafeCastKV = Unsafe.unsafeCoerce
{-# INLINE unsafeCastKV #-}


----------------------------------------------------------------------
-- Windowed grouping
----------------------------------------------------------------------

{- | Window the grouped stream by time (tumbling / hopping /
sliding — selected via the 'Windows' value).

/JVM equivalent:/ @KGroupedStream.windowedBy(Windows)@.
-}
windowedByTime
  :: Windows
  -> KGroupedStream k v
  -> TimeWindowedKStream k v
windowedByTime ws kgs =
  TimeWindowedKStream
    { twksParent = kgsParent kgs
    , twksKey = kgsKey kgs
    , twksValue = kgsValue kgs
    , twksBuilder = kgsBuilder kgs
    , twksWindows = ws
    , twksRepartition = kgsRepartition kgs
    }


{- | Window the grouped stream by session (gap-based) windows.

/JVM equivalent:/ @KGroupedStream.windowedBy(SessionWindows)@.
-}
windowedBySession
  :: SessionWindows
  -> KGroupedStream k v
  -> SessionWindowedKStream k v
windowedBySession sw kgs =
  SessionWindowedKStream
    { swksParent = kgsParent kgs
    , swksKey = kgsKey kgs
    , swksValue = kgsValue kgs
    , swksBuilder = kgsBuilder kgs
    , swksWindows = sw
    , swksRepartition = kgsRepartition kgs
    }


----------------------------------------------------------------------
-- Window / session structure (defined here to avoid dep cycles)
----------------------------------------------------------------------

data TimeWindowedKStream k v = TimeWindowedKStream
  { twksParent :: !Topo.NodeName
  , twksKey :: !(Serde k)
  , twksValue :: !(Serde v)
  , twksBuilder :: !StreamsBuilder
  , twksWindows :: !Windows
  , twksRepartition :: !(Maybe (Maybe T.Text))
  -- ^ As 'kgsRepartition'
  }


data SessionWindowedKStream k v = SessionWindowedKStream
  { swksParent :: !Topo.NodeName
  , swksKey :: !(Serde k)
  , swksValue :: !(Serde v)
  , swksBuilder :: !StreamsBuilder
  , swksWindows :: !SessionWindows
  , swksRepartition :: !(Maybe (Maybe T.Text))
  -- ^ As 'kgsRepartition'
  }


----------------------------------------------------------------------
-- Returned aggregate handle
----------------------------------------------------------------------

-- | The table an aggregation of @kgs@ produces: keyed like @kgs@, valued by @vs@.
aggregateTable :: KGroupedStream k v -> Topo.NodeName -> StoreName -> Serde a -> KTable k a
aggregateTable kgs nodeNm storeNm vs =
  KTable
    { ktableNode = nodeNm
    , ktableStore = storeNm
    , ktableBuilder = kgsBuilder kgs
    , ktableKeySerde = kgsKey kgs
    , ktableValueSerde = vs
    }


{- | An aggregate's value serde: the 'Materialized' one. Lazy: only a
changelog write or a sink that needs the bytes forces the error.
-}
aggregateSerde :: Materialized k a -> Serde a
aggregateSerde m =
  fromMaybe
    (error "aggregate: give the Materialized a value serde (withValueSerde) for the aggregate type")
    (matValueSerde m)
