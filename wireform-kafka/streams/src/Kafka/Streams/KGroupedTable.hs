{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

{- |
Module      : Kafka.Streams.KGroupedTable
Description : Aggregation over a KTable's changelog

Mirrors @org.apache.kafka.streams.kstream.KGroupedTable<K,V>@.
Where 'Kafka.Streams.KGroupedStream' aggregates an append-only
stream, 'KGroupedTable' aggregates a /changelog/: a table row that
changes moves its contribution, so each change is applied as a
subtraction of the row's old value (from the group the old value
belonged to) and an addition of its new one (to its new group).

== Wiring

@
table   = builder.tableFromTopic ...
grouped = groupTableBy (\\k v -> (k', v')) g table
counts  = countKGroupedTable m grouped
        | reduceKGroupedTable adder subtractor m grouped
        | aggregateKGroupedTable initializer adder subtractor m grouped
@

'groupTableBy' maps a change's old and new row to their groups and
sends them, as a subtraction and an addition, through a repartition
topic keyed by group (the JVM's @KTableRepartitionMap@), so every
group is aggregated by the one task that owns it.
-}
module Kafka.Streams.KGroupedTable (
  KGroupedTable (..),
  groupTableBy,

  -- * Aggregations
  countKGroupedTable,
  reduceKGroupedTable,
  aggregateKGroupedTable,
) where

import Data.IORef
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Kafka.Streams.Consumed (consumed, consumedExtractor)
import Kafka.Streams.Grouped (Grouped (..))
import Kafka.Streams.KGroupedStream (aggregateSerde)
import Kafka.Streams.KTable (
  Change (..),
  KTable (..),
  bindTableCache,
  changeSerde,
  writeRow,
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
import Kafka.Streams.State.Store (
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
import Kafka.Streams.Types (Record (..), topicName)
import Unsafe.Coerce qualified as Unsafe


{- | A table's changelog regrouped by a derived key: the source node
reading the repartition topic, which forwards a 'Change' per group
(an old row's subtraction, or a new row's addition).
-}
data KGroupedTable k v = KGroupedTable
  { parent :: !Topo.NodeName
  , key :: !(Serde k)
  , value :: !(Serde v)
  , builder :: !StreamsBuilder
  }


{- | @KTable.groupBy(KeyValueMapper, Grouped)@. @f@ maps a row to its
group key and the value it contributes. The repartition topic is
@\<name\>-repartition@ ('groupedName', else a fresh node name).
-}
groupTableBy
  :: forall k v k' v'
   . (Ord k, Ord k')
  => (k -> v -> (k', v'))
  -> Grouped k' v'
  -> KTable k v
  -> IO (KGroupedTable k' v')
groupTableBy f g t = do
  let b = ktableBuilder t
      ks = groupedKeySerde g
      cs = changeSerde (groupedValueSerde g)
  selectNm <- freshNodeName b "KTABLE-SELECT"
  sinkNm <- freshNodeName b "KTABLE-REPARTITION-SINK"
  sourceNm <- freshNodeName b "KTABLE-REPARTITION-SOURCE"
  let base = fromMaybe (Topo.unNodeName selectNm) (groupedName g)
      topic = topicName (base <> "-repartition")
  withTopology_ b $ \tp ->
    let !t1 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = selectNm
              , Topo.processorSpecParents = [ktableNode t]
              , Topo.processorSpecSupplier = Topo.AnyProcessor (selectProcessor @k @v @k' @v' f)
              , Topo.processorSpecStores = []
              }
            tp
        !t2 = Topo.addInternalTopic topic Topo.InternalTopicSpec {Topo.itsPartitions = Nothing} t1
        !t3 = Topo.addSink sinkNm topic ks cs [selectNm] t2
        !t4 = Topo.addSource sourceNm [topic] ks cs (consumedExtractor (consumed ks cs)) t3
    in t4
  pure
    KGroupedTable
      { parent = sourceNm
      , key = ks
      , value = groupedValueSerde g
      , builder = b
      }


{- | Each change becomes a subtraction of the old row from its group,
then an addition of the new row to its group (two records even when
the group is the same, applied in that order by the aggregation).
-}
selectProcessor :: forall k v k' v'. (k -> v -> (k', v')) -> IO (Processor k (Change v))
selectProcessor f = do
  ref <- newIORef Nothing
  pure
    Processor
      { procName = processorName "KTABLE-SELECT"
      , procInit = writeIORef ref . Just
      , procClose = pure ()
      , procProcess = \r -> do
          mctx <- readIORef ref
          case (mctx, recordKey r) of
            (Just ctx, Just k) -> do
              let Change new old = recordValue r
                  send (gk, gv) c = forwardRecord ctx r {recordKey = Just gk, recordValue = c gv}
              mapM_ (\o -> send (f k o) (\gv -> Change Nothing (Just gv) :: Change v')) old
              mapM_ (\n -> send (f k n) (\gv -> Change (Just gv) Nothing :: Change v')) new
            _ -> pure ()
      }


----------------------------------------------------------------------
-- Aggregations
----------------------------------------------------------------------

{- | @KGroupedTable.count(Materialized)@: rows per group (an update
moves its row from the old group to the new one).
-}
countKGroupedTable
  :: forall k v
   . Ord k
  => Materialized k Int64
  -> KGroupedTable k v
  -> IO (KTable k Int64)
countKGroupedTable m =
  aggregateKGroupedTable
    (pure 0)
    (\_ _ acc -> acc + 1)
    (\_ _ acc -> acc - 1)
    m {matValueSerde = Just (fromMaybe int64Serde (matValueSerde m))}


{- | @KGroupedTable.reduce(adder, subtractor, Materialized)@, as the
JVM's @KTableReduce@: the subtractor removes an old row from its
group's value, the adder folds a new row in (a group's first row
becomes its value), and a group whose value is gone is deleted.
-}
reduceKGroupedTable
  :: forall k v
   . Ord k
  => (v -> v -> v)
  -- ^ adder (aggregate, new row)
  -> (v -> v -> v)
  -- ^ subtractor (aggregate, old row)
  -> Materialized k v
  -> KGroupedTable k v
  -> IO (KTable k v)
reduceKGroupedTable add sub m kgt =
  groupedAggregate "KTABLE-REDUCE" m kgt kgt.value $ \_ change prev ->
    let afterSub = case (changeOld change, prev) of
          (Just o, Just agg) -> Just (sub agg o)
          _ -> prev
    in pure $ case changeNew change of
         Just n -> Just (maybe n (`add` n) afterSub)
         Nothing -> afterSub


{- | @KGroupedTable.aggregate(initializer, adder, subtractor,
Materialized)@, as the JVM's @KTableAggregate@: the subtractor
removes an old row from its group's aggregate, the adder folds a
new row into it (starting from the initializer).
-}
aggregateKGroupedTable
  :: forall k v a
   . Ord k
  => IO a
  -- ^ initialiser
  -> (k -> v -> a -> a)
  -- ^ adder
  -> (k -> v -> a -> a)
  -- ^ subtractor
  -> Materialized k a
  -> KGroupedTable k v
  -> IO (KTable k a)
aggregateKGroupedTable initial add sub m kgt =
  groupedAggregate "KTABLE-AGGREGATE" m kgt (aggregateSerde m) $ \k change prev -> do
    let afterSub = case (changeOld change, prev) of
          (Just o, Just agg) -> Just (sub k o agg)
          _ -> prev
    case changeNew change of
      Just n -> do
        base <- maybe initial pure afterSub
        pure (Just (add k n base))
      Nothing -> pure afterSub


{- | The aggregation node: reads the regrouped changes, applies
@step@ (key, change, the group's current value) and forwards the
group's change.
-}
groupedAggregate
  :: forall k v a
   . Ord k
  => T.Text
  -> Materialized k a
  -> KGroupedTable k v
  -> Serde a
  -> (k -> Change v -> Maybe a -> IO (Maybe a))
  -> IO (KTable k a)
groupedAggregate label m kgt vs step = do
  let b = kgt.builder
  storeNm <- maybe (freshStoreName b (label <> "-STORE")) pure (matName m)
  procNm <- freshNodeName b label
  let supplier = materializedKV m storeNm kgt.key vs :: StoreBuilderKV k a
      proc_ = do
        ref <- newIORef Nothing
        pure
          Processor
            { procName = processorName label
            , procInit = \ctx -> do
                kvs <- getStateStore ctx storeNm >>= \case
                  Just (AnyKeyValueStore kvs) -> pure (Unsafe.unsafeCoerce kvs :: KeyValueStore k a)
                  _ -> error (T.unpack label <> ": store not found: " <> show storeNm)
                bindTableCache ctx kvs
                writeIORef ref (Just (ctx, kvs))
            , procClose = pure ()
            , procProcess = \r -> do
                mst <- readIORef ref
                case (mst, recordKey r) of
                  (Just (ctx, kvs), Just k) -> do
                    prev <- kvsGet kvs k
                    next <- step k (recordValue r) prev
                    writeRow ctx kvs r k next prev
                  _ -> pure ()
            }
  withTopology_ b $ \t ->
    Topo.addStateStoreKV
      supplier
      [procNm]
      ( Topo.addProcessorWith
          Topo.ProcessorSpec
            { Topo.processorSpecName = procNm
            , Topo.processorSpecParents = [kgt.parent]
            , Topo.processorSpecSupplier = Topo.AnyProcessor proc_
            , Topo.processorSpecStores = []
            }
          t
      )
  pure
    KTable
      { ktableNode = procNm
      , ktableStore = storeNm
      , ktableBuilder = b
      , ktableKeySerde = kgt.key
      , ktableValueSerde = vs
      }
