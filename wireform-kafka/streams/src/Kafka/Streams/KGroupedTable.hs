{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

{- |
Module      : Kafka.Streams.KGroupedTable
Description : Aggregation over a KTable's change-log

Mirrors @org.apache.kafka.streams.kstream.KGroupedTable<K,V>@.
Where 'Kafka.Streams.KGroupedStream' aggregates an
append-only stream, 'KGroupedTable' aggregates a /changelog/:
every input record represents either an insertion, an update,
or a delete (tombstone) for the same key, and the aggregator
must run a /subtractor/ first to remove the previous value's
contribution before adding the new one. Without the
subtractor a key's running total would double-count on every
update.

== Wiring

@
table        = builder.tableFromTopic ...
grouped      = table.groupBy(\\k v -> (k', v))
table'       = grouped.count(materialized)
             | grouped.reduce(adder, subtractor, materialized)
             | grouped.aggregate(initializer, adder, subtractor, materialized)
@

The DSL stays close to the JVM contract: 'reduceKGroupedTable'
and 'aggregateKGroupedTable' take both an adder and a
subtractor and apply them in subtractor-then-adder order on
every input record that's a true update (i.e. the previous
value for the same key was non-tombstone).
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
import Kafka.Streams.Grouped (Grouped (..), grouped)
import Kafka.Streams.KGroupedStream (aggregateSerde)
import Kafka.Streams.KTable (
  KTable (..),
  ktableBuilder,
  ktableKeySerde,
  ktableNode,
  ktableValueSerde,
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
import Data.Maybe (fromMaybe)
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
import Unsafe.Coerce qualified as Unsafe


{- | A KTable's change-log grouped by a derived key, ready to
be aggregated with subtractor-aware combinators.

The intermediate carrier stores three things: the upstream
KTable node to attach to, the typed key/value serdes for the
aggregation result, and the builder.
-}
data KGroupedTable k v = KGroupedTable
  { parent :: !Topo.NodeName
  , key :: !(Serde k)
  , value :: !(Serde v)
  , builder :: !StreamsBuilder
  , keyMap :: !(k -> v -> (k, v))
  {- ^ Re-key function applied per input record. The JVM
  @KTable.groupBy@ takes a @KeyValueMapper@; we expose
  the same shape and capture it here so the aggregation
  processor doesn't need an extra explicit re-key node.
  -}
  }


{- | @KTable.groupBy(KeyValueMapper, Grouped)@ — produces a
'KGroupedTable' for subtractor-aware aggregation. The
re-key function returns @(k', v')@; downstream aggregations
operate on those.

Named 'groupTableBy' (rather than @groupBy@ or
@groupByKTable@) to keep the namespace clean against the
existing 'Kafka.Streams.KStream.groupByKTable' which
performs a different operation (re-key a KTable into a
KStream).
-}
groupTableBy
  :: (Ord k, Ord k')
  => (k -> v -> (k', v'))
  -> Grouped k' v'
  -> KTable k v
  -> KGroupedTable k' v'
groupTableBy f g parentT =
  KGroupedTable
    { parent = ktableNode parentT
    , key = groupedKeySerde g
    , value = groupedValueSerde g
    , builder = ktableBuilder parentT
    , keyMap = Unsafe.unsafeCoerce f
    -- f's actual type is k -> v -> (k', v'); we re-key
    -- inside the aggregation processor and let the engine's
    -- type erasure carry the typed values.
    }


----------------------------------------------------------------------
-- Aggregations
----------------------------------------------------------------------

{- | @KTable.groupBy(...).count(Materialized)@. The subtractor
is implicit (-1) and the adder is implicit (+1). Mirrors
@KGroupedTable.count@.
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
    (\_ _ acc -> acc + 1) -- add a record
    (\_ _ acc -> acc - 1) -- subtract the prior version
    m {matValueSerde = Just (fromMaybe int64Serde (matValueSerde m))}


{- | @KTable.groupBy(...).reduce(adder, subtractor, Materialized)@.
Mirrors @KGroupedTable.reduce@: on every input record, the
subtractor removes the previous value's contribution (if
any) and then the adder folds the new value in. The first
record per key bypasses the subtractor and seeds the store
with the value as-is — the JVM contract.
-}
reduceKGroupedTable
  :: forall k v
   . Ord k
  => (v -> v -> v)
  -- ^ adder    (oldAgg new -> newAgg)
  -> (v -> v -> v)
  -- ^ subtractor (oldAgg old -> partial)
  -> Materialized k v
  -> KGroupedTable k v
  -> IO (KTable k v)
reduceKGroupedTable add sub m kgt = do
  let b = kgt.builder
  storeNm <-
    maybe
      (freshStoreName b "KTABLE-REDUCE-STORE")
      pure
      (matName m)
  prevNm <- freshStoreName b "KTABLE-REDUCE-PREV"
  let outBuilder =
        materializedKV m storeNm kgt.key kgt.value
          :: StoreBuilderKV k v
      prevBuilder =
        withStoreSerdesKV kgt.key kgt.value (inMemoryKeyValueStoreBuilder prevNm)
          :: StoreBuilderKV k v
  procNm <- freshNodeName b "KTABLE-REDUCE"
  withTopology_ b $ \t ->
    let !t1 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = procNm
              , Topo.processorSpecParents = [kgt.parent]
              , Topo.processorSpecSupplier =
                  Topo.AnyProcessor
                    ( reduceKGroupedTableProc @k @v
                        (kgt.keyMap)
                        add
                        sub
                        storeNm
                        prevNm
                    )
              , Topo.processorSpecStores = [storeNm, prevNm]
              }
            t
        !t2 = Topo.addStateStoreKV outBuilder [procNm] t1
        !t3 = Topo.addStateStoreKV prevBuilder [procNm] t2
    in t3
  pure
    KTable
      { ktableNode = procNm
      , ktableStore = storeNm
      , ktableBuilder = b
      , ktableKeySerde = kgt.key
      , ktableValueSerde = kgt.value
      }


reduceKGroupedTableProc
  :: forall k v
   . Ord k
  => (k -> v -> (k, v))
  -> (v -> v -> v)
  -> (v -> v -> v)
  -> StoreName
  -> StoreName
  -> IO (Processor k v)
reduceKGroupedTableProc keyMap add sub outNm prevNm = do
  ctxRef <- newIORef Nothing
  outRef <- newIORef (Nothing :: Maybe (KeyValueStore k v))
  prevRef <- newIORef (Nothing :: Maybe (KeyValueStore k v))
  pure
    Processor
      { procName = processorName "KTABLE-REDUCE"
      , procInit = \ctx -> do
          writeIORef ctxRef (Just ctx)
          getStateStore ctx outNm >>= \case
            Just (AnyKeyValueStore kvs) ->
              writeIORef outRef (Just (Unsafe.unsafeCoerce kvs))
            _ ->
              error $
                "KGroupedTable.reduce: out store missing: "
                  <> show outNm
          getStateStore ctx prevNm >>= \case
            Just (AnyKeyValueStore kvs) ->
              writeIORef prevRef (Just (Unsafe.unsafeCoerce kvs))
            _ ->
              error $
                "KGroupedTable.reduce: prev store missing: "
                  <> show prevNm
      , procClose = pure ()
      , procProcess = \r -> case recordKey r of
          Nothing -> pure ()
          Just kIn -> do
            mctx <- readIORef ctxRef
            mout <- readIORef outRef
            mprev <- readIORef prevRef
            case (mctx, mout, mprev) of
              (Just ctx, Just outS, Just prevS) -> do
                mOldV <- kvsGet prevS kIn
                let !(kNewG, vNew) = keyMap kIn (recordValue r)
                    mOldGroup = (\v -> keyMap kIn v) <$> mOldV
                    kOldG = fst <$> mOldGroup
                case kOldG of
                  Just oldG | oldG /= kNewG -> do
                    let Just (_, vOldG) = mOldGroup
                    mOldAcc <- kvsGet outS oldG
                    let !accOld' = case mOldAcc of
                          Just acc -> sub acc vOldG
                          Nothing -> vOldG -- shouldn't happen,
                          -- but defensive
                    kvsPut outS oldG accOld'
                    forwardRecord
                      ctx
                      ( Record
                          (Just oldG)
                          accOld'
                          (recordTimestamp r)
                          (recordHeaders r)
                      )
                    mNewAcc <- kvsGet outS kNewG
                    let !accNew = case mNewAcc of
                          Nothing -> vNew
                          Just acc -> add acc vNew
                    kvsPut outS kNewG accNew
                    forwardRecord
                      ctx
                      ( Record
                          (Just kNewG)
                          accNew
                          (recordTimestamp r)
                          (recordHeaders r)
                      )
                  _ -> do
                    mAgg <- kvsGet outS kNewG
                    let !next = case mAgg of
                          Nothing -> vNew
                          Just acc -> case mOldGroup of
                            Nothing -> add acc vNew
                            Just (_oldG, vOldG) ->
                              add (sub acc vOldG) vNew
                    kvsPut outS kNewG next
                    forwardRecord
                      ctx
                      ( Record
                          (Just kNewG)
                          next
                          (recordTimestamp r)
                          (recordHeaders r)
                      )
                kvsPut prevS kIn vNew
              _ -> pure ()
      }


{- | @KTable.groupBy(...).aggregate(initializer, adder,
subtractor, Materialized)@. The aggregator runs the
subtractor on the /previous/ input value (the value of the
same key the last time it was seen, or 'Nothing' for the
very first record) and then the adder on the /new/ value.
For tombstone records (recordValue r is the user-supplied
"null" sentinel; we don't have a wire-level null distinct
from a real value, so users must encode tombstones via the
value type), only the subtractor fires.
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
aggregateKGroupedTable initial add sub m kgt = do
  let b = kgt.builder
  storeNm <-
    maybe
      (freshStoreName b "KTABLE-AGGREGATE-STORE")
      pure
      (matName m)
  -- Two stores: the materialised output @storeNm@ +
  -- a hidden @prevNm@ that remembers the last value seen
  -- per key (so we know what to subtract on the next
  -- update).
  prevNm <- freshStoreName b "KTABLE-AGGREGATE-PREV"
  let outBuilder =
        materializedKV m storeNm kgt.key (aggregateSerde m)
          :: StoreBuilderKV k a
      prevBuilder =
        withStoreSerdesKV kgt.key kgt.value (inMemoryKeyValueStoreBuilder prevNm)
          :: StoreBuilderKV k v
  procNm <- freshNodeName b "KTABLE-AGGREGATE"
  withTopology_ b $ \t ->
    let !t1 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = procNm
              , Topo.processorSpecParents = [kgt.parent]
              , Topo.processorSpecSupplier =
                  Topo.AnyProcessor
                    ( aggregateKGroupedTableProc @k @v @a
                        (kgt.keyMap)
                        initial
                        add
                        sub
                        storeNm
                        prevNm
                    )
              , Topo.processorSpecStores = [storeNm, prevNm]
              }
            t
        !t2 = Topo.addStateStoreKV outBuilder [procNm] t1
        !t3 = Topo.addStateStoreKV prevBuilder [procNm] t2
    in t3
  pure
    KTable
      { ktableNode = procNm
      , ktableStore = storeNm
      , ktableBuilder = b
      , ktableKeySerde = kgt.key
      , ktableValueSerde = aggregateSerde m
      }


aggregateKGroupedTableProc
  :: forall k v a
   . Ord k
  => (k -> v -> (k, v))
  -> IO a
  -> (k -> v -> a -> a)
  -> (k -> v -> a -> a)
  -> StoreName -- output store
  -> StoreName -- prev-value store
  -> IO (Processor k v)
aggregateKGroupedTableProc keyMap initial add sub outNm prevNm = do
  ctxRef <- newIORef Nothing
  outRef <- newIORef (Nothing :: Maybe (KeyValueStore k a))
  prevRef <- newIORef (Nothing :: Maybe (KeyValueStore k v))
  pure
    Processor
      { procName = processorName "KTABLE-AGGREGATE"
      , procInit = \ctx -> do
          writeIORef ctxRef (Just ctx)
          getStateStore ctx outNm >>= \case
            Just (AnyKeyValueStore kvs) ->
              writeIORef outRef (Just (Unsafe.unsafeCoerce kvs))
            _ ->
              error $
                "KGroupedTable.aggregate: out store missing: "
                  <> show outNm
          getStateStore ctx prevNm >>= \case
            Just (AnyKeyValueStore kvs) ->
              writeIORef prevRef (Just (Unsafe.unsafeCoerce kvs))
            _ ->
              error $
                "KGroupedTable.aggregate: prev store missing: "
                  <> show prevNm
      , procClose = pure ()
      , procProcess = \r -> case recordKey r of
          Nothing -> pure ()
          Just kIn -> do
            mctx <- readIORef ctxRef
            mout <- readIORef outRef
            mprev <- readIORef prevRef
            case (mctx, mout, mprev) of
              (Just ctx, Just outS, Just prevS) -> do
                mOldV <- kvsGet prevS kIn
                let !(kNewG, vNew) = keyMap kIn (recordValue r)
                    mOldGroup = (\v -> keyMap kIn v) <$> mOldV
                    kOldG = fst <$> mOldGroup
                case kOldG of
                  Just oldG | oldG /= kNewG -> do
                    -- Group changed: subtract from old group, add
                    -- to new group, emit two records.
                    let Just (_, vOldG) = mOldGroup
                    mOldAcc <- kvsGet outS oldG
                    !accO0 <- maybe initial pure mOldAcc
                    let !accOld' = sub oldG vOldG accO0
                    kvsPut outS oldG accOld'
                    forwardRecord
                      ctx
                      ( Record
                          (Just oldG)
                          accOld'
                          (recordTimestamp r)
                          (recordHeaders r)
                      )
                    mNewAcc <- kvsGet outS kNewG
                    !accN0 <- maybe initial pure mNewAcc
                    let !accNew = add kNewG vNew accN0
                    kvsPut outS kNewG accNew
                    forwardRecord
                      ctx
                      ( Record
                          (Just kNewG)
                          accNew
                          (recordTimestamp r)
                          (recordHeaders r)
                      )
                  _ -> do
                    -- Same group (or first record): subtract+add on
                    -- the single output key.
                    mAcc <- kvsGet outS kNewG
                    !acc0 <- maybe initial pure mAcc
                    let !accAfterSub = case mOldGroup of
                          Nothing -> acc0
                          Just (_oldG, vOldGrouped) ->
                            sub kNewG vOldGrouped acc0
                        !accNew = add kNewG vNew accAfterSub
                    kvsPut outS kNewG accNew
                    forwardRecord
                      ctx
                      ( Record
                          (Just kNewG)
                          accNew
                          (recordTimestamp r)
                          (recordHeaders r)
                      )
                kvsPut prevS kIn vNew
              _ -> pure ()
      }
