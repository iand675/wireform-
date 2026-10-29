{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

{- |
Module      : Kafka.Streams.Cogroup
Description : Co-grouped aggregations across multiple streams

A 'CogroupedStream k a' captures a set of pre-grouped streams,
each with their own value type, all feeding the same aggregator
state of type @a@. Calling 'aggregateCogrouped' builds a single
output 'KTable' whose value is updated by /any/ of the source
streams via the source-specific aggregator.

Mirrors @KGroupedStream.cogroup(...)@ +
@CogroupedKStream.aggregate(...)@.
-}
module Kafka.Streams.Cogroup (
  CogroupedStream,
  cogroup,
  addCogrouped,
  aggregateCogrouped,

  -- * Windowed cogroup
  TimeWindowedCogroupedStream (..),
  windowedByCogroup,
  aggregateWindowedCogrouped,
) where

import Data.IORef
import Data.Int (Int64)
import Kafka.Streams.KGroupedStream (
  CountedTableLocal (..),
  KGroupedStream,
  kgsBuilder,
  kgsKey,
  groupedInput,
 )
import Kafka.Streams.Serde (Serde)
import Kafka.Streams.Materialized (
  Materialized (..),
  materializedKV,
  materializedS,
  materializedW,
 )
import Kafka.Streams.Processor (
  Processor (..),
  forwardRecord,
  getStateStore,
  processorName,
 )
import Kafka.Streams.State.KeyValue.InMemory (
  inMemoryKeyValueStoreBuilder,
 )
import Kafka.Streams.State.Store (
  AnyStateStore (..),
  KeyValueStore (..),
  StoreBuilderKV,
  StoreBuilderW,
  StoreName,
  WindowStore (..),
 )
import Kafka.Streams.State.Window.InMemory (
  inMemoryWindowStoreBuilder,
 )
import Kafka.Streams.StreamsBuilder (
  StreamsBuilder,
  freshNodeName,
  freshStoreName,
  withTopology_,
 )
import Kafka.Streams.Time (Timestamp (..))
import Kafka.Streams.TimeWindowedKStream qualified
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Types (Record (..))
import Kafka.Streams.Window qualified
import Unsafe.Coerce qualified as Unsafe


{- | One element of a cogroup: a typed grouped stream paired with
its source-specific aggregator.
-}
data CogroupSource k a where
  CogroupSource
    :: KGroupedStream k v
    -> (k -> v -> a -> a)
    -> CogroupSource k a


{- | A cogroup-in-progress. The 'a' type is the shared aggregator
state; each entry in 'sources' contributes a different source
value type via the existential carrier.
-}
data CogroupedStream k a = CogroupedStream
  { builder :: !StreamsBuilder
  , sources :: ![CogroupSource k a]
  }


-- | The grouping key's serde (every source shares the key type).
cogroupKeySerde :: CogroupedStream k a -> Serde k
cogroupKeySerde cgs = case cgs.sources of
  CogroupSource kgs _ : _ -> kgsKey kgs
  [] -> error "cogroup: no sources"


-- | Start a cogroup with one source.
cogroup
  :: KGroupedStream k v
  -> (k -> v -> a -> a)
  -> CogroupedStream k a
cogroup kgs agg =
  CogroupedStream
    { builder = kgsBuilder kgs
    , sources = [CogroupSource kgs agg]
    }


{- | Add another source to an in-progress cogroup. The new source
can have a different value type but must share the aggregator
state type @a@.
-}
addCogrouped
  :: CogroupedStream k a
  -> KGroupedStream k v
  -> (k -> v -> a -> a)
  -> CogroupedStream k a
addCogrouped cgs kgs agg =
  cgs
    { sources = cgs.sources ++ [CogroupSource kgs agg]
    }


{- | Build the cogroup's output table. Mirrors
@CogroupedKStream.aggregate(initializer, Materialized)@.
-}
aggregateCogrouped
  :: forall k a
   . Ord k
  => IO a -- initialiser
  -> Materialized k a
  -> CogroupedStream k a
  -> IO (CountedTableLocal k a)
aggregateCogrouped initial m cgs = do
  let b = cgs.builder
  storeNm <-
    maybe
      (freshStoreName b "KSTREAM-COGROUP-STORE")
      pure
      (matName m)
  let supplier =
        materializedKV m storeNm (cogroupKeySerde cgs) (error "aggregate: pass Materialized with a value serde for the aggregate type")
          :: StoreBuilderKV k a
  -- For each source we register the per-side processor immediately,
  -- and only export the side's NODE NAME outside the existential.
  -- That way the source's @v@ never escapes.
  ownerNms <-
    traverse
      ( \src -> do
          nm <- freshNodeName b "KSTREAM-COGROUP-SIDE"
          addCogrupSideToTopology b nm storeNm initial src
          pure nm
      )
      cgs.sources
  withTopology_ b $ \t ->
    Topo.addStateStoreKV supplier ownerNms t
  pure
    CountedTableLocal
      { ctlNode = case ownerNms of
          [] -> error "aggregateCogrouped: no sources"
          (nm : _) -> nm
      , ctlStore = storeNm
      , ctlBuilder = b
      }


addCogrupSideToTopology
  :: forall k a
   . Ord k
  => StreamsBuilder
  -> Topo.NodeName -- side processor name
  -> StoreName
  -> IO a
  -> CogroupSource k a
  -> IO ()
addCogrupSideToTopology b nm storeNm initial (CogroupSource kgs agg) = do
  input <- groupedInput kgs
  withTopology_ b $
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName = nm
        , Topo.processorSpecParents = [input]
        , Topo.processorSpecSupplier =
            Topo.AnyProcessor (cogroupSideProc @k storeNm initial agg)
        , Topo.processorSpecStores = [storeNm]
        }


{- | The processor body for a single side of the cogroup. Reads the
shared store, calls this side's aggregator, and writes back.
-}
cogroupSideProc
  :: forall k v a
   . Ord k
  => StoreName
  -> IO a
  -> (k -> v -> a -> a)
  -> IO (Processor k v)
cogroupSideProc sn initial agg = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore k a))
  pure
    Processor
      { procName = processorName "KSTREAM-COGROUP-SIDE"
      , procInit = \ctx -> do
          writeIORef ctxRef (Just ctx)
          getStateStore ctx sn >>= \case
            Just (AnyKeyValueStore kvs) ->
              writeIORef storeRef (Just (Unsafe.unsafeCoerce kvs))
            _ -> error $ "cogroup: store missing: " <> show sn
      , procClose = pure ()
      , procProcess = \r -> case recordKey r of
          Nothing -> pure ()
          Just k -> do
            mctx <- readIORef ctxRef
            mst <- readIORef storeRef
            case (mctx, mst) of
              (Just ctx, Just kvs) -> do
                mPrev <- kvsGet kvs k
                !cur <- maybe initial pure mPrev
                let !next = agg k (recordValue r) cur
                kvsPut kvs k next
                forwardRecord ctx r {recordValue = next}
              _ -> pure ()
      }


----------------------------------------------------------------------
-- Windowed cogroup (KIP-150)
----------------------------------------------------------------------

{- | A 'CogroupedStream' with an attached 'Windows' definition.
Mirrors Java's 'TimeWindowedCogroupedKStream'. The aggregate
output is a windowed KTable produced by
'aggregateWindowedCogrouped' (a follow-up combinator).
-}
data TimeWindowedCogroupedStream k a = TimeWindowedCogroupedStream
  { inner :: !(CogroupedStream k a)
  , windows :: !Kafka.Streams.Window.Windows
  }


{- | @CogroupedStream.windowedBy(windows)@: attach a 'Windows'
to a cogrouped stream so the subsequent aggregation produces
a windowed KTable. The actual windowed aggregation is
implemented by combining the per-side processors with the
existing 'aggregateWindowed' implementation; this carrier
type is the type-level entry point that mirrors the JVM
contract.
-}
windowedByCogroup
  :: Kafka.Streams.Window.Windows
  -> CogroupedStream k a
  -> TimeWindowedCogroupedStream k a
windowedByCogroup ws cg =
  TimeWindowedCogroupedStream
    { inner = cg
    , windows = ws
    }


{- | Close out a time-windowed cogroup builder and emit its
result as a windowed table. Mirrors JVM
@TimeWindowedCogroupedKStream.aggregate(Initializer, Materialized)@.

Each side's processor walks the windows the record belongs to
and applies the side's aggregator to the per-window
accumulator stored in a shared 'WindowStore'.
-}
aggregateWindowedCogrouped
  :: forall k a
   . Ord k
  => IO a
  -> Materialized k a
  -> TimeWindowedCogroupedStream k a
  -> IO (Kafka.Streams.TimeWindowedKStream.WindowedTableHandle k a)
aggregateWindowedCogrouped initial m twcg = do
  let cgs = twcg.inner
      ws = twcg.windows
      b = cgs.builder
      sz = Kafka.Streams.Window.windowsSize ws
      ret = max sz (Kafka.Streams.Window.windowsRetention ws)
  storeNm <-
    maybe
      (freshStoreName b "KSTREAM-COGROUP-WIN-STORE")
      pure
      (matName m)
  let supplier =
        materializedW m storeNm sz ret (cogroupKeySerde cgs) (error "aggregate: pass Materialized with a value serde for the aggregate type")
          :: StoreBuilderW k a
  ownerNms <-
    traverse
      ( \src -> do
          nm <- freshNodeName b "KSTREAM-COGROUP-WIN-SIDE"
          addCogroupWindowedSide b nm storeNm sz initial src
          pure nm
      )
      cgs.sources
  withTopology_ b $ \t ->
    Topo.addStateStoreW supplier ownerNms t
  pure
    Kafka.Streams.TimeWindowedKStream.WindowedTableHandle
      { Kafka.Streams.TimeWindowedKStream.wthNode =
          case ownerNms of
            [] -> error "aggregateWindowedCogrouped: no sources"
            (nm : _) -> nm
      , Kafka.Streams.TimeWindowedKStream.wthStore = storeNm
      , Kafka.Streams.TimeWindowedKStream.wthBuilder = b
      , Kafka.Streams.TimeWindowedKStream.wthWindows = ws
      , Kafka.Streams.TimeWindowedKStream.wthEmit =
          Kafka.Streams.TimeWindowedKStream.emitOnWindowUpdate
      }


addCogroupWindowedSide
  :: forall k a
   . Ord k
  => StreamsBuilder
  -> Topo.NodeName -- side processor name
  -> StoreName
  -> Int64 -- window size (ms)
  -> IO a
  -> CogroupSource k a
  -> IO ()
addCogroupWindowedSide b nm storeNm sz initial (CogroupSource kgs agg) = do
  input <- groupedInput kgs
  withTopology_ b $
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName = nm
        , Topo.processorSpecParents = [input]
        , Topo.processorSpecSupplier =
            Topo.AnyProcessor
              (cogroupWindowedSideProc @k storeNm sz initial agg)
        , Topo.processorSpecStores = [storeNm]
        }


{- | The processor body for a single side of a windowed cogroup.
For each incoming record, computes the window the record's
timestamp falls into, fetches the per-window accumulator,
applies the side's aggregator, writes back.
-}
cogroupWindowedSideProc
  :: forall k v a
   . Ord k
  => StoreName
  -> Int64 -- window size (ms)
  -> IO a
  -> (k -> v -> a -> a)
  -> IO (Processor k v)
cogroupWindowedSideProc sn sz initial agg = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (WindowStore k a))
  pure
    Processor
      { procName = processorName "KSTREAM-COGROUP-WIN-SIDE"
      , procInit = \ctx -> do
          writeIORef ctxRef (Just ctx)
          getStateStore ctx sn >>= \case
            Just (AnyWindowStore wsv) ->
              writeIORef storeRef (Just (Unsafe.unsafeCoerce wsv))
            _ -> error $ "cogroup-windowed: store missing: " <> show sn
      , procClose = pure ()
      , procProcess = \r -> case recordKey r of
          Nothing -> pure ()
          Just k -> do
            mctx <- readIORef ctxRef
            mst <- readIORef storeRef
            case (mctx, mst) of
              (Just ctx, Just wsv) -> do
                -- Bucket the record's timestamp into its window
                -- start (tumbling-style: floor to window-size).
                let Timestamp tsMs = recordTimestamp r
                    !windowStart = Timestamp ((tsMs `div` sz) * sz)
                mPrev <- wsFetch wsv k windowStart
                !cur <- maybe initial pure mPrev
                let !next = agg k (recordValue r) cur
                wsPut wsv k next windowStart
                forwardRecord ctx r {recordValue = next}
              _ -> pure ()
      }
