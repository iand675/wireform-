{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Streams.GlobalKTable
Description : 'GlobalKTable' — every-partition replicated lookup table

A 'GlobalKTable' is conceptually a regular 'KTable' but with two
key differences:

  * The runtime reads /all/ partitions of the source topic into
    /every/ instance's local store (rather than just the assigned
    partitions), so the table is fully replicated.
  * Joins against a 'GlobalKTable' do /not/ require co-partitioning
    of the stream's key with the table's key. The user supplies a
    'KeyValueMapper' that derives the lookup key from the stream
    record.

Within the single-task 'TopologyTestDriver' there is no
partitioning so the runtime difference disappears; what remains is
the API surface and the joiner-with-key-mapper for stream joins.

Mirrors @org.apache.kafka.streams.kstream.GlobalKTable@.
-}
module Kafka.Streams.GlobalKTable (
  GlobalKTable,
  globalTable,
  globalKTableStore,
  globalKTableNode,
  globalKTableBuilder,
  globalKTableKeySerde,
  globalKTableValueSerde,

  -- * Joins
  joinKStreamGlobalKTable,
  leftJoinKStreamGlobalKTable,
) where

import Data.IORef
import Kafka.Streams.Consumed (Consumed (..))
import Kafka.Streams.KStream (
  KStream (..),
  kstreamBuilder,
  kstreamKeySerde,
  kstreamParent,
 )
import Kafka.Streams.KStream qualified as KS
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
import Kafka.Streams.Serde (HasSerde (..), Serde)
import Kafka.Streams.State.KeyValue.InMemory (
  inMemoryKeyValueStoreBuilder,
 )
import Kafka.Streams.State.Store (
  withLoggingDisabledKV,
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
import Kafka.Streams.Types (
  Record (..),
  TopicName,
 )
import Unsafe.Coerce qualified as Unsafe


{- | Handle to a 'GlobalKTable'. Wraps the underlying source topic +
materialised store. The 'globalKTableNode' is the source-side
update node; the 'globalKTableStore' is the materialised store the
joiner reads.
-}
data GlobalKTable k v = GlobalKTable
  { globalKTableNode :: !Topo.NodeName
  , globalKTableStore :: !StoreName
  , globalKTableBuilder :: !StreamsBuilder
  , globalKTableKeySerde :: ~(Serde k)
  , globalKTableValueSerde :: ~(Serde v)
  }


{- | Materialise a topic as a globally-replicated table.

Every instance holds the whole table: the runtime's global thread
reads every partition of the topic into the store, and tasks look it
up read-only.
-}
globalTable
  :: forall k v
   . Ord k
  => StreamsBuilder
  -> TopicName
  -> Consumed k v
  -> Materialized k v
  -> IO (GlobalKTable k v)
globalTable b topic c m = do
  storeNm <-
    maybe
      (freshStoreName b "GLOBAL-TABLE-STORE")
      pure
      (matName m)
  -- The global table's topic is its changelog: it is restored by
  -- reading the topic, never written to a separate one.
  let supplier =
        withLoggingDisabledKV
          (materializedKV m storeNm (consumedKeySerde c) (consumedValueSerde c))
          :: StoreBuilderKV k v
  sourceNm <- freshNodeName b "GLOBAL-TABLE-SOURCE"
  procNm <- freshNodeName b "GLOBAL-TABLE-SOURCE-PROCESSOR"
  withTopology_ b $ \t ->
    let !t1 =
          Topo.addTableSource
            sourceNm
            [topic]
            (consumedKeySerde c)
            (consumedValueSerde c)
            (consumedExtractor c)
            t
        !t2 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = procNm
              , Topo.processorSpecParents = [sourceNm]
              , Topo.processorSpecSupplier =
                  Topo.AnyProcessor (globalSourceProc @k @v storeNm)
              , Topo.processorSpecStores = []
              }
            t1
        !t3 = Topo.addStateStoreKV supplier [procNm] t2
    in Topo.markGlobalStore storeNm t3
  pure
    GlobalKTable
      { globalKTableNode = procNm
      , globalKTableStore = storeNm
      , globalKTableBuilder = b
      , globalKTableKeySerde = consumedKeySerde c
      , globalKTableValueSerde = consumedValueSerde c
      }


globalSourceProc
  :: forall k v
   . Ord k
  => StoreName -> IO (Processor k (Maybe v))
globalSourceProc sn = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore k v))
  pure
    Processor
      { procName = processorName "GLOBAL-TABLE-SOURCE-PROCESSOR"
      , procInit = \ctx -> do
          writeIORef ctxRef (Just ctx)
          getStateStore ctx sn >>= \case
            Just (AnyKeyValueStore kvs) ->
              writeIORef storeRef (Just (Unsafe.unsafeCoerce kvs))
            _ -> error $ "GlobalKTable: store missing: " <> show sn
      , procClose = pure ()
      , procProcess = \r -> do
          mst <- readIORef storeRef
          case (recordKey r, mst) of
            (Just k, Just kvs) -> case recordValue r of
              Just v -> kvsPut kvs k v
              Nothing -> () <$ kvsDelete kvs k
            _ -> pure ()
      }


----------------------------------------------------------------------
-- Joins: KStream JOIN GlobalKTable
----------------------------------------------------------------------

{- | Inner-join a stream with a global table using a key mapper.

Mirrors @KStream.join(GlobalKTable, KeyValueMapper, ValueJoiner)@.
-}
joinKStreamGlobalKTable
  :: forall k v kg vg v'
   . (Ord kg, HasSerde v')
  => (k -> v -> kg)
  -- ^ derive lookup key from stream record
  -> (v -> vg -> v')
  -- ^ joiner
  -> KStream k v
  -> GlobalKTable kg vg
  -> IO (KStream k v')
joinKStreamGlobalKTable keyMap joiner s g = do
  let b = kstreamBuilder s
  nm <- freshNodeName b "KSTREAM-GLOBALKTABLE-JOIN"
  withTopology_ b $ \t ->
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName = nm
        , Topo.processorSpecParents = [kstreamParent s]
        , Topo.processorSpecSupplier =
            Topo.AnyProcessor
              ( mkGlobalJoinProc @k @v @kg @vg @v'
                  (globalKTableStore g)
                  keyMap
                  (InnerG joiner :: GMode v vg v')
              )
        , Topo.processorSpecStores = [globalKTableStore g]
        }
      t
  pure
    KS.KStream
      { kstreamBuilder = b
      , kstreamParent = nm
      , kstreamKeyChanged = kstreamKeyChanged s
      , kstreamKeySerde = kstreamKeySerde s
      , kstreamValueSerde =
          error
            "GlobalKTable join: pass Produced with a value Serde to a downstream sink"
      }


-- | Left-join a stream with a global table.
leftJoinKStreamGlobalKTable
  :: forall k v kg vg v'
   . (Ord kg, HasSerde v')
  => (k -> v -> kg)
  -> (v -> Maybe vg -> v')
  -> KStream k v
  -> GlobalKTable kg vg
  -> IO (KStream k v')
leftJoinKStreamGlobalKTable keyMap joiner s g = do
  let b = kstreamBuilder s
  nm <- freshNodeName b "KSTREAM-GLOBALKTABLE-LEFTJOIN"
  withTopology_ b $ \t ->
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName = nm
        , Topo.processorSpecParents = [kstreamParent s]
        , Topo.processorSpecSupplier =
            Topo.AnyProcessor
              ( mkGlobalJoinProc @k @v @kg @vg @v'
                  (globalKTableStore g)
                  keyMap
                  (LeftG joiner :: GMode v vg v')
              )
        , Topo.processorSpecStores = [globalKTableStore g]
        }
      t
  pure
    KS.KStream
      { kstreamBuilder = b
      , kstreamParent = nm
      , kstreamKeyChanged = kstreamKeyChanged s
      , kstreamKeySerde = kstreamKeySerde s
      , kstreamValueSerde = serde
      }


data GMode v vg v' where
  InnerG :: (v -> vg -> v') -> GMode v vg v'
  LeftG :: (v -> Maybe vg -> v') -> GMode v vg v'


mkGlobalJoinProc
  :: forall k v kg vg v'
   . Ord kg
  => StoreName
  -> (k -> v -> kg)
  -> GMode v vg v'
  -> IO (Processor k v)
mkGlobalJoinProc storeNm keyMap mode = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore kg vg))
  pure
    Processor
      { procName = processorName "GLOBAL-JOIN"
      , procInit = \ctx -> do
          writeIORef ctxRef (Just ctx)
          getStateStore ctx storeNm >>= \case
            Just (AnyKeyValueStore kvs) ->
              writeIORef storeRef (Just (Unsafe.unsafeCoerce kvs))
            _ -> error $ "GlobalKTable join: store missing: " <> show storeNm
      , procClose = pure ()
      , procProcess = \r ->
          case recordKey r of
            Nothing -> pure ()
            Just k -> do
              mctx <- readIORef ctxRef
              mst <- readIORef storeRef
              case (mctx, mst) of
                (Just ctx, Just kvs) -> do
                  let !kg_ = keyMap k (recordValue r)
                  mvg <- kvsGet kvs kg_
                  case mode of
                    InnerG f ->
                      case mvg of
                        Just vg ->
                          forwardRecord
                            ctx
                            ( Record
                                { recordKey = Just k
                                , recordValue = f (recordValue r) vg
                                , recordTimestamp = recordTimestamp r
                                , recordHeaders = recordHeaders r
                                }
                                :: Record k v'
                            )
                        Nothing -> pure ()
                    LeftG f ->
                      forwardRecord
                        ctx
                        ( Record
                            { recordKey = Just k
                            , recordValue = f (recordValue r) mvg
                            , recordTimestamp = recordTimestamp r
                            , recordHeaders = recordHeaders r
                            }
                            :: Record k v'
                        )
                _ -> pure ()
      }
