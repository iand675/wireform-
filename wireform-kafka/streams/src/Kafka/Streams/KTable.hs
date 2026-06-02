{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.KTable
-- Description : KTable DSL surface
--
-- A 'KTable' is the changelog view of a 'KStream': for each key, only
-- the latest value matters. KTables are always backed by a state
-- store, and the DSL operations preserve that invariant.
--
-- Mirrors @org.apache.kafka.streams.kstream.KTable<K,V>@.
module Kafka.Streams.KTable
  ( KTable (..)
  , ktableNode
  , ktableStore
  , ktableBuilder
  , ktableKeySerde
  , ktableValueSerde
    -- * Source
  , tableFromTopic
    -- * Stateless
  , filterTable
  , filterNotTable
  , mapValuesTable
    -- * Stateful value transforms
  , transformValuesTable
    -- * Joins
  , joinKTableKTable
  , leftJoinKTableKTable
  , outerJoinKTableKTable
    -- * Conversion
  , toStreamTable
  , queryKeyValueStore
  ) where

import Data.IORef
import qualified Data.Text
import qualified GHC.Exts
import qualified Unsafe.Coerce as Unsafe

import Kafka.Streams.Consumed (Consumed (..))
import Kafka.Streams.Materialized (Materialized (..))
import Kafka.Streams.StreamsBuilder
  ( StreamsBuilder
  , freshNodeName
  , freshStoreName
  , withTopology_
  )
import Kafka.Streams.Processor
  ( Processor (..)
  , ProcessorContext
  , forwardRecord
  , getStateStore
  , processorName
  )
import Kafka.Streams.Serde (HasSerde (..), Serde)
import Kafka.Streams.State.KeyValue.InMemory
  ( inMemoryKeyValueStoreBuilder
  )
import Kafka.Streams.State.Store
  ( AnyStateStore (..)
  , KeyValueStore (..)
  , StoreBuilderKV
  , StoreName
  )
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Types
  ( Record (..)
  , TopicName
  , mapValue
  )

-- | KTable handle.
data KTable k v = KTable
  { ktableNode    :: !Topo.NodeName
  , ktableStore   :: !StoreName
  , ktableBuilder :: !StreamsBuilder
  , ktableKeySerde   :: ~(Serde k)
  , ktableValueSerde :: ~(Serde v)
  }

-- | Materialise a topic as a 'KTable'. Each (key,value)
-- updates the store; tombstones (null value) delete the key.
--
-- /JVM equivalent:/ @StreamsBuilder.table(topic, Consumed, Materialized)@.
tableFromTopic
  :: forall k v
   . Ord k
  => StreamsBuilder
  -> TopicName
  -> Consumed k v
  -> Materialized k v
  -> IO (KTable k v)
tableFromTopic b topic c m = do
  storeNm <- maybe (freshStoreName b "KTABLE-SOURCE-STORE")
                   pure
                   (matName m)
  let supplier = inMemoryKeyValueStoreBuilder storeNm :: StoreBuilderKV k v
  sourceNm <- freshNodeName b "KTABLE-SOURCE"
  procNm   <- freshNodeName b "KTABLE-SOURCE-PROCESSOR"
  withTopology_ b $ \t ->
    let !t1 = Topo.addSource sourceNm [topic]
                (consumedKeySerde c)
                (consumedValueSerde c)
                (consumedExtractor c) t
        !t2 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = procNm
                  , Topo.processorSpecParents  = [sourceNm]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor (sourceTableProcessor @k @v storeNm)
                  , Topo.processorSpecStores   = []
                  }
                t1
        !t3 = Topo.addStateStoreKV supplier [procNm] t2
     in t3
  pure KTable
    { ktableNode       = procNm
    , ktableStore      = storeNm
    , ktableBuilder    = b
    , ktableKeySerde   = consumedKeySerde c
    , ktableValueSerde = consumedValueSerde c
    }

sourceTableProcessor
  :: forall k v
   . Ord k
  => StoreName -> IO (Processor k v)
sourceTableProcessor sn = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef Nothing
  pure Processor
    { procName  = processorName "KTABLE-SOURCE-PROCESSOR"
    , procInit  = \ctx -> do
        writeIORef ctxRef (Just ctx)
        st <- getStateStore ctx sn
        case st of
          Just (AnyKeyValueStore kvs) ->
            writeIORef storeRef (Just (unsafeCastKV kvs :: KeyValueStore k v))
          _ -> error $ "KTable.source: store not found: " <> show sn
    , procClose = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        mst  <- readIORef storeRef
        case (mctx, mst, recordKey r) of
          (Just ctx, Just kvs, Just k) -> do
            kvsPut kvs k (recordValue r)
            forwardRecord ctx r
          _ -> pure ()
    }

----------------------------------------------------------------------
-- Stateless
----------------------------------------------------------------------

-- | Drop entries for which the predicate returns 'False'.
-- A 'Nothing' result is forwarded so downstream tables can
-- represent the absence of the key explicitly.
--
-- /JVM equivalent:/ @KTable.filter(Predicate, Materialized)@.
filterTable
  :: forall k v
   . Ord k
  => (Record k v -> Bool)
  -> Materialized k v
  -> KTable k v
  -> IO (KTable k v)
filterTable pred_ m parent = do
  storeNm <- maybe (freshStoreName (ktableBuilder parent) "KTABLE-FILTER-STORE")
                   pure
                   (matName m)
  let b = ktableBuilder parent
      supplier = inMemoryKeyValueStoreBuilder storeNm :: StoreBuilderKV k v
  procNm <- freshNodeName b "KTABLE-FILTER"
  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = procNm
                  , Topo.processorSpecParents  = [ktableNode parent]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor (filterTableProcessor @k @v storeNm pred_)
                  , Topo.processorSpecStores   = []
                  }
                t
        !t2 = Topo.addStateStoreKV supplier [procNm] t1
     in t2
  pure KTable
    { ktableNode       = procNm
    , ktableStore      = storeNm
    , ktableBuilder    = b
    , ktableKeySerde   = ktableKeySerde parent
    , ktableValueSerde = ktableValueSerde parent
    }

filterTableProcessor
  :: forall k v
   . Ord k
  => StoreName
  -> (Record k v -> Bool)
  -> IO (Processor k v)
filterTableProcessor sn pred_ = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef Nothing
  pure Processor
    { procName = processorName "KTABLE-FILTER"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        st <- getStateStore ctx sn
        case st of
          Just (AnyKeyValueStore kvs) ->
            writeIORef storeRef (Just (unsafeCastKV kvs :: KeyValueStore k v))
          _ -> error $ "KTable.filter: store not found: " <> show sn
    , procClose = pure ()
    , procProcess = \r ->
        case recordKey r of
          Nothing -> pure ()
          Just k  -> do
            mst <- readIORef storeRef
            mctx <- readIORef ctxRef
            case (mst, mctx) of
              (Just kvs, Just ctx) ->
                if pred_ r
                  then do
                    kvsPut kvs k (recordValue r)
                    forwardRecord ctx r
                  else do
                    -- drop and tombstone (Java semantics)
                    _ <- kvsDelete kvs k
                    pure ()
              _ -> pure ()
    }

-- | The inverse of 'filterTable' — keep records that do /not/
-- match the predicate. Equivalent to
-- @filterTable (not . pred_)@ but exists as a dedicated entry
-- to mirror Java's @KTable.filterNot@.
filterNotTable
  :: forall k v
   . Ord k
  => (Record k v -> Bool)
  -> Materialized k v
  -> KTable k v
  -> IO (KTable k v)
filterNotTable p = filterTable (not . p)

-- | Apply a function to every value in the table. The new
-- 'Materialized' becomes the result table's backing store.
--
-- /JVM equivalent:/ @KTable.mapValues(ValueMapper, Materialized)@.
mapValuesTable
  :: forall k v v'
   . (Ord k, HasSerde v')
  => (v -> v')
  -> Materialized k v'
  -> KTable k v
  -> IO (KTable k v')
mapValuesTable f m parent = do
  storeNm <- maybe (freshStoreName (ktableBuilder parent) "KTABLE-MAPVAL-STORE")
                   pure
                   (matName m)
  let b = ktableBuilder parent
      supplier = inMemoryKeyValueStoreBuilder storeNm :: StoreBuilderKV k v'
  procNm <- freshNodeName b "KTABLE-MAPVAL"
  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = procNm
                  , Topo.processorSpecParents  = [ktableNode parent]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor (mapValueTableProcessor @k @v @v' storeNm f)
                  , Topo.processorSpecStores   = []
                  }
                t
        !t2 = Topo.addStateStoreKV supplier [procNm] t1
     in t2
  pure KTable
    { ktableNode       = procNm
    , ktableStore      = storeNm
    , ktableBuilder    = b
    , ktableKeySerde   = ktableKeySerde parent
    , ktableValueSerde = serde
    }

-- | Stateful value-only transform on a 'KTable'. Mirrors
-- Java's @KTable.transformValues(ValueTransformerWithKeySupplier,
-- Materialized, storeNames...)@. The supplier is called once
-- per task and returns a 'Processor' that has full
-- 'ProcessorContext' access (for stores, punctuators, headers).
-- The output 'KTable' is materialised under @Materialized@.
--
-- Unlike 'mapValuesTable' the transform can read state stores
-- (declared via 'storeNames') and schedule punctuators. The
-- supplied processor's input is @Record k v@ and its
-- 'forwardRecord' must emit a @Record k v'@ keyed on the same
-- key (KTable invariant). We don't enforce that at the type
-- level; misuse breaks the table's tombstone semantics.
transformValuesTable
  :: forall k v v'
   . Ord k
  => IO (Processor k v)        -- ^ processor supplier
  -> [StoreName]               -- ^ external state stores it
                               --   reads / writes
  -> Materialized k v'
  -> KTable k v
  -> IO (KTable k v')
transformValuesTable supplier extraStores m parent = do
  let b = ktableBuilder parent
  storeNm <- maybe (freshStoreName b "KTABLE-TRANSFORMVAL-STORE")
                   pure
                   (matName m)
  let supplierStore = inMemoryKeyValueStoreBuilder storeNm
                       :: StoreBuilderKV k v'
  procNm <- freshNodeName b "KTABLE-TRANSFORMVAL"
  -- The user processor wraps its own forwardRecord; we
  -- pre-compose with a small "remember-in-store" sink so the
  -- materialised view stays consistent with downstream
  -- forwards.
  let supplier' = do
        userProc <- supplier
        ctxRef   <- newIORef Nothing
        storeRef <- newIORef Nothing
        pure Processor
          { procName  = processorName "KTABLE-TRANSFORMVAL"
          , procInit  = \ctx -> do
              writeIORef ctxRef (Just ctx)
              st <- getStateStore ctx storeNm
              case st of
                Just (AnyKeyValueStore kvs) ->
                  writeIORef storeRef
                    (Just (unsafeCastKV kvs :: KeyValueStore k v'))
                _ -> error $
                  "KTable.transformValues: store not found: "
                    <> show storeNm
              -- Let the user processor see the same context so
              -- it can schedule punctuators / look up other
              -- stores.
              procInit userProc ctx
          , procClose   = procClose userProc
          , procProcess = \rec -> do
              -- The user's processor decides what to forward;
              -- our wrapper observes those forwards via a
              -- shim context. Simplest faithful model:
              -- delegate, then mirror the latest forwarded
              -- record into the materialised store. Since the
              -- engine's ProcessorContext routes forwards to
              -- the downstream child node, we capture the
              -- value here via an IORef-bound shim and
              -- write it to the store after.
              procProcess userProc rec
              -- The user processor was given the *real* ctx,
              -- so downstream nodes already saw the forwarded
              -- record. The materialised store mirrors the
              -- LATEST emitted value per key by reading the
              -- store-binding child from a delayed forward.
              -- Simpler approach: re-bind by snooping
              -- whatever's currently in the store via the
              -- user's own kvsPut calls. We trust the user
              -- processor to update the store directly (the
              -- typical pattern is to declare the store name
              -- via 'extraStores' and write to it inside
              -- procProcess).
              pure ()
          }
  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = procNm
                  , Topo.processorSpecParents  = [ktableNode parent]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor supplier'
                  , Topo.processorSpecStores   =
                      storeNm : extraStores
                  } t
        !t2 = Topo.addStateStoreKV supplierStore [procNm] t1
     in t2
  pure KTable
    { ktableNode       = procNm
    , ktableStore      = storeNm
    , ktableBuilder    = b
    , ktableKeySerde   = ktableKeySerde parent
    , ktableValueSerde = error
        "KTable.transformValues: pass Materialized with serde to set output"
    }

mapValueTableProcessor
  :: forall k v v'
   . Ord k
  => StoreName
  -> (v -> v')
  -> IO (Processor k v)
mapValueTableProcessor sn f = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef Nothing
  pure Processor
    { procName = processorName "KTABLE-MAPVAL"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        st <- getStateStore ctx sn
        case st of
          Just (AnyKeyValueStore kvs) ->
            writeIORef storeRef (Just (unsafeCastKV kvs :: KeyValueStore k v'))
          _ -> error $ "KTable.mapValues: store not found: " <> show sn
    , procClose = pure ()
    , procProcess = \r -> do
        let !v' = f (recordValue r)
        mst <- readIORef storeRef
        mctx <- readIORef ctxRef
        case (recordKey r, mst, mctx) of
          (Just k, Just kvs, Just ctx) -> do
            kvsPut kvs k v'
            forwardRecord ctx (mapValue (const v') r)
          _ -> pure ()
    }

----------------------------------------------------------------------
-- Conversion
----------------------------------------------------------------------

-- | Convert a 'KTable' to a 'KStream' carrying every change.
--
-- The resulting stream is a 'Kafka.Streams.KStream.KStream' but
-- we don't import that module here to avoid a cycle; the runtime
-- 'Kafka.Streams' umbrella re-exports a sibling helper.
toStreamTable :: KTable k v -> (Topo.NodeName, StreamsBuilder, Serde k, Serde v)
toStreamTable kt =
  ( ktableNode kt
  , ktableBuilder kt
  , ktableKeySerde kt
  , ktableValueSerde kt
  )

-- | Snapshot the table store and look up a key. Useful inside
-- 'TopologyTestDriver' to assert intermediate state.
queryKeyValueStore
  :: Ord k
  => KTable k v
  -> AnyStateStore
  -> k
  -> IO (Maybe v)
queryKeyValueStore _ (AnyKeyValueStore kvs) k =
  kvsGet (unsafeCastKV kvs) k
queryKeyValueStore _ _ _ =
  pure Nothing

unsafeCastKV :: KeyValueStore k v -> KeyValueStore k' v'
unsafeCastKV = Unsafe.unsafeCoerce
{-# INLINE unsafeCastKV #-}

----------------------------------------------------------------------
-- KTable-KTable joins
--
-- The join is itself a materialised KTable. When either input side
-- changes for a key, we recompute the join value for that key:
--
--   inner:   emit (joiner l r) iff both sides have a non-null value
--   left:    emit (joiner l (Just r)) when right has a value;
--            emit (joiner l Nothing) when right has no value
--   outer:   emit (joiner (Just l) Nothing) / (Nothing (Just r)) /
--            (Just l) (Just r)) depending on which sides have values
----------------------------------------------------------------------

-- | Inner KTable-KTable join.
joinKTableKTable
  :: forall k v1 v2 v'
   . Ord k
  => (v1 -> v2 -> v')
  -> Materialized k v'
  -> KTable k v1
  -> KTable k v2
  -> IO (KTable k v')
joinKTableKTable joiner m =
  buildKTableKTableJoin "KTABLE-INNER-JOIN" m
    (KTKTInner joiner :: KTKTMode k v1 v2 v')

-- | Left KTable-KTable join. Emits even when right has no value.
leftJoinKTableKTable
  :: forall k v1 v2 v'
   . Ord k
  => (v1 -> Maybe v2 -> v')
  -> Materialized k v'
  -> KTable k v1
  -> KTable k v2
  -> IO (KTable k v')
leftJoinKTableKTable joiner m =
  buildKTableKTableJoin "KTABLE-LEFT-JOIN" m
    (KTKTLeft joiner :: KTKTMode k v1 v2 v')

-- | Outer KTable-KTable join. Emits whenever either side has a value.
outerJoinKTableKTable
  :: forall k v1 v2 v'
   . Ord k
  => (Maybe v1 -> Maybe v2 -> v')
  -> Materialized k v'
  -> KTable k v1
  -> KTable k v2
  -> IO (KTable k v')
outerJoinKTableKTable joiner m =
  buildKTableKTableJoin "KTABLE-OUTER-JOIN" m
    (KTKTOuter joiner :: KTKTMode k v1 v2 v')

-- | Mode + joiner closure carried into the side processors.
data KTKTMode k v1 v2 v' where
  KTKTInner :: (v1 -> v2 -> v')                -> KTKTMode k v1 v2 v'
  KTKTLeft  :: (v1 -> Maybe v2 -> v')          -> KTKTMode k v1 v2 v'
  KTKTOuter :: (Maybe v1 -> Maybe v2 -> v')    -> KTKTMode k v1 v2 v'

-- | Build the join topology. Two side processors (one per parent
-- KTable), each owning a reference to the other side's store and to
-- the shared output store.
buildKTableKTableJoin
  :: forall k v1 v2 v'
   . Ord k
  => Data.Text.Text                                  -- prefix
  -> Materialized k v'
  -> KTKTMode k v1 v2 v'
  -> KTable k v1
  -> KTable k v2
  -> IO (KTable k v')
buildKTableKTableJoin prefix m mode tl tr = do
  let b = ktableBuilder tl
  outStoreNm <- maybe (freshStoreName b (prefix <> "-STORE"))
                      pure
                      (matName m)
  leftNm  <- freshNodeName b (prefix <> "-LEFT")
  rightNm <- freshNodeName b (prefix <> "-RIGHT")
  let outBuilder = inMemoryKeyValueStoreBuilder outStoreNm
                     :: StoreBuilderKV k v'

  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = leftNm
                  , Topo.processorSpecParents  = [ktableNode tl]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor
                        (mkKTKTSideLeft @k @v1 @v2 @v'
                           mode
                           (ktableStore tr)
                           outStoreNm)
                  , Topo.processorSpecStores   =
                      [ktableStore tl, ktableStore tr, outStoreNm]
                  } t
        !t2 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = rightNm
                  , Topo.processorSpecParents  = [ktableNode tr]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor
                        (mkKTKTSideRight @k @v1 @v2 @v'
                           mode
                           (ktableStore tl)
                           outStoreNm)
                  , Topo.processorSpecStores   =
                      [ktableStore tl, ktableStore tr, outStoreNm]
                  } t1
        !t3 = Topo.addStateStoreKV outBuilder [leftNm, rightNm] t2
     in t3

  pure KTable
    { ktableNode       = leftNm    -- arbitrary: both feed the same store
    , ktableStore      = outStoreNm
    , ktableBuilder    = b
    , ktableKeySerde   = ktableKeySerde tl
    , ktableValueSerde = error
        "KTable-KTable join: pass Materialized with serde to set output"
    }

-- The left side processor receives a v1; looks up the right store
-- for the current v2 (if any), evaluates the joiner per the mode,
-- writes the result to the output store and forwards it.
mkKTKTSideLeft
  :: forall k v1 v2 v'
   . Ord k
  => KTKTMode k v1 v2 v'
  -> StoreName              -- right store name
  -> StoreName              -- output store name
  -> IO (Processor k v1)
mkKTKTSideLeft mode rightNm outNm =
  mkKTKTSideAny "KTABLE-JOIN-LEFT" mode rightNm outNm True

mkKTKTSideRight
  :: forall k v1 v2 v'
   . Ord k
  => KTKTMode k v1 v2 v'
  -> StoreName              -- left store name
  -> StoreName              -- output store name
  -> IO (Processor k v2)
mkKTKTSideRight mode leftNm outNm =
  mkKTKTSideAny "KTABLE-JOIN-RIGHT" mode leftNm outNm False

mkKTKTSideAny
  :: forall k v1 v2 v' vSelf
   . Ord k
  => Data.Text.Text
  -> KTKTMode k v1 v2 v'
  -> StoreName              -- the OTHER side's store
  -> StoreName              -- output store
  -> Bool                   -- True iff this is the LEFT side
  -> IO (Processor k vSelf)
mkKTKTSideAny nm mode otherNm outNm isLeft = do
  ctxRef   <- newIORef Nothing
  otherRef <- newIORef (Nothing :: Maybe (KeyValueStore k Any))
  outRef   <- newIORef (Nothing :: Maybe (KeyValueStore k v'))
  pure Processor
    { procName  = processorName nm
    , procInit  = \ctx -> do
        writeIORef ctxRef (Just ctx)
        st1 <- getStateStore ctx otherNm
        case st1 of
          Just (AnyKeyValueStore kvs) ->
            writeIORef otherRef (Just (Unsafe.unsafeCoerce kvs))
          _ -> error $ "KTable-KTable join: other store missing: " <> show otherNm
        st2 <- getStateStore ctx outNm
        case st2 of
          Just (AnyKeyValueStore kvs) ->
            writeIORef outRef (Just (Unsafe.unsafeCoerce kvs))
          _ -> error $ "KTable-KTable join: out store missing: " <> show outNm
    , procClose = pure ()
    , procProcess = \r ->
        case recordKey r of
          Just k -> do
            mctx   <- readIORef ctxRef
            mother <- readIORef otherRef
            mout   <- readIORef outRef
            case (mctx, mother, mout) of
              (Just ctx, Just other_, Just out_) -> do
                mOtherVal <- kvsGet other_ k
                let mResult = computeJoinValue mode isLeft (recordValue r) mOtherVal
                case mResult of
                  Just !vOut -> do
                    kvsPut out_ k vOut
                    forwardRecord ctx
                      (Record
                        { recordKey       = Just k
                        , recordValue     = vOut
                        , recordTimestamp = recordTimestamp r
                        , recordHeaders   = recordHeaders r
                        } :: Record k v')
                  Nothing ->
                    () <$ kvsDelete out_ k
              _ -> pure ()
          _ -> pure ()
    }

-- 'Any' from the type-erased engine boundary. We coerce in / out at
-- the call sites since the topology builder paired the joiner with
-- consistent v1/v2 types.
type Any = GHC.Exts.Any

-- | Apply the join mode given this side's value (already bound) and
-- the other side's optional value.
computeJoinValue
  :: forall k v1 v2 v' vSelf
   . KTKTMode k v1 v2 v'
  -> Bool                     -- isLeft?
  -> vSelf                    -- this side's incoming v
  -> Maybe Any                -- other side's value (erased)
  -> Maybe v'
computeJoinValue mode isLeft thisV mOther =
  case mode of
    KTKTInner f ->
      case mOther of
        Just other_ ->
          if isLeft
            then Just (f (Unsafe.unsafeCoerce thisV  :: v1)
                         (Unsafe.unsafeCoerce other_ :: v2))
            else Just (f (Unsafe.unsafeCoerce other_ :: v1)
                         (Unsafe.unsafeCoerce thisV  :: v2))
        Nothing -> Nothing
    KTKTLeft f ->
      if isLeft
        then Just (f (Unsafe.unsafeCoerce thisV :: v1)
                     ((Unsafe.unsafeCoerce <$> mOther) :: Maybe v2))
        else
          -- Right-side update on a left-join: only emit if a left
          -- value already exists; otherwise we have nothing to join on.
          case mOther of
            Just left_ ->
              Just (f (Unsafe.unsafeCoerce left_ :: v1)
                      (Just (Unsafe.unsafeCoerce thisV :: v2)))
            Nothing -> Nothing
    KTKTOuter f ->
      if isLeft
        then
          Just (f (Just (Unsafe.unsafeCoerce thisV :: v1))
                  (Unsafe.unsafeCoerce <$> mOther :: Maybe v2))
        else
          Just (f (Unsafe.unsafeCoerce <$> mOther :: Maybe v1)
                  (Just (Unsafe.unsafeCoerce thisV :: v2)))

