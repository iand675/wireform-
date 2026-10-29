{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Streams.KTable
Description : KTable DSL surface

A 'KTable' is the changelog view of a 'KStream': for each key, only
the latest value matters. KTables are always backed by a state
store, and the DSL operations preserve that invariant.

Mirrors @org.apache.kafka.streams.kstream.KTable<K,V>@.
-}
module Kafka.Streams.KTable (
  KTable (..),
  Change (..),
  changeSerde,
  ktableNode,
  ktableStore,
  ktableBuilder,
  ktableKeySerde,
  ktableValueSerde,

  -- * Source
  tableFromTopic,

  -- * Stateless
  filterTable,
  filterNotTable,
  mapValuesTable,

  -- * Stateful value transforms
  transformValuesTable,

  -- * Joins
  joinKTableKTable,
  leftJoinKTableKTable,
  outerJoinKTableKTable,

  -- * Conversion
  toStreamTable,
  queryKeyValueStore,
) where

import Data.Bits (testBit, (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BSL
import Data.IORef
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified
import Kafka.Streams.Consumed (Consumed (..))
import Kafka.Streams.Internal.Serdes (lengthPrefixed, splitLengthPrefixed)
import Kafka.Streams.Materialized (Materialized (..), materializedKV)
import Kafka.Streams.Processor (
  Processor (..),
  ProcessorContext (..),
  forwardRecord,
  getStateStore,
  processorName,
 )
import Kafka.Streams.Serde (HasSerde (..), Serde (..))
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
import Kafka.Streams.Types (
  Record (..),
  TopicName,
 )
import Unsafe.Coerce qualified as Unsafe


-- | KTable handle.
data KTable k v = KTable
  { ktableNode :: !Topo.NodeName
  -- ^ The node forwarding the table's 'Change's
  , ktableStore :: !StoreName
  , ktableBuilder :: !StreamsBuilder
  , ktableKeySerde :: ~(Serde k)
  , ktableValueSerde :: ~(Serde v)
  }


{- | What a table node forwards for a key: its new value and the one it
replaces (the JVM's @Change<V>@ with old values on). 'changeNew' is
'Nothing' for a delete, 'changeOld' for a key that had no value.

Carrying the old value is what makes tables composable: a filter or
join can retract a row it emitted before, and a re-grouped
aggregation can subtract the row's previous contribution from the
group it used to belong to.
-}
data Change v = Change
  { changeNew :: !(Maybe v)
  , changeOld :: !(Maybe v)
  }
  deriving stock (Eq, Show, Functor)


{- | A 'Change' on the wire (repartition topics): a flags byte (bit 0:
new present, bit 1: old present) then the present values,
length-prefixed.
-}
changeSerde :: Serde v -> Serde (Change v)
changeSerde vs =
  Serde
    { serialize = \(Change n o) ->
        BSL.toStrict . BB.toLazyByteString $
          BB.word8 ((if isJust n then 1 else 0) .|. (if isJust o then 2 else 0))
            <> maybe mempty (lengthPrefixed . serialize vs) n
            <> maybe mempty (lengthPrefixed . serialize vs) o
    , deserialize = \bs -> case BS.uncons bs of
        Nothing -> Left "empty change"
        Just (flags, rest) -> do
          (n, rest') <-
            if testBit flags 0
              then do
                (b, r) <- splitLengthPrefixed rest
                v <- deserialize vs b
                pure (Just v, r)
              else pure (Nothing, rest)
          o <-
            if testBit flags 1
              then do
                (b, _) <- splitLengthPrefixed rest'
                Just <$> deserialize vs b
              else pure Nothing
          pure (Change n o)
    , serializeHeaders = const mempty
    }


-- | Forward a change for @k@, unless nothing changed hands.
emitChange :: ProcessorContext -> Record kin vin -> k -> Change v -> IO ()
emitChange ctx r k c = case c of
  Change Nothing Nothing -> pure ()
  _ -> forwardRecord ctx r {recordKey = Just k, recordValue = c}


{- | Set @k@'s row to @new@ (a delete when 'Nothing') and forward the
change, with the row's previous value from the store as the old one.
-}
updateRow :: ProcessorContext -> KeyValueStore k v -> Record kin vin -> k -> Maybe v -> IO ()
updateRow ctx kvs r k new = do
  old <- kvsGet kvs k
  writeTable kvs k new
  emitChange ctx r k (Change new old)


-- | Write a table's new value for @k@ (a delete when 'Nothing').
writeTable :: KeyValueStore k v -> k -> Maybe v -> IO ()
writeTable kvs k = \case
  Just v -> kvsPut kvs k v
  Nothing -> () <$ kvsDelete kvs k


{- | A table processor: binds its context and store @sn@ at init, then
runs @step@ for every keyed record (keyless ones are dropped: a table
row needs a key).
-}
tableProcessor
  :: forall k vin vs
   . Data.Text.Text
  -> StoreName
  -> (ProcessorContext -> KeyValueStore k vs -> Record k vin -> k -> IO ())
  -> IO (Processor k vin)
tableProcessor nm sn step = do
  ref <- newIORef Nothing
  pure
    Processor
      { procName = processorName nm
      , procInit = \ctx -> do
          st <- getStateStore ctx sn
          case st of
            Just (AnyKeyValueStore kvs) -> writeIORef ref (Just (ctx, unsafeCastKV kvs :: KeyValueStore k vs))
            _ -> error (Data.Text.unpack nm <> ": store not found: " <> show sn)
      , procClose = pure ()
      , procProcess = \r -> do
          m <- readIORef ref
          case (m, recordKey r) of
            (Just (ctx, kvs), Just k) -> step ctx kvs r k
            _ -> pure ()
      }


{- | Materialise a topic as a 'KTable'. Each (key,value)
updates the store; tombstones (null value) delete the key.

/JVM equivalent:/ @StreamsBuilder.table(topic, Consumed, Materialized)@.
-}
tableFromTopic
  :: forall k v
   . Ord k
  => StreamsBuilder
  -> TopicName
  -> Consumed k v
  -> Materialized k v
  -> IO (KTable k v)
tableFromTopic b topic c m = do
  storeNm <-
    maybe
      (freshStoreName b "KTABLE-SOURCE-STORE")
      pure
      (matName m)
  let supplier =
        materializedKV m storeNm (consumedKeySerde c) (consumedValueSerde c)
          :: StoreBuilderKV k v
  sourceNm <- freshNodeName b "KTABLE-SOURCE"
  procNm <- freshNodeName b "KTABLE-SOURCE-PROCESSOR"
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
                  Topo.AnyProcessor (sourceTableProcessor @k @v storeNm)
              , Topo.processorSpecStores = []
              }
            t1
        !t3 = Topo.addStateStoreKV supplier [procNm] t2
    in t3
  pure
    KTable
      { ktableNode = procNm
      , ktableStore = storeNm
      , ktableBuilder = b
      , ktableKeySerde = consumedKeySerde c
      , ktableValueSerde = consumedValueSerde c
      }


-- | The source's @Maybe v@ (null for a tombstone) becomes the table's change.
sourceTableProcessor :: forall k v. StoreName -> IO (Processor k (Maybe v))
sourceTableProcessor sn =
  tableProcessor "KTABLE-SOURCE-PROCESSOR" sn $ \ctx kvs r k ->
    updateRow ctx kvs r k (recordValue r)


----------------------------------------------------------------------
-- Stateless
----------------------------------------------------------------------

{- | Keep the rows the predicate accepts. A row that stops matching is
deleted from the result (and the delete forwarded), so downstream
tables never keep a row the filter has since rejected.

/JVM equivalent:/ @KTable.filter(Predicate, Materialized)@.
-}
filterTable
  :: forall k v
   . Ord k
  => (Record k v -> Bool)
  -> Materialized k v
  -> KTable k v
  -> IO (KTable k v)
filterTable pred_ m parent =
  derivedTable "KTABLE-FILTER" m parent (ktableValueSerde parent) $ \sn ->
    tableProcessor "KTABLE-FILTER" sn $ \ctx kvs r k -> do
      let new = changeNew (recordValue r) >>= \v -> if pred_ r {recordValue = v} then Just v else Nothing
      updateRow ctx kvs r k new


-- | The rows the predicate rejects. Mirrors @KTable.filterNot@.
filterNotTable
  :: forall k v
   . Ord k
  => (Record k v -> Bool)
  -> Materialized k v
  -> KTable k v
  -> IO (KTable k v)
filterNotTable p = filterTable (not . p)


{- | Apply a function to every value in the table.

/JVM equivalent:/ @KTable.mapValues(ValueMapper, Materialized)@.
-}
mapValuesTable
  :: forall k v v'
   . (Ord k, HasSerde v')
  => (v -> v')
  -> Materialized k v'
  -> KTable k v
  -> IO (KTable k v')
mapValuesTable f m parent =
  derivedTable "KTABLE-MAPVAL" m parent serde $ \sn ->
    tableProcessor "KTABLE-MAPVAL" sn $ \ctx kvs r k ->
      updateRow ctx kvs r k (f <$> changeNew (recordValue r :: Change v))


{- | A table derived row by row from @parent@, materialised under @m@
(store name fresh unless @m@ names it) with value serde @vs@ (unless
@m@ sets one). @mk@ gets the store name and builds the processor.
-}
derivedTable
  :: forall k v v'
   . Ord k
  => Data.Text.Text
  -> Materialized k v'
  -> KTable k v
  -> Serde v'
  -> (StoreName -> IO (Processor k (Change v)))
  -> IO (KTable k v')
derivedTable prefix m parent vs mk = do
  let b = ktableBuilder parent
  storeNm <- maybe (freshStoreName b (prefix <> "-STORE")) pure (matName m)
  procNm <- freshNodeName b prefix
  let supplier = materializedKV m storeNm (ktableKeySerde parent) vs :: StoreBuilderKV k v'
  withTopology_ b $ \t ->
    Topo.addStateStoreKV
      supplier
      [procNm]
      ( Topo.addProcessorWith
          Topo.ProcessorSpec
            { Topo.processorSpecName = procNm
            , Topo.processorSpecParents = [ktableNode parent]
            , Topo.processorSpecSupplier = Topo.AnyProcessor (mk storeNm)
            , Topo.processorSpecStores = []
            }
          t
      )
  pure
    KTable
      { ktableNode = procNm
      , ktableStore = storeNm
      , ktableBuilder = b
      , ktableKeySerde = ktableKeySerde parent
      , ktableValueSerde = fromMaybe vs (matValueSerde m)
      }


{- | Stateful value transform on a 'KTable'. Mirrors Java's
@KTable.transformValues(ValueTransformerWithKeySupplier,
Materialized, storeNames...)@. The supplier is called once per task
and returns a 'Processor' with full 'ProcessorContext' access (for
stores, punctuators, headers).

The processor sees the table's new values (@Record k v@); every
record it forwards becomes the row's new value in the result
(materialised under @Materialized@, whose value serde is required).
A deleted input row is deleted from the result without consulting
the processor.
-}
transformValuesTable
  :: forall k v v'
   . Ord k
  => IO (Processor k v)
  -- ^ processor supplier
  -> [StoreName]
  -- ^ external state stores it reads / writes
  -> Materialized k v'
  -> KTable k v
  -> IO (KTable k v')
transformValuesTable supplier extraStores m parent = do
  let b = ktableBuilder parent
      outSerde =
        fromMaybe
          (error "KTable.transformValues: pass Materialized with a value serde for the result")
          (matValueSerde m)
  storeNm <- maybe (freshStoreName b "KTABLE-TRANSFORMVAL-STORE") pure (matName m)
  procNm <- freshNodeName b "KTABLE-TRANSFORMVAL"
  let supplierStore = materializedKV m storeNm (ktableKeySerde parent) outSerde :: StoreBuilderKV k v'
      wrapped = do
        userProc <- supplier
        ref <- newIORef Nothing
        pure
          Processor
            { procName = processorName "KTABLE-TRANSFORMVAL"
            , procInit = \ctx -> do
                kvs <- getStateStore ctx storeNm >>= \case
                  Just (AnyKeyValueStore kvs) -> pure (unsafeCastKV kvs :: KeyValueStore k v')
                  _ -> error ("KTable.transformValues: store not found: " <> show storeNm)
                writeIORef ref (Just (ctx, kvs))
                -- The user's forwards become the row's new value.
                let shim =
                      ctx
                        { ctxForward = \out -> case recordKey out of
                            Nothing -> pure ()
                            Just ko -> do
                              let k = Unsafe.unsafeCoerce ko :: k
                                  v = Unsafe.unsafeCoerce (recordValue out) :: v'
                              updateRow ctx kvs out k (Just v)
                        }
                procInit userProc shim
            , procClose = procClose userProc
            , procProcess = \r -> do
                m' <- readIORef ref
                case (m', recordKey r) of
                  (Just (ctx, kvs), Just k) -> case recordValue r of
                    Change (Just v) _ -> procProcess userProc r {recordValue = v}
                    Change Nothing _ -> updateRow ctx kvs r k (Nothing :: Maybe v')
                  _ -> pure ()
            }
  withTopology_ b $ \t ->
    Topo.addStateStoreKV
      supplierStore
      [procNm]
      ( Topo.addProcessorWith
          Topo.ProcessorSpec
            { Topo.processorSpecName = procNm
            , Topo.processorSpecParents = [ktableNode parent]
            , Topo.processorSpecSupplier = Topo.AnyProcessor wrapped
            , Topo.processorSpecStores = storeNm : extraStores
            }
          t
      )
  pure
    KTable
      { ktableNode = procNm
      , ktableStore = storeNm
      , ktableBuilder = b
      , ktableKeySerde = ktableKeySerde parent
      , ktableValueSerde = outSerde
      }


----------------------------------------------------------------------
-- Conversion
----------------------------------------------------------------------

{- | The pieces 'Kafka.Streams.KStream.toKStreamFromKTable' builds a
stream from (that module imports this one).
-}
toStreamTable :: KTable k v -> (Topo.NodeName, StreamsBuilder, Serde k, Serde v)
toStreamTable kt =
  ( ktableNode kt
  , ktableBuilder kt
  , ktableKeySerde kt
  , ktableValueSerde kt
  )


{- | Snapshot the table store and look up a key. Useful inside
'TopologyTestDriver' to assert intermediate state.
-}
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
-- The join is itself a materialised KTable. A change on either side
-- for a key recomputes that key's join row from the change's new and
-- old value and the other side's current value:
--
--   inner:   a row iff both sides have a value
--   left:    a row iff the left side has a value
--   outer:   a row iff either side has a value
--
-- and forwards the change of the join row.
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
joinKTableKTable joiner =
  buildKTableKTableJoin "KTABLE-INNER-JOIN" (\l r -> joiner <$> l <*> r)


-- | Left KTable-KTable join: a row for every left row.
leftJoinKTableKTable
  :: forall k v1 v2 v'
   . Ord k
  => (v1 -> Maybe v2 -> v')
  -> Materialized k v'
  -> KTable k v1
  -> KTable k v2
  -> IO (KTable k v')
leftJoinKTableKTable joiner =
  buildKTableKTableJoin "KTABLE-LEFT-JOIN" (\l r -> (`joiner` r) <$> l)


-- | Outer KTable-KTable join: a row whenever either side has one.
outerJoinKTableKTable
  :: forall k v1 v2 v'
   . Ord k
  => (Maybe v1 -> Maybe v2 -> v')
  -> Materialized k v'
  -> KTable k v1
  -> KTable k v2
  -> IO (KTable k v')
outerJoinKTableKTable joiner =
  buildKTableKTableJoin
    "KTABLE-OUTER-JOIN"
    (\l r -> case (l, r) of (Nothing, Nothing) -> Nothing; _ -> Just (joiner l r))


{- | Two side processors (each reading the other side's store and
writing the shared result store) and a merge node forwarding both
sides' changes. @row@ computes a key's join row from the two sides'
values.
-}
buildKTableKTableJoin
  :: forall k v1 v2 v'
   . Ord k
  => Data.Text.Text
  -> (Maybe v1 -> Maybe v2 -> Maybe v')
  -> Materialized k v'
  -> KTable k v1
  -> KTable k v2
  -> IO (KTable k v')
buildKTableKTableJoin prefix row m tl tr = do
  let b = ktableBuilder tl
      outSerde =
        fromMaybe
          (error "KTable-KTable join: pass Materialized with a value serde for the result")
          (matValueSerde m)
  outStoreNm <- maybe (freshStoreName b (prefix <> "-STORE")) pure (matName m)
  leftNm <- freshNodeName b (prefix <> "-LEFT")
  rightNm <- freshNodeName b (prefix <> "-RIGHT")
  mergeNm <- freshNodeName b (prefix <> "-MERGE")
  let outBuilder = materializedKV m outStoreNm (ktableKeySerde tl) outSerde :: StoreBuilderKV k v'
      side :: forall vSelf vOther. Data.Text.Text -> StoreName -> (Maybe vSelf -> Maybe vOther -> Maybe v') -> IO (Processor k (Change vSelf))
      side nm otherStore rowOf = do
        ref <- newIORef Nothing
        pure
          Processor
            { procName = processorName nm
            , procInit = \ctx -> do
                let bind :: forall a b. StoreName -> IO (KeyValueStore a b)
                    bind sn = getStateStore ctx sn >>= \case
                      Just (AnyKeyValueStore kvs) -> pure (unsafeCastKV kvs)
                      _ -> error ("KTable-KTable join: store missing: " <> show sn)
                other <- bind otherStore
                out <- bind outStoreNm
                writeIORef ref (Just (ctx, other :: KeyValueStore k vOther, out :: KeyValueStore k v'))
            , procClose = pure ()
            , procProcess = \r -> do
                m' <- readIORef ref
                case (m', recordKey r) of
                  (Just (ctx, other, out), Just k) -> do
                    o <- kvsGet other k
                    updateRow ctx out r k (rowOf (changeNew (recordValue r)) o)
                  _ -> pure ()
            }
  withTopology_ b $ \t ->
    let !t1 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = leftNm
              , Topo.processorSpecParents = [ktableNode tl]
              , Topo.processorSpecSupplier = Topo.AnyProcessor (side @v1 @v2 (prefix <> "-LEFT") (ktableStore tr) row)
              , Topo.processorSpecStores = [ktableStore tl, ktableStore tr, outStoreNm]
              }
            t
        !t2 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = rightNm
              , Topo.processorSpecParents = [ktableNode tr]
              , Topo.processorSpecSupplier = Topo.AnyProcessor (side @v2 @v1 (prefix <> "-RIGHT") (ktableStore tl) (flip row))
              , Topo.processorSpecStores = [ktableStore tl, ktableStore tr, outStoreNm]
              }
            t1
        !t3 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = mergeNm
              , Topo.processorSpecParents = [leftNm, rightNm]
              , Topo.processorSpecSupplier = Topo.AnyProcessor (passThrough @k @(Change v') (prefix <> "-MERGE"))
              , Topo.processorSpecStores = []
              }
            t2
    in Topo.addStateStoreKV outBuilder [leftNm, rightNm] t3
  pure
    KTable
      { ktableNode = mergeNm
      , ktableStore = outStoreNm
      , ktableBuilder = b
      , ktableKeySerde = ktableKeySerde tl
      , ktableValueSerde = outSerde
      }


-- | Forward everything unchanged.
passThrough :: forall k v. Data.Text.Text -> IO (Processor k v)
passThrough nm = do
  ref <- newIORef Nothing
  pure
    Processor
      { procName = processorName nm
      , procInit = writeIORef ref . Just
      , procClose = pure ()
      , procProcess = \r -> readIORef ref >>= mapM_ (`forwardRecord` r)
      }
