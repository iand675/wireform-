{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.TimeWindowedKStream
Description : TimeWindowedKStream + windowed aggregations

Mirrors @org.apache.kafka.streams.kstream.TimeWindowedKStream@.
-}
module Kafka.Streams.TimeWindowedKStream (
  TimeWindowedKStream,
  twksParent,
  twksKey,
  twksValue,
  twksBuilder,
  twksWindows,
  countWindowed,
  aggregateWindowed,
  reduceWindowed,
  WindowedTableHandle (..),
  windowedUpdateProc,

  -- * Emit strategy (KIP-825)
  EmitStrategy (..),
  emitOnWindowUpdate,
  emitOnWindowClose,
  withEmitStrategy,
) where

import Control.Monad (forM_, when)
import Data.IORef
import Data.Int (Int64)
import Kafka.Streams.KGroupedStream (
  TimeWindowedKStream (..),
  groupedInputOf,
 )
import Kafka.Streams.Materialized (Materialized (..), materializedKV, materializedS, materializedW)
import Kafka.Streams.Processor (Processor (..), ProcessorContext (..), forwardRecord, getStateStore, processorName)
import Kafka.Streams.State.Store (
  AnyStateStore (..),
  StoreBuilderW,
  StoreName,
  WindowStore (..),
  WindowedKey (..),
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
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Types (Record (..))
import Kafka.Streams.Window (
  Window (..),
  Windows (..),
 )
import Unsafe.Coerce qualified as Unsafe


{- | KIP-825 emit policy. The default ('OnWindowUpdate') emits a
downstream record on every record that updates a window's
aggregate. 'OnWindowClose' buffers until stream-time has
advanced past the window's end + grace period and emits a
single record per window.
-}
data EmitStrategy
  = OnWindowUpdate
  | OnWindowClose
  deriving stock (Eq, Show)


-- | The JVM-default 'EmitStrategy'. Every update propagates.
emitOnWindowUpdate :: EmitStrategy
emitOnWindowUpdate = OnWindowUpdate


{- | Emit one record per window after it closes; equivalent to
@suppress(Suppressed.untilWindowCloses(unbounded()))@ in
effect but expressed at the windowed-aggregation level so
there's no separate downstream node.
-}
emitOnWindowClose :: EmitStrategy
emitOnWindowClose = OnWindowClose


{- | Result of a windowed aggregation: its store, and the node that
forwards every update as a @(WindowedKey k, v)@ record
("Kafka.Streams.Suppress" turns it into a stream).
-}
data WindowedTableHandle k v = WindowedTableHandle
  { wthNode :: !Topo.NodeName
  , wthStore :: !StoreName
  , wthBuilder :: !StreamsBuilder
  , wthWindows :: !Windows
  , wthEmit :: !EmitStrategy
  {- ^ KIP-825: how the downstream-as-stream adapter should
  emit. 'OnWindowUpdate' (the JVM default) emits per
  update; 'OnWindowClose' buffers until stream-time has
  advanced past windowEnd + grace.
  -}
  }


{- | Override the emit strategy on an existing handle. Used by
'streamFromWindowedHandle' downstream.
-}
withEmitStrategy
  :: EmitStrategy
  -> WindowedTableHandle k v
  -> WindowedTableHandle k v
withEmitStrategy e h = h {wthEmit = e}


countWindowed
  :: forall k v
   . Ord k
  => Materialized k Int64
  -> TimeWindowedKStream k v
  -> IO (WindowedTableHandle k Int64)
countWindowed =
  aggregateWindowed @k @v @Int64 (pure 0) (\_ _ acc -> acc + 1)


reduceWindowed
  :: forall k v
   . Ord k
  => (v -> v -> v)
  -> Materialized k v
  -> TimeWindowedKStream k v
  -> IO (WindowedTableHandle k v)
reduceWindowed combine m twks = do
  let b = twksBuilder twks
      ws = twksWindows twks
  storeNm <-
    maybe
      (freshStoreName b "WINDOWED-REDUCE-STORE")
      pure
      (matName m)
  let supplier =
        materializedW m storeNm (windowsSize ws) (windowsRetention ws) (twksKey twks) (twksValue twks)
          :: StoreBuilderW k v
  procNm <- freshNodeName b "WINDOWED-REDUCE"
  input <- groupedInputOf b (twksParent twks) (twksKey twks) (twksValue twks) (twksRepartition twks)
  withTopology_ b $ \t ->
    let !t1 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = procNm
              , Topo.processorSpecParents = [input]
              , Topo.processorSpecSupplier =
                  Topo.AnyProcessor
                    (windowedReduceProc @k @v storeNm ws combine)
              , Topo.processorSpecStores = [storeNm]
              }
            t
        !t2 = Topo.addStateStoreW supplier [procNm] t1
    in t2
  pure
    WindowedTableHandle
      { wthNode = procNm
      , wthStore = storeNm
      , wthBuilder = b
      , wthWindows = ws
      , wthEmit = emitOnWindowUpdate
      }


{- | The windowed update processor (see 'windowedUpdateProc') for a
reduce: the first record of a window seeds it, later ones combine
with @adder existing new@ (the JVM's @TimeWindowedKStream.reduce@).
-}
windowedReduceProc
  :: forall k v
   . StoreName
  -> Windows
  -> (v -> v -> v)
  -> IO (Processor k v)
windowedReduceProc sn ws combine =
  windowedUpdateProc sn ws (\_ v prev -> pure (maybe v (`combine` v) prev))


aggregateWindowed
  :: forall k v a
   . Ord k
  => IO a
  -> (k -> v -> a -> a)
  -> Materialized k a
  -> TimeWindowedKStream k v
  -> IO (WindowedTableHandle k a)
aggregateWindowed initial agg m twks = do
  let b = twksBuilder twks
      ws = twksWindows twks
  storeNm <-
    maybe
      (freshStoreName b "WINDOWED-AGG-STORE")
      pure
      (matName m)
  let supplier =
        materializedW m storeNm (windowsSize ws) (windowsRetention ws) (twksKey twks) (error "aggregate: pass Materialized with a value serde for the aggregate type")
          :: StoreBuilderW k a
  nodeNm <- freshNodeName b "WINDOWED-AGG"
  input <- groupedInputOf b (twksParent twks) (twksKey twks) (twksValue twks) (twksRepartition twks)
  withTopology_ b $ \t ->
    let !t1 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = nodeNm
              , Topo.processorSpecParents = [input]
              , Topo.processorSpecSupplier =
                  Topo.AnyProcessor
                    (windowedAggProc @k @v @a storeNm ws initial agg)
              , Topo.processorSpecStores = []
              }
            t
        !t2 = Topo.addStateStoreW supplier [nodeNm] t1
    in t2
  pure
    WindowedTableHandle
      { wthNode = nodeNm
      , wthStore = storeNm
      , wthBuilder = b
      , wthWindows = ws
      , wthEmit = emitOnWindowUpdate
      }


windowedAggProc
  :: forall k v a
   . StoreName
  -> Windows
  -> IO a
  -> (k -> v -> a -> a)
  -> IO (Processor k v)
windowedAggProc sn ws initial agg =
  windowedUpdateProc sn ws (\k v prev -> agg k v <$> maybe initial pure prev)


{- | Fold a record into every window it falls in that is still open,
and forward each window's new aggregate keyed by the window. A window
is open while its end is past the processor's stream time (the latest
timestamp it has seen) minus the grace period; a record's closed
windows are skipped, its open ones still updated (the JVM's
@KStreamWindowAggregate@).
-}
windowedUpdateProc
  :: forall k v a
   . StoreName
  -> Windows
  -> (k -> v -> Maybe a -> IO a)
  -> IO (Processor k v)
windowedUpdateProc sn ws step = do
  ref <- newIORef Nothing
  observed <- newIORef (minBound :: Int64)
  pure
    Processor
      { procName = processorName "WINDOWED-AGG"
      , procInit = \ctx -> do
          st <- getStateStore ctx sn
          case st of
            Just (AnyWindowStore wstore) -> writeIORef ref (Just (ctx, unsafeCastWS wstore :: WindowStore k a))
            _ -> error $ "windowed aggregation: store not found: " <> show sn
      , procClose = pure ()
      , procProcess = \r -> do
          m <- readIORef ref
          case (m, recordKey r) of
            (Just (ctx, store_), Just k) -> do
              let Timestamp ts = recordTimestamp r
              now <- atomicModifyIORef' observed (\o -> let !n = max o ts in (n, n))
              let !closeTime = now - windowsGracePeriod ws
              forM_ (windowsAssign ws (recordTimestamp r)) $ \(Window start (Timestamp end)) ->
                when (end > closeTime) $ do
                  prev <- wsFetch store_ k start
                  !next <- step k (recordValue r) prev
                  wsPut store_ k next start
                  forwardRecord ctx (Record (Just (WindowedKey k start)) next (recordTimestamp r) (recordHeaders r))
            _ -> pure ()
      }


unsafeCastWS :: WindowStore k v -> WindowStore k' v'
unsafeCastWS = Unsafe.unsafeCoerce
{-# INLINE unsafeCastWS #-}
