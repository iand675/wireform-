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

  -- * Emit strategy (KIP-825)
  EmitStrategy (..),
  emitOnWindowUpdate,
  emitOnWindowClose,
  withEmitStrategy,
) where

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


{- | Result of a windowed aggregation: a (synthetic) handle to a
window-store materialisation.  Convert into a stream of windowed
records using 'toStreamWindowed'.
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


{- | Per-window reduce processor. Mirrors the JVM
@TimeWindowedKStream.reduce(adder)@ semantics: the first
record assigned to a window stores its value verbatim;
subsequent records combine via @adder(existing, new)@. We
can't model this through 'aggregateWindowed' without a
'Maybe v' wrapper because the JVM contract has no
"initialiser" — the first value /is/ the seed.
-}
windowedReduceProc
  :: forall k v
   . Ord k
  => StoreName
  -> Windows
  -> (v -> v -> v)
  -> IO (Processor k v)
windowedReduceProc sn ws combine = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (WindowStore k v))
  pure
    Processor
      { procName = processorName "WINDOWED-REDUCE"
      , procInit = \ctx -> do
          writeIORef ctxRef (Just ctx)
          st <- getStateStore ctx sn
          case st of
            Just (AnyWindowStore wstore) ->
              writeIORef storeRef (Just (unsafeCastWS wstore))
            _ -> error $ "windowedReduceProc: store not found: " <> show sn
      , procClose = pure ()
      , procProcess = \r -> do
          mctx <- readIORef ctxRef
          mst <- readIORef storeRef
          case (mctx, mst, recordKey r) of
            (Just ctx, Just store_, Just k) -> do
              Timestamp now <- ctxStreamTime ctx
              let !grace = windowsGracePeriod ws
                  !winSize = windowsSize ws
                  Timestamp rt = recordTimestamp r
                  !rightEdge = rt + winSize
                  isExpired = rightEdge + grace < now
              if isExpired
                then pure ()
                else do
                  let !windows = windowsAssign ws (recordTimestamp r)
                  mapM_
                    ( \(Window startT _) -> do
                        mPrev <- wsFetch store_ k startT
                        let !next = case mPrev of
                              Just acc -> combine acc (recordValue r)
                              Nothing -> recordValue r
                        wsPut store_ k next startT
                        forwardRecord ctx r {recordValue = next}
                    )
                    windows
            _ -> pure ()
      }


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
   . Ord k
  => StoreName
  -> Windows
  -> IO a
  -> (k -> v -> a -> a)
  -> IO (Processor k v)
windowedAggProc sn ws initial agg = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (WindowStore k a))
  pure
    Processor
      { procName = processorName "WINDOWED-AGG"
      , procInit = \ctx -> do
          writeIORef ctxRef (Just ctx)
          st <- getStateStore ctx sn
          case st of
            Just (AnyWindowStore wstore) ->
              writeIORef storeRef (Just (unsafeCastWS wstore))
            _ -> error $ "windowedAggProc: store not found: " <> show sn
      , procClose = pure ()
      , procProcess = \r -> do
          mctx <- readIORef ctxRef
          mst <- readIORef storeRef
          case (mctx, mst, recordKey r) of
            (Just ctx, Just store_, Just k) -> do
              -- Grace-period enforcement: skip records whose latest
              -- assignable window has fully closed below stream-time -
              -- grace. Mirrors the "late record drop" behaviour of
              -- KIP-633.
              Timestamp now <- ctxStreamTime ctx
              let !grace = windowsGracePeriod ws
                  !winSize = windowsSize ws
                  Timestamp rt = recordTimestamp r
                  !rightEdge = rt + winSize
                  isExpired = rightEdge + grace < now
              if isExpired
                then pure () -- silently drop the late record
                else do
                  let !windows = windowsAssign ws (recordTimestamp r)
                  mapM_
                    ( \(Window startT _) -> do
                        mPrev <- wsFetch store_ k startT
                        cur <- maybe initial pure mPrev
                        let !next = agg k (recordValue r) cur
                        wsPut store_ k next startT
                        forwardRecord ctx r {recordValue = next}
                    )
                    windows
            _ -> pure ()
      }


unsafeCastWS :: WindowStore k v -> WindowStore k' v'
unsafeCastWS = Unsafe.unsafeCoerce
{-# INLINE unsafeCastWS #-}
