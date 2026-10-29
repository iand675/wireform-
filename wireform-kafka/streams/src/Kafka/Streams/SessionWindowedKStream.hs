{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Streams.SessionWindowedKStream
Description : SessionWindowedKStream + session aggregations
-}
module Kafka.Streams.SessionWindowedKStream (
  SessionWindowedKStream,
  countSessionWindowed,
  aggregateSessionWindowed,
  SessionWindowedTableHandle (..),
) where

import Control.Monad (forM_, unless)
import Data.IORef
import Data.Int (Int64)
import Kafka.Streams.KGroupedStream (
  SessionWindowedKStream (..),
  groupedInputOf,
 )
import Kafka.Streams.Materialized (Materialized (..), materializedKV, materializedS, materializedW)
import Kafka.Streams.Processor (
  Processor (..),
  forwardRecord,
  getStateStore,
  processorName,
 )
import Kafka.Streams.State.Session.InMemory (
  inMemorySessionStoreBuilder,
 )
import Kafka.Streams.State.Store (
  AnyStateStore (..),
  SessionKey (..),
  SessionStore (..),
  StoreBuilderS,
  StoreName,
  kvIteratorToList,
 )
import Kafka.Streams.StreamsBuilder (
  StreamsBuilder,
  freshNodeName,
  freshStoreName,
  withTopology_,
 )
import Kafka.Streams.Time qualified as Time
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Types (Record (..))
import Kafka.Streams.Window (SessionWindows (..))
import Unsafe.Coerce qualified as Unsafe


data SessionWindowedTableHandle k v = SessionWindowedTableHandle
  { swthNode :: !Topo.NodeName
  , swthStore :: !StoreName
  , swthBuilder :: !StreamsBuilder
  }


countSessionWindowed
  :: forall k v
   . Ord k
  => Materialized k Int64
  -> SessionWindowedKStream k v
  -> IO (SessionWindowedTableHandle k Int64)
countSessionWindowed =
  aggregateSessionWindowed @k @v @Int64
    (pure 0)
    (\_ _ acc -> acc + 1)
    (\_ a b -> a + b)


{- | Generic session aggregator. The 'merger' is invoked when two
adjacent sessions are merged into one (Java's @Merger<K, A>@).
-}
aggregateSessionWindowed
  :: forall k v a
   . Ord k
  => IO a
  -> (k -> v -> a -> a)
  -> (k -> a -> a -> a)
  -> Materialized k a
  -> SessionWindowedKStream k v
  -> IO (SessionWindowedTableHandle k a)
aggregateSessionWindowed initial agg merger m swks = do
  let b = swksBuilder swks
      sw = swksWindows swks
  storeNm <-
    maybe
      (freshStoreName b "SESSION-AGG-STORE")
      pure
      (matName m)
  let supplier =
        materializedS m storeNm (swRetention sw) (swksKey swks) (error "aggregate: pass Materialized with a value serde for the aggregate type")
          :: StoreBuilderS k a
  nodeNm <- freshNodeName b "SESSION-AGG"
  input <- groupedInputOf b (swksParent swks) (swksKey swks) (swksValue swks) (swksRepartition swks)
  withTopology_ b $ \t ->
    let !t1 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = nodeNm
              , Topo.processorSpecParents = [input]
              , Topo.processorSpecSupplier =
                  Topo.AnyProcessor
                    (sessionAggProc @k @v @a storeNm sw initial agg merger)
              , Topo.processorSpecStores = []
              }
            t
        !t2 = Topo.addStateStoreS supplier [nodeNm] t1
    in t2
  pure
    SessionWindowedTableHandle
      { swthNode = nodeNm
      , swthStore = storeNm
      , swthBuilder = b
      }


{- | Fold a record into its session: the sessions of its key within
the inactivity gap merge with it into one. Each merged-away session is
retracted (forwarded as @Nothing@ under its old key) and the new one
forwarded, each at its end. A record whose merged session ends before
the processor's stream time minus the grace period is dropped (the
JVM's @KStreamSessionWindowAggregate@).
-}
sessionAggProc
  :: forall k v a
   . StoreName
  -> SessionWindows
  -> IO a
  -> (k -> v -> a -> a)
  -> (k -> a -> a -> a)
  -> IO (Processor k v)
sessionAggProc sn sw initial agg merger = do
  ref <- newIORef Nothing
  observed <- newIORef (minBound :: Int64)
  pure
    Processor
      { procName = processorName "SESSION-AGG"
      , procInit = \ctx -> do
          st <- getStateStore ctx sn
          case st of
            Just (AnySessionStore ss) -> writeIORef ref (Just (ctx, unsafeCastSS ss :: SessionStore k a))
            _ -> error $ "sessionAggProc: store not found: " <> show sn
      , procClose = pure ()
      , procProcess = \r -> do
          m <- readIORef ref
          case (m, recordKey r) of
            (Just (ctx, store_), Just k) -> do
              let !ts = recordTimestamp r
                  Time.Timestamp tsMs = ts
                  !gap = swInactivityGap sw
              now <- atomicModifyIORef' observed (\o -> let !n = max o tsMs in (n, n))
              let !closeTime = now - swGracePeriod sw
              -- the sessions ending within [ts - gap] and starting by ts + gap
              adjacents <- ssFindSessions store_ k (timestampMinus ts gap) (timestampPlus ts gap) >>= kvIteratorToList
              let !lo = foldl (\s (SessionKey _ s' _, _) -> min s s') ts adjacents
                  !hi@(Time.Timestamp hiMs) = foldl (\e (SessionKey _ _ e', _) -> max e e') ts adjacents
                  forward key v (Time.Timestamp at) = forwardRecord ctx (Record (Just key) v (Time.Timestamp at) (recordHeaders r))
              unless (hiMs < closeTime) $ do
                base <- case adjacents of
                  [] -> initial
                  (_, a0) : rest -> pure $! foldl (\a (_, a') -> merger k a a') a0 rest
                -- (a session that is exactly [ts, ts] is just overwritten)
                unless (lo == ts && hi == ts) $
                  forM_ adjacents $ \(old, _) -> do
                    ssRemove store_ old
                    forward old (Nothing :: Maybe a) (skEnd old)
                let !newAgg = agg k (recordValue r) base
                    !sk = SessionKey k lo hi
                ssPut store_ sk newAgg
                forward sk (Just newAgg) hi
            _ -> pure ()
      }


timestampMinus :: Time.Timestamp -> Int64 -> Time.Timestamp
timestampMinus (Time.Timestamp t) n = Time.Timestamp (t - n)


timestampPlus :: Time.Timestamp -> Int64 -> Time.Timestamp
timestampPlus (Time.Timestamp t) n = Time.Timestamp (t + n)


unsafeCastSS :: SessionStore k v -> SessionStore k' v'
unsafeCastSS = Unsafe.unsafeCoerce
{-# INLINE unsafeCastSS #-}
