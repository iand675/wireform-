{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.State.Window.InMemory
Description : In-memory window store

Implementation:

  * Storage is @Map (windowStart, key) value@. Window-start is the
    primary axis so retention sweeps are an @O(log n)@ split.
  * 'wsFetchRange' / 'wsFetch' do an @O(log n)@ range followed by
    filtering on the key axis.
  * Retention is enforced lazily on every put: any entries with
    @windowStart < observedTime - retention@ are dropped.

For high cardinality this is materially slower than a RocksDB-backed
store; it is, however, perfectly correct and is the default for
tests / TopologyTestDriver.
-}
module Kafka.Streams.State.Window.InMemory (
  inMemoryWindowStore,
  inMemoryWindowStoreBuilder,
) where

import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Kafka.Streams.State.Store (
  StateStore (..),
  StoreBuilderW (..),
  StoreName,
  WindowStore (..),
  WindowedKey (..),
  defaultLoggingConfig,
  kvIteratorFromList,
 )
import Kafka.Streams.Time (Timestamp (..))


{- | Build a fresh in-memory window store. @retention@ is the maximum
age of any entry the store will keep; older entries are dropped on
the next put.

@
inMemoryWindowStore "my-store" sizeMs retentionMs
@
-}
inMemoryWindowStore
  :: forall k v
   . Ord k
  => StoreName
  -> Int64
  -> Int64
  -> IO (WindowStore k v)
inMemoryWindowStore nm sizeMs retentionMs = do
  ref <- newIORef (Map.empty :: Map (Timestamp, k) v)
  obs <- newIORef (Timestamp minBound) -- observed max timestamp
  pure (mkStore nm sizeMs retentionMs ref obs)


mkStore
  :: forall k v
   . Ord k
  => StoreName
  -> Int64
  -> Int64
  -> IORef (Map (Timestamp, k) v)
  -> IORef Timestamp
  -> WindowStore k v
mkStore nm sizeMs retentionMs ref obs =
  WindowStore
    { wsBase =
        StateStore
          { storeStoreName = nm
          , storePersistent = False
          , storeFlush = pure ()
          , storeClose = writeIORef ref Map.empty
          }
    , wsWindowSize = sizeMs
    , wsRetention = retentionMs
    , wsPut = \k v t -> do
        atomicModifyIORef' obs $ \cur ->
          let !newObs = max cur t in (newObs, ())
        atomicModifyIORef' ref $ \m ->
          let !m' = Map.insert (t, k) v m
              !m'' = expire retentionMs t m'
          in (m'', ())
    , wsFetch = \k t -> do
        m <- readIORef ref
        pure (Map.lookup (t, k) m)
    , wsFetchRange = \k from to -> do
        m <- readIORef ref
        let lo = (from, k)
            hi = (to, k)
            slice =
              Map.takeWhileAntitone (<= hi) $
                Map.dropWhileAntitone (< lo) m
            flat = filter (\((_, k'), _) -> k' == k) (Map.toAscList slice)
        kvIteratorFromList
          [(ts, v) | ((ts, _), v) <- flat]
    , wsFetchAllRange = \from to -> do
        m <- readIORef ref
        let slice =
              Map.takeWhileAntitone (\(t, _) -> t <= to) $
                Map.dropWhileAntitone (\(t, _) -> t < from) m
        kvIteratorFromList
          [ (WindowedKey k ts, v)
          | ((ts, k), v) <- Map.toAscList slice
          ]
    , wsAll = do
        m <- readIORef ref
        kvIteratorFromList
          [ (WindowedKey k ts, v)
          | ((ts, k), v) <- Map.toAscList m
          ]
    }


expire
  :: Int64
  -> Timestamp
  -> Map (Timestamp, k) v
  -> Map (Timestamp, k) v
expire retentionMs (Timestamp now) m =
  let !cutoff = Timestamp (now - retentionMs)
  in Map.dropWhileAntitone (\(t, _) -> t < cutoff) m


inMemoryWindowStoreBuilder
  :: Ord k
  => StoreName
  -> Int64
  -> Int64
  -> StoreBuilderW k v
inMemoryWindowStoreBuilder nm sz ret =
  StoreBuilderW
    { sbWName = nm
    , sbWLogging = defaultLoggingConfig
    , sbWSerdes = Nothing
    , sbWBuild = inMemoryWindowStore nm sz ret
    }
