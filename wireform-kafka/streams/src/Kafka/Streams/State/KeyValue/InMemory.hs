{-# LANGUAGE BangPatterns #-}

{- |
Module      : Kafka.Streams.State.KeyValue.InMemory
Description : In-memory @KeyValueStore@

Single-task store. Tasks are inherently single-threaded inside
Streams, so the store is 'IORef'-backed (no STM / MVar); this
matches the Java in-memory store's lock-free design.

Use 'inMemoryKeyValueStoreBuilder' to declare a store inside a
topology — the runtime calls 'sbKvBuild' once per assigned task.
-}
module Kafka.Streams.State.KeyValue.InMemory (
  inMemoryKeyValueStore,
  inMemoryKeyValueStoreBuilder,

  -- * LRU (Java @Stores.lruMap@)
  inMemoryLruKeyValueStore,
  inMemoryLruKeyValueStoreBuilder,
) where

import Data.IORef (
  IORef,
  atomicModifyIORef',
  newIORef,
  readIORef,
  writeIORef,
 )
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Kafka.Streams.State.Store (
  KeyValueStore (..),
  LoggingConfig (..),
  StateStore (..),
  StoreBuilderKV (..),
  StoreName,
  defaultLoggingConfig,
  kvIteratorFromList,
 )


-- | Build a fresh in-memory key-value store with the given name.
inMemoryKeyValueStore
  :: forall k v
   . Ord k
  => StoreName
  -> IO (KeyValueStore k v)
inMemoryKeyValueStore nm = do
  ref <- newIORef (Map.empty :: Map k v)
  pure (mkStore nm ref)


mkStore
  :: forall k v
   . Ord k
  => StoreName
  -> IORef (Map k v)
  -> KeyValueStore k v
mkStore nm ref =
  KeyValueStore
    { kvsBase =
        StateStore
          { storeStoreName = nm
          , storePersistent = False
          , storeFlush = pure ()
          , storeClose = writeIORef ref Map.empty
          }
    , kvsGet = \k -> Map.lookup k <$> readIORef ref
    , kvsPut = \k v -> atomicModifyIORef' ref $ \m ->
        let !m' = Map.insert k v m in (m', ())
    , kvsPutIfAbsent = \k v ->
        atomicModifyIORef' ref $ \m ->
          case Map.lookup k m of
            Just existing -> (m, Just existing)
            Nothing ->
              let !m' = Map.insert k v m in (m', Nothing)
    , kvsDelete = \k ->
        atomicModifyIORef' ref $ \m ->
          case Map.lookup k m of
            Nothing -> (m, Nothing)
            Just v ->
              let !m' = Map.delete k m in (m', Just v)
    , kvsRange = \lo hi -> do
        m <- readIORef ref
        let inRange =
              Map.takeWhileAntitone (<= hi) $
                Map.dropWhileAntitone (< lo) m
        kvIteratorFromList (Map.toAscList inRange)
    , kvsAll = do
        m <- readIORef ref
        kvIteratorFromList (Map.toAscList m)
    , kvsApproxEntries = countMap ref
    , kvsReverseRange = \lo hi -> do
        m <- readIORef ref
        let inRange =
              Map.takeWhileAntitone (<= hi) $
                Map.dropWhileAntitone (< lo) m
        kvIteratorFromList (Map.toDescList inRange)
    , kvsReverseAll = do
        m <- readIORef ref
        kvIteratorFromList (Map.toDescList m)
    }


countMap :: IORef (Map k v) -> IO Int64
countMap ref = (fromIntegral . Map.size) <$> readIORef ref


{- | Bounded LRU in-memory store. Mirrors Java's
@Stores.lruMap(name, maxCacheSize)@. Eviction is
strict-LRU: on every read or write the touched key moves to
the head; when 'maxEntries' is exceeded the tail is dropped.

Useful for caches that must not grow without bound (a
typical fronting cache for an upstream service, an active-key
bloom-filter substitute, etc.). State is /not/ replayed from
a changelog because eviction makes the order-preserving
guarantees Kafka assumes for compacted topics unsafe; LRU
stores default to logging-disabled.
-}
inMemoryLruKeyValueStore
  :: forall k v
   . Ord k
  => StoreName
  -> Int
  -- ^ max entries
  -> IO (KeyValueStore k v)
inMemoryLruKeyValueStore nm maxEntries = do
  ref <- newIORef (Map.empty :: Map k v)
  -- 'orderRef' is a list of keys, head=most-recent,
  -- tail=oldest. Lookups touch the order, writes prepend.
  orderRef <- newIORef ([] :: [k])
  let touch k = do
        atomicModifyIORef' orderRef $ \os ->
          (k : filter (/= k) os, ())
      evictIfFull = do
        os <- readIORef orderRef
        m <- readIORef ref
        if Map.size m > maxEntries
          then do
            let !os' = take maxEntries os
                !keep = Set.fromList os'
                !m' = Map.filterWithKey (\k _ -> Set.member k keep) m
            writeIORef orderRef os'
            writeIORef ref m'
          else pure ()
      kvs =
        KeyValueStore
          { kvsBase =
              StateStore
                { storeStoreName = nm
                , storePersistent = False
                , storeFlush = pure ()
                , storeClose = do
                    writeIORef ref Map.empty
                    writeIORef orderRef []
                }
          , kvsGet = \k -> do
              v <- Map.lookup k <$> readIORef ref
              case v of
                Just _ -> touch k
                Nothing -> pure ()
              pure v
          , kvsPut = \k v -> do
              atomicModifyIORef' ref (\m -> (Map.insert k v m, ()))
              touch k
              evictIfFull
          , kvsPutIfAbsent = \k v -> do
              r <- atomicModifyIORef' ref $ \m ->
                case Map.lookup k m of
                  Just existing -> (m, Just existing)
                  Nothing -> (Map.insert k v m, Nothing)
              touch k
              evictIfFull
              pure r
          , kvsDelete = \k -> do
              old <- atomicModifyIORef' ref $ \m ->
                case Map.lookup k m of
                  Nothing -> (m, Nothing)
                  Just v -> (Map.delete k m, Just v)
              atomicModifyIORef'
                orderRef
                (\os -> (filter (/= k) os, ()))
              pure old
          , kvsRange = \lo hi -> do
              m <- readIORef ref
              let inRange =
                    Map.takeWhileAntitone (<= hi) $
                      Map.dropWhileAntitone (< lo) m
              kvIteratorFromList (Map.toAscList inRange)
          , kvsAll = do
              m <- readIORef ref
              kvIteratorFromList (Map.toAscList m)
          , kvsApproxEntries = countMap ref
          , kvsReverseRange = \lo hi -> do
              m <- readIORef ref
              let inRange =
                    Map.takeWhileAntitone (<= hi) $
                      Map.dropWhileAntitone (< lo) m
              kvIteratorFromList (Map.toDescList inRange)
          , kvsReverseAll = do
              m <- readIORef ref
              kvIteratorFromList (Map.toDescList m)
          }
  pure kvs


{- | 'StoreBuilderKV' for an in-memory LRU store. Logging is
/disabled/ by default — see 'inMemoryLruKeyValueStore'.
-}
inMemoryLruKeyValueStoreBuilder
  :: Ord k
  => StoreName
  -> Int
  -> StoreBuilderKV k v
inMemoryLruKeyValueStoreBuilder nm maxEntries =
  StoreBuilderKV
    { sbKvName = nm
    , sbKvLogging = LoggingConfig False [] Nothing
    , sbKvSerdes = Nothing
    , sbKvBuild = \_ -> inMemoryLruKeyValueStore nm maxEntries
    }


{- | 'StoreBuilderKV' for an in-memory store.  Logging defaults to
'defaultLoggingConfig' (changelog enabled, compacted topic) so
in-memory stores still survive failover.
-}
inMemoryKeyValueStoreBuilder
  :: Ord k
  => StoreName
  -> StoreBuilderKV k v
inMemoryKeyValueStoreBuilder nm =
  StoreBuilderKV
    { sbKvName = nm
    , sbKvLogging = defaultLoggingConfig
    , sbKvSerdes = Nothing
    , sbKvBuild = \_ -> inMemoryKeyValueStore nm
    }
