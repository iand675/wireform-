{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Stores
Description : Java-style 'Stores.*' factory helpers

Mirrors @org.apache.kafka.streams.state.Stores@. Re-exports each
backend's factory under the 'Stores.' namespace so user code can
write

@
import qualified Kafka.Streams.Stores as Stores
...
store <- Stores.inMemoryKeyValueStore "my-counts"
builder <- Stores.inMemoryKeyValueStoreBuilder "my-counts"
@

without having to import each per-backend module individually.
-}
module Kafka.Streams.Stores (
  -- * Key-value
  inMemoryKeyValueStore,
  inMemoryKeyValueStoreBuilder,
  lruMap,
  lruMapBuilder,
  persistentKeyValueStore,
  persistentKeyValueStoreBuilder,

  -- * Window
  inMemoryWindowStore,
  inMemoryWindowStoreBuilder,
  inMemoryTimestampedWindowStore,

  -- * Session
  inMemorySessionStore,
  inMemorySessionStoreBuilder,

  -- * Versioned
  versionedKeyValueStore,

  -- * Timestamped
  timestampedKeyValueStore,

  -- * Common types re-exported
  module Kafka.Streams.State.Store,
  KVPers.PersistentConfig (..),
  KVPers.defaultPersistentConfig,
  KVVer.VersionedConfig (..),
  KVVer.defaultVersionedConfig,
  KVVer.VersionedKeyValueStore,
  KVTs.ValueAndTimestamp (..),
  KVTs.TimestampedKeyValueStore,
) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Kafka.Streams.State.KeyValue.InMemory qualified as KVInMem
import Kafka.Streams.State.KeyValue.Persistent qualified as KVPers
import Kafka.Streams.State.KeyValue.Timestamped qualified as KVTs
import Kafka.Streams.State.KeyValue.Versioned qualified as KVVer
import Kafka.Streams.State.Session.InMemory qualified as SSInMem
import Kafka.Streams.State.Store
import Kafka.Streams.State.Window.InMemory qualified as WSInMem
import Kafka.Streams.State.Window.Timestamped qualified as WSTS


----------------------------------------------------------------------

inMemoryKeyValueStore
  :: Ord k => StoreName -> IO (KeyValueStore k v)
inMemoryKeyValueStore = KVInMem.inMemoryKeyValueStore


inMemoryKeyValueStoreBuilder
  :: Ord k => StoreName -> StoreBuilderKV k v
inMemoryKeyValueStoreBuilder = KVInMem.inMemoryKeyValueStoreBuilder


{- | JVM's @Stores.lruMap(name, maxCacheSize)@: a bounded LRU
in-memory KV store. Logging is disabled by default (LRU
eviction breaks the changelog replay assumption).
-}
lruMap
  :: Ord k => StoreName -> Int -> IO (KeyValueStore k v)
lruMap = KVInMem.inMemoryLruKeyValueStore


lruMapBuilder
  :: Ord k => StoreName -> Int -> StoreBuilderKV k v
lruMapBuilder = KVInMem.inMemoryLruKeyValueStoreBuilder


persistentKeyValueStore
  :: StoreName
  -> KVPers.PersistentConfig
  -> IO (KeyValueStore ByteString ByteString)
persistentKeyValueStore = KVPers.persistentKeyValueStore


persistentKeyValueStoreBuilder
  :: StoreName
  -> KVPers.PersistentConfig
  -> StoreBuilderKV ByteString ByteString
persistentKeyValueStoreBuilder = KVPers.persistentKeyValueStoreBuilder


inMemoryWindowStore
  :: Ord k
  => StoreName -> Int64 -> Int64 -> IO (WindowStore k v)
inMemoryWindowStore = WSInMem.inMemoryWindowStore


inMemoryWindowStoreBuilder
  :: Ord k
  => StoreName -> Int64 -> Int64 -> StoreBuilderW k v
inMemoryWindowStoreBuilder = WSInMem.inMemoryWindowStoreBuilder


{- | JVM's @Stores.timestampedWindowStoreBuilder(...)@ as an
in-memory store. Value type is 'WSTS.ValueAndTimestamp v'.
-}
inMemoryTimestampedWindowStore
  :: Ord k
  => StoreName
  -> Int64
  -> Int64
  -> IO (WSTS.TimestampedWindowStore k v)
inMemoryTimestampedWindowStore =
  WSTS.inMemoryTimestampedWindowStore


inMemorySessionStore
  :: Ord k => StoreName -> Int64 -> IO (SessionStore k v)
inMemorySessionStore = SSInMem.inMemorySessionStore


inMemorySessionStoreBuilder
  :: Ord k => StoreName -> Int64 -> StoreBuilderS k v
inMemorySessionStoreBuilder = SSInMem.inMemorySessionStoreBuilder


versionedKeyValueStore
  :: Ord k
  => StoreName
  -> KVVer.VersionedConfig
  -> IO (KVVer.VersionedKeyValueStore k v)
versionedKeyValueStore = KVVer.inMemoryVersionedKeyValueStore


{- | An in-memory 'TimestampedKeyValueStore'. Mirrors Java's
@Stores.timestampedKeyValueStoreBuilder(...)@.
-}
timestampedKeyValueStore
  :: Ord k
  => StoreName
  -> IO (KVTs.TimestampedKeyValueStore k v)
timestampedKeyValueStore nm =
  KVTs.timestampedFromKV <$> KVInMem.inMemoryKeyValueStore nm
