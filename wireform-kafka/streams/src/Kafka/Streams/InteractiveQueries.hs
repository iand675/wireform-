{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

{- |
Module      : Kafka.Streams.InteractiveQueries
Description : Read-only access to live state stores from outside
              the stream thread

Mirrors @org.apache.kafka.streams.KafkaStreams.store(...)@. From a
running 'KafkaStreams' instance you can obtain a typed read-only
handle to any state store the topology declared. This is what the
Java docs call "interactive queries".

== Concurrency model

The in-memory stores are backed by 'Data.IORef.IORef' over
'Data.Map.Strict'.  All writes go through 'atomicModifyIORef'',
and reads through 'readIORef' — which means a query thread can
always observe a consistent snapshot of the map at the moment it
reads, with the same /linearisability/ guarantees as the rest of
the engine.

The persistent file-backed store is similarly thread-safe for
reads (the in-memory shadow map is updated atomically; a query
thread reads from the same shadow).

/Iterators/ returned by 'queryRange' / 'queryAll' are eager
snapshots (the underlying iterator is materialised at iterator-
creation time), so they don't observe writes that happen after
iterator creation but before iteration completes.

== Tasks

Each task holds its own instance of a store (its partition's keys);
the handles here are composites over every task on this instance,
plus the single copy of a global store. Keys owned by another
instance's tasks are not visible (no remote queries).
-}
module Kafka.Streams.InteractiveQueries (
  -- * Querying a 'KafkaStreams' instance
  queryKVStore,
  queryWindowStore,
  querySessionStore,

  -- * StoreQueryParameters
  StoreQueryParameters (..),
  storeQueryParameters,
  queryKVStoreWithParameters,

  -- * Read-only handles
  ReadOnlyKeyValueStore (..),
  ReadOnlyWindowStore (..),
  ReadOnlySessionStore (..),
  readOnlyKV,
  readOnlyWindow,
  readOnlySession,

  -- * Errors
  StoreNotFound (..),
  StoreTypeMismatch (..),

  -- * Lower-level (pre-running runtime)
  queryEngineStore,
) where

import Control.Exception (Exception)
import Data.Maybe (mapMaybe)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Kafka.Streams.Internal.Engine (
  Engine,
  StoreEntry (..),
  storeByName,
 )
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime (
  KafkaStreams,
  StoreInstance (..),
  StreamsStatus (..),
  localStoreInstances,
  streamsStatus,
 )
import Kafka.Streams.State.Store (
  AnyStateStore (..),
  KeyValueIterator (..),
  KeyValueStore (..),
  SessionKey,
  SessionStore (..),
  StoreName,
  WindowStore (..),
  WindowedKey,
 )
import Kafka.Streams.Time (Timestamp)
import Unsafe.Coerce qualified as Unsafe


----------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------

newtype StoreNotFound = StoreNotFound StoreName
  deriving stock (Show)
  deriving anyclass (Exception)


data StoreTypeMismatch = StoreTypeMismatch !StoreName !String
  deriving stock (Show)
  deriving anyclass (Exception)


----------------------------------------------------------------------
-- Read-only handles
----------------------------------------------------------------------

{- | The read-only side of a 'KeyValueStore'. Same shape as Java's
@ReadOnlyKeyValueStore@.
-}
data ReadOnlyKeyValueStore k v = ReadOnlyKeyValueStore
  { roKvGet :: !(k -> IO (Maybe v))
  , roKvRange :: !(k -> k -> IO (KeyValueIterator k v))
  , roKvAll :: !(IO (KeyValueIterator k v))
  , roKvCount :: !(IO Int64)
  }


readOnlyKV :: KeyValueStore k v -> ReadOnlyKeyValueStore k v
readOnlyKV kvs =
  ReadOnlyKeyValueStore
    { roKvGet = kvsGet kvs
    , roKvRange = kvsRange kvs
    , roKvAll = kvsAll kvs
    , roKvCount = kvsApproxEntries kvs
    }


data ReadOnlyWindowStore k v = ReadOnlyWindowStore
  { roWsFetch :: !(k -> Timestamp -> IO (Maybe v))
  , roWsFetchRange :: !(k -> Timestamp -> Timestamp -> IO (KeyValueIterator Timestamp v))
  , roWsFetchAllRange :: !(Timestamp -> Timestamp -> IO (KeyValueIterator (WindowedKey k) v))
  , roWsAll :: !(IO (KeyValueIterator (WindowedKey k) v))
  }


readOnlyWindow :: WindowStore k v -> ReadOnlyWindowStore k v
readOnlyWindow ws =
  ReadOnlyWindowStore
    { roWsFetch = wsFetch ws
    , roWsFetchRange = wsFetchRange ws
    , roWsFetchAllRange = wsFetchAllRange ws
    , roWsAll = wsAll ws
    }


data ReadOnlySessionStore k v = ReadOnlySessionStore
  { roSsFetchSession :: !(SessionKey k -> IO (Maybe v))
  , roSsFindSessions :: !(k -> Timestamp -> Timestamp -> IO (KeyValueIterator (SessionKey k) v))
  , roSsFindAllSessions :: !(Timestamp -> Timestamp -> IO (KeyValueIterator (SessionKey k) v))
  }


readOnlySession :: SessionStore k v -> ReadOnlySessionStore k v
readOnlySession ss =
  ReadOnlySessionStore
    { roSsFetchSession = ssFetchSession ss
    , roSsFindSessions = ssFindSessions ss
    , roSsFindAllSessions = ssFindAllSessions ss
    }


----------------------------------------------------------------------
-- Querying a running KafkaStreams
----------------------------------------------------------------------

{- | A @StoreQueryParameters@ — the parameter bag the JVM
@KafkaStreams.store(...)@ entry-point takes.
-}
data StoreQueryParameters = StoreQueryParameters
  { storeName :: !StoreName
  , staleStoresEnabled :: !Bool
  {- ^ When 'True', answer even while the instance is rebalancing or
  restoring (possibly stale). Mirrors @enableStaleStores()@.
  -}
  , partition :: !(Maybe Int)
  -- ^ Only the instance of the task for this partition. Mirrors @withPartition(int)@.
  }
  deriving stock (Eq, Show)


storeQueryParameters :: StoreName -> StoreQueryParameters
storeQueryParameters n =
  StoreQueryParameters
    { storeName = n
    , staleStoresEnabled = False
    , partition = Nothing
    }


{- | Every local instance of the store: one per task (a key lives in
exactly one, its partition's), or the single global copy.
-}
instancesOf :: KafkaStreams -> StoreName -> Maybe Int -> IO [AnyStateStore]
instancesOf ks sn mPart = do
  insts <- localStoreInstances ks sn
  let keep i = case (mPart, i.siTask) of
        (Nothing, _) -> True
        (Just _, Nothing) -> True
        (Just p, Just (TaskId _ tp)) -> fromIntegral tp == p
  pure (map (\i -> storeEntryAny i.siEntry) (filter keep insts))


{- | Resolve a key-value store with 'StoreQueryParameters'. Unless
'staleStoresEnabled', the instance must be 'StreamsRunning'.
-}
queryKVStoreWithParameters
  :: forall k v
   . KafkaStreams
  -> StoreQueryParameters
  -> IO (Maybe (ReadOnlyKeyValueStore k v))
queryKVStoreWithParameters ks p = do
  st <- streamsStatus ks
  if st /= StreamsRunning && not p.staleStoresEnabled
    then pure Nothing
    else compositeKV <$> instancesOf ks p.storeName p.partition


{- | A key-value store across every local task (the JVM's
@CompositeReadOnlyKeyValueStore@): 'roKvGet' asks each task until one
has the key, the iterators chain the tasks. 'Nothing' if no local
task has the store.
-}
queryKVStore
  :: forall k v
   . KafkaStreams
  -> StoreName
  -> IO (Maybe (ReadOnlyKeyValueStore k v))
queryKVStore ks sn = compositeKV <$> instancesOf ks sn Nothing


compositeKV :: forall k v. [AnyStateStore] -> Maybe (ReadOnlyKeyValueStore k v)
compositeKV insts =
  case mapMaybe (\case AnyKeyValueStore kvs -> Just (readOnlyKV (Unsafe.unsafeCoerce kvs)); _ -> Nothing) insts of
    [] -> Nothing
    [one] -> Just one
    stores ->
      Just
        ReadOnlyKeyValueStore
          { roKvGet = \k -> firstHit (map (\s -> s.roKvGet k) stores)
          , roKvRange = \lo hi -> traverse (\s -> s.roKvRange lo hi) stores >>= chainIterators
          , roKvAll = traverse (\s -> s.roKvAll) stores >>= chainIterators
          , roKvCount = sum <$> traverse (\s -> s.roKvCount) stores
          }


-- | First IO action that yields 'Just'.
firstHit :: [IO (Maybe a)] -> IO (Maybe a)
firstHit [] = pure Nothing
firstHit (a : as) = do
  r <- a
  case r of
    Just _ -> pure r
    Nothing -> firstHit as


{- | Sequence a list of iterators into one. Each iterator is
consumed in turn; we close it when it's drained.
-}
chainIterators :: [KeyValueIterator k v] -> IO (KeyValueIterator k v)
chainIterators its0 = do
  ref <- newIORef its0
  pure
    KeyValueIterator
      { kvIterNext = pump ref
      , kvIterClose = do
          rs <- readIORef ref
          mapM_ kvIterClose rs
          writeIORef ref []
      }
  where
    pump ref = do
      rs <- readIORef ref
      case rs of
        [] -> pure Nothing
        (it : rest) -> do
          mNext <- kvIterNext it
          case mNext of
            Just _ -> pure mNext
            Nothing -> do
              kvIterClose it
              writeIORef ref rest
              pump ref


-- | A window store across every local task.
queryWindowStore
  :: forall k v
   . KafkaStreams
  -> StoreName
  -> IO (Maybe (ReadOnlyWindowStore k v))
queryWindowStore ks sn = do
  insts <- instancesOf ks sn Nothing
  pure $ case mapMaybe (\case AnyWindowStore ws -> Just (readOnlyWindow (Unsafe.unsafeCoerce ws)); _ -> Nothing) insts of
    [] -> Nothing
    [one] -> Just one
    stores ->
      Just
        ReadOnlyWindowStore
          { roWsFetch = \k t -> firstHit (map (\s -> s.roWsFetch k t) stores)
          , roWsFetchRange = \k lo hi -> traverse (\s -> s.roWsFetchRange k lo hi) stores >>= chainIterators
          , roWsFetchAllRange = \lo hi -> traverse (\s -> s.roWsFetchAllRange lo hi) stores >>= chainIterators
          , roWsAll = traverse (\s -> s.roWsAll) stores >>= chainIterators
          }


-- | A session store across every local task.
querySessionStore
  :: forall k v
   . KafkaStreams
  -> StoreName
  -> IO (Maybe (ReadOnlySessionStore k v))
querySessionStore ks sn = do
  insts <- instancesOf ks sn Nothing
  pure $ case mapMaybe (\case AnySessionStore ss -> Just (readOnlySession (Unsafe.unsafeCoerce ss)); _ -> Nothing) insts of
    [] -> Nothing
    [one] -> Just one
    stores ->
      Just
        ReadOnlySessionStore
          { roSsFetchSession = \sk -> firstHit (map (\s -> s.roSsFetchSession sk) stores)
          , roSsFindSessions = \k lo hi -> traverse (\s -> s.roSsFindSessions k lo hi) stores >>= chainIterators
          , roSsFindAllSessions = \lo hi -> traverse (\s -> s.roSsFindAllSessions lo hi) stores >>= chainIterators
          }


{- | Lower-level: query an 'Engine' directly. Useful from inside
'TopologyTestDriver' tests where there isn't a 'KafkaStreams' but
there is an engine.
-}
queryEngineStore
  :: forall k v
   . Engine
  -> StoreName
  -> IO (Maybe (ReadOnlyKeyValueStore k v))
queryEngineStore eng sn = do
  m <- storeByName eng sn
  case storeEntryAny <$> m of
    Just (AnyKeyValueStore kvs) ->
      pure (Just (readOnlyKV (Unsafe.unsafeCoerce kvs)))
    _ -> pure Nothing
