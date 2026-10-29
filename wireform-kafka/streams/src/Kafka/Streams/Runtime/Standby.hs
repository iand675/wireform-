{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.Runtime.Standby
Description : Standby tasks: state-store replicas fed by the changelog

A standby task is a "warm replica" of an active task's state
stores. It does no record processing; it only consumes the
changelog topic and applies each entry to a local store. When
the broker rebalances the active task off a node, that node's
standby (if any) is promoted to active and continues from the
offset it has already replayed.

This module provides:

  * 'ChangelogTopic' — an in-memory ordered queue of
    'ChangelogEntry's with publish + read-from-offset.
  * 'loggedKeyValueStore' — a write-through wrapper that pushes
    every put/delete to a 'ChangelogTopic'.
  * 'StandbyTask' — owns a store + a consume offset; @advance@
    replays new entries.

Tests (see 'Streams.StandbySpec') use these directly without a
broker; the broker-backed runtime would back 'ChangelogTopic'
with a real Kafka topic via the existing
'Kafka.Client.Producer' / 'Kafka.Client.Consumer'.
-}
module Kafka.Streams.Runtime.Standby (
  -- * Changelog topic
  ChangelogTopic,
  ChangelogEntry (..),
  newInMemoryChangelogTopic,
  publishEntry,
  readEntriesFrom,
  currentChangelogOffset,

  -- * Logged store wrapper
  loggedKeyValueStore,

  -- * Standby task
  StandbyTask (..),
  newStandbyTask,
  advanceStandby,

  -- * Restore listener
  RestoreListener (..),
  noopRestoreListener,
  setRestoreListener,
) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Kafka.Streams.Serde (Serde, deserialize, serialize)
import Kafka.Streams.State.Store (
  KeyValueStore (..),
  StoreName,
 )


----------------------------------------------------------------------
-- Changelog topic
----------------------------------------------------------------------

data ChangelogEntry = ChangelogEntry
  { clStoreName :: !StoreName
  , clKey :: !(Maybe ByteString)
  , clValue :: !(Maybe ByteString)
  -- ^ 'Nothing' is a tombstone (the active store deleted the key).
  , clOffset :: !Int64
  }
  deriving stock (Eq, Show)


-- | An in-memory log of changelog entries, indexed by offset.
data ChangelogTopic = ChangelogTopic
  { clogEntries :: !(TVar (Map.Map Int64 ChangelogEntry))
  , clogNextOff :: !(TVar Int64)
  }


newInMemoryChangelogTopic :: IO ChangelogTopic
newInMemoryChangelogTopic = do
  es <- newTVarIO Map.empty
  off <- newTVarIO 0
  pure ChangelogTopic {clogEntries = es, clogNextOff = off}


publishEntry
  :: ChangelogTopic
  -> StoreName
  -> Maybe ByteString
  -> Maybe ByteString
  -> IO Int64
publishEntry topic sn k v = atomically $ do
  off <- readTVar (clogNextOff topic)
  let !e = ChangelogEntry sn k v off
  modifyTVar' (clogEntries topic) (Map.insert off e)
  writeTVar (clogNextOff topic) (off + 1)
  pure off


-- | Read every entry with @offset >= from@ in offset order.
readEntriesFrom :: ChangelogTopic -> Int64 -> IO [ChangelogEntry]
readEntriesFrom topic from = atomically $ do
  m <- readTVar (clogEntries topic)
  pure $
    map snd $
      Map.toAscList $
        Map.dropWhileAntitone (< from) m


currentChangelogOffset :: ChangelogTopic -> IO Int64
currentChangelogOffset = readTVarIO . clogNextOff


----------------------------------------------------------------------
-- Logged store wrapper (active side)
----------------------------------------------------------------------

{- | Wrap a 'KeyValueStore' so every put / delete also publishes a
changelog entry. Reads pass through unchanged.
-}
loggedKeyValueStore
  :: forall k v
   . KeyValueStore k v
  -> ChangelogTopic
  -> StoreName
  -> Serde k
  -> Serde v
  -> IO (KeyValueStore k v)
loggedKeyValueStore underlying topic sn ks vs =
  pure
    KeyValueStore
      { kvsBase = kvsBase underlying
      , kvsCache = Nothing
      , kvsGet = kvsGet underlying
      , kvsRange = kvsRange underlying
      , kvsAll = kvsAll underlying
      , kvsApproxEntries = kvsApproxEntries underlying
      , kvsReverseRange = kvsReverseRange underlying
      , kvsReverseAll = kvsReverseAll underlying
      , kvsPut = \k v -> do
          kvsPut underlying k v
          _ <- publishEntry topic sn (Just (serialize ks k)) (Just (serialize vs v))
          pure ()
      , kvsPutIfAbsent = \k v -> do
          r <- kvsPutIfAbsent underlying k v
          case r of
            Nothing -> do
              _ <- publishEntry topic sn (Just (serialize ks k)) (Just (serialize vs v))
              pure Nothing
            Just _ -> pure r
      , kvsDelete = \k -> do
          r <- kvsDelete underlying k
          case r of
            Nothing -> pure Nothing
            Just _ -> do
              _ <- publishEntry topic sn (Just (serialize ks k)) Nothing
              pure r
      }


----------------------------------------------------------------------
-- Standby task
----------------------------------------------------------------------

{- | Callbacks fired during changelog replay. Mirrors Java's
@StateRestoreListener@. Each callback receives the standby task's
store name plus the offset (end-offset for batch / total restored
for end).
-}
data RestoreListener = RestoreListener
  { onRestoreStart :: !(StoreName -> Int64 -> Int64 -> IO ())
  , onBatchRestored :: !(StoreName -> Int64 -> Int -> IO ())
  , onRestoreEnd :: !(StoreName -> Int64 -> IO ())
  }


noopRestoreListener :: RestoreListener
noopRestoreListener =
  RestoreListener
    { onRestoreStart = \_ _ _ -> pure ()
    , onBatchRestored = \_ _ _ -> pure ()
    , onRestoreEnd = \_ _ -> pure ()
    }


-- | A standby replica of one active task's store.
data StandbyTask k v = StandbyTask
  { sbStore :: !(KeyValueStore k v)
  , sbTopic :: !ChangelogTopic
  , sbOffset :: !(IORef Int64)
  , sbStoreNm :: !StoreName
  , sbKeySerde :: !(Serde k)
  , sbValueSerde :: !(Serde v)
  , sbListener :: !(IORef RestoreListener)
  }


-- | Replace the restore listener attached to a standby task.
setRestoreListener :: StandbyTask k v -> RestoreListener -> IO ()
setRestoreListener sb lis = writeIORef (sbListener sb) lis


newStandbyTask
  :: KeyValueStore k v
  -> ChangelogTopic
  -> StoreName
  -> Serde k
  -> Serde v
  -> IO (StandbyTask k v)
newStandbyTask kvs topic sn ks vs = do
  off <- newIORef 0
  lis <- newIORef noopRestoreListener
  pure
    StandbyTask
      { sbStore = kvs
      , sbTopic = topic
      , sbOffset = off
      , sbStoreNm = sn
      , sbKeySerde = ks
      , sbValueSerde = vs
      , sbListener = lis
      }


{- | Replay every changelog entry that's newer than the standby's
last seen offset. Returns the number of entries applied. Fires
the registered 'RestoreListener' on start / batch / end.
-}
advanceStandby :: StandbyTask k v -> IO Int
advanceStandby sb = do
  cur <- readIORef (sbOffset sb)
  entries <- readEntriesFrom (sbTopic sb) cur
  let !mine = filter (\e -> clStoreName e == sbStoreNm sb) entries
  case entries of
    [] -> pure 0
    _ -> do
      lis <- readIORef (sbListener sb)
      let !lastOff = clOffset (last entries)
          !applied = length mine
      onRestoreStart lis (sbStoreNm sb) cur lastOff
      mapM_ apply mine
      onBatchRestored lis (sbStoreNm sb) lastOff applied
      writeIORef (sbOffset sb) (lastOff + 1)
      onRestoreEnd lis (sbStoreNm sb) (fromIntegral applied)
      pure applied
  where
    apply e =
      case clKey e of
        Nothing -> pure () -- malformed; the active path always
        --     publishes a key
        Just kb ->
          case deserialize (sbKeySerde sb) kb of
            Left _ -> pure ()
            Right k -> case clValue e of
              Just vb -> case deserialize (sbValueSerde sb) vb of
                Left _ -> pure ()
                Right v -> kvsPut (sbStore sb) k v
              Nothing -> () <$ kvsDelete (sbStore sb) k
