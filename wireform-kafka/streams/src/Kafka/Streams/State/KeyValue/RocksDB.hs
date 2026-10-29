{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.State.KeyValue.RocksDB
Description : RocksDB-backed 'KeyValueStore' (gated on the @+rocksdb@ Cabal flag)

Wraps a RocksDB column-family with the streams 'KeyValueStore'
interface. Each store gets its own RocksDB directory at
@\<state.dir\>\/\<storeName\>\/@; opening the store also serves
as restart recovery (RocksDB's WAL replay handles unclean
shutdowns).

== Cabal flag

This module is only built when the @+rocksdb@ Cabal flag is on.
The flag is /default False/ because the @rocksdb-haskell-kadena@
binding requires the @librocksdb@ system library at link time and
we don't want the default build to depend on it.

@
cabal build wireform-kafka:wireform-kafka-streams -frocksdb
@

The interface is the same shape as
"Kafka.Streams.State.KeyValue.Persistent" so user code can switch
backends with a one-line change.
-}
module Kafka.Streams.State.KeyValue.RocksDB (
  RocksDBConfig (..),
  defaultRocksDBConfig,
  rocksDBKeyValueStore,
  rocksDBKeyValueStoreBuilder,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Database.RocksDB qualified as R
import Database.RocksDB.Iterator qualified as RI
import Kafka.Streams.Serde (byteStringSerde)
import Kafka.Streams.State.Store (
  KeyValueIterator (..),
  KeyValueStore (..),
  StateStore (..),
  StoreBuilderKV (..),
  StoreSerdes (..),
  StoreName,
  defaultLoggingConfig,
  unStoreName,
 )
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))


----------------------------------------------------------------------
-- Config
----------------------------------------------------------------------

data RocksDBConfig = RocksDBConfig
  { rdbDirectory :: !FilePath
  , rdbCreateIfMissing :: !Bool
  , rdbWriteSync :: !Bool
  -- ^ When True, every put / delete fsyncs the WAL.
  }
  deriving stock (Show)


defaultRocksDBConfig :: FilePath -> RocksDBConfig
defaultRocksDBConfig dir =
  RocksDBConfig
    { rdbDirectory = dir
    , rdbCreateIfMissing = True
    , rdbWriteSync = False
    }


----------------------------------------------------------------------
-- Construction
----------------------------------------------------------------------

{- | Open (or create) a RocksDB-backed key-value store.

The keys / values are arbitrary 'ByteString'; users layer their
own 'Serde' on top, just like the file-WAL-backed
'Kafka.Streams.State.KeyValue.Persistent.persistentKeyValueStore'.
-}
rocksDBKeyValueStore
  :: StoreName
  -> RocksDBConfig
  -> IO (KeyValueStore ByteString ByteString)
rocksDBKeyValueStore nm cfg = do
  let !path = rdbDirectory cfg </> T.unpack (unStoreName nm)
  createDirectoryIfMissing True path
  let opts =
        R.defaultOptions
          { R.createIfMissing = rdbCreateIfMissing cfg
          }
      writeOpts =
        R.defaultWriteOptions
          { R.sync = rdbWriteSync cfg
          }
  db <- R.open path opts
  pure
    KeyValueStore
      { kvsBase =
          StateStore
            { storeStoreName = nm
            , storePersistent = True
            , storeFlush = pure () -- RocksDB's WAL is its flush
            , storeClose = R.close db
            }
      , kvsGet = \k -> do
          mv <- R.get db R.defaultReadOptions k
          pure mv
      , kvsPut = \k v ->
          R.put db writeOpts k v
      , kvsPutIfAbsent = \k v -> do
          mv <- R.get db R.defaultReadOptions k
          case mv of
            Just _ -> pure mv
            Nothing -> do
              R.put db writeOpts k v
              pure Nothing
      , kvsDelete = \k -> do
          mv <- R.get db R.defaultReadOptions k
          case mv of
            Nothing -> pure Nothing
            Just _ -> do
              R.delete db writeOpts k
              pure mv
      , kvsRange = \lo hi ->
          rocksRangeIter db lo hi
      , kvsAll = rocksAllIter db
      , kvsApproxEntries = pure 0 -- RocksDB doesn't expose a cheap count
      , kvsReverseRange = \lo hi ->
          rocksReverseRangeIter db lo hi
      , kvsReverseAll = rocksReverseAllIter db
      }


rocksDBKeyValueStoreBuilder
  :: StoreName
  -> RocksDBConfig
  -> StoreBuilderKV ByteString ByteString
rocksDBKeyValueStoreBuilder nm cfg =
  StoreBuilderKV
    { sbKvName = nm
    , sbKvLogging = defaultLoggingConfig
    , sbKvSerdes = Just (StoreSerdes byteStringSerde byteStringSerde)
    , sbKvBuild = rocksDBKeyValueStore nm cfg
    }


----------------------------------------------------------------------
-- Iterators
----------------------------------------------------------------------

rocksRangeIter
  :: R.DB
  -> ByteString
  -> ByteString
  -> IO (KeyValueIterator ByteString ByteString)
rocksRangeIter db lo hi = do
  it <- RI.createIter db R.defaultReadOptions
  RI.iterSeek it lo
  pure
    KeyValueIterator
      { kvIterNext = do
          valid <- RI.iterValid it
          if not valid
            then pure Nothing
            else do
              mk <- RI.iterKey it
              mv <- RI.iterValue it
              case (mk, mv) of
                (Just kb, Just vb) | kb <= hi -> do
                  RI.iterNext it
                  pure (Just (kb, vb))
                _ -> pure Nothing
      , kvIterClose = RI.releaseIter it
      }


rocksAllIter
  :: R.DB
  -> IO (KeyValueIterator ByteString ByteString)
rocksAllIter db = do
  it <- RI.createIter db R.defaultReadOptions
  RI.iterFirst it
  pure
    KeyValueIterator
      { kvIterNext = do
          valid <- RI.iterValid it
          if not valid
            then pure Nothing
            else do
              mk <- RI.iterKey it
              mv <- RI.iterValue it
              case (mk, mv) of
                (Just kb, Just vb) -> do
                  RI.iterNext it
                  pure (Just (kb, vb))
                _ -> pure Nothing
      , kvIterClose = RI.releaseIter it
      }


rocksReverseRangeIter
  :: R.DB
  -> ByteString
  -> ByteString
  -> IO (KeyValueIterator ByteString ByteString)
rocksReverseRangeIter db lo hi = do
  it <- RI.createIter db R.defaultReadOptions
  -- Seek to the upper bound, then walk backwards.
  RI.iterSeek it hi
  pure
    KeyValueIterator
      { kvIterNext = do
          valid <- RI.iterValid it
          if not valid
            then pure Nothing
            else do
              mk <- RI.iterKey it
              mv <- RI.iterValue it
              case (mk, mv) of
                (Just kb, Just vb) | kb >= lo -> do
                  RI.iterPrev it
                  pure (Just (kb, vb))
                _ -> pure Nothing
      , kvIterClose = RI.releaseIter it
      }


rocksReverseAllIter
  :: R.DB
  -> IO (KeyValueIterator ByteString ByteString)
rocksReverseAllIter db = do
  it <- RI.createIter db R.defaultReadOptions
  RI.iterLast it
  pure
    KeyValueIterator
      { kvIterNext = do
          valid <- RI.iterValid it
          if not valid
            then pure Nothing
            else do
              mk <- RI.iterKey it
              mv <- RI.iterValue it
              case (mk, mv) of
                (Just kb, Just vb) -> do
                  RI.iterPrev it
                  pure (Just (kb, vb))
                _ -> pure Nothing
      , kvIterClose = RI.releaseIter it
      }
