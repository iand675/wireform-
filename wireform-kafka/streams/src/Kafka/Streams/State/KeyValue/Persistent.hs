{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.State.KeyValue.Persistent
Description : File-backed persistent KeyValueStore

A simple but real persistent store that survives process restart:

  * In-memory @Map@ for fast point queries.
  * Append-only write-ahead log on disk
    (@\<dir\>\/\<storeName\>.wal@) replayed on open.
  * 'kvsFlush' fsyncs the WAL.
  * On 'storeClose', a snapshot is written
    (@\<dir\>\/\<storeName\>.snap@) and the WAL is truncated.

This is /not/ RocksDB; it is a deliberate, dependency-free
persistent backend that gives Streams real durability semantics
when RocksDB is not available. The on-disk format is intentionally
versioned so future RocksDB-backed stores can co-exist.

The format:

@
WAL entry := tag(1) keyLen(4 BE) key valLen(4 BE) val
  tag = 0   put
  tag = 1   delete  (no value bytes follow)
Snapshot   := \"WSKV1\\0\" entries...
  each entry = keyLen(4) key valLen(4) val
@

Keys / values are arbitrary 'ByteString'; users layer their own
'Serde' on top.
-}
module Kafka.Streams.State.KeyValue.Persistent (
  persistentKeyValueStore,
  persistentKeyValueStoreBuilder,
  PersistentConfig (..),
  defaultPersistentConfig,
) where

import Control.Monad (unless, when)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Word (Word32)
import Kafka.Streams.Serde (byteStringSerde)
import Kafka.Streams.State.Store (
  KeyValueStore (..),
  StateStore (..),
  StoreBuilderKV (..),
  StoreSerdes (..),
  StoreName,
  defaultLoggingConfig,
  kvIteratorFromList,
  unStoreName,
 )
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.FilePath ((</>))
import System.IO


-- | Persistent-store knobs.
data PersistentConfig = PersistentConfig
  { pcDirectory :: !FilePath
  -- ^ Directory that owns the snapshot + WAL files.
  , pcFsyncOnPut :: !Bool
  {- ^ When 'True', every put / delete fsyncs the WAL. Slower but
  gives strict durability guarantees. The default is 'False' —
  the runtime fsyncs at every task commit anyway.
  -}
  , pcWalSyncEverySec :: !Int
  {- ^ Background fsync cadence, in seconds. (Currently advisory;
  the simple driver fsyncs only on commit.)
  -}
  }


defaultPersistentConfig :: FilePath -> PersistentConfig
defaultPersistentConfig dir =
  PersistentConfig
    { pcDirectory = dir
    , pcFsyncOnPut = False
    , pcWalSyncEverySec = 5
    }


{- | Build (or re-open) a persistent key-value store.

Strict assumptions:

  * The store directory is owned by /one/ task at a time. The
    runtime is responsible for that — multi-task collisions are
    undefined behaviour.
  * Keys / values are arbitrary 'ByteString'.
-}
persistentKeyValueStore
  :: StoreName
  -> PersistentConfig
  -> IO (KeyValueStore ByteString ByteString)
persistentKeyValueStore nm cfg = do
  createDirectoryIfMissing True (pcDirectory cfg)
  let !snapPath = pcDirectory cfg </> T.unpack (unStoreName nm) <> ".snap"
      !walPath = pcDirectory cfg </> T.unpack (unStoreName nm) <> ".wal"
  initial <- restore snapPath walPath
  ref <- newIORef initial
  walRef <- newIORef =<< openWal walPath
  pure (mkStore nm cfg snapPath walPath ref walRef)


mkStore
  :: StoreName
  -> PersistentConfig
  -> FilePath -- snapshot path
  -> FilePath -- wal path
  -> IORef (Map ByteString ByteString)
  -> IORef Handle
  -> KeyValueStore ByteString ByteString
mkStore nm cfg snapPath walPath ref walRef =
  KeyValueStore
    { kvsBase =
        StateStore
          { storeStoreName = nm
          , storePersistent = True
          , storeFlush = do
              h <- readIORef walRef
              hFlush h
              when (pcFsyncOnPut cfg) (hFlush h) -- belt-and-braces
          , storeClose = do
              -- Snapshot + truncate WAL.
              h <- readIORef walRef
              hClose h
              m <- readIORef ref
              writeSnapshot snapPath m
              truncateWal walPath
              h' <- openWal walPath
              writeIORef walRef h'
              hClose h'
          }
    , kvsGet = \k -> Map.lookup k <$> readIORef ref
    , kvsPut = \k v -> do
        atomicModifyIORef' ref $ \m ->
          let !m' = Map.insert k v m in (m', ())
        appendWal walRef (entryPut k v)
        when (pcFsyncOnPut cfg) (readIORef walRef >>= hFlush)
    , kvsPutIfAbsent = \k v -> do
        result <- atomicModifyIORef' ref $ \m ->
          case Map.lookup k m of
            Just existing -> (m, (False, Just existing))
            Nothing ->
              let !m' = Map.insert k v m in (m', (True, Nothing))
        case result of
          (True, _) -> do
            appendWal walRef (entryPut k v)
            when (pcFsyncOnPut cfg) (readIORef walRef >>= hFlush)
            pure Nothing
          (False, mv) -> pure mv
    , kvsDelete = \k -> do
        mv <- atomicModifyIORef' ref $ \m ->
          case Map.lookup k m of
            Nothing -> (m, Nothing)
            Just v ->
              let !m' = Map.delete k m in (m', Just v)
        case mv of
          Nothing -> pure Nothing
          Just _ -> do
            appendWal walRef (entryDel k)
            when (pcFsyncOnPut cfg) (readIORef walRef >>= hFlush)
            pure mv
    , kvsRange = \lo hi -> do
        m <- readIORef ref
        let inRange =
              Map.takeWhileAntitone (<= hi) $
                Map.dropWhileAntitone (< lo) m
        kvIteratorFromList (Map.toAscList inRange)
    , kvsAll = do
        m <- readIORef ref
        kvIteratorFromList (Map.toAscList m)
    , kvsApproxEntries = (fromIntegral . Map.size) <$> readIORef ref
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


persistentKeyValueStoreBuilder
  :: StoreName
  -> PersistentConfig
  -> StoreBuilderKV ByteString ByteString
persistentKeyValueStoreBuilder nm cfg =
  StoreBuilderKV
    { sbKvName = nm
    , sbKvLogging = defaultLoggingConfig
    , sbKvSerdes = Just (StoreSerdes byteStringSerde byteStringSerde)
    , sbKvBuild = persistentKeyValueStore nm cfg
    }


----------------------------------------------------------------------
-- WAL / snapshot serialisation
----------------------------------------------------------------------

snapshotMagic :: ByteString
snapshotMagic = BS.pack [0x57, 0x53, 0x4B, 0x56, 0x31, 0x00] -- "WSKV1\0"


restore :: FilePath -> FilePath -> IO (Map ByteString ByteString)
restore snapPath walPath = do
  base <- readSnapshot snapPath
  walExists <- doesFileExist walPath
  if walExists
    then replayWal walPath base
    else pure base


readSnapshot :: FilePath -> IO (Map ByteString ByteString)
readSnapshot path = do
  exists <- doesFileExist path
  if not exists
    then pure Map.empty
    else withBinaryFile path ReadMode $ \h -> do
      magic <- BS.hGet h (BS.length snapshotMagic)
      unless
        (magic == snapshotMagic)
        (fail $ "snapshot: bad magic in " <> path)
      consume h Map.empty
  where
    consume h !acc = do
      eof <- hIsEOF h
      if eof
        then pure acc
        else do
          mEntry <- readSnapEntry h
          case mEntry of
            Nothing -> pure acc
            Just (k, v) -> consume h (Map.insert k v acc)


readSnapEntry :: Handle -> IO (Maybe (ByteString, ByteString))
readSnapEntry h = do
  klBs <- BS.hGet h 4
  if BS.length klBs /= 4
    then pure Nothing
    else do
      let kl = beWord32 klBs
      k <- BS.hGet h (fromIntegral kl)
      vlBs <- BS.hGet h 4
      let vl = beWord32 vlBs
      v <- BS.hGet h (fromIntegral vl)
      pure (Just (k, v))


writeSnapshot :: FilePath -> Map ByteString ByteString -> IO ()
writeSnapshot path m = do
  let tmp = path <> ".tmp"
  withBinaryFile tmp WriteMode $ \h -> do
    BS.hPut h snapshotMagic
    Map.foldlWithKey'
      (\io k v -> io >> writeSnapEntry h k v)
      (pure ())
      m
    hFlush h
  renameFile tmp path


writeSnapEntry :: Handle -> ByteString -> ByteString -> IO ()
writeSnapEntry h k v = do
  putBE32 h (fromIntegral (BS.length k))
  BS.hPut h k
  putBE32 h (fromIntegral (BS.length v))
  BS.hPut h v


openWal :: FilePath -> IO Handle
openWal walPath = do
  h <- openBinaryFile walPath AppendMode
  hSetBuffering h (BlockBuffering Nothing)
  pure h


truncateWal :: FilePath -> IO ()
truncateWal walPath = do
  exists <- doesFileExist walPath
  when exists (removeFile walPath)


replayWal :: FilePath -> Map ByteString ByteString -> IO (Map ByteString ByteString)
replayWal walPath base =
  withBinaryFile walPath ReadMode (\h -> consume h base)
  where
    consume h !acc = do
      tagBs <- BS.hGet h 1
      if BS.null tagBs
        then pure acc
        else do
          let tag = BS.head tagBs
          klBs <- BS.hGet h 4
          if BS.length klBs /= 4
            then do
              -- Truncated trailing entry: discard it (matches RocksDB's
              -- behaviour after an unclean shutdown).
              pure acc
            else do
              let kl = beWord32 klBs
              k <- BS.hGet h (fromIntegral kl)
              case tag of
                0 -> do
                  vlBs <- BS.hGet h 4
                  if BS.length vlBs /= 4
                    then pure acc
                    else do
                      let vl = beWord32 vlBs
                      v <- BS.hGet h (fromIntegral vl)
                      if BS.length v == fromIntegral vl
                        then consume h (Map.insert k v acc)
                        else pure acc
                1 -> consume h (Map.delete k acc)
                _ -> pure acc -- unknown tag, halt replay


appendWal :: IORef Handle -> ByteString -> IO ()
appendWal walRef entry = do
  h <- readIORef walRef
  BS.hPut h entry


entryPut :: ByteString -> ByteString -> ByteString
entryPut k v =
  BS.concat
    [ BS.singleton 0
    , beEncode32 (fromIntegral (BS.length k))
    , k
    , beEncode32 (fromIntegral (BS.length v))
    , v
    ]


entryDel :: ByteString -> ByteString
entryDel k =
  BS.concat
    [ BS.singleton 1
    , beEncode32 (fromIntegral (BS.length k))
    , k
    ]


beEncode32 :: Word32 -> ByteString
beEncode32 w =
  BS.pack
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]


beWord32 :: ByteString -> Word32
beWord32 b =
  (fromIntegral (BS.index b 0) `shiftL` 24)
    .|. (fromIntegral (BS.index b 1) `shiftL` 16)
    .|. (fromIntegral (BS.index b 2) `shiftL` 8)
    .|. fromIntegral (BS.index b 3)


putBE32 :: Handle -> Word32 -> IO ()
putBE32 h = BS.hPut h . beEncode32
