{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.State.KeyValue.Caching
Description : Write-back caching layer over a 'KeyValueStore'

Mirrors the Streams write-back cache that fronts a state store
when @cache.max.bytes.buffering > 0@.  Writes buffer in a per-key
map; on flush (every commit) and on cache eviction the dirty
entries are written through to the underlying store and forwarded
to a user-supplied emit callback.

== Properties

  * /Read-your-writes/: 'kvsGet' on the cached store sees the
    latest buffered write before falling back to the underlying.
  * /Dedup-per-commit/: multiple writes to the same key between
    two flushes collapse into a single emit on flush.
  * /Tombstones/: 'kvsDelete' marks a buffered tombstone, which
    is also emitted (as 'Nothing') on flush.
-}
module Kafka.Streams.State.KeyValue.Caching (
  CachingConfig (..),
  defaultCachingConfig,
  cachingKeyValueStore,
  cachingKeyValueStoreBuilder,
  CacheEmitter,
) where

import Control.Monad (when)
import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Kafka.Streams.State.Store (
  KeyValueIterator (..),
  KeyValueStore (..),
  StateStore (..),
  StoreBuilderKV (..),
  kvIteratorFromList,
 )


{- | What gets called for every entry evicted out of the cache.
'Just v' means write-through with the latest value; 'Nothing'
means the entry was tombstoned (deleted).
-}
type CacheEmitter k v = k -> Maybe v -> IO ()


data CachingConfig = CachingConfig
  { ccMaxEntries :: !Int64
  {- ^ Eviction trigger: when the number of dirty entries exceeds
  this, eagerly evict the oldest. The Java config uses bytes,
  but per-record byte sizing is opaque here so we approximate
  with an entry count.
  -}
  }
  deriving stock (Show)


defaultCachingConfig :: CachingConfig
defaultCachingConfig =
  CachingConfig
    { ccMaxEntries = 10_000
    }


-- | One cache entry is either a buffered live value or a tombstone.
data CacheSlot v = CSPut !v | CSTomb
  deriving stock (Eq, Show)


-- | Wrap an existing 'KeyValueStore' with a write-back cache.
cachingKeyValueStore
  :: forall k v
   . Ord k
  => KeyValueStore k v
  -> CachingConfig
  -> CacheEmitter k v
  -> IO (KeyValueStore k v)
cachingKeyValueStore underlying cfg emit = do
  cache <- newIORef (Map.empty :: Map k (CacheSlot v))
  let
    flushAll = do
      m <- atomicModifyIORef' cache (\xs -> (Map.empty, xs))
      mapM_
        ( \(k, slot) -> case slot of
            CSPut v -> do
              kvsPut underlying k v
              emit k (Just v)
            CSTomb -> do
              _ <- kvsDelete underlying k
              emit k Nothing
        )
        (Map.toAscList m)

    maybeEvict = do
      n <- atomicModifyIORef' cache $ \m ->
        (m, fromIntegral (Map.size m) :: Int64)
      when (n > ccMaxEntries cfg) flushAll

    cachedGet k = do
      m <- readIORef cache
      case Map.lookup k m of
        Just (CSPut v) -> pure (Just v)
        Just CSTomb -> pure Nothing
        Nothing -> kvsGet underlying k

    cachedPut k v = do
      atomicModifyIORef' cache $ \m ->
        let !m' = Map.insert k (CSPut v) m in (m', ())
      maybeEvict

    cachedDelete k = do
      mPrev <- cachedGet k
      atomicModifyIORef' cache $ \m ->
        let !m' = Map.insert k CSTomb m in (m', ())
      maybeEvict
      pure mPrev

    cachedPutIfAbsent k v = do
      mPrev <- cachedGet k
      case mPrev of
        Just _ -> pure mPrev
        Nothing -> do
          cachedPut k v
          pure Nothing

    cachedRange lo hi = do
      m <- readIORef cache
      base <- kvsRange underlying lo hi
      baseList <- drainAll base
      let cacheSlice =
            Map.takeWhileAntitone (<= hi) $
              Map.dropWhileAntitone (< lo) m
          merged = mergeRange cacheSlice baseList
      kvIteratorFromList merged

    cachedAll = do
      m <- readIORef cache
      base <- kvsAll underlying
      baseList <- drainAll base
      kvIteratorFromList (mergeRange m baseList)

    cachedCount = do
      n0 <- kvsApproxEntries underlying
      m <- readIORef cache
      let dirty = fromIntegral (Map.size m) :: Int64
      -- Approximate: assumes cache and store don't overlap heavily.
      pure (n0 + dirty)

    underlyingBase = kvsBase underlying

    !storeNm = storeStoreName underlyingBase
    !persistent = storePersistent underlyingBase

  pure
    KeyValueStore
      { kvsBase =
          StateStore
            { storeStoreName = storeNm
            , storePersistent = persistent
            , storeFlush = do
                flushAll
                storeFlush underlyingBase
            , storeClose = do
                flushAll
                storeClose underlyingBase
            }
      , kvsGet = cachedGet
      , kvsPut = cachedPut
      , kvsPutIfAbsent = cachedPutIfAbsent
      , kvsDelete = cachedDelete
      , kvsRange = cachedRange
      , kvsAll = cachedAll
      , kvsApproxEntries = cachedCount
      , kvsReverseRange = \lo hi -> do
          m <- readIORef cache
          base <- kvsReverseRange underlying lo hi
          baseList <- drainAll base
          let cacheSlice =
                Map.takeWhileAntitone (<= hi) $
                  Map.dropWhileAntitone (< lo) m
          kvIteratorFromList
            ( reverse
                ( mergeRange
                    cacheSlice
                    (reverse baseList)
                )
            )
      , kvsReverseAll = do
          m <- readIORef cache
          base <- kvsReverseAll underlying
          baseList <- drainAll base
          kvIteratorFromList (reverse (mergeRange m (reverse baseList)))
      }
  where
    drainAll it = go []
      where
        go acc = do
          mx <- kvIterNext it
          case mx of
            Nothing -> do
              kvIterClose it
              pure (reverse acc)
            Just kv -> go (kv : acc)

    -- 'Map.union' would prefer the cache values, but it doesn't drop
    -- tombstoned keys.  Build the merged list manually so tombstones
    -- remove store entries.
    mergeRange cacheSlice baseList =
      let baseMap = Map.fromAscList baseList
          merged = Map.foldrWithKey insertOrTomb baseMap cacheSlice
      in Map.toAscList merged

    insertOrTomb k slot acc = case slot of
      CSPut v -> Map.insert k v acc
      CSTomb -> Map.delete k acc


{- | A store builder for a caching layer.  Composes with another
builder by realising both at task-init time, then wrapping.
-}
cachingKeyValueStoreBuilder
  :: Ord k
  => CachingConfig
  -> CacheEmitter k v
  -> StoreBuilderKV k v
  -- ^ underlying-store builder
  -> StoreBuilderKV k v
cachingKeyValueStoreBuilder cfg emit base =
  StoreBuilderKV
    { sbKvName = sbKvName base
    , sbKvLogging = sbKvLogging base
    , sbKvSerdes = sbKvSerdes base
    , sbKvBuild = do
        under <- sbKvBuild base
        cachingKeyValueStore under cfg emit
    }
