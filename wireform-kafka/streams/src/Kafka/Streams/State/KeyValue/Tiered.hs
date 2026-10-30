{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.State.KeyValue.Tiered
Description : Hot + cold tiered KV store backend (Riffle §7)

Long-tail topologies blow up traditional state-store sizing:
99% of reads hit the recently-touched working set and 1% hit
entries from days or weeks ago. Keeping all of it in the hot
tier (e.g. RocksDB) is wasteful; keeping all of it cold (e.g.
S3) is slow.

Riffle §7 ships a /tiered/ wrapper: a hot 'KeyValueStore' (any
in-memory or persistent backend you like) backed by a cold
store. Both sides see the same 'KeyValueStore k v' interface.
Reads probe hot first, then cold; writes go to hot. Eviction
is driven by either count or age, and demotes entries to the
cold tier. Promotion happens lazily on read.

This module is the contract + an in-process reference cold
store. The S3 / object-store backend ships separately, since it
pulls in an HTTP client + credentials machinery.
-}
module Kafka.Streams.State.KeyValue.Tiered (
  -- * Cold tier contract
  ColdTier (..),
  inMemoryColdTier,

  -- * Wrapper
  TieredConfig (..),
  tieredKeyValueStore,

  -- * Eviction policies
  EvictionPolicy (..),
  countBasedEviction,

  -- * Diagnostics
  tieredStats,
  TieredStats (..),
) where

import Control.Monad (forM_, when)
import Data.IORef (
  IORef,
  atomicModifyIORef',
  modifyIORef',
  newIORef,
  readIORef,
 )
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Kafka.Streams.State.Store (
  KeyValueIterator (..),
  KeyValueStore (..),
 )


----------------------------------------------------------------------
-- Cold tier contract
----------------------------------------------------------------------

{- | The cold tier sees a simpler API than 'KeyValueStore'
because most cold tiers (S3, GCS, archival blob store) only
support point lookup + bulk scan. The wrapper translates the
richer 'KeyValueStore' calls into these primitives.
-}
data ColdTier k v = ColdTier
  { ctName :: !String
  , ctGet :: !(k -> IO (Maybe v))
  , ctPut :: !(k -> v -> IO ())
  , ctDelete :: !(k -> IO ())
  , ctScan :: !(IO [(k, v)])
  {- ^ Bulk scan. The wrapper uses this to merge cold-only
  entries into 'kvsAll' results. For very large cold tiers
  a streaming variant would be preferable; that's a Phase 3
  elaboration.
  -}
  }


{- | In-process cold tier backed by an 'IORef Map'. Used by tests
and by deployments that have no genuine cold store yet but
want the wrapper's eviction semantics for measuring.
-}
inMemoryColdTier :: Ord k => String -> IO (ColdTier k v)
inMemoryColdTier nm = do
  ref <- newIORef (Map.empty :: Map k v)
  pure
    ColdTier
      { ctName = nm
      , ctGet = \k -> Map.lookup k <$> readIORef ref
      , ctPut = \k v ->
          atomicModifyIORef'
            ref
            (\m -> (Map.insert k v m, ()))
      , ctDelete = \k ->
          atomicModifyIORef'
            ref
            (\m -> (Map.delete k m, ()))
      , ctScan = Map.toAscList <$> readIORef ref
      }


----------------------------------------------------------------------
-- Eviction
----------------------------------------------------------------------

{- | An eviction policy decides which hot-tier entries to demote
when the hot tier is over-full. The wrapper passes the current
hot-tier snapshot and returns the keys to evict (in the order
they should be evicted).
-}
data EvictionPolicy k v = EvictionPolicy
  { epName :: !String
  , epEvict :: !([(k, v)] -> [k])
  {- ^ Pure function from the current hot-tier snapshot to a
  list of keys to evict. Returning @[]@ leaves the wrapper
  to retry on the next sweep.
  -}
  }


{- | Evict the oldest @overflow@ entries by their position in
the hot-tier scan. \"Oldest\" here means \"earliest in
insertion order\" since the hot tier is typically a
'Data.Map.Strict.Map' that doesn't track access time. Real
LRU is a Phase 3 elaboration.
-}
countBasedEviction
  :: Int
  -- ^ hot tier capacity
  -> EvictionPolicy k v
countBasedEviction cap =
  EvictionPolicy
    { epName = "count-based-" <> show cap
    , epEvict = \pairs ->
        let overflow = length pairs - cap
        in if overflow <= 0
             then []
             else take overflow (map fst pairs)
    }


----------------------------------------------------------------------
-- Wrapper
----------------------------------------------------------------------

data TieredConfig k v = TieredConfig
  { tcHot :: !(KeyValueStore k v)
  , tcCold :: !(ColdTier k v)
  , tcEvict :: !(EvictionPolicy k v)
  , tcEvictEvery :: !Int
  {- ^ Run the eviction policy every N writes. A higher number
  amortises the sweep cost across more writes; 0 disables
  automatic eviction (callers can still drive it manually
  via 'tieredStats'). Default 64.
  -}
  }


{- | Wrap a hot/cold pair. Reads probe hot first, fall through
to cold on miss (and promote the cold entry back into hot).
Writes go to hot. Every @tcEvictEvery@ writes, the wrapper
runs the eviction policy and demotes the chosen entries to
cold.
-}
tieredKeyValueStore
  :: forall k v
   . Ord k
  => TieredConfig k v
  -> IO (KeyValueStore k v, IORef TieredStats)
tieredKeyValueStore cfg = do
  stats <- newIORef emptyStats
  writeCounter <- newIORef (0 :: Int)
  let hot = tcHot cfg
      cold = tcCold cfg
      bumpStat f =
        modifyIORef' stats f
      doEvictIfDue = when (tcEvictEvery cfg > 0) $ do
        n <- atomicModifyIORef' writeCounter (\x -> (x + 1, x + 1))
        when (n `mod` tcEvictEvery cfg == 0) $ do
          it <- kvsAll hot
          snap <- drainIter it
          let toEvict = epEvict (tcEvict cfg) snap
          forM_ toEvict $ \k -> do
            mv <- kvsGet hot k
            case mv of
              Nothing -> pure ()
              Just v -> do
                ctPut cold k v
                _ <- kvsDelete hot k
                bumpStat (\s -> s {tsEvictions = tsEvictions s + 1})
                pure ()
  pure
    ( KeyValueStore
        { kvsBase = kvsBase hot
        , kvsCache = Nothing
        , kvsApproxEntries = kvsApproxEntries hot
        , kvsGet = \k -> do
            mhot <- kvsGet hot k
            case mhot of
              Just v -> do
                bumpStat (\s -> s {tsHotHits = tsHotHits s + 1})
                pure (Just v)
              Nothing -> do
                mcold <- ctGet cold k
                case mcold of
                  Nothing -> do
                    bumpStat (\s -> s {tsMisses = tsMisses s + 1})
                    pure Nothing
                  Just v -> do
                    -- Promote.
                    kvsPut hot k v
                    ctDelete cold k
                    bumpStat
                      ( \s ->
                          s
                            { tsColdHits = tsColdHits s + 1
                            , tsPromotions = tsPromotions s + 1
                            }
                      )
                    pure (Just v)
        , kvsPut = \k v -> do
            kvsPut hot k v
            ctDelete cold k
            bumpStat (\s -> s {tsPuts = tsPuts s + 1})
            doEvictIfDue
        , kvsPutIfAbsent = \k v -> do
            -- Atomic across both tiers requires a "would-overwrite"
            -- check: hot.putIfAbsent, on Nothing also check cold.
            r <- kvsPutIfAbsent hot k v
            case r of
              Just _ -> pure r
              Nothing -> do
                mcold <- ctGet cold k
                case mcold of
                  Nothing -> do
                    bumpStat (\s -> s {tsPuts = tsPuts s + 1})
                    doEvictIfDue
                    pure Nothing
                  Just vOld -> do
                    -- A value lives in cold; promote it and
                    -- pretend hot.putIfAbsent saw it.
                    kvsPut hot k vOld
                    ctDelete cold k
                    pure (Just vOld)
        , kvsDelete = \k -> do
            rh <- kvsDelete hot k
            case rh of
              Just v -> do
                ctDelete cold k
                pure (Just v)
              Nothing -> do
                mc <- ctGet cold k
                ctDelete cold k
                pure mc
        , kvsRange = \lo hi -> rangeIter cfg lo hi
        , kvsReverseRange = \lo hi -> reverseRangeIter cfg lo hi
        , kvsAll = allIter cfg
        , kvsReverseAll = reverseAllIter cfg
        }
    , stats
    )


-- | Drain an iterator into a list (consumes it fully).
drainIter :: KeyValueIterator k v -> IO [(k, v)]
drainIter it = go []
  where
    go acc = do
      m <- kvIterNext it
      case m of
        Nothing -> do
          kvIterClose it
          pure (reverse acc)
        Just x -> go (x : acc)


iterFromList :: [(k, v)] -> IO (KeyValueIterator k v)
iterFromList xs0 = do
  ref <- newIORef xs0
  pure
    KeyValueIterator
      { kvIterNext = atomicModifyIORef' ref $ \xs -> case xs of
          [] -> ([], Nothing)
          (h : tl) -> (tl, Just h)
      , kvIterClose = atomicModifyIORef' ref (\_ -> ([], ()))
      }


{- | Merge hot + cold entries into a single ascending range. Hot
wins on conflict (it's the more recent write).
-}
rangeIter
  :: Ord k
  => TieredConfig k v -> k -> k -> IO (KeyValueIterator k v)
rangeIter cfg lo hi = do
  hotIt <- kvsRange (tcHot cfg) lo hi
  hotXs <- drainIter hotIt
  coldXs <- filter (\(k, _) -> k >= lo && k <= hi) <$> ctScan (tcCold cfg)
  iterFromList (mergeAsc hotXs coldXs)


reverseRangeIter
  :: Ord k
  => TieredConfig k v -> k -> k -> IO (KeyValueIterator k v)
reverseRangeIter cfg lo hi = do
  hotIt <- kvsReverseRange (tcHot cfg) lo hi
  hotXs <- drainIter hotIt
  coldXs <- filter (\(k, _) -> k >= lo && k <= hi) <$> ctScan (tcCold cfg)
  iterFromList (reverse (mergeAsc hotXs (reverse coldXs)))


allIter
  :: Ord k
  => TieredConfig k v -> IO (KeyValueIterator k v)
allIter cfg = do
  hotIt <- kvsAll (tcHot cfg)
  hotXs <- drainIter hotIt
  coldXs <- ctScan (tcCold cfg)
  iterFromList (mergeAsc hotXs coldXs)


reverseAllIter
  :: Ord k
  => TieredConfig k v -> IO (KeyValueIterator k v)
reverseAllIter cfg = do
  hotIt <- kvsReverseAll (tcHot cfg)
  hotXs <- drainIter hotIt
  coldXs <- ctScan (tcCold cfg)
  iterFromList (reverse (mergeAsc hotXs (reverse coldXs)))


{- | Merge two ascending key-sorted lists, hot taking precedence
on duplicate keys.
-}
mergeAsc :: Ord k => [(k, v)] -> [(k, v)] -> [(k, v)]
mergeAsc [] ys = ys
mergeAsc xs [] = xs
mergeAsc xs@((kx, vx) : restX) ys@((ky, vy) : restY) =
  case compare kx ky of
    LT -> (kx, vx) : mergeAsc restX ys
    GT -> (ky, vy) : mergeAsc xs restY
    EQ -> (kx, vx) : mergeAsc restX restY


-- Hot wins on duplicates.

----------------------------------------------------------------------
-- Diagnostics
----------------------------------------------------------------------

data TieredStats = TieredStats
  { tsHotHits :: !Int64
  , tsColdHits :: !Int64
  , tsMisses :: !Int64
  , tsPromotions :: !Int64
  , tsEvictions :: !Int64
  , tsPuts :: !Int64
  }
  deriving stock (Eq, Show)


emptyStats :: TieredStats
emptyStats = TieredStats 0 0 0 0 0 0


-- | Read the live stats snapshot.
tieredStats :: IORef TieredStats -> IO TieredStats
tieredStats = readIORef
