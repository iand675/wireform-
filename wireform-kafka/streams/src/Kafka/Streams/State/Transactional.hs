{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.State.Transactional
Description : KIP-892 EOS-V3 transactional buffer for state stores

Today the engine commits the producer transaction and the
changelog write atomically, but state-store puts happen
/before/ the transaction commit — if the commit fails after a
put, the store is left in a state inconsistent with the
broker-side log.

KIP-892 introduces a /store transaction/ layer: every put goes
into a per-task buffer first, the buffer is replayed onto the
underlying store only when the producer transaction commits
(immediately, atomically), and an abort discards the buffer.

This module provides the wrapper. It works on any
'KeyValueStore'; the buffer is a per-task in-memory
@Map@ overlay that:

  * intercepts 'kvsPut' / 'kvsPutIfAbsent' / 'kvsDelete' and
    records the change rather than applying it;
  * falls back to the underlying store on 'kvsGet' so
    /reads see writes/ within the same transaction (matches
    the JVM EOS-V3 read-your-writes property);
  * merges the buffer overlay into 'kvsRange' / 'kvsAll' /
    'kvsReverseRange' / 'kvsReverseAll' so scans also see
    in-transaction writes (extends read-your-writes from point
    lookups to iterators — caught by the
    'Streams.Properties.KVStoreSMSpec' state-machine test);
  * exposes 'commit' and 'abort' that the engine driver calls
    when the producer transaction commits / aborts.

Note: this is a /single-writer/ overlay. The engine guarantees
each task's store is written from one thread (the task's
processor); concurrent writers would need extra synchronisation.
-}
module Kafka.Streams.State.Transactional (
  TransactionalStore,
  newTransactionalStore,
  txnStore,
  txnCommit,
  txnAbort,
  txnPendingCount,
) where

import Control.Concurrent.STM
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Kafka.Streams.State.Store (
  KeyValueIterator (..),
  KeyValueStore (..),
  kvIteratorFromList,
 )
import ListT qualified
import StmContainers.Map qualified as StmMap


{- | A wrapper around a 'KeyValueStore' that buffers writes until
'txnCommit' (or discards them on 'txnAbort').
-}
data TransactionalStore k v = TransactionalStore
  { tsUnderlying :: !(KeyValueStore k v)
  , tsBuffer :: !(StmMap.Map k (BufferedOp v))
  }


{- | A single deferred operation. We collapse repeated puts /
deletes for the same key by overwrite — only the latest survives
to commit time, matching the semantics the active store would
end up with anyway.
-}
data BufferedOp v
  = BufPut !v
  | BufDelete


-- | Wrap an existing store with a transactional buffer.
newTransactionalStore
  :: KeyValueStore k v
  -> IO (TransactionalStore k v)
newTransactionalStore kvs = do
  buf <- StmMap.newIO
  pure TransactionalStore {tsUnderlying = kvs, tsBuffer = buf}


{- | Project the transactional wrapper as a 'KeyValueStore' that
the engine can hand to processor code. Reads — both point
lookups and range\/all iterators — see /pending writes/ first;
writes are buffered.

An 'Ord' constraint on @k@ is required so the buffer overlay
can be merged into the iterator results in key order. Every
shipped backend already requires 'Ord' for its own range
semantics, so this is not an additional caller obligation in
practice.
-}
txnStore
  :: forall k v
   . (Ord k, Hashable k)
  => TransactionalStore k v
  -> KeyValueStore k v
txnStore ts@TransactionalStore {..} =
  KeyValueStore
    { kvsBase = kvsBase tsUnderlying
    , kvsCache = Nothing
    , kvsApproxEntries = approxEntriesWithBuffer ts
    , kvsRange = \lo hi -> do
        base <- kvsRange tsUnderlying lo hi >>= drainIter
        merged <- mergeBufferIntoList tsBuffer (Just (lo, hi)) base
        kvIteratorFromList merged
    , kvsAll = do
        base <- kvsAll tsUnderlying >>= drainIter
        merged <- mergeBufferIntoList tsBuffer Nothing base
        kvIteratorFromList merged
    , kvsReverseRange = \lo hi -> do
        base <- kvsRange tsUnderlying lo hi >>= drainIter
        merged <- mergeBufferIntoList tsBuffer (Just (lo, hi)) base
        kvIteratorFromList (reverse merged)
    , kvsReverseAll = do
        base <- kvsAll tsUnderlying >>= drainIter
        merged <- mergeBufferIntoList tsBuffer Nothing base
        kvIteratorFromList (reverse merged)
    , kvsGet = \k -> do
        -- Read-your-writes: the buffer wins.
        mPending <- atomically $ StmMap.lookup k tsBuffer
        case mPending of
          Just BufDelete -> pure Nothing
          Just (BufPut v) -> pure (Just v)
          Nothing -> kvsGet tsUnderlying k
    , kvsPut = \k v ->
        atomically $
          StmMap.insert (BufPut v) k tsBuffer
    , kvsPutIfAbsent = \k v -> do
        -- Honour the underlying store's existing value if any
        -- (consulting the buffer first for read-your-writes).
        mExisting <- kvsGet (txnStore ts) k
        case mExisting of
          Just _ -> pure mExisting
          Nothing -> do
            atomically (StmMap.insert (BufPut v) k tsBuffer)
            pure Nothing
    , kvsDelete = \k -> do
        mExisting <- kvsGet (txnStore ts) k
        atomically (StmMap.insert BufDelete k tsBuffer)
        pure mExisting
    }


{- | Drain an iterator into a list in iteration order. Closes
the iterator as part of materialising it.
-}
drainIter :: KeyValueIterator k v -> IO [(k, v)]
drainIter it = go []
  where
    go acc = do
      mx <- kvIterNext it
      case mx of
        Nothing -> do
          kvIterClose it
          pure (reverse acc)
        Just kv -> go (kv : acc)


{- | Merge the in-buffer overlay onto an underlying-store
ascending list. Optionally constrains the result to a
@(lo, hi)@ range (inclusive). The buffer is consulted
atomically and snapshot-read for the duration of the merge.

For a buffered key in range:

  * 'BufPut' shadows the underlying value (and adds the key
    if absent).
  * 'BufDelete' removes the key.

Buffered keys outside the range are ignored.
-}
mergeBufferIntoList
  :: forall k v
   . Ord k
  => StmMap.Map k (BufferedOp v)
  -> Maybe (k, k)
  -> [(k, v)]
  -> IO [(k, v)]
mergeBufferIntoList buf mRange underlying = do
  bufPairs <- atomically $ ListT.toList (StmMap.listT buf)
  let filt = case mRange of
        Nothing -> id
        Just (lo, hi) -> filter (\(k, _) -> k >= lo && k <= hi)
      base = Map.fromList underlying
      merged = foldl' applyOp base (filt bufPairs)
      result = case mRange of
        Nothing -> Map.toAscList merged
        Just (lo, hi) ->
          Map.toAscList
            ( Map.takeWhileAntitone
                (<= hi)
                (Map.dropWhileAntitone (< lo) merged)
            )
  pure result
  where
    applyOp m (k, BufPut v) = Map.insert k v m
    applyOp m (k, BufDelete) = Map.delete k m


{- | 'kvsApproxEntries' adjusted for the buffer overlay. Used
by callers that consult 'approxEntries' to bound memory; an
underlying-only count would mislead during a long-running
transaction.
-}
approxEntriesWithBuffer
  :: TransactionalStore k v
  -> IO Int64
approxEntriesWithBuffer TransactionalStore {..} = do
  underN <- kvsApproxEntries tsUnderlying
  bufPairs <- atomically $ ListT.toList (StmMap.listT tsBuffer)
  -- Walk the buffer once: each 'BufPut' for a key not in the
  -- underlying adds 1; each 'BufDelete' for a key /in/ the
  -- underlying subtracts 1. We can't cheaply check
  -- "underlying contains key" so we approximate using
  -- 'kvsGet'; this is an approxEntries by design.
  delta <- foldDelta bufPairs 0
  pure (underN + delta)
  where
    foldDelta [] !acc = pure acc
    foldDelta ((k, op) : rest) !acc = do
      mExisting <- kvsGet tsUnderlying k
      let !d = case (op, mExisting) of
            (BufPut _, Nothing) -> 1
            (BufPut _, Just _) -> 0
            (BufDelete, Nothing) -> 0
            (BufDelete, Just _) -> -1
      foldDelta rest (acc + d)


{- | Drain the buffer onto the underlying store.

The pending operations are ordered by 'StmMap''s iteration order
(which is /not/ guaranteed across runs but is stable for any
single transaction). Since each (key) at most appears once in
the buffer (by overwrite collapse), order doesn't affect the
final state.
-}
txnCommit
  :: Hashable k
  => TransactionalStore k v -> IO ()
txnCommit TransactionalStore {..} = do
  pairs <- atomically $ ListT.toList (StmMap.listT tsBuffer)
  mapM_ apply pairs
  atomically $ resetBuffer tsBuffer
  where
    apply (k, BufPut v) = kvsPut tsUnderlying k v
    apply (k, BufDelete) = () <$ kvsDelete tsUnderlying k


-- | Discard the buffered writes.
txnAbort
  :: Hashable k
  => TransactionalStore k v -> IO ()
txnAbort TransactionalStore {..} = atomically (resetBuffer tsBuffer)


{- | How many uncommitted operations are buffered. Useful for
assertions / observability.
-}
txnPendingCount :: TransactionalStore k v -> IO Int
txnPendingCount TransactionalStore {..} = atomically $ do
  pairs <- ListT.toList (StmMap.listT tsBuffer)
  pure (length pairs)


resetBuffer
  :: Hashable k
  => StmMap.Map k v
  -> STM ()
resetBuffer m = do
  pairs <- ListT.toList (StmMap.listT m)
  mapM_ (\(k, _) -> StmMap.delete k m) pairs
