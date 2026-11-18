{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{- |
Module      : Hashtable
Description : Fast mutable open-addressing hash tables.

A mutable hash table with the API shape of @vector-hashtables@'
"Data.Vector.Hashtables", on a different engine: Boost
@unordered_flat_map@-style open addressing with 64-bit SWAR group words,
8-bit tags, per-group overflow Bloom bytes and no tombstones. See
"Hashtable.Internal" for the layout.

@
import qualified Hashtable as H

type Counts = H.Dictionary RealWorld H.Unboxed Int H.Unboxed Int

main = do
  t <- H.initialize 1024 :: IO Counts
  H.upsert t (maybe 1 (+ 1)) 42
  H.lookup t 42          -- Just 1
@

== Choosing storage

Keys and values each pick a storage tag from "Hashtable.Storage":
'Unboxed' (any 'Data.Primitive.Types.Prim' type — by far the fastest),
'Strict' (boxed, WHNF-forced on write; the right default for boxed
values) or 'Boxed' (lazy).

== Performance notes

* Every operation is @INLINE@ or @INLINABLE@. Use the table at concrete
  types (or @SPECIALIZE@ your own polymorphic wrappers) and the probe loop
  is compiled for your key type with no dictionary passing.
* 'lookup' returns 'Maybe'; if you scrutinise the result straight away
  GHC removes the 'Just'. 'findWithDefault' never allocates.
* Iteration order is the internal slot order: unspecified, and it changes
  when the table grows.
* Not thread-safe. Wrap in an @MVar@ if you share one between threads.
-}
module Hashtable
  ( -- * Types
    Dictionary
  , FrozenDictionary
  , Storage
  , Boxed
  , Strict
  , Unboxed

    -- * Construction
  , initialize
  , newSized
  , new
  , clone
  , reserve
  , clear

    -- * Size
  , null
  , size
  , length
  , capacity

    -- * Queries
  , lookup
  , lookup'
  , at
  , at'
  , member
  , findWithDefault
  , findEntry
  , lookupIndex

    -- * Updates
  , insert
  , insertWith
  , insertIfAbsent
  , delete
  , adjust
  , adjustM
  , update
  , upsert
  , alter
  , alterM
  , mutate
  , mutateM

    -- * Bulk in-place updates
  , mapInPlace
  , filterInPlace

    -- * Batched operations (software-prefetched)
  , lookupBatch
  , insertBatch
  , upsertBatch

    -- * Traversal
  , foldM
  , forWithKey_
  , keys
  , values

    -- * Combine
  , union
  , unionWith
  , unionWithKey
  , difference
  , differenceWith
  , intersection
  , intersectionWith
  , intersectionWithKey

    -- * Lists
  , fromList
  , fromListN
  , fromListWith
  , toList

    -- * Freezing
  , unsafeFreeze
  , unsafeThaw
  , freeze
  , thaw
  , findElem
  , frozenLookup
  , frozenMember
  , frozenSize
  , frozenKeyAt
  , frozenValueAt
  , frozenToList

    -- * Re-exports
  , PrimMonad (..)
  , RealWorld
  ) where

import Control.Monad.Primitive (PrimMonad (..), primitive_)
import Data.Hashable (Hashable)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Primitive.PrimArray (newPrimArray, readPrimArray, writePrimArray)
import GHC.Exts hiding (fromList, fromListN, toList)
import Hashtable.Internal
import Hashtable.Storage
import Prelude hiding (length, lookup, null)

------------------------------------------------------------------------
-- Construction

-- | A table with room for at least @n@ entries before it first grows.
initialize ::
  (PrimMonad m, Storage ks k, Storage vs v) =>
  Int -> m (Dictionary (PrimState m) ks k vs v)
initialize n = primitive $ \s -> case groupsFor n of
  I# g -> case newTable# g s of
    (# s1, t #) -> case newMutVar# t s1 of
      (# s2, ref #) -> (# s2, Dictionary ref #)
{-# INLINE initialize #-}

-- | Alias for 'initialize' (the @hashtables@ name).
newSized ::
  (PrimMonad m, Storage ks k, Storage vs v) =>
  Int -> m (Dictionary (PrimState m) ks k vs v)
newSized = initialize
{-# INLINE newSized #-}

-- | An empty table (room for 6 entries).
new :: (PrimMonad m, Storage ks k, Storage vs v) => m (Dictionary (PrimState m) ks k vs v)
new = initialize 0
{-# INLINE new #-}

-- | An independent copy.
clone ::
  (PrimMonad m, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> m (Dictionary (PrimState m) ks k vs v)
clone (Dictionary ref) = primitive $ \s -> case readMutVar# ref s of
  (# s1, t #) -> case cloneTable# t s1 of
    (# s2, t' #) -> case newMutVar# t' s2 of
      (# s3, ref' #) -> (# s3, Dictionary ref' #)
{-# INLINE clone #-}

-- | Make room for at least @n@ entries in total, so that many inserts
-- will not trigger a rehash.
reserve ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> Int -> m ()
reserve (Dictionary ref) (I# want) = primitive_ $ \s -> case readMutVar# ref s of
  (# s1, t@(Table _ ctrl _ _) #) -> case readMaxLoad# ctrl s1 of
    (# s2, ml #)
      | isTrue# (ml >=# want) -> s2
      | otherwise -> case groupsFor (I# want) of
          I# g -> case rehash# t g s2 of
            (# s3, t' #) -> writeMutVar# ref t' s3
{-# INLINE reserve #-}

-- | Remove every entry, keeping the current capacity. Boxed columns are
-- replaced, so nothing they referenced stays reachable.
clear ::
  (PrimMonad m, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> m ()
clear (Dictionary ref) = primitive_ $ \s -> case readMutVar# ref s of
  (# s1, Table mask _ _ _ #) -> case newTable# (mask +# 1#) s1 of
    (# s2, t #) -> writeMutVar# ref t s2
{-# INLINE clear #-}

------------------------------------------------------------------------
-- Size

size :: PrimMonad m => Dictionary (PrimState m) ks k vs v -> m Int
size (Dictionary ref) = primitive $ \s -> case readMutVar# ref s of
  (# s1, Table _ ctrl _ _ #) -> case readSize# ctrl s1 of
    (# s2, n #) -> (# s2, I# n #)
{-# INLINE size #-}

-- | Alias for 'size'.
length :: PrimMonad m => Dictionary (PrimState m) ks k vs v -> m Int
length = size
{-# INLINE length #-}

{- | Entries the table can hold before its next rehash (live entries plus
remaining headroom). At most 7/8 of the slot count; deletions from
overflowed groups lower it until the next rehash (see "Hashtable.Internal").
-}
capacity :: PrimMonad m => Dictionary (PrimState m) ks k vs v -> m Int
capacity (Dictionary ref) = primitive $ \s -> case readMutVar# ref s of
  (# s1, Table _ ctrl _ _ #) -> case readMaxLoad# ctrl s1 of
    (# s2, ml #) -> (# s2, I# ml #)
{-# INLINE capacity #-}

null :: PrimMonad m => Dictionary (PrimState m) ks k vs v -> m Bool
null d = (== 0) <$> size d
{-# INLINE null #-}

------------------------------------------------------------------------
-- Queries

lookup ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> k -> m (Maybe v)
lookup (Dictionary ref) k = primitive $ \s -> case readMutVar# ref s of
  (# s1, Table mask ctrl kcol vcol #) -> case lookupPos# mask ctrl kcol k (inline hashKey# k) s1 of
    (# s2, p #)
      | isTrue# (p <# 0#) -> (# s2, Nothing #)
      | otherwise -> case readM vcol (inline slotOf# p) s2 of
          (# s3, v #) -> (# s3, Just v #)
{-# INLINE lookup #-}

-- | Like 'lookup', but a missing key is an error.
lookup' ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> k -> m v
lookup' (Dictionary ref) k = primitive $ \s -> case readMutVar# ref s of
  (# s1, Table mask ctrl kcol vcol #) -> case lookupPos# mask ctrl kcol k (inline hashKey# k) s1 of
    (# s2, p #)
      | isTrue# (p <# 0#) -> (# s2, keyNotFound #)
      | otherwise -> readM vcol (inline slotOf# p) s2
{-# INLINE lookup' #-}

keyNotFound :: a
keyNotFound = errorWithoutStackTrace "Hashtable: key not found"
{-# NOINLINE keyNotFound #-}

-- | Alias for 'lookup''.
at ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> k -> m v
at = lookup'
{-# INLINE at #-}

-- | Alias for 'lookup'.
at' ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> k -> m (Maybe v)
at' = lookup
{-# INLINE at' #-}

member ::
  (PrimMonad m, Hashable k, Storage ks k) =>
  Dictionary (PrimState m) ks k vs v -> k -> m Bool
member (Dictionary ref) k = primitive $ \s -> case readMutVar# ref s of
  (# s1, Table mask ctrl kcol _ #) -> case lookupPos# mask ctrl kcol k (inline hashKey# k) s1 of
    (# s2, p #) -> (# s2, isTrue# (p >=# 0#) #)
{-# INLINE member #-}

-- | The value at a key, or the default. Never allocates.
findWithDefault ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> v -> k -> m v
findWithDefault (Dictionary ref) def k = primitive $ \s -> case readMutVar# ref s of
  (# s1, Table mask ctrl kcol vcol #) -> case lookupPos# mask ctrl kcol k (inline hashKey# k) s1 of
    (# s2, p #)
      | isTrue# (p <# 0#) -> (# s2, def #)
      | otherwise -> readM vcol (inline slotOf# p) s2
{-# INLINE findWithDefault #-}

{- | Entry index of a key, or @-1@. The index addresses the key and value
columns; it stays valid until the next insert that grows the table.
-}
findEntry ::
  (PrimMonad m, Hashable k, Storage ks k) =>
  Dictionary (PrimState m) ks k vs v -> k -> m Int
findEntry (Dictionary ref) k = primitive $ \s -> case readMutVar# ref s of
  (# s1, Table mask ctrl kcol _ #) -> case lookupPos# mask ctrl kcol k (inline hashKey# k) s1 of
    (# s2, p #)
      | isTrue# (p <# 0#) -> (# s2, -1 #)
      | otherwise -> (# s2, I# (inline slotOf# p) #)
{-# INLINE findEntry #-}

-- | 'findEntry' as a 'Maybe'.
lookupIndex ::
  (PrimMonad m, Hashable k, Storage ks k) =>
  Dictionary (PrimState m) ks k vs v -> k -> m (Maybe Int)
lookupIndex d k = do
  i <- findEntry d k
  pure $! if i < 0 then Nothing else Just i
{-# INLINE lookupIndex #-}

------------------------------------------------------------------------
-- Updates

-- | Insert or overwrite.
insert ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> k -> v -> m ()
insert (Dictionary ref) k v = primitive_ $ \s -> case readMutVar# ref s of
  (# s1, t@(Table mask ctrl kcol vcol) #) ->
    let w = inline hashKey# k
     in case lookupPos# mask ctrl kcol k w s1 of
          (# s2, r #)
            | isTrue# (r >=# 0#) -> writeM vcol (inline slotOf# r) v s2
            | otherwise -> insertAbsent# ref t w k v s2
{-# INLINE insert #-}

-- | @insertWith f t k new@ stores @f new old@ if @k@ is present, @new@
-- otherwise.
insertWith ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  (v -> v -> v) -> Dictionary (PrimState m) ks k vs v -> k -> v -> m ()
insertWith f (Dictionary ref) k v = primitive_ $ \s -> case readMutVar# ref s of
  (# s1, t@(Table mask ctrl kcol vcol) #) ->
    let w = inline hashKey# k
     in case lookupPos# mask ctrl kcol k w s1 of
          (# s2, r #)
            | isTrue# (r >=# 0#) ->
                let i = inline slotOf# r
                 in case readM vcol i s2 of
                      (# s3, old #) -> writeM vcol i (f v old) s3
            | otherwise -> insertAbsent# ref t w k v s2
{-# INLINE insertWith #-}

-- | Insert only if the key is absent. 'True' if it was inserted.
insertIfAbsent ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> k -> v -> m Bool
insertIfAbsent (Dictionary ref) k v = primitive $ \s -> case readMutVar# ref s of
  (# s1, t@(Table mask ctrl kcol _) #) ->
    let w = hashKey# k
     in case lookupPos# mask ctrl kcol k w s1 of
          (# s2, r #)
            | isTrue# (r >=# 0#) -> (# s2, False #)
            | otherwise -> (# insertAbsent# ref t w k v s2, True #)
{-# INLINE insertIfAbsent #-}

delete ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> k -> m ()
delete (Dictionary ref) k = primitive_ $ \s -> case readMutVar# ref s of
  (# s1, t@(Table mask ctrl kcol _) #) -> case lookupPos# mask ctrl kcol k (inline hashKey# k) s1 of
    (# s2, p #)
      | isTrue# (p <# 0#) -> s2
      | otherwise -> deleteAtPos# t p s2
{-# INLINE delete #-}

-- | @adjust t f k@ replaces the value at @k@ with @f@ of it; absent keys
-- are left alone. One probe.
adjust ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> (v -> v) -> k -> m ()
adjust (Dictionary ref) f k = primitive_ $ \s -> case readMutVar# ref s of
  (# s1, Table mask ctrl kcol vcol #) -> case lookupPos# mask ctrl kcol k (hashKey# k) s1 of
    (# s2, p #)
      | isTrue# (p <# 0#) -> s2
      | otherwise ->
          let i = slotOf# p
           in case readM vcol i s2 of
                (# s3, old #) -> writeM vcol i (f old) s3
{-# INLINE adjust #-}

-- | Monadic 'adjust'. Like 'alterM', the key is looked up again after the
-- callback, which may use the table; if the callback deleted the key, the
-- new value is inserted.
adjustM ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> (v -> m v) -> k -> m ()
adjustM d f = alterM d (traverse f)
{-# INLINE adjustM #-}

-- | @update t f k@: like 'adjust', but 'Nothing' from @f@ deletes. One
-- probe.
update ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> (v -> Maybe v) -> k -> m ()
update (Dictionary ref) f k = primitive_ $ \s -> case readMutVar# ref s of
  (# s1, t@(Table mask ctrl kcol vcol) #) -> case lookupPos# mask ctrl kcol k (hashKey# k) s1 of
    (# s2, p #)
      | isTrue# (p <# 0#) -> s2
      | otherwise ->
          let i = slotOf# p
           in case readM vcol i s2 of
                (# s3, old #) -> case f old of
                  Nothing -> deleteAtPos# t p s3
                  Just new' -> writeM vcol i new' s3
{-# INLINE update #-}

-- | @upsert t f k@ stores @f (lookup k)@ at @k@. One probe.
upsert ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> (Maybe v -> v) -> k -> m ()
upsert (Dictionary ref) f k = primitive_ $ \s -> case readMutVar# ref s of
  (# s1, t@(Table mask ctrl kcol vcol) #) ->
    let w = inline hashKey# k
     in case lookupPos# mask ctrl kcol k w s1 of
          (# s2, r #)
            | isTrue# (r >=# 0#) ->
                let i = inline slotOf# r
                 in case readM vcol i s2 of
                      (# s3, old #) -> writeM vcol i (f (Just old)) s3
            | otherwise -> insertAbsent# ref t w k (f Nothing) s2
{-# INLINE upsert #-}

{- | @alter t f k@: 'Nothing' from @f@ deletes, @'Just' v@ stores @v@. One
probe.
-}
alter ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> (Maybe v -> Maybe v) -> k -> m ()
alter (Dictionary ref) f k = primitive_ $ \s -> case readMutVar# ref s of
  (# s1, t@(Table mask ctrl kcol vcol) #) ->
    let w = inline hashKey# k
     in case lookupPos# mask ctrl kcol k w s1 of
          (# s2, r #)
            | isTrue# (r >=# 0#) ->
                let i = inline slotOf# r
                 in case readM vcol i s2 of
                      (# s3, old #) -> case f (Just old) of
                        Nothing -> deleteAtPos# t r s3
                        Just new' -> writeM vcol i new' s3
            | otherwise -> case f Nothing of
                Nothing -> s2
                Just new' -> insertAbsent# ref t w k new' s2
{-# INLINE alter #-}

{- | @mutate t f k@: 'alter' that also returns a result, the shape of
@hashtables@' @mutate@ (with this module's table-function-key argument
order). One probe.
-}
mutate ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> (Maybe v -> (Maybe v, a)) -> k -> m a
mutate (Dictionary ref) f k = primitive $ \s -> case readMutVar# ref s of
  (# s1, t@(Table mask ctrl kcol vcol) #) ->
    let w = hashKey# k
     in case lookupPos# mask ctrl kcol k w s1 of
          (# s2, r #)
            | isTrue# (r >=# 0#) ->
                let i = slotOf# r
                 in case readM vcol i s2 of
                      (# s3, old #) -> case f (Just old) of
                        (Nothing, a) -> (# deleteAtPos# t r s3, a #)
                        (Just new', a) -> (# writeM vcol i new' s3, a #)
            | otherwise -> case f Nothing of
                (Nothing, a) -> (# s2, a #)
                (Just new', a) -> (# insertAbsent# ref t w k new' s2, a #)
{-# INLINE mutate #-}

-- | Monadic 'mutate'; looks the key up again after the callback, like
-- 'alterM'.
mutateM ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> (Maybe v -> m (Maybe v, a)) -> k -> m a
mutateM d f k = do
  old <- lookup d k
  (r, a) <- f old
  case r of
    Just v -> insert d k v
    Nothing -> case old of
      Nothing -> pure ()
      Just _ -> delete d k
  pure a
{-# INLINE mutateM #-}

{- | Monadic 'alter'. The callback may itself use the table, so the key is
looked up again after it returns: two probes, always correct.
-}
alterM ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> (Maybe v -> m (Maybe v)) -> k -> m ()
alterM d f k = do
  old <- lookup d k
  r <- f old
  case r of
    Just v -> insert d k v
    Nothing -> case old of
      Nothing -> pure ()
      Just _ -> delete d k
{-# INLINE alterM #-}

------------------------------------------------------------------------
-- Bulk in-place updates

-- | Every position of a full slot, in slot order, at the 'State#' level.
-- For pure per-entry work: no user code can touch the table meanwhile.
forFullPositions# ::
  Table s ks k vs v -> (Int# -> State# s -> State# s) -> State# s -> State# s
forFullPositions# (Table mask ctrl _ _) f = groups 0#
 where
  groups g s
    | isTrue# (g ># mask) = s
    | otherwise = case inline loadGroup# ctrl g s of
        (# s1, grp #) -> laneLoop g (inline matchFull# grp) s1
  laneLoop g m s
    | isTrue# (eqWord# m 0##) = groups (g +# 1#) s
    | otherwise = case f (inline lanePos# g m) s of
        s1 -> laneLoop g (and# m (minusWord# m 1##)) s1
{-# INLINE forFullPositions# #-}

-- | Replace every value with @f key value@.
mapInPlace ::
  (PrimMonad m, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> (k -> v -> v) -> m ()
mapInPlace (Dictionary ref) f = primitive_ $ \s -> case readMutVar# ref s of
  (# s1, t@(Table _ _ kcol vcol) #) -> forFullPositions# t
    ( \p s2 ->
        let i = inline slotOf# p
         in case readM kcol i s2 of
              (# s3, k #) -> case readM vcol i s3 of
                (# s4, v #) -> writeM vcol i (f k v) s4
    )
    s1
{-# INLINE mapInPlace #-}

-- | Delete every entry for which the predicate is 'False'. Never rehashes.
filterInPlace ::
  (PrimMonad m, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> (k -> v -> Bool) -> m ()
filterInPlace (Dictionary ref) keep = primitive_ $ \s -> case readMutVar# ref s of
  (# s1, t@(Table _ _ kcol vcol) #) -> forFullPositions# t
    ( \p s2 ->
        let i = inline slotOf# p
         in case readM kcol i s2 of
              (# s3, k #) -> case readM vcol i s3 of
                (# s4, v #) -> if keep k v then s4 else deleteAtPos# t p s4
    )
    s1
{-# INLINE filterInPlace #-}

------------------------------------------------------------------------
-- Batched operations

{- $batch
Batched operations run the keys through a three-stage software pipeline so
that, on tables too big for cache, memory latency overlaps across many keys
instead of being paid one probe at a time:

  * at @i + 2·D@: hash the key into a small ring, prefetch its home group;
  * at @i + D@: match the (now cached) group word and prefetch the exact key
    and value lines the probe will touch;
  * at @i@: the probe itself, against warm lines.

Each key is hashed once. On tables that fit in cache the pipeline is pure
overhead, so below 'batchThreshold' groups it falls back to the plain loop.
-}

-- | Pipeline distance @D@ (keys). Two stages in flight keep about @2·D@
-- lines outstanding, inside a core's line-fill buffers.
batchDistance :: Int
batchDistance = 8

-- | Group count from which batches prefetch: 2^15 groups, about 229k
-- slots (~4 MB of 'Int'/'Int' table, past a core's L2). Below it the
-- table is cache-resident and the pipeline is pure overhead (measured: 1.5×
-- slower at 100k entries, 2× faster at 1M and 16M).
batchThreshold :: Int
batchThreshold = 32768

-- | Hash ring: a power of two strictly greater than the @2·D@ keys in
-- flight, or stage A would overwrite a hash before stage C reads it.
ringMask :: Int
ringMask = go 1 - 1
 where
  go r
    | r > 2 * batchDistance = r
    | otherwise = go (r * 2)

{- | Drive @step i w@ over @[0, n)@ (@w@ is key @i@'s hash), prefetching
@D@ and @2·D@ keys ahead when the table is big. One loop with a
loop-invariant (perfectly predicted) branch, rather than two loops, so
@step@ has a single call site and is inlined into it. @key i@ must be
cheap (an index).
-}
batched ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Bool -> Dictionary (PrimState m) ks k vs v -> Int -> (Int -> k) -> (Int -> Word -> m ()) -> m ()
batched forInsert d@(Dictionary ref) n key step = do
  pipelined <- (&& n > 2 * batchDistance) <$> bigTable d
  ring <- newPrimArray (if pipelined then ringMask + 1 else 0)
  let dist = batchDistance
      !(I# rm) = ringMask
      stageA j = do
        let !w@(W# w#) = hashW (key j)
        writePrimArray ring (I# (andI# (case j of I# j# -> j#) rm)) w
        primitive_ $ \s -> case readMutVar# ref s of
          (# s1, Table mask ctrl _ _ #) -> prefetchHome# mask ctrl w# s1
      stageB j = do
        W# w# <- readPrimArray ring (I# (andI# (case j of I# j# -> j#) rm))
        primitive_ $ \s -> case readMutVar# ref s of
          (# s1, t #) -> prefetchSlot# forInsert t w# s1
      prime j
        | j >= min n (2 * dist) = pure ()
        | otherwise = stageA j >> prime (j + 1)
      go i
        | i >= n = pure ()
        | otherwise = do
            w <-
              if pipelined
                then do
                  if i + 2 * dist < n then stageA (i + 2 * dist) else pure ()
                  if i + dist < n then stageB (i + dist) else pure ()
                  readPrimArray ring (I# (andI# (case i of I# i# -> i#) rm))
                else pure (hashW (key i))
            step i w
            go (i + 1)
  if pipelined then prime 0 else pure ()
  go 0
{-# INLINE batched #-}

-- | Hash a key outside the pipeline.
hashW :: Hashable k => k -> Word
hashW k = W# (hashKey# k)
{-# INLINE hashW #-}

bigTable :: PrimMonad m => Dictionary (PrimState m) ks k vs v -> m Bool
bigTable (Dictionary ref) = primitive $ \s -> case readMutVar# ref s of
  (# s1, Table mask _ _ _ #) -> (# s1, isTrue# (mask >=# (case batchThreshold of I# b -> b -# 1#)) #)
{-# INLINE bigTable #-}

-- | Like 'findWithDefault' for every key of a vector, pipelined.
lookupBatch ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v, G.Vector kv k, G.Vector vv v) =>
  Dictionary (PrimState m) ks k vs v -> v -> kv k -> m (vv v)
lookupBatch d@(Dictionary ref) def ks = do
  let n = G.length ks
  out <- GM.unsafeNew n
  batched False d n (G.unsafeIndex ks) $ \i (W# w) -> do
    let !k = G.unsafeIndex ks i
    v <- primitive $ \s -> case readMutVar# ref s of
      (# s1, Table mask ctrl kcol vcol #) -> case lookupPos# mask ctrl kcol k w s1 of
        (# s2, p #)
          | isTrue# (p <# 0#) -> (# s2, def #)
          | otherwise -> readM vcol (slotOf# p) s2
    GM.unsafeWrite out i v
  G.unsafeFreeze out
{-# INLINE lookupBatch #-}

-- | 'insert' every pair of two vectors (up to the shorter), pipelined.
-- Later duplicates win.
insertBatch ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v, G.Vector kv k, G.Vector vv v) =>
  Dictionary (PrimState m) ks k vs v -> kv k -> vv v -> m ()
insertBatch d@(Dictionary ref) ks vs = do
  let n = min (G.length ks) (G.length vs)
  reserve d . (+ n) =<< size d
  batched True d n (G.unsafeIndex ks) $ \i (W# w) -> do
    let !k = G.unsafeIndex ks i
        v = G.unsafeIndex vs i
    primitive_ $ \s -> case readMutVar# ref s of
      (# s1, t@(Table mask ctrl kcol vcol) #) -> case lookupPos# mask ctrl kcol k w s1 of
        (# s2, r #)
          | isTrue# (r >=# 0#) -> writeM vcol (slotOf# r) v s2
          | otherwise -> insertAbsent# ref t w k v s2
{-# INLINE insertBatch #-}

-- | 'upsert' every key of a vector, pipelined: e.g. a histogram is
-- @upsertBatch t (maybe 1 (+ 1)) keys@.
upsertBatch ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v, G.Vector kv k) =>
  Dictionary (PrimState m) ks k vs v -> (Maybe v -> v) -> kv k -> m ()
upsertBatch d@(Dictionary ref) f ks =
  batched True d (G.length ks) (G.unsafeIndex ks) $ \i (W# w) -> do
    let !k = G.unsafeIndex ks i
    primitive_ $ \s -> case readMutVar# ref s of
      (# s1, t@(Table mask ctrl kcol vcol) #) -> case lookupPos# mask ctrl kcol k w s1 of
        (# s2, r #)
          | isTrue# (r >=# 0#) ->
              let j = slotOf# r
               in case readM vcol j s2 of
                    (# s3, old #) -> writeM vcol j (f (Just old)) s3
          | otherwise -> insertAbsent# ref t w k (f Nothing) s2
{-# INLINE upsertBatch #-}

------------------------------------------------------------------------
-- Traversal

{- | Visit every full slot index. The control byte is re-read before each
visit, so a callback that deletes entries never sees a dead slot. If the
callback grows the table, iteration continues over the old generation.
-}
foldSlotsChecked ::
  PrimMonad m =>
  Table (PrimState m) ks k vs v -> (a -> Int# -> m a) -> a -> m a
foldSlotsChecked (Table mask ctrl _ _) f = groups 0#
 where
  groups g !acc
    | isTrue# (g ># mask) = pure acc
    | otherwise = do
        W# grp <- primitive $ \s -> case inline loadGroup# ctrl g s of (# s1, w #) -> (# s1, W# w #)
        lanes' g (inline matchFull# grp) acc
  lanes' g m !acc
    | isTrue# (eqWord# m 0##) = groups (g +# 1#) acc
    | otherwise = do
        let p = inline lanePos# g m
        live <- primitive $ \s -> case readWord8Array# ctrl (16# +# p) s of
          (# s1, b #) -> (# s1, isTrue# (neWord# (word8ToWord# b) 0##) #)
        acc' <- if live then f acc (inline slotOf# p) else pure acc
        lanes' g (and# m (minusWord# m 1##)) acc'
{-# INLINE foldSlotsChecked #-}

-- | Visit every full slot index; no user code may run in between.
foldSlotsRaw ::
  PrimMonad m =>
  Table (PrimState m) ks k vs v -> (a -> Int# -> m a) -> a -> m a
foldSlotsRaw (Table mask ctrl _ _) f = groups 0#
 where
  groups g !acc
    | isTrue# (g ># mask) = pure acc
    | otherwise = do
        W# grp <- primitive $ \s -> case inline loadGroup# ctrl g s of (# s1, w #) -> (# s1, W# w #)
        lanes' g (inline matchFull# grp) acc
  lanes' g m !acc
    | isTrue# (eqWord# m 0##) = groups (g +# 1#) acc
    | otherwise = do
        acc' <- f acc (inline slotOf# (inline lanePos# g m))
        lanes' g (and# m (minusWord# m 1##)) acc'
{-# INLINE foldSlotsRaw #-}

readKeyAt :: (PrimMonad m, Storage ks k) => MArr ks (PrimState m) k -> Int# -> m k
readKeyAt kcol i = primitive (readM kcol i)
{-# INLINE readKeyAt #-}

-- | Strict left monadic fold over entries.
foldM ::
  (PrimMonad m, Storage ks k, Storage vs v) =>
  (a -> k -> v -> m a) -> a -> Dictionary (PrimState m) ks k vs v -> m a
foldM f z d = do
  Snapshot t@(Table _ _ kcol vcol) <- readSnapshot d
  foldSlotsChecked t (\acc i -> do k <- readKeyAt kcol i; v <- readKeyAt vcol i; f acc k v) z
{-# INLINE foldM #-}

forWithKey_ ::
  (PrimMonad m, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> (k -> v -> m ()) -> m ()
forWithKey_ d f = foldM (\() k v -> f k v) () d
{-# INLINE forWithKey_ #-}

-- | All keys, in slot order.
keys ::
  (PrimMonad m, G.Vector vec k, Storage ks k) =>
  Dictionary (PrimState m) ks k vs v -> m (vec k)
keys d = do
  n <- size d
  Snapshot t@(Table _ _ ks _) <- readSnapshot d
  out <- GM.unsafeNew n
  _ <- foldSlotsRaw t (\j i -> do k <- readKeyAt ks i; GM.unsafeWrite out j k; pure (j + 1)) 0
  G.unsafeFreeze out
{-# INLINE keys #-}

-- | All values, in the same order as 'keys'.
values ::
  (PrimMonad m, G.Vector vec v, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> m (vec v)
values d = do
  n <- size d
  Snapshot t@(Table _ _ _ vs) <- readSnapshot d
  out <- GM.unsafeNew n
  _ <- foldSlotsRaw t (\j i -> do v <- readKeyAt vs i; GM.unsafeWrite out j v; pure (j + 1)) 0
  G.unsafeFreeze out
{-# INLINE values #-}

------------------------------------------------------------------------
-- Lists

-- | Later pairs win.
fromList ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  [(k, v)] -> m (Dictionary (PrimState m) ks k vs v)
fromList = fromListN 0
{-# INLINE fromList #-}

-- | 'fromList' with a size hint.
fromListN ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Int -> [(k, v)] -> m (Dictionary (PrimState m) ks k vs v)
fromListN n kvs = do
  d <- initialize n
  foldr (\(k, v) r -> insert d k v >> r) (pure ()) kvs
  pure d
{-# INLINE fromListN #-}

-- | Duplicate keys are combined with @f new old@ (as
-- @Data.HashMap.Strict.fromListWith@).
fromListWith ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  (v -> v -> v) -> [(k, v)] -> m (Dictionary (PrimState m) ks k vs v)
fromListWith f kvs = do
  d <- initialize 0
  foldr (\(k, v) r -> insertWith f d k v >> r) (pure ()) kvs
  pure d
{-# INLINE fromListWith #-}

toList ::
  (PrimMonad m, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> m [(k, v)]
toList d = do
  Snapshot t@(Table _ _ ks vs) <- readSnapshot d
  foldSlotsRaw t (\acc i -> do k <- readKeyAt ks i; v <- readKeyAt vs i; pure ((k, v) : acc)) []
{-# INLINE toList #-}

------------------------------------------------------------------------
-- Combine

-- | Left-biased union into a new table.
union ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> Dictionary (PrimState m) ks k vs v -> m (Dictionary (PrimState m) ks k vs v)
union = unionWith const
{-# INLINE union #-}

unionWith ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  (v -> v -> v) -> Dictionary (PrimState m) ks k vs v -> Dictionary (PrimState m) ks k vs v -> m (Dictionary (PrimState m) ks k vs v)
unionWith f = unionWithKey (const f)
{-# INLINE unionWith #-}

-- | Common keys get @f k left right@. Copies the larger side and inserts
-- the smaller.
unionWithKey ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  (k -> v -> v -> v) -> Dictionary (PrimState m) ks k vs v -> Dictionary (PrimState m) ks k vs v -> m (Dictionary (PrimState m) ks k vs v)
unionWithKey f a b = do
  na <- size a
  nb <- size b
  if na >= nb
    then do
      r <- clone a
      reserve r (na + nb)
      forWithKey_ b $ \k vb -> upsert r (maybe vb (\va -> f k va vb)) k
      pure r
    else do
      r <- clone b
      reserve r (na + nb)
      forWithKey_ a $ \k va -> upsert r (maybe va (\vb -> f k va vb)) k
      pure r
{-# INLINABLE unionWithKey #-}

-- | Entries of the first table whose key is not in the second.
difference ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> Dictionary (PrimState m) ks k ws w -> m (Dictionary (PrimState m) ks k vs v)
difference a b = do
  r <- initialize =<< size a
  forWithKey_ a $ \k v -> do
    present <- member b k
    if present then pure () else insert r k v
  pure r
{-# INLINABLE difference #-}

-- | For common keys @f@ decides: 'Nothing' drops, @'Just' v@ keeps @v@.
differenceWith ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v, Storage ws w) =>
  (v -> w -> Maybe v) -> Dictionary (PrimState m) ks k vs v -> Dictionary (PrimState m) ks k ws w -> m (Dictionary (PrimState m) ks k vs v)
differenceWith f a b = do
  r <- initialize =<< size a
  forWithKey_ a $ \k v -> do
    mw <- lookup b k
    case mw of
      Nothing -> insert r k v
      Just w -> maybe (pure ()) (insert r k) (f v w)
  pure r
{-# INLINABLE differenceWith #-}

-- | Entries of the first table whose key is also in the second.
intersection ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs v, Storage ws w) =>
  Dictionary (PrimState m) ks k vs v -> Dictionary (PrimState m) ks k ws w -> m (Dictionary (PrimState m) ks k vs v)
intersection = intersectionWith const
{-# INLINE intersection #-}

intersectionWith ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs a, Storage ws b, Storage vs c) =>
  (a -> b -> c) -> Dictionary (PrimState m) ks k vs a -> Dictionary (PrimState m) ks k ws b -> m (Dictionary (PrimState m) ks k vs c)
intersectionWith f = intersectionWithKey (const f)
{-# INLINE intersectionWith #-}

-- | Walks the smaller table, probes the larger. The result stores its
-- values like the left table does.
intersectionWithKey ::
  (PrimMonad m, Hashable k, Storage ks k, Storage vs a, Storage ws b, Storage vs c) =>
  (k -> a -> b -> c) -> Dictionary (PrimState m) ks k vs a -> Dictionary (PrimState m) ks k ws b -> m (Dictionary (PrimState m) ks k vs c)
intersectionWithKey f a b = do
  na <- size a
  nb <- size b
  r <- initialize (min na nb)
  if na <= nb
    then forWithKey_ a $ \k va -> do
      mb <- lookup b k
      maybe (pure ()) (insert r k . f k va) mb
    else forWithKey_ b $ \k vb -> do
      ma <- lookup a k
      maybe (pure ()) (\va -> insert r k (f k va vb)) ma
  pure r
{-# INLINABLE intersectionWithKey #-}

------------------------------------------------------------------------
-- Freezing

-- | O(1). The table must not be used afterwards.
unsafeFreeze ::
  (PrimMonad m, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> m (FrozenDictionary ks k vs v)
unsafeFreeze (Dictionary ref) = primitive $ \s -> case readMutVar# ref s of
  (# s1, Table mask ctrl kcol vcol #) -> case unsafeFreezeByteArray# ctrl s1 of
    (# s2, ctrl' #) -> case unsafeFreezeM kcol s2 of
      (# s3, keys' #) -> case unsafeFreezeM vcol s3 of
        (# s4, vcol' #) -> (# s4, FrozenDictionary mask ctrl' keys' vcol' #)
{-# INLINE unsafeFreeze #-}

-- | O(1). The frozen table must not be used afterwards.
unsafeThaw ::
  (PrimMonad m, Storage ks k, Storage vs v) =>
  FrozenDictionary ks k vs v -> m (Dictionary (PrimState m) ks k vs v)
unsafeThaw (FrozenDictionary mask ctrl kcol vcol) = primitive $ \s ->
  case unsafeThawI kcol s of
    (# s1, keys' #) -> case unsafeThawI vcol s1 of
      (# s2, vcol' #) -> case newMutVar# (Table mask (unsafeCoerce# ctrl) keys' vcol') s2 of
        (# s3, ref #) -> (# s3, Dictionary ref #)
{-# INLINE unsafeThaw #-}

-- | Copying freeze.
freeze ::
  (PrimMonad m, Storage ks k, Storage vs v) =>
  Dictionary (PrimState m) ks k vs v -> m (FrozenDictionary ks k vs v)
freeze d = clone d >>= unsafeFreeze
{-# INLINE freeze #-}

-- | Copying thaw.
thaw ::
  (PrimMonad m, Storage ks k, Storage vs v) =>
  FrozenDictionary ks k vs v -> m (Dictionary (PrimState m) ks k vs v)
thaw f = unsafeThaw f >>= clone
{-# INLINE thaw #-}

-- | Entry index of a key in a frozen table, or @-1@.
findElem :: (Hashable k, Storage ks k) => FrozenDictionary ks k vs v -> k -> Int
findElem (FrozenDictionary mask ctrl kcol _) k = I# (frozenLookupSlot# mask ctrl kcol k)
{-# INLINE findElem #-}

frozenLookup :: (Hashable k, Storage ks k, Storage vs v) => FrozenDictionary ks k vs v -> k -> Maybe v
frozenLookup (FrozenDictionary mask ctrl kcol vcol) k = case frozenLookupSlot# mask ctrl kcol k of
  i
    | isTrue# (i <# 0#) -> Nothing
    | otherwise -> case indexI vcol i of (# v #) -> Just v
{-# INLINE frozenLookup #-}

frozenMember :: (Hashable k, Storage ks k) => FrozenDictionary ks k vs v -> k -> Bool
frozenMember f k = findElem f k >= 0
{-# INLINE frozenMember #-}

frozenSize :: FrozenDictionary ks k vs v -> Int
frozenSize (FrozenDictionary _ ctrl _ _) = I# (indexIntArray# ctrl 1# -# indexIntArray# ctrl 0#)
{-# INLINE frozenSize #-}

-- | Key at an entry index from 'findElem'. Unchecked.
frozenKeyAt :: Storage ks k => FrozenDictionary ks k vs v -> Int -> k
frozenKeyAt (FrozenDictionary _ _ kcol _) (I# i) = case indexI kcol i of (# k #) -> k
{-# INLINE frozenKeyAt #-}

-- | Value at an entry index from 'findElem'. Unchecked.
frozenValueAt :: Storage vs v => FrozenDictionary ks k vs v -> Int -> v
frozenValueAt (FrozenDictionary _ _ _ vcol) (I# i) = case indexI vcol i of (# v #) -> v
{-# INLINE frozenValueAt #-}

frozenToList :: (Storage ks k, Storage vs v) => FrozenDictionary ks k vs v -> [(k, v)]
frozenToList (FrozenDictionary mask ctrl kcol vcol) = groups 0#
 where
  groups g
    | isTrue# (g ># mask) = []
    | otherwise = lanes' g (inline matchFull# (inline indexGroup# ctrl g))
  lanes' g m
    | isTrue# (eqWord# m 0##) = groups (g +# 1#)
    | otherwise =
        let i = inline slotOf# (inline lanePos# g m)
         in case indexI kcol i of
              (# k #) -> case indexI vcol i of
                (# v #) -> (k, v) : lanes' g (and# m (minusWord# m 1##))
{-# INLINABLE frozenToList #-}
