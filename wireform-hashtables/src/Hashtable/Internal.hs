{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_HADDOCK not-home #-}

{- |
Module      : Hashtable.Internal
Description : Representation and probing core of "Hashtable".

Nothing here is checked: indices, positions and invariants are the
caller's responsibility. Import "Hashtable" unless you are extending the
table itself.

= Layout

A table with @G@ groups (a power of two) is four objects:

@
  ctrl  :: MutableByteArray#    16 + 8·G bytes
  keys  :: MArr ks s k          7·G elements
  vals  :: MArr vs s v          7·G elements
@

plus its group mask @G - 1@, all held in one immutable, /unlifted/ 'Table'
record. The 'Dictionary' handle is a @MutVar#@ pointing at the current
'Table'; growing swaps the record. So a lookup is: handle → record →
(ctrl, keys, vals) with the mask already in the record.

@ctrl@ starts with two machine words of metadata

@
  word 0   growthLeft    inserts allowed before the next rehash
  word 1   maxLoad       entry budget (lowered by drift, see below)

The live entry count is @maxLoad - growthLeft@: an insert touches one
metadata word (test @growthLeft@ for zero, decrement it).
@

followed by one 64-bit /group word/ per group:

@
  byte   0    1    2    3    4    5    6  |  7
        tag  tag  tag  tag  tag  tag  tag | overflow bits
@

A tag is 0 for an empty slot or the top 8 bits of the key's mixed hash
(remapped away from 0) for a full one. The overflow byte is an 8-bit Bloom
filter: bit @tag .&. 7@ is set whenever an insertion with that tag had to
probe /past/ this group because it was full.

This is the design of Boost's @unordered_flat_map@ squeezed into a 64-bit
SWAR word (GHC has no @pmovmskb@, so there is no 16-byte SIMD group to be
had): matching a tag against seven slots is a handful of ALU ops on one
load, a miss stops at the first group whose overflow bit for the key is
clear, and deletion just zeroes the tag — there are no tombstones.

= Positions

Probing works in /positions/ @p = 8·g + j@ (group @g@, lane @j < 7@), which
address the control byte directly (@16 + p@). The dense slot index into
@keys@ / @vals@ is @p - g = p - (p >> 3)@ — the 7-wide groups never need a
division. Slot indices are what the public API calls entry indices.

= Load and churn

@maxLoad@ starts at ⌊7·G · 7/8⌋. Deleting an element whose tag's overflow
bit is set in its group lowers @maxLoad@ by one (it may have been what
pushed other keys further out), so repeated insert/delete cycles cannot
silently lengthen probes: they bring the next rehash forward instead, and
a rehash rebuilds every overflow byte from scratch.
-}
module Hashtable.Internal
  ( -- * Representation
    Table (..)
  , Dictionary (..)
  , FrozenDictionary (..)
  , Snapshot (..)
  , readSnapshot
    -- * Sizing
  , maxLoadFor
  , groupsFor
    -- * Hashing
  , hashKey#
  , tagOf#
    -- * Group words
  , loadGroup#
  , indexGroup#
  , matchTag#
  , matchEmpty#
  , matchFull#
  , mustContinue#
  , probeStep#
  , lanePos#
  , slotOf#
    -- * Metadata
  , readSize#
  , readGrowth#
  , writeGrowth#
  , readMaxLoad#
  , writeMaxLoad#
    -- * Core operations
  , newTable#
  , lookupPos#
  , placeWalk#
  , insertAbsent#
  , growAndPlace#
  , deleteAtPos#
  , rehash#
  , cloneTable#
  , frozenLookupSlot#
    -- * Prefetch
  , prefetchHome#
  , prefetchSlot#
  ) where

import Control.Monad.Primitive (PrimMonad (..))
import Data.Bits (countLeadingZeros, finiteBitSize, unsafeShiftL)
import Data.Hashable (Hashable (hash))
import Data.Kind (Type)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Exts
import Hashtable.Storage

------------------------------------------------------------------------
-- Representation

-- | One generation of a table. Immutable record; its arrays are mutable.
type Table :: Type -> Type -> Type -> Type -> Type -> UnliftedType
data Table s ks k vs v
  = Table
      Int#
      -- ^ group mask (@G - 1@)
      (MutableByteArray# s)
      -- ^ metadata words followed by the group words
      (MArr ks s k)
      -- ^ keys, @7·G@ slots
      (MArr vs s v)
      -- ^ values, @7·G@ slots

{- | A mutable hash table in state thread @s@ with keys of type @k@ stored
under storage tag @ks@ and values of type @v@ stored under @vs@.

See "Hashtable.Storage" for the tags. Not thread-safe.
-}
data Dictionary s ks k vs v = Dictionary (MutVar# s (Table s ks k vs v))

-- | An immutable table, produced by 'Hashtable.unsafeFreeze' /
-- 'Hashtable.freeze'.
data FrozenDictionary ks k vs v
  = FrozenDictionary
      Int#
      ByteArray#
      (IArr ks k)
      (IArr vs v)

-- | A lifted box around the current 'Table', for code that has to pass a
-- table through a monadic bind. GHC removes the box once inlined.
data Snapshot s ks k vs v = Snapshot (Table s ks k vs v)

readSnapshot :: PrimMonad m => Dictionary (PrimState m) ks k vs v -> m (Snapshot (PrimState m) ks k vs v)
readSnapshot (Dictionary ref) = primitive $ \s -> case readMutVar# ref s of
  (# s1, t #) -> (# s1, Snapshot t #)
{-# INLINE readSnapshot #-}

------------------------------------------------------------------------
-- Sizing

-- | Entries allowed in a table of @g@ groups: 7/8 of its @7·g@ slots.
maxLoadFor :: Int -> Int
maxLoadFor g = (g * 49) `quot` 8
{-# INLINE maxLoadFor #-}

-- | The smallest power-of-two group count holding @n@ entries.
groupsFor :: Int -> Int
groupsFor n = go start
 where
  need = max 1 ((8 * n + 48) `quot` 49)
  start
    | need <= 1 = 1
    | otherwise = 1 `unsafeShiftL` (finiteBitSize need - countLeadingZeros (need - 1))
  go g
    | g > maxGroups = errorWithoutStackTrace "Hashtable: table too large (more than 2^31 groups)"
    | maxLoadFor g >= n = g
    | otherwise = go (g * 2)

tableTooLarge :: ()
tableTooLarge = errorWithoutStackTrace "Hashtable: table too large (more than 2^31 groups)"
{-# NOINLINE tableTooLarge #-}

-- | Probe loops pack the group index into 32 bits (see $probestate), so a
-- table is capped at 2^31 groups — about 15 billion slots.
maxGroups :: Int
maxGroups = 2 ^ (31 :: Int)

------------------------------------------------------------------------
-- Hashing

{- | Finish the user's 'hash' into the word the table uses: low bits pick
the home group, the top byte is the tag.

@hashable@ hashes 'Int' as the identity, so the word has to be mixed —
but a full mix throws away the one useful property of dense integer keys,
that neighbours are looked up together. So the mix is split:

@
  m = fold (h >> 8)              -- folded 64x64->128 multiply of the high bits
  w = (h + m) `xor` (h << 56)
@

Keys in the same 256-aligned block share @m@, so their home groups are
consecutive (@h + m@) and a dense ID range turns into sequential memory
traffic the hardware prefetcher can follow. Across blocks, @m@ is a full
mix, so placement is as random as hashing everything. Two keys of one block
cannot share a group once the table has 256 groups (their low bits differ);
below that, keys strided by exactly the group count can share one, at most
@256 / G@ deep. The tag gets the key's low byte xor'd in, so neighbours in
a block also get distinct tags (and overflow bits).
-}
hashKey# :: Hashable k => k -> Word#
hashKey# k = case hash k of
  I# h' ->
    let h = int2Word# h'
     in case timesWord2# (uncheckedShiftRL# h 8#) 0x9E3779B97F4A7C15## of
          (# hi, lo #) -> xor# (plusWord# h (xor# hi lo)) (uncheckedShiftL# h 56#)
{-# INLINE hashKey# #-}

{- | Top byte of the mixed hash, remapped into @[2, 255]@. Empty is 0, and
keeping tags away from 1 as well is what makes the cheap SWAR zero-byte
test exact on empty lanes (see 'matchTag#').
-}
tagOf# :: Word# -> Word#
tagOf# w =
  let t = uncheckedShiftRL# w 56#
   in plusWord# t (uncheckedShiftL# (uncheckedShiftRL# (minusWord# t 2##) 63#) 1#)
{-# INLINE tagOf# #-}

------------------------------------------------------------------------
-- Group words

pattern CtrlBase :: Int#
pattern CtrlBase = 16#

-- | Bytes 0..6 repeated: broadcast multiplier for a tag.
pattern Lanes :: Word#
pattern Lanes = 0x0001010101010101##

-- | High bit of bytes 0..6.
pattern LaneHighs :: Word#
pattern LaneHighs = 0x0080808080808080##

fixEndian :: Word# -> Word#
fixEndian w = case targetByteOrder of
  LittleEndian -> w
  BigEndian -> byteSwap# w
{-# INLINE fixEndian #-}

loadGroup# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
loadGroup# ctrl g s = case readWord8ArrayAsWord64# ctrl (CtrlBase +# uncheckedIShiftL# g 3#) s of
  (# s1, w #) -> (# s1, fixEndian (word64ToWord# w) #)
{-# INLINE loadGroup# #-}

indexGroup# :: ByteArray# -> Int# -> Word#
indexGroup# ctrl g = fixEndian (word64ToWord# (indexWord8ArrayAsWord64# ctrl (CtrlBase +# uncheckedIShiftL# g 3#)))
{-# INLINE indexGroup# #-}

{- | Lanes (0..6) whose tag equals the broadcast pattern @tag * Lanes@.

This is the classic @(x - 0x01..) & ~x & 0x80..@ zero-byte test. Its only
false positives come from a borrow out of a true zero lane, and land on a
lane whose byte is 1 after the xor — i.e. a /full/ slot holding @tag ^ 1@.
An empty lane is 0 and can never xor to 0 or 1 against a tag in @[2,255]@,
so no match ever lands on an empty slot (whose 'Unboxed' key is garbage
that could compare equal). False positives on full slots just cost a key
comparison.
-}
matchTag# :: Word# -> Word# -> Word#
matchTag# grp pat =
  let x = xor# grp pat
   in and# (and# (minusWord# x Lanes) (not# x)) LaneHighs
{-# INLINE matchTag# #-}

-- | Empty lanes. Exact: a borrow can only reach a lane holding 1, and no
-- lane does.
matchEmpty# :: Word# -> Word#
matchEmpty# grp = and# (and# (minusWord# grp Lanes) (not# grp)) LaneHighs
{-# INLINE matchEmpty# #-}

matchFull# :: Word# -> Word#
matchFull# grp = xor# (inline matchEmpty# grp) LaneHighs
{-# INLINE matchFull# #-}

{- | Whether a probe for a key with this tag must go on past a group: the
key's overflow bit is set in the group's byte 7. Probes only ask after a
group's candidate lanes are exhausted, and re-read the group word from
@ctrl@ to do it (an L1 hit: it was loaded a moment ago), so neither the
group word nor the overflow bit occupies a register on the hit path.
-}
mustContinue# :: Word# -> Word# -> Bool
mustContinue# grp tag = isTrue# (neWord# (and# grp (inline overflowBit# tag)) 0##)
{-# INLINE mustContinue# #-}

{- $probestate
Probe loops carry the group index and the step count in one word,
group in the low 32 bits and steps taken in the high 32 (tables are far below
2^32 groups): one register instead of two, and the group comes back out
with a zero-extending 32-bit move.
-}

{- | Probe step between groups: double hashing with an odd stride from the
tag, @2·tag + 1@. Odd, so it cycles through every group of a power-of-two
table. Not triangular @+1, +2, ...@: 'hashKey#' gives neighbouring keys
neighbouring home groups, and a first step of @+1@ would land an overflow on
the neighbour's home, cascading at high load and saturating the overflow
bytes along the run. Neighbours have different tags, so their overflows
scatter.
-}
probeStep# :: Word# -> Int#
probeStep# tag = word2Int# (or# (uncheckedShiftL# tag 1#) 1##)
{-# INLINE probeStep# #-}

lowHalf# :: Int# -> Int#
lowHalf# gs = word2Int# (word32ToWord# (wordToWord32# (int2Word# gs)))
{-# INLINE lowHalf# #-}

-- | Position of the lowest lane in a nonzero match mask. in a nonzero match mask.
lanePos# :: Int# -> Word# -> Int#
lanePos# g m = uncheckedIShiftL# g 3# +# word2Int# (uncheckedShiftRL# (ctz# m) 3#)
{-# INLINE lanePos# #-}

-- | Dense slot index of a position.
slotOf# :: Int# -> Int#
slotOf# p = p -# uncheckedIShiftRA# p 3#
{-# INLINE slotOf# #-}

-- | Overflow bit for a tag, already shifted into byte 7 of a group word.
overflowBit# :: Word# -> Word#
overflowBit# tag = uncheckedShiftL# 1## (word2Int# (and# tag 7##) +# 56#)
{-# INLINE overflowBit# #-}

------------------------------------------------------------------------
-- Metadata

readGrowth#, readMaxLoad# :: MutableByteArray# s -> State# s -> (# State# s, Int# #)
readGrowth# ctrl = readIntArray# ctrl 0#
readMaxLoad# ctrl = readIntArray# ctrl 1#
{-# INLINE readGrowth# #-}
{-# INLINE readMaxLoad# #-}

writeGrowth#, writeMaxLoad# :: MutableByteArray# s -> Int# -> State# s -> State# s
writeGrowth# ctrl = writeIntArray# ctrl 0#
writeMaxLoad# ctrl = writeIntArray# ctrl 1#
{-# INLINE writeGrowth# #-}
{-# INLINE writeMaxLoad# #-}

-- | Live entries: @maxLoad - growthLeft@.
readSize# :: MutableByteArray# s -> State# s -> (# State# s, Int# #)
readSize# ctrl s0 = case readGrowth# ctrl s0 of
  (# s1, gl #) -> case readMaxLoad# ctrl s1 of
    (# s2, ml #) -> (# s2, ml -# gl #)
{-# INLINE readSize# #-}

------------------------------------------------------------------------
-- Core operations

-- | A fresh, empty table of @g@ groups.
newTable# :: forall s ks k vs v. (Storage ks k, Storage vs v) => Int# -> State# s -> (# State# s, Table s ks k vs v #)
newTable# g s0 =
  let bytes = uncheckedIShiftL# g 3#
      slots = g *# 7#
   in case newByteArray# (CtrlBase +# bytes) s0 of
        (# s1, ctrl #) -> case setByteArray# ctrl CtrlBase bytes 0# s1 of
          s2 -> case maxLoadFor (I# g) of
            I# ml -> case writeGrowth# ctrl ml s2 of
              s3 -> case writeMaxLoad# ctrl ml s3 of
                s4 -> case newM slots s4 of
                  (# s5, ks #) -> case newM slots s5 of
                    (# s6, vs #) -> (# s6, Table (g -# 1#) ctrl ks vs #)
{-# INLINABLE newTable# #-}

{- | Position of @k@, or @-1#@ if absent. The hash is passed in so callers
that go on to insert compute it once.
-}
lookupPos# ::
  (Eq k, Storage ks k) =>
  Int# -> MutableByteArray# s -> MArr ks s k -> k -> Word# -> State# s -> (# State# s, Int# #)
lookupPos# mask ctrl keys k w s0 = group (word2Int# w `andI#` mask) s0
 where
  pat = timesWord# (inline tagOf# w) Lanes
  group gs s = case inline loadGroup# ctrl (inline lowHalf# gs) s of
    (# s1, grp #) -> scan gs (inline matchTag# grp pat) s1
  scan gs m s
    | isTrue# (eqWord# m 0##) = case inline loadGroup# ctrl (inline lowHalf# gs) s of
        (# s1, grp #)
          | inline mustContinue# grp (and# pat 0xFF##) ->
              let steps = uncheckedIShiftRL# gs 32#
               in if isTrue# (steps ==# mask)
                    then (# s1, -1# #)
                    else
                      let g' = (inline lowHalf# gs +# inline probeStep# (and# pat 0xFF##)) `andI#` mask
                       in group (g' `orI#` uncheckedIShiftL# (steps +# 1#) 32#) s1
          | otherwise -> (# s1, -1# #)
    | otherwise =
        let g = inline lowHalf# gs
            p = uncheckedIShiftL# g 3# +# word2Int# (uncheckedShiftRL# (ctz# m) 3#)
         in case readM keys (p -# g) s of
              (# s1, k' #)
                | k' == k -> (# s1, p #)
                | otherwise -> scan gs (and# m (minusWord# m 1##)) s1
{-# INLINE lookupPos# #-}

{- | Find the insertion position for a key known to be absent, setting the
overflow bit of every full group walked past. Terminates because a table
never exceeds 7/8 load.
-}
placeWalk# :: Int# -> MutableByteArray# s -> Word# -> State# s -> (# State# s, Int# #)
placeWalk# mask ctrl w s0 = go (word2Int# w `andI#` mask) s0
 where
  tag = inline tagOf# w
  step = inline probeStep# tag
  obByte = uncheckedShiftRL# (inline overflowBit# tag) 56#
  go g s = case inline loadGroup# ctrl g s of
    (# s1, grp #) ->
      let e = inline matchEmpty# grp
       in if isTrue# (neWord# e 0##)
            then (# s1, inline lanePos# g e #)
            else
              let off = CtrlBase +# uncheckedIShiftL# g 3# +# 7#
                  ofl = or# (uncheckedShiftRL# grp 56#) obByte
               in case writeWord8Array# ctrl off (wordToWord8# ofl) s1 of
                    s2 -> go ((g +# step) `andI#` mask) s2
{-# INLINE placeWalk# #-}

-- | Write a full slot. Value first: for 'Strict' storage forcing it may
-- throw, and nothing else must have changed if it does.
fillSlot# ::
  (Storage ks k, Storage vs v) =>
  MutableByteArray# s -> MArr ks s k -> MArr vs s v -> Int# -> Word# -> k -> v -> State# s -> State# s
fillSlot# ctrl keys vals p tag k v s0 =
  let i = inline slotOf# p
   in case writeM vals i v s0 of
        s1 -> case writeM keys i k s1 of
          s2 -> writeWord8Array# ctrl (CtrlBase +# p) (wordToWord8# tag) s2
{-# INLINE fillSlot# #-}

{- | Insert a key known to be absent ('lookupPos#' said @-1#@). The
no-growth path is inline; growth is a call. 'placeWalk#' starts at the home
group, which the failed lookup just loaded, so it is an L1 hit.
-}
insertAbsent# ::
  (Hashable k, Storage ks k, Storage vs v) =>
  MutVar# s (Table s ks k vs v) -> Table s ks k vs v -> Word# -> k -> v -> State# s -> State# s
insertAbsent# ref t@(Table mask ctrl keys vals) w k v s0 =
  case readGrowth# ctrl s0 of
    (# s1, gl #)
      | isTrue# (gl ==# 0#) -> growAndPlace# ref t w k v s1
      | otherwise -> case placeWalk# mask ctrl w s1 of
          (# s2, p #) -> case fillSlot# ctrl keys vals p (inline tagOf# w) k v s2 of
            s3 -> writeGrowth# ctrl (gl -# 1#) s3
{-# INLINE insertAbsent# #-}

-- | Rehash (growing unless the load budget was eaten by churn), install
-- the new table, then place the key.
growAndPlace# ::
  (Hashable k, Storage ks k, Storage vs v) =>
  MutVar# s (Table s ks k vs v) -> Table s ks k vs v -> Word# -> k -> v -> State# s -> State# s
growAndPlace# ref t@(Table mask ctrl _ _) w k v s0 =
  case readSize# ctrl s0 of
    (# s1, n #) ->
      let g = mask +# 1#
          g' = case maxLoadFor (I# g) of
            I# full
              | isTrue# (((n +# 1#) *# 2#) <=# full) -> g
              | isTrue# (g >=# (case maxGroups of I# mg -> mg)) -> case tableTooLarge of () -> g
              | otherwise -> g *# 2#
       in case rehash# t g' s1 of
            (# s2, t'@(Table mask' ctrl' keys' vals') #) -> case writeMutVar# ref t' s2 of
              s3 -> case placeWalk# mask' ctrl' w s3 of
                (# s4, p #) -> case fillSlot# ctrl' keys' vals' p (inline tagOf# w) k v s4 of
                  s5 -> case readGrowth# ctrl' s5 of
                    (# s6, gl #) -> writeGrowth# ctrl' (gl -# 1#) s6
{-# INLINABLE growAndPlace# #-}

{- | Rebuild into a fresh table of @g@ groups. Every overflow byte is
recomputed, so this also undoes any drift from deletions. Keys are known
distinct, so no equality tests are made.
-}
rehash# ::
  forall s ks k vs v.
  (Hashable k, Storage ks k, Storage vs v) =>
  Table s ks k vs v -> Int# -> State# s -> (# State# s, Table s ks k vs v #)
rehash# (Table mask ctrl keys vals) g' s0 =
  case newTable# g' s0 of
    (# s1, t'@(Table mask' ctrl' keys' vals') #) ->
      let groups g s
            | isTrue# (g ># mask) = s
            | otherwise = case inline loadGroup# ctrl g s of
                (# s2, grp #) -> laneLoop g (inline matchFull# grp) s2
          laneLoop g m s
            | isTrue# (eqWord# m 0##) = groups (g +# 1#) s
            | otherwise =
                let i = inline slotOf# (inline lanePos# g m)
                 in case readM keys i s of
                      (# s2, k #) -> case readM vals i s2 of
                        (# s3, v #) ->
                          let w = inline hashKey# k
                           in case placeWalk# mask' ctrl' w s3 of
                                (# s4, p #) -> case fillSlot# ctrl' keys' vals' p (inline tagOf# w) k v s4 of
                                  s5 -> laneLoop g (and# m (minusWord# m 1##)) s5
       in case groups 0# s1 of
            s2 -> case readSize# ctrl s2 of
              (# s3, n #) -> case readMaxLoad# ctrl' s3 of
                (# s4, ml' #) -> case writeGrowth# ctrl' (ml' -# n) s4 of
                  s5 -> (# s5, t' #)
{-# INLINABLE rehash# #-}

-- | Remove the entry at a position.
deleteAtPos# ::
  (Storage ks k, Storage vs v) =>
  Table s ks k vs v -> Int# -> State# s -> State# s
deleteAtPos# (Table _ ctrl keys vals) p s0 =
  let g = uncheckedIShiftRA# p 3#
   in case inline loadGroup# ctrl g s0 of
        (# s1, grp #) ->
          let tag = uncheckedShiftRL# grp (uncheckedIShiftL# (p -# uncheckedIShiftL# g 3#) 3#) `and#` 0xFF##
              overflowed = isTrue# (neWord# (and# grp (inline overflowBit# tag)) 0##)
              i = inline slotOf# p
           in case writeWord8Array# ctrl (CtrlBase +# p) (wordToWord8# 0##) s1 of
                s2 -> case (if overflowed
                              then case readMaxLoad# ctrl s2 of (# s3, ml #) -> writeMaxLoad# ctrl (ml -# 1#) s3
                              else case readGrowth# ctrl s2 of (# s3, gl #) -> writeGrowth# ctrl (gl +# 1#) s3) of
                      s5 -> case clearM keys i s5 of
                        s6 -> clearM vals i s6
{-# INLINE deleteAtPos# #-}

-- | Deep copy of a table's arrays.
cloneTable# ::
  (Storage ks k, Storage vs v) =>
  Table s ks k vs v -> State# s -> (# State# s, Table s ks k vs v #)
cloneTable# (Table mask ctrl keys vals) s0 =
  let g = mask +# 1#
      bytes = CtrlBase +# uncheckedIShiftL# g 3#
      slots = g *# 7#
   in case newByteArray# bytes s0 of
        (# s1, ctrl' #) -> case copyMutableByteArray# ctrl 0# ctrl' 0# bytes s1 of
          s2 -> case cloneM keys slots s2 of
            (# s3, keys' #) -> case cloneM vals slots s3 of
              (# s4, vals' #) -> (# s4, Table mask ctrl' keys' vals' #)
{-# INLINABLE cloneTable# #-}

-- | Slot index of @k@ in a frozen table, or @-1#@.
frozenLookupSlot# ::
  (Hashable k, Storage ks k) =>
  Int# -> ByteArray# -> IArr ks k -> k -> Int#
frozenLookupSlot# mask ctrl keys k = case inline hashKey# k of
  w ->
    let pat = timesWord# (inline tagOf# w) Lanes
        groupAt gs = inline indexGroup# ctrl (inline lowHalf# gs)
        group gs = scan gs (inline matchTag# (groupAt gs) pat)
        scan gs m
          | isTrue# (eqWord# m 0##) =
              if inline mustContinue# (groupAt gs) (and# pat 0xFF##)
                then
                  let steps = uncheckedIShiftRL# gs 32#
                   in if isTrue# (steps ==# mask)
                        then -1#
                        else
                          let g' = (inline lowHalf# gs +# inline probeStep# (and# pat 0xFF##)) `andI#` mask
                           in group (g' `orI#` uncheckedIShiftL# (steps +# 1#) 32#)
                else -1#
          | otherwise =
              let g = inline lowHalf# gs
                  i = uncheckedIShiftL# g 3# +# word2Int# (uncheckedShiftRL# (ctz# m) 3#) -# g
               in case indexI keys i of
                    (# k' #)
                      | k' == k -> i
                      | otherwise -> scan gs (and# m (minusWord# m 1##))
     in group (word2Int# w `andI#` mask)
{-# INLINE frozenLookupSlot# #-}

------------------------------------------------------------------------
-- Prefetch

-- | Stage A of a batch pipeline: prefetch the key's home group word.
prefetchHome# :: Int# -> MutableByteArray# s -> Word# -> State# s -> State# s
prefetchHome# mask ctrl w =
  prefetchMutableByteArray3# ctrl (CtrlBase +# uncheckedIShiftL# (word2Int# w `andI#` mask) 3#)
{-# INLINE prefetchHome# #-}

{- | Stage B: with the home group word (hopefully) in cache, prefetch the
key and value lines the probe will actually touch: the first tag match,
or, when @forInsert@, the first empty lane (where an insert will write).
A lookup that will miss prefetches nothing — a miss only needs @ctrl@, and
dragging dead key/value lines in would just burn memory bandwidth.
-}
prefetchSlot# ::
  (Storage ks k, Storage vs v) =>
  Bool -> Table s ks k vs v -> Word# -> State# s -> State# s
prefetchSlot# forInsert (Table mask ctrl keys vals) w s0 =
  let g = word2Int# w `andI#` mask
   in case inline loadGroup# ctrl g s0 of
        (# s1, grp #) ->
          let m = inline matchTag# grp (timesWord# (inline tagOf# w) Lanes)
              m'
                | isTrue# (neWord# m 0##) = m
                | forInsert = inline matchEmpty# grp
                | otherwise = 0##
           in if isTrue# (eqWord# m' 0##)
                then s1
                else
                  let i = g *# 7# +# word2Int# (uncheckedShiftRL# (ctz# m') 3#)
                   in case prefetchM keys i s1 of
                        s2 -> prefetchM vals i s2
{-# INLINE prefetchSlot# #-}
