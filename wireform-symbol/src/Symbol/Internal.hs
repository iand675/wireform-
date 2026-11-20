{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK not-home #-}

{- |
Module      : Symbol.Internal
Description : The interned-symbol representation and the global table.

Everything here is re-exported, minus the implementation details, by
"Symbol". Import this module only for those details (the sweep, shard
statistics) — they may change in any release.

= Representation

A 'Symbol' is the canonical 'ByteArray#' holding its UTF-8 bytes, plus a
process-unique id. Canonical means: while a symbol for some content is
alive, every 'intern' of that content returns the very same array (and id),
so equality is pointer equality and 'hash' is a field read.

The id, not the content's SipHash, is the symbol's 'hash'. The table
itself stays keyed by SipHash, which is what defends it against chosen
content. A symbol's hash only has to be stable while it lives and distinct
from other live symbols, and an id is both, with no collisions at all.
Ids are dense, so a @HashMap@ keyed by symbols gets compact nodes: with
random 64-bit hashes, a dozen header names already collide in the low
bits and push entries into sub-nodes.

= The table

32 shards, chosen by the top bits of the hash, each an @MVar@ around a
"Hashtable" from the hash to the weak pointers of the symbols with that
hash (one, unless two contents collide in 64 bits).

An entry for content @c@ is in one of three states, and every transition
happens with its shard's lock held:

@
  Absent ──intern──▶ Live w ──GC──▶ Dead w ──┬─ sweep ──▶ Absent
                                             └─ intern ─▶ Live w'
@

The GC's move (@Live@ to @Dead@) is the one thing that happens outside the
lock, and it is harmless: a dead weak pointer only ever makes a lookup miss
for content no live symbol has, which is the correct answer. There are no
finalizers. A dead entry is noticed when a lookup reaches it and is
replaced in place. Otherwise it is reclaimed by a sweep that runs just
before its shard would grow, which keeps the cost O(1) amortised per
insert, the same accounting as the rehash it may avoid.

Without a finalizer there is no race between a late finalizer and a
re-intern of the same content. That race, plus @vector-hashtables@
dropping colliding keys when deleting through @alter@, is how a
finalizer-based interner can evict a live symbol and then mint a second
copy of it.
-}
module Symbol.Internal
  ( -- * Symbols
    Symbol (..)
  , intern
  , internIO
  , internBytes
  , lookup
  , lookupBytes
  , unintern
  , symbolLength
  , symbolByteArray

    -- * Conversions
  , Textual (..)

    -- * Table maintenance and statistics
  , symbolTableSize
  , sweepSymbolTable
  , shardCount
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVarMasked)
import Control.DeepSeq (NFData (..))
import Control.Monad (forM_, unless, when)
import Data.Array.Byte (ByteArray (..))
import Data.Binary (Binary (..), Get)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString (..))
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Unsafe as BSU
import Data.Hashable (Hashable (..))
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, smallArrayFromListN)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Data.Text.Encoding as TE
import Data.Text.Internal (Text (..))
import GHC.Word (Word64 (..))
import GHC.Exts hiding (Symbol)
import GHC.IO (IO (..), unsafeDupablePerformIO, unsafePerformIO)
import Hashtable (Strict, Unboxed)
import qualified Hashtable as H
import Symbol.SipHash
import Prelude hiding (lookup)

------------------------------------------------------------------------
-- Symbols

{- | An interned string: O(1) '==' and 'hash', lexicographic 'compare'.

The content is UTF-8. Interning any 'Textual' with the same characters
gives the same symbol; invalid UTF-8 in a 'ByteString' or
'ShortByteString' is replaced with U+FFFD first, as
'TE.decodeUtf8Lenient' does.
-}
data Symbol = Symbol ByteArray# Int#

-- | Pointer equality: only one live symbol exists per content.
instance Eq Symbol where
  Symbol a _ == Symbol b _ = isTrue# (sameByteArray# a b)
  {-# INLINE (==) #-}

-- | Byte-lexicographic order of the UTF-8, which is code-point order and
-- the order of 'Text'. O(1) for equal symbols, O(n) otherwise.
instance Ord Symbol where
  compare (Symbol a _) (Symbol b _)
    | isTrue# (sameByteArray# a b) = EQ
    | otherwise =
        let la = sizeofByteArray# a
            lb = sizeofByteArray# b
            c = compareByteArrays# a 0# b 0# (if isTrue# (la <# lb) then la else lb)
         in if isTrue# (c <# 0#)
              then LT
              else
                if isTrue# (c ># 0#)
                  then GT
                  else compare (I# la) (I# lb)
  {-# INLINE compare #-}

-- | 'hash' is the symbol's id, stored with it: a field read. Ids are
-- assigned in creation order, so they differ between runs.
instance Hashable Symbol where
  hash (Symbol _ h) = I# h
  hashWithSalt s (Symbol _ h) = hashWithSalt s (I# h)
  {-# INLINE hash #-}
  {-# INLINE hashWithSalt #-}

-- | Symbols are always fully evaluated.
instance NFData Symbol where
  rnf s = s `seq` ()

-- | Shows the content as a string literal, which reads back through
-- 'Read' (and is a valid expression under @OverloadedStrings@).
instance Show Symbol where
  showsPrec p s = showsPrec p (unintern s :: String)

instance Read Symbol where
  readsPrec p str = do
    (s, rest) <- readsPrec p str
    pure (intern (s :: String), rest)

instance IsString Symbol where
  fromString = intern
  {-# INLINE fromString #-}

-- | The content's UTF-8 bytes, as a length-prefixed 'ShortByteString'.
instance Binary Symbol where
  put = put . (unintern :: Symbol -> ShortByteString)
  get = intern <$> (get :: Get ShortByteString)

-- | The content's length in bytes (of UTF-8).
symbolLength :: Symbol -> Int
symbolLength (Symbol a _) = I# (sizeofByteArray# a)
{-# INLINE symbolLength #-}

-- | The canonical array. Zero-copy.
symbolByteArray :: Symbol -> ByteArray
symbolByteArray (Symbol a _) = ByteArray a
{-# INLINE symbolByteArray #-}

------------------------------------------------------------------------
-- Conversions

{- | String types that can be interned and recovered.

'withUtf8' lends the value's UTF-8 bytes without copying wherever the type
allows it ('Text', valid 'ShortByteString', valid 'ByteString'); only a new
symbol copies them. 'fromUtf8' wraps the canonical array without copying for
'Text' and 'ShortByteString'.
-}
class Textual a where
  withUtf8 :: a -> (Bytes -> IO r) -> IO r
  fromUtf8 :: ByteArray# -> a

instance Textual Text where
  withUtf8 (Text (TA.ByteArray ba) (I# off) (I# len)) k = k (BytesBA ba off len)
  fromUtf8 ba = Text (TA.ByteArray ba) 0 (I# (sizeofByteArray# ba))
  {-# INLINE withUtf8 #-}
  {-# INLINE fromUtf8 #-}

instance Textual ShortByteString where
  withUtf8 sbs@(SBS ba) k
    | SBS.isValidUtf8 sbs = k (BytesBA ba 0# (sizeofByteArray# ba))
    | otherwise = withUtf8 (TE.decodeUtf8Lenient (SBS.fromShort sbs)) k
  fromUtf8 = SBS
  {-# INLINE withUtf8 #-}
  {-# INLINE fromUtf8 #-}

instance Textual ByteString where
  withUtf8 bs k
    | BS.isValidUtf8 bs = BSU.unsafeUseAsCStringLen bs $ \(Ptr a, I# n) -> k (BytesAddr a n)
    | otherwise = withUtf8 (TE.decodeUtf8Lenient bs) k
  fromUtf8 ba = SBS.fromShort (SBS ba)
  {-# INLINE withUtf8 #-}
  {-# INLINE fromUtf8 #-}

instance Textual String where
  withUtf8 s = withUtf8 (T.pack s)
  fromUtf8 ba = T.unpack (fromUtf8 ba)
  {-# INLINE withUtf8 #-}
  {-# INLINE fromUtf8 #-}

-- | The symbol for this content, creating it if no live one exists.
-- Thread-safe; outwardly pure.
intern :: Textual a => a -> Symbol
intern x = unsafeDupablePerformIO (internIO x)
{-# INLINE intern #-}

-- | 'intern' in 'IO'.
internIO :: Textual a => a -> IO Symbol
internIO x = withUtf8 x internBytes
{-# INLINE internIO #-}

-- | The content back, as any 'Textual'. Zero-copy for 'Text' and
-- 'ShortByteString'.
unintern :: Textual a => Symbol -> a
unintern (Symbol a _) = fromUtf8 a
{-# INLINE unintern #-}

-- | The live symbol for this content, if there is one. Never creates one.
lookup :: Textual a => a -> IO (Maybe Symbol)
lookup x = withUtf8 x lookupBytes
{-# INLINE lookup #-}

------------------------------------------------------------------------
-- The table

-- | Weak pointers to the live (or recently dead) symbols with one hash.
-- More than one only on a 64-bit SipHash collision. Each carries its
-- symbol's id (see 'Symbol').
data Entry = Single (Weak# ByteArray#) Int# | Chain (Weak# ByteArray#) Int# !Entry

type Shard = H.Dictionary RealWorld Unboxed Int Strict Entry

-- | The SipHash key, the shards, and the next-symbol-id counter.
data Global = Global !SipKey !(SmallArray (MVar Shard)) Counter

data Counter = Counter (MutableByteArray# RealWorld)

shardBits :: Int
shardBits = 5

-- | Number of independently locked shards.
shardCount :: Int
shardCount = 2 ^ shardBits

global :: Global
global = unsafePerformIO $ do
  key <- newSipKey
  shards <- mapM (const (H.new >>= newMVar)) [1 .. shardCount]
  counter <- IO $ \s -> case newByteArray# 8# s of
    (# s1, m #) -> case writeIntArray# m 0# 0# s1 of
      s2 -> (# s2, Counter m #)
  pure (Global key (smallArrayFromListN shardCount shards) counter)
{-# NOINLINE global #-}

-- | The shard and table key for some bytes.
locate :: Bytes -> (# MVar Shard, Int #)
locate b = case global of
  Global key shards _ ->
    let !h = sipHash13 key b
     in (# indexSmallArray shards (fromIntegral (h `uncheckedShiftR64` (64 - shardBits))), fromIntegral h #)
{-# INLINE locate #-}

uncheckedShiftR64 :: Word64 -> Int -> Word64
uncheckedShiftR64 (W64# w) (I# n) = W64# (uncheckedShiftRL64# w n)
{-# INLINE uncheckedShiftR64 #-}

-- | The symbol for these bytes (valid UTF-8), creating it if needed.
internBytes :: Bytes -> IO Symbol
internBytes b = case locate b of
  (# shard, !h #) -> withMVarMasked shard $ \t -> do
    found <- H.lookup t h
    case found of
      Just e -> IO $ \s -> case liveMatch# b e s of
        (# s1, (# (# ba, i #) | #) #) -> (# s1, Symbol ba i #)
        (# s1, (# | (# #) #) #) -> case insertFresh t b h (Just e) of IO f -> f s1
      Nothing -> do
        sweepBeforeGrowth t
        insertFresh t b h Nothing
{-# INLINE internBytes #-}

-- | The live symbol for these bytes, if any.
lookupBytes :: Bytes -> IO (Maybe Symbol)
lookupBytes b = case locate b of
  (# shard, !h #) -> withMVarMasked shard $ \t -> do
    found <- H.lookup t h
    case found of
      Nothing -> pure Nothing
      Just e -> IO $ \s -> case liveMatch# b e s of
        (# s1, (# (# ba, i #) | #) #) -> (# s1, Just (Symbol ba i) #)
        (# s1, (# | (# #) #) #) -> (# s1, Nothing #)
{-# INLINE lookupBytes #-}

-- | The live array (and its id) among @e@'s weak pointers whose content
-- is @b@.
liveMatch# :: Bytes -> Entry -> State# RealWorld -> (# State# RealWorld, (# (# ByteArray#, Int# #) | (# #) #) #)
liveMatch# b = go
 where
  go (Single w i) s = check w i s
  go (Chain w i rest) s = case check w i s of
    (# s1, (# hit | #) #) -> (# s1, (# hit | #) #)
    (# s1, (# | (# #) #) #) -> go rest s1
  check :: Weak# ByteArray# -> Int# -> State# RealWorld -> (# State# RealWorld, (# (# ByteArray#, Int# #) | (# #) #) #)
  check w i s = case deRefWeak# w s of
    (# s1, alive, ba #)
      | isTrue# alive, sameBytes b ba -> (# s1, (# (# ba, i #) | #) #)
      | otherwise -> (# s1, (# | (# #) #) #)
{-# INLINE liveMatch# #-}

sameBytes :: Bytes -> ByteArray# -> Bool
sameBytes (BytesBA src off n) ba =
  isTrue# (n ==# sizeofByteArray# ba) && isTrue# (compareByteArrays# src off ba 0# n ==# 0#)
sameBytes (BytesAddr a n) ba = isTrue# (n ==# sizeofByteArray# ba) && go 0#
 where
  full = n -# remInt# n 8#
  go i
    | isTrue# (i <# full) =
        isTrue# (eqWord64# (indexWord8OffAddrAsWord64# a i) (indexWord8ArrayAsWord64# ba i)) && go (i +# 8#)
    | isTrue# (i <# n) =
        isTrue# (eqWord8# (indexWord8OffAddr# a i) (indexWord8Array# ba i)) && go (i +# 1#)
    | otherwise = True
{-# INLINE sameBytes #-}

-- | Copy the bytes into a fresh canonical array, give it the next id,
-- register it (dropping dead weak pointers from the old entry) and return
-- its symbol. Cold: only a new symbol gets here.
insertFresh :: Shard -> Bytes -> Int -> Maybe Entry -> IO Symbol
insertFresh t b h old = do
  live <- maybe (pure Nothing) pruneEntry old
  IO $ \s -> case copyBytes b s of
    (# s1, ba #) -> case mkWeakNoFinalizer# ba ba s1 of
      (# s2, w #) -> case global of
        Global _ _ (Counter c) -> case fetchAddIntArray# c 0# 1# s2 of
          (# s3, prev #) ->
            let i = prev +# 1#
                e = maybe (Single w i) (Chain w i) live
             in case H.insert t h e of
                  IO f -> case f s3 of
                    (# s4, () #) -> (# s4, Symbol ba i #)
{-# NOINLINE insertFresh #-}

copyBytes :: Bytes -> State# RealWorld -> (# State# RealWorld, ByteArray# #)
copyBytes b s0 = case b of
  BytesBA src off n -> case newByteArray# n s0 of
    (# s1, m #) -> case copyByteArray# src off m 0# n s1 of
      s2 -> unsafeFreezeByteArray# m s2
  BytesAddr a n -> case newByteArray# n s0 of
    (# s1, m #) -> case copyAddrToByteArray# a m 0# n s1 of
      s2 -> unsafeFreezeByteArray# m s2

-- | The entry without its dead weak pointers, if any are left.
pruneEntry :: Entry -> IO (Maybe Entry)
pruneEntry = \case
  Single w i -> keep w i Nothing
  Chain w i rest -> pruneEntry rest >>= keep w i
 where
  keep :: Weak# ByteArray# -> Int# -> Maybe Entry -> IO (Maybe Entry)
  keep w i rest = IO $ \s -> case deRefWeak# w s of
    (# s1, alive, _ #)
      | isTrue# alive -> (# s1, Just (maybe (Single w i) (Chain w i) rest) #)
      | otherwise -> (# s1, rest #)

isLiveEntry :: Entry -> IO Bool
isLiveEntry e = maybe False (const True) <$> pruneEntry e

{- | Called before inserting a new key: if the shard is at capacity, drop
the entries whose symbols have all died first. If more than half the
capacity is still live afterwards, double it now, so the next sweep is at
least half a capacity of inserts away (O(1) amortised).
-}
sweepBeforeGrowth :: Shard -> IO ()
sweepBeforeGrowth t = do
  n <- H.size t
  c <- H.capacity t
  when (n >= c) $ do
    sweepShard t
    n' <- H.size t
    when (2 * n' > c) $ H.reserve t (2 * c)

sweepShard :: Shard -> IO ()
sweepShard t = H.foldM (\() k e -> isLiveEntry e >>= \alive -> unless alive (H.delete t k)) () t

-- | Number of live symbols. Walks every shard.
symbolTableSize :: IO Int
symbolTableSize = case global of
  Global _ shards _ -> do
    let shardLive m = withMVarMasked m $ H.foldM (\acc _ e -> (\a -> if a then acc + 1 else acc) <$> isLiveEntry e) 0
    sum <$> mapM (\i -> shardLive (indexSmallArray shards i)) [0 .. shardCount - 1]

-- | Drop every entry whose symbols have all been collected. Never needed
-- for correctness or for bounding memory (inserts sweep as they go); useful
-- after dropping a large set of symbols, to return the table's memory.
sweepSymbolTable :: IO ()
sweepSymbolTable = case global of
  Global _ shards _ -> forM_ [0 .. shardCount - 1] $ \i ->
    withMVarMasked (indexSmallArray shards i) sweepShard
