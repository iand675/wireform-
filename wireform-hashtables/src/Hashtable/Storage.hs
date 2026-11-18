{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

{- |
Module      : Hashtable.Storage
Description : Per-column element storage for "Hashtable".

A table keeps its keys and its values in two separate flat arrays. How each
array stores its elements is chosen independently by a /storage tag/, the
same role the @ks@ / @vs@ vector-type parameters play in
@vector-hashtables@:

[@'Unboxed'@] elements live unboxed in a @MutableByteArray#@ through
  'Prim'. No pointers, so the GC never scans the column. Use this for
  @Int@, @Word@, @Double@, @Char@, … — it is the fast path.

[@'Strict'@] elements are pointers in a @MutableArray#@, forced to WHNF
  before they are stored. The right default for boxed values: an
  @upsert (fmap (+1))@ counter can never build a thunk chain.

[@'Boxed'@] elements are lazy pointers in a @MutableArray#@. Only for when
  you really want to store unevaluated values.

Unlike @vector@'s @MVector@, the arrays here are /unlifted/ (they are the raw
@MutableArray#@ / @MutableByteArray#@ themselves, wrapped in unlifted
newtypes), so a table's record points straight at the array payload: no
intermediate @MVector@ box, no offset field, no length field.
-}
module Hashtable.Storage
  ( Storage (..)
  , Boxed
  , Strict
  , Unboxed
  ) where

import Data.Primitive.Types (Prim (..))
import GHC.Exts
import qualified GHC.Exts as Exts

-- | Lazy boxed elements.
data Boxed

-- | Boxed elements, forced to weak head normal form on every write.
data Strict

-- | Unboxed elements, stored through 'Prim'.
data Unboxed

{- | How a column stores elements of type @a@ under tag @r@.

All methods work at the @State#@ level with unboxed indices so the
table core never touches a boxed 'Int'. Every method is @INLINE@: once the
tag and element type are known at a call site the class disappears.

Indices are never bounds-checked; the table core guarantees them.
-}
class Storage r a where
  -- | Mutable column.
  data MArr r s a :: UnliftedType

  -- | Immutable (frozen) column.
  data IArr r a :: UnliftedType

  -- | @newM n@ allocates a column of @n@ elements. The table never reads a
  -- slot its control word says is empty, but 'Unboxed' columns are still
  -- zero-filled: inserts write to random slots, and a store to a cold line
  -- stalls on a read-for-ownership miss the prefetcher cannot predict. One
  -- sequential fill up front measured 1.5× faster inserts at 1k entries.
  newM :: Int# -> State# s -> (# State# s, MArr r s a #)

  readM :: MArr r s a -> Int# -> State# s -> (# State# s, a #)

  -- | Store an element. For 'Strict' this forces the element first, and the
  -- table relies on that happening /before/ the write, so an exception
  -- thrown while forcing leaves the column untouched.
  writeM :: MArr r s a -> Int# -> a -> State# s -> State# s

  -- | Drop whatever the slot refers to so the GC can reclaim it. A no-op
  -- for 'Unboxed'.
  clearM :: MArr r s a -> Int# -> State# s -> State# s

  -- | @cloneM arr n@ copies the first @n@ elements into a fresh column.
  cloneM :: MArr r s a -> Int# -> State# s -> (# State# s, MArr r s a #)

  unsafeFreezeM :: MArr r s a -> State# s -> (# State# s, IArr r a #)

  unsafeThawI :: IArr r a -> State# s -> (# State# s, MArr r s a #)

  -- | Index a frozen column. The unboxed 1-tuple lets the caller force the
  -- read without forcing the element (no thunk around the array access).
  indexI :: IArr r a -> Int# -> (# a #)

-- | Filler for unoccupied boxed slots. Never read through the table API.
deadSlot :: a
deadSlot = errorWithoutStackTrace "Hashtable: read of an unoccupied slot"
{-# NOINLINE deadSlot #-}

instance Storage Boxed a where
  newtype MArr Boxed s a = MBoxed (MutableArray# s a)
  newtype IArr Boxed a = IBoxed (Array# a)
  newM n s = case newArray# n deadSlot s of (# s1, a #) -> (# s1, MBoxed a #)
  readM (MBoxed a) = readArray# a
  writeM (MBoxed a) = writeArray# a
  clearM (MBoxed a) i = writeArray# a i deadSlot
  cloneM (MBoxed a) n s = case cloneMutableArray# a 0# n s of (# s1, b #) -> (# s1, MBoxed b #)
  unsafeFreezeM (MBoxed a) s = case unsafeFreezeArray# a s of (# s1, b #) -> (# s1, IBoxed b #)
  unsafeThawI (IBoxed a) s = case unsafeThawArray# a s of (# s1, b #) -> (# s1, MBoxed b #)
  indexI (IBoxed a) = indexArray# a
  {-# INLINE newM #-}
  {-# INLINE readM #-}
  {-# INLINE writeM #-}
  {-# INLINE clearM #-}
  {-# INLINE cloneM #-}
  {-# INLINE unsafeFreezeM #-}
  {-# INLINE unsafeThawI #-}
  {-# INLINE indexI #-}

instance Storage Strict a where
  newtype MArr Strict s a = MStrict (MutableArray# s a)
  newtype IArr Strict a = IStrict (Array# a)
  newM n s = case newArray# n deadSlot s of (# s1, a #) -> (# s1, MStrict a #)
  readM (MStrict a) = readArray# a
  writeM (MStrict a) i !x = writeArray# a i x
  clearM (MStrict a) i = writeArray# a i deadSlot
  cloneM (MStrict a) n s = case cloneMutableArray# a 0# n s of (# s1, b #) -> (# s1, MStrict b #)
  unsafeFreezeM (MStrict a) s = case unsafeFreezeArray# a s of (# s1, b #) -> (# s1, IStrict b #)
  unsafeThawI (IStrict a) s = case unsafeThawArray# a s of (# s1, b #) -> (# s1, MStrict b #)
  indexI (IStrict a) = indexArray# a
  {-# INLINE newM #-}
  {-# INLINE readM #-}
  {-# INLINE writeM #-}
  {-# INLINE clearM #-}
  {-# INLINE cloneM #-}
  {-# INLINE unsafeFreezeM #-}
  {-# INLINE unsafeThawI #-}
  {-# INLINE indexI #-}

instance Prim a => Storage Unboxed a where
  newtype MArr Unboxed s a = MUnboxed (MutableByteArray# s)
  newtype IArr Unboxed a = IUnboxed ByteArray#
  newM n s =
    let bytes = n *# sizeOf# (undefined :: a)
     in case newByteArray# bytes s of
          (# s1, a #) -> case Exts.setByteArray# a 0# bytes 0# s1 of
            s2 -> (# s2, MUnboxed a #)
  readM (MUnboxed a) = readByteArray# a
  writeM (MUnboxed a) = writeByteArray# a
  clearM _ _ s = s
  cloneM (MUnboxed a) n s =
    let bytes = n *# sizeOf# (undefined :: a)
     in case newByteArray# bytes s of
          (# s1, b #) -> case copyMutableByteArray# a 0# b 0# bytes s1 of
            s2 -> (# s2, MUnboxed b #)
  unsafeFreezeM (MUnboxed a) s = case unsafeFreezeByteArray# a s of (# s1, b #) -> (# s1, IUnboxed b #)
  unsafeThawI (IUnboxed a) s = (# s, MUnboxed (unsafeCoerce# a) #)
  indexI (IUnboxed a) i = (# indexByteArray# a i #)
  {-# INLINE newM #-}
  {-# INLINE readM #-}
  {-# INLINE writeM #-}
  {-# INLINE clearM #-}
  {-# INLINE cloneM #-}
  {-# INLINE unsafeFreezeM #-}
  {-# INLINE unsafeThawI #-}
  {-# INLINE indexI #-}
