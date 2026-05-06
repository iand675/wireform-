{-# LANGUAGE BangPatterns #-}

-- | Allocate-once helpers for the very common
--
-- @
--   VP.fromList [ f x | Just x <- V.toList v ]
-- @
--
-- pattern that shows up everywhere in the Arrow ↔ Parquet ↔ ORC
-- bridges (lower a nullable boxed column to its dense /present/
-- values).
--
-- The naive version is O(N) but with a quadruple-ish constant
-- factor: build a cons-cell list of length N, walk it again to
-- filter, walk again to fromList, then GC three intermediate
-- structures.  For 1 M-row columns with 50 % nulls the naive
-- form was the dominant allocation in our writer benchmarks.
--
-- These helpers do one upper-bound 'unsafeNew' (size = total
-- length, since at worst every slot is present), one walk, one
-- 'unsafeFreeze' + 'slice'.  The slice is a no-op if the column
-- is null-free.
module Columnar.NullableBuild
  ( presentValuesP
  , presentValuesPMap
  , presentValuesV
  , presentValuesVMap
  , presentCount
  ) where

import           Data.Vector             (Vector)
import qualified Data.Vector             as V
import qualified Data.Vector.Generic     as VG
import qualified Data.Vector.Generic.Mutable as VGM
import           Data.Vector.Primitive   (Prim)
import qualified Data.Vector.Primitive   as VP

-- | Drop the nulls from a boxed @'Vector' ('Maybe' a)@, returning
-- a dense unboxed primitive vector of just the present values.
{-# INLINE presentValuesP #-}
presentValuesP :: Prim a => Vector (Maybe a) -> VP.Vector a
presentValuesP = presentValuesPMap id

-- | Same as 'presentValuesP' but applies @f@ to each present
-- value (e.g. @fromIntegral@ widening Int8 → Int32).
{-# INLINE presentValuesPMap #-}
presentValuesPMap
  :: (Prim b) => (a -> b) -> Vector (Maybe a) -> VP.Vector b
presentValuesPMap = presentValuesG
{-# SPECIALIZE presentValuesPMap :: Prim b => (Int  -> b) -> Vector (Maybe Int)  -> VP.Vector b #-}

-- | Boxed equivalent (used for @ByteString@ / @Text@ /
-- @Bool@ values that live in a boxed vector).
{-# INLINE presentValuesV #-}
presentValuesV :: Vector (Maybe a) -> Vector a
presentValuesV = presentValuesVMap id

{-# INLINE presentValuesVMap #-}
presentValuesVMap :: (a -> b) -> Vector (Maybe a) -> Vector b
presentValuesVMap = presentValuesG

-- | Generic single-pass present-values builder. Allocates one
-- mutable buffer of size @V.length v@ (worst case), writes
-- /present/ slots, then returns a slice of length /presentCount/.
{-# INLINE presentValuesG #-}
presentValuesG
  :: VG.Vector w b
  => (a -> b)
  -> Vector (Maybe a)
  -> w b
presentValuesG f v = VG.create $ do
  let !n = V.length v
  buf <- VGM.unsafeNew n
  let go !i !w
        | i >= n    = pure w
        | otherwise = case V.unsafeIndex v i of
            Nothing -> go (i + 1) w
            Just x  -> do
              VGM.unsafeWrite buf w (f x)
              go (i + 1) (w + 1)
  !nWritten <- go 0 0
  pure (VGM.unsafeSlice 0 nWritten buf)

-- | Number of @Just@ slots in a boxed @Vector (Maybe a)@.
{-# INLINE presentCount #-}
presentCount :: Vector (Maybe a) -> Int
presentCount = V.foldl' step 0
  where
    step !acc Nothing  = acc
    step !acc (Just _) = acc + 1
