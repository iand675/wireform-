{-# LANGUAGE BangPatterns #-}

{- | Pure growing accumulator for repeated fields.

__Stability:__ exposed for use by wireform-proto-generated code; not
part of the stable public API.

Stores elements in a reversed cons list with a count.  Each 'snocGrowList'
allocates exactly one cons cell.  Materialisation via 'growListToVector'
uses 'V.create' and writes elements backwards into the mutable vector,
so no intermediate reversed list is allocated.

Benchmark results (aarch64, GHC 9.8.4 -O2) vs the previous difference-list
implementation: ~30% faster across all sizes for both boxed and unboxed
element types.
-}
module Proto.Internal.GrowList (
  GrowList (..),
  emptyGrowList,
  snocGrowList,
  growListToVector,
  growListToVectorU,
  growListLength,
) where

import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU


data GrowList a = GrowList
  { glBuild :: ![a]             -- reversed: most-recently-snoced element is at the head
  , glCount :: {-# UNPACK #-} !Int
  }


emptyGrowList :: GrowList a
emptyGrowList = GrowList [] 0
{-# INLINE emptyGrowList #-}


snocGrowList :: GrowList a -> a -> GrowList a
snocGrowList (GrowList xs n) x = GrowList (x : xs) (n + 1)
{-# INLINE snocGrowList #-}


growListLength :: GrowList a -> Int
growListLength = glCount
{-# INLINE growListLength #-}


-- | Materialise to a boxed Vector.
--
-- Fills the vector by writing index @n-1-i@ for each element of the
-- reversed list, traversing front-to-back.  This is equivalent to
-- @reverse@ + forward fill but avoids allocating the reversed list.
growListToVector :: GrowList a -> V.Vector a
growListToVector (GrowList xs n)
  | n == 0    = V.empty
  | otherwise = V.create $ do
      mv <- MV.unsafeNew n
      let fill !_ []       = pure ()
          fill !i (e : es) = MV.unsafeWrite mv (n - 1 - i) e >> fill (i + 1) es
      fill 0 xs
      pure mv
{-# INLINE growListToVector #-}


-- | Materialise to an unboxed Vector.
growListToVectorU :: VU.Unbox a => GrowList a -> VU.Vector a
growListToVectorU (GrowList xs n)
  | n == 0    = VU.empty
  | otherwise = VU.create $ do
      mv <- MVU.unsafeNew n
      let fill !_ []       = pure ()
          fill !i (e : es) = MVU.unsafeWrite mv (n - 1 - i) e >> fill (i + 1) es
      fill 0 xs
      pure mv
{-# INLINE growListToVectorU #-}
