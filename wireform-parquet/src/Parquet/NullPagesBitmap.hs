{-# LANGUAGE BangPatterns #-}
-- | SIMD-accelerated helpers for Parquet @ColumnIndex.null_pages@.
--
-- @ColumnIndex@ stores a @Vector Bool@ where index @i@ is @True@ when the
-- corresponding page is entirely null. Scan planners want fast answers
-- to two questions:
--
-- 1. /How many/ pages can be skipped because they are all-null?
-- 2. /Which/ pages overlap with a given delete-position range or
--    deletion vector? The complement (\"which pages have at least one
--    non-null row\") is the working set.
--
-- The @null_pages@ vector is dense (one bit per page; tables typically
-- have hundreds to thousands of pages per row group), so packing it as
-- a 1-bit-per-page bitmap and using "Wireform.Hash"'s Roaring
-- 32-bit kernels (or "Columnar.SIMD"'s @popcount@ for non-zero counts)
-- gives a much tighter scan than the boxed boolean vector.
module Parquet.NullPagesBitmap
  ( packNullPages
  , unpackNullPages
  , nullPageCount
  , nonNullPages
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as VPM
import Data.Word (Word8)
import Foreign.Storable (pokeByteOff)

import Columnar.SIMD (bitmapPopCount)

-- | Pack a @null_pages@ vector into an LSB-first bitmap, one bit per
-- page (1 = null page, 0 = page has at least one non-null row).
packNullPages :: V.Vector Bool -> ByteString
packNullPages vs =
  let !n        = V.length vs
      !numBytes = (n + 7) `shiftR` 3
  in BSI.unsafeCreate numBytes $ \p ->
       let writeByte !byteIdx
             | byteIdx >= numBytes = pure ()
             | otherwise =
                 let !startBit = byteIdx `shiftL` 3
                     go !bit !acc
                       | bit >= 8                = acc
                       | startBit + bit >= n     = acc
                       | otherwise =
                           let !v = V.unsafeIndex vs (startBit + bit)
                           in go (bit + 1)
                                  (if v then acc .|. (1 `shiftL` bit) else acc)
                 in do
                      pokeByteOff p byteIdx (fromIntegral (go 0 (0 :: Int)) :: Word8)
                      writeByte (byteIdx + 1)
       in writeByte 0

-- | Inverse of 'packNullPages'.
unpackNullPages :: Int -> ByteString -> V.Vector Bool
unpackNullPages n bs =
  let !len = BS.length bs
  in V.generate n $ \i ->
       let !byteIdx = i `shiftR` 3
           !bit     = i .&. 7
           !b = if byteIdx < len then BSU.unsafeIndex bs byteIdx else 0
       in (b `shiftR` bit) .&. 1 == 1

-- | Number of null pages in the bitmap. Backed by the SIMD
-- 'bitmapPopCount' kernel from "Columnar.SIMD".
nullPageCount :: ByteString -> Int
nullPageCount = bitmapPopCount
{-# INLINE nullPageCount #-}

-- | Indices of pages that are /not/ all-null - i.e. the pages a scan
-- needs to read. Walks the bitmap byte-by-byte; allocates one
-- mutable @VP.Vector Int@ of worst-case size and freezes a slice
-- with the actual count.
nonNullPages :: Int -> ByteString -> VP.Vector Int
nonNullPages totalPages bs = VP.create $ do
  let !len = BS.length bs
  buf <- VPM.unsafeNew totalPages
  let go !i !w
        | i >= totalPages = pure (VPM.unsafeSlice 0 w buf)
        | otherwise =
            let !byteIdx = i `shiftR` 3
                !bit     = i .&. 7
                !b = if byteIdx < len then BSU.unsafeIndex bs byteIdx else 0
            in if (b `shiftR` bit) .&. 1 == 1
                 then go (i + 1) w
                 else do
                   VPM.unsafeWrite buf w i
                   go (i + 1) (w + 1)
  go 0 0
