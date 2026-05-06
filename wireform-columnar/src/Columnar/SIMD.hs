{-# LANGUAGE BangPatterns #-}

-- | FFI to @cbits/columnar_simd.c@ (SIMDe-backed helpers for packed bits and
-- bitmaps). Used by Parquet and Arrow column readers on hot paths.
--
-- /Precondition:/ 'unpackBitsLsbUnsafe' requires @'BS.length' bs >= (n + 7) \`quot\` 8@.
module Columnar.SIMD
  ( bitmapPopCount
  , unpackBitsLsbUnsafe
  , memcpyFast
  ) where

import Data.Bits (shiftR, (.&.))
import Data.Int (Int32)
import Data.Word (Word8)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peekByteOff)

import Data.ByteString qualified as BS
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Vector qualified as V
import qualified Data.Vector.Mutable as VM
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "hs_columnar_bitmap_popcount"
  c_bitmap_popcount :: Ptr Word8 -> CInt -> Int32

foreign import ccall unsafe "hs_columnar_unpack_bits_lsb"
  c_unpack_bits_lsb :: Ptr Word8 -> Int32 -> Ptr Word8 -> IO ()

foreign import ccall unsafe "hs_columnar_memcpy_fast"
  c_memcpy_fast :: Ptr Word8 -> Ptr Word8 -> CInt -> IO ()

-- | Count set bits in a byte range (entire bytes).
bitmapPopCount :: BS.ByteString -> Int
bitmapPopCount bs
  | BS.length bs == 0 = 0
  | otherwise =
      fromIntegral $
        unsafePerformIO $
          unsafeUseAsCStringLen bs $ \(p, len) ->
            pure $! c_bitmap_popcount (castPtr p) (fromIntegral len)

-- | Expand @n@ LSB-first packed bits (Arrow / Parquet bool layout) into a
-- boxed 'Bool' vector.
--
-- Implementation note (perf): the previous version went
--
--   1. allocate a Storable Word8 buffer of size @n@,
--   2. C unpack bits into it,
--   3. @VU.convert@ to an Unboxed Word8 vector (allocates +
--      copies),
--   4. @VU.map (/= 0)@ to an Unboxed Bool vector (allocates +
--      copies; Unboxed Bool packs back into bits, so this is
--      really 100 KB of bit-twiddling for 100 K rows),
--   5. @V.convert@ to a Boxed Bool vector (allocates the spine
--      and unpacks each bit back to a 'Bool' pointer).
--
-- Three intermediate allocations + three traversals just to
-- end up where we wanted in step 1. For 100 k rows that's
-- ~300 µs of pure copying that adds up on parallel reads.
--
-- The new path allocates one boxed mutable vector and walks
-- the source one byte at a time, writing all 8 bits in a
-- straight-line inner block. 'False' / 'True' are static
-- closures so each write is just a pointer store into the
-- spine.
unpackBitsLsbUnsafe :: Int -> BS.ByteString -> V.Vector Bool
unpackBitsLsbUnsafe !n bs = unsafePerformIO $ do
  mv <- VM.unsafeNew n
  unsafeUseAsCStringLen bs $ \(src, _) -> do
    let !srcPtr = castPtr src :: Ptr Word8
        -- Walk one source byte at a time and write up to 8 bits
        -- into the destination. Avoids the per-bit byteIdx /
        -- bitIdx division in a 100 k-iter loop.
        outer !srcOff !i
          | i >= n = pure ()
          | otherwise = do
              !b <- peekByteOff srcPtr srcOff :: IO Word8
              let !nThisByte = min 8 (n - i)
                  inner !bit
                    | bit >= nThisByte = pure ()
                    | otherwise = do
                        VM.unsafeWrite mv (i + bit)
                          ((b `shiftR` bit) .&. 1 /= 0)
                        inner (bit + 1)
              inner 0
              outer (srcOff + 1) (i + 8)
    outer 0 0
  V.unsafeFreeze mv

-- | Bulk copy (SIMDe 16-byte chunks inside C). @dst@ must be at least @len@
-- bytes; only @len@ bytes are written.
memcpyFast :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memcpyFast !dst !src !len =
  c_memcpy_fast dst src (fromIntegral len)
