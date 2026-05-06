{-# LANGUAGE BangPatterns #-}

-- | FFI to @cbits/columnar_simd.c@ (SIMDe-backed helpers for packed bits and
-- bitmaps). Used by Parquet and Arrow column readers on hot paths.
--
-- /Precondition:/ 'unpackBitsLsbUnsafe' requires @'BS.length' bs >= (n + 7) \`quot\` 8@.
module Columnar.SIMD
  ( bitmapPopCount
  , unpackBitsLsbUnsafe
  , memcpyFast
    -- * SIMD ASCII validation + endian-swap copies
  , isAsciiPtr
  , isAsciiBS
  , bswap16Copy
  , bswap32Copy
  , bswap64Copy
  ) where

import Data.Bits (shiftL, shiftR, (.&.))
import Data.Int (Int32)
import Data.Word (Word8)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peekByteOff)
import qualified Data.ByteString as BS
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

foreign import ccall unsafe "hs_columnar_is_ascii"
  c_is_ascii :: Ptr Word8 -> Int32 -> Int32

foreign import ccall unsafe "hs_columnar_bswap16_copy"
  c_bswap16_copy :: Ptr Word8 -> Ptr Word8 -> Int32 -> IO ()

foreign import ccall unsafe "hs_columnar_bswap32_copy"
  c_bswap32_copy :: Ptr Word8 -> Ptr Word8 -> Int32 -> IO ()

foreign import ccall unsafe "hs_columnar_bswap64_copy"
  c_bswap64_copy :: Ptr Word8 -> Ptr Word8 -> Int32 -> IO ()

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
        -- Whole-byte stride: read one source byte, write 8
        -- destination cells, jump 8. The per-cell branch
        -- ((b >> bit) .&. 1 /= 0) compiles to a tight
        -- shift+test+conditional-move; GHC's True/False
        -- constructors are static singletons so each write
        -- is just a pointer store into the V.Vector spine.
        --
        -- Tail (last partial byte, if n isn't a multiple of
        -- 8) handled by the slow path with explicit bit
        -- count.
        !nFull = n `shiftR` 3
        !nTail = n .&. 7

        whole !srcOff !i
          | srcOff >= nFull = pure ()
          | otherwise = do
              !b <- peekByteOff srcPtr srcOff :: IO Word8
              VM.unsafeWrite mv  i      (b               .&. 1 /= 0)
              VM.unsafeWrite mv (i + 1) ((b `shiftR` 1)  .&. 1 /= 0)
              VM.unsafeWrite mv (i + 2) ((b `shiftR` 2)  .&. 1 /= 0)
              VM.unsafeWrite mv (i + 3) ((b `shiftR` 3)  .&. 1 /= 0)
              VM.unsafeWrite mv (i + 4) ((b `shiftR` 4)  .&. 1 /= 0)
              VM.unsafeWrite mv (i + 5) ((b `shiftR` 5)  .&. 1 /= 0)
              VM.unsafeWrite mv (i + 6) ((b `shiftR` 6)  .&. 1 /= 0)
              VM.unsafeWrite mv (i + 7) ((b `shiftR` 7)  .&. 1 /= 0)
              whole (srcOff + 1) (i + 8)

        partial !i !b !bit
          | bit >= nTail = pure ()
          | otherwise = do
              VM.unsafeWrite mv (i + bit) ((b `shiftR` bit) .&. 1 /= 0)
              partial i b (bit + 1)
    whole 0 0
    if nTail > 0
      then do
        !b <- peekByteOff srcPtr nFull :: IO Word8
        partial (nFull `shiftL` 3) b 0
      else pure ()
  V.unsafeFreeze mv

-- | Bulk copy (SIMDe 16-byte chunks inside C). @dst@ must be at least @len@
-- bytes; only @len@ bytes are written.
memcpyFast :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memcpyFast !dst !src !len =
  c_memcpy_fast dst src (fromIntegral len)

-- | True iff every byte in @[ptr, ptr+len)@ is < 0x80. SSE2-accelerated
-- via SIMDe: 64 bytes per loop iteration with 4 16-byte vector ORs and
-- a final movemask test of the high-bit accumulator.
isAsciiPtr :: Ptr Word8 -> Int -> Bool
isAsciiPtr !ptr !len = c_is_ascii ptr (fromIntegral len) /= 0

-- | True iff every byte of the bytestring is < 0x80. Convenience
-- wrapper around 'isAsciiPtr'.
isAsciiBS :: BS.ByteString -> Bool
isAsciiBS bs = unsafePerformIO $
  unsafeUseAsCStringLen bs $ \(cstr, len) ->
    pure $! isAsciiPtr (castPtr cstr) len

-- | SIMD byte-swap memcpy at 16-bit lane width: copy @nElems@ 2-byte
-- words from @src@ to @dst@, byte-swapping each one. Used for the
-- BE↔LE conversion path on Arrow / Parquet reads when host and wire
-- endianness disagree (which is rare in practice — most Arrow / Parquet
-- producers emit little-endian).
--
-- Implemented in C via SSSE3 'pshufb' through SIMDe (so we get an ARM
-- NEON path on Apple Silicon for free). Tail handled scalarly with
-- @__builtin_bswap16@.
bswap16Copy :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
bswap16Copy !dst !src !nElems = c_bswap16_copy src dst (fromIntegral nElems)

-- | Like 'bswap16Copy' but at 32-bit lane width.
bswap32Copy :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
bswap32Copy !dst !src !nElems = c_bswap32_copy src dst (fromIntegral nElems)

-- | Like 'bswap16Copy' but at 64-bit lane width.
bswap64Copy :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
bswap64Copy !dst !src !nElems = c_bswap64_copy src dst (fromIntegral nElems)
