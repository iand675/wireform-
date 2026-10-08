{-# LANGUAGE BangPatterns #-}

{- | FFI to @cbits/columnar_simd.c@ (SIMDe-backed helpers for packed bits and
bitmaps). Used by Parquet and Arrow column readers on hot paths.

/Precondition:/ 'unpackBitsLsbUnsafe' requires @'BS.length' bs >= (n + 7) \`quot\` 8@.
-}
module Columnar.SIMD (
  bitmapPopCount,
  unpackBitsLsbUnsafe,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int (Int32)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word8)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)


foreign import ccall unsafe "hs_columnar_bitmap_popcount"
  c_bitmap_popcount :: Ptr Word8 -> CInt -> Int32


foreign import ccall safe "hs_columnar_unpack_bits_lsb"
  c_unpack_bits_lsb :: Ptr Word8 -> Int32 -> Ptr Word8 -> IO ()


-- | Count set bits in a byte range (entire bytes).
bitmapPopCount :: BS.ByteString -> Int
bitmapPopCount bs
  | BS.length bs == 0 = 0
  | otherwise =
      fromIntegral $
        unsafePerformIO $
          unsafeUseAsCStringLen bs $ \(p, len) ->
            pure $! c_bitmap_popcount (castPtr p) (fromIntegral len)


{- | Expand @n@ LSB-first packed bits (Arrow / Parquet bool layout) into a
boxed 'Bool' vector.
-}
unpackBitsLsbUnsafe :: Int -> BS.ByteString -> V.Vector Bool
unpackBitsLsbUnsafe !n bs = unsafeDupablePerformIO $ do
  mvs <- VSM.unsafeNew n
  VSM.unsafeWith mvs $ \dst ->
    unsafeUseAsCStringLen bs $ \(src, _) ->
      c_unpack_bits_lsb (castPtr src) (fromIntegral n) dst
  sv <- VS.unsafeFreeze mvs
  pure $! V.convert (VU.map (/= (0 :: Word8)) (VU.convert sv))
