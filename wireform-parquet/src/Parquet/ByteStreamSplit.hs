{-# LANGUAGE BangPatterns #-}

{- | Encoder side of @BYTE_STREAM_SPLIT@ (Parquet encoding 9).

Transposes the little-endian byte representation of fixed-width
numeric values: instead of writing @[byte0, byte1, byte2, byte3]@ of
value 0, then the same of value 1, etc., the encoder writes
@[byte0(v0), byte0(v1), ...]@, then @[byte1(v0), byte1(v1), ...]@,
and so on. For columns of mostly-similar floats this clusters the
redundant high bytes together so downstream compressors (zstd /
snappy / gzip) recover most of the savings.

Reader counterparts: 'Parquet.Read.decodeByteStreamSplitFloat',
'Parquet.Read.decodeByteStreamSplitDouble'.
-}
module Parquet.ByteStreamSplit (
  encodeByteStreamSplitFloat,
  encodeByteStreamSplitDouble,
) where

import Columnar.SIMD qualified as SIMD
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSUnsafe
import Data.Primitive.ByteArray (ByteArray, copyByteArrayToAddr)
import Data.Vector.Primitive qualified as VP
import Data.Word (Word32, Word64, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (pokeByteOff)
import GHC.ByteOrder (ByteOrder (LittleEndian), targetByteOrder)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import System.IO.Unsafe (unsafeDupablePerformIO)


-- | @BYTE_STREAM_SPLIT@ for @FLOAT@: 4 streams of @n@ bytes (4n bytes total).
encodeByteStreamSplitFloat :: VP.Vector Float -> ByteString
encodeByteStreamSplitFloat vs
  | n > maxBound `quot` 4 = error "Parquet.ByteStreamSplit: FLOAT output size overflow"
  | n >= 16 && targetByteOrder == LittleEndian =
      encodeSplit 4 SIMD.splitByteStreams4Array SIMD.splitByteStreams4 vs
  | otherwise = unsafeDupablePerformIO $ do
      fp <- BSI.mallocByteString (4 * n)
      withForeignPtr fp $ \ptr -> writeFloats ptr vs n 0
      pure (BSI.fromForeignPtr fp 0 (4 * n))
  where
    n = VP.length vs


writeFloats :: Ptr Word8 -> VP.Vector Float -> Int -> Int -> IO ()
writeFloats !ptr !vs !n !i
  | i >= n = pure ()
  | otherwise = do
      let !w = castFloatToWord32 (VP.unsafeIndex vs i)
      pokeByteOff ptr i (byteOf32 0 w)
      pokeByteOff ptr (n + i) (byteOf32 1 w)
      pokeByteOff ptr (2 * n + i) (byteOf32 2 w)
      pokeByteOff ptr (3 * n + i) (byteOf32 3 w)
      writeFloats ptr vs n (i + 1)


byteOf32 :: Int -> Word32 -> Word8
byteOf32 k w = fromIntegral ((w `shiftR` (k * 8)) .&. 0xFF)
{-# INLINE byteOf32 #-}


-- | @BYTE_STREAM_SPLIT@ for @DOUBLE@: 8 streams of @n@ bytes (8n bytes total).
encodeByteStreamSplitDouble :: VP.Vector Double -> ByteString
encodeByteStreamSplitDouble vs
  | n > maxBound `quot` 8 = error "Parquet.ByteStreamSplit: DOUBLE output size overflow"
  | n >= 16 && targetByteOrder == LittleEndian =
      encodeSplit 8 SIMD.splitByteStreams8Array SIMD.splitByteStreams8 vs
  | otherwise = unsafeDupablePerformIO $ do
      fp <- BSI.mallocByteString (8 * n)
      withForeignPtr fp $ \ptr -> writeDoubles ptr vs n 0
      pure (BSI.fromForeignPtr fp 0 (8 * n))
  where
    n = VP.length vs


-- Small immutable arrays may move between calls, but cannot move during a
-- bounded unsafe FFI call. Larger inputs are copied once into pinned
-- storage so the safe call can allow GC while C reads them.
encodeSplit
  :: Int
  -> (ByteArray -> Int -> Ptr Word8 -> Int -> IO ())
  -> (Ptr Word8 -> Ptr Word8 -> Int -> IO ())
  -> VP.Vector a
  -> ByteString
encodeSplit width splitArray split (VP.Vector offset n bytes) =
  unsafeDupablePerformIO $
    if n <= SIMD.unsafeCallBytes `quot` width
      then BSI.create total $ \dst -> splitArray bytes (offset * width) dst n
      else do
        input <- BSI.create total $ \src -> copyByteArrayToAddr src bytes (offset * width) total
        BSI.create total $ \dst -> BSUnsafe.unsafeUseAsCString input $ \src -> split (castPtr src) dst n
  where
    total = width * n
{-# INLINE encodeSplit #-}


writeDoubles :: Ptr Word8 -> VP.Vector Double -> Int -> Int -> IO ()
writeDoubles !ptr !vs !n !i
  | i >= n = pure ()
  | otherwise = do
      let !w = castDoubleToWord64 (VP.unsafeIndex vs i)
      pokeByteOff ptr i (byteOf64 0 w)
      pokeByteOff ptr (n + i) (byteOf64 1 w)
      pokeByteOff ptr (2 * n + i) (byteOf64 2 w)
      pokeByteOff ptr (3 * n + i) (byteOf64 3 w)
      pokeByteOff ptr (4 * n + i) (byteOf64 4 w)
      pokeByteOff ptr (5 * n + i) (byteOf64 5 w)
      pokeByteOff ptr (6 * n + i) (byteOf64 6 w)
      pokeByteOff ptr (7 * n + i) (byteOf64 7 w)
      writeDoubles ptr vs n (i + 1)


byteOf64 :: Int -> Word64 -> Word8
byteOf64 k w = fromIntegral ((w `shiftR` (k * 8)) .&. 0xFF)
{-# INLINE byteOf64 #-}
