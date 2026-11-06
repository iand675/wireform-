{-# LANGUAGE BangPatterns #-}

{- | Adaptive Lossless floating-Point page decoding (encoding 10).

The layout and two-step decimal reconstruction follow Apache Parquet's
AlpEncoding.md. Validation precedes output allocation. Exceptions are copied
as IEEE bits, including negative zero and noncanonical NaN payloads.
-}
module Parquet.Alp (decodeAlpFloat, decodeAlpDouble) where

import Columnar.SIMD qualified as SIMD
import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32)
import Data.Primitive.Types (Prim)
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as M
import Data.Word (Word16, Word32, Word64, byteSwap16, byteSwap32, byteSwap64)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.ByteOrder (ByteOrder (LittleEndian), targetByteOrder)
import GHC.ForeignPtr (unsafeWithForeignPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)


decodeAlpFloat :: Int -> ByteString -> Either String (P.Vector Float)
decodeAlpFloat = decodeAlp 4 10
{-# INLINE decodeAlpFloat #-}


decodeAlpDouble :: Int -> ByteString -> Either String (P.Vector Double)
decodeAlpDouble = decodeAlp 8 18
{-# INLINE decodeAlpDouble #-}


data PageInfo = PageInfo
  { pageVectorSize :: {-# UNPACK #-} !Int
  , pageVectors :: {-# UNPACK #-} !Int
  }


data VectorInfo = VectorInfo
  { vectorExponent :: {-# UNPACK #-} !Int
  , vectorFactor :: {-# UNPACK #-} !Int
  , vectorExceptions :: {-# UNPACK #-} !Int
  , vectorFrame :: {-# UNPACK #-} !Word64
  , vectorWidth :: {-# UNPACK #-} !Int
  , vectorPacked :: {-# UNPACK #-} !Int
  , vectorPositions :: {-# UNPACK #-} !Int
  , vectorValues :: {-# UNPACK #-} !Int
  , vectorEnd :: {-# UNPACK #-} !Int
  }


decodeAlp :: Prim a => Int -> Int -> Int -> ByteString -> Either String (P.Vector a)
decodeAlp width maxExponent count bytes = do
  page <- validatePage width maxExponent count bytes
  -- Source ownership spans every bounded unsafe call. The output remains a
  -- movable array: each call receives its current ByteArray# directly, and
  -- no raw destination pointer survives a GC point between chunks.
  Right $ unsafeDupablePerformIO $ BSU.unsafeUseAsCString bytes $ \rawInput -> do
    output@(M.MVector _ _ array) <- M.unsafeNew count
    let input = castPtr rawInput
        chunkLimit = SIMD.unsafeCallBytes `quot` width
        exceptionLimit = SIMD.unsafeCallBytes `quot` (width + 2)
        vectors !index !written
          | index == pageVectors page = P.unsafeFreeze output
          | otherwise = do
              let !offset = 7 + fromIntegral (readLE bytes (7 + index * 4) 4)
                  !size = min (pageVectorSize page) (count - written)
                  info = inspectVector width bytes offset size
                  !packed = input `plusPtr` vectorPacked info
                  !packedSize = vectorPositions info - vectorPacked info
                  !bits = vectorWidth info
                  !e = vectorExponent info
                  !f = vectorFactor info
                  values !first
                    | first == size = pure ()
                    | otherwise = do
                        let !n = min chunkLimit (size - first)
                            !destination = (written + first) * width
                        if width == 4
                          then SIMD.unpackFloat32Array (SIMD.PackedFloat32 bits (fromIntegral (vectorFrame info)) (positivePower f) (negativePower e)) packed packedSize array destination first n
                          else SIMD.unpackFloat64Array (SIMD.PackedFloat64 bits (vectorFrame info) (positivePower f) (negativePower e)) packed packedSize array destination first n
                        values (first + n)
                  exceptions !first
                    | first == vectorExceptions info = pure ()
                    | otherwise = do
                        let !n = min exceptionLimit (vectorExceptions info - first)
                            !positions = input `plusPtr` (vectorPositions info + first * 2)
                            !valuesPtr = input `plusPtr` (vectorValues info + first * width)
                            !destination = written * width
                        if width == 4
                          then SIMD.scatterFloat32Array positions valuesPtr array destination size n
                          else SIMD.scatterFloat64Array positions valuesPtr array destination size n
                        exceptions (first + n)
              values 0
              exceptions 0
              vectors (index + 1) (written + size)
    vectors 0 0
{-# INLINE decodeAlp #-}


validatePage :: Int -> Int -> Int -> ByteString -> Either String PageInfo
validatePage width maxExponent count bytes
  | count < 0 || count > fromIntegral (maxBound :: Int32) = invalid "invalid value count"
  | count > maxBound `quot` width = invalid "output exceeds addressable size"
  | BS.length bytes < 7 = invalid "truncated page header"
  | BSU.unsafeIndex bytes 0 /= 0 = invalid "unsupported compression mode"
  | BSU.unsafeIndex bytes 1 /= 0 = invalid "unsupported integer encoding"
  | logSize < 3 || logSize > 15 = invalid "invalid vector size"
  | readLE bytes 3 4 /= fromIntegral count = invalid "value count differs from page header"
  | vectorCount > (BS.length bytes - 7) `quot` 4 = invalid "truncated offset array"
  | otherwise = do
      vectors 0 0 (7 + vectorCount * 4)
      Right (PageInfo vectorSize vectorCount)
  where
    logSize = fromIntegral (BSU.unsafeIndex bytes 2)
    vectorSize = 1 `shiftL` logSize
    vectorCount = if count == 0 then 0 else (count - 1) `quot` vectorSize + 1
    vectors !index !written !expected
      | index == vectorCount =
          if expected == BS.length bytes then Right () else invalid "trailing page bytes"
      | readLE bytes (7 + index * 4) 4 /= fromIntegral (expected - 7) =
          invalid "vector offsets are not contiguous"
      | BS.length bytes - expected < width + 5 = invalid "truncated vector header"
      | otherwise = do
          let !size = min vectorSize (count - written)
              info = inspectVector width bytes expected size
          if vectorExponent info > maxExponent || vectorFactor info > vectorExponent info
            then invalid "invalid decimal parameters"
            else Right ()
          if vectorWidth info > width * 8
            then invalid "invalid bit width"
            else Right ()
          if vectorEnd info > BS.length bytes
            then invalid "truncated vector data"
            else Right ()
          let !usedBits = size * vectorWidth info
              !padding = usedBits `rem` 8
          if padding /= 0 && BSU.unsafeIndex bytes (vectorPositions info - 1) `shiftR` padding /= 0
            then invalid "nonzero packed padding bits"
            else Right ()
          let positions !i
                | i == vectorExceptions info = Right ()
                | readLE bytes (vectorPositions info + i * 2) 2 >= fromIntegral size =
                    invalid "exception position exceeds vector"
                | otherwise = positions (i + 1)
          positions 0
          vectors (index + 1) (written + size) (vectorEnd info)


-- Only called after checking the fixed header. Sizes are bounded by a
-- 32768-element vector, byte-sized bit width, and uint16 exception count.
inspectVector :: Int -> ByteString -> Int -> Int -> VectorInfo
inspectVector width bytes offset size =
  let !exceptions = fromIntegral (readLE bytes (offset + 2) 2)
      !bits = fromIntegral (BSU.unsafeIndex bytes (offset + 4 + width))
      !packed = offset + 5 + width
      !positions = packed + (size * bits + 7) `quot` 8
      !values = positions + exceptions * 2
  in VectorInfo
       (fromIntegral (BSU.unsafeIndex bytes offset))
       (fromIntegral (BSU.unsafeIndex bytes (offset + 1)))
       exceptions
       (readLE bytes (offset + 4) width)
       bits
       packed
       positions
       values
       (values + exceptions * width)
{-# INLINE inspectVector #-}


-- The caller has validated each complete field before reaching this helper.
-- Reading whole unaligned words avoids rebuilding uint16 exception positions
-- and uint32 offsets byte by byte. The ByteString owner stays live throughout
-- each load; every access remains within the validated field itself.
readLE :: ByteString -> Int -> Int -> Word64
readLE bytes offset width = case BSI.toForeignPtr bytes of
  (owner, base, _) -> BSI.accursedUnutterablePerformIO $ unsafeWithForeignPtr owner $ \input ->
    let fieldOffset = base + offset
    in case width of
         2 -> do
           value <- peekByteOff input fieldOffset :: IO Word16
           pure (fromIntegral (if targetByteOrder == LittleEndian then value else byteSwap16 value))
         4 -> do
           value <- peekByteOff input fieldOffset :: IO Word32
           pure (fromIntegral (if targetByteOrder == LittleEndian then value else byteSwap32 value))
         8 -> do
           value <- peekByteOff input fieldOffset :: IO Word64
           pure (if targetByteOrder == LittleEndian then value else byteSwap64 value)
         _ -> pure (go 0 0)
  where
    go !i !acc
      | i == width = acc
      | otherwise = go (i + 1) (acc .|. (fromIntegral (BSU.unsafeIndex bytes (offset + i)) `shiftL` (i * 8)))
{-# INLINE readLE #-}


-- Literal conversion supplies correctly rounded binary32/binary64 values.
-- Runtime exponentiation and combining the two powers are not conformant.
positivePower :: Fractional a => Int -> a
positivePower n = case n of
  0 -> 1e0
  1 -> 1e1
  2 -> 1e2
  3 -> 1e3
  4 -> 1e4
  5 -> 1e5
  6 -> 1e6
  7 -> 1e7
  8 -> 1e8
  9 -> 1e9
  10 -> 1e10
  11 -> 1e11
  12 -> 1e12
  13 -> 1e13
  14 -> 1e14
  15 -> 1e15
  16 -> 1e16
  17 -> 1e17
  _ -> 1e18
{-# INLINE positivePower #-}


negativePower :: Fractional a => Int -> a
negativePower n = case n of
  0 -> 1e0
  1 -> 1e-1
  2 -> 1e-2
  3 -> 1e-3
  4 -> 1e-4
  5 -> 1e-5
  6 -> 1e-6
  7 -> 1e-7
  8 -> 1e-8
  9 -> 1e-9
  10 -> 1e-10
  11 -> 1e-11
  12 -> 1e-12
  13 -> 1e-13
  14 -> 1e-14
  15 -> 1e-15
  16 -> 1e-16
  17 -> 1e-17
  _ -> 1e-18
{-# INLINE negativePower #-}


invalid :: String -> Either String a
invalid message = Left ("Parquet.Alp: " <> message)
