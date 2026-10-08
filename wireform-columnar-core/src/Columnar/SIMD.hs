{-# LANGUAGE BangPatterns #-}

{- | FFI to @cbits/columnar_simd.c@: hot loops over packed bits, bitmaps and
Arrow buffers, used by the Parquet and Arrow column readers.

The 'BS.ByteString' helpers ('bitmapPopCount', 'unpackBitsLsbUnsafe') are
pure. Everything else is a thin @IO@ wrapper over an @unsafe@ foreign call
taking raw pointers; the caller keeps the memory alive for the duration of
the call and guarantees the buffers are as large as described below. No
kernel assumes any pointer alignment.

Conventions:

* Bitmaps are LSB-first: bit @k@ of a buffer is
  @(buf[k \`shiftR\` 3] \`shiftR\` (k .&. 7)) .&. 1@. Bit offsets may be any
  value, not just multiples of 8.
* A validity pointer of 'nullPtr' means every row is valid; otherwise row
  @i@ is valid when bit @validoff + i@ is set.
* Validators return @-1@ when the input is valid, else the index of the
  first failing element.
* Index arrays (@'Ptr' 'Int'@) hold 64-bit indices and are trusted: gathers
  and takes perform no bounds checks.

Validators:

* 'offsetsCheckI32', 'offsetsCheckI64': @n@ offsets (rows + 1). Valid when
  @n == 0@, or @offs[0] >= 0@, the offsets never decrease and
  @offs[n-1] <= limit@. Fails at 0 for a negative first offset, at @i+1@ when
  @offs[i+1] < offs[i]@, and at @n-1@ when the last offset exceeds the limit.
* 'utf8BoundariesI32', 'utf8BoundariesI64': offsets already validated against
  the data length; every offset below the length must not point at a UTF-8
  continuation byte. Full UTF-8 validity is checked separately.
* 'keysInRangeI8' .. 'keysInRangeU64': every valid dictionary key satisfies
  @0 <= key < max@ (unsigned keys at or above 2^63 always fail).
* 'runEndsCheckI16', 'runEndsCheckI32', 'runEndsCheckI64': run ends are
  positive, strictly increasing and the last is at least the minimum end. An
  empty array is valid only when the minimum end is at most 0 (else fails at
  0).
* 'listViewCheckI32', 'listViewCheckI64': for every valid row, offset and
  size are non-negative and @offset + size <= childLen@.
* 'denseUnionCheck': for every row, the type id names an existing child and
  the offset is within that child's length.
* 'viewRefsCheck': Arrow BinaryView / Utf8View (16 bytes per view). Lengths
  are non-negative; out-of-line views reference an existing buffer, lie
  within it and carry a matching 4-byte prefix. With the UTF-8 flag every
  payload (inline or referenced) must be strict UTF-8 (no overlongs,
  surrogates, code points above U+10FFFF or truncated sequences). Only valid
  rows are checked.

Bit kernels:

* 'popCountBits' counts set bits in a bit range.
* 'copyBits' copies a bit range, preserving the destination's other bits.
* 'andBits' writes the AND of two bit ranges from bit 0 of the destination,
  zeroes the trailing bits of the last byte, and returns the set-bit count.
* 'gatherBits' writes source bit @srcoff + idx[k]@ to destination bit @k@,
  zeroes the trailing bits of the last byte, and returns the set-bit count.

Gathers and takes:

* 'gather1' .. 'gather16': @dst[k] = src[idx[k]]@ for fixed-width elements
  of 1, 2, 4, 8 or 16 bytes.
* 'takeOffsetsI32', 'takeOffsetsI64': first pass of a variable-length take.
  Writes @n + 1@ destination offsets starting at 0 and returns the total
  byte length, or @-1@ when it does not fit the offset type.
* 'takeBytesI32', 'takeBytesI64': second pass, concatenating the selected
  rows' bytes.
* 'rebaseOffsetsI32', 'rebaseOffsetsI64': @dst[i] = src[i] + delta@; @dst@
  may equal @src@.

/Precondition:/ 'unpackBitsLsbUnsafe' requires @'BS.length' bs >= (n + 7) \`quot\` 8@.
-}
module Columnar.SIMD (
  bitmapPopCount,
  unpackBitsLsbUnsafe,
  memcpyFast,

  -- * Validators
  offsetsCheckI32,
  offsetsCheckI64,
  utf8BoundariesI32,
  utf8BoundariesI64,
  keysInRangeI8,
  keysInRangeI16,
  keysInRangeI32,
  keysInRangeI64,
  keysInRangeU8,
  keysInRangeU16,
  keysInRangeU32,
  keysInRangeU64,
  runEndsCheckI16,
  runEndsCheckI32,
  runEndsCheckI64,
  listViewCheckI32,
  listViewCheckI64,
  denseUnionCheck,
  viewRefsCheck,

  -- * Bit kernels
  popCountBits,
  copyBits,
  andBits,
  gatherBits,

  -- * Gathers, takes and offset rebasing
  gather1,
  gather2,
  gather4,
  gather8,
  gather16,
  takeOffsetsI32,
  takeOffsetsI64,
  takeBytesI32,
  takeBytesI64,
  rebaseOffsetsI32,
  rebaseOffsetsI64,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)


foreign import ccall unsafe "hs_columnar_bitmap_popcount"
  c_bitmap_popcount :: Ptr Word8 -> CInt -> Int32


foreign import ccall safe "hs_columnar_unpack_bits_lsb"
  c_unpack_bits_lsb :: Ptr Word8 -> Int32 -> Ptr Word8 -> IO ()


-- | Arguments are @src, dst, len@.
foreign import ccall unsafe "hs_columnar_memcpy_fast"
  c_memcpy_fast :: Ptr Word8 -> Ptr Word8 -> CInt -> IO ()


foreign import ccall unsafe "hs_columnar_offsets_i32"
  c_offsets_i32 :: Ptr Int32 -> CSize -> Int64 -> IO Int64


foreign import ccall unsafe "hs_columnar_offsets_i64"
  c_offsets_i64 :: Ptr Int64 -> CSize -> Int64 -> IO Int64


foreign import ccall unsafe "hs_columnar_utf8_boundaries_i32"
  c_utf8_boundaries_i32 :: Ptr Int32 -> CSize -> Ptr Word8 -> Int64 -> IO Int64


foreign import ccall unsafe "hs_columnar_utf8_boundaries_i64"
  c_utf8_boundaries_i64 :: Ptr Int64 -> CSize -> Ptr Word8 -> Int64 -> IO Int64


foreign import ccall unsafe "hs_columnar_popcount_bits"
  c_popcount_bits :: Ptr Word8 -> CSize -> CSize -> IO Int64


foreign import ccall unsafe "hs_columnar_keys_in_range_i8"
  c_keys_in_range_i8 :: Ptr Int8 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall unsafe "hs_columnar_keys_in_range_i16"
  c_keys_in_range_i16 :: Ptr Int16 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall unsafe "hs_columnar_keys_in_range_i32"
  c_keys_in_range_i32 :: Ptr Int32 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall unsafe "hs_columnar_keys_in_range_i64"
  c_keys_in_range_i64 :: Ptr Int64 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall unsafe "hs_columnar_keys_in_range_u8"
  c_keys_in_range_u8 :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall unsafe "hs_columnar_keys_in_range_u16"
  c_keys_in_range_u16 :: Ptr Word16 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall unsafe "hs_columnar_keys_in_range_u32"
  c_keys_in_range_u32 :: Ptr Word32 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall unsafe "hs_columnar_keys_in_range_u64"
  c_keys_in_range_u64 :: Ptr Word64 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall unsafe "hs_columnar_copy_bits"
  c_copy_bits :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CSize -> IO ()


foreign import ccall unsafe "hs_columnar_rebase_offsets_i32"
  c_rebase_offsets_i32 :: Ptr Int32 -> Ptr Int32 -> CSize -> Int64 -> IO ()


foreign import ccall unsafe "hs_columnar_rebase_offsets_i64"
  c_rebase_offsets_i64 :: Ptr Int64 -> Ptr Int64 -> CSize -> Int64 -> IO ()


foreign import ccall unsafe "hs_columnar_run_ends_i16"
  c_run_ends_i16 :: Ptr Int16 -> CSize -> Int64 -> IO Int64


foreign import ccall unsafe "hs_columnar_run_ends_i32"
  c_run_ends_i32 :: Ptr Int32 -> CSize -> Int64 -> IO Int64


foreign import ccall unsafe "hs_columnar_run_ends_i64"
  c_run_ends_i64 :: Ptr Int64 -> CSize -> Int64 -> IO Int64


foreign import ccall unsafe "hs_columnar_list_view_i32"
  c_list_view_i32 :: Ptr Int32 -> Ptr Int32 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall unsafe "hs_columnar_list_view_i64"
  c_list_view_i64 :: Ptr Int64 -> Ptr Int64 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall unsafe "hs_columnar_dense_union"
  c_dense_union :: Ptr Int8 -> Ptr Int32 -> CSize -> Ptr Int64 -> CSize -> IO Int64


foreign import ccall unsafe "hs_columnar_view_refs"
  c_view_refs
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr Int64 -> CSize -> CInt -> IO Int64


foreign import ccall unsafe "hs_columnar_gather_1"
  c_gather_1 :: Ptr Word8 -> Ptr Word8 -> Ptr Int -> CSize -> IO ()


foreign import ccall unsafe "hs_columnar_gather_2"
  c_gather_2 :: Ptr Word8 -> Ptr Word8 -> Ptr Int -> CSize -> IO ()


foreign import ccall unsafe "hs_columnar_gather_4"
  c_gather_4 :: Ptr Word8 -> Ptr Word8 -> Ptr Int -> CSize -> IO ()


foreign import ccall unsafe "hs_columnar_gather_8"
  c_gather_8 :: Ptr Word8 -> Ptr Word8 -> Ptr Int -> CSize -> IO ()


foreign import ccall unsafe "hs_columnar_gather_16"
  c_gather_16 :: Ptr Word8 -> Ptr Word8 -> Ptr Int -> CSize -> IO ()


foreign import ccall unsafe "hs_columnar_gather_bits"
  c_gather_bits :: Ptr Word8 -> Ptr Word8 -> CSize -> Ptr Int -> CSize -> IO Int64


foreign import ccall unsafe "hs_columnar_and_bits"
  c_and_bits :: Ptr Word8 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CSize -> IO Int64


foreign import ccall unsafe "hs_columnar_take_offsets_i32"
  c_take_offsets_i32 :: Ptr Int32 -> Ptr Int32 -> Ptr Int -> CSize -> IO Int64


foreign import ccall unsafe "hs_columnar_take_offsets_i64"
  c_take_offsets_i64 :: Ptr Int64 -> Ptr Int64 -> Ptr Int -> CSize -> IO Int64


foreign import ccall unsafe "hs_columnar_take_bytes_i32"
  c_take_bytes_i32 :: Ptr Word8 -> Ptr Int32 -> Ptr Word8 -> Ptr Int -> CSize -> IO ()


foreign import ccall unsafe "hs_columnar_take_bytes_i64"
  c_take_bytes_i64 :: Ptr Word8 -> Ptr Int64 -> Ptr Word8 -> Ptr Int -> CSize -> IO ()


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


{- | Bulk copy (SIMDe 16-byte chunks inside C). @dst@ must be at least @len@
bytes; only @len@ bytes are written.
-}
memcpyFast :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memcpyFast !dst !src !len =
  c_memcpy_fast src dst (fromIntegral len)


-- | Offsets check: @offs@, number of offsets (rows + 1), limit.
offsetsCheckI32 :: Ptr Int32 -> Int -> Int64 -> IO Int
offsetsCheckI32 offs n limit = fromIntegral <$> c_offsets_i32 offs (fromIntegral n) limit
{-# INLINE offsetsCheckI32 #-}


-- | Offsets check: @offs@, number of offsets (rows + 1), limit.
offsetsCheckI64 :: Ptr Int64 -> Int -> Int64 -> IO Int
offsetsCheckI64 offs n limit = fromIntegral <$> c_offsets_i64 offs (fromIntegral n) limit
{-# INLINE offsetsCheckI64 #-}


-- | Character boundary check: @offs@, number of offsets, data, data length.
utf8BoundariesI32 :: Ptr Int32 -> Int -> Ptr Word8 -> Int -> IO Int
utf8BoundariesI32 offs n dat len =
  fromIntegral <$> c_utf8_boundaries_i32 offs (fromIntegral n) dat (fromIntegral len)
{-# INLINE utf8BoundariesI32 #-}


-- | Character boundary check: @offs@, number of offsets, data, data length.
utf8BoundariesI64 :: Ptr Int64 -> Int -> Ptr Word8 -> Int -> IO Int
utf8BoundariesI64 offs n dat len =
  fromIntegral <$> c_utf8_boundaries_i64 offs (fromIntegral n) dat (fromIntegral len)
{-# INLINE utf8BoundariesI64 #-}


-- | Set bits in a bit range: buffer, bit offset, number of bits.
popCountBits :: Ptr Word8 -> Int -> Int -> IO Int
popCountBits buf bitoff nbits =
  fromIntegral <$> c_popcount_bits buf (fromIntegral bitoff) (fromIntegral nbits)
{-# INLINE popCountBits #-}


-- | Dictionary key check: keys, rows, validity (or 'nullPtr'), validity bit offset, max.
keysInRangeI8 :: Ptr Int8 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
keysInRangeI8 keys n valid validoff mx =
  fromIntegral <$> c_keys_in_range_i8 keys (fromIntegral n) valid (fromIntegral validoff) mx
{-# INLINE keysInRangeI8 #-}


-- | See 'keysInRangeI8'.
keysInRangeI16 :: Ptr Int16 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
keysInRangeI16 keys n valid validoff mx =
  fromIntegral <$> c_keys_in_range_i16 keys (fromIntegral n) valid (fromIntegral validoff) mx
{-# INLINE keysInRangeI16 #-}


-- | See 'keysInRangeI8'.
keysInRangeI32 :: Ptr Int32 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
keysInRangeI32 keys n valid validoff mx =
  fromIntegral <$> c_keys_in_range_i32 keys (fromIntegral n) valid (fromIntegral validoff) mx
{-# INLINE keysInRangeI32 #-}


-- | See 'keysInRangeI8'.
keysInRangeI64 :: Ptr Int64 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
keysInRangeI64 keys n valid validoff mx =
  fromIntegral <$> c_keys_in_range_i64 keys (fromIntegral n) valid (fromIntegral validoff) mx
{-# INLINE keysInRangeI64 #-}


-- | See 'keysInRangeI8'.
keysInRangeU8 :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
keysInRangeU8 keys n valid validoff mx =
  fromIntegral <$> c_keys_in_range_u8 keys (fromIntegral n) valid (fromIntegral validoff) mx
{-# INLINE keysInRangeU8 #-}


-- | See 'keysInRangeI8'.
keysInRangeU16 :: Ptr Word16 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
keysInRangeU16 keys n valid validoff mx =
  fromIntegral <$> c_keys_in_range_u16 keys (fromIntegral n) valid (fromIntegral validoff) mx
{-# INLINE keysInRangeU16 #-}


-- | See 'keysInRangeI8'.
keysInRangeU32 :: Ptr Word32 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
keysInRangeU32 keys n valid validoff mx =
  fromIntegral <$> c_keys_in_range_u32 keys (fromIntegral n) valid (fromIntegral validoff) mx
{-# INLINE keysInRangeU32 #-}


-- | See 'keysInRangeI8'.
keysInRangeU64 :: Ptr Word64 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
keysInRangeU64 keys n valid validoff mx =
  fromIntegral <$> c_keys_in_range_u64 keys (fromIntegral n) valid (fromIntegral validoff) mx
{-# INLINE keysInRangeU64 #-}


-- | Bit copy: dst, dst bit offset, src, src bit offset, number of bits.
copyBits :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Int -> IO ()
copyBits dst dstoff src srcoff nbits =
  c_copy_bits dst (fromIntegral dstoff) src (fromIntegral srcoff) (fromIntegral nbits)
{-# INLINE copyBits #-}


-- | Offset rebase: dst, src, count, delta.
rebaseOffsetsI32 :: Ptr Int32 -> Ptr Int32 -> Int -> Int64 -> IO ()
rebaseOffsetsI32 dst src n delta = c_rebase_offsets_i32 dst src (fromIntegral n) delta
{-# INLINE rebaseOffsetsI32 #-}


-- | Offset rebase: dst, src, count, delta.
rebaseOffsetsI64 :: Ptr Int64 -> Ptr Int64 -> Int -> Int64 -> IO ()
rebaseOffsetsI64 dst src n delta = c_rebase_offsets_i64 dst src (fromIntegral n) delta
{-# INLINE rebaseOffsetsI64 #-}


-- | Run-end check: ends, count, minimum last end.
runEndsCheckI16 :: Ptr Int16 -> Int -> Int64 -> IO Int
runEndsCheckI16 ends n minEnd = fromIntegral <$> c_run_ends_i16 ends (fromIntegral n) minEnd
{-# INLINE runEndsCheckI16 #-}


-- | Run-end check: ends, count, minimum last end.
runEndsCheckI32 :: Ptr Int32 -> Int -> Int64 -> IO Int
runEndsCheckI32 ends n minEnd = fromIntegral <$> c_run_ends_i32 ends (fromIntegral n) minEnd
{-# INLINE runEndsCheckI32 #-}


-- | Run-end check: ends, count, minimum last end.
runEndsCheckI64 :: Ptr Int64 -> Int -> Int64 -> IO Int
runEndsCheckI64 ends n minEnd = fromIntegral <$> c_run_ends_i64 ends (fromIntegral n) minEnd
{-# INLINE runEndsCheckI64 #-}


-- | List view check: offsets, sizes, rows, validity (or 'nullPtr'), validity bit offset, child length.
listViewCheckI32 :: Ptr Int32 -> Ptr Int32 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
listViewCheckI32 offs sizes n valid validoff childLen =
  fromIntegral <$> c_list_view_i32 offs sizes (fromIntegral n) valid (fromIntegral validoff) childLen
{-# INLINE listViewCheckI32 #-}


-- | See 'listViewCheckI32'.
listViewCheckI64 :: Ptr Int64 -> Ptr Int64 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
listViewCheckI64 offs sizes n valid validoff childLen =
  fromIntegral <$> c_list_view_i64 offs sizes (fromIntegral n) valid (fromIntegral validoff) childLen
{-# INLINE listViewCheckI64 #-}


-- | Dense union check: type ids, offsets, rows, child lengths, number of children.
denseUnionCheck :: Ptr Int8 -> Ptr Int32 -> Int -> Ptr Int64 -> Int -> IO Int
denseUnionCheck types offs n childLens nchildren =
  fromIntegral <$> c_dense_union types offs (fromIntegral n) childLens (fromIntegral nchildren)
{-# INLINE denseUnionCheck #-}


{- | View check: views, rows, validity (or 'nullPtr'), validity bit offset,
data buffer pointers, data buffer lengths, number of buffers, UTF-8 flag.
-}
viewRefsCheck :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr (Ptr Word8) -> Ptr Int64 -> Int -> Bool -> IO Int
viewRefsCheck views n valid validoff bufs buflens nbufs utf8 =
  fromIntegral
    <$> c_view_refs
      views
      (fromIntegral n)
      valid
      (fromIntegral validoff)
      bufs
      buflens
      (fromIntegral nbufs)
      (if utf8 then 1 else 0)
{-# INLINE viewRefsCheck #-}


-- | Fixed-width gather of 1-byte elements: dst, src, indices, count.
gather1 :: Ptr Word8 -> Ptr Word8 -> Ptr Int -> Int -> IO ()
gather1 dst src idx n = c_gather_1 dst src idx (fromIntegral n)
{-# INLINE gather1 #-}


-- | Fixed-width gather of 2-byte elements: dst, src, indices, count.
gather2 :: Ptr Word8 -> Ptr Word8 -> Ptr Int -> Int -> IO ()
gather2 dst src idx n = c_gather_2 dst src idx (fromIntegral n)
{-# INLINE gather2 #-}


-- | Fixed-width gather of 4-byte elements: dst, src, indices, count.
gather4 :: Ptr Word8 -> Ptr Word8 -> Ptr Int -> Int -> IO ()
gather4 dst src idx n = c_gather_4 dst src idx (fromIntegral n)
{-# INLINE gather4 #-}


-- | Fixed-width gather of 8-byte elements: dst, src, indices, count.
gather8 :: Ptr Word8 -> Ptr Word8 -> Ptr Int -> Int -> IO ()
gather8 dst src idx n = c_gather_8 dst src idx (fromIntegral n)
{-# INLINE gather8 #-}


-- | Fixed-width gather of 16-byte elements: dst, src, indices, count.
gather16 :: Ptr Word8 -> Ptr Word8 -> Ptr Int -> Int -> IO ()
gather16 dst src idx n = c_gather_16 dst src idx (fromIntegral n)
{-# INLINE gather16 #-}


-- | Bit gather: dst, src, src bit offset, indices, count. Returns the set-bit count.
gatherBits :: Ptr Word8 -> Ptr Word8 -> Int -> Ptr Int -> Int -> IO Int
gatherBits dst src srcoff idx n =
  fromIntegral <$> c_gather_bits dst src (fromIntegral srcoff) idx (fromIntegral n)
{-# INLINE gatherBits #-}


-- | Bitwise AND: dst, a, a bit offset, b, b bit offset, number of bits. Returns the set-bit count.
andBits :: Ptr Word8 -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> Int -> IO Int
andBits dst a aoff b boff nbits =
  fromIntegral
    <$> c_and_bits dst a (fromIntegral aoff) b (fromIntegral boff) (fromIntegral nbits)
{-# INLINE andBits #-}


-- | Take pass 1: dst offsets, src offsets, indices, count. Returns the total or -1.
takeOffsetsI32 :: Ptr Int32 -> Ptr Int32 -> Ptr Int -> Int -> IO Int
takeOffsetsI32 dstOffs srcOffs idx n =
  fromIntegral <$> c_take_offsets_i32 dstOffs srcOffs idx (fromIntegral n)
{-# INLINE takeOffsetsI32 #-}


-- | See 'takeOffsetsI32'.
takeOffsetsI64 :: Ptr Int64 -> Ptr Int64 -> Ptr Int -> Int -> IO Int
takeOffsetsI64 dstOffs srcOffs idx n =
  fromIntegral <$> c_take_offsets_i64 dstOffs srcOffs idx (fromIntegral n)
{-# INLINE takeOffsetsI64 #-}


-- | Take pass 2: dst bytes, src offsets, src bytes, indices, count.
takeBytesI32 :: Ptr Word8 -> Ptr Int32 -> Ptr Word8 -> Ptr Int -> Int -> IO ()
takeBytesI32 dst srcOffs src idx n = c_take_bytes_i32 dst srcOffs src idx (fromIntegral n)
{-# INLINE takeBytesI32 #-}


-- | See 'takeBytesI32'.
takeBytesI64 :: Ptr Word8 -> Ptr Int64 -> Ptr Word8 -> Ptr Int -> Int -> IO ()
takeBytesI64 dst srcOffs src idx n = c_take_bytes_i64 dst srcOffs src idx (fromIntegral n)
{-# INLINE takeBytesI64 #-}
