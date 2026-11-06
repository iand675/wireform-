{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

{- | FFI to @cbits/columnar_simd.c@: hot loops over packed bits, bitmaps and
Arrow buffers, used by the Parquet and Arrow column readers.

The 'BS.ByteString' helpers ('bitmapPopCount', 'unpackBitsLsbUnsafe') are
pure. Everything else is a thin @IO@ wrapper over a foreign call taking raw
pointers; the caller keeps the memory alive for the duration of the call and
guarantees the buffers are as large as described below. No kernel assumes
any pointer alignment.

Calling convention: an @unsafe@ call saves about 100 ns but stops every
other capability from reaching a GC sync point until it returns, so it is
only worth it for bounded work. Each wrapper uses the @unsafe@ import when
the input is at most 'unsafeCallBytes' and the @safe@ import otherwise
(the same split @bytestring@ uses for 'BS.isValidUtf8'). Kernels whose work
is not bounded by their arguments ('takeBytesI32', 'takeBytesI64',
'viewRefsCheck' with UTF-8 validation) are always @safe@.

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
* Every length and element count crosses the FFI as a 'CSize' (C @size_t@),
  never as a 32-bit 'CInt': a count is never negative, so it widens
  exactly, and a buffer above 2 GiB is passed in full.

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

  -- * Byte-stream transposes
  unsafeCallBytes,
  splitByteStreams4,
  splitByteStreams8,
  joinByteStreams4,
  joinByteStreams8,
  splitByteStreams4Array,
  splitByteStreams8Array,
  joinByteStreams4Array,
  joinByteStreams8Array,

  -- * Packed signed integers with floating-point scaling
  PackedFloat32 (..),
  PackedFloat64 (..),
  unpackFloat32Array,
  unpackFloat64Array,
  scatterFloat32Array,
  scatterFloat64Array,

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
import Data.Primitive.ByteArray (ByteArray (..), MutableByteArray (..), getSizeofMutableByteArray, sizeofByteArray)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types (CDouble (..), CFloat (..), CInt (..), CSize (..), CUInt (..))
import Foreign.Ptr (Ptr, castPtr)
import GHC.Exts (ByteArray#, Int (I#), MutableByteArray#, RealWorld, and#, eqWord#, gtWord#, int2Word#, isTrue#, neWord#, timesWord2#, uncheckedShiftRL#, word2Int#)
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)


foreign import ccall unsafe "hs_columnar_bitmap_popcount"
  c_bitmap_popcount :: Ptr Word8 -> CSize -> Int64


foreign import ccall safe "hs_columnar_unpack_bits_lsb"
  c_unpack_bits_lsb :: Ptr Word8 -> CSize -> Ptr Word8 -> IO ()


-- | Arguments are @src, dst, len@.
foreign import ccall unsafe "hs_columnar_memcpy_fast"
  c_memcpy_fast :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()


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


-- Safe twins of the kernels above, for inputs over 'unsafeCallBytes'.

foreign import ccall safe "hs_columnar_bitmap_popcount"
  c_bitmap_popcount_safe :: Ptr Word8 -> CSize -> Int64


foreign import ccall safe "hs_columnar_memcpy_fast"
  c_memcpy_fast_safe :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()


foreign import ccall safe "hs_columnar_offsets_i32"
  c_offsets_i32_safe :: Ptr Int32 -> CSize -> Int64 -> IO Int64


foreign import ccall safe "hs_columnar_offsets_i64"
  c_offsets_i64_safe :: Ptr Int64 -> CSize -> Int64 -> IO Int64


foreign import ccall safe "hs_columnar_utf8_boundaries_i32"
  c_utf8_boundaries_i32_safe :: Ptr Int32 -> CSize -> Ptr Word8 -> Int64 -> IO Int64


foreign import ccall safe "hs_columnar_utf8_boundaries_i64"
  c_utf8_boundaries_i64_safe :: Ptr Int64 -> CSize -> Ptr Word8 -> Int64 -> IO Int64


foreign import ccall safe "hs_columnar_popcount_bits"
  c_popcount_bits_safe :: Ptr Word8 -> CSize -> CSize -> IO Int64


foreign import ccall safe "hs_columnar_keys_in_range_i8"
  c_keys_in_range_i8_safe :: Ptr Int8 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall safe "hs_columnar_keys_in_range_i16"
  c_keys_in_range_i16_safe :: Ptr Int16 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall safe "hs_columnar_keys_in_range_i32"
  c_keys_in_range_i32_safe :: Ptr Int32 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall safe "hs_columnar_keys_in_range_i64"
  c_keys_in_range_i64_safe :: Ptr Int64 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall safe "hs_columnar_keys_in_range_u8"
  c_keys_in_range_u8_safe :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall safe "hs_columnar_keys_in_range_u16"
  c_keys_in_range_u16_safe :: Ptr Word16 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall safe "hs_columnar_keys_in_range_u32"
  c_keys_in_range_u32_safe :: Ptr Word32 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall safe "hs_columnar_keys_in_range_u64"
  c_keys_in_range_u64_safe :: Ptr Word64 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall safe "hs_columnar_copy_bits"
  c_copy_bits_safe :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CSize -> IO ()


foreign import ccall safe "hs_columnar_rebase_offsets_i32"
  c_rebase_offsets_i32_safe :: Ptr Int32 -> Ptr Int32 -> CSize -> Int64 -> IO ()


foreign import ccall safe "hs_columnar_rebase_offsets_i64"
  c_rebase_offsets_i64_safe :: Ptr Int64 -> Ptr Int64 -> CSize -> Int64 -> IO ()


foreign import ccall safe "hs_columnar_run_ends_i16"
  c_run_ends_i16_safe :: Ptr Int16 -> CSize -> Int64 -> IO Int64


foreign import ccall safe "hs_columnar_run_ends_i32"
  c_run_ends_i32_safe :: Ptr Int32 -> CSize -> Int64 -> IO Int64


foreign import ccall safe "hs_columnar_run_ends_i64"
  c_run_ends_i64_safe :: Ptr Int64 -> CSize -> Int64 -> IO Int64


foreign import ccall safe "hs_columnar_list_view_i32"
  c_list_view_i32_safe :: Ptr Int32 -> Ptr Int32 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall safe "hs_columnar_list_view_i64"
  c_list_view_i64_safe :: Ptr Int64 -> Ptr Int64 -> CSize -> Ptr Word8 -> CSize -> Int64 -> IO Int64


foreign import ccall safe "hs_columnar_dense_union"
  c_dense_union_safe :: Ptr Int8 -> Ptr Int32 -> CSize -> Ptr Int64 -> CSize -> IO Int64


foreign import ccall safe "hs_columnar_view_refs"
  c_view_refs_safe
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr Int64 -> CSize -> CInt -> IO Int64


foreign import ccall safe "hs_columnar_gather_1"
  c_gather_1_safe :: Ptr Word8 -> Ptr Word8 -> Ptr Int -> CSize -> IO ()


foreign import ccall safe "hs_columnar_gather_2"
  c_gather_2_safe :: Ptr Word8 -> Ptr Word8 -> Ptr Int -> CSize -> IO ()


foreign import ccall safe "hs_columnar_gather_4"
  c_gather_4_safe :: Ptr Word8 -> Ptr Word8 -> Ptr Int -> CSize -> IO ()


foreign import ccall safe "hs_columnar_gather_8"
  c_gather_8_safe :: Ptr Word8 -> Ptr Word8 -> Ptr Int -> CSize -> IO ()


foreign import ccall safe "hs_columnar_gather_16"
  c_gather_16_safe :: Ptr Word8 -> Ptr Word8 -> Ptr Int -> CSize -> IO ()


foreign import ccall safe "hs_columnar_gather_bits"
  c_gather_bits_safe :: Ptr Word8 -> Ptr Word8 -> CSize -> Ptr Int -> CSize -> IO Int64


foreign import ccall safe "hs_columnar_and_bits"
  c_and_bits_safe :: Ptr Word8 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CSize -> IO Int64


foreign import ccall safe "hs_columnar_take_offsets_i32"
  c_take_offsets_i32_safe :: Ptr Int32 -> Ptr Int32 -> Ptr Int -> CSize -> IO Int64


foreign import ccall safe "hs_columnar_take_offsets_i64"
  c_take_offsets_i64_safe :: Ptr Int64 -> Ptr Int64 -> Ptr Int -> CSize -> IO Int64


foreign import ccall safe "hs_columnar_take_bytes_i32"
  c_take_bytes_i32_safe :: Ptr Word8 -> Ptr Int32 -> Ptr Word8 -> Ptr Int -> CSize -> IO ()


foreign import ccall safe "hs_columnar_take_bytes_i64"
  c_take_bytes_i64_safe :: Ptr Word8 -> Ptr Int64 -> Ptr Word8 -> Ptr Int -> CSize -> IO ()


foreign import ccall unsafe "hs_columnar_split_bytes4"
  c_split_bytes4 :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()


foreign import ccall safe "hs_columnar_split_bytes4"
  c_split_bytes4_safe :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()


foreign import ccall unsafe "hs_columnar_split_bytes8"
  c_split_bytes8 :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()


foreign import ccall safe "hs_columnar_split_bytes8"
  c_split_bytes8_safe :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()


foreign import ccall unsafe "hs_columnar_join_bytes4"
  c_join_bytes4 :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()


foreign import ccall safe "hs_columnar_join_bytes4"
  c_join_bytes4_safe :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()


foreign import ccall unsafe "hs_columnar_join_bytes8"
  c_join_bytes8 :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()


foreign import ccall safe "hs_columnar_join_bytes8"
  c_join_bytes8_safe :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()


foreign import ccall unsafe "hs_columnar_split_bytes4_array"
  c_split_bytes4_array :: ByteArray# -> CSize -> Ptr Word8 -> CSize -> IO ()


foreign import ccall unsafe "hs_columnar_split_bytes8_array"
  c_split_bytes8_array :: ByteArray# -> CSize -> Ptr Word8 -> CSize -> IO ()


foreign import ccall unsafe "hs_columnar_join_bytes4"
  c_join_bytes4_array :: Ptr Word8 -> MutableByteArray# RealWorld -> CSize -> IO ()


foreign import ccall unsafe "hs_columnar_join_bytes8"
  c_join_bytes8_array :: Ptr Word8 -> MutableByteArray# RealWorld -> CSize -> IO ()


{- | Largest input, in bytes, a kernel reads through an @unsafe@ call.
The slowest kernels run at about 0.35 ns per byte here (UTF-8 boundary
checks), so 64 KiB keeps an unsafe call under roughly 25 us of
uninterruptible work; anything larger pays the ~100 ns safe-call overhead,
which is noise next to the work itself.
-}
unsafeCallBytes :: Int
unsafeCallBytes = 65536


-- | Whether a kernel reading this many bytes may use the @unsafe@ import.
small :: Int -> Bool
small bytes = bytes <= unsafeCallBytes
{-# INLINE small #-}


{- | Split @n@ four-byte values into four byte lanes. Arguments are source,
destination and value count. Both disjoint buffers cover @4*n@ bytes;
@n >= 0@ and @4*n@ must fit in 'Int'. Bytes within a value retain their
order, so callers handle host endianness. No pointer alignment is required.
-}
splitByteStreams4 :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
splitByteStreams4 src dst n =
  (if small (n * 4) then c_split_bytes4 else c_split_bytes4_safe) src dst (fromIntegral n)
{-# INLINE splitByteStreams4 #-}


-- | Like 'splitByteStreams4', for eight-byte values and @8*n@ buffer bytes.
splitByteStreams8 :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
splitByteStreams8 src dst n =
  (if small (n * 8) then c_split_bytes8 else c_split_bytes8_safe) src dst (fromIntegral n)
{-# INLINE splitByteStreams8 #-}


-- | Inverse of 'splitByteStreams4', with the same bounds and ownership contract.
joinByteStreams4 :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
joinByteStreams4 src dst n =
  (if small (n * 4) then c_join_bytes4 else c_join_bytes4_safe) src dst (fromIntegral n)
{-# INLINE joinByteStreams4 #-}


-- | Inverse of 'splitByteStreams8', with the same bounds and ownership contract.
joinByteStreams8 :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
joinByteStreams8 src dst n =
  (if small (n * 8) then c_join_bytes8 else c_join_bytes8_safe) src dst (fromIntegral n)
{-# INLINE joinByteStreams8 #-}


{- | Bounded split from an immutable array, which need not be pinned.
Arguments are source array, source byte offset, disjoint destination and
value count. The source range and 64 KiB unsafe-call limit are checked;
the caller supplies @4*n@ writable destination bytes. The unsafe FFI call
prevents the GC moving the source while C reads it.
-}
splitByteStreams4Array :: ByteArray -> Int -> Ptr Word8 -> Int -> IO ()
splitByteStreams4Array array@(ByteArray bytes) offset dst n
  | n < 0 || n > unsafeCallBytes `quot` 4 || offset < 0 || offset > sizeofByteArray array - n * 4 =
      error "Columnar.SIMD: invalid bounded four-byte split"
  | otherwise = c_split_bytes4_array bytes (fromIntegral offset) dst (fromIntegral n)
{-# INLINE splitByteStreams4Array #-}


-- | Like 'splitByteStreams4Array', with @8*n@ source and destination bytes.
splitByteStreams8Array :: ByteArray -> Int -> Ptr Word8 -> Int -> IO ()
splitByteStreams8Array array@(ByteArray bytes) offset dst n
  | n < 0 || n > unsafeCallBytes `quot` 8 || offset < 0 || offset > sizeofByteArray array - n * 8 =
      error "Columnar.SIMD: invalid bounded eight-byte split"
  | otherwise = c_split_bytes8_array bytes (fromIntegral offset) dst (fromIntegral n)
{-# INLINE splitByteStreams8Array #-}


{- | Bounded join into a mutable array, starting at byte zero. The array
need not be pinned; its size and the 64 KiB unsafe-call limit are checked.
The caller supplies @4*n@ readable source bytes, disjoint from the array,
and must not resize the array during the call.
-}
joinByteStreams4Array :: Ptr Word8 -> MutableByteArray RealWorld -> Int -> IO ()
joinByteStreams4Array src array@(MutableByteArray bytes) n
  | n < 0 || n > unsafeCallBytes `quot` 4 =
      error "Columnar.SIMD: invalid bounded four-byte join"
  | otherwise = do
      size <- getSizeofMutableByteArray array
      if n > size `quot` 4
        then error "Columnar.SIMD: invalid bounded four-byte join"
        else c_join_bytes4_array src bytes (fromIntegral n)
{-# INLINE joinByteStreams4Array #-}


-- | Like 'joinByteStreams4Array', with @8*n@ source and destination bytes.
joinByteStreams8Array :: Ptr Word8 -> MutableByteArray RealWorld -> Int -> IO ()
joinByteStreams8Array src array@(MutableByteArray bytes) n
  | n < 0 || n > unsafeCallBytes `quot` 8 =
      error "Columnar.SIMD: invalid bounded eight-byte join"
  | otherwise = do
      size <- getSizeofMutableByteArray array
      if n > size `quot` 8
        then error "Columnar.SIMD: invalid bounded eight-byte join"
        else c_join_bytes8_array src bytes (fromIntegral n)
{-# INLINE joinByteStreams8Array #-}


{- | A packed unsigned delta is added to the frame with 32-bit wrapping,
reinterpreted as signed, converted to 'Float', then multiplied by the factor
and scale in two separate, correctly rounded operations.
-}
data PackedFloat32 = PackedFloat32
  { packedFloat32Width :: {-# UNPACK #-} !Int
  , packedFloat32Frame :: {-# UNPACK #-} !Word32
  , packedFloat32Factor :: {-# UNPACK #-} !Float
  , packedFloat32Scale :: {-# UNPACK #-} !Float
  }


-- | The 64-bit integer and 'Double' counterpart of 'PackedFloat32'.
data PackedFloat64 = PackedFloat64
  { packedFloat64Width :: {-# UNPACK #-} !Int
  , packedFloat64Frame :: {-# UNPACK #-} !Word64
  , packedFloat64Factor :: {-# UNPACK #-} !Double
  , packedFloat64Scale :: {-# UNPACK #-} !Double
  }


foreign import ccall unsafe "hs_columnar_unpack_for_f32"
  c_unpack_for_f32 :: Ptr Word8 -> CSize -> MutableByteArray# RealWorld -> CSize -> CSize -> CSize -> CUInt -> Word32 -> CFloat -> CFloat -> IO ()


foreign import ccall unsafe "hs_columnar_unpack_for_f64"
  c_unpack_for_f64 :: Ptr Word8 -> CSize -> MutableByteArray# RealWorld -> CSize -> CSize -> CSize -> CUInt -> Word64 -> CDouble -> CDouble -> IO ()


foreign import ccall unsafe "hs_columnar_scatter_le_f32"
  c_scatter_le_f32 :: Ptr Word8 -> Ptr Word8 -> MutableByteArray# RealWorld -> CSize -> CSize -> IO ()


foreign import ccall unsafe "hs_columnar_scatter_le_f64"
  c_scatter_le_f64 :: Ptr Word8 -> Ptr Word8 -> MutableByteArray# RealWorld -> CSize -> CSize -> IO ()


{- | Unpack @count@ LSB-first values starting at packed row @first@ into
the mutable array at the given byte offset. Arguments are frame, source
pointer, readable source byte length, destination array, destination byte
offset, first packed row and count. Source and destination must be disjoint;
the caller owns the readable source range. No alignment is required.

The source bit range, current destination size, and 64 KiB output limit are
checked. The destination may be movable: the bounded unsafe FFI call prevents
GC during C's writes. The caller must not resize it concurrently.
-}
unpackFloat32Array :: PackedFloat32 -> Ptr Word8 -> Int -> MutableByteArray RealWorld -> Int -> Int -> Int -> IO ()
unpackFloat32Array (PackedFloat32 width frame factor scale) src len array@(MutableByteArray bytes) offset first count
  | not (packedFloatBounds 4 width len first count) = error "Columnar.SIMD: invalid packed Float range"
  | otherwise = do
      checkFloatDestination 4 array offset count
      c_unpack_for_f32 src (fromIntegral len) bytes (fromIntegral offset) (fromIntegral first) (fromIntegral count) (fromIntegral width) frame (CFloat factor) (CFloat scale)
{-# INLINE unpackFloat32Array #-}


-- | Like 'unpackFloat32Array', with 64-bit wrapping and native 'Double' output.
unpackFloat64Array :: PackedFloat64 -> Ptr Word8 -> Int -> MutableByteArray RealWorld -> Int -> Int -> Int -> IO ()
unpackFloat64Array (PackedFloat64 width frame factor scale) src len array@(MutableByteArray bytes) offset first count
  | not (packedFloatBounds 8 width len first count) = error "Columnar.SIMD: invalid packed Double range"
  | otherwise = do
      checkFloatDestination 8 array offset count
      c_unpack_for_f64 src (fromIntegral len) bytes (fromIntegral offset) (fromIntegral first) (fromIntegral count) (fromIntegral width) frame (CDouble factor) (CDouble scale)
{-# INLINE unpackFloat64Array #-}


packedFloatBounds :: Int -> Int -> Int -> Int -> Int -> Bool
packedFloatBounds physical width len first count
  | width < 0 || width > physical * 8 || len < 0 || first < 0 || count < 0 = False
  | count > unsafeCallBytes `quot` physical || first > maxBound - count = False
  -- An exact double-width product checks both unsigned overflow and the
  -- signed Int limit. A division-based bound makes GHC float a boxed quotient
  -- out of callers' loops, allocating once per packed vector.
  | otherwise = case width of
      I# width# -> case first + count of
        I# end# -> case (maxBound :: Int) of
          I# limit# -> case timesWord2# (int2Word# width#) (int2Word# end#) of
            (# high#, bits# #) ->
              if isTrue# (neWord# high# 0##) || isTrue# (gtWord# bits# (int2Word# limit#))
                then False
                else
                  let !whole = I# (word2Int# (uncheckedShiftRL# bits# 3#))
                      !extra = if isTrue# (eqWord# (and# bits# 7##) 0##) then 0 else 1
                  in whole + extra <= len
{-# INLINE packedFloatBounds #-}


checkFloatDestination :: Int -> MutableByteArray RealWorld -> Int -> Int -> IO ()
checkFloatDestination physical array offset rows
  | offset < 0 || rows < 0 = error "Columnar.SIMD: invalid floating destination"
  | otherwise = do
      size <- getSizeofMutableByteArray array
      if offset > size || rows > (size - offset) `quot` physical
        then error "Columnar.SIMD: floating destination too small"
        else pure ()
{-# INLINE checkFloatDestination #-}


{- | Scatter little-endian IEEE bits without floating-point arithmetic.
Arguments are uint16 little-endian positions, little-endian value bytes,
destination array, destination byte offset, destination row limit and count.
The caller supplies disjoint readable sources of @2*count@ and @4*count@
bytes, and must have validated every position as less than the row limit.
Positions may repeat or be unordered; later entries overwrite earlier ones.

The current destination extent and combined 64 KiB source limit are checked.
The movable destination must not be resized concurrently. The call preserves
NaN payloads and signed zero, and requires no pointer alignment.
-}
scatterFloat32Array :: Ptr Word8 -> Ptr Word8 -> MutableByteArray RealWorld -> Int -> Int -> Int -> IO ()
scatterFloat32Array positions values array@(MutableByteArray bytes) offset rows count
  | count < 0 || count > unsafeCallBytes `quot` 6 || (rows == 0 && count > 0) = error "Columnar.SIMD: invalid Float scatter count"
  | otherwise = do
      checkFloatDestination 4 array offset rows
      c_scatter_le_f32 positions values bytes (fromIntegral offset) (fromIntegral count)
{-# INLINE scatterFloat32Array #-}


-- | Like 'scatterFloat32Array', with eight-byte IEEE values and @10*count@ source bytes.
scatterFloat64Array :: Ptr Word8 -> Ptr Word8 -> MutableByteArray RealWorld -> Int -> Int -> Int -> IO ()
scatterFloat64Array positions values array@(MutableByteArray bytes) offset rows count
  | count < 0 || count > unsafeCallBytes `quot` 10 || (rows == 0 && count > 0) = error "Columnar.SIMD: invalid Double scatter count"
  | otherwise = do
      checkFloatDestination 8 array offset rows
      c_scatter_le_f64 positions values bytes (fromIntegral offset) (fromIntegral count)
{-# INLINE scatterFloat64Array #-}


-- | Count set bits in a byte range (entire bytes).
bitmapPopCount :: BS.ByteString -> Int
bitmapPopCount bs
  | BS.length bs == 0 = 0
  | otherwise =
      fromIntegral $
        unsafePerformIO $
          unsafeUseAsCStringLen bs $ \(p, len) ->
            pure $!
              if small len
                then c_bitmap_popcount (castPtr p) (fromIntegral len)
                else c_bitmap_popcount_safe (castPtr p) (fromIntegral len)


{- | Expand @n@ LSB-first packed bits (Arrow / Parquet bool layout) into a
boxed 'Bool' vector.

The bit count is a non-negative 'Int' and reaches C as a 'CSize', so a
count above @maxBound :: Int32@ (2^31 bits, which is 256 MiB of output) is
no longer truncated. A non-positive count expands to an empty vector.
-}
unpackBitsLsbUnsafe :: Int -> BS.ByteString -> V.Vector Bool
unpackBitsLsbUnsafe !n bs
  | n <= 0 = V.empty
  | otherwise = unsafeDupablePerformIO $ do
      mvs <- VSM.unsafeNew n
      VSM.unsafeWith mvs $ \dst ->
        unsafeUseAsCStringLen bs $ \(src, _) ->
          c_unpack_bits_lsb (castPtr src) (fromIntegral n) dst
      sv <- VS.unsafeFreeze mvs
      pure $! V.convert (VU.map (/= (0 :: Word8)) (VU.convert sv))


{- | Bulk copy (SIMDe 16-byte chunks inside C). @dst@ must be at least @len@
bytes; only @len@ bytes are written. @len@ is a byte count and reaches C as
a 'CSize', so a copy above @maxBound :: Int32@ is no longer truncated; a
non-positive @len@ copies nothing.
-}
memcpyFast :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memcpyFast _ _ len | len <= 0 = pure ()
memcpyFast !dst !src !len
  | small len = c_memcpy_fast src dst (fromIntegral len)
  | otherwise = c_memcpy_fast_safe src dst (fromIntegral len)


-- | Offsets check: @offs@, number of offsets (rows + 1), limit.
offsetsCheckI32 :: Ptr Int32 -> Int -> Int64 -> IO Int
offsetsCheckI32 offs n limit =
  fromIntegral <$> (if small (n * 4) then c_offsets_i32 else c_offsets_i32_safe) offs (fromIntegral n) limit
{-# INLINE offsetsCheckI32 #-}


-- | Offsets check: @offs@, number of offsets (rows + 1), limit.
offsetsCheckI64 :: Ptr Int64 -> Int -> Int64 -> IO Int
offsetsCheckI64 offs n limit =
  fromIntegral <$> (if small (n * 8) then c_offsets_i64 else c_offsets_i64_safe) offs (fromIntegral n) limit
{-# INLINE offsetsCheckI64 #-}


-- | Character boundary check: @offs@, number of offsets, data, data length.
utf8BoundariesI32 :: Ptr Int32 -> Int -> Ptr Word8 -> Int -> IO Int
utf8BoundariesI32 offs n dat len =
  fromIntegral
    <$> (if small (n * 4) then c_utf8_boundaries_i32 else c_utf8_boundaries_i32_safe)
      offs
      (fromIntegral n)
      dat
      (fromIntegral len)
{-# INLINE utf8BoundariesI32 #-}


-- | Character boundary check: @offs@, number of offsets, data, data length.
utf8BoundariesI64 :: Ptr Int64 -> Int -> Ptr Word8 -> Int -> IO Int
utf8BoundariesI64 offs n dat len =
  fromIntegral
    <$> (if small (n * 8) then c_utf8_boundaries_i64 else c_utf8_boundaries_i64_safe)
      offs
      (fromIntegral n)
      dat
      (fromIntegral len)
{-# INLINE utf8BoundariesI64 #-}


-- | Set bits in a bit range: buffer, bit offset, number of bits.
popCountBits :: Ptr Word8 -> Int -> Int -> IO Int
popCountBits buf bitoff nbits =
  fromIntegral
    <$> (if small (nbits `quot` 8) then c_popcount_bits else c_popcount_bits_safe)
      buf
      (fromIntegral bitoff)
      (fromIntegral nbits)
{-# INLINE popCountBits #-}


-- | Dictionary key check: keys, rows, validity (or 'nullPtr'), validity bit offset, max.
keysInRangeI8 :: Ptr Int8 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
keysInRangeI8 keys n valid validoff mx =
  fromIntegral <$> (if small n then c_keys_in_range_i8 else c_keys_in_range_i8_safe) keys (fromIntegral n) valid (fromIntegral validoff) mx
{-# INLINE keysInRangeI8 #-}


-- | See 'keysInRangeI8'.
keysInRangeI16 :: Ptr Int16 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
keysInRangeI16 keys n valid validoff mx =
  fromIntegral <$> (if small (n * 2) then c_keys_in_range_i16 else c_keys_in_range_i16_safe) keys (fromIntegral n) valid (fromIntegral validoff) mx
{-# INLINE keysInRangeI16 #-}


-- | See 'keysInRangeI8'.
keysInRangeI32 :: Ptr Int32 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
keysInRangeI32 keys n valid validoff mx =
  fromIntegral <$> (if small (n * 4) then c_keys_in_range_i32 else c_keys_in_range_i32_safe) keys (fromIntegral n) valid (fromIntegral validoff) mx
{-# INLINE keysInRangeI32 #-}


-- | See 'keysInRangeI8'.
keysInRangeI64 :: Ptr Int64 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
keysInRangeI64 keys n valid validoff mx =
  fromIntegral <$> (if small (n * 8) then c_keys_in_range_i64 else c_keys_in_range_i64_safe) keys (fromIntegral n) valid (fromIntegral validoff) mx
{-# INLINE keysInRangeI64 #-}


-- | See 'keysInRangeI8'.
keysInRangeU8 :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
keysInRangeU8 keys n valid validoff mx =
  fromIntegral <$> (if small n then c_keys_in_range_u8 else c_keys_in_range_u8_safe) keys (fromIntegral n) valid (fromIntegral validoff) mx
{-# INLINE keysInRangeU8 #-}


-- | See 'keysInRangeI8'.
keysInRangeU16 :: Ptr Word16 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
keysInRangeU16 keys n valid validoff mx =
  fromIntegral <$> (if small (n * 2) then c_keys_in_range_u16 else c_keys_in_range_u16_safe) keys (fromIntegral n) valid (fromIntegral validoff) mx
{-# INLINE keysInRangeU16 #-}


-- | See 'keysInRangeI8'.
keysInRangeU32 :: Ptr Word32 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
keysInRangeU32 keys n valid validoff mx =
  fromIntegral <$> (if small (n * 4) then c_keys_in_range_u32 else c_keys_in_range_u32_safe) keys (fromIntegral n) valid (fromIntegral validoff) mx
{-# INLINE keysInRangeU32 #-}


-- | See 'keysInRangeI8'.
keysInRangeU64 :: Ptr Word64 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
keysInRangeU64 keys n valid validoff mx =
  fromIntegral <$> (if small (n * 8) then c_keys_in_range_u64 else c_keys_in_range_u64_safe) keys (fromIntegral n) valid (fromIntegral validoff) mx
{-# INLINE keysInRangeU64 #-}


-- | Bit copy: dst, dst bit offset, src, src bit offset, number of bits.
copyBits :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Int -> IO ()
copyBits dst dstoff src srcoff nbits =
  (if small (nbits `quot` 8) then c_copy_bits else c_copy_bits_safe)
    dst
    (fromIntegral dstoff)
    src
    (fromIntegral srcoff)
    (fromIntegral nbits)
{-# INLINE copyBits #-}


-- | Offset rebase: dst, src, count, delta.
rebaseOffsetsI32 :: Ptr Int32 -> Ptr Int32 -> Int -> Int64 -> IO ()
rebaseOffsetsI32 dst src n delta =
  (if small (n * 4) then c_rebase_offsets_i32 else c_rebase_offsets_i32_safe) dst src (fromIntegral n) delta
{-# INLINE rebaseOffsetsI32 #-}


-- | Offset rebase: dst, src, count, delta.
rebaseOffsetsI64 :: Ptr Int64 -> Ptr Int64 -> Int -> Int64 -> IO ()
rebaseOffsetsI64 dst src n delta =
  (if small (n * 8) then c_rebase_offsets_i64 else c_rebase_offsets_i64_safe) dst src (fromIntegral n) delta
{-# INLINE rebaseOffsetsI64 #-}


-- | Run-end check: ends, count, minimum last end.
runEndsCheckI16 :: Ptr Int16 -> Int -> Int64 -> IO Int
runEndsCheckI16 ends n minEnd =
  fromIntegral <$> (if small (n * 2) then c_run_ends_i16 else c_run_ends_i16_safe) ends (fromIntegral n) minEnd
{-# INLINE runEndsCheckI16 #-}


-- | Run-end check: ends, count, minimum last end.
runEndsCheckI32 :: Ptr Int32 -> Int -> Int64 -> IO Int
runEndsCheckI32 ends n minEnd =
  fromIntegral <$> (if small (n * 4) then c_run_ends_i32 else c_run_ends_i32_safe) ends (fromIntegral n) minEnd
{-# INLINE runEndsCheckI32 #-}


-- | Run-end check: ends, count, minimum last end.
runEndsCheckI64 :: Ptr Int64 -> Int -> Int64 -> IO Int
runEndsCheckI64 ends n minEnd =
  fromIntegral <$> (if small (n * 8) then c_run_ends_i64 else c_run_ends_i64_safe) ends (fromIntegral n) minEnd
{-# INLINE runEndsCheckI64 #-}


-- | List view check: offsets, sizes, rows, validity (or 'nullPtr'), validity bit offset, child length.
listViewCheckI32 :: Ptr Int32 -> Ptr Int32 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
listViewCheckI32 offs sizes n valid validoff childLen =
  fromIntegral
    <$> (if small (n * 8) then c_list_view_i32 else c_list_view_i32_safe)
      offs
      sizes
      (fromIntegral n)
      valid
      (fromIntegral validoff)
      childLen
{-# INLINE listViewCheckI32 #-}


-- | See 'listViewCheckI32'.
listViewCheckI64 :: Ptr Int64 -> Ptr Int64 -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int
listViewCheckI64 offs sizes n valid validoff childLen =
  fromIntegral
    <$> (if small (n * 16) then c_list_view_i64 else c_list_view_i64_safe)
      offs
      sizes
      (fromIntegral n)
      valid
      (fromIntegral validoff)
      childLen
{-# INLINE listViewCheckI64 #-}


-- | Dense union check: type ids, offsets, rows, child lengths, number of children.
denseUnionCheck :: Ptr Int8 -> Ptr Int32 -> Int -> Ptr Int64 -> Int -> IO Int
denseUnionCheck types offs n childLens nchildren =
  fromIntegral
    <$> (if small (n * 5) then c_dense_union else c_dense_union_safe)
      types
      offs
      (fromIntegral n)
      childLens
      (fromIntegral nchildren)
{-# INLINE denseUnionCheck #-}


{- | View check: views, rows, validity (or 'nullPtr'), validity bit offset,
data buffer pointers, data buffer lengths, number of buffers, UTF-8 flag.
-}
viewRefsCheck :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr (Ptr Word8) -> Ptr Int64 -> Int -> Bool -> IO Int
viewRefsCheck views n valid validoff bufs buflens nbufs utf8 =
  fromIntegral
    <$> (if not utf8 && small (n * 16) then c_view_refs else c_view_refs_safe)
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
gather1 dst src idx n = (if small (n * 9) then c_gather_1 else c_gather_1_safe) dst src idx (fromIntegral n)
{-# INLINE gather1 #-}


-- | Fixed-width gather of 2-byte elements: dst, src, indices, count.
gather2 :: Ptr Word8 -> Ptr Word8 -> Ptr Int -> Int -> IO ()
gather2 dst src idx n = (if small (n * 10) then c_gather_2 else c_gather_2_safe) dst src idx (fromIntegral n)
{-# INLINE gather2 #-}


-- | Fixed-width gather of 4-byte elements: dst, src, indices, count.
gather4 :: Ptr Word8 -> Ptr Word8 -> Ptr Int -> Int -> IO ()
gather4 dst src idx n = (if small (n * 12) then c_gather_4 else c_gather_4_safe) dst src idx (fromIntegral n)
{-# INLINE gather4 #-}


-- | Fixed-width gather of 8-byte elements: dst, src, indices, count.
gather8 :: Ptr Word8 -> Ptr Word8 -> Ptr Int -> Int -> IO ()
gather8 dst src idx n = (if small (n * 16) then c_gather_8 else c_gather_8_safe) dst src idx (fromIntegral n)
{-# INLINE gather8 #-}


-- | Fixed-width gather of 16-byte elements: dst, src, indices, count.
gather16 :: Ptr Word8 -> Ptr Word8 -> Ptr Int -> Int -> IO ()
gather16 dst src idx n = (if small (n * 24) then c_gather_16 else c_gather_16_safe) dst src idx (fromIntegral n)
{-# INLINE gather16 #-}


-- | Bit gather: dst, src, src bit offset, indices, count. Returns the set-bit count.
gatherBits :: Ptr Word8 -> Ptr Word8 -> Int -> Ptr Int -> Int -> IO Int
gatherBits dst src srcoff idx n =
  fromIntegral
    <$> (if small (n * 8) then c_gather_bits else c_gather_bits_safe)
      dst
      src
      (fromIntegral srcoff)
      idx
      (fromIntegral n)
{-# INLINE gatherBits #-}


-- | Bitwise AND: dst, a, a bit offset, b, b bit offset, number of bits. Returns the set-bit count.
andBits :: Ptr Word8 -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> Int -> IO Int
andBits dst a aoff b boff nbits =
  fromIntegral
    <$> (if small (nbits `quot` 4) then c_and_bits else c_and_bits_safe)
      dst
      a
      (fromIntegral aoff)
      b
      (fromIntegral boff)
      (fromIntegral nbits)
{-# INLINE andBits #-}


-- | Take pass 1: dst offsets, src offsets, indices, count. Returns the total or -1.
takeOffsetsI32 :: Ptr Int32 -> Ptr Int32 -> Ptr Int -> Int -> IO Int
takeOffsetsI32 dstOffs srcOffs idx n =
  fromIntegral <$> (if small (n * 8) then c_take_offsets_i32 else c_take_offsets_i32_safe) dstOffs srcOffs idx (fromIntegral n)
{-# INLINE takeOffsetsI32 #-}


-- | See 'takeOffsetsI32'.
takeOffsetsI64 :: Ptr Int64 -> Ptr Int64 -> Ptr Int -> Int -> IO Int
takeOffsetsI64 dstOffs srcOffs idx n =
  fromIntegral <$> (if small (n * 8) then c_take_offsets_i64 else c_take_offsets_i64_safe) dstOffs srcOffs idx (fromIntegral n)
{-# INLINE takeOffsetsI64 #-}


{- | Take pass 2: dst bytes, src offsets, src bytes, indices, count. Always a
safe call: the bytes copied are not bounded by the row count.
-}
takeBytesI32 :: Ptr Word8 -> Ptr Int32 -> Ptr Word8 -> Ptr Int -> Int -> IO ()
takeBytesI32 dst srcOffs src idx n = c_take_bytes_i32_safe dst srcOffs src idx (fromIntegral n)
{-# INLINE takeBytesI32 #-}


-- | See 'takeBytesI32'.
takeBytesI64 :: Ptr Word8 -> Ptr Int64 -> Ptr Word8 -> Ptr Int -> Int -> IO ()
takeBytesI64 dst srcOffs src idx n = c_take_bytes_i64_safe dst srcOffs src idx (fromIntegral n)
{-# INLINE takeBytesI64 #-}
