{-# LANGUAGE BangPatterns #-}

{- | Encoder side of @DELTA_BINARY_PACKED@.

The reader's "Parquet.Delta" handles decode; this module produces
bytes the reader accepts:

@
ULEB128 block_size
ULEB128 num_miniblocks_in_block
ULEB128 total_value_count
ZigzagLEB128 first_value

per block (until total_value_count consumed):
  ZigzagLEB128 min_delta
  uint8 bit_width[num_miniblocks_in_block]
  per miniblock:
    ceil(miniblock_size * bit_width / 8) bytes of LSB-packed (delta - min_delta)
@

We default @block_size = 128@ and @num_miniblocks = 4@ (so each
miniblock holds 32 values), matching parquet-mr's writer config.
-}
module Parquet.DeltaEncode (
  encodeDeltaBinaryPackedInt32,
  encodeDeltaBinaryPackedInt64,
  encodeDeltaBinaryPackedRaw,
  encodeDeltaLengthByteArray,
  encodeDeltaByteArray,
) where

import Data.Bits (countLeadingZeros, shiftL, shiftR, unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder.Prim qualified as Prim
import Data.ByteString.Builder.Prim.Internal qualified as PrimInternal
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32, Int64)
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Data.Word (Word32, Word64)
import Foreign.Ptr (plusPtr)
import Wireform.Builder qualified as B


defaultBlockSize, defaultNumMiniblocks :: Int
defaultBlockSize = 128
defaultNumMiniblocks = 4


{- | DELTA_BINARY_PACKED for INT32. Differences wrap at the physical
32-bit width before promotion; widening the inputs first can produce
33-bit miniblocks and min deltas that an INT32 reader cannot consume.
-}
encodeDeltaBinaryPackedInt32 :: VP.Vector Int32 -> ByteString
encodeDeltaBinaryPackedInt32 vs
  | VP.null vs = encodeDeltaStream 0 0 (const 0)
  | otherwise =
      encodeDeltaStream
        (VP.length vs)
        (fromIntegral (VP.unsafeIndex vs 0))
        (\i -> fromIntegral (VP.unsafeIndex vs (i + 1) - VP.unsafeIndex vs i))


-- | DELTA_BINARY_PACKED for INT64.
encodeDeltaBinaryPackedInt64 :: VP.Vector Int64 -> ByteString
encodeDeltaBinaryPackedInt64 = encodeDeltaBinaryPackedRaw


-- | Lower-level raw encoder.
encodeDeltaBinaryPackedRaw :: VP.Vector Int64 -> ByteString
encodeDeltaBinaryPackedRaw vs
  | VP.null vs = encodeDeltaStream 0 0 (const 0)
  | otherwise = encodeDeltaStream (VP.length vs) (VP.unsafeIndex vs 0) (\i -> VP.unsafeIndex vs (i + 1) - VP.unsafeIndex vs i)


-- Keep the physical-width subtraction in the typed caller. Inlining this
-- pipeline removes the accessor closure from the block's scanning/packing
-- loops and avoids materialising an input-sized vector of Int64 deltas.
encodeDeltaStream :: Int -> Int64 -> (Int -> Int64) -> ByteString
encodeDeltaStream n firstVal deltaAt =
  let !header =
        encodeULeb (fromIntegral defaultBlockSize)
          <> encodeULeb (fromIntegral defaultNumMiniblocks)
          <> encodeULeb (fromIntegral n)
      !blocks = encodeBlocks (max 0 (n - 1)) deltaAt
  in BL.toStrict (B.toLazyByteString (header <> encodeZigzagLeb firstVal <> blocks))
{-# INLINE encodeDeltaStream #-}


encodeBlocks :: Int -> (Int -> Int64) -> B.Builder
encodeBlocks n deltaAt = go 0
  where
    go !off
      | off >= n = mempty
      | otherwise =
          encodeOneBlock off (min defaultBlockSize (n - off)) deltaAt
            <> go (off + defaultBlockSize)
{-# INLINE encodeBlocks #-}


encodeOneBlock :: Int -> Int -> (Int -> Int64) -> B.Builder
encodeOneBlock off count deltaAt =
  let !miniblockSize = defaultBlockSize `quot` defaultNumMiniblocks
      minimumFrom !i !smallest
        | i == count = smallest
        | otherwise = minimumFrom (i + 1) (min smallest (deltaAt (off + i)))
      !minDelta = minimumFrom 1 (deltaAt off)
      payload !start !width =
        encodeMiniblockPayload
          minDelta
          miniblockSize
          width
          (max 0 (min miniblockSize (count - start)))
          (\i -> deltaAt (off + start + i))
      widthAt !start =
        let !end = min count (start + miniblockSize)
            maximumFrom !i !largest
              | i >= end = bitWidthW64 largest
              | otherwise = maximumFrom (i + 1) (max largest (fromIntegral (deltaAt (off + i) - minDelta)))
        in maximumFrom start 0
      !width0 = widthAt 0
      !width1 = widthAt miniblockSize
      !width2 = widthAt (2 * miniblockSize)
      !width3 = widthAt (3 * miniblockSize)
  in encodeZigzagLeb minDelta
       <> B.word8 (fromIntegral width0)
       <> B.word8 (fromIntegral width1)
       <> B.word8 (fromIntegral width2)
       <> B.word8 (fromIntegral width3)
       <> payload 0 width0
       <> payload miniblockSize width1
       <> payload (2 * miniblockSize) width2
       <> payload (3 * miniblockSize) width3
{-# INLINE encodeOneBlock #-}


bitWidthW64 :: Word64 -> Int
bitWidthW64 value = 64 - countLeadingZeros value


-- Serialise the miniblock's packed bits. Always emits
-- @ceil(miniblockSize * bw / 8)@ bytes (i.e. uses the *spec* miniblock
-- size, not the actual chunk length). Trailing values past the end of
-- the actual miniblock length are zero-padded.
encodeMiniblockPayload :: Int64 -> Int -> Int -> Int -> (Int -> Int64) -> B.Builder
encodeMiniblockPayload _minDelta _miniblockSize 0 _ _ = mempty
encodeMiniblockPayload minDelta miniblockSize bw n deltaAt =
  let !totalBits = miniblockSize * bw
      !totalBytes = (totalBits + 7) `shiftR` 3
  in B.primFixed (PrimInternal.fixedPrim totalBytes fill) ()
  where
    valueAt :: Int -> Word64
    valueAt i = if i < n then fromIntegral (deltaAt i - minDelta) else 0
    {-# INLINE valueAt #-}
    -- FixedPrim reserves the whole miniblock once. Its standard LE stores
    -- support unaligned builder cursors and write exactly four/eight bytes.
    fill () output
      | bw == 64 = words64 0
      | bw == 32 = words32 0
      | otherwise = packed 0 0 0 0
      where
        words64 !i
          | i == miniblockSize = pure ()
          | otherwise = PrimInternal.runF Prim.word64LE (valueAt i) (output `plusPtr` (i * 8)) *> words64 (i + 1)
        words32 !i
          | i == miniblockSize = pure ()
          | otherwise = PrimInternal.runF Prim.word32LE (fromIntegral (valueAt i)) (output `plusPtr` (i * 4)) *> words32 (i + 1)
        packed !i !offset !used !acc
          | i == miniblockSize = tailBytes offset used acc
          | otherwise =
              let !value = valueAt i
                  !merged = acc .|. (value `unsafeShiftL` used)
                  !bits = used + bw
              in if bits < 64
                   then packed (i + 1) offset bits merged
                   else do
                     PrimInternal.runF Prim.word64LE merged (output `plusPtr` offset)
                     -- used stays in [0,63]. A positive carry implies
                     -- used > 0, so the remaining shift is also in [1,63].
                     let !carry = if bits == 64 then 0 else value `unsafeShiftR` (64 - used)
                     packed (i + 1) (offset + 8) (bits - 64) carry
        tailBytes !offset !used !acc
          | used == 0 = pure ()
          | used == 32 = PrimInternal.runF Prim.word32LE (fromIntegral acc) (output `plusPtr` offset)
          | otherwise = bytes 0
          where
            bytes !i
              | i == (used + 7) `quot` 8 = pure ()
              | otherwise = PrimInternal.runF Prim.word8 (fromIntegral (acc `unsafeShiftR` (i * 8))) (output `plusPtr` (offset + i)) *> bytes (i + 1)
{-# INLINE encodeMiniblockPayload #-}


-- ============================================================
-- ULEB128 / zigzag
-- ============================================================

encodeULeb :: Word64 -> B.Builder
encodeULeb = go
  where
    go !n
      | n < 0x80 = B.word8 (fromIntegral n)
      | otherwise =
          B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80)
            <> go (n `shiftR` 7)


encodeZigzagLeb :: Int64 -> B.Builder
encodeZigzagLeb v =
  let !u =
        if v >= 0
          then fromIntegral (v `shiftL` 1) :: Word64
          else fromIntegral (negate v `shiftL` 1 - 1)
  in encodeULeb u


-- ============================================================
-- DELTA_LENGTH_BYTE_ARRAY
-- ============================================================

{- | Encode a vector of byte arrays as @DELTA_LENGTH_BYTE_ARRAY@ (Parquet
encoding 6).

Layout:

@
  <DELTA_BINARY_PACKED of all lengths (Int32)>
  <concatenated value bytes>
@

The total length / value count of the lengths block matches the input
vector's length, and value bytes are written in input order with no
separators or length prefixes (the lengths block is the index).
-}
encodeDeltaLengthByteArray :: V.Vector ByteString -> ByteString
encodeDeltaLengthByteArray vs =
  let !lens = VP.fromList [fromIntegral (BS.length b) :: Int32 | b <- V.toList vs]
      !lengthsEncoded = encodeDeltaBinaryPackedInt32 lens
      !concatenated =
        BL.toStrict
          ( B.toLazyByteString
              (V.foldl' (\acc b -> acc <> B.byteString b) mempty vs)
          )
  in lengthsEncoded <> concatenated


-- ============================================================
-- DELTA_BYTE_ARRAY (incremental encoding)
-- ============================================================

{- | Encode a vector of byte arrays as @DELTA_BYTE_ARRAY@ (Parquet
encoding 7), aka /incremental encoding/.

Layout:

@
  <DELTA_BINARY_PACKED of prefix lengths (Int32)>
  <DELTA_BINARY_PACKED of suffix lengths (Int32)>
  <concatenated suffix bytes>
@

The /prefix length/ is the number of leading bytes shared with the
previous element (always 0 for the first element); the /suffix/ is
the trailing bytes that differ. Reassembly is
@value[i] = value[i-1][:prefix[i]] <> suffix[i]@.

This is the encoding parquet-mr uses for sorted string columns.
-}
encodeDeltaByteArray :: V.Vector ByteString -> ByteString
encodeDeltaByteArray vs =
  let (prefixLens, suffixes) = computePrefixesSuffixes vs
      !prefixVec = VP.fromList prefixLens
      !suffixLensVec = VP.fromList [fromIntegral (BS.length s) :: Int32 | s <- suffixes]
      !prefixesEncoded = encodeDeltaBinaryPackedInt32 prefixVec
      !suffixLensEncoded = encodeDeltaBinaryPackedInt32 suffixLensVec
      !suffixBytes =
        BL.toStrict
          ( B.toLazyByteString
              (foldMap B.byteString suffixes)
          )
  in prefixesEncoded <> suffixLensEncoded <> suffixBytes


-- | Walk the input vector once computing each (prefixLen, suffix) pair.
computePrefixesSuffixes :: V.Vector ByteString -> ([Int32], [ByteString])
computePrefixesSuffixes vs = go 0 BS.empty [] []
  where
    n = V.length vs
    go !i !prev accLens accSufs
      | i >= n = (reverse accLens, reverse accSufs)
      | otherwise =
          let !cur = V.unsafeIndex vs i
              !pfx = commonPrefixLen prev cur
              !suf = BS.drop pfx cur
          in go (i + 1) cur (fromIntegral pfx : accLens) (suf : accSufs)


commonPrefixLen :: ByteString -> ByteString -> Int
commonPrefixLen a b =
  let !maxN = min (BS.length a) (BS.length b)
      go !i
        | i >= maxN = i
        | BS.index a i /= BS.index b i = i
        | otherwise = go (i + 1)
  in go 0


-- Suppress unused-import warning if 'Word32' isn't referenced directly.
_unusedW32 :: Word32 -> Word32
_unusedW32 = id
