{-# LANGUAGE BangPatterns #-}
-- | Encoder side of Parquet definition / repetition levels.
--
-- The reader's "Parquet.Levels" handles the (length-prefixed RLE-hybrid)
-- decode path; this module emits compatible bytes. Bit-width 0 streams
-- are entirely omitted from the page body, matching the spec.
--
-- We use the BIT_PACKED-only sub-encoding when the bit width is small,
-- because parquet-mr accepts both BIT_PACKED and RLE within the same
-- stream. For simplicity (and to match the reader on this codebase) we
-- emit a single RLE-encoded run when all values are equal, and a single
-- BIT_PACKED run otherwise.
module Parquet.LevelsEncode
  ( encodeLengthPrefixedHybrid
  , encodeRLEHybrid
  , bitWidthFor
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BL
import Foreign.Storable (pokeByteOff)
import Data.Int (Int32)
import qualified Data.Vector.Primitive as VP
import Data.Word (Word32, Word8)

-- | The bit width needed to represent values in @[0, n]@. Returns @0@
-- when @n == 0@ (Parquet writes empty level streams in that case).
bitWidthFor :: Int -> Int
bitWidthFor 0 = 0
bitWidthFor n = go 0 n
  where
    go !w 0 = w
    go !w k = go (w + 1) (k `shiftR` 1)

-- | Encode a level/index stream with the standard 4-byte length prefix
-- the Parquet spec requires for V1 data pages and PLAIN_DICTIONARY index
-- streams. Returns an empty 'ByteString' (no length prefix at all) when
-- @bw == 0@ - omission is correct for Parquet's level streams when the
-- max level is zero.
encodeLengthPrefixedHybrid :: Int -> VP.Vector Int32 -> ByteString
encodeLengthPrefixedHybrid 0  _    = BS.empty
encodeLengthPrefixedHybrid bw vals =
  let !payload = encodeRLEHybrid bw vals
      !len = BS.length payload
   in BL.toStrict $ B.toLazyByteString $
        B.word32LE (fromIntegral len) <> B.byteString payload

-- | Encode the RLE-hybrid payload (no length prefix). Picks between a
-- single RLE run and a single BIT_PACKED run based on whether the input
-- is constant.
encodeRLEHybrid :: Int -> VP.Vector Int32 -> ByteString
encodeRLEHybrid bw vals
  | VP.null vals = BS.empty
  | constant     = rleRun bw (VP.head vals) (VP.length vals)
  | otherwise    = bitPackedRun bw vals
  where
    !v0 = VP.head vals
    constant = VP.all (== v0) vals

-- | Single RLE run: ULEB128 header @<count << 1 | 0>@, followed by the
-- repeated value packed into ceil(bw/8) bytes little-endian.
rleRun :: Int -> Int32 -> Int -> ByteString
rleRun bw value count =
  let !header = (count `shiftL` 1)
      !valueWord = fromIntegral value :: Word32
      !valueBytesCount = (bw + 7) `shiftR` 3
      !valueBytes = BSI.unsafeCreate valueBytesCount $ \p ->
        let writeByte !i
              | i >= valueBytesCount = pure ()
              | otherwise = do
                  pokeByteOff p i (fromIntegral (valueWord `shiftR` (i * 8) .&. 0xFF) :: Word8)
                  writeByte (i + 1)
        in writeByte 0
   in BL.toStrict $ B.toLazyByteString $
        encodeULeb128 (fromIntegral header) <> B.byteString valueBytes

-- | Single BIT_PACKED run covering the entire input. The header is the
-- ULEB128 of @<groupCount << 1 | 1>@ where each group is 8 values.
-- We always pad up to a multiple of 8 with zeros so we emit an integer
-- number of groups - the consumer's @numValues@ field cuts off the
-- padding correctly.
bitPackedRun :: Int -> VP.Vector Int32 -> ByteString
bitPackedRun bw vals =
  let !n = VP.length vals
      !nGroups = (n + 7) `shiftR` 3
      !padded = if n `rem` 8 == 0
                  then vals
                  else vals VP.++ VP.replicate (nGroups * 8 - n) 0
      !packed = packBitsLsb bw padded
      !header = (nGroups `shiftL` 1) .|. 1
   in BL.toStrict $ B.toLazyByteString $
        encodeULeb128 (fromIntegral header) <> B.byteString packed

-- | LSB-first bit packing. Mirrors what 'Parquet.Levels.decodeBitPacked'
-- consumes.
packBitsLsb :: Int -> VP.Vector Int32 -> ByteString
packBitsLsb 0  _    = BS.empty
packBitsLsb bw vals =
  let !n = VP.length vals
      !totalBits = n * bw
      !totalBytes = (totalBits + 7) `shiftR` 3
   in BSI.unsafeCreate totalBytes $ \p ->
        let writeByte !byteIdx
              | byteIdx >= totalBytes = pure ()
              | otherwise = do
                  pokeByteOff p byteIdx (byteAt byteIdx)
                  writeByte (byteIdx + 1)
        in writeByte 0
  where
    byteAt :: Int -> Word8
    byteAt byteIdx =
      let !startBit = byteIdx * 8
          go !bit !acc
            | bit >= 8 = acc
            | otherwise =
                let !globalBit = startBit + bit
                    !valueIdx  = globalBit `quot` bw
                    !innerBit  = globalBit `rem` bw
                 in if valueIdx >= VP.length vals
                      then go (bit + 1) acc
                      else
                        let !v = fromIntegral (VP.unsafeIndex vals valueIdx) :: Word32
                            !b = if (v `shiftR` innerBit) .&. 1 == 1
                                   then acc .|. (1 `shiftL` bit)
                                   else acc
                         in go (bit + 1) b
       in fromIntegral (go 0 (0 :: Int))

-- | Standard ULEB128 of an unsigned integer.
encodeULeb128 :: Word32 -> B.Builder
encodeULeb128 = go
  where
    go !n
      | n < 0x80  = B.word8 (fromIntegral n)
      | otherwise =
          B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80)
            <> go (n `shiftR` 7)
