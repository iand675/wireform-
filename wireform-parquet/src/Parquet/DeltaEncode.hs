{-# LANGUAGE BangPatterns #-}
-- | Encoder side of @DELTA_BINARY_PACKED@.
--
-- The reader's "Parquet.Delta" handles decode; this module produces
-- bytes the reader accepts:
--
-- @
-- ULEB128 block_size
-- ULEB128 num_miniblocks_in_block
-- ULEB128 total_value_count
-- ZigzagLEB128 first_value
--
-- per block (until total_value_count consumed):
--   ZigzagLEB128 min_delta
--   uint8 bit_width[num_miniblocks_in_block]
--   per miniblock:
--     ceil(miniblock_size * bit_width / 8) bytes of LSB-packed (delta - min_delta)
-- @
--
-- We default @block_size = 128@ and @num_miniblocks = 4@ (so each
-- miniblock holds 32 values), matching parquet-mr's writer config.
module Parquet.DeltaEncode
  ( encodeDeltaBinaryPackedInt32
  , encodeDeltaBinaryPackedInt64
  , encodeDeltaBinaryPackedRaw
  , encodeDeltaLengthByteArray
  , encodeDeltaByteArray
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BL
import Foreign.Storable (pokeByteOff)
import Data.Int (Int32, Int64)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import Data.Word (Word32, Word64, Word8)

defaultBlockSize, defaultNumMiniblocks :: Int
defaultBlockSize     = 128
defaultNumMiniblocks = 4

-- | DELTA_BINARY_PACKED for INT32. Promotes to Int64 internally and
-- delegates to the shared raw encoder.
encodeDeltaBinaryPackedInt32 :: VP.Vector Int32 -> ByteString
encodeDeltaBinaryPackedInt32 vs =
  encodeDeltaBinaryPackedRaw (VP.map fromIntegral vs)

-- | DELTA_BINARY_PACKED for INT64.
encodeDeltaBinaryPackedInt64 :: VP.Vector Int64 -> ByteString
encodeDeltaBinaryPackedInt64 = encodeDeltaBinaryPackedRaw

-- | Lower-level raw encoder.
encodeDeltaBinaryPackedRaw :: VP.Vector Int64 -> ByteString
encodeDeltaBinaryPackedRaw vs =
  let !n = VP.length vs
      !blockSize = defaultBlockSize
      !numMiniblocks = defaultNumMiniblocks
      !miniblockSize = blockSize `quot` numMiniblocks
      !header = encodeULeb (fromIntegral blockSize)
             <> encodeULeb (fromIntegral numMiniblocks)
             <> encodeULeb (fromIntegral n)
   in if n == 0
        then BL.toStrict (B.toLazyByteString (header <> encodeZigzagLeb 0))
        else
          let !firstVal = VP.unsafeIndex vs 0
              !deltas = VP.zipWith (-) (VP.tail vs) (VP.init vs)
              !blocks = encodeBlocks blockSize numMiniblocks miniblockSize deltas
              !payload = header
                       <> encodeZigzagLeb firstVal
                       <> blocks
           in BL.toStrict (B.toLazyByteString payload)

encodeBlocks :: Int -> Int -> Int -> VP.Vector Int64 -> B.Builder
encodeBlocks blockSize numMiniblocks miniblockSize deltas =
  let !n = VP.length deltas
      go !off
        | off >= n = mempty
        | otherwise =
            let !chunkLen = min blockSize (n - off)
                !chunk = VP.slice off chunkLen deltas
             in encodeOneBlock numMiniblocks miniblockSize chunk
                  <> go (off + blockSize)
   in go 0

encodeOneBlock :: Int -> Int -> VP.Vector Int64 -> B.Builder
encodeOneBlock numMiniblocks miniblockSize chunk =
  let !minDelta = VP.foldl' min (VP.head chunk) chunk
      -- Per-miniblock bit widths.
      !mini = splitIntoMiniblocks numMiniblocks miniblockSize chunk
      !bitWidths = map (miniblockBitWidth minDelta) mini
   in encodeZigzagLeb minDelta
        <> mconcat (map (B.word8 . fromIntegral) bitWidths)
        <> mconcat (zipWith (encodeMiniblockPayload minDelta miniblockSize) bitWidths mini)

-- Split a block (possibly less than blockSize values) into miniblocks,
-- padding the last one with the value 'minDelta' so the bit-width loop
-- doesn't go out of range.
splitIntoMiniblocks :: Int -> Int -> VP.Vector Int64 -> [VP.Vector Int64]
splitIntoMiniblocks numMiniblocks miniblockSize chunk =
  go 0 numMiniblocks
  where
    n = VP.length chunk
    go !off 0 = if off < n then [VP.drop off chunk] else []
    go !off remaining
      | off >= n =
          replicate remaining VP.empty
      | otherwise =
          let !take' = min miniblockSize (n - off)
           in VP.slice off take' chunk : go (off + miniblockSize) (remaining - 1)

-- Bit width needed to hold every (delta - minDelta) in the miniblock.
miniblockBitWidth :: Int64 -> VP.Vector Int64 -> Int
miniblockBitWidth minDelta vs
  | VP.null vs = 0
  | otherwise =
      let !mx = VP.foldl' (\a x -> max a (x - minDelta)) 0 vs
       in bitWidthW64 (fromIntegral mx :: Word64)

bitWidthW64 :: Word64 -> Int
bitWidthW64 = go 0
  where
    go !w 0 = w
    go !w n = go (w + 1) (n `shiftR` 1)

-- Serialise the miniblock's packed bits. Always emits
-- @ceil(miniblockSize * bw / 8)@ bytes (i.e. uses the *spec* miniblock
-- size, not the actual chunk length). Trailing values past the end of
-- @vs@ are zero-padded.
encodeMiniblockPayload :: Int64 -> Int -> Int -> VP.Vector Int64 -> B.Builder
encodeMiniblockPayload _minDelta _miniblockSize 0 _ = mempty
encodeMiniblockPayload minDelta miniblockSize bw vs =
  let !totalBits = miniblockSize * bw
      !totalBytes = (totalBits + 7) `shiftR` 3
      !packed = BSI.unsafeCreate totalBytes $ \p ->
        let writeByte !i
              | i >= totalBytes = pure ()
              | otherwise = do
                  pokeByteOff p i (byteAt i)
                  writeByte (i + 1)
        in writeByte 0
   in B.byteString packed
  where
    n = VP.length vs

    byteAt :: Int -> Word8
    byteAt byteIdx =
      let !startBit = byteIdx * 8
          go !bit !acc
            | bit >= 8 = acc
            | otherwise =
                let !globalBit = startBit + bit
                    !valIdx = globalBit `quot` bw
                    !innerBit = globalBit `rem` bw
                 in if valIdx >= n
                      then go (bit + 1) acc
                      else
                        let !v = fromIntegral
                                   (VP.unsafeIndex vs valIdx - minDelta)
                                   :: Word64
                            !flag = if (v `shiftR` innerBit) .&. 1 == 1
                                      then acc .|. (1 `shiftL` bit)
                                      else acc
                         in go (bit + 1) flag
       in fromIntegral (go 0 (0 :: Int))

-- ============================================================
-- ULEB128 / zigzag
-- ============================================================

encodeULeb :: Word64 -> B.Builder
encodeULeb = go
  where
    go !n
      | n < 0x80  = B.word8 (fromIntegral n)
      | otherwise =
          B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80)
            <> go (n `shiftR` 7)

encodeZigzagLeb :: Int64 -> B.Builder
encodeZigzagLeb v =
  let !u = if v >= 0 then fromIntegral (v `shiftL` 1) :: Word64
                    else fromIntegral (negate v `shiftL` 1 - 1)
   in encodeULeb u

-- ============================================================
-- DELTA_LENGTH_BYTE_ARRAY
-- ============================================================

-- | Encode a vector of byte arrays as @DELTA_LENGTH_BYTE_ARRAY@ (Parquet
-- encoding 6).
--
-- Layout:
--
-- @
--   <DELTA_BINARY_PACKED of all lengths (Int32)>
--   <concatenated value bytes>
-- @
--
-- The total length / value count of the lengths block matches the input
-- vector's length, and value bytes are written in input order with no
-- separators or length prefixes (the lengths block is the index).
encodeDeltaLengthByteArray :: V.Vector ByteString -> ByteString
encodeDeltaLengthByteArray vs =
  let !lens = VP.generate (V.length vs)
                (\i -> fromIntegral (BS.length (V.unsafeIndex vs i)) :: Int32)
      !lengthsEncoded = encodeDeltaBinaryPackedInt32 lens
      !concatenated   = BS.concat (V.toList vs)
   in lengthsEncoded <> concatenated

-- ============================================================
-- DELTA_BYTE_ARRAY (incremental encoding)
-- ============================================================

-- | Encode a vector of byte arrays as @DELTA_BYTE_ARRAY@ (Parquet
-- encoding 7), aka /incremental encoding/.
--
-- Layout:
--
-- @
--   <DELTA_BINARY_PACKED of prefix lengths (Int32)>
--   <DELTA_BINARY_PACKED of suffix lengths (Int32)>
--   <concatenated suffix bytes>
-- @
--
-- The /prefix length/ is the number of leading bytes shared with the
-- previous element (always 0 for the first element); the /suffix/ is
-- the trailing bytes that differ. Reassembly is
-- @value[i] = value[i-1][:prefix[i]] <> suffix[i]@.
--
-- This is the encoding parquet-mr uses for sorted string columns.
encodeDeltaByteArray :: V.Vector ByteString -> ByteString
encodeDeltaByteArray vs =
  let (prefixLens, suffixes) = computePrefixesSuffixes vs
      !prefixVec = VP.fromList prefixLens
      !suffixLensVec = VP.fromList [fromIntegral (BS.length s) :: Int32 | s <- suffixes]
      !prefixesEncoded = encodeDeltaBinaryPackedInt32 prefixVec
      !suffixLensEncoded = encodeDeltaBinaryPackedInt32 suffixLensVec
      !suffixBytes = BL.toStrict
                       (B.toLazyByteString
                         (foldMap B.byteString suffixes))
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
