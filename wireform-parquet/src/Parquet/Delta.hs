{-# LANGUAGE BangPatterns #-}
-- | DELTA_BINARY_PACKED, DELTA_LENGTH_BYTE_ARRAY, and DELTA_BYTE_ARRAY
-- encodings for Parquet columns.
--
-- See Apache Parquet spec Encodings.md.
module Parquet.Delta
  ( decodeDeltaBinaryPackedInt32
  , decodeDeltaBinaryPackedInt64
  , decodeDeltaLengthByteArray
  , decodeDeltaByteArray
  ) where

import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import Data.Word (Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as MVP
import qualified Data.Vector.Storable as VS

-- | DELTA_BINARY_PACKED INT32: storable-vector output for the
-- post-views Arrow bridge.
decodeDeltaBinaryPackedInt32 :: Int -> ByteString -> Either String (VS.Vector Int32)
decodeDeltaBinaryPackedInt32 n bs = do
  (v64, _) <- decodeDeltaBinaryPackedRaw n bs
  Right $! VS.generate (VP.length v64)
            (\i -> fromIntegral (VP.unsafeIndex v64 i))
{-# INLINE decodeDeltaBinaryPackedInt32 #-}

decodeDeltaBinaryPackedInt64 :: Int -> ByteString -> Either String (VS.Vector Int64)
decodeDeltaBinaryPackedInt64 n bs = do
  (v64, _) <- decodeDeltaBinaryPackedRaw n bs
  Right $! VS.convert v64
{-# INLINE decodeDeltaBinaryPackedInt64 #-}

decodeDeltaBinaryPackedRaw :: Int -> ByteString -> Either String (VP.Vector Int64, Int)
decodeDeltaBinaryPackedRaw _n bs = do
  (blockSize64, off1) <- readULEB128 bs 0
  (numMiniblocks64, off2) <- readULEB128 bs off1
  (totalValues64, off3) <- readULEB128 bs off2
  (firstValue, off4) <- readZigzagLEB128 bs off3
  let !blockSize = fromIntegral blockSize64 :: Int
      !numMiniblocks = fromIntegral numMiniblocks64 :: Int
      !totalValues = fromIntegral totalValues64 :: Int
  if totalValues == 0
    then Right (VP.empty, off4)
    else if numMiniblocks == 0 || blockSize == 0
      then Left "Parquet.Delta: zero block_size or num_miniblocks"
      else do
        let !miniblockSize = blockSize `quot` numMiniblocks
            !numDeltas = totalValues - 1
        (deltas, finalOff) <- decodeAllBlocks bs off4 numDeltas numMiniblocks miniblockSize
        Right (prefixSumVector totalValues firstValue deltas, finalOff)

readULEB128 :: ByteString -> Int -> Either String (Word64, Int)
readULEB128 bs = go 0 0
  where
    go !acc !shift !off
      | off >= BS.length bs = Left "Parquet.Delta: truncated ULEB128"
      | shift > 63 = Left "Parquet.Delta: ULEB128 overflow"
      | otherwise =
          let !b = BS.index bs off
              !val = acc .|. (fromIntegral (b .&. 0x7F) `shiftL` shift)
          in if b .&. 0x80 == 0
            then Right (val, off + 1)
            else go val (shift + 7) (off + 1)

readZigzagLEB128 :: ByteString -> Int -> Either String (Int64, Int)
readZigzagLEB128 bs off = do
  (raw, next) <- readULEB128 bs off
  let !v = fromIntegral raw :: Int64
      !decoded = (v `shiftR` 1) `xor` negate (v .&. 1)
  Right (decoded, next)

prefixSumVector :: Int -> Int64 -> VP.Vector Int64 -> VP.Vector Int64
prefixSumVector n firstVal deltas = runST $ do
  mv <- MVP.new n
  MVP.write mv 0 firstVal
  let go !i !prev
        | i >= n = pure ()
        | otherwise = do
            let !d = if i - 1 < VP.length deltas
                      then VP.unsafeIndex deltas (i - 1)
                      else 0
                !cur = prev + d
            MVP.write mv i cur
            go (i + 1) cur
  go 1 firstVal
  VP.unsafeFreeze mv

decodeAllBlocks ::
  ByteString -> Int -> Int -> Int -> Int ->
  Either String (VP.Vector Int64, Int)
decodeAllBlocks bs off0 numDeltas nMini miniblockSize = runST $ do
  mv <- MVP.new (max numDeltas 0) :: ST s (MVP.MVector s Int64)
  result <- goBlock off0 0 mv
  case result of
    Left e -> pure (Left e)
    Right finalOff -> do
      v <- VP.unsafeFreeze mv
      pure (Right (v, finalOff))
  where
    goBlock :: Int -> Int -> MVP.MVector s Int64 -> ST s (Either String Int)
    goBlock !off !written mv
      | written >= numDeltas = pure (Right off)
      | otherwise =
          case readZigzagLEB128 bs off of
            Left e -> pure (Left e)
            Right (minDelta, off1) ->
              if off1 + nMini > BS.length bs
                then pure (Left "Parquet.Delta: truncated bit widths")
                else do
                  let !off2 = off1 + nMini
                  er <- goMini off2 0 written minDelta off1 mv
                  case er of
                    Left e -> pure (Left e)
                    Right (off3, written3) -> goBlock off3 written3 mv

    goMini :: Int -> Int -> Int -> Int64 -> Int -> MVP.MVector s Int64 -> ST s (Either String (Int, Int))
    goMini !off !miniIdx !written !minDelta !bwOff mv
      | miniIdx >= nMini = pure (Right (off, written))
      | written >= numDeltas = pure (Right (off, written))
      | otherwise = do
          let !bw = fromIntegral (BS.index bs (bwOff + miniIdx)) :: Int
              !nInMini = min miniblockSize (numDeltas - written)
              !totalBitsInMini = miniblockSize * bw
              !totalBytesInMini = (totalBitsInMini + 7) `quot` 8
          if bw == 0
            then do
              let fill !i
                    | i >= nInMini = pure ()
                    | otherwise = do
                        MVP.write mv (written + i) minDelta
                        fill (i + 1)
              fill 0
              goMini off (miniIdx + 1) (written + nInMini) minDelta bwOff mv
            else if off + totalBytesInMini > BS.length bs
              then pure (Left "Parquet.Delta: truncated miniblock data")
              else do
                let unpack !i
                      | i >= nInMini = pure ()
                      | otherwise = do
                          let !bitStart = i * bw
                              !raw = readBitsLE bs off bitStart bw
                              !delta = fromIntegral raw + minDelta
                          MVP.write mv (written + i) delta
                          unpack (i + 1)
                unpack 0
                goMini (off + totalBytesInMini) (miniIdx + 1) (written + nInMini) minDelta bwOff mv

{-# INLINE readBitsLE #-}
readBitsLE :: ByteString -> Int -> Int -> Int -> Word64
readBitsLE bs baseOff bitOff bw
  | bw == 0 = 0
  | otherwise =
      let !startByte = baseOff + (bitOff `quot` 8)
          !startBit = bitOff `rem` 8
          !endBit = startBit + bw
          !bytesNeeded = (endBit + 7) `quot` 8
          !mask = if bw >= 64 then maxBound else (1 `shiftL` bw) - 1
          accum :: Word64 -> Int -> Word64
          accum !acc !i
            | i >= bytesNeeded = acc
            | startByte + i >= BS.length bs = acc
            | otherwise =
                let !b = fromIntegral (BS.index bs (startByte + i)) :: Word64
                in accum (acc .|. (b `shiftL` (8 * i))) (i + 1)
      in (accum 0 0 `shiftR` startBit) .&. mask

-- | DELTA_LENGTH_BYTE_ARRAY: delta-packed lengths followed by concatenated payloads.
decodeDeltaLengthByteArray :: Int -> ByteString -> Either String (V.Vector ByteString)
decodeDeltaLengthByteArray _n bs = do
  (lengths64, dataOffset) <- decodeDeltaBinaryPackedRaw 0 bs
  let !payload = BS.drop dataOffset bs
  splitByLengths lengths64 payload
{-# INLINE decodeDeltaLengthByteArray #-}

splitByLengths :: VP.Vector Int64 -> ByteString -> Either String (V.Vector ByteString)
splitByLengths lens payload = runST $ do
  -- Output count is known up front, so allocate the boxed
  -- vector directly and write in order. Replaces the
  -- previous cons-then-reverse-then-V.fromList loop.
  let !n = VP.length lens
  mv <- VM.unsafeNew n
  let go !i !off
        | i >= n = do
            v <- V.unsafeFreeze mv
            pure (Right v)
        | otherwise =
            let !len = fromIntegral (VP.unsafeIndex lens i) :: Int
            in if len < 0 || off + len > BS.length payload
                 then pure (Left "Parquet.Delta: DELTA_LENGTH_BYTE_ARRAY payload truncated")
                 else do
                   let !val = BS.take len (BS.drop off payload)
                   VM.unsafeWrite mv i val
                   go (i + 1) (off + len)
  go 0 0

-- | DELTA_BYTE_ARRAY: delta-packed prefix lengths + DELTA_LENGTH_BYTE_ARRAY suffixes.
-- Front-compressed / incremental string encoding.
decodeDeltaByteArray :: Int -> ByteString -> Either String (V.Vector ByteString)
decodeDeltaByteArray _n bs = do
  (prefixLens64, suffixOffset) <- decodeDeltaBinaryPackedRaw 0 bs
  let !suffixBs = BS.drop suffixOffset bs
  suffixes <- decodeDeltaLengthByteArray (VP.length prefixLens64) suffixBs
  reconstructPrefixCompressed prefixLens64 suffixes
{-# INLINE decodeDeltaByteArray #-}

reconstructPrefixCompressed :: VP.Vector Int64 -> V.Vector ByteString -> Either String (V.Vector ByteString)
reconstructPrefixCompressed prefixLens suffixes
  | VP.length prefixLens /= V.length suffixes =
      Left "Parquet.Delta: prefix/suffix count mismatch"
  | otherwise = runST $ do
      let !n = VP.length prefixLens
      mv <- VM.unsafeNew n
      let go !i !prev
            | i >= n = do
                v <- V.unsafeFreeze mv
                pure (Right v)
            | otherwise =
                let !pLen = fromIntegral (VP.unsafeIndex prefixLens i) :: Int
                    !suffix = V.unsafeIndex suffixes i
                in if pLen < 0 || pLen > BS.length prev
                     then pure (Left "Parquet.Delta: prefix length exceeds previous value length")
                     else do
                       let !prefix = BS.take pLen prev
                           !val = BS.append prefix suffix
                       VM.unsafeWrite mv i val
                       go (i + 1) val
      go 0 BS.empty
