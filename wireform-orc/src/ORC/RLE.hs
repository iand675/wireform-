{-# LANGUAGE BangPatterns #-}

-- | ORC Run-Length Encoding for integer and boolean streams.
--
-- ORC uses two generations of integer RLE:
--
-- * __RLE v1__ (older files): run/literal with signed varints and byte deltas
-- * __RLE v2__ (modern files): four sub-encodings selected by the top 2 bits
--   of the header byte — Short Repeat, Direct, Patched Base, Delta
--
-- Boolean columns use byte-level RLE followed by MSB-first bit extraction.
module ORC.RLE
  ( decodeRLEv1Int
  , decodeRLEv2Int
  , decodeBooleanRLE
  , decodePresentStream
    -- * Encoding helpers (used by ORC.Write)
  , zigzagEncode
  , putVulong
  , encodeWidth
  , bitWidth
  , closestWidth
  , packBitsMSB
  , encodeByteRLE
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64, Int8)
import Data.STRef ()
import Data.Word (Word64, Word8)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as MVP

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

-- | Decode a PRESENT stream (boolean null-mask). Equivalent to 'decodeBooleanRLE'.
{-# INLINE decodePresentStream #-}
decodePresentStream :: Int -> ByteString -> Either String (V.Vector Bool)
decodePresentStream = decodeBooleanRLE

-- | Decode ORC boolean column data.
--
-- Byte-level RLE produces raw bytes, then each bit is extracted MSB-first.
decodeBooleanRLE :: Int -> ByteString -> Either String (V.Vector Bool)
decodeBooleanRLE numValues bs = do
  bytes <- decodeByteRLE bs
  let !nbits = VP.length bytes * 8
  if numValues > nbits
    then Left "ORC.RLE: boolean stream too short for requested values"
    else Right $! V.generate numValues $ \i ->
           let !byteIdx = i `quot` 8
               !bitIdx  = 7 - (i `rem` 8)
               !b       = VP.unsafeIndex bytes byteIdx
           in b .&. (1 `shiftL` bitIdx) /= 0

-- | Decode ORC RLE v1 signed integers.
decodeRLEv1Int :: Int -> ByteString -> Either String (VP.Vector Int64)
decodeRLEv1Int numValues bs
  | numValues <= 0 = Right VP.empty
  -- ORC RLE delta runs encode up to 512 values from a tiny header.
  -- Multiply by a generous cap so we still reject obvious DoS
  -- attempts (a 9-byte stream that declares 4 GB values) without
  -- false-positives on legitimate dense compression.
  | numValues > BS.length bs * 1024 =
      Left "ORC.RLE: numValues exceeds plausible RLE v1 capacity"
  | otherwise = runST $ do
      out <- MVP.unsafeNew numValues
      result <- rleV1Loop bs 0 out 0 numValues
      case result of
        Left e  -> return (Left e)
        Right _ -> Right <$> VP.unsafeFreeze out

-- | Decode ORC RLE v2 integers.
--
-- When @signed@ is 'True', values are zigzag-decoded.
-- Supports Short Repeat, Direct, and Delta sub-encodings.
decodeRLEv2Int :: Bool -> Int -> ByteString -> Either String (VP.Vector Int64)
decodeRLEv2Int _signed numValues bs
  | numValues <= 0 = Right VP.empty
  | numValues > BS.length bs * 1024 =
      Left "ORC.RLE: numValues exceeds plausible RLE v2 capacity"
  | otherwise = runST $ do
      out <- MVP.unsafeNew numValues
      result <- rleV2Loop _signed bs 0 out 0 numValues
      case result of
        Left e  -> return (Left e)
        Right _ -> Right <$> VP.unsafeFreeze out

------------------------------------------------------------------------
-- Byte RLE (used by boolean streams)
------------------------------------------------------------------------

decodeByteRLE :: ByteString -> Either String (VP.Vector Word8)
decodeByteRLE bs = do
  chunks <- go 0 []
  let !combined = BS.concat (reverse chunks)
  Right $! VP.generate (BS.length combined) (BS.index combined)
  where
    !bsLen = BS.length bs
    go :: Int -> [ByteString] -> Either String [ByteString]
    go !off !acc
      | off >= bsLen = Right acc
      | otherwise = do
          let !ctrl    = fromIntegral (BS.index bs off) :: Int8
              !ctrlInt = fromIntegral ctrl :: Int
          if ctrl >= 0
            then do
              let !runLen = ctrlInt + 3
              if off + 1 >= bsLen
                then Left "ORC.RLE: truncated byte RLE run"
                else do
                  let !val   = BS.index bs (off + 1)
                      !chunk = BS.replicate runLen val
                  go (off + 2) (chunk : acc)
            else do
              let !litLen = negate ctrlInt
              if off + 1 + litLen > bsLen
                then Left "ORC.RLE: truncated byte RLE literals"
                else do
                  let !chunk = BS.take litLen (BS.drop (off + 1) bs)
                  go (off + 1 + litLen) (chunk : acc)

------------------------------------------------------------------------
-- RLE v1
------------------------------------------------------------------------

rleV1Loop
  :: ByteString -> Int
  -> MVP.MVector s Int64 -> Int -> Int
  -> ST s (Either String ())
rleV1Loop bs !off out !written !need
  | written >= need = return (Right ())
  | off >= BS.length bs =
      return (Left "ORC.RLE: truncated RLE v1 stream")
  | otherwise = do
      let !ctrl    = fromIntegral (BS.index bs off) :: Int8
          !ctrlInt = fromIntegral ctrl :: Int
      if ctrl >= 0
        then do
          let !runLen = ctrlInt + 3
          if off + 1 >= BS.length bs
            then return (Left "ORC.RLE: truncated RLE v1 run header")
            else do
              let !delta = fromIntegral (fromIntegral (BS.index bs (off + 1)) :: Int8) :: Int64
              case readVslong bs (off + 2) of
                Left e -> return (Left e)
                Right (base, off') -> do
                  let !emit = min runLen (need - written)
                      emitRun !i !val
                        | i >= emit = pure ()
                        | otherwise = do
                            MVP.unsafeWrite out (written + i) val
                            emitRun (i + 1) (val + delta)
                  emitRun 0 base
                  rleV1Loop bs off' out (written + emit) need
        else do
          let !litLen = negate ctrlInt
              !emit   = min litLen (need - written)
          result <- readLits bs (off + 1) out written emit
          case result of
            Left e       -> return (Left e)
            Right off' -> rleV1Loop bs off' out (written + emit) need

readLits
  :: ByteString -> Int
  -> MVP.MVector s Int64 -> Int -> Int
  -> ST s (Either String Int)
readLits bs !off out !written !n = go 0 off
  where
    go !i !o
      | i >= n = return (Right o)
      | otherwise = case readVslong bs o of
          Left e        -> return (Left e)
          Right (val, o') -> do
            MVP.unsafeWrite out (written + i) val
            go (i + 1) o'

------------------------------------------------------------------------
-- RLE v2
------------------------------------------------------------------------

rleV2Loop
  :: Bool -> ByteString -> Int
  -> MVP.MVector s Int64 -> Int -> Int
  -> ST s (Either String ())
rleV2Loop signed bs !off out !written !need
  | written >= need = return (Right ())
  | off >= BS.length bs =
      return (Left "ORC.RLE: truncated RLE v2 stream")
  | otherwise = do
      let !firstByte = fromIntegral (BS.index bs off) :: Int
          !enc       = (firstByte `shiftR` 6) .&. 3
      case enc of
        0 -> rleV2ShortRepeat signed firstByte bs off out written need
        1 -> rleV2Direct      signed firstByte bs off out written need
        2 -> rleV2PatchedBase signed firstByte bs off out written need
        3 -> rleV2Delta       signed firstByte bs off out written need
        _ -> return (Left "ORC.RLE: invalid RLE v2 encoding")

-- | Short Repeat: value repeated 3-10 times.
rleV2ShortRepeat
  :: Bool -> Int -> ByteString -> Int
  -> MVP.MVector s Int64 -> Int -> Int
  -> ST s (Either String ())
rleV2ShortRepeat signed firstByte bs !off out !written !need = do
  let !widthBytes = ((firstByte `shiftR` 3) .&. 7) + 1
      !count      = (firstByte .&. 7) + 3
  if off + 1 + widthBytes > BS.length bs
    then return (Left "ORC.RLE: truncated SHORT_REPEAT value")
    else do
      let !rawVal = readBigEndian bs (off + 1) widthBytes
          !val    = if signed then zigzagDecode rawVal else fromIntegral rawVal
          !emit   = min count (need - written)
          fill !i
            | i >= emit = pure ()
            | otherwise = do
                MVP.unsafeWrite out (written + i) val
                fill (i + 1)
      fill 0
      rleV2Loop signed bs (off + 1 + widthBytes) out (written + emit) need

-- | Direct: bit-packed values at a fixed width.
rleV2Direct
  :: Bool -> Int -> ByteString -> Int
  -> MVP.MVector s Int64 -> Int -> Int
  -> ST s (Either String ())
rleV2Direct signed firstByte bs !off out !written !need = do
  if off + 1 >= BS.length bs
    then return (Left "ORC.RLE: truncated DIRECT header")
    else do
      let !encodedW   = (firstByte `shiftR` 1) .&. 0x1F
          !w          = decodeWidth encodedW
          !lenHigh    = firstByte .&. 1
          !secondByte = fromIntegral (BS.index bs (off + 1)) :: Int
          !len        = (lenHigh `shiftL` 8) .|. secondByte + 1
          !totalBits  = len * w
          !totalBytes = (totalBits + 7) `quot` 8
          !dataOff    = off + 2
      if dataOff + totalBytes > BS.length bs
        then return (Left "ORC.RLE: truncated DIRECT data")
        else do
          let !emit = min len (need - written)
              unpack !i
                | i >= emit = pure ()
                | otherwise = do
                    let !rawVal = extractBitsMSB (i * w) w bs dataOff
                        !val    = if signed
                                    then zigzagDecode rawVal
                                    else fromIntegral rawVal
                    MVP.unsafeWrite out (written + i) val
                    unpack (i + 1)
          unpack 0
          rleV2Loop signed bs (dataOff + totalBytes) out (written + emit) need

-- | Delta: base value + incremental deltas.
rleV2Delta
  :: Bool -> Int -> ByteString -> Int
  -> MVP.MVector s Int64 -> Int -> Int
  -> ST s (Either String ())
rleV2Delta signed firstByte bs !off out !written !need = do
  if off + 1 >= BS.length bs
    then return (Left "ORC.RLE: truncated DELTA header")
    else do
      let !encodedW   = (firstByte `shiftR` 1) .&. 0x1F
          !w          = decodeWidth encodedW
          !lenHigh    = firstByte .&. 1
          !secondByte = fromIntegral (BS.index bs (off + 1)) :: Int
          !headerLen  = (lenHigh `shiftL` 8) .|. secondByte
          !len        = headerLen + 1

      let readBase = if signed then readVslong bs (off + 2) else readVulongAsInt64 bs (off + 2)
      case readBase of
        Left e -> return (Left e)
        Right (baseVal, off1) -> do
          if len <= 1
            then do
              when (written < need) $
                MVP.unsafeWrite out written baseVal
              rleV2Loop signed bs off1 out (written + min 1 (need - written)) need
            else case readVslong bs off1 of
              Left e -> return (Left e)
              Right (deltaBase, off2) -> do
                let !secondVal = baseVal + deltaBase
                    !emitBase  = min 1 (need - written)
                when (emitBase > 0) $
                  MVP.unsafeWrite out written baseVal
                when (need - written > 1) $
                  MVP.unsafeWrite out (written + 1) secondVal

                if w == 0
                  then do
                    let fillConst !i !val
                          | i >= len || written + i >= need = pure ()
                          | otherwise = do
                              MVP.unsafeWrite out (written + i) val
                              fillConst (i + 1) (val + deltaBase)
                    fillConst 2 (secondVal + deltaBase)
                    rleV2Loop signed bs off2 out (written + min len (need - written)) need
                  else do
                    let !numDeltas = len - 2
                        !deltaBytes = (numDeltas * w + 7) `quot` 8
                    if off2 + deltaBytes > BS.length bs
                      then return (Left "ORC.RLE: truncated DELTA packed data")
                      else do
                        let emitDeltas !i !prevVal
                              | i >= numDeltas || written + 2 + i >= need = pure ()
                              | otherwise = do
                                  let !adj = fromIntegral (extractBitsMSB (i * w) w bs off2) :: Int64
                                      !val = if deltaBase >= 0
                                               then prevVal + adj
                                               else prevVal - adj
                                  MVP.unsafeWrite out (written + 2 + i) val
                                  emitDeltas (i + 1) val
                        emitDeltas 0 secondVal
                        rleV2Loop signed bs (off2 + deltaBytes) out (written + min len (need - written)) need

-- | Patched Base: base value + packed values + sparse patches.
rleV2PatchedBase
  :: Bool -> Int -> ByteString -> Int
  -> MVP.MVector s Int64 -> Int -> Int
  -> ST s (Either String ())
rleV2PatchedBase signed firstByte bs !off out !written !need = do
  if off + 3 >= BS.length bs
    then return (Left "ORC.RLE: truncated PATCHED_BASE header")
    else do
      let !encodedW    = (firstByte `shiftR` 1) .&. 0x1F
          !w           = decodeWidth encodedW
          !lenHigh     = firstByte .&. 1
          !secondByte  = fromIntegral (BS.index bs (off + 1)) :: Int
          !len         = (lenHigh `shiftL` 8) .|. secondByte + 1
          !thirdByte   = fromIntegral (BS.index bs (off + 2)) :: Int
          !fourthByte  = fromIntegral (BS.index bs (off + 3)) :: Int
          !baseWidth   = ((thirdByte `shiftR` 5) .&. 7) + 1
          !patchWidth  = decodeWidth (thirdByte .&. 0x1F)
          !patchGapWidth = ((fourthByte `shiftR` 5) .&. 7) + 1
          !patchListLen  = fourthByte .&. 0x1F
      let !baseOff = off + 4
          !baseBytes = (baseWidth * 8 + 7) `quot` 8
      if baseOff + baseBytes > BS.length bs
        then return (Left "ORC.RLE: truncated PATCHED_BASE base value")
        else do
          let !rawBase = readBigEndian bs baseOff baseBytes
              -- Sign-extend the base value: if MSB of baseWidth*8 bits is set, it's negative
              !baseBits = baseWidth * 8
              !baseVal = if baseBits < 64 && rawBase .&. (1 `shiftL` (baseBits - 1)) /= 0
                           then fromIntegral (rawBase .|. (complement ((1 `shiftL` baseBits) - 1))) :: Int64
                           else fromIntegral rawBase :: Int64

              !dataOff = baseOff + baseBytes
              !totalBits = len * w
              !totalBytes = (totalBits + 7) `quot` 8
          if dataOff + totalBytes > BS.length bs
            then return (Left "ORC.RLE: truncated PATCHED_BASE data")
            else do
              let !patchOff = dataOff + totalBytes
                  !patchEntryWidth = patchWidth + patchGapWidth
                  !patchTotalBits = patchListLen * patchEntryWidth
                  !patchTotalBytes = (patchTotalBits + 7) `quot` 8
              if patchOff + patchTotalBytes > BS.length bs
                then return (Left "ORC.RLE: truncated PATCHED_BASE patch list")
                else do
                  let !emit = min len (need - written)
                      -- Unpack base values (w-bit packed)
                      unpackAndPatch !i
                        | i >= emit = pure ()
                        | otherwise = do
                            let !rawVal = fromIntegral (extractBitsMSB (i * w) w bs dataOff) :: Int64
                                !val = baseVal + rawVal
                            MVP.unsafeWrite out (written + i) val
                            unpackAndPatch (i + 1)
                  unpackAndPatch 0
                  -- Apply patches
                  let applyPatches !p !pos
                        | p >= patchListLen = pure ()
                        | otherwise = do
                            let !entry = extractBitsMSB (p * patchEntryWidth) patchEntryWidth bs patchOff
                                !gap   = fromIntegral (entry `shiftR` patchWidth) :: Int
                                !patch = fromIntegral (entry .&. ((1 `shiftL` patchWidth) - 1)) :: Int64
                                !pos'  = pos + gap
                            when (pos' < emit) $ do
                              !cur <- MVP.unsafeRead out (written + pos')
                              let !adjusted = cur + (patch `shiftL` w)
                              MVP.unsafeWrite out (written + pos') adjusted
                            applyPatches (p + 1) (pos' + 1)
                  applyPatches 0 0

                  -- Zigzag decode if signed
                  when signed $ do
                    let zigzagAll !i
                          | i >= emit = pure ()
                          | otherwise = do
                              !v <- MVP.unsafeRead out (written + i)
                              MVP.unsafeWrite out (written + i) (zigzagDecode (fromIntegral v))
                              zigzagAll (i + 1)
                    zigzagAll 0

                  let !nextOff = patchOff + patchTotalBytes
                  rleV2Loop signed bs nextOff out (written + emit) need

------------------------------------------------------------------------
-- Varint primitives
------------------------------------------------------------------------

{-# INLINE readVulong #-}
readVulong :: ByteString -> Int -> Either String (Word64, Int)
readVulong bs !off = go off 0 0
  where
    !bsLen = BS.length bs
    go !pos !val !shift
      | pos >= bsLen = Left "ORC.RLE: truncated varint"
      | shift >= 64  = Left "ORC.RLE: varint overflow"
      | otherwise    =
          let !b    = fromIntegral (BS.index bs pos) :: Word64
              !val' = val .|. ((b .&. 0x7F) `shiftL` shift)
          in if b .&. 0x80 == 0
               then Right (val', pos + 1)
               else go (pos + 1) val' (shift + 7)

{-# INLINE readVslong #-}
readVslong :: ByteString -> Int -> Either String (Int64, Int)
readVslong bs !off = do
  (n, off') <- readVulong bs off
  Right (zigzagDecode n, off')

{-# INLINE readVulongAsInt64 #-}
readVulongAsInt64 :: ByteString -> Int -> Either String (Int64, Int)
readVulongAsInt64 bs !off = do
  (v, off') <- readVulong bs off
  Right (fromIntegral v :: Int64, off')

{-# INLINE zigzagDecode #-}
zigzagDecode :: Word64 -> Int64
zigzagDecode !n = fromIntegral ((n `shiftR` 1) `xor` negate (n .&. 1))

------------------------------------------------------------------------
-- Bit-packing helpers
------------------------------------------------------------------------

-- | ORC RLE v2 encoded-width table (5-bit code -> actual bit width).
{-# INLINE decodeWidth #-}
decodeWidth :: Int -> Int
decodeWidth !n
  | n <= 23   = n + 1
  | otherwise = case n of
      24 -> 26;  25 -> 28;  26 -> 30;  27 -> 32
      28 -> 40;  29 -> 48;  30 -> 56;  31 -> 64
      _  -> 1

-- | Read @nbytes@ in big-endian order as a 'Word64'.
{-# INLINE readBigEndian #-}
readBigEndian :: ByteString -> Int -> Int -> Word64
readBigEndian bs !off !nbytes = go 0 0
  where
    go !i !acc
      | i >= nbytes = acc
      | otherwise   =
          let !b = fromIntegral (BS.index bs (off + i)) :: Word64
          in go (i + 1) ((acc `shiftL` 8) .|. b)

-- | Extract a @w@-bit value from an MSB-first packed bit stream.
{-# INLINE extractBitsMSB #-}
extractBitsMSB :: Int -> Int -> ByteString -> Int -> Word64
extractBitsMSB !startBit !w bs !dataOff
  | w == 0    = 0
  | otherwise =
      let !byteIdx     = startBit `quot` 8
          !bitOff      = startBit `rem` 8
          !bytesNeeded = (bitOff + w + 7) `quot` 8
          !raw         = readBigEndian bs (dataOff + byteIdx) bytesNeeded
          !shift       = bytesNeeded * 8 - bitOff - w
          !mask        = if w >= 64 then maxBound else (1 `shiftL` w) - 1
      in (raw `shiftR` shift) .&. mask

------------------------------------------------------------------------
-- Encoding helpers (used by ORC.Write)
------------------------------------------------------------------------

{-# INLINE zigzagEncode #-}
zigzagEncode :: Int64 -> Word64
zigzagEncode !n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 63))

-- | Encode a Word64 as a protobuf-style varint.
putVulong :: Word64 -> B.Builder
putVulong = go
  where
    go !v
      | v < 0x80  = B.word8 (fromIntegral v)
      | otherwise = B.word8 (fromIntegral (v .&. 0x7F) .|. 0x80) <> go (v `shiftR` 7)

-- | Inverse of 'decodeWidth': map actual bit width to the 5-bit encoded value.
encodeWidth :: Int -> Int
encodeWidth !w
  | w >= 1 && w <= 24 = w - 1
  | otherwise = case w of
      26 -> 24;  28 -> 25;  30 -> 26;  32 -> 27
      40 -> 28;  48 -> 29;  56 -> 30;  64 -> 31
      _  -> 0

-- | Compute the minimum bit-width to represent a Word64 value.
bitWidth :: Word64 -> Int
bitWidth 0 = 1
bitWidth !v = 64 - countLeadingZeros64 v

countLeadingZeros64 :: Word64 -> Int
countLeadingZeros64 !v = go 0 63
  where
    go !cnt !bit
      | bit < 0 = cnt
      | v .&. (1 `shiftL` bit) /= 0 = cnt
      | otherwise = go (cnt + 1) (bit - 1)

-- | Find the nearest ORC-valid width >= the given width.
closestWidth :: Int -> Int
closestWidth !w
  | w <= 24   = w
  | w <= 26   = 26
  | w <= 28   = 28
  | w <= 30   = 30
  | w <= 32   = 32
  | w <= 40   = 40
  | w <= 48   = 48
  | w <= 56   = 56
  | otherwise = 64

-- | Pack values MSB-first into bytes at a given bit width.
packBitsMSB :: VP.Vector Word64 -> Int -> ByteString
packBitsMSB vals !w =
  let !totalBits = VP.length vals * w
      !totalBytes = (totalBits + 7) `quot` 8
  in BL.toStrict $ B.toLazyByteString $ go 0 0 0 totalBytes
  where
    !n = VP.length vals
    go :: Int -> Int -> Word8 -> Int -> B.Builder
    go !valIdx !bitPos !curByte !bytesLeft
      | bytesLeft <= 0 = mempty
      | valIdx >= n =
          -- Flush remaining partial byte with zero padding
          if bitPos > 0
            then B.word8 curByte
            else B.word8 0 <> go valIdx 0 0 (bytesLeft - 1)
      | otherwise =
          let !val = VP.unsafeIndex vals valIdx
          in packVal val w valIdx bitPos curByte bytesLeft

    packVal :: Word64 -> Int -> Int -> Int -> Word8 -> Int -> B.Builder
    packVal !val !bitsLeft !valIdx !bitPos !curByte !bytesLeft
      | bitsLeft <= 0 = go (valIdx + 1) bitPos curByte bytesLeft
      | otherwise =
          let !avail    = 8 - bitPos
              !take_    = min avail bitsLeft
              !shifted  = fromIntegral (val `shiftR` (bitsLeft - take_)) :: Word8
              !mask     = (1 `shiftL` take_) - 1
              !bits     = shifted .&. mask
              !curByte' = curByte .|. (bits `shiftL` (avail - take_))
              !bitPos'  = bitPos + take_
          in if bitPos' >= 8
               then B.word8 curByte' <> packVal val (bitsLeft - take_) valIdx 0 0 (bytesLeft - 1)
               else packVal val (bitsLeft - take_) valIdx bitPos' curByte' bytesLeft

-- | Byte-level RLE encoder. Groups bytes into runs and literals.
encodeByteRLE :: VP.Vector Word8 -> ByteString
encodeByteRLE vals = BL.toStrict $ B.toLazyByteString $ goRLE 0
  where
    !n = VP.length vals
    goRLE :: Int -> B.Builder
    goRLE !i
      | i >= n = mempty
      | otherwise =
          let !runLen = countRun i
          in if runLen >= 3
               then let !emitLen = min runLen 130
                        !ctrl = fromIntegral (emitLen - 3) :: Word8
                    in B.word8 ctrl
                       <> B.word8 (VP.unsafeIndex vals i)
                       <> goRLE (i + emitLen)
               else let !litLen = countLiterals i
                        !emitLen = min litLen 128
                        !ctrl = fromIntegral (negate (fromIntegral emitLen :: Int8)) :: Word8
                    in B.word8 ctrl
                       <> mconcat (fmap (\j -> B.word8 (VP.unsafeIndex vals (i + j))) [0 .. emitLen - 1])
                       <> goRLE (i + emitLen)

    countRun :: Int -> Int
    countRun !start
      | start >= n = 0
      | otherwise =
          let !v = VP.unsafeIndex vals start
              go_ !j
                | j >= n = j - start
                | VP.unsafeIndex vals j == v = go_ (j + 1)
                | otherwise = j - start
          in go_ (start + 1)

    countLiterals :: Int -> Int
    countLiterals !start = go_ start
      where
        go_ !j
          | j >= n = j - start
          | j + 2 < n
          , VP.unsafeIndex vals j == VP.unsafeIndex vals (j + 1)
          , VP.unsafeIndex vals j == VP.unsafeIndex vals (j + 2) = j - start
          | otherwise = go_ (j + 1)
