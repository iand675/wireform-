{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Encoder side of Parquet's hybrid RLE / bit-packed streams.

Repeated spans use RLE; other inputs use bit-packed groups of eight,
with zero padding only in the final group. Runs are split at the format's
signed 32-bit value-count limit.

A 'HybridPlan' validates values and measures the encoded bytes without
allocating a payload. Page writers can reject a size limit before encoding.
-}
module Parquet.LevelsEncode (
  encodeLengthPrefixedHybrid,
  encodeRLEHybrid,
  bitWidthFor,
  HybridPlan,
  planRLEHybrid,
  planRepeatedRLEHybrid,
  hybridEncodedSize,
  encodeHybridPlan,
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int32)
import Data.Primitive.ByteArray (copyByteArrayToAddr)
import Data.Vector.Primitive qualified as VP
import Data.Word (Word32, Word64, Word8, byteSwap32, byteSwap64)
import Foreign.Ptr (Ptr, plusPtr)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Exts (Int (I#), Int#, RealWorld, State#)
import GHC.IO (IO (..))
import Wireform.Encode.Direct qualified as Direct


-- | Bits required for values in [0, n]. Negative maxima are invalid.
bitWidthFor :: Int -> Int
bitWidthFor n
  | n < 0 = error "Parquet.LevelsEncode: negative maximum value"
  | otherwise = go 0 n
  where
    go !width 0 = width
    go !width remaining = go (width + 1) (shiftR remaining 1)


data HybridBody
  = EmptyHybrid
  | RepeatedHybrid {-# UNPACK #-} !Int {-# UNPACK #-} !Word32
  | PackedHybrid !(VP.Vector Int32)
  | MixedHybrid !(VP.Vector Int32) {-# UNPACK #-} !Int


{- | A validated, exactly sized encoding. Constructors remain private so
encoding cannot bypass the range and address-space checks.
-}
data HybridPlan = HybridPlan
  { hybridWidth :: {-# UNPACK #-} !Int
  , hybridEncodedSize :: {-# UNPACK #-} !Int
  , hybridBody :: !HybridBody
  }


maxRepeatedValues :: Int
maxRepeatedValues = 0x7fffffff


-- A packed header counts groups; the limit applies to values, not groups.
maxPackedGroups :: Int
maxPackedGroups = 0x0fffffff


widthMask :: Int -> Either String Word32
widthMask width
  | width < 0 || width > 32 = Left "Parquet.LevelsEncode: bit width out of range"
  | otherwise = Right (fromIntegral (shiftL (1 :: Word64) width - 1))


{- | Validate a stream and its inclusive maximum value, then measure it.
Int32 input elements carry unsigned wire bits: negative elements are valid
at width 32 when the configured maximum allows them. A dictionary writer
supplies its largest valid index as the maximum instead.
-}
planRLEHybrid :: Int -> Word32 -> VP.Vector Int32 -> Either String HybridPlan
planRLEHybrid width maximumValue values = do
  mask <- widthMask width
  if maximumValue > mask
    then Left "Parquet.LevelsEncode: maximum exceeds bit width"
    else
      if VP.null values
        then Right (HybridPlan width 0 EmptyHybrid)
        else
          let !first = VP.unsafeHead values
              !firstWord = fromIntegral first :: Word32
              outOfRange row = Left ("Parquet.LevelsEncode: value at row " ++ show row ++ " exceeds maximum")
              validate !firstDifferent !row
                | row == VP.length values = measureMixedHybrid width values firstDifferent
                | otherwise =
                    let !value = fromIntegral (VP.unsafeIndex values row) :: Word32
                    in if value > maximumValue
                         then outOfRange row
                         else validate firstDifferent (row + 1)
              -- An equal value inherits the first value's range check.
              -- Once a difference is found, only range checks remain;
              -- at full width all Word32 bit patterns are already valid.
              seekDifference !row
                | row == VP.length values = repeatedPlan width (VP.length values) firstWord
                | VP.unsafeIndex values row == first = seekDifference (row + 1)
                | maximumValue == maxBound = measureMixedHybrid width values row
                | maximumValue == mask && valuesFitMask mask (VP.unsafeDrop row values) = measureMixedHybrid width values row
                | otherwise = validate row row
          in if firstWord > maximumValue
               then outOfRange 0
               else seekDifference 1


-- A bit-width mask is contiguous, so OR-ing the values preserves every
-- out-of-range bit. Align the logical slice before reinterpreting pairs;
-- drain an odd leading/trailing Int32 separately, without reading padding.
-- Endianness changes lane order, but not the OR of the two 32-bit halves.
valuesFitMask :: Word32 -> VP.Vector Int32 -> Bool
valuesFitMask maximumValue values@(VP.Vector offset count bytes)
  | count == 0 = True
  | otherwise =
      let !start = offset .&. 1
          !remaining = count - start
          !pairs = VP.Vector ((offset + start) `quot` 2) (remaining `quot` 2) bytes :: VP.Vector Word64
          !combined = VP.foldl' (.|.) 0 pairs
          !lanes = fromIntegral combined .|. fromIntegral (shiftR combined 32) :: Word32
          !leading = if start == 0 then lanes else lanes .|. fromIntegral (VP.unsafeHead values)
          !allBits = if remaining .&. 1 == 0 then leading else leading .|. fromIntegral (VP.unsafeLast values)
      in allBits <= maximumValue
{-# INLINE valuesFitMask #-}


-- At width one, a run of eight takes twice the payload of a packed group.
-- Wait for sixteen equal values; wider values break even by eight.
minimumRepeatedValues :: Int -> Int
minimumRepeatedValues width = if width == 1 then 16 else 8
{-# INLINE minimumRepeatedValues #-}


-- Start candidates only at a packed-group boundary. An emitted repeated
-- run may end at any row: the next packed segment starts a fresh group.
-- This keeps padding out of the middle of the logical value stream.
findRepeatedEnd# :: VP.Vector Int32 -> Int -> Int -> Int#
findRepeatedEnd# values start width =
  let !first = VP.unsafeIndex values start
      !minimumCount = minimumRepeatedValues width
      go !row
        | row == VP.length values || VP.unsafeIndex values row /= first =
            let !count = row - start
                -- A short constant tail can avoid a whole padded group.
                -- At one bit, its two-byte RLE needs at least nine values.
                !compactTail = row == VP.length values && (width /= 1 || count >= 9)
            in if count >= minimumCount || compactTail then case row of I# end# -> end# else 0#
        | otherwise = go (row + 1)
  in -- A different value at the minimum run's last position rules the run
     -- out immediately. Matching endpoints still need the full interior
     -- check; a short final tail keeps its separate compact-tail rule.
     if VP.length values - start >= minimumCount
       && VP.unsafeIndex values (start + minimumCount - 1) /= first
       then 0#
       else go (start + 1)
{-# INLINE findRepeatedEnd# #-}


-- Size arithmetic stays in unboxed machine integers on the hot path.
-- Both additions are checked before multiplication or narrowing; no
-- segment list, run object or Integer is allocated for each group.
withPackedSize :: Int -> Int -> Int -> (String -> r) -> (Int -> r) -> r
withPackedSize width count total failed next
  | count == 0 = next total
  | otherwise =
      let (whole, tailValues) = quotRem count 8
          !groups = whole + if tailValues == 0 then 0 else 1
          (full, rest) = quotRem groups maxPackedGroups
          !headers = full * 5 + if rest == 0 then 0 else varintSize (shiftL (fromIntegral rest) 1 .|. 1)
          !remaining = maxBound - total - headers
      in if remaining < 0 || (width /= 0 && groups > remaining `quot` width)
           then failed "Parquet.LevelsEncode: encoded stream exceeds addressable size"
           else next (total + headers + groups * width)
{-# INLINE withPackedSize #-}


withRepeatedSize :: Int -> Int -> Int -> (String -> r) -> (Int -> r) -> r
withRepeatedSize width count total failed next =
  let !bytes = (width + 7) `quot` 8
      !size =
        if count == 0
          then 0
          else
            if count <= maxRepeatedValues
              then varintSize (shiftL (fromIntegral count) 1) + bytes
              else
                let (full, rest) = quotRem count maxRepeatedValues
                    !tailSize = if rest == 0 then 0 else varintSize (shiftL (fromIntegral rest) 1) + bytes
                in full * (5 + bytes) + tailSize
  in if total > maxBound - size
       then failed "Parquet.LevelsEncode: encoded stream exceeds addressable size"
       else next (total + size)
{-# INLINE withRepeatedSize #-}


measureMixedHybrid :: Int -> VP.Vector Int32 -> Int -> Either String HybridPlan
measureMixedHybrid width values firstDifferent
  | firstRepeated == 0 = go 0 0 0 False
  | otherwise = withRepeatedSize width firstRepeated 0 Left $ \size -> go firstRepeated firstRepeated size True
  where
    -- The constant-prefix check already found this maximal run. Retain
    -- it in the plan so neither sizing nor encoding reads it a second time.
    firstRepeated = if firstDifferent >= minimumRepeatedValues width then firstDifferent else 0
    go !row !packedStart !total !hasRepeated
      | row == VP.length values =
          withPackedSize width (row - packedStart) total Left $ \size ->
            Right (HybridPlan width size (if hasRepeated then MixedHybrid values firstRepeated else PackedHybrid values))
      | otherwise =
          case findRepeatedEnd# values row width of
            0# -> go (row + min 8 (VP.length values - row)) packedStart total hasRepeated
            end# ->
              let !end = I# end#
              in withPackedSize width (row - packedStart) total Left $ \beforeRun ->
                   withRepeatedSize width (end - row) beforeRun Left $ \afterRun ->
                     go end end afterRun True


{- | Plan a repeated value without materializing a vector. Large counts
are represented by several legal runs, rather than one oversized header.
-}
planRepeatedRLEHybrid :: Int -> Int -> Word32 -> Either String HybridPlan
planRepeatedRLEHybrid width count value = do
  mask <- widthMask width
  if count < 0
    then Left "Parquet.LevelsEncode: negative value count"
    else
      if count == 0
        then Right (HybridPlan width 0 EmptyHybrid)
        else
          if value > mask
            then Left "Parquet.LevelsEncode: repeated value exceeds bit width"
            else repeatedPlan width count value


repeatedPlan :: Int -> Int -> Word32 -> Either String HybridPlan
repeatedPlan width count value =
  let (full, rest) = quotRem count maxRepeatedValues
      bytes = (width + 7) `quot` 8
      tailSize = if rest == 0 then 0 else varintSize (shiftL (fromIntegral rest) 1) + bytes
      size = toInteger full * toInteger (5 + bytes) + toInteger tailSize
  in sizedPlan width (RepeatedHybrid count value) size


sizedPlan :: Int -> HybridBody -> Integer -> Either String HybridPlan
sizedPlan width body size
  | size > toInteger (maxBound :: Int) = Left "Parquet.LevelsEncode: encoded stream exceeds addressable size"
  | otherwise = Right (HybridPlan width (fromInteger size) body)


varintSize :: Word64 -> Int
varintSize = go 1
  where
    go !bytes value
      | value < 128 = bytes
      | otherwise = go (bytes + 1) (shiftR value 7)


-- | Encode a previously validated plan, with one exact-size allocation.
encodeHybridPlan :: HybridPlan -> ByteString
encodeHybridPlan plan
  | hybridEncodedSize plan == 0 = BS.empty
  | otherwise = Direct.directEncode (hybridEncodedSize plan) (\ptr offset -> encodeHybridInto ptr offset plan)


{- | Encode a raw hybrid stream. Invalid widths and unrepresentable values
fail before payload allocation. Use 'planRLEHybrid' for a checked result
or for a smaller semantic maximum than the bit width permits.
-}
encodeRLEHybrid :: Int -> VP.Vector Int32 -> ByteString
encodeRLEHybrid width values = either error encodeHybridPlan $ do
  maximumValue <- widthMask width
  planRLEHybrid width maximumValue values


{- | Encode a V1 repetition/definition-level stream with its unsigned
four-byte length prefix. Dictionary index streams have no such prefix.
Width zero omits the level stream; values must still be representable.
-}
encodeLengthPrefixedHybrid :: Int -> VP.Vector Int32 -> ByteString
encodeLengthPrefixedHybrid width values = either error encode $ do
  maximumValue <- widthMask width
  planRLEHybrid width maximumValue values
  where
    encode plan
      | width == 0 = BS.empty
      | toInteger (hybridEncodedSize plan) > toInteger (maxBound :: Word32) = error "Parquet.LevelsEncode: length prefix exceeds unsigned 32-bit size"
      | hybridEncodedSize plan > maxBound - 4 = error "Parquet.LevelsEncode: prefixed stream exceeds addressable size"
      | otherwise = Direct.directEncode (hybridEncodedSize plan + 4) $ \ptr offset -> do
          let size = fromIntegral (hybridEncodedSize plan)
          payload <- Direct.dWord32LE ptr offset (case targetByteOrder of LittleEndian -> size; BigEndian -> byteSwap32 size)
          encodeHybridInto ptr payload plan


-- Only the outer Direct writer returns a boxed offset. All run workers
-- return the state and offset unboxed, including across width dispatch.
encodeHybridInto :: Ptr Word8 -> Int -> HybridPlan -> IO Int
encodeHybridInto ptr start (HybridPlan width _ body) = IO $ \state ->
  let write s = case body of
        EmptyHybrid -> case start of I# start# -> (# s, start# #)
        RepeatedHybrid count value -> encodeRepeatedRuns# ptr start width count value s
        PackedHybrid values -> encodePackedRuns# ptr start width values 0 (VP.length values) s
        MixedHybrid values firstRepeated -> encodeMixedRuns# ptr start width values firstRepeated s
  in case write state of (# next, end# #) -> (# next, I# end# #)
{-# INLINE encodeHybridInto #-}


runWrite# :: IO Int -> State# RealWorld -> (# State# RealWorld, Int# #)
runWrite# (IO write) state = case write state of (# next, I# offset# #) -> (# next, offset# #)
{-# INLINE runWrite# #-}


encodeRepeatedRuns# :: Ptr Word8 -> Int -> Int -> Int -> Word32 -> State# RealWorld -> (# State# RealWorld, Int# #)
encodeRepeatedRuns# ptr start width count !value state =
  let !valueBytes = (width + 7) `quot` 8
      writeValue !offset s = case valueBytes of
        0 -> case offset of I# offset# -> (# s, offset# #)
        1 -> runWrite# (Direct.dWord8 ptr offset (fromIntegral value)) s
        2 -> case runWrite# (Direct.dWord8 ptr offset (fromIntegral value)) s of
          (# second, next# #) -> runWrite# (Direct.dWord8 ptr (I# next#) (fromIntegral (shiftR value 8))) second
        3 -> case runWrite# (Direct.dWord8 ptr offset (fromIntegral value)) s of
          (# second, next# #) -> case runWrite# (Direct.dWord8 ptr (I# next#) (fromIntegral (shiftR value 8))) second of
            (# third, last# #) -> runWrite# (Direct.dWord8 ptr (I# last#) (fromIntegral (shiftR value 16))) third
        _ -> runWrite# (Direct.dWord32LE ptr offset (case targetByteOrder of LittleEndian -> value; BigEndian -> byteSwap32 value)) s
      writeRun !countHere !offset s =
        case runWrite# (Direct.dVarint ptr offset (shiftL (fromIntegral countHere) 1)) s of
          (# afterHeader, payload# #) -> writeValue (I# payload#) afterHeader
      runs !remaining !offset s
        | remaining == 0 = case offset of I# offset# -> (# s, offset# #)
        | remaining <= maxRepeatedValues = writeRun remaining offset s
        | otherwise = case writeRun maxRepeatedValues offset s of
            (# next, offset# #) -> runs (remaining - maxRepeatedValues) (I# offset#) next
  in runs count start state
{-# INLINE encodeRepeatedRuns# #-}


encodePackedRuns# :: Ptr Word8 -> Int -> Int -> VP.Vector Int32 -> Int -> Int -> State# RealWorld -> (# State# RealWorld, Int# #)
encodePackedRuns# ptr start width values startRow count state =
  let runs !row !remaining !offset s
        | remaining == 0 = case offset of I# offset# -> (# s, offset# #)
        | otherwise =
            let (whole, tailValues) = quotRem remaining 8
                !groups = min maxPackedGroups (whole + if tailValues == 0 then 0 else 1)
                !countHere = min remaining (groups * 8)
            in case runWrite# (Direct.dVarint ptr offset (shiftL (fromIntegral groups) 1 .|. 1)) s of
                 (# afterHeader, payload# #) ->
                   case packValues# ptr width values row countHere (I# payload#) (groups * width) afterHeader of
                     (# next, end# #) -> runs (row + countHere) (remaining - countHere) (I# end#) next
  in runs startRow count start state
{-# INLINE encodePackedRuns# #-}


encodeMixedRuns# :: Ptr Word8 -> Int -> Int -> VP.Vector Int32 -> Int -> State# RealWorld -> (# State# RealWorld, Int# #)
encodeMixedRuns# ptr start width values firstRepeated state
  | firstRepeated == 0 = go 0 0 start state
  | otherwise =
      case encodeRepeatedRuns# ptr start width firstRepeated (fromIntegral (VP.unsafeHead values)) state of
        (# next, end# #) -> go firstRepeated firstRepeated (I# end#) next
  where
    go !row !packedStart !offset s
      | row == VP.length values = encodePackedRuns# ptr offset width values packedStart (row - packedStart) s
      | otherwise = case findRepeatedEnd# values row width of
          0# -> go (row + min 8 (VP.length values - row)) packedStart offset s
          end# ->
            let !end = I# end#
            in case encodePackedRuns# ptr offset width values packedStart (row - packedStart) s of
                 (# afterPacked, payload# #) ->
                   case encodeRepeatedRuns# ptr (I# payload#) width (end - row) (fromIntegral (VP.unsafeIndex values row)) afterPacked of
                     (# afterRepeated, next# #) -> go end end (I# next#) afterRepeated
{-# INLINE encodeMixedRuns# #-}


-- Emit complete machine words, retaining any high bits of a straddling
-- value in the next accumulator. Only the last partial word uses byte
-- writes. Every input load and output store stays within its logical slice.
packValues# :: Ptr Word8 -> Int -> VP.Vector Int32 -> Int -> Int -> Int -> Int -> State# RealWorld -> (# State# RealWorld, Int# #)
packValues# ptr width values startRow count startOffset byteCount state =
  let !end = startOffset + byteCount
      zeroPad !offset s
        | offset == end = case offset of I# offset# -> (# s, offset# #)
        | otherwise = case runWrite# (Direct.dWord8 ptr offset 0) s of
            (# next, offset# #) -> zeroPad (I# offset#) next
      tailBytes !offset !bits !acc s
        | bits <= 0 = zeroPad offset s
        | otherwise = case runWrite# (Direct.dWord8 ptr offset (fromIntegral acc)) s of
            (# next, offset# #) -> tailBytes (I# offset#) (bits - 8) (shiftR acc 8) next
      consume !row !offset !bits !acc s
        | row == count = tailBytes offset bits acc s
        | otherwise =
            let !word = fromIntegral (fromIntegral (VP.unsafeIndex values (startRow + row)) :: Word32) :: Word64
                !combined = acc .|. shiftL word bits
                !nextBits = bits + width
            in if nextBits < 64
                 then consume (row + 1) offset nextBits combined s
                 else case runWrite# (Direct.dWord64LE ptr offset (case targetByteOrder of LittleEndian -> combined; BigEndian -> byteSwap64 combined)) s of
                   (# next, offset# #) -> consume (row + 1) (I# offset#) (nextBits - 64) (shiftR word (64 - bits)) next
  in if width == 32 && targetByteOrder == LittleEndian
       then case values of
         VP.Vector sliceOffset _ bytes ->
           case copyByteArrayToAddr (ptr `plusPtr` startOffset) bytes ((sliceOffset + startRow) * 4) (count * 4) of
             IO copy -> case copy state of
               (# next, _ #) -> zeroPad (startOffset + count * 4) next
       else consume 0 startOffset 0 (0 :: Word64) state
{-# INLINE packValues# #-}
