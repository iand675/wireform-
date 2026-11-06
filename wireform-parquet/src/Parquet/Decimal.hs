{-# LANGUAGE BangPatterns #-}

-- | Signed big-endian two's-complement operations for decimal storage.
module Parquet.Decimal (
  compareDecimalBytes,
  decodeDecimalInteger,
  encodeDecimalInteger,
  decimalPrecisionFits,
) where

import Data.Bits (shiftR, testBit, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32)


{- | Compare represented integers, including different widths and redundant
sign extension. Offsets stay unboxed after inlining; no slices or Integer
values are allocated by the comparison loop.
-}
compareDecimalBytes :: ByteString -> ByteString -> Ordering
compareDecimalBytes a b
  | BS.null a || BS.null b = compare (BS.length a) (BS.length b)
  | negativeA /= negativeB = if negativeA then LT else GT
  | widthA /= widthB = if negativeA then compare widthB widthA else compare widthA widthB
  | otherwise = bytes 0
  where
    negativeA = testBit (BSU.unsafeIndex a 0) 7
    negativeB = testBit (BSU.unsafeIndex b 0) 7
    startA = significantStart a
    startB = significantStart b
    widthA = BS.length a - startA
    widthB = BS.length b - startB
    bytes !i
      | i == widthA = EQ
      | otherwise = case compare (BSU.unsafeIndex a (startA + i)) (BSU.unsafeIndex b (startB + i)) of
          EQ -> bytes (i + 1)
          result -> result
{-# INLINE compareDecimalBytes #-}


significantStart :: ByteString -> Int
significantStart bytes = go 0
  where
    go !i
      | i >= BS.length bytes - 1 = i
      | otherwise =
          let byte = BSU.unsafeIndex bytes i
              nextNegative = testBit (BSU.unsafeIndex bytes (i + 1)) 7
          in if (byte == 0 && not nextNegative) || (byte == 255 && nextNegative)
               then go (i + 1)
               else i
{-# INLINE significantStart #-}


-- | Metadata-only conversion; the per-value writer uses compareDecimalBytes.
decodeDecimalInteger :: ByteString -> Maybe Integer
decodeDecimalInteger bytes
  | BS.null bytes = Nothing
  | otherwise =
      Just
        ( BS.foldl'
            (\n byte -> n * 256 + fromIntegral byte)
            (if testBit (BSU.unsafeIndex bytes 0) 7 then -1 else 0)
            bytes
        )


encodeDecimalInteger :: Integer -> ByteString
encodeDecimalInteger = BS.reverse . BS.unfoldr step . Just
  where
    step Nothing = Nothing
    step (Just n) =
      let byte = fromIntegral (n .&. 255)
          rest = n `shiftR` 8
          done = (rest == 0 && not (testBit byte 7)) || (rest == -1 && testBit byte 7)
      in Just (byte, if done then Nothing else Just rest)


{- | Whether a positive DECIMAL precision fits signed, fixed-width storage.
All positive Int32 widths/precisions are handled without floating point or
large powers derived from the metadata. BYTE_ARRAY has no such bound.

The capacity is floor((8*width-1)*log10(2)). The rational below is a lower
bound on log10(2), less than 10^-50 below its upper bound. Its floor is exact
for every bit count <=17179869175: the whole log interval is strictly between
adjacent Farey fractions 5107137094/16965542197 and 579001193/1923400330.
Their determinant is 1 and denominator sum 18888942527 exceeds that bit limit;
no rational k/n in this interval can have an admitted denominator n.

scripts/verify_parquet_decimal_capacity.py proves both the logarithm bounds
with exact rational series and the Farey certificate against these constants.
This is metadata validation, outside the per-value comparison path.
-}
decimalPrecisionFits :: Int32 -> Int32 -> Bool
decimalPrecisionFits precision width
  | precision <= 0 || width <= 0 = False
  | otherwise =
      let bits = 8 * toInteger width - 1
          capacity = (bits * log10TwoLowerNumerator) `quot` log10TwoDenominator
      in toInteger precision <= capacity


log10TwoLowerNumerator :: Integer
log10TwoLowerNumerator = 30102999566398119521373889472449302676818988146210


log10TwoDenominator :: Integer
log10TwoDenominator = 100000000000000000000000000000000000000000000000000
