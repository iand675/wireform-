{-# LANGUAGE BangPatterns #-}

-- | Shared numeric conversions for the CBOR codecs.
module CBOR.Internal (
  floatToHalf,
  halfToFloat,
  withInteger,
  integerValue,
  bignumMagnitude,
) where

import CBOR.Value qualified as C
import Data.Bits (countLeadingZeros, shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word16, Word32, Word64)
import GHC.Float (castWord32ToFloat)


-- | Convert IEEE 754 binary32 bits to binary16 with round-to-nearest,
-- ties to even. Keep the high payload bits of a NaN.
floatToHalf :: Word32 -> Word16
floatToHalf !w
  | expo == 255 =
      sign .|. 0x7c00 .|. (if fraction /= 0 && payload == 0 then 0x0200 else payload)
  | expo > 142 = sign .|. 0x7c00
  | expo >= 113 =
      -- Adding, rather than OR-ing, carries a rounded significand into
      -- the exponent, including the finite-to-infinite boundary.
      sign .|. fromIntegral (((expo - 112) `shiftL` 10) + roundEven fraction 13)
  | expo >= 102 =
      sign .|. fromIntegral (roundEven (fraction .|. 0x800000) (126 - fromIntegral expo))
  | otherwise = sign
  where
    sign = fromIntegral ((w `shiftR` 16) .&. 0x8000)
    expo = (w `shiftR` 23) .&. 0xff
    fraction = w .&. 0x7fffff
    payload = fromIntegral (fraction `shiftR` 13)

    roundEven :: Word32 -> Int -> Word32
    roundEven n shift =
      let !truncated = n `shiftR` shift
          !bias = (1 `shiftL` (shift - 1)) - 1 + (truncated .&. 1)
      in (n + bias) `shiftR` shift
    {-# INLINE roundEven #-}
{-# INLINE floatToHalf #-}


-- | Widen binary16 losslessly, including signed zero and NaN payloads.
halfToFloat :: Word16 -> Float
halfToFloat !h = castWord32ToFloat (sign .|. magnitude)
  where
    w = fromIntegral h :: Word32
    sign = (w .&. 0x8000) `shiftL` 16
    expo = (w `shiftR` 10) .&. 0x1f
    fraction = w .&. 0x03ff
    magnitude
      | expo == 31 = 0x7f800000 .|. (fraction `shiftL` 13)
      | expo /= 0 = ((expo + 112) `shiftL` 23) .|. (fraction `shiftL` 13)
      | fraction == 0 = 0
      | otherwise =
          let !topBit = 31 - countLeadingZeros fraction
              !normal = (fraction `shiftL` (23 - topBit)) .&. 0x7fffff
          in ((103 + fromIntegral topBit) `shiftL` 23) .|. normal
{-# INLINE halfToFloat #-}


-- | Use a bignum tag only when a basic integer cannot carry the value.
integerValue :: Integer -> C.Value
integerValue n = withInteger n C.UInt C.NInt (\tag -> C.Tag tag . C.ByteString)


-- Give the AST and builder encoders the same integer representation
-- without constructing an intermediate AST for the builder.
withInteger :: Integer -> (Word64 -> a) -> (Word64 -> a) -> (Word64 -> ByteString -> a) -> a
withInteger n unsigned negative bignum
  | n < 0 = convert negative 3 (-1 - n)
  | otherwise = convert unsigned 2 n
  where
    convert small tag magnitude
      | magnitude <= toInteger (maxBound :: Word64) = small (fromInteger magnitude)
      | otherwise = bignum tag (BS.pack (bytes magnitude []))

    bytes 0 acc = acc
    bytes magnitude acc =
      bytes (magnitude `shiftR` 8) (fromInteger magnitude : acc)
{-# INLINE withInteger #-}


-- | Interpret the unsigned big-endian magnitude of a bignum tag.
bignumMagnitude :: ByteString -> Integer
bignumMagnitude = BS.foldl' (\n b -> (n `shiftL` 8) .|. fromIntegral b) 0
