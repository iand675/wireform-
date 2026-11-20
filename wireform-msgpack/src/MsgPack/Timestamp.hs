{-# LANGUAGE UnboxedSums #-}

-- | Shared wire rules for the reserved MessagePack timestamp extension.
module MsgPack.Timestamp (
  validNanoseconds,
  invalidNanoseconds,
  timestamp64Word,
  decodeTimestampPayload,
  checkExtension,
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int64, Int8)
import Data.Word (Word32, Word64)
import MsgPack.Value qualified as MV


validNanoseconds :: Word32 -> Bool
validNanoseconds ns = ns <= 999999999
{-# INLINE validNanoseconds #-}


invalidNanoseconds :: String
invalidNanoseconds = "MsgPack: timestamp nanoseconds must be at most 999999999"


timestamp64Word :: Int64 -> Word32 -> Word64
timestamp64Word seconds ns = (fromIntegral ns `shiftL` 34) .|. fromIntegral seconds
{-# INLINE timestamp64Word #-}


-- | The caller has checked that the entire payload fits in the input.
decodeTimestampPayload :: ByteString -> Int -> Int -> (# MV.Value | String #)
decodeTimestampPayload !bs !off !size = case size of
  4 -> let !value = MV.Timestamp (fromIntegral (word32 off)) 0 in (# value | #)
  8 ->
    let !word = word64 off
    in checked (fromIntegral (word .&. 0x3FFFFFFFF)) (fromIntegral (word `shiftR` 34))
  12 -> checked (fromIntegral (word64 (off + 4))) (word32 off)
  _ -> (# | "MsgPack: invalid timestamp extension size " ++ show size #)
  where
    checked !seconds !ns
      | validNanoseconds ns = (# MV.Timestamp seconds ns | #)
      | otherwise = (# | invalidNanoseconds #)
    word32 :: Int -> Word32
    word32 i =
      (fromIntegral (BSU.unsafeIndex bs i) `shiftL` 24)
        .|. (fromIntegral (BSU.unsafeIndex bs (i + 1)) `shiftL` 16)
        .|. (fromIntegral (BSU.unsafeIndex bs (i + 2)) `shiftL` 8)
        .|. fromIntegral (BSU.unsafeIndex bs (i + 3))
    word64 :: Int -> Word64
    word64 i = (fromIntegral (word32 i) `shiftL` 32) .|. fromIntegral (word32 (i + 4))


-- | Extension type -1 must carry a valid timestamp payload.
checkExtension :: Int8 -> ByteString -> a -> a
checkExtension ty bs result
  | ty /= -1 = result
  | otherwise = case decodeTimestampPayload bs 0 (BS.length bs) of
      (# _ | #) -> result
      (# | e #) -> error e
{-# INLINE checkExtension #-}
