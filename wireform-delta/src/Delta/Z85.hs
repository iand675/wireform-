{-# LANGUAGE BangPatterns #-}

-- | Delta uses block-aligned Z85 for UUIDs, deletion vectors and Variant statistics.
module Delta.Z85 (decodeZ85, encodeZ85) where

import Control.Monad (unless)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word64, Word8)
import Foreign.Storable (pokeByteOff)


alphabet :: ByteString
alphabet = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.-:+=^!/*?&<>()[]{}@%$#"


decodeZ85 :: ByteString -> Either String ByteString
decodeZ85 input = do
  require (BS.length input `rem` 5 == 0) "invalid Z85 length"
  validate 0
  pure $ BSI.unsafeCreate (blocks * 4) $ \dst ->
    let write !i
          | i == blocks = pure ()
          | otherwise = do
              let word = fromIntegral (block (i * 5)) :: Word32
              pokeByteOff dst (i * 4) (fromIntegral (word `shiftR` 24) :: Word8)
              pokeByteOff dst (i * 4 + 1) (fromIntegral (word `shiftR` 16) :: Word8)
              pokeByteOff dst (i * 4 + 2) (fromIntegral (word `shiftR` 8) :: Word8)
              pokeByteOff dst (i * 4 + 3) (fromIntegral word :: Word8)
              write (i + 1)
    in write 0
  where
    blocks = BS.length input `quot` 5
    digit i = fromIntegral (BSU.unsafeIndex z85Digits (fromIntegral (BSU.unsafeIndex input i))) :: Word64
    block i = (((digit i * 85 + digit (i + 1)) * 85 + digit (i + 2)) * 85 + digit (i + 3)) * 85 + digit (i + 4)
    validate !i
      | i == blocks = Right ()
      | otherwise = do
          -- Invalid characters map to 255. Even the last digit must be
          -- checked independently since it needn't make the word overflow.
          let p = i * 5
          require (digit p < 85 && digit (p + 1) < 85 && digit (p + 2) < 85 && digit (p + 3) < 85 && digit (p + 4) < 85 && block p <= 0xffffffff) "invalid Z85 block"
          validate (i + 1)


z85Digits :: ByteString
z85Digits = BS.pack (map index [0 .. 255])
  where
    index b = maybe 255 fromIntegral (BS.elemIndex b alphabet)


require :: Bool -> String -> Either String ()
require ok why = unless ok (Left ("Delta.Z85: " ++ why))


{- | Write each 32-bit block as five digits, padding a final short block
with zero bytes, as in the Delta reference codec.
-}
encodeZ85 :: ByteString -> Text
encodeZ85 input = TE.decodeUtf8 $ BSI.unsafeCreate (blocks * 5) $ \dst ->
  let write !i
        | i == blocks = pure ()
        | otherwise = do
            let off = i * 4
                word =
                  (byte off `shiftL` 24)
                    .|. (byte (off + 1) `shiftL` 16)
                    .|. (byte (off + 2) `shiftL` 8)
                    .|. byte (off + 3)
                digits !j !value
                  | j < 0 = pure ()
                  | otherwise =
                      let (rest, digit) = value `quotRem` 85
                      in pokeByteOff dst (i * 5 + j) (BSU.unsafeIndex alphabet (fromIntegral digit))
                           >> digits (j - 1) rest
            digits 4 word
            write (i + 1)
  in write 0
  where
    len = BS.length input
    blocks = (len + 3) `quot` 4
    byte :: Int -> Word32
    byte i
      | i < len = fromIntegral (BSU.unsafeIndex input i) .&. 255
      | otherwise = 0
