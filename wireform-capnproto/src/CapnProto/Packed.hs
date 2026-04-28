{-# LANGUAGE BangPatterns #-}
-- | Cap'n Proto packed encoding.
--
-- Cap'n Proto messages are word-aligned. The unpacked binary contains
-- many zero bytes (struct alignment, unused union arms, default-valued
-- fields) which compress poorly through gzip but very well through the
-- format-specific 'packed' transform defined at
-- <https://capnproto.org/encoding.html#packing>.
--
-- The transform is defined word-by-word:
--
-- * Output one /tag byte/ whose 8 bits indicate, from LSB to MSB,
--   which of the eight input bytes in this word are non-zero.
-- * Output the non-zero bytes in order.
-- * If the tag byte is @0x00@ (the entire word is zero), follow it
--   with a single /count/ byte indicating how many additional all-zero
--   words follow (@count@ \xC2\xB1 in @[0, 255]@).
-- * If the tag byte is @0xFF@ (every byte non-zero), follow it with
--   the 8 word bytes plus a single /count/ byte indicating how many
--   additional words to copy unchanged.
--
-- This module provides 'pack' / 'unpack' working on raw 'ByteString's
-- whose length is a multiple of eight.
module CapnProto.Packed
  ( pack
  , unpack
  ) where

import Data.Bits (shiftL, shiftR, testBit, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word8)

-- | Pack a Cap'n Proto message (the unpacked output of
-- "CapnProto.Encode") to its space-efficient on-the-wire form.
--
-- The input length must be a multiple of 8; non-aligned input is
-- right-padded with zeros to the next word boundary, which is
-- harmless because Cap'n Proto messages are always word-aligned.
pack :: ByteString -> ByteString
pack bs =
  let !len = BS.length bs
      !alignedLen = (len + 7) `quot` 8 * 8
  in BL.toStrict $ B.toLazyByteString $ go 0 alignedLen
  where
    -- Read one byte, treating bytes past the end as zero.
    rd off
      | off < BS.length bs = BSU.unsafeIndex bs off
      | otherwise          = 0

    go !off !alignedLen
      | off >= alignedLen = mempty
      | otherwise =
          let !w0 = rd off
              !w1 = rd (off + 1)
              !w2 = rd (off + 2)
              !w3 = rd (off + 3)
              !w4 = rd (off + 4)
              !w5 = rd (off + 5)
              !w6 = rd (off + 6)
              !w7 = rd (off + 7)
              !tag = nonZeroBit w0 0
                 .|. nonZeroBit w1 1
                 .|. nonZeroBit w2 2
                 .|. nonZeroBit w3 3
                 .|. nonZeroBit w4 4
                 .|. nonZeroBit w5 5
                 .|. nonZeroBit w6 6
                 .|. nonZeroBit w7 7
          in case tag of
            0x00 ->
              let !runEnd = countZeroWords (off + 8) alignedLen 0
                  !off'   = off + 8 + runEnd * 8
              in B.word8 0x00
                 <> B.word8 (fromIntegral runEnd)
                 <> go off' alignedLen
            0xFF ->
              -- Mirror the all-zero case: emit the 8 raw bytes, then a
              -- count of how many subsequent /every-byte-non-zero/
              -- words follow, then those words verbatim.
              let !rawTail = countAllNonzeroWords (off + 8) alignedLen 0
                  !endOff = off + 8 + rawTail * 8
                  !rawBytes = BS.take (rawTail * 8) (BS.drop (off + 8) bs)
              in B.word8 0xFF
                 <> B.word8 w0 <> B.word8 w1 <> B.word8 w2 <> B.word8 w3
                 <> B.word8 w4 <> B.word8 w5 <> B.word8 w6 <> B.word8 w7
                 <> B.word8 (fromIntegral rawTail)
                 <> B.byteString rawBytes
                 <> go endOff alignedLen
            _ ->
              let bs' =
                       (if testBit tag 0 then B.word8 w0 else mempty)
                    <> (if testBit tag 1 then B.word8 w1 else mempty)
                    <> (if testBit tag 2 then B.word8 w2 else mempty)
                    <> (if testBit tag 3 then B.word8 w3 else mempty)
                    <> (if testBit tag 4 then B.word8 w4 else mempty)
                    <> (if testBit tag 5 then B.word8 w5 else mempty)
                    <> (if testBit tag 6 then B.word8 w6 else mempty)
                    <> (if testBit tag 7 then B.word8 w7 else mempty)
              in B.word8 tag <> bs' <> go (off + 8) alignedLen

    nonZeroBit b i = if b /= 0 then 1 `shiftL` i else 0

    -- Count contiguous all-zero words starting at @off@, capped at
    -- 255 (one byte) and at the buffer end.
    countZeroWords !off !endOff !n
      | n >= 255             = 255
      | off + 8 > endOff     = n
      | wordIsZero off       = countZeroWords (off + 8) endOff (n + 1)
      | otherwise            = n

    countAllNonzeroWords !off !endOff !n
      | n >= 255             = 255
      | off + 8 > endOff     = n
      | wordAllNonzero off   = countAllNonzeroWords (off + 8) endOff (n + 1)
      | otherwise            = n

    wordIsZero off =
         rd off == 0 && rd (off + 1) == 0
      && rd (off + 2) == 0 && rd (off + 3) == 0
      && rd (off + 4) == 0 && rd (off + 5) == 0
      && rd (off + 6) == 0 && rd (off + 7) == 0

    wordAllNonzero off =
         rd off /= 0 && rd (off + 1) /= 0
      && rd (off + 2) /= 0 && rd (off + 3) /= 0
      && rd (off + 4) /= 0 && rd (off + 5) /= 0
      && rd (off + 6) /= 0 && rd (off + 7) /= 0

-- | Unpack a Cap'n Proto packed-encoded message back to its 8-byte
-- aligned wire form. Returns 'Left' on truncated or malformed input.
unpack :: ByteString -> Either String ByteString
unpack bs = case go 0 mempty of
  Left e -> Left e
  Right b -> Right $! BL.toStrict (B.toLazyByteString b)
  where
    !len = BS.length bs

    go !off !acc
      | off >= len = Right acc
      | otherwise =
          let !tag = BSU.unsafeIndex bs off
          in case tag of
            0x00 ->
              if off + 1 >= len
                then Left "CapnProto.Packed.unpack: truncated zero-run count"
                else
                  let !cnt = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Int
                      !zeros = BS.replicate ((cnt + 1) * 8) 0
                  in go (off + 2) (acc <> B.byteString zeros)
            0xFF ->
              let !need = 1 + 8 + 1
              in if off + need > len
                   then Left "CapnProto.Packed.unpack: truncated all-nonzero word"
                   else
                     let !rawWord = BS.take 8 (BS.drop (off + 1) bs)
                         !rawCnt  = fromIntegral (BSU.unsafeIndex bs (off + 9)) :: Int
                         !rawTail = BS.take (rawCnt * 8) (BS.drop (off + 10) bs)
                     in if BS.length rawTail < rawCnt * 8
                          then Left "CapnProto.Packed.unpack: truncated all-nonzero tail"
                          else go (off + 10 + rawCnt * 8)
                                  (acc <> B.byteString rawWord <> B.byteString rawTail)
            _ ->
              let !nonZeroCount = popCount8 tag
                  !need = 1 + nonZeroCount
              in if off + need > len
                   then Left "CapnProto.Packed.unpack: truncated word body"
                   else
                     let !word = expandWord tag bs (off + 1)
                     in go (off + 1 + nonZeroCount) (acc <> word)

    popCount8 :: Word8 -> Int
    popCount8 w =
      let bit i = if testBit w i then 1 else 0 :: Int
      in bit 0 + bit 1 + bit 2 + bit 3 + bit 4 + bit 5 + bit 6 + bit 7

    expandWord :: Word8 -> ByteString -> Int -> B.Builder
    expandWord tag src !srcOff = go2 0 0
      where
        go2 :: Int -> Int -> B.Builder
        go2 !i !pos
          | i >= 8 = mempty
          | testBit tag i =
              B.word8 (BSU.unsafeIndex src (srcOff + pos))
              <> go2 (i + 1) (pos + 1)
          | otherwise =
              B.word8 0 <> go2 (i + 1) pos

-- Silence unused import warning when 'shiftR' isn't otherwise needed.
_silenceUnused :: Int -> Int
_silenceUnused n = n `shiftR` 0
{-# NOINLINE _silenceUnused #-}
