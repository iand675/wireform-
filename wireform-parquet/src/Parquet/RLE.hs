{-# LANGUAGE BangPatterns #-}

-- | Parquet hybrid RLE / bit-packed decoding for dictionary indices and similar
-- integer streams (see Apache Parquet @Encodings.md@, RLE section).
--
-- Dictionary data pages store a 1-byte bit width, then hybrid-encoded unsigned
-- indices (no 4-byte length prefix). Definition / repetition levels on data page
-- v1 use a 4-byte little-endian length prefix before the hybrid payload (see
-- 'decodeHybridRleLengthPrefixed').
module Parquet.RLE
  ( decodeDictionaryIndices
  , decodeHybridRleUnsigned32
  , decodeHybridRleLengthPrefixed
  ) where

import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int32, Int64)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Word (Word32, Word64)
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as MVP
import Foreign.Ptr (plusPtr)
import Foreign.Storable (peekByteOff)
import System.IO.Unsafe (unsafePerformIO)

import Proto.Wire.Decode (DecodeResult (..), getVarint, runDecoder')

-- | Decode @n@ dictionary indices: first byte is bit width @w@, remainder is
-- hybrid RLE (no leading 4-byte length).
decodeDictionaryIndices :: Int -> ByteString -> Either String (VP.Vector Int32)
decodeDictionaryIndices n bs
  | n < 0 = Left "Parquet.RLE: negative value count"
  | n == 0 = Right VP.empty
  | BS.null bs = Left "Parquet.RLE: empty dictionary index buffer"
  | otherwise =
      let w = fromIntegral (BS.head bs) :: Int
       in decodeHybridRleUnsigned32 w n (BS.tail bs)

-- | Data page v1 definition / repetition levels: @4@-byte little-endian
-- unsigned length @L@, then @L@ bytes of hybrid RLE (no bit-width byte; width
-- comes from schema max level). Decodes exactly @numValues@ integers.
decodeHybridRleLengthPrefixed :: Int -> Int -> ByteString -> Either String (VP.Vector Int32)
decodeHybridRleLengthPrefixed bw numValues bs
  | BS.length bs < 4 = Left "Parquet.RLE: length-prefixed buffer shorter than 4 bytes"
  | otherwise =
      let !len = fromIntegral (readLE32 bs 0) :: Int
          !rest = BS.drop 4 bs
       in if len < 0 || len > BS.length rest
            then Left "Parquet.RLE: invalid length-prefixed hybrid size"
            else decodeHybridRleUnsigned32 bw numValues (BS.take len rest)

-- | Hybrid RLE / bit-packed unsigned integers at most @bitWidth@ bits, returned
-- as 'Int32' (Parquet dictionary indices).
decodeHybridRleUnsigned32 :: Int -> Int -> ByteString -> Either String (VP.Vector Int32)
decodeHybridRleUnsigned32 bw need bs
  | need < 0 = Left "Parquet.RLE: negative value count"
  | need == 0 = Right VP.empty
  | bw < 0 || bw > 32 = Left "Parquet.RLE: bit width out of range"
  | bw == 0 = Right $ VP.replicate need 0
  | otherwise = fillHybrid bw need bs

fillHybrid :: Int -> Int -> ByteString -> Either String (VP.Vector Int32)
fillHybrid bw need bs = runST $ do
  out <- MVP.unsafeNew need
  wref <- newSTRef 0
  oref <- newSTRef 0
  rref <- newSTRef Nothing
  pref <- newSTRef Nothing
  let len = BS.length bs
      loop =
        readSTRef wref >>= \written ->
          if written >= need
            then return (Right ())
            else do
              p <- readSTRef pref
              case p of
                Just (vec, ix)
                  | ix < VP.length vec -> do
                      let v = VP.unsafeIndex vec ix
                      MVP.unsafeWrite out written (fromIntegral v :: Int32)
                      writeSTRef wref (written + 1)
                      writeSTRef pref (Just (vec, ix + 1))
                      loop
                  | otherwise -> do
                      writeSTRef pref Nothing
                      loop
                Nothing -> drainRle
      drainRle =
        readSTRef wref >>= \written ->
          if written >= need
            then return (Right ())
            else do
              r <- readSTRef rref
              case r of
                Just (val, cnt)
                  | cnt > 0 -> do
                      let takeN = min cnt (need - written)
                      let goR !i
                            | i >= takeN = pure ()
                            | otherwise = do
                                MVP.unsafeWrite out (written + i) val
                                goR (i + 1)
                      goR 0
                      writeSTRef wref (written + takeN)
                      let cnt' = cnt - takeN
                      if cnt' > 0
                        then writeSTRef rref (Just (val, cnt'))
                        else writeSTRef rref Nothing
                      loop
                  | otherwise -> do
                      writeSTRef rref Nothing
                      readHeader
                Nothing -> readHeader
      readHeader =
        readSTRef wref >>= \written ->
          if written >= need
            then return (Right ())
            else do
              o <- readSTRef oref
              if o >= len
                then
                  if written == need
                    then return (Right ())
                    else return (Left "Parquet.RLE: truncated hybrid stream")
                else case runDecoder' getVarint bs o of
                  DecodeFail _ -> return (Left "Parquet.RLE: invalid varint header")
                  DecodeOK header o1 ->
                    if odd header
                      then do
                        let !numGroups = fromIntegral (header `shiftR` 1) :: Int
                            !nbytes = numGroups * bw
                        if numGroups < 0 || o1 + nbytes > len
                          then return (Left "Parquet.RLE: truncated bit-packed run")
                          else case unpackAllGroups bw bs o1 numGroups of
                            Left e -> return (Left e)
                            Right flat -> do
                              writeSTRef oref (o1 + nbytes)
                              readSTRef wref >>= \w0 ->
                                let !needMore = need - w0
                                    !nFlat = VP.length flat
                                    !takeN = min nFlat needMore
                                    goP !i
                                      | i >= takeN = pure ()
                                      | otherwise = do
                                          let v = VP.unsafeIndex flat i
                                          MVP.unsafeWrite out (w0 + i) (fromIntegral v :: Int32)
                                          goP (i + 1)
                                 in do
                                      goP 0
                                      writeSTRef wref (w0 + takeN)
                                      if takeN < nFlat
                                        then writeSTRef pref (Just (flat, takeN))
                                        else writeSTRef pref Nothing
                                      loop
                      else do
                        let !runLen = fromIntegral (header `shiftR` 1) :: Int64
                        if runLen <= 0
                          then return (Left "Parquet.RLE: invalid RLE run length")
                          else case readPaddedLE bw bs o1 of
                            Left e -> return (Left e)
                            Right (rawVal, o2) -> do
                              let !val = fromIntegral rawVal :: Int32
                              writeSTRef oref o2
                              readSTRef wref >>= \w0 ->
                                let !needMore = need - w0
                                    !emit64 = min (fromIntegral needMore :: Int64) runLen
                                    !emit = fromIntegral emit64 :: Int
                                    goE !i
                                      | i >= emit = pure ()
                                      | otherwise = do
                                          MVP.unsafeWrite out (w0 + i) val
                                          goE (i + 1)
                                 in do
                                      goE 0
                                      writeSTRef wref (w0 + emit)
                                      let !left = runLen - emit64
                                      if left > 0
                                        then writeSTRef rref (Just (val, fromIntegral left))
                                        else writeSTRef rref Nothing
                                      loop
  er <- loop
  case er of
    Left e -> return (Left e)
    Right () -> Right <$> VP.unsafeFreeze out

-- | Decode @numGroups@ bit-packed groups (each holding 8 unsigned
-- @bw@-bit values) into one flat 'VP.Vector'.
--
-- Implementation note (perf): the previous shape was
--
--     go !g !acc
--       | g >= numGroups = Right acc
--       | otherwise = do
--           grp <- unpackGroup8 bw bs (off + g * bw)
--           go (g + 1) (acc VP.++ grp)
--
-- where 'VP.++' is O(|acc| + |grp|) per step, so the whole loop
-- is O(numGroups²) memcpy work. For a 100k-row dictionary
-- index stream that's ~12500 groups and ~78 million wasted
-- 4-byte writes. Now we allocate one destination vector of
-- size @numGroups * 8@ up front and unpack each group
-- straight into its slice.
unpackAllGroups :: Int -> ByteString -> Int -> Int -> Either String (VP.Vector Word32)
unpackAllGroups bw bs off numGroups
  | numGroups <= 0 = Right VP.empty
  | bw < 0 || bw > 32 = Left "Parquet.RLE: bit-packed width out of range"
  | off + numGroups * bw > BS.length bs =
      Left "Parquet.RLE: bit-packed groups out of bounds"
  | otherwise = runST $ do
      let !total = numGroups * 8
      mv <- MVP.unsafeNew total
      let go !g
            | g >= numGroups = pure (Right ())
            | otherwise = do
                let !o = off + g * bw
                unpackGroup8Into mv (g * 8) bw bs o
                go (g + 1)
      r <- go 0
      case r of
        Left e   -> pure (Left e)
        Right () -> Right <$> VP.unsafeFreeze mv

-- | Unpack one 8-element bit-packed group directly into a
-- destination 'MVP.MVector' starting at @dstOff@. Avoids the
-- per-group VP.Vector allocation that the old @unpackGroup8 +
-- VP.++@ shape paid.
--
-- Layout matches Apache Parquet Java @BytePacker.LITTLE_ENDIAN@
-- / arrow-rs @unpack8@.
unpackGroup8Into
  :: MVP.MVector s Word32  -- ^ destination
  -> Int                   -- ^ destination offset (in elements)
  -> Int                   -- ^ bit width @w@
  -> ByteString            -- ^ source bytes
  -> Int                   -- ^ source offset (in bytes)
  -> ST s ()
unpackGroup8Into mv dstOff w bs off
  | w == 0 = do
      let goZero !i
            | i >= 8 = pure ()
            | otherwise = MVP.unsafeWrite mv (dstOff + i) 0 >> goZero (i + 1)
      goZero 0
  | otherwise = do
      let !mask = if w == 32 then maxBound else ((1 :: Word32) `shiftL` w) - 1
          -- Read source bytes via unsafeIndex now that the bounds
          -- check is hoisted to the caller (unpackAllGroups).
          r si = fromIntegral (BSU.unsafeIndex bs (off + si)) :: Word32
          go !i
            | i >= 8 = pure ()
            | otherwise = do
                let !startBit = i * w
                    !endBit = startBit + w
                    !startBitOffset = startBit `rem` 8
                    !endBitOffset = endBit `rem` 8
                    !startByte = startBit `quot` 8
                    !endByte = endBit `quot` 8
                    !v = if startByte /= endByte && endBitOffset /= 0
                           then
                             let !val  = r startByte
                                 !a    = val `shiftR` startBitOffset
                                 !val2 = r endByte
                                 !b    = val2 `shiftL` (w - endBitOffset)
                             in  (a .|. (b .&. mask)) .&. mask
                           else
                             let !val = r startByte
                             in  (val `shiftR` startBitOffset) .&. mask
                MVP.unsafeWrite mv (dstOff + i) v
                go (i + 1)
      go 0

-- | Read up to 4 bytes as a little-endian 'Word32', padded to
-- the requested bit width's byte length. Used by RLE-run
-- headers where the run value is stored in @ceil (w/8)@ bytes.
--
-- Tight unrolled loop over @ceil (w/8)@ ∈ {1..5} bytes via
-- 'BSU.unsafeIndex'; replaces the @sum [...]@ list-comprehension
-- shape which allocated cons cells per byte.
readPaddedLE :: Int -> ByteString -> Int -> Either String (Word32, Int)
readPaddedLE w _ _ | w < 0 || w > 32 = Left "Parquet.RLE: padded read width"
readPaddedLE 0 _ off = Right (0, off)
readPaddedLE w bs off
  | off + nbytes > BS.length bs =
      Left "Parquet.RLE: truncated padded value"
  | otherwise =
      let !mask = if w == 32 then maxBound else ((1 :: Word32) `shiftL` w) - 1
          rd i = fromIntegral (BSU.unsafeIndex bs (off + i)) :: Word32
          !v = case nbytes of
                 1 -> rd 0
                 2 -> rd 0 .|. (rd 1 `shiftL` 8)
                 3 -> rd 0 .|. (rd 1 `shiftL` 8)
                                .|. (rd 2 `shiftL` 16)
                 _ -> rd 0 .|. (rd 1 `shiftL` 8)
                                .|. (rd 2 `shiftL` 16)
                                .|. (rd 3 `shiftL` 24)
      in  Right (v .&. mask, off + nbytes)
  where
    !nbytes = (w + 7) `quot` 8

-- | LE 'Word32' read via a single unsafe 'peekByteOff'. Replaces
-- the byte-by-byte BS.index + shifts.
readLE32 :: ByteString -> Int -> Word32
readLE32 bs o = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(cstr, _) ->
    peekByteOff (cstr `plusPtr` o) 0
