{-# LANGUAGE BangPatterns #-}

{- | Parquet hybrid RLE / bit-packed decoding. Dictionary indices carry
their bit width in one byte; V1 level streams carry a four-byte length.
-}
module Parquet.RLE (
  decodeDictionaryIndices,
  validateDictionaryExpansion,
  validateDictionaryExpansionFor,
  decodeHybridRleUnsigned32,
  decodeHybridRleLengthPrefixed,
  decodeHybridRleBoolean,
) where

import Columnar.SIMD (copyBits)
import Control.Monad.ST (runST)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32, Int64)
import Data.Vector.Primitive qualified as VP
import Data.Vector.Primitive.Mutable qualified as MVP
import Data.Vector.Storable qualified as VS
import Data.Word (Word32, Word64, Word8)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable, peekByteOff, pokeByteOff)
import System.IO.Unsafe (unsafePerformIO)


decodeDictionaryIndices :: Int -> ByteString -> Either String (VP.Vector Int32)
decodeDictionaryIndices count bytes
  | count < 0 = Left "Parquet.RLE: negative value count"
  | count == 0 = Right VP.empty
  | BS.null bytes = Left "Parquet.RLE: empty dictionary index buffer"
  | otherwise = decodeHybridRleUnsigned32 (fromIntegral (BS.head bytes)) count (BS.tail bytes)


{- | Check a dictionary index prefix against a byte budget without allocating
an index or output vector. Dictionary offsets must be nonnegative and
monotonic; a nonzero base is allowed. Only the requested prefix is inspected,
as with 'decodeDictionaryIndices'. Zero-width indices implicitly reference
entry zero. Repeated runs are checked in constant time, even for huge counts.
-}
validateDictionaryExpansion :: Int -> VS.Vector Int32 -> Int -> ByteString -> Either String ()
validateDictionaryExpansion = validateDictionaryExpansionFor


validateDictionaryExpansionFor :: (Integral o, Storable o) => Int -> VS.Vector o -> Int -> ByteString -> Either String ()
validateDictionaryExpansionFor limit offsets count encoded
  | limit < 0 = Left "Parquet.RLE: negative dictionary byte budget"
  | count < 0 = Left "Parquet.RLE: negative value count"
  | count == 0 = Right ()
  | VS.length offsets < 2 = Left "Parquet.RLE: dictionary index out of range"
  | VS.unsafeHead offsets < 0 = Left "Parquet.RLE: negative dictionary offset"
  | BS.null encoded = Left "Parquet.RLE: empty dictionary index buffer"
  | width > 32 = Left "Parquet.RLE: bit width out of range"
  | otherwise = do
      checkOffsets 1 (VS.unsafeHead offsets)
      if width == 0
        then withSize 0 (0 :: Word64) count (\_ -> Right ())
        else loop 0 count 0
  where
    width = fromIntegral (BSU.unsafeHead encoded)
    bytes = BSU.unsafeTail encoded
    dictionaryCount = VS.length offsets - 1
    mask = ((1 :: Word64) `shiftL` width) - 1
    checkOffsets !i !previous
      | i == VS.length offsets = Right ()
      | next < previous = Left "Parquet.RLE: dictionary offsets are not monotonic"
      | otherwise = checkOffsets (i + 1) next
      where
        next = VS.unsafeIndex offsets i
    withSize !total !key !emit continue
      | key >= fromIntegral dictionaryCount = Left "Parquet.RLE: dictionary index out of range"
      | size /= 0 && emit > (limit - total) `quot` size = Left "Parquet.RLE: dictionary expansion exceeds byte budget"
      | otherwise = continue (total + emit * size)
      where
        index = fromIntegral key
        size = fromIntegral (VS.unsafeIndex offsets (index + 1) - VS.unsafeIndex offsets index) :: Int
    {-# INLINE withSize #-}
    loop !offset !remaining !total
      | remaining == 0 = Right ()
      | otherwise = withHybridRun width bytes offset remaining Left packed repeated
      where
        packed start next emit = values 0 start 0 total
          where
            values !i !byteOffset !bitOffset !used
              | i == emit = loop next (remaining - emit) used
              | otherwise =
                  let key = readBits bytes byteOffset bitOffset width .&. mask
                      totalBits = bitOffset + width
                  in withSize used key 1 $ \nextUsed ->
                       values (i + 1) (byteOffset + totalBits `quot` 8) (totalBits `rem` 8) nextUsed
        repeated start next emit =
          let key = readBits bytes start 0 width .&. mask
          in withSize total key emit (loop next (remaining - emit))
{-# SPECIALIZE validateDictionaryExpansionFor :: Int -> VS.Vector Int32 -> Int -> ByteString -> Either String () #-}
{-# SPECIALIZE validateDictionaryExpansionFor :: Int -> VS.Vector Int64 -> Int -> ByteString -> Either String () #-}


decodeHybridRleLengthPrefixed :: Int -> Int -> ByteString -> Either String (VP.Vector Int32)
decodeHybridRleLengthPrefixed width count bytes
  | BS.length bytes < 4 = Left "Parquet.RLE: length-prefixed buffer shorter than 4 bytes"
  | otherwise =
      let size = fromIntegral (readLE32 bytes 0)
      in if size > BS.length bytes - 4
           then Left "Parquet.RLE: invalid length-prefixed hybrid size"
           else decodeHybridRleUnsigned32 width count (BS.take size (BS.drop 4 bytes))


{- | Decode exactly count values into one vector. Run state and byte/bit
cursors are threaded directly; there are no per-value STRefs, temporary
group vectors, or repeated vector concatenations.
-}
decodeHybridRleUnsigned32 :: Int -> Int -> ByteString -> Either String (VP.Vector Int32)
decodeHybridRleUnsigned32 width count bytes
  | count < 0 = Left "Parquet.RLE: negative value count"
  | width < 0 || width > 32 = Left "Parquet.RLE: bit width out of range"
  | count == 0 = Right VP.empty
  | count > maxBound `quot` 4 = Left "Parquet.RLE: decoded vector exceeds addressable size"
  | width == 0 = Right (VP.replicate count 0)
  | BS.null bytes = Left "Parquet.RLE: empty hybrid stream"
  | otherwise = withHybridRun width bytes 0 count Left (prepare True) (prepare False)
  where
    -- Check run structure before allocating from an untrusted value count.
    -- Retain the first parsed run: single-run pages need no second header pass.
    prepare packed offset next emit = do
      validate next (count - emit)
      decode packed offset next emit
    decode firstPacked firstOffset firstNext firstCount = runST $ do
      output <- MVP.unsafeNew count
      let header !offset !written
            | written == count = pure (Right ())
            | otherwise =
                withHybridRun
                  width
                  bytes
                  offset
                  (count - written)
                  (pure . Left)
                  (packed written)
                  (repeated written)
          packed !written !offset !next !emit =
            let values !i !byteOffset !bitOffset
                  | i == emit = header next (written + emit)
                  | otherwise = do
                      let value = readBits bytes byteOffset bitOffset width .&. mask
                          totalBits = bitOffset + width
                          advance = totalBits `quot` 8
                          nextBit = totalBits `rem` 8
                      MVP.unsafeWrite output (written + i) (fromIntegral value)
                      values (i + 1) (byteOffset + advance) nextBit
                -- Common level/dictionary widths divide a byte. Decode
                -- that byte once and write all of its complete fields;
                -- a partial last byte still uses the checked scalar tail.
                bits1 !i !off
                  | emit - i < 8 = values i off 0
                  | otherwise = do
                      let byte = BSU.unsafeIndex bytes off
                          base = written + i
                      MVP.unsafeWrite output base (fromIntegral (byte .&. 1))
                      MVP.unsafeWrite output (base + 1) (fromIntegral ((byte `shiftR` 1) .&. 1))
                      MVP.unsafeWrite output (base + 2) (fromIntegral ((byte `shiftR` 2) .&. 1))
                      MVP.unsafeWrite output (base + 3) (fromIntegral ((byte `shiftR` 3) .&. 1))
                      MVP.unsafeWrite output (base + 4) (fromIntegral ((byte `shiftR` 4) .&. 1))
                      MVP.unsafeWrite output (base + 5) (fromIntegral ((byte `shiftR` 5) .&. 1))
                      MVP.unsafeWrite output (base + 6) (fromIntegral ((byte `shiftR` 6) .&. 1))
                      MVP.unsafeWrite output (base + 7) (fromIntegral (byte `shiftR` 7))
                      bits1 (i + 8) (off + 1)
                bits2 !i !off
                  | emit - i < 4 = values i off 0
                  | otherwise = do
                      let byte = BSU.unsafeIndex bytes off
                          base = written + i
                      MVP.unsafeWrite output base (fromIntegral (byte .&. 3))
                      MVP.unsafeWrite output (base + 1) (fromIntegral ((byte `shiftR` 2) .&. 3))
                      MVP.unsafeWrite output (base + 2) (fromIntegral ((byte `shiftR` 4) .&. 3))
                      MVP.unsafeWrite output (base + 3) (fromIntegral (byte `shiftR` 6))
                      bits2 (i + 4) (off + 1)
                bits4 !i !off
                  | emit - i < 2 = values i off 0
                  | otherwise = do
                      let byte = BSU.unsafeIndex bytes off
                          base = written + i
                      MVP.unsafeWrite output base (fromIntegral (byte .&. 15))
                      MVP.unsafeWrite output (base + 1) (fromIntegral (byte `shiftR` 4))
                      bits4 (i + 2) (off + 1)
                bits8 !i !off
                  | i == emit = header next (written + emit)
                  | otherwise = do
                      MVP.unsafeWrite output (written + i) (fromIntegral (BSU.unsafeIndex bytes off))
                      bits8 (i + 1) (off + 1)
            in case width of
                 1 -> bits1 0 offset
                 2 -> bits2 0 offset
                 4 -> bits4 0 offset
                 8 -> bits8 0 offset
                 _ -> values 0 offset 0
          repeated !written !offset !next !emit =
            let value = fromIntegral (readBits bytes offset 0 width .&. mask) :: Int32
            in do
                 MVP.set (MVP.unsafeSlice written emit output) value
                 header next (written + emit)
      result <-
        if firstPacked
          then packed 0 firstOffset firstNext firstCount
          else repeated 0 firstOffset firstNext firstCount
      case result of
        Left err -> pure (Left err)
        Right () -> Right <$> VP.unsafeFreeze output
    mask = ((1 :: Word64) `shiftL` width) - 1
    validate !offset !remaining
      | remaining == 0 = Right ()
      | otherwise = withHybridRun width bytes offset remaining Left advance advance
      where
        advance _ next emit = validate next (remaining - emit)


-- Both preflight and decoding use the same structural checks. Continuations
-- receive the payload offset, next header offset and clipped output count.
-- Inlining keeps the header cursor/count unboxed without allocating run nodes.
withHybridRun
  :: Int
  -> ByteString
  -> Int
  -> Int
  -> (String -> r)
  -> (Int -> Int -> Int -> r)
  -> (Int -> Int -> Int -> r)
  -> r
withHybridRun width bytes start remaining invalid packed repeated = varint start 0 0
  where
    size = BS.length bytes
    paddedBytes = (width + 7) `quot` 8
    varint !offset !acc !shift
      | offset == size = invalid "Parquet.RLE: truncated hybrid stream"
      | otherwise =
          let byte = BSU.unsafeIndex bytes offset
              value = acc .|. (fromIntegral (byte .&. 0x7f) `shiftL` shift) :: Word64
          in if shift == 63 && byte > 1
               then invalid "Parquet.RLE: run header overflows 64 bits"
               else
                 if byte .&. 0x80 /= 0
                   then varint (offset + 1) value (shift + 7)
                   else run (offset + 1) value
    run !offset !value
      | value .&. 1 /= 0 =
          -- The specification caps values per run at 2^31 - 1;
          -- a packed header counts groups of eight, not values.
          let groups = value `shiftR` 1
          in if groups == 0
               then invalid "Parquet.RLE: zero-length bit-packed run"
               else
                 if groups > 0x0fffffff
                   then invalid "Parquet.RLE: bit-packed run exceeds signed 32-bit length"
                   else
                     if groups > fromIntegral ((size - offset) `quot` width)
                       then invalid "Parquet.RLE: truncated bit-packed run"
                       else
                         let next = offset + fromIntegral groups * width
                             emit =
                               if groups > fromIntegral (remaining `quot` 8)
                                 then remaining
                                 else fromIntegral groups * 8
                         in packed offset next emit
      | otherwise =
          let count = value `shiftR` 1
          in if count == 0
               then invalid "Parquet.RLE: invalid RLE run length"
               else
                 if count > 0x7fffffff
                   then invalid "Parquet.RLE: RLE run exceeds signed 32-bit length"
                   else
                     if paddedBytes > size - offset
                       then invalid "Parquet.RLE: truncated padded value"
                       else
                         repeated
                           offset
                           (offset + paddedBytes)
                           (if count > fromIntegral remaining then remaining else fromIntegral count)
{-# INLINE withHybridRun #-}


-- A 32-bit value may touch five bytes when its starting bit is unaligned.
-- The complete packed run has already been bounds-checked by the caller.
readBits :: ByteString -> Int -> Int -> Int -> Word64
readBits bytes offset bit width = go 0 0 `shiftR` bit
  where
    needed = (bit + width + 7) `quot` 8
    go !i !acc
      | i == needed = acc
      | otherwise =
          go
            (i + 1)
            (acc .|. (fromIntegral (BSU.unsafeIndex bytes (offset + i)) `shiftL` (i * 8)))
{-# INLINE readBits #-}


readLE32 :: ByteString -> Int -> Word32
readLE32 bytes offset = fromIntegral (readBits bytes offset 0 32)
{-# INLINE readLE32 #-}


-- BOOLEAN RLE retains the hybrid stream's LSB-first layout. Validate every
-- requested run before allocation, then copy packed spans or set repeated
-- one bits directly; neither a Word32 nor a boxed Bool is built per value.
decodeHybridRleBoolean :: Int -> ByteString -> Either String ByteString
decodeHybridRleBoolean count framed
  | count < 0 = Left "Parquet.RLE: negative value count"
  | BS.length framed < 4 = Left "Parquet.RLE: length-prefixed buffer shorter than 4 bytes"
  | encodedSize > BS.length framed - 4 = Left "Parquet.RLE: invalid length-prefixed hybrid size"
  | count == 0 = Right BS.empty
  | otherwise = withHybridRun 1 bytes 0 count Left (prepare True) (prepare False)
  where
    encodedSize = fromIntegral (readLE32 framed 0)
    bytes = BS.take encodedSize (BS.drop 4 framed)
    outputSize = let (!whole, !tailBits) = count `quotRem` 8 in whole + if tailBits == 0 then 0 else 1
    prepare packed offset next emit = do
      validate next (count - emit)
      Right $! unsafePerformIO $ BSI.create outputSize $ \target -> do
        fillBytes target 0 outputSize
        let header !written !position
              | written == count = pure ()
              | otherwise = withHybridRun 1 bytes position (count - written) (error . ("validated BOOLEAN run changed: " ++)) (copyPacked written) (copyRepeated written)
            copyPacked !written !start !after !size = do
              BSU.unsafeUseAsCString bytes $ \source -> copyBits target written (castPtr source `plusPtr` start) 0 size
              header (written + size) after
            copyRepeated !written !start !after !size = do
              if BSU.unsafeIndex bytes start .&. 1 == 0 then pure () else setOnes target written size
              header (written + size) after
        if packed then copyPacked 0 offset next emit else copyRepeated 0 offset next emit
    validate !offset !remaining
      | remaining == 0 = Right ()
      | otherwise = withHybridRun 1 bytes offset remaining Left (\_ next emit -> validate next (remaining - emit)) (\_ next emit -> validate next (remaining - emit))
    setOnes target !start !size = do
      let (!firstByte, !firstBit) = start `quotRem` 8
          firstCount = min size (8 - firstBit)
          mask n = fromIntegral (((1 :: Word32) `shiftL` n) - 1) :: Word8
          remaining = size - firstCount
          (!whole, !tailCount) = remaining `quotRem` 8
      old <- peekByteOff target firstByte :: IO Word8
      pokeByteOff target firstByte (old .|. (mask firstCount `shiftL` firstBit))
      if whole == 0 then pure () else fillBytes (target `plusPtr` (firstByte + 1)) 255 whole
      if tailCount == 0
        then pure ()
        else do
          lastByte <- peekByteOff target (firstByte + 1 + whole) :: IO Word8
          pokeByteOff target (firstByte + 1 + whole) (lastByte .|. mask tailCount)
{-# NOINLINE decodeHybridRleBoolean #-}
