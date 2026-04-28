{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- | Low-level Avro binary encoding and decoding wire primitives.
--
-- Avro's binary wire format differs from Protocol Buffers in several key ways:
--
-- * __No field tags__: Avro records are positional — fields are encoded in
--   schema order with no field numbers or wire types on the wire.
-- * __ZigZag everywhere__: Both @int@ and @long@ use ZigZag encoding followed
--   by unsigned variable-length encoding (ULEB128), whereas Protobuf only
--   ZigZag-encodes @sint32@\/@sint64@.
-- * __Boolean__: A single byte, @0x00@ for false, @0x01@ for true (Protobuf
--   uses a varint).
-- * __Float\/Double__: 4 or 8 bytes little-endian, same as Protobuf's
--   @fixed32@\/@fixed64@ but with no tag prefix.
-- * __Bytes\/String__: Length-prefixed using a ZigZag long (not an unsigned
--   varint as in Protobuf), followed by raw bytes (UTF-8 for strings).
-- * __Null__: Zero bytes on the wire.
--
-- This module mirrors the performance techniques from "Proto.Wire.Encode" and
-- "Proto.Wire.Decode": direct 'Foreign.Storable.pokeByteOff' writes for
-- encoding, 'Foreign.Storable.peek'-based reads for decoding, a 4-byte inline
-- varint fast path, and @INLINE@ pragmas throughout.
module Avro.Wire
  ( -- * Encode primitives
    avroEncodeNull
  , avroEncodeBool
  , avroEncodeInt
  , avroEncodeLong
  , avroEncodeFloat
  , avroEncodeDouble
  , avroEncodeBytes
  , avroEncodeString

    -- * Decode primitives
  , avroDecodeNull
  , avroDecodeBool
  , avroDecodeInt
  , avroDecodeLong
  , avroDecodeFloat
  , avroDecodeDouble
  , avroDecodeBytes
  , avroDecodeString

    -- * Decode result
  , AvroDecodeResult(..)

    -- * ZigZag varint (long)
  , avroEncodeVarint
  , avroDecodeVarint

    -- * Size calculation
  , avroVarintSize
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor, countLeadingZeros)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.Storable (peek)
import Wireform.FFI (decodeTextFast)
import GHC.Float (castWord32ToFloat, castWord64ToDouble,
                  castFloatToWord32, castDoubleToWord64)
import GHC.Exts (Int#, Int(I#), (+#), (>=#), isTrue#)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- ============================================================
-- ZigZag helpers
-- ============================================================

zigZag64 :: Int64 -> Word64
zigZag64 n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 63))
{-# INLINE zigZag64 #-}

unZigZag64 :: Word64 -> Int64
unZigZag64 n = fromIntegral ((n `shiftR` 1) `xor` negate (n .&. 1))
{-# INLINE unZigZag64 #-}

-- ============================================================
-- Unsigned varint (ULEB128) — shared by encode and decode
-- ============================================================

-- | Encode an unsigned 64-bit value as a ULEB128 varint 'Builder'.
-- Inline fast path for values fitting in 1–4 bytes.
putUVarint :: Word64 -> Builder
putUVarint !n
  | n < 0x80 =
      B.word8 (fromIntegral n)
  | n < 0x4000 =
      B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80) <>
      B.word8 (fromIntegral (n `shiftR` 7))
  | n < 0x200000 =
      B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80) <>
      B.word8 (fromIntegral ((n `shiftR` 7) .&. 0x7F) .|. 0x80) <>
      B.word8 (fromIntegral (n `shiftR` 14))
  | n < 0x10000000 =
      B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80) <>
      B.word8 (fromIntegral ((n `shiftR` 7) .&. 0x7F) .|. 0x80) <>
      B.word8 (fromIntegral ((n `shiftR` 14) .&. 0x7F) .|. 0x80) <>
      B.word8 (fromIntegral (n `shiftR` 21))
  | otherwise = putUVarintSlow n
{-# INLINE putUVarint #-}

putUVarintSlow :: Word64 -> Builder
putUVarintSlow = go
  where
    go !n
      | n < 0x80  = B.word8 (fromIntegral n)
      | otherwise = B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80) <> go (n `shiftR` 7)

-- | Decode a ULEB128 varint from a 'ByteString' at an unboxed offset.
-- Returns @(# (# Word64, Int# #) | () #)@ — success with new offset, or
-- error (unit signals failure).
getUVarint :: ByteString -> Int# -> (# (# Word64, Int# #) | () #)
getUVarint bs off =
  let len = bsLen bs
  in if isTrue# (off >=# len)
     then (# | () #)
     else
       let !b0 = fromIntegral (BSU.unsafeIndex bs (I# off)) :: Word64
       in if b0 < 0x80
          then (# (# b0, off +# 1# #) | #)
          else let off1 = off +# 1#
               in if isTrue# (off1 >=# len)
                  then (# | () #)
                  else
                    let !b1 = fromIntegral (BSU.unsafeIndex bs (I# off1)) :: Word64
                    in if b1 < 0x80
                       then (# (# (b0 .&. 0x7F) .|. (b1 `shiftL` 7), off +# 2# #) | #)
                       else let off2 = off +# 2#
                            in if isTrue# (off2 >=# len)
                               then (# | () #)
                               else
                                 let !b2 = fromIntegral (BSU.unsafeIndex bs (I# off2)) :: Word64
                                 in if b2 < 0x80
                                    then (# (# (b0 .&. 0x7F)
                                                .|. ((b1 .&. 0x7F) `shiftL` 7)
                                                .|. (b2 `shiftL` 14)
                                            , off +# 3# #) | #)
                                    else let off3 = off +# 3#
                                         in if isTrue# (off3 >=# len)
                                            then (# | () #)
                                            else
                                              let !b3 = fromIntegral (BSU.unsafeIndex bs (I# off3)) :: Word64
                                              in if b3 < 0x80
                                                 then (# (# (b0 .&. 0x7F)
                                                             .|. ((b1 .&. 0x7F) `shiftL` 7)
                                                             .|. ((b2 .&. 0x7F) `shiftL` 14)
                                                             .|. (b3 `shiftL` 21)
                                                         , off +# 4# #) | #)
                                                 else getUVarintSlow bs off
{-# INLINE getUVarint #-}

getUVarintSlow :: ByteString -> Int# -> (# (# Word64, Int# #) | () #)
getUVarintSlow bs = go 0 0
  where
    len = bsLen bs
    go :: Word64 -> Int -> Int# -> (# (# Word64, Int# #) | () #)
    go !acc !shift !pos
      | shift > 63             = (# | () #)
      | isTrue# (pos >=# len) = (# | () #)
      | otherwise =
          let !b = BSU.unsafeIndex bs (I# pos)
              !val = acc .|. ((fromIntegral b .&. 0x7F) `shiftL` shift)
          in if b < 0x80
             then (# (# val, pos +# 1# #) | #)
             else go val (shift + 7) (pos +# 1#)
{-# INLINE getUVarintSlow #-}

bsLen :: ByteString -> Int#
bsLen bs = case BS.length bs of I# n -> n
{-# INLINE bsLen #-}

-- ============================================================
-- Decode result type (boxed, for the public API)
-- ============================================================

data AvroDecodeResult a
  = AvroDecodeOK !a {-# UNPACK #-} !Int
  | AvroDecodeFail !String
  deriving stock (Show)

-- ============================================================
-- Encode primitives
-- ============================================================

-- | Encode Avro null. Zero bytes on the wire.
avroEncodeNull :: Builder
avroEncodeNull = mempty
{-# INLINE avroEncodeNull #-}

-- | Encode Avro boolean: @0x00@ for 'False', @0x01@ for 'True'.
avroEncodeBool :: Bool -> Builder
avroEncodeBool False = B.word8 0x00
avroEncodeBool True  = B.word8 0x01
{-# INLINE avroEncodeBool #-}

-- | Encode Avro @int@ (32-bit signed): ZigZag then ULEB128 varint.
avroEncodeInt :: Int32 -> Builder
avroEncodeInt !n = putUVarint (zigZag64 (fromIntegral n))
{-# INLINE avroEncodeInt #-}

-- | Encode Avro @long@ (64-bit signed): ZigZag then ULEB128 varint.
avroEncodeLong :: Int64 -> Builder
avroEncodeLong !n = putUVarint (zigZag64 n)
{-# INLINE avroEncodeLong #-}

-- | Encode Avro @float@: 4 bytes little-endian.
avroEncodeFloat :: Float -> Builder
avroEncodeFloat !f = B.word32LE (castFloatToWord32 f)
{-# INLINE avroEncodeFloat #-}

-- | Encode Avro @double@: 8 bytes little-endian.
avroEncodeDouble :: Double -> Builder
avroEncodeDouble !d = B.word64LE (castDoubleToWord64 d)
{-# INLINE avroEncodeDouble #-}

-- | Encode Avro @bytes@: ZigZag-encoded long length prefix, then raw bytes.
avroEncodeBytes :: ByteString -> Builder
avroEncodeBytes !bs =
  let !len = BS.length bs
  in avroEncodeLong (fromIntegral len) <> B.byteString bs
{-# INLINE avroEncodeBytes #-}

-- | Encode Avro @string@: ZigZag-encoded long length prefix, then UTF-8 bytes.
avroEncodeString :: Text -> Builder
avroEncodeString !t =
  let !bs = TE.encodeUtf8 t
      !len = BS.length bs
  in avroEncodeLong (fromIntegral len) <> B.byteString bs
{-# INLINE avroEncodeString #-}

-- | Encode a ZigZag-encoded long as a ULEB128 varint 'Builder'.
-- This is the Avro wire encoding for @long@ values.
avroEncodeVarint :: Int64 -> Builder
avroEncodeVarint = avroEncodeLong
{-# INLINE avroEncodeVarint #-}

-- ============================================================
-- Decode primitives
-- ============================================================

-- | Decode Avro null. Consumes zero bytes; always succeeds.
avroDecodeNull :: ByteString -> Int -> AvroDecodeResult ()
avroDecodeNull _bs !off = AvroDecodeOK () off
{-# INLINE avroDecodeNull #-}

-- | Decode Avro boolean: reads one byte, @0x00@ = False, @0x01@ = True.
avroDecodeBool :: ByteString -> Int -> AvroDecodeResult Bool
avroDecodeBool !bs !off
  | off >= BS.length bs = AvroDecodeFail "avroDecodeBool: unexpected end of input"
  | otherwise =
      let !b = BSU.unsafeIndex bs off
      in case b of
        0x00 -> AvroDecodeOK False (off + 1)
        0x01 -> AvroDecodeOK True  (off + 1)
        _    -> AvroDecodeFail ("avroDecodeBool: invalid byte " ++ show b)
{-# INLINE avroDecodeBool #-}

-- | Decode Avro @int@ (32-bit signed): ULEB128 varint then un-ZigZag.
avroDecodeInt :: ByteString -> Int -> AvroDecodeResult Int32
avroDecodeInt !bs !(I# off) =
  case getUVarint bs off of
    (# (# w, off' #) | #) -> AvroDecodeOK (fromIntegral (unZigZag64 w)) (I# off')
    (# | () #)             -> AvroDecodeFail "avroDecodeInt: invalid varint"
{-# INLINE avroDecodeInt #-}

-- | Decode Avro @long@ (64-bit signed): ULEB128 varint then un-ZigZag.
avroDecodeLong :: ByteString -> Int -> AvroDecodeResult Int64
avroDecodeLong !bs !(I# off) =
  case getUVarint bs off of
    (# (# w, off' #) | #) -> AvroDecodeOK (unZigZag64 w) (I# off')
    (# | () #)             -> AvroDecodeFail "avroDecodeLong: invalid varint"
{-# INLINE avroDecodeLong #-}

-- | Decode Avro @float@: 4 bytes little-endian.
avroDecodeFloat :: ByteString -> Int -> AvroDecodeResult Float
avroDecodeFloat !bs !off
  | off + 4 > BS.length bs = AvroDecodeFail "avroDecodeFloat: unexpected end of input"
  | otherwise =
      let !w = readWord32LE bs off
      in AvroDecodeOK (castWord32ToFloat w) (off + 4)
{-# INLINE avroDecodeFloat #-}

-- | Decode Avro @double@: 8 bytes little-endian.
avroDecodeDouble :: ByteString -> Int -> AvroDecodeResult Double
avroDecodeDouble !bs !off
  | off + 8 > BS.length bs = AvroDecodeFail "avroDecodeDouble: unexpected end of input"
  | otherwise =
      let !w = readWord64LE bs off
      in AvroDecodeOK (castWord64ToDouble w) (off + 8)
{-# INLINE avroDecodeDouble #-}

-- | Decode Avro @bytes@: ZigZag long length prefix, then raw bytes.
avroDecodeBytes :: ByteString -> Int -> AvroDecodeResult ByteString
avroDecodeBytes !bs !off =
  case avroDecodeLong bs off of
    AvroDecodeOK !lenI64 !off' ->
      let !len = fromIntegral lenI64 :: Int
      in if len < 0
         then AvroDecodeFail "avroDecodeBytes: negative length"
         else if len > BS.length bs - off'
         then AvroDecodeFail "avroDecodeBytes: unexpected end of input"
         else AvroDecodeOK (BSU.unsafeTake len (BSU.unsafeDrop off' bs)) (off' + len)
    AvroDecodeFail e -> AvroDecodeFail e
{-# INLINE avroDecodeBytes #-}

-- | Decode Avro @string@: ZigZag long length prefix, then UTF-8 bytes.
avroDecodeString :: ByteString -> Int -> AvroDecodeResult Text
avroDecodeString !bs !off =
  case avroDecodeBytes bs off of
    AvroDecodeOK !raw !off' ->
      case decodeTextFast raw of
        Right t -> AvroDecodeOK t off'
        Left _  -> AvroDecodeFail "avroDecodeString: invalid UTF-8"
    AvroDecodeFail e -> AvroDecodeFail e
{-# INLINE avroDecodeString #-}

-- | Decode a ZigZag-encoded ULEB128 varint as an 'Int64'.
-- This is the Avro wire decoding for @long@ values.
avroDecodeVarint :: ByteString -> Int -> AvroDecodeResult Int64
avroDecodeVarint = avroDecodeLong
{-# INLINE avroDecodeVarint #-}

-- ============================================================
-- Size calculation
-- ============================================================

-- | Compute the number of bytes needed to encode an Avro @long@ value
-- (ZigZag + ULEB128). Uses CLZ for a branchless computation.
avroVarintSize :: Int64 -> Int
avroVarintSize !n =
  let !w = zigZag64 n
      !bits = 64 - countLeadingZeros (w .|. 1)
      !sz = (bits + 6) `quot` 7
  in sz
{-# INLINE avroVarintSize #-}

-- ============================================================
-- Low-level word reads (same technique as Proto.Wire.Decode)
-- ============================================================

readWord32LE :: ByteString -> Int -> Word32
readWord32LE (BSI.BS fp _) off = unsafeDupablePerformIO $
  withForeignPtr fp $ \ptr ->
    peek (castPtr (ptr `plusPtr` off) :: Ptr Word32)
{-# INLINE readWord32LE #-}

readWord64LE :: ByteString -> Int -> Word64
readWord64LE (BSI.BS fp _) off = unsafeDupablePerformIO $
  withForeignPtr fp $ \ptr ->
    peek (castPtr (ptr `plusPtr` off) :: Ptr Word64)
{-# INLINE readWord64LE #-}
