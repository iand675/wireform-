{-# LANGUAGE BangPatterns #-}
-- | Low-level Thrift wire encoding primitives for both the Binary Protocol
-- and the Compact Protocol.
--
-- Binary Protocol: big-endian integers, IEEE 754 doubles, i32-prefixed strings.
-- Compact Protocol: zigzag+varint integers, delta field headers, little-endian doubles.
--
-- Performance: direct 'pokeByteOff' \/ 'peekByteOff', INLINE on everything.
module Thrift.Wire
  ( -- * Thrift type codes
    ThriftType (..)
  , thriftTypeToBin
  , thriftTypeFromBin
  , thriftTypeToCompact
  , thriftTypeFromCompact

    -- * Binary Protocol — Encode
  , tBinEncodeI8
  , tBinEncodeI16
  , tBinEncodeI32
  , tBinEncodeI64
  , tBinEncodeBool
  , tBinEncodeDouble
  , tBinEncodeString
  , tBinEncodeBinary
  , tBinEncodeFieldBegin
  , tBinEncodeFieldStop
  , tBinEncodeListBegin
  , tBinEncodeSetBegin
  , tBinEncodeMapBegin

    -- * Binary Protocol — Decode
  , tBinDecodeI8
  , tBinDecodeI16
  , tBinDecodeI32
  , tBinDecodeI64
  , tBinDecodeBool
  , tBinDecodeDouble
  , tBinDecodeString
  , tBinDecodeBinary
  , tBinDecodeFieldBegin
  , tBinDecodeListBegin
  , tBinDecodeSetBegin
  , tBinDecodeMapBegin

    -- * Compact Protocol — Encode
  , tCompEncodeI8
  , tCompEncodeI16
  , tCompEncodeI32
  , tCompEncodeI64
  , tCompEncodeBool
  , tCompEncodeDouble
  , tCompEncodeString
  , tCompEncodeBinary
  , tCompEncodeFieldBegin
  , tCompEncodeFieldStop
  , tCompEncodeListBegin
  , tCompEncodeSetBegin
  , tCompEncodeMapBegin
  , tCompEncodeVarint
  , tCompEncodeZigZag

    -- * Compact Protocol — Decode
  , tCompDecodeI8
  , tCompDecodeI16
  , tCompDecodeI32
  , tCompDecodeI64
  , tCompDecodeBool
  , tCompDecodeDouble
  , tCompDecodeString
  , tCompDecodeBinary
  , tCompDecodeFieldBegin
  , tCompDecodeListBegin
  , tCompDecodeSetBegin
  , tCompDecodeMapBegin
  , tCompDecodeVarint
  , tCompDecodeZigZag
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Float (castDoubleToWord64, castWord64ToDouble)

--------------------------------------------------------------------------------
-- Thrift type codes
--------------------------------------------------------------------------------

-- | Thrift logical types used in both protocols.
data ThriftType
  = TT_STOP
  | TT_BOOL
  | TT_BYTE
  | TT_I16
  | TT_I32
  | TT_I64
  | TT_DOUBLE
  | TT_STRING
  | TT_STRUCT
  | TT_MAP
  | TT_LIST
  | TT_SET
  | TT_UUID
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Binary protocol type codes.
thriftTypeToBin :: ThriftType -> Word8
thriftTypeToBin !t = case t of
  TT_STOP   -> 0
  TT_BOOL   -> 2
  TT_BYTE   -> 3
  TT_I16    -> 6
  TT_I32    -> 8
  TT_I64    -> 10
  TT_DOUBLE -> 4
  TT_STRING -> 11
  TT_STRUCT -> 12
  TT_MAP    -> 13
  TT_LIST   -> 15
  TT_SET    -> 14
  TT_UUID   -> 16
{-# INLINE thriftTypeToBin #-}

-- | Decode a binary protocol type code.
thriftTypeFromBin :: Word8 -> Maybe ThriftType
thriftTypeFromBin !w = case w of
  0  -> Just TT_STOP
  2  -> Just TT_BOOL
  3  -> Just TT_BYTE
  6  -> Just TT_I16
  8  -> Just TT_I32
  10 -> Just TT_I64
  4  -> Just TT_DOUBLE
  11 -> Just TT_STRING
  12 -> Just TT_STRUCT
  13 -> Just TT_MAP
  15 -> Just TT_LIST
  14 -> Just TT_SET
  16 -> Just TT_UUID
  _  -> Nothing
{-# INLINE thriftTypeFromBin #-}

-- | Compact protocol type codes.
--
-- BOOLEAN_TRUE=1, BOOLEAN_FALSE=2, BYTE=3, I16=4, I32=5, I64=6,
-- DOUBLE=7, BINARY=8, LIST=9, SET=10, MAP=11, STRUCT=12, UUID=13
thriftTypeToCompact :: ThriftType -> Word8
thriftTypeToCompact !t = case t of
  TT_STOP   -> 0
  TT_BOOL   -> 1  -- BOOLEAN_TRUE by default; caller adjusts for field headers
  TT_BYTE   -> 3
  TT_I16    -> 4
  TT_I32    -> 5
  TT_I64    -> 6
  TT_DOUBLE -> 7
  TT_STRING -> 8
  TT_STRUCT -> 12
  TT_MAP    -> 11
  TT_LIST   -> 9
  TT_SET    -> 10
  TT_UUID   -> 13
{-# INLINE thriftTypeToCompact #-}

-- | Decode a compact protocol type code.
thriftTypeFromCompact :: Word8 -> Maybe ThriftType
thriftTypeFromCompact !w = case w of
  0  -> Just TT_STOP
  1  -> Just TT_BOOL
  2  -> Just TT_BOOL
  3  -> Just TT_BYTE
  4  -> Just TT_I16
  5  -> Just TT_I32
  6  -> Just TT_I64
  7  -> Just TT_DOUBLE
  8  -> Just TT_STRING
  9  -> Just TT_LIST
  10 -> Just TT_SET
  11 -> Just TT_MAP
  12 -> Just TT_STRUCT
  13 -> Just TT_UUID
  _  -> Nothing
{-# INLINE thriftTypeFromCompact #-}

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

-- Big-endian encode/decode helpers using pokeByteOff/peekByteOff

putBE16 :: Word16 -> Builder
putBE16 !w = B.word8 (fromIntegral (w `shiftR` 8))
          <> B.word8 (fromIntegral (w .&. 0xFF))
{-# INLINE putBE16 #-}

putBE32 :: Word32 -> Builder
putBE32 !w = B.word8 (fromIntegral (w `shiftR` 24))
          <> B.word8 (fromIntegral ((w `shiftR` 16) .&. 0xFF))
          <> B.word8 (fromIntegral ((w `shiftR` 8) .&. 0xFF))
          <> B.word8 (fromIntegral (w .&. 0xFF))
{-# INLINE putBE32 #-}

putBE64 :: Word64 -> Builder
putBE64 !w = B.word8 (fromIntegral (w `shiftR` 56))
          <> B.word8 (fromIntegral ((w `shiftR` 48) .&. 0xFF))
          <> B.word8 (fromIntegral ((w `shiftR` 40) .&. 0xFF))
          <> B.word8 (fromIntegral ((w `shiftR` 32) .&. 0xFF))
          <> B.word8 (fromIntegral ((w `shiftR` 24) .&. 0xFF))
          <> B.word8 (fromIntegral ((w `shiftR` 16) .&. 0xFF))
          <> B.word8 (fromIntegral ((w `shiftR` 8) .&. 0xFF))
          <> B.word8 (fromIntegral (w .&. 0xFF))
{-# INLINE putBE64 #-}

getBE16 :: ByteString -> Int -> Maybe (Word16, Int)
getBE16 !bs !off
  | off + 2 > BS.length bs = Nothing
  | otherwise =
      let !b0 = fromIntegral (BSU.unsafeIndex bs off)
          !b1 = fromIntegral (BSU.unsafeIndex bs (off + 1))
          !v  = (b0 `shiftL` 8) .|. b1
      in Just (v, off + 2)
{-# INLINE getBE16 #-}

getBE32 :: ByteString -> Int -> Maybe (Word32, Int)
getBE32 !bs !off
  | off + 4 > BS.length bs = Nothing
  | otherwise =
      let !b0 = fromIntegral (BSU.unsafeIndex bs off)
          !b1 = fromIntegral (BSU.unsafeIndex bs (off + 1))
          !b2 = fromIntegral (BSU.unsafeIndex bs (off + 2))
          !b3 = fromIntegral (BSU.unsafeIndex bs (off + 3))
          !v  = (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3
      in Just (v, off + 4)
{-# INLINE getBE32 #-}

getBE64 :: ByteString -> Int -> Maybe (Word64, Int)
getBE64 !bs !off
  | off + 8 > BS.length bs = Nothing
  | otherwise =
      let !b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word64
          !b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word64
          !b2 = fromIntegral (BSU.unsafeIndex bs (off + 2)) :: Word64
          !b3 = fromIntegral (BSU.unsafeIndex bs (off + 3)) :: Word64
          !b4 = fromIntegral (BSU.unsafeIndex bs (off + 4)) :: Word64
          !b5 = fromIntegral (BSU.unsafeIndex bs (off + 5)) :: Word64
          !b6 = fromIntegral (BSU.unsafeIndex bs (off + 6)) :: Word64
          !b7 = fromIntegral (BSU.unsafeIndex bs (off + 7)) :: Word64
          !v  = (b0 `shiftL` 56) .|. (b1 `shiftL` 48) .|. (b2 `shiftL` 40) .|. (b3 `shiftL` 32)
             .|. (b4 `shiftL` 24) .|. (b5 `shiftL` 16) .|. (b6 `shiftL` 8) .|. b7
      in Just (v, off + 8)
{-# INLINE getBE64 #-}

-- Little-endian 64-bit (for compact protocol doubles)
putLE64 :: Word64 -> Builder
putLE64 !w = B.word8 (fromIntegral (w .&. 0xFF))
          <> B.word8 (fromIntegral ((w `shiftR` 8) .&. 0xFF))
          <> B.word8 (fromIntegral ((w `shiftR` 16) .&. 0xFF))
          <> B.word8 (fromIntegral ((w `shiftR` 24) .&. 0xFF))
          <> B.word8 (fromIntegral ((w `shiftR` 32) .&. 0xFF))
          <> B.word8 (fromIntegral ((w `shiftR` 40) .&. 0xFF))
          <> B.word8 (fromIntegral ((w `shiftR` 48) .&. 0xFF))
          <> B.word8 (fromIntegral ((w `shiftR` 56) .&. 0xFF))
{-# INLINE putLE64 #-}

getLE64 :: ByteString -> Int -> Maybe (Word64, Int)
getLE64 !bs !off
  | off + 8 > BS.length bs = Nothing
  | otherwise =
      let !b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word64
          !b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word64
          !b2 = fromIntegral (BSU.unsafeIndex bs (off + 2)) :: Word64
          !b3 = fromIntegral (BSU.unsafeIndex bs (off + 3)) :: Word64
          !b4 = fromIntegral (BSU.unsafeIndex bs (off + 4)) :: Word64
          !b5 = fromIntegral (BSU.unsafeIndex bs (off + 5)) :: Word64
          !b6 = fromIntegral (BSU.unsafeIndex bs (off + 6)) :: Word64
          !b7 = fromIntegral (BSU.unsafeIndex bs (off + 7)) :: Word64
          !v  = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
             .|. (b4 `shiftL` 32) .|. (b5 `shiftL` 40) .|. (b6 `shiftL` 48) .|. (b7 `shiftL` 56)
      in Just (v, off + 8)
{-# INLINE getLE64 #-}

-- Varint encode/decode for compact protocol
putVarint :: Word64 -> Builder
putVarint !n
  | n < 0x80 = B.word8 (fromIntegral n)
  | n < 0x4000 =
      B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80) <>
      B.word8 (fromIntegral (n `shiftR` 7))
  | n < 0x200000 =
      B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80) <>
      B.word8 (fromIntegral ((n `shiftR` 7) .&. 0x7F) .|. 0x80) <>
      B.word8 (fromIntegral (n `shiftR` 14))
  | otherwise = putVarintSlow n
{-# INLINE putVarint #-}

putVarintSlow :: Word64 -> Builder
putVarintSlow = go
  where
    go !n
      | n < 0x80  = B.word8 (fromIntegral n)
      | otherwise = B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80) <> go (n `shiftR` 7)
{-# INLINE putVarintSlow #-}

getVarint :: ByteString -> Int -> Maybe (Word64, Int)
getVarint !bs !off = go off 0 0
  where
    go !i !acc !shift
      | i >= BS.length bs = Nothing
      | shift >= 64 = Nothing
      | otherwise =
          let !b = fromIntegral (BSU.unsafeIndex bs i) :: Word64
              !acc' = acc .|. ((b .&. 0x7F) `shiftL` shift)
          in if b < 0x80
             then Just (acc', i + 1)
             else go (i + 1) acc' (shift + 7)
{-# INLINE getVarint #-}

-- ZigZag encoding (same as protobuf)
zigZagEncode64 :: Int64 -> Word64
zigZagEncode64 !n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 63))
{-# INLINE zigZagEncode64 #-}

zigZagDecode64 :: Word64 -> Int64
zigZagDecode64 !n = fromIntegral ((n `shiftR` 1) `xor` negate (n .&. 1))
{-# INLINE zigZagDecode64 #-}

zigZagEncode32 :: Int32 -> Word32
zigZagEncode32 !n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 31))
{-# INLINE zigZagEncode32 #-}

zigZagDecode32 :: Word32 -> Int32
zigZagDecode32 !n = fromIntegral ((n `shiftR` 1) `xor` negate (n .&. 1))
{-# INLINE zigZagDecode32 #-}

--------------------------------------------------------------------------------
-- Binary Protocol — Encode
--------------------------------------------------------------------------------

tBinEncodeI8 :: Int8 -> Builder
tBinEncodeI8 !v = B.word8 (fromIntegral v)
{-# INLINE tBinEncodeI8 #-}

tBinEncodeI16 :: Int16 -> Builder
tBinEncodeI16 !v = putBE16 (fromIntegral v)
{-# INLINE tBinEncodeI16 #-}

tBinEncodeI32 :: Int32 -> Builder
tBinEncodeI32 !v = putBE32 (fromIntegral v)
{-# INLINE tBinEncodeI32 #-}

tBinEncodeI64 :: Int64 -> Builder
tBinEncodeI64 !v = putBE64 (fromIntegral v)
{-# INLINE tBinEncodeI64 #-}

tBinEncodeBool :: Bool -> Builder
tBinEncodeBool !b = B.word8 (if b then 1 else 0)
{-# INLINE tBinEncodeBool #-}

tBinEncodeDouble :: Double -> Builder
tBinEncodeDouble !d = putBE64 (castDoubleToWord64 d)
{-# INLINE tBinEncodeDouble #-}

-- | Encode a string (i32 length prefix + bytes).
tBinEncodeString :: ByteString -> Builder
tBinEncodeString !bs =
  putBE32 (fromIntegral (BS.length bs)) <> B.byteString bs
{-# INLINE tBinEncodeString #-}

-- | Encode raw binary (same wire format as string).
tBinEncodeBinary :: ByteString -> Builder
tBinEncodeBinary = tBinEncodeString
{-# INLINE tBinEncodeBinary #-}

-- | Encode a struct field header: type byte + field id.
tBinEncodeFieldBegin :: ThriftType -> Int16 -> Builder
tBinEncodeFieldBegin !tt !fid =
  B.word8 (thriftTypeToBin tt) <> putBE16 (fromIntegral fid)
{-# INLINE tBinEncodeFieldBegin #-}

-- | Encode the struct stop byte.
tBinEncodeFieldStop :: Builder
tBinEncodeFieldStop = B.word8 0x00
{-# INLINE tBinEncodeFieldStop #-}

-- | Encode a list header: element type + i32 size.
tBinEncodeListBegin :: ThriftType -> Int32 -> Builder
tBinEncodeListBegin !elemType !sz =
  B.word8 (thriftTypeToBin elemType) <> putBE32 (fromIntegral sz)
{-# INLINE tBinEncodeListBegin #-}

-- | Encode a set header (same as list header).
tBinEncodeSetBegin :: ThriftType -> Int32 -> Builder
tBinEncodeSetBegin = tBinEncodeListBegin
{-# INLINE tBinEncodeSetBegin #-}

-- | Encode a map header: key type + value type + i32 size.
tBinEncodeMapBegin :: ThriftType -> ThriftType -> Int32 -> Builder
tBinEncodeMapBegin !keyType !valType !sz =
  B.word8 (thriftTypeToBin keyType)
  <> B.word8 (thriftTypeToBin valType)
  <> putBE32 (fromIntegral sz)
{-# INLINE tBinEncodeMapBegin #-}

--------------------------------------------------------------------------------
-- Binary Protocol — Decode
--------------------------------------------------------------------------------

-- | Decode result: value + new offset, or Nothing on error.
tBinDecodeI8 :: ByteString -> Int -> Maybe (Int8, Int)
tBinDecodeI8 !bs !off
  | off >= BS.length bs = Nothing
  | otherwise = Just (fromIntegral (BSU.unsafeIndex bs off), off + 1)
{-# INLINE tBinDecodeI8 #-}

tBinDecodeI16 :: ByteString -> Int -> Maybe (Int16, Int)
tBinDecodeI16 !bs !off = case getBE16 bs off of
  Just (!w, !off') -> Just (fromIntegral w, off')
  Nothing -> Nothing
{-# INLINE tBinDecodeI16 #-}

tBinDecodeI32 :: ByteString -> Int -> Maybe (Int32, Int)
tBinDecodeI32 !bs !off = case getBE32 bs off of
  Just (!w, !off') -> Just (fromIntegral w, off')
  Nothing -> Nothing
{-# INLINE tBinDecodeI32 #-}

tBinDecodeI64 :: ByteString -> Int -> Maybe (Int64, Int)
tBinDecodeI64 !bs !off = case getBE64 bs off of
  Just (!w, !off') -> Just (fromIntegral w, off')
  Nothing -> Nothing
{-# INLINE tBinDecodeI64 #-}

tBinDecodeBool :: ByteString -> Int -> Maybe (Bool, Int)
tBinDecodeBool !bs !off
  | off >= BS.length bs = Nothing
  | otherwise = Just (BSU.unsafeIndex bs off /= 0, off + 1)
{-# INLINE tBinDecodeBool #-}

tBinDecodeDouble :: ByteString -> Int -> Maybe (Double, Int)
tBinDecodeDouble !bs !off = case getBE64 bs off of
  Just (!w, !off') -> Just (castWord64ToDouble w, off')
  Nothing -> Nothing
{-# INLINE tBinDecodeDouble #-}

-- | Decode a string: i32 length prefix + bytes.
tBinDecodeString :: ByteString -> Int -> Maybe (ByteString, Int)
tBinDecodeString !bs !off = do
  (!lenW, !off1) <- getBE32 bs off
  let !len = fromIntegral lenW :: Int
  -- Reject negative lengths (Word32 -> Int wrap on 32-bit hosts) and
  -- impossibly-large lengths via 'len > BS.length bs - off1' so the
  -- comparison cannot overflow.
  if len < 0 || len > BS.length bs - off1
    then Nothing
    else Just (BS.take len (BS.drop off1 bs), off1 + len)
{-# INLINE tBinDecodeString #-}

-- | Decode raw binary (same wire format as string).
tBinDecodeBinary :: ByteString -> Int -> Maybe (ByteString, Int)
tBinDecodeBinary = tBinDecodeString
{-# INLINE tBinDecodeBinary #-}

-- | Decode a struct field header. Returns Nothing if at end of bytes,
-- returns (TT_STOP, 0) on stop byte, or (type, fieldId) otherwise.
tBinDecodeFieldBegin :: ByteString -> Int -> Maybe (ThriftType, Int16, Int)
tBinDecodeFieldBegin !bs !off
  | off >= BS.length bs = Nothing
  | otherwise =
      let !typeByte = BSU.unsafeIndex bs off
      in if typeByte == 0x00
         then Just (TT_STOP, 0, off + 1)
         else case thriftTypeFromBin typeByte of
           Nothing -> Nothing
           Just !tt -> case getBE16 bs (off + 1) of
             Nothing -> Nothing
             Just (!fid, !off') -> Just (tt, fromIntegral fid, off')
{-# INLINE tBinDecodeFieldBegin #-}

-- | Decode a list header: element type + i32 size.
tBinDecodeListBegin :: ByteString -> Int -> Maybe (ThriftType, Int32, Int)
tBinDecodeListBegin !bs !off
  | off >= BS.length bs = Nothing
  | otherwise =
      let !typeByte = BSU.unsafeIndex bs off
      in case thriftTypeFromBin typeByte of
           Nothing -> Nothing
           Just !tt -> case getBE32 bs (off + 1) of
             Nothing -> Nothing
             Just (!sz, !off') -> Just (tt, fromIntegral sz, off')
{-# INLINE tBinDecodeListBegin #-}

-- | Decode a set header (same as list).
tBinDecodeSetBegin :: ByteString -> Int -> Maybe (ThriftType, Int32, Int)
tBinDecodeSetBegin = tBinDecodeListBegin
{-# INLINE tBinDecodeSetBegin #-}

-- | Decode a map header: key type + value type + i32 size.
tBinDecodeMapBegin :: ByteString -> Int -> Maybe (ThriftType, ThriftType, Int32, Int)
tBinDecodeMapBegin !bs !off
  | off + 2 > BS.length bs = Nothing
  | otherwise =
      let !kByte = BSU.unsafeIndex bs off
          !vByte = BSU.unsafeIndex bs (off + 1)
      in case (thriftTypeFromBin kByte, thriftTypeFromBin vByte) of
           (Just !kt, Just !vt) -> case getBE32 bs (off + 2) of
             Nothing -> Nothing
             Just (!sz, !off') -> Just (kt, vt, fromIntegral sz, off')
           _ -> Nothing
{-# INLINE tBinDecodeMapBegin #-}

--------------------------------------------------------------------------------
-- Compact Protocol — Encode
--------------------------------------------------------------------------------

-- | Encode a byte (compact protocol).
tCompEncodeI8 :: Int8 -> Builder
tCompEncodeI8 !v = B.word8 (fromIntegral v)
{-# INLINE tCompEncodeI8 #-}

-- | Encode i16 as zigzag + varint.
tCompEncodeI16 :: Int16 -> Builder
tCompEncodeI16 !v = putVarint (fromIntegral (zigZagEncode32 (fromIntegral v)))
{-# INLINE tCompEncodeI16 #-}

-- | Encode i32 as zigzag + varint.
tCompEncodeI32 :: Int32 -> Builder
tCompEncodeI32 !v = putVarint (fromIntegral (zigZagEncode32 v))
{-# INLINE tCompEncodeI32 #-}

-- | Encode i64 as zigzag + varint.
tCompEncodeI64 :: Int64 -> Builder
tCompEncodeI64 !v = putVarint (zigZagEncode64 v)
{-# INLINE tCompEncodeI64 #-}

-- | Encode a bool. In compact protocol, booleans are typically encoded
-- in the field header. This standalone version uses a single byte.
tCompEncodeBool :: Bool -> Builder
tCompEncodeBool !b = B.word8 (if b then 1 else 0)
{-# INLINE tCompEncodeBool #-}

-- | Encode a double as 8 bytes LITTLE-endian IEEE 754.
tCompEncodeDouble :: Double -> Builder
tCompEncodeDouble !d = putLE64 (castDoubleToWord64 d)
{-# INLINE tCompEncodeDouble #-}

-- | Encode a string: varint length + bytes.
tCompEncodeString :: ByteString -> Builder
tCompEncodeString !bs =
  putVarint (fromIntegral (BS.length bs)) <> B.byteString bs
{-# INLINE tCompEncodeString #-}

-- | Encode raw binary (same wire format as string in compact).
tCompEncodeBinary :: ByteString -> Builder
tCompEncodeBinary = tCompEncodeString
{-# INLINE tCompEncodeBinary #-}

-- | Encode a compact field header using delta encoding.
--
-- If the delta from the previous field ID fits in 4 bits (1-15),
-- use the short form: (delta << 4 | compactType).
-- Otherwise, use the long form: (0x00 | compactType), then zigzag varint field ID.
--
-- For bool fields, the compact type encodes the value: 1=true, 2=false.
tCompEncodeFieldBegin :: ThriftType -> Int16 -> Int16 -> Bool -> Builder
tCompEncodeFieldBegin !tt !fid !lastFid !boolVal =
  let !delta = fid - lastFid
      !ctype = case tt of
                 TT_BOOL -> if boolVal then 1 else 2
                 _       -> thriftTypeToCompact tt
  in if delta > 0 && delta <= 15
     then B.word8 (fromIntegral delta `shiftL` 4 .|. ctype)
     else B.word8 ctype <> putVarint (fromIntegral (zigZagEncode32 (fromIntegral fid)))
{-# INLINE tCompEncodeFieldBegin #-}

-- | Encode the struct stop byte (compact).
tCompEncodeFieldStop :: Builder
tCompEncodeFieldStop = B.word8 0x00
{-# INLINE tCompEncodeFieldStop #-}

-- | Encode a compact list header.
--
-- If size < 15: single byte (size << 4 | elemType).
-- Else: (0xF0 | elemType), varint size.
tCompEncodeListBegin :: ThriftType -> Int32 -> Builder
tCompEncodeListBegin !elemType !sz =
  let !ctype = thriftTypeToCompact elemType
  in if sz < 15
     then B.word8 (fromIntegral sz `shiftL` 4 .|. ctype)
     else B.word8 (0xF0 .|. ctype) <> putVarint (fromIntegral sz)
{-# INLINE tCompEncodeListBegin #-}

-- | Encode a compact set header (same as list).
tCompEncodeSetBegin :: ThriftType -> Int32 -> Builder
tCompEncodeSetBegin = tCompEncodeListBegin
{-# INLINE tCompEncodeSetBegin #-}

-- | Encode a compact map header.
--
-- If empty: single 0x00 byte.
-- Else: varint size, then (keyType << 4 | valType).
tCompEncodeMapBegin :: ThriftType -> ThriftType -> Int32 -> Builder
tCompEncodeMapBegin !keyType !valType !sz
  | sz == 0   = B.word8 0x00
  | otherwise =
      putVarint (fromIntegral sz)
      <> B.word8 (thriftTypeToCompact keyType `shiftL` 4 .|. thriftTypeToCompact valType)
{-# INLINE tCompEncodeMapBegin #-}

-- | Raw varint encode (exposed for external use).
tCompEncodeVarint :: Word64 -> Builder
tCompEncodeVarint = putVarint
{-# INLINE tCompEncodeVarint #-}

-- | ZigZag encode (exposed for external use).
tCompEncodeZigZag :: Int64 -> Word64
tCompEncodeZigZag = zigZagEncode64
{-# INLINE tCompEncodeZigZag #-}

--------------------------------------------------------------------------------
-- Compact Protocol — Decode
--------------------------------------------------------------------------------

-- | Decode a byte (compact protocol).
tCompDecodeI8 :: ByteString -> Int -> Maybe (Int8, Int)
tCompDecodeI8 !bs !off
  | off >= BS.length bs = Nothing
  | otherwise = Just (fromIntegral (BSU.unsafeIndex bs off), off + 1)
{-# INLINE tCompDecodeI8 #-}

-- | Decode i16 from zigzag + varint.
tCompDecodeI16 :: ByteString -> Int -> Maybe (Int16, Int)
tCompDecodeI16 !bs !off = case getVarint bs off of
  Just (!w, !off') -> Just (fromIntegral (zigZagDecode32 (fromIntegral w)), off')
  Nothing -> Nothing
{-# INLINE tCompDecodeI16 #-}

-- | Decode i32 from zigzag + varint.
tCompDecodeI32 :: ByteString -> Int -> Maybe (Int32, Int)
tCompDecodeI32 !bs !off = case getVarint bs off of
  Just (!w, !off') -> Just (zigZagDecode32 (fromIntegral w), off')
  Nothing -> Nothing
{-# INLINE tCompDecodeI32 #-}

-- | Decode i64 from zigzag + varint.
tCompDecodeI64 :: ByteString -> Int -> Maybe (Int64, Int)
tCompDecodeI64 !bs !off = case getVarint bs off of
  Just (!w, !off') -> Just (zigZagDecode64 w, off')
  Nothing -> Nothing
{-# INLINE tCompDecodeI64 #-}

-- | Decode a bool (standalone, not from field header).
tCompDecodeBool :: ByteString -> Int -> Maybe (Bool, Int)
tCompDecodeBool !bs !off
  | off >= BS.length bs = Nothing
  | otherwise = Just (BSU.unsafeIndex bs off /= 0, off + 1)
{-# INLINE tCompDecodeBool #-}

-- | Decode a double as 8 bytes LITTLE-endian.
tCompDecodeDouble :: ByteString -> Int -> Maybe (Double, Int)
tCompDecodeDouble !bs !off = case getLE64 bs off of
  Just (!w, !off') -> Just (castWord64ToDouble w, off')
  Nothing -> Nothing
{-# INLINE tCompDecodeDouble #-}

-- | Decode a string: varint length + bytes.
tCompDecodeString :: ByteString -> Int -> Maybe (ByteString, Int)
tCompDecodeString !bs !off = do
  (!lenW, !off1) <- getVarint bs off
  let !len = fromIntegral lenW :: Int
  if len < 0 || len > BS.length bs - off1
    then Nothing
    else Just (BS.take len (BS.drop off1 bs), off1 + len)
{-# INLINE tCompDecodeString #-}

-- | Decode raw binary (same wire format as string in compact).
tCompDecodeBinary :: ByteString -> Int -> Maybe (ByteString, Int)
tCompDecodeBinary = tCompDecodeString
{-# INLINE tCompDecodeBinary #-}

-- | Decode a compact field header.
--
-- Returns (ThriftType, fieldId, newOffset, boolValue).
-- The boolValue is meaningful only when the type is TT_BOOL.
-- Returns (TT_STOP, 0, off+1, False) on stop byte.
--
-- Takes the previous field ID for delta decoding.
tCompDecodeFieldBegin :: ByteString -> Int -> Int16 -> Maybe (ThriftType, Int16, Int, Bool)
tCompDecodeFieldBegin !bs !off !lastFid
  | off >= BS.length bs = Nothing
  | otherwise =
      let !hdr = BSU.unsafeIndex bs off
      in if hdr == 0x00
         then Just (TT_STOP, 0, off + 1, False)
         else
           let !ctype = hdr .&. 0x0F
               !delta = hdr `shiftR` 4
               !boolVal = ctype == 1  -- BOOLEAN_TRUE
           in case thriftTypeFromCompact ctype of
                Nothing -> Nothing
                Just !tt ->
                  if delta /= 0
                  then Just (tt, lastFid + fromIntegral delta, off + 1, boolVal)
                  else case getVarint bs (off + 1) of
                    Nothing -> Nothing
                    Just (!zz, !off') ->
                      let !fid = fromIntegral (zigZagDecode32 (fromIntegral zz))
                      in Just (tt, fid, off', boolVal)
{-# INLINE tCompDecodeFieldBegin #-}

-- | Decode a compact list header.
-- Returns (elemType, size, newOffset).
tCompDecodeListBegin :: ByteString -> Int -> Maybe (ThriftType, Int32, Int)
tCompDecodeListBegin !bs !off
  | off >= BS.length bs = Nothing
  | otherwise =
      let !hdr   = BSU.unsafeIndex bs off
          !szHi  = hdr `shiftR` 4
          !ctype = hdr .&. 0x0F
      in case thriftTypeFromCompact ctype of
           Nothing -> Nothing
           Just !tt ->
             if szHi /= 0x0F
             then Just (tt, fromIntegral szHi, off + 1)
             else case getVarint bs (off + 1) of
               Nothing -> Nothing
               Just (!sz, !off') -> Just (tt, fromIntegral sz, off')
{-# INLINE tCompDecodeListBegin #-}

-- | Decode a compact set header (same as list).
tCompDecodeSetBegin :: ByteString -> Int -> Maybe (ThriftType, Int32, Int)
tCompDecodeSetBegin = tCompDecodeListBegin
{-# INLINE tCompDecodeSetBegin #-}

-- | Decode a compact map header.
-- Returns (keyType, valType, size, newOffset).
-- For empty maps: a single 0x00 byte (size=0).
tCompDecodeMapBegin :: ByteString -> Int -> Maybe (ThriftType, ThriftType, Int32, Int)
tCompDecodeMapBegin !bs !off = do
  (!szW, !off1) <- getVarint bs off
  let !sz = fromIntegral szW :: Int32
  if sz == 0
    then Just (TT_STOP, TT_STOP, 0, off1)
    else do
      if off1 >= BS.length bs
        then Nothing
        else do
          let !kvByte = BSU.unsafeIndex bs off1
              !kCode  = kvByte `shiftR` 4
              !vCode  = kvByte .&. 0x0F
          kt <- thriftTypeFromCompact kCode
          vt <- thriftTypeFromCompact vCode
          Just (kt, vt, sz, off1 + 1)
{-# INLINE tCompDecodeMapBegin #-}

-- | Raw varint decode (exposed for external use).
tCompDecodeVarint :: ByteString -> Int -> Maybe (Word64, Int)
tCompDecodeVarint = getVarint
{-# INLINE tCompDecodeVarint #-}

-- | ZigZag decode (exposed for external use).
tCompDecodeZigZag :: Word64 -> Int64
tCompDecodeZigZag = zigZagDecode64
{-# INLINE tCompDecodeZigZag #-}
