{-# LANGUAGE BangPatterns #-}
-- | High-level Avro binary encoding.
--
-- Encodes an 'Avro.Value.Value' according to an 'AvroType' schema using
-- direct buffer writes via 'Wireform.Encode.Direct.directEncode'.
--
-- @
-- import Avro.Encode (encodeAvro)
-- import Avro.Schema (AvroType(..), AvroPrimitive(..))
-- import qualified Avro.Value as A
--
-- let bytes = encodeAvro (AvroPrimitive AvroString) (A.String \"hello\")
-- @
module Avro.Encode
  ( encodeAvro
  , encodeAvroBuilder
  ) where

import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word8, Word64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Float (castFloatToWord32, castDoubleToWord64)

import Avro.Schema (AvroType(..), AvroSchema(..), AvroField(..))
import qualified Avro.Value as AV
import Avro.Wire
  ( avroEncodeNull, avroEncodeBool, avroEncodeInt, avroEncodeLong
  , avroEncodeFloat, avroEncodeDouble, avroEncodeBytes, avroEncodeString
  , avroVarintSize
  )
import Wireform.Encode.Direct (directEncode)

-- | Encode a value according to its Avro schema, returning strict 'ByteString'.
encodeAvro :: AvroType -> AV.Value -> ByteString
encodeAvro !ty !val = directEncode (avroValueSize ty val) (writeAvro ty val)
{-# INLINE encodeAvro #-}

-- | Encode a value as a 'Builder' for zero-copy composition (kept for API compat).
encodeAvroBuilder :: AvroType -> AV.Value -> Builder
encodeAvroBuilder = goBuilder
  where
    goBuilder :: AvroType -> AV.Value -> Builder
    goBuilder (AvroPrimitive s) v = encodePrimitive s v
    goBuilder (AvroRecord {avroRecordFields = fields}) (AV.Record vals) =
      encodeRecord (V.toList fields) (V.toList vals)
    goBuilder (AvroEnum {}) (AV.Enum idx) =
      avroEncodeInt (fromIntegral idx)
    goBuilder (AvroArray {avroArrayItems = itemTy}) (AV.Array items) =
      encodeArray itemTy (V.toList items)
    goBuilder (AvroMap {avroMapValues = valTy}) (AV.Map entries) =
      encodeMap valTy (V.toList entries)
    goBuilder (AvroUnion {avroUnionBranches = branches}) (AV.Union idx val') =
      avroEncodeLong (fromIntegral idx) <> goBuilder (V.unsafeIndex branches idx) val'
    goBuilder (AvroFixed {}) (AV.Fixed bs) =
      B.byteString bs
    goBuilder (AvroLogical {avroLogicalBase = base}) v =
      goBuilder base v
    goBuilder _ v = error $ "Avro.Encode: schema/value mismatch: " ++ show v

    encodePrimitive :: AvroSchema -> AV.Value -> Builder
    encodePrimitive AvroNull   AV.Null       = avroEncodeNull
    encodePrimitive AvroBool   (AV.Bool b)   = avroEncodeBool b
    encodePrimitive AvroInt    (AV.Int n)    = avroEncodeInt n
    encodePrimitive AvroLong   (AV.Long n)   = avroEncodeLong n
    encodePrimitive AvroFloat  (AV.Float f)  = avroEncodeFloat f
    encodePrimitive AvroDouble (AV.Double d)  = avroEncodeDouble d
    encodePrimitive AvroBytes  (AV.Bytes bs)  = avroEncodeBytes bs
    encodePrimitive AvroString (AV.String t)  = avroEncodeString t
    encodePrimitive s v = error $ "Avro.Encode: primitive mismatch: " ++ show s ++ " vs " ++ show v

    encodeRecord :: [AvroField] -> [AV.Value] -> Builder
    encodeRecord [] [] = mempty
    encodeRecord (f:fs) (v:vs) =
      goBuilder (avroFieldType f) v <> encodeRecord fs vs
    encodeRecord _ _ = error "Avro.Encode: record field count mismatch"

    encodeArray :: AvroType -> [AV.Value] -> Builder
    encodeArray _itemTy [] = avroEncodeLong 0
    encodeArray itemTy items =
      let !cnt = length items
      in avroEncodeLong (fromIntegral cnt)
         <> foldMap (goBuilder itemTy) items
         <> avroEncodeLong 0

    encodeMap :: AvroType -> [(Text, AV.Value)] -> Builder
    encodeMap _valTy [] = avroEncodeLong 0
    encodeMap valTy entries =
      let !cnt = length entries
      in avroEncodeLong (fromIntegral cnt)
         <> foldMap (\(k, v) -> avroEncodeString k <> goBuilder valTy v) entries
         <> avroEncodeLong 0

-- Size computation

avroValueSize :: AvroType -> AV.Value -> Int
avroValueSize (AvroPrimitive s) v = primSize s v
avroValueSize (AvroRecord {avroRecordFields = fields}) (AV.Record vals) =
  recordFieldsSize fields vals 0
avroValueSize (AvroEnum {}) (AV.Enum idx) =
  avroVarintSize (fromIntegral idx)
avroValueSize (AvroArray {avroArrayItems = itemTy}) (AV.Array items) =
  arraySizeAvro itemTy items
avroValueSize (AvroMap {avroMapValues = valTy}) (AV.Map entries) =
  mapSizeAvro valTy entries
avroValueSize (AvroUnion {avroUnionBranches = branches}) (AV.Union idx val') =
  avroVarintSize (fromIntegral idx) + avroValueSize (V.unsafeIndex branches idx) val'
avroValueSize (AvroFixed {}) (AV.Fixed bs) = BS.length bs
avroValueSize (AvroLogical {avroLogicalBase = base}) v = avroValueSize base v
avroValueSize _ _ = 0

primSize :: AvroSchema -> AV.Value -> Int
primSize AvroNull   AV.Null       = 0
primSize AvroBool   (AV.Bool _)   = 1
primSize AvroInt    (AV.Int n)    = avroVarintSize (fromIntegral n)
primSize AvroLong   (AV.Long n)   = avroVarintSize n
primSize AvroFloat  (AV.Float _)  = 4
primSize AvroDouble (AV.Double _) = 8
primSize AvroBytes  (AV.Bytes bs) = avroVarintSize (fromIntegral (BS.length bs)) + BS.length bs
primSize AvroString (AV.String t) = let !bs = TE.encodeUtf8 t
                                        !len = BS.length bs
                                    in avroVarintSize (fromIntegral len) + len
primSize _ _ = 0

recordFieldsSize :: V.Vector AvroField -> V.Vector AV.Value -> Int -> Int
recordFieldsSize fields vals !acc
  | V.null fields = acc
  | otherwise =
      let !f = V.head fields
          !v = V.head vals
          !sz = avroValueSize (avroFieldType f) v
      in recordFieldsSize (V.tail fields) (V.tail vals) (acc + sz)

arraySizeAvro :: AvroType -> V.Vector AV.Value -> Int
arraySizeAvro _itemTy items
  | V.null items = avroVarintSize 0
  | otherwise =
      let !cnt = V.length items
          !blockSz = V.foldl' (\s v -> s + avroValueSize _itemTy v) 0 items
      in avroVarintSize (fromIntegral cnt) + blockSz + avroVarintSize 0

mapSizeAvro :: AvroType -> V.Vector (Text, AV.Value) -> Int
mapSizeAvro _valTy entries
  | V.null entries = avroVarintSize 0
  | otherwise =
      let !cnt = V.length entries
          !blockSz = V.foldl' (\s (k, v) ->
            let !kbs = TE.encodeUtf8 k
                !klen = BS.length kbs
            in s + avroVarintSize (fromIntegral klen) + klen + avroValueSize _valTy v) 0 entries
      in avroVarintSize (fromIntegral cnt) + blockSz + avroVarintSize 0

-- Offset-based writers

writeAvro :: AvroType -> AV.Value -> Ptr Word8 -> Int -> IO Int
writeAvro ty val p off = writeAvroValue ty val p off

writeAvroValue :: AvroType -> AV.Value -> Ptr Word8 -> Int -> IO Int
writeAvroValue (AvroPrimitive s) v p off = writePrim s v p off
writeAvroValue (AvroRecord {avroRecordFields = fields}) (AV.Record vals) p off =
  writeRecordFields fields vals p off 0
writeAvroValue (AvroEnum {}) (AV.Enum idx) p off =
  writeZigZagVarint p off (fromIntegral idx)
writeAvroValue (AvroArray {avroArrayItems = itemTy}) (AV.Array items) p off =
  writeArrayAvro itemTy items p off
writeAvroValue (AvroMap {avroMapValues = valTy}) (AV.Map entries) p off =
  writeMapAvro valTy entries p off
writeAvroValue (AvroUnion {avroUnionBranches = branches}) (AV.Union idx val') p off = do
  off1 <- writeZigZagVarint p off (fromIntegral idx)
  writeAvroValue (V.unsafeIndex branches idx) val' p off1
writeAvroValue (AvroFixed {}) (AV.Fixed bs) p off = writeRaw p off bs
writeAvroValue (AvroLogical {avroLogicalBase = base}) v p off = writeAvroValue base v p off
writeAvroValue _ _ _ off = pure off

writePrim :: AvroSchema -> AV.Value -> Ptr Word8 -> Int -> IO Int
writePrim AvroNull   AV.Null       _ off = pure off
writePrim AvroBool   (AV.Bool b)   p off = do
  pokeByteOff p off (if b then 0x01 :: Word8 else 0x00)
  pure $! off + 1
writePrim AvroInt    (AV.Int n)    p off = writeZigZagVarint p off (fromIntegral n)
writePrim AvroLong   (AV.Long n)   p off = writeZigZagVarint p off n
writePrim AvroFloat  (AV.Float f)  p off = do
  pokeByteOff p off (castFloatToWord32 f)
  pure $! off + 4
writePrim AvroDouble (AV.Double d) p off = do
  pokeByteOff p off (castDoubleToWord64 d)
  pure $! off + 8
writePrim AvroBytes  (AV.Bytes bs) p off = do
  off1 <- writeZigZagVarint p off (fromIntegral (BS.length bs))
  writeRaw p off1 bs
writePrim AvroString (AV.String t) p off = do
  let !bs = TE.encodeUtf8 t
      !len = BS.length bs
  off1 <- writeZigZagVarint p off (fromIntegral len)
  writeRaw p off1 bs
writePrim _ _ _ off = pure off

writeRecordFields :: V.Vector AvroField -> V.Vector AV.Value -> Ptr Word8 -> Int -> Int -> IO Int
writeRecordFields fields vals p off !idx
  | idx >= V.length fields = pure off
  | otherwise = do
      let !f = V.unsafeIndex fields idx
          !v = V.unsafeIndex vals idx
      off1 <- writeAvroValue (avroFieldType f) v p off
      writeRecordFields fields vals p off1 (idx + 1)

writeArrayAvro :: AvroType -> V.Vector AV.Value -> Ptr Word8 -> Int -> IO Int
writeArrayAvro _itemTy items p off
  | V.null items = writeZigZagVarint p off 0
  | otherwise = do
      off1 <- writeZigZagVarint p off (fromIntegral (V.length items))
      off2 <- V.foldM' (\o v -> writeAvroValue _itemTy v p o) off1 items
      writeZigZagVarint p off2 0

writeMapAvro :: AvroType -> V.Vector (Text, AV.Value) -> Ptr Word8 -> Int -> IO Int
writeMapAvro _valTy entries p off
  | V.null entries = writeZigZagVarint p off 0
  | otherwise = do
      off1 <- writeZigZagVarint p off (fromIntegral (V.length entries))
      off2 <- V.foldM' (\o (k, v) -> do
        let !kbs = TE.encodeUtf8 k
            !klen = BS.length kbs
        o1 <- writeZigZagVarint p o (fromIntegral klen)
        o2 <- writeRaw p o1 kbs
        writeAvroValue _valTy v p o2) off1 entries
      writeZigZagVarint p off2 0

-- Shared write helpers

writeRaw :: Ptr Word8 -> Int -> ByteString -> IO Int
writeRaw p off (BSI.BS fp len) = do
  withForeignPtr fp $ \src -> BSI.memcpy (p `plusPtr` off) src len
  pure $! off + len
{-# INLINE writeRaw #-}

zigZag64 :: Int64 -> Word64
zigZag64 n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 63))
{-# INLINE zigZag64 #-}

writeZigZagVarint :: Ptr Word8 -> Int -> Int64 -> IO Int
writeZigZagVarint p off n = writeVarint p off (zigZag64 n)
{-# INLINE writeZigZagVarint #-}

writeVarint :: Ptr Word8 -> Int -> Word64 -> IO Int
writeVarint !p !off !n
  | n < 0x80 = do
      pokeByteOff p off (fromIntegral n :: Word8)
      pure $! off + 1
  | n < 0x4000 = do
      pokeByteOff p off (fromIntegral (n .&. 0x7F .|. 0x80) :: Word8)
      pokeByteOff p (off + 1) (fromIntegral (n `shiftR` 7) :: Word8)
      pure $! off + 2
  | n < 0x200000 = do
      pokeByteOff p off (fromIntegral (n .&. 0x7F .|. 0x80) :: Word8)
      pokeByteOff p (off + 1) (fromIntegral ((n `shiftR` 7) .&. 0x7F .|. 0x80) :: Word8)
      pokeByteOff p (off + 2) (fromIntegral (n `shiftR` 14) :: Word8)
      pure $! off + 3
  | otherwise = writeVarintSlow p off n
{-# INLINE writeVarint #-}

writeVarintSlow :: Ptr Word8 -> Int -> Word64 -> IO Int
writeVarintSlow !p !off !n
  | n < 0x80 = do
      pokeByteOff p off (fromIntegral n :: Word8)
      pure $! off + 1
  | otherwise = do
      pokeByteOff p off (fromIntegral (n .&. 0x7F .|. 0x80) :: Word8)
      writeVarintSlow p (off + 1) (n `shiftR` 7)
