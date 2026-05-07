{-# LANGUAGE BangPatterns #-}
-- | Apache Arrow IPC message framing.
--
-- Arrow IPC messages:
--   continuation (0xFFFFFFFF) + 4-byte LE metadata size + FlatBuffer metadata + padding + body
--
-- We use a simplified FlatBuffer encoding for the Arrow Message wrapper,
-- with the schema/record batch serialized into the metadata flatbuffer.
-- Buffer validation uses SIMD-accelerated checks via 'Proto.Wire.FFI'.
module Arrow.IPC
  ( encodeIPCMessage
  , decodeIPCMessage
  , validateRecordBatchBuffers
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int32, Int64)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafePerformIO)

import Arrow.Types
import Wireform.FFI (validateArrowBuffers)

encodeIPCMessage :: Message -> ByteString
encodeIPCMessage msg =
  let !metadataBytes = encodeMessageFlatBuf msg
      !metaLen = BS.length metadataBytes
      !paddedMetaLen = alignTo8 metaLen
      !metaPadding = paddedMetaLen - metaLen
  in BL.toStrict $ B.toLazyByteString $
       buildLE32 0xFFFFFFFF  -- continuation
       <> buildLE32 (fromIntegral paddedMetaLen)  -- metadata size
       <> B.byteString metadataBytes
       <> B.byteString (BS.replicate metaPadding 0)

decodeIPCMessage :: ByteString -> Either String Message
decodeIPCMessage bs = do
  ensure bs 0 8
  let !cont = readLE32 bs 0
  if cont /= 0xFFFFFFFF
    then Left "Arrow.IPC: missing continuation marker"
    else do
      let !metaLen = fromIntegral (readLE32 bs 4) :: Int
      ensure bs 8 metaLen
      let !metaBytes = BSU.unsafeTake metaLen (BSU.unsafeDrop 8 bs)
      decodeMessageFlatBuf metaBytes

-- Simplified FlatBuffer encoding for Arrow Message metadata.
-- Message table layout:
--   version (i16), header_type (i8), header (union), bodyLength (i64)

encodeMessageFlatBuf :: Message -> ByteString
encodeMessageFlatBuf msg =
  let (!headerType, !headerBytes) = encodeHeader msg
      !bodyLength = case msg of
            RecordBatch _ -> 0 :: Int64
            _             -> 0
      -- Build a simple flat encoding: version + header_type + header + body_length
      builder =
        buildLE16 4  -- MetadataVersion.V4
        <> B.word8 headerType
        <> buildLE32 (fromIntegral (BS.length headerBytes))
        <> B.byteString headerBytes
        <> buildLE64 (fromIntegral bodyLength)
  in BL.toStrict (B.toLazyByteString builder)

encodeHeader :: Message -> (Word8, ByteString)
encodeHeader (SchemaMessage schema) = (1, encodeSchema schema)
encodeHeader DictionaryBatch = (2, BS.empty)
encodeHeader (RecordBatch rb) = (3, encodeRecordBatch rb)

encodeSchema :: Schema -> ByteString
encodeSchema schema = BL.toStrict $ B.toLazyByteString $
  B.word8 (fromIntegral (fromEnum (arrowEndianness schema)))
  <> buildLE32 (fromIntegral (V.length (arrowFields schema)))
  <> V.foldl' (\acc f -> acc <> encodeField f) mempty (arrowFields schema)

encodeField :: Field -> B.Builder
encodeField f =
  let !nameBS = TE.encodeUtf8 (fieldName f)
  in buildLE16 (fromIntegral (BS.length nameBS))
     <> B.byteString nameBS
     <> B.word8 (if fieldNullable f then 1 else 0)
     <> encodeArrowType (fieldType f)
     <> buildLE32 (fromIntegral (V.length (fieldChildren f)))
     <> V.foldl' (\acc c -> acc <> encodeField c) mempty (fieldChildren f)

encodeArrowType :: ArrowType -> B.Builder
encodeArrowType = \case
  ANull             -> B.word8 0
  AInt bits signed  -> B.word8 1 <> B.word8 (fromIntegral bits) <> B.word8 (if signed then 1 else 0)
  AFloatingPoint p  -> B.word8 2 <> B.word8 (fromIntegral (fromEnum p))
  ABinary           -> B.word8 3
  AUtf8             -> B.word8 4
  ABool             -> B.word8 5
  ADecimal p s      -> B.word8 6 <> B.word8 (fromIntegral p) <> B.word8 (fromIntegral s)
  ADate u           -> B.word8 7 <> B.word8 (fromIntegral (fromEnum u))
  ATime u bits      -> B.word8 8 <> B.word8 (fromIntegral (fromEnum u)) <> B.word8 (fromIntegral bits)
  ATimestamp u tz   -> B.word8 9 <> B.word8 (fromIntegral (fromEnum u))
                       <> case tz of
                            Nothing -> buildLE16 0
                            Just t ->
                              let !bs = TE.encodeUtf8 t
                              in buildLE16 (fromIntegral (BS.length bs)) <> B.byteString bs
  AInterval u       -> B.word8 10 <> B.word8 (fromIntegral (fromEnum u))
  AList             -> B.word8 11
  AStruct           -> B.word8 12
  AUnion mode ids   -> B.word8 13 <> B.word8 (fromIntegral (fromEnum mode))
                       <> buildLE32 (fromIntegral (V.length ids))
                       <> V.foldl' (\acc i -> acc <> buildLE32 (fromIntegral i)) mempty ids
  AFixedSizeBinary n -> B.word8 14 <> buildLE32 (fromIntegral n)
  AFixedSizeList n  -> B.word8 15 <> buildLE32 (fromIntegral n)
  AMap sorted       -> B.word8 16 <> B.word8 (if sorted then 1 else 0)
  ADuration u       -> B.word8 17 <> B.word8 (fromIntegral (fromEnum u))
  ALargeBinary      -> B.word8 18
  ALargeUtf8        -> B.word8 19
  ALargeList        -> B.word8 20
  ADecimal256 p s   -> B.word8 21 <> B.word8 (fromIntegral p) <> B.word8 (fromIntegral s)

encodeRecordBatch :: RecordBatchDef -> ByteString
encodeRecordBatch rb = BL.toStrict $ B.toLazyByteString $
  buildLE64 (fromIntegral (rbLength rb))
  <> buildLE32 (fromIntegral (VS.length (rbNodes rb)))
  <> VS.foldl' (\acc n -> acc <> buildLE64 (fromIntegral (fnLength n))
                              <> buildLE64 (fromIntegral (fnNullCount n))) mempty (rbNodes rb)
  <> buildLE32 (fromIntegral (VS.length (rbBuffers rb)))
  <> VS.foldl' (\acc b -> acc <> buildLE64 (fromIntegral (bufOffset b))
                              <> buildLE64 (fromIntegral (bufLength b))) mempty (rbBuffers rb)

-- Decoding

decodeMessageFlatBuf :: ByteString -> Either String Message
decodeMessageFlatBuf bs = do
  ensure bs 0 4
  let !_version = readLE16 bs 0
      !headerType = rdByte bs 2
      !headerLen = fromIntegral (readLE32 bs 3) :: Int
  ensure bs 7 headerLen
  let !headerBytes = BSU.unsafeTake headerLen (BSU.unsafeDrop 7 bs)
  case headerType of
    1 -> SchemaMessage <$> decodeSchema headerBytes
    2 -> Right DictionaryBatch
    3 -> RecordBatch <$> decodeRecordBatch headerBytes
    _ -> Left $ "Arrow.IPC: unknown header type " ++ show headerType

decodeSchema :: ByteString -> Either String Schema
decodeSchema bs = do
  ensure bs 0 5
  let !endian = if rdByte bs 0 == 0 then Little else Big
      !nFields = fromIntegral (readLE32 bs 1) :: Int
  (fields, _) <- decodeFields bs 5 nFields
  Right Schema
    { arrowFields     = V.fromList fields
    , arrowEndianness = endian
    , arrowMetadata   = V.empty
    , arrowFeatures = V.empty
    }

decodeFields :: ByteString -> Int -> Int -> Either String ([Field], Int)
decodeFields bs off 0 = Right ([], off)
decodeFields bs off n = do
  (f, off') <- decodeField bs off
  (rest, off'') <- decodeFields bs off' (n - 1)
  Right (f : rest, off'')

decodeField :: ByteString -> Int -> Either String (Field, Int)
decodeField bs off = do
  ensure bs off 2
  let !nameLen = fromIntegral (readLE16 bs off) :: Int
  ensure bs (off + 2) nameLen
  let !nameRaw = BSU.unsafeTake nameLen (BSU.unsafeDrop (off + 2) bs)
  name <- case TE.decodeUtf8' nameRaw of
            Left _ -> Left "Arrow.IPC: invalid UTF-8 in field name"
            Right t -> Right t
  let !off1 = off + 2 + nameLen
  ensure bs off1 1
  let !nullable = rdByte bs off1 /= 0
      !off2 = off1 + 1
  (atype, off3) <- decodeArrowType bs off2
  ensure bs off3 4
  let !nChildren = fromIntegral (readLE32 bs off3) :: Int
  (children, off4) <- decodeFields bs (off3 + 4) nChildren
  Right (Field name nullable atype (V.fromList children) Nothing V.empty, off4)

decodeArrowType :: ByteString -> Int -> Either String (ArrowType, Int)
decodeArrowType bs off = do
  ensure bs off 1
  let !tag = rdByte bs off
  case tag of
    0  -> Right (ANull, off + 1)
    1  -> do ensure bs (off + 1) 2
             let !bits = fromIntegral (rdByte bs (off + 1))
                 !signed = rdByte bs (off + 2) /= 0
             Right (AInt bits signed, off + 3)
    2  -> do ensure bs (off + 1) 1
             Right (AFloatingPoint (toEnum (fromIntegral (rdByte bs (off + 1)))), off + 2)
    3  -> Right (ABinary, off + 1)
    4  -> Right (AUtf8, off + 1)
    5  -> Right (ABool, off + 1)
    6  -> do ensure bs (off + 1) 2
             Right (ADecimal (fromIntegral (rdByte bs (off + 1))) (fromIntegral (rdByte bs (off + 2))), off + 3)
    7  -> do ensure bs (off + 1) 1
             Right (ADate (toEnum (fromIntegral (rdByte bs (off + 1)))), off + 2)
    8  -> do ensure bs (off + 1) 2
             Right (ATime (toEnum (fromIntegral (rdByte bs (off + 1)))) (fromIntegral (rdByte bs (off + 2))), off + 3)
    9  -> do ensure bs (off + 1) 3
             let !unit = toEnum (fromIntegral (rdByte bs (off + 1)))
                 !tzLen = fromIntegral (readLE16 bs (off + 2)) :: Int
             if tzLen == 0
               then Right (ATimestamp unit Nothing, off + 4)
               else do
                 ensure bs (off + 4) tzLen
                 let !tzRaw = BSU.unsafeTake tzLen (BSU.unsafeDrop (off + 4) bs)
                 case TE.decodeUtf8' tzRaw of
                   Left _ -> Left "Arrow.IPC: invalid timezone"
                   Right t -> Right (ATimestamp unit (Just t), off + 4 + tzLen)
    10 -> do ensure bs (off + 1) 1
             Right (AInterval (toEnum (fromIntegral (rdByte bs (off + 1)))), off + 2)
    11 -> Right (AList, off + 1)
    12 -> Right (AStruct, off + 1)
    13 -> do ensure bs (off + 1) 5
             let !mode = toEnum (fromIntegral (rdByte bs (off + 1)))
                 !nIds = fromIntegral (readLE32 bs (off + 2)) :: Int
             ensure bs (off + 6) (nIds * 4)
             let !ids = V.generate nIds (\i -> fromIntegral (readLE32 bs (off + 6 + i * 4)) :: Int32)
             Right (AUnion mode ids, off + 6 + nIds * 4)
    14 -> do ensure bs (off + 1) 4
             Right (AFixedSizeBinary (fromIntegral (readLE32 bs (off + 1))), off + 5)
    15 -> do ensure bs (off + 1) 4
             Right (AFixedSizeList (fromIntegral (readLE32 bs (off + 1))), off + 5)
    16 -> do ensure bs (off + 1) 1
             Right (AMap (rdByte bs (off + 1) /= 0), off + 2)
    17 -> do ensure bs (off + 1) 1
             Right (ADuration (toEnum (fromIntegral (rdByte bs (off + 1)))), off + 2)
    18 -> Right (ALargeBinary, off + 1)
    19 -> Right (ALargeUtf8, off + 1)
    20 -> Right (ALargeList, off + 1)
    21 -> do ensure bs (off + 1) 2
             Right (ADecimal256 (fromIntegral (rdByte bs (off + 1))) (fromIntegral (rdByte bs (off + 2))), off + 3)
    _  -> Left $ "Arrow.IPC: unknown type tag " ++ show tag

decodeRecordBatch :: ByteString -> Either String RecordBatchDef
decodeRecordBatch bs = do
  ensure bs 0 12
  let !len = fromIntegral (readLE64 bs 0) :: Int64
      !nNodes = fromIntegral (readLE32 bs 8) :: Int
  ensure bs 12 (nNodes * 16)
  let !nodes = VS.generate nNodes (\i ->
        let !o = 12 + i * 16
        in FieldNode (fromIntegral (readLE64 bs o)) (fromIntegral (readLE64 bs (o + 8))))
      !off2 = 12 + nNodes * 16
  ensure bs off2 4
  let !nBufs = fromIntegral (readLE32 bs off2) :: Int
  ensure bs (off2 + 4) (nBufs * 16)
  let !bufs = VS.generate nBufs (\i ->
        let !o = off2 + 4 + i * 16
        in Buffer (fromIntegral (readLE64 bs o)) (fromIntegral (readLE64 bs (o + 8))))
  Right RecordBatchDef
    { rbLength = len
    , rbNodes = nodes
    , rbBuffers = bufs
    , rbVariadicBufferCounts = VS.empty
    , rbBodyCompression = Nothing
    }

-- Primitives

alignTo8 :: Int -> Int
alignTo8 n = (n + 7) .&. (complement 7)
  where complement x = -1 - x + 1  -- two's complement for .&.

rdByte :: ByteString -> Int -> Word8
rdByte !bs !off = BSU.unsafeIndex bs off
{-# INLINE rdByte #-}

readLE16 :: ByteString -> Int -> Word16
readLE16 bs off =
  fromIntegral (rdByte bs off) .|. (fromIntegral (rdByte bs (off + 1)) `shiftL` 8)

readLE32 :: ByteString -> Int -> Word32
readLE32 bs off =
  let !b0 = fromIntegral (rdByte bs off) :: Word32
      !b1 = fromIntegral (rdByte bs (off + 1)) :: Word32
      !b2 = fromIntegral (rdByte bs (off + 2)) :: Word32
      !b3 = fromIntegral (rdByte bs (off + 3)) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)

readLE64 :: ByteString -> Int -> Word64
readLE64 bs off =
  let rd i = fromIntegral (rdByte bs (off + i)) :: Word64
  in rd 0 .|. (rd 1 `shiftL` 8) .|. (rd 2 `shiftL` 16) .|. (rd 3 `shiftL` 24)
     .|. (rd 4 `shiftL` 32) .|. (rd 5 `shiftL` 40) .|. (rd 6 `shiftL` 48) .|. (rd 7 `shiftL` 56)

buildLE16 :: Word16 -> B.Builder
buildLE16 w = B.word8 (fromIntegral (w .&. 0xFF))
           <> B.word8 (fromIntegral ((w `shiftR` 8) .&. 0xFF))

buildLE32 :: Word32 -> B.Builder
buildLE32 w = B.word8 (fromIntegral (w .&. 0xFF))
           <> B.word8 (fromIntegral ((w `shiftR` 8) .&. 0xFF))
           <> B.word8 (fromIntegral ((w `shiftR` 16) .&. 0xFF))
           <> B.word8 (fromIntegral ((w `shiftR` 24) .&. 0xFF))

buildLE64 :: Word64 -> B.Builder
buildLE64 w = B.word8 (fromIntegral (w .&. 0xFF))
           <> B.word8 (fromIntegral ((w `shiftR` 8) .&. 0xFF))
           <> B.word8 (fromIntegral ((w `shiftR` 16) .&. 0xFF))
           <> B.word8 (fromIntegral ((w `shiftR` 24) .&. 0xFF))
           <> B.word8 (fromIntegral ((w `shiftR` 32) .&. 0xFF))
           <> B.word8 (fromIntegral ((w `shiftR` 40) .&. 0xFF))
           <> B.word8 (fromIntegral ((w `shiftR` 48) .&. 0xFF))
           <> B.word8 (fromIntegral ((w `shiftR` 56) .&. 0xFF))

ensure :: ByteString -> Int -> Int -> Either String ()
ensure bs off n
  | off + n > BS.length bs = Left "Arrow.IPC: unexpected end of input"
  | otherwise = Right ()
{-# INLINE ensure #-}

-- | Validate that all buffer offset/length pairs in a 'RecordBatchDef' are
-- non-negative, within the given body length, and non-overlapping.
-- Uses SIMD-accelerated pairwise checks.
validateRecordBatchBuffers :: RecordBatchDef -> Int64 -> Bool
validateRecordBatchBuffers rb bodyLen = unsafePerformIO $ do
  let !bufs = rbBuffers rb
      !n = VS.length bufs
  if n == 0
    then pure True
    else
      -- VS.Vector Buffer is laid out as N x (Int64,Int64) =
      -- N x 16 bytes, exactly the shape validateArrowBuffers
      -- expects. Hand the underlying ForeignPtr's pointer
      -- straight in — no per-buffer copy.
      VS.unsafeWith bufs $ \bufPtr ->
        pure $! validateArrowBuffers (castPtr bufPtr :: Ptr Int64) n bodyLen
{-# INLINE validateRecordBatchBuffers #-}
