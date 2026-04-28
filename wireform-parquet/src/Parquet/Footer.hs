{-# LANGUAGE BangPatterns #-}
-- | Read/write Apache Parquet file footer.
--
-- Parquet file layout ends with:
--   [Thrift Compact Protocol encoded FileMetadata] [4-byte LE metadata length] [PAR1 magic]
--
-- We use the existing Thrift Compact Protocol encoder/decoder to serialize
-- the FileMetadata as a Thrift struct.
module Parquet.Footer
  ( readFooter
  , writeFooter
  , parquetMagic
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int16, Int32, Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding
import Data.Word (Word32)
import qualified Data.Vector as V

import Parquet.Footer.Build
  ( buildFields
  , pushField
  , pushFieldMb
  , pushFieldWhen
  )
import Parquet.Types
import qualified Thrift.Value as TV
import qualified Thrift.Wire as TW
import Thrift.Encode (encodeCompactSorted)
import Thrift.Decode (decodeCompact)

parquetMagic :: ByteString
parquetMagic = BS.pack [0x50, 0x41, 0x52, 0x31]

writeFooter :: FileMetadata -> ByteString
writeFooter fm =
  let !thriftVal = fileMetadataToThrift fm
      -- All Parquet *ToThrift builders emit fields in ascending
      -- order, so we use the sorted-fast-path 'encodeCompactSorted'
      -- to skip the encoder's two redundant 'sortBy . V.toList' walks
      -- (one for the size pass, one for the write pass).
      !encoded = encodeCompactSorted thriftVal
      !metaLen = BS.length encoded
  in BL.toStrict $ B.toLazyByteString $
       B.byteString encoded
       <> B.word8 (fromIntegral (metaLen .&. 0xFF))
       <> B.word8 (fromIntegral ((metaLen `shiftR` 8) .&. 0xFF))
       <> B.word8 (fromIntegral ((metaLen `shiftR` 16) .&. 0xFF))
       <> B.word8 (fromIntegral ((metaLen `shiftR` 24) .&. 0xFF))
       <> B.byteString parquetMagic

readFooter :: ByteString -> Either String FileMetadata
readFooter bs
  | BS.length bs < 8 = Left "Parquet.Footer: input too short"
  | otherwise = do
      let !totalLen = BS.length bs
          !magic = BSU.unsafeTake 4 (BSU.unsafeDrop (totalLen - 4) bs)
      if magic /= parquetMagic
        then Left "Parquet.Footer: invalid magic (expected PAR1)"
        else do
          let !metaLenOff = totalLen - 8
              !metaLen = fromIntegral (readLE32 bs metaLenOff) :: Int
          if metaLen < 0 || metaLen > totalLen - 8
            then Left "Parquet.Footer: invalid metadata length"
            else do
              let !metaStart = totalLen - 8 - metaLen
                  !metaBytes = BSU.unsafeTake metaLen (BSU.unsafeDrop metaStart bs)
              thriftVal <- decodeCompact metaBytes
              thriftToFileMetadata thriftVal

readLE32 :: ByteString -> Int -> Word32
readLE32 bs off =
  let !b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word32
      !b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word32
      !b2 = fromIntegral (BSU.unsafeIndex bs (off + 2)) :: Word32
      !b3 = fromIntegral (BSU.unsafeIndex bs (off + 3)) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)

-- Thrift field IDs for FileMetadata:
-- 1: version (i32), 2: schema (list<SchemaElement>), 3: num_rows (i64),
-- 4: row_groups (list<RowGroup>), 5: created_by (string)

-- ============================================================
-- *ToThrift encoders
--
-- Every encoder in this module pushes its fields in ascending
-- 'Int16' order, with no allocation per absent optional field.  The
-- 'buildFields' / 'pushFieldMb' combinators in "Parquet.Footer.Build"
-- write into a pre-sized mutable vector and freeze the populated
-- prefix; downstream 'encodeCompactSorted' relies on that ordering
-- to skip the redundant per-struct 'sortBy . V.toList' that
-- 'encodeCompact' would otherwise perform twice.
-- ============================================================

fileMetadataToThrift :: FileMetadata -> TV.Value
fileMetadataToThrift fm = TV.Struct $ buildFields 5 $ \fb -> do
  pushField fb 1 (TV.I32 (fmVersion fm))
  pushField fb 2 (TV.List TW.TT_STRUCT
                    (V.map schemaElementToThrift (fmSchema fm)))
  pushField fb 3 (TV.I64 (fmNumRows fm))
  pushField fb 4 (TV.List TW.TT_STRUCT
                    (V.map rowGroupToThrift (fmRowGroups fm)))
  pushFieldMb fb 5 TV.String (fmCreatedBy fm)

schemaElementToThrift :: SchemaElement -> TV.Value
schemaElementToThrift se = TV.Struct $ buildFields 5 $ \fb -> do
  pushField fb 1 (TV.String (seName se))
  pushFieldMb fb 2 (\r -> TV.I32 (fromIntegral (fromEnum r))) (seRepetition se)
  pushFieldMb fb 3 (TV.I32 . parquetTypeToInt) (seType se)
  pushFieldMb fb 4 TV.I32 (seNumChildren se)
  pushFieldMb fb 5 (\c -> TV.I32 (fromIntegral (fromEnum c))) (seConvertedType se)

rowGroupToThrift :: RowGroup -> TV.Value
rowGroupToThrift rg = TV.Struct $ buildFields 3 $ \fb -> do
  pushField fb 1 (TV.List TW.TT_STRUCT
                    (V.map columnChunkToThrift (rgColumns rg)))
  pushField fb 2 (TV.I64 (rgTotalByteSize rg))
  pushField fb 3 (TV.I64 (rgNumRows rg))

-- | parquet.thrift @ColumnChunk@:
--
--   1: optional string  file_path
--   2: required i64     file_offset
--   3: optional ColumnMetaData  meta_data
--   4: optional i64     offset_index_offset
--   5: optional i32     offset_index_length
--   6: optional i64     column_index_offset
--   7: optional i32     column_index_length
columnChunkToThrift :: ColumnChunk -> TV.Value
columnChunkToThrift cc = TV.Struct $ buildFields 7 $ \fb -> do
  pushFieldMb fb 1 TV.String                 (ccFilePath cc)
  pushField   fb 2 (TV.I64 (ccFileOffset cc))
  pushFieldMb fb 3 columnMetadataToThrift    (ccMetadata cc)
  pushFieldMb fb 4 TV.I64                    (ccOffsetIndexOffset cc)
  pushFieldMb fb 5 TV.I32                    (ccOffsetIndexLength cc)
  pushFieldMb fb 6 TV.I64                    (ccColumnIndexOffset cc)
  pushFieldMb fb 7 TV.I32                    (ccColumnIndexLength cc)

-- | parquet.thrift @ColumnMetaData@: 8 mandatory + 7 optional fields.
columnMetadataToThrift :: ColumnMetadata -> TV.Value
columnMetadataToThrift cm = TV.Struct $ buildFields 15 $ \fb -> do
  pushField   fb 1  (TV.I32 (parquetTypeToInt (cmType cm)))
  pushField   fb 2  (TV.List TW.TT_I32
                       (V.map (TV.I32 . encodingToInt) (cmEncodings cm)))
  pushField   fb 3  (TV.List TW.TT_STRING
                       (V.map TV.String (cmPathInSchema cm)))
  pushField   fb 4  (TV.I32 (compressionToInt (cmCodec cm)))
  pushField   fb 5  (TV.I64 (cmNumValues cm))
  pushField   fb 6  (TV.I64 (cmTotalUncompressedSize cm))
  pushField   fb 7  (TV.I64 (cmTotalCompressedSize cm))
  pushField   fb 8  (TV.I64 (cmDataPageOffset cm))
  pushFieldMb fb 9  statisticsToThrift (cmStatistics cm)
  pushFieldMb fb 10 TV.I64             (cmIndexPageOffset cm)
  pushFieldMb fb 11 TV.I64             (cmDictionaryPageOffset cm)
  pushFieldWhen fb 12
    (not (V.null (cmKeyValueMetadata cm)))
    (TV.List TW.TT_STRUCT (V.map keyValueToThrift (cmKeyValueMetadata cm)))
  pushFieldWhen fb 13
    (not (V.null (cmEncodingStats cm)))
    (TV.List TW.TT_STRUCT (V.map pageEncodingStatsToThrift (cmEncodingStats cm)))
  pushFieldMb fb 14 TV.I64 (cmBloomFilterOffset cm)
  pushFieldMb fb 15 TV.I32 (cmBloomFilterLength cm)

keyValueToThrift :: KeyValue -> TV.Value
keyValueToThrift kv = TV.Struct $ buildFields 2 $ \fb -> do
  pushField   fb 1 (TV.String (kvKey kv))
  pushFieldMb fb 2 TV.String (kvValue kv)

pageEncodingStatsToThrift :: PageEncodingStats -> TV.Value
pageEncodingStatsToThrift pes = TV.Struct $ buildFields 3 $ \fb -> do
  pushField fb 1 (TV.I32 (pesPageType pes))
  pushField fb 2 (TV.I32 (encodingToInt (pesEncoding pes)))
  pushField fb 3 (TV.I32 (pesCount pes))

statisticsToThrift :: Statistics -> TV.Value
statisticsToThrift st = TV.Struct $ buildFields 8 $ \fb -> do
  pushFieldMb fb 1 TV.Binary (statMax st)
  pushFieldMb fb 2 TV.Binary (statMin st)
  pushFieldMb fb 3 TV.I64    (statNullCount st)
  pushFieldMb fb 4 TV.I64    (statDistinctCount st)
  pushFieldMb fb 5 TV.Binary (statMaxValue st)
  pushFieldMb fb 6 TV.Binary (statMinValue st)
  pushFieldMb fb 7 TV.Bool   (statIsMaxValueExact st)
  pushFieldMb fb 8 TV.Bool   (statIsMinValueExact st)

encodingToInt :: Encoding -> Int32
encodingToInt = \case
  Plain               -> 0
  PlainDictionary     -> 2
  RLE                 -> 3
  BitPacked           -> 4
  DeltaBinaryPacked   -> 5
  DeltaLengthByteArray -> 6
  DeltaByteArray      -> 7
  RLEDictionary       -> 8
  ByteStreamSplit     -> 9

intToEncoding :: Int32 -> Maybe Encoding
intToEncoding = \case
  0 -> Just Plain
  2 -> Just PlainDictionary
  3 -> Just RLE
  4 -> Just BitPacked
  5 -> Just DeltaBinaryPacked
  6 -> Just DeltaLengthByteArray
  7 -> Just DeltaByteArray
  8 -> Just RLEDictionary
  9 -> Just ByteStreamSplit
  _ -> Nothing

compressionToInt :: Compression -> Int32
compressionToInt = \case
  Uncompressed -> 0
  Snappy       -> 1
  GZip         -> 2
  LZO          -> 3
  Brotli       -> 4
  LZ4          -> 5
  ZSTD         -> 6
  LZ4Raw       -> 7

intToCompression :: Int32 -> Maybe Compression
intToCompression = \case
  0 -> Just Uncompressed
  1 -> Just Snappy
  2 -> Just GZip
  3 -> Just LZO
  4 -> Just Brotli
  5 -> Just LZ4
  6 -> Just ZSTD
  7 -> Just LZ4Raw
  _ -> Nothing

-- Decoding from Thrift value back to our types

thriftToFileMetadata :: TV.Value -> Either String FileMetadata
thriftToFileMetadata (TV.Struct fields) = do
  let fm = V.toList fields
  version <- getI32 fm 1 "version"
  schema <- getListStruct fm 2 "schema" thriftToSchemaElement
  numRows <- getI64 fm 3 "num_rows"
  rowGroups <- getListStruct fm 4 "row_groups" thriftToRowGroup
  let createdBy = getOptionalString fm 5
  Right FileMetadata
    { fmVersion = version
    , fmSchema = schema
    , fmNumRows = numRows
    , fmRowGroups = rowGroups
    , fmCreatedBy = createdBy
    }
thriftToFileMetadata _ = Left "Parquet.Footer: expected struct"

thriftToSchemaElement :: TV.Value -> Either String SchemaElement
thriftToSchemaElement (TV.Struct fields) = do
  let fm = V.toList fields
  name <- getString fm 1 "schema name"
  let rep = case lookupField fm 2 of
              Just (TV.I32 r) -> Just (toEnum (fromIntegral r))
              _ -> Nothing
      typ = case lookupField fm 3 of
              Just (TV.I32 t) -> intToParquetType t
              _ -> Nothing
      numCh = case lookupField fm 4 of
                Just (TV.I32 n) -> Just n
                _ -> Nothing
      conv = case lookupField fm 5 of
               Just (TV.I32 c) | c >= 0, c <= 21 -> Just (toEnum (fromIntegral c))
               _ -> Nothing
  Right SchemaElement
    { seName = name
    , seRepetition = rep
    , seType = typ
    , seNumChildren = numCh
    , seConvertedType = conv
    , seLogicalType = Nothing
    }
thriftToSchemaElement _ = Left "Parquet.Footer: expected struct for SchemaElement"

thriftToRowGroup :: TV.Value -> Either String RowGroup
thriftToRowGroup (TV.Struct fields) = do
  let fm = V.toList fields
  cols <- getListStruct fm 1 "columns" thriftToColumnChunk
  totalBytes <- getI64 fm 2 "total_byte_size"
  numRows <- getI64 fm 3 "num_rows"
  Right RowGroup
    { rgColumns = cols
    , rgTotalByteSize = totalBytes
    , rgNumRows = numRows
    }
thriftToRowGroup _ = Left "Parquet.Footer: expected struct for RowGroup"

thriftToColumnChunk :: TV.Value -> Either String ColumnChunk
thriftToColumnChunk (TV.Struct fields) = do
  let fm = V.toList fields
      fp = getOptionalString fm 1
  fileOff <- getI64 fm 2 "file_offset"
  let meta = case lookupField fm 3 of
               Just v -> case thriftToColumnMetadata v of
                           Right m -> Just m
                           Left _  -> Nothing
               Nothing -> Nothing
      oio = getOptionalI64 fm 4
      oil = getOptionalI32 fm 5
      cio = getOptionalI64 fm 6
      cil = getOptionalI32 fm 7
  Right ColumnChunk
    { ccFilePath = fp
    , ccFileOffset = fileOff
    , ccMetadata = meta
    , ccOffsetIndexOffset = oio
    , ccOffsetIndexLength = oil
    , ccColumnIndexOffset = cio
    , ccColumnIndexLength = cil
    }
thriftToColumnChunk _ = Left "Parquet.Footer: expected struct for ColumnChunk"

thriftToColumnMetadata :: TV.Value -> Either String ColumnMetadata
thriftToColumnMetadata (TV.Struct fields) = do
  let fm = V.toList fields
  typeVal <- getI32 fm 1 "type"
  pt <- maybe (Left "Parquet.Footer: invalid parquet type") Right (intToParquetType typeVal)
  encodings <- case lookupField fm 2 of
    Just (TV.List _ es) -> V.mapM (\case
      TV.I32 e -> maybe (Left "Parquet.Footer: invalid encoding") Right (intToEncoding e)
      _ -> Left "Parquet.Footer: expected i32 in encodings") es
    _ -> Left "Parquet.Footer: missing encodings"
  paths <- case lookupField fm 3 of
    Just (TV.List _ ps) -> V.mapM (\case
      TV.String t -> Right t
      _ -> Left "Parquet.Footer: expected string in path") ps
    _ -> Left "Parquet.Footer: missing path_in_schema"
  codecVal <- getI32 fm 4 "codec"
  codec <- maybe (Left "Parquet.Footer: invalid compression") Right (intToCompression codecVal)
  numVals <- getI64 fm 5 "num_values"
  uncompSz <- getI64 fm 6 "total_uncompressed_size"
  compSz <- getI64 fm 7 "total_compressed_size"
  dataOff <- getI64 fm 8 "data_page_offset"
  let stats = case lookupField fm 9 of
        Just v -> case thriftToStatistics v of
          Right s -> Just s
          Left _  -> Nothing
        Nothing -> Nothing
      ipo = getOptionalI64 fm 10
      dpo = getOptionalI64 fm 11
  kvs <- case lookupField fm 12 of
    Nothing -> Right V.empty
    Just (TV.List _ vs) -> V.mapM thriftToKeyValue vs
    Just _ -> Left "Parquet.Footer: key_value_metadata is not a list"
  encStats <- case lookupField fm 13 of
    Nothing -> Right V.empty
    Just (TV.List _ vs) -> V.mapM thriftToPageEncodingStats vs
    Just _ -> Left "Parquet.Footer: encoding_stats is not a list"
  let bfo = getOptionalI64 fm 14
      bfl = getOptionalI32 fm 15
  Right ColumnMetadata
    { cmType = pt
    , cmEncodings = encodings
    , cmPathInSchema = paths
    , cmCodec = codec
    , cmNumValues = numVals
    , cmTotalUncompressedSize = uncompSz
    , cmTotalCompressedSize = compSz
    , cmDataPageOffset = dataOff
    , cmStatistics = stats
    , cmIndexPageOffset = ipo
    , cmDictionaryPageOffset = dpo
    , cmKeyValueMetadata = kvs
    , cmEncodingStats = encStats
    , cmBloomFilterOffset = bfo
    , cmBloomFilterLength = bfl
    }
thriftToColumnMetadata _ = Left "Parquet.Footer: expected struct for ColumnMetadata"

thriftToKeyValue :: TV.Value -> Either String KeyValue
thriftToKeyValue (TV.Struct fields) = do
  let fm = V.toList fields
  k <- case lookupField fm 1 of
    Just (TV.String t) -> Right t
    _ -> Left "Parquet.Footer: KeyValue.key missing or not a string"
  let v = case lookupField fm 2 of
        Just (TV.String t) -> Just t
        _ -> Nothing
  Right (KeyValue k v)
thriftToKeyValue _ = Left "Parquet.Footer: KeyValue is not a struct"

thriftToPageEncodingStats :: TV.Value -> Either String PageEncodingStats
thriftToPageEncodingStats (TV.Struct fields) = do
  let fm = V.toList fields
  pt <- case lookupField fm 1 of
    Just (TV.I32 v) -> Right v
    _ -> Left "Parquet.Footer: PageEncodingStats.page_type missing"
  enc <- case lookupField fm 2 of
    Just (TV.I32 v) -> case intToEncoding v of
      Just e -> Right e
      Nothing -> Left "Parquet.Footer: PageEncodingStats.encoding invalid"
    _ -> Left "Parquet.Footer: PageEncodingStats.encoding missing"
  cnt <- case lookupField fm 3 of
    Just (TV.I32 v) -> Right v
    _ -> Left "Parquet.Footer: PageEncodingStats.count missing"
  Right (PageEncodingStats pt enc cnt)
thriftToPageEncodingStats _ = Left "Parquet.Footer: PageEncodingStats is not a struct"

thriftToStatistics :: TV.Value -> Either String Statistics
thriftToStatistics (TV.Struct fields) = do
  let fm = V.toList fields
      -- Thrift Compact stores both binary and UTF-8 strings under
      -- TT_STRING; the decoder surfaces TV.String when the bytes
      -- happen to parse as UTF-8.  Stats values are arbitrary bytes
      -- (PLAIN-encoded primitives) so accept either shape.
      getBinary fid = case lookupField fm fid of
        Just (TV.Binary b) -> Just b
        Just (TV.String t) -> Just (Data.Text.Encoding.encodeUtf8 t)
        _ -> Nothing
      getOptI64 fid = case lookupField fm fid of
        Just (TV.I64 v) -> Just v
        _ -> Nothing
      getOptBool fid = case lookupField fm fid of
        Just (TV.Bool v) -> Just v
        _ -> Nothing
  Right Statistics
    { statMax = getBinary 1
    , statMin = getBinary 2
    , statNullCount = getOptI64 3
    , statDistinctCount = getOptI64 4
    , statMaxValue = getBinary 5
    , statMinValue = getBinary 6
    , statIsMaxValueExact = getOptBool 7
    , statIsMinValueExact = getOptBool 8
    }
thriftToStatistics _ = Left "Parquet.Footer: expected struct for Statistics"

-- Helpers

lookupField :: [(Int16, TV.Value)] -> Int16 -> Maybe TV.Value
lookupField fm fid = lookup fid fm

getI32 :: [(Int16, TV.Value)] -> Int16 -> String -> Either String Int32
getI32 fm fid name = case lookupField fm fid of
  Just (TV.I32 v) -> Right v
  _ -> Left $ "Parquet.Footer: missing or invalid field " ++ name

getI64 :: [(Int16, TV.Value)] -> Int16 -> String -> Either String Int64
getI64 fm fid name = case lookupField fm fid of
  Just (TV.I64 v) -> Right v
  _ -> Left $ "Parquet.Footer: missing or invalid field " ++ name

getString :: [(Int16, TV.Value)] -> Int16 -> String -> Either String T.Text
getString fm fid name = case lookupField fm fid of
  Just (TV.String t) -> Right t
  _ -> Left $ "Parquet.Footer: missing or invalid field " ++ name

getOptionalString :: [(Int16, TV.Value)] -> Int16 -> Maybe T.Text
getOptionalString fm fid = case lookupField fm fid of
  Just (TV.String t) -> Just t
  _ -> Nothing

getOptionalI32 :: [(Int16, TV.Value)] -> Int16 -> Maybe Int32
getOptionalI32 fm fid = case lookupField fm fid of
  Just (TV.I32 v) -> Just v
  _ -> Nothing

getOptionalI64 :: [(Int16, TV.Value)] -> Int16 -> Maybe Int64
getOptionalI64 fm fid = case lookupField fm fid of
  Just (TV.I64 v) -> Just v
  _ -> Nothing

getListStruct :: [(Int16, TV.Value)] -> Int16 -> String
              -> (TV.Value -> Either String a) -> Either String (V.Vector a)
getListStruct fm fid name decode = case lookupField fm fid of
  Just (TV.List _ vs) -> V.mapM decode vs
  _ -> Left $ "Parquet.Footer: missing or invalid field " ++ name
