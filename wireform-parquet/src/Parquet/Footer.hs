{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Read/write Apache Parquet file footer.

Parquet file layout ends with:
  [Thrift Compact Protocol encoded FileMetadata] [4-byte LE metadata length] [PAR1 magic]

We use the existing Thrift Compact Protocol encoder/decoder to serialize
the FileMetadata as a Thrift struct. Struct-field placement is mediated
by the bidirectional pattern synonyms in "Parquet.Thrift.Schema" so
that field numbers live in one spec-tracking module; writers and
readers reference the fields by name.
-}
module Parquet.Footer (
  readFooter,
  readFooterRaw,
  writeFooter,
  writeFooterEither,
  writeRawFooter,
  writeRawFooterEither,
  fileMetadataToThrift,
  thriftToFileMetadata,
  thriftToColumnMetadata,
  thriftToColumnChunk,
  sanitizeColumnOrders,
  parquetMagic,
  parquetEncryptedMagic,

  -- * Trailer parsing
  FooterTrailer (..),
  readFooterTrailer,
) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int16, Int32)
import Data.Text qualified as T
import Data.Text.Encoding qualified
import Data.Vector qualified as V
import Data.Word (Word32)
import Parquet.Thrift.Schema
import Parquet.Types
import Thrift.Decode (decodeCompact)
import Thrift.Encode (encodeCompactBounded)
import Thrift.Value qualified as TV
import Wireform.Builder qualified as B
import Parquet.Decimal (decimalPrecisionFits)


parquetMagic :: ByteString
parquetMagic = BS.pack [0x50, 0x41, 0x52, 0x31]


{- | The trailing magic for an encrypted-footer Parquet file: @PARE@.
Replaces 'parquetMagic' in the encrypted-footer mode so a reader
can distinguish a file whose footer is itself an AES-GCM module
from a plaintext-footer file whose columns happen to be encrypted.
-}
parquetEncryptedMagic :: ByteString
parquetEncryptedMagic = BS.pack [0x50, 0x41, 0x52, 0x45]


writeFooter :: FileMetadata -> ByteString
writeFooter = either error id . writeFooterEither


-- | Serialize metadata and check its footer length/trailer bounds.
writeFooterEither :: FileMetadata -> Either String ByteString
writeFooterEither fm = do
  let !thriftVal = fileMetadataToThrift fm
      !limit = fromInteger (min (toInteger (maxBound :: Word32)) (toInteger (maxBound :: Int) - 8))
  encoded <- encodeCompactBounded limit thriftVal
  writeRawFooterEither parquetMagic encoded


{- | Build a footer trailer from already-encoded thrift bytes plus a
magic number. Encrypted-footer mode passes 'parquetEncryptedMagic'
and the GCM-encrypted thrift bytes; plaintext-footer mode passes
'parquetMagic' and the raw thrift.
-}
writeRawFooter :: ByteString -> ByteString -> ByteString
writeRawFooter magic = either error id . writeRawFooterEither magic


{- | Check the four-byte footer length before narrowing it or copying the
already encoded metadata. The magic must occupy four bytes; its contents
remain caller-selected (normally PAR1 or PARE). Logical metadata validation
belongs to the higher-level writer, not this raw framing helper.
-}
writeRawFooterEither :: ByteString -> ByteString -> Either String ByteString
writeRawFooterEither magic encoded
  | BS.length magic /= 4 = Left "Parquet.Footer: trailer magic must contain four bytes"
  | metaLen < 0 || toInteger metaLen > toInteger (maxBound :: Word32) = Left "Parquet.Footer: metadata length exceeds unsigned 32-bit footer limit"
  | metaLen > maxBound - 8 = Left "Parquet.Footer: footer trailer exceeds addressable byte size"
  | otherwise =
      Right $
        B.toStrictByteStringWith (metaLen + 8) $
          B.byteString encoded
            <> B.word32LE (fromIntegral metaLen)
            <> B.byteString magic
  where
    !metaLen = BS.length encoded


{- | The bytes the footer trailer points at, plus which magic ended
the file. For a plaintext-footer file (@PAR1@) the bytes are the
compact-encoded 'FileMetadata' thrift; for an encrypted-footer
file (@PARE@) they are the AES-GCM module the caller must
decrypt before parsing the same thrift.
-}
data FooterTrailer = FooterTrailer
  { ftMagic :: !ByteString
  -- ^ Either 'parquetMagic' or 'parquetEncryptedMagic'.
  , ftBytes :: !ByteString
  {- ^ Footer bytes (raw thrift for plaintext, GCM module for
    encrypted).
  -}
  }
  deriving (Show, Eq)


{- | Parse the trailing 8 bytes (length + magic) and slice out the
footer module bytes. Doesn't decode them - that's the caller's
decision based on which magic appeared.
-}
readFooterTrailer :: ByteString -> Either String FooterTrailer
readFooterTrailer bs
  | BS.length bs < 8 = Left "Parquet.Footer: input too short"
  | otherwise =
      let !totalLen = BS.length bs
          !magic = BSU.unsafeTake 4 (BSU.unsafeDrop (totalLen - 4) bs)
      in if magic /= parquetMagic && magic /= parquetEncryptedMagic
           then Left "Parquet.Footer: invalid magic (expected PAR1 or PARE)"
           else
             let !metaLenOff = totalLen - 8
                 !metaLen = fromIntegral (readLE32 bs metaLenOff) :: Int
             in if metaLen < 0 || metaLen > totalLen - 8
                  then Left "Parquet.Footer: invalid metadata length"
                  else
                    let !metaStart = totalLen - 8 - metaLen
                        !metaBytes =
                          BSU.unsafeTake
                            metaLen
                            (BSU.unsafeDrop metaStart bs)
                    in Right (FooterTrailer magic metaBytes)


{- | Parse the plaintext-thrift bytes of a footer (i.e. what
'readFooterTrailer' returns for a @PAR1@ file or what the caller
got after decrypting a @PARE@ footer module).
-}
readFooterRaw :: ByteString -> Either String FileMetadata
readFooterRaw thriftBytes = do
  thriftVal <- decodeCompact thriftBytes
  thriftToFileMetadata thriftVal


readFooter :: ByteString -> Either String FileMetadata
readFooter bs = do
  trailer <- readFooterTrailer bs
  if ftMagic trailer == parquetEncryptedMagic
    then Left "Parquet.Footer: file has encrypted footer (PARE); use loadParquetFileEncrypted"
    else readFooterRaw (ftBytes trailer)


readLE32 :: ByteString -> Int -> Word32
readLE32 bs off =
  let !b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word32
      !b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word32
      !b2 = fromIntegral (BSU.unsafeIndex bs (off + 2)) :: Word32
      !b3 = fromIntegral (BSU.unsafeIndex bs (off + 3)) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)


fileMetadataToThrift :: FileMetadata -> TV.Value
fileMetadataToThrift fm =
  TV.Struct $
    V.fromList $
      [ FileMetadata_Version (fmVersion fm)
      , FileMetadata_Schema (V.map schemaElementToThrift (fmSchema fm))
      , FileMetadata_NumRows (fmNumRows fm)
      , FileMetadata_RowGroups (V.map rowGroupToThrift (fmRowGroups fm))
      ]
        ++ optField (fmKeyValueMetadata fm) (FileMetadata_KeyValueMetadata . V.map keyValueToThrift)
        ++ optField (fmCreatedBy fm) FileMetadata_CreatedBy
        ++ optField
          (fmColumnOrders fm)
          (FileMetadata_ColumnOrders . V.map columnOrderToThrift)


keyValueToThrift :: KeyValue -> TV.Value
keyValueToThrift entry =
  TV.Struct $
    V.singleton (KeyValue_Key (TV.Binary (kvKey entry)))
      V.++ maybe V.empty (V.singleton . KeyValue_Value . TV.Binary) (kvValue entry)


columnOrderToThrift :: ColumnOrder -> TV.Value
columnOrderToThrift TypeDefinedOrder = columnOrderTypeDefined
columnOrderToThrift IEEE754TotalOrder = columnOrderIeee754Total
columnOrderToThrift Int96TimestampOrder = columnOrderInt96Timestamp
columnOrderToThrift UnsupportedColumnOrder = error "Parquet.Footer: cannot write an unsupported column order"


schemaElementToThrift :: SchemaElement -> TV.Value
schemaElementToThrift se =
  TV.Struct $
    V.fromList $
      concat
        [ optField (seType se) (SchemaElement_Type . parquetTypeToInt)
        , optField (seTypeLength se) SchemaElement_TypeLength
        , optField (seRepetition se) (SchemaElement_RepetitionType . fromIntegral . fromEnum)
        , [SchemaElement_Name (seName se)]
        , optField (seNumChildren se) SchemaElement_NumChildren
        , optField
            ( case (seConvertedType se, seLogicalType se) of
                (Nothing, Just (LTDecimal _ _)) -> Just CTDecimal
                (converted, _) -> converted
            )
            (SchemaElement_ConvertedType . fromIntegral . fromEnum)
        , optField (seFieldId se) SchemaElement_FieldId
        , optField
            (seLogicalType se)
            (SchemaElement_LogicalType . V.fromList . logicalTypeFields)
        , case seLogicalType se of
            Just (LTDecimal precision scale) ->
              [SchemaElement_Scale scale, SchemaElement_Precision precision]
            _ -> []
        ]


{- | Encode a 'LogicalType' as the inner struct of the
@parquet.thrift@ @LogicalType@ union — exactly one
variant-tagged field.
-}
logicalTypeFields :: LogicalType -> [(Int16, TV.Value)]
logicalTypeFields = \case
  LTString -> [(1, emptyStruct)]
  LTMap -> [(2, emptyStruct)]
  LTList -> [(3, emptyStruct)]
  LTEnum -> [(4, emptyStruct)]
  LTDecimal p s ->
    [
      ( 5
      , TV.Struct $
          V.fromList
            [ (1, TV.I32 s) -- scale
            , (2, TV.I32 p) -- precision
            ]
      )
    ]
  LTDate -> [(6, emptyStruct)]
  LTTime adj unit ->
    [
      ( 7
      , TV.Struct $
          V.fromList
            [ (1, TV.Bool adj)
            , (2, encodeTimeUnitStruct unit)
            ]
      )
    ]
  LTTimestamp adj unit ->
    [
      ( 8
      , TV.Struct $
          V.fromList
            [ (1, TV.Bool adj)
            , (2, encodeTimeUnitStruct unit)
            ]
      )
    ]
  LTInteger w isSigned ->
    [
      ( 10
      , TV.Struct $
          V.fromList
            [ (1, TV.Byte (fromIntegral w))
            , (2, TV.Bool isSigned)
            ]
      )
    ]
  LTNull -> [(11, emptyStruct)]
  LTJson -> [(12, emptyStruct)]
  LTBson -> [(13, emptyStruct)]
  LTUUID -> [(14, emptyStruct)]
  LTFloat16 -> [(15, emptyStruct)]
  LTVariant ver ->
    [
      ( 16
      , TV.Struct $
          V.fromList
            [ (1, TV.Byte (fromIntegral ver))
            ]
      )
    ]
  LTGeometry parameters ->
    [(17, TV.Struct (V.fromList (maybe [] (\crs -> [(1, TV.String crs)]) (geometryCrs parameters))))]
  LTGeography parameters ->
    [
      ( 18
      , TV.Struct
          ( V.fromList
              ( maybe [] (\crs -> [(1, TV.String crs)]) (geographyCrs parameters)
                  ++ maybe [] (\algorithm -> [(2, TV.I32 (interpolationAlgorithmToInt algorithm))]) (geographyAlgorithm parameters)
              )
          )
      )
    ]
  LTFile -> [(19, emptyStruct)]
  LTUnsupported -> error "Parquet.Footer: cannot write an unsupported logical annotation"


emptyStruct :: TV.Value
emptyStruct = TV.Struct V.empty


encodeTimeUnitStruct :: LtTimeUnit -> TV.Value
encodeTimeUnitStruct unit =
  TV.Struct $
    V.fromList
      [
        ( case unit of
            LtMillis -> 1
            LtMicros -> 2
            LtNanos -> 3
        , emptyStruct
        )
      ]


decodeLogicalType :: V.Vector (Int16, TV.Value) -> Maybe LogicalType
decodeLogicalType fs = Just (maybe LTUnsupported id (decodeKnownLogicalType fs))


decodeKnownLogicalType :: V.Vector (Int16, TV.Value) -> Maybe LogicalType
decodeKnownLogicalType fs = case V.toList fs of
  [entry] -> case entry of
    (1, TV.Struct _) -> Just LTString
    (2, TV.Struct _) -> Just LTMap
    (3, TV.Struct _) -> Just LTList
    (4, TV.Struct _) -> Just LTEnum
    (5, TV.Struct sb) -> decodeDecimal sb
    (6, TV.Struct _) -> Just LTDate
    (7, TV.Struct sb) -> decodeTimeOrTimestamp LTTime sb
    (8, TV.Struct sb) -> decodeTimeOrTimestamp LTTimestamp sb
    (10, TV.Struct sb) -> decodeIntType sb
    (11, TV.Struct _) -> Just LTNull
    (12, TV.Struct _) -> Just LTJson
    (13, TV.Struct _) -> Just LTBson
    (14, TV.Struct _) -> Just LTUUID
    (15, TV.Struct _) -> Just LTFloat16
    (16, TV.Struct sb) -> decodeVariant sb
    (17, TV.Struct sb) -> LTGeometry . GeometryParameters <$> optionalLogicalField 1 asLogicalText sb
    (18, TV.Struct sb) ->
      LTGeography
        <$> ( GeographyParameters
                <$> optionalLogicalField 1 asLogicalText sb
                <*> optionalLogicalField 2 (fmap intToInterpolationAlgorithm . asI32) sb
            )
    (19, TV.Struct _) -> Just LTFile
    -- FINITE_VECTOR uses the three-level LIST representation. Read it as
    -- LIST until the logical type and its element constraints are supported.
    (20, TV.Struct sb)
      | Just (Just count) <- optionalLogicalField 1 asI32 sb
      , count > 0 -> Just LTList
    _ -> Nothing
  _ -> Nothing
  where
    decodeDecimal :: V.Vector (Int16, TV.Value) -> Maybe LogicalType
    decodeDecimal sb =
      let mScale = lookup 1 (V.toList sb) >>= asI32
          mPrec = lookup 2 (V.toList sb) >>= asI32
      in case (mScale, mPrec) of
           (Just s, Just p) -> Just (LTDecimal p s)
           _ -> Nothing
    decodeTimeOrTimestamp
      :: (Bool -> LtTimeUnit -> LogicalType)
      -> V.Vector (Int16, TV.Value)
      -> Maybe LogicalType
    decodeTimeOrTimestamp ctor sb =
      let mAdj = lookup 1 (V.toList sb) >>= asBool
          mUnit = lookup 2 (V.toList sb) >>= asTimeUnit
      in ctor <$> mAdj <*> mUnit
    decodeIntType :: V.Vector (Int16, TV.Value) -> Maybe LogicalType
    decodeIntType sb =
      let mWidth = lookup 1 (V.toList sb) >>= asByte
          mSigned = lookup 2 (V.toList sb) >>= asBool
      in LTInteger <$> (fromIntegral <$> mWidth) <*> mSigned
    decodeVariant :: V.Vector (Int16, TV.Value) -> Maybe LogicalType
    decodeVariant sb =
      let mVer = lookup 1 (V.toList sb) >>= asByte
      in LTVariant . fromIntegral <$> mVer


-- Unknown fields permit extension; malformed/duplicate known parameters must
-- fall back to physical storage rather than silently becoming default values.
optionalLogicalField :: Int16 -> (TV.Value -> Maybe a) -> V.Vector (Int16, TV.Value) -> Maybe (Maybe a)
optionalLogicalField field parse fields = case V.toList (V.filter ((== field) . fst) fields) of
  [] -> Just Nothing
  [(_, value)] -> Just <$> parse value
  _ -> Nothing


asLogicalText :: TV.Value -> Maybe T.Text
asLogicalText (TV.String value) = Just value
asLogicalText (TV.Binary value) = either (const Nothing) Just (Data.Text.Encoding.decodeUtf8' value)
asLogicalText _ = Nothing


asI32 :: TV.Value -> Maybe Int32
asI32 (TV.I32 v) = Just v
asI32 _ = Nothing


asBool :: TV.Value -> Maybe Bool
asBool (TV.Bool v) = Just v
asBool _ = Nothing


asByte :: TV.Value -> Maybe Int
asByte (TV.Byte v) = Just (fromIntegral v)
asByte (TV.I32 v) = Just (fromIntegral v)
asByte _ = Nothing


asTimeUnit :: TV.Value -> Maybe LtTimeUnit
asTimeUnit (TV.Struct sb) = case V.toList sb of
  [(1, TV.Struct _)] -> Just LtMillis
  [(2, TV.Struct _)] -> Just LtMicros
  [(3, TV.Struct _)] -> Just LtNanos
  _ -> Nothing
asTimeUnit _ = Nothing


rowGroupToThrift :: RowGroup -> TV.Value
rowGroupToThrift rg =
  TV.Struct $
    V.fromList $
      [ RowGroup_Columns (V.map columnChunkToThrift (rgColumns rg))
      , RowGroup_TotalByteSize (rgTotalByteSize rg)
      , RowGroup_NumRows (rgNumRows rg)
      ]
        ++ optField
          (rgSortingColumns rg)
          (RowGroup_SortingColumns . V.map sortingColumnToThrift)


sortingColumnToThrift :: SortingColumn -> TV.Value
sortingColumnToThrift sc =
  TV.Struct $
    V.fromList
      [ SortingColumn_ColumnIdx (scColumnIdx sc)
      , SortingColumn_Descending (scDescending sc)
      , SortingColumn_NullsFirst (scNullsFirst sc)
      ]


columnChunkToThrift :: ColumnChunk -> TV.Value
columnChunkToThrift cc =
  TV.Struct $
    V.fromList $
      concat
        [ optField (ccFilePath cc) ColumnChunk_FilePath
        , [ColumnChunk_FileOffset (ccFileOffset cc)]
        , optField
            (ccMetadata cc)
            (\cm -> ColumnChunk_MetaData (V.fromList (columnMetadataFields cm)))
        , -- Offset/column-index placement is spec'd on ColumnChunk itself;
          -- older wireform writers omitted these fields entirely, which
          -- decoders treat as @Nothing@.
          optField (ccOffsetIndexOffset cc) ColumnChunk_OffsetIndexOffset
        , optField (ccOffsetIndexLength cc) ColumnChunk_OffsetIndexLength
        , optField (ccColumnIndexOffset cc) ColumnChunk_ColumnIndexOffset
        , optField (ccColumnIndexLength cc) ColumnChunk_ColumnIndexLength
        ]


columnMetadataFields :: ColumnMetadata -> [(Int16, TV.Value)]
columnMetadataFields cm =
  concat
    [
      [ ColumnMetaData_Type (parquetTypeToInt (cmType cm))
      , ColumnMetaData_Encodings
          (V.map (TV.I32 . encodingToInt) (cmEncodings cm))
      , ColumnMetaData_PathInSchema (V.map TV.String (cmPathInSchema cm))
      , ColumnMetaData_Codec (compressionToInt (cmCodec cm))
      , ColumnMetaData_NumValues (cmNumValues cm)
      , ColumnMetaData_TotalUncompressedSize (cmTotalUncompressedSize cm)
      , ColumnMetaData_TotalCompressedSize (cmTotalCompressedSize cm)
      , ColumnMetaData_DataPageOffset (cmDataPageOffset cm)
      ]
    , optField (cmDictionaryPageOffset cm) ColumnMetaData_DictionaryPageOffset
    , optField
        (cmStatistics cm)
        (\s -> ColumnMetaData_Statistics (V.fromList (statisticsFields s)))
    , optField (cmBloomFilterOffset cm) ColumnMetaData_BloomFilterOffset
    , optField (cmBloomFilterLength cm) ColumnMetaData_BloomFilterLength
    ]


statisticsFields :: Statistics -> [(Int16, TV.Value)]
statisticsFields st =
  concat
    [ optField (statMax st) Statistics_Max
    , optField (statMin st) Statistics_Min
    , optField (statNullCount st) Statistics_NullCount
    , optField (statDistinctCount st) Statistics_DistinctCount
    , optField (statMaxValue st) Statistics_MaxValue
    , optField (statMinValue st) Statistics_MinValue
    , optField (statMaxValueExact st) Statistics_MaxValueExact
    , optField (statMinValueExact st) Statistics_MinValueExact
    , optField (statNanCount st) Statistics_NanCount
    ]


encodingToInt :: Encoding -> Int32
encodingToInt = \case
  Plain -> 0
  PlainDictionary -> 2
  RLE -> 3
  BitPacked -> 4
  DeltaBinaryPacked -> 5
  DeltaLengthByteArray -> 6
  DeltaByteArray -> 7
  RLEDictionary -> 8
  ByteStreamSplit -> 9
  Alp -> 10


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
  10 -> Just Alp
  _ -> Nothing


compressionToInt :: Compression -> Int32
compressionToInt = \case
  Uncompressed -> 0
  Snappy -> 1
  GZip -> 2
  LZO -> 3
  Brotli -> 4
  LZ4 -> 5
  ZSTD -> 6
  LZ4Raw -> 7


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
  version <- require fm "version" $ \case
    FileMetadata_Version v -> Just v
    _ -> Nothing
  schema <- requireListStruct fm "schema" thriftToSchemaElement $ \case
    FileMetadata_Schema xs -> Just xs
    _ -> Nothing
  numRows <- require fm "num_rows" $ \case
    FileMetadata_NumRows v -> Just v
    _ -> Nothing
  rowGroups <- requireListStruct fm "row_groups" thriftToRowGroup $ \case
    FileMetadata_RowGroups xs -> Just xs
    _ -> Nothing
  if numRows < 0
    then Left "Parquet.Footer: negative file row count"
    else Right ()
  -- Historical files can have a stale file total while their row groups and
  -- data remain readable (parquet-testing/repeated_no_annotation.parquet).
  -- Preserve it for checked aggregate validation instead of losing the data.
  if V.any ((< 0) . rgNumRows) rowGroups
    then Left "Parquet.Footer: negative row-group count"
    else Right ()
  keyValues <- case V.toList (V.filter ((== 5) . fst) fields) of
    [] -> Right Nothing
    [FileMetadata_KeyValueMetadata entries] -> Just <$> V.mapM thriftToKeyValue entries
    _ -> Left "Parquet.Footer: invalid or duplicate key_value_metadata"
  let createdBy = findField fm $ \case
        FileMetadata_CreatedBy t -> Just t
        _ -> Nothing
  let columnOrders = findField fm $ \case
        FileMetadata_ColumnOrders xs ->
          Just (V.map thriftToColumnOrder xs)
        _ -> Nothing
  case columnOrders of
    Just orders
      | V.length orders /= V.length (V.filter (maybe False (const True) . seType) schema) ->
          Left "Parquet.Footer: column-order count differs from leaf-column count"
    _ -> Right ()
  Right
    FileMetadata
      { fmVersion = version
      , fmSchema = schema
      , fmNumRows = numRows
      , fmRowGroups = maybe rowGroups (\orders -> V.map (sanitizeColumnOrders schema orders) rowGroups) columnOrders
      , fmCreatedBy = createdBy
      , fmColumnOrders = columnOrders
      , fmKeyValueMetadata = keyValues
      }
thriftToFileMetadata _ = Left "Parquet.Footer: expected struct"


thriftToKeyValue :: TV.Value -> Either String KeyValue
thriftToKeyValue (TV.Struct fields) = do
  key <- metadataField "key" (\case KeyValue_Key value -> Just value; _ -> Nothing)
  value <- metadataField "value" (\case KeyValue_Value value -> Just value; _ -> Nothing)
  requiredKey <- maybe (Left "Parquet.Footer: metadata entry has no key") Right key
  Right (KeyValue requiredKey value)
  where
    metadataField name select = case V.toList (V.mapMaybe select fields) of
      [] -> Right Nothing
      [TV.Binary bytes] -> Right (Just bytes)
      [TV.String text] -> Right (Just (Data.Text.Encoding.encodeUtf8 text))
      _ -> Left ("Parquet.Footer: invalid or duplicate metadata " ++ name)
thriftToKeyValue _ = Left "Parquet.Footer: expected struct for metadata entry"


thriftToColumnOrder :: TV.Value -> ColumnOrder
thriftToColumnOrder (TV.Struct fields) =
  case V.toList fields of
    [(1, TV.Struct _)] -> TypeDefinedOrder
    [(2, TV.Struct _)] -> IEEE754TotalOrder
    [(3, TV.Struct _)] -> Int96TimestampOrder
    _ -> UnsupportedColumnOrder
thriftToColumnOrder _ = UnsupportedColumnOrder


-- Preserve the position of unknown union variants. Dropping one would
-- shift the ordering of all later columns and could cause false pruning.
-- Bounds with an unsupported order are unavailable; null counts remain valid.
sanitizeColumnOrders :: V.Vector SchemaElement -> V.Vector ColumnOrder -> RowGroup -> RowGroup
sanitizeColumnOrders schema orders rg = rg {rgColumns = V.imap sanitize (rgColumns rg)}
  where
    leaves = V.filter (maybe False (const True) . seType) schema
    sanitize i cc
      | maybe False (supported i . cmType) (ccMetadata cc) = cc
      | otherwise =
          cc
            { ccMetadata = sanitizeMetadata i <$> ccMetadata cc
            , ccColumnIndexOffset = Nothing
            , ccColumnIndexLength = Nothing
            }
    sanitizeMetadata i md
      | supported i (cmType md) = md
      | otherwise = md {cmStatistics = fmap clearBounds (cmStatistics md)}
    supported i ty =
      maybe False hasDefinedStatisticsOrder (leaves V.!? i) && case orders V.!? i of
        Just TypeDefinedOrder -> ty /= PTInt96
        Just IEEE754TotalOrder ->
          ty == PTFloat
            || ty == PTDouble
            || (ty == PTFixedLenByteArray && (leaves V.!? i >>= seLogicalType) == Just LTFloat16)
        Just Int96TimestampOrder -> ty == PTInt96
        _ -> False
    clearBounds stats =
      stats
        { statMin = Nothing
        , statMax = Nothing
        , statMinValue = Nothing
        , statMaxValue = Nothing
        , statMinValueExact = Nothing
        , statMaxValueExact = Nothing
        }


thriftToSchemaElement :: TV.Value -> Either String SchemaElement
thriftToSchemaElement (TV.Struct fields) = do
  let fm = V.toList fields
  name <- require fm "schema name" $ \case
    SchemaElement_Name t -> Just t
    _ -> Nothing
  typ <- traverse decodeType $ findField fm $ \case
    SchemaElement_Type t -> Just t
    _ -> Nothing
  rep <- traverse decodeRepetition $ findField fm $ \case
    SchemaElement_RepetitionType r -> Just r
    _ -> Nothing
  let numCh = findField fm $ \case
        SchemaElement_NumChildren n -> Just n
        _ -> Nothing
      conv = findField fm $ \case
        SchemaElement_ConvertedType c
          | c >= 0, c <= 21 -> Just (toEnum (fromIntegral c))
        _ -> Nothing
      fid = findField fm $ \case
        SchemaElement_FieldId v -> Just v
        _ -> Nothing
      typeLength = findField fm $ \case
        SchemaElement_TypeLength n -> Just n
        _ -> Nothing
      modernLogical = findField fm $ \case
        SchemaElement_LogicalType inner -> decodeLogicalType inner
        _ -> Nothing
      legacyPrecision = findField fm $ \case
        SchemaElement_Precision n -> Just n
        _ -> Nothing
      legacyScale = findField fm $ \case
        SchemaElement_Scale n -> Just n
        _ -> Nothing
  logical <- case (modernLogical, conv, legacyPrecision) of
    (Just annotation, _, _) -> Right (Just annotation)
    (Nothing, Just CTDecimal, Just precision) ->
      Right (Just (LTDecimal precision (maybe 0 id legacyScale)))
    (Nothing, Just CTDecimal, Nothing) -> Left "Parquet.Footer: legacy DECIMAL is missing precision"
    _ -> Right Nothing
  case numCh of
    Just n | n < 0 -> Left "Parquet.Footer: negative schema child count"
    _ -> Right ()
  case (typ, typeLength) of
    (Just PTFixedLenByteArray, Just n) | n > 0 -> Right ()
    (Just PTFixedLenByteArray, _) -> Left "Parquet.Footer: fixed byte array needs a positive type_length"
    (_, Just n) | n < 0 -> Left "Parquet.Footer: negative type_length"
    _ -> Right ()
  case logical of
    Just (LTDecimal precision scale)
      | precision <= 0 || scale < 0 || scale > precision ->
          Left "Parquet.Footer: invalid DECIMAL precision or scale"
      | typ == Just PTInt32 && precision > 9 -> Left "Parquet.Footer: INT32 DECIMAL precision exceeds 9"
      | typ == Just PTInt64 && precision > 18 -> Left "Parquet.Footer: INT64 DECIMAL precision exceeds 18"
      | Just width <- typeLength
      , typ == Just PTFixedLenByteArray
      , not (decimalPrecisionFits precision width) ->
          Left "Parquet.Footer: DECIMAL precision exceeds fixed byte width"
    _ -> Right ()
  Right
    SchemaElement
      { seTypeLength = typeLength
      , seName = name
      , seRepetition = rep
      , seType = typ
      , seNumChildren = numCh
      , seConvertedType = if logical == Just LTUnsupported then Nothing else conv
      , seLogicalType = logical
      , seFieldId = fid
      }
  where
    decodeType tag = case intToParquetType tag of
      Just ty -> Right ty
      Nothing -> Left ("Parquet.Footer: unknown physical type " ++ show tag)
    decodeRepetition = \case
      0 -> Right Required
      1 -> Right Optional
      2 -> Right Repeated
      tag -> Left ("Parquet.Footer: unknown repetition type " ++ show tag)
thriftToSchemaElement _ = Left "Parquet.Footer: expected struct for SchemaElement"


thriftToRowGroup :: TV.Value -> Either String RowGroup
thriftToRowGroup (TV.Struct fields) = do
  let fm = V.toList fields
  cols <- requireListStruct fm "columns" thriftToColumnChunk $ \case
    RowGroup_Columns xs -> Just xs
    _ -> Nothing
  totalBytes <- require fm "total_byte_size" $ \case
    RowGroup_TotalByteSize v -> Just v
    _ -> Nothing
  numRows <- require fm "num_rows" $ \case
    RowGroup_NumRows v -> Just v
    _ -> Nothing
  let sortingColumns = findField fm $ \case
        RowGroup_SortingColumns xs ->
          Just (V.mapMaybe thriftToSortingColumn xs)
        _ -> Nothing
  Right
    RowGroup
      { rgColumns = cols
      , rgTotalByteSize = totalBytes
      , rgNumRows = numRows
      , rgSortingColumns = sortingColumns
      }
thriftToRowGroup _ = Left "Parquet.Footer: expected struct for RowGroup"


thriftToSortingColumn :: TV.Value -> Maybe SortingColumn
thriftToSortingColumn (TV.Struct fields) = do
  let look p = V.find p fields
  TV.I32 i <- snd <$> look (\(fid, _) -> fid == 1)
  TV.Bool d <- snd <$> look (\(fid, _) -> fid == 2)
  TV.Bool n <- snd <$> look (\(fid, _) -> fid == 3)
  Just SortingColumn {scColumnIdx = i, scDescending = d, scNullsFirst = n}
thriftToSortingColumn _ = Nothing


thriftToColumnChunk :: TV.Value -> Either String ColumnChunk
thriftToColumnChunk (TV.Struct fields) = do
  let fm = V.toList fields
      fp = findField fm $ \case
        ColumnChunk_FilePath t -> Just t
        _ -> Nothing
  fileOff <- require fm "file_offset" $ \case
    ColumnChunk_FileOffset v -> Just v
    _ -> Nothing
  meta <- case findField
    fm
    ( \case
        ColumnChunk_MetaData fs -> Just fs
        _ -> Nothing
    ) of
    Just fs -> Just <$> thriftToColumnMetadata (TV.Struct fs)
    Nothing -> Right Nothing
  let oio = findField fm $ \case
        ColumnChunk_OffsetIndexOffset v -> Just v
        _ -> Nothing
      oil = findField fm $ \case
        ColumnChunk_OffsetIndexLength v -> Just v
        _ -> Nothing
      cio = findField fm $ \case
        ColumnChunk_ColumnIndexOffset v -> Just v
        _ -> Nothing
      cil = findField fm $ \case
        ColumnChunk_ColumnIndexLength v -> Just v
        _ -> Nothing
  Right
    ColumnChunk
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
  typeVal <- require fm "type" $ \case
    ColumnMetaData_Type v -> Just v
    _ -> Nothing
  pt <- maybe (Left "Parquet.Footer: invalid parquet type") Right (intToParquetType typeVal)
  encodings <- case findField
    fm
    ( \case
        ColumnMetaData_Encodings xs -> Just xs
        _ -> Nothing
    ) of
    Just es -> V.mapM expectEncoding es
    Nothing -> Left "Parquet.Footer: missing encodings"
  paths <- case findField
    fm
    ( \case
        ColumnMetaData_PathInSchema xs -> Just xs
        _ -> Nothing
    ) of
    Just ps -> V.mapM expectPathStr ps
    Nothing -> Left "Parquet.Footer: missing path_in_schema"
  codecVal <- require fm "codec" $ \case
    ColumnMetaData_Codec v -> Just v
    _ -> Nothing
  codec <- maybe (Left "Parquet.Footer: invalid compression") Right (intToCompression codecVal)
  numVals <- require fm "num_values" $ \case
    ColumnMetaData_NumValues v -> Just v
    _ -> Nothing
  uncompSz <- require fm "total_uncompressed_size" $ \case
    ColumnMetaData_TotalUncompressedSize v -> Just v
    _ -> Nothing
  compSz <- require fm "total_compressed_size" $ \case
    ColumnMetaData_TotalCompressedSize v -> Just v
    _ -> Nothing
  dataOff <- require fm "data_page_offset" $ \case
    ColumnMetaData_DataPageOffset v -> Just v
    _ -> Nothing
  -- index_page_offset / dictionary_page_offset / encoding_stats /
  -- key_value_metadata are all spec-defined but unused by this reader.
  stats <- traverse (thriftToStatistics . TV.Struct) $ findField fm $ \case
    ColumnMetaData_Statistics fs -> Just fs
    _ -> Nothing
  if numVals < 0 || uncompSz < 0 || compSz < 0 || dataOff < 0
    then Left "Parquet.Footer: negative column count, size, or page offset"
    else Right ()
  case stats of
    Just st
      | maybe False (> numVals) (statNullCount st)
          || maybe False (> numVals - maybe 0 id (statNullCount st)) (statNanCount st) ->
          Left "Parquet.Footer: statistics counts exceed column value count"
    _ -> Right ()
  let bfo = findField fm $ \case
        ColumnMetaData_BloomFilterOffset v -> Just v
        _ -> Nothing
      bfl = findField fm $ \case
        ColumnMetaData_BloomFilterLength v -> Just v
        _ -> Nothing
      dpo = findField fm $ \case
        ColumnMetaData_DictionaryPageOffset v -> Just v
        _ -> Nothing
  Right
    ColumnMetadata
      { cmType = pt
      , cmEncodings = encodings
      , cmPathInSchema = paths
      , cmCodec = codec
      , cmNumValues = numVals
      , cmTotalUncompressedSize = uncompSz
      , cmTotalCompressedSize = compSz
      , cmDataPageOffset = dataOff
      , cmDictionaryPageOffset = dpo
      , cmStatistics = stats
      , cmBloomFilterOffset = bfo
      , cmBloomFilterLength = bfl
      }
thriftToColumnMetadata _ = Left "Parquet.Footer: expected struct for ColumnMetadata"


expectEncoding :: TV.Value -> Either String Encoding
expectEncoding (TV.I32 e) =
  maybe (Left "Parquet.Footer: invalid encoding") Right (intToEncoding e)
expectEncoding _ = Left "Parquet.Footer: expected i32 in encodings"


expectPathStr :: TV.Value -> Either String T.Text
expectPathStr (TV.String t) = Right t
expectPathStr _ = Left "Parquet.Footer: expected string in path"


thriftToStatistics :: TV.Value -> Either String Statistics
thriftToStatistics (TV.Struct fields) = do
  minExact <- exactFlag 8
  maxExact <- exactFlag 7
  let fm = V.toList fields
      -- Thrift Compact stores both binary and UTF-8 strings under
      -- TT_STRING; the decoder surfaces TV.String when the bytes happen
      -- to parse as UTF-8. Statistics values are arbitrary bytes
      -- (PLAIN-encoded primitives) so accept either shape.
      minProbe = \case
        Statistics_Min b -> Just b
        (2, TV.String t) -> Just (Data.Text.Encoding.encodeUtf8 t)
        _ -> Nothing
      maxProbe = \case
        Statistics_Max b -> Just b
        (1, TV.String t) -> Just (Data.Text.Encoding.encodeUtf8 t)
        _ -> Nothing
      minValueProbe = \case
        Statistics_MinValue b -> Just b
        (6, TV.String t) -> Just (Data.Text.Encoding.encodeUtf8 t)
        _ -> Nothing
      maxValueProbe = \case
        Statistics_MaxValue b -> Just b
        (5, TV.String t) -> Just (Data.Text.Encoding.encodeUtf8 t)
        _ -> Nothing
  let statistics =
        Statistics
          { statMax = findField fm maxProbe
          , statMin = findField fm minProbe
          , statNullCount = findField fm $ \case
              Statistics_NullCount v -> Just v
              _ -> Nothing
          , statDistinctCount = findField fm $ \case
              Statistics_DistinctCount v -> Just v
              _ -> Nothing
          , statMaxValue = findField fm maxValueProbe
          , statMinValue = findField fm minValueProbe
          , statNanCount = findField fm $ \case
              Statistics_NanCount v -> Just v
              _ -> Nothing
          , statMinValueExact = minExact
          , statMaxValueExact = maxExact
          }
  if any
    (maybe False (< 0))
    [statNullCount statistics, statDistinctCount statistics, statNanCount statistics]
    then Left "Parquet.Footer: negative statistics count"
    else Right statistics
  where
    exactFlag tag = case V.toList (V.filter ((== tag) . fst) fields) of
      [] -> Right Nothing
      [(_, TV.Bool value)] -> Right (Just value)
      _ -> Left "Parquet.Footer: invalid or duplicate statistics exactness flag"
thriftToStatistics _ = Left "Parquet.Footer: expected struct for Statistics"


-- Helpers

{- | Look up a scalar field with a caller-supplied probe and fail with
a uniform error message if the field is absent or the probe rejects
its shape.
-}
require
  :: [(Int16, TV.Value)]
  -> String
  -> ((Int16, TV.Value) -> Maybe a)
  -> Either String a
require fm name probe = case findField fm probe of
  Just v -> Right v
  Nothing -> Left $ "Parquet.Footer: missing or invalid field " ++ name


{- | Variant of 'require' that expects a struct-list field; each element
is decoded with the supplied function.
-}
requireListStruct
  :: [(Int16, TV.Value)]
  -> String
  -> (TV.Value -> Either String a)
  -> ((Int16, TV.Value) -> Maybe (V.Vector TV.Value))
  -> Either String (V.Vector a)
requireListStruct fm name decode probe = case findField fm probe of
  Just xs -> V.mapM decode xs
  Nothing -> Left $ "Parquet.Footer: missing or invalid field " ++ name
