{-# LANGUAGE PatternSynonyms #-}

{- | Named Thrift-field pattern synonyms for every @parquet.thrift@
struct this codebase encodes or decodes.

Each pattern synonym has the form

@
pattern \<StructName\>_\<FieldName\> :: \<HaskellType\> -> (Int16, TV.Value)
pattern \<StructName\>_\<FieldName\> v = (\<field-id-from-parquet.thrift\>, TV.\<WireTag\> v)
@

and is bidirectional, so both writers and readers name the field slot
directly rather than by a bare integer. The motivation is concrete: a
recent bug had the writer emit @Iceberg field_id@ into Thrift field 8
of @SchemaElement@, which @parquet.thrift@ actually reserves for
@precision@. Because both sides used @(8, TV.I32 _)@ the wireform
self-round-trip hid it; pyarrow and parquet-mr silently dropped the
id. With pattern synonyms named after the @.thrift@ file, any such
mismatch becomes a visible name collision.

/Scope./ The synonyms cover exactly the fields we construct or match
against in "Parquet.Footer", "Parquet.Page", "Parquet.PageIndex",
"Parquet.Write" and "Parquet.BloomFilter". Fields we never touch
(e.g. @ColumnMetaData.key_value_metadata@ or
@ColumnMetaData.encoding_stats@) are intentionally absent; adding one
is a one-line exercise.

/Spec refs./ Field numbers below are quoted from
@apache/parquet-format@ at
@src/main/thrift/parquet.thrift@. Encryption structs are from the
same file's @FileCryptoMetaData@ / @EncryptionAlgorithm@ /
@AesGcmV1@ blocks (Parquet Modular Encryption, section 5.2).

Conventions:

* Scalar-valued fields carry their Thrift wire type directly
  (@I32@ / @I64@ / @String@ / @Binary@ / @Bool@).
* Enum-valued fields are exposed as @Int32@ — the caller converts
  through the domain-specific @\<foo\>ToInt@ / @intTo\<Foo\>@ helpers
  from "Parquet.Types". Keeping the synonyms at wire level makes the
  spec-to-code mapping 1:1 and avoids hiding Maybe-failures inside a
  pattern.
* Nested struct fields carry a @Vector (Int16, TV.Value)@ so the
  match succeeds only for a struct, and the inner pattern synonyms
  of that nested type are directly usable inside.
-}
module Parquet.Thrift.Schema (
  -- * Composition helpers
  optField,
  findField,

  -- * FileMetadata
  pattern FileMetadata_Version,
  pattern FileMetadata_Schema,
  pattern FileMetadata_NumRows,
  pattern FileMetadata_RowGroups,
  pattern FileMetadata_KeyValueMetadata,
  pattern KeyValue_Key,
  pattern KeyValue_Value,
  pattern FileMetadata_CreatedBy,
  pattern FileMetadata_ColumnOrders,
  pattern FileMetadata_EncryptionAlgorithm,
  pattern FileMetadata_FooterSigningKeyMetadata,

  -- * SchemaElement

  --

  {- | @parquet.thrift@:

  @
  1: optional Type                    type
  2: optional i32                     type_length
  3: optional FieldRepetitionType     repetition_type
  4: required string                  name
  5: optional i32                     num_children
  6: optional ConvertedType           converted_type
  7: optional i32                     scale
  8: optional i32                     precision
  9: optional i32                     field_id
  10: optional LogicalType            logicalType
  @
  -}
  pattern SchemaElement_Type,
  pattern SchemaElement_TypeLength,
  pattern SchemaElement_RepetitionType,
  pattern SchemaElement_Name,
  pattern SchemaElement_NumChildren,
  pattern SchemaElement_ConvertedType,
  pattern SchemaElement_Scale,
  pattern SchemaElement_Precision,
  pattern SchemaElement_FieldId,
  pattern SchemaElement_LogicalType,

  -- * RowGroup
  pattern RowGroup_Columns,
  pattern RowGroup_Ordinal,
  pattern RowGroup_TotalByteSize,
  pattern RowGroup_NumRows,
  pattern RowGroup_SortingColumns,

  -- ** SortingColumn struct fields
  pattern SortingColumn_ColumnIdx,
  pattern SortingColumn_Descending,
  pattern SortingColumn_NullsFirst,

  -- ** ColumnOrder union variants
  columnOrderTypeDefined,
  columnOrderIeee754Total,
  columnOrderInt96Timestamp,

  -- * ColumnChunk
  pattern ColumnChunk_FilePath,
  pattern ColumnChunk_FileOffset,
  pattern ColumnChunk_MetaData,
  pattern ColumnChunk_OffsetIndexOffset,
  pattern ColumnChunk_OffsetIndexLength,
  pattern ColumnChunk_ColumnIndexOffset,
  pattern ColumnChunk_ColumnIndexLength,
  pattern ColumnChunk_CryptoMetadata,
  pattern ColumnCryptoMetaData_EncryptionWithFooterKey,
  pattern ColumnCryptoMetaData_EncryptionWithColumnKey,
  pattern EncryptionWithColumnKey_PathInSchema,
  pattern EncryptionWithColumnKey_KeyMetadata,
  pattern ColumnChunk_EncryptedColumnMetadata,

  -- * ColumnMetaData
  pattern ColumnMetaData_Type,
  pattern ColumnMetaData_Encodings,
  pattern ColumnMetaData_PathInSchema,
  pattern ColumnMetaData_Codec,
  pattern ColumnMetaData_NumValues,
  pattern ColumnMetaData_TotalUncompressedSize,
  pattern ColumnMetaData_TotalCompressedSize,
  pattern ColumnMetaData_DataPageOffset,
  pattern ColumnMetaData_DictionaryPageOffset,
  pattern ColumnMetaData_Statistics,
  pattern ColumnMetaData_BloomFilterOffset,
  pattern ColumnMetaData_BloomFilterLength,

  -- * Statistics

  --

  {- | Parquet stores min/max as @TT_STRING@, which the Thrift compact
  decoder surfaces as either 'TV.Binary' or 'TV.String'. Only the
  'TV.Binary' constructor is used on the write side; reader callers
  should fall back to 'TV.String' via a dedicated helper.
  -}
  pattern Statistics_Max,
  pattern Statistics_Min,
  pattern Statistics_NullCount,
  pattern Statistics_DistinctCount,
  pattern Statistics_MaxValue,
  pattern Statistics_MinValue,
  pattern Statistics_NanCount,
  pattern Statistics_MinValueExact,
  pattern Statistics_MaxValueExact,

  -- * PageHeader

  --

  {- | @parquet.thrift@:

  @
  1: required PageType               type
  2: required i32                    uncompressed_page_size
  3: required i32                    compressed_page_size
  4: optional i32                    crc (skipped)
  5: optional DataPageHeader         data_page_header
  6: optional IndexPageHeader        index_page_header (skipped)
  7: optional DictionaryPageHeader   dictionary_page_header
  8: optional DataPageHeaderV2       data_page_header_v2
  @
  -}
  pattern PageHeader_Type,
  pattern PageHeader_UncompressedSize,
  pattern PageHeader_CompressedSize,
  pattern PageHeader_Crc,
  pattern PageHeader_DataPageHeader,
  pattern PageHeader_DictionaryPageHeader,
  pattern PageHeader_DataPageHeaderV2,

  -- * DataPageHeader
  pattern DataPageHeader_NumValues,
  pattern DataPageHeader_Encoding,
  pattern DataPageHeader_DefinitionLevelEncoding,
  pattern DataPageHeader_RepetitionLevelEncoding,

  -- * DictionaryPageHeader
  pattern DictionaryPageHeader_NumValues,
  pattern DictionaryPageHeader_Encoding,

  -- * DataPageHeaderV2
  pattern DataPageHeaderV2_NumValues,
  pattern DataPageHeaderV2_NumNulls,
  pattern DataPageHeaderV2_NumRows,
  pattern DataPageHeaderV2_Encoding,
  pattern DataPageHeaderV2_DefinitionLevelsByteLength,
  pattern DataPageHeaderV2_RepetitionLevelsByteLength,
  pattern DataPageHeaderV2_IsCompressed,

  -- * OffsetIndex
  pattern OffsetIndex_PageLocations,
  pattern OffsetIndex_UnencodedByteArrayDataBytes,

  -- * PageLocation
  pattern PageLocation_Offset,
  pattern PageLocation_CompressedPageSize,
  pattern PageLocation_FirstRowIndex,

  -- * ColumnIndex
  pattern ColumnIndex_NullPages,
  pattern ColumnIndex_MinValues,
  pattern ColumnIndex_MaxValues,
  pattern ColumnIndex_BoundaryOrder,
  pattern ColumnIndex_NullCounts,
  pattern ColumnIndex_NanCounts,
  pattern ColumnIndex_RepetitionLevelHistograms,
  pattern ColumnIndex_DefinitionLevelHistograms,

  -- * BloomFilterHeader
  pattern BloomFilterHeader_NumBytes,
  pattern BloomFilterHeader_Algorithm,
  pattern BloomFilterHeader_Hash,
  pattern BloomFilterHeader_Compression,

  -- * FileCryptoMetaData (Modular Encryption)
  pattern FileCryptoMetaData_EncryptionAlgorithm,
  pattern FileCryptoMetaData_KeyMetadata,

  -- * EncryptionAlgorithm

  --

  -- | Only the @AES_GCM_V1@ branch is used by the footer writer.
  pattern EncryptionAlgorithm_AesGcmV1,
  pattern EncryptionAlgorithm_AesGcmCtrV1,

  -- * AesGcmV1
  pattern AesGcmV1_AadPrefix,
  pattern AesGcmV1_AadFileUnique,
  pattern AesGcmV1_SupplyAadPrefix,
) where

import Data.ByteString (ByteString)
import Data.Int (Int16, Int32, Int64)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Thrift.Value qualified as TV
import Thrift.Wire (ThriftType (..))


-- ============================================================
-- Composition helpers
-- ============================================================

{- | @optField (Just x) P@ emits @[P x]@; @optField Nothing _@ emits
@[]@. Intended to be used on a list of pattern-synonym constructors
so that a writer body looks like the corresponding @.thrift@ struct:

> TV.Struct $ V.fromList $ concat
>   [ optField (seType se) (SchemaElement_Type . parquetTypeToInt)
>   , [SchemaElement_Name (seName se)]
>   , optField (seFieldId se) SchemaElement_FieldId
>   ]
-}
optField :: Maybe a -> (a -> (Int16, TV.Value)) -> [(Int16, TV.Value)]
optField Nothing _ = []
optField (Just x) f = [f x]


{- | Find the first field entry for which @probe@ returns @Just@.
Typical call site:

> findField fm $ \case
>   SchemaElement_FieldId v -> Just v
>   _                       -> Nothing

Thrift compact sorts struct fields by id on the wire so the first
match is always the intended one.
-}
findField
  :: [(Int16, TV.Value)]
  -> ((Int16, TV.Value) -> Maybe a)
  -> Maybe a
findField fs probe = listToMaybe (mapMaybe probe fs)


-- ============================================================
-- FileMetadata
-- ============================================================

pattern FileMetadata_Version :: Int32 -> (Int16, TV.Value)
pattern FileMetadata_Version v = (1, TV.I32 v)


pattern FileMetadata_Schema :: Vector TV.Value -> (Int16, TV.Value)
pattern FileMetadata_Schema elems <- (2, TV.List _ elems)
  where
    FileMetadata_Schema elems = (2, TV.List TT_STRUCT elems)


-- FileMetaData.key_value_metadata is field 5. KeyValue uses the Thrift
-- string wire type; Value exposes both decoded Text and opaque bytes.
pattern FileMetadata_KeyValueMetadata :: Vector TV.Value -> (Int16, TV.Value)
pattern FileMetadata_KeyValueMetadata xs <- (5, TV.List _ xs)
  where
    FileMetadata_KeyValueMetadata xs = (5, TV.List TT_STRUCT xs)


pattern KeyValue_Key :: TV.Value -> (Int16, TV.Value)
pattern KeyValue_Key value = (1, value)


pattern KeyValue_Value :: TV.Value -> (Int16, TV.Value)
pattern KeyValue_Value value = (2, value)


pattern FileMetadata_NumRows :: Int64 -> (Int16, TV.Value)
pattern FileMetadata_NumRows v = (3, TV.I64 v)


pattern FileMetadata_RowGroups :: Vector TV.Value -> (Int16, TV.Value)
pattern FileMetadata_RowGroups elems <- (4, TV.List _ elems)
  where
    FileMetadata_RowGroups elems = (4, TV.List TT_STRUCT elems)


{- | @FileMetaData.created_by@ is field id 6 in the official
parquet.thrift; field id 5 is reserved for
@key_value_metadata@. Strict readers (pyarrow, the dataframe
library, parquet-mr's footer parser) raise on the wrong slot.
-}
pattern FileMetadata_CreatedBy :: Text -> (Int16, TV.Value)
pattern FileMetadata_CreatedBy t = (6, TV.String t)


{- | @FileMetaData.column_orders@ is field id 7 in
parquet.thrift. The list is parallel to the leaf-column
order in 'fmSchema' and tells readers how to compare
BYTE_ARRAY statistics (every modern writer emits this; the
column index without it is unreliable for pushdown on
string columns).
-}
pattern FileMetadata_ColumnOrders :: Vector TV.Value -> (Int16, TV.Value)
pattern FileMetadata_ColumnOrders xs <- (7, TV.List _ xs)
  where
    FileMetadata_ColumnOrders xs = (7, TV.List TT_STRUCT xs)


pattern FileMetadata_EncryptionAlgorithm :: Vector (Int16, TV.Value) -> (Int16, TV.Value)
pattern FileMetadata_EncryptionAlgorithm fs = (8, TV.Struct fs)


pattern FileMetadata_FooterSigningKeyMetadata :: ByteString -> (Int16, TV.Value)
pattern FileMetadata_FooterSigningKeyMetadata v = (9, TV.Binary v)


-- ============================================================
-- SchemaElement
-- ============================================================

pattern SchemaElement_Type :: Int32 -> (Int16, TV.Value)
pattern SchemaElement_Type v = (1, TV.I32 v)


pattern SchemaElement_TypeLength :: Int32 -> (Int16, TV.Value)
pattern SchemaElement_TypeLength v = (2, TV.I32 v)


pattern SchemaElement_RepetitionType :: Int32 -> (Int16, TV.Value)
pattern SchemaElement_RepetitionType v = (3, TV.I32 v)


pattern SchemaElement_Name :: Text -> (Int16, TV.Value)
pattern SchemaElement_Name t = (4, TV.String t)


pattern SchemaElement_NumChildren :: Int32 -> (Int16, TV.Value)
pattern SchemaElement_NumChildren v = (5, TV.I32 v)


pattern SchemaElement_ConvertedType :: Int32 -> (Int16, TV.Value)
pattern SchemaElement_ConvertedType v = (6, TV.I32 v)


pattern SchemaElement_Scale :: Int32 -> (Int16, TV.Value)
pattern SchemaElement_Scale v = (7, TV.I32 v)


pattern SchemaElement_Precision :: Int32 -> (Int16, TV.Value)
pattern SchemaElement_Precision v = (8, TV.I32 v)


pattern SchemaElement_FieldId :: Int32 -> (Int16, TV.Value)
pattern SchemaElement_FieldId v = (9, TV.I32 v)


{- | LogicalType is encoded as field 10, holding a struct that
represents the @LogicalType@ union from @parquet.thrift@.
The struct carries exactly one field whose id picks the
variant (StringType=1, MapType=2, …).
-}
pattern SchemaElement_LogicalType
  :: Vector (Int16, TV.Value) -> (Int16, TV.Value)
pattern SchemaElement_LogicalType fs = (10, TV.Struct fs)


-- ============================================================
-- RowGroup
-- ============================================================

pattern RowGroup_Columns :: Vector TV.Value -> (Int16, TV.Value)
pattern RowGroup_Columns elems <- (1, TV.List _ elems)
  where
    RowGroup_Columns elems = (1, TV.List TT_STRUCT elems)


pattern RowGroup_Ordinal :: Int16 -> (Int16, TV.Value)
pattern RowGroup_Ordinal v = (7, TV.I16 v)


pattern RowGroup_TotalByteSize :: Int64 -> (Int16, TV.Value)
pattern RowGroup_TotalByteSize v = (2, TV.I64 v)


pattern RowGroup_NumRows :: Int64 -> (Int16, TV.Value)
pattern RowGroup_NumRows v = (3, TV.I64 v)


{- | @RowGroup.sorting_columns@ is field id 4 — list of
'SortingColumn' (struct {column_idx, descending, nulls_first}).
Lets a reader skip ORDER BY when scanning a sorted file.
-}
pattern RowGroup_SortingColumns :: Vector TV.Value -> (Int16, TV.Value)
pattern RowGroup_SortingColumns xs <- (4, TV.List _ xs)
  where
    RowGroup_SortingColumns xs = (4, TV.List TT_STRUCT xs)


-- ============================================================
-- SortingColumn
-- ============================================================

pattern SortingColumn_ColumnIdx :: Int32 -> (Int16, TV.Value)
pattern SortingColumn_ColumnIdx v = (1, TV.I32 v)


pattern SortingColumn_Descending :: Bool -> (Int16, TV.Value)
pattern SortingColumn_Descending b = (2, TV.Bool b)


pattern SortingColumn_NullsFirst :: Bool -> (Int16, TV.Value)
pattern SortingColumn_NullsFirst b = (3, TV.Bool b)


-- ============================================================
-- ColumnOrder
-- ============================================================
-- All three ColumnOrder union variants carry an empty struct.
columnOrderTypeDefined :: TV.Value
columnOrderTypeDefined = TV.Struct (V.singleton (1, TV.Struct V.empty))


columnOrderIeee754Total :: TV.Value
columnOrderIeee754Total = TV.Struct (V.singleton (2, TV.Struct V.empty))


columnOrderInt96Timestamp :: TV.Value
columnOrderInt96Timestamp = TV.Struct (V.singleton (3, TV.Struct V.empty))


-- ============================================================
-- ColumnChunk
-- ============================================================

pattern ColumnChunk_FilePath :: Text -> (Int16, TV.Value)
pattern ColumnChunk_FilePath t = (1, TV.String t)


pattern ColumnChunk_FileOffset :: Int64 -> (Int16, TV.Value)
pattern ColumnChunk_FileOffset v = (2, TV.I64 v)


pattern ColumnChunk_MetaData :: Vector (Int16, TV.Value) -> (Int16, TV.Value)
pattern ColumnChunk_MetaData fs = (3, TV.Struct fs)


pattern ColumnChunk_OffsetIndexOffset :: Int64 -> (Int16, TV.Value)
pattern ColumnChunk_OffsetIndexOffset v = (4, TV.I64 v)


pattern ColumnChunk_OffsetIndexLength :: Int32 -> (Int16, TV.Value)
pattern ColumnChunk_OffsetIndexLength v = (5, TV.I32 v)


pattern ColumnChunk_ColumnIndexOffset :: Int64 -> (Int16, TV.Value)
pattern ColumnChunk_ColumnIndexOffset v = (6, TV.I64 v)


pattern ColumnChunk_ColumnIndexLength :: Int32 -> (Int16, TV.Value)
pattern ColumnChunk_ColumnIndexLength v = (7, TV.I32 v)


pattern ColumnChunk_CryptoMetadata :: Vector (Int16, TV.Value) -> (Int16, TV.Value)
pattern ColumnChunk_CryptoMetadata fs = (8, TV.Struct fs)


pattern ColumnCryptoMetaData_EncryptionWithFooterKey :: Vector (Int16, TV.Value) -> (Int16, TV.Value)
pattern ColumnCryptoMetaData_EncryptionWithFooterKey fs = (1, TV.Struct fs)


pattern ColumnCryptoMetaData_EncryptionWithColumnKey :: Vector (Int16, TV.Value) -> (Int16, TV.Value)
pattern ColumnCryptoMetaData_EncryptionWithColumnKey fs = (2, TV.Struct fs)


pattern EncryptionWithColumnKey_PathInSchema :: Vector TV.Value -> (Int16, TV.Value)
pattern EncryptionWithColumnKey_PathInSchema values <- (1, TV.List _ values)
  where
    EncryptionWithColumnKey_PathInSchema values = (1, TV.List TT_STRING values)


pattern EncryptionWithColumnKey_KeyMetadata :: ByteString -> (Int16, TV.Value)
pattern EncryptionWithColumnKey_KeyMetadata value = (2, TV.Binary value)


pattern ColumnChunk_EncryptedColumnMetadata :: ByteString -> (Int16, TV.Value)
pattern ColumnChunk_EncryptedColumnMetadata value = (9, TV.Binary value)


-- ============================================================
-- ColumnMetaData
-- ============================================================

pattern ColumnMetaData_Type :: Int32 -> (Int16, TV.Value)
pattern ColumnMetaData_Type v = (1, TV.I32 v)


pattern ColumnMetaData_Encodings :: Vector TV.Value -> (Int16, TV.Value)
pattern ColumnMetaData_Encodings elems <- (2, TV.List _ elems)
  where
    ColumnMetaData_Encodings elems = (2, TV.List TT_I32 elems)


pattern ColumnMetaData_PathInSchema :: Vector TV.Value -> (Int16, TV.Value)
pattern ColumnMetaData_PathInSchema elems <- (3, TV.List _ elems)
  where
    ColumnMetaData_PathInSchema elems = (3, TV.List TT_STRING elems)


pattern ColumnMetaData_Codec :: Int32 -> (Int16, TV.Value)
pattern ColumnMetaData_Codec v = (4, TV.I32 v)


pattern ColumnMetaData_NumValues :: Int64 -> (Int16, TV.Value)
pattern ColumnMetaData_NumValues v = (5, TV.I64 v)


pattern ColumnMetaData_TotalUncompressedSize :: Int64 -> (Int16, TV.Value)
pattern ColumnMetaData_TotalUncompressedSize v = (6, TV.I64 v)


pattern ColumnMetaData_TotalCompressedSize :: Int64 -> (Int16, TV.Value)
pattern ColumnMetaData_TotalCompressedSize v = (7, TV.I64 v)


pattern ColumnMetaData_DataPageOffset :: Int64 -> (Int16, TV.Value)
pattern ColumnMetaData_DataPageOffset v = (9, TV.I64 v)


{- | parquet.thrift @ColumnMetaData.index_page_offset@ (field
10). Optional; we don't currently consume it but keeping
the slot reserved means we never collide with it on
subsequent additions.
-}
pattern ColumnMetaData_IndexPageOffset :: Int64 -> (Int16, TV.Value)
pattern ColumnMetaData_IndexPageOffset v = (10, TV.I64 v)


{- | parquet.thrift @ColumnMetaData.dictionary_page_offset@
(field 11). Byte offset of the dictionary page; lives
/before/ 'ColumnMetaData_DataPageOffset' when present.
Modern writers (pyarrow / duckdb / polars / parquet-mr)
always populate this for dictionary-encoded columns; readers
that ignore it can't access the dictionary, so RLE_DICTIONARY
pages fail to decode.
-}
pattern ColumnMetaData_DictionaryPageOffset :: Int64 -> (Int16, TV.Value)
pattern ColumnMetaData_DictionaryPageOffset v = (11, TV.I64 v)


pattern ColumnMetaData_Statistics
  :: Vector (Int16, TV.Value) -> (Int16, TV.Value)
pattern ColumnMetaData_Statistics fs = (12, TV.Struct fs)


pattern ColumnMetaData_BloomFilterOffset :: Int64 -> (Int16, TV.Value)
pattern ColumnMetaData_BloomFilterOffset v = (14, TV.I64 v)


pattern ColumnMetaData_BloomFilterLength :: Int32 -> (Int16, TV.Value)
pattern ColumnMetaData_BloomFilterLength v = (15, TV.I32 v)


-- ============================================================
-- Statistics
-- ============================================================

pattern Statistics_Max :: ByteString -> (Int16, TV.Value)
pattern Statistics_Max v = (1, TV.Binary v)


pattern Statistics_Min :: ByteString -> (Int16, TV.Value)
pattern Statistics_Min v = (2, TV.Binary v)


pattern Statistics_NanCount :: Int64 -> (Int16, TV.Value)
pattern Statistics_NanCount v = (9, TV.I64 v)


pattern Statistics_NullCount :: Int64 -> (Int16, TV.Value)
pattern Statistics_NullCount v = (3, TV.I64 v)


pattern Statistics_DistinctCount :: Int64 -> (Int16, TV.Value)
pattern Statistics_DistinctCount v = (4, TV.I64 v)


pattern Statistics_MaxValue :: ByteString -> (Int16, TV.Value)
pattern Statistics_MaxValue v = (5, TV.Binary v)


pattern Statistics_MinValue :: ByteString -> (Int16, TV.Value)
pattern Statistics_MinValue v = (6, TV.Binary v)


pattern Statistics_MaxValueExact :: Bool -> (Int16, TV.Value)
pattern Statistics_MaxValueExact v = (7, TV.Bool v)


pattern Statistics_MinValueExact :: Bool -> (Int16, TV.Value)
pattern Statistics_MinValueExact v = (8, TV.Bool v)


-- ============================================================
-- PageHeader
-- ============================================================

pattern PageHeader_Type :: Int32 -> (Int16, TV.Value)
pattern PageHeader_Type v = (1, TV.I32 v)


pattern PageHeader_UncompressedSize :: Int32 -> (Int16, TV.Value)
pattern PageHeader_UncompressedSize v = (2, TV.I32 v)


pattern PageHeader_CompressedSize :: Int32 -> (Int16, TV.Value)
pattern PageHeader_CompressedSize v = (3, TV.I32 v)


pattern PageHeader_Crc :: Int32 -> (Int16, TV.Value)
pattern PageHeader_Crc v = (4, TV.I32 v)


pattern PageHeader_DataPageHeader
  :: Vector (Int16, TV.Value) -> (Int16, TV.Value)
pattern PageHeader_DataPageHeader fs = (5, TV.Struct fs)


pattern PageHeader_DictionaryPageHeader
  :: Vector (Int16, TV.Value) -> (Int16, TV.Value)
pattern PageHeader_DictionaryPageHeader fs = (7, TV.Struct fs)


pattern PageHeader_DataPageHeaderV2
  :: Vector (Int16, TV.Value) -> (Int16, TV.Value)
pattern PageHeader_DataPageHeaderV2 fs = (8, TV.Struct fs)


-- ============================================================
-- DataPageHeader
-- ============================================================

pattern DataPageHeader_NumValues :: Int32 -> (Int16, TV.Value)
pattern DataPageHeader_NumValues v = (1, TV.I32 v)


pattern DataPageHeader_Encoding :: Int32 -> (Int16, TV.Value)
pattern DataPageHeader_Encoding v = (2, TV.I32 v)


{- | Field 3 (@definition_level_encoding@) and field 4
(@repetition_level_encoding@) are both marked /required/ in
parquet.thrift's @DataPageHeader@. For max-def-level-0 columns
the encoded level data is zero bytes regardless, but strict
readers (pyarrow, parquet-mr, the dataframe library) still
require the fields to be present in the page header.
-}
pattern DataPageHeader_DefinitionLevelEncoding :: Int32 -> (Int16, TV.Value)
pattern DataPageHeader_DefinitionLevelEncoding v = (3, TV.I32 v)


pattern DataPageHeader_RepetitionLevelEncoding :: Int32 -> (Int16, TV.Value)
pattern DataPageHeader_RepetitionLevelEncoding v = (4, TV.I32 v)


-- ============================================================
-- DictionaryPageHeader
-- ============================================================

pattern DictionaryPageHeader_NumValues :: Int32 -> (Int16, TV.Value)
pattern DictionaryPageHeader_NumValues v = (1, TV.I32 v)


pattern DictionaryPageHeader_Encoding :: Int32 -> (Int16, TV.Value)
pattern DictionaryPageHeader_Encoding v = (2, TV.I32 v)


-- ============================================================
-- DataPageHeaderV2
-- ============================================================

pattern DataPageHeaderV2_NumValues :: Int32 -> (Int16, TV.Value)
pattern DataPageHeaderV2_NumValues v = (1, TV.I32 v)


pattern DataPageHeaderV2_NumNulls :: Int32 -> (Int16, TV.Value)
pattern DataPageHeaderV2_NumNulls v = (2, TV.I32 v)


pattern DataPageHeaderV2_NumRows :: Int32 -> (Int16, TV.Value)
pattern DataPageHeaderV2_NumRows v = (3, TV.I32 v)


pattern DataPageHeaderV2_Encoding :: Int32 -> (Int16, TV.Value)
pattern DataPageHeaderV2_Encoding v = (4, TV.I32 v)


pattern DataPageHeaderV2_DefinitionLevelsByteLength
  :: Int32 -> (Int16, TV.Value)
pattern DataPageHeaderV2_DefinitionLevelsByteLength v = (5, TV.I32 v)


pattern DataPageHeaderV2_RepetitionLevelsByteLength
  :: Int32 -> (Int16, TV.Value)
pattern DataPageHeaderV2_RepetitionLevelsByteLength v = (6, TV.I32 v)


pattern DataPageHeaderV2_IsCompressed :: Bool -> (Int16, TV.Value)
pattern DataPageHeaderV2_IsCompressed v = (7, TV.Bool v)


-- ============================================================
-- OffsetIndex
-- ============================================================

pattern OffsetIndex_PageLocations :: Vector TV.Value -> (Int16, TV.Value)
pattern OffsetIndex_PageLocations elems <- (1, TV.List _ elems)
  where
    OffsetIndex_PageLocations elems = (1, TV.List TT_STRUCT elems)


pattern OffsetIndex_UnencodedByteArrayDataBytes
  :: Vector TV.Value -> (Int16, TV.Value)
pattern OffsetIndex_UnencodedByteArrayDataBytes elems <- (2, TV.List _ elems)
  where
    OffsetIndex_UnencodedByteArrayDataBytes elems =
      (2, TV.List TT_I64 elems)


-- ============================================================
-- PageLocation
-- ============================================================

pattern PageLocation_Offset :: Int64 -> (Int16, TV.Value)
pattern PageLocation_Offset v = (1, TV.I64 v)


pattern PageLocation_CompressedPageSize :: Int32 -> (Int16, TV.Value)
pattern PageLocation_CompressedPageSize v = (2, TV.I32 v)


pattern PageLocation_FirstRowIndex :: Int64 -> (Int16, TV.Value)
pattern PageLocation_FirstRowIndex v = (3, TV.I64 v)


-- ============================================================
-- ColumnIndex
-- ============================================================

pattern ColumnIndex_NullPages :: Vector TV.Value -> (Int16, TV.Value)
pattern ColumnIndex_NullPages elems <- (1, TV.List _ elems)
  where
    ColumnIndex_NullPages elems = (1, TV.List TT_BOOL elems)


pattern ColumnIndex_MinValues :: Vector TV.Value -> (Int16, TV.Value)
pattern ColumnIndex_MinValues elems <- (2, TV.List _ elems)
  where
    ColumnIndex_MinValues elems = (2, TV.List TT_STRING elems)


pattern ColumnIndex_MaxValues :: Vector TV.Value -> (Int16, TV.Value)
pattern ColumnIndex_MaxValues elems <- (3, TV.List _ elems)
  where
    ColumnIndex_MaxValues elems = (3, TV.List TT_STRING elems)


pattern ColumnIndex_BoundaryOrder :: Int32 -> (Int16, TV.Value)
pattern ColumnIndex_BoundaryOrder v = (4, TV.I32 v)


pattern ColumnIndex_NullCounts :: Vector TV.Value -> (Int16, TV.Value)
pattern ColumnIndex_NullCounts elems <- (5, TV.List _ elems)
  where
    ColumnIndex_NullCounts elems = (5, TV.List TT_I64 elems)


pattern ColumnIndex_NanCounts :: Vector TV.Value -> (Int16, TV.Value)
pattern ColumnIndex_NanCounts elems <- (8, TV.List _ elems)
  where
    ColumnIndex_NanCounts elems = (8, TV.List TT_I64 elems)


pattern ColumnIndex_RepetitionLevelHistograms
  :: Vector TV.Value -> (Int16, TV.Value)
pattern ColumnIndex_RepetitionLevelHistograms elems <- (6, TV.List _ elems)
  where
    ColumnIndex_RepetitionLevelHistograms elems =
      (6, TV.List TT_I64 elems)


pattern ColumnIndex_DefinitionLevelHistograms
  :: Vector TV.Value -> (Int16, TV.Value)
pattern ColumnIndex_DefinitionLevelHistograms elems <- (7, TV.List _ elems)
  where
    ColumnIndex_DefinitionLevelHistograms elems =
      (7, TV.List TT_I64 elems)


-- ============================================================
-- BloomFilterHeader
-- ============================================================
--
-- Parquet's BloomFilterHeader carries three nested "enum-tag structs",
-- each being a union where one empty struct variant is emitted:
--

-- * @algorithm@: only @BLOCK@ (variant 1) is defined.


-- * @hash@:      only @XXHASH@ (variant 1) is defined.


-- * @compression@: only @UNCOMPRESSED@ (variant 1) is defined.


pattern BloomFilterHeader_NumBytes :: Int32 -> (Int16, TV.Value)
pattern BloomFilterHeader_NumBytes v = (1, TV.I32 v)


pattern BloomFilterHeader_Algorithm
  :: Vector (Int16, TV.Value) -> (Int16, TV.Value)
pattern BloomFilterHeader_Algorithm fs = (2, TV.Struct fs)


pattern BloomFilterHeader_Hash
  :: Vector (Int16, TV.Value) -> (Int16, TV.Value)
pattern BloomFilterHeader_Hash fs = (3, TV.Struct fs)


pattern BloomFilterHeader_Compression
  :: Vector (Int16, TV.Value) -> (Int16, TV.Value)
pattern BloomFilterHeader_Compression fs = (4, TV.Struct fs)


-- ============================================================
-- FileCryptoMetaData
-- ============================================================

pattern FileCryptoMetaData_EncryptionAlgorithm
  :: Vector (Int16, TV.Value) -> (Int16, TV.Value)
pattern FileCryptoMetaData_EncryptionAlgorithm fs = (1, TV.Struct fs)


pattern FileCryptoMetaData_KeyMetadata :: ByteString -> (Int16, TV.Value)
pattern FileCryptoMetaData_KeyMetadata v = (2, TV.Binary v)


-- ============================================================
-- EncryptionAlgorithm
-- ============================================================

pattern EncryptionAlgorithm_AesGcmV1
  :: Vector (Int16, TV.Value) -> (Int16, TV.Value)
pattern EncryptionAlgorithm_AesGcmV1 fs = (1, TV.Struct fs)


pattern EncryptionAlgorithm_AesGcmCtrV1
  :: Vector (Int16, TV.Value) -> (Int16, TV.Value)
pattern EncryptionAlgorithm_AesGcmCtrV1 fs = (2, TV.Struct fs)


-- ============================================================
-- AesGcmV1
-- ============================================================

pattern AesGcmV1_AadPrefix :: ByteString -> (Int16, TV.Value)
pattern AesGcmV1_AadPrefix v = (1, TV.Binary v)


pattern AesGcmV1_AadFileUnique :: ByteString -> (Int16, TV.Value)
pattern AesGcmV1_AadFileUnique v = (2, TV.Binary v)


pattern AesGcmV1_SupplyAadPrefix :: Bool -> (Int16, TV.Value)
pattern AesGcmV1_SupplyAadPrefix v = (3, TV.Bool v)
