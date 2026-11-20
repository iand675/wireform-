{- | Apache Parquet file metadata types.

Parquet is a columnar storage format. The metadata (file footer, schema,
row groups, column chunks) is Thrift Compact Protocol encoded. These types
mirror the Parquet spec's Thrift definitions.
-}
module Parquet.Types (
  FileMetadata (..),
  KeyValue (..),
  SchemaElement (..),
  hasDefinedStatisticsOrder,
  hasCompatiblePhysicalType,
  RowGroup (..),
  SortingColumn (..),
  ColumnOrder (..),
  ColumnChunk (..),
  ColumnMetadata (..),
  ParquetType (..),
  Repetition (..),
  Encoding (..),
  Compression (..),
  ConvertedType (..),
  LogicalType (..),
  LtTimeUnit (..),
  GeometryParameters (..),
  GeographyParameters (..),
  EdgeInterpolationAlgorithm (..),
  defaultGeometryParameters,
  defaultGeographyParameters,
  interpolationAlgorithmToInt,
  intToInterpolationAlgorithm,
  Statistics (..),
  PageLocation (..),
  OffsetIndex (..),
  ColumnIndex (..),
  BoundaryOrder (..),
  parquetTypeToInt,
  intToParquetType,
  boundaryOrderToInt,
  intToBoundaryOrder,
) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Parquet.Decimal (decimalPrecisionFits)


data ParquetType
  = PTBoolean
  | PTInt32
  | PTInt64
  | PTInt96
  | PTFloat
  | PTDouble
  | PTByteArray
  | PTFixedLenByteArray
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)


parquetTypeToInt :: ParquetType -> Int32
parquetTypeToInt = \case
  PTBoolean -> 0
  PTInt32 -> 1
  PTInt64 -> 2
  PTInt96 -> 3
  PTFloat -> 4
  PTDouble -> 5
  PTByteArray -> 6
  PTFixedLenByteArray -> 7


intToParquetType :: Int32 -> Maybe ParquetType
intToParquetType = \case
  0 -> Just PTBoolean
  1 -> Just PTInt32
  2 -> Just PTInt64
  3 -> Just PTInt96
  4 -> Just PTFloat
  5 -> Just PTDouble
  6 -> Just PTByteArray
  7 -> Just PTFixedLenByteArray
  _ -> Nothing


data Repetition = Required | Optional | Repeated
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)


data Encoding
  = Plain
  | PlainDictionary
  | RLE
  | BitPacked
  | DeltaBinaryPacked
  | DeltaLengthByteArray
  | DeltaByteArray
  | RLEDictionary
  | ByteStreamSplit
  | Alp
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)


data Compression
  = Uncompressed
  | Snappy
  | GZip
  | LZO
  | Brotli
  | LZ4
  | ZSTD
  | LZ4Raw
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)


data ConvertedType
  = CTUtf8
  | CTMap
  | CTMapKeyValue
  | CTList
  | CTEnum
  | CTDecimal
  | CTDate
  | CTTimeMillis
  | CTTimeMicros
  | CTTimestampMillis
  | CTTimestampMicros
  | CTUInt8
  | CTUInt16
  | CTUInt32
  | CTUInt64
  | CTInt8
  | CTInt16
  | CTInt32
  | CTInt64
  | CTJson
  | CTBson
  | CTInterval
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)


-- | LogicalType time/timestamp unit (parquet.thrift @TimeUnit@).
data LtTimeUnit = LtMillis | LtMicros | LtNanos
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)


-- | Preserve absence separately from an explicit CRS. Absent means OGC:CRS84.
data GeometryParameters = GeometryParameters
  { geometryCrs :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | Absent CRS means OGC:CRS84; absent interpolation means spherical edges.
data GeographyParameters = GeographyParameters
  { geographyCrs :: !(Maybe Text)
  , geographyAlgorithm :: !(Maybe EdgeInterpolationAlgorithm)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data EdgeInterpolationAlgorithm
  = Spherical
  | Vincenty
  | Thomas
  | Andoyer
  | Karney
  | UnrecognizedInterpolation {-# UNPACK #-} !Int32
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


defaultGeometryParameters :: GeometryParameters
defaultGeometryParameters = GeometryParameters Nothing


defaultGeographyParameters :: GeographyParameters
defaultGeographyParameters = GeographyParameters Nothing Nothing


interpolationAlgorithmToInt :: EdgeInterpolationAlgorithm -> Int32
interpolationAlgorithmToInt Spherical = 0
interpolationAlgorithmToInt Vincenty = 1
interpolationAlgorithmToInt Thomas = 2
interpolationAlgorithmToInt Andoyer = 3
interpolationAlgorithmToInt Karney = 4
interpolationAlgorithmToInt (UnrecognizedInterpolation code) = code


intToInterpolationAlgorithm :: Int32 -> EdgeInterpolationAlgorithm
intToInterpolationAlgorithm 0 = Spherical
intToInterpolationAlgorithm 1 = Vincenty
intToInterpolationAlgorithm 2 = Thomas
intToInterpolationAlgorithm 3 = Andoyer
intToInterpolationAlgorithm 4 = Karney
intToInterpolationAlgorithm code = UnrecognizedInterpolation code


{- | Modern Parquet logical-type annotation. Mirrors the
@LogicalType@ union in @parquet.thrift@. Each constructor
corresponds to a single field of the union.

This ADT is the modern annotation slot ('seLogicalType');
'ConvertedType' is the legacy slot kept around for older
readers. Writers should populate /both/ slots when possible.
-}
data LogicalType
  = LTString
  | LTMap
  | LTList
  | LTEnum
  | -- | @(precision, scale)@.
    LTDecimal !Int32 !Int32
  | LTDate
  | LTTime
      !Bool
      -- ^ @isAdjustedToUTC@
      !LtTimeUnit
      -- ^ unit (millis / micros / nanos)
  | LTTimestamp
      !Bool
      -- ^ @isAdjustedToUTC@
      !LtTimeUnit
      -- ^ unit
  | LTInteger
      !Int32
      -- ^ bit width (8, 16, 32, 64)
      !Bool
      -- ^ is signed
  | LTNull
  | LTJson
  | LTBson
  | LTUUID
  | {- | Parquet IEEE 754 half-precision; physical type is
    @FIXED_LEN_BYTE_ARRAY(2)@.
    -}
    LTFloat16
  | {- | Parquet geospatial geometry (parquet-format 2.11+);
    physical type is @BYTE_ARRAY@ holding WKB.
    -}
    LTGeometry !GeometryParameters
  | {- | Parquet geospatial geography (parquet-format 2.11+);
    physical type is @BYTE_ARRAY@ holding WKB.
    -}
    LTGeography !GeographyParameters
  | -- | A group of optional inline/external byte-reference fields (format 2.14).
    LTFile
  | {- | Parquet 'Variant' (semi-structured) annotation. The
    payload column is itself a struct of (metadata, value)
    BYTE_ARRAY columns; the carried Int32 is the
    @specification_version@ field.
    -}
    LTVariant !Int32
  | {- | An unrecognized or malformed modern annotation. Readers may expose
    the physical storage, but cannot infer a logical ordering from it.
    -}
    LTUnsupported
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data SchemaElement = SchemaElement
  { seName :: !Text
  , seRepetition :: !(Maybe Repetition)
  , seType :: !(Maybe ParquetType)
  , seNumChildren :: !(Maybe Int32)
  , seConvertedType :: !(Maybe ConvertedType)
  , seLogicalType :: !(Maybe LogicalType)
  , seFieldId :: !(Maybe Int32)
  {- ^ Iceberg's identifier for this leaf column. Required for any
  Parquet file that participates in an Iceberg table — readers
  match @field_id@, not @name@. Encoded into Thrift field 9 of
  @parquet.thrift::SchemaElement@ (field 8 is @precision@, field 7
  is @scale@).
  -}
  , seTypeLength :: !(Maybe Int32)
  -- ^ Physical byte width for FIXED_LEN_BYTE_ARRAY (Thrift field 2).
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


{- | These annotations explicitly forbid min/max statistics. Raw storage
order is not a substitute for an undefined logical order.
-}
hasDefinedStatisticsOrder :: SchemaElement -> Bool
hasDefinedStatisticsOrder field =
  hasCompatiblePhysicalType field && case seLogicalType field of
    Just LTGeometry {} -> False
    Just LTGeography {} -> False
    Just LTFile -> False
    Just LTNull -> False
    Just LTVariant {} -> False
    _ -> seConvertedType field /= Just CTInterval


{- | Recognized annotations must use the carriers specified by Parquet.
Unknown/unsupported combinations cannot supply usable ordered bounds.
-}
hasCompatiblePhysicalType :: SchemaElement -> Bool
hasCompatiblePhysicalType field = case seLogicalType field of
  Just LTUnsupported -> False
  Just LTString -> bytes
  Just LTEnum -> bytes
  Just LTJson -> bytes
  Just LTBson -> bytes
  Just LTGeometry {} -> bytes
  Just LTGeography {} -> bytes
  Just LTDate -> physical PTInt32
  Just (LTTime _ LtMillis) -> physical PTInt32
  Just (LTTime _ _) -> physical PTInt64
  Just LTTimestamp {} -> physical PTInt64 || fixed 12
  Just (LTInteger width _) -> (width `elem` [8, 16, 32] && physical PTInt32) || (width == 64 && physical PTInt64)
  Just (LTDecimal precision scale) ->
    precision > 0 && scale >= 0 && scale <= precision && case seType field of
      Just PTInt32 -> precision <= 9
      Just PTInt64 -> precision <= 18
      Just PTByteArray -> True
      Just PTFixedLenByteArray -> maybe False (decimalPrecisionFits precision) (seTypeLength field)
      _ -> False
  Just LTFloat16 -> fixed 2
  Just LTUUID -> fixed 16
  Just LTList -> group
  Just LTMap -> group
  Just LTVariant {} -> group
  Just LTFile -> group
  Just LTNull -> True
  Nothing -> case seConvertedType field of
    Just CTUtf8 -> bytes
    Just CTEnum -> bytes
    Just CTJson -> bytes
    Just CTBson -> bytes
    Just CTDate -> physical PTInt32
    Just CTTimeMillis -> physical PTInt32
    Just CTTimeMicros -> physical PTInt64
    Just CTTimestampMillis -> physical PTInt64
    Just CTTimestampMicros -> physical PTInt64
    Just CTUInt8 -> physical PTInt32
    Just CTUInt16 -> physical PTInt32
    Just CTUInt32 -> physical PTInt32
    Just CTUInt64 -> physical PTInt64
    Just CTInt8 -> physical PTInt32
    Just CTInt16 -> physical PTInt32
    Just CTInt32 -> physical PTInt32
    Just CTInt64 -> physical PTInt64
    Just CTInterval -> fixed 12
    Just CTMap -> group
    Just CTMapKeyValue -> group
    Just CTList -> group
    Just CTDecimal -> seType field `elem` (Just <$> [PTInt32, PTInt64, PTByteArray, PTFixedLenByteArray])
    Nothing -> True
  where
    physical ty = seType field == Just ty
    bytes = physical PTByteArray
    fixed size = physical PTFixedLenByteArray && seTypeLength field == Just size
    group = seType field == Nothing


data ColumnMetadata = ColumnMetadata
  { cmType :: !ParquetType
  , cmEncodings :: !(Vector Encoding)
  , cmPathInSchema :: !(Vector Text)
  , cmCodec :: !Compression
  , cmNumValues :: !Int64
  , cmTotalUncompressedSize :: !Int64
  , cmTotalCompressedSize :: !Int64
  , cmDataPageOffset :: !Int64
  , cmDictionaryPageOffset :: !(Maybe Int64)
  {- ^ Byte offset of the (optional) dictionary page (parquet.thrift
  field 11). When present this is /before/ 'cmDataPageOffset' and
  callers reading raw column-chunk bytes must start their slice
  here, not at 'cmDataPageOffset'. Modern writers (pyarrow, polars,
  duckdb) always populate this when the column uses dictionary
  encoding.
  -}
  , cmStatistics :: !(Maybe Statistics)
  , cmBloomFilterOffset :: !(Maybe Int64)
  {- ^ Byte offset from beginning of file to the bloom filter for this
  column chunk, if a bloom filter is written. Field 14.
  -}
  , cmBloomFilterLength :: !(Maybe Int32)
  {- ^ Length of the bloom filter (header + bitset) in bytes. Field 15
  (added in parquet-format 2.10).
  -}
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data ColumnChunk = ColumnChunk
  { ccFilePath :: !(Maybe Text)
  , ccFileOffset :: !Int64
  , ccMetadata :: !(Maybe ColumnMetadata)
  , ccOffsetIndexOffset :: !(Maybe Int64)
  {- ^ Byte offset of this column chunk's 'OffsetIndex' (Thrift Compact),
  relative to the start of the file. Page-index spec, field 4.
  -}
  , ccOffsetIndexLength :: !(Maybe Int32)
  -- ^ Length in bytes of the serialized 'OffsetIndex'. Field 5.
  , ccColumnIndexOffset :: !(Maybe Int64)
  {- ^ Byte offset of this column chunk's 'ColumnIndex' (Thrift Compact),
  relative to the start of the file. Field 6.
  -}
  , ccColumnIndexLength :: !(Maybe Int32)
  -- ^ Length in bytes of the serialized 'ColumnIndex'. Field 7.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


{- | One entry of @RowGroup.sorting_columns@. Tells readers
(DuckDB, Trino, parquet-mr) which leaf columns the row
group is sorted on so they can skip ORDER BY scans.
-}
data SortingColumn = SortingColumn
  { scColumnIdx :: !Int32
  {- ^ Leaf-column index in the row group (0-based, parallel
  to @rgColumns@).
  -}
  , scDescending :: !Bool
  -- ^ Sort direction. @False@ = ascending.
  , scNullsFirst :: !Bool
  -- ^ Whether nulls sort before non-nulls.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data RowGroup = RowGroup
  { rgColumns :: !(Vector ColumnChunk)
  , rgTotalByteSize :: !Int64
  , rgNumRows :: !Int64
  , rgSortingColumns :: !(Maybe (Vector SortingColumn))
  {- ^ Per-row-group sort metadata. 'Nothing' for unsorted
  row groups (the common case); populated by writers that
  want to advertise an ordering. Readers use this to skip
  an ORDER BY when scanning a sorted file.
  -}
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


{- | Per-leaf-column ordering rule. parquet-format defines this
as a union describing natural, IEEE 754 total, or legacy INT96
timestamp ordering. Without it, statistics
on BYTE_ARRAY columns can be reported but readers may refuse
to use them for pushdown.
-}
data ColumnOrder
  = TypeDefinedOrder
  | IEEE754TotalOrder
  | Int96TimestampOrder
  | UnsupportedColumnOrder
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)


{- | Opaque application metadata. Order, duplicate keys, absent values and
empty values are retained. Decode values as UTF-8 only when interpreting
a particular metadata convention (such as a PROJJSON CRS definition).
-}
data KeyValue = KeyValue
  { kvKey :: !ByteString
  , kvValue :: !(Maybe ByteString)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data FileMetadata = FileMetadata
  { fmVersion :: !Int32
  , fmSchema :: !(Vector SchemaElement)
  , fmNumRows :: !Int64
  , fmRowGroups :: !(Vector RowGroup)
  , fmCreatedBy :: !(Maybe Text)
  , fmColumnOrders :: !(Maybe (Vector ColumnOrder))
  {- ^ Per-leaf-column ordering rules (parquet-format field 7).
  Length must match the leaf-column count when populated.
  'Nothing' means readers fall back to legacy ordering;
  modern writers always emit this.
  -}
  , fmKeyValueMetadata :: !(Maybe (Vector KeyValue))
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data Statistics = Statistics
  { statMin :: !(Maybe ByteString)
  , statMax :: !(Maybe ByteString)
  , statNullCount :: !(Maybe Int64)
  , statDistinctCount :: !(Maybe Int64)
  , statMinValue :: !(Maybe ByteString)
  , statMaxValue :: !(Maybe ByteString)
  , statNanCount :: !(Maybe Int64)
  -- ^ Missing means NaNs may be present, never an implicit zero.
  , statMinValueExact :: !(Maybe Bool)
  -- ^ Only @Just True@ proves the modern lower bound is an actual value.
  , statMaxValueExact :: !(Maybe Bool)
  -- ^ Only @Just True@ proves the modern upper bound is an actual value.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data PageLocation = PageLocation
  { plOffset :: !Int64
  , plCompressedPageSize :: !Int32
  , plFirstRowIndex :: !Int64
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data OffsetIndex = OffsetIndex
  { oiPageLocations :: !(Vector PageLocation)
  , oiUnencodedByteArrayDataBytes :: !(Maybe (Vector Int64))
  {- ^ Per-page unencoded byte counts for BYTE_ARRAY columns (parquet-format
  2.11+). Same length as 'oiPageLocations' when present; @Nothing@ for
  non-BYTE_ARRAY columns or older writers.
  -}
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


{- | Page-level boundary ordering for a column index's @min_values@ and
@max_values@. See @parquet.thrift@ enum @BoundaryOrder@.
-}
data BoundaryOrder
  = OrderUnordered
  | OrderAscending
  | OrderDescending
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)


boundaryOrderToInt :: BoundaryOrder -> Int32
boundaryOrderToInt = \case
  OrderUnordered -> 0
  OrderAscending -> 1
  OrderDescending -> 2


intToBoundaryOrder :: Int32 -> Maybe BoundaryOrder
intToBoundaryOrder = \case
  0 -> Just OrderUnordered
  1 -> Just OrderAscending
  2 -> Just OrderDescending
  _ -> Nothing


data ColumnIndex = ColumnIndex
  { ciNullPages :: !(Vector Bool)
  , ciMinValues :: !(Vector ByteString)
  , ciMaxValues :: !(Vector ByteString)
  , ciBoundaryOrder :: !BoundaryOrder
  , ciNullCounts :: !(Maybe (Vector Int64))
  , ciNanCounts :: !(Maybe (Vector Int64))
  -- ^ Per-page NaN counts. Missing counts do not imply zero.
  , ciRepetitionLevelHistograms :: !(Maybe (Vector Int64))
  {- ^ Per-page repetition level histograms (flattened: @max_rep + 1@ values
  per page). Optional, parquet-format 2.11+.
  -}
  , ciDefinitionLevelHistograms :: !(Maybe (Vector Int64))
  {- ^ Per-page definition level histograms (flattened: @max_def + 1@ values
  per page). Optional, parquet-format 2.11+.
  -}
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
