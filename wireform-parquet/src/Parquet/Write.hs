{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Write Parquet files.

Provides page-level encoding, column chunk assembly, and whole-file builders.
The user-facing entry point is 'buildParquetFile' (or
'buildParquetFileWithIndex' when emitting bloom filter / page index
regions). Both accept heterogeneous primitive columns via 'ColumnData'.

@
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector as V
import Parquet.Write
import Parquet.Types

let schema = V.fromList
      [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
      , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
      ]
    vals = VP.fromList [1, 2, 3 :: Int32]
    bs = buildParquetFile schema (V.singleton (V.singleton (ColInt32 vals)))
@
-}
module Parquet.Write (
  -- * Whole-file builders (preferred entry points)
  buildParquetFile,
  buildParquetFileWithIndex,
  buildParquetFileWithIndexIO,

  -- * Per-column auxiliary metadata for the indexed writer
  ColumnAux (..),
  buildParquetFileMixedWithAux,
  buildParquetFileMixedWithAuxUsing,
  emptyColumnAux,

  -- * Per-column encryption (modular AES-GCM / AES-GCM-CTR)
  ColumnEncryption (..),
  columnEncryptionFor,
  encryptPageBytes,
  encryptPageBytesV2,
  encryptAuxModule,

  -- * Encrypted-footer mode
  FooterEncryption (..),
  newFooterEncryption,
  buildParquetFileWithIndexEncryptedFooter,

  -- * Column data
  ColumnData (..),
  columnDataLength,
  columnDataParquetType,
  columnDataStatistics,
  encodeColumnDataPage,
  encodeColumnDataPageParts,

  -- * Nullable columns (definition levels)
  OptionalColumn (..),
  optionalColumnLength,
  optionalColumnNullCount,
  optionalColumnPresentValues,
  encodeOptionalColumnPage,

  -- * Dictionary encoding
  Dictionary (..),
  buildDictionary,
  encodeDictPage,
  encodeDictDataPage,

  -- * Data page version (V1 vs V2)
  PageVersion (..),
  encodeColumnDataPageV2,
  encodeColumnDataPageV2Parts,
  encodeOptionalColumnPageV2,
  encodeOptionalColumnPageV2Parts,

  -- * Page / footer building blocks (used by composite writers)
  writeParquetFile,
  encodePageHeader,
  assembleColumnChunk,
  ColumnAuxLayout (..),
  layoutColumnAuxiliaries,
  validateAuxIndexes,

  -- * Mixed required + optional columns (nullable-aware)
  ParquetColumn (..),
  parquetColumnLength,
  parquetColumnParquetType,
  buildParquetFileMixed,
  buildParquetFileMixedWith,

  -- * Size statistics
  columnDataUnencodedByteArrayBytes,
  buildOffsetIndexWithSizeStats,
) where

import Control.Monad.ST (runST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Crypto.Random.Types qualified as RNG
import Data.Bits (finiteBitSize, rotateL, setBit, shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int16, Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Primitive.ByteArray (copyByteArrayToAddr)
import Data.Primitive.Types (Prim)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Primitive qualified as VP
import Data.Vector.Primitive.Mutable qualified as MVP
import Data.Word (Word16, Word32, Word64, Word8, byteSwap16, byteSwap32, byteSwap64)
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (plusPtr, ptrToWordPtr, wordPtrToPtr)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Exts (Int (I#), Int#, inline, keepAlive#, readWord8OffAddr#, readWord8OffAddrAsWord16#, readWord8OffAddrAsWord32#, readWord8OffAddrAsWord64#, timesWord2#, word64ToWord#, wordToWord64#, xor#, (<=#), (==#), (>#), (>=#))
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import GHC.IO (IO (..))
import GHC.Ptr (Ptr (..))
import GHC.ST (ST (..))
import GHC.Word (Word16 (W16#), Word32 (W32#), Word64 (W64#), Word8 (W8#))
import Parquet.BloomFilter (Sbbf, encodeBloomFilter, sbbfNumBytes)
import Parquet.BloomFilter qualified as BF
import Parquet.Compress (compressPageBytes)
import Parquet.Compress qualified as Compress
import Parquet.Decimal (compareDecimalBytes, encodeDecimalInteger)
import Parquet.Encryption qualified as Enc
import Parquet.Footer (
  fileMetadataToThrift,
  parquetEncryptedMagic,
  parquetMagic,
  writeFooter,
  writeRawFooterEither,
 )
import Parquet.LevelsEncode qualified as LE
import Parquet.Page (
  DataPageHeader (..),
  DataPageHeaderV2 (..),
  DictionaryPageHeader (..),
  PageHeader (..),
  PageType (..),
  pageTypeTag,
  readPageHeaderAt,
 )
import Parquet.PageIndex (
  encodeColumnIndex,
  encodeOffsetIndex,
 )
import Parquet.Thrift.Schema
import Parquet.Types (
  BoundaryOrder (..),
  ColumnChunk (..),
  ColumnIndex (..),
  ColumnMetadata (..),
  ColumnOrder (..),
  Compression (..),
  ConvertedType (..),
  Encoding (..),
  FileMetadata (..),
  LogicalType (..),
  OffsetIndex (..),
  PageLocation (..),
  ParquetType (..),
  Repetition (..),
  RowGroup (..),
  SchemaElement (..),
  Statistics (..),
  hasCompatiblePhysicalType,
  hasDefinedStatisticsOrder,
 )
import Thrift.Decode qualified as Thrift
import Thrift.Encode (compactEncodedSizeBounded, encodeCompact, encodeCompactBounded)
import Thrift.Value qualified as TV
import Wireform.Builder qualified as B
import Wireform.Encode.Direct qualified as Direct
import Wireform.Hash qualified as Hash


{- | Assemble a complete Parquet file from pre-computed metadata and encoded
column chunk data. Each inner vector is one row group's column chunks
(already-encoded pages).
-}
writeParquetFile :: FileMetadata -> V.Vector (V.Vector ByteString) -> ByteString
writeParquetFile fm rowGroupData =
  BL.toStrict $
    B.toLazyByteString $
      B.byteString parquetMagic
        <> V.foldl' (\b rg -> V.foldl' (\b2 col -> b2 <> B.byteString col) b rg) mempty rowGroupData
        <> B.byteString (writeFooter fm)


writeFloatLE :: Float -> B.Builder
writeFloatLE = B.word32LE . castFloatToWord32


writeDoubleLE :: Double -> B.Builder
writeDoubleLE = B.word64LE . castDoubleToWord64


mkPlainDataPageHeader :: Int32 -> Int32 -> PageHeader
mkPlainDataPageHeader numValues bodySize =
  PageHeader
    { phType =
        PtDataPage
          DataPageHeader
            { dphNumValues = numValues
            , dphEncoding = 0
            }
    , phUncompressedPageSize = Just bodySize
    , phCompressedPageSize = Just bodySize
    }


-- | Thrift compact-encode a 'PageHeader'.
encodePageHeader :: PageHeader -> ByteString
encodePageHeader hdr = encodeCompact (pageHeaderToThrift hdr)


pageHeaderToThrift :: PageHeader -> TV.Value
pageHeaderToThrift hdr =
  TV.Struct $
    V.fromList $
      concat
        [ [PageHeader_Type (pageTypeTag (phType hdr))]
        , optField (phUncompressedPageSize hdr) PageHeader_UncompressedSize
        , optField (phCompressedPageSize hdr) PageHeader_CompressedSize
        , case phType hdr of
            PtDataPage dph ->
              [ PageHeader_DataPageHeader
                  (dataPageHeaderFields dph)
              ]
            PtDictionaryPage dk ->
              [ PageHeader_DictionaryPageHeader
                  (dictPageHeaderFields dk)
              ]
            PtDataPageV2 v2 ->
              [ PageHeader_DataPageHeaderV2
                  (dataPageHeaderV2Fields v2)
              ]
            PtIndexPage -> []
        ]


dataPageHeaderFields :: DataPageHeader -> V.Vector (Int16, TV.Value)
dataPageHeaderFields dph =
  V.fromList
    [ DataPageHeader_NumValues (dphNumValues dph)
    , DataPageHeader_Encoding (dphEncoding dph)
    , -- Both fields are /required/ in parquet.thrift's DataPageHeader.
      -- For required (max-def-level=0) columns the encoded level data
      -- is zero bytes, but the encoding still has to be declared or
      -- strict readers reject the page header. RLE (3) is what
      -- parquet-mr / arrow-cpp stamp here, so we mirror that.
      DataPageHeader_DefinitionLevelEncoding 3
    , DataPageHeader_RepetitionLevelEncoding 3
    ]


dictPageHeaderFields :: DictionaryPageHeader -> V.Vector (Int16, TV.Value)
dictPageHeaderFields dk =
  V.fromList
    [ DictionaryPageHeader_NumValues (dictNumValues dk)
    , DictionaryPageHeader_Encoding (dictEncoding dk)
    ]


dataPageHeaderV2Fields :: DataPageHeaderV2 -> V.Vector (Int16, TV.Value)
dataPageHeaderV2Fields v2 =
  V.fromList
    [ DataPageHeaderV2_NumValues (dph2NumValues v2)
    , DataPageHeaderV2_NumNulls (dph2NumNulls v2)
    , DataPageHeaderV2_NumRows (dph2NumRows v2)
    , DataPageHeaderV2_Encoding (dph2Encoding v2)
    , DataPageHeaderV2_DefinitionLevelsByteLength (dph2DefLevelsLen v2)
    , DataPageHeaderV2_RepetitionLevelsByteLength (dph2RepLevelsLen v2)
    , DataPageHeaderV2_IsCompressed (dph2IsCompressed v2)
    ]


{- | Concatenate pre-encoded pages into a single column chunk. Currently only
@Uncompressed@ is supported for writing; pages must already be encoded with
their headers.
-}
assembleColumnChunk :: Compression -> [ByteString] -> ByteString
assembleColumnChunk _codec pages = mconcat pages


-- ============================================================
-- DATA_PAGE_V2
-- ============================================================

{- | Encode an 'OptionalColumn' as a single @DATA_PAGE_V2@.

Layout (per the Parquet spec):

@
<repetition_levels>   -- length given by header.repetition_levels_byte_length
                      -- always uncompressed; we emit zero bytes for
                      -- non-repeated columns.
<definition_levels>   -- length given by header.definition_levels_byte_length
                      -- always uncompressed; max def-level is 1 for
                      -- optional columns.
<values>              -- compressed iff codec /= Uncompressed; PLAIN encoded.
@

The level segments are RLE-hybrid encoded *without* the 4-byte length
prefix that V1 uses. The header records both segment lengths and an
@is_compressed@ flag.
-}
encodeOptionalColumnPageV2 :: Compression -> OptionalColumn -> Either String ByteString
encodeOptionalColumnPageV2 codec oc = do
  parts <- encodeOptionalColumnPageV2Parts codec oc
  Right (concatV2Parts parts)


{- | Like 'encodeOptionalColumnPageV2' but returns the four byte
segments separately so callers (in particular the encrypted writer
path) can apply per-module ciphers. The tuple is
@(pageHeader, repLevels, defLevels, values)@.
-}
encodeOptionalColumnPageV2Parts
  :: Compression
  -> OptionalColumn
  -> Either String (ByteString, ByteString, ByteString, ByteString)
encodeOptionalColumnPageV2Parts codec oc = do
  page <- encodeOptionalV2Prepared codec oc (optionalColumnPresentValues oc)
  Right (eov2Header page, eov2Repetitions page, eov2Definitions page, eov2Values page)


data EncodedOptionalV2 = EncodedOptionalV2
  { eov2Header :: !ByteString
  , eov2Repetitions :: !ByteString
  , eov2Definitions :: !ByteString
  , eov2Values :: !ByteString
  , eov2UncompressedSize :: {-# UNPACK #-} !Int
  }


encodeOptionalV2Prepared :: Compression -> OptionalColumn -> ColumnData -> Either String EncodedOptionalV2
encodeOptionalV2Prepared codec oc present = do
  nVals <- pageInt32 "value count" (optionalColumnLength oc)
  valuePlan <- planPlainPayload present
  defPlan <- LE.planRLEHybrid 1 1 (optionalDefinitionLevels oc)
  uncompPageSize <- pageSizeWithOverhead (LE.hybridEncodedSize defPlan) (plainPayloadSize valuePlan)
  let !defStream = LE.encodeHybridPlan defPlan
      !defLen = LE.hybridEncodedSize defPlan
      !repStream = BS.empty
      !repLen = 0 :: Int
      !valuesRaw = encodePlannedPlainPayload valuePlan present
  (valuesBs, codecActual) <- case Compress.compressPageBytes codec valuesRaw of
    Right cb -> Right (cb, codec)
    Left e -> Left e
  compPageSize <- pageSizeWithOverhead defLen (BS.length valuesBs)
  let !nNulls = optionalColumnLength oc - columnDataLength present
      !nRows = nVals -- non-repeated => num_rows == num_values
      !hdrBytes =
        encodePageHeader
          PageHeader
            { phType =
                PtDataPageV2
                  DataPageHeaderV2
                    { dph2NumValues = nVals
                    , dph2NumNulls = fromIntegral nNulls
                    , dph2NumRows = nRows
                    , dph2Encoding = 0 -- PLAIN
                    , dph2DefLevelsLen = fromIntegral defLen
                    , dph2RepLevelsLen = fromIntegral repLen
                    , dph2IsCompressed = codecActual /= Uncompressed
                    }
            , phUncompressedPageSize = Just uncompPageSize
            , phCompressedPageSize = Just compPageSize
            }
  Right (EncodedOptionalV2 hdrBytes repStream defStream valuesBs (fromIntegral uncompPageSize))


-- | Concatenate a @V2 parts tuple@ in spec order.
concatV2Parts :: (ByteString, ByteString, ByteString, ByteString) -> ByteString
concatV2Parts (h, r, d, v) = BS.concat [h, r, d, v]


{- | Encode a required (non-null) 'ColumnData' as a single
@DATA_PAGE_V2@. No def/rep level streams are emitted; only the values
segment, compressed if the caller requested it.
-}
encodeColumnDataPageV2 :: Compression -> ColumnData -> Either String ByteString
encodeColumnDataPageV2 codec cd = do
  page <- encodeRequiredV2 codec cd
  Right (erv2Header page <> erv2Values page)


{- | Like 'encodeColumnDataPageV2' but returns the four parts
separately. For required columns the rep / def segments are empty
so this is just @(pageHeader, "", "", values)@.
-}
encodeColumnDataPageV2Parts
  :: Compression
  -> ColumnData
  -> Either String (ByteString, ByteString, ByteString, ByteString)
encodeColumnDataPageV2Parts codec cd = do
  page <- encodeRequiredV2 codec cd
  Right (erv2Header page, BS.empty, BS.empty, erv2Values page)


data EncodedRequiredV2 = EncodedRequiredV2
  { erv2Header :: !ByteString
  , erv2Values :: !ByteString
  , erv2UncompressedSize :: {-# UNPACK #-} !Int
  }


encodeRequiredV2 :: Compression -> ColumnData -> Either String EncodedRequiredV2
encodeRequiredV2 codec cd = do
  plan <- planPlainPayload cd
  let !valuesRaw = encodePlannedPlainPayload plan cd
      !uncompValuesLen = plainPayloadSize plan
  valuesBs <- Compress.compressPageBytes codec valuesRaw
  compPageSize <- pageInt32 "compressed page size" (BS.length valuesBs)
  let !nVals = plainValueCount plan
      !hdrBytes =
        encodePageHeader
          PageHeader
            { phType =
                PtDataPageV2
                  DataPageHeaderV2
                    { dph2NumValues = nVals
                    , dph2NumNulls = 0
                    , dph2NumRows = nVals
                    , dph2Encoding = 0
                    , dph2DefLevelsLen = 0
                    , dph2RepLevelsLen = 0
                    , dph2IsCompressed = codec /= Uncompressed
                    }
            , phUncompressedPageSize = Just (fromIntegral uncompValuesLen)
            , phCompressedPageSize = Just compPageSize
            }
  Right (EncodedRequiredV2 hdrBytes valuesBs uncompValuesLen)


-- ============================================================
-- Page / column statistics
-- ============================================================

{- | Compute Parquet 'Statistics' for an @INT32@ column.

Encodes @min_value@ / @max_value@ as little-endian @INT32@ per the
spec (PLAIN encoding for variable-length types is the same except
for byte arrays).  Both legacy @min@/@max@ and the modern
@min_value@/@max_value@ slots are populated.
-}
statisticsForInt32 :: VP.Vector Int32 -> Statistics
statisticsForInt32 vs
  | VP.null vs = emptyStats
  | otherwise = go 1 first first
  where
    !count = VP.length vs
    first = VP.unsafeIndex vs 0
    go !row !mn !mx
      | row == count = integralStatistics (i32LE mn) (i32LE mx)
      | otherwise =
          let !value = VP.unsafeIndex vs row
          in go (row + 1) (min mn value) (max mx value)


-- | Compute Parquet 'Statistics' for an @INT64@ column (LE i64 min/max).
statisticsForInt64 :: VP.Vector Int64 -> Statistics
statisticsForInt64 vs
  | VP.null vs = emptyStats
  | otherwise = go 1 first first
  where
    !count = VP.length vs
    first = VP.unsafeIndex vs 0
    go !row !mn !mx
      | row == count = integralStatistics (i64LE mn) (i64LE mx)
      | otherwise =
          let !value = VP.unsafeIndex vs row
          in go (row + 1) (min mn value) (max mx value)


integralStatistics :: ByteString -> ByteString -> Statistics
integralStatistics encMin encMax =
  Statistics
    { statMinValueExact = Just True
    , statMaxValueExact = Just True
    , statNanCount = Nothing
    , statMin = Just encMin
    , statMax = Just encMax
    , statNullCount = Just 0
    , statDistinctCount = Nothing
    , statMinValue = Just encMin
    , statMaxValue = Just encMax
    }


{- | Compute Parquet 'Statistics' for a @BYTE_ARRAY@ column. Values are
compared lexicographically (unsigned byte-by-byte).  The min/max
bytes are stored without their PLAIN length prefix per the spec.
-}
statisticsForByteArray :: V.Vector ByteString -> Statistics
statisticsForByteArray vs
  | V.null vs = emptyStats
  | BS.length first > 8 = general 1 first first
  | otherwise = runST $ withByteDictionarySource vs $ do
      key <- unsafeIOToST (shortStatisticsKeyIO first)
      short 1 0 0 key key (BS.length first) (BS.length first)
  where
    !count = V.length vs
    first = V.unsafeIndex vs 0
    general !row !mn !mx
      | row == count = byteStatistics mn mx
      | otherwise =
          let !value = V.unsafeIndex vs row
          in if value < mn
               then general (row + 1) value mx
               else general (row + 1) mn (if value > mx then value else mx)
    short !row !minRow !maxRow !minKey !maxKey !minLength !maxLength
      | row == count = do
          let !mn = V.unsafeIndex vs minRow
              !mx = V.unsafeIndex vs maxRow
          pure $! byteStatistics mn mx
      | otherwise = do
          let !value = V.unsafeIndex vs row
              !size = BS.length value
          if size > 8
            then pure $! general row (V.unsafeIndex vs minRow) (V.unsafeIndex vs maxRow)
            else do
              key <- unsafeIOToST (shortStatisticsKeyIO value)
              if key < minKey || (key == minKey && size < minLength)
                then short (row + 1) row maxRow key maxKey size maxLength
                else
                  if key > maxKey || (key == maxKey && size > maxLength)
                    then short (row + 1) minRow row minKey key minLength size
                    else short (row + 1) minRow maxRow minKey maxKey minLength maxLength


-- The caller proves length <=8 and keeps the entire source column alive.
-- Big-endian, zero-padded keys have unsigned byte order; equal keys need
-- the lengths as well to distinguish a prefix from trailing NUL bytes.
shortStatisticsKeyIO :: ByteString -> IO Word64
shortStatisticsKeyIO bytes = do
  let (fp, offset, size) = BSI.toForeignPtr bytes
      ptr = unsafeForeignPtrToPtr fp `plusPtr` offset
  key <- dictionaryReadShort ptr size
  pure (byteSwap64 key)
{-# INLINE shortStatisticsKeyIO #-}


byteStatistics :: ByteString -> ByteString -> Statistics
byteStatistics !mn !mx =
  Statistics
    { statMinValueExact = Just True
    , statMaxValueExact = Just True
    , statNanCount = Nothing
    , statMin = Nothing
    , statMax = Nothing
    , statNullCount = Just 0
    , statDistinctCount = Nothing
    , statMinValue = Just mn
    , statMaxValue = Just mx
    }


emptyStats :: Statistics
emptyStats = Statistics Nothing Nothing (Just 0) Nothing Nothing Nothing Nothing Nothing Nothing


-- Demand scalar bounds in all four numeric encoders. Otherwise GHC can
-- carry a boxed maximum through the statistics loop and allocate on updates.
i32LE :: Int32 -> ByteString
i32LE !v = BL.toStrict (B.toLazyByteString (B.int32LE v))


i64LE :: Int64 -> ByteString
i64LE !v = BL.toStrict (B.toLazyByteString (B.int64LE v))


{- | Float statistics use TYPE_ORDER: omit NaNs from bounds, count them,
and canonicalize zero bounds to -0/+0. This also handles all-NaN pages.
-}
statisticsForFloat :: VP.Vector Float -> Statistics
statisticsForFloat = floatingStatistics fLE


statisticsForDouble :: VP.Vector Double -> Statistics
statisticsForDouble = floatingStatistics dLE


floatingStatistics :: (VP.Prim a, RealFloat a) => (a -> ByteString) -> VP.Vector a -> Statistics
floatingStatistics encode values = seek 0 0
  where
    !count = VP.length values
    -- IEEE self-inequality tests NaN without a per-value C call. The
    -- specialized Core keeps the bounds, counter and offset unboxed.
    seek !i !nans
      | i == count = emptyStats {statNanCount = Just nans}
      | otherwise =
          let !value = VP.unsafeIndex values i
          in if value /= value
               then seek (i + 1) (nans + 1)
               else bounds (i + 1) nans value value
    bounds !i !nans !lo !hi
      | i == count =
          let !encodedMin = encode (if lo == 0 then negate 0 else lo)
              !encodedMax = encode (if hi == 0 then 0 else hi)
          in Statistics
               (Just encodedMin)
               (Just encodedMax)
               (Just 0)
               Nothing
               (Just encodedMin)
               (Just encodedMax)
               (Just nans)
               (Just True)
               (Just True)
      | otherwise =
          let !value = VP.unsafeIndex values i
          in if value /= value
               then bounds (i + 1) (nans + 1) lo hi
               else bounds (i + 1) nans (min lo value) (max hi value)
{-# INLINE floatingStatistics #-}


{- | Compute Parquet 'Statistics' for a @BOOLEAN@ column. Min is the
first @False@ that appears (otherwise @True@); max is the first @True@.
Encoded as a single byte (0 or 1) per the PLAIN spec.
-}
statisticsForBool :: V.Vector Bool -> Statistics
statisticsForBool vs
  | V.null vs = emptyStats
  | otherwise =
      let hasFalse = V.any not vs
          hasTrue = V.any id vs
          encMin = if hasFalse then BS.singleton 0 else BS.singleton 1
          encMax = if hasTrue then BS.singleton 1 else BS.singleton 0
      in Statistics
           (Just encMin)
           (Just encMax)
           (Just 0)
           Nothing
           (Just encMin)
           (Just encMax)
           Nothing
           (Just True)
           (Just True)


fLE :: Float -> ByteString
fLE !v = BL.toStrict (B.toLazyByteString (B.word32LE (castFloatToWord32 v)))


dLE :: Double -> ByteString
dLE !v = BL.toStrict (B.toLazyByteString (B.word64LE (castDoubleToWord64 v)))


-- ============================================================
-- Heterogeneous column data
-- ============================================================

{- | A single column's worth of values, tagged with its physical type.
Used by 'buildParquetFileTyped' / 'buildParquetFileTypedWithIndex' so
writers can emit any primitive type from one entry point.
-}
data ColumnData
  = ColInt32 !(VP.Vector Int32)
  | ColInt64 !(VP.Vector Int64)
  | ColFloat !(VP.Vector Float)
  | ColDouble !(VP.Vector Double)
  | ColBool !(V.Vector Bool)
  | ColByteArray !(V.Vector ByteString)
  deriving (Show, Eq)


-- | Number of values in the column.
columnDataLength :: ColumnData -> Int
columnDataLength = \case
  ColInt32 v -> VP.length v
  ColInt64 v -> VP.length v
  ColFloat v -> VP.length v
  ColDouble v -> VP.length v
  ColBool v -> V.length v
  ColByteArray v -> V.length v


-- | The Parquet physical type the column data should be written as.
columnDataParquetType :: ColumnData -> ParquetType
columnDataParquetType = \case
  ColInt32 {} -> PTInt32
  ColInt64 {} -> PTInt64
  ColFloat {} -> PTFloat
  ColDouble {} -> PTDouble
  ColBool {} -> PTBoolean
  ColByteArray {} -> PTByteArray


-- | Encode the column as a single uncompressed @PLAIN@ @DATA_PAGE@.
encodeColumnDataPage :: ColumnData -> ByteString
encodeColumnDataPage cd =
  let (h, b) = encodeColumnDataPageParts cd in BS.append h b


{- | Encode a column data page as @(headerBytes, bodyBytes)@. Same
contents as 'encodeColumnDataPage' but split so callers (in
particular 'encryptPageBytes') can encrypt the two parts under
different module-type AADs.
-}
encodeColumnDataPageParts :: ColumnData -> (ByteString, ByteString)
encodeColumnDataPageParts = either error id . encodeColumnDataPagePartsChecked


encodeColumnDataPagePartsChecked :: ColumnData -> Either String (ByteString, ByteString)
encodeColumnDataPagePartsChecked cd = do
  plan <- planPlainPayload cd
  let !body = encodePlannedPlainPayload plan cd
      !hdr = encodePageHeader (mkPlainDataPageHeader (plainValueCount plan) (fromIntegral (plainPayloadSize plan)))
  Right (hdr, body)


-- | Compute Parquet 'Statistics' for the column.
columnDataStatistics :: ColumnData -> Statistics
columnDataStatistics = \case
  ColInt32 v -> statisticsForInt32 v
  ColInt64 v -> statisticsForInt64 v
  ColFloat v -> statisticsForFloat v
  ColDouble v -> statisticsForDouble v
  ColBool v -> statisticsForBool v
  ColByteArray v -> statisticsForByteArray v


-- Unsigned logical integers share signed physical storage. Comparing that
-- storage as Int32/Int64 gives false bounds at the signed boundary, allowing
-- an external engine to incorrectly skip matching row groups.
columnStatisticsForSchema :: SchemaElement -> ColumnData -> Statistics
columnStatisticsForSchema leaf col
  | not (hasDefinedStatisticsOrder leaf) = emptyStats
  | isDecimal
  , ColByteArray values <- col =
      if V.null values
        then emptyStats
        else
          let low = V.foldl1' (\a b -> if compareDecimalBytes a b == GT then b else a) values
              high = V.foldl1' (\a b -> if compareDecimalBytes a b == LT then b else a) values
          in Statistics Nothing Nothing (Just 0) Nothing (Just low) (Just high) Nothing (Just True) (Just True)
  | isUnsigned = case col of
      ColInt32 xs
        | not (VP.null xs) ->
            unsignedStats (i32LE (VP.foldl1' min32 xs)) (i32LE (VP.foldl1' max32 xs))
      ColInt64 xs
        | not (VP.null xs) ->
            unsignedStats (i64LE (VP.foldl1' min64 xs)) (i64LE (VP.foldl1' max64 xs))
      _ -> columnDataStatistics col
  | otherwise = columnDataStatistics col
  where
    isDecimal = case seLogicalType leaf of
      Just LTDecimal {} -> True
      _ -> seConvertedType leaf == Just CTDecimal
    isUnsigned = case seLogicalType leaf of
      Just (LTInteger _ signed) -> not signed
      _ -> maybe False (`elem` [CTUInt8, CTUInt16, CTUInt32, CTUInt64]) (seConvertedType leaf)
    min32 a b = if (fromIntegral a :: Word32) <= fromIntegral b then a else b
    max32 a b = if (fromIntegral a :: Word32) >= fromIntegral b then a else b
    min64 a b = if (fromIntegral a :: Word64) <= fromIntegral b then a else b
    max64 a b = if (fromIntegral a :: Word64) >= fromIntegral b then a else b
    -- The deprecated bounds use signed ordering and must stay absent.
    unsignedStats lo hi = Statistics Nothing Nothing (Just 0) Nothing (Just lo) (Just hi) Nothing (Just True) (Just True)


validateLogicalColumn :: SchemaElement -> ColumnData -> Either String ()
validateLogicalColumn leaf column
  | not (hasCompatiblePhysicalType leaf) = Left "Parquet.Write: logical annotation has incompatible physical storage"
  | otherwise = case seLogicalType leaf of
      Just LTNull
        | columnDataLength column == 0 -> Right ()
        | otherwise -> Left "Parquet.Write: NULL logical type requires every value to be null"
      Just (LTDecimal precision scale)
        | precision < 1 || scale < 0 || scale > precision -> Left "Parquet.Write: invalid DECIMAL precision/scale"
        | otherwise -> case column of
            ColInt32 values
              | precision <= 9 ->
                  let !bound = fromInteger (10 ^ precision - 1) :: Int64
                  in require
                       ( VP.all
                           ( \value ->
                               let n = fromIntegral value :: Int64
                               in n >= negate bound && n <= bound
                           )
                           values
                       )
            ColInt64 values
              | precision <= 18 ->
                  let !bound = fromInteger (10 ^ precision - 1) :: Int64
                  in require (VP.all (\value -> value >= negate bound && value <= bound) values)
            ColByteArray values ->
              let width = V.foldl' (\n value -> max n (BS.length value)) 0 values
                  -- At most 3 decimal digits per stored byte; avoid constructing
                  -- an enormous power for a permissive precision on tiny values.
                  boundNeeded = toInteger precision < 3 * toInteger width
                  high = encodeDecimalInteger (10 ^ precision - 1)
                  low = encodeDecimalInteger (negate (10 ^ precision - 1))
                  valid value =
                    not (BS.null value)
                      && ( not boundNeeded
                             || (compareDecimalBytes low value /= GT && compareDecimalBytes value high /= GT)
                         )
              in require (V.all valid values)
            _ -> Left "Parquet.Write: invalid physical storage or precision for DECIMAL"
      _
        | seConvertedType leaf == Just CTDecimal -> Left "Parquet.Write: DECIMAL requires precision/scale in LogicalType"
        | otherwise -> Right ()
  where
    require True = Right ()
    require False = Left "Parquet.Write: DECIMAL value exceeds precision or has empty storage"


-- ============================================================
-- Nullable columns: definition levels + PLAIN values
-- ============================================================

{- | A column with optional values. Internally we store the present-flag
vector alongside the present-only values; readers reconstruct the
@Maybe@ shape via 'Parquet.Levels.materializePlainInt32Optional' and
friends.
-}
data OptionalColumn
  = OptInt32 !(V.Vector (Maybe Int32))
  | OptInt64 !(V.Vector (Maybe Int64))
  | OptFloat !(V.Vector (Maybe Float))
  | OptDouble !(V.Vector (Maybe Double))
  | OptBool !(V.Vector (Maybe Bool))
  | OptByteArray !(V.Vector (Maybe ByteString))
  deriving (Show, Eq)


optionalColumnLength :: OptionalColumn -> Int
optionalColumnLength = \case
  OptInt32 v -> V.length v
  OptInt64 v -> V.length v
  OptFloat v -> V.length v
  OptDouble v -> V.length v
  OptBool v -> V.length v
  OptByteArray v -> V.length v


optionalColumnNullCount :: OptionalColumn -> Int
optionalColumnNullCount = \case
  OptInt32 v -> count v
  OptInt64 v -> count v
  OptFloat v -> count v
  OptDouble v -> count v
  OptBool v -> count v
  OptByteArray v -> count v
  where
    count = V.foldl' (\n -> maybe (n + 1) (const n)) 0


-- | Strip nulls and return only the present values as a 'ColumnData'.
optionalColumnPresentValues :: OptionalColumn -> ColumnData
optionalColumnPresentValues = \case
  OptInt32 v -> ColInt32 (V.convert (V.mapMaybe id v))
  OptInt64 v -> ColInt64 (V.convert (V.mapMaybe id v))
  OptFloat v -> ColFloat (V.convert (V.mapMaybe id v))
  OptDouble v -> ColDouble (V.convert (V.mapMaybe id v))
  OptBool v -> ColBool (V.mapMaybe id v)
  OptByteArray v -> ColByteArray (V.mapMaybe id v)


optionalDefinitionLevels :: OptionalColumn -> VP.Vector Int32
optionalDefinitionLevels = \case
  OptInt32 v -> levels v
  OptInt64 v -> levels v
  OptFloat v -> levels v
  OptDouble v -> levels v
  OptBool v -> levels v
  OptByteArray v -> levels v
  where
    levels values =
      VP.generate
        (V.length values)
        (\i -> maybe 0 (const 1) (V.unsafeIndex values i))


{- | Encode an 'OptionalColumn' as a single uncompressed @PLAIN@
@DATA_PAGE@ V1 carrying the definition-level stream + present-only
PLAIN values.
-}
encodeOptionalColumnPage :: OptionalColumn -> ByteString
encodeOptionalColumnPage = either error id . encodeOptionalColumnPageChecked


encodeOptionalColumnPageChecked :: OptionalColumn -> Either String ByteString
encodeOptionalColumnPageChecked oc = encodeOptionalPagePrepared oc (optionalColumnPresentValues oc)


encodeOptionalPagePrepared :: OptionalColumn -> ColumnData -> Either String ByteString
encodeOptionalPagePrepared oc present = do
  n <- pageInt32 "value count" (optionalColumnLength oc)
  valuePlan <- planPlainPayload present
  defPlan <- LE.planRLEHybrid 1 1 (optionalDefinitionLevels oc)
  -- Include the V1 level-length prefix before allocating either segment.
  bodySize <- pageSizeWithOverhead (LE.hybridEncodedSize defPlan + 4) (plainPayloadSize valuePlan)
  let !defs = LE.encodeHybridPlan defPlan
      !prefix = Direct.directEncode 4 (\ptr offset -> let size = fromIntegral (LE.hybridEncodedSize defPlan) in Direct.dWord32LE ptr offset (case targetByteOrder of LittleEndian -> size; BigEndian -> byteSwap32 size))
      !valuesBs = encodePlannedPlainPayload valuePlan present
      !hdr = mkPlainDataPageHeader n bodySize
  Right (BS.concat [encodePageHeader hdr, prefix, defs, valuesBs])


{- | Encode just the PLAIN-values portion (no page header). Used inside
'encodeOptionalColumnPage' so we can prepend the definition-level
stream and write a single page header.
-}
encodeColumnDataPagePayload :: ColumnData -> ByteString
encodeColumnDataPagePayload cd = either error (\plan -> encodePlannedPlainPayload plan cd) (planPlainPayload cd)


-- Counts and page sizes are signed i32 fields in parquet.thrift. Validate
-- their complete PLAIN payload before allocating or narrowing anything.
data PlainPayloadPlan = PlainPayloadPlan
  { plainValueCount :: {-# UNPACK #-} !Int32
  , plainPayloadSize :: {-# UNPACK #-} !Int
  }


pageInt32 :: String -> Int -> Either String Int32
pageInt32 label value
  | value < 0 || toInteger value > toInteger (maxBound :: Int32) =
      Left ("Parquet.Write: " ++ label ++ " exceeds signed 32-bit limit")
  | otherwise = Right (fromIntegral value)


pageSizeWithOverhead :: Int -> Int -> Either String Int32
pageSizeWithOverhead overhead payload
  | overhead < 0 || payload < 0 || toInteger overhead + toInteger payload > toInteger (maxBound :: Int32) =
      Left "Parquet.Write: page payload exceeds signed 32-bit limit"
  | otherwise = Right (fromIntegral (overhead + payload))


planPlainPayload :: ColumnData -> Either String PlainPayloadPlan
planPlainPayload cd = do
  count <- pageInt32 "value count" (columnDataLength cd)
  size <- case cd of
    ColInt32 v -> fixed 4 (VP.length v)
    ColInt64 v -> fixed 8 (VP.length v)
    ColFloat v -> fixed 4 (VP.length v)
    ColDouble v -> fixed 8 (VP.length v)
    ColBool v ->
      let (bytes, tailBits) = quotRem (V.length v) 8
      in Right (bytes + if tailBits == 0 then 0 else 1)
    ColByteArray v
      | V.length v > limit `quot` 4 -> tooLarge
      | otherwise ->
          let go !row !total
                | row == V.length v = Right total
                | total > limit - 4 = tooLarge
                | otherwise =
                    let !size = BS.length (V.unsafeIndex v row)
                    in if size > limit - total - 4
                         then tooLarge
                         else go (row + 1) (total + 4 + size)
          in go 0 0
  Right (PlainPayloadPlan count size)
  where
    limit = fromIntegral (maxBound :: Int32)
    tooLarge = Left "Parquet.Write: PLAIN page payload exceeds signed 32-bit limit"
    fixed width count
      | count > limit `quot` width = tooLarge
      | otherwise = Right (count * width)


encodePlannedPlainPayload :: PlainPayloadPlan -> ColumnData -> ByteString
encodePlannedPlainPayload plan = \case
  ColInt32 v ->
    fixedPayload
      (plainPayloadSize plan)
      4
      ( \xs ->
          BL.toStrict $
            B.toLazyByteString $
              VP.foldl' (\b x -> b <> B.int32LE x) mempty xs
      )
      v
  ColInt64 v ->
    fixedPayload
      (plainPayloadSize plan)
      8
      ( \xs ->
          BL.toStrict $
            B.toLazyByteString $
              VP.foldl' (\b x -> b <> B.int64LE x) mempty xs
      )
      v
  ColFloat v ->
    fixedPayload
      (plainPayloadSize plan)
      4
      ( \xs ->
          BL.toStrict $
            B.toLazyByteString $
              VP.foldl' (\b x -> b <> writeFloatLE x) mempty xs
      )
      v
  ColDouble v ->
    fixedPayload
      (plainPayloadSize plan)
      8
      ( \xs ->
          BL.toStrict $
            B.toLazyByteString $
              VP.foldl' (\b x -> b <> writeDoubleLE x) mempty xs
      )
      v
  ColBool v ->
    let !n = V.length v
        !numBytes = plainPayloadSize plan
    in Direct.directEncode numBytes $ \ptr ->
         let go !byteIdx !offset
               | byteIdx == numBytes = pure offset
               | otherwise = do
                   let goBit !bit !acc
                         | bit == 8 || byteIdx * 8 + bit == n = acc
                         | otherwise =
                             goBit
                               (bit + 1)
                               (if V.unsafeIndex v (byteIdx * 8 + bit) then setBit acc bit else acc)
                   next <- Direct.dWord8 ptr offset (goBit 0 0)
                   go (byteIdx + 1) next
         in go 0
  ColByteArray v | targetByteOrder == LittleEndian ->
    Direct.directEncode (plainPayloadSize plan) $ \ptr ->
      let go !i !offset
            | i == V.length v = pure offset
            | otherwise = do
                let !bytes = V.unsafeIndex v i
                payload <- Direct.dWord32LE ptr offset (fromIntegral (BS.length bytes))
                next <- Direct.dBytes ptr payload bytes
                go (i + 1) next
      in go 0
  ColByteArray v ->
    BL.toStrict $
      B.toLazyByteString $
        V.foldl'
          ( \b x ->
              b
                <> B.word32LE (fromIntegral (BS.length x))
                <> B.byteString x
          )
          mempty
          v


-- On little-endian hosts the primitive vector is already PLAIN-encoded.
-- Copy its logical slice once; retain the portable builder on big-endian
-- hosts rather than writing native-endian bytes into a little-endian file.
fixedPayload :: Int -> Int -> (VP.Vector a -> ByteString) -> VP.Vector a -> ByteString
fixedPayload size width fallback values@(VP.Vector offset _ bytes)
  | targetByteOrder /= LittleEndian = fallback values
  | otherwise = Direct.directEncode size $ \ptr _ -> do
      copyByteArrayToAddr ptr bytes (offset * width) size
      pure size
{-# INLINE fixedPayload #-}


-- ============================================================
-- Dictionary encoding (PLAIN_DICTIONARY / RLE_DICTIONARY)
-- ============================================================

{- | A computed dictionary for a single column chunk. 'dictUniques' holds
the column values in dictionary order; 'dictIndices' maps each row's
value to its dictionary index (0-based).
-}
data Dictionary = Dictionary
  { dictUniques :: !ColumnData
  , dictIndices :: !(VP.Vector Int32)
  }
  deriving (Show, Eq)


{- | Compute a dictionary for a 'ColumnData' by deduplicating the input.
Order of unique values follows their first appearance.
-}
buildDictionary :: ColumnData -> Dictionary
buildDictionary = \case
  ColInt32 values -> buildScalarDictionary ColInt32 fromIntegral values
  ColInt64 values -> buildScalarDictionary ColInt64 fromIntegral values
  -- Preserve IEEE wire bits, including signed zeros and NaN payloads.
  ColFloat values -> buildScalarDictionary (ColFloat . VP.unsafeCast) fromIntegral (VP.unsafeCast values :: VP.Vector Word32)
  ColDouble values -> buildScalarDictionary (ColDouble . VP.unsafeCast) id (VP.unsafeCast values :: VP.Vector Word64)
  ColBool values ->
    buildDictionaryFromIndices
      True
      (V.length values)
      (\i -> pure (if V.unsafeIndex values i then 1 else 0))
      (\i j -> V.unsafeIndex values i == V.unsafeIndex values j)
      (ColBool . gatherDictionaryValues values)
  ColByteArray values -> buildByteDictionary values


-- Exact scalar keys double as the returned dictionary. Freezing the native
-- values avoids a representative-row array and a second gather/copy.
buildScalarDictionary :: (Prim a, Eq a) => (VP.Vector a -> ColumnData) -> (a -> Word64) -> VP.Vector a -> Dictionary
buildScalarDictionary makeColumn toHash values
  | count == 0 = Dictionary (makeColumn VP.empty) VP.empty
  | count > maxBound `quot` 4 = error "Parquet.Write: dictionary indices exceed addressable size"
  | otherwise = runST $ do
      indices <- MVP.unsafeNew count
      keys <- MVP.unsafeNew (min count 32)
      table <- MVP.replicate 64 (-1 :: Int32)
      let go !row !distinct !uniqueKeys !slots
            | row == count = do
                uniques <- VP.unsafeFreeze (MVP.unsafeSlice 0 distinct uniqueKeys)
                rowIndices <- VP.unsafeFreeze indices
                pure (Dictionary (makeColumn uniques) rowIndices)
            | otherwise =
                let !value = VP.unsafeIndex values row
                in probe value (fromIntegral (dictionaryHashWord64 (toHash value)) .&. mask)
            where
              !mask = MVP.length slots - 1
              probe !value !bucket = do
                entry <- MVP.unsafeRead slots bucket
                if entry < 0
                  then case dictionaryCapacityReached distinct (MVP.length uniqueKeys) of
                    1# -> grow
                    _ -> do
                      case dictionaryEntriesExceeded distinct of
                        1# -> error "Parquet.Write: too many dictionary entries"
                        _ -> pure ()
                      let !entryId = fromIntegral distinct
                      MVP.unsafeWrite uniqueKeys distinct value
                      MVP.unsafeWrite slots bucket entryId
                      MVP.unsafeWrite indices row entryId
                      go (row + 1) (distinct + 1) uniqueKeys slots
                  else do
                    cached <- MVP.unsafeRead uniqueKeys (fromIntegral entry)
                    if cached == value
                      then do
                        MVP.unsafeWrite indices row entry
                        go (row + 1) distinct uniqueKeys slots
                      else probe value ((bucket + 1) .&. mask)
              grow
                | 1# <- dictionaryTableSizeExceeded (MVP.length slots) = error "Parquet.Write: dictionary exceeds addressable size"
                | otherwise = do
                    let !capacity = MVP.length uniqueKeys
                        !extra = min capacity (count - capacity)
                    expandedKeys <- MVP.unsafeGrow uniqueKeys extra
                    expandedTable <- MVP.replicate (MVP.length slots * 2) (-1 :: Int32)
                    let !newMask = MVP.length expandedTable - 1
                        rehash !entry
                          | entry == distinct = go row distinct expandedKeys expandedTable
                          | otherwise = do
                              key <- MVP.unsafeRead expandedKeys entry
                              let insert !bucket = do
                                    old <- MVP.unsafeRead expandedTable bucket
                                    if old < 0
                                      then MVP.unsafeWrite expandedTable bucket (fromIntegral entry) >> rehash (entry + 1)
                                      else insert ((bucket + 1) .&. newMask)
                              insert (fromIntegral (dictionaryHashWord64 (toHash key)) .&. newMask)
                    rehash 0
      go 0 0 keys table
  where
    !count = VP.length values
{-# INLINE buildScalarDictionary #-}


data ByteKeyShape = GeneralByteKeys | TaggedByteKeys | EightByteKeys | MixedShortByteKeys


-- A small-key column can use exact primitive keys. The scan examines only
-- lengths and exits at the first larger value, so normal binary columns
-- incur constant dispatch cost. Tags make all zero-padded <=7-byte values
-- injective; an all-eight-byte column needs no length tag or equality read.
classifyByteKeys :: V.Vector ByteString -> ByteKeyShape
classifyByteKeys values = go 0 8 0
  where
    go !i !minimumSize !maximumSize
      | i == V.length values =
          if maximumSize < 8 then TaggedByteKeys else if minimumSize == 8 then EightByteKeys else MixedShortByteKeys
      | otherwise =
          let !size = BS.length (V.unsafeIndex values i)
          in if size > 8 then GeneralByteKeys else go (i + 1) (min minimumSize size) (max maximumSize size)
{-# INLINE classifyByteKeys #-}


buildByteDictionary :: V.Vector ByteString -> Dictionary
buildByteDictionary values
  | V.length values > maxBound `quot` 4 = error "Parquet.Write: dictionary indices exceed addressable size"
  | otherwise = case classifyByteKeys values of
      TaggedByteKeys -> buildSmallByteDictionary True True values
      EightByteKeys -> buildSmallByteDictionary True False values
      MixedShortByteKeys -> buildSmallByteDictionary False False values
      GeneralByteKeys ->
        if preferPointerByteDictionary values
          then internByteDictionary values
          else case internLargeByteDictionary values of
            DictionaryBuild rows indices -> Dictionary (ColByteArray (gatherDictionaryValues values rows)) indices


-- This is a bounded performance hint, never a size invariant: either
-- scanner handles every byte-string length, including later wider values.
-- Avoid an extra full-column pass just to choose the scratch layout.
preferPointerByteDictionary :: V.Vector ByteString -> Bool
preferPointerByteDictionary values = go 0
  where
    !limit = min 32 (V.length values)
    go !row
      | row == limit = True
      | BS.length (V.unsafeIndex values row) > 128 = False
      | otherwise = go (row + 1)
{-# INLINE preferPointerByteDictionary #-}


buildSmallByteDictionary :: Bool -> Bool -> V.Vector ByteString -> Dictionary
buildSmallByteDictionary exactKeys tagged values =
  buildDictionaryFromIndices
    exactKeys
    (V.length values)
    (\i -> unsafeIOToST (dictionarySmallKeyIO tagged (V.unsafeIndex values i)))
    (\i j -> BS.length (V.unsafeIndex values i) == BS.length (V.unsafeIndex values j))
    (ColByteArray . gatherDictionaryValues values)
{-# INLINE buildSmallByteDictionary #-}


-- Called only after the length scan proved that every input is <=8 bytes.
dictionarySmallKeyIO :: Bool -> ByteString -> IO Word64
dictionarySmallKeyIO tagged bytes
  | BS.null bytes = pure 0
  | otherwise = do
      let (fp, offset, size) = BSI.toForeignPtr bytes
          ptr = unsafeForeignPtrToPtr fp `plusPtr` offset
      key <- dictionaryReadShort ptr size
      touchForeignPtr fp
      pure (if tagged then key .|. (fromIntegral size `shiftL` 56) else key)
{-# INLINE dictionarySmallKeyIO #-}


-- Force each selected reference before storing it. A lazy boxed generate
-- would retain indexing closures over the entire input, including rows
-- that the dictionary no longer needs, and defer construction work.
gatherDictionaryValues :: V.Vector a -> VP.Vector Int -> V.Vector a
gatherDictionaryValues values rows = V.create $ do
  output <- MV.unsafeNew (VP.length rows)
  let go !i
        | i == VP.length rows = pure output
        | otherwise = do
            let !value = V.unsafeIndex values (VP.unsafeIndex rows i)
            MV.unsafeWrite output i value
            go (i + 1)
  go 0
{-# INLINE gatherDictionaryValues #-}


-- The table stores dense dictionary ids, whose representative source rows
-- live in a growable primitive vector. This avoids boxing/hash-map nodes
-- on every row and never duplicates byte-array payloads. The scratch buffers grow
-- only when the number of distinct values grows; the table stays <= 50%
-- full, including during rehashing. Inlining specialises the key,
-- equality and collision callbacks.
data DictionaryBuild = DictionaryBuild
  { dictionarySourceRows :: !(VP.Vector Int)
  , dictionaryRowIndices :: !(VP.Vector Int32)
  }


-- Scalar keys contain every input bit, so they need no second equality
-- check. Variable-length byte keys additionally check length/content.
buildDictionaryFromIndices :: Bool -> Int -> (forall s. Int -> ST s Word64) -> (Int -> Int -> Bool) -> (VP.Vector Int -> ColumnData) -> Dictionary
buildDictionaryFromIndices exactKeys count keyAt same makeColumn =
  case internDictionary exactKeys count keyAt same of
    DictionaryBuild rows indices -> Dictionary (makeColumn rows) indices
{-# INLINE buildDictionaryFromIndices #-}


internDictionary :: Bool -> Int -> (forall s. Int -> ST s Word64) -> (Int -> Int -> Bool) -> DictionaryBuild
internDictionary _ 0 _ _ = DictionaryBuild VP.empty VP.empty
internDictionary exactKeys count keyAt same
  | count > maxBound `quot` 4 = error "Parquet.Write: dictionary indices exceed addressable size"
  | otherwise = runST $ do
      indices <- MVP.unsafeNew count
      sourceRows <- MVP.unsafeNew (min count 32)
      cachedKeys <- MVP.unsafeNew (min count 32)
      slots <- MVP.replicate 64 (-1 :: Int32)
      scanDictionary exactKeys count keyAt same continueDictionaryCollision indices 0 0 sourceRows cachedKeys slots
{-# INLINE internDictionary #-}


-- Ambiguous sampled keys transfer the existing buffers to a refined scan,
-- then to full keys if necessary. Full-key/scalar scans inline an identity handler,
-- so their row loops carry no adaptive-mode argument or branch.
-- Arguments are the current row, representative row, unique count,
-- source-row array, cached keys, slots, and ordinary probe continuation.
type DictionaryCollision s = Int -> Int -> Int -> MVP.MVector s Int -> MVP.MVector s Word64 -> MVP.MVector s Int32 -> ST s DictionaryBuild -> ST s DictionaryBuild


continueDictionaryCollision :: DictionaryCollision s
continueDictionaryCollision _ _ _ _ _ _ continue = continue
{-# INLINE continueDictionaryCollision #-}


-- Large payloads retain the compact source-row representation; its lower
-- allocation and lookup cost wins when byte comparison dominates.
type LargeDictionaryScan s = Int -> Int -> MVP.MVector s Int -> MVP.MVector s Word64 -> MVP.MVector s Int32 -> ST s DictionaryBuild


sameLargeByteRows :: V.Vector ByteString -> Int -> Int -> Bool
sameLargeByteRows values i j =
  let !a = V.unsafeIndex values i
      !b = V.unsafeIndex values j
  in case dictionaryShortSize (BS.length a) of
       1# -> BS.length a == BS.length b
       _ -> a == b
{-# INLINE sameLargeByteRows #-}


internLargeByteDictionary :: V.Vector ByteString -> DictionaryBuild
internLargeByteDictionary values = runST $ do
  let count = V.length values
      sampledKey i = unsafeIOToST (dictionaryByteKeyIO False (V.unsafeIndex values i))
  indices <- MVP.unsafeNew count
  sourceRows <- MVP.unsafeNew (min count 32)
  cachedKeys <- MVP.unsafeNew (min count 32)
  slots <- MVP.replicate 64 (-1 :: Int32)
  scanDictionary False count sampledKey (sameLargeByteRows values) (promoteRefinedLargeDictionary values indices) indices 0 0 sourceRows cachedKeys slots
{-# INLINE internLargeByteDictionary #-}


scanFullLargeByteDictionary :: V.Vector ByteString -> MVP.MVector s Int32 -> LargeDictionaryScan s
scanFullLargeByteDictionary values indices =
  scanDictionary
    False
    (V.length values)
    (\i -> unsafeIOToST (dictionaryByteKeyIO True (V.unsafeIndex values i)))
    (sameLargeByteRows values)
    continueDictionaryCollision
    indices
{-# NOINLINE scanFullLargeByteDictionary #-}


scanRefinedLargeByteDictionary :: Int -> V.Vector ByteString -> MVP.MVector s Int32 -> LargeDictionaryScan s
scanRefinedLargeByteDictionary !pivot values indices =
  scanDictionary
    False
    (V.length values)
    (\i -> unsafeIOToST (dictionaryRefinedKeyIO pivot (V.unsafeIndex values i)))
    (sameLargeByteRows values)
    (promoteFullLargeDictionary values indices)
    indices
{-# NOINLINE scanRefinedLargeByteDictionary #-}


promoteRefinedLargeDictionary :: V.Vector ByteString -> MVP.MVector s Int32 -> DictionaryCollision s
promoteRefinedLargeDictionary values indices row representative distinct rows keys table _ = do
  pivot <- unsafeIOToST (dictionaryDifferenceOffsetIO (V.unsafeIndex values row) (V.unsafeIndex values representative))
  if pivot < 0
    then do
      rekeyLargeDictionary distinct rows keys table (\i -> unsafeIOToST (dictionaryByteKeyIO True (V.unsafeIndex values i)))
      scanFullLargeByteDictionary values indices row distinct rows keys table
    else do
      rekeyLargeDictionary distinct rows keys table (\i -> unsafeIOToST (dictionaryRefinedKeyIO pivot (V.unsafeIndex values i)))
      scanRefinedLargeByteDictionary pivot values indices row distinct rows keys table
{-# NOINLINE promoteRefinedLargeDictionary #-}


promoteFullLargeDictionary :: V.Vector ByteString -> MVP.MVector s Int32 -> DictionaryCollision s
promoteFullLargeDictionary values indices row _ distinct rows keys table _ = do
  rekeyLargeDictionary distinct rows keys table (\i -> unsafeIOToST (dictionaryByteKeyIO True (V.unsafeIndex values i)))
  scanFullLargeByteDictionary values indices row distinct rows keys table
{-# NOINLINE promoteFullLargeDictionary #-}


rekeyLargeDictionary :: Int -> MVP.MVector s Int -> MVP.MVector s Word64 -> MVP.MVector s Int32 -> (Int -> ST s Word64) -> ST s ()
rekeyLargeDictionary distinct rows keys table keyAt = do
  MVP.set table (-1)
  let !mask = MVP.length table - 1
      rekey !entry
        | entry == distinct = pure ()
        | otherwise = do
            representative <- MVP.unsafeRead rows entry
            key <- inline keyAt representative
            MVP.unsafeWrite keys entry key
            let insert !bucket = do
                  old <- MVP.unsafeRead table bucket
                  if old < 0
                    then MVP.unsafeWrite table bucket (fromIntegral entry) >> rekey (entry + 1)
                    else insert ((bucket + 1) .&. mask)
            insert (fromIntegral (dictionaryHashWord64 key) .&. mask)
  rekey 0
{-# INLINE rekeyLargeDictionary #-}


-- Each unique byte string owns the cached address in its metadata record.
-- Metadata stores (key, length, address) in three adjacent Word64 cells;
-- the boxed owner array stays live through every lookup and becomes the
-- result dictionary. No source-row lookup or payload copy is needed.
type ByteDictionaryKey s = ByteString -> Ptr Word8 -> Int -> ST s Word64


type ByteDictionaryCollision s = Int -> Int -> Int -> MV.MVector s ByteString -> MVP.MVector s Word64 -> MVP.MVector s Int32 -> ST s Dictionary -> ST s Dictionary


type ByteDictionaryScan s = Int -> Int -> MV.MVector s ByteString -> MVP.MVector s Word64 -> MVP.MVector s Int32 -> ST s Dictionary


continueByteDictionaryCollision :: ByteDictionaryCollision s
continueByteDictionaryCollision _ _ _ _ _ _ continue = continue
{-# INLINE continueByteDictionaryCollision #-}


-- Retain the entire owner graph once, around construction. Every cached
-- address comes from an input value, including during growth/refinement.
-- keepAlive# also covers exceptional exits; unlike a final touch, it
-- cannot disappear when the optimiser proves that a branch throws.
-- The returned dictionary owns its selected references after the scope.
-- See https://www.haskell.org/ghc/blog/20210607-the-keepAlive-story.html.
withByteDictionarySource :: V.Vector ByteString -> ST s a -> ST s a
withByteDictionarySource values (ST action) = ST (\state -> keepAlive# values state action)
{-# INLINE withByteDictionarySource #-}


internByteDictionary :: V.Vector ByteString -> Dictionary
internByteDictionary values = runST $ withByteDictionarySource values $ do
  let !count = V.length values
      !capacity = min count 32
      keyAt value ptr size = unsafeIOToST (dictionaryByteKeyAtIO False value ptr size)
  indices <- MVP.unsafeNew count
  entries <- MV.unsafeNew capacity
  metadata <- MVP.unsafeNew (capacity * 3)
  slots <- MVP.replicate 64 (-1 :: Int32)
  scanByteDictionary count keyAt (promoteRefinedDictionary values indices) values indices 0 0 entries metadata slots
{-# INLINE internByteDictionary #-}


scanFullByteDictionary :: V.Vector ByteString -> MVP.MVector s Int32 -> ByteDictionaryScan s
scanFullByteDictionary values indices =
  scanByteDictionary
    (V.length values)
    (\value ptr size -> unsafeIOToST (dictionaryByteKeyAtIO True value ptr size))
    continueByteDictionaryCollision
    values
    indices
{-# NOINLINE scanFullByteDictionary #-}


scanRefinedByteDictionary :: Int -> V.Vector ByteString -> MVP.MVector s Int32 -> ByteDictionaryScan s
scanRefinedByteDictionary !pivot values indices =
  scanByteDictionary
    (V.length values)
    (\_ ptr size -> unsafeIOToST (dictionaryRefinedKeyAtIO pivot ptr size))
    (promoteFullDictionary values indices)
    values
    indices
{-# NOINLINE scanRefinedByteDictionary #-}


promoteRefinedDictionary :: V.Vector ByteString -> MVP.MVector s Int32 -> ByteDictionaryCollision s
promoteRefinedDictionary values indices row representative distinct entries metadata table _ = do
  previous <- MV.unsafeRead entries representative
  pivot <- unsafeIOToST (dictionaryDifferenceOffsetIO (V.unsafeIndex values row) previous)
  if pivot < 0
    then do
      rekeyByteDictionary distinct entries metadata table (\value ptr size -> unsafeIOToST (dictionaryByteKeyAtIO True value ptr size))
      scanFullByteDictionary values indices row distinct entries metadata table
    else do
      rekeyByteDictionary distinct entries metadata table (\_ ptr size -> unsafeIOToST (dictionaryRefinedKeyAtIO pivot ptr size))
      scanRefinedByteDictionary pivot values indices row distinct entries metadata table
{-# NOINLINE promoteRefinedDictionary #-}


promoteFullDictionary :: V.Vector ByteString -> MVP.MVector s Int32 -> ByteDictionaryCollision s
promoteFullDictionary values indices row _ distinct entries metadata table _ = do
  rekeyByteDictionary distinct entries metadata table (\value ptr size -> unsafeIOToST (dictionaryByteKeyAtIO True value ptr size))
  scanFullByteDictionary values indices row distinct entries metadata table
{-# NOINLINE promoteFullDictionary #-}


rekeyByteDictionary :: Int -> MV.MVector s ByteString -> MVP.MVector s Word64 -> MVP.MVector s Int32 -> ByteDictionaryKey s -> ST s ()
rekeyByteDictionary distinct entries metadata table keyAt = do
  MVP.set table (-1)
  let !mask = MVP.length table - 1
      rekey !entry
        | entry == distinct = pure ()
        | otherwise = do
            value <- MV.unsafeRead entries entry
            let (fp, _, _) = BSI.toForeignPtr value
                !base = entry * 3
            size <- MVP.unsafeRead metadata (base + 1)
            address <- MVP.unsafeRead metadata (base + 2)
            key <- inline keyAt value (wordPtrToPtr (fromIntegral address)) (fromIntegral size)
            unsafeIOToST (touchForeignPtr fp)
            MVP.unsafeWrite metadata base key
            let insert !bucket = do
                  old <- MVP.unsafeRead table bucket
                  if old < 0
                    then MVP.unsafeWrite table bucket (fromIntegral entry) >> rekey (entry + 1)
                    else insert ((bucket + 1) .&. mask)
            insert (fromIntegral (dictionaryHashWord64 key) .&. mask)
  rekey 0
{-# INLINE rekeyByteDictionary #-}


scanByteDictionary
  :: Int
  -> ByteDictionaryKey s
  -> ByteDictionaryCollision s
  -> V.Vector ByteString
  -> MVP.MVector s Int32
  -> ByteDictionaryScan s
scanByteDictionary count keyAt onCollision values indices startRow startDistinct startEntries startMetadata startTable =
  let go !row !distinct !entries !metadata !table
        | row == count = do
            uniqueValues <- V.unsafeFreeze (MV.unsafeSlice 0 distinct entries)
            rowIndices <- VP.unsafeFreeze indices
            pure (Dictionary (ColByteArray uniqueValues) rowIndices)
        | otherwise = do
            let !value = V.unsafeIndex values row
                (fp, offset, size) = BSI.toForeignPtr value
                !ptr = unsafeForeignPtrToPtr fp `plusPtr` offset
                !mask = MVP.length table - 1
                emit !entry = do
                  MVP.unsafeWrite indices row entry
                  go (row + 1) distinct entries metadata table
                collide !entry !key !bucket =
                  onCollision row entry distinct entries metadata table (probe key ((bucket + 1) .&. mask))
                probe !key !bucket = do
                  entryId <- MVP.unsafeRead table bucket
                  if entryId < 0
                    then case dictionaryCapacityReached distinct (MV.length entries) of
                      1# -> grow row distinct entries metadata table
                      _ -> do
                        case dictionaryEntriesExceeded distinct of
                          1# -> error "Parquet.Write: too many dictionary entries"
                          _ -> pure ()
                        let !base = distinct * 3
                            !newId = fromIntegral distinct
                        -- Store the owner before its address. Growing copies
                        -- these references; freezing returns the same owners.
                        MV.unsafeWrite entries distinct value
                        MVP.unsafeWrite metadata base key
                        MVP.unsafeWrite metadata (base + 1) (fromIntegral size)
                        MVP.unsafeWrite metadata (base + 2) (fromIntegral (ptrToWordPtr ptr))
                        MVP.unsafeWrite table bucket newId
                        MVP.unsafeWrite indices row newId
                        go (row + 1) (distinct + 1) entries metadata table
                    else do
                      let !entry = fromIntegral entryId
                          !base = entry * 3
                      cachedKey <- MVP.unsafeRead metadata base
                      if cachedKey /= key
                        then probe key ((bucket + 1) .&. mask)
                        else do
                          cachedSize <- MVP.unsafeRead metadata (base + 1)
                          if cachedSize /= fromIntegral size
                            then collide entry key bucket
                            else case dictionaryShortSize size of
                              1# -> emit entryId
                              _ -> do
                                address <- MVP.unsafeRead metadata (base + 2)
                                let !previous = wordPtrToPtr (fromIntegral address) :: Ptr Word8
                                equal <-
                                  if ptr == previous
                                    then pure True
                                    else
                                      if size == 64
                                        then unsafeIOToST (dictionaryEqual64IO ptr previous)
                                        else (== 0) <$> unsafeIOToST (BSI.memcmp ptr previous size)
                                if equal then emit entryId else collide entry key bucket
            key <- inline keyAt value ptr size
            probe key (fromIntegral (dictionaryHashWord64 key) .&. mask)
      grow !row !distinct !entries !metadata !table
        | 1# <- dictionaryTableSizeExceeded (MVP.length table) = error "Parquet.Write: dictionary exceeds addressable size"
        | otherwise = do
            let !capacity = MV.length entries
                !extra = min capacity (count - capacity)
                !nextCapacity = capacity + extra
            if nextCapacity > maxBound `quot` 24
              then error "Parquet.Write: dictionary exceeds addressable size"
              else pure ()
            expandedEntries <- MV.unsafeGrow entries extra
            expandedMetadata <- MVP.unsafeGrow metadata (extra * 3)
            expandedTable <- MVP.replicate (MVP.length table * 2) (-1 :: Int32)
            let !mask = MVP.length expandedTable - 1
                rehash !entry
                  | entry == distinct = go row distinct expandedEntries expandedMetadata expandedTable
                  | otherwise = do
                      key <- MVP.unsafeRead expandedMetadata (entry * 3)
                      let insert !bucket = do
                            old <- MVP.unsafeRead expandedTable bucket
                            if old < 0
                              then MVP.unsafeWrite expandedTable bucket (fromIntegral entry) >> rehash (entry + 1)
                              else insert ((bucket + 1) .&. mask)
                      insert (fromIntegral (dictionaryHashWord64 key) .&. mask)
            rehash 0
  in go startRow startDistinct startEntries startMetadata startTable
{-# INLINE scanByteDictionary #-}


scanDictionary
  :: Bool
  -> Int
  -> (Int -> ST s Word64)
  -> (Int -> Int -> Bool)
  -> DictionaryCollision s
  -> MVP.MVector s Int32
  -> Int
  -> Int
  -> MVP.MVector s Int
  -> MVP.MVector s Word64
  -> MVP.MVector s Int32
  -> ST s DictionaryBuild
scanDictionary exactKeys count keyAt same onCollision indices startRow startDistinct startRows startKeys startTable =
  let go !row !distinct !rows !keys !table
        | row == count = do
            uniqueRows <- VP.unsafeFreeze (MVP.unsafeSlice 0 distinct rows)
            rowIndices <- VP.unsafeFreeze indices
            pure (DictionaryBuild uniqueRows rowIndices)
        | otherwise = do
            -- Inlining keeps keys unboxed across both scan instantiations.
            key <- inline keyAt row
            probe key (fromIntegral (dictionaryHashWord64 key) .&. mask)
        where
          !mask = MVP.length table - 1
          emit !entry = do
            MVP.unsafeWrite indices row entry
            go (row + 1) distinct rows keys table
          probe !key !bucket = do
            entry <- MVP.unsafeRead table bucket
            if entry < 0
              then case dictionaryCapacityReached distinct (MVP.length rows) of
                1# -> grow
                _ -> do
                  case dictionaryEntriesExceeded distinct of
                    1# -> error "Parquet.Write: too many dictionary entries"
                    _ -> pure ()
                  let !entryId = fromIntegral distinct
                  MVP.unsafeWrite rows distinct row
                  MVP.unsafeWrite keys distinct key
                  MVP.unsafeWrite table bucket entryId
                  MVP.unsafeWrite indices row entryId
                  go (row + 1) (distinct + 1) rows keys table
              else do
                cached <- MVP.unsafeRead keys (fromIntegral entry)
                if cached /= key
                  then probe key ((bucket + 1) .&. mask)
                  else
                    if exactKeys
                      then emit entry
                      else do
                        representative <- MVP.unsafeRead rows (fromIntegral entry)
                        if inline same row representative
                          then emit entry
                          else onCollision row representative distinct rows keys table (probe key ((bucket + 1) .&. mask))
          grow
            | MVP.length table > maxBound `quot` 8 = error "Parquet.Write: dictionary exceeds addressable size"
            | otherwise = do
                let extra = min (MVP.length rows) (count - MVP.length rows)
                expandedRows <- MVP.unsafeGrow rows extra
                expandedKeys <- MVP.unsafeGrow keys extra
                expandedTable <- MVP.replicate (MVP.length table * 2) (-1 :: Int32)
                let !newMask = MVP.length expandedTable - 1
                    rehash !entry
                      | entry == distinct = go row distinct expandedRows expandedKeys expandedTable
                      | otherwise = do
                          key <- MVP.unsafeRead keys entry
                          let insert !bucket = do
                                old <- MVP.unsafeRead expandedTable bucket
                                if old < 0
                                  then MVP.unsafeWrite expandedTable bucket (fromIntegral entry) >> rehash (entry + 1)
                                  else insert ((bucket + 1) .&. newMask)
                          insert (fromIntegral (dictionaryHashWord64 key) .&. newMask)
                rehash 0
  in go startRow startDistinct startRows startKeys startTable
{-# INLINE scanDictionary #-}


-- These predicates are shared by probe branches. Lifted Bools otherwise
-- float into one heap thunk per input row when a cold transition escapes.
dictionaryCapacityReached :: Int -> Int -> Int#
dictionaryCapacityReached (I# used) (I# capacity) = used ==# capacity
{-# INLINE dictionaryCapacityReached #-}


dictionaryEntriesExceeded :: Int -> Int#
dictionaryEntriesExceeded (I# count) = count >=# 2147483647#
{-# INLINE dictionaryEntriesExceeded #-}


dictionaryTableSizeExceeded :: Int -> Int#
dictionaryTableSizeExceeded (I# size) = case maxBound `quot` 8 of
  I# limit -> size ># limit
{-# INLINE dictionaryTableSizeExceeded #-}


-- Shared by key calculation and equality. A lifted predicate otherwise
-- becomes a fresh thunk per input row when a cold collision call escapes.
dictionaryShortSize :: Int -> Int#
dictionaryShortSize (I# size) = size <=# 8#
{-# INLINE dictionaryShortSize #-}


-- Private table-address mix; never a persisted Parquet/Iceberg hash.
-- On 64-bit hosts, fold both halves of a seeded 64x64 product. GHC lowers
-- this to native low/high multiplies and an XOR without boxed intermediates.
-- Word# cannot carry a Word64 on 32-bit hosts, so retain the old mixer there.
dictionaryHashWord64 :: Word64 -> Word64
dictionaryHashWord64 value
  | finiteBitSize (0 :: Word) == 64 =
      case value `xor` 0xa0761d6478bd642f of
        W64# x -> case (0xe7037ed1a0b428db :: Word64) of
          W64# y -> case timesWord2# (word64ToWord# x) (word64ToWord# y) of
            (# high, low #) -> W64# (wordToWord64# (xor# high low))
  | otherwise =
      let !a = (value `xor` (value `shiftR` 33)) * 0xff51afd7ed558ccd
          !b = (a `xor` (a `shiftR` 33)) * 0xc4ceb9fe1a85ec53
      in b `xor` (b `shiftR` 33)
{-# INLINE dictionaryHashWord64 #-}


-- Short byte strings use exact little-endian word keys, followed by a
-- length comparison. Longer values first use a bounded end fingerprint;
-- an ambiguous fingerprint refines it with a differing word, then falls
-- back to full-content keys on a second ambiguity. Every mode still
-- requires full byte equality for longer values.
-- Cached keys therefore never stand in for equality of larger payloads.
-- Every load below is unaligned and fits inside the logical byte string;
-- in particular, a short tail is never read as a complete eight-byte word.
dictionaryByteKeyAtIO :: Bool -> ByteString -> Ptr Word8 -> Int -> IO Word64
dictionaryByteKeyAtIO fullHash bytes ptr size = case dictionaryShortSize size of
  1# -> dictionaryReadShort ptr size
  _ -> if not fullHash then dictionaryEndKey ptr size else if size > 128 then Hash.xxh64IO 0 bytes else dictionaryMediumKey ptr size
{-# INLINE dictionaryByteKeyAtIO #-}


dictionaryByteKeyIO :: Bool -> ByteString -> IO Word64
dictionaryByteKeyIO fullHash bytes = do
  let (fp, offset, size) = BSI.toForeignPtr bytes
      ptr = unsafeForeignPtrToPtr fp `plusPtr` offset
  key <- dictionaryByteKeyAtIO fullHash bytes ptr size
  touchForeignPtr fp
  pure key
{-# INLINE dictionaryByteKeyIO #-}


-- Longer values have at least nine bytes. Both unaligned word loads fit
-- inside the logical slice, including when they overlap. Length separates
-- equal ends at different sizes; full equality verifies every cached hit.
dictionaryEndKey :: Ptr Word8 -> Int -> IO Word64
dictionaryEndKey ptr size = do
  first <- dictionaryRead64 ptr
  lastWord <- dictionaryRead64 (ptr `plusPtr` (size - 8))
  pure (first `xor` rotateL lastWord 27 `xor` (fromIntegral size * 0x9e3779b97f4a7c15))
{-# INLINE dictionaryEndKey #-}


-- Refinement is chosen once, on the first ambiguous fingerprint. An
-- equal-length pair supplies a differing bounded word; other shapes fall
-- back to full keys immediately. Neither payload is read past its slice.
dictionaryDifferenceOffsetIO :: ByteString -> ByteString -> IO Int
dictionaryDifferenceOffsetIO a b
  | size <= 8 || size /= BS.length b = pure (-1)
  | otherwise = do
      let (fpA, offsetA, _) = BSI.toForeignPtr a
          (fpB, offsetB, _) = BSI.toForeignPtr b
          ptrA = unsafeForeignPtrToPtr fpA `plusPtr` offsetA
          ptrB = unsafeForeignPtrToPtr fpB `plusPtr` offsetB
          go !offset
            | offset == size = pure (-1)
            | otherwise = do
                let !at = min offset (size - 8)
                x <- dictionaryRead64 (ptrA `plusPtr` at)
                y <- dictionaryRead64 (ptrB `plusPtr` at)
                if x /= y then pure at else if size - offset <= 8 then pure (-1) else go (offset + 8)
      result <- go 0
      touchForeignPtr fpA
      touchForeignPtr fpB
      pure result
  where
    size = BS.length a
{-# INLINE dictionaryDifferenceOffsetIO #-}


-- Include the chosen word in addition to both ends. Multiplication by an
-- odd constant and rotation are injective, so the triggering equal-end
-- pair is separated. A second ambiguity promotes to full-content keys.
dictionaryRefinedKeyAtIO :: Int -> Ptr Word8 -> Int -> IO Word64
dictionaryRefinedKeyAtIO pivot ptr size = case dictionaryShortSize size of
  1# -> dictionaryReadShort ptr size
  _ -> do
    ends <- dictionaryEndKey ptr size
    middle <- dictionaryRead64 (ptr `plusPtr` min pivot (size - 8))
    pure (ends `xor` rotateL (middle * 0xd6e8feb86659fd93) 17)
{-# INLINE dictionaryRefinedKeyAtIO #-}


dictionaryRefinedKeyIO :: Int -> ByteString -> IO Word64
dictionaryRefinedKeyIO pivot bytes = do
  let (fp, offset, size) = BSI.toForeignPtr bytes
      ptr = unsafeForeignPtrToPtr fp `plusPtr` offset
  key <- dictionaryRefinedKeyAtIO pivot ptr size
  touchForeignPtr fp
  pure key
{-# INLINE dictionaryRefinedKeyIO #-}


-- For small dictionary values a native four-lane word mix avoids a
-- foreign-call boundary per row. These are internal lookup keys, never
-- Parquet's persisted hashes. Full byte equality still verifies matches.
dictionaryMediumKey :: Ptr Word8 -> Int -> IO Word64
dictionaryMediumKey ptr size = blocks 0 0x9e3779b97f4a7c15 0x243f6a8885a308d3 0x13198a2e03707344 0xa4093822299f31d0
  where
    step !acc !word = (acc `xor` word) * 0x9e3779b185ebca87
    {-# INLINE step #-}
    blocks !offset !a !b !c !d
      | size - offset < 32 =
          finish offset (fromIntegral size `xor` rotateL a 5 `xor` rotateL b 17 `xor` rotateL c 37 `xor` rotateL d 53)
      | otherwise = do
          w0 <- dictionaryRead64 (ptr `plusPtr` offset)
          w1 <- dictionaryRead64 (ptr `plusPtr` (offset + 8))
          w2 <- dictionaryRead64 (ptr `plusPtr` (offset + 16))
          w3 <- dictionaryRead64 (ptr `plusPtr` (offset + 24))
          blocks (offset + 32) (step a w0) (step b w1) (step c w2) (step d w3)
    finish !offset !acc
      | size - offset < 8 = do
          tailWord <- dictionaryReadShort (ptr `plusPtr` offset) (size - offset)
          pure (acc `xor` tailWord)
      | otherwise = do
          word <- dictionaryRead64 (ptr `plusPtr` offset)
          finish (offset + 8) (rotateL (step acc word) 27)
{-# INLINE dictionaryMediumKey #-}


-- Called only after both logical lengths have been checked to be 64.
-- Reduce all word differences once; no variable loop or foreign call.
dictionaryEqual64IO :: Ptr Word8 -> Ptr Word8 -> IO Bool
dictionaryEqual64IO a b = do
  a0 <- dictionaryRead64 a
  b0 <- dictionaryRead64 b
  let !d0 = a0 `xor` b0
  a1 <- dictionaryRead64 (a `plusPtr` 8)
  b1 <- dictionaryRead64 (b `plusPtr` 8)
  let !d1 = d0 .|. (a1 `xor` b1)
  a2 <- dictionaryRead64 (a `plusPtr` 16)
  b2 <- dictionaryRead64 (b `plusPtr` 16)
  let !d2 = d1 .|. (a2 `xor` b2)
  a3 <- dictionaryRead64 (a `plusPtr` 24)
  b3 <- dictionaryRead64 (b `plusPtr` 24)
  let !d3 = d2 .|. (a3 `xor` b3)
  a4 <- dictionaryRead64 (a `plusPtr` 32)
  b4 <- dictionaryRead64 (b `plusPtr` 32)
  let !d4 = d3 .|. (a4 `xor` b4)
  a5 <- dictionaryRead64 (a `plusPtr` 40)
  b5 <- dictionaryRead64 (b `plusPtr` 40)
  let !d5 = d4 .|. (a5 `xor` b5)
  a6 <- dictionaryRead64 (a `plusPtr` 48)
  b6 <- dictionaryRead64 (b `plusPtr` 48)
  let !d6 = d5 .|. (a6 `xor` b6)
  a7 <- dictionaryRead64 (a `plusPtr` 56)
  b7 <- dictionaryRead64 (b `plusPtr` 56)
  pure ((d6 .|. (a7 `xor` b7)) == 0)
{-# INLINE dictionaryEqual64IO #-}


-- Exact zero-padded LE keys for a caller-checked tail of zero to eight bytes.
dictionaryReadShort :: Ptr Word8 -> Int -> IO Word64
dictionaryReadShort ptr size = case size of
  0 -> pure 0
  1 -> fromIntegral <$> dictionaryRead8 ptr
  2 -> fromIntegral <$> dictionaryRead16 ptr
  3 -> do
    a <- dictionaryRead16 ptr
    b <- dictionaryRead8 (ptr `plusPtr` 2)
    pure (fromIntegral a .|. (fromIntegral b `shiftL` 16))
  4 -> fromIntegral <$> dictionaryRead32 ptr
  5 -> do
    a <- dictionaryRead32 ptr
    b <- dictionaryRead8 (ptr `plusPtr` 4)
    pure (fromIntegral a .|. (fromIntegral b `shiftL` 32))
  6 -> do
    a <- dictionaryRead32 ptr
    b <- dictionaryRead16 (ptr `plusPtr` 4)
    pure (fromIntegral a .|. (fromIntegral b `shiftL` 32))
  7 -> do
    a <- dictionaryRead32 ptr
    b <- dictionaryRead16 (ptr `plusPtr` 4)
    c <- dictionaryRead8 (ptr `plusPtr` 6)
    pure (fromIntegral a .|. (fromIntegral b `shiftL` 32) .|. (fromIntegral c `shiftL` 48))
  _ -> dictionaryRead64 ptr
{-# INLINE dictionaryReadShort #-}


-- Keep the shared size predicate unboxed. A lifted Bool shared between
-- hashing and equality otherwise becomes one heap thunk per input row.
dictionaryRead8 :: Ptr Word8 -> IO Word8
dictionaryRead8 (Ptr address) = IO $ \s -> case readWord8OffAddr# address 0# s of
  (# next, value #) -> (# next, W8# value #)
{-# INLINE dictionaryRead8 #-}


dictionaryRead16 :: Ptr Word8 -> IO Word16
dictionaryRead16 (Ptr address) = do
  value <- IO $ \s -> case readWord8OffAddrAsWord16# address 0# s of
    (# next, word #) -> (# next, W16# word #)
  pure (case targetByteOrder of LittleEndian -> value; BigEndian -> byteSwap16 value)
{-# INLINE dictionaryRead16 #-}


dictionaryRead32 :: Ptr Word8 -> IO Word32
dictionaryRead32 (Ptr address) = do
  value <- IO $ \s -> case readWord8OffAddrAsWord32# address 0# s of
    (# next, word #) -> (# next, W32# word #)
  pure (case targetByteOrder of LittleEndian -> value; BigEndian -> byteSwap32 value)
{-# INLINE dictionaryRead32 #-}


dictionaryRead64 :: Ptr Word8 -> IO Word64
dictionaryRead64 (Ptr address) = do
  value <- IO $ \s -> case readWord8OffAddrAsWord64# address 0# s of
    (# next, word #) -> (# next, W64# word #)
  pure (case targetByteOrder of LittleEndian -> value; BigEndian -> byteSwap64 value)
{-# INLINE dictionaryRead64 #-}


-- | Encode a 'Dictionary' as a @DICTIONARY_PAGE@.
encodeDictPage :: Dictionary -> ByteString
encodeDictPage d = either error encode (planPlainPayload (dictUniques d))
  where
    encode plan =
      let !payload = encodePlannedPlainPayload plan (dictUniques d)
          !numEntries = plainValueCount plan
          !size = fromIntegral (plainPayloadSize plan)
          !hdr =
            PageHeader
              { phType =
                  PtDictionaryPage
                    DictionaryPageHeader
                      { dictNumValues = numEntries
                      , dictEncoding = 0 -- Dictionary entries use PLAIN.
                      }
              , phUncompressedPageSize = Just size
              , phCompressedPageSize = Just size
              }
      in encodePageHeader hdr <> payload


{- | Encode the 'DATA_PAGE' that consumes a dictionary (RLE-hybrid index
stream prefixed by a single byte recording the bit width).
-}
encodeDictDataPage :: Dictionary -> ByteString
encodeDictDataPage d = either error id $ do
  let !indices = dictIndices d
      !numIndices = VP.length indices
      !uniqueCount = columnDataLength (dictUniques d)
  numValues <- pageInt32 "dictionary index count" numIndices
  _ <- pageInt32 "dictionary entry count" uniqueCount
  if uniqueCount == 0 && numIndices /= 0
    then Left "Parquet.Write: nonempty indices require a nonempty dictionary"
    else Right ()
  let !largestIndex = max 0 (uniqueCount - 1)
      !bw = LE.bitWidthFor largestIndex
  plan <- LE.planRLEHybrid bw (fromIntegral largestIndex) indices
  bodySize <- pageSizeWithOverhead 1 (LE.hybridEncodedSize plan)
  let !indexStream = LE.encodeHybridPlan plan
      !body = BS.singleton (fromIntegral bw) <> indexStream
      !hdr =
        PageHeader
          { phType =
              PtDataPage
                DataPageHeader
                  { dphNumValues = numValues
                  , dphEncoding = parquetEncodingRleDictionary
                  }
          , phUncompressedPageSize = Just bodySize
          , phCompressedPageSize = Just bodySize
          }
  Right (encodePageHeader hdr <> body)


parquetEncodingRleDictionary :: Int32
parquetEncodingRleDictionary = 8


-- ============================================================
-- Indexed writer: bloom filter + page index footers
-- ============================================================

{- | Which Parquet data page header to emit for a column chunk.

- 'PageV1': @DATA_PAGE@ (legacy). Definition + repetition levels are
  length-prefixed RLE-hybrid streams concatenated with the values
  inside the (optionally compressed) page body.
- 'PageV2': @DATA_PAGE_V2@. Definition and repetition levels are NOT
  length-prefixed and are NOT compressed. Only the values segment is
  compressed, and the level segment lengths are recorded in the page
  header so readers can skip directly to the data.
-}
data PageVersion = PageV1 | PageV2
  deriving (Show, Eq)


{- | Per-column encryption knobs the writer needs at page-emit time.

Lifted out of the global 'Enc.EncryptionConfig' so a single column
aux record carries everything: the key, the algorithm, the file-id /
AAD-prefix / key-metadata bytes, and the column ordinal that goes
into the AAD suffix. All columns in a typical Parquet file share the
same algorithm and AAD-prefix; the helper 'columnEncryptionFor'
builds a per-column 'ColumnEncryption' from the file-wide
'Enc.EncryptionConfig' so callers don't have to repeat the boilerplate.
-}
data ColumnEncryption = ColumnEncryption
  { ceAlgorithm :: !Enc.EncryptionAlgorithm
  {- ^ 'Enc.AesGcmV1' encrypts every page (header + body) with GCM;
  'Enc.AesGcmCtrV1' encrypts data\/dictionary page bodies with CTR
  and everything else (page headers, column metadata, indexes) with
  GCM. Both algorithms produce the same on-the-wire framing.
  -}
  , ceKey :: !BS.ByteString
  -- ^ AES key (16, 24, or 32 bytes).
  , ceFileId :: !BS.ByteString
  -- ^ Fresh random file identifier, shared with the footer's AAD context.
  , ceAadPrefix :: !BS.ByteString
  -- ^ Caller-supplied AAD prefix (typically empty).
  , ceKeyMetadata :: !BS.ByteString
  {- ^ Reserved for column-specific keys. The whole-file writer currently
  supports footer-key encryption and uses 'feKeyMetadata' for key recovery.
  -}
  , ceColumnOrdinal :: !Int
  -- ^ Column ordinal within the row group.
  }
  deriving (Show, Eq)


{- | Auxiliary metadata for one column chunk. Explicit values override
generated indexes; 'caGeneratePageIndex' requests automatic indexes when
the corresponding explicit fields are 'Nothing'.
-}
data ColumnAux = ColumnAux
  { caGeneratePageIndex :: !Bool
  -- ^ Generate indexes from the actual single-page layout and statistics.
  , caBloomFilter :: !(Maybe Sbbf)
  {- ^ Pre-built split-block bloom filter for this column chunk. The
  writer serialises it into the bloom-filter region and records the
  (offset, length) on the column metadata.
  -}
  , caOffsetIndex :: !(Maybe OffsetIndex)
  {- ^ Per-page @(offset, compressed_size, first_row_index)@ entries.
  The writer rewrites the @plOffset@ of each 'PageLocation' so that
  it points at the actual page-bytes location in the assembled file.
  -}
  , caColumnIndex :: !(Maybe ColumnIndex)
  -- ^ Per-page null/min/max statistics. Written verbatim.
  , caCodec :: !Compression
  {- ^ Compression codec for the column-chunk page bytes. Default
  'Uncompressed'; use any codec listed in 'Parquet.Compress.compressPageBytes'.
  -}
  , caPageVersion :: !PageVersion
  -- ^ Page header version to emit. Defaults to 'PageV1'.
  , caEncryption :: !(Maybe ColumnEncryption)
  {- ^ When 'Just', the page header + body for this column chunk are
  encrypted per the spec module-wise framing (nonce || ciphertext
  || tag for GCM modules; nonce || ciphertext for CTR modules).
  Defaults to 'Nothing' (plaintext).
  -}
  }
  deriving (Show, Eq)


-- These builders emit one data page per column. Explicit auxiliary indexes
-- must describe that page; their offsets/sizes are fixed during layout.
validateAuxIndexes :: ColumnAux -> Either String ()
validateAuxIndexes aux = do
  case caOffsetIndex aux of
    Just oi
      | V.length (oiPageLocations oi) /= 1
          || maybe False ((/= 1) . V.length) (oiUnencodedByteArrayDataBytes oi) ->
          Left "Parquet.Write: explicit offset index must describe exactly one page"
    _ -> Right ()
  case caColumnIndex aux of
    Just ci
      | V.length (ciNullPages ci) /= 1
          || V.length (ciMinValues ci) /= 1
          || V.length (ciMaxValues ci) /= 1
          || maybe False ((/= 1) . V.length) (ciNullCounts ci)
          || maybe False ((/= 1) . V.length) (ciNanCounts ci) ->
          Left "Parquet.Write: explicit column index must describe exactly one page"
    _ -> Right ()


emptyColumnAux :: ColumnAux
emptyColumnAux = ColumnAux False Nothing Nothing Nothing Uncompressed PageV1 Nothing


{- | Convenience: build a 'ColumnEncryption' for a leaf column from
the file-wide 'Enc.EncryptionConfig'. Looks up the column key from
@encColumnKeys@ by the leaf @SchemaElement@'s @seName@ (if absent
we fall back to @ekFooterKey@ so a single-key configuration "just
works"); copies the algorithm / file-id / AAD-prefix / key-metadata
through verbatim.

Returns 'Nothing' when the configuration is 'Enc.unencrypted'
(empty footer key /and/ empty column-keys map).
-}
columnEncryptionFor
  :: Enc.EncryptionConfig
  -> Text
  -- ^ leaf column name
  -> Int
  -- ^ column ordinal
  -> Maybe ColumnEncryption
columnEncryptionFor cfg colName colOrd
  | BS.null (Enc.ekFooterKey keys)
      && Map.null (Enc.ekColumnKeys keys) =
      Nothing
  | otherwise =
      Just
        ColumnEncryption
          { ceAlgorithm = Enc.encAlgorithm cfg
          , ceKey = lookupKey
          , ceFileId = Enc.encAadFileId cfg
          , ceAadPrefix = Enc.encAadPrefix cfg
          , ceKeyMetadata = Enc.encKeyMetadata cfg
          , ceColumnOrdinal = colOrd
          }
  where
    keys = Enc.encKeys cfg
    lookupKey = case Map.lookup colName (Enc.ekColumnKeys keys) of
      Just k -> k
      Nothing -> Enc.ekFooterKey keys


{- | Encrypt a single page (header || body) according to a
'ColumnEncryption'. The protocol the writer follows for one page:

1. Build the page header thrift bytes /and/ the (possibly compressed)
   body bytes as usual.
2. Compute @AAD = aad_prefix || aad_file_id || module_type || rg ||
   col || page@ for the page header module.
3. Encrypt the page header bytes with GCM (always) under that AAD,
   yielding @nonce || ciphertext || tag@ (28 bytes of overhead).
4. Compute the same AAD with @module_type = ModuleDataPage@ for the
   body. Encrypt the body with the algorithm-appropriate cipher
   ('Enc.AesGcmV1' uses GCM, 'Enc.AesGcmCtrV1' uses CTR).
5. Concatenate @encryptedHeader || encryptedBody@. The result is
   what gets written to disk; the only metadata change on the column
   chunk is that @cmEncoding@ continues to advertise the underlying
   encoding and the encryption is recorded on the column-chunk's
   @cmKeyMetadata@.
-}
encryptPageBytes
  :: RNG.MonadRandom m
  => ColumnEncryption
  -> Enc.ModuleType
  -- ^ module type for the page body (DataPage, DictionaryPage)
  -> Int
  -- ^ row-group ordinal
  -> Int
  -- ^ page ordinal within the column chunk
  -> ByteString
  -- ^ unencrypted page header bytes
  -> ByteString
  -- ^ unencrypted page body bytes
  -> m (Either String ByteString)
encryptPageBytes ce bodyModule rgOrd pageOrd hdrBytes bodyBytes = do
  nonces <- RNG.getRandomBytes 24
  pure (encryptPageBytesWithNonces nonces ce bodyModule rgOrd pageOrd hdrBytes bodyBytes)


encryptPageBytesWithNonces
  :: ByteString
  -> ColumnEncryption
  -> Enc.ModuleType
  -> Int
  -> Int
  -> ByteString
  -> ByteString
  -> Either String ByteString
encryptPageBytesWithNonces nonces ce bodyModule rgOrd pageOrd hdrBytes bodyBytes = do
  encryptedSize <-
    pageSizeWithOverhead
      (case ceAlgorithm ce of Enc.AesGcmV1 -> 32; Enc.AesGcmCtrV1 -> 16)
      (BS.length bodyBytes)
  let !aadHdrModule = case bodyModule of
        Enc.ModuleDataPage -> Enc.ModuleDataPageHeader
        Enc.ModuleDictionaryPage -> Enc.ModuleDictionaryPageHeader
        other -> other
      !suffixHdr =
        Enc.buildAadSuffix
          (ceFileId ce)
          aadHdrModule
          (fromIntegral rgOrd)
          (fromIntegral (ceColumnOrdinal ce))
          (fromIntegral pageOrd)
      !suffixBody =
        Enc.buildAadSuffix
          (ceFileId ce)
          bodyModule
          (fromIntegral rgOrd)
          (fromIntegral (ceColumnOrdinal ce))
          (fromIntegral pageOrd)
      !aadHdr = Enc.buildAad (ceAadPrefix ce) suffixHdr
      !aadBody = Enc.buildAad (ceAadPrefix ce) suffixBody
  encBody <- case ceAlgorithm ce of
    Enc.AesGcmV1 -> Enc.encryptGcmModuleFramedWithNonce (ceKey ce) (BS.take 12 (BS.drop 12 nonces)) aadBody bodyBytes
    Enc.AesGcmCtrV1 -> Enc.encryptCtrModuleFramedWithNonce (ceKey ce) (BS.take 12 (BS.drop 12 nonces)) bodyBytes
  (header, _) <- readPageHeaderAt hdrBytes 0
  let encryptedHeader =
        encodePageHeader
          header
            { phCompressedPageSize = Just encryptedSize
            }
  encHdr <- Enc.encryptGcmModuleFramedWithNonce (ceKey ce) (BS.take 12 nonces) aadHdr encryptedHeader
  Right (BS.append encHdr encBody)


{- | Encrypt a DATA_PAGE_V2 page, given the four parts produced by
'encodeColumnDataPageV2Parts' / 'encodeOptionalColumnPageV2Parts'.

The level segments remain uncompressed, but are encrypted along with
all values as a single page body. The authenticated page header is a
separate encrypted module, exactly as for V1.
-}
encryptPageBytesV2
  :: RNG.MonadRandom m
  => ColumnEncryption
  -> Int
  -> Int
  -> ByteString
  -> ByteString
  -> ByteString
  -> ByteString
  -> m (Either String ByteString)
encryptPageBytesV2 ce rgOrd pageOrd hdrBytes repBytes defBytes valBytes =
  encryptPageBytes
    ce
    Enc.ModuleDataPage
    rgOrd
    pageOrd
    hdrBytes
    (BS.concat [repBytes, defBytes, valBytes])


{- | Encrypt a single auxiliary module (bloom filter, offset index,
column index) with the column's GCM key under the module's AAD,
when 'caEncryption' is set. Returns the original payload unchanged
otherwise. CTR isn't used for these modules even on
'AesGcmCtrV1' columns: the spec mandates GCM for everything except
data\/dictionary page bodies, so the algorithm flag is irrelevant
here.
-}
encryptAuxModule
  :: RNG.MonadRandom m
  => Maybe ColumnEncryption
  -> Enc.ModuleType
  -> Int
  -> ByteString
  -> m (Either String ByteString)
encryptAuxModule Nothing _ _ payload = pure (Right payload)
encryptAuxModule (Just ce) mt rgOrd payload = do
  nonce <- RNG.getRandomBytes 12
  pure (encryptAuxModuleWithNonce nonce ce mt rgOrd payload)


encryptAuxModuleWithNonce
  :: ByteString
  -> ColumnEncryption
  -> Enc.ModuleType
  -> Int
  -> ByteString
  -> Either String ByteString
encryptAuxModuleWithNonce nonce ce mt rgOrd payload =
  let suffix =
        Enc.buildAadSuffix
          (ceFileId ce)
          mt
          (fromIntegral rgOrd)
          (fromIntegral (ceColumnOrdinal ce))
          0
  in Enc.encryptGcmModuleFramedWithNonce
       (ceKey ce)
       nonce
       (Enc.buildAad (ceAadPrefix ce) suffix)
       payload


-- One random nonce per module, indexed by physical row group and column.
-- The writer currently emits one data page per column chunk. These slots
-- must be extended if it starts emitting dictionary or multiple data pages.
newtype EncryptionNonces = EncryptionNonces (V.Vector (V.Vector ByteString))


moduleNonce :: Maybe EncryptionNonces -> Int -> Int -> Enc.ModuleType -> ByteString
moduleNonce Nothing _ _ _ = error "Parquet.Write: encryption requires the IO writer"
moduleNonce (Just (EncryptionNonces groups)) rg col mt =
  let slot = case mt of
        Enc.ModuleDataPageHeader -> 0
        Enc.ModuleDataPage -> 1
        Enc.ModuleBloomFilterHeader -> 2
        Enc.ModuleBloomFilterBitset -> 3
        Enc.ModuleOffsetIndex -> 4
        Enc.ModuleColumnIndex -> 5
        _ -> error "Parquet.Write: unsupported encryption nonce slot"
  in BS.take 12 (BS.drop (slot * 12) (groups V.! rg V.! col))


-- Layout functions use the physical column index, never a caller's
-- possibly stale ceColumnOrdinal, for both the nonce and the AAD.
encryptLayoutModule
  :: Maybe EncryptionNonces
  -> Int
  -> Int
  -> Maybe ColumnEncryption
  -> Enc.ModuleType
  -> ByteString
  -> ByteString
encryptLayoutModule _ _ _ Nothing _ payload = payload
encryptLayoutModule nonces rg col (Just ce) mt payload =
  either
    (error . ("Parquet.Write: encryption failed: " ++))
    id
    ( encryptAuxModuleWithNonce
        (moduleNonce nonces rg col mt)
        ce {ceColumnOrdinal = col}
        mt
        rg
        payload
    )


layoutBlooms
  :: Maybe EncryptionNonces
  -> V.Vector RowGroup
  -> V.Vector (V.Vector ColumnAux)
  -> Int
  -- ^ starting offset
  -> (V.Vector RowGroup, [ByteString], Int)
layoutBlooms nonces rgs auxes start = go 0 start [] V.empty
  where
    go !i !off !payloads !acc
      | i >= V.length rgs = (acc, reverse payloads, off)
      | otherwise =
          let !rg = V.unsafeIndex rgs i
              !cols = rgColumns rg
              !aux = if i < V.length auxes then V.unsafeIndex auxes i else V.empty
              (!cols', !off', !payloads') =
                V.ifoldl' (rewriteCol i aux) (V.empty, off, payloads) cols
              !rg' = rg {rgColumns = cols'}
          in go (i + 1) off' payloads' (V.snoc acc rg')

    rewriteCol
      :: Int -- row-group ordinal
      -> V.Vector ColumnAux
      -> (V.Vector ColumnChunk, Int, [ByteString])
      -> Int
      -> ColumnChunk
      -> (V.Vector ColumnChunk, Int, [ByteString])
    rewriteCol rgOrd aux (!ccs, !off, !payloads) cIdx cc =
      let mAux = if cIdx < V.length aux then Just (V.unsafeIndex aux cIdx) else Nothing
      in case mAux >>= caBloomFilter of
           Nothing -> (V.snoc ccs cc, off, payloads)
           Just bf ->
             let !raw = encodeBloomFilter bf
                 -- Bloom headers and bitsets are separate GCM modules,
                 -- with distinct AAD module types and independent nonces.
                 !bs = case mAux >>= caEncryption of
                   Nothing -> raw
                   Just ce -> case Thrift.decodeCompactFrom raw 0 of
                     Left err -> error ("Parquet.Write: invalid encoded bloom header: " ++ err)
                     Right (_, headerEnd) ->
                       encryptLayoutModule
                         nonces
                         rgOrd
                         cIdx
                         (Just ce)
                         Enc.ModuleBloomFilterHeader
                         (BS.take headerEnd raw)
                         <> encryptLayoutModule
                           nonces
                           rgOrd
                           cIdx
                           (Just ce)
                           Enc.ModuleBloomFilterBitset
                           (BS.drop headerEnd raw)
                 !bsLen = BS.length bs
                 !cm0 = fromMaybe defaultMetadata (ccMetadata cc)
                 !cm' =
                   cm0
                     { cmBloomFilterOffset = Just (fromIntegral off)
                     -- The length field is optional and signed32, while a
                     -- legal bitset plus its header/framing can exceed it.
                     , cmBloomFilterLength = if bsLen <= fromIntegral (maxBound :: Int32) then Just (fromIntegral bsLen) else Nothing
                     }
                 !cc' = cc {ccMetadata = Just cm'}
             in (V.snoc ccs cc', off + bsLen, bs : payloads)

    defaultMetadata =
      ColumnMetadata
        { cmType = PTInt32
        , cmEncodings = V.empty
        , cmPathInSchema = V.empty
        , cmCodec = Uncompressed
        , cmNumValues = 0
        , cmTotalUncompressedSize = 0
        , cmTotalCompressedSize = 0
        , cmDataPageOffset = 0
        , cmDictionaryPageOffset = Nothing
        , cmStatistics = Nothing
        , cmBloomFilterOffset = Nothing
        , cmBloomFilterLength = Nothing
        }


layoutOffsetIndex
  :: Maybe EncryptionNonces
  -> V.Vector RowGroup
  -> V.Vector (V.Vector ColumnAux)
  -> Int
  -> (V.Vector RowGroup, [ByteString], Int)
layoutOffsetIndex nonces rgs auxes start = go 0 start [] V.empty
  where
    go !i !off !payloads !acc
      | i >= V.length rgs = (acc, reverse payloads, off)
      | otherwise =
          let !rg = V.unsafeIndex rgs i
              !cols = rgColumns rg
              !aux = if i < V.length auxes then V.unsafeIndex auxes i else V.empty
              (!cols', !off', !payloads') =
                V.ifoldl' (rewriteCol i aux) (V.empty, off, payloads) cols
              !rg' = rg {rgColumns = cols'}
          in go (i + 1) off' payloads' (V.snoc acc rg')

    rewriteCol
      :: Int
      -> V.Vector ColumnAux
      -> (V.Vector ColumnChunk, Int, [ByteString])
      -> Int
      -> ColumnChunk
      -> (V.Vector ColumnChunk, Int, [ByteString])
    rewriteCol rgOrd aux (!ccs, !off, !payloads) cIdx cc =
      let mAux = if cIdx < V.length aux then Just (V.unsafeIndex aux cIdx) else Nothing
          requested = case mAux >>= caOffsetIndex of
            Just oi -> Just oi
            Nothing | maybe False (\aux -> caGeneratePageIndex aux || maybe False (const True) (caColumnIndex aux)) mAux -> Just (OffsetIndex V.empty Nothing)
            _ -> Nothing
      in case requested of
           Nothing -> (V.snoc ccs cc, off, payloads)
           Just oi ->
             let !locations = case ccMetadata cc of
                   Just metadata
                     | cmTotalCompressedSize metadata <= fromIntegral (maxBound :: Int32) ->
                         V.singleton
                           ( PageLocation
                               (cmDataPageOffset metadata)
                               (fromIntegral (cmTotalCompressedSize metadata))
                               0
                           )
                   _ -> error "Parquet.Write: indexed page size exceeds signed 32-bit range"
                 !raw = encodeOffsetIndex oi {oiPageLocations = locations}
                 !bs =
                   encryptLayoutModule
                     nonces
                     rgOrd
                     cIdx
                     (mAux >>= caEncryption)
                     Enc.ModuleOffsetIndex
                     raw
                 !bsLen = BS.length bs
                 !cc' =
                   cc
                     { ccOffsetIndexOffset = Just (fromIntegral off)
                     , ccOffsetIndexLength = Just (fromIntegral bsLen)
                     }
             in (V.snoc ccs cc', off + bsLen, bs : payloads)


layoutColumnIndex
  :: Maybe EncryptionNonces
  -> V.Vector RowGroup
  -> V.Vector (V.Vector ColumnAux)
  -> Int
  -> (V.Vector RowGroup, [ByteString], Int)
layoutColumnIndex nonces rgs auxes start = go 0 start [] V.empty
  where
    go !i !off !payloads !acc
      | i >= V.length rgs = (acc, reverse payloads, off)
      | otherwise =
          let !rg = V.unsafeIndex rgs i
              !cols = rgColumns rg
              !aux = if i < V.length auxes then V.unsafeIndex auxes i else V.empty
              (!cols', !off', !payloads') =
                V.ifoldl' (rewriteCol i aux) (V.empty, off, payloads) cols
              !rg' = rg {rgColumns = cols'}
          in go (i + 1) off' payloads' (V.snoc acc rg')

    rewriteCol
      :: Int
      -> V.Vector ColumnAux
      -> (V.Vector ColumnChunk, Int, [ByteString])
      -> Int
      -> ColumnChunk
      -> (V.Vector ColumnChunk, Int, [ByteString])
    rewriteCol rgOrd aux (!ccs, !off, !payloads) cIdx cc =
      let mAux = if cIdx < V.length aux then Just (V.unsafeIndex aux cIdx) else Nothing
          requested = case mAux >>= caColumnIndex of
            Just ci -> Just ci
            Nothing | maybe False caGeneratePageIndex mAux -> singlePageColumnIndex cc
            _ -> Nothing
      in case requested of
           Nothing -> (V.snoc ccs cc, off, payloads)
           Just ci ->
             let !raw = encodeColumnIndex ci
                 !bs =
                   encryptLayoutModule
                     nonces
                     rgOrd
                     cIdx
                     (mAux >>= caEncryption)
                     Enc.ModuleColumnIndex
                     raw
                 !bsLen = BS.length bs
                 !cc' =
                   cc
                     { ccColumnIndexOffset = Just (fromIntegral off)
                     , ccColumnIndexLength = Just (fromIntegral bsLen)
                     }
             in (V.snoc ccs cc', off + bsLen, bs : payloads)


-- | The auxiliary regions and rewritten metadata for composite writers.
data ColumnAuxLayout = ColumnAuxLayout
  { auxRowGroups :: !(V.Vector RowGroup)
  , auxParts :: ![ByteString]
  , auxEndOffset :: {-# UNPACK #-} !Int
  }


{- | Append bloom filters and page indexes for already encoded, single-page
column chunks. Column metadata must describe the final page bytes, with
file-relative offsets. This pure entry point rejects encryption; encrypted
writers must allocate independent nonces for each module.
-}
layoutColumnAuxiliaries
  :: V.Vector RowGroup
  -> V.Vector (V.Vector ColumnAux)
  -> Int
  -> Either String ColumnAuxLayout
layoutColumnAuxiliaries groups auxes start = do
  if start < 4 || V.length groups /= V.length auxes
    then Left "Parquet.Write: invalid auxiliary layout shape or start offset"
    else Right ()
  V.imapM_
    ( \index group -> do
        let columns = rgColumns group
            options = auxes V.! index
        if V.length columns /= V.length options
          then Left "Parquet.Write: auxiliary column count mismatch"
          else Right ()
        V.imapM_
          ( \column aux -> do
              validateAuxIndexes aux
              case caEncryption aux of
                Just _ -> Left "Parquet.Write: pure auxiliary layout does not support encryption"
                Nothing -> Right ()
              case ccMetadata (columns V.! column) of
                Nothing -> Left "Parquet.Write: auxiliary column metadata missing"
                Just metadata
                  | cmDataPageOffset metadata < 4 || cmTotalCompressedSize metadata < 0 ->
                      Left "Parquet.Write: invalid auxiliary column bounds"
                  | (caGeneratePageIndex aux || maybe False (const True) (caOffsetIndex aux) || maybe False (const True) (caColumnIndex aux))
                      && cmTotalCompressedSize metadata > fromIntegral (maxBound :: Int32) ->
                      Left "Parquet.Write: indexed page size exceeds signed 32-bit range"
                  | otherwise -> Right ()
          )
          options
    )
    groups
  let active aux = caGeneratePageIndex aux || maybe False (const True) (caBloomFilter aux) || maybe False (const True) (caOffsetIndex aux) || maybe False (const True) (caColumnIndex aux)
  if not (V.any (V.any active) auxes)
    then Right (ColumnAuxLayout groups [] start)
    else layout
  where
    layout =
      let (bloomGroups, blooms, bloomEnd) = layoutBlooms Nothing groups auxes start
          (offsetGroups, offsets, offsetEnd) = layoutOffsetIndex Nothing bloomGroups auxes bloomEnd
          (indexedGroups, indexes, end) = layoutColumnIndex Nothing offsetGroups auxes offsetEnd
          parts = blooms ++ offsets ++ indexes
          total = toInteger start + sum (map (toInteger . BS.length) parts)
      in if total > toInteger (maxBound :: Int) || any ((> toInteger (maxBound :: Int32)) . toInteger . BS.length) parts
           then Left "Parquet.Write: auxiliary regions exceed addressable file or signed 32-bit module lengths"
           else Right (ColumnAuxLayout indexedGroups parts end)


singlePageColumnIndex :: ColumnChunk -> Maybe ColumnIndex
singlePageColumnIndex chunk = do
  metadata <- ccMetadata chunk
  statistics <- cmStatistics metadata
  let allNull = statNullCount statistics == Just (cmNumValues metadata)
  low <- if allNull then Just BS.empty else statMinValue statistics
  high <- if allNull then Just BS.empty else statMaxValue statistics
  pure
    ColumnIndex
      { ciNullPages = V.singleton allNull
      , ciMinValues = V.singleton low
      , ciMaxValues = V.singleton high
      , ciBoundaryOrder = OrderUnordered
      , ciNullCounts = V.singleton <$> statNullCount statistics
      , ciNanCounts = V.singleton <$> statNanCount statistics
      , ciRepetitionLevelHistograms = Nothing
      , ciDefinitionLevelHistograms = Nothing
      }


-- Suppress unused-import warning when bloom-filter sizing helpers aren't
-- referenced directly in the writer (they are part of the public API).
_unusedBF :: Sbbf -> Int
_unusedBF = BF.sbbfNumBytes


-- Keep headers and payloads separate until the final file allocation.
-- Joining each page first would copy every payload one extra time.
data EncodedColumn = EncodedColumn
  { ecParts :: ![ByteString]
  , ecCompressedSize :: {-# UNPACK #-} !Int
  , ecUncompressedSize :: {-# UNPACK #-} !Int
  , ecCodec :: !Compression
  }


encodedColumn :: [ByteString] -> Int -> Compression -> EncodedColumn
encodedColumn parts uncompressed codec =
  EncodedColumn parts (sum (map BS.length parts)) uncompressed codec


-- ============================================================
-- Heterogeneous typed builders
-- ============================================================

{- | Build a complete Parquet file from a schema and one or more row
groups of typed column data. Each leaf in the schema (i.e. each
entry whose 'seType' is @Just@) must have a matching 'ColumnData'
column in every row group, in the same order. Produces @PAR1@
magic, uncompressed @PLAIN@ pages, footer, and trailing magic.

Use 'buildParquetFileWithIndex' to additionally emit bloom filter,
offset index, and column index regions.
-}
buildParquetFile
  :: V.Vector SchemaElement
  -> V.Vector (V.Vector ColumnData)
  -> ByteString
buildParquetFile schema rowGroups =
  buildParquetFileWithIndex
    schema
    rowGroups
    (V.map (V.map (const emptyColumnAux)) rowGroups)


{- | Build a Parquet file with optional bloom filter, offset index,
column index, and per-column compression. The 'ColumnAux' parallel
array specifies what to emit for each column; 'emptyColumnAux'
omits all extras.

File layout:

@
PAR1                         -- 4 bytes
<row group bytes ...>        -- compressed PLAIN data pages
<bloom filter bitsets ...>   -- one per column with caBloomFilter = Just
<offset index thrifts ...>   -- one per column with caOffsetIndex = Just
<column index thrifts ...>   -- one per column with caColumnIndex = Just
<footer>                     -- Thrift-encoded FileMetadata
<footer length: int32 LE>
PAR1
@
-}
buildParquetFileWithIndex
  :: V.Vector SchemaElement
  -> V.Vector (V.Vector ColumnData)
  -> V.Vector (V.Vector ColumnAux)
  -> ByteString
buildParquetFileWithIndex schema rowGroups auxes =
  case validateRequiredLayout schema rowGroups auxes of
    Left err -> error err
    Right () ->
      if V.any (V.any (maybe False (const True) . caEncryption)) auxes
        then error "Parquet.Write: encryption requires buildParquetFileWithIndexIO and an encrypted footer"
        else buildParquetFileWithIndex' Nothing Nothing BS.empty schema rowGroups auxes


validateRequiredLayout
  :: V.Vector SchemaElement
  -> V.Vector (V.Vector ColumnData)
  -> V.Vector (V.Vector ColumnAux)
  -> Either String ()
validateRequiredLayout schema groups auxes = do
  V.mapM_ (V.mapM_ (Compress.validateCompression . caCodec)) auxes
  V.mapM_ (V.mapM_ validateAuxIndexes) auxes
  if V.null schema || seType (V.head schema) /= Nothing
    then Left "Parquet.Write: schema must start with a group root"
    else Right ()
  let leaves = V.tail schema
  if seNumChildren (V.head schema) /= Just (fromIntegral (V.length leaves))
    then Left "Parquet.Write: flat schema root child count does not match its fields"
    else Right ()
  V.mapM_
    ( \leaf ->
        if seType leaf == Nothing || seNumChildren leaf /= Nothing || seRepetition leaf /= Just Required
          then Left "Parquet.Write: required writer needs flat required fields; use the mixed or nested writer"
          else
            if not (hasCompatiblePhysicalType leaf)
              then Left "Parquet.Write: logical annotation has incompatible physical storage"
              else Right ()
    )
    leaves
  if V.length auxes > V.length groups
    then Left "Parquet.Write: auxiliary metadata has extra row groups"
    else Right ()
  V.imapM_
    ( \rg cols -> do
        if V.length cols /= V.length leaves
          then Left "Parquet.Write: column count does not match schema"
          else Right ()
        if rg < V.length auxes && V.length (auxes V.! rg) > V.length cols
          then Left "Parquet.Write: auxiliary metadata has extra columns"
          else Right ()
        let count = if V.null cols then 0 else columnDataLength (V.head cols)
        if count > fromIntegral (maxBound :: Int32)
          then Left "Parquet.Write: row count exceeds a single page's 32-bit limit"
          else Right ()
        V.imapM_
          ( \col values ->
              if columnDataLength values /= count
                then Left "Parquet.Write: columns in a row group have different lengths"
                else
                  if seType (leaves V.! col) /= Just (columnDataParquetType values)
                    then Left "Parquet.Write: column physical type does not match schema"
                    else validateLogicalColumn (leaves V.! col) values
          )
          cols
    )
    groups


{- | Configuration for encrypted-footer mode (see
'buildParquetFileWithIndexEncryptedFooter'). Encapsulates the AES
key plus the AAD context so the writer can produce the
@ModuleFooter@ ciphertext that the parquet-format spec requires.
-}
data FooterEncryption = FooterEncryption
  { feKey :: !ByteString
  -- ^ AES key (16, 24, or 32 bytes).
  , feFileId :: !ByteString
  -- ^ Fresh random file identifier. Generate a new value for every file.
  , feAadPrefix :: !ByteString
  -- ^ Caller AAD prefix (typically empty).
  , feKeyMetadata :: !ByteString
  {- ^ Opaque KMS handle to record on the file. Round-trips
  verbatim through the encrypted-footer trailer; readers feed it
  back to their KMS to recover the key.
  -}
  }
  deriving (Show, Eq)


{- | Create a footer configuration with a fresh random file identifier.
Call once per output file, including when reusing the same encryption key.
-}
newFooterEncryption
  :: RNG.MonadRandom m
  => ByteString -> ByteString -> ByteString -> m FooterEncryption
newFooterEncryption key prefix metadata = do
  fileId <- RNG.getRandomBytes 8
  pure (FooterEncryption key fileId prefix metadata)


{- | Encode a 'FooterEncryption' as the @FileCryptoMetaData@ thrift
struct the encrypted-footer mode prepends to the encrypted footer
(parquet-format Encryption.md §5.2). Only the algorithm + key
metadata + AAD prefix are emitted; the encryption itself is in the
module that follows this struct.
-}
fileCryptoMetaDataToThrift :: Enc.EncryptionAlgorithm -> FooterEncryption -> TV.Value
fileCryptoMetaDataToThrift algorithm fe =
  TV.Struct $
    V.fromList
      [ FileCryptoMetaData_EncryptionAlgorithm (encryptionAlgorithmFields algorithm fe)
      , FileCryptoMetaData_KeyMetadata (feKeyMetadata fe)
      ]


-- The file algorithm selects GCM versus CTR for all encrypted page
-- bodies; headers, indexes, and the footer always use GCM.
encryptionAlgorithmFields
  :: Enc.EncryptionAlgorithm -> FooterEncryption -> V.Vector (Int16, TV.Value)
encryptionAlgorithmFields algorithm fe =
  V.singleton $ case algorithm of
    Enc.AesGcmV1 -> EncryptionAlgorithm_AesGcmV1 (aesGcmV1Fields fe)
    Enc.AesGcmCtrV1 -> EncryptionAlgorithm_AesGcmCtrV1 (aesGcmV1Fields fe)


aesGcmV1Fields :: FooterEncryption -> V.Vector (Int16, TV.Value)
aesGcmV1Fields fe =
  V.fromList $
    concat
      [ optNonEmpty (feAadPrefix fe) AesGcmV1_AadPrefix
      , optNonEmpty (feFileId fe) AesGcmV1_AadFileUnique
      ]
  where
    optNonEmpty bs mk
      | BS.null bs = []
      | otherwise = [mk bs]


{- | Build a Parquet file with an /encrypted footer/. Identical to
'buildParquetFileWithIndex' for the row-group + bloom / offset /
column index regions, but the trailing footer is wrapped as a
single AES-GCM module under @ModuleFooter@ AAD and the file ends
with the @PARE@ magic instead of @PAR1@.

This is the parquet-format "encrypted-footer" file mode (the
alternative to the "plaintext-footer" mode where the column
payloads are encrypted but the footer stays in the clear).
Iceberg's encryption configuration emits encrypted-footer files
when @write.encryption.encrypt-footer@ is true.
-}
buildParquetFileWithIndexEncryptedFooter
  :: RNG.MonadRandom m
  => FooterEncryption
  -> V.Vector SchemaElement
  -> V.Vector (V.Vector ColumnData)
  -> V.Vector (V.Vector ColumnAux)
  -> m (Either String ByteString)
buildParquetFileWithIndexEncryptedFooter fe = buildParquetFileRandom (Just fe)


{- | Build with fresh random module nonces. Encryption errors return 'Left';
an invalid key never falls back to plaintext. Pure callers must use
'buildParquetFileWithIndex' without encryption.
-}
buildParquetFileWithIndexIO
  :: RNG.MonadRandom m
  => V.Vector SchemaElement
  -> V.Vector (V.Vector ColumnData)
  -> V.Vector (V.Vector ColumnAux)
  -> m (Either String ByteString)
buildParquetFileWithIndexIO = buildParquetFileRandom Nothing


buildParquetFileRandom
  :: RNG.MonadRandom m
  => Maybe FooterEncryption
  -> V.Vector SchemaElement
  -> V.Vector (V.Vector ColumnData)
  -> V.Vector (V.Vector ColumnAux)
  -> m (Either String ByteString)
buildParquetFileRandom footer schema groups auxes =
  case validateEncryption of
    Left err -> pure (Left err)
    Right () -> do
      nonces <- V.mapM (V.mapM (const (RNG.getRandomBytes 72))) groups
      footerNonce <- RNG.getRandomBytes 12
      pure
        ( Right
            ( buildParquetFileWithIndex'
                footer
                (Just (EncryptionNonces nonces))
                footerNonce
                schema
                groups
                auxes
            )
        )
  where
    validKey key
      | BS.length key `elem` [16, 24, 32] = Right ()
      | otherwise = Left "Parquet.Write: AES key must contain 16, 24, or 32 bytes"
    validateEncryption = do
      validateRequiredLayout schema groups auxes
      maybe (Right ()) (validKey . feKey) footer
      case footer of
        Just fe | BS.null (feFileId fe) -> Left "Parquet.Write: encrypted footer requires a fresh random file identifier"
        _ -> Right ()
      V.imapM_ (\rg cols -> V.imapM_ (checkColumn rg) cols) auxes
      -- Reject an oversized schema or KMS/AAD metadata before nonces, page
      -- buffers, UTF8 conversion, or encryption are allocated. Final metadata
      -- is measured again once row-group offsets and crypto fields are known.
      case footer of
        Nothing -> Right ()
        Just fe -> do
          let fm = FileMetadata 1 schema 0 V.empty Nothing Nothing Nothing
          _ <-
            encryptedFooterSizes
              (fileCryptoMetaDataToThrift (fileEncryptionAlgorithm auxes) fe)
              (fileMetadataToThrift fm)
          Right ()
    checkColumn rg col aux = case caEncryption aux of
      Nothing -> Right ()
      Just ce
        | rg > 32767 || col > 32767 -> Left "Parquet.Write: encryption ordinal exceeds signed 16-bit range"
        | otherwise -> do
            validKey (ceKey ce)
            case footer of
              Nothing -> Left "Parquet.Write: encrypted columns require an encrypted footer; plaintext-footer signing is not supported"
              Just fe
                | ceKey ce /= feKey fe -> Left "Parquet.Write: separate column keys are not supported"
                | ceFileId ce /= feFileId fe || ceAadPrefix ce /= feAadPrefix fe ->
                    Left "Parquet.Write: column and footer AAD contexts must match"
                | ceAlgorithm ce /= fileEncryptionAlgorithm auxes ->
                    Left "Parquet.Write: encrypted columns must use one file-level algorithm"
                | otherwise -> Right ()


fileEncryptionAlgorithm :: V.Vector (V.Vector ColumnAux) -> Enc.EncryptionAlgorithm
fileEncryptionAlgorithm auxes =
  fromMaybe Enc.AesGcmV1 $
    V.foldl'
      ( \found cols -> case found of
          Just _ -> found
          Nothing ->
            V.foldl'
              ( \acc aux -> case acc of
                  Just _ -> acc
                  Nothing -> ceAlgorithm <$> caEncryption aux
              )
              Nothing
              cols
      )
      Nothing
      auxes


-- The public metadata model omits crypto fields. Add the standardized
-- footer-key marker and row-group ordinals at the Thrift serialization
-- seam, after every index/bloom offset has reached its final value.
footerMetadataWithCrypto
  :: V.Vector (V.Vector ColumnAux) -> FileMetadata -> TV.Value
footerMetadataWithCrypto auxes fm = case fileMetadataToThrift fm of
  TV.Struct fields -> TV.Struct (V.map rewriteFile fields)
  value -> value
  where
    rewriteFile (FileMetadata_RowGroups groups) =
      FileMetadata_RowGroups (V.imap rewriteGroup groups)
    rewriteFile field = field
    rewriteGroup rg (TV.Struct fields) =
      TV.Struct $
        V.snoc (V.map (rewriteGroupField rg) fields) (RowGroup_Ordinal (fromIntegral rg))
    rewriteGroup _ value = value
    rewriteGroupField rg (RowGroup_Columns cols) =
      RowGroup_Columns (V.imap (rewriteColumn rg) cols)
    rewriteGroupField _ field = field
    rewriteColumn rg col (TV.Struct fields)
      | rg < V.length auxes
      , col < V.length (auxes V.! rg)
      , Just _ <- caEncryption (auxes V.! rg V.! col) =
          TV.Struct
            ( V.snoc
                fields
                ( ColumnChunk_CryptoMetadata
                    (V.singleton (ColumnCryptoMetaData_EncryptionWithFooterKey V.empty))
                )
            )
    rewriteColumn _ _ value = value


-- Framed GCM adds a four-byte length, twelve-byte nonce and sixteen-byte
-- authentication tag. The outer footer length covers this and crypto metadata.
data EncryptedFooterSizes = EncryptedFooterSizes
  { efsCryptoBytes :: {-# UNPACK #-} !Int
  , efsPlainBytes :: {-# UNPACK #-} !Int
  }


encryptedFooterSizes :: TV.Value -> TV.Value -> Either String EncryptedFooterSizes
encryptedFooterSizes cryptoValue plainValue = do
  let limit = fromInteger (min (toInteger (maxBound :: Word32)) (toInteger (maxBound :: Int) - 8))
  cryptoSize <- compactEncodedSizeBounded (limit - 32) cryptoValue
  plainSize <- compactEncodedSizeBounded (limit - 32 - cryptoSize) plainValue
  Right (EncryptedFooterSizes cryptoSize plainSize)


buildParquetFileWithIndex'
  :: Maybe FooterEncryption
  -> Maybe EncryptionNonces
  -> ByteString
  -> V.Vector SchemaElement
  -> V.Vector (V.Vector ColumnData)
  -> V.Vector (V.Vector ColumnAux)
  -> ByteString
buildParquetFileWithIndex' mFootEnc nonces footerNonce schema rowGroups auxes =
  let
    -- Per-column page bytes plus uncompressed size, both required to
    -- populate ColumnMetadata. Unsupported codecs are rejected by validation.
    !encodedRGs = V.imap encodeRG rowGroups
    !rowGroupBytes = concatMap (concatMap ecParts . V.toList) (V.toList encodedRGs)
    !rgBytesLen = sum (map BS.length rowGroupBytes)
    !startOfData = 4 :: Int
    !startOfBloom = startOfData + rgBytesLen

    (!rgMetasBase, _) = V.ifoldl' buildRG (V.empty, startOfData) encodedRGs

    (!rgMetasBloom, !bloomBytes, !endOfBloom) =
      layoutBlooms nonces rgMetasBase auxes startOfBloom

    (!rgMetasOff, !offBytes, !endOfOff) =
      layoutOffsetIndex
        nonces
        rgMetasBloom
        auxes
        endOfBloom

    (!rgMetasCol, !colBytes, _endOfCol) =
      layoutColumnIndex nonces rgMetasOff auxes endOfOff

    !totalRows = V.foldl' (\a rg -> a + rgNumRows rg) 0 rgMetasCol
    !fm =
      FileMetadata
        { fmVersion = 1
        , fmSchema = schema
        , fmNumRows = totalRows
        , fmRowGroups = rgMetasCol
        , fmCreatedBy = Just "wireform"
        , fmKeyValueMetadata = Nothing
        , fmColumnOrders = Just (V.replicate (V.length leaves) TypeDefinedOrder)
        }
    -- Encrypted-footer mode (parquet-format Encryption.md §5.4):
    --
    --   <FileCryptoMetaData thrift>
    --   <encrypted footer module: nonce || ct || tag>
    --   <combined-length: i32 LE>     -- of the two blobs above
    --   PARE
    --
    -- The footer module retains its section 5.1 length framing; the
    -- trailing combined length also includes FileCryptoMetaData.
    !footerBytes = case mFootEnc of
      Nothing -> writeFooter fm
      Just fe -> either error id $ do
        let cryptoValue = fileCryptoMetaDataToThrift (fileEncryptionAlgorithm auxes) fe
            plainValue = footerMetadataWithCrypto auxes fm
        sizes <- encryptedFooterSizes cryptoValue plainValue
        -- Both metadata trees fit before either output buffer is allocated.
        cryptoMeta <- encodeCompactBounded (efsCryptoBytes sizes) cryptoValue
        plainThrift <- encodeCompactBounded (efsPlainBytes sizes) plainValue
        let suffix = Enc.buildAadSuffix (feFileId fe) Enc.ModuleFooter 0 0 0
            aad = Enc.buildAad (feAadPrefix fe) suffix
        encModule <- Enc.encryptGcmModuleFramedWithNonce (feKey fe) footerNonce aad plainThrift
        writeRawFooterEither parquetEncryptedMagic (cryptoMeta <> encModule)
  in
    BS.concat ((maybe parquetMagic (const parquetEncryptedMagic) mFootEnc) : rowGroupBytes ++ bloomBytes ++ offBytes ++ colBytes ++ [footerBytes])
  where
    encryptedHeaderSize ce plainBodySize bytes =
      BS.length bytes - plainBodySize - case ceAlgorithm ce of
        Enc.AesGcmV1 -> 32 -- module length + nonce + tag
        Enc.AesGcmCtrV1 -> 16 -- module length + nonce
    pageNonces rg col =
      moduleNonce nonces rg col Enc.ModuleDataPageHeader
        <> moduleNonce nonces rg col Enc.ModuleDataPage
    !leaves = V.filter (maybe False (const True) . seType) schema

    -- Encode each column's page, then compress it with the codec the
    -- caller requested via the matching ColumnAux. Returns the *final*
    -- bytes that go into the file plus the uncompressed size.
    encodeRG :: Int -> V.Vector ColumnData -> V.Vector EncodedColumn
    encodeRG rgIdx colsData =
      let !aux = if rgIdx < V.length auxes then V.unsafeIndex auxes rgIdx else V.empty
      in V.imap (encodeOne rgIdx aux) colsData

    encodeOne
      :: Int -> V.Vector ColumnAux -> Int -> ColumnData -> EncodedColumn
    encodeOne rgIdx aux cIdx cd =
      let !mAux =
            if cIdx < V.length aux
              then Just (V.unsafeIndex aux cIdx)
              else Nothing
          !codecRequested = maybe Uncompressed caCodec mAux
          !pageVer = maybe PageV1 caPageVersion mAux
          !mEnc = mAux >>= caEncryption
      in case pageVer of
           PageV1 ->
             let !body = encodeColumnDataPagePayload cd
                 (compBody, cActual) = case Compress.compressPageBytes codecRequested body of
                   Right cb -> (cb, codecRequested)
                   Left err -> error err
                 -- Re-encode the header now that the body has its
                 -- compressed size; the header records the on-disk
                 -- compressed_page_size so the reader can frame pages
                 -- without scanning forward.
                 !n = columnDataLength cd
                 !hdr' =
                   encodePageHeader
                     ( PageHeader
                         { phType =
                             PtDataPage
                               DataPageHeader
                                 { dphNumValues = fromIntegral n
                                 , dphEncoding = 0
                                 }
                         , phUncompressedPageSize = Just (fromIntegral (BS.length body))
                         , phCompressedPageSize = Just (either error id (pageInt32 "compressed page size" (BS.length compBody)))
                         }
                     )
                 !uncompSz = BS.length hdr' + BS.length body
             in case mEnc of
                  Nothing ->
                    encodedColumn [hdr', compBody] uncompSz cActual
                  Just ce ->
                    case encryptPageBytesWithNonces
                      (pageNonces rgIdx cIdx)
                      ce {ceColumnOrdinal = cIdx}
                      Enc.ModuleDataPage
                      rgIdx
                      0
                      hdr'
                      compBody of
                      Right encBytes ->
                        encodedColumn [encBytes] (BS.length body + encryptedHeaderSize ce (BS.length compBody) encBytes) cActual
                      Left err -> error ("Parquet.Write: page encryption failed: " ++ err)
           PageV2 ->
             -- For V2 the codec is applied only to the values segment,
             -- but we account for the *full* page bytes (header +
             -- segments) when recording sizes on the column metadata so
             -- that ccTotalCompressedSize / ccTotalUncompressedSize
             -- reflect on-disk reality.
             let finish codecActual page =
                   let !h = erv2Header page
                       !v = erv2Values page
                       !uncompSz = BS.length h + erv2UncompressedSize page
                   in case mEnc of
                        Nothing -> encodedColumn [h, v] uncompSz codecActual
                        Just ce -> case encryptPageBytesWithNonces
                          (pageNonces rgIdx cIdx)
                          ce {ceColumnOrdinal = cIdx}
                          Enc.ModuleDataPage
                          rgIdx
                          0
                          h
                          v of
                          Right enc ->
                            encodedColumn
                              [enc]
                              (erv2UncompressedSize page + encryptedHeaderSize ce (BS.length v) enc)
                              codecActual
                          Left err -> error ("Parquet.Write: page encryption failed: " ++ err)
             in case encodeRequiredV2 codecRequested cd of
                  Right page -> finish codecRequested page
                  Left err -> error err

    buildRG
      :: (V.Vector RowGroup, Int)
      -> Int
      -> V.Vector EncodedColumn
      -> (V.Vector RowGroup, Int)
    buildRG (!rgs, !off) rgIdx encodedCols =
      let !colsData = V.unsafeIndex rowGroups rgIdx
          (!cols, !off2) = V.ifoldl' (buildCol colsData) (V.empty, off) encodedCols
          !nRows =
            if V.null colsData
              then 0
              else fromIntegral (columnDataLength (V.unsafeIndex colsData 0))
          !rg =
            RowGroup
              { rgColumns = cols
              , rgTotalByteSize = V.foldl' (\total column -> total + fromIntegral (ecUncompressedSize column)) 0 encodedCols
              , rgNumRows = nRows
              , rgSortingColumns = Nothing
              }
      in (V.snoc rgs rg, off2)

    buildCol
      :: V.Vector ColumnData
      -> (V.Vector ColumnChunk, Int)
      -> Int
      -> EncodedColumn
      -> (V.Vector ColumnChunk, Int)
    buildCol colsData (!cs, !cOff) colIdx encoded =
      let !cd = V.unsafeIndex colsData colIdx
          !leaf = V.unsafeIndex leaves colIdx
          !sz = ecCompressedSize encoded
          !uncompSize = ecUncompressedSize encoded
          !codec = ecCodec encoded
          !cc =
            ColumnChunk
              { ccFilePath = Nothing
              , ccFileOffset = 0
              , ccMetadata =
                  Just
                    ColumnMetadata
                      { cmType = fromMaybe (columnDataParquetType cd) (seType leaf)
                      , cmEncodings = V.singleton Plain
                      , cmPathInSchema = V.singleton (seName leaf)
                      , cmCodec = codec
                      , cmNumValues = fromIntegral (columnDataLength cd)
                      , cmTotalUncompressedSize = fromIntegral uncompSize
                      , cmTotalCompressedSize = fromIntegral sz
                      , cmDataPageOffset = fromIntegral cOff
                      , cmDictionaryPageOffset = Nothing
                      , cmStatistics = Just (columnStatisticsForSchema leaf cd)
                      , cmBloomFilterOffset = Nothing
                      , cmBloomFilterLength = Nothing
                      }
              , ccOffsetIndexOffset = Nothing
              , ccOffsetIndexLength = Nothing
              , ccColumnIndexOffset = Nothing
              , ccColumnIndexLength = Nothing
              }
      in (V.snoc cs cc, cOff + sz)


-- ============================================================
-- Mixed required + optional columns
-- ============================================================

{- | Sum of the two column-data shapes the writer can emit:
plain @ColumnData@ (required leaf columns) and
@OptionalColumn@ (nullable leaf columns with definition
levels). The 'buildParquetFileMixed' entrypoint below accepts
a @V.Vector ParquetColumn@ per row group so callers can mix
nullable and non-nullable leaves in the same file without
dropping to the page-level builders.
-}
data ParquetColumn
  = -- | A required (non-null) column.
    PCRequired !ColumnData
  | {- | A nullable column. Encoded as @DATA_PAGE_V1@ with a
    definition-level stream followed by present-only values.
    -}
    PCOptional !OptionalColumn
  deriving (Show, Eq)


parquetColumnLength :: ParquetColumn -> Int
parquetColumnLength = \case
  PCRequired cd -> columnDataLength cd
  PCOptional oc -> optionalColumnLength oc


parquetColumnParquetType :: ParquetColumn -> ParquetType
parquetColumnParquetType = \case
  PCRequired cd -> columnDataParquetType cd
  PCOptional oc -> case oc of
    OptInt32 {} -> PTInt32
    OptInt64 {} -> PTInt64
    OptFloat {} -> PTFloat
    OptDouble {} -> PTDouble
    OptBool {} -> PTBoolean
    OptByteArray {} -> PTByteArray


parquetColumnStatistics :: SchemaElement -> ParquetColumn -> ColumnData -> Statistics
parquetColumnStatistics leaf source present =
  let stats = columnStatisticsForSchema leaf present
  in case source of
       PCRequired _ -> stats
       PCOptional column -> stats {statNullCount = Just (fromIntegral (optionalColumnLength column - columnDataLength present))}


{- | Page-encode one mixed column. Uncompressed @DATA_PAGE_V1@
for both variants; the simpler readers in
"Parquet.Read" expect that shape. Returns
@(pageBytes, uncompressedSize)@ so the row-group builder can
stamp the matching @ColumnMetadata@ offsets.
-}
encodeMixedPageV1 :: ParquetColumn -> ColumnData -> Either String (ByteString, Int)
encodeMixedPageV1 source present = case source of
  PCRequired cd -> do
    (hdr, body) <- encodeColumnDataPagePartsChecked cd
    Right (BS.append hdr body, BS.length hdr + BS.length body)
  PCOptional oc -> do
    pg <- encodeOptionalPagePrepared oc present
    Right (pg, BS.length pg)


{- | Compression-aware mixed page encoder. Compresses the page
/body/ (header stays plaintext) and rewrites the page header
with the new compressed_page_size so the round-trip readers
in "Parquet.Read" can decompress on the way back.
-}
encodeMixedPageV1With
  :: Compression -> ParquetColumn -> ColumnData -> Either String (ByteString, Int)
encodeMixedPageV1With Uncompressed col present = encodeMixedPageV1 col present
encodeMixedPageV1With codec col present = do
  (uncompPage, _) <- encodeMixedPageV1 col present
  -- Layout is [PageHeader thrift][body]; re-encode the header
  -- with a compressed size matching the compressed body, then
  -- stitch the two together.
  (hdr, afterHdr) <- readPageHeaderAt uncompPage 0
  let !body = BS.drop afterHdr uncompPage
  compBody <- compressPageBytes codec body
  compressedSize <- pageInt32 "compressed page size" (BS.length compBody)
  let !newHdr = hdr {phCompressedPageSize = Just compressedSize}
      !hdrBs = encodePageHeader newHdr
      !page = hdrBs <> compBody
  Right (page, BS.length hdrBs + BS.length body)


{- | Assemble a Parquet file from a flat schema + row groups
where each column is either required ('PCRequired') or
nullable ('PCOptional'). Unlike 'buildParquetFileWithIndex',
no bloom filters / page indexes / encryption are emitted
here; this is the fast path used by the Arrow <-> Parquet
bridge when any leaf column is nullable.

Output format:

@
PAR1
\<page bytes per (row-group × column)\>
\<footer thrift (FileMetadata)\>
\<footer length : i32 LE\>
PAR1
@
-}
buildParquetFileMixed
  :: V.Vector SchemaElement
  -> V.Vector (V.Vector ParquetColumn)
  -> ByteString
buildParquetFileMixed schema groups =
  either error id (buildParquetFileMixedWith Uncompressed schema groups)


-- | Encode flat required/nullable V1 pages with a uniform codec.
buildParquetFileMixedWith
  :: Compression
  -> V.Vector SchemaElement
  -> V.Vector (V.Vector ParquetColumn)
  -> Either String ByteString
buildParquetFileMixedWith codec schema groups = do
  Compress.validateCompression codec
  buildParquetFileMixedWithAux
    schema
    groups
    (V.map (V.map (const (emptyColumnAux {caCodec = codec}))) groups)


validateMixedLayout
  :: V.Vector SchemaElement
  -> V.Vector (V.Vector ParquetColumn)
  -> V.Vector (V.Vector ColumnAux)
  -> V.Vector (V.Vector ColumnData)
  -> Either String ()
validateMixedLayout schema groups auxes presentRows = do
  V.mapM_ (V.mapM_ (Compress.validateCompression . caCodec)) auxes
  V.mapM_ (V.mapM_ validateAuxIndexes) auxes
  if V.null schema || seType (V.head schema) /= Nothing
    then Left "Parquet.Write: mixed schema must start with a group root"
    else Right ()
  let leaves = V.tail schema
  if seNumChildren (V.head schema) /= Just (fromIntegral (V.length leaves))
    || V.any
      ( \leaf ->
          seType leaf == Nothing
            || seNumChildren leaf /= Nothing
            || seRepetition leaf `notElem` [Just Required, Just Optional]
            || not (hasCompatiblePhysicalType leaf)
      )
      leaves
    then Left "Parquet.Write: mixed writer needs a flat required/optional schema"
    else Right ()
  if V.length groups /= V.length auxes
    then Left "Parquet.Write: mixed auxiliary row-group count mismatch"
    else Right ()
  V.imapM_
    ( \rg cols -> do
        if V.length cols /= V.length leaves || V.length (auxes V.! rg) /= V.length cols
          then Left "Parquet.Write: mixed column/auxiliary count does not match schema"
          else Right ()
        let count = if V.null cols then 0 else parquetColumnLength (V.head cols)
        if count > fromIntegral (maxBound :: Int32)
          then Left "Parquet.Write: mixed row count exceeds signed 32-bit page limit"
          else Right ()
        V.imapM_
          ( \col values -> do
              let leaf = leaves V.! col
                  repetition = case values of PCRequired {} -> Required; PCOptional {} -> Optional
              if parquetColumnLength values /= count
                || seType leaf /= Just (parquetColumnParquetType values)
                || seRepetition leaf /= Just repetition
                then Left "Parquet.Write: mixed column length/type/nullability does not match schema"
                else
                  validateLogicalColumn leaf (presentRows V.! rg V.! col)
              case caEncryption (auxes V.! rg V.! col) of
                Just _ -> Left "Parquet.Write: mixed column encryption is not supported"
                Nothing -> Right ()
          )
          cols
    )
    groups


encodeMixedColumn :: ColumnAux -> ParquetColumn -> ColumnData -> Either String EncodedColumn
encodeMixedColumn aux values present = case caPageVersion aux of
  PageV1 -> do
    (bytes, uncompressed) <- encodeMixedPageV1With (caCodec aux) values present
    Right (encodedColumn [bytes] uncompressed (caCodec aux))
  PageV2 -> case values of
    PCRequired column -> do
      page <- encodeRequiredV2 (caCodec aux) column
      Right
        ( encodedColumn
            [erv2Header page, erv2Values page]
            (BS.length (erv2Header page) + erv2UncompressedSize page)
            (caCodec aux)
        )
    PCOptional column -> do
      page <- encodeOptionalV2Prepared (caCodec aux) column present
      Right
        ( encodedColumn
            [eov2Header page, eov2Repetitions page, eov2Definitions page, eov2Values page]
            (BS.length (eov2Header page) + eov2UncompressedSize page)
            (caCodec aux)
        )


{- | Flat required/nullable writer honoring each column's codec, page
version, bloom filter and indexes. Encryption requires the separate
encrypted writer and is explicitly rejected here.
-}
buildParquetFileMixedWithAux
  :: V.Vector SchemaElement
  -> V.Vector (V.Vector ParquetColumn)
  -> V.Vector (V.Vector ColumnAux)
  -> Either String ByteString
buildParquetFileMixedWithAux schema groups auxes =
  buildParquetFileMixedWithAuxUsing
    (\fm parts -> BS.concat (parquetMagic : parts ++ [writeFooter fm]))
    schema groups (const auxes)


-- | Assemble a validated mixed writer's pages and metadata directly. This
-- lets encrypted writers consume the metadata without serializing a footer
-- only to decode it again.
buildParquetFileMixedWithAuxUsing
  :: (FileMetadata -> [ByteString] -> a)
  -> V.Vector SchemaElement
  -> V.Vector (V.Vector ParquetColumn)
  -> (V.Vector (V.Vector ColumnData) -> V.Vector (V.Vector ColumnAux))
  -> Either String a
buildParquetFileMixedWithAuxUsing finish schema rowGroups makeAuxes = do
  let presentRows = V.map (V.map (\case PCRequired values -> values; PCOptional values -> optionalColumnPresentValues values)) rowGroups
      auxes = makeAuxes presentRows
  validateMixedLayout schema rowGroups auxes presentRows
  encodedRGs <-
    V.imapM
      ( \rg columns ->
          V.imapM
            ( \col values ->
                encodeMixedColumn (auxes V.! rg V.! col) values (presentRows V.! rg V.! col)
            )
            columns
      )
      rowGroups
  let !leaves = V.tail schema
      !allPageBytes = concatMap (concatMap ecParts . V.toList) (V.toList encodedRGs)
      !startOfData = 4 :: Int
      -- One RowGroup metadata per input group.
      (!rgMetasBase, !endOfData) =
        V.ifoldl'
          ( \(!rgs, !off) rgIdx encoded ->
              let !colsData = V.unsafeIndex rowGroups rgIdx
                  (!chunks, !off') =
                    V.ifoldl' (buildChunk colsData (presentRows V.! rgIdx)) (V.empty, off) encoded
                  !nRows =
                    if V.null colsData
                      then 0
                      else fromIntegral (parquetColumnLength (V.unsafeIndex colsData 0))
                  !rg =
                    RowGroup
                      { rgColumns = chunks
                      , rgTotalByteSize = V.foldl' (\total column -> total + fromIntegral (ecUncompressedSize column)) 0 encoded
                      , rgNumRows = nRows
                      , rgSortingColumns = Nothing
                      }
              in (V.snoc rgs rg, off')
          )
          (V.empty, startOfData)
          encodedRGs

      (!rgMetasBloom, !bloomBytes, !endOfBloom) = layoutBlooms Nothing rgMetasBase auxes endOfData
      (!rgMetasOff, !offsetBytes, !endOfOffset) = layoutOffsetIndex Nothing rgMetasBloom auxes endOfBloom
      (!rgMetas, !columnBytes, _) = layoutColumnIndex Nothing rgMetasOff auxes endOfOffset
      !totalRows = V.foldl' (\a rg -> a + rgNumRows rg) 0 rgMetas
      !fm =
        FileMetadata
          { fmVersion = 1
          , fmSchema = schema
          , fmNumRows = totalRows
          , fmRowGroups = rgMetas
          , fmCreatedBy = Just "wireform"
          , fmKeyValueMetadata = Nothing
          , fmColumnOrders = Just (V.replicate (V.length leaves) TypeDefinedOrder)
          }
  Right (finish fm (allPageBytes ++ bloomBytes ++ offsetBytes ++ columnBytes))
  where
    !leaves' = V.filter (maybe False (const True) . seType) schema

    buildChunk
      :: V.Vector ParquetColumn
      -> V.Vector ColumnData
      -> (V.Vector ColumnChunk, Int)
      -> Int
      -> EncodedColumn
      -> (V.Vector ColumnChunk, Int)
    buildChunk colsData presentColumns (!cs, !cOff) colIdx encoded =
      let !cd = V.unsafeIndex colsData colIdx
          !leaf = V.unsafeIndex leaves' colIdx
          !sz = ecCompressedSize encoded
          !uncompSize = ecUncompressedSize encoded
          !cc =
            ColumnChunk
              { ccFilePath = Nothing
              , ccFileOffset = 0
              , ccMetadata =
                  Just
                    ColumnMetadata
                      { cmType = fromMaybe (parquetColumnParquetType cd) (seType leaf)
                      , cmEncodings = case cd of
                          PCRequired {} -> V.singleton Plain
                          PCOptional {} -> V.fromList [Plain, RLE]
                      , cmPathInSchema = V.singleton (seName leaf)
                      , cmCodec = ecCodec encoded
                      , cmNumValues = fromIntegral (parquetColumnLength cd)
                      , cmTotalUncompressedSize = fromIntegral uncompSize
                      , cmTotalCompressedSize = fromIntegral sz
                      , cmDataPageOffset = fromIntegral cOff
                      , cmDictionaryPageOffset = Nothing
                      , cmStatistics = Just (parquetColumnStatistics leaf cd (presentColumns V.! colIdx))
                      , cmBloomFilterOffset = Nothing
                      , cmBloomFilterLength = Nothing
                      }
              , ccOffsetIndexOffset = Nothing
              , ccOffsetIndexLength = Nothing
              , ccColumnIndexOffset = Nothing
              , ccColumnIndexLength = Nothing
              }
      in (V.snoc cs cc, cOff + sz)


-- ============================================================
-- Size statistics
-- ============================================================

{- | Per-page unencoded byte counts for a BYTE_ARRAY column.
Used to populate 'OffsetIndex.oiUnencodedByteArrayDataBytes'
(parquet-format 2.11). For non-BYTE_ARRAY columns returns
'Nothing'.

The unit is /unencoded bytes/ — for PLAIN-encoded BYTE_ARRAY
pages that's the sum of value lengths (not including the
4-byte length prefixes the wire format adds).
-}
columnDataUnencodedByteArrayBytes
  :: ColumnData
  -> Maybe Int64
columnDataUnencodedByteArrayBytes = \case
  ColByteArray vs ->
    Just $! V.foldl' (\a bs -> a + fromIntegral (BS.length bs)) 0 vs
  _ -> Nothing


{- | Attach 'oiUnencodedByteArrayDataBytes' to an existing
'OffsetIndex' using the supplied per-page byte counts. The
vector length must match the number of page locations or the
attached field is dropped (silently — readers tolerate the
missing slot, an inconsistent one would be worse).
-}
buildOffsetIndexWithSizeStats
  :: V.Vector Int64
  -> OffsetIndex
  -> OffsetIndex
buildOffsetIndexWithSizeStats sizes oi
  | V.length sizes == V.length (oiPageLocations oi) =
      oi {oiUnencodedByteArrayDataBytes = Just sizes}
  | otherwise = oi
