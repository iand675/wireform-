{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Write Parquet files.
--
-- Provides page-level encoding, column chunk assembly, and whole-file builders.
-- The user-facing entry point is 'buildParquetFile' (or
-- 'buildParquetFileWithIndex' when emitting bloom filter / page index
-- regions). Both accept heterogeneous primitive columns via 'ColumnData'.
--
-- @
-- import qualified Data.Vector.Primitive as VP
-- import qualified Data.Vector as V
-- import Parquet.Write
-- import Parquet.Types
--
-- let schema = V.fromList
--       [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing
--       , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing
--       ]
--     vals = VP.fromList [1, 2, 3 :: Int32]
--     bs = buildParquetFile schema (V.singleton (V.singleton (ColInt32 vals)))
-- @
module Parquet.Write
  ( -- * Whole-file builders (preferred entry points)
    buildParquetFile
  , buildParquetFileWithIndex
    -- * Per-column auxiliary metadata for the indexed writer
  , ColumnAux(..)
  , emptyColumnAux
    -- * Per-column encryption (modular AES-GCM / AES-GCM-CTR)
  , ColumnEncryption(..)
  , columnEncryptionFor
  , encryptPageBytes
  , encryptPageBytesV2
  , encryptAuxModule
    -- * Encrypted-footer mode
  , FooterEncryption(..)
  , buildParquetFileWithIndexEncryptedFooter
    -- * Column data
  , ColumnData(..)
  , columnDataLength
  , columnDataPlainEncodedSize
  , columnDataParquetType
  , columnDataStatistics
  , encodeColumnDataPage
  , encodeColumnDataPageParts
    -- * Nullable columns (definition levels)
  , OptionalColumn(..)
  , optionalColumnLength
  , optionalColumnNullCount
  , optionalColumnPresentValues
  , encodeOptionalColumnPage
    -- * Dictionary encoding
  , Dictionary(..)
  , buildDictionary
  , encodeDictPage
  , encodeDictDataPage
    -- * Data page version (V1 vs V2)
  , PageVersion(..)
  , encodeColumnDataPageV2
  , encodeColumnDataPageV2Parts
  , encodeOptionalColumnPageV2
  , encodeOptionalColumnPageV2Parts
    -- * Page / footer building blocks (used by composite writers)
  , writeParquetFile
  , encodePageHeader
  , assembleColumnChunk
    -- * Mixed required + optional columns (nullable-aware)
  , ParquetColumn (..)
  , parquetColumnLength
  , parquetColumnParquetType
  , buildParquetFileMixed
  , buildParquetFileMixedWith
    -- * Size statistics
  , columnDataUnencodedByteArrayBytes
  , buildOffsetIndexWithSizeStats
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import Foreign.Ptr (castPtr)
import Data.Bits (setBit, zeroBits)
import Data.Int (Int16, Int32, Int64)
import qualified Data.Map.Strict as Map
import Control.Monad.ST (runST)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as MVP
import Data.Word (Word8, Word32)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff, pokeElemOff)
import GHC.Float (castFloatToWord32, castDoubleToWord64)
import GHC.Float (castDoubleToWord64, castFloatToWord32)

import Parquet.BloomFilter (Sbbf, encodeBloomFilter, sbbfNumBytes)
import qualified Parquet.BloomFilter as BF
import qualified Parquet.Compress as Compress
import qualified Parquet.Encryption as Enc
import qualified Parquet.LevelsEncode as LE
import Parquet.Footer
  ( fileMetadataToThrift
  , parquetEncryptedMagic
  , parquetMagic
  , writeFooter
  , writeRawFooter
  )
import Parquet.PageIndex
  ( encodeColumnIndex
  , encodeOffsetIndex
  )
import Parquet.Types
  ( ColumnIndex (..)
  , OffsetIndex (..)
  )
import Parquet.Page
  ( DataPageHeader (..)
  , DataPageHeaderV2 (..)
  , DictionaryPageHeader (..)
  , PageHeader (..)
  , PageType (..)
  , pageTypeTag
  , readPageHeaderAt
  )
import Parquet.Compress (compressPageBytes)
import Parquet.Thrift.Schema
import Parquet.Types
  ( ColumnChunk (..)
  , ColumnMetadata (..)
  , Compression (..)
  , Encoding (..)
  , FileMetadata (..)
  , ParquetType (..)
  , RowGroup (..)
  , SchemaElement (..)
  , Statistics (..)
  )
import Thrift.Encode (encodeCompact)
import qualified Thrift.Value as TV

-- | Assemble a complete Parquet file from pre-computed metadata and encoded
-- column chunk data. Each inner vector is one row group's column chunks
-- (already-encoded pages).
writeParquetFile :: FileMetadata -> V.Vector (V.Vector ByteString) -> ByteString
writeParquetFile fm rowGroupData = BL.toStrict $ B.toLazyByteString $
  B.byteString parquetMagic
  <> V.foldl' (\b rg -> V.foldl' (\b2 col -> b2 <> B.byteString col) b rg) mempty rowGroupData
  <> B.byteString (writeFooter fm)

writeFloatLE :: Float -> B.Builder
writeFloatLE = B.word32LE . castFloatToWord32

writeDoubleLE :: Double -> B.Builder
writeDoubleLE = B.word64LE . castDoubleToWord64

mkPlainDataPageHeader :: Int32 -> Int32 -> PageHeader
mkPlainDataPageHeader numValues bodySize = PageHeader
  { phType = PtDataPage DataPageHeader
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
pageHeaderToThrift hdr = TV.Struct $ V.fromList $ concat
  [ [ PageHeader_Type (pageTypeTag (phType hdr)) ]
  , optField (phUncompressedPageSize hdr) PageHeader_UncompressedSize
  , optField (phCompressedPageSize hdr)   PageHeader_CompressedSize
  , case phType hdr of
      PtDataPage dph      -> [ PageHeader_DataPageHeader
                                 (dataPageHeaderFields dph) ]
      PtDictionaryPage dk -> [ PageHeader_DictionaryPageHeader
                                 (dictPageHeaderFields dk) ]
      PtDataPageV2 v2     -> [ PageHeader_DataPageHeaderV2
                                 (dataPageHeaderV2Fields v2) ]
      PtIndexPage         -> []
  ]

dataPageHeaderFields :: DataPageHeader -> V.Vector (Int16, TV.Value)
dataPageHeaderFields dph = V.fromList
  [ DataPageHeader_NumValues                   (dphNumValues dph)
  , DataPageHeader_Encoding                    (dphEncoding  dph)
    -- Both fields are /required/ in parquet.thrift's DataPageHeader.
    -- For required (max-def-level=0) columns the encoded level data
    -- is zero bytes, but the encoding still has to be declared or
    -- strict readers reject the page header. RLE (3) is what
    -- parquet-mr / arrow-cpp stamp here, so we mirror that.
  , DataPageHeader_DefinitionLevelEncoding     3
  , DataPageHeader_RepetitionLevelEncoding     3
  ]

dictPageHeaderFields :: DictionaryPageHeader -> V.Vector (Int16, TV.Value)
dictPageHeaderFields dk = V.fromList
  [ DictionaryPageHeader_NumValues (dictNumValues dk)
  , DictionaryPageHeader_Encoding  (dictEncoding  dk)
  ]

dataPageHeaderV2Fields :: DataPageHeaderV2 -> V.Vector (Int16, TV.Value)
dataPageHeaderV2Fields v2 = V.fromList
  [ DataPageHeaderV2_NumValues (dph2NumValues v2)
  , DataPageHeaderV2_NumNulls  (dph2NumNulls  v2)
  , DataPageHeaderV2_NumRows   (dph2NumRows   v2)
  , DataPageHeaderV2_Encoding  (dph2Encoding  v2)
  , DataPageHeaderV2_DefinitionLevelsByteLength (dph2DefLevelsLen v2)
  , DataPageHeaderV2_RepetitionLevelsByteLength (dph2RepLevelsLen v2)
  , DataPageHeaderV2_IsCompressed (dph2IsCompressed v2)
  ]

-- | Concatenate pre-encoded pages into a single column chunk. Currently only
-- @Uncompressed@ is supported for writing; pages must already be encoded with
-- their headers.
assembleColumnChunk :: Compression -> [ByteString] -> ByteString
assembleColumnChunk _codec pages = mconcat pages

-- ============================================================
-- DATA_PAGE_V2
-- ============================================================

-- | Encode an 'OptionalColumn' as a single @DATA_PAGE_V2@.
--
-- Layout (per the Parquet spec):
--
-- @
-- <repetition_levels>   -- length given by header.repetition_levels_byte_length
--                       -- always uncompressed; we emit zero bytes for
--                       -- non-repeated columns.
-- <definition_levels>   -- length given by header.definition_levels_byte_length
--                       -- always uncompressed; max def-level is 1 for
--                       -- optional columns.
-- <values>              -- compressed iff codec /= Uncompressed; PLAIN encoded.
-- @
--
-- The level segments are RLE-hybrid encoded *without* the 4-byte length
-- prefix that V1 uses. The header records both segment lengths and an
-- @is_compressed@ flag.
encodeOptionalColumnPageV2 :: Compression -> OptionalColumn -> Either String ByteString
encodeOptionalColumnPageV2 codec oc = do
  parts <- encodeOptionalColumnPageV2Parts codec oc
  Right (concatV2Parts parts)

-- | Like 'encodeOptionalColumnPageV2' but returns the four byte
-- segments separately so callers (in particular the encrypted writer
-- path) can apply per-module ciphers. The tuple is
-- @(pageHeader, repLevels, defLevels, values)@.
encodeOptionalColumnPageV2Parts
  :: Compression
  -> OptionalColumn
  -> Either String (ByteString, ByteString, ByteString, ByteString)
encodeOptionalColumnPageV2Parts codec oc = do
  let !defs = VP.fromList
        [ if isJust v then 1 else 0
        | v <- presenceList oc
        ]
      !defStream = LE.encodeRLEHybrid 1 defs
      !defLen = BS.length defStream
      !repStream = BS.empty
      !repLen = 0 :: Int
      !valuesRaw = encodeColumnDataPagePayload (optionalColumnPresentValues oc)
      !uncompValuesLen = BS.length valuesRaw
  (valuesBs, codecActual) <- case Compress.compressPageBytes codec valuesRaw of
        Right cb -> Right (cb, codec)
        Left  e  -> Left e
  let !nVals = optionalColumnLength oc
      !nNulls = sum (map (\v -> if isJust v then 0 else 1) (presenceList oc)) :: Int
      !nRows = nVals  -- non-repeated => num_rows == num_values
      !uncompPageSize = repLen + defLen + uncompValuesLen
      !compPageSize   = repLen + defLen + BS.length valuesBs
      !hdrBytes = encodePageHeader PageHeader
        { phType = PtDataPageV2 DataPageHeaderV2
            { dph2NumValues    = fromIntegral nVals
            , dph2NumNulls     = fromIntegral nNulls
            , dph2NumRows      = fromIntegral nRows
            , dph2Encoding     = 0 -- PLAIN
            , dph2DefLevelsLen = fromIntegral defLen
            , dph2RepLevelsLen = fromIntegral repLen
            , dph2IsCompressed = codecActual /= Uncompressed
            }
        , phUncompressedPageSize = Just (fromIntegral uncompPageSize)
        , phCompressedPageSize   = Just (fromIntegral compPageSize)
        }
  Right (hdrBytes, repStream, defStream, valuesBs)
  where
    isJust (Just _) = True
    isJust Nothing  = False

    presenceList :: OptionalColumn -> [Maybe ()]
    presenceList = \case
      OptInt32 v     -> map void' (V.toList v)
      OptInt64 v     -> map void' (V.toList v)
      OptFloat v     -> map void' (V.toList v)
      OptDouble v    -> map void' (V.toList v)
      OptBool v      -> map void' (V.toList v)
      OptByteArray v -> map void' (V.toList v)

    void' Nothing  = Nothing
    void' (Just _) = Just ()

-- | Concatenate a @V2 parts tuple@ in spec order.
concatV2Parts :: (ByteString, ByteString, ByteString, ByteString) -> ByteString
concatV2Parts (h, r, d, v) = BS.concat [h, r, d, v]

-- | Encode a required (non-null) 'ColumnData' as a single
-- @DATA_PAGE_V2@. No def/rep level streams are emitted; only the values
-- segment, compressed if the caller requested it.
encodeColumnDataPageV2 :: Compression -> ColumnData -> Either String ByteString
encodeColumnDataPageV2 codec cd = do
  parts <- encodeColumnDataPageV2Parts codec cd
  Right (concatV2Parts parts)

-- | Like 'encodeColumnDataPageV2' but returns the four parts
-- separately. For required columns the rep / def segments are empty
-- so this is just @(pageHeader, "", "", values)@.
encodeColumnDataPageV2Parts
  :: Compression
  -> ColumnData
  -> Either String (ByteString, ByteString, ByteString, ByteString)
encodeColumnDataPageV2Parts codec cd = do
  let !valuesRaw = encodeColumnDataPagePayload cd
      !uncompValuesLen = BS.length valuesRaw
  (valuesBs, codecActual) <- case Compress.compressPageBytes codec valuesRaw of
        Right cb -> Right (cb, codec)
        Left  e  -> Left e
  let !nVals = columnDataLength cd
      !uncompPageSize = uncompValuesLen
      !compPageSize   = BS.length valuesBs
      !hdrBytes = encodePageHeader PageHeader
        { phType = PtDataPageV2 DataPageHeaderV2
            { dph2NumValues    = fromIntegral nVals
            , dph2NumNulls     = 0
            , dph2NumRows      = fromIntegral nVals
            , dph2Encoding     = 0
            , dph2DefLevelsLen = 0
            , dph2RepLevelsLen = 0
            , dph2IsCompressed = codecActual /= Uncompressed
            }
        , phUncompressedPageSize = Just (fromIntegral uncompPageSize)
        , phCompressedPageSize   = Just (fromIntegral compPageSize)
        }
  Right (hdrBytes, BS.empty, BS.empty, valuesBs)

-- ============================================================
-- Page / column statistics
-- ============================================================

-- | A strict pair, used for fused single-pass min/max folds in the
-- @statisticsFor*@ family. Avoids allocating a tuple per step.
data MinMax a = MinMax !a !a

-- | Single-pass min/max fold over a 'VP.Vector'. Replaces the
-- two-pass @VP.foldl1' min vs ; VP.foldl1' max vs@ shape used by
-- statistics computation; saves one full sweep of the column on
-- every write.
{-# INLINE minMaxVP #-}
minMaxVP :: (VP.Prim a, Ord a) => VP.Vector a -> MinMax a
minMaxVP vs =
  let !v0 = VP.unsafeIndex vs 0
  in VP.foldl' step (MinMax v0 v0) (VP.unsafeSlice 1 (VP.length vs - 1) vs)
  where
    step (MinMax !mn !mx) !x = MinMax (min mn x) (max mx x)

-- | Single-pass min/max fold over a 'V.Vector'.
{-# INLINE minMaxV #-}
minMaxV :: Ord a => V.Vector a -> MinMax a
minMaxV vs =
  let !v0 = V.unsafeIndex vs 0
  in V.foldl' step (MinMax v0 v0) (V.unsafeSlice 1 (V.length vs - 1) vs)
  where
    step (MinMax !mn !mx) !x = MinMax (min mn x) (max mx x)

-- | Compute Parquet 'Statistics' for an @INT32@ column.
--
-- Encodes @min_value@ / @max_value@ as little-endian @INT32@ per the
-- spec (PLAIN encoding for variable-length types is the same except
-- for byte arrays).  Both legacy @min@/@max@ and the modern
-- @min_value@/@max_value@ slots are populated.
statisticsForInt32 :: VP.Vector Int32 -> Statistics
statisticsForInt32 vs
  | VP.null vs = emptyStats
  | otherwise =
      let !(MinMax mn mx) = minMaxVP vs
          encMin = i32LE mn
          encMax = i32LE mx
      in Statistics
           { statMin = Just encMin
           , statMax = Just encMax
           , statNullCount = Just 0
           , statDistinctCount = Nothing
           , statMinValue = Just encMin
           , statMaxValue = Just encMax
           }

-- | Compute Parquet 'Statistics' for an @INT64@ column (LE i64 min/max).
statisticsForInt64 :: VP.Vector Int64 -> Statistics
statisticsForInt64 vs
  | VP.null vs = emptyStats
  | otherwise =
      let !(MinMax mn mx) = minMaxVP vs
          encMin = i64LE mn
          encMax = i64LE mx
      in Statistics
           { statMin = Just encMin
           , statMax = Just encMax
           , statNullCount = Just 0
           , statDistinctCount = Nothing
           , statMinValue = Just encMin
           , statMaxValue = Just encMax
           }

-- | Compute Parquet 'Statistics' for a @BYTE_ARRAY@ column. Values are
-- compared lexicographically (unsigned byte-by-byte).  The min/max
-- bytes are stored without their PLAIN length prefix per the spec.
statisticsForByteArray :: V.Vector ByteString -> Statistics
statisticsForByteArray vs
  | V.null vs = emptyStats
  | otherwise =
      let !(MinMax mn mx) = minMaxV vs
      in Statistics
           { statMin = Just mn
           , statMax = Just mx
           , statNullCount = Just 0
           , statDistinctCount = Nothing
           , statMinValue = Just mn
           , statMaxValue = Just mx
           }

emptyStats :: Statistics
emptyStats = Statistics Nothing Nothing (Just 0) Nothing Nothing Nothing

i32LE :: Int32 -> ByteString
i32LE v = BL.toStrict (B.toLazyByteString (B.int32LE v))

i64LE :: Int64 -> ByteString
i64LE v = BL.toStrict (B.toLazyByteString (B.int64LE v))

-- | Compute Parquet 'Statistics' for a @FLOAT@ column. Min/max are
-- compared as IEEE 754 floats then encoded as 4-byte little-endian.
statisticsForFloat :: VP.Vector Float -> Statistics
statisticsForFloat vs
  | VP.null vs = emptyStats
  | otherwise =
      let !(MinMax mn mx) = minMaxVP vs
          encMin = fLE mn
          encMax = fLE mx
      in Statistics (Just encMin) (Just encMax) (Just 0) Nothing
                   (Just encMin) (Just encMax)

-- | Compute Parquet 'Statistics' for a @DOUBLE@ column.
statisticsForDouble :: VP.Vector Double -> Statistics
statisticsForDouble vs
  | VP.null vs = emptyStats
  | otherwise =
      let !(MinMax mn mx) = minMaxVP vs
          encMin = dLE mn
          encMax = dLE mx
      in Statistics (Just encMin) (Just encMax) (Just 0) Nothing
                   (Just encMin) (Just encMax)

-- | Compute Parquet 'Statistics' for a @BOOLEAN@ column. Min is the
-- first @False@ that appears (otherwise @True@); max is the first @True@.
-- Encoded as a single byte (0 or 1) per the PLAIN spec.
statisticsForBool :: V.Vector Bool -> Statistics
statisticsForBool vs
  | V.null vs = emptyStats
  | otherwise =
      let hasFalse = V.any not vs
          hasTrue  = V.any id vs
          encMin = if hasFalse then BS.singleton 0 else BS.singleton 1
          encMax = if hasTrue  then BS.singleton 1 else BS.singleton 0
      in Statistics (Just encMin) (Just encMax) (Just 0) Nothing
                   (Just encMin) (Just encMax)

fLE :: Float -> ByteString
fLE v = BL.toStrict (B.toLazyByteString (B.word32LE (castFloatToWord32 v)))

dLE :: Double -> ByteString
dLE v = BL.toStrict (B.toLazyByteString (B.word64LE (castDoubleToWord64 v)))

-- ============================================================
-- Heterogeneous column data
-- ============================================================

-- | A single column's worth of values, tagged with its physical type.
-- Used by 'buildParquetFileTyped' / 'buildParquetFileTypedWithIndex' so
-- writers can emit any primitive type from one entry point.
data ColumnData
  = ColInt32     !(VP.Vector Int32)
  | ColInt64     !(VP.Vector Int64)
  | ColFloat     !(VP.Vector Float)
  | ColDouble    !(VP.Vector Double)
  | ColBool      !(V.Vector  Bool)
  | ColByteArray !(V.Vector ByteString)
  deriving (Show, Eq)

-- | Number of values in the column.
-- | Return the size in bytes that 'encodeColumnDataPagePayload'
-- would produce, without actually encoding. Constant time for
-- fixed-width primitives and bools, O(n) for byte-array columns
-- (one fold over the lengths). Used by the V2 page writer to
-- avoid encoding the column twice just to record the
-- uncompressed page size on the column metadata.
columnDataPlainEncodedSize :: ColumnData -> Int
columnDataPlainEncodedSize = \case
  ColInt32  v -> VP.length v * 4
  ColInt64  v -> VP.length v * 8
  ColFloat  v -> VP.length v * 4
  ColDouble v -> VP.length v * 8
  ColBool   v -> (V.length v + 7) `quot` 8
  ColByteArray v ->
    -- 4-byte LE length prefix per value plus the bytes themselves.
    V.foldl' (\acc bs -> acc + 4 + BS.length bs) 0 v

columnDataLength :: ColumnData -> Int
columnDataLength = \case
  ColInt32 v     -> VP.length v
  ColInt64 v     -> VP.length v
  ColFloat v     -> VP.length v
  ColDouble v    -> VP.length v
  ColBool v      -> V.length v
  ColByteArray v -> V.length v

-- | The Parquet physical type the column data should be written as.
columnDataParquetType :: ColumnData -> ParquetType
columnDataParquetType = \case
  ColInt32{}     -> PTInt32
  ColInt64{}     -> PTInt64
  ColFloat{}     -> PTFloat
  ColDouble{}    -> PTDouble
  ColBool{}      -> PTBoolean
  ColByteArray{} -> PTByteArray

-- | Encode the column as a single uncompressed @PLAIN@ @DATA_PAGE@.
encodeColumnDataPage :: ColumnData -> ByteString
encodeColumnDataPage cd =
  let (h, b) = encodeColumnDataPageParts cd in BS.append h b

-- | Encode a column data page as @(headerBytes, bodyBytes)@. Same
-- contents as 'encodeColumnDataPage' but split so callers (in
-- particular 'encryptPageBytes') can encrypt the two parts under
-- different module-type AADs.
encodeColumnDataPageParts :: ColumnData -> (ByteString, ByteString)
encodeColumnDataPageParts cd =
  let !body = encodeColumnDataPagePayload cd
      !n    = columnDataLength cd
      !hdr  = encodePageHeader (mkPlainDataPageHeader (fromIntegral n)
                                  (fromIntegral (BS.length body)))
   in (hdr, body)

-- | Compute Parquet 'Statistics' for the column.
columnDataStatistics :: ColumnData -> Statistics
columnDataStatistics = \case
  ColInt32 v     -> statisticsForInt32 v
  ColInt64 v     -> statisticsForInt64 v
  ColFloat v     -> statisticsForFloat v
  ColDouble v    -> statisticsForDouble v
  ColBool v      -> statisticsForBool v
  ColByteArray v -> statisticsForByteArray v

-- ============================================================
-- Nullable columns: definition levels + PLAIN values
-- ============================================================

-- | A column with optional values. Internally we store the present-flag
-- vector alongside the present-only values; readers reconstruct the
-- @Maybe@ shape via 'Parquet.Levels.materializePlainInt32Optional' and
-- friends.
data OptionalColumn
  = OptInt32     !(V.Vector (Maybe Int32))
  | OptInt64     !(V.Vector (Maybe Int64))
  | OptFloat     !(V.Vector (Maybe Float))
  | OptDouble    !(V.Vector (Maybe Double))
  | OptBool      !(V.Vector (Maybe Bool))
  | OptByteArray !(V.Vector (Maybe ByteString))
  deriving (Show, Eq)

optionalColumnLength :: OptionalColumn -> Int
optionalColumnLength = \case
  OptInt32 v     -> V.length v
  OptInt64 v     -> V.length v
  OptFloat v     -> V.length v
  OptDouble v    -> V.length v
  OptBool v      -> V.length v
  OptByteArray v -> V.length v

-- | Count 'Nothing' entries without materialising an intermediate
-- vector.
--
-- The previous shape was @V.length (V.filter (== Nothing) v)@
-- which allocates a whole new boxed vector just to count it.
-- The new shape is a single 'V.foldl'' tally — no allocation.
optionalColumnNullCount :: OptionalColumn -> Int
optionalColumnNullCount = \case
  OptInt32 v     -> countNothings v
  OptInt64 v     -> countNothings v
  OptFloat v     -> countNothings v
  OptDouble v    -> countNothings v
  OptBool v      -> countNothings v
  OptByteArray v -> countNothings v
  where
    {-# INLINE countNothings #-}
    countNothings :: V.Vector (Maybe a) -> Int
    countNothings = V.foldl' (\a x -> case x of Nothing -> a + 1; _ -> a) 0

-- | Strip nulls and return only the present values as a 'ColumnData'.
--
-- Two-pass mutable-vector build. The previous shape went
--
--   VP.fromList [x | Just x <- V.toList v]
--
-- which materialised the source vector as a list (~n cons
-- cells), built a filtered list (~present cons cells), and
-- then walked the filtered list to size + fill the destination
-- VP.Vector. The new shape is one pre-allocation +
-- single-pass write — no intermediate list at all.
optionalColumnPresentValues :: OptionalColumn -> ColumnData
optionalColumnPresentValues = \case
  OptInt32 v     -> ColInt32     (collectPrim v)
  OptInt64 v     -> ColInt64     (collectPrim v)
  OptFloat v     -> ColFloat     (collectPrim v)
  OptDouble v    -> ColDouble    (collectPrim v)
  OptBool v      -> ColBool      (collectBoxed v)
  OptByteArray v -> ColByteArray (collectBoxed v)

-- | Collect the 'Just' values of a boxed 'V.Vector' into a
-- primitive 'VP.Vector'. Two passes: count present values,
-- allocate exactly, fill in.
{-# INLINE collectPrim #-}
collectPrim :: VP.Prim a => V.Vector (Maybe a) -> VP.Vector a
collectPrim v =
  let !n = V.length v
      !present = V.foldl' (\a x -> case x of Just _ -> a + 1; _ -> a) 0 v
  in VP.create $ do
       mv <- MVP.unsafeNew present
       let go !i !j
             | i >= n    = pure ()
             | otherwise = case V.unsafeIndex v i of
                 Just x  -> do MVP.unsafeWrite mv j x; go (i + 1) (j + 1)
                 Nothing -> go (i + 1) j
       go 0 0
       pure mv

-- | Same shape as 'collectPrim' but for boxed element types.
{-# INLINE collectBoxed #-}
collectBoxed :: V.Vector (Maybe a) -> V.Vector a
collectBoxed v =
  let !n = V.length v
      !present = V.foldl' (\a x -> case x of Just _ -> a + 1; _ -> a) 0 v
  in V.create $ do
       mv <- VM.unsafeNew present
       let go !i !j
             | i >= n    = pure ()
             | otherwise = case V.unsafeIndex v i of
                 Just x  -> do VM.unsafeWrite mv j x; go (i + 1) (j + 1)
                 Nothing -> go (i + 1) j
       go 0 0
       pure mv

-- | Encode an 'OptionalColumn' as a single uncompressed @PLAIN@
-- @DATA_PAGE@ V1 carrying the definition-level stream + present-only
-- PLAIN values.
encodeOptionalColumnPage :: OptionalColumn -> ByteString
encodeOptionalColumnPage oc =
  let !defs       = presenceVector oc
      !defStream  = LE.encodeLengthPrefixedHybrid 1 defs
      !valuesBs   = encodeColumnDataPagePayload (optionalColumnPresentValues oc)
      !body       = defStream <> valuesBs
      !bodySize   = BS.length body
      !n          = optionalColumnLength oc
      !hdr        = mkPlainDataPageHeader (fromIntegral n) (fromIntegral bodySize)
   in encodePageHeader hdr <> body

-- | Build the definition-level vector for an 'OptionalColumn'
-- (1 for present, 0 for null) in a single pass over the source.
--
-- Replaces the previous @VP.fromList [if isPresent v then 1
-- else 0 | v <- map void (V.toList v)]@ shape, which built two
-- intermediate lists before allocating the destination vector.
presenceVector :: OptionalColumn -> VP.Vector Int32
presenceVector = \case
  OptInt32 v     -> presenceVecFrom v
  OptInt64 v     -> presenceVecFrom v
  OptFloat v     -> presenceVecFrom v
  OptDouble v    -> presenceVecFrom v
  OptBool v      -> presenceVecFrom v
  OptByteArray v -> presenceVecFrom v
  where
    {-# INLINE presenceVecFrom #-}
    presenceVecFrom :: V.Vector (Maybe a) -> VP.Vector Int32
    presenceVecFrom v =
      let !n = V.length v
      in VP.generate n $ \i ->
           case V.unsafeIndex v i of
             Just _  -> 1
             Nothing -> 0

-- | Encode just the PLAIN-values portion (no page header). Used inside
-- 'encodeOptionalColumnPage' so we can prepend the definition-level
-- stream and write a single page header.
--
-- Implementation notes (perf): the previous version of this
-- function went through @ByteString.Builder@ + @BL.toStrict@,
-- which (a) allocated a Builder thunk per element via
-- @VP.foldl' (\\b x -> b \<\> B.int64LE x)@, (b) materialised
-- a lazy chunk list, and (c) performed a final O(n) copy to
-- a strict ByteString. For 100k Int64 values that is ~3-4x
-- the cost of a single buffer allocation + tight @pokeElemOff@
-- loop. The current version pre-allocates one strict
-- ByteString of the exact size and writes elements directly,
-- relying on the host being little-endian for the wire-format
-- match (asserted by 'Parquet.HighLevel' on x86_64).
encodeColumnDataPagePayload :: ColumnData -> ByteString
encodeColumnDataPagePayload = \case
  ColInt32  v -> plainPrimVecBytes @Int32  4 (\p o x -> pokeByteOff p o x) v
  ColInt64  v -> plainPrimVecBytes @Int64  8 (\p o x -> pokeByteOff p o x) v
  ColFloat  v -> plainPrimVecBytes @Float  4
                   (\p o x -> pokeByteOff p o (castFloatToWord32 x)) v
  ColDouble v -> plainPrimVecBytes @Double 8
                   (\p o x -> pokeByteOff p o (castDoubleToWord64 x)) v
  ColBool   v -> plainBoolVecBytes v
  ColByteArray v -> plainByteArrayVecBytes v

-- | Encode a primitive vector as a contiguous buffer of @elemBytes@
-- bytes per element, using the host byte order. Parquet PLAIN
-- requires little-endian which matches little-endian hosts.
--
-- The poker takes (base ptr, byte offset within the buffer, value)
-- so callers can transform the value (e.g. @castFloatToWord32@)
-- before writing.
{-# INLINE plainPrimVecBytes #-}
plainPrimVecBytes
  :: forall a. VP.Prim a
  => Int                                 -- ^ size of one element in bytes
  -> (Ptr Word8 -> Int -> a -> IO ())    -- ^ poker (base, byte-off, value)
  -> VP.Vector a
  -> ByteString
plainPrimVecBytes !elemBytes poker v =
  let !n     = VP.length v
      !nByte = n * elemBytes
  in BSI.unsafeCreate nByte $ \ptr -> do
       let go !i !off
             | i >= n    = pure ()
             | otherwise = do
                 poker ptr off (VP.unsafeIndex v i)
                 go (i + 1) (off + elemBytes)
       go 0 0

-- | Encode a boxed bool vector as Parquet PLAIN BOOLEAN: 1 bit per
-- value, LSB-first, packed into bytes (the last byte is zero-padded
-- on the high end).
{-# INLINE plainBoolVecBytes #-}
plainBoolVecBytes :: V.Vector Bool -> ByteString
plainBoolVecBytes v =
  let !n        = V.length v
      !numBytes = (n + 7) `quot` 8
  in BSI.unsafeCreate numBytes $ \ptr -> do
       let goByte !b
             | b >= numBytes = pure ()
             | otherwise = do
                 let !base = b * 8
                     packBit !bit !acc
                       | bit >= 8        = acc
                       | base + bit >= n = acc
                       | otherwise =
                           let !x = V.unsafeIndex v (base + bit)
                               !acc' = if x
                                         then setBit acc bit
                                         else acc
                           in  packBit (bit + 1) acc'
                     !out = packBit 0 (zeroBits :: Word8)
                 pokeByteOff ptr b out
                 goByte (b + 1)
       goByte 0

-- | Encode a boxed bytestring vector as Parquet PLAIN BYTE_ARRAY:
-- @len :: u32 LE@ followed by @len@ bytes per value.
--
-- Single-pass: walk once to compute the total size, allocate, then
-- copy the length prefix + payload directly with @memcpy@.
{-# INLINE plainByteArrayVecBytes #-}
plainByteArrayVecBytes :: V.Vector ByteString -> ByteString
plainByteArrayVecBytes v =
  let !n     = V.length v
      !nByte = V.foldl' (\acc bs -> acc + 4 + BS.length bs) 0 v
  in BSI.unsafeCreate nByte $ \ptr -> do
       let go !i !off
             | i >= n    = pure ()
             | otherwise = do
                 let !x  = V.unsafeIndex v i
                     !lx = BS.length x
                 pokeByteOff ptr off (fromIntegral lx :: Word32)
                 BSU.unsafeUseAsCStringLen x $ \(srcPtr, _) ->
                   BSI.memcpy (ptr `plusPtr` (off + 4))
                              (castPtr srcPtr)
                              lx
                 go (i + 1) (off + 4 + lx)
       go 0 0

-- ============================================================
-- Dictionary encoding (PLAIN_DICTIONARY / RLE_DICTIONARY)
-- ============================================================

-- | A computed dictionary for a single column chunk. 'dictUniques' holds
-- the column values in dictionary order; 'dictIndices' maps each row's
-- value to its dictionary index (0-based).
data Dictionary = Dictionary
  { dictUniques :: !ColumnData
  , dictIndices :: !(VP.Vector Int32)
  } deriving (Show, Eq)

-- | Compute a dictionary for a 'ColumnData' by deduplicating the input.
-- Order of unique values follows their first appearance.
--
-- Implementation note (perf): the previous version went
--
--   orderedUniq xs0 = V.fromList (go (V.toList xs0) [])
--     where go (x:xs) acc | x \`elem\` acc = go xs acc | ...
--
-- which is O(n²) in the column length (each row scans the
-- accumulator), then walked the uniques again with
-- @lookup x (zip uniques [0 ..])@ per row to assign indices —
-- another O(n × |uniques|). For a 100k-row column with 1000
-- unique values the dictionary build was ~100 M comparisons.
--
-- Plus the original wrapped each primitive vector through
-- @V.fromList (VP.toList v)@ — a full primitive-to-list and
-- list-to-boxed conversion just to feed the generic helper.
--
-- The new shape uses a Data.Map.Strict for lookup
-- (O(log |uniques|) per row → O(n log u) total), writes the
-- index stream directly into a mutable VP.Vector, and works
-- on the source vector type without round-tripping through a
-- list.
buildDictionary :: ColumnData -> Dictionary
buildDictionary = \case
  ColInt32 v     -> primDict  v   ColInt32
  ColInt64 v     -> primDict  v   ColInt64
  ColFloat v     -> primDict  v   ColFloat
  ColDouble v    -> primDict  v   ColDouble
  ColBool v      -> boxedDict v   ColBool
  ColByteArray v -> boxedDict v   ColByteArray

-- | Dictionary builder for primitive vectors.
{-# INLINE primDict #-}
primDict
  :: (VP.Prim a, Ord a)
  => VP.Vector a
  -> (VP.Vector a -> ColumnData)
  -> Dictionary
primDict xs reify =
  let !n = VP.length xs
      (uniqRev, indices) = runST $ do
        mv <- MVP.unsafeNew n
        let go !i !dict !rev !next
              | i >= n = pure (rev, next)
              | otherwise = do
                  let !x = VP.unsafeIndex xs i
                  case Map.lookup x dict of
                    Just k  -> do
                      MVP.unsafeWrite mv i (fromIntegral k :: Int32)
                      go (i + 1) dict rev next
                    Nothing -> do
                      MVP.unsafeWrite mv i (fromIntegral next :: Int32)
                      go (i + 1) (Map.insert x next dict) (x : rev) (next + 1)
        (revFinal, _) <- go 0 Map.empty [] (0 :: Int)
        idx <- VP.unsafeFreeze mv
        pure (revFinal, idx)
  in Dictionary
       { dictUniques = reify (VP.fromList (reverse uniqRev))
       , dictIndices = indices
       }

-- | Dictionary builder for boxed vectors (Bool, ByteString).
{-# INLINE boxedDict #-}
boxedDict
  :: Ord a
  => V.Vector a
  -> (V.Vector a -> ColumnData)
  -> Dictionary
boxedDict xs reify =
  let !n = V.length xs
      (uniqRev, indices) = runST $ do
        mv <- MVP.unsafeNew n
        let go !i !dict !rev !next
              | i >= n = pure (rev, next)
              | otherwise = do
                  let !x = V.unsafeIndex xs i
                  case Map.lookup x dict of
                    Just k  -> do
                      MVP.unsafeWrite mv i (fromIntegral k :: Int32)
                      go (i + 1) dict rev next
                    Nothing -> do
                      MVP.unsafeWrite mv i (fromIntegral next :: Int32)
                      go (i + 1) (Map.insert x next dict) (x : rev) (next + 1)
        (revFinal, _) <- go 0 Map.empty [] (0 :: Int)
        idx <- VP.unsafeFreeze mv
        pure (revFinal, idx)
  in Dictionary
       { dictUniques = reify (V.fromList (reverse uniqRev))
       , dictIndices = indices
       }

-- | Encode a 'Dictionary' as a @DICTIONARY_PAGE@.
encodeDictPage :: Dictionary -> ByteString
encodeDictPage d =
  let !payload = encodeColumnDataPagePayload (dictUniques d)
      !numEntries = columnDataLength (dictUniques d)
      !hdr = PageHeader
        { phType = PtDictionaryPage DictionaryPageHeader
            { dictNumValues = fromIntegral numEntries
            , dictEncoding  = parquetEncodingPlainDictionary
            }
        , phUncompressedPageSize = Just (fromIntegral (BS.length payload))
        , phCompressedPageSize   = Just (fromIntegral (BS.length payload))
        }
   in encodePageHeader hdr <> payload

-- | Encode the 'DATA_PAGE' that consumes a dictionary (RLE-hybrid index
-- stream prefixed by a single byte recording the bit width).
encodeDictDataPage :: Dictionary -> ByteString
encodeDictDataPage d =
  let !indices = dictIndices d
      !numIndices = VP.length indices
      !uniqueCount = columnDataLength (dictUniques d)
      !bw = LE.bitWidthFor (max 0 (uniqueCount - 1))
      !indexStream = LE.encodeRLEHybrid bw indices
      !body = BS.singleton (fromIntegral bw) <> indexStream
      !hdr = PageHeader
        { phType = PtDataPage DataPageHeader
            { dphNumValues = fromIntegral numIndices
            , dphEncoding  = parquetEncodingRleDictionary
            }
        , phUncompressedPageSize = Just (fromIntegral (BS.length body))
        , phCompressedPageSize   = Just (fromIntegral (BS.length body))
        }
   in encodePageHeader hdr <> body

parquetEncodingPlainDictionary :: Int32
parquetEncodingPlainDictionary = 2

parquetEncodingRleDictionary :: Int32
parquetEncodingRleDictionary = 8

-- ============================================================
-- Indexed writer: bloom filter + page index footers
-- ============================================================

-- | Which Parquet data page header to emit for a column chunk.
--
-- - 'PageV1': @DATA_PAGE@ (legacy). Definition + repetition levels are
--   length-prefixed RLE-hybrid streams concatenated with the values
--   inside the (optionally compressed) page body.
-- - 'PageV2': @DATA_PAGE_V2@. Definition and repetition levels are NOT
--   length-prefixed and are NOT compressed. Only the values segment is
--   compressed, and the level segment lengths are recorded in the page
--   header so readers can skip directly to the data.
data PageVersion = PageV1 | PageV2
  deriving (Show, Eq)

-- | Per-column encryption knobs the writer needs at page-emit time.
--
-- Lifted out of the global 'Enc.EncryptionConfig' so a single column
-- aux record carries everything: the key, the algorithm, the file-id /
-- AAD-prefix / key-metadata bytes, and the column ordinal that goes
-- into the AAD suffix. All columns in a typical Parquet file share the
-- same algorithm and AAD-prefix; the helper 'columnEncryptionFor'
-- builds a per-column 'ColumnEncryption' from the file-wide
-- 'Enc.EncryptionConfig' so callers don't have to repeat the boilerplate.
data ColumnEncryption = ColumnEncryption
  { ceAlgorithm     :: !Enc.EncryptionAlgorithm
    -- ^ 'Enc.AesGcmV1' encrypts every page (header + body) with GCM;
    -- 'Enc.AesGcmCtrV1' encrypts data\/dictionary page bodies with CTR
    -- and everything else (page headers, column metadata, indexes) with
    -- GCM. Both algorithms produce the same on-the-wire framing.
  , ceKey           :: !BS.ByteString
    -- ^ AES key (16, 24, or 32 bytes).
  , ceFileId        :: !BS.ByteString
    -- ^ The 8-byte @aad_file_id@ for AAD construction. Padded to 8
    -- bytes by 'Enc.buildAadSuffix'.
  , ceAadPrefix     :: !BS.ByteString
    -- ^ Caller-supplied AAD prefix (typically empty).
  , ceKeyMetadata   :: !BS.ByteString
    -- ^ Opaque KMS handle to record on the column chunk; round-trips
    -- through to 'cmKeyMetadata' so readers can reconstruct the key.
  , ceColumnOrdinal :: !Int
    -- ^ Column ordinal within the row group.
  } deriving (Show, Eq)

-- | Auxiliary metadata for one column chunk that the indexed writer
-- emits alongside the data pages. Any 'Nothing' field is omitted from
-- the produced file.
data ColumnAux = ColumnAux
  { caBloomFilter  :: !(Maybe Sbbf)
    -- ^ Pre-built split-block bloom filter for this column chunk. The
    -- writer serialises it into the bloom-filter region and records the
    -- (offset, length) on the column metadata.
  , caOffsetIndex  :: !(Maybe OffsetIndex)
    -- ^ Per-page @(offset, compressed_size, first_row_index)@ entries.
    -- The writer rewrites the @plOffset@ of each 'PageLocation' so that
    -- it points at the actual page-bytes location in the assembled file.
  , caColumnIndex  :: !(Maybe ColumnIndex)
    -- ^ Per-page null/min/max statistics. Written verbatim.
  , caCodec        :: !Compression
    -- ^ Compression codec for the column-chunk page bytes. Default
    -- 'Uncompressed'; use any codec listed in 'Parquet.Compress.compressPageBytes'.
  , caPageVersion  :: !PageVersion
    -- ^ Page header version to emit. Defaults to 'PageV1'.
  , caEncryption   :: !(Maybe ColumnEncryption)
    -- ^ When 'Just', the page header + body for this column chunk are
    -- encrypted per the spec module-wise framing (nonce || ciphertext
    -- || tag for GCM modules; nonce || ciphertext for CTR modules).
    -- Defaults to 'Nothing' (plaintext).
  , caUseDictionary :: !Bool
    -- ^ When 'True', emit a @DICTIONARY_PAGE@ + @RLE_DICTIONARY@-encoded
    -- @DATA_PAGE@ for this column chunk instead of a @PLAIN@-encoded
    -- @DATA_PAGE@. The high-level writer's 'DictPolicy' decides this
    -- per-column based on a size heuristic; callers using the lower-level
    -- @buildParquetFileWithIndex@ entry point control it directly.
    -- Defaults to 'False' (PLAIN).
  } deriving (Show, Eq)

emptyColumnAux :: ColumnAux
emptyColumnAux = ColumnAux Nothing Nothing Nothing Uncompressed PageV1 Nothing False

-- | Convenience: build a 'ColumnEncryption' for a leaf column from
-- the file-wide 'Enc.EncryptionConfig'. Looks up the column key from
-- @encColumnKeys@ by the leaf @SchemaElement@'s @seName@ (if absent
-- we fall back to @ekFooterKey@ so a single-key configuration "just
-- works"); copies the algorithm / file-id / AAD-prefix / key-metadata
-- through verbatim.
--
-- Returns 'Nothing' when the configuration is 'Enc.unencrypted'
-- (empty footer key /and/ empty column-keys map).
columnEncryptionFor
  :: Enc.EncryptionConfig
  -> Text                 -- ^ leaf column name
  -> Int                  -- ^ column ordinal
  -> Maybe ColumnEncryption
columnEncryptionFor cfg colName colOrd
  | BS.null (Enc.ekFooterKey keys)
    && Map.null (Enc.ekColumnKeys keys) = Nothing
  | otherwise = Just ColumnEncryption
      { ceAlgorithm     = Enc.encAlgorithm cfg
      , ceKey           = lookupKey
      , ceFileId        = Enc.encAadFileId cfg
      , ceAadPrefix     = Enc.encAadPrefix cfg
      , ceKeyMetadata   = Enc.encKeyMetadata cfg
      , ceColumnOrdinal = colOrd
      }
  where
    keys = Enc.encKeys cfg
    lookupKey = case Map.lookup colName (Enc.ekColumnKeys keys) of
      Just k  -> k
      Nothing -> Enc.ekFooterKey keys

-- | Encrypt a single page (header || body) according to a
-- 'ColumnEncryption'. The protocol the writer follows for one page:
--
-- 1. Build the page header thrift bytes /and/ the (possibly compressed)
--    body bytes as usual.
-- 2. Compute @AAD = aad_prefix || aad_file_id || module_type || rg ||
--    col || page@ for the page header module.
-- 3. Encrypt the page header bytes with GCM (always) under that AAD,
--    yielding @nonce || ciphertext || tag@ (28 bytes of overhead).
-- 4. Compute the same AAD with @module_type = ModuleDataPage@ for the
--    body. Encrypt the body with the algorithm-appropriate cipher
--    ('Enc.AesGcmV1' uses GCM, 'Enc.AesGcmCtrV1' uses CTR).
-- 5. Concatenate @encryptedHeader || encryptedBody@. The result is
--    what gets written to disk; the only metadata change on the column
--    chunk is that @cmEncoding@ continues to advertise the underlying
--    encoding and the encryption is recorded on the column-chunk's
--    @cmKeyMetadata@.
encryptPageBytes
  :: ColumnEncryption
  -> Enc.ModuleType   -- ^ module type for the page body (DataPage, DictionaryPage)
  -> Int              -- ^ row-group ordinal
  -> Int              -- ^ page ordinal within the column chunk
  -> ByteString       -- ^ unencrypted page header bytes
  -> ByteString       -- ^ unencrypted page body bytes
  -> Either String ByteString
encryptPageBytes ce bodyModule rgOrd pageOrd hdrBytes bodyBytes = do
  let !aadHdrModule = case bodyModule of
        Enc.ModuleDataPage       -> Enc.ModuleDataPageHeader
        Enc.ModuleDictionaryPage -> Enc.ModuleDictionaryPageHeader
        other                    -> other
      !suffixHdr  = Enc.buildAadSuffix
                     (ceFileId ce) aadHdrModule
                     (fromIntegral rgOrd) (fromIntegral (ceColumnOrdinal ce)) (fromIntegral pageOrd)
      !suffixBody = Enc.buildAadSuffix
                     (ceFileId ce) bodyModule
                     (fromIntegral rgOrd) (fromIntegral (ceColumnOrdinal ce)) (fromIntegral pageOrd)
      !aadHdr  = Enc.buildAad (ceAadPrefix ce) suffixHdr
      !aadBody = Enc.buildAad (ceAadPrefix ce) suffixBody
  encHdr <- Enc.encryptGcmModuleFramed (ceKey ce) aadHdr hdrBytes
  encBody <- case ceAlgorithm ce of
    Enc.AesGcmV1    -> Enc.encryptGcmModuleFramed (ceKey ce) aadBody bodyBytes
    Enc.AesGcmCtrV1 -> Enc.encryptCtrModuleFramed (ceKey ce) bodyBytes
  Right (BS.append encHdr encBody)

-- | Encrypt a DATA_PAGE_V2 page, given the four parts produced by
-- 'encodeColumnDataPageV2Parts' / 'encodeOptionalColumnPageV2Parts'.
--
-- Per the Parquet modular-encryption spec for V2:
--
-- * The page header is GCM-encrypted under the
--   'Enc.ModuleDataPageHeader' AAD (always, regardless of which
--   algorithm the column uses for the body).
-- * The repetition and definition level segments are written
--   /unencrypted and uncompressed/, exactly as a plaintext V2 page
--   would emit them. Levels are page-internal scaffolding the
--   reader needs even before the body cipher is keyed up.
-- * The values segment is encrypted with GCM (for AesGcmV1) or CTR
--   (for AesGcmCtrV1) under the 'Enc.ModuleDataPage' AAD, matching
--   what we already do for V1.
--
-- Result: @<encrypted-header> <> <rep> <> <def> <> <encrypted-values>@.
encryptPageBytesV2
  :: ColumnEncryption
  -> Int             -- ^ row-group ordinal
  -> Int             -- ^ page ordinal within the column chunk
  -> ByteString      -- ^ pageHeader bytes (plaintext)
  -> ByteString      -- ^ repetition-levels segment
  -> ByteString      -- ^ definition-levels segment
  -> ByteString      -- ^ values segment (post-compression)
  -> Either String ByteString
encryptPageBytesV2 ce rgOrd pageOrd hdrBytes repBytes defBytes valBytes = do
  let !suffixHdr  = Enc.buildAadSuffix
                     (ceFileId ce) Enc.ModuleDataPageHeader
                     (fromIntegral rgOrd) (fromIntegral (ceColumnOrdinal ce)) (fromIntegral pageOrd)
      !suffixBody = Enc.buildAadSuffix
                     (ceFileId ce) Enc.ModuleDataPage
                     (fromIntegral rgOrd) (fromIntegral (ceColumnOrdinal ce)) (fromIntegral pageOrd)
      !aadHdr  = Enc.buildAad (ceAadPrefix ce) suffixHdr
      !aadBody = Enc.buildAad (ceAadPrefix ce) suffixBody
  encHdr  <- Enc.encryptGcmModuleFramed (ceKey ce) aadHdr hdrBytes
  encVals <- case ceAlgorithm ce of
    Enc.AesGcmV1    -> Enc.encryptGcmModuleFramed (ceKey ce) aadBody valBytes
    Enc.AesGcmCtrV1 -> Enc.encryptCtrModuleFramed (ceKey ce) valBytes
  Right (BS.concat [encHdr, repBytes, defBytes, encVals])

-- | Encrypt a single auxiliary module (bloom filter, offset index,
-- column index) with the column's GCM key under the module's AAD,
-- when 'caEncryption' is set. Returns the original payload unchanged
-- otherwise. CTR isn't used for these modules even on
-- 'AesGcmCtrV1' columns: the spec mandates GCM for everything except
-- data\/dictionary page bodies, so the algorithm flag is irrelevant
-- here.
encryptAuxModule
  :: Maybe ColumnEncryption -> Enc.ModuleType -> Int -> ByteString -> ByteString
encryptAuxModule mEnc mt rgOrd payload = case mEnc of
  Nothing -> payload
  Just ce ->
    let !suffix = Enc.buildAadSuffix
                    (ceFileId ce) mt
                    (fromIntegral rgOrd) (fromIntegral (ceColumnOrdinal ce)) 0
        !aad    = Enc.buildAad (ceAadPrefix ce) suffix
     in case Enc.encryptGcmModuleFramed (ceKey ce) aad payload of
          Right enc -> enc
          Left  _   -> payload

layoutBlooms
  :: V.Vector RowGroup
  -> V.Vector (V.Vector ColumnAux)
  -> Int                        -- ^ starting offset
  -> (V.Vector RowGroup, [ByteString], Int)
layoutBlooms rgs auxes start = go 0 start [] V.empty
  where
    go !i !off !payloads !acc
      | i >= V.length rgs = (acc, reverse payloads, off)
      | otherwise =
          let !rg = V.unsafeIndex rgs i
              !cols = rgColumns rg
              !aux  = if i < V.length auxes then V.unsafeIndex auxes i else V.empty
              (!cols', !off', !payloads') =
                V.ifoldl' (rewriteCol i aux) (V.empty, off, payloads) cols
              !rg' = rg { rgColumns = cols' }
           in go (i + 1) off' payloads' (V.snoc acc rg')

    rewriteCol
      :: Int                       -- row-group ordinal
      -> V.Vector ColumnAux
      -> (V.Vector ColumnChunk, Int, [ByteString])
      -> Int -> ColumnChunk
      -> (V.Vector ColumnChunk, Int, [ByteString])
    rewriteCol rgOrd aux (!ccs, !off, !payloads) cIdx cc =
      let mAux = if cIdx < V.length aux then Just (V.unsafeIndex aux cIdx) else Nothing
       in case mAux >>= caBloomFilter of
            Nothing -> (V.snoc ccs cc, off, payloads)
            Just bf ->
              let !raw = encodeBloomFilter bf
                  -- Encrypt the bloom filter bitset module if the
                  -- column's encrypted. We treat the encoded bloom
                  -- filter (which already includes the
                  -- length-delimited header) as a single
                  -- 'ModuleBloomFilterBitset' module per the spec's
                  -- "modular encryption" framing.
                  !bs  = encryptAuxModule (mAux >>= caEncryption)
                           Enc.ModuleBloomFilterBitset rgOrd raw
                  !bsLen = BS.length bs
                  !cm0 = fromMaybe defaultMetadata (ccMetadata cc)
                  !cm' = cm0
                    { cmBloomFilterOffset = Just (fromIntegral off)
                    , cmBloomFilterLength = Just (fromIntegral bsLen)
                    }
                  !cc' = cc { ccMetadata = Just cm' }
               in (V.snoc ccs cc', off + bsLen, bs : payloads)

    defaultMetadata = ColumnMetadata
      { cmType = PTInt32, cmEncodings = V.empty, cmPathInSchema = V.empty
      , cmCodec = Uncompressed, cmNumValues = 0
      , cmTotalUncompressedSize = 0, cmTotalCompressedSize = 0
      , cmDataPageOffset = 0
      , cmDictionaryPageOffset = Nothing
      , cmStatistics = Nothing
      , cmBloomFilterOffset = Nothing, cmBloomFilterLength = Nothing
      }

layoutOffsetIndex
  :: V.Vector RowGroup
  -> V.Vector (V.Vector ColumnAux)
  -> Int
  -> V.Vector (V.Vector ByteString)
  -> (V.Vector RowGroup, [ByteString], Int)
layoutOffsetIndex rgs auxes start _ = go 0 start [] V.empty
  where
    go !i !off !payloads !acc
      | i >= V.length rgs = (acc, reverse payloads, off)
      | otherwise =
          let !rg = V.unsafeIndex rgs i
              !cols = rgColumns rg
              !aux  = if i < V.length auxes then V.unsafeIndex auxes i else V.empty
              (!cols', !off', !payloads') =
                V.ifoldl' (rewriteCol i aux) (V.empty, off, payloads) cols
              !rg' = rg { rgColumns = cols' }
           in go (i + 1) off' payloads' (V.snoc acc rg')

    rewriteCol
      :: Int
      -> V.Vector ColumnAux
      -> (V.Vector ColumnChunk, Int, [ByteString])
      -> Int -> ColumnChunk
      -> (V.Vector ColumnChunk, Int, [ByteString])
    rewriteCol rgOrd aux (!ccs, !off, !payloads) cIdx cc =
      let mAux = if cIdx < V.length aux then Just (V.unsafeIndex aux cIdx) else Nothing
       in case mAux >>= caOffsetIndex of
            Nothing -> (V.snoc ccs cc, off, payloads)
            Just oi ->
              let !raw = encodeOffsetIndex oi
                  !bs  = encryptAuxModule (mAux >>= caEncryption)
                           Enc.ModuleOffsetIndex rgOrd raw
                  !bsLen = BS.length bs
                  !cc' = cc
                    { ccOffsetIndexOffset = Just (fromIntegral off)
                    , ccOffsetIndexLength = Just (fromIntegral bsLen)
                    }
               in (V.snoc ccs cc', off + bsLen, bs : payloads)

layoutColumnIndex
  :: V.Vector RowGroup
  -> V.Vector (V.Vector ColumnAux)
  -> Int
  -> (V.Vector RowGroup, [ByteString], Int)
layoutColumnIndex rgs auxes start = go 0 start [] V.empty
  where
    go !i !off !payloads !acc
      | i >= V.length rgs = (acc, reverse payloads, off)
      | otherwise =
          let !rg = V.unsafeIndex rgs i
              !cols = rgColumns rg
              !aux  = if i < V.length auxes then V.unsafeIndex auxes i else V.empty
              (!cols', !off', !payloads') =
                V.ifoldl' (rewriteCol i aux) (V.empty, off, payloads) cols
              !rg' = rg { rgColumns = cols' }
           in go (i + 1) off' payloads' (V.snoc acc rg')

    rewriteCol
      :: Int
      -> V.Vector ColumnAux
      -> (V.Vector ColumnChunk, Int, [ByteString])
      -> Int -> ColumnChunk
      -> (V.Vector ColumnChunk, Int, [ByteString])
    rewriteCol rgOrd aux (!ccs, !off, !payloads) cIdx cc =
      let mAux = if cIdx < V.length aux then Just (V.unsafeIndex aux cIdx) else Nothing
       in case mAux >>= caColumnIndex of
            Nothing -> (V.snoc ccs cc, off, payloads)
            Just ci ->
              let !raw = encodeColumnIndex ci
                  !bs  = encryptAuxModule (mAux >>= caEncryption)
                           Enc.ModuleColumnIndex rgOrd raw
                  !bsLen = BS.length bs
                  !cc' = cc
                    { ccColumnIndexOffset = Just (fromIntegral off)
                    , ccColumnIndexLength = Just (fromIntegral bsLen)
                    }
               in (V.snoc ccs cc', off + bsLen, bs : payloads)

-- Suppress unused-import warning when bloom-filter sizing helpers aren't
-- referenced directly in the writer (they are part of the public API).
_unusedBF :: Sbbf -> Int
_unusedBF = BF.sbbfNumBytes

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- ============================================================
-- Heterogeneous typed builders
-- ============================================================

-- | Build a complete Parquet file from a schema and one or more row
-- groups of typed column data. Each leaf in the schema (i.e. each
-- entry whose 'seType' is @Just@) must have a matching 'ColumnData'
-- column in every row group, in the same order. Produces @PAR1@
-- magic, uncompressed @PLAIN@ pages, footer, and trailing magic.
--
-- Use 'buildParquetFileWithIndex' to additionally emit bloom filter,
-- offset index, and column index regions.
buildParquetFile
  :: V.Vector SchemaElement
  -> V.Vector (V.Vector ColumnData)
  -> ByteString
buildParquetFile schema rowGroups =
  buildParquetFileWithIndex schema rowGroups
    (V.map (V.map (const emptyColumnAux)) rowGroups)

-- | Build a Parquet file with optional bloom filter, offset index,
-- column index, and per-column compression. The 'ColumnAux' parallel
-- array specifies what to emit for each column; 'emptyColumnAux'
-- omits all extras.
--
-- File layout:
--
-- @
-- PAR1                         -- 4 bytes
-- <row group bytes ...>        -- compressed PLAIN data pages
-- <bloom filter bitsets ...>   -- one per column with caBloomFilter = Just
-- <offset index thrifts ...>   -- one per column with caOffsetIndex = Just
-- <column index thrifts ...>   -- one per column with caColumnIndex = Just
-- <footer>                     -- Thrift-encoded FileMetadata
-- <footer length: int32 LE>
-- PAR1
-- @
buildParquetFileWithIndex
  :: V.Vector SchemaElement
  -> V.Vector (V.Vector ColumnData)
  -> V.Vector (V.Vector ColumnAux)
  -> ByteString
buildParquetFileWithIndex schema rowGroups auxes =
  buildParquetFileWithIndex' Nothing schema rowGroups auxes

-- | Configuration for encrypted-footer mode (see
-- 'buildParquetFileWithIndexEncryptedFooter'). Encapsulates the AES
-- key plus the AAD context so the writer can produce the
-- @ModuleFooter@ ciphertext that the parquet-format spec requires.
data FooterEncryption = FooterEncryption
  { feKey         :: !ByteString
    -- ^ AES key (16, 24, or 32 bytes).
  , feFileId      :: !ByteString
    -- ^ The 8-byte @aad_file_id@ (padded / truncated to 8 bytes).
  , feAadPrefix   :: !ByteString
    -- ^ Caller AAD prefix (typically empty).
  , feKeyMetadata :: !ByteString
    -- ^ Opaque KMS handle to record on the file. Round-trips
    -- verbatim through the encrypted-footer trailer; readers feed it
    -- back to their KMS to recover the key.
  } deriving (Show, Eq)

-- | Encode a 'FooterEncryption' as the @FileCryptoMetaData@ thrift
-- struct the encrypted-footer mode prepends to the encrypted footer
-- (parquet-format Encryption.md §5.2). Only the algorithm + key
-- metadata + AAD prefix are emitted; the encryption itself is in the
-- module that follows this struct.
fileCryptoMetaDataToThrift :: FooterEncryption -> TV.Value
fileCryptoMetaDataToThrift fe = TV.Struct $ V.fromList
  [ FileCryptoMetaData_EncryptionAlgorithm (encryptionAlgorithmFields fe)
  , FileCryptoMetaData_KeyMetadata         (feKeyMetadata fe)
  ]

-- We always emit AesGcmV1 here for the file-level algorithm: the
-- column-level CTR variant is signalled per-column via
-- ColumnCryptoMetaData and doesn't change the footer module.
encryptionAlgorithmFields :: FooterEncryption -> V.Vector (Int16, TV.Value)
encryptionAlgorithmFields fe = V.singleton
  (EncryptionAlgorithm_AesGcmV1 (aesGcmV1Fields fe))

aesGcmV1Fields :: FooterEncryption -> V.Vector (Int16, TV.Value)
aesGcmV1Fields fe = V.fromList $ concat
  [ optNonEmpty (feAadPrefix fe) AesGcmV1_AadPrefix
  , optNonEmpty (feFileId    fe) AesGcmV1_AadFileUnique
  ]
  where
    optNonEmpty bs mk
      | BS.null bs = []
      | otherwise  = [mk bs]

-- | Build a Parquet file with an /encrypted footer/. Identical to
-- 'buildParquetFileWithIndex' for the row-group + bloom / offset /
-- column index regions, but the trailing footer is wrapped as a
-- single AES-GCM module under @ModuleFooter@ AAD and the file ends
-- with the @PARE@ magic instead of @PAR1@.
--
-- This is the parquet-format "encrypted-footer" file mode (the
-- alternative to the "plaintext-footer" mode where the column
-- payloads are encrypted but the footer stays in the clear).
-- Iceberg's encryption configuration emits encrypted-footer files
-- when @write.encryption.encrypt-footer@ is true.
buildParquetFileWithIndexEncryptedFooter
  :: FooterEncryption
  -> V.Vector SchemaElement
  -> V.Vector (V.Vector ColumnData)
  -> V.Vector (V.Vector ColumnAux)
  -> ByteString
buildParquetFileWithIndexEncryptedFooter fe =
  buildParquetFileWithIndex' (Just fe)

buildParquetFileWithIndex'
  :: Maybe FooterEncryption
  -> V.Vector SchemaElement
  -> V.Vector (V.Vector ColumnData)
  -> V.Vector (V.Vector ColumnAux)
  -> ByteString
buildParquetFileWithIndex' mFootEnc schema rowGroups auxes =
  let -- Per-column page bytes plus uncompressed size, both required to
      -- populate ColumnMetadata. If a codec is requested but not built,
      -- the writer falls back to Uncompressed so callers get a usable
      -- file rather than a runtime crash.
      !encodedRGs = V.imap encodeRG rowGroups
      pageBytesOnly = V.map eccBytes
      !rowGroupBytes = concatMap (V.toList . pageBytesOnly) (V.toList encodedRGs)
      !rgBytesLen = sum (map BS.length rowGroupBytes)
      !startOfData = 4 :: Int
      !startOfBloom = startOfData + rgBytesLen

      (!rgMetasBase, _) = V.ifoldl' buildRG (V.empty, startOfData) encodedRGs

      (!rgMetasBloom, !bloomBytes, !endOfBloom) =
        layoutBlooms rgMetasBase auxes startOfBloom

      (!rgMetasOff, !offBytes, !endOfOff) =
        layoutOffsetIndex rgMetasBloom auxes endOfBloom
          (V.map pageBytesOnly encodedRGs)

      (!rgMetasCol, !colBytes, _endOfCol) =
        layoutColumnIndex rgMetasOff auxes endOfOff

      !totalRows = V.foldl' (\a rg -> a + rgNumRows rg) 0 rgMetasCol
      !fm = FileMetadata
        { fmVersion = 1
        , fmSchema = schema
        , fmNumRows = totalRows
        , fmRowGroups = rgMetasCol
        , fmCreatedBy = Just "wireform"
        , fmColumnOrders = Nothing
        }
      -- Encrypted-footer mode (parquet-format Encryption.md §5.4):
      --
      --   <FileCryptoMetaData thrift>
      --   <encrypted footer module: nonce || ct || tag>
      --   <combined-length: i32 LE>     -- of the two blobs above
      --   PARE
      --
      -- The encrypted footer module here is /unframed/ (no per-module
      -- §5.1 length prefix); the combined-length serves that role.
      -- Page / aux modules elsewhere in the file still use §5.1
      -- framing (encryptGcmModuleFramed).
      !footerBytes = case mFootEnc of
        Nothing -> writeFooter fm
        Just fe ->
          let !plainThrift = encodeCompact (fileMetadataToThrift fm)
              !suffix = Enc.buildAadSuffix (feFileId fe) Enc.ModuleFooter 0 0 0
              !aad    = Enc.buildAad (feAadPrefix fe) suffix
              !cryptoMeta = encodeCompact (fileCryptoMetaDataToThrift fe)
           in case Enc.encryptGcmModulePure (feKey fe) aad plainThrift of
                Right encModule ->
                  writeRawFooter parquetEncryptedMagic
                    (cryptoMeta <> encModule)
                Left _ -> writeFooter fm
   in BL.toStrict $ B.toLazyByteString $
        B.byteString parquetMagic
        <> mconcat (map B.byteString rowGroupBytes)
        <> mconcat (map B.byteString bloomBytes)
        <> mconcat (map B.byteString offBytes)
        <> mconcat (map B.byteString colBytes)
        <> B.byteString footerBytes
  where
    !leaves = V.filter (maybe False (const True) . seType) schema

    -- Encode each column's page, then compress it with the codec the
    -- caller requested via the matching ColumnAux. Returns the *final*
    -- bytes that go into the file plus the uncompressed size.
    encodeRG :: Int -> V.Vector ColumnData -> V.Vector EncodedColumnChunk
    encodeRG rgIdx colsData =
      let !aux = if rgIdx < V.length auxes then V.unsafeIndex auxes rgIdx else V.empty
       in V.imap (encodeOne aux) colsData

    encodeOne
      :: V.Vector ColumnAux -> Int -> ColumnData -> EncodedColumnChunk
    encodeOne aux cIdx cd =
      let !mAux = if cIdx < V.length aux
                    then Just (V.unsafeIndex aux cIdx)
                    else Nothing
          !codecRequested = maybe Uncompressed caCodec mAux
          !pageVer        = maybe PageV1 caPageVersion mAux
          !mEnc           = mAux >>= caEncryption
          !useDict        = maybe False caUseDictionary mAux
                            -- Dict encoding currently only emits via the
                            -- V1 page format; fall through to PLAIN for
                            -- V2 + dict requests.
                            && pageVer == PageV1
       in case pageVer of
            _ | useDict -> encodeDictV1 cd codecRequested mEnc
            PageV1 ->
              let (hdr, body)         = encodeColumnDataPageParts cd
                  (compBody, cActual) = case Compress.compressPageBytes codecRequested body of
                    Right cb -> (cb, codecRequested)
                    Left _   -> (body, Uncompressed)
                  -- Re-encode the header now that the body has its
                  -- compressed size; the header records the on-disk
                  -- compressed_page_size so the reader can frame pages
                  -- without scanning forward.
                  !n      = columnDataLength cd
                  !hdr'   = encodePageHeader (PageHeader
                              { phType = PtDataPage DataPageHeader
                                  { dphNumValues = fromIntegral n
                                  , dphEncoding  = 0
                                  }
                              , phUncompressedPageSize = Just (fromIntegral (BS.length body))
                              , phCompressedPageSize   = Just (fromIntegral (BS.length compBody))
                              })
                  !uncompSz = BS.length hdr + BS.length body
               in case mEnc of
                    Nothing ->
                      EncodedColumnChunk (BS.append hdr' compBody) uncompSz cActual Nothing False
                    Just ce ->
                      case encryptPageBytes ce Enc.ModuleDataPage 0 0 hdr' compBody of
                        Right encBytes ->
                          EncodedColumnChunk encBytes uncompSz cActual Nothing False
                        Left  _        ->
                          EncodedColumnChunk (BS.append hdr' compBody) uncompSz cActual Nothing False
            PageV2 ->
              -- For V2 the codec is applied only to the values segment,
              -- but we account for the *full* page bytes (header +
              -- segments) when recording sizes on the column metadata so
              -- that ccTotalCompressedSize / ccTotalUncompressedSize
              -- reflect on-disk reality.
              --
              -- Perf note: we used to call 'encodeColumnDataPage cd'
              -- here just to compute @uncompSz@, then 'encodeColumnDataPageV2Parts'
              -- to get the actual output — re-encoding the entire
              -- column twice. 'columnDataPlainEncodedSize' gives the
              -- same number in O(1) for primitives (and one fold
              -- for byte arrays).
              let !uncompSz   = columnDataPlainEncodedSize cd
                  partsResult = encodeColumnDataPageV2Parts codecRequested cd
                  parts = case partsResult of
                    Right p  -> p
                    Left  _  -> case encodeColumnDataPageV2Parts Uncompressed cd of
                                  Right p  -> p
                                  Left  _  -> (encodeColumnDataPage cd, BS.empty, BS.empty, BS.empty)
                  codecActual = if codecRequested == Uncompressed
                                  then Uncompressed
                                  else codecRequested
                  finalBytes = case mEnc of
                    Nothing -> concatV2Parts parts
                    Just ce ->
                      case parts of
                        (h, r, d, v) ->
                          case encryptPageBytesV2 ce 0 0 h r d v of
                            Right enc -> enc
                            Left  _   -> concatV2Parts parts
               in EncodedColumnChunk finalBytes uncompSz codecActual Nothing False

    -- | Build a dict-encoded V1 column chunk: DICTIONARY_PAGE
    -- (PLAIN-encoded uniques) followed by a DATA_PAGE with
    -- RLE_DICTIONARY-encoded indices.
    encodeDictV1
      :: ColumnData -> Compression -> Maybe ColumnEncryption
      -> EncodedColumnChunk
    encodeDictV1 cd codecRequested mEnc =
      let !dict       = buildDictionary cd
          !dictBytes  = encodeDictPage dict
          !dataBytes  = encodeDictDataPage dict
          -- The dict page goes first; the reader's
          -- 'genericReadColumnChunk' walks pages by header.
          -- Compression is handled inside encodeDict*Page
          -- (currently uncompressed; the codec-applied path
          -- can be added without changing the consumer side).
          _codecUsed  = codecRequested  -- TODO: compress dict + data page bodies
          !uncompSz   = BS.length dictBytes + BS.length dataBytes
          !chunkBytes = BS.append dictBytes dataBytes
          !dictLen    = BS.length dictBytes
          finalBytes  = case mEnc of
            Nothing -> chunkBytes
            Just _  -> chunkBytes  -- TODO: per-page encryption for dict pages
      in EncodedColumnChunk finalBytes uncompSz Uncompressed (Just dictLen) True

    buildRG
      :: (V.Vector RowGroup, Int) -> Int -> V.Vector EncodedColumnChunk
      -> (V.Vector RowGroup, Int)
    buildRG (!rgs, !off) rgIdx encodedCols =
      let !colsData = V.unsafeIndex rowGroups rgIdx
          (!cols, !off2) = V.ifoldl' (buildCol colsData) (V.empty, off) encodedCols
          !nRows = if V.null colsData
                     then 0
                     else fromIntegral (columnDataLength (V.unsafeIndex colsData 0))
          !rg = RowGroup
            { rgColumns = cols
            , rgTotalByteSize = fromIntegral (off2 - off)
            , rgNumRows = nRows
            , rgSortingColumns = Nothing
            }
      in (V.snoc rgs rg, off2)

    buildCol
      :: V.Vector ColumnData
      -> (V.Vector ColumnChunk, Int) -> Int -> EncodedColumnChunk
      -> (V.Vector ColumnChunk, Int)
    buildCol colsData (!cs, !cOff) colIdx ecc =
      let !cd = V.unsafeIndex colsData colIdx
          !leaf = V.unsafeIndex leaves colIdx
          !sz = BS.length (eccBytes ecc)
          -- For dict-encoded chunks the dict page is first,
          -- followed by the data page. cmDictionaryPageOffset
          -- points at the dict page (= cOff), and
          -- cmDataPageOffset points past it.
          !dataPageOff = case eccDictPageLen ecc of
            Just dictLen -> cOff + dictLen
            Nothing      -> cOff
          !encs = if eccUsedDict ecc
                    then V.fromList [PlainDictionary, RLEDictionary]
                    else V.singleton Plain
          !cc = ColumnChunk
            { ccFilePath = Nothing
            , ccFileOffset = fromIntegral cOff
            , ccMetadata = Just ColumnMetadata
                { cmType = fromMaybe (columnDataParquetType cd) (seType leaf)
                , cmEncodings = encs
                , cmPathInSchema = V.singleton (seName leaf)
                , cmCodec = eccCodec ecc
                , cmNumValues = fromIntegral (columnDataLength cd)
                , cmTotalUncompressedSize = fromIntegral (eccUncompSize ecc)
                , cmTotalCompressedSize = fromIntegral sz
                , cmDataPageOffset = fromIntegral dataPageOff
                , cmDictionaryPageOffset = case eccDictPageLen ecc of
                    Just _  -> Just (fromIntegral cOff)
                    Nothing -> Nothing
                , cmStatistics = Just (columnDataStatistics cd)
                , cmBloomFilterOffset = Nothing
                , cmBloomFilterLength = Nothing
                }
            , ccOffsetIndexOffset = Nothing
            , ccOffsetIndexLength = Nothing
            , ccColumnIndexOffset = Nothing
            , ccColumnIndexLength = Nothing
            }
      in (V.snoc cs cc, cOff + sz)

-- | Output of 'encodeOne' — carries enough state for 'buildCol'
-- to populate 'ColumnMetadata' correctly, including the
-- dict-page offset / encodings list when dictionary encoding
-- is in use.
data EncodedColumnChunk = EncodedColumnChunk
  { eccBytes        :: !ByteString
    -- ^ Final on-disk bytes for this column chunk (one or
    -- more pages already concatenated).
  , eccUncompSize   :: !Int
    -- ^ Uncompressed total: matches what
    -- 'cmTotalUncompressedSize' should record.
  , eccCodec        :: !Compression
    -- ^ Codec actually used (may differ from the requested
    -- codec when the requested one isn't available).
  , eccDictPageLen  :: !(Maybe Int)
    -- ^ If the chunk leads with a 'DICTIONARY_PAGE', its
    -- length in bytes — used by 'buildCol' to compute the
    -- data-page offset (= chunk offset + dict-page len) and
    -- the dictionary-page offset (= chunk offset).
  , eccUsedDict     :: !Bool
    -- ^ Drives 'cmEncodings' on the column metadata:
    -- @[PLAIN_DICTIONARY, RLE_DICTIONARY]@ when 'True',
    -- @[PLAIN]@ otherwise.
  } deriving (Show, Eq)

-- ============================================================
-- Mixed required + optional columns
-- ============================================================

-- | Sum of the two column-data shapes the writer can emit:
-- plain @ColumnData@ (required leaf columns) and
-- @OptionalColumn@ (nullable leaf columns with definition
-- levels). The 'buildParquetFileMixed' entrypoint below accepts
-- a @V.Vector ParquetColumn@ per row group so callers can mix
-- nullable and non-nullable leaves in the same file without
-- dropping to the page-level builders.
data ParquetColumn
  = PCRequired !ColumnData
    -- ^ A required (non-null) column.
  | PCOptional !OptionalColumn
    -- ^ A nullable column. Encoded as @DATA_PAGE_V1@ with a
    -- definition-level stream followed by present-only values.
  deriving (Show, Eq)

parquetColumnLength :: ParquetColumn -> Int
parquetColumnLength = \case
  PCRequired cd -> columnDataLength cd
  PCOptional oc -> optionalColumnLength oc

parquetColumnParquetType :: ParquetColumn -> ParquetType
parquetColumnParquetType = \case
  PCRequired cd -> columnDataParquetType cd
  PCOptional oc -> columnDataParquetType (optionalColumnPresentValues oc)

parquetColumnStatistics :: ParquetColumn -> Statistics
parquetColumnStatistics = \case
  PCRequired cd -> columnDataStatistics cd
  -- For nullable columns the statistics include a null count;
  -- min / max are computed over the present values, which
  -- columnDataStatistics on the stripped ColumnData handles.
  PCOptional oc ->
    let !stats = columnDataStatistics (optionalColumnPresentValues oc)
        !nulls = fromIntegral (optionalColumnNullCount oc)
    in  stats { statNullCount = Just nulls }

-- | Page-encode one mixed column. Uncompressed @DATA_PAGE_V1@
-- for both variants; the simpler readers in
-- "Parquet.Read" expect that shape. Returns
-- @(pageBytes, uncompressedSize)@ so the row-group builder can
-- stamp the matching @ColumnMetadata@ offsets.
encodeMixedPageV1 :: ParquetColumn -> (ByteString, Int)
encodeMixedPageV1 = \case
  PCRequired cd ->
    let (hdr, body) = encodeColumnDataPageParts cd
    in  (BS.append hdr body, BS.length hdr + BS.length body)
  PCOptional oc ->
    let !pg = encodeOptionalColumnPage oc
    in  (pg, BS.length pg)

-- | Compression-aware mixed page encoder. Compresses the page
-- /body/ (header stays plaintext) and rewrites the page header
-- with the new compressed_page_size so the round-trip readers
-- in "Parquet.Read" can decompress on the way back.
encodeMixedPageV1With
  :: Compression -> ParquetColumn -> Either String (ByteString, Int)
encodeMixedPageV1With Uncompressed col =
  Right (encodeMixedPageV1 col)
encodeMixedPageV1With codec col = do
  let (uncompPage, uncompSz) = encodeMixedPageV1 col
  -- Layout is [PageHeader thrift][body]; re-encode the header
  -- with a compressed size matching the compressed body, then
  -- stitch the two together.
  (hdr, afterHdr) <- readPageHeaderAt uncompPage 0
  let !body = BS.drop afterHdr uncompPage
  compBody <- compressPageBytes codec body
  let !newHdr = hdr { phCompressedPageSize = Just (fromIntegral (BS.length compBody)) }
      !hdrBs  = encodePageHeader newHdr
      !page   = hdrBs <> compBody
  Right (page, uncompSz)

-- | Assemble a Parquet file from a flat schema + row groups
-- where each column is either required ('PCRequired') or
-- nullable ('PCOptional'). Unlike 'buildParquetFileWithIndex',
-- no bloom filters / page indexes / encryption are emitted
-- here; this is the fast path used by the Arrow <-> Parquet
-- bridge when any leaf column is nullable.
--
-- Output format:
--
-- @
-- PAR1
-- \<page bytes per (row-group × column)\>
-- \<footer thrift (FileMetadata)\>
-- \<footer length : i32 LE\>
-- PAR1
-- @
buildParquetFileMixed
  :: V.Vector SchemaElement
  -> V.Vector (V.Vector ParquetColumn)
  -> ByteString
buildParquetFileMixed = buildParquetFileMixedRaw Uncompressed

-- | Compression-aware variant of 'buildParquetFileMixed'.
-- The named codec is applied to every page body and stamped on
-- every column-chunk metadata @cmCodec@. Returns 'Left' if the
-- caller picks a codec the build doesn't support (e.g. Snappy
-- without the @+snappy@ flag).
buildParquetFileMixedWith
  :: Compression
  -> V.Vector SchemaElement
  -> V.Vector (V.Vector ParquetColumn)
  -> Either String ByteString
buildParquetFileMixedWith Uncompressed schema rgs =
  Right (buildParquetFileMixed schema rgs)
buildParquetFileMixedWith codec schema rgs = do
  -- Build the file under the requested codec; the recursive
  -- 'buildParquetFileMixedRaw' threads compression through the
  -- per-page encoder + the cmCodec slot.
  let !rgPages = V.map (V.map (encodeMixedPageV1With codec)) rgs
  -- If any page failed to compress (e.g. Snappy without the
  -- flag), surface the first error.
  case V.foldr collectFirstError Nothing rgPages of
    Just e -> Left e
    Nothing -> Right (buildParquetFileMixedRaw codec schema rgs)
  where
    collectFirstError :: V.Vector (Either String a) -> Maybe String -> Maybe String
    collectFirstError vec acc = case acc of
      Just _  -> acc
      Nothing -> V.foldr (\r a -> case (a, r) of
                            (Just _, _) -> a
                            (Nothing, Left e) -> Just e
                            _ -> a) Nothing vec

-- | Internal: 'buildParquetFileMixed' parameterised by codec.
buildParquetFileMixedRaw
  :: Compression
  -> V.Vector SchemaElement
  -> V.Vector (V.Vector ParquetColumn)
  -> ByteString
buildParquetFileMixedRaw codec schema rowGroups =
  let !leaves = V.filter (maybe False (const True) . seType) schema
      -- For Uncompressed, encodeMixedPageV1 trivially returns
      -- (Right page); for any other codec we route through the
      -- compression-aware variant. Either-failures bubble up via
      -- 'error' here because 'buildParquetFileMixedWith' has
      -- already validated the codec choice.
      !encodedRGs = V.map (V.map (\c -> case encodeMixedPageV1With codec c of
                                          Right x -> x
                                          Left e  -> error e)) rowGroups
      !allPageBytes = concat
        [ V.toList (V.map fst rg) | rg <- V.toList encodedRGs ]
      !rgBytesLen = sum (map BS.length allPageBytes)
      !startOfData = 4 :: Int

      -- One RowGroup metadata per input group.
      (!rgMetas, !_) =
        V.ifoldl'
          (\(!rgs, !off) rgIdx encoded ->
              let !colsData = V.unsafeIndex rowGroups rgIdx
                  (!chunks, !off') =
                    V.ifoldl' (buildChunk colsData) (V.empty, off) encoded
                  !nRows = if V.null colsData
                             then 0
                             else fromIntegral (parquetColumnLength (V.unsafeIndex colsData 0))
                  !rg = RowGroup
                    { rgColumns = chunks
                    , rgTotalByteSize = fromIntegral (off' - off)
                    , rgNumRows = nRows
                    , rgSortingColumns = Nothing
                    }
               in (V.snoc rgs rg, off'))
          (V.empty, startOfData)
          encodedRGs

      !totalRows = V.foldl' (\a rg -> a + rgNumRows rg) 0 rgMetas
      !fm = FileMetadata
        { fmVersion   = 1
        , fmSchema    = schema
        , fmNumRows   = totalRows
        , fmRowGroups = rgMetas
        , fmCreatedBy = Just "wireform"
        , fmColumnOrders = Nothing
        }
  in BL.toStrict $ B.toLazyByteString $
       B.byteString parquetMagic
       <> mconcat (map B.byteString allPageBytes)
       <> B.byteString (writeFooter fm)
  where
    !leaves' = V.filter (maybe False (const True) . seType) schema

    buildChunk
      :: V.Vector ParquetColumn
      -> (V.Vector ColumnChunk, Int) -> Int -> (ByteString, Int)
      -> (V.Vector ColumnChunk, Int)
    buildChunk colsData (!cs, !cOff) colIdx (pageBs, uncompSize) =
      let !cd = V.unsafeIndex colsData colIdx
          !leaf = V.unsafeIndex leaves' colIdx
          !sz = BS.length pageBs
          !cc = ColumnChunk
            { ccFilePath = Nothing
            , ccFileOffset = fromIntegral cOff
            , ccMetadata = Just ColumnMetadata
                { cmType = fromMaybe (parquetColumnParquetType cd) (seType leaf)
                , cmEncodings = V.singleton Plain
                , cmPathInSchema = V.singleton (seName leaf)
                , cmCodec = codec
                , cmNumValues = fromIntegral (parquetColumnLength cd)
                , cmTotalUncompressedSize = fromIntegral uncompSize
                , cmTotalCompressedSize = fromIntegral sz
                , cmDataPageOffset = fromIntegral cOff
                , cmDictionaryPageOffset = Nothing
                , cmStatistics = Just (parquetColumnStatistics cd)
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

-- | Per-page unencoded byte counts for a BYTE_ARRAY column.
-- Used to populate 'OffsetIndex.oiUnencodedByteArrayDataBytes'
-- (parquet-format 2.11). For non-BYTE_ARRAY columns returns
-- 'Nothing'.
--
-- The unit is /unencoded bytes/ — for PLAIN-encoded BYTE_ARRAY
-- pages that's the sum of value lengths (not including the
-- 4-byte length prefixes the wire format adds).
columnDataUnencodedByteArrayBytes
  :: ColumnData
  -> Maybe Int64
columnDataUnencodedByteArrayBytes = \case
  ColByteArray vs ->
    Just $! V.foldl' (\a bs -> a + fromIntegral (BS.length bs)) 0 vs
  _ -> Nothing

-- | Attach 'oiUnencodedByteArrayDataBytes' to an existing
-- 'OffsetIndex' using the supplied per-page byte counts. The
-- vector length must match the number of page locations or the
-- attached field is dropped (silently — readers tolerate the
-- missing slot, an inconsistent one would be worse).
buildOffsetIndexWithSizeStats
  :: V.Vector Int64
  -> OffsetIndex
  -> OffsetIndex
buildOffsetIndexWithSizeStats sizes oi
  | V.length sizes == V.length (oiPageLocations oi) =
      oi { oiUnencodedByteArrayDataBytes = Just sizes }
  | otherwise = oi
