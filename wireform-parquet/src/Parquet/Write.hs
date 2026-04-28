{-# LANGUAGE BangPatterns #-}
-- | Write Parquet files.
--
-- Provides page-level encoding, column chunk assembly, and whole-file builders.
--
-- @
-- import qualified Data.Vector.Primitive as VP
-- import qualified Data.Vector as V
-- import Parquet.Write
-- import Parquet.Types
--
-- let schema = V.fromList
--       [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing
--       , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing
--       ]
--     vals = VP.fromList [1, 2, 3 :: Int32]
--     bs = buildParquetFile schema (V.singleton (V.singleton vals))
-- @
module Parquet.Write
  ( writeParquetFile
  , encodePlainInt32Page
  , encodePlainInt64Page
  , encodePlainFloatPage
  , encodePlainDoublePage
  , encodePlainBoolPage
  , encodePlainByteArrayPage
  , encodePageHeader
  , assembleColumnChunk
  , buildParquetFile
  , buildParquetFileWithBloom
    -- * Statistics
  , statisticsForInt32
  , statisticsForInt64
  , statisticsForByteArray
  ) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import Data.Word (Word8)

import Parquet.BloomFilter
  ( encodeBloomFilter
  , newSbbf
  , sbbfInsert
  )
import Parquet.Footer (writeFooter, parquetMagic)
import Parquet.Page
  ( DataPageHeader (..)
  , DataPageHeaderV2 (..)
  , DictionaryPageHeader (..)
  , PageHeader (..)
  , pageTypeDataPage
  )
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

-- | Encode a vector of @INT32@ values as a single uncompressed @PLAIN@
-- @DATA_PAGE@ (header + body).
encodePlainInt32Page :: VP.Vector Int32 -> ByteString
encodePlainInt32Page vals =
  let !n = VP.length vals
      !bodySize = n * 4
      !body = BL.toStrict $ B.toLazyByteString $
        VP.foldl' (\b v -> b <> B.int32LE v) mempty vals
      !hdr = mkPlainDataPageHeader (fromIntegral n) (fromIntegral bodySize)
  in encodePageHeader hdr <> body

-- | Encode a vector of @INT64@ values as a single uncompressed @PLAIN@
-- @DATA_PAGE@.
encodePlainInt64Page :: VP.Vector Int64 -> ByteString
encodePlainInt64Page vals =
  let !n = VP.length vals
      !bodySize = n * 8
      !body = BL.toStrict $ B.toLazyByteString $
        VP.foldl' (\b v -> b <> B.int64LE v) mempty vals
      !hdr = mkPlainDataPageHeader (fromIntegral n) (fromIntegral bodySize)
  in encodePageHeader hdr <> body

-- | Encode a vector of @FLOAT@ values as a single uncompressed @PLAIN@
-- @DATA_PAGE@ (IEEE little-endian, 4 bytes per value).
encodePlainFloatPage :: VP.Vector Float -> ByteString
encodePlainFloatPage vals =
  let !n = VP.length vals
      !bodySize = n * 4
      !body = BL.toStrict $ B.toLazyByteString $
        VP.foldl' (\b v -> b <> B.floatLE v) mempty vals
      !hdr = mkPlainDataPageHeader (fromIntegral n) (fromIntegral bodySize)
  in encodePageHeader hdr <> body

-- | Encode a vector of @DOUBLE@ values as a single uncompressed @PLAIN@
-- @DATA_PAGE@ (IEEE little-endian, 8 bytes per value).
encodePlainDoublePage :: VP.Vector Double -> ByteString
encodePlainDoublePage vals =
  let !n = VP.length vals
      !bodySize = n * 8
      !body = BL.toStrict $ B.toLazyByteString $
        VP.foldl' (\b v -> b <> B.doubleLE v) mempty vals
      !hdr = mkPlainDataPageHeader (fromIntegral n) (fromIntegral bodySize)
  in encodePageHeader hdr <> body

-- | Encode a vector of @BOOLEAN@ values as a single uncompressed @PLAIN@
-- @DATA_PAGE@ (LSB-first bit-packed; first value goes into the low bit
-- of the first byte).
encodePlainBoolPage :: V.Vector Bool -> ByteString
encodePlainBoolPage vals =
  let !n = V.length vals
      !nBytes = (n + 7) `quot` 8
      !body = BS.pack (packBitsLSB n vals)
      !hdr = mkPlainDataPageHeader (fromIntegral n) (fromIntegral nBytes)
  in encodePageHeader hdr <> body

-- | Pack @n@ booleans into @ceil(n/8)@ bytes, LSB first.
packBitsLSB :: Int -> V.Vector Bool -> [Word8]
packBitsLSB n vs = go 0
  where
    go !byteIdx
      | byteIdx * 8 >= n = []
      | otherwise =
          let !lo = byteIdx * 8
              !hi = min (lo + 8) n
              !b = foldr (\bit acc ->
                     let !i = bit
                         !v = if V.unsafeIndex vs (lo + i)
                                then 1 `shiftL` i
                                else 0 :: Word8
                     in acc .|. v) 0 [0 .. hi - lo - 1]
          in b : go (byteIdx + 1)

-- | Encode a vector of @BYTE_ARRAY@ values as a single uncompressed @PLAIN@
-- @DATA_PAGE@ (4-byte LE length prefix per value).
encodePlainByteArrayPage :: V.Vector ByteString -> ByteString
encodePlainByteArrayPage vals =
  let !body = BL.toStrict $ B.toLazyByteString $
        V.foldl' (\b v ->
          b <> B.word32LE (fromIntegral (BS.length v)) <> B.byteString v
        ) mempty vals
      !bodySize = BS.length body
      !n = V.length vals
      !hdr = mkPlainDataPageHeader (fromIntegral n) (fromIntegral bodySize)
  in encodePageHeader hdr <> body

mkPlainDataPageHeader :: Int32 -> Int32 -> PageHeader
mkPlainDataPageHeader numValues bodySize = PageHeader
  { phType = pageTypeDataPage
  , phUncompressedPageSize = Just bodySize
  , phCompressedPageSize = Just bodySize
  , phDataPage = Just DataPageHeader
      { dphNumValues = numValues
      , dphEncoding = 0
      }
  , phDictionaryPage = Nothing
  , phDataPageV2 = Nothing
  }

-- | Thrift compact-encode a 'PageHeader'.
encodePageHeader :: PageHeader -> ByteString
encodePageHeader hdr = encodeCompact (pageHeaderToThrift hdr)

pageHeaderToThrift :: PageHeader -> TV.Value
pageHeaderToThrift hdr = TV.Struct $ V.fromList $
  [(1, TV.I32 (phType hdr))]
  ++ maybe [] (\s -> [(2, TV.I32 s)]) (phUncompressedPageSize hdr)
  ++ maybe [] (\s -> [(3, TV.I32 s)]) (phCompressedPageSize hdr)
  ++ maybe [] (\dph -> [(5, dataPageHeaderToThrift dph)]) (phDataPage hdr)
  ++ maybe [] (\dk -> [(7, dictPageHeaderToThrift dk)]) (phDictionaryPage hdr)
  ++ maybe [] (\v2 -> [(8, dataPageHeaderV2ToThrift v2)]) (phDataPageV2 hdr)

dataPageHeaderToThrift :: DataPageHeader -> TV.Value
dataPageHeaderToThrift dph = TV.Struct $ V.fromList
  [ (1, TV.I32 (dphNumValues dph))
  , (2, TV.I32 (dphEncoding dph))
  ]

dictPageHeaderToThrift :: DictionaryPageHeader -> TV.Value
dictPageHeaderToThrift dk = TV.Struct $ V.fromList
  [ (1, TV.I32 (dictNumValues dk))
  , (2, TV.I32 (dictEncoding dk))
  ]

dataPageHeaderV2ToThrift :: DataPageHeaderV2 -> TV.Value
dataPageHeaderV2ToThrift v2 = TV.Struct $ V.fromList
  [ (1, TV.I32 (dph2NumValues v2))
  , (2, TV.I32 (dph2NumNulls v2))
  , (3, TV.I32 (dph2NumRows v2))
  , (4, TV.I32 (dph2Encoding v2))
  , (5, TV.I32 (dph2DefLevelsLen v2))
  , (6, TV.I32 (dph2RepLevelsLen v2))
  , (7, TV.Bool (dph2IsCompressed v2))
  ]

-- | Concatenate pre-encoded pages into a single column chunk. Currently only
-- @Uncompressed@ is supported for writing; pages must already be encoded with
-- their headers.
assembleColumnChunk :: Compression -> [ByteString] -> ByteString
assembleColumnChunk _codec pages = mconcat pages

-- | Build a complete Parquet file from a schema and row groups of @INT32@
-- column vectors. Produces @PAR1@ magic, uncompressed @PLAIN@ pages, footer,
-- and trailing magic.
buildParquetFile :: V.Vector SchemaElement -> V.Vector (V.Vector (VP.Vector Int32)) -> ByteString
buildParquetFile schema rowGroupVecs =
  let !encodedRGs = V.map (V.map encodePlainInt32Page) rowGroupVecs
      (!rgMetas, !_) = V.ifoldl' buildRG (V.empty, 4) encodedRGs
      !totalRows = V.foldl' (\a rg -> a + rgNumRows rg) 0 rgMetas
      !fm = FileMetadata
        { fmVersion = 1
        , fmSchema = schema
        , fmNumRows = totalRows
        , fmRowGroups = rgMetas
        , fmCreatedBy = Just "wireform"
        }
  in writeParquetFile fm encodedRGs
  where
    leaves :: V.Vector SchemaElement
    !leaves = V.filter (maybe False (const True) . seType) schema

    buildRG :: (V.Vector RowGroup, Int) -> Int -> V.Vector ByteString -> (V.Vector RowGroup, Int)
    buildRG (!rgs, !off) rgIdx encodedCols =
      let !colVecs = V.unsafeIndex rowGroupVecs rgIdx
          (!cols, !off2) = V.ifoldl' (buildCol colVecs) (V.empty, off) encodedCols
          !nRows = if V.null colVecs then 0 else fromIntegral (VP.length (V.unsafeIndex colVecs 0))
          !rg = RowGroup
            { rgColumns = cols
            , rgTotalByteSize = fromIntegral (off2 - off)
            , rgNumRows = nRows
            }
      in (V.snoc rgs rg, off2)

    buildCol :: V.Vector (VP.Vector Int32) -> (V.Vector ColumnChunk, Int) -> Int -> ByteString -> (V.Vector ColumnChunk, Int)
    buildCol colVecs (!cs, !cOff) colIdx pageBs =
      let !colVec = V.unsafeIndex colVecs colIdx
          !leaf = V.unsafeIndex leaves colIdx
          !sz = BS.length pageBs
          !cc = ColumnChunk
            { ccFilePath = Nothing
            , ccFileOffset = fromIntegral cOff
            , ccMetadata = Just ColumnMetadata
                { cmType = fromMaybe PTInt32 (seType leaf)
                , cmEncodings = V.singleton Plain
                , cmPathInSchema = V.singleton (seName leaf)
                , cmCodec = Uncompressed
                , cmNumValues = fromIntegral (VP.length colVec)
                , cmTotalUncompressedSize = fromIntegral sz
                , cmTotalCompressedSize = fromIntegral sz
                , cmDataPageOffset = fromIntegral cOff
                , cmStatistics = Just (statisticsForInt32 colVec)
                , cmIndexPageOffset = Nothing
                , cmDictionaryPageOffset = Nothing
                , cmKeyValueMetadata = V.empty
                , cmEncodingStats = V.empty
                , cmBloomFilterOffset = Nothing
                , cmBloomFilterLength = Nothing
                }
            , ccOffsetIndexOffset = Nothing
            , ccOffsetIndexLength = Nothing
            , ccColumnIndexOffset = Nothing
            , ccColumnIndexLength = Nothing
            }
      in (V.snoc cs cc, cOff + sz)

-- | Like 'buildParquetFile' but also emits a split-block bloom filter
-- ("Parquet.BloomFilter") per Int32 column. Each column's
-- 'ColumnMetadata' will carry @bloom_filter_offset@ and
-- @bloom_filter_length@ pointing to the appended bloom-filter blob.
--
-- Column chunks remain at the head of the file (pages, then dictionary
-- pages); bloom filters are concatenated in row-group / column order
-- after the last page and before the file footer, as suggested by the
-- parquet-format spec for newly written files.
buildParquetFileWithBloom
  :: Int                                                  -- ^ bloom filter bytes per column (rounded up to a multiple of 32)
  -> V.Vector SchemaElement
  -> V.Vector (V.Vector (VP.Vector Int32))
  -> ByteString
buildParquetFileWithBloom !bfBytes schema rowGroupVecs =
  let !encodedRGs = V.map (V.map encodePlainInt32Page) rowGroupVecs

      -- First pass: lay out the data pages, recording column-chunk
      -- offsets exactly as 'buildParquetFile' does.
      (!rgMetas0, !pagesEndOff) = V.ifoldl' buildRG' (V.empty, 4) encodedRGs

      -- Build a bloom filter per column and concatenate the blobs.
      (!blooms, !bloomBlobs, !_) = layoutBlooms pagesEndOff
      !bloomConcat = mconcat bloomBlobs

      -- Second pass: walk the row groups again, attaching the
      -- bloom-filter pointer to each column metadata.
      !rgMetas = applyBlooms rgMetas0 blooms

      !totalRows = V.foldl' (\a rg -> a + rgNumRows rg) 0 rgMetas
      !fm = FileMetadata
        { fmVersion = 1
        , fmSchema = schema
        , fmNumRows = totalRows
        , fmRowGroups = rgMetas
        , fmCreatedBy = Just "wireform"
        }
  in writeParquetFileWithExtras fm encodedRGs bloomConcat
  where
    leaves :: V.Vector SchemaElement
    !leaves = V.filter (maybe False (const True) . seType) schema

    -- Build the bloom-filter blob for one column and the (offset, length)
    -- pair to record on its ColumnMetaData.
    buildOneBloom :: VP.Vector Int32 -> Int -> ((Int64, Int32), ByteString)
    buildOneBloom colVec startOff =
      let !sbbf0 = newSbbf bfBytes
          !sbbf  = VP.foldl' (\acc v -> sbbfInsert (i32LE v) acc) sbbf0 colVec
          !blob = encodeBloomFilter sbbf
          !len  = BS.length blob
      in ((fromIntegral startOff, fromIntegral len), blob)

    layoutBlooms
      :: Int
      -> (V.Vector (V.Vector (Int64, Int32)), [ByteString], Int)
    layoutBlooms startOff =
      let go !rgIdx !off !accB !accBlobs
            | rgIdx >= V.length rowGroupVecs =
                (V.fromList (reverse accB), reverse accBlobs, off)
            | otherwise =
                let !cols = V.unsafeIndex rowGroupVecs rgIdx
                    (colTuples, off', blobs') = layoutCols cols off
                in go (rgIdx + 1) off' (colTuples : accB) (blobs' ++ accBlobs)
          layoutCols
            :: V.Vector (VP.Vector Int32)
            -> Int
            -> (V.Vector (Int64, Int32), Int, [ByteString])
          layoutCols cols off =
            let go2 !ci !cur !accT !accB
                  | ci >= V.length cols =
                      (V.fromList (reverse accT), cur, reverse accB)
                  | otherwise =
                      let ((o, l), b) = buildOneBloom (V.unsafeIndex cols ci) cur
                      in go2 (ci + 1) (cur + fromIntegral l) ((o, l) : accT) (b : accB)
            in go2 0 off [] []
      in go 0 startOff [] []

    applyBlooms
      :: V.Vector RowGroup
      -> V.Vector (V.Vector (Int64, Int32))
      -> V.Vector RowGroup
    applyBlooms = V.zipWith (\rg bs -> rg { rgColumns = V.zipWith attach (rgColumns rg) bs })
      where
        attach cc (off, len) = case ccMetadata cc of
          Nothing -> cc
          Just cm -> cc
            { ccMetadata = Just cm
                { cmBloomFilterOffset = Just off
                , cmBloomFilterLength = Just len
                }
            }

    buildRG' :: (V.Vector RowGroup, Int) -> Int -> V.Vector ByteString -> (V.Vector RowGroup, Int)
    buildRG' (!rgs, !off) rgIdx encodedCols =
      let !colVecs = V.unsafeIndex rowGroupVecs rgIdx
          (!cols, !off2) = V.ifoldl' (buildCol colVecs) (V.empty, off) encodedCols
          !nRows = if V.null colVecs then 0 else fromIntegral (VP.length (V.unsafeIndex colVecs 0))
          !rg = RowGroup
            { rgColumns = cols
            , rgTotalByteSize = fromIntegral (off2 - off)
            , rgNumRows = nRows
            }
      in (V.snoc rgs rg, off2)

    buildCol :: V.Vector (VP.Vector Int32) -> (V.Vector ColumnChunk, Int) -> Int -> ByteString -> (V.Vector ColumnChunk, Int)
    buildCol colVecs (!cs, !cOff) colIdx pageBs =
      let !colVec = V.unsafeIndex colVecs colIdx
          !leaf = V.unsafeIndex leaves colIdx
          !sz = BS.length pageBs
          !cc = ColumnChunk
            { ccFilePath = Nothing
            , ccFileOffset = fromIntegral cOff
            , ccMetadata = Just ColumnMetadata
                { cmType = fromMaybe PTInt32 (seType leaf)
                , cmEncodings = V.singleton Plain
                , cmPathInSchema = V.singleton (seName leaf)
                , cmCodec = Uncompressed
                , cmNumValues = fromIntegral (VP.length colVec)
                , cmTotalUncompressedSize = fromIntegral sz
                , cmTotalCompressedSize = fromIntegral sz
                , cmDataPageOffset = fromIntegral cOff
                , cmStatistics = Just (statisticsForInt32 colVec)
                , cmIndexPageOffset = Nothing
                , cmDictionaryPageOffset = Nothing
                , cmKeyValueMetadata = V.empty
                , cmEncodingStats = V.empty
                , cmBloomFilterOffset = Nothing
                , cmBloomFilterLength = Nothing
                }
            , ccOffsetIndexOffset = Nothing
            , ccOffsetIndexLength = Nothing
            , ccColumnIndexOffset = Nothing
            , ccColumnIndexLength = Nothing
            }
      in (V.snoc cs cc, cOff + sz)

-- | Writer that splices one extra blob between the page data and the
-- footer. Used by 'buildParquetFileWithBloom' to lay out bloom filters.
writeParquetFileWithExtras
  :: FileMetadata
  -> V.Vector (V.Vector ByteString)
  -> ByteString
  -> ByteString
writeParquetFileWithExtras fm rowGroupData extras = BL.toStrict $ B.toLazyByteString $
  B.byteString parquetMagic
  <> V.foldl' (\b rg -> V.foldl' (\b2 col -> b2 <> B.byteString col) b rg) mempty rowGroupData
  <> B.byteString extras
  <> B.byteString (writeFooter fm)

-- ============================================================
-- Page / column statistics
-- ============================================================

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
      let !mn = VP.foldl1' min vs
          !mx = VP.foldl1' max vs
          encMin = i32LE mn
          encMax = i32LE mx
      in Statistics
           { statMin = Just encMin
           , statMax = Just encMax
           , statNullCount = Just 0
           , statDistinctCount = Nothing
           , statMinValue = Just encMin
           , statMaxValue = Just encMax
           , statIsMinValueExact = Just True
           , statIsMaxValueExact = Just True
           }

-- | Compute Parquet 'Statistics' for an @INT64@ column (LE i64 min/max).
statisticsForInt64 :: VP.Vector Int64 -> Statistics
statisticsForInt64 vs
  | VP.null vs = emptyStats
  | otherwise =
      let !mn = VP.foldl1' min vs
          !mx = VP.foldl1' max vs
          encMin = i64LE mn
          encMax = i64LE mx
      in Statistics
           { statMin = Just encMin
           , statMax = Just encMax
           , statNullCount = Just 0
           , statDistinctCount = Nothing
           , statMinValue = Just encMin
           , statMaxValue = Just encMax
           , statIsMinValueExact = Just True
           , statIsMaxValueExact = Just True
           }

-- | Compute Parquet 'Statistics' for a @BYTE_ARRAY@ column. Values are
-- compared lexicographically (unsigned byte-by-byte).  The min/max
-- bytes are stored without their PLAIN length prefix per the spec.
statisticsForByteArray :: V.Vector ByteString -> Statistics
statisticsForByteArray vs
  | V.null vs = emptyStats
  | otherwise =
      let !mn = V.foldl1' minBS vs
          !mx = V.foldl1' maxBS vs
      in Statistics
           { statMin = Just mn
           , statMax = Just mx
           , statNullCount = Just 0
           , statDistinctCount = Nothing
           , statMinValue = Just mn
           , statMaxValue = Just mx
           , statIsMinValueExact = Just True
           , statIsMaxValueExact = Just True
           }
  where
    minBS a b = if a <= b then a else b
    maxBS a b = if a >= b then a else b

emptyStats :: Statistics
emptyStats = Statistics Nothing Nothing (Just 0) Nothing Nothing Nothing Nothing Nothing

i32LE :: Int32 -> ByteString
i32LE v = BL.toStrict (B.toLazyByteString (B.int32LE v))

i64LE :: Int64 -> ByteString
i64LE v = BL.toStrict (B.toLazyByteString (B.int64LE v))
