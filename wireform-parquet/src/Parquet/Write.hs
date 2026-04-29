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
  , encodePlainByteArrayPage
  , encodePageHeader
  , assembleColumnChunk
  , buildParquetFile
  , buildParquetFileWithIndex
    -- * Statistics
  , statisticsForInt32
  , statisticsForInt64
  , statisticsForByteArray
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP

import Parquet.BloomFilter (Sbbf, encodeBloomFilter, newSbbf, sbbfInsert)
import Parquet.Footer (writeFooter, parquetMagic)
import Parquet.Page
  ( DataPageHeader (..)
  , DataPageHeaderV2 (..)
  , DictionaryPageHeader (..)
  , PageHeader (..)
  , pageTypeDataPage
  )
import Parquet.PageIndex (encodeColumnIndex, encodeOffsetIndex)
import Parquet.Types
  ( BoundaryOrder (..)
  , ColumnChunk (..)
  , ColumnIndex (..)
  , ColumnMetadata (..)
  , Compression (..)
  , Encoding (..)
  , FileMetadata (..)
  , OffsetIndex (..)
  , PageLocation (..)
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
-- buildParquetFileWithIndex: emit page index + bloom filter footers
-- ============================================================

-- | Build a complete Parquet file with @OffsetIndex@, @ColumnIndex@,
-- and a split-block bloom filter for each column chunk, in addition
-- to the standard row-group metadata.
--
-- Each column's vector is split into pages of at most
-- @rowsPerPage@ values. For every page we record its file offset,
-- compressed size, and starting row index (the @OffsetIndex@) plus its
-- min/max values (the @ColumnIndex@). A column-level split-block bloom
-- filter is built from every value (sized to roughly 10 bits/value).
--
-- Layout produced:
--
-- > PAR1
-- > <row group 0 col 0 page 0> <... page n>
-- > <row group 0 col 1 page 0> ...
-- > ...
-- > <bloom filter rg0 col0> <bloom filter rg0 col1> ...
-- > <offset index rg0 col0> <offset index rg0 col1> ...
-- > <column index rg0 col0> <column index rg0 col1> ...
-- > <thrift FileMetadata footer> <footer length (4 bytes LE)> PAR1
--
-- Existing readers that ignore the index pointers still read the file;
-- index-aware readers can jump straight to the relevant pages.
buildParquetFileWithIndex
  :: Int                                      -- ^ rows per page
  -> V.Vector SchemaElement                   -- ^ schema
  -> V.Vector (V.Vector (VP.Vector Int32))    -- ^ row groups, each a vector of column-vectors
  -> ByteString
buildParquetFileWithIndex !rowsPerPage schema rowGroupVecs =
  let !leaves = V.filter (maybe False (const True) . seType) schema

      -- Pass 1: compute per-(rg, col) page bytes, OffsetIndex, ColumnIndex,
      -- and bloom filter — all offsets relative to the start of pages
      -- (i.e. byte 4, after PAR1 magic).
      !pageHeaderSize = 0  -- recomputed per page below

      -- Render each row group's column data as a flat ByteString and
      -- collect the side-channel structures.
      walk :: Int
           -> Int
           -> [RowGroupOut]
           -> [[ByteString]]
           -> ([RowGroupOut], [[ByteString]], Int)
      walk !rgIdx !off acc colsAcc
        | rgIdx >= V.length rowGroupVecs = (reverse acc, reverse colsAcc, off)
        | otherwise =
            let !cols = V.unsafeIndex rowGroupVecs rgIdx
                (rgOut, colBs, off') = renderRowGroup leaves rowsPerPage off cols
            in walk (rgIdx + 1) off' (rgOut : acc) (colBs : colsAcc)

      (!rgOuts, !rgBytes, !afterPagesOff) =
        walk 0 4 [] []

      -- Emit bloom filters in the same (rg, col) order, recording the
      -- (offset, length) for each.
      buildBlooms :: Int -> [RowGroupOut] -> [BloomEntry] -> ([BloomEntry], [ByteString], Int)
      buildBlooms !off [] acc = (reverse acc, [], off)
      buildBlooms !off (rg : rest) acc =
        let (off', accCols, bfBs) = goCols off (rgoColumns rg) acc []
            (rest', bss, off'') = buildBlooms off' rest accCols
        in (rest', bfBs ++ bss, off'')
        where
          goCols !o [] !a !bb = (o, a, reverse bb)
          goCols !o (c : cs) !a !bb =
            let !bs = encodeBloomFilter (rcBloom c)
                !len = BS.length bs
                !entry = BloomEntry o len
            in goCols (o + len) cs (entry : a) (bs : bb)

      (!bloomEntries, !bloomBytes, !afterBloomsOff) =
        buildBlooms afterPagesOff rgOuts []

      -- Emit OffsetIndexes
      buildOIs :: Int -> [RowGroupOut] -> [IndexEntry] -> ([IndexEntry], [ByteString], Int)
      buildOIs !off [] acc = (reverse acc, [], off)
      buildOIs !off (rg : rest) acc =
        let (off', accCols, oiBs) = goCols off (rgoColumns rg) acc []
            (rest', bss, off'') = buildOIs off' rest accCols
        in (rest', oiBs ++ bss, off'')
        where
          goCols !o [] !a !bb = (o, a, reverse bb)
          goCols !o (c : cs) !a !bb =
            let !bs = encodeOffsetIndex (rcOffsetIndex c)
                !len = BS.length bs
                !entry = IndexEntry o len
            in goCols (o + len) cs (entry : a) (bs : bb)

      (!oiEntries, !oiBytes, !afterOIsOff) =
        buildOIs afterBloomsOff rgOuts []

      -- Emit ColumnIndexes
      buildCIs :: Int -> [RowGroupOut] -> [IndexEntry] -> ([IndexEntry], [ByteString], Int)
      buildCIs !off [] acc = (reverse acc, [], off)
      buildCIs !off (rg : rest) acc =
        let (off', accCols, ciBs) = goCols off (rgoColumns rg) acc []
            (rest', bss, off'') = buildCIs off' rest accCols
        in (rest', ciBs ++ bss, off'')
        where
          goCols !o [] !a !bb = (o, a, reverse bb)
          goCols !o (c : cs) !a !bb =
            let !bs = encodeColumnIndex (rcColumnIndex c)
                !len = BS.length bs
                !entry = IndexEntry o len
            in goCols (o + len) cs (entry : a) (bs : bb)

      (!ciEntries, !ciBytes, !_afterCIsOff) =
        buildCIs afterOIsOff rgOuts []

      -- Build the row-group / column-chunk metadata, splicing in the
      -- index pointers we just computed.
      !rgMetas = V.fromList $
        zipWith3 (assembleRowGroupMeta leaves) rgOuts
          (chunksOf rgOuts bloomEntries)
          (zip (chunksOf rgOuts oiEntries) (chunksOf rgOuts ciEntries))

      !totalRows = V.foldl' (\a rg -> a + rgNumRows rg) 0 rgMetas

      !fm = FileMetadata
        { fmVersion = 1
        , fmSchema = schema
        , fmNumRows = totalRows
        , fmRowGroups = rgMetas
        , fmCreatedBy = Just "wireform"
        }

      !pagesBs   = BS.concat (concat rgBytes)
      !bloomsBs  = BS.concat bloomBytes
      !oiBs      = BS.concat oiBytes
      !ciBs      = BS.concat ciBytes
  in BS.concat
       [ parquetMagic
       , pagesBs
       , bloomsBs
       , oiBs
       , ciBs
       , writeFooter fm
       ]

-- | Per-(row-group, column) intermediate result from page rendering.
data RowGroupOut = RowGroupOut
  { rgoColumns :: ![RenderedColumn]
  , rgoNumRows :: !Int64
  }

data RenderedColumn = RenderedColumn
  { rcType        :: !ParquetType
  , rcSize        :: !Int       -- bytes of all pages
  , rcStartOff    :: !Int       -- file offset of first page
  , rcNumValues   :: !Int64
  , rcStats       :: !Statistics
  , rcOffsetIndex :: !OffsetIndex
  , rcColumnIndex :: !ColumnIndex
  , rcBloom       :: !Sbbf
  }

data BloomEntry = BloomEntry !Int !Int  -- offset, length
data IndexEntry = IndexEntry !Int !Int

renderRowGroup
  :: V.Vector SchemaElement
  -> Int
  -> Int
  -> V.Vector (VP.Vector Int32)
  -> (RowGroupOut, [ByteString], Int)
renderRowGroup leaves !rowsPerPage !startOff cols =
  let !nCols = V.length cols
      goCols !off !i acc bytesAcc
        | i >= nCols = (reverse acc, reverse bytesAcc, off)
        | otherwise =
            let !leaf = V.unsafeIndex leaves i
                !vec = V.unsafeIndex cols i
                (rc, bs) = renderColumn leaf rowsPerPage off vec
            in goCols (off + rcSize rc) (i + 1) (rc : acc) (bs : bytesAcc)
      (!cs, !colBs, !off') = goCols startOff 0 [] []
      !nRows = if V.null cols then 0 else fromIntegral (VP.length (V.unsafeIndex cols 0))
  in (RowGroupOut cs nRows, colBs, off')

renderColumn
  :: SchemaElement
  -> Int
  -> Int
  -> VP.Vector Int32
  -> (RenderedColumn, ByteString)
renderColumn leaf !rowsPerPage !startOff vec =
  let !pages = pagesForVector rowsPerPage vec
      goPages !off [] !off2 locs mins maxs nullPages bytes =
        (off, reverse locs, reverse mins, reverse maxs, reverse nullPages, reverse bytes)
        where _ = off2
      goPages !off ((firstRow, slice) : rest) !startRow locs mins maxs nullPages bytes =
        let !pageBs = encodePlainInt32Page slice
            !sz = BS.length pageBs
            !loc = PageLocation
              { plOffset = fromIntegral off
              , plCompressedPageSize = fromIntegral sz
              , plFirstRowIndex = fromIntegral firstRow
              }
            !mn = if VP.null slice then 0 else VP.foldl1' min slice
            !mx = if VP.null slice then 0 else VP.foldl1' max slice
            !isNullPage = VP.null slice
        in goPages (off + sz) rest startRow
                   (loc : locs)
                   (i32LE mn : mins)
                   (i32LE mx : maxs)
                   (isNullPage : nullPages)
                   (pageBs : bytes)
      (!off', !locs, !mins, !maxs, !nullPages, !pageBytes) =
        goPages startOff pages 0 [] [] [] [] []
      !sizeTotal = off' - startOff
      !boundary  = boundaryOrderOf mins  -- on raw bytes (LE int32 lex == numeric for non-negative; we just default to Unordered)

      -- Bloom filter sized to ~10 bits/value, rounded up to a multiple
      -- of 32 bytes (the SBBF block size).
      !nVals = VP.length vec
      !bfBytes = max 32 (((nVals * 10 + 7) `div` 8 + 31) `div` 32 * 32)
      !bf0 = newSbbf bfBytes
      !bf = VP.foldl' (\b v -> sbbfInsert (i32LE v) b) bf0 vec

      !offsetIdx = OffsetIndex
        { oiPageLocations = V.fromList locs
        , oiUnencodedByteArrayDataBytes = Nothing
        }
      !columnIdx = ColumnIndex
        { ciNullPages = V.fromList nullPages
        , ciMinValues = V.fromList mins
        , ciMaxValues = V.fromList maxs
        , ciBoundaryOrder = boundary
        , ciNullCounts = Nothing
        , ciRepetitionLevelHistograms = Nothing
        , ciDefinitionLevelHistograms = Nothing
        }
      !rc = RenderedColumn
        { rcType        = fromMaybe PTInt32 (seType leaf)
        , rcSize        = sizeTotal
        , rcStartOff    = startOff
        , rcNumValues   = fromIntegral nVals
        , rcStats       = statisticsForInt32 vec
        , rcOffsetIndex = offsetIdx
        , rcColumnIndex = columnIdx
        , rcBloom       = bf
        }
  in (rc, BS.concat pageBytes)

-- | Default the boundary order to @Unordered@. Detecting ascending /
-- descending requires interpreting the encoded min/max bytes as the
-- column's numeric type; we conservatively report Unordered, which is
-- always safe.
boundaryOrderOf :: [a] -> BoundaryOrder
boundaryOrderOf _ = OrderUnordered

-- | Slice a column's value vector into @rowsPerPage@-sized pages, each
-- tagged with its starting row index.
pagesForVector :: Int -> VP.Vector Int32 -> [(Int, VP.Vector Int32)]
pagesForVector !rowsPerPage v
  | rowsPerPage <= 0 = [(0, v)]
  | VP.null v = []
  | otherwise = go 0
  where
    !n = VP.length v
    go !i
      | i >= n = []
      | otherwise =
          let !chunk = VP.slice i (min rowsPerPage (n - i)) v
          in (i, chunk) : go (i + rowsPerPage)

-- | Splice one row group's bloom-filter and index pointers into a
-- 'RowGroup' metadata structure.
assembleRowGroupMeta
  :: V.Vector SchemaElement
  -> RowGroupOut
  -> [BloomEntry]
  -> ([IndexEntry], [IndexEntry])
  -> RowGroup
assembleRowGroupMeta leaves rgo blooms (ois, cis) =
  let cols = zipWith4 (mkColumnChunk leaves) [0 ..] (rgoColumns rgo) blooms
                                              (zip ois cis)
  in RowGroup
       { rgColumns = V.fromList cols
       , rgTotalByteSize = fromIntegral (sum (map rcSize (rgoColumns rgo)))
       , rgNumRows = rgoNumRows rgo
       }

mkColumnChunk
  :: V.Vector SchemaElement
  -> Int
  -> RenderedColumn
  -> BloomEntry
  -> (IndexEntry, IndexEntry)
  -> ColumnChunk
mkColumnChunk leaves !colIdx rc (BloomEntry bfOff bfLen) (IndexEntry oiOff oiLen, IndexEntry ciOff ciLen) =
  let !leaf = V.unsafeIndex leaves colIdx
  in ColumnChunk
       { ccFilePath = Nothing
       , ccFileOffset = fromIntegral (rcStartOff rc)
       , ccMetadata = Just ColumnMetadata
           { cmType = rcType rc
           , cmEncodings = V.singleton Plain
           , cmPathInSchema = V.singleton (seName leaf)
           , cmCodec = Uncompressed
           , cmNumValues = rcNumValues rc
           , cmTotalUncompressedSize = fromIntegral (rcSize rc)
           , cmTotalCompressedSize = fromIntegral (rcSize rc)
           , cmDataPageOffset = fromIntegral (rcStartOff rc)
           , cmStatistics = Just (rcStats rc)
           , cmBloomFilterOffset = Just (fromIntegral bfOff)
           , cmBloomFilterLength = Just (fromIntegral bfLen)
           }
       , ccOffsetIndexOffset = Just (fromIntegral oiOff)
       , ccOffsetIndexLength = Just (fromIntegral oiLen)
       , ccColumnIndexOffset = Just (fromIntegral ciOff)
       , ccColumnIndexLength = Just (fromIntegral ciLen)
       }

-- | Split a flat list of entries (one per column across all row groups)
-- back into one sub-list per row group based on each row group's column
-- count.
chunksOf :: [RowGroupOut] -> [a] -> [[a]]
chunksOf rgs xs = go rgs xs
  where
    go [] _ = []
    go (rg : rest) ys =
      let !n = length (rgoColumns rg)
          (!h, !t) = splitAt n ys
      in h : go rest t

zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 f (a : as) (b : bs) (c : cs) (d : ds) = f a b c d : zipWith4 f as bs cs ds
zipWith4 _ _ _ _ _ = []

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
           }
  where
    minBS a b = if a <= b then a else b
    maxBS a b = if a >= b then a else b

emptyStats :: Statistics
emptyStats = Statistics Nothing Nothing (Just 0) Nothing Nothing Nothing

i32LE :: Int32 -> ByteString
i32LE v = BL.toStrict (B.toLazyByteString (B.int32LE v))

i64LE :: Int64 -> ByteString
i64LE v = BL.toStrict (B.toLazyByteString (B.int64LE v))
