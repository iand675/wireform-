{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
-- | Read Parquet column data into primitive vectors.
--
-- Supports @DATA_PAGE@ sequences with @PLAIN@ physical encoding for common
-- types, optional @DICTIONARY_PAGE@ + @PLAIN_DICTIONARY@ (hybrid RLE @INT32@
-- indices per Parquet spec),
-- and @Uncompressed@ / @GZip@ / (with @-fsnappy@) @Snappy@ / (with @-fzstd@) @ZSTD@ compression.
--
-- * @readPlain*@*ColumnChunk@ for primitives without nested nullability assumes the
--   column is @REQUIRED@ (no level data on disk).
-- * @readPlain*Optional*@ helpers decode data page v1 with definition\/repetition
--   levels ('Parquet.Levels'); use 'Parquet.Levels.maxLevelsForColumnPath' with
--   footer schema + column path. Available for @INT32@, @INT64@, @FLOAT@,
--   @DOUBLE@, @BOOL@, and @BYTE_ARRAY@.
--
-- For metadata and footer I/O use "Parquet.Footer".
module Parquet.Read
  ( ParquetFile (..)
  , loadParquetFile
  , columnChunkSlice
  , readPlainInt32FirstPage
  , readPlainInt32ColumnChunk
  , readPlainInt64ColumnChunk
  , readPlainFloatColumnChunk
  , readPlainDoubleColumnChunk
  , readPlainBoolColumnChunk
  , readPlainByteArrayColumnChunk
  , readPlainInt96ColumnChunk
  , readPlainFixedLenByteArrayColumnChunk
  , readPlainDictionaryInt32ColumnChunk
  , readDictionaryOptionalColumnChunk
  , readDictionaryInt32OptionalColumnChunk
  , decompressDataPageBody
  , decompressDataPageV2Body
  , decompressChunk
  , decodePlainInt32
  , decodePlainInt64
  , decodePlainFloat
  , decodePlainDouble
  , decodePlainBool
  , decodePlainByteArray
  , decodePlainInt96
  , decodePlainFixedLenByteArray
  , decodeByteStreamSplitFloat
  , decodeByteStreamSplitDouble
  , decodeDictionaryIndices
  , decodeHybridRleLengthPrefixed
  , readPlainInt32OptionalFirstPage
  , readPlainInt32OptionalColumnChunk
  , readPlainInt64OptionalFirstPage
  , readPlainInt64OptionalColumnChunk
  , readPlainFloatOptionalFirstPage
  , readPlainFloatOptionalColumnChunk
  , readPlainDoubleOptionalFirstPage
  , readPlainDoubleOptionalColumnChunk
  , readPlainBoolOptionalFirstPage
  , readPlainBoolOptionalColumnChunk
  , readPlainByteArrayOptionalFirstPage
  , readPlainByteArrayOptionalColumnChunk
  , decodeDeltaBinaryPackedInt32
  , decodeDeltaBinaryPackedInt64
  , encRleDictionary
  ) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad.ST (runST)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as MVP
import Data.Word (Word32, Word64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import System.IO.Unsafe (unsafePerformIO)

import Columnar.SIMD (unpackBitsLsbUnsafe)

import Parquet.Delta (decodeDeltaBinaryPackedInt32, decodeDeltaBinaryPackedInt64)
import Parquet.RLE (decodeDictionaryIndices, decodeHybridRleLengthPrefixed, decodeHybridRleUnsigned32)

import qualified Codec.Compression.GZip as GZip
#ifdef HAVE_ZSTD
import Codec.Compression.Zstd (Decompress (..), decompress)
#endif
#ifdef HAVE_SNAPPY
import qualified Codec.Compression.Snappy as Snappy
#endif
#ifdef HAVE_LZ4
import qualified Codec.Compression.LZ4 as LZ4
#endif

import Parquet.Footer (readFooter)
import Parquet.Levels
  ( levelBitWidth
  , materializePlainBoolOptional
  , materializePlainByteArrayOptional
  , materializePlainDoubleOptional
  , materializePlainFloatOptional
  , materializePlainInt32Optional
  , materializePlainInt64Optional
  , parseDataPageV1Levels
  )
import Parquet.Page
  ( DataPageHeader (..)
  , DataPageHeaderV2 (..)
  , DictionaryPageHeader (..)
  , PageHeader (..)
  , readPageHeaderAt
  , pageTypeDataPage
  , pageTypeDictionaryPage
  , pageTypeDataPageV2
  )
import Parquet.Types
  ( ColumnChunk (..)
  , ColumnMetadata (..)
  , Compression (..)
  , FileMetadata (..)
  , RowGroup (..)
  )

-- | A Parquet file loaded in memory with parsed footer metadata.
data ParquetFile = ParquetFile
  { pfBytes  :: !ByteString
  , pfFooter :: !FileMetadata
  } deriving stock (Show, Eq)

-- | Parse the footer from the tail of a complete @PAR1@ file.
loadParquetFile :: ByteString -> Either String ParquetFile
loadParquetFile bs = do
  fm <- readFooter bs
  Right ParquetFile {pfBytes = bs, pfFooter = fm}

-- | Raw bytes for one column chunk (from @data_page_offset@ through compressed size).
columnChunkSlice :: ParquetFile -> Int -> Int -> Either String ByteString
columnChunkSlice pf rgIdx colIdx = do
  let fm = pfFooter pf
      rgs = fmRowGroups fm
  whenOutOfRange rgIdx (V.length rgs) "row group"
  let rg = V.unsafeIndex rgs rgIdx
      cols = rgColumns rg
  whenOutOfRange colIdx (V.length cols) "column"
  let chunk = V.unsafeIndex cols colIdx
  meta <- case ccMetadata chunk of
    Nothing -> Left "Parquet.Read: column chunk missing ColumnMetaData"
    Just m -> Right m
  let !off = fromIntegral (cmDataPageOffset meta) :: Int
      !sz = fromIntegral (cmTotalCompressedSize meta) :: Int
      !bs0 = pfBytes pf
  if off < 0 || sz < 0 || off + sz > BS.length bs0
    then Left "Parquet.Read: column chunk slice out of bounds"
    else Right $! BS.take sz (BS.drop off bs0)

whenOutOfRange :: Int -> Int -> String -> Either String ()
whenOutOfRange i n msg
  | i >= 0 && i < n = Right ()
  | otherwise = Left $ "Parquet.Read: " ++ msg ++ " index out of range"

-- | Thrift @Encoding@ for @PLAIN@.
encPlain :: Int32
encPlain = 0

-- | Thrift @Encoding@ for @PLAIN_DICTIONARY@.
encPlainDictionary :: Int32
encPlainDictionary = 2

-- | Thrift @Encoding@ for @RLE_DICTIONARY@ (modern name for PLAIN_DICTIONARY).
encRleDictionary :: Int32
encRleDictionary = 8

isDictionaryEncoding :: Int32 -> Bool
isDictionaryEncoding e = e == encPlainDictionary || e == encRleDictionary

{-# INLINE decompressDataPageBody #-}
decompressDataPageBody ::
  Compression ->
  ByteString ->
  Int ->
  Either String (PageHeader, DataPageHeader, ByteString, Int)
decompressDataPageBody codec chunk off = do
  (hdr, afterHdr) <- readPageHeaderAt chunk off
  if phType hdr /= pageTypeDataPage
    then Left "Parquet.Read: expected DATA_PAGE"
    else do
      compSz <- case phCompressedPageSize hdr of
        Nothing -> Left "Parquet.Read: missing compressed_page_size"
        Just s -> Right (fromIntegral s :: Int)
      dph <- case phDataPage hdr of
        Nothing -> Left "Parquet.Read: missing data_page_header"
        Just d -> Right d
      let !bodyStart = afterHdr
      if bodyStart + compSz > BS.length chunk
        then Left "Parquet.Read: truncated page body"
        else do
          let !compBody = BS.take compSz (BS.drop bodyStart chunk)
          !raw <- decompressPageData codec (phUncompressedPageSize hdr) compBody
          let !nextOff = bodyStart + compSz
          Right (hdr, dph, raw, nextOff)

-- | Read every @DATA_PAGE@ with @PLAIN@ @INT32@ in order until the chunk ends.
readPlainInt32ColumnChunk :: Compression -> ByteString -> Either String (VP.Vector Int32)
readPlainInt32ColumnChunk codec chunk = go 0 VP.empty
  where
    go !off !acc
      | off >= BS.length chunk = Right acc
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= encPlain
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              pageVec <- decodePlainInt32 n raw
              go nextOff (acc VP.++ pageVec)

-- | Read the first data page of a chunk as @PLAIN@ @INT32@ values.
readPlainInt32FirstPage :: Compression -> ByteString -> Either String (VP.Vector Int32)
readPlainInt32FirstPage codec chunk = do
  (_hdr, dph, raw, _) <- decompressDataPageBody codec chunk 0
  if dphEncoding dph /= encPlain
    then Left "Parquet.Read: encoding is not PLAIN (0)"
    else do
      let !n = fromIntegral (dphNumValues dph) :: Int
      decodePlainInt32 n raw

readPlainOptionalFirstPageWith ::
  Compression ->
  Int ->
  Int ->
  ByteString ->
  (VP.Vector Int32 -> Int -> ByteString -> Either String (V.Vector (Maybe a))) ->
  Either String (V.Vector (Maybe a))
readPlainOptionalFirstPageWith codec maxRep maxDef chunk mat = do
  (_hdr, dph, raw, _) <- decompressDataPageBody codec chunk 0
  if dphEncoding dph /= encPlain
    then Left "Parquet.Read: encoding is not PLAIN (0)"
    else do
      let !n = fromIntegral (dphNumValues dph) :: Int
      (_rep, def, rest) <- parseDataPageV1Levels maxRep maxDef n raw
      mat def maxDef rest

readPlainOptionalColumnChunkWith ::
  Compression ->
  Int ->
  Int ->
  ByteString ->
  (VP.Vector Int32 -> Int -> ByteString -> Either String (V.Vector (Maybe a))) ->
  Either String (V.Vector (Maybe a))
readPlainOptionalColumnChunkWith codec maxRep maxDef chunk mat = go 0 V.empty
  where
    go !off !acc
      | off >= BS.length chunk = Right acc
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= encPlain
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              (_rep, def, rest) <- parseDataPageV1Levels maxRep maxDef n raw
              page <- mat def maxDef rest
              go nextOff (acc V.++ page)

-- | First @DATA_PAGE@ as @PLAIN@ @INT32@ with levels.
readPlainInt32OptionalFirstPage ::
  Compression -> Int -> Int -> ByteString -> Either String (V.Vector (Maybe Int32))
readPlainInt32OptionalFirstPage codec mr md ch =
  readPlainOptionalFirstPageWith codec mr md ch materializePlainInt32Optional

-- | All @DATA_PAGE@s as optional @PLAIN@ @INT32@.
readPlainInt32OptionalColumnChunk ::
  Compression -> Int -> Int -> ByteString -> Either String (V.Vector (Maybe Int32))
readPlainInt32OptionalColumnChunk codec mr md ch =
  readPlainOptionalColumnChunkWith codec mr md ch materializePlainInt32Optional

readPlainInt64OptionalFirstPage ::
  Compression -> Int -> Int -> ByteString -> Either String (V.Vector (Maybe Int64))
readPlainInt64OptionalFirstPage codec mr md ch =
  readPlainOptionalFirstPageWith codec mr md ch materializePlainInt64Optional

readPlainInt64OptionalColumnChunk ::
  Compression -> Int -> Int -> ByteString -> Either String (V.Vector (Maybe Int64))
readPlainInt64OptionalColumnChunk codec mr md ch =
  readPlainOptionalColumnChunkWith codec mr md ch materializePlainInt64Optional

readPlainFloatOptionalFirstPage ::
  Compression -> Int -> Int -> ByteString -> Either String (V.Vector (Maybe Float))
readPlainFloatOptionalFirstPage codec mr md ch =
  readPlainOptionalFirstPageWith codec mr md ch materializePlainFloatOptional

readPlainFloatOptionalColumnChunk ::
  Compression -> Int -> Int -> ByteString -> Either String (V.Vector (Maybe Float))
readPlainFloatOptionalColumnChunk codec mr md ch =
  readPlainOptionalColumnChunkWith codec mr md ch materializePlainFloatOptional

readPlainDoubleOptionalFirstPage ::
  Compression -> Int -> Int -> ByteString -> Either String (V.Vector (Maybe Double))
readPlainDoubleOptionalFirstPage codec mr md ch =
  readPlainOptionalFirstPageWith codec mr md ch materializePlainDoubleOptional

readPlainDoubleOptionalColumnChunk ::
  Compression -> Int -> Int -> ByteString -> Either String (V.Vector (Maybe Double))
readPlainDoubleOptionalColumnChunk codec mr md ch =
  readPlainOptionalColumnChunkWith codec mr md ch materializePlainDoubleOptional

readPlainBoolOptionalFirstPage ::
  Compression -> Int -> Int -> ByteString -> Either String (V.Vector (Maybe Bool))
readPlainBoolOptionalFirstPage codec mr md ch =
  readPlainOptionalFirstPageWith codec mr md ch materializePlainBoolOptional

readPlainBoolOptionalColumnChunk ::
  Compression -> Int -> Int -> ByteString -> Either String (V.Vector (Maybe Bool))
readPlainBoolOptionalColumnChunk codec mr md ch =
  readPlainOptionalColumnChunkWith codec mr md ch materializePlainBoolOptional

readPlainByteArrayOptionalFirstPage ::
  Compression -> Int -> Int -> ByteString -> Either String (V.Vector (Maybe ByteString))
readPlainByteArrayOptionalFirstPage codec mr md ch =
  readPlainOptionalFirstPageWith codec mr md ch materializePlainByteArrayOptional

readPlainByteArrayOptionalColumnChunk ::
  Compression -> Int -> Int -> ByteString -> Either String (V.Vector (Maybe ByteString))
readPlainByteArrayOptionalColumnChunk codec mr md ch =
  readPlainOptionalColumnChunkWith codec mr md ch materializePlainByteArrayOptional

-- | @PLAIN@ @INT64@ (little-endian), all @DATA_PAGE@s concatenated.
readPlainInt64ColumnChunk :: Compression -> ByteString -> Either String (VP.Vector Int64)
readPlainInt64ColumnChunk codec chunk = go 0 VP.empty
  where
    go !off !acc
      | off >= BS.length chunk = Right acc
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= encPlain
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              pageVec <- decodePlainInt64 n raw
              go nextOff (acc VP.++ pageVec)

-- | @PLAIN@ @FLOAT@ (IEEE little-endian).
readPlainFloatColumnChunk :: Compression -> ByteString -> Either String (VP.Vector Float)
readPlainFloatColumnChunk codec chunk = go 0 VP.empty
  where
    go !off !acc
      | off >= BS.length chunk = Right acc
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= encPlain
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              pageVec <- decodePlainFloat n raw
              go nextOff (acc VP.++ pageVec)

-- | @PLAIN@ @DOUBLE@ (IEEE little-endian).
readPlainDoubleColumnChunk :: Compression -> ByteString -> Either String (VP.Vector Double)
readPlainDoubleColumnChunk codec chunk = go 0 VP.empty
  where
    go !off !acc
      | off >= BS.length chunk = Right acc
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= encPlain
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              pageVec <- decodePlainDouble n raw
              go nextOff (acc VP.++ pageVec)

-- | @PLAIN@ @BOOLEAN@ (packed bits, LSB of first byte is first value).
readPlainBoolColumnChunk :: Compression -> ByteString -> Either String (V.Vector Bool)
readPlainBoolColumnChunk codec chunk = go 0 V.empty
  where
    go !off !acc
      | off >= BS.length chunk = Right acc
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= encPlain
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              pageVec <- decodePlainBool n raw
              go nextOff (acc V.++ pageVec)

-- | @PLAIN@ @BYTE_ARRAY@ (length-prefixed 4-byte LE + bytes per value).
readPlainByteArrayColumnChunk :: Compression -> ByteString -> Either String (V.Vector ByteString)
readPlainByteArrayColumnChunk codec chunk = go 0 V.empty
  where
    go !off !acc
      | off >= BS.length chunk = Right acc
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= encPlain
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              pageVec <- decodePlainByteArray n raw
              go nextOff (acc V.++ pageVec)

-- | Dictionary page (@PLAIN@ @INT32@ values) followed by @DATA_PAGE@s with
-- @PLAIN_DICTIONARY@ (indices as @PLAIN@ @INT32@). Plain @DATA_PAGE@s without
-- a dictionary are also accepted.
readPlainDictionaryInt32ColumnChunk :: Compression -> ByteString -> Either String (VP.Vector Int32)
readPlainDictionaryInt32ColumnChunk codec chunk = go 0 Nothing VP.empty
  where
    go !off !mDict !acc
      | off >= BS.length chunk =
          if VP.null acc
            then Left "Parquet.Read: empty dictionary column chunk"
            else Right acc
      | otherwise = do
          (hdr, afterHdr) <- readPageHeaderAt chunk off
          compSz <- case phCompressedPageSize hdr of
            Nothing -> Left "Parquet.Read: missing compressed_page_size"
            Just s -> Right (fromIntegral s :: Int)
          let !bodyStart = afterHdr
          if bodyStart + compSz > BS.length chunk
            then Left "Parquet.Read: truncated page body"
            else do
              let !compBody = BS.take compSz (BS.drop bodyStart chunk)
              !raw <- decompressPageData codec (phUncompressedPageSize hdr) compBody
              let !nextOff = bodyStart + compSz
              case phType hdr of
                t
                  | t == pageTypeDictionaryPage -> do
                      dk <- case phDictionaryPage hdr of
                        Nothing -> Left "Parquet.Read: missing dictionary_page_header"
                        Just d -> Right d
                      if dictEncoding dk /= encPlain
                        then Left "Parquet.Read: dictionary encoding is not PLAIN (0)"
                        else do
                          let !nDict = fromIntegral (dictNumValues dk) :: Int
                          dict <- decodePlainInt32 nDict raw
                          go nextOff (Just dict) acc
                t
                  | t == pageTypeDataPage -> do
                      dph <- case phDataPage hdr of
                        Nothing -> Left "Parquet.Read: missing data_page_header"
                        Just d -> Right d
                      case dphEncoding dph of
                        e
                          | e == encPlain -> do
                              let !n = fromIntegral (dphNumValues dph) :: Int
                              pageVec <- decodePlainInt32 n raw
                              go nextOff mDict (acc VP.++ pageVec)
                          | isDictionaryEncoding e -> do
                              dict0 <- case mDict of
                                Nothing ->
                                  Left "Parquet.Read: PLAIN_DICTIONARY data page before dictionary page"
                                Just d -> Right d
                              let !n = fromIntegral (dphNumValues dph) :: Int
                              ix <- decodeDictionaryIndices n raw
                              let !nD = VP.length dict0
                                  ok =
                                    VP.foldl'
                                      ( \a k ->
                                          a
                                            && ( let !j = fromIntegral k :: Int
                                                 in j >= 0 && j < nD
                                               )
                                      )
                                      True
                                      ix
                              if not ok
                                then Left "Parquet.Read: dictionary index out of range"
                                else do
                                  let pageVec = VP.map (\k -> dict0 VP.! fromIntegral k) ix
                                  go nextOff mDict (acc VP.++ pageVec)
                        e ->
                          Left $
                            "Parquet.Read: unsupported data page encoding "
                              ++ show e
                              ++ " (expected PLAIN, PLAIN_DICTIONARY, or RLE_DICTIONARY)"
                _ -> Left "Parquet.Read: expected DICTIONARY_PAGE or DATA_PAGE"

decompressChunk :: Compression -> ByteString -> Either String ByteString
decompressChunk Uncompressed bs = Right bs
decompressChunk GZip bs = tryGZip bs
decompressChunk Snappy bs = trySnappy bs
#ifdef HAVE_ZSTD
decompressChunk ZSTD bs = tryZstd bs
#endif
decompressChunk LZ4 _ =
  Left "Parquet.Read: LZ4 (deprecated Hadoop variant, codec 5) not supported; use LZ4_RAW (codec 7)"
decompressChunk LZ4Raw _ =
  Left "Parquet.Read: LZ4_RAW requires uncompressed size; use decompressPage internally"
decompressChunk c _ =
  Left $
    "Parquet.Read: compression "
      ++ show c
      ++ " not supported (use Uncompressed, GZip, Snappy with -fsnappy"
#ifdef HAVE_ZSTD
      ++ ", Zstandard with -fzstd"
#endif
#ifdef HAVE_LZ4
      ++ ", LZ4_RAW with -flz4"
#endif
      ++ ")"

decompressPageData :: Compression -> Maybe Int32 -> ByteString -> Either String ByteString
#ifdef HAVE_LZ4
decompressPageData LZ4Raw (Just uncompSz) bs = tryLZ4Raw (fromIntegral uncompSz) bs
decompressPageData LZ4Raw Nothing _ =
  Left "Parquet.Read: LZ4_RAW decompression requires uncompressed_page_size in header"
#endif
decompressPageData codec _ bs = decompressChunk codec bs

tryGZip :: ByteString -> Either String ByteString
tryGZip bs =
  unsafePerformIO $ do
    er <- try @SomeException $ evaluate $ BL.toStrict $ GZip.decompress $ BL.fromStrict bs
    case er of
      Left e -> pure $ Left $ "Parquet.Read: gzip decompress failed: " ++ show e
      Right x -> pure $ Right x

trySnappy :: ByteString -> Either String ByteString
#ifdef HAVE_SNAPPY
trySnappy bs = Right (Snappy.decompress bs)
#else
trySnappy _ =
  Left "Parquet.Read: Snappy requires building wireform with -fsnappy"
#endif

#ifdef HAVE_ZSTD
tryZstd :: ByteString -> Either String ByteString
tryZstd bs =
  case decompress bs of
    Decompress out -> Right out
    Skip ->
      Left "Parquet.Read: zstd decompress skipped (empty or unsupported frame)"
    Error msg ->
      Left $ "Parquet.Read: zstd decompress failed: " ++ msg
#endif

#ifdef HAVE_LZ4
tryLZ4Raw :: Int -> ByteString -> Either String ByteString
tryLZ4Raw uncompSize bs =
  case LZ4.decompress uncompSize bs of
    Nothing -> Left "Parquet.Read: LZ4 raw block decompression failed"
    Just out -> Right out
#endif

decodePlainInt32 :: Int -> ByteString -> Either String (VP.Vector Int32)
decodePlainInt32 n bs
  | n < 0 = Left "Parquet.Read: PLAIN INT32 negative count"
  | n > BS.length bs `div` 4 =
      Left "Parquet.Read: PLAIN INT32 buffer too small"
  | otherwise =
      Right $
        runST $ do
          mv <- MVP.new n
          let go2 !i
                | i >= n = VP.unsafeFreeze mv
                | otherwise = do
                    let !o = i * 4
                        !v = readLE32 bs o
                    MVP.write mv i (fromIntegral v :: Int32)
                    go2 (i + 1)
          go2 0

decodePlainInt64 :: Int -> ByteString -> Either String (VP.Vector Int64)
decodePlainInt64 n bs
  | n < 0 = Left "Parquet.Read: PLAIN INT64 negative count"
  | n > BS.length bs `div` 8 =
      Left "Parquet.Read: PLAIN INT64 buffer too small"
  | otherwise =
      Right $
        runST $ do
          mv <- MVP.new n
          let go2 !i
                | i >= n = VP.unsafeFreeze mv
                | otherwise = do
                    let !o = i * 8
                        !v = readLE64 bs o
                    MVP.write mv i (fromIntegral v :: Int64)
                    go2 (i + 1)
          go2 0

decodePlainFloat :: Int -> ByteString -> Either String (VP.Vector Float)
decodePlainFloat n bs
  | n < 0 = Left "Parquet.Read: PLAIN FLOAT negative count"
  | n > BS.length bs `div` 4 =
      Left "Parquet.Read: PLAIN FLOAT buffer too small"
  | otherwise =
      Right $
        runST $ do
          mv <- MVP.new n
          let go2 !i
                | i >= n = VP.unsafeFreeze mv
                | otherwise = do
                    let !o = i * 4
                        !w = readLE32 bs o
                    MVP.write mv i (castWord32ToFloat w)
                    go2 (i + 1)
          go2 0

decodePlainDouble :: Int -> ByteString -> Either String (VP.Vector Double)
decodePlainDouble n bs
  | n < 0 = Left "Parquet.Read: PLAIN DOUBLE negative count"
  | n > BS.length bs `div` 8 =
      Left "Parquet.Read: PLAIN DOUBLE buffer too small"
  | otherwise =
      Right $
        runST $ do
          mv <- MVP.new n
          let go2 !i
                | i >= n = VP.unsafeFreeze mv
                | otherwise = do
                    let !o = i * 8
                        !w = readLE64 bs o
                    MVP.write mv i (castWord64ToDouble w)
                    go2 (i + 1)
          go2 0

decodePlainBool :: Int -> ByteString -> Either String (V.Vector Bool)
decodePlainBool n bs =
  let !need = (n + 7) `quot` 8
  in if BS.length bs < need
    then Left "Parquet.Read: PLAIN BOOLEAN buffer too small"
    else Right $! unpackBitsLsbUnsafe n bs

decodePlainByteArray :: Int -> ByteString -> Either String (V.Vector ByteString)
decodePlainByteArray n bs0 = go 0 0 V.empty
  where
    go !i !off !acc
      | i >= n = Right acc
      | off + 4 > BS.length bs0 = Left "Parquet.Read: PLAIN BYTE_ARRAY truncated length"
      | otherwise =
          let !len = fromIntegral (readLE32 bs0 off) :: Int
              !off2 = off + 4
          in if len < 0 || off2 + len > BS.length bs0
            then Left "Parquet.Read: PLAIN BYTE_ARRAY payload out of bounds"
            else
              let !payload = BS.take len (BS.drop off2 bs0)
              in go (i + 1) (off2 + len) (V.snoc acc payload)

-- | Read a column chunk with optional @DICTIONARY_PAGE@ + @PLAIN_DICTIONARY@
-- data pages, supporting definition\/repetition levels.
--
-- @decodeDictValues@: decodes PLAIN dictionary page body into a container.
-- @lookupDict@: retrieves a value by dictionary index (returns 'Nothing' if
-- the index is out of range).
{-# INLINE readDictionaryOptionalColumnChunk #-}
readDictionaryOptionalColumnChunk ::
  (Int -> ByteString -> Either String dict) ->
  (dict -> Int32 -> Maybe a) ->
  Compression -> Int -> Int -> ByteString ->
  Either String (V.Vector (Maybe a))
readDictionaryOptionalColumnChunk decodeDictValues lookupDict codec maxRep maxDef chunk =
  go 0 Nothing V.empty
  where
    go !off !mDict !acc
      | off >= BS.length chunk = Right acc
      | otherwise = do
          (hdr, afterHdr) <- readPageHeaderAt chunk off
          compSz <- case phCompressedPageSize hdr of
            Nothing -> Left "Parquet.Read: missing compressed_page_size"
            Just s -> Right (fromIntegral s :: Int)
          let !bodyStart = afterHdr
          if bodyStart + compSz > BS.length chunk
            then Left "Parquet.Read: truncated page body"
            else do
              let !compBody = BS.take compSz (BS.drop bodyStart chunk)
              !raw <- decompressPageData codec (phUncompressedPageSize hdr) compBody
              let !nextOff = bodyStart + compSz
              case phType hdr of
                t
                  | t == pageTypeDictionaryPage -> do
                      dk <- case phDictionaryPage hdr of
                        Nothing -> Left "Parquet.Read: missing dictionary_page_header"
                        Just d -> Right d
                      if dictEncoding dk /= encPlain
                        then Left "Parquet.Read: dictionary encoding is not PLAIN (0)"
                        else do
                          let !nDict = fromIntegral (dictNumValues dk) :: Int
                          dict <- decodeDictValues nDict raw
                          go nextOff (Just dict) acc
                t
                  | t == pageTypeDataPage -> do
                      dph <- case phDataPage hdr of
                        Nothing -> Left "Parquet.Read: missing data_page_header"
                        Just d -> Right d
                      if not (isDictionaryEncoding (dphEncoding dph))
                        then Left "Parquet.Read: expected PLAIN_DICTIONARY or RLE_DICTIONARY encoding for dictionary optional column"
                        else do
                          dict0 <- case mDict of
                            Nothing ->
                              Left "Parquet.Read: PLAIN_DICTIONARY data page before dictionary page"
                            Just d -> Right d
                          let !n = fromIntegral (dphNumValues dph) :: Int
                          (_rep, def, rest) <- parseDataPageV1Levels maxRep maxDef n raw
                          let !maxD = fromIntegral maxDef :: Int32
                              !nDefined = VP.foldl' (\a d -> if d == maxD then a + 1 else a) 0 def
                          ix <- decodeDictionaryIndices nDefined rest
                          page <- materializeDictOptional def maxDef ix dict0 lookupDict
                          go nextOff mDict (acc V.++ page)
                _ -> Left "Parquet.Read: expected DICTIONARY_PAGE or DATA_PAGE"

materializeDictOptional ::
  VP.Vector Int32 ->
  Int ->
  VP.Vector Int32 ->
  dict ->
  (dict -> Int32 -> Maybe a) ->
  Either String (V.Vector (Maybe a))
materializeDictOptional defs maxDef indices dict lookupDict =
  let !n = VP.length defs
      !maxD = fromIntegral maxDef :: Int32
      go !acc !i !ixPos
        | i >= n =
            if ixPos == VP.length indices
              then Right acc
              else Left "Parquet.Read: unconsumed dictionary indices"
        | otherwise =
            let !d = VP.unsafeIndex defs i
            in if d == maxD
              then
                if ixPos >= VP.length indices
                  then Left "Parquet.Read: ran out of dictionary indices"
                  else
                    let !idx = VP.unsafeIndex indices ixPos
                    in case lookupDict dict idx of
                      Nothing -> Left "Parquet.Read: dictionary index out of range"
                      Just v -> go (Just v : acc) (i + 1) (ixPos + 1)
              else go (Nothing : acc) (i + 1) ixPos
  in case go [] 0 0 of
    Left e -> Left e
    Right xs -> Right $! V.fromList (reverse xs)

-- | Specialized dictionary optional reader for @INT32@ columns.
readDictionaryInt32OptionalColumnChunk ::
  Compression -> Int -> Int -> ByteString -> Either String (V.Vector (Maybe Int32))
readDictionaryInt32OptionalColumnChunk =
  readDictionaryOptionalColumnChunk decodePlainInt32 vpLookupInt32
  where
    vpLookupInt32 :: VP.Vector Int32 -> Int32 -> Maybe Int32
    vpLookupInt32 v idx =
      let !i = fromIntegral idx :: Int
      in if i >= 0 && i < VP.length v
        then Just (VP.unsafeIndex v i)
        else Nothing

-- | Decompress a @DATA_PAGE_V2@ body. In v2 the repetition\/definition levels
-- are stored uncompressed before the (optionally compressed) values section.
{-# INLINE decompressDataPageV2Body #-}
decompressDataPageV2Body ::
  Compression ->
  Int ->
  Int ->
  ByteString ->
  Int ->
  Either String (PageHeader, DataPageHeaderV2, VP.Vector Int32, VP.Vector Int32, ByteString, Int)
decompressDataPageV2Body codec maxRep maxDef chunk off = do
  (hdr, afterHdr) <- readPageHeaderAt chunk off
  if phType hdr /= pageTypeDataPageV2
    then Left "Parquet.Read: expected DATA_PAGE_V2"
    else do
      compSz <- case phCompressedPageSize hdr of
        Nothing -> Left "Parquet.Read: missing compressed_page_size"
        Just s -> Right (fromIntegral s :: Int)
      dph2 <- case phDataPageV2 hdr of
        Nothing -> Left "Parquet.Read: missing data_page_header_v2"
        Just d -> Right d
      let !bodyStart = afterHdr
      if bodyStart + compSz > BS.length chunk
        then Left "Parquet.Read: truncated page v2 body"
        else do
          let !body = BS.take compSz (BS.drop bodyStart chunk)
              !repLen = fromIntegral (dph2RepLevelsLen dph2) :: Int
              !defLen = fromIntegral (dph2DefLevelsLen dph2) :: Int
              !levelsLen = repLen + defLen
          if levelsLen > BS.length body
            then Left "Parquet.Read: v2 levels exceed body size"
            else do
              let !numValues = fromIntegral (dph2NumValues dph2) :: Int
                  !repBs = BS.take repLen body
                  !defBs = BS.take defLen (BS.drop repLen body)
                  !valuesSection = BS.drop levelsLen body
                  !bwRep = levelBitWidth maxRep
                  !bwDef = levelBitWidth maxDef
              repLevels <- if repLen == 0
                then Right (VP.replicate numValues 0)
                else decodeHybridRleUnsigned32 bwRep numValues repBs
              defLevels <- if defLen == 0
                then Right (VP.replicate numValues 0)
                else decodeHybridRleUnsigned32 bwDef numValues defBs
              !values <- if dph2IsCompressed dph2
                then decompressPageData codec (phUncompressedPageSize hdr) valuesSection
                else Right valuesSection
              let !nextOff = bodyStart + compSz
              Right (hdr, dph2, repLevels, defLevels, values, nextOff)

readLE32 :: ByteString -> Int -> Word32
readLE32 bs o =
  let b0 = fromIntegral (BS.index bs o) :: Word32
      b1 = fromIntegral (BS.index bs (o + 1)) :: Word32
      b2 = fromIntegral (BS.index bs (o + 2)) :: Word32
      b3 = fromIntegral (BS.index bs (o + 3)) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)

readLE64 :: ByteString -> Int -> Word64
readLE64 bs o =
  let w0 = fromIntegral (readLE32 bs o) :: Word64
      w1 = fromIntegral (readLE32 bs (o + 4)) :: Word64
  in w0 .|. (w1 `shiftL` 32)

-- | @PLAIN@ @INT96@ — each value is 12 raw bytes, returned as 'ByteString'.
{-# INLINE decodePlainInt96 #-}
decodePlainInt96 :: Int -> ByteString -> Either String (V.Vector ByteString)
decodePlainInt96 n bs
  | n < 0 = Left "Parquet.Read: PLAIN INT96 negative count"
  | n > BS.length bs `div` 12 =
      Left "Parquet.Read: PLAIN INT96 buffer too small"
  | otherwise = Right $! V.generate n $ \i ->
      BS.take 12 (BS.drop (i * 12) bs)

-- | @PLAIN@ @FIXED_LEN_BYTE_ARRAY@ — each value is @typeLen@ raw bytes.
{-# INLINE decodePlainFixedLenByteArray #-}
decodePlainFixedLenByteArray :: Int -> Int -> ByteString -> Either String (V.Vector ByteString)
decodePlainFixedLenByteArray typeLen n bs
  | typeLen <= 0 = Left "Parquet.Read: FIXED_LEN_BYTE_ARRAY type_length must be positive"
  | n < 0 = Left "Parquet.Read: FIXED_LEN_BYTE_ARRAY negative count"
  -- Use 'div' on the larger side so the multiplication 'n * typeLen'
  -- never happens (overflow-safe on hostile input).
  | n > BS.length bs `div` typeLen =
      Left "Parquet.Read: PLAIN FIXED_LEN_BYTE_ARRAY buffer too small"
  | otherwise = Right $! V.generate n $ \i ->
      BS.take typeLen (BS.drop (i * typeLen) bs)

-- | All @DATA_PAGE@s as @PLAIN@ @INT96@.
readPlainInt96ColumnChunk :: Compression -> ByteString -> Either String (V.Vector ByteString)
readPlainInt96ColumnChunk codec chunk = go 0 V.empty
  where
    go !off !acc
      | off >= BS.length chunk = Right acc
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= encPlain
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              pageVec <- decodePlainInt96 n raw
              go nextOff (acc V.++ pageVec)

-- | All @DATA_PAGE@s as @PLAIN@ @FIXED_LEN_BYTE_ARRAY@ with given type length.
readPlainFixedLenByteArrayColumnChunk :: Int -> Compression -> ByteString -> Either String (V.Vector ByteString)
readPlainFixedLenByteArrayColumnChunk typeLen codec chunk = go 0 V.empty
  where
    go !off !acc
      | off >= BS.length chunk = Right acc
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= encPlain
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              pageVec <- decodePlainFixedLenByteArray typeLen n raw
              go nextOff (acc V.++ pageVec)

-- | @BYTE_STREAM_SPLIT@ for @FLOAT@: bytes are transposed into 4 runs of N bytes.
{-# INLINE decodeByteStreamSplitFloat #-}
decodeByteStreamSplitFloat :: Int -> ByteString -> Either String (VP.Vector Float)
decodeByteStreamSplitFloat n bs
  | n < 0 = Left "Parquet.Read: BYTE_STREAM_SPLIT FLOAT negative count"
  | n > BS.length bs `div` 4 =
      Left "Parquet.Read: BYTE_STREAM_SPLIT FLOAT buffer too small"
  | otherwise =
      Right $
        runST $ do
          mv <- MVP.new n
          let go2 !i
                | i >= n = VP.unsafeFreeze mv
                | otherwise = do
                    let !b0 = fromIntegral (BS.index bs i) :: Word32
                        !b1 = fromIntegral (BS.index bs (n + i)) :: Word32
                        !b2 = fromIntegral (BS.index bs (2 * n + i)) :: Word32
                        !b3 = fromIntegral (BS.index bs (3 * n + i)) :: Word32
                        !w = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
                    MVP.write mv i (castWord32ToFloat w)
                    go2 (i + 1)
          go2 0

-- | @BYTE_STREAM_SPLIT@ for @DOUBLE@: bytes are transposed into 8 runs of N bytes.
{-# INLINE decodeByteStreamSplitDouble #-}
decodeByteStreamSplitDouble :: Int -> ByteString -> Either String (VP.Vector Double)
decodeByteStreamSplitDouble n bs
  | n < 0 = Left "Parquet.Read: BYTE_STREAM_SPLIT DOUBLE negative count"
  | n > BS.length bs `div` 8 =
      Left "Parquet.Read: BYTE_STREAM_SPLIT DOUBLE buffer too small"
  | otherwise =
      Right $
        runST $ do
          mv <- MVP.new n
          let go2 !i
                | i >= n = VP.unsafeFreeze mv
                | otherwise = do
                    let !b0 = fromIntegral (BS.index bs i) :: Word64
                        !b1 = fromIntegral (BS.index bs (n + i)) :: Word64
                        !b2 = fromIntegral (BS.index bs (2 * n + i)) :: Word64
                        !b3 = fromIntegral (BS.index bs (3 * n + i)) :: Word64
                        !b4 = fromIntegral (BS.index bs (4 * n + i)) :: Word64
                        !b5 = fromIntegral (BS.index bs (5 * n + i)) :: Word64
                        !b6 = fromIntegral (BS.index bs (6 * n + i)) :: Word64
                        !b7 = fromIntegral (BS.index bs (7 * n + i)) :: Word64
                        !w = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
                              .|. (b4 `shiftL` 32) .|. (b5 `shiftL` 40) .|. (b6 `shiftL` 48) .|. (b7 `shiftL` 56)
                    MVP.write mv i (castWord64ToDouble w)
                    go2 (i + 1)
          go2 0
