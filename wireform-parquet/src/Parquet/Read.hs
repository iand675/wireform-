{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
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
  , loadParquetFileEncrypted
  , loadParquetFilePath
  , openParquetReader
  , FooterDecryption (..)
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
  , decodePlainByteArrayAsText
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
    -- * Generic per-page dispatch
    --
    -- | The 'readGeneric*ColumnChunk' family handles every encoding the
    -- spec defines for the matching physical type, dispatching on
    -- 'phType' (DATA_PAGE / DATA_PAGE_V2 / DICTIONARY_PAGE) and the
    -- per-page encoding tag. Use these in preference to the
    -- @readPlain*@ helpers when reading files produced by other
    -- writers — wireform's own writer emits PLAIN, but pyarrow /
    -- parquet-cpp / arrow-rs routinely produce dictionary-encoded
    -- BYTE_ARRAY columns + DELTA_BINARY_PACKED INT32/INT64 +
    -- BYTE_STREAM_SPLIT FLOAT/DOUBLE + DATA_PAGE_V2 pages, all of
    -- which the @readPlain*@ helpers reject.
  , readGenericInt32ColumnChunk
  , readGenericInt64ColumnChunk
  , readGenericFloatColumnChunk
  , readGenericDoubleColumnChunk
  , readGenericBoolColumnChunk
  , readGenericByteArrayColumnChunk
  , readGenericTextColumnChunk
    -- ** Optional / nullable variants
  , readGenericInt32OptionalColumnChunk
  , readGenericInt64OptionalColumnChunk
  , readGenericFloatOptionalColumnChunk
  , readGenericDoubleOptionalColumnChunk
  , readGenericBoolOptionalColumnChunk
  , readGenericByteArrayOptionalColumnChunk
    -- ** Flat-shape ('NullableView') optional readers
    --
    -- These return @(VU.Vector Bit, VS.Vector a)@ pair-shaped
    -- 'AC.NullableView's directly: per-page validity bits and
    -- dense storable values are written into pre-allocated
    -- mutable buffers as the level / value streams are
    -- consumed. They never go through @V.Vector (Maybe a)@,
    -- so there is no 'Just'/'Nothing' allocation, no per-row
    -- pointer indirection, and no second walk to convert to
    -- the bridge's 'NullableView' shape. Use these instead of
    -- the @V.Vector (Maybe a)@ variants when feeding the
    -- Arrow bridge.
  , readGenericInt32OptionalColumnChunkNV
  , readGenericInt64OptionalColumnChunkNV
  , readGenericFloatOptionalColumnChunkNV
  , readGenericDoubleOptionalColumnChunkNV
  , readGenericBoolOptionalColumnChunkNV
  , readGenericByteArrayOptionalColumnChunkNV
    -- ** Flat-shape ('BinaryView') non-nullable byte-array reader
  , readGenericByteArrayColumnChunkBV
    -- * Page-index-driven page skipping
  , readGenericInt32SelectedPages
  , readGenericInt64SelectedPages
  , readGenericFloatSelectedPages
  , readGenericDoubleSelectedPages
  , readGenericBoolSelectedPages
  , readGenericByteArraySelectedPages
    -- ** Optional / nullable variants
  , readGenericInt32OptionalSelectedPages
  , readGenericInt64OptionalSelectedPages
  , readGenericFloatOptionalSelectedPages
  , readGenericDoubleOptionalSelectedPages
  , readGenericBoolOptionalSelectedPages
  , readGenericByteArrayOptionalSelectedPages
  ) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int32, Int64)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as MVP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Arrow.Column as AC
import qualified Arrow.View as AV
import Columnar.Bit (Bit (..))
import qualified Data.ByteString.Unsafe as BSU
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Marshal.Utils (copyBytes)
import Data.Word (Word8)
import Unsafe.Coerce (unsafeCoerce)
import Data.Text (Text)
import qualified Data.Text.Array as TA
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Internal as TI
import Data.Word (Word8, Word32, Word64)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.Exts (ByteArray#)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import qualified Data.Primitive.ByteArray as PBA
import System.IO.Unsafe (unsafePerformIO)

import qualified Columnar.SIMD as SIMD
import Columnar.SIMD (unpackBitsLsbUnsafe)

import Parquet.Delta
  ( decodeDeltaBinaryPackedInt32
  , decodeDeltaBinaryPackedInt64
  , decodeDeltaByteArray
  , decodeDeltaLengthByteArray
  )
import Parquet.RLE (decodeDictionaryIndices, decodeHybridRleLengthPrefixed, decodeHybridRleUnsigned32)

import qualified Codec.Compression.GZip as GZip
#ifdef HAVE_ZSTD
import Codec.Compression.Zstd (Decompress (..), decompress)
#endif
#ifdef HAVE_SNAPPY
import qualified Codec.Compression.Snappy as Snappy
#endif
#ifdef HAVE_LZ4
import qualified Columnar.LZ4 as LZ4
#endif
#ifdef HAVE_BROTLI
import qualified Codec.Compression.Brotli as Brotli
#endif

import qualified Columnar.IO as IO
import qualified Columnar.Stream as IS
import qualified Parquet.Encryption as Enc
import Parquet.Footer (readFooter)
import qualified Parquet.Footer as F
import qualified Thrift.Decode as TC
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
  , PageType (..)
  , readPageHeaderAt
  )
import Parquet.Types
  ( ColumnChunk (..)
  , ColumnMetadata (..)
  , Compression (..)
  , FileMetadata (..)
  , PageLocation (..)
  , RowGroup (..)
  )

-- | A Parquet file loaded in memory with parsed footer metadata.
data ParquetFile = ParquetFile
  { pfBytes  :: !ByteString
  , pfFooter :: !FileMetadata
  } deriving stock (Show, Eq)

-- | Parse the footer from the tail of a complete @PAR1@ file.
-- Refuses encrypted-footer files with a clear error pointing to
-- 'loadParquetFileEncrypted'.
loadParquetFile :: ByteString -> Either String ParquetFile
loadParquetFile bs = do
  fm <- readFooter bs
  Right ParquetFile {pfBytes = bs, pfFooter = fm}

-- | Read a Parquet file from disk and parse its footer.
--
-- Uses 'Columnar.IO.loadFile' under the hood, which mmaps
-- files above 'Columnar.IO.defaultLoadStrategy''s threshold
-- (64 KiB) and reads smaller files eagerly. The 'ParquetFile'
-- references the loaded bytes directly, so opening a 50 GB
-- file costs roughly a syscall + page-fault-on-access cost
-- rather than copying the whole file into the GC heap.
--
-- For an explicit choice, use 'Columnar.IO.loadFileMmap' /
-- 'Columnar.IO.loadFileEager' and pass the bytes to
-- 'loadParquetFile' directly.
loadParquetFilePath :: FilePath -> IO (Either String ParquetFile)
loadParquetFilePath path = do
  bs <- IO.loadFile path
  pure (loadParquetFile bs)

-- | Open a Parquet file as an 'IS.IterIO' over its row groups.
-- Each step yields one row-group index on demand; callers
-- join with 'Parquet.Arrow.parquetRowGroupToArrow' (or any
-- per-format reader) to materialise columns lazily.
--
-- Pairs with 'loadParquetFilePath''s mmap-aware loader: the
-- file's bytes are mmapped (on files above the default
-- threshold), so per-row-group slices are pointer arithmetic
-- and only the touched pages are paged in by the kernel. A
-- 50 GB file with one queried row group reads only the
-- footer + the slice at that row group's offset.
openParquetReader
  :: FilePath
  -> IO (Either String (ParquetFile, IS.IterIO Int))
    -- ^ Returns the parsed footer + an iterator that yields
    -- one row-group index at a time. Callers join the index
    -- with the file's per-format readers (e.g.
    -- 'Parquet.Arrow.parquetRowGroupToArrow') to materialise
    -- columns lazily.
openParquetReader path = do
  loaded <- loadParquetFilePath path
  case loaded of
    Left e -> pure (Left e)
    Right pf ->
      let !nRg = V.length (fmRowGroups (pfFooter pf))
          step ref
            | ref >= nRg = pure (Right Nothing)
            | otherwise  = pure (Right (Just ref))
          mkIter k = IS.IterIO $ do
            r <- step k
            pure $ case r of
              Left e -> Left e
              Right Nothing -> Right IS.IterIODone
              Right (Just i) -> Right (IS.IterIOYield i (mkIter (k + 1)))
      in pure (Right (pf, mkIter 0))

-- | Footer-decryption configuration. Mirrors
-- 'Parquet.Write.FooterEncryption' but on the read side: the AAD
-- prefix and file id must match what the writer used or GCM auth
-- will reject the trailing module.
data FooterDecryption = FooterDecryption
  { fdKey       :: !ByteString
  , fdFileId    :: !ByteString
  , fdAadPrefix :: !ByteString
  } deriving (Show, Eq)

-- | Parse a Parquet file whose trailing magic is either @PAR1@
-- (plaintext footer) or @PARE@ (encrypted footer). For @PARE@ files
-- the supplied 'FooterDecryption' is used to decrypt the footer
-- module under @ModuleFooter@ AAD. For @PAR1@ files the
-- 'FooterDecryption' is ignored; this is convenient for callers that
-- want a single entry point for "files that may or may not have an
-- encrypted footer".
--
-- For @PARE@ files the bytes between the leading @PAR1@ magic and
-- the trailing @PARE@ magic match the parquet-format §5.4 layout:
-- @<FileCryptoMetaData thrift> <encrypted footer module>@. We
-- skip the FileCryptoMetaData (the caller supplies the key
-- separately) and decrypt the footer module under ModuleFooter AAD.
loadParquetFileEncrypted :: FooterDecryption -> ByteString -> Either String ParquetFile
loadParquetFileEncrypted fd bs = do
  trailer <- F.readFooterTrailer bs
  thriftBytes <- if F.ftMagic trailer == F.parquetEncryptedMagic
    then do
      -- Skip past the FileCryptoMetaData thrift; what remains is
      -- the encrypted footer blob (nonce || ct || tag).
      (_, encStart) <- skipFileCryptoMetaData (F.ftBytes trailer)
      let !encModule = BS.drop encStart (F.ftBytes trailer)
          !suffix    = Enc.buildAadSuffix
                        (fdFileId fd) Enc.ModuleFooter 0 0 0
          !aad       = Enc.buildAad (fdAadPrefix fd) suffix
      Enc.decryptGcmModule (fdKey fd) aad encModule
    else
      Right (F.ftBytes trailer)
  fm <- F.readFooterRaw thriftBytes
  Right ParquetFile {pfBytes = bs, pfFooter = fm}

-- | Walk past a Thrift compact-encoded @FileCryptoMetaData@ struct
-- and report the byte offset just past it. We only need the offset;
-- the parsed value is discarded because the caller already has the
-- key + AAD context.
skipFileCryptoMetaData :: ByteString -> Either String ((), Int)
skipFileCryptoMetaData bs = do
  -- The thrift compact codec we use already supports streaming
  -- offsets via decodeCompactFrom; this pattern mirrors what the
  -- page header reader does.
  (_, off) <- TC.decodeCompactFrom bs 0
  Right ((), off)

-- | Raw bytes for one column chunk.
--
-- A column chunk's wire layout is
-- @[dictionary page (optional)][data page 1]...[data page N]@.
-- When the column uses dictionary encoding 'cmDictionaryPageOffset'
-- points at the dictionary page, which sits immediately before
-- 'cmDataPageOffset'; the chunk's slice must start at the
-- dictionary page so the page-walking decoder sees the dictionary
-- before any RLE_DICTIONARY data page references it.
--
-- Per the Parquet spec the rule is: start at @min(dictionary_page_offset,
-- data_page_offset)@ when both are present, else start at
-- @data_page_offset@. 'cmTotalCompressedSize' covers both.
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
  let !dataOff = fromIntegral (cmDataPageOffset meta) :: Int
      !startOff = case cmDictionaryPageOffset meta of
        Just dpo
          | dpo > 0 && fromIntegral dpo < dataOff ->
              fromIntegral dpo :: Int
        _ -> dataOff
      !sz  = fromIntegral (cmTotalCompressedSize meta) :: Int
      !bs0 = pfBytes pf
  if startOff < 0 || sz < 0 || startOff + sz > BS.length bs0
    then Left "Parquet.Read: column chunk slice out of bounds"
    else Right $! BS.take sz (BS.drop startOff bs0)

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

-- | Thrift @Encoding@ for @RLE@ (encoding 3). In Parquet V1
-- this only appears as a column-chunk-level encoding for
-- repetition / definition levels (not data); in V2 BOOLEAN
-- columns are encoded as RLE/bit-packed directly.
encRle :: Int32
encRle = 3

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
  dph <- case phType hdr of
    PtDataPage d -> Right d
    _            -> Left "Parquet.Read: expected DATA_PAGE"
  do
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
          Right (hdr, dph, raw, nextOff)

-- | Walk every DATA_PAGE in a chunk, decode each one with the
-- supplied per-page decoder, and collect the results as a
-- /reverse/-cons list of pages. Caller is expected to call
-- 'VP.concat (reverse pages)' or similar.
--
-- Was previously fused into each per-type readPlain*ColumnChunk
-- as 'go nextOff (acc VP.++ pageVec)' which was O(K^2) in
-- page count K — copying the whole accumulator each step. The
-- reverse-cons + single concat at the end is O(K + total
-- elements).
{-# INLINE collectPagesPrim #-}
-- | Walk a column chunk and apply a per-page decoder of any
-- vector flavour (VS / VP / V / VU). The per-page result type
-- is whatever the decoder returns; the caller chooses how to
-- concatenate ('VS.concat', 'VP.concat', 'V.concat', etc.).
collectPagesPrim
  :: ByteString
  -> Compression
  -> Int32                         -- ^ Required PLAIN-style encoding tag
  -> (Int -> ByteString -> Either String v)
  -> Either String [v]
collectPagesPrim chunk codec wantedEnc decode = go 0 []
  where
    go !off !acc
      | off >= BS.length chunk = Right acc
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= wantedEnc
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              page <- decode n raw
              go nextOff (page : acc)

{-# INLINE collectPagesBoxed #-}
collectPagesBoxed
  :: ByteString
  -> Compression
  -> Int32
  -> (Int -> ByteString -> Either String (V.Vector a))
  -> Either String [V.Vector a]
collectPagesBoxed chunk codec wantedEnc decode = go 0 []
  where
    go !off !acc
      | off >= BS.length chunk = Right acc
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= wantedEnc
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              page <- decode n raw
              go nextOff (page : acc)

{-# INLINE collectOptionalPagesBoxed #-}
collectOptionalPagesBoxed
  :: ByteString
  -> Compression
  -> Int
  -> Int
  -> (VP.Vector Int32 -> Int -> ByteString -> Either String (V.Vector (Maybe a)))
  -> Either String [V.Vector (Maybe a)]
collectOptionalPagesBoxed chunk codec maxRep maxDef mat = go 0 []
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
              go nextOff (page : acc)

-- | Read every @DATA_PAGE@ with @PLAIN@ @INT32@ in order until the chunk ends.
readPlainInt32ColumnChunk :: Compression -> ByteString -> Either String (VS.Vector Int32)
readPlainInt32ColumnChunk codec chunk = do
  pages <- collectPagesPrim chunk codec encPlain decodePlainInt32
  Right (VS.concat (reverse pages))

-- | Read the first data page of a chunk as @PLAIN@ @INT32@ values.
readPlainInt32FirstPage :: Compression -> ByteString -> Either String (VS.Vector Int32)
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
readPlainOptionalColumnChunkWith codec maxRep maxDef chunk mat = do
  pages <- collectOptionalPagesBoxed chunk codec maxRep maxDef mat
  Right (V.concat (reverse pages))

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
readPlainInt64ColumnChunk :: Compression -> ByteString -> Either String (VS.Vector Int64)
readPlainInt64ColumnChunk codec chunk = do
  pages <- collectPagesPrim chunk codec encPlain decodePlainInt64
  Right (VS.concat (reverse pages))

-- | @PLAIN@ @FLOAT@ (IEEE little-endian).
readPlainFloatColumnChunk :: Compression -> ByteString -> Either String (VS.Vector Float)
readPlainFloatColumnChunk codec chunk = do
  pages <- collectPagesPrim chunk codec encPlain decodePlainFloat
  Right (VS.concat (reverse pages))

-- | @PLAIN@ @DOUBLE@ (IEEE little-endian).
readPlainDoubleColumnChunk :: Compression -> ByteString -> Either String (VS.Vector Double)
readPlainDoubleColumnChunk codec chunk = do
  pages <- collectPagesPrim chunk codec encPlain decodePlainDouble
  Right (VS.concat (reverse pages))

-- | @PLAIN@ @BOOLEAN@ (packed bits, LSB of first byte is first value).
readPlainBoolColumnChunk :: Compression -> ByteString -> Either String (V.Vector Bool)
readPlainBoolColumnChunk codec chunk = do
  pages <- collectPagesBoxed chunk codec encPlain decodePlainBool
  Right (V.concat (reverse pages))

-- | @PLAIN@ @BYTE_ARRAY@ (length-prefixed 4-byte LE + bytes per value).
readPlainByteArrayColumnChunk :: Compression -> ByteString -> Either String (V.Vector ByteString)
readPlainByteArrayColumnChunk codec chunk = do
  pages <- collectPagesBoxed chunk codec encPlain decodePlainByteArray
  Right (V.concat (reverse pages))

-- | Dictionary page (@PLAIN@ @INT32@ values) followed by @DATA_PAGE@s with
-- @PLAIN_DICTIONARY@ (indices as @PLAIN@ @INT32@). Plain @DATA_PAGE@s without
-- a dictionary are also accepted.
readPlainDictionaryInt32ColumnChunk :: Compression -> ByteString -> Either String (VS.Vector Int32)
readPlainDictionaryInt32ColumnChunk codec chunk = do
  -- Reverse-cons of pages, concat once at end. O(K + total
  -- elements) instead of O(K^2 + total elements) for K data
  -- pages.
  (mDict, pagesRev) <- go 0 Nothing []
  if null pagesRev && case mDict of { Just _ -> True; Nothing -> False }
    then Left "Parquet.Read: empty dictionary column chunk"
    else if null pagesRev
           then Left "Parquet.Read: empty dictionary column chunk"
           else Right (VS.concat (reverse pagesRev))
  where
    go !off !mDict !pagesRev
      | off >= BS.length chunk = Right (mDict, pagesRev)
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
                PtDictionaryPage dk
                  | not (dictEncoding dk == encPlain || dictEncoding dk == encPlainDictionary) ->
                      Left "Parquet.Read: dictionary page encoding is neither PLAIN (0) nor PLAIN_DICTIONARY (2)"
                  | otherwise -> do
                      let !nDict = fromIntegral (dictNumValues dk) :: Int
                      dict <- decodePlainInt32 nDict raw
                      go nextOff (Just dict) pagesRev
                PtDataPage dph -> case dphEncoding dph of
                  e
                    | e == encPlain -> do
                        let !n = fromIntegral (dphNumValues dph) :: Int
                        pageVec <- decodePlainInt32 n raw
                        go nextOff mDict (pageVec : pagesRev)
                    | isDictionaryEncoding e -> do
                        dict0 <- case mDict of
                          Nothing ->
                            Left "Parquet.Read: PLAIN_DICTIONARY data page before dictionary page"
                          Just d -> Right d
                        let !n = fromIntegral (dphNumValues dph) :: Int
                        ix <- decodeDictionaryIndices n raw
                        case dictLookupVS dict0 ix of
                          Left e' -> Left e'
                          Right pageVec ->
                            go nextOff mDict (pageVec : pagesRev)
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
#ifdef HAVE_BROTLI
decompressChunk Brotli bs = tryBrotli bs
#else
decompressChunk Brotli _ =
  Left "Parquet.Read: Brotli requires building wireform with -fbrotli"
#endif
decompressChunk LZO _ =
  Left "Parquet.Read: LZO (codec 3) is not supported; it's a legacy Hadoop codec not emitted by modern writers"
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
#ifdef HAVE_BROTLI
      ++ ", Brotli with -fbrotli"
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
-- | Decompress an LZ4_RAW block — the codec Parquet's spec
-- numbers as 7. The block is in raw LZ4 block format with no
-- header at all; 'Columnar.LZ4.decompress' wraps a direct FFI
-- call to @LZ4_decompress_safe@ from @liblz4@.
tryLZ4Raw :: Int -> ByteString -> Either String ByteString
tryLZ4Raw uncompSize bs = case LZ4.decompress uncompSize bs of
  Right out
    | BS.length out == uncompSize -> Right out
    | otherwise -> Left $
        "Parquet.Read: LZ4_RAW size mismatch (page header said "
          ++ show uncompSize ++ ", got " ++ show (BS.length out)
          ++ "; the file is malformed or the page header is wrong)"
  Left e -> Left ("Parquet.Read: " ++ e)
#endif

#ifdef HAVE_BROTLI
tryBrotli :: ByteString -> Either String ByteString
tryBrotli bs =
  unsafePerformIO $ do
    er <- try @SomeException $ evaluate $
            BL.toStrict $ Brotli.decompress $ BL.fromStrict bs
    case er of
      Left e  -> pure $ Left $ "Parquet.Read: Brotli decompress failed: " ++ show e
      Right x -> pure $ Right x
#endif

-- | Decode PLAIN-encoded fixed-width primitives.
--
-- Implementation note (perf): the previous version called
-- 'readLE32'/'readLE64' per element, which each performed
-- bounds-checked 'BS.index' calls byte-by-byte and reassembled
-- the word with shifts (~10-20 ops per value). For 100k Int64
-- values that is hundreds of thousands of bounds checks.
--
-- For Int32 / Int64 / Float / Double the Parquet PLAIN wire
-- format is little-endian and bit-compatible with the host
-- (x86_64 / aarch64 — both little-endian). The fastest decoder
-- is therefore a single 'memcpy' from the source bytestring
-- into the destination primitive vector's underlying mutable
-- byte array; this is what 'decodePlainPrimLEMemcpy' does.
decodePlainInt32 :: Int -> ByteString -> Either String (VS.Vector Int32)
decodePlainInt32 = decodePlainPrimLEMemcpyVS 4
{-# INLINE decodePlainInt32 #-}

decodePlainInt64 :: Int -> ByteString -> Either String (VS.Vector Int64)
decodePlainInt64 = decodePlainPrimLEMemcpyVS 8
{-# INLINE decodePlainInt64 #-}

decodePlainFloat :: Int -> ByteString -> Either String (VS.Vector Float)
decodePlainFloat = decodePlainPrimLEMemcpyVS 4
{-# INLINE decodePlainFloat #-}

decodePlainDouble :: Int -> ByteString -> Either String (VS.Vector Double)
decodePlainDouble = decodePlainPrimLEMemcpyVS 8
{-# INLINE decodePlainDouble #-}

-- | Decode a PLAIN-encoded primitive vector by a single
-- 'memcpy' from the source bytestring into a freshly-allocated
-- /storable/ vector ('VS.Vector', 'ForeignPtr'-backed). Assumes
-- the host byte order matches the wire format (Parquet PLAIN
-- is little-endian; x86_64 and aarch64 are little-endian).
--
-- This is the read-side counterpart of the Arrow bridge: the
-- Arrow column types (after the views migration) hold
-- 'VS.Vector' for primitives, so decoding straight into VS
-- means the bridge can take the result with no extra copy
-- (the previous shape allocated a 'VP.Vector' here and the
-- bridge then ran 'VS.convert', which memcpy'd ByteArray ->
-- ForeignPtr -- one full N-element pass per column chunk).
{-# INLINE decodePlainPrimLEMemcpyVS #-}
decodePlainPrimLEMemcpyVS
  :: forall a. VS.Storable a
  => Int                       -- ^ element size in bytes
  -> Int                       -- ^ number of elements
  -> ByteString
  -> Either String (VS.Vector a)
decodePlainPrimLEMemcpyVS !elemBytes n bs
  | n <= 0 = Right VS.empty
  | BS.length bs < n * elemBytes =
      Left "Parquet.Read: PLAIN buffer too small"
  | otherwise = Right $! BSI.accursedUnutterablePerformIO $ do
      -- Allocate a fresh ForeignPtr-backed mutable storable
      -- vector of the right size, memcpy from the
      -- ByteString into it, freeze. One copy total.
      mv <- VSM.unsafeNew n :: IO (VSM.IOVector a)
      let !nBytes = n * elemBytes
      VSM.unsafeWith mv $ \dstPtr ->
        BSU.unsafeUseAsCStringLen bs $ \(srcCStr, _) ->
          copyBytes (castPtr dstPtr :: Ptr Word8)
                    (castPtr srcCStr :: Ptr Word8)
                    nBytes
      VS.unsafeFreeze mv

decodePlainBool :: Int -> ByteString -> Either String (V.Vector Bool)
decodePlainBool n bs =
  let !need = (n + 7) `quot` 8
  in if BS.length bs < need
    then Left "Parquet.Read: PLAIN BOOLEAN buffer too small"
    else Right $! unpackBitsLsbUnsafe n bs

-- | Decode a PLAIN-encoded BYTE_ARRAY page body into a vector
-- of @n@ slices into @bs0@.
--
-- Implementation note: the previous version of this function
-- accumulated values via @V.snoc acc payload@ in a tight loop,
-- which is O(n²) — for a 100k-row page that meant ~5 billion
-- copies and dominated the entire read path (~4.5 s on a 4-col
-- benchmark; 99% of total read time). We now allocate a
-- mutable boxed vector once and write each slice in O(1), for
-- O(n) total work. Each payload is a slice into @bs0@ (no
-- copy), matching the old behaviour.
decodePlainByteArray :: Int -> ByteString -> Either String (V.Vector ByteString)
decodePlainByteArray n bs0
  | n <= 0    = Right V.empty
  | otherwise = runST $ do
      mv <- VM.unsafeNew n
      let totalLen = BS.length bs0
          go !i !off
            | i >= n    = pure (Right ())
            | off + 4 > totalLen =
                pure (Left "Parquet.Read: PLAIN BYTE_ARRAY truncated length")
            | otherwise =
                let !len  = fromIntegral (readLE32 bs0 off) :: Int
                    !off2 = off + 4
                in  if len < 0 || off2 + len > totalLen
                      then pure (Left "Parquet.Read: PLAIN BYTE_ARRAY payload out of bounds")
                      else do
                        let !payload = BS.take len (BS.drop off2 bs0)
                        VM.unsafeWrite mv i payload
                        go (i + 1) (off2 + len)
      r <- go 0 0
      case r of
        Left err -> pure (Left err)
        Right () -> do
          v <- V.unsafeFreeze mv
          pure (Right v)

-- | Decode a PLAIN-encoded BYTE_ARRAY page body directly into a
-- 'V.Vector' 'Text', sharing one underlying 'TA.Array' (a
-- 'ByteArray') across every value.
--
-- Implementation note (perf): the obvious implementation
-- (@V.map decodeUtf8Lossy <$> decodePlainByteArray n bs@) does
-- one validate + allocate per value. For 100k 12-byte values
-- that is 100k tiny @ByteArray@ allocations — the bulk of
-- @ColUtf8@ read time.
--
-- Here we (1) ASCII-precheck the entire page body in one pass,
-- (2) on the fast path, allocate ONE 'TA.Array' of size
-- @len(bs0)@, copy each value's bytes into it contiguously,
-- and build N 'Text' values that all share that one
-- 'TA.Array' via the unsafe 'TI.text' constructor. ASCII is
-- always valid UTF-8, so no per-value validation is needed.
--
-- Falls back to the per-value decode if the page contains
-- any byte ≥ 0x80 — including length-prefix bytes. (A
-- string longer than 127 bytes will set bit 7 of one of its
-- length-prefix bytes and trigger the fallback even if the
-- string content is ASCII; this is a perf-only false-negative,
-- correctness is unaffected.)
decodePlainByteArrayAsText :: Int -> ByteString -> Either String (V.Vector Text)
decodePlainByteArrayAsText n bs0
  | n <= 0           = Right V.empty
  | SIMD.isAsciiBS bs0 = decodePlainByteArrayAsTextAscii n bs0
  | otherwise        = do
      bs <- decodePlainByteArray n bs0
      Right $! V.map decodeUtf8LossyTextRead bs

-- | Scan a page body for any byte ≥ 0x80. Delegates to the
-- SIMDe-accelerated 'SIMD.isAsciiBS' (SSE2 OR-then-movemask;
-- portable to ARM NEON via SIMDe).
isAsciiPage :: ByteString -> Bool
isAsciiPage = SIMD.isAsciiBS

-- | Lossy UTF-8 decoder used as the fallback. Kept here (rather
-- than imported from @Parquet.Arrow@) so 'Parquet.Read' has no
-- back-edge into the Arrow layer.
decodeUtf8LossyTextRead :: ByteString -> Text
decodeUtf8LossyTextRead bs = case TE.decodeUtf8' bs of
  Right t -> t
  Left _  -> TE.decodeUtf8With TEE.lenientDecode bs

-- | ASCII fast path for 'decodePlainByteArrayAsText'. Caller
-- guarantees every byte of @bs0@ is < 0x80 (verified by
-- 'isAsciiPage'), so the value bytes are valid UTF-8 with one
-- byte per code point and no per-value validation is required.
--
-- Two passes:
--
--   1. Walk the page once to compute per-value lengths and the
--      total content size. Stores lengths into a primitive
--      vector for re-use in pass 2. This pass costs only the
--      4-byte LE length reads (~6 ns/value).
--   2. Allocate the destination 'TA.Array' at exactly the
--      content size, copy each value's bytes into it
--      contiguously, and build N 'Text' values that share the
--      single 'TA.Array' via the unsafe 'TI.text' constructor.
--
-- The two-pass shape is faster than a one-pass shape that
-- over-allocates @len(bs0)@ bytes: it removes ~33% of nursery
-- pressure (length prefixes drop out of the destination) and
-- the second pass becomes a tight @memcpy@ loop over a
-- pre-known offset table.
decodePlainByteArrayAsTextAscii
  :: Int -> ByteString -> Either String (V.Vector Text)
decodePlainByteArrayAsTextAscii n bs0 = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs0 $ \(cstr, totalLen) -> do
    let !srcBase = castPtr cstr :: Ptr Word8
    -- Single bulk memcpy: copy the entire page (including the
    -- 4-byte length prefixes) into one destination ByteArray.
    -- Wastes 4n bytes of memory but eliminates 100k tiny
    -- per-value memcpy calls — the page memcpy runs at ~10 GB/s
    -- in one syscall, vs ~50 ns per call * n for the per-value
    -- variant.
    marr <- PBA.newByteArray totalLen
    PBA.copyPtrToMutableByteArray
      marr 0 (srcBase :: Ptr Word8) totalLen
    frozenPba <- PBA.unsafeFreezeByteArray marr
    let !textArr = pbaToTextArr frozenPba
    mv <- VM.unsafeNew n
    -- Single pass: walk page structure, build Text values
    -- pointing past each value's length prefix into the
    -- shared ByteArray.
    let go !i !srcOff
          | i >= n = pure (Right ())
          | srcOff + 4 > totalLen =
              pure (Left "Parquet.Read: PLAIN BYTE_ARRAY truncated length")
          | otherwise = do
              lenW <- peekByteOff srcBase srcOff :: IO Word32
              let !len = fromIntegral lenW :: Int
                  !srcOff' = srcOff + 4
              if len < 0 || srcOff' + len > totalLen
                then pure (Left "Parquet.Read: PLAIN BYTE_ARRAY payload out of bounds")
                else do
                  -- Force the Text constructor to WHNF as we go
                  -- so the per-value work is captured at decode
                  -- time, not deferred until first access.
                  let !t = TI.text textArr srcOff' len
                  VM.unsafeWrite mv i t
                  go (i + 1) (srcOff' + len)
    r <- go 0 0
    case r of
      Left err -> pure (Left err)
      Right () -> do
        v <- V.unsafeFreeze mv
        pure (Right v)

-- | Coerce a 'PBA.ByteArray' to text's 'TA.Array' (both wrap
-- the same 'ByteArray#' under the hood).
pbaToTextArr :: PBA.ByteArray -> TA.Array
pbaToTextArr (PBA.ByteArray ba#) = TA.ByteArray ba#

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
readDictionaryOptionalColumnChunk decodeDictValues lookupDict codec maxRep maxDef chunk = do
  pagesRev <- go 0 Nothing []
  Right (V.concat (reverse pagesRev))
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
                PtDictionaryPage dk
                  | not (dictEncoding dk == encPlain || dictEncoding dk == encPlainDictionary) ->
                      Left "Parquet.Read: dictionary page encoding is neither PLAIN (0) nor PLAIN_DICTIONARY (2)"
                  | otherwise -> do
                      let !nDict = fromIntegral (dictNumValues dk) :: Int
                      dict <- decodeDictValues nDict raw
                      go nextOff (Just dict) acc
                PtDataPage dph
                  | not (isDictionaryEncoding (dphEncoding dph)) ->
                      Left "Parquet.Read: expected PLAIN_DICTIONARY or RLE_DICTIONARY encoding for dictionary optional column"
                  | otherwise -> do
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
                      go nextOff mDict (page : acc)
                _ -> Left "Parquet.Read: expected DICTIONARY_PAGE or DATA_PAGE"

materializeDictOptional ::
  VP.Vector Int32 ->
  Int ->
  VP.Vector Int32 ->
  dict ->
  (dict -> Int32 -> Maybe a) ->
  Either String (V.Vector (Maybe a))
materializeDictOptional defs maxDef indices dict lookupDict = runST $ do
  let !n      = VP.length defs
      !maxD   = fromIntegral maxDef :: Int32
      !nIdx   = VP.length indices
  mv <- VM.unsafeNew n
  let go !i !ixPos
        | i >= n =
            if ixPos == nIdx
              then pure (Right ())
              else pure (Left "Parquet.Read: unconsumed dictionary indices")
        | otherwise = do
            let !d = VP.unsafeIndex defs i
            if d == maxD
              then if ixPos >= nIdx
                then pure (Left "Parquet.Read: ran out of dictionary indices")
                else do
                  let !idx = VP.unsafeIndex indices ixPos
                  case lookupDict dict idx of
                    Nothing -> pure (Left "Parquet.Read: dictionary index out of range")
                    Just v  -> do
                      VM.unsafeWrite mv i (Just v)
                      go (i + 1) (ixPos + 1)
              else do
                VM.unsafeWrite mv i Nothing
                go (i + 1) ixPos
  r <- go 0 0
  case r of
    Left e   -> pure (Left e)
    Right () -> Right <$> V.unsafeFreeze mv

-- | Specialized dictionary optional reader for @INT32@ columns.
readDictionaryInt32OptionalColumnChunk ::
  Compression -> Int -> Int -> ByteString -> Either String (V.Vector (Maybe Int32))
readDictionaryInt32OptionalColumnChunk =
  readDictionaryOptionalColumnChunk decodePlainInt32 vsLookupInt32
  where
    vsLookupInt32 :: VS.Vector Int32 -> Int32 -> Maybe Int32
    vsLookupInt32 v idx =
      let !i = fromIntegral idx :: Int
      in if i >= 0 && i < VS.length v
        then Just (VS.unsafeIndex v i)
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
  dph2 <- case phType hdr of
    PtDataPageV2 d -> Right d
    _              -> Left "Parquet.Read: expected DATA_PAGE_V2"
  do
      compSz <- case phCompressedPageSize hdr of
        Nothing -> Left "Parquet.Read: missing compressed_page_size"
        Just s -> Right (fromIntegral s :: Int)
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
  | BS.length bs < n * 12 = Left "Parquet.Read: PLAIN INT96 buffer too small"
  | otherwise = Right $! V.generate n $ \i ->
      BS.take 12 (BS.drop (i * 12) bs)

-- | @PLAIN@ @FIXED_LEN_BYTE_ARRAY@ — each value is @typeLen@ raw bytes.
{-# INLINE decodePlainFixedLenByteArray #-}
decodePlainFixedLenByteArray :: Int -> Int -> ByteString -> Either String (V.Vector ByteString)
decodePlainFixedLenByteArray typeLen n bs
  | typeLen <= 0 = Left "Parquet.Read: FIXED_LEN_BYTE_ARRAY type_length must be positive"
  | BS.length bs < n * typeLen = Left "Parquet.Read: PLAIN FIXED_LEN_BYTE_ARRAY buffer too small"
  | otherwise = Right $! V.generate n $ \i ->
      BS.take typeLen (BS.drop (i * typeLen) bs)

-- | All @DATA_PAGE@s as @PLAIN@ @INT96@.
readPlainInt96ColumnChunk :: Compression -> ByteString -> Either String (V.Vector ByteString)
readPlainInt96ColumnChunk codec chunk = go 0 []
  where
    go !off !acc
      | off >= BS.length chunk = Right (V.concat (reverse acc))
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= encPlain
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              pageVec <- decodePlainInt96 n raw
              go nextOff (pageVec : acc)

-- | All @DATA_PAGE@s as @PLAIN@ @FIXED_LEN_BYTE_ARRAY@ with given type length.
readPlainFixedLenByteArrayColumnChunk :: Int -> Compression -> ByteString -> Either String (V.Vector ByteString)
readPlainFixedLenByteArrayColumnChunk typeLen codec chunk = go 0 []
  where
    go !off !acc
      | off >= BS.length chunk = Right (V.concat (reverse acc))
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= encPlain
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              pageVec <- decodePlainFixedLenByteArray typeLen n raw
              go nextOff (pageVec : acc)

-- | @BYTE_STREAM_SPLIT@ for @FLOAT@: bytes are transposed into 4 runs of N bytes.
{-# INLINE decodeByteStreamSplitFloat #-}
decodeByteStreamSplitFloat :: Int -> ByteString -> Either String (VS.Vector Float)
decodeByteStreamSplitFloat n bs
  | BS.length bs < n * 4 = Left "Parquet.Read: BYTE_STREAM_SPLIT FLOAT buffer too small"
  | otherwise =
      Right $
        runST $ do
          mv <- VSM.new n
          let go2 !i
                | i >= n = VS.unsafeFreeze mv
                | otherwise = do
                    let !b0 = fromIntegral (BS.index bs i) :: Word32
                        !b1 = fromIntegral (BS.index bs (n + i)) :: Word32
                        !b2 = fromIntegral (BS.index bs (2 * n + i)) :: Word32
                        !b3 = fromIntegral (BS.index bs (3 * n + i)) :: Word32
                        !w = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
                    VSM.write mv i (castWord32ToFloat w)
                    go2 (i + 1)
          go2 0

-- | @BYTE_STREAM_SPLIT@ for @DOUBLE@: bytes are transposed into 8 runs of N bytes.
{-# INLINE decodeByteStreamSplitDouble #-}
decodeByteStreamSplitDouble :: Int -> ByteString -> Either String (VS.Vector Double)
decodeByteStreamSplitDouble n bs
  | BS.length bs < n * 8 = Left "Parquet.Read: BYTE_STREAM_SPLIT DOUBLE buffer too small"
  | otherwise =
      Right $
        runST $ do
          mv <- VSM.new n
          let go2 !i
                | i >= n = VS.unsafeFreeze mv
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
                    VSM.write mv i (castWord64ToDouble w)
                    go2 (i + 1)
          go2 0

-- ============================================================
-- Generic per-page chunk dispatch
-- ============================================================

-- | Encoding tags used by the generic dispatch.
encDeltaBinaryPacked, encDeltaLengthByteArray, encDeltaByteArray, encByteStreamSplit :: Int32
encDeltaBinaryPacked     = 5
encDeltaLengthByteArray  = 6
encDeltaByteArray        = 7
encByteStreamSplit       = 9

-- | Dispatcher type: per-page decoder produces a chunk of values
-- of the type the caller asked for.
--
-- /Inlining contract — please don't break it./
--
-- A record of functions is normally a GHC inlining hazard:
-- field projections look like ordinary calls and don't get
-- specialised at use sites unless the compiler can see the
-- literal record. We've verified by Core dump
-- (@-ddump-simpl@) that GHC fully erases the abstraction in
-- our case — the dispatch records turn into worker functions
-- like
--
-- @
-- $wdispatchInt2 = \\ ww ww1 ww2 ww3 ww4 ww5 -> ...
-- @
--
-- with all six 'PerPage' fields unboxed to separate primitive
-- arguments, and the hot inner loops use 'indexInt64Array#' /
-- 'copyByteArray#' / 'writeArray#' primops directly — no
-- record projections, no function-pointer indirection, no
-- allocation in the steady state.
--
-- /Why it works:/
--
--   1. The @dispatch*@ values below are top-level CAFs whose
--      RHS is a literal 'PerPage' constructor application
--      with all strict fields. GHC unboxes the record via
--      worker-wrapper.
--   2. Each @readGeneric*ColumnChunk@ is a partial
--      application of 'genericReadColumnChunk' to one of
--      those CAFs. Both are small enough to be auto-inlined
--      under @-O@ even without explicit pragmas — empirical
--      check: removing every @INLINE@ pragma in this module
--      didn't change criterion numbers, and all
--      @$wdispatch*@ workers still appeared in the Core
--      dump.
--   3. Once both are inlined, case-of-known-constructor
--      reduces every @ppDecodePlain pp@ / @ppConcat pp@ /
--      etc. to the underlying function, and worker-wrapper
--      cleans up the record entirely.
--
-- /What would break this:/
--
--   * Constructing a @dispatch*@ via a helper instead of a
--     literal @PerPage { ... }@ — the CAF would no longer be
--     a known constructor.
--   * Adding many more dispatchers and triggering GHC's
--     unfolding-size threshold so the page walker stops
--     getting auto-inlined. The explicit @INLINE@ pragmas
--     below are belt-and-suspenders against that.
--   * Replacing the strict @!@ fields with lazy ones — GHC
--     would keep the record around to preserve thunks.
data PerPage a = PerPage
  { ppDecodePlain :: !(Int -> ByteString -> Either String a)
    -- ^ Decode a PLAIN-encoded page body into a chunk of length n.
  , ppDecodeDictIndices :: !(Int -> ByteString -> a -> VP.Vector Int32 -> Either String a)
    -- ^ Look up dictionary indices into the materialised values.
    -- @ppDecodeDictIndices n raw acc indices@ : @raw@ is the data
    -- page body (after stripping the bit-width prefix is the
    -- decoder's job), @acc@ is the dictionary chunk, @indices@ is
    -- pre-decoded.
  , ppExtended :: !(Int32 -> Int -> ByteString -> Either String a)
    -- ^ Encoding-specific decoders (delta / byte-stream-split).
    -- @ppExtended encoding numValues body@ — return Left if the
    -- encoding is genuinely unsupported for this physical type.
  , ppConcat :: !([a] -> a)
    -- ^ N-way concatenation of page chunks in order. Must be
    -- O(total bytes) (= 'VP.concat' / 'V.concat' — one
    -- allocation of the final length + one pass of per-page
    -- memcpy). The reverse-cons + single 'ppConcat' shape that
    -- every caller uses below relies on this; do NOT supply
    -- a left-fold of (++) here, that would re-introduce the
    -- O(K^2) bug we just removed.
  , ppEmpty :: !a
  }

-- | Walk every page in a column chunk; for each DATA_PAGE /
-- DATA_PAGE_V2, dispatch on its encoding via the supplied
-- 'PerPage' record. DICTIONARY_PAGE pages are decoded once with
-- 'ppDecodePlain' and held for reuse by RLE_DICTIONARY pages.
{-# INLINE genericReadColumnChunk #-}
genericReadColumnChunk
  :: PerPage a
  -> Compression
  -> ByteString
  -> Either String a
genericReadColumnChunk pp codec chunk0 = go 0 Nothing []
  where
    -- @pageVecs@ collects per-page results in REVERSE order. We
    -- finish with one 'ppConcat' (= 'VP.concat' / 'V.concat')
    -- over @reverse pageVecs@, which is O(total bytes). The
    -- previous shape used 'ppAppend' incrementally, which for a
    -- column with k pages allocated + memcpy'd O(k²) page bytes
    -- — for a 1 M-row Utf8 column with 12 pages that's ~78
    -- pages of redundant memcpy.
    go !off !mDict !pageVecs
      | off >= BS.length chunk0 = case pageVecs of
          []  -> Right (ppEmpty pp)
          [v] -> Right v
          vs  -> Right $! ppConcat pp (reverse vs)
      | otherwise = do
          (hdr, afterHdr) <- readPageHeaderAt chunk0 off
          compSz <- case phCompressedPageSize hdr of
            Nothing -> Left "Parquet.Read: missing compressed_page_size"
            Just s -> Right (fromIntegral s :: Int)
          let !bodyStart = afterHdr
          if bodyStart + compSz > BS.length chunk0
            then Left "Parquet.Read: truncated page body"
            else do
              let !compBody = BS.take compSz (BS.drop bodyStart chunk0)
                  !nextOff = bodyStart + compSz
              case phType hdr of
                PtDictionaryPage dk
                  | not (dictEncoding dk == encPlain || dictEncoding dk == encPlainDictionary) ->
                      Left "Parquet.Read: dictionary page encoding is neither PLAIN (0) nor PLAIN_DICTIONARY (2)"
                  | otherwise -> do
                      raw <- decompressPageData codec
                               (phUncompressedPageSize hdr) compBody
                      let !nDict = fromIntegral (dictNumValues dk) :: Int
                      dict <- ppDecodePlain pp nDict raw
                      go nextOff (Just dict) pageVecs
                PtDataPage dph -> do
                  raw <- decompressPageData codec
                           (phUncompressedPageSize hdr) compBody
                  let !n = fromIntegral (dphNumValues dph) :: Int
                  pageVec <- decodeDataPage pp mDict (dphEncoding dph) n raw
                  go nextOff mDict (pageVec : pageVecs)
                PtDataPageV2 dph2 -> do
                  -- V2 page body is: rep_levels ++ def_levels ++ values.
                  -- For required (max_def=0) flat columns the level
                  -- streams are zero bytes. Skip them and decode the
                  -- values section under the page's encoding.
                  let !repLen = fromIntegral (dph2RepLevelsLen dph2) :: Int
                      !defLen = fromIntegral (dph2DefLevelsLen dph2) :: Int
                      !levelsLen = repLen + defLen
                      !body = compBody
                  if levelsLen > BS.length body
                    then Left "Parquet.Read: V2 levels exceed body size"
                    else do
                      let !valuesSection = BS.drop levelsLen body
                      values <- if dph2IsCompressed dph2
                        then decompressPageData codec
                               (phUncompressedPageSize hdr) valuesSection
                        else Right valuesSection
                      let !n = fromIntegral (dph2NumValues dph2) :: Int
                      pageVec <-
                        decodeDataPage pp mDict (dph2Encoding dph2) n values
                      go nextOff mDict (pageVec : pageVecs)
                _ -> Left "Parquet.Read: expected DATA_PAGE / DATA_PAGE_V2 / DICTIONARY_PAGE"

    decodeDataPage pp' mDict !enc !n !raw
      | enc == encPlain = ppDecodePlain pp' n raw
      | isDictionaryEncoding enc = case mDict of
          Nothing ->
            Left "Parquet.Read: RLE_DICTIONARY page before dictionary page"
          Just dict -> do
            indices <- decodeDictionaryIndices n raw
            ppDecodeDictIndices pp' n raw dict indices
      | otherwise = ppExtended pp' enc n raw

-- ============================================================
-- Per-physical-type dispatchers
-- ============================================================

-- | Dispatchers for primitive types now produce 'VS.Vector'
-- per page. The chunk-level concat is 'VS.concat', a single
-- linear allocation + memcpy of all pages. Output flows
-- straight into Arrow's 'ColInt32' / 'ColInt64' / etc.
-- without any further conversion at the bridge.
dispatchInt32 :: PerPage (VS.Vector Int32)
dispatchInt32 = PerPage
  { ppDecodePlain  = decodePlainInt32
  , ppDecodeDictIndices = \_n _raw dict indices -> dictLookupVS dict indices
  , ppExtended = \enc n raw ->
      if enc == encDeltaBinaryPacked
        then decodeDeltaBinaryPackedInt32 n raw
        else Left $ unsupportedEncoding "INT32" enc
  , ppConcat = VS.concat
  , ppEmpty = VS.empty
  }

dispatchInt64 :: PerPage (VS.Vector Int64)
dispatchInt64 = PerPage
  { ppDecodePlain  = decodePlainInt64
  , ppDecodeDictIndices = \_n _raw dict indices -> dictLookupVS dict indices
  , ppExtended = \enc n raw ->
      if enc == encDeltaBinaryPacked
        then decodeDeltaBinaryPackedInt64 n raw
        else Left $ unsupportedEncoding "INT64" enc
  , ppConcat = VS.concat
  , ppEmpty = VS.empty
  }

dispatchFloat :: PerPage (VS.Vector Float)
dispatchFloat = PerPage
  { ppDecodePlain = decodePlainFloat
  , ppDecodeDictIndices = \_n _raw dict indices -> dictLookupVS dict indices
  , ppExtended = \enc n raw ->
      if enc == encByteStreamSplit
        then decodeByteStreamSplitFloat n raw
        else Left $ unsupportedEncoding "FLOAT" enc
  , ppConcat = VS.concat
  , ppEmpty = VS.empty
  }

dispatchDouble :: PerPage (VS.Vector Double)
dispatchDouble = PerPage
  { ppDecodePlain = decodePlainDouble
  , ppDecodeDictIndices = \_n _raw dict indices -> dictLookupVS dict indices
  , ppExtended = \enc n raw ->
      if enc == encByteStreamSplit
        then decodeByteStreamSplitDouble n raw
        else Left $ unsupportedEncoding "DOUBLE" enc
  , ppConcat = VS.concat
  , ppEmpty = VS.empty
  }

dispatchBool :: PerPage (V.Vector Bool)
dispatchBool = PerPage
  { ppDecodePlain = decodePlainBool
  , ppDecodeDictIndices = \_n _raw _dict _ ->
      -- BOOLEAN columns are never dictionary-encoded in practice
      -- (the dictionary would have at most 2 entries).
      Left "Parquet.Read: BOOLEAN unexpectedly dictionary-encoded"
  , ppExtended = \enc n raw ->
      if enc == encRle
        -- BOOLEAN with RLE encoding: the values section is a
        -- length-prefixed hybrid RLE block (4-byte LE length
        -- prefix, then bit-packed/RLE 1-bit values). Common
        -- shape for both DATA_PAGE_V2 BOOLEAN columns and any
        -- writer that picks RLE explicitly for BOOLEAN.
        then do
          ws <- decodeHybridRleLengthPrefixed 1 n raw
          Right $! V.generate n (\i -> VP.unsafeIndex ws i /= 0)
        else Left $ unsupportedEncoding "BOOLEAN" enc
  , ppConcat = V.concat
  , ppEmpty = V.empty
  }

dispatchByteArray :: PerPage (V.Vector ByteString)
dispatchByteArray = PerPage
  { ppDecodePlain = decodePlainByteArray
  , ppDecodeDictIndices = \_n _raw dict indices ->
      dictLookupVBS dict indices
  , ppExtended = \enc n raw ->
      if enc == encDeltaLengthByteArray
        then decodeDeltaLengthByteArray n raw
        else if enc == encDeltaByteArray
          then decodeDeltaByteArray n raw
          else Left $ unsupportedEncoding "BYTE_ARRAY" enc
  , ppConcat = V.concat
  , ppEmpty = V.empty
  }

-- | Like 'dispatchByteArray', but produces a 'V.Vector' 'Text'
-- directly so the Arrow @ColUtf8@ path can avoid the
-- @V.map decodeUtf8Lossy@ that would otherwise allocate one
-- 'Text' (and one underlying 'ByteArray') per value.
--
-- Dictionary pages are decoded once into a small
-- @V.Vector Text@ (1 alloc per dictionary entry, typically
-- ≤ 1024 entries); subsequent data pages just gather
-- references — zero per-row allocation.
--
-- PLAIN-encoded data pages take the
-- 'decodePlainByteArrayAsText' fast path, which shares one
-- underlying 'TA.Array' across all values.
dispatchUtf8 :: PerPage (V.Vector Text)
dispatchUtf8 = PerPage
  { ppDecodePlain = decodePlainByteArrayAsText
  , ppDecodeDictIndices = \_n _raw dict indices ->
      -- Dictionary lookup: just gather Text references from
      -- the small dict (already decoded as Text by ppDecodePlain).
      let !nD = V.length dict
          !ok = VP.foldl' (\a k -> a && let !j = fromIntegral k :: Int
                                        in j >= 0 && j < nD) True indices
      in if not ok
           then Left "Parquet.Read: dictionary index out of range"
           else Right $! V.generate (VP.length indices)
                  (\i -> V.unsafeIndex dict (fromIntegral (VP.unsafeIndex indices i)))
  , ppExtended = \enc n raw ->
      -- DELTA_LENGTH_BYTE_ARRAY / DELTA_BYTE_ARRAY are rare for
      -- string columns; fall back to the per-value decode.
      if enc == encDeltaLengthByteArray
        then do bs <- decodeDeltaLengthByteArray n raw
                Right $! V.map decodeUtf8LossyTextRead bs
        else if enc == encDeltaByteArray
          then do bs <- decodeDeltaByteArray n raw
                  Right $! V.map decodeUtf8LossyTextRead bs
          else Left $ unsupportedEncoding "BYTE_ARRAY (Utf8)" enc
  , ppConcat = V.concat
  , ppEmpty = V.empty
  }

-- | Storable-vector dictionary lookup: gather decoded values
-- by indexing into a 'VS.Vector' dictionary. Allocates one
-- 'VS.Vector' of the result size and fills it with a single
-- linear pass; no boxed intermediate. Used by the primitive
-- 'PerPage' dispatchers ('dispatchInt32', 'dispatchInt64',
-- 'dispatchFloat', 'dispatchDouble').
dictLookupVS
  :: VS.Storable a
  => VS.Vector a -> VP.Vector Int32 -> Either String (VS.Vector a)
dictLookupVS dict indices =
  let !nD = VS.length dict
      !ok = VP.foldl' (\a k -> a && let !j = fromIntegral k :: Int
                                    in j >= 0 && j < nD) True indices
  in if not ok
       then Left "Parquet.Read: dictionary index out of range"
       else Right $!
              VS.generate (VP.length indices)
                (\i -> VS.unsafeIndex dict
                         (fromIntegral (VP.unsafeIndex indices i)))

dictLookupVBS
  :: V.Vector ByteString -> VP.Vector Int32 -> Either String (V.Vector ByteString)
dictLookupVBS dict indices =
  let !nD = V.length dict
      !ok = VP.foldl' (\a k -> a && let !j = fromIntegral k :: Int
                                    in j >= 0 && j < nD) True indices
  in if not ok
       then Left "Parquet.Read: dictionary index out of range"
       else Right $! V.generate (VP.length indices)
              (\i -> V.unsafeIndex dict (fromIntegral (VP.unsafeIndex indices i)))

unsupportedEncoding :: String -> Int32 -> String
unsupportedEncoding ty enc =
  "Parquet.Read: " ++ ty ++ " column has unsupported encoding "
    ++ show enc
    ++ " (PLAIN=0, PLAIN_DICTIONARY=2, RLE_DICTIONARY=8, "
    ++ "DELTA_BINARY_PACKED=5, DELTA_LENGTH_BYTE_ARRAY=6, "
    ++ "DELTA_BYTE_ARRAY=7, BYTE_STREAM_SPLIT=9)"

-- ============================================================
-- Public generic readers (required)
-- ============================================================

readGenericInt32ColumnChunk :: Compression -> ByteString -> Either String (VS.Vector Int32)
readGenericInt32ColumnChunk = genericReadColumnChunk dispatchInt32
{-# INLINE readGenericInt32ColumnChunk #-}

readGenericInt64ColumnChunk :: Compression -> ByteString -> Either String (VS.Vector Int64)
readGenericInt64ColumnChunk = genericReadColumnChunk dispatchInt64
{-# INLINE readGenericInt64ColumnChunk #-}

readGenericFloatColumnChunk :: Compression -> ByteString -> Either String (VS.Vector Float)
readGenericFloatColumnChunk = genericReadColumnChunk dispatchFloat
{-# INLINE readGenericFloatColumnChunk #-}

readGenericDoubleColumnChunk :: Compression -> ByteString -> Either String (VS.Vector Double)
readGenericDoubleColumnChunk = genericReadColumnChunk dispatchDouble
{-# INLINE readGenericDoubleColumnChunk #-}

readGenericBoolColumnChunk :: Compression -> ByteString -> Either String (V.Vector Bool)
readGenericBoolColumnChunk = genericReadColumnChunk dispatchBool
{-# INLINE readGenericBoolColumnChunk #-}

readGenericByteArrayColumnChunk :: Compression -> ByteString -> Either String (V.Vector ByteString)
readGenericByteArrayColumnChunk = genericReadColumnChunk dispatchByteArray
{-# INLINE readGenericByteArrayColumnChunk #-}

-- | Like 'readGenericByteArrayColumnChunk' but produces a
-- 'V.Vector' 'Text' directly. Use this for @ColUtf8@ Arrow
-- columns to avoid the per-value 'Text' allocation cost.
readGenericTextColumnChunk :: Compression -> ByteString -> Either String (V.Vector Text)
readGenericTextColumnChunk = genericReadColumnChunk dispatchUtf8
{-# INLINE readGenericTextColumnChunk #-}

-- ============================================================
-- Public generic readers (optional / nullable)
-- ============================================================
--
-- For nullable columns the page body carries definition-level
-- streams in addition to the values; the bridge currently keeps
-- the existing 'readPlain*OptionalColumnChunk' paths for the V1
-- + PLAIN case (which the wireform writer always produces) and
-- falls back to the generic dispatcher for any non-PLAIN
-- encoding the optional reader sees.
--
-- The generic optional path defers to the existing
-- materializePlain* combinators after the level streams are
-- parsed, so it inherits the same nullable-column shape.

readGenericInt32OptionalColumnChunk
  :: Compression -> Int -> Int -> ByteString
  -> Either String (V.Vector (Maybe Int32))
readGenericInt32OptionalColumnChunk =
  readGenericOptionalColumnChunk dispatchInt32 vsToBoxed

readGenericInt64OptionalColumnChunk
  :: Compression -> Int -> Int -> ByteString
  -> Either String (V.Vector (Maybe Int64))
readGenericInt64OptionalColumnChunk =
  readGenericOptionalColumnChunk dispatchInt64 vsToBoxed

readGenericFloatOptionalColumnChunk
  :: Compression -> Int -> Int -> ByteString
  -> Either String (V.Vector (Maybe Float))
readGenericFloatOptionalColumnChunk =
  readGenericOptionalColumnChunk dispatchFloat vsToBoxed

readGenericDoubleOptionalColumnChunk
  :: Compression -> Int -> Int -> ByteString
  -> Either String (V.Vector (Maybe Double))
readGenericDoubleOptionalColumnChunk =
  readGenericOptionalColumnChunk dispatchDouble vsToBoxed

readGenericBoolOptionalColumnChunk
  :: Compression -> Int -> Int -> ByteString
  -> Either String (V.Vector (Maybe Bool))
readGenericBoolOptionalColumnChunk =
  readGenericOptionalColumnChunk dispatchBool id

readGenericByteArrayOptionalColumnChunk
  :: Compression -> Int -> Int -> ByteString
  -> Either String (V.Vector (Maybe ByteString))
readGenericByteArrayOptionalColumnChunk =
  readGenericOptionalColumnChunk dispatchByteArray id

-- | Walk every page in a column chunk and interleave a
-- definition-level stream per page so the result is
-- @V.Vector (Maybe a)@. Handles V1 and V2 pages and any of the
-- encodings that the underlying 'PerPage' supports for the
-- /defined/ values (PLAIN, dictionary, delta, byte-stream-split).
readGenericOptionalColumnChunk
  :: forall vec a.
     PerPage vec
  -> (vec -> V.Vector a)
  -> Compression
  -> Int  -- ^ max_repetition_level (typically 0 for flat)
  -> Int  -- ^ max_definition_level (typically 1 for flat optional)
  -> ByteString
  -> Either String (V.Vector (Maybe a))
readGenericOptionalColumnChunk pp toBoxed codec maxRep maxDef chunk0 = do
  pagesRev <- go 0 Nothing []
  Right (V.concat (reverse pagesRev))
  where
    go !off !mDict !acc
      | off >= BS.length chunk0 = Right acc
      | otherwise = do
          (hdr, afterHdr) <- readPageHeaderAt chunk0 off
          compSz <- case phCompressedPageSize hdr of
            Nothing -> Left "Parquet.Read: missing compressed_page_size"
            Just s -> Right (fromIntegral s :: Int)
          let !bodyStart = afterHdr
          if bodyStart + compSz > BS.length chunk0
            then Left "Parquet.Read: truncated page body"
            else do
              let !compBody = BS.take compSz (BS.drop bodyStart chunk0)
                  !nextOff = bodyStart + compSz
              case phType hdr of
                PtDictionaryPage dk
                  | not (dictEncoding dk == encPlain || dictEncoding dk == encPlainDictionary) ->
                      Left "Parquet.Read: dictionary page encoding is neither PLAIN (0) nor PLAIN_DICTIONARY (2)"
                  | otherwise -> do
                      raw <- decompressPageData codec
                               (phUncompressedPageSize hdr) compBody
                      let !nDict = fromIntegral (dictNumValues dk) :: Int
                      dict <- ppDecodePlain pp nDict raw
                      go nextOff (Just dict) acc
                PtDataPage dph -> do
                  raw <- decompressPageData codec
                           (phUncompressedPageSize hdr) compBody
                  let !nVals = fromIntegral (dphNumValues dph) :: Int
                  (_rep, def, valBytes) <-
                    parseDataPageV1Levels maxRep maxDef nVals raw
                  page <- materialiseOptionalPage pp toBoxed mDict
                            (dphEncoding dph) maxDef def valBytes
                  go nextOff mDict (page : acc)
                PtDataPageV2 dph2 -> do
                  let !repLen = fromIntegral (dph2RepLevelsLen dph2) :: Int
                      !defLen = fromIntegral (dph2DefLevelsLen dph2) :: Int
                      !levelsLen = repLen + defLen
                      !body = compBody
                  if levelsLen > BS.length body
                    then Left "Parquet.Read: V2 levels exceed body size"
                    else do
                      let !defBs = BS.take defLen (BS.drop repLen body)
                          !valuesSection = BS.drop levelsLen body
                          !nVals = fromIntegral (dph2NumValues dph2) :: Int
                          !bwDef = levelBitWidth maxDef
                      values <- if dph2IsCompressed dph2
                        then decompressPageData codec
                               (phUncompressedPageSize hdr) valuesSection
                        else Right valuesSection
                      def <- if defLen == 0
                        then Right (VP.replicate nVals 0)
                        else decodeHybridRleUnsigned32 bwDef nVals defBs
                      page <- materialiseOptionalPage pp toBoxed mDict
                                (dph2Encoding dph2) maxDef def values
                      go nextOff mDict (page : acc)
                _ -> Left "Parquet.Read: expected DATA_PAGE / DATA_PAGE_V2 / DICTIONARY_PAGE"

-- | Decode a /defined/ values block in any encoding the
-- 'PerPage' supports, then interleave with the definition-level
-- vector to produce a @V.Vector (Maybe a)@.
materialiseOptionalPage
  :: PerPage vec
  -> (vec -> V.Vector a)
  -> Maybe vec
  -> Int32
  -> Int
  -> VP.Vector Int32
  -> ByteString
  -> Either String (V.Vector (Maybe a))
materialiseOptionalPage pp toBoxed mDict !enc !maxDef !def !valBytes = do
  let !maxD = fromIntegral maxDef :: Int32
      !nDef = VP.foldl' (\a d -> if d == maxD then a + 1 else a) 0 def
  defined <-
    if enc == encPlain
      then ppDecodePlain pp nDef valBytes
      else if isDictionaryEncoding enc
        then case mDict of
          Nothing ->
            Left "Parquet.Read: RLE_DICTIONARY page before dictionary page"
          Just dict -> do
            indices <- decodeDictionaryIndices nDef valBytes
            ppDecodeDictIndices pp nDef valBytes dict indices
        else ppExtended pp enc nDef valBytes
  let !definedBoxed = toBoxed defined
  Right $! interleaveDefined def maxD definedBoxed

-- | Convert a 'VP.Vector' to a 'V.Vector' via 'VP.convert'.
vpToBoxed :: VP.Prim a => VP.Vector a -> V.Vector a
vpToBoxed = VP.convert

-- | Convert a 'VS.Vector' to a 'V.Vector' for the boxed
-- @V.Vector (Maybe a)@ optional path. Used by the
-- 'readGeneric*OptionalColumnChunk' (boxed shape) wrappers
-- now that the per-page output is 'VS.Vector'.
vsToBoxed :: VS.Storable a => VS.Vector a -> V.Vector a
vsToBoxed v = V.generate (VS.length v) (VS.unsafeIndex v)

interleaveDefined :: VP.Vector Int32 -> Int32 -> V.Vector a -> V.Vector (Maybe a)
interleaveDefined def maxD defined = runST $ do
  let !n = VP.length def
  v <- VM.unsafeNew n
  let go !i !j
        | i >= n = pure ()
        | VP.unsafeIndex def i == maxD = do
            VM.unsafeWrite v i (Just (V.unsafeIndex defined j))
            go (i + 1) (j + 1)
        | otherwise = do
            VM.unsafeWrite v i Nothing
            go (i + 1) j
  go 0 0
  V.unsafeFreeze v

-- ============================================================
-- Flat-shape ('NullableView') optional readers
-- ============================================================
--
-- Same per-page state machine as 'readGenericOptionalColumnChunk'
-- but the per-page result is a @(VU.Vector Bit, VS.Vector a)@
-- pair instead of a @V.Vector (Maybe a)@. Per-page interleave
-- writes directly into the storable + bit-packed buffers; no
-- 'Maybe' constructor allocation, no boxed-pointer per slot.
-- Concatenation across pages is one 'VU.concat' + one
-- 'VS.concat' at the end of the chunk.

readGenericInt32OptionalColumnChunkNV
  :: Compression -> Int -> Int -> ByteString
  -> Either String (AC.NullableView Int32)
readGenericInt32OptionalColumnChunkNV =
  readGenericOptionalColumnChunkPrim dispatchInt32 0
{-# INLINE readGenericInt32OptionalColumnChunkNV #-}

readGenericInt64OptionalColumnChunkNV
  :: Compression -> Int -> Int -> ByteString
  -> Either String (AC.NullableView Int64)
readGenericInt64OptionalColumnChunkNV =
  readGenericOptionalColumnChunkPrim dispatchInt64 0
{-# INLINE readGenericInt64OptionalColumnChunkNV #-}

readGenericFloatOptionalColumnChunkNV
  :: Compression -> Int -> Int -> ByteString
  -> Either String (AC.NullableView Float)
readGenericFloatOptionalColumnChunkNV =
  readGenericOptionalColumnChunkPrim dispatchFloat 0
{-# INLINE readGenericFloatOptionalColumnChunkNV #-}

readGenericDoubleOptionalColumnChunkNV
  :: Compression -> Int -> Int -> ByteString
  -> Either String (AC.NullableView Double)
readGenericDoubleOptionalColumnChunkNV =
  readGenericOptionalColumnChunkPrim dispatchDouble 0
{-# INLINE readGenericDoubleOptionalColumnChunkNV #-}

-- | Storable-primitive optional column reader. Walks
-- pages, per page calls 'materialiseOptionalPagePrim' which
-- writes directly into 'VS.MVector' / 'VUM.MVector' buffers,
-- then concatenates the per-page validity + values vectors at
-- the end.
--
-- @sentinel@ is the byte pattern written into 'nvValues' for
-- null slots; readers must check 'nvValidity' before consuming
-- the value (the bridge always does).
readGenericOptionalColumnChunkPrim
  :: forall a.
     VS.Storable a
  => PerPage (VS.Vector a)
  -> a
  -> Compression -> Int -> Int -> ByteString
  -> Either String (AC.NullableView a)
readGenericOptionalColumnChunkPrim pp sentinel codec maxRep maxDef chunk0 = do
  pagesRev <- go 0 Nothing []
  case pagesRev of
    []        -> Right (AC.NullableView VU.empty VS.empty)
    [(b, vs)] -> Right (AC.NullableView b vs)
    ps        ->
      -- Reverse the page list (built in encounter order
      -- via cons + reverse — same shape as the existing
      -- readers) and concat once. VU.concat / VS.concat
      -- both allocate exactly the total length and copy in
      -- a single linear pass.
      let !ordered = reverse ps
          !validity = VU.concat (map fst ordered)
          !values   = VS.concat (map snd ordered)
      in Right (AC.NullableView validity values)
  where
    go !off !mDict !acc
      | off >= BS.length chunk0 = Right acc
      | otherwise = do
          (hdr, afterHdr) <- readPageHeaderAt chunk0 off
          compSz <- case phCompressedPageSize hdr of
            Nothing -> Left "Parquet.Read: missing compressed_page_size"
            Just s  -> Right (fromIntegral s :: Int)
          let !bodyStart = afterHdr
          if bodyStart + compSz > BS.length chunk0
            then Left "Parquet.Read: truncated page body"
            else do
              let !compBody = BS.take compSz (BS.drop bodyStart chunk0)
                  !nextOff = bodyStart + compSz
              case phType hdr of
                PtDictionaryPage dk
                  | not (dictEncoding dk == encPlain || dictEncoding dk == encPlainDictionary) ->
                      Left "Parquet.Read: dictionary page encoding is neither PLAIN (0) nor PLAIN_DICTIONARY (2)"
                  | otherwise -> do
                      raw <- decompressPageData codec
                               (phUncompressedPageSize hdr) compBody
                      let !nDict = fromIntegral (dictNumValues dk) :: Int
                      dict <- ppDecodePlain pp nDict raw
                      go nextOff (Just dict) acc
                PtDataPage dph -> do
                  raw <- decompressPageData codec
                           (phUncompressedPageSize hdr) compBody
                  let !nVals = fromIntegral (dphNumValues dph) :: Int
                  (_rep, def, valBytes) <-
                    parseDataPageV1Levels maxRep maxDef nVals raw
                  page <- materialiseOptionalPagePrim pp sentinel mDict
                            (dphEncoding dph) maxDef def valBytes
                  go nextOff mDict (page : acc)
                PtDataPageV2 dph2 -> do
                  let !repLen = fromIntegral (dph2RepLevelsLen dph2) :: Int
                      !defLen = fromIntegral (dph2DefLevelsLen dph2) :: Int
                      !levelsLen = repLen + defLen
                      !body = compBody
                  if levelsLen > BS.length body
                    then Left "Parquet.Read: V2 levels exceed body size"
                    else do
                      let !defBs = BS.take defLen (BS.drop repLen body)
                          !valuesSection = BS.drop levelsLen body
                          !nVals = fromIntegral (dph2NumValues dph2) :: Int
                          !bwDef = levelBitWidth maxDef
                      values <- if dph2IsCompressed dph2
                        then decompressPageData codec
                               (phUncompressedPageSize hdr) valuesSection
                        else Right valuesSection
                      def <- if defLen == 0
                        then Right (VP.replicate nVals 0)
                        else decodeHybridRleUnsigned32 bwDef nVals defBs
                      page <- materialiseOptionalPagePrim pp sentinel mDict
                                (dph2Encoding dph2) maxDef def values
                      go nextOff mDict (page : acc)
                _ -> Left "Parquet.Read: expected DATA_PAGE / DATA_PAGE_V2 / DICTIONARY_PAGE"

-- | Per-page flat-shape optional materialiser.
--
-- Decodes the defined values once into the 'PerPage' vector
-- type (typically 'VP.Vector a'), then walks the def-level
-- vector once and writes each slot of the output buffers in
-- order:
--
--   * @validity[i] = (def[i] == maxDef)@
--   * @values[i]   = defined[j]@ when valid (and @j@
--     advances), else @sentinel@.
--
-- Two mutable buffers (one VS, one VU) of size @n@ are
-- allocated once per page and frozen immediately. No 'Maybe'
-- boxing.
materialiseOptionalPagePrim
  :: forall a.
     VS.Storable a
  => PerPage (VS.Vector a)
  -> a
  -> Maybe (VS.Vector a)
  -> Int32           -- ^ @encoding@ value from the page header
  -> Int             -- ^ max definition level (Int form for the level walk)
  -> VP.Vector Int32 -- ^ definition levels
  -> ByteString      -- ^ encoded values
  -> Either String (VU.Vector Bit, VS.Vector a)
materialiseOptionalPagePrim pp sentinel mDict !enc !maxDef def valBytes = do
  let !maxD = fromIntegral maxDef :: Int32
      !n    = VP.length def
      !nDef = VP.foldl' (\a d -> if d == maxD then a + 1 else a) 0 def
  defined <-
    if enc == encPlain
      then ppDecodePlain pp nDef valBytes
      else if isDictionaryEncoding enc
        then case mDict of
          Nothing ->
            Left "Parquet.Read: RLE_DICTIONARY page before dictionary page"
          Just dict -> do
            indices <- decodeDictionaryIndices nDef valBytes
            ppDecodeDictIndices pp nDef valBytes dict indices
        else ppExtended pp enc nDef valBytes
  Right $! runST $ do
    valM <- VSM.unsafeNew n
    bitM <- VUM.unsafeNew n
    let go !i !j
          | i >= n = pure ()
          | VP.unsafeIndex def i == maxD = do
              VUM.unsafeWrite bitM i (Bit True)
              VSM.unsafeWrite valM i (VS.unsafeIndex defined j)
              go (i + 1) (j + 1)
          | otherwise = do
              VUM.unsafeWrite bitM i (Bit False)
              VSM.unsafeWrite valM i sentinel
              go (i + 1) j
    go 0 0
    !validity <- VU.unsafeFreeze bitM
    !values   <- VS.unsafeFreeze valM
    pure (validity, values)

-- | Bool variant of 'readGenericOptionalColumnChunkPrim': both
-- validity and values are bit-packed @VU.Vector Bit@. Per page
-- the decoded boolean payload still arrives as a boxed
-- @V.Vector Bool@ (that's what 'dispatchBool' produces, since
-- the existing PerPage shape is monomorphic on a single chunk
-- type), but we do the boxed-to-bit-packed copy in the same
-- pass that interleaves validity, so the boxed vector dies
-- before the page write returns. No persistent
-- @V.Vector (Maybe Bool)@ in the pipeline.
readGenericBoolOptionalColumnChunkNV
  :: Compression -> Int -> Int -> ByteString
  -> Either String AV.NullableBoolView
readGenericBoolOptionalColumnChunkNV codec maxRep maxDef chunk0 = do
  pagesRev <- go 0 Nothing []
  case pagesRev of
    []        -> Right (AV.NullableBoolView VU.empty VU.empty)
    [(b, vs)] -> Right (AV.NullableBoolView b vs)
    ps        ->
      let !ordered = reverse ps
          !validity = VU.concat (map fst ordered)
          !values   = VU.concat (map snd ordered)
      in Right (AV.NullableBoolView validity values)
  where
    pp = dispatchBool

    go !off !mDict !acc
      | off >= BS.length chunk0 = Right acc
      | otherwise = do
          (hdr, afterHdr) <- readPageHeaderAt chunk0 off
          compSz <- case phCompressedPageSize hdr of
            Nothing -> Left "Parquet.Read: missing compressed_page_size"
            Just s  -> Right (fromIntegral s :: Int)
          let !bodyStart = afterHdr
          if bodyStart + compSz > BS.length chunk0
            then Left "Parquet.Read: truncated page body"
            else do
              let !compBody = BS.take compSz (BS.drop bodyStart chunk0)
                  !nextOff = bodyStart + compSz
              case phType hdr of
                PtDictionaryPage _ ->
                  -- BOOLEAN columns aren't dictionary-encoded
                  -- in practice; skip and let the data pages
                  -- proceed without a dictionary.
                  go nextOff mDict acc
                PtDataPage dph -> do
                  raw <- decompressPageData codec
                           (phUncompressedPageSize hdr) compBody
                  let !nVals = fromIntegral (dphNumValues dph) :: Int
                  (_rep, def, valBytes) <-
                    parseDataPageV1Levels maxRep maxDef nVals raw
                  page <- materialiseOptionalPageBool pp mDict
                            (dphEncoding dph) maxDef def valBytes
                  go nextOff mDict (page : acc)
                PtDataPageV2 dph2 -> do
                  let !repLen = fromIntegral (dph2RepLevelsLen dph2) :: Int
                      !defLen = fromIntegral (dph2DefLevelsLen dph2) :: Int
                      !levelsLen = repLen + defLen
                      !body = compBody
                  if levelsLen > BS.length body
                    then Left "Parquet.Read: V2 levels exceed body size"
                    else do
                      let !defBs = BS.take defLen (BS.drop repLen body)
                          !valuesSection = BS.drop levelsLen body
                          !nVals = fromIntegral (dph2NumValues dph2) :: Int
                          !bwDef = levelBitWidth maxDef
                      values <- if dph2IsCompressed dph2
                        then decompressPageData codec
                               (phUncompressedPageSize hdr) valuesSection
                        else Right valuesSection
                      def <- if defLen == 0
                        then Right (VP.replicate nVals 0)
                        else decodeHybridRleUnsigned32 bwDef nVals defBs
                      page <- materialiseOptionalPageBool pp mDict
                                (dph2Encoding dph2) maxDef def values
                      go nextOff mDict (page : acc)
                _ -> Left "Parquet.Read: expected DATA_PAGE / DATA_PAGE_V2 / DICTIONARY_PAGE"

materialiseOptionalPageBool
  :: PerPage (V.Vector Bool)
  -> Maybe (V.Vector Bool)
  -> Int32
  -> Int
  -> VP.Vector Int32
  -> ByteString
  -> Either String (VU.Vector Bit, VU.Vector Bit)
materialiseOptionalPageBool pp mDict !enc !maxDef def valBytes = do
  let !maxD = fromIntegral maxDef :: Int32
      !n    = VP.length def
      !nDef = VP.foldl' (\a d -> if d == maxD then a + 1 else a) 0 def
  defined <-
    if enc == encPlain
      then ppDecodePlain pp nDef valBytes
      else if isDictionaryEncoding enc
        then case mDict of
          Nothing ->
            Left "Parquet.Read: RLE_DICTIONARY page before dictionary page"
          Just dict -> do
            indices <- decodeDictionaryIndices nDef valBytes
            ppDecodeDictIndices pp nDef valBytes dict indices
        else ppExtended pp enc nDef valBytes
  Right $! runST $ do
    valM <- VUM.unsafeNew n
    bitM <- VUM.unsafeNew n
    let go !i !j
          | i >= n = pure ()
          | VP.unsafeIndex def i == maxD = do
              VUM.unsafeWrite bitM i (Bit True)
              VUM.unsafeWrite valM i (Bit (V.unsafeIndex defined j))
              go (i + 1) (j + 1)
          | otherwise = do
              VUM.unsafeWrite bitM i (Bit False)
              VUM.unsafeWrite valM i (Bit False)
              go (i + 1) j
    go 0 0
    !validity <- VU.unsafeFreeze bitM
    !values   <- VU.unsafeFreeze valM
    pure (validity, values)

-- ============================================================
-- Non-nullable BYTE_ARRAY -> BinaryView (flat-shape) reader
-- ============================================================

-- | 'BinaryView' (offsets + dense data) variant of the
-- non-nullable BYTE_ARRAY reader. Per-page output is a
-- 'BinaryView' built directly via 'decodePlainByteArrayBV';
-- chunk-level concat is one allocation of the total
-- offsets + data buffers and a copy-in per page (offsets are
-- rewritten to be absolute against the chunk-level data
-- buffer). Dictionary pages are decoded into a
-- 'BinaryView' once and held; data pages with
-- @PLAIN_DICTIONARY@ / @RLE_DICTIONARY@ encoding gather
-- bytes from the dictionary into a fresh per-page
-- 'BinaryView' via 'gatherBinaryView'.
--
-- Replaces the bridge's previous
--   AC.ColBinary . AV.binaryViewFromVector
--     <$> PR.readGenericByteArrayColumnChunk codec chunk
-- which paid: N short-lived 'ByteString' slice triples in a
-- boxed 'V.Vector', then a separate
-- 'binaryViewFromVector' walk to produce the offsets +
-- contiguous data buffer (plus the O(N^2) bug that fix is in
-- the previous commit).
readGenericByteArrayColumnChunkBV
  :: Compression -> ByteString -> Either String AV.BinaryView
readGenericByteArrayColumnChunkBV =
  genericReadColumnChunk dispatchByteArrayBV
{-# INLINE readGenericByteArrayColumnChunkBV #-}

dispatchByteArrayBV :: PerPage AV.BinaryView
dispatchByteArrayBV = PerPage
  { ppDecodePlain = decodePlainByteArrayBV
  , ppDecodeDictIndices = \_n _raw dict indices ->
      Right (gatherBinaryView dict indices)
  , ppExtended = \enc n raw ->
      if enc == encDeltaLengthByteArray
        then do
          v <- decodeDeltaLengthByteArray n raw
          Right (AV.binaryViewFromVector v)
        else if enc == encDeltaByteArray
          then do
            v <- decodeDeltaByteArray n raw
            Right (AV.binaryViewFromVector v)
          else Left $ unsupportedEncoding "BYTE_ARRAY" enc
  , ppConcat = concatBinaryViews
  , ppEmpty = AV.BinaryView (VS.singleton 0) VS.empty
  }

-- | Single-page PLAIN BYTE_ARRAY -> 'BinaryView'.
--
-- Two passes: first scan the page to count + sum the
-- length-prefixed payloads (gives the exact total data byte
-- count, so the output buffer is one allocation); second
-- pass writes offsets and memcpy's bytes in lockstep.
decodePlainByteArrayBV
  :: Int -> ByteString -> Either String AV.BinaryView
decodePlainByteArrayBV n bs0
  | n <= 0 = Right (AV.BinaryView (VS.singleton 0) VS.empty)
  | otherwise = do
      totalBytes <- preScan 0 0 0
      Right $! runST $ do
        offM <- VSM.unsafeNew (n + 1)
        datM <- VSM.unsafeNew totalBytes
        let go !i !off !cur
              | i >= n = do
                  VSM.unsafeWrite offM n (fromIntegral cur :: Int32)
                  pure ()
              | otherwise = do
                  let !len = fromIntegral (readLE32 bs0 off) :: Int
                      !off2 = off + 4
                  VSM.unsafeWrite offM i (fromIntegral cur :: Int32)
                  copyBSIntoMV datM cur (BS.take len (BS.drop off2 bs0))
                  go (i + 1) (off2 + len) (cur + len)
        go 0 0 0
        !offs <- VS.unsafeFreeze offM
        !dat  <- VS.unsafeFreeze datM
        pure (AV.BinaryView offs dat)
  where
    !totalLen = BS.length bs0
    preScan !i !off !acc
      | i >= n = Right acc
      | off + 4 > totalLen =
          Left "Parquet.Read: PLAIN BYTE_ARRAY truncated length"
      | otherwise =
          let !len = fromIntegral (readLE32 bs0 off) :: Int
              !off2 = off + 4
          in if len < 0 || off2 + len > totalLen
               then Left "Parquet.Read: PLAIN BYTE_ARRAY payload out of bounds"
               else preScan (i + 1) (off2 + len) (acc + len)

-- | Gather a fresh 'BinaryView' by indexing into a dictionary
-- 'BinaryView'. Two passes: sum the gathered byte count from
-- the dictionary's offset deltas, then write offsets + memcpy
-- per row from the dictionary's data buffer into the output
-- data buffer.
gatherBinaryView
  :: AV.BinaryView -> VP.Vector Int32 -> AV.BinaryView
gatherBinaryView dict indices = runST $ do
  let !n = VP.length indices
      !dOffs = AV.bvOffsets dict
      !dDat  = AV.bvData dict
      !nDict = max 0 (VS.length dOffs - 1)
      bytesAt !i =
        let !s = fromIntegral (VS.unsafeIndex dOffs i)       :: Int
            !e = fromIntegral (VS.unsafeIndex dOffs (i + 1)) :: Int
        in e - s
      totalBytes = VP.foldl'
        (\acc k ->
            let !i = fromIntegral k :: Int
            in if i < 0 || i >= nDict then acc else acc + bytesAt i)
        0 indices
  offM <- VSM.unsafeNew (n + 1)
  datM <- VSM.unsafeNew totalBytes
  let go !i !cur
        | i >= n = do
            VSM.unsafeWrite offM n (fromIntegral cur :: Int32)
            pure ()
        | otherwise = do
            let !k    = fromIntegral (VP.unsafeIndex indices i) :: Int
                !s    = fromIntegral (VS.unsafeIndex dOffs k)       :: Int
                !e    = fromIntegral (VS.unsafeIndex dOffs (k + 1)) :: Int
                !len  = e - s
            VSM.unsafeWrite offM i (fromIntegral cur :: Int32)
            -- Copy the dict slice [s, s+len) into datM at cur.
            VS.unsafeCopy
              (VSM.unsafeSlice cur len datM)
              (VS.unsafeSlice s len dDat)
            go (i + 1) (cur + len)
  go 0 0
  !offs <- VS.unsafeFreeze offM
  !dat  <- VS.unsafeFreeze datM
  pure (AV.BinaryView offs dat)

-- | Stitch a list of 'BinaryView's into a single 'BinaryView'.
-- One allocation each for the merged offsets + data buffers;
-- per-page offsets are rewritten to absolute against the
-- merged data buffer.
concatBinaryViews :: [AV.BinaryView] -> AV.BinaryView
concatBinaryViews [] = AV.BinaryView (VS.singleton 0) VS.empty
concatBinaryViews [bv] = bv
concatBinaryViews bvs = runST $ do
  let totalSlots = sum (map (\bv -> max 0 (VS.length (AV.bvOffsets bv) - 1)) bvs)
      totalBytes = sum (map (\bv -> VS.length (AV.bvData bv)) bvs)
  offM <- VSM.unsafeNew (totalSlots + 1)
  datM <- VSM.unsafeNew totalBytes
  let writePages !slotBase !byteBase [] = do
        VSM.unsafeWrite offM slotBase (fromIntegral byteBase :: Int32)
        pure ()
      writePages !slotBase !byteBase (p : rest) = do
        let !pOffs  = AV.bvOffsets p
            !pData  = AV.bvData p
            !nSlots = max 0 (VS.length pOffs - 1)
            !nBytes = VS.length pData
        let copyOffs !i
              | i >= nSlots = pure ()
              | otherwise = do
                  let !o = fromIntegral (VS.unsafeIndex pOffs i) :: Int
                  VSM.unsafeWrite offM (slotBase + i)
                                  (fromIntegral (byteBase + o) :: Int32)
                  copyOffs (i + 1)
        copyOffs 0
        VS.unsafeCopy
          (VSM.unsafeSlice byteBase nBytes datM) pData
        writePages (slotBase + nSlots) (byteBase + nBytes) rest
  writePages 0 0 bvs
  !offs <- VS.unsafeFreeze offM
  !dat  <- VS.unsafeFreeze datM
  pure (AV.BinaryView offs dat)

-- | ByteArray (UTF-8 / binary) variant. Per page emits a
-- fully-formed 'NullableBinaryView': validity bits + Int32
-- offsets + a single contiguous data buffer. The decoded
-- @V.Vector ByteString@ for the page is consumed in-pass:
-- we walk @def@ once, computing offsets cumulatively and
-- copying the per-row bytes into a single pre-allocated
-- 'ByteString' buffer of the known total size. No
-- @V.Vector (Maybe ByteString)@ ever exists.
--
-- Across pages we then build the chunk-level
-- 'NullableBinaryView' by:
--
--   * VU.concat'ing the per-page validity bitmaps.
--   * Concatenating the per-page data buffers and rewriting
--     each page's offsets to be relative to the chunk-level
--     buffer.
--
-- The rewrite is just an addition of a per-page base offset
-- to each entry, done once into a fresh VS buffer.
readGenericByteArrayOptionalColumnChunkNV
  :: Compression -> Int -> Int -> ByteString
  -> Either String AV.NullableBinaryView
readGenericByteArrayOptionalColumnChunkNV codec maxRep maxDef chunk0 = do
  pagesRev <- go 0 Nothing []
  case pagesRev of
    []   -> Right (AV.NullableBinaryView VU.empty
                    (AV.BinaryView (VS.singleton 0) VS.empty))
    [p]  -> Right p
    pgs  ->
      -- Stitch pages: total slot count = sum of per-page
      -- slot counts; total data bytes = sum of per-page
      -- data lengths. Allocate the offset + data buffers
      -- once and copy each page in.
      Right $! concatNullableBinaryPages (reverse pgs)
  where
    pp = dispatchByteArray

    go !off !mDict !acc
      | off >= BS.length chunk0 = Right acc
      | otherwise = do
          (hdr, afterHdr) <- readPageHeaderAt chunk0 off
          compSz <- case phCompressedPageSize hdr of
            Nothing -> Left "Parquet.Read: missing compressed_page_size"
            Just s  -> Right (fromIntegral s :: Int)
          let !bodyStart = afterHdr
          if bodyStart + compSz > BS.length chunk0
            then Left "Parquet.Read: truncated page body"
            else do
              let !compBody = BS.take compSz (BS.drop bodyStart chunk0)
                  !nextOff = bodyStart + compSz
              case phType hdr of
                PtDictionaryPage dk
                  | not (dictEncoding dk == encPlain || dictEncoding dk == encPlainDictionary) ->
                      Left "Parquet.Read: dictionary page encoding is neither PLAIN (0) nor PLAIN_DICTIONARY (2)"
                  | otherwise -> do
                      raw <- decompressPageData codec
                               (phUncompressedPageSize hdr) compBody
                      let !nDict = fromIntegral (dictNumValues dk) :: Int
                      dict <- ppDecodePlain pp nDict raw
                      go nextOff (Just dict) acc
                PtDataPage dph -> do
                  raw <- decompressPageData codec
                           (phUncompressedPageSize hdr) compBody
                  let !nVals = fromIntegral (dphNumValues dph) :: Int
                  (_rep, def, valBytes) <-
                    parseDataPageV1Levels maxRep maxDef nVals raw
                  page <- materialiseOptionalPageBA pp mDict
                            (dphEncoding dph) maxDef def valBytes
                  go nextOff mDict (page : acc)
                PtDataPageV2 dph2 -> do
                  let !repLen = fromIntegral (dph2RepLevelsLen dph2) :: Int
                      !defLen = fromIntegral (dph2DefLevelsLen dph2) :: Int
                      !levelsLen = repLen + defLen
                      !body = compBody
                  if levelsLen > BS.length body
                    then Left "Parquet.Read: V2 levels exceed body size"
                    else do
                      let !defBs = BS.take defLen (BS.drop repLen body)
                          !valuesSection = BS.drop levelsLen body
                          !nVals = fromIntegral (dph2NumValues dph2) :: Int
                          !bwDef = levelBitWidth maxDef
                      values <- if dph2IsCompressed dph2
                        then decompressPageData codec
                               (phUncompressedPageSize hdr) valuesSection
                        else Right valuesSection
                      def <- if defLen == 0
                        then Right (VP.replicate nVals 0)
                        else decodeHybridRleUnsigned32 bwDef nVals defBs
                      page <- materialiseOptionalPageBA pp mDict
                                (dph2Encoding dph2) maxDef def values
                      go nextOff mDict (page : acc)
                _ -> Left "Parquet.Read: expected DATA_PAGE / DATA_PAGE_V2 / DICTIONARY_PAGE"

-- | Per-page builder for the ByteArray flat-shape variant.
-- Walks @def@ twice: once to compute the page's total data-
-- buffer size + the validity bitmap (cheap, primitive), then
-- once to write offsets and copy bytes in lockstep.
materialiseOptionalPageBA
  :: PerPage (V.Vector ByteString)
  -> Maybe (V.Vector ByteString)
  -> Int32
  -> Int
  -> VP.Vector Int32
  -> ByteString
  -> Either String AV.NullableBinaryView
materialiseOptionalPageBA pp mDict !enc !maxDef def valBytes = do
  let !maxD = fromIntegral maxDef :: Int32
      !n    = VP.length def
      !nDef = VP.foldl' (\a d -> if d == maxD then a + 1 else a) 0 def
  defined <-
    if enc == encPlain
      then ppDecodePlain pp nDef valBytes
      else if isDictionaryEncoding enc
        then case mDict of
          Nothing ->
            Left "Parquet.Read: RLE_DICTIONARY page before dictionary page"
          Just dict -> do
            indices <- decodeDictionaryIndices nDef valBytes
            ppDecodeDictIndices pp nDef valBytes dict indices
        else ppExtended pp enc nDef valBytes
  -- Pre-pass: total bytes across the defined values; gives
  -- us the data-buffer size up front so the value buffer is
  -- a single VS.unsafeNew (no resizing).
  let !totalBytes = V.foldl' (\acc bs -> acc + BS.length bs) 0 defined
  Right $! runST $ do
    bitM  <- VUM.unsafeNew n
    offM  <- VSM.unsafeNew (n + 1)
    datM  <- VSM.unsafeNew totalBytes
    let go !i !j !curOff
          | i >= n = do
              VSM.unsafeWrite offM n (fromIntegral curOff :: Int32)
              pure ()
          | VP.unsafeIndex def i == maxD = do
              VUM.unsafeWrite bitM i (Bit True)
              VSM.unsafeWrite offM i (fromIntegral curOff :: Int32)
              let !bs = V.unsafeIndex defined j
                  !len = BS.length bs
              -- Copy the row's bytes into the dense data
              -- buffer at @curOff@. Same shape used by the
              -- non-optional ByteArray reader.
              copyBSIntoMV datM curOff bs
              go (i + 1) (j + 1) (curOff + len)
          | otherwise = do
              VUM.unsafeWrite bitM i (Bit False)
              VSM.unsafeWrite offM i (fromIntegral curOff :: Int32)
              go (i + 1) j curOff
    go 0 0 0
    !validity <- VU.unsafeFreeze bitM
    !offs     <- VS.unsafeFreeze offM
    !dat      <- VS.unsafeFreeze datM
    pure (AV.NullableBinaryView validity (AV.BinaryView offs dat))

-- | Copy a 'ByteString' byte-for-byte into a mutable
-- 'VS.MVector' starting at the given destination offset.
-- Used by 'materialiseOptionalPageBA' (and would be a fine
-- factoring target for the non-optional path too).
-- The mutable vector here lives in 'ST s'; we coerce it to
-- 'IOVector' just for the duration of the FFI memcpy via
-- 'unsafeIOToST'. This is the same pattern 'Data.Vector.Storable'
-- uses internally for 'unsafeWith' on mutable vectors.
copyBSIntoMV :: VSM.MVector s Word8 -> Int -> ByteString -> ST s ()
copyBSIntoMV mv dstOff bs = unsafeIOToST $
  VSM.unsafeWith (unsafeCoerceMV mv) $ \dstPtr ->
    BSU.unsafeUseAsCStringLen bs $ \(srcPtr, srcLen) ->
      copyBytes (castPtr (dstPtr `plusPtr` dstOff))
                (castPtr srcPtr) srcLen
  where
    unsafeCoerceMV :: VSM.MVector s Word8 -> VSM.IOVector Word8
    unsafeCoerceMV = unsafeCoerce

-- | Stitch a non-empty page list into a single
-- 'NullableBinaryView'. Page offsets are rewritten to be
-- absolute against the chunk-level data buffer (each page
-- contributes a base = sum of prior pages' data lengths).
concatNullableBinaryPages
  :: [AV.NullableBinaryView] -> AV.NullableBinaryView
concatNullableBinaryPages pages = runST $ do
  -- Total slot count + total data byte count.
  let totalSlots = sum (map (\p -> VU.length (AV.nbvBinValidity p)) pages)
      totalBytes = sum (map (\p -> VS.length (AV.bvData (AV.nbvBinValues p))) pages)
  bitM <- VUM.unsafeNew totalSlots
  offM <- VSM.unsafeNew (totalSlots + 1)
  datM <- VSM.unsafeNew totalBytes
  let writePages !slotBase !byteBase [] = do
        VSM.unsafeWrite offM slotBase (fromIntegral byteBase :: Int32)
        pure ()
      writePages !slotBase !byteBase (p : rest) = do
        let !validity = AV.nbvBinValidity p
            !bview    = AV.nbvBinValues p
            !pOffs    = AV.bvOffsets bview
            !pData    = AV.bvData bview
            !nSlots   = VU.length validity
            !nBytes   = VS.length pData
        -- 1. Copy validity bits one by one (bit-packed
        --    cross-byte slicing makes a bulk memcpy
        --    awkward; this is one pass per page over n
        --    bits, no allocation).
        let copyBits !i
              | i >= nSlots = pure ()
              | otherwise = do
                  VUM.unsafeWrite bitM (slotBase + i)
                                  (VU.unsafeIndex validity i)
                  copyBits (i + 1)
        copyBits 0
        -- 2. Rewrite offsets: out[i] = byteBase + in[i].
        let copyOffs !i
              | i >= nSlots = pure ()
              | otherwise = do
                  let !o = fromIntegral (VS.unsafeIndex pOffs i) :: Int
                  VSM.unsafeWrite offM (slotBase + i)
                                  (fromIntegral (byteBase + o) :: Int32)
                  copyOffs (i + 1)
        copyOffs 0
        -- 3. Copy the page's data bytes. VS slice-copy is
        --    a single memcpy.
        VS.unsafeCopy
          (VSM.unsafeSlice byteBase nBytes datM) pData
        writePages (slotBase + nSlots) (byteBase + nBytes) rest
  writePages 0 0 pages
  !validity <- VU.unsafeFreeze bitM
  !offs     <- VS.unsafeFreeze offM
  !dat      <- VS.unsafeFreeze datM
  pure (AV.NullableBinaryView validity (AV.BinaryView offs dat))

-- ============================================================
-- Page-index-driven page skipping
-- ============================================================
--
-- The 'PageLocation' offsets in an 'Parquet.Types.OffsetIndex'
-- are absolute file offsets (per the spec), so the page-level
-- skipping API takes the full file 'ByteString' rather than a
-- column-chunk slice.
--
-- The 'V.Vector Bool' alongside @pageLocations@ is the
-- per-page \"keep this page\" mask: 'True' = decode it, 'False'
-- = skip. Producers typically build this by running
-- 'Parquet.Predicate.evalPagesByColumnIndex' against the chunk's
-- 'ColumnIndex' and mapping 'PMaybeKeep' -> True / 'PSkip' ->
-- False.
--
-- All variants assume each page contributes a contiguous chunk
-- of values to the surviving result vector — i.e. they're
-- correct for required (max-def-level=0) columns. For nullable
-- columns the per-page def-level streams need to be parsed to
-- know how many values a page actually contributes; that's a
-- separate optional-page-skipping API.

readGenericInt32SelectedPages
  :: Compression -> ByteString -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (VS.Vector Int32)
readGenericInt32SelectedPages = readSelectedPages dispatchInt32

readGenericInt64SelectedPages
  :: Compression -> ByteString -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (VS.Vector Int64)
readGenericInt64SelectedPages = readSelectedPages dispatchInt64

readGenericFloatSelectedPages
  :: Compression -> ByteString -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (VS.Vector Float)
readGenericFloatSelectedPages = readSelectedPages dispatchFloat

readGenericDoubleSelectedPages
  :: Compression -> ByteString -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (VS.Vector Double)
readGenericDoubleSelectedPages = readSelectedPages dispatchDouble

readGenericBoolSelectedPages
  :: Compression -> ByteString -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (V.Vector Bool)
readGenericBoolSelectedPages = readSelectedPages dispatchBool

readGenericByteArraySelectedPages
  :: Compression -> ByteString -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (V.Vector ByteString)
readGenericByteArraySelectedPages = readSelectedPages dispatchByteArray

-- | Walk the 'PageLocation' vector and decode only the pages
-- whose corresponding 'V.Vector Bool' entry is 'True'. The
-- page bodies are sliced directly out of the file 'ByteString'
-- using @plOffset@.
--
-- Dictionary pages are /always/ decoded: skipping the
-- dictionary would invalidate any RLE_DICTIONARY page that
-- survives the keep mask.
readSelectedPages
  :: PerPage a
  -> Compression
  -> ByteString
  -> V.Vector PageLocation
  -> V.Vector Bool
  -> Either String a
readSelectedPages pp codec fileBs locs keep
  | V.length locs /= V.length keep =
      Left $ "Parquet.Read: keep-mask length "
              ++ show (V.length keep)
              ++ " doesn't match page-location count "
              ++ show (V.length locs)
  | otherwise = do
      pagesRev <- walk 0 Nothing []
      Right $! ppConcat pp (reverse pagesRev)
  where
    !nLocs = V.length locs

    walk !i !mDict !acc
      | i >= nLocs = Right acc
      | otherwise = do
          let !pl   = V.unsafeIndex locs i
              !off  = fromIntegral (plOffset pl) :: Int
          -- Each PageLocation points at the page header; read
          -- header to learn whether it's data or dictionary.
          if off < 0 || off >= BS.length fileBs
            then Left "Parquet.Read: page offset outside file bounds"
            else do
              (hdr, afterHdr) <- readPageHeaderAt fileBs off
              compSz <- case phCompressedPageSize hdr of
                Nothing -> Left "Parquet.Read: missing compressed_page_size"
                Just s -> Right (fromIntegral s :: Int)
              let !bodyStart = afterHdr
              if bodyStart + compSz > BS.length fileBs
                then Left "Parquet.Read: truncated page body in file slice"
                else do
                  let !compBody = BS.take compSz (BS.drop bodyStart fileBs)
                  case phType hdr of
                    PtDictionaryPage dk
                      | not (dictEncoding dk == encPlain || dictEncoding dk == encPlainDictionary) ->
                          Left "Parquet.Read: dictionary page encoding is neither PLAIN (0) nor PLAIN_DICTIONARY (2)"
                      | otherwise -> do
                          raw <- decompressPageData codec
                                   (phUncompressedPageSize hdr) compBody
                          let !nDict = fromIntegral (dictNumValues dk) :: Int
                          dict <- ppDecodePlain pp nDict raw
                          walk (i + 1) (Just dict) acc
                    PtDataPage dph -> do
                      if not (V.unsafeIndex keep i)
                        then walk (i + 1) mDict acc
                        else do
                          raw <- decompressPageData codec
                                   (phUncompressedPageSize hdr) compBody
                          let !n = fromIntegral (dphNumValues dph) :: Int
                          page <- decodeSelectedDataPage pp mDict (dphEncoding dph) n raw
                          walk (i + 1) mDict (page : acc)
                    PtDataPageV2 dph2 -> do
                      if not (V.unsafeIndex keep i)
                        then walk (i + 1) mDict acc
                        else do
                          let !repLen = fromIntegral (dph2RepLevelsLen dph2) :: Int
                              !defLen = fromIntegral (dph2DefLevelsLen dph2) :: Int
                              !levelsLen = repLen + defLen
                              !body = compBody
                          if levelsLen > BS.length body
                            then Left "Parquet.Read: V2 levels exceed body size (selected)"
                            else do
                              let !valuesSection = BS.drop levelsLen body
                              values <- if dph2IsCompressed dph2
                                then decompressPageData codec
                                       (phUncompressedPageSize hdr) valuesSection
                                else Right valuesSection
                              let !n = fromIntegral (dph2NumValues dph2) :: Int
                              page <- decodeSelectedDataPage pp mDict (dph2Encoding dph2) n values
                              walk (i + 1) mDict (page : acc)
                    _ -> Left "Parquet.Read: expected DATA_PAGE / DATA_PAGE_V2 / DICTIONARY_PAGE in selection"

    decodeSelectedDataPage pp' mDict !enc !n !raw
      | enc == encPlain = ppDecodePlain pp' n raw
      | isDictionaryEncoding enc = case mDict of
          Nothing ->
            Left "Parquet.Read: RLE_DICTIONARY data page before dictionary page (selected)"
          Just dict -> do
            indices <- decodeDictionaryIndices n raw
            ppDecodeDictIndices pp' n raw dict indices
      | otherwise = ppExtended pp' enc n raw

-- ============================================================
-- Optional page-index-driven page skipping
-- ============================================================
--
-- The required-page-skip path above assumes every surviving
-- page contributes a contiguous chunk of values. Nullable
-- columns carry per-page def-level streams; a /skipped/ page
-- still elides its rows from the output, so we have to:
--
--   1. Parse each page's def-level stream to learn how many
--      rows the page held.
--   2. For kept pages, parse + interleave just that page's
--      defs with the decoded values like the non-skipping
--      'readGenericXxxOptionalColumnChunk' family does.
--   3. For skipped pages, emit the right number of @Nothing@
--      rows so downstream row-index alignment stays correct.

readGenericInt32OptionalSelectedPages
  :: Compression -> Int -> Int -> ByteString
  -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (V.Vector (Maybe Int32))
readGenericInt32OptionalSelectedPages =
  readGenericOptionalSelectedPages dispatchInt32 vsToBoxed

readGenericInt64OptionalSelectedPages
  :: Compression -> Int -> Int -> ByteString
  -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (V.Vector (Maybe Int64))
readGenericInt64OptionalSelectedPages =
  readGenericOptionalSelectedPages dispatchInt64 vsToBoxed

readGenericFloatOptionalSelectedPages
  :: Compression -> Int -> Int -> ByteString
  -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (V.Vector (Maybe Float))
readGenericFloatOptionalSelectedPages =
  readGenericOptionalSelectedPages dispatchFloat vsToBoxed

readGenericDoubleOptionalSelectedPages
  :: Compression -> Int -> Int -> ByteString
  -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (V.Vector (Maybe Double))
readGenericDoubleOptionalSelectedPages =
  readGenericOptionalSelectedPages dispatchDouble vsToBoxed

readGenericBoolOptionalSelectedPages
  :: Compression -> Int -> Int -> ByteString
  -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (V.Vector (Maybe Bool))
readGenericBoolOptionalSelectedPages =
  readGenericOptionalSelectedPages dispatchBool id

readGenericByteArrayOptionalSelectedPages
  :: Compression -> Int -> Int -> ByteString
  -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (V.Vector (Maybe ByteString))
readGenericByteArrayOptionalSelectedPages =
  readGenericOptionalSelectedPages dispatchByteArray id

-- | Walk pages by 'PageLocation', honour the keep mask, and
-- materialise an interleaved @V.Vector (Maybe a)@. Skipped
-- pages don't contribute rows to the output; if the caller
-- wants positional alignment with the unfiltered column they
-- need to also collect the per-page row counts (the
-- 'PageLocation.plFirstRowIndex' deltas give that).
readGenericOptionalSelectedPages
  :: PerPage vec
  -> (vec -> V.Vector a)
  -> Compression
  -> Int  -- ^ max_repetition_level
  -> Int  -- ^ max_definition_level
  -> ByteString
  -> V.Vector PageLocation
  -> V.Vector Bool
  -> Either String (V.Vector (Maybe a))
readGenericOptionalSelectedPages pp toBoxed codec maxRep maxDef
                                  fileBs locs keep
  | V.length locs /= V.length keep =
      Left $ "Parquet.Read: keep-mask length "
              ++ show (V.length keep)
              ++ " doesn't match page-location count "
              ++ show (V.length locs)
  | otherwise = do
      pagesRev <- walk 0 Nothing []
      Right (V.concat (reverse pagesRev))
  where
    !nLocs = V.length locs

    walk !i !mDict !acc
      | i >= nLocs = Right acc
      | otherwise = do
          let !pl  = V.unsafeIndex locs i
              !off = fromIntegral (plOffset pl) :: Int
          if off < 0 || off >= BS.length fileBs
            then Left "Parquet.Read: page offset outside file bounds"
            else do
              (hdr, afterHdr) <- readPageHeaderAt fileBs off
              compSz <- case phCompressedPageSize hdr of
                Nothing -> Left "Parquet.Read: missing compressed_page_size"
                Just s -> Right (fromIntegral s :: Int)
              let !bodyStart = afterHdr
              if bodyStart + compSz > BS.length fileBs
                then Left "Parquet.Read: truncated page body in file slice"
                else do
                  let !compBody = BS.take compSz (BS.drop bodyStart fileBs)
                  case phType hdr of
                    PtDictionaryPage dk
                      | not (dictEncoding dk == encPlain || dictEncoding dk == encPlainDictionary) ->
                          Left "Parquet.Read: dictionary page encoding is neither PLAIN (0) nor PLAIN_DICTIONARY (2)"
                      | otherwise -> do
                          raw <- decompressPageData codec
                                   (phUncompressedPageSize hdr) compBody
                          let !nDict = fromIntegral (dictNumValues dk) :: Int
                          dict <- ppDecodePlain pp nDict raw
                          walk (i + 1) (Just dict) acc
                    PtDataPage dph ->
                      if not (V.unsafeIndex keep i)
                        then walk (i + 1) mDict acc
                        else do
                          raw <- decompressPageData codec
                                   (phUncompressedPageSize hdr) compBody
                          let !nVals = fromIntegral (dphNumValues dph) :: Int
                          (_rep, def, valBytes) <-
                            parseDataPageV1Levels maxRep maxDef nVals raw
                          page <- materialiseOptionalPage pp toBoxed mDict
                                    (dphEncoding dph) maxDef def valBytes
                          walk (i + 1) mDict (page : acc)
                    PtDataPageV2 dph2 ->
                      if not (V.unsafeIndex keep i)
                        then walk (i + 1) mDict acc
                        else do
                          let !repLen = fromIntegral (dph2RepLevelsLen dph2) :: Int
                              !defLen = fromIntegral (dph2DefLevelsLen dph2) :: Int
                              !levelsLen = repLen + defLen
                              !body = compBody
                          if levelsLen > BS.length body
                            then Left "Parquet.Read: V2 levels exceed body size (selected/optional)"
                            else do
                              let !defBs = BS.take defLen (BS.drop repLen body)
                                  !valuesSection = BS.drop levelsLen body
                                  !nVals = fromIntegral (dph2NumValues dph2) :: Int
                                  !bwDef = levelBitWidth maxDef
                              values <- if dph2IsCompressed dph2
                                then decompressPageData codec
                                       (phUncompressedPageSize hdr) valuesSection
                                else Right valuesSection
                              def <- if defLen == 0
                                then Right (VP.replicate nVals 0)
                                else decodeHybridRleUnsigned32 bwDef nVals defBs
                              page <- materialiseOptionalPage pp toBoxed mDict
                                        (dph2Encoding dph2) maxDef def values
                              walk (i + 1) mDict (page : acc)
                    _ -> Left "Parquet.Read: expected DATA_PAGE / DATA_PAGE_V2 / DICTIONARY_PAGE in selection"
