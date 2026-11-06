{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
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
  , loadParquetFileWithDecryption
  , loadParquetFilePath
  , openParquetReader
  , FooterDecryption (..)
  , columnChunkSlice
  , rawColumnChunkSlice
  , decryptFileColumnModule
  , isColumnEncrypted
  , encryptedColumnKeyMetadata
  , PreparedSelection (..)
  , prepareColumnSelection
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
  , BinaryBuffers
  , LargeBinaryBuffers
  , decodePlainLargeBinaryBuffers
  , readLargeBinaryBuffersColumnChunk
  , readLargeBinaryBuffersLevels
  , readLargeBinaryBuffersSelectedValues
  , readLargeBinaryBuffersSelectedLevels
  , binaryOffsets
  , binaryData
  , decodePlainBinaryBuffers
  , readBinaryBuffersColumnChunk
  , readBinaryBuffersLevels
  , readBinaryBuffersSelectedValues
  , readBinaryBuffersSelectedLevels
  , BoolBuffers
  , boolCount
  , boolData
  , readBoolBuffersColumnChunk
  , readBoolBuffersLevels
  , readBoolBuffersSelectedValues
  , readBoolBuffersSelectedLevels
  , ScalarBuffers
  , scalarData
  , readScalarBuffersColumnChunk
  , readScalarBuffersLevels
  , readScalarBuffersSelectedValues
  , readScalarBuffersSelectedLevels
  , decodePlainInt96
  , decodePlainFixedLenByteArray
  , decodeByteStreamSplitFloat
  , decodeByteStreamSplitDouble
  , decodeAlpFloat
  , decodeAlpDouble
  , decodeDictionaryIndices
  , validateDictionaryExpansion
  , decodeHybridRleLengthPrefixed
  , decodeHybridRleUnsigned32
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
  , readGenericFixedLenByteArrayColumnChunk
    -- ** Optional / nullable variants
  , readGenericInt32OptionalColumnChunk
  , readGenericInt64OptionalColumnChunk
  , readGenericFloatOptionalColumnChunk
  , readGenericDoubleOptionalColumnChunk
  , readGenericBoolOptionalColumnChunk
  , readGenericByteArrayOptionalColumnChunk
  , readGenericFixedLenByteArrayOptionalColumnChunk
    -- * Repeated readers (rep > 0 columns: lists, maps)
  , ColumnLevels (..)
  , readInt32Levels
  , readInt64Levels
  , readFloatLevels
  , readDoubleLevels
  , readBoolLevels
  , readByteArrayLevels
  , readFixedLevels
  , PageSelection (..)
  , readInt32SelectedLevels
  , readInt64SelectedLevels
  , readFloatSelectedLevels
  , readDoubleSelectedLevels
  , readBoolSelectedLevels
  , readByteArraySelectedLevels
  , readFixedSelectedLevels
  , readInt32SelectedValues
  , readInt64SelectedValues
  , readFloatSelectedValues
  , readDoubleSelectedValues
  , readBoolSelectedValues
  , readByteArraySelectedValues
  , readFixedSelectedValues
  , readGenericByteArrayRepeatedColumnChunk
  , readGenericByteArrayRepeatedColumnChunkWith
  , readGenericInt32RepeatedColumnChunk
  , readGenericInt64RepeatedColumnChunk
  , readGenericRepeatedColumnChunkWith
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

#include "MachDeps.h"

import Control.Exception (SomeException, evaluate, try)
import Control.Monad.ST (runST)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSUnsafe
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import Data.Primitive.ByteArray (MutableByteArray, copyByteArrayToAddr, mutableByteArrayContents, newPinnedByteArray)
import Data.Primitive.Types (Prim)

#if !defined(WORDS_BIGENDIAN)
import Data.Primitive.ByteArray (copyPtrToMutableByteArray)
#endif
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr, ptrToWordPtr)
import Foreign.Storable (Storable, sizeOf, peekByteOff, pokeByteOff)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as MVP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word (Word8, Word32, Word64)
import GHC.ByteOrder (ByteOrder (LittleEndian), targetByteOrder)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import GHC.ForeignPtr (mallocPlainForeignPtrAlignedBytes)
import GHC.Exts (RealWorld)
import Wireform.Encode.Direct qualified as Direct
import System.IO.Unsafe (unsafePerformIO)

import Columnar.LZO qualified as LZO
import Columnar.SIMD (copyBits, unpackBitsLsbUnsafe)
import qualified Columnar.SIMD as SIMD

import Parquet.Alp (decodeAlpFloat, decodeAlpDouble)
import Parquet.Delta
  ( decodeDeltaBinaryPackedInt32
  , decodeDeltaBinaryPackedInt64
  , decodeDeltaByteArray
  , decodeDeltaLengthByteArray
  )
import Parquet.RLE (decodeDictionaryIndices, decodeHybridRleBoolean, decodeHybridRleLengthPrefixed, decodeHybridRleUnsigned32, validateDictionaryExpansion, validateDictionaryExpansionFor)

import qualified Parquet.GZip as GZip
#ifdef HAVE_ZSTD
import qualified Codec.Compression.Zstd.Base as ZstdBase
import qualified Codec.Compression.Zstd.Lazy as ZstdLazy
#endif
#ifdef HAVE_SNAPPY
import qualified Codec.Compression.Snappy as Snappy
#endif
#ifdef HAVE_LZ4
import qualified Columnar.LZ4 as LZ4
#endif
#ifdef HAVE_BROTLI
import qualified Parquet.Brotli as Brotli
#endif

import qualified Columnar.IO as IO
import qualified Columnar.Stream as IS
import qualified Parquet.Encryption as Enc
import qualified Parquet.Decryption as Decryption
import qualified Parquet.Page as Page
import Parquet.Thrift.Schema (pattern PageHeader_CompressedSize, pattern PageHeader_Crc)
import qualified Thrift.Decode as TC
import Thrift.Encode (encodeCompact)
import qualified Thrift.Value as TV
import Parquet.Footer (readFooter)
import qualified Parquet.Footer as F
import Parquet.Levels
  ( levelBitWidth
  , materializePlainBoolOptional
  , materializePlainByteArrayOptional
  , materializePlainDoubleOptional
  , materializePlainFloatOptional
  , materializePlainInt32Optional
  , materializePlainInt64Optional
  , parseDataPageV1Levels
  , parseDataPageV1LevelsWithEncoding
  )
import Parquet.Page
  ( DataPageHeader (..)
  , DataPageHeaderV2 (..)
  , DictionaryPageHeader (..)
  , PageHeader (..)
  , PageType (..)
  , readPageHeaderCheckedAt
  , PageDetails (..)
  , readPageDetailsCheckedAt
  , readPageDetailsAt
  )
#ifdef HAVE_SNAPPY
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
#endif
#if defined(HAVE_SNAPPY) || defined(HAVE_ZSTD)
import Foreign.C.Types (CSize (..))
#endif
#ifdef HAVE_ZSTD
import Foreign.C.Types (CULLong (..))
#endif
import Parquet.Types
  ( ColumnChunk (..)
  , ColumnMetadata (..)
  , Compression (..)
  , FileMetadata (..)
  , PageLocation (..)
  , ParquetType (..)
  , RowGroup (..)
  )

-- | A Parquet file loaded in memory with parsed footer metadata.
data ParquetFile = ParquetFile
  { pfBytes  :: !ByteString
  , pfFooter :: !FileMetadata
  }
  | DecryptedParquetFile
  { pfBytes :: !ByteString
  , pfFooter :: !FileMetadata
  , pfDecryptionState :: !Decryption.DecryptionState
  }
  deriving stock (Show, Eq)

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

-- | Authenticate a @PARE@ encrypted footer or a @PAR1@ signed plaintext
-- footer. The supplied file identifier and AAD prefix must agree with the
-- file's crypto metadata. Ordinary unsigned @PAR1@ files remain accepted.
-- The supplied key also decrypts columns encrypted with the footer key. For
-- column-specific keys use 'loadParquetFileWithDecryption'. Use
-- 'Enc.readFooterCryptoMetadata' to inspect opaque key metadata
-- before resolving a key with an application's own KMS.
loadParquetFileEncrypted :: FooterDecryption -> ByteString -> Either String ParquetFile
loadParquetFileEncrypted fd bs = do
  loadParquetFileWithDecryption Decryption.defaultDecryptionConfig
    { Decryption.dcFooterKey = Just (fdKey fd)
    , Decryption.dcAadPrefix = Just (fdAadPrefix fd)
    , Decryption.dcExpectedFileId = Just (fdFileId fd)
    } bs


-- | Authenticate metadata and retain the context for decrypting requested
-- columns. Column keys can be supplied explicitly by schema path or resolved
-- by the application from opaque key metadata. Unrequested page bytes remain
-- untouched, including when the file is memory mapped.
loadParquetFileWithDecryption :: Decryption.DecryptionConfig -> ByteString -> Either String ParquetFile
loadParquetFileWithDecryption config bytes = do
  loaded <- Decryption.decryptFileMetadata config bytes
  Right DecryptedParquetFile
    { pfBytes = bytes
    , pfFooter = Decryption.decryptedMetadata loaded
    , pfDecryptionState = Decryption.decryptionState loaded
    }


fileColumnCrypto :: ParquetFile -> Int -> Int -> Maybe Decryption.ColumnCrypto
fileColumnCrypto ParquetFile{} _ _ = Nothing
fileColumnCrypto DecryptedParquetFile{pfDecryptionState = state} group column = Decryption.columnCrypto state group column


isColumnEncrypted :: ParquetFile -> Int -> Int -> Bool
isColumnEncrypted file group column = maybe False (const True) (fileColumnCrypto file group column)


-- | Column-specific key routes from an authenticated footer. This allows an
-- application to discover unresolved key IDs without parsing raw Thrift.
encryptedColumnKeyMetadata :: ParquetFile -> V.Vector Decryption.ColumnKeyMetadata
encryptedColumnKeyMetadata ParquetFile{} = V.empty
encryptedColumnKeyMetadata DecryptedParquetFile{pfDecryptionState = state} = Decryption.columnKeyMetadata state


-- | Authenticate an encrypted column auxiliary module, leaving plaintext
-- columns unchanged. Index/bloom readers use the same column key and AAD.
decryptFileColumnModule :: ParquetFile -> Int -> Int -> Enc.ModuleType -> ByteString -> Either String ByteString
decryptFileColumnModule file group column kind bytes = case fileColumnCrypto file group column of
  Nothing -> Right bytes
  Just context -> Decryption.decryptColumnModule context kind bytes

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
columnChunkSlice pf group column = do
  bytes <- rawColumnChunkSlice pf group column
  case fileColumnCrypto pf group column of
    Nothing -> Right bytes
    Just context -> do
      metadata <- Decryption.cryptoColumnMetadata context
      let dictionary = maybe False (> 0) (cmDictionaryPageOffset metadata)
      decryptChunk context metadata dictionary bytes


-- | Original on-disk bytes, before any page decryption. This is also the
-- bounds-checking seam for offset indexes; validating an index must not read
-- or decrypt the pages that a predicate will skip.
rawColumnChunkSlice :: ParquetFile -> Int -> Int -> Either String ByteString
rawColumnChunkSlice pf rgIdx colIdx = do
  case fileColumnCrypto pf rgIdx colIdx of
    Nothing -> Right ()
    Just context -> Decryption.cryptoColumnMetadata context >> Right ()
  let fm = pfFooter pf
      rgs = fmRowGroups fm
  whenOutOfRange rgIdx (V.length rgs) "row group"
  let rg = V.unsafeIndex rgs rgIdx
      cols = rgColumns rg
  whenOutOfRange colIdx (V.length cols) "column"
  let chunk = V.unsafeIndex cols colIdx
  case ccFilePath chunk of
    Just path | path /= mempty -> Left "Parquet.Read: external column-chunk files are not supported"
    _ -> Right ()
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
  if cmNumValues meta == 0
    then Right BS.empty
    else if startOff < 0 || sz < 0 || startOff > BS.length bs0 || sz > BS.length bs0 - startOff
    then Left "Parquet.Read: column chunk slice out of bounds"
    else do
      correctedSize <- case fileColumnCrypto pf rgIdx colIdx of
        Just _ -> Right sz
        Nothing -> legacyDictionaryChunkSize pf meta startOff sz
      Right $! BS.take correctedSize (BS.drop startOff bs0)


data DecryptedPage = DecryptedPage
  { decryptedPageBytes :: !ByteString
  , decryptedPageEnd :: {-# UNPACK #-} !Int
  , decryptedPageValues :: {-# UNPACK #-} !Int64
  , decryptedPageUsesDictionary :: !Bool
  }


decryptChunk :: Decryption.ColumnCrypto -> ColumnMetadata -> Bool -> ByteString -> Either String ByteString
decryptChunk context metadata dictionary bytes = walk 0 0 dictionary 0 []
  where
    walk !offset !page !isDictionary !values !pages
      | offset == BS.length bytes =
          if values == cmNumValues metadata
            then Right (BS.concat (reverse pages))
            else Left "Parquet.Read: encrypted chunk value count differs from metadata"
      | otherwise = do
          decoded <- decryptPage context isDictionary page bytes offset
          let count = decryptedPageValues decoded
          if count < 0 || count > cmNumValues metadata - values
            then Left "Parquet.Read: encrypted chunk exceeds declared value count"
            else walk (decryptedPageEnd decoded) (if isDictionary then page else page + 1) False (values + count) (decryptedPageBytes decoded : pages)


decryptPage :: Decryption.ColumnCrypto -> Bool -> Int -> ByteString -> Int -> Either String DecryptedPage
decryptPage context dictionary page bytes offset = do
  (encryptedHeader, afterHeader) <- Enc.readFramedModule bytes offset
  let headerKind = if dictionary then Enc.ModuleDictionaryPageHeader else Enc.ModuleDataPageHeader
      bodyKind = if dictionary then Enc.ModuleDictionaryPage else Enc.ModuleDataPage
  headerBytes <- Decryption.decryptColumnPageHeader context headerKind page encryptedHeader
  details <- Page.readPageDetailsAt headerBytes 0
  (headerValue, headerEnd) <- TC.decodeCompactFrom headerBytes 0
  if not (BS.all (== 0) (BS.drop headerEnd headerBytes))
    then Left "Parquet.Read: nonzero trailing encrypted page header bytes"
    else Right ()
  count <- case Page.phType (Page.pdHeader details) of
    PtDictionaryPage{} | dictionary -> Right 0
    PtDataPage dataHeader | not dictionary -> Right (fromIntegral (dphNumValues dataHeader))
    PtDataPageV2 dataHeader | not dictionary -> Right (fromIntegral (dph2NumValues dataHeader))
    _ -> Left "Parquet.Read: encrypted page type disagrees with dictionary/data position"
  (encryptedBody, afterBody) <- Enc.readFramedModule bytes afterHeader
  let wireBody = BS.take (afterBody - afterHeader) (BS.drop afterHeader bytes)
  case phCompressedPageSize (Page.pdHeader details) of
    Just size | fromIntegral size == BS.length wireBody -> Right ()
    _ -> Left "Parquet.Read: encrypted body size differs from authenticated header"
  case Page.pdChecksum details of
    Just expected -> Page.verifyPageChecksum expected wireBody
    Nothing -> Right ()
  body <- Decryption.decryptColumnPageBody context bodyKind page encryptedBody
  if BS.length body > fromIntegral (maxBound :: Int32)
    then Left "Parquet.Read: decrypted page exceeds signed 32-bit size"
    else Right ()
  fields <- case headerValue of
    TV.Struct fields -> Right fields
    _ -> Left "Parquet.Read: expected decrypted page header struct"
  -- CRC covers the on-disk encrypted body, which was checked above. Remove
  -- it from the temporary plaintext view; ordinary decoders still validate
  -- every other header, level, compression and value invariant.
  let normalized = TV.Struct (V.map (\field -> case field of
        PageHeader_CompressedSize _ -> PageHeader_CompressedSize (fromIntegral (BS.length body))
        _ -> field) (V.filter (\field -> fst field /= fst (PageHeader_Crc 0)) fields))
      usesDictionary = case phType (Page.pdHeader details) of
        PtDataPage header -> dphEncoding header `elem` [2, 8]
        PtDataPageV2 header -> dph2Encoding header `elem` [2, 8]
        _ -> False
  Right (DecryptedPage (encodeCompact normalized <> body) afterBody count usesDictionary)


data PreparedSelection = PreparedSelection
  { preparedSelectionBytes :: !ByteString
  , preparedSelection :: !PageSelection
  }


-- | Produce a temporary plaintext view containing only retained encrypted
-- pages and their dictionary when needed. Skipped locations get one-byte
-- placeholders, preserving all original row boundaries without reading those
-- page bytes. Plaintext columns keep their original file and index unchanged.
prepareColumnSelection :: ParquetFile -> Int -> Int -> PageSelection -> Either String PreparedSelection
prepareColumnSelection file group column selection = case fileColumnCrypto file group column of
  Nothing -> Right (PreparedSelection (pfBytes file) selection)
  Just context -> do
    let bytes = pfBytes file
        locations = selectionLocations selection
    validateSelection bytes selection
    raw <- rawColumnChunkSlice file group column
    metadata <- Decryption.cryptoColumnMetadata context
    let start = maybe (cmDataPageOffset metadata) (min (cmDataPageOffset metadata)) (cmDictionaryPageOffset metadata)
        end = start + fromIntegral (BS.length raw)
    V.mapM_ (\location ->
      if plOffset location < start || plOffset location > end || fromIntegral (plCompressedPageSize location) > end - plOffset location
        then Left "Parquet.Read: encrypted page selection is outside its column"
        else Right ()) locations
    pages <- V.imapM (\ordinal location ->
      if not (selectionKeep selection V.! ordinal)
        then Right (DecryptedPage (BS.singleton 0) 0 0 False)
        else do
          decoded <- decryptPage context False ordinal bytes (fromIntegral (plOffset location))
          if fromIntegral (decryptedPageEnd decoded) /= plOffset location + fromIntegral (plCompressedPageSize location)
            then Left "Parquet.Read: encrypted page size differs from offset index"
            else Right decoded) locations
    dictionary <- if V.any decryptedPageUsesDictionary pages
      then do
        offset <- maybe (Left "Parquet.Read: encrypted dictionary page offset is missing") Right (cmDictionaryPageOffset metadata)
        if V.null locations || offset < start || offset >= plOffset (V.head locations)
          then Left "Parquet.Read: encrypted dictionary lies outside the column"
          else Right ()
        decoded <- decryptPage context True 0 bytes (fromIntegral offset)
        if fromIntegral (decryptedPageEnd decoded) > plOffset (V.head locations)
          then Left "Parquet.Read: encrypted dictionary overlaps data pages"
          else Right (decryptedPageBytes decoded)
      else Right BS.empty
    let sizes = VP.generate (V.length pages) (BS.length . decryptedPageBytes . (pages V.!))
        offsets = VP.scanl' (+) (BS.length dictionary) sizes
    if VP.any (< 0) offsets || VP.last offsets < BS.length dictionary
      then Left "Parquet.Read: selected plaintext pages exceed addressable size"
      else Right ()
    normalizedLocations <- V.imapM (\index location ->
      let size = sizes VP.! index
      in if toInteger size > toInteger (maxBound :: Int32)
           then Left "Parquet.Read: plaintext indexed page exceeds signed 32-bit size"
           else Right location {plOffset = fromIntegral (offsets VP.! index), plCompressedPageSize = fromIntegral size}) locations
    let normalized = selection
          { selectionLocations = normalizedLocations
          , selectionDictionaryOffset = if BS.null dictionary then Nothing else Just 0
          }
        plaintext = BS.concat (dictionary : V.toList (V.map decryptedPageBytes pages))
    Right (PreparedSelection plaintext normalized)

-- Early parquet-mr files sometimes point data_page_offset at an unadvertised
-- dictionary and omit its header from total_compressed_size. Repair only that
-- exact legacy shape, by exactly one dictionary-header length. A complete
-- page walk must consume the corrected slice and the declared value count;
-- it may never enter another column, an auxiliary index, or the footer.
legacyDictionaryChunkSize :: ParquetFile -> ColumnMetadata -> Int -> Int -> Either String Int
legacyDictionaryChunkSize pf meta start size
  | fmCreatedBy footer /= Just "parquet-mr"
      || not (V.null (cmEncodings meta))
      || cmDictionaryPageOffset meta /= Nothing = Right size
  | otherwise = do
      details <- readPageDetailsAt original 0
      case phType (pdHeader details) of
        PtDictionaryPage {} -> case walk original of
          Right () -> Right size
          Left _ -> do
            trailer <- F.readFooterTrailer bytes
            let footerStart = BS.length bytes - 8 - BS.length (F.ftBytes trailer)
                boundary = V.foldl' rowBoundary (fromIntegral footerStart) (fmRowGroups footer)
                headerSize = pdBodyOffset details
            if fromIntegral headerSize > (boundary :: Int64) - fromIntegral start - fromIntegral size
              then Left "Parquet.Read: legacy dictionary size repair crosses a column/index/footer boundary"
              else do
                let corrected = size + headerSize
                walk (BS.take corrected (BS.drop start bytes))
                Right corrected
        _ -> Right size
  where
    bytes = pfBytes pf
    footer = pfFooter pf
    original = BS.take size (BS.drop start bytes)
    nearer boundary off
      | off > fromIntegral start = min boundary off
      | otherwise = boundary
    optionalBoundary boundary = maybe boundary (nearer boundary)
    rowBoundary boundary row = V.foldl' columnBoundary boundary (rgColumns row)
    columnBoundary boundary column =
      let indexed = optionalBoundary
            (optionalBoundary boundary (ccOffsetIndexOffset column))
            (ccColumnIndexOffset column)
      in case ccMetadata column of
        Nothing -> indexed
        Just m -> optionalBoundary
          (optionalBoundary (nearer indexed (cmDataPageOffset m)) (cmDictionaryPageOffset m))
          (cmBloomFilterOffset m)
    walk chunk = go 0 0 True
      where
        go !off !values !first
          | off == BS.length chunk =
              if not first && values == cmNumValues meta
                then Right ()
                else Left "Parquet.Read: legacy dictionary chunk value count mismatch"
          | otherwise = do
              details <- readPageDetailsCheckedAt chunk off
              let header = pdHeader details
              count <- case phType header of
                PtDictionaryPage {} | first -> Right 0
                PtDataPage p | not first -> Right (fromIntegral (dphNumValues p))
                PtDataPageV2 p | not first -> Right (fromIntegral (dph2NumValues p))
                _ -> Left "Parquet.Read: invalid page sequence in legacy dictionary chunk"
              if count > cmNumValues meta - values
                then Left "Parquet.Read: legacy dictionary chunk exceeds its declared value count"
                else case phCompressedPageSize header of
                  Just bodySize -> go (pdBodyOffset details + fromIntegral bodySize) (values + count) False
                  Nothing -> Left "Parquet.Read: missing legacy page size"

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
  (hdr, afterHdr) <- readPageHeaderCheckedAt chunk off
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

-- | Read every @DATA_PAGE@ with @PLAIN@ @INT32@ in order until the chunk ends.
readPlainInt32ColumnChunk :: Compression -> ByteString -> Either String (VP.Vector Int32)
readPlainInt32ColumnChunk codec chunk = go 0 []
  where
    go !off !acc
      | off >= BS.length chunk = Right (concatPages VP.concat acc)
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= encPlain
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              pageVec <- decodePlainInt32 n raw
              go nextOff (pageVec : acc)

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
readPlainOptionalColumnChunkWith codec maxRep maxDef chunk mat = go 0 []
  where
    go !off !acc
      | off >= BS.length chunk = Right (concatPages V.concat acc)
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= encPlain
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              (_rep, def, rest) <- parseDataPageV1Levels maxRep maxDef n raw
              page <- mat def maxDef rest
              go nextOff (page : acc)

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
readPlainInt64ColumnChunk codec chunk = go 0 []
  where
    go !off !acc
      | off >= BS.length chunk = Right (concatPages VP.concat acc)
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= encPlain
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              pageVec <- decodePlainInt64 n raw
              go nextOff (pageVec : acc)

-- | @PLAIN@ @FLOAT@ (IEEE little-endian).
readPlainFloatColumnChunk :: Compression -> ByteString -> Either String (VP.Vector Float)
readPlainFloatColumnChunk codec chunk = go 0 []
  where
    go !off !acc
      | off >= BS.length chunk = Right (concatPages VP.concat acc)
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= encPlain
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              pageVec <- decodePlainFloat n raw
              go nextOff (pageVec : acc)

-- | @PLAIN@ @DOUBLE@ (IEEE little-endian).
readPlainDoubleColumnChunk :: Compression -> ByteString -> Either String (VP.Vector Double)
readPlainDoubleColumnChunk codec chunk = go 0 []
  where
    go !off !acc
      | off >= BS.length chunk = Right (concatPages VP.concat acc)
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= encPlain
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              pageVec <- decodePlainDouble n raw
              go nextOff (pageVec : acc)

-- | @PLAIN@ @BOOLEAN@ (packed bits, LSB of first byte is first value).
readPlainBoolColumnChunk :: Compression -> ByteString -> Either String (V.Vector Bool)
readPlainBoolColumnChunk codec chunk = go 0 []
  where
    go !off !acc
      | off >= BS.length chunk = Right (concatPages V.concat acc)
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= encPlain
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              pageVec <- decodePlainBool n raw
              go nextOff (pageVec : acc)

-- | @PLAIN@ @BYTE_ARRAY@ (length-prefixed 4-byte LE + bytes per value).
readPlainByteArrayColumnChunk :: Compression -> ByteString -> Either String (V.Vector ByteString)
readPlainByteArrayColumnChunk codec chunk = go 0 []
  where
    go !off !acc
      | off >= BS.length chunk = Right (concatPages V.concat acc)
      | otherwise = do
          (_hdr, dph, raw, nextOff) <- decompressDataPageBody codec chunk off
          if dphEncoding dph /= encPlain
            then Left "Parquet.Read: encoding is not PLAIN (0)"
            else do
              let !n = fromIntegral (dphNumValues dph) :: Int
              pageVec <- decodePlainByteArray n raw
              go nextOff (pageVec : acc)

-- | Dictionary page (@PLAIN@ @INT32@ values) followed by @DATA_PAGE@s with
-- @PLAIN_DICTIONARY@ (indices as @PLAIN@ @INT32@). Plain @DATA_PAGE@s without
-- a dictionary are also accepted.
readPlainDictionaryInt32ColumnChunk :: Compression -> ByteString -> Either String (VP.Vector Int32)
readPlainDictionaryInt32ColumnChunk codec chunk = go 0 Nothing []
  where
    go !off !mDict !acc
      | off >= BS.length chunk =
          if all VP.null acc
            then Left "Parquet.Read: empty dictionary column chunk"
            else Right (concatPages VP.concat acc)
      | otherwise = do
          (hdr, afterHdr) <- readPageHeaderCheckedAt chunk off
          compSz <- case phCompressedPageSize hdr of
            Nothing -> Left "Parquet.Read: missing compressed_page_size"
            Just s -> Right (fromIntegral s :: Int)
          let !bodyStart = afterHdr
          if bodyStart + compSz > BS.length chunk
            then Left "Parquet.Read: truncated page body"
            else do
              let !compBody = BS.take compSz (BS.drop bodyStart chunk)
              checkDictionaryPagePosition off hdr
              !raw <- decompressPageData codec (phUncompressedPageSize hdr) compBody
              let !nextOff = bodyStart + compSz
              case phType hdr of
                PtDictionaryPage dk
                  | not (dictEncoding dk == encPlain || dictEncoding dk == encPlainDictionary) ->
                      Left "Parquet.Read: dictionary page encoding is neither PLAIN (0) nor PLAIN_DICTIONARY (2)"
                  | otherwise -> do
                      let !nDict = fromIntegral (dictNumValues dk) :: Int
                      dict <- decodePlainInt32 nDict raw
                      go nextOff (Just dict) acc
                PtDataPage dph -> case dphEncoding dph of
                  e
                    | e == encPlain -> do
                        let !n = fromIntegral (dphNumValues dph) :: Int
                        pageVec <- decodePlainInt32 n raw
                        go nextOff mDict (pageVec : acc)
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
                            go nextOff mDict (pageVec : acc)
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
  Left "Parquet.Read: legacy LZ4 requires uncompressed size; use decompressDataPageBody"
decompressChunk LZ4Raw _ =
  Left "Parquet.Read: LZ4_RAW requires uncompressed size; use decompressPage internally"
#ifdef HAVE_BROTLI
decompressChunk Brotli bs = tryBrotli bs
#else
decompressChunk Brotli _ =
  Left "Parquet.Read: Brotli requires building wireform with -fbrotli"
#endif
decompressChunk LZO bytes =
  either (Left . (("Parquet.Read: " ++) . LZO.prettyLZOError)) Right
    (LZO.decompress (fromIntegral (maxBound :: Int32)) bytes)
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
decompressPageData GZip (Just expected) bytes = GZip.decompressSized (fromIntegral expected) bytes
#ifdef HAVE_SNAPPY
decompressPageData Snappy (Just expected) bytes = trySnappySized (fromIntegral expected) bytes
#endif
decompressPageData LZO (Just expected) bytes = do
  raw <- either (Left . (("Parquet.Read: " ++) . LZO.prettyLZOError)) Right
    (LZO.decompress (fromIntegral expected) bytes)
  if BS.length raw == fromIntegral expected
    then Right raw
    else Left "Parquet.Read: LZO decompressed size differs from page header"
#ifdef HAVE_BROTLI
decompressPageData Brotli (Just expected) bytes = Brotli.decompressSized (fromIntegral expected) bytes
#endif
#ifdef HAVE_ZSTD
decompressPageData ZSTD (Just uncompSz) bs = tryZstdSized (fromIntegral uncompSz) bs
#endif
#ifdef HAVE_LZ4
decompressPageData LZ4 (Just uncompSz) bs = tryLegacyLZ4 (fromIntegral uncompSz) bs
decompressPageData LZ4Raw (Just uncompSz) bs = tryLZ4Raw (fromIntegral uncompSz) bs
decompressPageData LZ4Raw Nothing _ =
  Left "Parquet.Read: LZ4_RAW decompression requires uncompressed_page_size in header"
#endif
decompressPageData codec (Just expected) bytes = do
  raw <- decompressChunk codec bytes
  if BS.length raw == fromIntegral expected then Right raw else Left "Parquet.Read: decompressed size differs from page header"
decompressPageData codec Nothing bytes = decompressChunk codec bytes

-- V2's uncompressed_page_size includes the uncompressed level streams,
-- while the codec sees only the values. All-null pages can have no values
-- at all (not a valid Snappy stream), or a compressed empty ZSTD stream.
decompressV2Values
  :: Compression -> PageHeader -> DataPageHeaderV2 -> ByteString
  -> Either String ByteString
decompressV2Values codec hdr dph2 bytes = do
  size <- case phUncompressedPageSize hdr of
    Nothing -> Left "Parquet.Read: V2 page missing uncompressed_page_size"
    Just n -> Right (fromIntegral n - fromIntegral (dph2RepLevelsLen dph2) - fromIntegral (dph2DefLevelsLen dph2) :: Int)
  if size < 0
    then Left "Parquet.Read: V2 levels exceed uncompressed page size"
    else if size == 0 && dph2NumValues dph2 == dph2NumNulls dph2 && BS.null bytes
      then Right BS.empty
      else do
        values <- if dph2IsCompressed dph2
          then decompressPageData codec (Just (fromIntegral size)) bytes
          else Right bytes
        if BS.length values == size
          then Right values
          else Left "Parquet.Read: V2 decompressed values size mismatch"
{-# INLINE decompressV2Values #-}

tryGZip :: ByteString -> Either String ByteString
tryGZip = GZip.decompress

trySnappy :: ByteString -> Either String ByteString
#ifdef HAVE_SNAPPY
trySnappy = trySnappyBounded Nothing
#else
trySnappy _ = Left "Parquet.Read: Snappy requires building wireform with -fsnappy"
#endif

#ifdef HAVE_SNAPPY
foreign import ccall unsafe "snappy_uncompressed_length"
  snappyDecodedLength :: Ptr Word8 -> CSize -> Ptr CSize -> IO CInt

trySnappySized :: Int -> ByteString -> Either String ByteString
trySnappySized expected = trySnappyBounded (Just expected)

trySnappyBounded :: Maybe Int -> ByteString -> Either String ByteString
trySnappyBounded expected bytes = unsafePerformIO $ BSUnsafe.unsafeUseAsCStringLen bytes $ \(source, count) -> alloca $ \size -> do
  status <- snappyDecodedLength (castPtr source) (fromIntegral count) size
  if status /= 0 then pure (Left "Parquet.Read: invalid Snappy length header") else do
    actual <- peek size
    if actual > fromIntegral (maxBound :: Int32) || maybe False (\n -> n < 0 || actual /= fromIntegral n) expected
      then pure (Left "Parquet.Read: Snappy decoded size differs from page header or exceeds INT32 limit")
      else do
        result <- try @SomeException (evaluate (Snappy.decompress bytes))
        pure $ case result of
          Left err -> Left ("Parquet.Read: snappy decompress failed: " ++ show err)
          Right raw
            | fromIntegral (BS.length raw) == actual -> Right raw
            | otherwise -> Left "Parquet.Read: Snappy decoded size mismatch"
#endif

#ifdef HAVE_ZSTD
foreign import ccall unsafe "ZSTD_getFrameContentSize"
  zstdFrameContentSize :: Ptr Word8 -> CSize -> IO CULLong

tryZstd :: ByteString -> Either String ByteString
tryZstd bytes = unsafePerformIO $ BSUnsafe.unsafeUseAsCStringLen bytes $ \(source, count) -> do
  size <- zstdFrameContentSize (castPtr source) (fromIntegral count)
  if size == maxBound - 1
    then pure (Left "Parquet.Read: invalid ZSTD frame header")
    else if size == maxBound
      then do
        result <- try @SomeException $ evaluate $ BL.toStrict $
          BL.take (fromIntegral (maxBound :: Int32) + 1) (ZstdLazy.decompress (BL.fromStrict bytes))
        pure $ case result of
          Left err -> Left ("Parquet.Read: zstd decompress failed: " ++ show err)
          Right raw
            | BS.length raw > fromIntegral (maxBound :: Int32) -> Left "Parquet.Read: ZSTD output exceeds INT32 page-size limit"
            | otherwise -> tryZstdSized (BS.length raw) bytes
      else if size > fromIntegral (maxBound :: Int32)
        then pure (Left "Parquet.Read: ZSTD output exceeds INT32 page-size limit")
        else pure (tryZstdSized (fromIntegral size) bytes)

-- A ZSTD frame may omit its content size, and a valid empty frame has
-- size zero. The high-level zstd API reports both as Skip. Parquet already
-- provides the exact size, so decode directly into that bounded buffer.
tryZstdSized :: Int -> ByteString -> Either String ByteString
tryZstdSized expected bytes
  | expected < 0 = Left "Parquet.Read: negative ZSTD uncompressed size"
  | otherwise = unsafePerformIO $ do
      output <- BSI.mallocByteString expected
      result <- withForeignPtr output $ \dst ->
        BSUnsafe.unsafeUseAsCStringLen bytes $ \(src, size) ->
          ZstdBase.decompress dst expected src size
      pure $ case result of
        Left err -> Left ("Parquet.Read: zstd decompress failed: " ++ err)
        Right size
          | size == expected -> Right (BSI.fromForeignPtr output 0 size)
          | otherwise -> Left "Parquet.Read: ZSTD decompressed size differs from page header"
#endif

#ifdef HAVE_LZ4
-- Codec 5 was emitted both with Hadoop's big-endian block/chunk lengths
-- and (by older parquet-cpp writers) as a raw block. Bound every advertised
-- size against the page size before asking liblz4 for an output buffer.
tryLegacyLZ4 :: Int -> ByteString -> Either String ByteString
tryLegacyLZ4 expected bytes = case blocks 0 0 [] of
  Right parts -> Right (BS.concat (reverse parts))
  Left _ -> tryLZ4Raw expected bytes
  where
    size = BS.length bytes
    be32 off = fromIntegral
      ((fromIntegral (BSUnsafe.unsafeIndex bytes off) `shiftL` 24
        .|. fromIntegral (BSUnsafe.unsafeIndex bytes (off + 1)) `shiftL` 16
        .|. fromIntegral (BSUnsafe.unsafeIndex bytes (off + 2)) `shiftL` 8
        .|. fromIntegral (BSUnsafe.unsafeIndex bytes (off + 3))) :: Word32)
    blocks !off !written !parts
      | off == size && written == expected = Right parts
      | size - off < 4 = Left "Parquet.Read: truncated Hadoop LZ4 block header"
      | otherwise =
          let blockSize = be32 off
          in if blockSize <= 0 || blockSize > expected - written
               then Left "Parquet.Read: invalid Hadoop LZ4 block size"
               else chunks (off + 4) written blockSize parts
    chunks !off !written !remaining !parts
      | remaining == 0 = blocks off written parts
      | size - off < 4 = Left "Parquet.Read: truncated Hadoop LZ4 chunk header"
      | otherwise = do
          let compressedSize = be32 off
          if compressedSize <= 0 || compressedSize > size - off - 4
            then Left "Parquet.Read: invalid Hadoop LZ4 chunk size"
            else do
              decoded <- LZ4.decompress remaining (BS.take compressedSize (BS.drop (off + 4) bytes))
              let actual = BS.length decoded
              if actual <= 0 || actual > remaining
                then Left "Parquet.Read: invalid Hadoop LZ4 decoded chunk size"
                else chunks (off + 4 + compressedSize) (written + actual)
                       (remaining - actual) (decoded : parts)

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
tryBrotli = Brotli.decompress
#endif

decodePlainInt32 :: Int -> ByteString -> Either String (VP.Vector Int32)
decodePlainInt32 n bs
  | n < 0 || n > BS.length bs `quot` 4 = Left "Parquet.Read: PLAIN INT32 buffer too small"
#if !defined(WORDS_BIGENDIAN)
  | otherwise = Right $! copyPlainNative n bs
#else
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
#endif

decodePlainInt64 :: Int -> ByteString -> Either String (VP.Vector Int64)
decodePlainInt64 n bs
  | n < 0 || n > BS.length bs `quot` 8 = Left "Parquet.Read: PLAIN INT64 buffer too small"
#if !defined(WORDS_BIGENDIAN)
  | otherwise = Right $! copyPlainNative n bs
#else
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
#endif

decodePlainFloat :: Int -> ByteString -> Either String (VP.Vector Float)
decodePlainFloat n bs
  | n < 0 || n > BS.length bs `quot` 4 = Left "Parquet.Read: PLAIN FLOAT buffer too small"
#if !defined(WORDS_BIGENDIAN)
  | otherwise = Right $! copyPlainNative n bs
#else
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
#endif

decodePlainDouble :: Int -> ByteString -> Either String (VP.Vector Double)
decodePlainDouble n bs
  | n < 0 || n > BS.length bs `quot` 8 = Left "Parquet.Read: PLAIN DOUBLE buffer too small"
#if !defined(WORDS_BIGENDIAN)
  | otherwise = Right $! copyPlainNative n bs
#else
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
#endif

#if !defined(WORDS_BIGENDIAN)
-- PLAIN primitives already have native layout on little-endian hosts.
-- Copy only the validated prefix, accepting unaligned input without typed
-- pointer loads. The fresh primitive vector owns the immutable result.
copyPlainNative :: forall a. Prim a => Int -> ByteString -> VP.Vector a
copyPlainNative n bs
  | n == 0 = VP.empty
  | otherwise = unsafePerformIO $ do
      mv@(MVP.MVector off _ array) <- MVP.unsafeNew n
      BSUnsafe.unsafeUseAsCString bs $ \ptr ->
        copyPtrToMutableByteArray array off (castPtr ptr :: Ptr a) n
      VP.unsafeFreeze mv
{-# NOINLINE copyPlainNative #-}
#endif

decodePlainBool :: Int -> ByteString -> Either String (V.Vector Bool)
decodePlainBool n bs
  -- n comes from the page header (dphNumValues, an Int32), so it can be
  -- negative in a malformed file; unpackBitsLsbUnsafe allocates n slots.
  | n < 0 = Left "Parquet.Read: PLAIN BOOLEAN negative value count"
  | wholeBytes > BS.length bs || (wholeBytes == BS.length bs && tailBits /= 0) = Left "Parquet.Read: PLAIN BOOLEAN buffer too small"
  | otherwise = Right $! unpackBitsLsbUnsafe n bs
  where
    (wholeBytes, tailBits) = n `quotRem` 8

decodePlainByteArray :: Int -> ByteString -> Either String (V.Vector ByteString)
decodePlainByteArray n bs0
  | n < 0 = Left "Parquet.Read: PLAIN BYTE_ARRAY negative value count"
  | n > BS.length bs0 `quot` 4 = Left "Parquet.Read: PLAIN BYTE_ARRAY truncated lengths"
  | otherwise = runST $ do
      out <- VM.unsafeNew n
      let go !i !off
            | i == n = Right <$> V.unsafeFreeze out
            | BS.length bs0 - off < 4 = pure (Left "Parquet.Read: PLAIN BYTE_ARRAY truncated length")
            | otherwise = do
                let !len = fromIntegral (readLE32 bs0 off) :: Int
                    !payloadOff = off + 4
                if len > BS.length bs0 - payloadOff
                  then pure (Left "Parquet.Read: PLAIN BYTE_ARRAY payload out of bounds")
                  else do
                    let !payload = BS.take len (BS.drop payloadOff bs0)
                    VM.unsafeWrite out i payload
                    go (i + 1) (payloadOff + len)
      go 0 0

{- | Dense BYTE_ARRAY values in Arrow's offsets/data layout. Construction is
checked, offsets start at zero, and the data fits signed 32-bit offsets.
The constructor stays private so dictionary gather can trust these bounds.
-}
type BinaryBuffers = BinaryBuffersOf Int32


-- | Dense BYTE_ARRAY buffers with signed64-bit Arrow offsets.
type LargeBinaryBuffers = BinaryBuffersOf Int64


data BinaryBuffersOf o = BinaryBuffers
  { binaryOffsets :: !(VS.Vector o)
  , binaryData :: !ByteString
  }


-- Internal page layouts retain validated source bytes until the entire
-- column's size is known. Only the final materialization copies payloads;
-- collecting many pages must not copy their data twice.
data BinaryPiece o = BinaryPiece
  { pieceOffsets :: !(VS.Vector o)
  , pieceSource :: !(BinarySource o)
  }


data BinarySource o
  = BinaryContiguous !ByteString
  | BinaryPrepared !(PreparedDictionary o)
  | BinaryPlain !ByteString
  | BinaryDictionary !(BinaryBuffersOf o) !(VP.Vector Int32)
  | BinaryBoxed !(V.Vector ByteString)
  | BinaryJoined ![BinaryPiece o]


data PreparedDictionary o = PreparedDictionary
  { preparedBuffers :: !(BinaryBuffersOf o)
  , preparedMaxBytes :: {-# UNPACK #-} !Int
  }


emptyBinaryPiece :: forall o. (Num o, Storable o) => BinaryPiece o
emptyBinaryPiece = BinaryPiece (VS.singleton 0) (BinaryContiguous BS.empty)


pieceBytes :: forall o. (Integral o, Storable o) => BinaryPiece o -> Int
pieceBytes = fromIntegral . VS.unsafeLast . pieceOffsets
{-# INLINE pieceBytes #-}


{- | Decode a complete PLAIN BYTE_ARRAY values section, without allocating
a ByteString slice for every row. Validate all lengths before copying.
-}
decodePlainBinaryBuffers :: Int -> ByteString -> Either String BinaryBuffers
decodePlainBinaryBuffers n raw = materializeBinaryPiece <$> decodePlainBinaryPiece n raw


decodePlainBinaryPiece :: forall o. (Integral o, Bounded o, Storable o) => Int -> ByteString -> Either String (BinaryPiece o)
decodePlainBinaryPiece n raw
  | n < 0 || n > BS.length raw `quot` 4 = Left "Parquet.Read: invalid PLAIN BYTE_ARRAY count"
  | n > binaryCountLimit @o = Left "Parquet.Read: BYTE_ARRAY offsets exceed address space"
  | dataSize > binaryOffsetLimit @o = Left ("Parquet.Read: BYTE_ARRAY data exceeds " ++ binaryOffsetName @o)
  | n == 0 = if BS.null raw then Right emptyBinaryPiece else Left "Parquet.Read: PLAIN BYTE_ARRAY count/size mismatch"
  | otherwise = unsafePerformIO $ do
      offsets <- VSM.unsafeNew (n + 1)
      result <- BSUnsafe.unsafeUseAsCStringLen raw $ \(src, _) -> do
        let go !i !sourceOffset !dataOffset
              | i == n =
                  if sourceOffset == BS.length raw && dataOffset == dataSize
                    then VSM.unsafeWrite offsets n (fromIntegral dataOffset) *> pure (Right ())
                    else pure (Left "Parquet.Read: PLAIN BYTE_ARRAY count/size mismatch")
              | BS.length raw - sourceOffset < 4 = pure (Left "Parquet.Read: truncated PLAIN BYTE_ARRAY length")
              | otherwise = do
                  encodedSize <- binaryLengthAt (castPtr src) sourceOffset
                  let !size = fromIntegral encodedSize :: Int
                      !payload = sourceOffset + 4
                  if size < 0 || size > BS.length raw - payload || size > dataSize - dataOffset
                    then pure (Left "Parquet.Read: PLAIN BYTE_ARRAY payload out of bounds")
                    else do
                      VSM.unsafeWrite offsets i (fromIntegral dataOffset)
                      go (i + 1) (payload + size) (dataOffset + size)
        go 0 0 0
      case result of
        Left err -> pure (Left err)
        Right () -> do
          frozen <- VS.unsafeFreeze offsets
          pure (Right (BinaryPiece frozen (BinaryPlain raw)))
  where
    !dataSize = BS.length raw - n * 4
{-# INLINEABLE decodePlainBinaryPiece #-}


-- The enclosing useAsCStringLen keeps the source alive. The caller checked
-- all four bytes; individual byte loads also work on unaligned input slices.
binaryLengthAt :: Ptr Word8 -> Int -> IO Word32
binaryLengthAt bytes offset = do
  a <- peekByteOff bytes offset :: IO Word8
  b <- peekByteOff bytes (offset + 1) :: IO Word8
  c <- peekByteOff bytes (offset + 2) :: IO Word8
  d <- peekByteOff bytes (offset + 3) :: IO Word8
  pure $!
    fromIntegral a
      .|. (fromIntegral b `shiftL` 8)
      .|. (fromIntegral c `shiftL` 16)
      .|. (fromIntegral d `shiftL` 24)
{-# INLINE binaryLengthAt #-}


-- Dictionaries are packed once when loaded, then shared by all data pages.
-- Validate indices and expansion sizes while filling only offsets. No output
-- payload is allocated until all selected pages passed their preflight.
gatherBinaryPiece :: forall o. (Integral o, Bounded o, Storable o) => BinaryPiece o -> VP.Vector Int32 -> Either String (BinaryPiece o)
gatherBinaryPiece dictionary indices
  | VP.length indices > binaryCountLimit @o = Left "Parquet.Read: BYTE_ARRAY offsets exceed address space"
  | otherwise = unsafePerformIO $ do
      let !buffers = materializeBinaryPiece dictionary
          !sourceOffsets = binaryOffsets buffers
          !dictionaryCount = VS.length sourceOffsets - 1
          !count = VP.length indices
      offsets <- VSM.unsafeNew (count + 1)
      let measure !i !total
            | i == count = VSM.unsafeWrite offsets count (fromIntegral total) *> pure (Right ())
            | key < 0 || key >= dictionaryCount = pure (Left "Parquet.Read: dictionary index out of range")
            | size > binaryOffsetLimit @o - total = pure (Left ("Parquet.Read: dictionary output exceeds " ++ binaryOffsetName @o))
            | otherwise = do
                VSM.unsafeWrite offsets i (fromIntegral total)
                measure (i + 1) (total + size)
            where
              key = fromIntegral (VP.unsafeIndex indices i) :: Int
              size = fromIntegral (VS.unsafeIndex sourceOffsets (key + 1) - VS.unsafeIndex sourceOffsets key) :: Int
      result <- measure 0 0
      case result of
        Left err -> pure (Left err)
        Right () -> do
          frozen <- VS.unsafeFreeze offsets
          pure (Right (BinaryPiece frozen (BinaryDictionary buffers indices)))
{-# INLINEABLE gatherBinaryPiece #-}


data BinarySize = BinarySize
  { binaryRows :: {-# UNPACK #-} !Int
  , binaryBytes :: {-# UNPACK #-} !Int
  }


concatBinaryPieces :: forall o. (Integral o, Bounded o, Storable o) => [BinaryPiece o] -> Either String (BinaryPiece o)
concatBinaryPieces [] = Right emptyBinaryPiece
concatBinaryPieces pieces = do
  size <- measure 0 0 pieces
  let !totalRows = binaryRows size
      !totalBytes = binaryBytes size
  Right $!
    unsafePerformIO
      ( do
          offsets <- VSM.unsafeNew (totalRows + 1)
          let fill !_ !_ [] = VSM.unsafeWrite offsets totalRows (fromIntegral totalBytes)
              fill !row !position (piece : rest) = do
                let !source = pieceOffsets piece
                    !n = VS.length source - 1
                    copyOffsets !i
                      | i == n = pure ()
                      | otherwise = do
                          VSM.unsafeWrite offsets (row + i) (fromIntegral position + VS.unsafeIndex source i)
                          copyOffsets (i + 1)
                copyOffsets 0
                fill (row + n) (position + pieceBytes piece) rest
          fill 0 0 pieces
          frozen <- VS.unsafeFreeze offsets
          pure (BinaryPiece frozen (BinaryJoined pieces))
      )
  where
    measure !rows !bytes [] = Right (BinarySize rows bytes)
    measure !rows !bytes (piece : rest)
      | n > binaryCountLimit @o - rows = Left "Parquet.Read: BYTE_ARRAY offsets exceed address space"
      | size > binaryOffsetLimit @o - bytes = Left ("Parquet.Read: BYTE_ARRAY data exceeds " ++ binaryOffsetName @o)
      | otherwise = measure (rows + n) (bytes + size) rest
      where
        !n = VS.length (pieceOffsets piece) - 1
        !size = pieceBytes piece
{-# INLINEABLE concatBinaryPieces #-}


-- DELTA encodings currently expose slices. Retain those slices until the
-- final copy, just as PLAIN and dictionary pages retain their source buffers.
packBinaryPiece :: forall o. (Integral o, Bounded o, Storable o) => V.Vector ByteString -> Either String (BinaryPiece o)
packBinaryPiece values
  | V.length values > binaryCountLimit @o = Left "Parquet.Read: BYTE_ARRAY offsets exceed address space"
  | otherwise = unsafePerformIO $ do
      let !count = V.length values
      offsets <- VSM.unsafeNew (count + 1)
      let measure !i !total
            | i == count = VSM.unsafeWrite offsets count (fromIntegral total) *> pure (Right ())
            | size > binaryOffsetLimit @o - total = pure (Left ("Parquet.Read: BYTE_ARRAY data exceeds " ++ binaryOffsetName @o))
            | otherwise = do
                VSM.unsafeWrite offsets i (fromIntegral total)
                measure (i + 1) (total + size)
            where
              size = BS.length (V.unsafeIndex values i)
      result <- measure 0 0
      case result of
        Left err -> pure (Left err)
        Right () -> do
          frozen <- VS.unsafeFreeze offsets
          pure (Right (BinaryPiece frozen (BinaryBoxed values)))
{-# INLINEABLE packBinaryPiece #-}


materializeBinaryPiece :: forall o. (Integral o, Bounded o, Storable o) => BinaryPiece o -> BinaryBuffersOf o
materializeBinaryPiece (BinaryPiece _ (BinaryPrepared dictionary)) = preparedBuffers dictionary
materializeBinaryPiece (BinaryPiece offsets (BinaryContiguous bytes)) = BinaryBuffers offsets bytes
materializeBinaryPiece piece = unsafePerformIO $ do
  let !size = pieceBytes piece
  output <- BSI.mallocByteString size
  withForeignPtr output (copyBinaryPiece piece)
  pure (BinaryBuffers (pieceOffsets piece) (BSI.fromForeignPtr output 0 size))
{-# INLINEABLE materializeBinaryPiece #-}


-- Every source span, index, offset and aggregate size was validated before
-- this loop. No per-value ByteString wrappers or intermediate payload copies.
copyBinaryPiece :: forall o. (Integral o, Bounded o, Storable o) => BinaryPiece o -> Ptr Word8 -> IO ()
copyBinaryPiece piece dst
  | pieceBytes piece == 0 = pure ()
  | otherwise = case pieceSource piece of
      BinaryPrepared dictionary ->
        BSUnsafe.unsafeUseAsCStringLen (binaryData (preparedBuffers dictionary)) $ \(src, n) -> copyBytes dst (castPtr src) n
      BinaryContiguous bytes ->
        BSUnsafe.unsafeUseAsCStringLen bytes $ \(src, n) -> copyBytes dst (castPtr src) n
      BinaryPlain raw ->
        BSUnsafe.unsafeUseAsCStringLen raw $ \(src, _) -> do
          let !offsets = pieceOffsets piece
              !count = VS.length offsets - 1
              fill !i !start
                | i == count = pure ()
                | otherwise = do
                    let !end = fromIntegral (VS.unsafeIndex offsets (i + 1))
                        !payload = start + (i + 1) * 4
                    copyBytes (dst `plusPtr` start) (castPtr src `plusPtr` payload) (end - start)
                    fill (i + 1) end
          fill 0 0
      BinaryDictionary (BinaryBuffers offsets bytes) indices ->
        BSUnsafe.unsafeUseAsCStringLen bytes $ \(src, _) -> do
          let !count = VP.length indices
              fill !i !position
                | i == count = pure ()
                | otherwise = do
                    let !key = fromIntegral (VP.unsafeIndex indices i)
                        !start = fromIntegral (VS.unsafeIndex offsets key)
                        !size = fromIntegral (VS.unsafeIndex offsets (key + 1)) - start
                    copyBytes (dst `plusPtr` position) (castPtr src `plusPtr` start) size
                    fill (i + 1) (position + size)
          fill 0 0
      BinaryBoxed values -> do
        let !count = V.length values
            fill !i !position
              | i == count = pure ()
              | otherwise = do
                  let !bytes = V.unsafeIndex values i
                  BSUnsafe.unsafeUseAsCStringLen bytes $ \(src, n) -> copyBytes (dst `plusPtr` position) (castPtr src) n
                  fill (i + 1) (position + BS.length bytes)
        fill 0 0
      BinaryJoined pieces ->
        let fill !_ [] = pure ()
            fill !position (part : rest) = do
              copyBinaryPiece part (dst `plusPtr` position)
              fill (position + pieceBytes part) rest
        in fill 0 pieces
{-# INLINEABLE copyBinaryPiece #-}


prepareBinaryDictionary :: forall o. (Integral o, Bounded o, Storable o) => BinaryPiece o -> Either String (BinaryPiece o)
prepareBinaryDictionary piece =
  let !buffers = materializeBinaryPiece piece
      !offsets = binaryOffsets buffers
      maximumSize !i !previous !maximumBytes
        | i == VS.length offsets = maximumBytes
        | otherwise =
            let !next = fromIntegral (VS.unsafeIndex offsets i)
            in maximumSize (i + 1) next (max maximumBytes (next - previous))
      !largest = maximumSize 1 0 0
  in Right (BinaryPiece offsets (BinaryPrepared (PreparedDictionary buffers largest)))


decodeBinaryDictionary :: forall o. (Integral o, Bounded o, Storable o) => Int -> ByteString -> BinaryPiece o -> Either String (BinaryPiece o)
decodeBinaryDictionary count bytes dictionary
  | count < 0 = Left "Parquet.Read: negative dictionary value count"
  | count == 0 = Right emptyBinaryPiece
  | count > binaryCountLimit @o = Left "Parquet.Read: BYTE_ARRAY offsets exceed address space"
  | VS.length (pieceOffsets dictionary) < 2 = Left "Parquet.Read: dictionary index out of range"
  | otherwise = do
      -- Most pages provably fit from count * maximum entry length. Only
      -- potentially oversized pages need a streaming exact preflight,
      -- before either decoded indices or expanded offsets are allocated.
      let maximumBytes = case pieceSource dictionary of
            BinaryPrepared prepared -> preparedMaxBytes prepared
            _ -> pieceBytes dictionary
      if maximumBytes /= 0 && count > binaryOffsetLimit @o `quot` maximumBytes
        then validateDictionaryExpansionFor (binaryOffsetLimit @o) (pieceOffsets dictionary) count bytes
        else Right ()
      indices <- decodeDictionaryIndices count bytes
      gatherBinaryPiece dictionary indices


binaryOffsetLimit :: forall o. (Integral o, Bounded o) => Int
binaryOffsetLimit = fromInteger (min (toInteger (maxBound :: o)) (toInteger (maxBound :: Int)))
{-# INLINE binaryOffsetLimit #-}


binaryCountLimit :: forall o. (Num o, Storable o) => Int
binaryCountLimit = maxBound `quot` sizeOf (0 :: o) - 1
{-# INLINE binaryCountLimit #-}


binaryOffsetName :: forall o. (Num o, Storable o) => String
binaryOffsetName = if sizeOf (0 :: o) == 4 then "32-bit offsets; request LargeUtf8/LargeBinary for larger columns" else "64-bit offsets"
{-# INLINE binaryOffsetName #-}


{-# SPECIALIZE emptyBinaryPiece :: BinaryPiece Int32 #-}
{-# SPECIALIZE emptyBinaryPiece :: BinaryPiece Int64 #-}


{-# SPECIALIZE pieceBytes :: BinaryPiece Int32 -> Int #-}
{-# SPECIALIZE pieceBytes :: BinaryPiece Int64 -> Int #-}


{-# SPECIALIZE decodePlainBinaryPiece :: Int -> ByteString -> Either String (BinaryPiece Int32) #-}
{-# SPECIALIZE decodePlainBinaryPiece :: Int -> ByteString -> Either String (BinaryPiece Int64) #-}


{-# SPECIALIZE gatherBinaryPiece :: BinaryPiece Int32 -> VP.Vector Int32 -> Either String (BinaryPiece Int32) #-}
{-# SPECIALIZE gatherBinaryPiece :: BinaryPiece Int64 -> VP.Vector Int32 -> Either String (BinaryPiece Int64) #-}


{-# SPECIALIZE concatBinaryPieces :: [BinaryPiece Int32] -> Either String (BinaryPiece Int32) #-}
{-# SPECIALIZE concatBinaryPieces :: [BinaryPiece Int64] -> Either String (BinaryPiece Int64) #-}


{-# SPECIALIZE packBinaryPiece :: V.Vector ByteString -> Either String (BinaryPiece Int32) #-}
{-# SPECIALIZE packBinaryPiece :: V.Vector ByteString -> Either String (BinaryPiece Int64) #-}


{-# SPECIALIZE materializeBinaryPiece :: BinaryPiece Int32 -> BinaryBuffersOf Int32 #-}
{-# SPECIALIZE materializeBinaryPiece :: BinaryPiece Int64 -> BinaryBuffersOf Int64 #-}


{-# SPECIALIZE copyBinaryPiece :: BinaryPiece Int32 -> Ptr Word8 -> IO () #-}
{-# SPECIALIZE copyBinaryPiece :: BinaryPiece Int64 -> Ptr Word8 -> IO () #-}


{-# SPECIALIZE prepareBinaryDictionary :: BinaryPiece Int32 -> Either String (BinaryPiece Int32) #-}
{-# SPECIALIZE prepareBinaryDictionary :: BinaryPiece Int64 -> Either String (BinaryPiece Int64) #-}


{-# SPECIALIZE decodeBinaryDictionary :: Int -> ByteString -> BinaryPiece Int32 -> Either String (BinaryPiece Int32) #-}
{-# SPECIALIZE decodeBinaryDictionary :: Int -> ByteString -> BinaryPiece Int64 -> Either String (BinaryPiece Int64) #-}


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
  go 0 Nothing []
  where
    go !off !mDict !acc
      | off >= BS.length chunk = Right (concatPages V.concat acc)
      | otherwise = do
          (hdr, afterHdr) <- readPageHeaderCheckedAt chunk off
          compSz <- case phCompressedPageSize hdr of
            Nothing -> Left "Parquet.Read: missing compressed_page_size"
            Just s -> Right (fromIntegral s :: Int)
          let !bodyStart = afterHdr
          if bodyStart + compSz > BS.length chunk
            then Left "Parquet.Read: truncated page body"
            else do
              let !compBody = BS.take compSz (BS.drop bodyStart chunk)
              checkDictionaryPagePosition off hdr
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
  (hdr, afterHdr) <- readPageHeaderCheckedAt chunk off
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
              !values <- decompressV2Values codec hdr dph2 valuesSection
              let !nextOff = bodyStart + compSz
              Right (hdr, dph2, repLevels, defLevels, values, nextOff)

readLE32 :: ByteString -> Int -> Word32
readLE32 bs o =
  let b0 = fromIntegral (BS.index bs o) :: Word32
      b1 = fromIntegral (BS.index bs (o + 1)) :: Word32
      b2 = fromIntegral (BS.index bs (o + 2)) :: Word32
      b3 = fromIntegral (BS.index bs (o + 3)) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)

#if defined(WORDS_BIGENDIAN)
readLE64 :: ByteString -> Int -> Word64
readLE64 bs o =
  let w0 = fromIntegral (readLE32 bs o) :: Word64
      w1 = fromIntegral (readLE32 bs (o + 4)) :: Word64
  in w0 .|. (w1 `shiftL` 32)
#endif

-- | @PLAIN@ @INT96@ — each value is 12 raw bytes, returned as 'ByteString'.
{-# INLINE decodePlainInt96 #-}
decodePlainInt96 :: Int -> ByteString -> Either String (V.Vector ByteString)
decodePlainInt96 n bs
  | n < 0 || n > BS.length bs `quot` 12 = Left "Parquet.Read: PLAIN INT96 buffer too small"
  | otherwise = Right $! V.generate n $ \i ->
      BS.take 12 (BS.drop (i * 12) bs)

-- | @PLAIN@ @FIXED_LEN_BYTE_ARRAY@ — each value is @typeLen@ raw bytes.
{-# INLINE decodePlainFixedLenByteArray #-}
decodePlainFixedLenByteArray :: Int -> Int -> ByteString -> Either String (V.Vector ByteString)
decodePlainFixedLenByteArray typeLen n bs
  | typeLen <= 0 = Left "Parquet.Read: FIXED_LEN_BYTE_ARRAY type_length must be positive"
  | n < 0 || n > BS.length bs `quot` typeLen = Left "Parquet.Read: PLAIN FIXED_LEN_BYTE_ARRAY buffer too small"
  | otherwise = Right $! V.generate n $ \i ->
      BS.take typeLen (BS.drop (i * typeLen) bs)

-- | All @DATA_PAGE@s as @PLAIN@ @INT96@.
readPlainInt96ColumnChunk :: Compression -> ByteString -> Either String (V.Vector ByteString)
readPlainInt96ColumnChunk codec chunk = go 0 []
  where
    go !off !acc
      | off >= BS.length chunk = Right (concatPages V.concat acc)
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
      | off >= BS.length chunk = Right (concatPages V.concat acc)
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
decodeByteStreamSplitFloat :: Int -> ByteString -> Either String (VP.Vector Float)
decodeByteStreamSplitFloat n bs
  | n < 0 || n > BS.length bs `quot` 4 = Left "Parquet.Read: BYTE_STREAM_SPLIT FLOAT buffer too small"
  | n >= 16 && targetByteOrder == LittleEndian =
      Right $ decodeByteStreamSplitNative 4 SIMD.joinByteStreams4Array SIMD.joinByteStreams4 n bs
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
  | n < 0 || n > BS.length bs `quot` 8 = Left "Parquet.Read: BYTE_STREAM_SPLIT DOUBLE buffer too small"
  | n >= 16 && targetByteOrder == LittleEndian =
      Right $ decodeByteStreamSplitNative 8 SIMD.joinByteStreams8Array SIMD.joinByteStreams8 n bs
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


-- The caller proves width*n fits inside the input. A bounded unsafe call
-- can fill an ordinary mutable array without allowing GC to move it.
-- Larger outputs are explicitly pinned for the safe call; freezing after
-- the call keeps their owner alive throughout C's writes.
decodeByteStreamSplitNative
  :: Prim a
  => Int
  -> (Ptr Word8 -> MutableByteArray RealWorld -> Int -> IO ())
  -> (Ptr Word8 -> Ptr Word8 -> Int -> IO ())
  -> Int
  -> ByteString
  -> VP.Vector a
decodeByteStreamSplitNative width joinArray join n bs = unsafePerformIO $
  if n <= SIMD.unsafeCallBytes `quot` width
    then do
      mv@(MVP.MVector _ _ bytes) <- MVP.unsafeNew n
      BSUnsafe.unsafeUseAsCString bs $ \src -> joinArray (castPtr src) bytes n
      VP.unsafeFreeze mv
    else do
      bytes <- newPinnedByteArray (width * n)
      BSUnsafe.unsafeUseAsCString bs $ \src -> join (castPtr src) (mutableByteArrayContents bytes) n
      VP.unsafeFreeze (MVP.MVector 0 n bytes)
{-# INLINE decodeByteStreamSplitNative #-}

-- ============================================================
-- Generic per-page chunk dispatch
-- ============================================================

-- | Encoding tags used by the generic dispatch.
encDeltaBinaryPacked, encDeltaLengthByteArray, encDeltaByteArray, encByteStreamSplit, encAlp :: Int32
encDeltaBinaryPacked     = 5
encDeltaLengthByteArray  = 6
encDeltaByteArray        = 7
encByteStreamSplit       = 9
encAlp                   = 10

-- | Dispatcher type: per-page decoder produces a chunk of values
-- of the type the caller asked for.
data PerPage a = PerPage
  { ppDecodePlain :: !(Int -> ByteString -> Either String a)
    -- ^ Decode a PLAIN-encoded page body into a chunk of length n.
  , ppPrepareDictionary :: !(a -> Either String a)
    -- ^ Materialize a reusable dictionary once, before caching it.
  , ppDecodeDictionary :: !(Int -> ByteString -> a -> Either String a)
    -- ^ Decode dictionary indices and look up their values. Keeping raw
    -- indices here allows output-size preflight before index allocation.
  , ppExtended :: !(Int32 -> Int -> ByteString -> Either String a)
    -- ^ Encoding-specific decoders (delta / byte-stream-split).
    -- @ppExtended encoding numValues body@ — return Left if the
    -- encoding is genuinely unsupported for this physical type.
  , ppConcat :: !([a] -> Either String a)
  , ppEmpty :: !a
  }

-- | Walk every page in a column chunk; for each DATA_PAGE /
-- DATA_PAGE_V2, dispatch on its encoding via the supplied
-- 'PerPage' record. DICTIONARY_PAGE pages are decoded once with
-- 'ppDecodePlain' and held for reuse by RLE_DICTIONARY pages.
-- Accumulate page-sized pieces and copy once. In particular, avoid copying
-- a growing column on every page; the singleton path retains its buffer.
concatPages :: ([a] -> a) -> [a] -> a
concatPages _ [page] = page
concatPages combine pages = combine (reverse pages)
{-# INLINE concatPages #-}

concatValuePages :: PerPage a -> [a] -> Either String a
concatValuePages _ [page] = Right page
concatValuePages pp pages = ppConcat pp (reverse pages)
{-# INLINE concatValuePages #-}

-- A chunk may contain one dictionary, before all data pages. Check before
-- decompressing a dictionary body, including in the legacy chunk readers.
checkDictionaryPagePosition :: Int -> PageHeader -> Either String ()
checkDictionaryPagePosition offset header = case phType header of
  PtDictionaryPage _ | offset /= 0 -> Left "Parquet.Read: dictionary must be the first and only dictionary page in a column"
  _ -> Right ()
{-# INLINE checkDictionaryPagePosition #-}

genericReadColumnChunk
  :: PerPage a
  -> Compression
  -> ByteString
  -> Either String a
genericReadColumnChunk pp codec chunk0 = go 0 Nothing []
  where
    go !off !mDict !acc
      | off >= BS.length chunk0 = concatValuePages pp acc
      | otherwise = do
          (hdr, afterHdr) <- readPageHeaderCheckedAt chunk0 off
          checkDictionaryPagePosition off hdr
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
                      dict <- ppDecodePlain pp nDict raw >>= ppPrepareDictionary pp
                      go nextOff (Just dict) acc
                PtDataPage dph -> do
                  raw <- decompressPageData codec
                           (phUncompressedPageSize hdr) compBody
                  let !n = fromIntegral (dphNumValues dph) :: Int
                  pageVec <- decodeDataPage pp mDict (dphEncoding dph) n raw
                  go nextOff mDict (pageVec : acc)
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
                      values <- decompressV2Values codec hdr dph2 valuesSection
                      let !n = fromIntegral (dph2NumValues dph2) :: Int
                      pageVec <-
                        decodeDataPage pp mDict (dph2Encoding dph2) n values
                      go nextOff mDict (pageVec : acc)
                _ -> Left "Parquet.Read: expected DATA_PAGE / DATA_PAGE_V2 / DICTIONARY_PAGE"

    decodeDataPage pp' mDict !enc !n !raw
      | enc == encPlain = ppDecodePlain pp' n raw
      | isDictionaryEncoding enc = case mDict of
          Nothing ->
            Left "Parquet.Read: RLE_DICTIONARY page before dictionary page"
          Just dict -> ppDecodeDictionary pp' n raw dict
      | otherwise = ppExtended pp' enc n raw

-- ============================================================
-- Per-physical-type dispatchers
-- ============================================================

dispatchInt32 :: PerPage (VP.Vector Int32)
dispatchInt32 = PerPage
  { ppDecodePlain  = decodePlainInt32
  , ppPrepareDictionary = Right
  , ppDecodeDictionary = \n raw dict -> decodeDictionaryIndices n raw >>= dictLookupVP dict
  , ppExtended = \enc n raw ->
      if enc == encDeltaBinaryPacked
        then decodeDeltaBinaryPackedInt32 n raw
        else if enc == encByteStreamSplit
          then decodeByteStreamSplitRaw 4 n raw >>= decodePlainInt32 n
        else Left $ unsupportedEncoding "INT32" enc
  , ppConcat = Right . VP.concat
  , ppEmpty = VP.empty
  }

dispatchInt64 :: PerPage (VP.Vector Int64)
dispatchInt64 = PerPage
  { ppDecodePlain  = decodePlainInt64
  , ppPrepareDictionary = Right
  , ppDecodeDictionary = \n raw dict -> decodeDictionaryIndices n raw >>= dictLookupVP dict
  , ppExtended = \enc n raw ->
      if enc == encDeltaBinaryPacked
        then decodeDeltaBinaryPackedInt64 n raw
        else if enc == encByteStreamSplit
          then decodeByteStreamSplitRaw 8 n raw >>= decodePlainInt64 n
        else Left $ unsupportedEncoding "INT64" enc
  , ppConcat = Right . VP.concat
  , ppEmpty = VP.empty
  }

dispatchFloat :: PerPage (VP.Vector Float)
dispatchFloat = PerPage
  { ppDecodePlain = decodePlainFloat
  , ppPrepareDictionary = Right
  , ppDecodeDictionary = \n raw dict -> decodeDictionaryIndices n raw >>= dictLookupVP dict
  , ppExtended = \enc n raw ->
      if enc == encByteStreamSplit
        then decodeByteStreamSplitFloat n raw
        else if enc == encAlp
          then decodeAlpFloat n raw
        else Left $ unsupportedEncoding "FLOAT" enc
  , ppConcat = Right . VP.concat
  , ppEmpty = VP.empty
  }

dispatchDouble :: PerPage (VP.Vector Double)
dispatchDouble = PerPage
  { ppDecodePlain = decodePlainDouble
  , ppPrepareDictionary = Right
  , ppDecodeDictionary = \n raw dict -> decodeDictionaryIndices n raw >>= dictLookupVP dict
  , ppExtended = \enc n raw ->
      if enc == encByteStreamSplit
        then decodeByteStreamSplitDouble n raw
        else if enc == encAlp
          then decodeAlpDouble n raw
        else Left $ unsupportedEncoding "DOUBLE" enc
  , ppConcat = Right . VP.concat
  , ppEmpty = VP.empty
  }

dispatchBool :: PerPage (V.Vector Bool)
dispatchBool = PerPage
  { ppDecodePlain = decodePlainBool
  , ppPrepareDictionary = Right
  , ppDecodeDictionary = \n raw dict -> decodeDictionaryIndices n raw >>= dictLookupV dict
  , ppExtended = \enc n raw ->
      if enc == encRle
        -- BOOLEAN with RLE encoding: the values section is a
        -- length-prefixed hybrid RLE block (4-byte LE length
        -- prefix, then bit-packed/RLE 1-bit values). Common
        -- shape for both DATA_PAGE_V2 BOOLEAN columns and any
        -- writer that picks RLE explicitly for BOOLEAN.
        then do
          bits <- decodeHybridRleBoolean n raw
          Right $! unpackBitsLsbUnsafe n bits
        else Left $ unsupportedEncoding "BOOLEAN" enc
  , ppConcat = Right . V.concat
  , ppEmpty = V.empty
  }


-- | Checked, LSB-first BOOLEAN values. The constructor stays private;
-- unused high bits are not values, and counts need not be byte-aligned.
data BoolBuffers = BoolBuffers
  { boolCount :: {-# UNPACK #-} !Int
  , boolData :: !ByteString
  }


data BoolPiece = BoolPiece {-# UNPACK #-} !Int !BoolSource


data BoolSource = BoolPlain !ByteString | BoolJoined ![BoolPiece]


boolPieceCount :: BoolPiece -> Int
boolPieceCount (BoolPiece count _) = count


boolByteCount :: Int -> Int
boolByteCount n = let (!whole, !tailBits) = n `quotRem` 8 in whole + if tailBits == 0 then 0 else 1
{-# INLINE boolByteCount #-}


decodePlainBoolPiece :: Int -> ByteString -> Either String BoolPiece
decodePlainBoolPiece count raw
  | count < 0 = Left "Parquet.Read: PLAIN BOOLEAN negative value count"
  | boolByteCount count > BS.length raw = Left "Parquet.Read: PLAIN BOOLEAN buffer too small"
  | otherwise = Right (BoolPiece count (BoolPlain (BS.take (boolByteCount count) raw)))


concatBoolPieces :: [BoolPiece] -> Either String BoolPiece
concatBoolPieces pieces = do
  count <- total 0 pieces
  Right (BoolPiece count (BoolJoined pieces))
  where
    total !count [] = Right count
    total !count (piece : rest)
      | boolPieceCount piece > maxBound - count = Left "Parquet.Read: BOOLEAN column exceeds address space"
      | otherwise = total (count + boolPieceCount piece) rest


materializeBool :: BoolPiece -> BoolBuffers
materializeBool (BoolPiece count source) = BoolBuffers count $ case source of
  BoolPlain bytes -> bytes
  BoolJoined pieces -> unsafePerformIO $ BSI.create (boolByteCount count) $ \target -> do
    fillBytes target 0 (boolByteCount count)
    let fill !offset (BoolPiece n current) = case current of
          BoolPlain bytes -> BSUnsafe.unsafeUseAsCString bytes $ \ptr -> copyBits target offset (castPtr ptr) 0 n
          BoolJoined parts -> each offset parts
        each !_ [] = pure ()
        each !offset (piece : rest) = fill offset piece >> each (offset + boolPieceCount piece) rest
    each 0 pieces
{-# NOINLINE materializeBool #-}


decodeBoolDictionary :: Int -> ByteString -> BoolPiece -> Either String BoolPiece
decodeBoolDictionary count raw dictionary = do
  indices <- decodeDictionaryIndices count raw
  if VP.any (\key -> key < 0 || fromIntegral key >= boolPieceCount dictionary) indices
    then Left "Parquet.Read: BOOLEAN dictionary index out of range"
    else
      let values = boolData (materializeBool dictionary)
          packed = packBoolIndices values indices
      in Right (BoolPiece count (BoolPlain packed))


packBoolIndices :: ByteString -> VP.Vector Int32 -> ByteString
packBoolIndices values indices = unsafePerformIO $ BSI.create (boolByteCount count) $ \target -> do
  let byte !row !lane !acc
        | row == count || lane == 8 = acc
        | otherwise =
            let key = fromIntegral (VP.unsafeIndex indices row)
                (!byteIndex, !bitIndex) = key `quotRem` 8
                bit = (BSUnsafe.unsafeIndex values byteIndex `shiftR` bitIndex) .&. 1
            in byte (row + 1) (lane + 1) (acc .|. (bit `shiftL` lane))
      loop !row
        | row == count = pure ()
        | otherwise = do
            let !value = byte row 0 (0 :: Word8)
            pokeByteOff target (row `quot` 8) value
            loop (row + min 8 (count - row))
  loop 0
  where
    count = VP.length indices
{-# NOINLINE packBoolIndices #-}


dispatchBoolPieces :: PerPage BoolPiece
dispatchBoolPieces = PerPage
  { ppDecodePlain = decodePlainBoolPiece
  , ppPrepareDictionary = Right
  , ppDecodeDictionary = decodeBoolDictionary
  , ppExtended = \encoding count raw ->
      if encoding == encRle
        then BoolPiece count . BoolPlain <$> decodeHybridRleBoolean count raw
        else Left (unsupportedEncoding "BOOLEAN" encoding)
  , ppConcat = concatBoolPieces
  , ppEmpty = BoolPiece 0 (BoolPlain BS.empty)
  }


-- | Keep PLAIN and RLE BOOLEAN pages packed, joining arbitrary bit tails.
readBoolBuffersColumnChunk :: Compression -> ByteString -> Either String BoolBuffers
readBoolBuffersColumnChunk codec chunk = materializeBool <$> genericReadColumnChunk dispatchBoolPieces codec chunk


readBoolBuffersLevels :: Compression -> Int -> Int -> ByteString -> Either String (ColumnLevels BoolBuffers)
readBoolBuffersLevels codec maxRep maxDef chunk = materializeBoolLevels <$> readLevelsColumnChunk dispatchBoolPieces codec maxRep maxDef chunk


readBoolBuffersSelectedValues :: Compression -> ByteString -> PageSelection -> Either String BoolBuffers
readBoolBuffersSelectedValues codec file selection = materializeBool <$> readSelectedValues dispatchBoolPieces codec file selection


readBoolBuffersSelectedLevels :: Compression -> Int -> Int -> ByteString -> PageSelection -> Either String (ColumnLevels BoolBuffers)
readBoolBuffersSelectedLevels codec maxRep maxDef file selection = materializeBoolLevels <$> readSelectedLevels dispatchBoolPieces codec maxRep maxDef file selection


materializeBoolLevels :: ColumnLevels BoolPiece -> ColumnLevels BoolBuffers
materializeBoolLevels levels = ColumnLevels (clRepetition levels) (clDefinition levels) (materializeBool (clValues levels))

{- | Checked dense scalars in native byte order. The private constructor
guarantees whole 4- or 8-byte values; Arrow aligns the final buffer if needed.
-}
newtype ScalarBuffers = ScalarBuffers {scalarData :: ByteString}


data ScalarPiece = ScalarPiece
  { scalarCount :: {-# UNPACK #-} !Int
  , scalarSource :: !ScalarSource
  }


data ScalarSource
  = ScalarPlain !ByteString
  | ScalarDictionary !ByteString !(VP.Vector Int32)
  | ScalarJoined ![ScalarPiece]


-- Keep source pages until the entire output size is checked. PLAIN pages
-- need no intermediate value vector; dictionary gathers write the final
-- column directly rather than materializing a vector for each data page.
scalarDispatcher :: ParquetType -> Either String (PerPage ScalarPiece)
scalarDispatcher physical = case physical of
  PTInt32 -> Right (dispatchScalar 4 dispatchInt32)
  PTInt64 -> Right (dispatchScalar 8 dispatchInt64)
  PTFloat -> Right (dispatchScalar 4 dispatchFloat)
  PTDouble -> Right (dispatchScalar 8 dispatchDouble)
  _ -> Left "Parquet.Read: scalar buffers require INT32, INT64, FLOAT or DOUBLE"


dispatchScalar :: Int -> PerPage (VP.Vector a) -> PerPage ScalarPiece
dispatchScalar width legacy =
  PerPage
    { ppDecodePlain = \n raw ->
        if n < 0 || n > BS.length raw `quot` width
          then Left "Parquet.Read: invalid PLAIN scalar count"
          else
            if targetByteOrder == LittleEndian
              then Right (ScalarPiece n (ScalarPlain (BS.take (n * width) raw)))
              else scalarFromVector width <$> ppDecodePlain legacy n raw
    , ppPrepareDictionary = Right . prepareScalarDictionary width
    , ppDecodeDictionary = \n raw dict -> do
        if n < 0 || n > maxBound `quot` width
          then Left "Parquet.Read: scalar dictionary exceeds address space"
          else Right ()
        indices <- decodeDictionaryIndices n raw
        if VP.any (\key -> key < 0 || fromIntegral key >= scalarCount dict) indices
          then Left "Parquet.Read: scalar dictionary index out of bounds"
          else Right (ScalarPiece n (ScalarDictionary (scalarData (materializeScalar width dict)) indices))
    , ppExtended = \encoding n raw -> scalarFromVector width <$> ppExtended legacy encoding n raw
    , ppConcat = concatScalars width
    , ppEmpty = ScalarPiece 0 (ScalarPlain BS.empty)
    }


scalarFromVector :: Int -> VP.Vector a -> ScalarPiece
scalarFromVector width (VP.Vector offset count bytes) =
  ScalarPiece
    count
    ( ScalarPlain
        ( unsafePerformIO $ do
            result <- mallocPlainForeignPtrAlignedBytes (count * width) width
            withForeignPtr result $ \target -> copyByteArrayToAddr target bytes (offset * width) (count * width)
            pure (BSI.PS result 0 (count * width))
        )
    )
{-# NOINLINE scalarFromVector #-}


-- A dictionary is reused by many pages. Align it once, if necessary, so
-- every gather can load one native word safely, without memcpy per value.
prepareScalarDictionary :: Int -> ScalarPiece -> ScalarPiece
prepareScalarDictionary width piece =
  ScalarPiece (scalarCount piece) (ScalarPlain aligned)
  where
    bytes = scalarData (materializeScalar width piece)
    aligned = unsafePerformIO $ BSUnsafe.unsafeUseAsCString bytes $ \source ->
      if ptrToWordPtr source .&. fromIntegral (width - 1) == 0
        then pure bytes
        else do
          result <- mallocPlainForeignPtrAlignedBytes (BS.length bytes) width
          withForeignPtr result $ \target -> copyBytes target (castPtr source) (BS.length bytes)
          pure (BSI.PS result 0 (BS.length bytes))
{-# NOINLINE prepareScalarDictionary #-}


concatScalars :: Int -> [ScalarPiece] -> Either String ScalarPiece
concatScalars width pieces = do
  count <- go 0 pieces
  Right (ScalarPiece count (ScalarJoined pieces))
  where
    go !count [] = Right count
    go !count (piece : rest)
      | scalarCount piece > maxBound `quot` width - count = Left "Parquet.Read: scalar column exceeds address space"
      | otherwise = go (count + scalarCount piece) rest


materializeScalar :: Int -> ScalarPiece -> ScalarBuffers
materializeScalar width piece = ScalarBuffers $ case scalarSource piece of
  ScalarPlain bytes -> bytes
  _ -> unsafePerformIO $ do
    result <- mallocPlainForeignPtrAlignedBytes (scalarCount piece * width) width
    withForeignPtr result $ \target -> fill target 0 piece
    pure (BSI.PS result 0 (scalarCount piece * width))
  where
    fill target !offset current = case scalarSource current of
      ScalarPlain bytes -> BSUnsafe.unsafeUseAsCString bytes $ \source ->
        copyBytes (target `plusPtr` offset) (castPtr source) (BS.length bytes)
      ScalarDictionary bytes indices -> BSUnsafe.unsafeUseAsCString bytes $ \source -> do
        -- Both the prepared dictionary and final buffer are width-aligned.
        let gather4 !i
              | i == VP.length indices = pure ()
              | otherwise = do
                  let !key = fromIntegral (VP.unsafeIndex indices i)
                  value <- peekByteOff source (key * 4) :: IO Word32
                  pokeByteOff target (offset + i * 4) value
                  gather4 (i + 1)
            gather8 !i
              | i == VP.length indices = pure ()
              | otherwise = do
                  let !key = fromIntegral (VP.unsafeIndex indices i)
                  value <- peekByteOff source (key * 8) :: IO Word64
                  pokeByteOff target (offset + i * 8) value
                  gather8 (i + 1)
        if width == 4 then gather4 0 else gather8 0
      ScalarJoined pieces -> do
        let next !_ [] = pure ()
            next !position (part : rest) = do
              fill target position part
              next (position + scalarCount part * width) rest
        next offset pieces
{-# NOINLINE materializeScalar #-}


scalarWidth :: ParquetType -> Int
scalarWidth PTInt32 = 4
scalarWidth PTFloat = 4
scalarWidth _ = 8


-- | Read supported physical scalars without intermediate per-page vectors.
readScalarBuffersColumnChunk :: ParquetType -> Compression -> ByteString -> Either String ScalarBuffers
readScalarBuffersColumnChunk physical codec chunk = do
  dispatcher <- scalarDispatcher physical
  materializeScalar (scalarWidth physical) <$> genericReadColumnChunk dispatcher codec chunk


readScalarBuffersLevels :: ParquetType -> Compression -> Int -> Int -> ByteString -> Either String (ColumnLevels ScalarBuffers)
readScalarBuffersLevels physical codec maxRep maxDef chunk = do
  dispatcher <- scalarDispatcher physical
  materializeScalarLevels (scalarWidth physical) <$> readLevelsColumnChunk dispatcher codec maxRep maxDef chunk


readScalarBuffersSelectedValues :: ParquetType -> Compression -> ByteString -> PageSelection -> Either String ScalarBuffers
readScalarBuffersSelectedValues physical codec file selection = do
  dispatcher <- scalarDispatcher physical
  materializeScalar (scalarWidth physical) <$> readSelectedValues dispatcher codec file selection


readScalarBuffersSelectedLevels :: ParquetType -> Compression -> Int -> Int -> ByteString -> PageSelection -> Either String (ColumnLevels ScalarBuffers)
readScalarBuffersSelectedLevels physical codec maxRep maxDef file selection = do
  dispatcher <- scalarDispatcher physical
  materializeScalarLevels (scalarWidth physical) <$> readSelectedLevels dispatcher codec maxRep maxDef file selection


materializeScalarLevels :: Int -> ColumnLevels ScalarPiece -> ColumnLevels ScalarBuffers
materializeScalarLevels width levels = ColumnLevels (clRepetition levels) (clDefinition levels) (materializeScalar width (clValues levels))

dispatchByteArray :: PerPage (V.Vector ByteString)
dispatchByteArray = PerPage
  { ppDecodePlain = decodePlainByteArray
  , ppPrepareDictionary = Right
  , ppDecodeDictionary = \n raw dict -> decodeDictionaryIndices n raw >>= dictLookupV dict
  , ppExtended = \enc n raw ->
      if enc == encDeltaLengthByteArray
        then decodeDeltaLengthByteArray n raw
        else if enc == encDeltaByteArray
          then decodeDeltaByteArray n raw
          else Left $ unsupportedEncoding "BYTE_ARRAY" enc
  , ppConcat = Right . V.concat
  , ppEmpty = V.empty
  }

dispatchBinaryPiecesFor :: forall o. (Integral o, Bounded o, Storable o) => PerPage (BinaryPiece o)
dispatchBinaryPiecesFor =
  PerPage
    { ppDecodePlain = decodePlainBinaryPiece
    , ppPrepareDictionary = prepareBinaryDictionary
    , ppDecodeDictionary = decodeBinaryDictionary
    , ppExtended = \enc n raw ->
        if enc == encDeltaLengthByteArray
          then decodeDeltaLengthByteArray n raw >>= packBinaryPiece
          else
            if enc == encDeltaByteArray
              then decodeDeltaByteArray n raw >>= packBinaryPiece
              else Left $ unsupportedEncoding "BYTE_ARRAY" enc
    , ppConcat = concatBinaryPieces
    , ppEmpty = emptyBinaryPiece
    }


{- | BYTE_ARRAY pages decoded directly to contiguous offsets/data buffers.
Dictionaries and both delta encodings are supported, as for the vector API.
-}
readBinaryBuffersColumnChunk :: Compression -> ByteString -> Either String BinaryBuffers
readBinaryBuffersColumnChunk codec chunk = materializeBinaryPiece <$> genericReadColumnChunk dispatchBinaryPieces codec chunk


readBinaryBuffersLevels :: Compression -> Int -> Int -> ByteString -> Either String (ColumnLevels BinaryBuffers)
readBinaryBuffersLevels codec maxRep maxDef chunk = materializeBinaryLevels <$> readLevelsColumnChunk dispatchBinaryPieces codec maxRep maxDef chunk


readBinaryBuffersSelectedValues :: Compression -> ByteString -> PageSelection -> Either String BinaryBuffers
readBinaryBuffersSelectedValues codec file selection = materializeBinaryPiece <$> readSelectedValues dispatchBinaryPieces codec file selection


readBinaryBuffersSelectedLevels :: Compression -> Int -> Int -> ByteString -> PageSelection -> Either String (ColumnLevels BinaryBuffers)
readBinaryBuffersSelectedLevels codec maxRep maxDef file selection = materializeBinaryLevels <$> readSelectedLevels dispatchBinaryPieces codec maxRep maxDef file selection


materializeBinaryLevels :: forall o. (Integral o, Bounded o, Storable o) => ColumnLevels (BinaryPiece o) -> ColumnLevels (BinaryBuffersOf o)
materializeBinaryLevels levels = ColumnLevels (clRepetition levels) (clDefinition levels) (materializeBinaryPiece (clValues levels))


dispatchBinaryPieces :: PerPage (BinaryPiece Int32)
dispatchBinaryPieces = dispatchBinaryPiecesFor @Int32


largeBinaryPieces :: PerPage (BinaryPiece Int64)
largeBinaryPieces = dispatchBinaryPiecesFor @Int64


-- | Decode PLAIN values into64-bit offsets without narrowing their sum.
decodePlainLargeBinaryBuffers :: Int -> ByteString -> Either String LargeBinaryBuffers
decodePlainLargeBinaryBuffers n raw = materializeBinaryPiece <$> decodePlainBinaryPiece @Int64 n raw


readLargeBinaryBuffersColumnChunk :: Compression -> ByteString -> Either String LargeBinaryBuffers
readLargeBinaryBuffersColumnChunk codec chunk = materializeBinaryPiece <$> genericReadColumnChunk largeBinaryPieces codec chunk


readLargeBinaryBuffersLevels :: Compression -> Int -> Int -> ByteString -> Either String (ColumnLevels LargeBinaryBuffers)
readLargeBinaryBuffersLevels codec maxRep maxDef chunk = materializeBinaryLevels <$> readLevelsColumnChunk largeBinaryPieces codec maxRep maxDef chunk


readLargeBinaryBuffersSelectedValues :: Compression -> ByteString -> PageSelection -> Either String LargeBinaryBuffers
readLargeBinaryBuffersSelectedValues codec file selection = materializeBinaryPiece <$> readSelectedValues largeBinaryPieces codec file selection


readLargeBinaryBuffersSelectedLevels :: Compression -> Int -> Int -> ByteString -> PageSelection -> Either String (ColumnLevels LargeBinaryBuffers)
readLargeBinaryBuffersSelectedLevels codec maxRep maxDef file selection = materializeBinaryLevels <$> readSelectedLevels largeBinaryPieces codec maxRep maxDef file selection


{-# SPECIALIZE dispatchBinaryPiecesFor :: PerPage (BinaryPiece Int32) #-}
{-# SPECIALIZE dispatchBinaryPiecesFor :: PerPage (BinaryPiece Int64) #-}


{-# SPECIALIZE materializeBinaryLevels :: ColumnLevels (BinaryPiece Int32) -> ColumnLevels BinaryBuffers #-}
{-# SPECIALIZE materializeBinaryLevels :: ColumnLevels (BinaryPiece Int64) -> ColumnLevels LargeBinaryBuffers #-}


-- Fixed byte arrays share the dictionary/delta page machinery, but PLAIN
-- values have no individual length prefix. Width always comes from schema.
dispatchFixedBytes :: Int -> PerPage (V.Vector ByteString)
dispatchFixedBytes width = PerPage
  { ppDecodePlain = decodePlainFixedLenByteArray width
  , ppPrepareDictionary = Right
  , ppDecodeDictionary = \n raw dict -> decodeDictionaryIndices n raw >>= dictLookupV dict
  , ppExtended = \encoding count bytes ->
      if encoding == encDeltaByteArray
        then do
          values <- decodeDeltaByteArray count bytes
          if V.all ((== width) . BS.length) values
            then Right values
            else Left "Parquet.Read: delta fixed byte array has an invalid width"
        else if encoding == encByteStreamSplit
          then decodeByteStreamSplitRaw width count bytes >>= decodePlainFixedLenByteArray width count
          else Left (unsupportedEncoding "FIXED_LEN_BYTE_ARRAY" encoding)
  , ppConcat = Right . V.concat
  , ppEmpty = V.empty
  }


-- Undo byte-stream splitting in one buffer, with no division or allocation
-- inside the per-byte loop. Bounds are established before any unsafe reads.
decodeByteStreamSplitRaw :: Int -> Int -> ByteString -> Either String ByteString
decodeByteStreamSplitRaw width count bytes
  | width <= 0 || count < 0 || count > BS.length bytes `quot` width =
      Left "Parquet.Read: invalid BYTE_STREAM_SPLIT size"
  | otherwise = Right $ Direct.directEncode (width * count) $ \ptr ->
      let row !i !offset
            | i == count = pure offset
            | otherwise = lane i 0 offset
          lane !i !j !offset
            | j == width = row (i + 1) offset
            | otherwise = do
                next <- Direct.dWord8 ptr offset (BSUnsafe.unsafeIndex bytes (j * count + i))
                lane i (j + 1) next
      in row 0


readGenericFixedLenByteArrayColumnChunk
  :: Int -> Compression -> ByteString -> Either String (V.Vector ByteString)
readGenericFixedLenByteArrayColumnChunk width = genericReadColumnChunk (dispatchFixedBytes width)


readGenericFixedLenByteArrayOptionalColumnChunk
  :: Int -> Compression -> Int -> Int -> ByteString
  -> Either String (V.Vector (Maybe ByteString))
readGenericFixedLenByteArrayOptionalColumnChunk width =
  readGenericOptionalColumnChunk (dispatchFixedBytes width) id


dictLookupVP
  :: VP.Prim a
  => VP.Vector a -> VP.Vector Int32 -> Either String (VP.Vector a)
dictLookupVP dict indices =
  let !nD = VP.length dict
      !ok = VP.foldl' (\a k -> a && let !j = fromIntegral k :: Int
                                    in j >= 0 && j < nD) True indices
  in if not ok
       then Left "Parquet.Read: dictionary index out of range"
       else Right $! VP.map (\k -> dict VP.! fromIntegral k) indices

dictLookupV
  :: V.Vector a -> VP.Vector Int32 -> Either String (V.Vector a)
dictLookupV dict indices =
  let !nD = V.length dict
      !ok = VP.foldl' (\a k -> a && let !j = fromIntegral k :: Int
                                    in j >= 0 && j < nD) True indices
  in if not ok
       then Left "Parquet.Read: dictionary index out of range"
       -- A boxed generate stores a deferred lookup per row. Resolve each
       -- index before writing so repeated strings share the dictionary's
       -- ByteString directly instead of allocating a closure per value.
       else Right $! runST (do
         let !n = VP.length indices
         out <- VM.unsafeNew n
         let fill !i
               | i == n = V.unsafeFreeze out
               | otherwise = do
                   let !value = V.unsafeIndex dict (fromIntegral (VP.unsafeIndex indices i))
                   VM.unsafeWrite out i value
                   fill (i + 1)
         fill 0)

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

readGenericInt32ColumnChunk :: Compression -> ByteString -> Either String (VP.Vector Int32)
readGenericInt32ColumnChunk = genericReadColumnChunk dispatchInt32

readGenericInt64ColumnChunk :: Compression -> ByteString -> Either String (VP.Vector Int64)
readGenericInt64ColumnChunk = genericReadColumnChunk dispatchInt64

readGenericFloatColumnChunk :: Compression -> ByteString -> Either String (VP.Vector Float)
readGenericFloatColumnChunk = genericReadColumnChunk dispatchFloat

readGenericDoubleColumnChunk :: Compression -> ByteString -> Either String (VP.Vector Double)
readGenericDoubleColumnChunk = genericReadColumnChunk dispatchDouble

readGenericBoolColumnChunk :: Compression -> ByteString -> Either String (V.Vector Bool)
readGenericBoolColumnChunk = genericReadColumnChunk dispatchBool

readGenericByteArrayColumnChunk :: Compression -> ByteString -> Either String (V.Vector ByteString)
readGenericByteArrayColumnChunk = genericReadColumnChunk dispatchByteArray

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
  readGenericOptionalColumnChunk dispatchInt32 vpToBoxed

readGenericInt64OptionalColumnChunk
  :: Compression -> Int -> Int -> ByteString
  -> Either String (V.Vector (Maybe Int64))
readGenericInt64OptionalColumnChunk =
  readGenericOptionalColumnChunk dispatchInt64 vpToBoxed

readGenericFloatOptionalColumnChunk
  :: Compression -> Int -> Int -> ByteString
  -> Either String (V.Vector (Maybe Float))
readGenericFloatOptionalColumnChunk =
  readGenericOptionalColumnChunk dispatchFloat vpToBoxed

readGenericDoubleOptionalColumnChunk
  :: Compression -> Int -> Int -> ByteString
  -> Either String (V.Vector (Maybe Double))
readGenericDoubleOptionalColumnChunk =
  readGenericOptionalColumnChunk dispatchDouble vpToBoxed

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

-- ============================================================
-- Public generic readers (repeated)
-- ============================================================
--
-- For columns where the schema path passes through a repeated
-- group (a Parquet list or map). 'readGenericXxxRepeatedColumnChunk'
-- returns one outer element per top-level row, each holding the
-- inner sequence of @Maybe a@s (one per @max_repetition_level@
-- boundary in the rep stream).
--
-- The values payload is decoded in any encoding the underlying
-- 'PerPage' supports (PLAIN, RLE_DICTIONARY, …); the rep + def
-- streams come from the standard data-page level layout.

readGenericByteArrayRepeatedColumnChunk
  :: Compression -> Int -> Int -> ByteString
  -> Either String (V.Vector (V.Vector (Maybe ByteString)))
readGenericByteArrayRepeatedColumnChunk codec maxRep maxDef =
  -- Default: assume the list element is required, so any
  -- def < maxDef on a rep=0 boundary collapses to "empty/absent
  -- list". Callers that need to surface element-level nulls
  -- inside a non-empty list (a Parquet 'optional' element)
  -- should pass an explicit 'parentEmptyDef' via
  -- 'readGenericByteArrayRepeatedColumnChunkWith'.
  readGenericRepeatedColumnChunkWith dispatchByteArray id
    codec maxRep maxDef (maxDef - 1)

-- | Like 'readGenericByteArrayRepeatedColumnChunk' but the
-- caller picks @parent_empty_def@ explicitly — typically
-- @maxDef - 2@ for an optional-element list / map-of-optional-
-- value.
readGenericByteArrayRepeatedColumnChunkWith
  :: Compression -> Int -> Int -> Int -> ByteString
  -> Either String (V.Vector (V.Vector (Maybe ByteString)))
readGenericByteArrayRepeatedColumnChunkWith =
  readGenericRepeatedColumnChunkWith dispatchByteArray id

readGenericInt32RepeatedColumnChunk
  :: Compression -> Int -> Int -> ByteString
  -> Either String (V.Vector (V.Vector (Maybe Int32)))
readGenericInt32RepeatedColumnChunk codec maxRep maxDef =
  readGenericRepeatedColumnChunkWith dispatchInt32 vpToBoxed
    codec maxRep maxDef (maxDef - 1)

readGenericInt64RepeatedColumnChunk
  :: Compression -> Int -> Int -> ByteString
  -> Either String (V.Vector (V.Vector (Maybe Int64)))
readGenericInt64RepeatedColumnChunk codec maxRep maxDef =
  readGenericRepeatedColumnChunkWith dispatchInt64 vpToBoxed
    codec maxRep maxDef (maxDef - 1)

-- | Walk every page in a column chunk that has @max_rep > 0@,
-- accumulate (rep level, def level, defined value) triples, and
-- group consecutive entries by the rep-level boundaries
-- (@rep == 0@ marks the start of a new top-level row).
--
-- Per-element interpretation:
--
--   * @def == maxDef@                         → @Just <value>@
--   * @parentEmptyDef < def < maxDef@         → @Nothing@ (a
--     null element inside a present list / map; only possible
--     when the list element itself is Parquet-optional, hence
--     the explicit @parentEmptyDef@ split here)
--   * @def <= parentEmptyDef@                 → no entry (the
--     parent list / map group is empty or absent on this row)
--
-- The @rep == 0@ boundary marks the start of a new row;
-- empty-list rows still produce an empty inner @V.Vector@ so
-- the output's outer length always equals the number of
-- top-level rows in the column.
--
-- For required-element lists pass @parentEmptyDef = maxDef - 1@
-- (which is what the public 'readGeneric*RepeatedColumnChunk'
-- helpers do).
-- | Physical levels and only the defined values, in file order. Retaining
-- the levels lets a nested reader distinguish absent lists, empty lists and
-- null elements, including where a row continues onto another page.
data ColumnLevels a = ColumnLevels
  { clRepetition :: !(VP.Vector Int32)
  , clDefinition :: !(VP.Vector Int32)
  , clValues :: !a
  }

readInt32Levels :: Compression -> Int -> Int -> ByteString -> Either String (ColumnLevels (VP.Vector Int32))
readInt32Levels = readLevelsColumnChunk dispatchInt32
readInt64Levels :: Compression -> Int -> Int -> ByteString -> Either String (ColumnLevels (VP.Vector Int64))
readInt64Levels = readLevelsColumnChunk dispatchInt64
readFloatLevels :: Compression -> Int -> Int -> ByteString -> Either String (ColumnLevels (VP.Vector Float))
readFloatLevels = readLevelsColumnChunk dispatchFloat
readDoubleLevels :: Compression -> Int -> Int -> ByteString -> Either String (ColumnLevels (VP.Vector Double))
readDoubleLevels = readLevelsColumnChunk dispatchDouble
readBoolLevels :: Compression -> Int -> Int -> ByteString -> Either String (ColumnLevels (V.Vector Bool))
readBoolLevels = readLevelsColumnChunk dispatchBool
readByteArrayLevels :: Compression -> Int -> Int -> ByteString -> Either String (ColumnLevels (V.Vector ByteString))
readByteArrayLevels = readLevelsColumnChunk dispatchByteArray
readFixedLevels :: Int -> Compression -> Int -> Int -> ByteString -> Either String (ColumnLevels (V.Vector ByteString))
readFixedLevels width = readLevelsColumnChunk (dispatchFixedBytes width)

-- | Data-page locations and their keep mask. Supplying the dictionary offset
-- avoids a footer lookup; the row count enables last-page row validation.
-- Skipped pages are not read or checksummed, as required for selective IO.
data PageSelection = PageSelection
  { selectionLocations :: !(V.Vector PageLocation)
  , selectionKeep :: !(V.Vector Bool)
  , selectionDictionaryOffset :: !(Maybe Int64)
  , selectionRowCount :: !(Maybe Int64)
  }

readInt32SelectedLevels :: Compression -> Int -> Int -> ByteString -> PageSelection -> Either String (ColumnLevels (VP.Vector Int32))
readInt32SelectedLevels = readSelectedLevels dispatchInt32
readInt64SelectedLevels :: Compression -> Int -> Int -> ByteString -> PageSelection -> Either String (ColumnLevels (VP.Vector Int64))
readInt64SelectedLevels = readSelectedLevels dispatchInt64
readFloatSelectedLevels :: Compression -> Int -> Int -> ByteString -> PageSelection -> Either String (ColumnLevels (VP.Vector Float))
readFloatSelectedLevels = readSelectedLevels dispatchFloat
readDoubleSelectedLevels :: Compression -> Int -> Int -> ByteString -> PageSelection -> Either String (ColumnLevels (VP.Vector Double))
readDoubleSelectedLevels = readSelectedLevels dispatchDouble
readBoolSelectedLevels :: Compression -> Int -> Int -> ByteString -> PageSelection -> Either String (ColumnLevels (V.Vector Bool))
readBoolSelectedLevels = readSelectedLevels dispatchBool
readByteArraySelectedLevels :: Compression -> Int -> Int -> ByteString -> PageSelection -> Either String (ColumnLevels (V.Vector ByteString))
readByteArraySelectedLevels = readSelectedLevels dispatchByteArray
readFixedSelectedLevels :: Int -> Compression -> Int -> Int -> ByteString -> PageSelection -> Either String (ColumnLevels (V.Vector ByteString))
readFixedSelectedLevels width = readSelectedLevels (dispatchFixedBytes width)

-- | Dense required columns need no materialized zero-level vectors.
readInt32SelectedValues :: Compression -> ByteString -> PageSelection -> Either String (VP.Vector Int32)
readInt32SelectedValues = readSelectedValues dispatchInt32
readInt64SelectedValues :: Compression -> ByteString -> PageSelection -> Either String (VP.Vector Int64)
readInt64SelectedValues = readSelectedValues dispatchInt64
readFloatSelectedValues :: Compression -> ByteString -> PageSelection -> Either String (VP.Vector Float)
readFloatSelectedValues = readSelectedValues dispatchFloat
readDoubleSelectedValues :: Compression -> ByteString -> PageSelection -> Either String (VP.Vector Double)
readDoubleSelectedValues = readSelectedValues dispatchDouble
readBoolSelectedValues :: Compression -> ByteString -> PageSelection -> Either String (V.Vector Bool)
readBoolSelectedValues = readSelectedValues dispatchBool
readByteArraySelectedValues :: Compression -> ByteString -> PageSelection -> Either String (V.Vector ByteString)
readByteArraySelectedValues = readSelectedValues dispatchByteArray
readFixedSelectedValues :: Int -> Compression -> ByteString -> PageSelection -> Either String (V.Vector ByteString)
readFixedSelectedValues width = readSelectedValues (dispatchFixedBytes width)

-- A page-sized intermediate, shared by sequential and indexed decoding.
-- Defined counts and level validation are computed in one unboxed loop.
data LevelPage = LevelPage
  { lpReps :: !(VP.Vector Int32)
  , lpDefs :: !(VP.Vector Int32)
  , lpEncoding :: {-# UNPACK #-} !Int32
  , lpDefined :: {-# UNPACK #-} !Int
  , lpRows :: {-# UNPACK #-} !Int
  , lpValues :: !ByteString
  }

data LevelCounts = LevelCounts
  { levelRows :: {-# UNPACK #-} !Int
  , levelDefined :: {-# UNPACK #-} !Int
  }

validateLevelLimits :: Int -> Int -> Either String ()
validateLevelLimits maxRep maxDef
  | maxRep < 0 || maxDef < maxRep || maxDef > fromIntegral (maxBound :: Int32) =
      Left "Parquet.Read: invalid maximum repetition/definition levels"
  | otherwise = Right ()

countLevelEvents :: Int -> Int -> VP.Vector Int32 -> VP.Vector Int32 -> Either String LevelCounts
countLevelEvents maxRep maxDef reps defs
  | VP.length reps /= VP.length defs = Left "Parquet.Read: mismatched level counts"
  | otherwise = go 0 0 0
  where
    go !i !rows !defined
      | i == VP.length defs = Right (LevelCounts rows defined)
      | otherwise =
          let !rep = VP.unsafeIndex reps i
              !def = VP.unsafeIndex defs i
          in if rep < 0 || rep > fromIntegral maxRep || def < 0 || def > fromIntegral maxDef
               then Left "Parquet.Read: invalid repetition/definition levels"
               else go (i + 1) (rows + if rep == 0 then 1 else 0)
                     (defined + if def == fromIntegral maxDef then 1 else 0)
{-# INLINE countLevelEvents #-}

readLevelDataPage :: Compression -> Int -> Int -> PageDetails -> ByteString -> Either String LevelPage
readLevelDataPage codec maxRep maxDef details compBody = case phType hdr of
  PtDataPage page -> do
    raw <- decompressPageData codec (phUncompressedPageSize hdr) compBody
    (reps, defs, values) <- parseDataPageV1LevelsWithEncoding
      (pdRepetitionEncoding details) (pdDefinitionEncoding details)
      maxRep maxDef (fromIntegral (dphNumValues page)) raw
    counts <- countLevelEvents maxRep maxDef reps defs
    Right (LevelPage reps defs (dphEncoding page) (levelDefined counts) (levelRows counts) values)
  PtDataPageV2 page -> do
    let repLength = fromIntegral (dph2RepLevelsLen page)
        defLength = fromIntegral (dph2DefLevelsLen page)
        count = fromIntegral (dph2NumValues page)
        levelsLength = repLength + defLength
    -- Some producers retain an explicit zero-width RLE stream. Its values
    -- are still all zero; the declared lengths locate the value payload.
    if levelsLength > BS.length compBody
      then Left "Parquet.Read: V2 level lengths exceed body"
      else Right ()
    reps <- decodeHybridRleUnsigned32 (levelBitWidth maxRep) count (BS.take repLength compBody)
    defs <- decodeHybridRleUnsigned32 (levelBitWidth maxDef) count (BS.take defLength (BS.drop repLength compBody))
    counts <- countLevelEvents maxRep maxDef reps defs
    if (not (VP.null reps) && VP.head reps /= 0)
        || levelRows counts /= fromIntegral (dph2NumRows page)
        || count - levelDefined counts /= fromIntegral (dph2NumNulls page)
      then Left "Parquet.Read: V2 row/null counts disagree with levels"
      else Right ()
    values <- decompressV2Values codec hdr page (BS.drop levelsLength compBody)
    Right (LevelPage reps defs (dph2Encoding page) (levelDefined counts) (levelRows counts) values)
  _ -> Left "Parquet.Read: expected a data page"
  where
    hdr = pdHeader details

-- Required data has implicit zero levels. Keep their counts without
-- allocating/filling repetition and definition vectors for every page.
readRequiredDataPage :: Compression -> PageDetails -> ByteString -> Either String LevelPage
readRequiredDataPage codec details body = case phType header of
  PtDataPage page -> do
    values <- decompressPageData codec (phUncompressedPageSize header) body
    let count = fromIntegral (dphNumValues page)
    Right (LevelPage VP.empty VP.empty (dphEncoding page) count count values)
  PtDataPageV2 page -> do
    let levelsLength = fromIntegral (dph2RepLevelsLen page) + fromIntegral (dph2DefLevelsLen page)
        count = fromIntegral (dph2NumValues page)
    if levelsLength > BS.length body || dph2NumRows page /= dph2NumValues page || dph2NumNulls page /= 0
      then Left "Parquet.Read: required V2 row/null/level counts disagree"
      else Right ()
    values <- decompressV2Values codec header page (BS.drop levelsLength body)
    Right (LevelPage VP.empty VP.empty (dph2Encoding page) count count values)
  _ -> Left "Parquet.Read: expected a required data page"
  where
    header = pdHeader details

readDictionaryValues :: PerPage vec -> Compression -> PageHeader -> ByteString -> Either String vec
readDictionaryValues pp codec header body = case phType header of
  PtDictionaryPage page
    | dictEncoding page == encPlain || dictEncoding page == encPlainDictionary -> do
        raw <- decompressPageData codec (phUncompressedPageSize header) body
        ppDecodePlain pp (fromIntegral (dictNumValues page)) raw >>= ppPrepareDictionary pp
    | otherwise -> Left "Parquet.Read: dictionary encoding must be PLAIN"
  _ -> Left "Parquet.Read: dictionary offset does not point to a dictionary page"

readLevelsColumnChunk :: forall vec. PerPage vec -> Compression -> Int -> Int -> ByteString -> Either String (ColumnLevels vec)
readLevelsColumnChunk pp codec maxRep maxDef chunk0 = do
  validateLevelLimits maxRep maxDef
  levels <- collectPages 0 Nothing [] [] []
  if not (VP.null (clRepetition levels)) && VP.head (clRepetition levels) /= 0
    then Left "Parquet.Read: first repetition level must be zero"
    else Right levels
  where
    collectPages !off !mDict !accReps !accDefs !accVals
      | off == BS.length chunk0 = ColumnLevels (concatPages VP.concat accReps) (concatPages VP.concat accDefs) <$> concatValuePages pp accVals
      | otherwise = do
          details <- readPageDetailsCheckedAt chunk0 off
          let hdr = pdHeader details
              bodyStart = pdBodyOffset details
          compSz <- maybe (Left "Parquet.Read: missing compressed_page_size") (Right . fromIntegral) (phCompressedPageSize hdr)
          let compBody = BS.take compSz (BS.drop bodyStart chunk0)
              nextOff = bodyStart + compSz
          case phType hdr of
            PtDictionaryPage _ -> do
              if off /= 0
                then Left "Parquet.Read: dictionary must be the first page in a column"
                else Right ()
              case mDict of
                Just _ -> Left "Parquet.Read: multiple dictionary pages in one column"
                Nothing -> Right ()
              dict <- readDictionaryValues pp codec hdr compBody
              collectPages nextOff (Just dict) accReps accDefs accVals
            _ -> do
              page <- readLevelDataPage codec maxRep maxDef details compBody
              defined <- decodeDefinedCount pp mDict (lpEncoding page) (lpDefined page) (lpValues page)
              collectPages nextOff mDict (lpReps page : accReps) (lpDefs page : accDefs) (defined : accVals)


readGenericRepeatedColumnChunkWith
  :: forall vec a.
     PerPage vec
  -> (vec -> V.Vector a)
  -> Compression
  -> Int  -- ^ max_repetition_level
  -> Int  -- ^ max_definition_level
  -> Int  -- ^ parent_empty_def (any def at-or-below this means
          --   the list / map is empty or absent on that row)
  -> ByteString
  -> Either String (V.Vector (V.Vector (Maybe a)))
readGenericRepeatedColumnChunkWith pp toBoxed codec maxRep maxDef parentEmptyDef chunk0 = do
  levels <- readLevelsColumnChunk pp codec maxRep maxDef chunk0
  Right (groupByRows (clRepetition levels) (clDefinition levels) (toBoxed (clValues levels)))
  where
    !maxD = fromIntegral maxDef :: Int32

    -- | Group the (rep, def, defined-values) triple into per-row
    -- inner vectors. Every @rep == 0@ boundary starts a new
    -- top-level row; the row built so far gets pushed even if
    -- it's empty.
    --
    -- Note we always push exactly one outer row per @rep == 0@
    -- boundary, including empty-row cases, so the resulting
    -- vector has the same length as the number of top-level
    -- rows in the column.
    groupByRows
      :: VP.Vector Int32
      -> VP.Vector Int32
      -> V.Vector a
      -> V.Vector (V.Vector (Maybe a))
    groupByRows reps defs defined
      | VP.length reps == 0 = V.empty
      | otherwise =
          let !n = VP.length reps
              go !rows !curRow !inRow !i !defIx
                | i >= n =
                    let !rows' = if inRow then curRow : rows else rows
                     in V.fromList (reverse (map finalise rows'))
                | otherwise =
                    let !r = VP.unsafeIndex reps i
                        !d = VP.unsafeIndex defs i
                        -- On a 'rep == 0' boundary we flush the
                        -- previous row (if any) and start a new
                        -- one. The very first entry has no row
                        -- to flush, hence the 'inRow' guard.
                        (!rows', !row0) =
                          if r == 0
                            then (if inRow then curRow : rows else rows, [])
                            else (rows, curRow)
                     in if d == maxD
                          then go rows'
                                  (Just (V.unsafeIndex defined defIx) : row0)
                                  True (i + 1) (defIx + 1)
                          else if d <= parentEmptyDef'
                            then go rows' row0 True (i + 1) defIx
                            else go rows' (Nothing : row0) True (i + 1) defIx
           in go [] [] False 0 0

    finalise xs = V.fromList (reverse xs)

    -- | Floor for "list / map is empty or absent on this row".
    -- Caller-supplied (see the 'parentEmptyDef' parameter on
    -- the public entry).
    parentEmptyDef' :: Int32
    parentEmptyDef' = fromIntegral parentEmptyDef

decodeDefinedCount :: PerPage vec -> Maybe vec -> Int32 -> Int -> ByteString -> Either String vec
decodeDefinedCount pp mDict !enc !count !bytes
  | count == 0 = Right (ppEmpty pp)
  | enc == encPlain = ppDecodePlain pp count bytes
  | isDictionaryEncoding enc = case mDict of
      Nothing -> Left "Parquet.Read: RLE_DICTIONARY page before dictionary page"
      Just dict -> ppDecodeDictionary pp count bytes dict
  | otherwise = ppExtended pp enc count bytes

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
readGenericOptionalColumnChunk pp toBoxed codec maxRep maxDef chunk = do
  levels <- readLevelsColumnChunk pp codec maxRep maxDef chunk
  Right (interleaveDefined (clDefinition levels) (fromIntegral maxDef) (toBoxed (clValues levels)))

-- | Convert a 'VP.Vector' to a 'V.Vector' via 'VP.convert'.
vpToBoxed :: VP.Prim a => VP.Vector a -> V.Vector a
vpToBoxed = VP.convert

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
  -> Either String (VP.Vector Int32)
readGenericInt32SelectedPages = readSelectedPages dispatchInt32

readGenericInt64SelectedPages
  :: Compression -> ByteString -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (VP.Vector Int64)
readGenericInt64SelectedPages = readSelectedPages dispatchInt64

readGenericFloatSelectedPages
  :: Compression -> ByteString -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (VP.Vector Float)
readGenericFloatSelectedPages = readSelectedPages dispatchFloat

readGenericDoubleSelectedPages
  :: Compression -> ByteString -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (VP.Vector Double)
readGenericDoubleSelectedPages = readSelectedPages dispatchDouble

readGenericBoolSelectedPages
  :: Compression -> ByteString -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (V.Vector Bool)
readGenericBoolSelectedPages = readSelectedPages dispatchBool

readGenericByteArraySelectedPages
  :: Compression -> ByteString -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (V.Vector ByteString)
readGenericByteArraySelectedPages = readSelectedPages dispatchByteArray

-- | Compatibility surface for dense columns. Dictionary offsets are resolved
-- from a full PAR1 file only if a kept page actually needs its dictionary.
-- Metadata-aware callers should use the selected-level readers directly.
readSelectedPages
  :: PerPage a -> Compression -> ByteString -> V.Vector PageLocation -> V.Vector Bool
  -> Either String a
readSelectedPages pp codec file locations keep =
  readSelectedValues pp codec file (PageSelection locations keep Nothing Nothing)

readSelectedValues :: PerPage vec -> Compression -> ByteString -> PageSelection -> Either String vec
readSelectedValues pp codec file selection = clValues <$> readSelectedCore False pp codec 0 0 file selection

-- Validate locations without touching page bytes. A retained page is sliced
-- to its declared size before parsing, so its header/body cannot escape into
-- a neighbour, index or footer even if its internal size is corrupt.
validateSelection :: ByteString -> PageSelection -> Either String ()
validateSelection file selection
  | V.length locations /= V.length keep = Left "Parquet.Read: keep-mask/page-location count mismatch"
  | maybe False (< 0) (selectionRowCount selection) = Left "Parquet.Read: negative row count"
  | V.null locations && maybe False (/= 0) (selectionRowCount selection) = Left "Parquet.Read: missing data pages"
  | otherwise = go 0 0 (-1)
  where
    locations = selectionLocations selection
    keep = selectionKeep selection
    fileSize = fromIntegral (BS.length file) :: Int64
    go !i !previousEnd !previousRow
      | i == V.length locations = Right ()
      | otherwise =
          let page = V.unsafeIndex locations i
              offset = plOffset page
              size = fromIntegral (plCompressedPageSize page) :: Int64
              row = plFirstRowIndex page
          in if offset < 0 || size <= 0 || offset > fileSize || size > fileSize - offset
                || (i > 0 && offset < previousEnd) || row < 0 || row <= previousRow
                || maybe False (\total -> row >= total && not (total == 0 && i == 0)) (selectionRowCount selection)
                || (i == 0 && selectionRowCount selection /= Nothing && row /= 0)
               then Left "Parquet.Read: invalid/overlapping page location or row boundary"
               else go (i + 1) (offset + size) row

readSelectedLevels :: PerPage vec -> Compression -> Int -> Int -> ByteString -> PageSelection -> Either String (ColumnLevels vec)
readSelectedLevels = readSelectedCore True

readSelectedCore :: Bool -> PerPage vec -> Compression -> Int -> Int -> ByteString -> PageSelection -> Either String (ColumnLevels vec)
readSelectedCore retainLevels pp codec maxRep maxDef file selection = do
  validateLevelLimits maxRep maxDef
  validateSelection file selection
  walk 0 Nothing [] [] []
  where
    locations = selectionLocations selection
    keep = selectionKeep selection
    walk !i !dictionary !reps !defs !values
      | i == V.length locations = ColumnLevels (concatPages VP.concat reps) (concatPages VP.concat defs) <$> concatValuePages pp values
      | not (V.unsafeIndex keep i) = walk (i + 1) dictionary reps defs values
      | otherwise = do
          let location = V.unsafeIndex locations i
              bytes = BS.take (fromIntegral (plCompressedPageSize location)) (BS.drop (fromIntegral (plOffset location)) file)
          details <- readPageDetailsCheckedAt bytes 0
          let header = pdHeader details
          size <- maybe (Left "Parquet.Read: missing page size") (Right . fromIntegral) (phCompressedPageSize header)
          if pdBodyOffset details + size /= BS.length bytes
            then Left "Parquet.Read: page size differs from offset index"
            else Right ()
          page <- if retainLevels
            then readLevelDataPage codec maxRep maxDef details (BS.drop (pdBodyOffset details) bytes)
            else readRequiredDataPage codec details (BS.drop (pdBodyOffset details) bytes)
          if not (VP.null (lpReps page)) && VP.head (lpReps page) /= 0
            then Left "Parquet.Read: indexed page starts within a row"
            else Right ()
          let nextRow = if i + 1 < V.length locations
                then Just (plFirstRowIndex (V.unsafeIndex locations (i + 1)))
                else selectionRowCount selection
          case nextRow of
            Just end | end - plFirstRowIndex location /= fromIntegral (lpRows page) ->
              Left "Parquet.Read: decoded page rows differ from offset index"
            _ -> Right ()
          dict <- if lpDefined page > 0 && isDictionaryEncoding (lpEncoding page)
            then case dictionary of
              Just _ -> Right dictionary
              Nothing -> Just <$> loadDictionary
            else Right dictionary
          defined <- decodeDefinedCount pp dict (lpEncoding page) (lpDefined page) (lpValues page)
          walk (i + 1) dict (if retainLevels then lpReps page : reps else reps)
            (if retainLevels then lpDefs page : defs else defs) (defined : values)
    loadDictionary = do
      offset <- case selectionDictionaryOffset selection of
        Just offset -> Right offset
        Nothing -> dictionaryOffsetFromFooter codec file locations
      let firstData = plOffset (V.head locations)
      if offset < 0 || offset >= firstData
        then Left "Parquet.Read: dictionary offset is not before data pages"
        else Right ()
      let bytes = BS.take (fromIntegral (firstData - offset)) (BS.drop (fromIntegral offset) file)
      details <- readPageDetailsCheckedAt bytes 0
      let size = maybe 0 fromIntegral (phCompressedPageSize (pdHeader details))
      readDictionaryValues pp codec (pdHeader details) (BS.take size (BS.drop (pdBodyOffset details) bytes))

-- This path is lazy: PLAIN/DELTA pages and synthetic page buffers do not pay
-- for parsing a footer. A unique matching column is required before using its
-- dictionary; guessing by scanning unrelated bytes could return wrong values.
dictionaryOffsetFromFooter :: Compression -> ByteString -> V.Vector PageLocation -> Either String Int64
dictionaryOffsetFromFooter codec file locations
  | BS.take 4 file /= BS.pack [80,65,82,49] = Left "Parquet.Read: selected dictionary page needs its metadata offset"
  | otherwise = do
      metadata <- readFooter file
      let firstData = plOffset (V.head locations)
          candidates = V.concatMap
            (V.mapMaybe (\column -> do
              md <- ccMetadata column
              offset <- cmDictionaryPageOffset md
              if cmDataPageOffset md == firstData && cmCodec md == codec
                then Just offset else Nothing) . rgColumns)
            (fmRowGroups metadata)
      if V.length candidates == 1
        then Right (V.head candidates)
        else Left "Parquet.Read: cannot uniquely resolve selected dictionary"

-- ============================================================
-- Optional page-index-driven page skipping
-- ============================================================
--
-- The required-page-skip path above assumes every surviving
-- page contributes a contiguous chunk of values. Nullable
-- columns carry per-page def-level streams; a /skipped/ page
-- still elides its rows from the output, so we have to:
--
-- Retain levels and defined values only for kept pages, then interleave
-- once. Skipped pages contribute no output rows. Use row-index ranges
-- from OffsetIndex when aligning selections across columns.

readGenericInt32OptionalSelectedPages
  :: Compression -> Int -> Int -> ByteString
  -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (V.Vector (Maybe Int32))
readGenericInt32OptionalSelectedPages =
  readGenericOptionalSelectedPages dispatchInt32 vpToBoxed

readGenericInt64OptionalSelectedPages
  :: Compression -> Int -> Int -> ByteString
  -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (V.Vector (Maybe Int64))
readGenericInt64OptionalSelectedPages =
  readGenericOptionalSelectedPages dispatchInt64 vpToBoxed

readGenericFloatOptionalSelectedPages
  :: Compression -> Int -> Int -> ByteString
  -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (V.Vector (Maybe Float))
readGenericFloatOptionalSelectedPages =
  readGenericOptionalSelectedPages dispatchFloat vpToBoxed

readGenericDoubleOptionalSelectedPages
  :: Compression -> Int -> Int -> ByteString
  -> V.Vector PageLocation -> V.Vector Bool
  -> Either String (V.Vector (Maybe Double))
readGenericDoubleOptionalSelectedPages =
  readGenericOptionalSelectedPages dispatchDouble vpToBoxed

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
readGenericOptionalSelectedPages pp toBoxed codec maxRep maxDef file locations keep = do
  levels <- readSelectedLevels pp codec maxRep maxDef file (PageSelection locations keep Nothing Nothing)
  Right (interleaveDefined (clDefinition levels) (fromIntegral maxDef) (toBoxed (clValues levels)))
