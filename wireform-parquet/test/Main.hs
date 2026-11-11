{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified GeospatialRegression
import qualified FileRegression
import qualified FileArrowRegression
import qualified FileZeroRegression
import qualified SchemaBoundaryRegression
import qualified CodecBoundaryRegression
import BloomBuilderRegression qualified
import NestedStatisticsRegression qualified
import NestedOptionsRegression qualified
import EncryptedWriterRegression qualified
import EncryptedReadRegression qualified
import EncryptionFooterRegression qualified
import LargeOffsetRegression qualified
import qualified TimestampRegression
import qualified NestedPrimitiveRegression
import qualified VariantSchemaRegression
import qualified VariantWriteRegression
import qualified VariantArrowRegression
import qualified VariantRegression
import qualified VariantParquetRegression
import qualified DecimalCapacityRegression
import qualified MetadataRegression
import qualified Aggregate
import qualified AlpRegression
import qualified Selection
import qualified WriterBounds
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless, when)
import Data.Bits (xor, setBit, testBit, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector as V
import Numeric (showHex)
import GHC.Float (castWord32ToFloat, castFloatToWord32, castDoubleToWord64)
import Data.List (isInfixOf)
import Data.Either (isLeft)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..), exitFailure)
import qualified System.Process as Proc

import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Ratio ((%))
import Data.List.NonEmpty qualified as NE
import Data.Word (Word8, Word16, Word32, Word64)

import qualified Crypto.Random as RNG
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Parquet.BloomFilter
import Parquet.Aggregate qualified as PAgg
import Columnar.Predicate qualified as SharedPred
import Parquet.Compress (compressPageBytes)
import Parquet.LevelsEncode qualified as LE
import Data.Bits (shiftL, shiftR, (.&.))
import Parquet.ByteStreamSplit
  ( encodeByteStreamSplitDouble
  , encodeByteStreamSplitFloat
  )
import Parquet.Delta
  ( decodeDeltaBinaryPackedInt64
  , decodeDeltaByteArray
  , decodeDeltaLengthByteArray
  )
import Parquet.DeltaEncode
  ( encodeDeltaBinaryPackedInt32
  , encodeDeltaBinaryPackedInt64
  , encodeDeltaByteArray
  , encodeDeltaLengthByteArray
  )
import qualified Parquet.Nested as Nested
import qualified Parquet.NullPagesBitmap as NPB
import qualified Parquet.Encryption as Enc
import Parquet.PageIndex
import qualified Parquet.Footer as Footer
import Parquet.Thrift.Schema qualified as TS
import Thrift.Encode qualified as Thrift
import Thrift.Value qualified as TV
import Parquet.Footer (FooterTrailer (..), parquetMagic)
import qualified Parquet.Predicate as Pred
import qualified Parquet.Read
import Parquet.Read
  ( decodeByteStreamSplitDouble
  , decodeByteStreamSplitFloat
  , loadParquetFile
  , pfFooter
  )
import Parquet.Types
import Parquet.Timestamp qualified as Timestamp
import Parquet.Page
  ( DataPageHeader (..)
  , DataPageHeaderV2 (..)
  , DictionaryPageHeader (..)
  , PageHeader (..)
  , PageType (..)
  , readPageHeaderAt
  , readPageHeaderCheckedAt
  )
import Parquet.Write
  ( ColumnAux (..)
  , ColumnEncryption (..)
  , FooterEncryption (..)
  , PageVersion (..)
  , ColumnData (..)
  , Dictionary (..)
  , OptionalColumn (..)
  , ParquetColumn (..)
  , buildDictionary
  , buildParquetFile
  , buildParquetFileWithIndex
  , buildParquetFileWithIndexIO
  , buildParquetFileWithIndexEncryptedFooter
  , columnDataLength
  , columnDataStatistics
  , emptyColumnAux
  , encodeColumnDataPageParts
  , encodeColumnDataPageV2
  , encodeDictDataPage
  , encodeDictPage
  , encodeOptionalColumnPage
  , encodeOptionalColumnPageV2Parts
  , encodePageHeader
  , encryptAuxModule
  , encryptPageBytes
  , encryptPageBytesV2
  , optionalColumnLength
  , optionalColumnNullCount
  )
import qualified Wireform.Hash as Hash

import qualified Arrow.Column      as AC
import qualified Arrow.Types       as AT
import qualified Parquet.Arrow     as PArrow
import qualified Parquet.HighLevel as PHL

main :: IO ()
main = do
  DecimalCapacityRegression.tests
  VariantWriteRegression.tests
  VariantArrowRegression.tests
  MetadataRegression.tests
  GeospatialRegression.tests
  FileRegression.tests
  FileArrowRegression.tests
  FileZeroRegression.tests
  SchemaBoundaryRegression.tests
  CodecBoundaryRegression.tests
  BloomBuilderRegression.tests
  NestedStatisticsRegression.tests
  NestedOptionsRegression.tests
  EncryptedWriterRegression.tests
  EncryptedReadRegression.tests
  EncryptionFooterRegression.tests
  LargeOffsetRegression.tests
  TimestampRegression.tests
  NestedPrimitiveRegression.tests
  VariantSchemaRegression.tests
  VariantRegression.tests
  VariantParquetRegression.tests
  -- XXH64 reference vectors.
  expectHash "" "ef46db3751d8e999"
  expectHash "abc" "44bc2cf5ad770999"
  expectHash "Nobody inspects the spammish repetition" "fbcea83c8a378bf1"
  -- 32 bytes of 'a' is exactly one bulk stripe (xxhsum -H1 reference).
  expectHashBs (BS.replicate 32 0x61) "856e843298f99ad7"
  -- 64 bytes (two stripes) — exercises the bulk-phase merge.
  expectHashBs (BS.replicate 64 0x62) "ecbaf4bdf26b6349"

  -- OffsetIndex round-trip.
  let oi = OffsetIndex
        { oiPageLocations = V.fromList
            [ PageLocation 100 200 0
            , PageLocation 300 250 50
            ]
        , oiUnencodedByteArrayDataBytes = Just (V.fromList [42, 99])
        }
  expect "OffsetIndex round-trip"
    (decodeOffsetIndex (encodeOffsetIndex oi) == Right oi)

  -- ColumnIndex round-trip with all optional fields.
  let ci = ColumnIndex
        { ciNanCounts = Just (V.fromList [0, 1, 0])
        , ciNullPages = V.fromList [False, False, True]
        , ciMinValues = V.fromList [BSC.pack "a", BSC.pack "b", BS.empty]
        , ciMaxValues = V.fromList [BSC.pack "z", BSC.pack "y", BS.empty]
        , ciBoundaryOrder = OrderAscending
        , ciNullCounts = Just (V.fromList [0, 0, 100])
        , ciRepetitionLevelHistograms = Just (V.fromList [10, 5])
        , ciDefinitionLevelHistograms = Just (V.fromList [3, 8])
        }
  expect "ColumnIndex round-trip"
    (decodeColumnIndex (encodeColumnIndex ci) == Right ci)

  -- Bloom filter membership.
  let sbbf0 = newSbbf 1024
      values = ["alpha", "beta", "gamma", "delta", "epsilon"]
      sbbf  = foldr (sbbfInsert . BSC.pack) sbbf0 values
  mapM_ (\v -> expect ("bloom contains " ++ v)
                 (sbbfCheck (BSC.pack v) sbbf)) values

  -- Golden vector from arrow-rs / parquet-mr: a 32-byte bitset produced
  -- by parquet-mr for the strings "a0".."a9" must report all of them
  -- present.  This proves byte-compatibility of our XXH64 + block layout
  -- with the reference writer.
  let goldenBits = BS.pack
        [ 200, 1, 80, 20, 64, 68, 8, 109, 6, 37, 4, 67, 144, 80, 96, 32
        , 8, 132, 43, 33, 0, 5, 99, 65, 2, 0, 224, 44, 64, 78, 96, 4 ]
      goldenSbbf = newSbbfFromBytes goldenBits
  mapM_ (\i -> let v = "a" <> show i in
                 expect ("golden contains " ++ v)
                   (sbbfCheck (BSC.pack v) goldenSbbf))
        [(0 :: Int) .. 9]

  -- Bloom filter false-positive sanity.
  let sbbfBig0 = newSbbf 2048
      inserted = map (BSC.pack . ("inserted-" <>) . show) [0 .. 255 :: Int]
      probes   = map (BSC.pack . ("probe-" <>) . show)   [0 .. 255 :: Int]
      sbbfBig  = foldr sbbfInsert sbbfBig0 inserted
      fp = length (filter (`sbbfCheck` sbbfBig) probes)
  expect ("bloom FP rate (got " ++ show fp ++ ")") (fp <= 16)

  -- Bloom encode/decode round-trip.
  let bs = encodeBloomFilter sbbf
  case decodeBloomFilter bs of
    Left e -> failTest ("decodeBloomFilter: " ++ e)
    Right (_hdr, sbbf') -> do
      expect "decoded numBytes" (sbbfNumBytes sbbf' == sbbfNumBytes sbbf)
      mapM_ (\v -> expect ("decoded contains " ++ v)
                     (sbbfCheck (BSC.pack v) sbbf')) values

  -- Statistics (via columnDataStatistics, the public column-typed API).
  let s32 = columnDataStatistics (ColInt32 (VP.fromList [3, -1, 7, 0, 4 :: Int32]))
  expect "Int32 stats min"
    (statMinValue s32 == Just (BS.pack [0xFF, 0xFF, 0xFF, 0xFF]))   -- -1 LE
  expect "Int32 stats max"
    (statMaxValue s32 == Just (BS.pack [0x07, 0x00, 0x00, 0x00]))
  expect "Int32 stats nullCount"
    (statNullCount s32 == Just 0)
  let s64 = columnDataStatistics (ColInt64 (VP.fromList [10, 5, -3, 100 :: Int64]))
  expect "Int64 stats min/max present"
    (statMinValue s64 /= Nothing && statMaxValue s64 /= Nothing)
  let sBA = columnDataStatistics
              (ColByteArray (V.fromList [BSC.pack "banana", BSC.pack "apple", BSC.pack "cherry"]))
  expect "ByteArray stats min == 'apple'"
    (statMinValue sBA == Just (BSC.pack "apple"))
  expect "ByteArray stats max == 'cherry'"
    (statMaxValue sBA == Just (BSC.pack "cherry"))
  let binaryBounds = columnDataStatistics (ColByteArray (V.fromList [BS.pack [0x80], BS.pack [0x7f]]))
  expect "unsigned byte bounds never advertise legacy signed ordering"
    (statMin binaryBounds == Nothing && statMax binaryBounds == Nothing
      && statMinValue binaryBounds == Just (BS.pack [0x7f])
      && statMaxValue binaryBounds == Just (BS.pack [0x80])
      && statMinValueExact binaryBounds == Just True && statMaxValueExact binaryBounds == Just True)
  let sEmpty = columnDataStatistics (ColInt32 VP.empty)
  expect "empty Int32 stats has no min/max"
    (statMinValue sEmpty == Nothing && statMaxValue sEmpty == Nothing)

  -- Writer attaches statistics that round-trip through readFooter.
  let schema = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
        , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
        ]
      vs   = VP.fromList [(3 :: Int32), -1, 7, 0, 4]
      fbs  = buildParquetFile schema (V.singleton (V.singleton (ColInt32 vs)))
  case loadParquetFile fbs of
    Left e -> failTest ("loadParquetFile: " ++ e)
    Right pf -> do
      let !rgs = fmRowGroups (pfFooter pf)
          !cm = ccMetadata
                  (V.unsafeIndex (rgColumns (V.unsafeIndex rgs 0)) 0)
      case cm of
        Nothing -> failTest "expected ColumnMetadata"
        Just m -> case cmStatistics m of
          Nothing -> failTest "writer omitted statistics"
          Just st -> do
            expect "writer min == -1"
              (statMinValue st == Just (BS.pack [0xFF, 0xFF, 0xFF, 0xFF]))
            expect "writer max == 7"
              (statMaxValue st == Just (BS.pack [0x07, 0x00, 0x00, 0x00]))

  -- buildParquetFileWithIndex: bloom filter + page index + column index
  -- offsets are populated on the round-tripped column metadata.
  let schemaIdx = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
        , SchemaElement "y" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
        ]
      vsIdx = ColInt32 (VP.fromList [(1 :: Int32), 2, 3, 4])
      bf    = sbbfInsertHash 0xdeadbeef (newSbbf (optimalNumBytes 1024 0.01))
      oiIdx = OffsetIndex
                { oiPageLocations = V.singleton (PageLocation 0 16 0)
                , oiUnencodedByteArrayDataBytes = Nothing
                }
      ciIdx = ColumnIndex
                { ciNanCounts = Nothing
                , ciNullPages = V.singleton False
                , ciMinValues = V.singleton (BS.pack [0x01, 0, 0, 0])
                , ciMaxValues = V.singleton (BS.pack [0x04, 0, 0, 0])
                , ciBoundaryOrder = OrderAscending
                , ciNullCounts = Nothing
                , ciRepetitionLevelHistograms = Nothing
                , ciDefinitionLevelHistograms = Nothing
                }
      aux   = ColumnAux False (Just bf) (Just oiIdx) (Just ciIdx) Uncompressed PageV1 Nothing
      fIdx  = buildParquetFileWithIndex schemaIdx
                (V.singleton (V.singleton vsIdx))
                (V.singleton (V.singleton aux))
      _ = vsIdx :: ColumnData
  case loadParquetFile fIdx of
    Left e -> failTest ("indexed-writer load: " ++ e)
    Right pf -> do
      let !rgs2 = fmRowGroups (pfFooter pf)
          !cc   = V.unsafeIndex (rgColumns (V.unsafeIndex rgs2 0)) 0
      case ccMetadata cc of
        Nothing -> failTest "indexed-writer: missing metadata"
        Just m -> do
          expect "indexed-writer: bloom filter offset populated"
            (cmBloomFilterOffset m /= Nothing)
          expect "indexed-writer: bloom filter length populated"
            (cmBloomFilterLength m /= Nothing)
      expect "indexed-writer: offset index offset populated"
        (ccOffsetIndexOffset cc /= Nothing)
      expect "indexed-writer: offset index length populated"
        (ccOffsetIndexLength cc /= Nothing)
      expect "indexed-writer: column index offset populated"
        (ccColumnIndexOffset cc /= Nothing)
      expect "indexed-writer: column index length populated"
        (ccColumnIndexLength cc /= Nothing)
      writtenIndex <- either failTest pure (readOffsetIndex pf 0 0)
      locations <- maybe (failTest "explicit offset index missing") (pure . oiPageLocations) writtenIndex
      selected <- either failTest pure (Parquet.Read.readGenericInt32SelectedPages
        Uncompressed fIdx locations (V.singleton True))
      expect "explicit index offsets and sizes are repaired to select the real page"
        (selected == VP.fromList [1,2,3,4])

  -- Typed (heterogeneous-primitive) writer: write a row group with one
  -- column of each primitive type and confirm each round-trips through
  -- the footer.
  let schemaTyped = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 6) Nothing Nothing Nothing Nothing
        , SchemaElement "i32"  (Just Required) (Just PTInt32)     Nothing Nothing Nothing Nothing Nothing
        , SchemaElement "i64"  (Just Required) (Just PTInt64)     Nothing Nothing Nothing Nothing Nothing
        , SchemaElement "f32"  (Just Required) (Just PTFloat)     Nothing Nothing Nothing Nothing Nothing
        , SchemaElement "f64"  (Just Required) (Just PTDouble)    Nothing Nothing Nothing Nothing Nothing
        , SchemaElement "bool" (Just Required) (Just PTBoolean)   Nothing Nothing Nothing Nothing Nothing
        , SchemaElement "ba"   (Just Required) (Just PTByteArray) Nothing Nothing Nothing Nothing Nothing
        ]
      cols  = V.fromList
        [ ColInt32     (VP.fromList [(1 :: Int32), 2, 3])
        , ColInt64     (VP.fromList [(10 :: Int64), 20, 30])
        , ColFloat     (VP.fromList [1.5 :: Float, 2.5, 3.5])
        , ColDouble    (VP.fromList [1e9 :: Double, 2e9, 3e9])
        , ColBool      (V.fromList [True, False, True])
        , ColByteArray (V.fromList [BSC.pack "alpha", BSC.pack "beta", BSC.pack "gamma"])
        ]
      fTyped = buildParquetFile schemaTyped (V.singleton cols)
  case loadParquetFile fTyped of
    Left e -> failTest ("typed-writer load: " ++ e)
    Right pf -> do
      let !rgs3 = fmRowGroups (pfFooter pf)
          !rg   = V.unsafeIndex rgs3 0
      expect "typed-writer: 6 columns recorded"
        (V.length (rgColumns rg) == 6)
      expect "typed-writer: numRows == 3"
        (rgNumRows rg == 3)
      let typeOf i = case ccMetadata (V.unsafeIndex (rgColumns rg) i) of
            Just m  -> Just (cmType m)
            Nothing -> Nothing
      expect "typed-writer: column 0 is INT32"
        (typeOf 0 == Just PTInt32)
      expect "typed-writer: column 1 is INT64"
        (typeOf 1 == Just PTInt64)
      expect "typed-writer: column 2 is FLOAT"
        (typeOf 2 == Just PTFloat)
      expect "typed-writer: column 3 is DOUBLE"
        (typeOf 3 == Just PTDouble)
      expect "typed-writer: column 4 is BOOLEAN"
        (typeOf 4 == Just PTBoolean)
      expect "typed-writer: column 5 is BYTE_ARRAY"
        (typeOf 5 == Just PTByteArray)

  -- Per-column compression: GZip is always available; round-trip a column
  -- through the writer and confirm the metadata records the codec.
  let auxesGzip = V.singleton (V.fromList
        [ ColumnAux False Nothing Nothing Nothing GZip PageV1 Nothing
        , ColumnAux False Nothing Nothing Nothing GZip PageV1 Nothing
        , ColumnAux False Nothing Nothing Nothing Uncompressed PageV1 Nothing  -- floats stay raw
        , ColumnAux False Nothing Nothing Nothing GZip PageV1 Nothing
        , ColumnAux False Nothing Nothing Nothing Uncompressed PageV1 Nothing
        , ColumnAux False Nothing Nothing Nothing GZip PageV1 Nothing
        ])
      fGz = buildParquetFileWithIndex schemaTyped (V.singleton cols) auxesGzip
  case loadParquetFile fGz of
    Left e -> failTest ("gzip-writer load: " ++ e)
    Right pf -> do
      let !rgs4 = fmRowGroups (pfFooter pf)
          !rg4  = V.unsafeIndex rgs4 0
          codecOf i = case ccMetadata (V.unsafeIndex (rgColumns rg4) i) of
            Just m  -> Just (cmCodec m)
            Nothing -> Nothing
      expect "gzip-writer: column 0 codec = GZip"
        (codecOf 0 == Just GZip)
      expect "gzip-writer: column 2 codec = Uncompressed"
        (codecOf 2 == Just Uncompressed)
      expect "gzip-writer: column 5 codec = GZip"
        (codecOf 5 == Just GZip)
      -- Compressed page bytes should be smaller for the byte-array column
      -- (5 letters per value, three values; gzip beats raw at that size
      -- by virtue of the 4-byte length-prefix overhead overlapping with
      -- the gzip header but for our test we only assert it's <= raw).
      let compSize i = case ccMetadata (V.unsafeIndex (rgColumns rg4) i) of
            Just m -> Just (cmTotalCompressedSize m, cmTotalUncompressedSize m)
            Nothing -> Nothing
      case compSize 0 of
        Just (c, u) -> expect "gzip-writer: column 0 has uncompressed > 0 in metadata"
          (u > 0 && c > 0)
        Nothing -> failTest "gzip-writer: column 0 missing metadata"

  -- DATA_PAGE_V2 file round-trip (uncompressed) through buildParquetFileWithIndex.
  -- We confirm the page header type changes accordingly and the values still
  -- decode through the V1 PLAIN reader (the value bytes are PLAIN; only the
  -- header / level layout differ).
  let v2Schema = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
        , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
        ]
      v2Vals = ColInt32 (VP.fromList [(11 :: Int32), 22, 33, 44])
      v2Aux  = ColumnAux False Nothing Nothing Nothing Uncompressed PageV2 Nothing
      v2File = buildParquetFileWithIndex v2Schema
                 (V.singleton (V.singleton v2Vals))
                 (V.singleton (V.singleton v2Aux))
  case loadParquetFile v2File of
    Left e -> failTest ("DATA_PAGE_V2 round-trip load: " ++ e)
    Right pf -> do
      let !rgsV2 = fmRowGroups (pfFooter pf)
      expect "DATA_PAGE_V2: numRows == 4"
        (rgNumRows (V.unsafeIndex rgsV2 0) == 4)
  -- Direct page-level V2 encoders must produce DATA_PAGE_V2 headers.
  case encodeColumnDataPageV2 Uncompressed v2Vals of
    Left e   -> failTest ("encodeColumnDataPageV2: " ++ e)
    Right pageBs -> case readPageHeaderAt pageBs 0 of
      Left e -> failTest ("V2 page header parse: " ++ e)
      Right (hdr, _) -> case phType hdr of
        PtDataPageV2 v2 -> do
          expect "V2 header reports 4 values" (dph2NumValues v2 == 4)
          expect "V2 header reports 0 nulls"  (dph2NumNulls v2 == 0)
          expect "V2 header reports 4 rows"   (dph2NumRows v2 == 4)
        _ -> failTest "V2 encoder did not produce PtDataPageV2"

  -- Nested column shred: optional list of optional INT32, the
  -- 'list<int?>?' shape every Iceberg V3 default-value array uses.
  -- For [[Just 1, Nothing, Just 3], [], Nothing] the spec says:
  --   maxDef=3, maxRep=1
  --   events:  (d=3,r=0,v=1), (d=2,r=1,v=null), (d=3,r=1,v=3),
  --            (d=1,r=0,v=null) (empty list),
  --            (d=0,r=0,v=null) (null list)
  let nestedRows = V.fromList
        [ Just (V.fromList [Just (1 :: Int32), Nothing, Just 3])
        , Just V.empty
        , Nothing
        ]
      nLeaf = Nested.encodeOptionalListOptionalI32 "xs" nestedRows
  expect "nested leaf maxDef == 3" (Nested.nlMaxDef nLeaf == 3)
  expect "nested leaf maxRep == 1" (Nested.nlMaxRep nLeaf == 1)
  expect "nested leaf def levels match Dremel"
    (VP.toList (Nested.nlDefLevels nLeaf) == [3, 2, 3, 1, 0])
  expect "nested leaf rep levels match Dremel"
    (VP.toList (Nested.nlRepLevels nLeaf) == [0, 1, 1, 0, 0])
  expect "nested leaf present-value count == 2"
    (Nested.nlValueCount nLeaf == 2)
  expect "nested leaf PLAIN bytes are LE [1, 3]"
    (Nested.nlValueBytes nLeaf
       == BS.pack [0x01,0,0,0, 0x03,0,0,0])
  expect "nested leaf path is xs.list.element"
    (V.toList (Nested.nlPath nLeaf) == ["xs", "list", "element"])

  -- Modular encryption round-trip: encrypted column-chunk page-bytes
  -- must round-trip through encryptPageBytes with the matching AAD.
  --
  -- We exercise both algorithms (AES-GCM-V1 and AES-GCM-CTR-V1) and
  -- assert that flipping one bit of the ciphertext breaks GCM auth.
  let encKey  = BS.replicate 16 0x42
      ce alg  = ColumnEncryption
                  { ceAlgorithm     = alg
                  , ceKey           = encKey
                  , ceFileId        = BSC.pack "fileid01"
                  , ceAadPrefix     = BS.empty
                  , ceKeyMetadata   = BSC.pack "kid:test"
                  , ceColumnOrdinal = 0
                  }
      encVals = ColInt32 (VP.fromList [(7 :: Int32), 8, 9])
      encSchema = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
        , SchemaElement "v" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
        ]
      encAux alg = ColumnAux False Nothing Nothing Nothing Uncompressed PageV1 (Just (ce alg))
      encFooter = FooterEncryption encKey "fileid01" BS.empty "kid:test"
      encFile alg = buildParquetFileWithIndexEncryptedFooter encFooter encSchema
                      (V.singleton (V.singleton encVals))
                      (V.singleton (V.singleton (encAux alg)))
  gcmFile <- encFile Enc.AesGcmV1 >>= either failTest pure
  ctrFile <- encFile Enc.AesGcmCtrV1 >>= either failTest pure
  otherGcmFile <- encFile Enc.AesGcmV1 >>= either failTest pure
  expect "encrypted file uses fresh nonces on repeated writes" (gcmFile /= otherGcmFile)
  expect "encrypted file (AES-GCM-V1) is non-empty"
    (BS.length gcmFile > 0)
  expect "encrypted file (AES-GCM-CTR-V1) is non-empty"
    (BS.length ctrFile > 0)
  expect "encrypted file differs from plaintext output"
    (gcmFile
       /= buildParquetFileWithIndex encSchema
            (V.singleton (V.singleton encVals))
            (V.singleton (V.singleton emptyColumnAux)))

  -- Round-trip the page through encryptPageBytes / decryptGcmModule
  -- directly so we know our AAD framing matches what the reader needs.
  -- Per parquet-format Encryption.md §5.1 each module on the wire is
  -- prefixed with a 4-byte little-endian length, so the framing is
  -- '<len4 || nonce(12) || ct || tag(16)>' for header followed by
  -- '<len4 || nonce(12) || ct || tag(16)>' for body.
  let (rawHdr, rawBody) = encodeColumnDataPageParts encVals
  encryptPageBytes (ce Enc.AesGcmV1) Enc.ModuleDataPage 0 0 rawHdr rawBody >>= \case
    Left e -> failTest ("encryptPageBytes: " ++ e)
    Right encBytes -> do
      let aadHdr = Enc.buildAad BS.empty
                     (Enc.buildAadSuffix (BSC.pack "fileid01")
                        Enc.ModuleDataPageHeader 0 0 0)
          aadBody = Enc.buildAad BS.empty
                     (Enc.buildAadSuffix (BSC.pack "fileid01")
                        Enc.ModuleDataPage 0 0 0)
      case Enc.readFramedModule encBytes 0 of
        Left e -> failTest ("read framed page header: " ++ e)
        Right (encHdr, after1) -> do
          case Enc.decryptGcmModule encKey aadHdr encHdr of
            Right plain -> expect "encrypted page header round-trips"
                                  (readPageHeaderAt plain 0 == fmap (\(h, _) -> (h {phCompressedPageSize = Just (fromIntegral (BS.length rawBody + 32))}, BS.length plain)) (readPageHeaderAt rawHdr 0))
            Left e -> failTest ("decrypt page header: " ++ e)
          case Enc.readFramedModule encBytes after1 of
            Left e -> failTest ("read framed page body: " ++ e)
            Right (encBody, _) ->
              case Enc.decryptGcmModule encKey aadBody encBody of
                Right plain -> expect "encrypted page body round-trips"
                                      (plain == rawBody)
                Left e -> failTest ("decrypt page body: " ++ e)
      -- Tampering with one byte of the body must invalidate the GCM tag.
      let tampered = BS.snoc (BS.init encBytes) (BS.last encBytes `xor` 1)
      case Enc.readFramedModule tampered 0 of
        Left _ -> expect "tampered framing rejected" True
        Right (encHdr, after1) -> do
          case Enc.decryptGcmModule encKey aadHdr encHdr of
            Left _  -> expect "tampered header fails GCM auth" True
            Right _ ->
              case Enc.readFramedModule tampered after1 of
                Left _ -> expect "tampered body framing rejected" True
                Right (encBody, _) ->
                  case Enc.decryptGcmModule encKey aadBody encBody of
                    Right _ -> failTest "tampered ciphertext decrypted (BAD)"
                    Left _  -> expect "tampered body fails GCM auth" True

  -- V2 keeps levels uncompressed, but encrypts them with the values.
  case encodeOptionalColumnPageV2Parts Uncompressed (OptInt32 (V.fromList [Just 7, Nothing, Just 9])) of
    Left e -> failTest e
    Right (headerV2, repV2, defV2, valuesV2) -> do
      expect "nullable V2 encryption test has actual definition levels" (not (BS.null defV2))
      mapM_ (\alg -> do
        result <- encryptPageBytesV2 (ce alg) 3 2 headerV2 repV2 defV2 valuesV2
        bytes <- either failTest pure result
        (encryptedHeader, afterHeader) <- either failTest pure (Enc.readFramedModule bytes 0)
        let aadHeader = Enc.buildAadSuffix "fileid01" Enc.ModuleDataPageHeader 3 0 2
            aadBody = Enc.buildAadSuffix "fileid01" Enc.ModuleDataPage 3 0 2
        decryptedHeader <- either failTest pure (Enc.decryptGcmModule encKey aadHeader encryptedHeader)
        (parsedHeader, _) <- either failTest pure (readPageHeaderAt decryptedHeader 0)
        (originalHeader, _) <- either failTest pure (readPageHeaderAt headerV2 0)
        (encryptedBody, afterBody) <- either failTest pure (Enc.readFramedModule bytes afterHeader)
        expect "V2 header records encrypted body size"
          (parsedHeader == originalHeader {phCompressedPageSize = Just (fromIntegral (afterBody - afterHeader))})
        expect "V2 has exactly two encrypted modules" (afterBody == BS.length bytes)
        let decoded = case alg of
              Enc.AesGcmV1 -> Enc.decryptGcmModule encKey aadBody encryptedBody
              Enc.AesGcmCtrV1 -> Enc.decryptCtrModule encKey encryptedBody
        expect "V2 body encrypts levels together with all values"
          (decoded == Right (BS.concat [repV2, defV2, valuesV2])))
        [Enc.AesGcmV1, Enc.AesGcmCtrV1]

  -- Encrypted aux modules: bloom filter / offset index / column
  -- index payloads are wrapped as separate GCM modules under their
  -- spec-assigned ModuleType AAD when caEncryption is set on the
  -- column. We exercise this by feeding the resulting bytes back
  -- through Enc.decryptGcmModule under the matching AAD.
  do
    let bfx = sbbfInsertHash 0xdeadbeef (newSbbf (optimalNumBytes 1024 0.01))
        oix = OffsetIndex
                { oiPageLocations = V.singleton (PageLocation 0 16 0)
                , oiUnencodedByteArrayDataBytes = Nothing
                }
        cix = ColumnIndex
                { ciNanCounts = Nothing
                , ciNullPages = V.singleton False
                , ciMinValues = V.singleton (BS.pack [0x01, 0, 0, 0])
                , ciMaxValues = V.singleton (BS.pack [0x04, 0, 0, 0])
                , ciBoundaryOrder = OrderAscending
                , ciNullCounts = Nothing
                , ciRepetitionLevelHistograms = Nothing
                , ciDefinitionLevelHistograms = Nothing
                }
        rawBloom = encodeBloomFilter bfx
        rawOff   = encodeOffsetIndex oix
        rawCol   = encodeColumnIndex cix
        encOf mt = encryptAuxModule
                    (Just (ce Enc.AesGcmV1)) mt 0
        aadOf mt = Enc.buildAad BS.empty
                    (Enc.buildAadSuffix (BSC.pack "fileid01") mt 0 0 0)
    let unframed mt raw = do
          encoded <- encOf mt raw >>= either failTest pure
          fst <$> either failTest pure (Enc.readFramedModule encoded 0)
    bloomModule <- unframed Enc.ModuleBloomFilterBitset rawBloom
    offsetModule <- unframed Enc.ModuleOffsetIndex rawOff
    columnModule <- unframed Enc.ModuleColumnIndex rawCol
    case Enc.decryptGcmModule encKey
           (aadOf Enc.ModuleBloomFilterBitset)
           bloomModule of
      Right p -> expect "encrypted bloom filter module round-trips"
                        (p == rawBloom)
      Left e  -> failTest ("decrypt bloom filter module: " ++ e)
    case Enc.decryptGcmModule encKey
           (aadOf Enc.ModuleOffsetIndex)
           offsetModule of
      Right p -> expect "encrypted offset-index module round-trips"
                        (p == rawOff)
      Left e  -> failTest ("decrypt offset-index module: " ++ e)
    case Enc.decryptGcmModule encKey
           (aadOf Enc.ModuleColumnIndex)
           columnModule of
      Right p -> expect "encrypted column-index module round-trips"
                        (p == rawCol)
      Left e  -> failTest ("decrypt column-index module: " ++ e)
    -- A whole-file build with caEncryption + bloom/offset/column
    -- index aux must produce a different file from the same build
    -- without the encryption (i.e. the auxes really got encrypted).
    let auxEnc = ColumnAux False (Just bfx) (Just oix) (Just cix)
                           Uncompressed PageV1 (Just (ce Enc.AesGcmV1))
        auxPlain = ColumnAux False (Just bfx) (Just oix) (Just cix)
                             Uncompressed PageV1 Nothing
        encFileEnc = buildParquetFileWithIndexEncryptedFooter encFooter encSchema
                       (V.singleton (V.singleton encVals))
                       (V.singleton (V.singleton auxEnc))
        encFilePlain = buildParquetFileWithIndex encSchema
                       (V.singleton (V.singleton encVals))
                       (V.singleton (V.singleton auxPlain))
    encryptedAuxFile <- encFileEnc >>= either failTest pure
    expect "aux-encrypted file diverges from plaintext-aux file"
      (encryptedAuxFile /= encFilePlain)

  -- Encrypted-footer mode: footer is a single GCM module under
  -- ModuleFooter AAD, file ends with PARE magic instead of PAR1, and
  -- decrypting recovers the original plaintext FileMetadata thrift.
  do
    let footerEnc = FooterEncryption
                      { feKey         = encKey
                      , feFileId      = BSC.pack "fileid01"
                      , feAadPrefix   = BS.empty
                      , feKeyMetadata = BSC.pack "kid:test"
                      }
        makeEncFooterFile = buildParquetFileWithIndexEncryptedFooter footerEnc
                    encSchema
                    (V.singleton (V.singleton encVals))
                    (V.singleton (V.singleton emptyColumnAux))
        plainFile = buildParquetFileWithIndex encSchema
                      (V.singleton (V.singleton encVals))
                      (V.singleton (V.singleton emptyColumnAux))
    encFooterFile <- makeEncFooterFile >>= either failTest pure
    expect "encrypted-footer file is non-empty"
      (BS.length encFooterFile > 0)
    expect "encrypted-footer file ends with PARE magic"
      (BS.takeEnd 4 encFooterFile == BSC.pack "PARE")
    expect "plaintext-footer file ends with PAR1 magic"
      (BS.takeEnd 4 plainFile == BSC.pack "PAR1")
    expect "encrypted-footer file diverges from plaintext-footer"
      (encFooterFile /= plainFile)
    -- Use the public encrypted-reader API to round-trip via the
    -- ModuleFooter AAD. Per parquet-format §5.4 the bytes between
    -- the leading PAR1 magic and the trailing PARE magic are
    -- '<FileCryptoMetaData thrift> <encrypted footer module>',
    -- and the writer + reader handle the layout symmetrically.
    let fdec = Parquet.Read.FooterDecryption
                 { Parquet.Read.fdKey       = encKey
                 , Parquet.Read.fdFileId    = BSC.pack "fileid01"
                 , Parquet.Read.fdAadPrefix = BS.empty
                 }
    case Parquet.Read.loadParquetFileEncrypted fdec encFooterFile of
      Left e -> failTest ("loadParquetFileEncrypted: " ++ e)
      Right pf' -> do
        let fmDecrypted = Parquet.Read.pfFooter pf'
        expect "encrypted-footer round-trips through reader"
          (V.length (fmSchema fmDecrypted) > 0)
        expect "encrypted-footer reader recovers row-group count"
          (V.length (fmRowGroups fmDecrypted) == 1)
    -- Wrong key must fail GCM auth.
    let wrongFd = fdec { Parquet.Read.fdKey = BS.replicate 16 0 }
    case Parquet.Read.loadParquetFileEncrypted wrongFd encFooterFile of
      Left _  -> expect "wrong key rejected by GCM auth" True
      Right _ -> failTest "encrypted footer decrypted with wrong key (BAD)"

  -- Whole-file V2 + encryption: ColumnAux carries both PageV2 and
  -- ColumnEncryption, so buildParquetFileWithIndex must dispatch
  -- through encryptPageBytesV2 internally.
  let v2EncAux alg = ColumnAux False Nothing Nothing Nothing Uncompressed PageV2 (Just (ce alg))
      v2EncFile alg = buildParquetFileWithIndexEncryptedFooter encFooter encSchema
                        (V.singleton (V.singleton encVals))
                        (V.singleton (V.singleton (v2EncAux alg)))
  v2GcmFile <- v2EncFile Enc.AesGcmV1 >>= either failTest pure
  v2CtrFile <- v2EncFile Enc.AesGcmCtrV1 >>= either failTest pure
  expect "V2 + AES-GCM-V1 whole-file write is non-empty"
    (BS.length v2GcmFile > 0)
  expect "V2 + AES-GCM-CTR-V1 whole-file write is non-empty"
    (BS.length v2CtrFile > 0)
  expect "V2 + encryption differs from V2 plaintext file"
    (v2GcmFile
       /= buildParquetFileWithIndex encSchema
            (V.singleton (V.singleton encVals))
            (V.singleton (V.singleton
              (ColumnAux False Nothing Nothing Nothing Uncompressed PageV2 Nothing))))

  -- Optional column page: definition levels + present-only PLAIN values.
  let optCol = OptInt32 (V.fromList [Just 1, Nothing, Just 3, Nothing, Just 5])
      pageBs = encodeOptionalColumnPage optCol
  expect "optional-column-page produces a non-empty body"
    (BS.length pageBs > 4)
  expect "optionalColumnLength counts all rows"
    (optionalColumnLength optCol == 5)
  expect "optionalColumnNullCount counts nulls"
    (optionalColumnNullCount optCol == 2)

  -- Dictionary encoding: build a dictionary, encode dictionary + data
  -- pages, and confirm the dictionary captured the unique values.
  let dictInput = ColByteArray (V.fromList
        [BSC.pack "alpha", BSC.pack "beta", BSC.pack "alpha"
        ,BSC.pack "gamma", BSC.pack "beta", BSC.pack "alpha"])
      dict = buildDictionary dictInput
      dictPage = encodeDictPage dict
      dataPage = encodeDictDataPage dict
  expect "dictionary unique count == 3"
    (columnDataLength (dictUniques dict) == 3)
  expect "dictionary index vector has one entry per row"
    (VP.length (dictIndices dict) == 6)
  expect "dictionary page header is non-empty"
    (BS.length dictPage > 4)
  expect "dictionary data page is non-empty"
    (BS.length dataPage > 4)

  -- NullPagesBitmap: pack a Vector Bool into an LSB-first bitmap,
  -- count null pages via SIMD popcount, list non-null page indices.
  let nullPages = V.fromList [True, False, True, False, False, True, True, False, True]
      packed = NPB.packNullPages nullPages
  expect "nullPagesBitmap round-trip"
    (NPB.unpackNullPages (V.length nullPages) packed == nullPages)
  expect "nullPagesBitmap popcount"
    (NPB.nullPageCount packed == 5)
  expect "nullPagesBitmap nonNullPages == [1,3,4,7]"
    (V.toList (NPB.nonNullPages (V.length nullPages) packed) == [1, 3, 4, 7])

  -- DELTA_BINARY_PACKED writer round-trips through the reader for a few
  -- shapes (constant deltas, mixed deltas, negative deltas, single value).
  let testCases =
        [ ("ascending integers", VP.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10 :: Int64])
        , ("constant",           VP.fromList (replicate 12 (42 :: Int64)))
        , ("descending negatives", VP.fromList [10, 5, 0, -5, -10, -50, -1000 :: Int64])
        , ("singleton",          VP.fromList [12345 :: Int64])
        , ("empty",              VP.empty :: VP.Vector Int64)
        , ("128 mixed",          VP.fromList [let i64 = fromIntegral i :: Int64 in i64 * 7 - i64 `mod` 13 | i <- [(0 :: Int) .. 127]])
        ]
  flip mapM_ testCases $ \(name, xs) ->
    case decodeDeltaBinaryPackedInt64 (VP.length xs) (encodeDeltaBinaryPackedInt64 xs) of
      Right out ->
        expect ("DELTA_BINARY_PACKED round-trip: " ++ name)
          (VP.toList out == VP.toList xs)
      Left e -> failTest ("DELTA_BINARY_PACKED " ++ name ++ ": " ++ e)

  -- Modular encryption: round-trip plaintext through encrypt/decrypt for
  -- both AES-GCM and AES-CTR, and verify GCM rejects a tampered tag.
  let key128 = BS.replicate 16 0x42
      aad    = Enc.buildAad "" (Enc.buildAadSuffix "fileid01" Enc.ModuleColumnMetaData 0 0 0)
      plain  = BSC.pack "Hello, Iceberg + Parquet!"
  drg0 <- RNG.drgNew
  let (eGcm, _) = RNG.withDRG drg0 (Enc.encryptModule key128 aad plain)
  case eGcm of
    Left e -> failTest ("GCM encrypt: " ++ e)
    Right ct -> do
      expect "GCM round-trip"
        (Enc.decryptModule key128 aad ct == Right plain)
      expect "GCM rejects tampered ciphertext"
        (case Enc.decryptModule key128 aad (BS.snoc (BS.init ct) (BS.last ct `xor` 1)) of
            Right _ -> False
            Left _  -> True)
  drg1 <- RNG.drgNew
  let (eCtr, _) = RNG.withDRG drg1 (Enc.encryptModuleCtr key128 plain)
  case eCtr of
    Left e -> failTest ("CTR encrypt: " ++ e)
    Right ct ->
      expect "CTR round-trip"
        (Enc.decryptModuleCtr key128 ct == Right plain)
  -- Independent spec bytes: footer has no ordinals; dictionary/index
  -- modules omit page ordinal; file identifiers are not padded/truncated.
  expect "footer AAD preserves file identifier and omits ordinals"
    (Enc.buildAadSuffix "abcdefghijk" Enc.ModuleFooter 7 8 9 == "abcdefghijk" <> BS.singleton 0)
  expect "column metadata AAD omits page ordinal"
    (Enc.buildAadSuffix "id" Enc.ModuleColumnMetaData 0x102 0x304 9 == "id" <> BS.pack [1, 2, 1, 4, 3])
  expect "data AAD includes little-endian page ordinal"
    (Enc.buildAadSuffix "id" Enc.ModuleDataPage 0x102 0x304 0x506 == "id" <> BS.pack [2, 2, 1, 4, 3, 6, 5])
  expect "CTR starts at counter one (independent AES-128 known answer)"
    (Enc.decryptModuleCtr (BS.replicate 16 0)
      (BS.replicate 12 0 <> BS.pack [0x58,0xe2,0xfc,0xce,0xfa,0x7e,0x30,0x61,0x36,0x7f,0x1d,0x57,0xa4,0xe7,0x45,0x5a])
      == Right (BS.replicate 16 0))

  -- BYTE_STREAM_SPLIT round-trips for FLOAT and DOUBLE.
  let bssFloats  = VP.fromList [1.5, -2.25, 3.14159, 0, 1e9, -1e-9 :: Float]
      bssDoubles = VP.fromList [1.5, -2.25, 3.14159, 0, 1e9, -1e-9 :: Double]
      bssFloatBs  = encodeByteStreamSplitFloat bssFloats
      bssDoubleBs = encodeByteStreamSplitDouble bssDoubles
  expect "BYTE_STREAM_SPLIT FLOAT byte length == 4 * n"
    (BS.length bssFloatBs == 4 * VP.length bssFloats)
  expect "BYTE_STREAM_SPLIT DOUBLE byte length == 8 * n"
    (BS.length bssDoubleBs == 8 * VP.length bssDoubles)
  case decodeByteStreamSplitFloat (VP.length bssFloats) bssFloatBs of
    Right xs -> expect "BYTE_STREAM_SPLIT FLOAT round-trip" (xs == bssFloats)
    Left  e  -> failTest ("BYTE_STREAM_SPLIT FLOAT decode: " ++ e)
  case decodeByteStreamSplitDouble (VP.length bssDoubles) bssDoubleBs of
    Right xs -> expect "BYTE_STREAM_SPLIT DOUBLE round-trip" (xs == bssDoubles)
    Left  e  -> failTest ("BYTE_STREAM_SPLIT DOUBLE decode: " ++ e)

  -- Compression codec round-trips. Legacy LZ4 remains read-only;
  -- LZO is always available without an external compression library.
  case compressPageBytes LZ4 "anything" of
    Left _  -> expect "LZ4 (codec 5) is refused by the compressor" True
    Right _ -> failTest "Parquet.Compress: LZ4 compression should fail"
  let lzoInput = BS.concat (replicate 256 "raw LZO1X page data ")
  case compressPageBytes LZO lzoInput of
    Left err -> failTest ("Parquet.Compress: LZO compression failed: " ++ err)
    Right compressed -> do
      expect "LZO compresses the repetitive fixture" (BS.length compressed < BS.length lzoInput)
      expect "LZO raw page round-trip" (Parquet.Read.decompressChunk LZO compressed == Right lzoInput)
  expect "LZO empty raw page uses the canonical end marker"
    (compressPageBytes LZO BS.empty == Right (BS.pack [17,0,0]))
#ifdef HAVE_BROTLI
  -- A mid-sized input so Brotli has something to chew on; the original
  -- plaintext must be recovered after a round-trip through the reader.
  let brotliInput = BS.concat (replicate 256 "the quick brown fox jumps over the lazy dog ")
  case compressPageBytes Brotli brotliInput of
    Left e  -> failTest ("Parquet.Compress: Brotli compression failed: " ++ e)
    Right compressed -> do
      expect "Brotli shrinks the repetitive fixture"
        (BS.length compressed < BS.length brotliInput)
      case Parquet.Read.decompressChunk Brotli compressed of
        Left e -> failTest ("Parquet.Read: Brotli decompression failed: " ++ e)
        Right restored ->
          expect "Brotli round-trip preserves the input"
            (restored == brotliInput)
#else
  -- Without -fbrotli the writer must surface a clear missing-codec error
  -- rather than silently falling through to uncompressed.
  case compressPageBytes Brotli "anything" of
    Left _  -> expect "Brotli reports the -fbrotli requirement" True
    Right _ -> failTest "Parquet.Compress: Brotli must fail without -fbrotli"
#endif

  -- DELTA_LENGTH_BYTE_ARRAY round-trip (encoder <-> decoder).
  let dlbaInputs =
        [ V.empty
        , V.singleton (BSC.pack "alpha")
        , V.fromList (map BSC.pack ["", "a", "ab", "abc", "abcd"])
        , V.fromList (map BSC.pack
            ["one", "tw", "three", "", "five-five-five", "6"])
        ]
  mapM_
    (\inp ->
       let encoded = encodeDeltaLengthByteArray inp
        in case decodeDeltaLengthByteArray (V.length inp) encoded of
             Right xs -> expect "DELTA_LENGTH_BYTE_ARRAY round-trip" (xs == inp)
             Left  e  -> failTest ("DELTA_LENGTH_BYTE_ARRAY decode: " ++ e))
    dlbaInputs

  -- DELTA_BYTE_ARRAY round-trip with shared prefixes (front compression).
  let dbaInputs =
        [ V.empty
        , V.singleton (BSC.pack "abc")
        , V.fromList (map BSC.pack ["", "a", "ab", "abc", "abcd", "abcde"])
        , V.fromList (map BSC.pack
            [ "iceberg/ns/tableA"
            , "iceberg/ns/tableB"
            , "iceberg/ns/tableC"
            , "iceberg/ns/zzz/last"
            , "other/ns/tableA"
            ])
        ]
  mapM_
    (\inp ->
       let encoded = encodeDeltaByteArray inp
        in case decodeDeltaByteArray (V.length inp) encoded of
             Right xs -> expect "DELTA_BYTE_ARRAY round-trip" (xs == inp)
             Left  e  -> failTest ("DELTA_BYTE_ARRAY decode: " ++ e))
    dbaInputs

  -- Golden parquet fixtures from pyarrow (only when present).
  goldenExist <- and <$> mapM doesFileExist
    [ "test/fixtures/simple_int.parquet"
    , "test/fixtures/mixed_types.parquet"
    , "test/fixtures/bloom_and_index.parquet"
    ]
  if goldenExist
    then do
      goldenSimpleInt
      goldenMixedTypes
      goldenBloomIndex
      putStrLn "OK: pyarrow golden round-trip"
    else putStrLn "SKIP: pyarrow golden fixtures not present"

  -- Validate the nested optional-list-of-optional-int writer against
  -- pyarrow if pyarrow is available on PATH; otherwise skip.
  pyarrowOk <- pyarrowAvailable
  if pyarrowOk
    then do
      validateNestedAgainstPyarrow
      validateListStructAgainstPyarrow
      validateListListAgainstPyarrow
      validateMapAgainstPyarrow
      -- Variant + Parquet pyarrow round-trip lives in
      -- wireform-iceberg's test suite (Iceberg.Variant imports
      -- wireform-iceberg, which already depends on wireform-parquet).
    else putStrLn "SKIP: pyarrow not available, nested cross-language checks skipped"

  -- Arrow ↔ Parquet bridge round-trip.
  arrowParquetBridge
  finiteVectorListFallback
  arrowParquetInputValidation
  arrowParquetIntegerTypes
  arrowParquetProjectionTypes
  arrowParquetTemporalRegression
  deltaWideIntegerRegression
  dataPageV2CompressionRegression
  malformedCountRegression
  pageChecksumRegression
  writerOptionsRegression
  decimalStatisticsRegression
  legacyDictionarySizeRegression
  wideTimestampStorageRegression
  hybridBitWidthRegression
  legacyLz4Regression
  floatingStatisticsRegression
  encryptionFailureRegression
  plainByteArrayRegression
  plainPrimitiveRegression
  plainWriterRegression
  plainArrowReadRegression

  -- Predicate evaluator unit tests
  predicateRowGroupSkipping
  predicatePageSkipping
  Selection.run
  AlpRegression.run
  WriterBounds.run
  statisticsOrderingRegression
  predicateBloomSkipping

  -- LogicalType round-trip
  logicalTypeRoundTrip
  logicalParameterRegression
  fixedDecimalMetadataRegression
  emptyColumnChunkRegression

  -- Generic per-page dispatch (DELTA, BYTE_STREAM_SPLIT, V2)
  genericPageDispatch
  manyPageRegression
  nestedReadRegression
  legacyLevelsRegression
  jsonExtensionRegression

  -- Property-style round-trips for every encoding family
  encodingRoundTripProperties

  -- Parquet.Aggregate: count(*) / count(col) / min / max
  Aggregate.run

  -- Auto-populated bloom filters via the high-level writer
  highLevelBloomFilters

  putStrLn "All Parquet page-index / bloom-filter / statistics tests passed."

-- ============================================================
-- Parquet.Predicate
-- ============================================================

-- The mixed bridge used zipWithM, silently truncating mismatched batches;
-- the dense bridge also compacted nulls without writing definition levels.
arrowParquetInputValidation :: IO ()
arrowParquetInputValidation = do
  let field = AT.Field "x" True (AT.AInt 32 True) V.empty Nothing V.empty
      schema = AT.Schema (V.singleton field) AT.Little V.empty V.empty
      col = AC.primColumn AC.PInt32 (VS.fromList [1, 2 :: Int32])
      short = AC.primColumn AC.PInt32 (VS.singleton (1 :: Int32))
      nullable = AC.fromMaybes AC.PInt32 (V.fromList [Just 1, Nothing])
      twoFields = schema {AT.arrowFields = V.fromList [field, field {AT.fieldName = "y"}]}
      checkBad label sch cols = do
        expect (label ++ " (dense)") (isLeft (PArrow.arrowToParquet sch [cols]))
        expect (label ++ " (mixed)") (isLeft (PArrow.arrowToParquetMixed sch [cols]))
  checkBad "bridge rejects missing columns" schema V.empty
  checkBad "bridge rejects extra columns" schema (V.fromList [col, col])
  checkBad "bridge rejects unequal row counts" twoFields (V.fromList [col, short])
  checkBad "bridge rejects mismatched physical types" schema (V.singleton (AC.fromTexts (V.fromList ["a", "b"])))
  checkBad "bridge rejects nulls under a required field" (schema {AT.arrowFields = V.singleton (field {AT.fieldNullable = False})}) (V.singleton nullable)
  expect "mixed bridge validates later batches"
    (isLeft (PArrow.arrowToParquetMixed schema [V.singleton col, V.empty]))
  expect "dense bridge rejects null compaction" (isLeft (PArrow.arrowToParquet schema [V.singleton nullable]))
  case PArrow.arrowToParquetMixed schema [V.singleton nullable] of
    Left e -> failTest ("mixed nullable bridge: " ++ e)
    Right (ps, groups) -> do
      let bytes = PHL.encodeParquetMixed PHL.defaultWriteOptions ps groups
      case PHL.decodeParquet PHL.defaultReadOptions bytes of
        Left e -> failTest ("mixed nullable footer: " ++ e)
        Right pf -> expect "mixed bridge preserves null rows"
          (PArrow.parquetRowGroupToArrow schema pf 0 == Right (V.singleton nullable))


arrowParquetTemporalRegression :: IO ()
arrowParquetTemporalRegression = do
  let field typ nullable = AT.Field "x" nullable typ V.empty Nothing V.empty
      schema fld = AT.Schema (V.singleton fld) AT.Little V.empty V.empty
      check typ input expected = do
        let fld = field typ False
        case PArrow.arrowToParquet (schema fld) [V.singleton input] of
          Left err -> failTest err
          Right (ps, groups) -> case loadParquetFile (PHL.encodeParquet PHL.defaultWriteOptions ps groups) of
            Left err -> failTest err
            Right pf -> expect ("temporal payload and annotation agree: " ++ show typ)
              (PArrow.parquetRowGroupToArrow (PArrow.parquetFileArrowSchema pf) pf 0
                == Right (V.singleton expected))
      rejects typ input = do
        let dense = PArrow.arrowToParquet (schema (field typ False)) [V.singleton input]
            mixed = PArrow.arrowToParquetMixed (schema (field typ True)) [V.singleton input]
        expect ("dense temporal writer rejects invalid values/type: " ++ show typ) (isLeft dense)
        expect ("nullable temporal writer rejects invalid values/type: " ++ show typ) (isLeft mixed)
  check (AT.ADate AT.DateMillisecond)
    (AC.primColumn AC.PDate64 (VS.fromList [-86400000, 0, 86400000]))
    (AC.primColumn AC.PDate32 (VS.fromList [-1, 0, 1]))
  check (AT.ATime AT.Second 32)
    (AC.primColumn AC.PTime32 (VS.fromList [0, 1, 86399]))
    (AC.primColumn AC.PTime32 (VS.fromList [0, 1000, 86399000]))
  check (AT.ATimestamp AT.Second Nothing)
    (AC.primColumn AC.PTimestamp (VS.fromList [-2, 0, 1700000000]))
    (AC.primColumn AC.PTimestamp (VS.fromList [-2000, 0, 1700000000000]))
  let nanos = AC.primColumn AC.PTime64 (VS.fromList [0, 1, 86399999999999])
  check (AT.ATime AT.Nanosecond 64) nanos nanos
  rejects (AT.ADate AT.DateMillisecond) (AC.primColumn AC.PDate64 (VS.singleton 1))
  rejects (AT.ADate AT.DateMillisecond) (AC.primColumn AC.PDate64 (VS.singleton maxBound))
  rejects (AT.ATime AT.Second 32) (AC.primColumn AC.PTime32 (VS.singleton 86400))
  rejects (AT.ATime AT.Millisecond 32) (AC.primColumn AC.PTime32 (VS.singleton (-1)))
  rejects (AT.ATimestamp AT.Second Nothing) (AC.primColumn AC.PTimestamp (VS.singleton maxBound))
  rejects (AT.ATimestamp AT.Second Nothing) (AC.primColumn AC.PTimestamp (VS.singleton minBound))
  rejects (AT.ADuration AT.Microsecond) (AC.primColumn AC.PDuration (VS.singleton 1))
  let fld = field (AT.ATimestamp AT.Second Nothing) True
      values = AC.fromMaybes AC.PTimestamp (V.fromList [Just (-1), Nothing, Just 2])
  case PArrow.arrowToParquetMixed (schema fld) [V.singleton values] of
    Left err -> failTest err
    Right (ps, groups) -> case loadParquetFile (PHL.encodeParquetMixed PHL.defaultWriteOptions ps groups) of
      Left err -> failTest err
      Right pf -> expect "nullable timestamp seconds scale while preserving nulls"
        (PArrow.parquetRowGroupToArrow (PArrow.parquetFileArrowSchema pf) pf 0
          == Right (V.singleton (AC.fromMaybes AC.PTimestamp (V.fromList [Just (-1000), Nothing, Just 2000]))))


plainArrowReadRegression :: IO ()
plainArrowReadRegression = do
  let field = AT.Field "x" False AT.AUtf8 V.empty Nothing V.empty
      schema = AT.Schema (V.fromList
        [ field
        , AT.Field "i32" False (AT.AInt 32 True) V.empty Nothing V.empty
        , AT.Field "i64" False (AT.AInt 64 True) V.empty Nothing V.empty
        , AT.Field "f64" False (AT.AFloatingPoint AT.DoublePrecision) V.empty Nothing V.empty
        , AT.Field "bool" False AT.ABool V.empty Nothing V.empty
        ]) AT.Little V.empty V.empty
      values = AC.fromTexts (V.fromList ["", "lambda λ", "emoji 😀", "\NUL"])
      batch = V.fromList
        [ values
        , AC.primColumn AC.PInt32 (VS.fromList [-1, 0, maxBound, minBound])
        , AC.primColumn AC.PInt64 (VS.fromList [minBound, maxBound, 1, 0])
        , AC.primColumn AC.PDouble (VS.fromList [1.5, -2.5, 0, -0.0])
        , AC.fromBools (V.fromList [True, False, True, True])
        ]
  case PArrow.arrowToParquet schema [batch] of
    Left e -> failTest e
    Right (ps, groups) ->
      mapM_ (\version -> mapM_ (\codec -> do
        let bytes = PHL.encodeParquet
              (PHL.defaultWriteOptions {PHL.writeCompression = codec, PHL.writePageVersion = version}) ps groups
        case loadParquetFile bytes of
          Left e -> failTest e
          Right pf -> expect ("direct Arrow string buffers " ++ show version ++ " " ++ show codec)
            (PArrow.parquetRowGroupToArrow schema pf 0 == Right batch))
        [Uncompressed, Snappy, ZSTD, LZ4Raw]) [PageV1, PageV2]
  let bad = BS.pack [0xc3]
      ps = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
        , SchemaElement "x" (Just Required) (Just PTByteArray) Nothing (Just CTUtf8) (Just LTString) Nothing Nothing
        ]
      badRequired = buildParquetFile ps (V.singleton (V.singleton (ColByteArray (V.singleton bad))))
      checkInvalid label bytes = case loadParquetFile bytes of
        Left e -> failTest e
        Right pf -> expect label
          (isLeft (PArrow.parquetRowGroupToArrow (PArrow.parquetFileArrowSchema pf) pf 0))
  checkInvalid "direct Arrow string reader rejects invalid UTF-8" badRequired
  let optional = ps V.// [(1, (ps V.! 1) {seRepetition = Just Optional})]
      badOptional = PHL.encodeParquetMixed PHL.defaultWriteOptions optional
        [V.singleton (PHL.PCOptional (OptByteArray (V.fromList [Just bad, Nothing])))]
  checkInvalid "generic Arrow string reader rejects invalid UTF-8" badOptional


plainWriterRegression :: IO ()
plainWriterRegression = do
  let payload = snd . encodeColumnDataPageParts
      ints = VP.slice 1 2 (VP.fromList [999, 1, -1, 999 :: Int32])
      longs = VP.slice 1 2 (VP.fromList [999, minBound, maxBound, 999 :: Int64])
      floats = VP.slice 1 2 (VP.fromList [0, 1.5, castWord32ToFloat 0x7fa00001, 0])
  expect "PLAIN Int32 writes only the logical vector slice"
    (payload (ColInt32 ints) == BS.pack [1, 0, 0, 0, 255, 255, 255, 255])
  expect "PLAIN Int64 preserves little-endian extreme values in a slice"
    (payload (ColInt64 longs) == BS.pack (replicate 7 0 ++ [128] ++ replicate 7 255 ++ [127]))
  expect "PLAIN Float preserves IEEE payload bits in a slice"
    (payload (ColFloat floats) == BS.pack [0, 0, 192, 63, 1, 0, 160, 127])
  mapM_ (\n -> do
    let values = V.generate n (\i -> i `rem` 3 == 0)
    expect ("PLAIN Bool tail length " ++ show n)
      (Parquet.Read.decodePlainBool n (payload (ColBool values)) == Right values)) [0 .. 65]
  let schema = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
        , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
        ]
      bytes = PHL.encodeParquet
        (PHL.defaultWriteOptions {PHL.writeCompression = Uncompressed, PHL.writePageVersion = PageV2})
        schema [V.singleton (ColInt32 ints)]
  case loadParquetFile bytes of
    Left e -> failTest e
    Right pf -> do
      let chunk = rgColumns (fmRowGroups (pfFooter pf) V.! 0) V.! 0
      expect "V2 uncompressed page records equal compressed/uncompressed sizes"
        (maybe False (\m -> cmCodec m == Uncompressed
                        && cmTotalUncompressedSize m == cmTotalCompressedSize m) (ccMetadata chunk))
      expect "V2 uncompressed page remains readable"
        (PArrow.parquetRowGroupToArrow (PArrow.parquetFileArrowSchema pf) pf 0
          == Right (V.singleton (AC.primColumn AC.PInt32 (VS.fromList [1, -1]))))


plainByteArrayRegression :: IO ()
plainByteArrayRegression = do
  expect "BYTE_ARRAY rejects a negative count"
    (isLeft (Parquet.Read.decodePlainByteArray (-1) BS.empty))
  expect "BYTE_ARRAY rejects an impossible count before allocating"
    (isLeft (Parquet.Read.decodePlainByteArray maxBound (BS.replicate 4 0)))
  expect "BYTE_ARRAY rejects a truncated length"
    (isLeft (Parquet.Read.decodePlainByteArray 1 (BS.pack [0, 0, 0])))
  expect "BYTE_ARRAY rejects a truncated payload"
    (isLeft (Parquet.Read.decodePlainByteArray 1 (BS.pack [2, 0, 0, 0, 1])))
  let values = V.generate 4096 (\i -> BS.replicate (i `rem` 17) (fromIntegral i))
      (_, body) = encodeColumnDataPageParts (ColByteArray values)
  expect "BYTE_ARRAY preserves all values in a large page"
    (Parquet.Read.decodePlainByteArray (V.length values) body == Right values)
  ok <- H.check $ H.property $ do
    input <- H.forAll (Gen.list (Range.linear 0 200) (Gen.bytes (Range.linear 0 128)))
    let columns = V.fromList input
        (_, payload) = encodeColumnDataPageParts (ColByteArray columns)
    Parquet.Read.decodePlainByteArray (V.length columns) payload H.=== Right columns
  unless ok exitFailure


plainPrimitiveRegression :: IO ()
plainPrimitiveRegression = do
  -- Literal little-endian payloads also cover NaN payloads and negative zero;
  -- compare floating bits rather than IEEE equality.
  let words32 = VP.fromList [0, 0x80000000, 0x7fc01234, 0x7f800000, 0xffffffff] :: VP.Vector Word32
      words64 = VP.fromList [0, 0x8000000000000000, 0x7ff8123456789abc, 0x7ff0000000000000, 0xffffffffffffffff] :: VP.Vector Word64
      bytes32 = BS.pack . concatMap (\n -> (\s -> fromIntegral (n `shiftR` s)) <$> [0,8,16,24]) . VP.toList
      bytes64 = BS.pack . concatMap (\n -> (\s -> fromIntegral (n `shiftR` s)) <$> [0,8,16,24,32,40,48,56]) . VP.toList
      check32 input expected = do
        (VP.map fromIntegral <$> Parquet.Read.decodePlainInt32 (VP.length expected) input) H.=== Right expected
        (VP.map castFloatToWord32 <$> Parquet.Read.decodePlainFloat (VP.length expected) input) H.=== Right expected
      check64 input expected = do
        (VP.map fromIntegral <$> Parquet.Read.decodePlainInt64 (VP.length expected) input) H.=== Right expected
        (VP.map castDoubleToWord64 <$> Parquet.Read.decodePlainDouble (VP.length expected) input) H.=== Right expected
  ok <- H.check $ H.property $ do
    prefix <- H.forAll (Gen.int (Range.linear 1 7))
    input32 <- VP.fromList <$> H.forAll (Gen.list (Range.linear 0 128) (Gen.word32 Range.constantBounded))
    input64 <- VP.fromList <$> H.forAll (Gen.list (Range.linear 0 128) (Gen.word64 Range.constantBounded))
    let unaligned payload = BS.drop prefix (BS.replicate prefix 0x5a <> payload <> BS.replicate 9 0xa5)
    check32 (unaligned (bytes32 words32)) words32
    check64 (unaligned (bytes64 words64)) words64
    check32 (unaligned (bytes32 input32)) input32
    check64 (unaligned (bytes64 input64)) input64
  unless ok exitFailure


arrowParquetProjectionTypes :: IO ()
arrowParquetProjectionTypes = do
  let field = AT.Field "x" True (AT.AInt 32 True) V.empty Nothing V.empty
      schema = AT.Schema (V.singleton field) AT.Little V.empty V.empty
      col = AC.primColumn AC.PInt32 (VS.fromList [-1, 2 :: Int32])
  case PArrow.arrowToParquet schema [V.singleton col] of
    Left e -> failTest e
    Right (ps, groups) -> do
      let bytes = PHL.encodeParquet PHL.defaultWriteOptions ps groups
      case PHL.decodeParquet PHL.defaultReadOptions bytes of
        Left e -> failTest e
        Right pf -> do
          let native = PArrow.parquetFileArrowSchema pf
              check ty = expect ("projection rejects reinterpretation as " ++ show ty)
                (isLeft (PArrow.parquetRowGroupToArrow
                  (native {AT.arrowFields = V.singleton (field {AT.fieldType = ty})}) pf 0))
          expect "dense bridge emits required schema without definition levels"
            (not (AT.fieldNullable (AT.arrowFields native V.! 0)))
          expect "dense bridge preserves all-present values from nullable schema"
            (PArrow.parquetRowGroupToArrow native pf 0 == Right (V.singleton col))
          mapM_ check [AT.ABool, AT.AFloatingPoint AT.Single, AT.AInt 64 False]


encryptionFailureRegression :: IO ()
encryptionFailureRegression = do
  let key = BS.replicate 16 0x42
      column = ColInt32 (VP.fromList [1, 2 :: Int32])
      schema = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
        , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
        ]
      groups = V.singleton (V.singleton column)
      footer = FooterEncryption key "fresh-id" BS.empty BS.empty
      ce = ColumnEncryption Enc.AesGcmV1 key "fresh-id" BS.empty BS.empty 0
      aux enc ver = V.singleton (V.singleton (emptyColumnAux {caEncryption = Just enc, caPageVersion = ver}))
      badColumn = ce {ceKey = "bad"}
      badFooter = footer {feKey = "bad"}
  mapM_ (\ver -> do
    result <- buildParquetFileWithIndexEncryptedFooter footer schema groups (aux badColumn ver)
    expect "invalid column key rejects rather than producing plaintext" (isLeft result)) [PageV1, PageV2]
  badFooterResult <- buildParquetFileWithIndexEncryptedFooter badFooter schema groups V.empty
  expect "invalid footer key rejects rather than producing plaintext" (isLeft badFooterResult)
  unsignedFooter <- buildParquetFileWithIndexIO schema groups (aux ce PageV1)
  expect "encrypted columns reject unsupported unsigned plaintext footer" (isLeft unsignedFooter)
  auxResult <- encryptAuxModule (Just badColumn) Enc.ModuleColumnIndex 0 "secret-index"
  expect "auxiliary encryption failure returns Left" (isLeft auxResult)
  pureResult <- try (evaluate (BS.length (buildParquetFileWithIndex schema groups (aux ce PageV1)))) :: IO (Either SomeException Int)
  expect "pure writer rejects encrypted configuration and directs caller to IO" (isLeft pureResult)
  let opts = PHL.defaultWriteOptions {PHL.writeColumnEncryption = V.singleton (Just ce)}
      mixed = V.singleton (PCRequired column)
  mixedResult <- try (evaluate (BS.length (PHL.encodeParquetMixed opts schema [mixed]))) :: IO (Either SomeException Int)
  expect "mixed writer never ignores requested encryption" (isLeft mixedResult)
  emptyResult <- try (evaluate (BS.length (PHL.encodeParquet opts schema []))) :: IO (Either SomeException Int)
  expect "pure high-level writer rejects encryption even without row groups" (isLeft emptyResult)
  first <- Enc.encryptGcmModuleFramed key "same-aad" "first" >>= either failTest pure
  second <- Enc.encryptGcmModuleFramed key "same-aad" "second" >>= either failTest pure
  expect "repeated AAD does not reuse GCM nonces"
    (BS.take 12 (BS.drop 4 first) /= BS.take 12 (BS.drop 4 second))
  expect "explicit nonce API rejects short nonce"
    (isLeft (Enc.encryptGcmModuleWithNonce key "short" "aad" "secret"))
  mapM_ (\offset -> expect "encrypted framing rejects invalid offsets"
    (isLeft (Enc.readFramedModule first offset))) [-1, maxBound]
  missing <- buildParquetFileWithIndexIO schema (V.singleton V.empty) V.empty
  expect "checked writer rejects missing columns" (isLeft missing)
  wrongType <- buildParquetFileWithIndexIO schema
    (V.singleton (V.singleton (ColInt64 (VP.fromList [1, 2])))) V.empty
  expect "checked writer rejects physical type mismatch" (isLeft wrongType)
  let twoFields = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 2) Nothing Nothing Nothing Nothing
        , schema V.! 1
        , (schema V.! 1) {seName = "y"}
        ]
  differentLengths <- buildParquetFileWithIndexIO twoFields
    (V.singleton (V.fromList [column, ColInt32 (VP.singleton 1)])) V.empty
  expect "checked writer rejects unequal column row counts" (isLeft differentLengths)
  extraAux <- buildParquetFileWithIndexIO schema groups (V.singleton (V.replicate 2 emptyColumnAux))
  expect "checked writer rejects unmatched auxiliary columns" (isLeft extraAux)


floatingStatisticsRegression :: IO ()
floatingStatisticsRegression = do
  let nanF = castWord32ToFloat 0x7fc00001
      nanD = realToFrac nanF :: Double
      floatStats xs = columnDataStatistics (ColFloat (VP.fromList xs))
      doubleStats xs = columnDataStatistics (ColDouble (VP.fromList xs))
      floatNaNs = [[nanF, -2, 3], [-2, nanF, 3], [-2, 3, nanF]]
      doubleNaNs = [[nanD, -2, 3], [-2, nanD, 3], [-2, 3, nanD]]
  mapM_ (\xs -> do
    let stats = floatStats xs
    expect "FLOAT statistics ignore NaN at every position"
      (statNanCount stats == Just 1
        && (statMinValue stats >>= Pred.decodePValueLE PTFloat) == Just (Pred.PVFloat (-2))
        && (statMaxValue stats >>= Pred.decodePValueLE PTFloat) == Just (Pred.PVFloat 3))) floatNaNs
  mapM_ (\xs -> do
    let stats = doubleStats xs
    expect "DOUBLE statistics ignore NaN at every position"
      (statNanCount stats == Just 1
        && (statMinValue stats >>= Pred.decodePValueLE PTDouble) == Just (Pred.PVDouble (-2))
        && (statMaxValue stats >>= Pred.decodePValueLE PTDouble) == Just (Pred.PVDouble 3))) doubleNaNs
  mapM_ (\stats -> expect "all-NaN columns omit every min/max bound and retain NaN count"
    (statMin stats == Nothing && statMax stats == Nothing
      && statMinValue stats == Nothing && statMaxValue stats == Nothing
      && statNullCount stats == Just 0 && statNanCount stats == Just 2))
    [floatStats [nanF, nanF], doubleStats [nanD, nanD]]
  mapM_ (\stats -> expect "empty floating columns explicitly report zero NaNs"
    (statNanCount stats == Just 0 && statMinValue stats == Nothing)) [floatStats [], doubleStats []]
  mapM_ (\xs -> do
    let stats = floatStats xs
    expect "FLOAT zero bounds are canonical -0 and +0"
      (statMinValue stats == Just (BS.pack [0, 0, 0, 0x80])
        && statMaxValue stats == Just (BS.replicate 4 0))) [[0], [-0.0], [0, -0.0]]
  mapM_ (\xs -> do
    let stats = doubleStats xs
    expect "DOUBLE zero bounds are canonical -0 and +0"
      (statMinValue stats == Just (BS.pack [0, 0, 0, 0, 0, 0, 0, 0x80])
        && statMaxValue stats == Just (BS.replicate 8 0))) [[0], [-0.0], [0, -0.0]]
  let nanBytes = BS.pack [1, 0, 0xc0, 0x7f]
      badStats = (floatStats [1]) {statMin = Just nanBytes, statMinValue = Just nanBytes}
  expect "predicate decoder ignores legacy NaN bounds" (Pred.decodePValueLE PTFloat nanBytes == Nothing)
  let floatNegativeZero = BS.pack [0, 0, 0, 0x80]
      doubleNegativeZero = BS.pack [0, 0, 0, 0, 0, 0, 0, 0x80]
      bloom bytes = sbbfInsert bytes (newSbbf 256)
  expect "FLOAT bloom lookup preserves = +0 matches in a -0-only column"
    (Pred.evalBloomChunk (physicalField PTFloat) (bloom floatNegativeZero) (Pred.PEq (Pred.PVFloat 0)) == Pred.PMaybeKeep)
  expect "FLOAT bloom lookup preserves = -0 matches in a +0-only column"
    (Pred.evalBloomChunk (physicalField PTFloat) (bloom (BS.replicate 4 0)) (Pred.PEq (Pred.PVFloat (-0.0))) == Pred.PMaybeKeep)
  expect "DOUBLE bloom lookup preserves both zero encodings"
    (Pred.evalBloomChunk (physicalField PTDouble) (bloom doubleNegativeZero) (Pred.PIn [Pred.PVDouble 0]) == Pred.PMaybeKeep
      && Pred.evalBloomChunk (physicalField PTDouble) (bloom (BS.replicate 8 0)) (Pred.PEq (Pred.PVDouble (-0.0))) == Pred.PMaybeKeep)
  let schema = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 2) Nothing Nothing Nothing Nothing
        , SchemaElement "f" (Just Required) (Just PTFloat) Nothing Nothing Nothing Nothing Nothing
        , SchemaElement "g" (Just Required) (Just PTFloat) Nothing Nothing Nothing Nothing Nothing
        ]
      columns = V.fromList [ColFloat (VP.fromList [nanF, -2, 3]), ColFloat (VP.fromList [1, 2, 3])]
      bytes = buildParquetFile schema (V.singleton columns)
  file <- either failTest pure (loadParquetFile bytes)
  let metadata = pfFooter file
      statsAt fm idx = ccMetadata (rgColumns (fmRowGroups fm V.! 0) V.! idx) >>= cmStatistics
  expect "footer round-trip preserves NaN counts"
    (fmap statNanCount (statsAt metadata 0) == Just (Just 1)
      && fmap statNanCount (statsAt metadata 1) == Just (Just 0))
  expect "writer declares TYPE_ORDER per leaf"
    (fmColumnOrders metadata == Just (V.replicate 2 TypeDefinedOrder))
  let original = rgColumns (fmRowGroups metadata V.! 0) V.! 0
      withBadStats = original {ccMetadata = fmap (\md -> md {cmStatistics = Just badStats}) (ccMetadata original)}
  expect "legacy NaN minimum cannot cause false range pruning"
    (Pred.evalColumnChunk (knownContext (physicalField PTFloat)) withBadStats (Pred.PLt (Pred.PVFloat 2)) == Pred.PMaybeKeep)
  let indexedMetadata = metadata {fmRowGroups = V.map (\rg -> rg
        {rgColumns = V.imap (\i cc -> cc
          {ccColumnIndexOffset = Just (100 + fromIntegral i), ccColumnIndexLength = Just 8})
          (rgColumns rg)}) (fmRowGroups metadata)}
      rawOrders orders = case Footer.fileMetadataToThrift indexedMetadata of
        TV.Struct fields -> TV.Struct (V.map (\field -> case field of
          TS.FileMetadata_ColumnOrders _ -> TS.FileMetadata_ColumnOrders orders
          other -> other) fields)
        value -> value
      unknown = TV.Struct (V.singleton (99, TV.Struct V.empty))
      known = TV.Struct (V.singleton (1, TV.Struct V.empty))
  unknownFile <- either failTest pure (Footer.readFooterRaw (Thrift.encodeCompact (rawOrders (V.fromList [unknown, known]))))
  expect "unknown column ordering preserves vector positions"
    (fmColumnOrders unknownFile == Just (V.fromList [UnsupportedColumnOrder, TypeDefinedOrder]))
  expect "unknown ordering invalidates only the affected bounds"
    ((statsAt unknownFile 0 >>= statMinValue) == Nothing
      && (statsAt unknownFile 1 >>= statMinValue) == (statsAt metadata 1 >>= statMinValue))
  let indexedColumns = rgColumns (fmRowGroups unknownFile V.! 0)
  expect "unknown ordering disables only the affected column index"
    (ccColumnIndexOffset (indexedColumns V.! 0) == Nothing
      && ccColumnIndexOffset (indexedColumns V.! 1) == Just 101)
  expect "column-order count must match leaf count"
    (isLeft (Footer.readFooterRaw (Thrift.encodeCompact (rawOrders V.empty))))
  let knownOrders = metadata
        { fmNumRows = 0
        , fmRowGroups = V.empty
        , fmSchema = schema V.// [(2, (schema V.! 2) {seType = Just PTInt96})]
        , fmColumnOrders = Just (V.fromList [IEEE754TotalOrder, Int96TimestampOrder])
        }
  parsedOrders <- either failTest pure
    (Footer.readFooterRaw (Thrift.encodeCompact (Footer.fileMetadataToThrift knownOrders)))
  expect "modern floating and INT96 column-order variants round-trip"
    (fmColumnOrders parsedOrders == fmColumnOrders knownOrders)
  let rowGroup = fmRowGroups metadata V.! 0
      negativeStats = original {ccMetadata = fmap
        (\md -> md {cmStatistics = fmap (\st -> st {statNanCount = Just (-1)}) (cmStatistics md)})
        (ccMetadata original)}
      negativeMetadata = metadata {fmRowGroups = V.singleton
        (rowGroup {rgColumns = rgColumns rowGroup V.// [(0, negativeStats)]})}
  expect "negative NaN counts are rejected"
    (isLeft (Footer.readFooterRaw (Thrift.encodeCompact (Footer.fileMetadataToThrift negativeMetadata))))
  mapM_ (\edit -> do
    let changed = original {ccMetadata = fmap
          (\md -> md {cmStatistics = edit <$> cmStatistics md}) (ccMetadata original)}
        excessive = metadata {fmRowGroups = V.singleton
          (rowGroup {rgColumns = rgColumns rowGroup V.// [(0, changed)]})}
    expect "impossible NaN/null counts are rejected"
      (isLeft (Footer.readFooterRaw (Thrift.encodeCompact (Footer.fileMetadataToThrift excessive)))))
    [ (\st -> st {statNanCount = Just 4})
    , (\st -> st {statNullCount = Just 4})
    , (\st -> st {statNullCount = Just 2, statNanCount = Just 2})
    ]


legacyLz4Regression :: IO ()
#ifdef HAVE_LZ4
legacyLz4Regression = do
  let literal = BS.pack [0x50, 104, 101, 108, 108, 111]
      lengthBE n = BS.pack [0, 0, 0, n]
      chunk = lengthBE 6 <> literal
      framed = lengthBE 5 <> chunk
      splitBlock = lengthBE 10 <> chunk <> chunk
      decode n payload = do
        let header = PageHeader (PtDataPage (DataPageHeader 1 0))
              (Just n) (Just (fromIntegral (BS.length payload)))
        (_, _, raw, _) <- Parquet.Read.decompressDataPageBody LZ4
          (encodePageHeader header <> payload) 0
        Right raw
  expect "legacy LZ4 accepts Hadoop framing" (decode 5 framed == Right "hello")
  expect "legacy LZ4 accepts several chunks per block" (decode 10 splitBlock == Right "hellohello")
  expect "legacy LZ4 accepts several Hadoop blocks" (decode 10 (framed <> framed) == Right "hellohello")
  expect "legacy LZ4 accepts old parquet-cpp raw blocks" (decode 5 literal == Right "hello")
  mapM_ (\payload -> expect "legacy LZ4 rejects malformed framing"
      (isLeft (decode 5 payload)))
    [ BS.take 3 framed, BS.init framed, lengthBE 6 <> chunk
    , lengthBE 5 <> lengthBE 255 <> literal, framed <> BS.singleton 0
    ]
#else
legacyLz4Regression = pure ()
#endif


hybridBitWidthRegression :: IO ()
hybridBitWidthRegression = do
  let golden = BS.pack [9, 3, 0, 254, 7, 240, 63, 0, 224, 191, 254]
  expect "9-bit dictionary indices preserve values across aligned group ends"
    (Parquet.Read.decodeDictionaryIndices 8 golden == Right (VP.fromList [0,511,1,510,3,256,255,509]))
  mapM_ (\width -> do
    let mask = ((1 :: Word64) `shiftL` width) - 1
        input = VP.generate 259 (\i -> fromIntegral ((fromIntegral i * 104729 + mask) .&. mask))
        encoded = LE.encodeRLEHybrid width input
    expect ("hybrid bit packing round-trips full width " ++ show width)
      (Parquet.Read.decodeHybridRleUnsigned32 width (VP.length input) encoded == Right input)) [1..32]
  expect "hybrid decoder rejects zero-length packed run"
    (isLeft (Parquet.Read.decodeHybridRleUnsigned32 9 1 (BS.singleton 1)))
  expect "hybrid decoder rejects empty input before allocating a huge output"
    (isLeft (Parquet.Read.decodeHybridRleUnsigned32 1 maxBound BS.empty))
  expect "dictionary decoder rejects a missing stream before allocating"
    (isLeft (Parquet.Read.decodeDictionaryIndices maxBound (BS.singleton 1)))
  expect "zero-width hybrid streams need no input bytes"
    (Parquet.Read.decodeHybridRleUnsigned32 0 3 BS.empty == Right (VP.replicate 3 0))
  mapM_ (\width -> do
    -- Each prefix is valid but cannot cover the advertised count. In
    -- particular, scanning only the first header would miss the bad suffix.
    let repeated = BS.singleton 2 <> BS.replicate ((width + 7) `quot` 8) 0
        packed = BS.singleton 3 <> BS.replicate width 0
        suffixes = [BS.empty, BS.singleton 0, BS.singleton 1, BS.singleton 128,
          BS.replicate 10 255, BS.singleton 3, BS.singleton 2]
    mapM_ (\prefix -> mapM_ (\suffix ->
      expect "hybrid decoder validates later runs before allocating a huge output"
        (isLeft (Parquet.Read.decodeHybridRleUnsigned32 width 100000000 (prefix <> suffix))))
      suffixes) [BS.empty, repeated, packed, repeated <> packed]) [1..32]
  expect "hybrid output size must not overflow the primitive vector byte count"
    (isLeft (Parquet.Read.decodeHybridRleUnsigned32 0 maxBound BS.empty))
  let longRepeated = BS.pack [128,128,8,1] -- 65536 repeated one-bit values.
  expect "valid long compressed runs remain supported"
    (Parquet.Read.decodeHybridRleUnsigned32 1 65536 longRepeated == Right (VP.replicate 65536 1))
  expect "hybrid validation stops at the requested count, before unused suffixes"
    (Parquet.Read.decodeHybridRleUnsigned32 1 3 (BS.pack [6,1,128]) == Right (VP.replicate 3 1))
  expect "hybrid decoder rejects overflowing run headers"
    (isLeft (Parquet.Read.decodeHybridRleUnsigned32 9 1 (BS.replicate 10 0xff)))
  expect "hybrid decoder checks packed size without multiplication overflow"
    (isLeft (Parquet.Read.decodeHybridRleUnsigned32 32 1 (BS.pack [0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,1])))
  ok <- H.check $ H.withTests 300 $ H.property $ do
    width <- H.forAll (Gen.element [1,2,4,8])
    groups <- H.forAll (Gen.int (Range.linear 1 96))
    packed <- H.forAll (Gen.bytes (Range.singleton (groups * width)))
    count <- H.forAll (Gen.int (Range.linear 0 (groups * 8)))
    padding <- H.forAll (Gen.bytes (Range.linear 1 16))
    repeated <- H.forAll (Gen.word8 Range.constantBounded)
    let run = groups * 2 + 1
        runHeader = BS.pack $ if run < 128 then [fromIntegral run]
          else [fromIntegral (run .&. 127) .|. 128, fromIntegral (run `shiftR` 7)]
        payload = runHeader <> packed
        sliced = BS.drop (BS.length padding) (padding <> payload)
        -- Independent bit-position oracle, rather than the format's encoder.
        at index = foldr (\bit acc ->
          let absolute = index * width + bit
              byte = BS.index packed (absolute `quot` 8)
          in if testBit byte (absolute `rem` 8) then setBit acc bit else acc)
          (0 :: Int32) [0 .. width - 1]
        values = VP.generate (groups * 8) at
        mask = fromIntegral ((1 `shiftL` width) - 1 :: Int) :: Word8
        first = repeated .&. mask
        lastValue = first `xor` mask
        mixed = BS.pack [6, first] <> payload <> BS.pack [4, lastValue]
        expected = VP.replicate 3 (fromIntegral first) <> values <> VP.replicate 2 (fromIntegral lastValue)
    Parquet.Read.decodeHybridRleUnsigned32 width count sliced H.=== Right (VP.take count values)
    Parquet.Read.decodeHybridRleUnsigned32 width (VP.length expected) mixed H.=== Right expected
    when (count > 0) $ H.assert (isLeft (Parquet.Read.decodeHybridRleUnsigned32 width count (BS.init payload)))
  unless ok exitFailure


decimalStatisticsRegression :: IO ()
decimalStatisticsRegression = do
  let pairs = V.fromList
        [(-129,BS.pack [255,127]),(-128,BS.pack [128]),(-1,BS.pack [255,255]),
         (0,BS.pack [0]),(1,BS.pack [1]),(127,BS.pack [127]),(128,BS.pack [0,128]),
         (256,BS.pack [1,0]),(1000,BS.pack [3,232])] :: V.Vector (Integer,BS.ByteString)
      field = (physicalField PTByteArray) {seName = "x",seLogicalType = Just (LTDecimal 6 2),seConvertedType = Just CTDecimal}
      schema = V.fromList [SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing,field]
      bytes = PHL.encodeParquet (PHL.defaultWriteOptions {PHL.writeCompression = Uncompressed}) schema
        [V.singleton (ColByteArray (snd <$> pairs))]
  file <- either failTest pure (loadParquetFile bytes)
  let metadata = pfFooter file
      group = V.head (fmRowGroups metadata)
      column = V.head (rgColumns group)
  stats <- maybe (failTest "missing decimal statistics") pure (ccMetadata column >>= cmStatistics)
  expect "DECIMAL byte statistics use signed numeric ordering across widths"
    (statMinValue stats == Just (BS.pack [255,127]) && statMaxValue stats == Just (BS.pack [3,232])
      && statMin stats == Nothing && statMax stats == Nothing)
  expect "DECIMAL aggregate min/max use exact numeric values"
    (PAgg.columnMin metadata 0 == Just (Pred.PVDecimal ((-129) % 100))
      && PAgg.columnMax metadata 0 == Just (Pred.PVDecimal 10))
  ci <- either failTest pure (readColumnIndex file 0 0)
  index <- maybe (failTest "missing decimal page index") pure ci
  V.mapM_ (\(number,_) -> do
    let predicate = Pred.PEq (Pred.PVDecimal (number % 100))
    expect "decimal row-group pruning keeps every present value"
      (Pred.evalRowGroup metadata (Pred.PCol "x" predicate) group == Pred.PMaybeKeep)
    expect "decimal column/page pruning keeps every present value"
      (Pred.evalColumnChunk (knownContext field) column predicate == Pred.PMaybeKeep
        && Pred.evalPagesByColumnIndex (knownContext field) index predicate == V.singleton Pred.PMaybeKeep)) pairs
  expect "decimal pruning rejects a value outside its numeric bounds"
    (Pred.evalRowGroup metadata (Pred.PCol "x" (Pred.PGt (Pred.PVDecimal 20))) group == Pred.PSkip)
  let unsigned = (physicalField PTInt32) {seLogicalType = Just (LTInteger 32 False)}
  expect "unsigned statistics retain the unsigned range"
    (Pred.decodePValueForSchema unsigned (BS.replicate 4 255) == Just (Pred.PVUnsigned 4294967295))
  expect "cross-type scalar comparisons never invent equality"
    (not (SharedPred.pvEq (Pred.PVInt32 1) (Pred.PVText "one"))
      && SharedPred.evalRange (Pred.PVInt32 1) (Pred.PVInt32 2) (Pred.PLt (Pred.PVDouble 100)) == Pred.PMaybeKeep)
  mapM_ (\(ty,bytes) -> expect "malformed physical bounds are never used for pruning"
    (Pred.decodePValueForSchema (physicalField ty) bytes == Nothing))
    [(PTInt32,BS.replicate 5 0),(PTInt64,BS.replicate 9 0),(PTFloat,BS.replicate 5 0),
     (PTDouble,BS.replicate 9 0),(PTBoolean,BS.pack [2]),(PTBoolean,BS.pack [1,0])]
  let halfField = (physicalField PTFixedLenByteArray) {seTypeLength = Just 2,seLogicalType = Just LTFloat16}
  expect "FLOAT16 statistics use numeric floating-point order"
    (Pred.decodePValueForSchema halfField (BS.pack [0,188]) == Just (Pred.PVDouble (-1))
      && Pred.decodePValueForSchema halfField (BS.pack [0,60]) == Just (Pred.PVDouble 1)
      && Pred.decodePValueForSchema halfField (BS.pack [1,124]) == Nothing)
  expect "unsupported logical/physical combinations cannot supply ordered bounds"
    (Pred.decodePValueForSchema (unsigned {seType = Just PTInt64}) (BS.replicate 8 0) == Nothing)
  let enumField = field {seLogicalType = Just LTEnum,seConvertedType = Just CTEnum}
      enumFile = Parquet.Read.ParquetFile BS.empty (metadata {fmSchema = V.fromList [schema V.! 0,enumField]})
  expect "ENUM annotations preserve their UTF-8 semantics"
    (AT.fieldType (V.head (AT.arrowFields (PArrow.parquetFileArrowSchema enumFile))) == AT.AUtf8
      && Pred.decodePValueForSchema enumField "ready" == Just (Pred.PVText "ready"))
  let paddedBloom = sbbfInsert (BS.pack [255,255]) (newSbbf 256)
  expect "decimal bloom probing cannot reject equivalent sign-extended values"
    (Pred.evalBloomChunk field paddedBloom (Pred.PEq (Pred.PVDecimal ((-1) % 100))) == Pred.PMaybeKeep)
  expect "bloom probing keeps incompatible scalar representations"
    (Pred.evalBloomChunk (physicalField PTInt32) (newSbbf 256) (Pred.PEq (Pred.PVInt64 1)) == Pred.PMaybeKeep)
  mapM_ (\logical -> do
    let unordered = field {seLogicalType = Just logical,seConvertedType = Nothing}
    expect "undefined logical ordering cannot be used for pruning"
      (Pred.decodePValueForSchema unordered (BS.pack [1]) == Nothing)
    unorderedFile <- either failTest pure (loadParquetFile (PHL.encodeParquet
      (PHL.defaultWriteOptions {PHL.writeCompression = Uncompressed})
      (V.fromList [schema V.! 0,unordered]) [V.singleton (ColByteArray (V.fromList [BS.pack [1],BS.pack [2]]))]))
    let metadataBounds = ccMetadata (V.head (rgColumns (V.head (fmRowGroups (pfFooter unorderedFile))))) >>= cmStatistics
    expect "writers omit bounds for undefined logical ordering"
      (maybe False (\s -> statMinValue s == Nothing && statMaxValue s == Nothing) metadataBounds))
    [LTGeometry defaultGeometryParameters,LTGeography defaultGeographyParameters]
  mapM_ (\values -> do
    result <- PHL.encodeParquetIO (PHL.defaultWriteOptions {PHL.writeCompression = Uncompressed})
      (V.fromList [schema V.! 0,field {seLogicalType = Just (LTDecimal 2 0)}])
      [V.singleton (ColByteArray (V.singleton values))]
    expect "writer rejects empty or out-of-precision decimal storage" (isLeft result))
    [BS.empty,BS.pack [100],BS.pack [156]]
  let unsignedSchema = V.fromList [schema V.! 0,unsigned {seName = "x"}]
      unsignedBytes = PHL.encodeParquet (PHL.defaultWriteOptions {PHL.writeCompression = Uncompressed})
        unsignedSchema [V.singleton (ColInt32 (VP.fromList [0,minBound,-1]))]
  unsignedFile <- either failTest pure (loadParquetFile unsignedBytes)
  let unsignedGroup = V.head (fmRowGroups (pfFooter unsignedFile))
  mapM_ (\value -> expect "unsigned row-group predicates keep values across the signed boundary"
    (Pred.evalRowGroup (pfFooter unsignedFile) (Pred.PCol "x" (Pred.PEq (Pred.PVUnsigned value))) unsignedGroup == Pred.PMaybeKeep))
    [0,2147483648,4294967295]
  expect "unsigned aggregates compare unsigned values"
    (PAgg.columnMax (pfFooter unsignedFile) 0 == Just (Pred.PVUnsigned 4294967295))
  passed <- H.check $ H.withTests 100 $ H.property $ do
    numbers <- H.forAll (Gen.nonEmpty (Range.linear 1 40) (Gen.int64 (Range.linear minBound maxBound)))
    width <- H.forAll (Gen.int (Range.linear 8 12))
    let encode number = BS.pack (reverse ((\offset -> fromIntegral ((fromIntegral number :: Integer) `shiftR` (8 * offset))) <$> [0 .. width-1]))
        exactField = field {seLogicalType = Just (LTDecimal 19 0)}
        exactSchema = V.fromList [schema V.! 0,exactField]
        encoded = PHL.encodeParquet (PHL.defaultWriteOptions {PHL.writeCompression = Uncompressed}) exactSchema
          [V.singleton (ColByteArray (V.fromList (encode <$> NE.toList numbers)))]
    loaded <- H.evalEither (loadParquetFile encoded)
    PAgg.columnMin (pfFooter loaded) 0 H.=== Just (Pred.PVDecimal (fromIntegral (minimum numbers)))
    PAgg.columnMax (pfFooter loaded) 0 H.=== Just (Pred.PVDecimal (fromIntegral (maximum numbers)))
  unless passed (failTest "signed decimal statistics property")


writerOptionsRegression :: IO ()
writerOptionsRegression = do
  let required = SchemaElement "r" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
      optional = SchemaElement "o" (Just Optional) (Just PTInt64) Nothing Nothing Nothing Nothing Nothing
      schema = V.fromList [SchemaElement "schema" Nothing Nothing (Just 2) Nothing Nothing Nothing Nothing, required, optional]
      rows = V.fromList [PCRequired (ColInt32 (VP.fromList [1,2,3])), PCOptional (OptInt64 (V.fromList [Just 10,Nothing,Just 30]))]
  optionalExpected <- either failTest pure (AC.mkPrim AC.PInt64
    (AC.validityFromBools (V.fromList [True,False,True])) (VS.fromList [10,0,30]))
  mapM_ (\version -> mapM_ (\codec -> mapM_ (\indexed -> do
    let opts = PHL.defaultWriteOptions {PHL.writePageVersion = version, PHL.writeCompression = codec,
          PHL.writePageIndex = indexed, PHL.writeBloomFilters = ["o"]}
        bytes = PHL.encodeParquetMixed opts schema [rows,rows]
    pf <- either failTest pure (PHL.decodeParquet PHL.defaultReadOptions bytes)
    mapM_ (\rg -> mapM_ (\col -> do
      let chunk = rgColumns (fmRowGroups (pfFooter pf) V.! rg) V.! col
      metadata <- maybe (failTest "missing options regression metadata") pure (ccMetadata chunk)
      expect "mixed writer honors requested compression" (cmCodec metadata == codec)
      (header, _) <- either failTest pure (readPageHeaderCheckedAt bytes (fromIntegral (cmDataPageOffset metadata)))
      expect "mixed writer honors requested page version" (case (version,phType header) of
        (PageV1,PtDataPage {}) -> True; (PageV2,PtDataPageV2 {}) -> True; _ -> False)
      expect "mixed writer honors page-index toggle" ((ccOffsetIndexOffset chunk /= Nothing) == indexed
        && (ccColumnIndexOffset chunk /= Nothing) == indexed)
      expect "mixed writer attaches bloom only to requested nullable column"
        ((cmBloomFilterOffset metadata /= Nothing) == (col == 1))
      when indexed $ do
        index <- either failTest pure (readOffsetIndex pf rg col)
        location <- maybe (failTest "missing generated offset index") (pure . V.head . oiPageLocations) index
        expect "generated index uses absolute page offset and complete on-disk size"
          (plOffset location == cmDataPageOffset metadata
            && fromIntegral (plCompressedPageSize location) == cmTotalCompressedSize metadata
            && plFirstRowIndex location == 0)
        ci <- either failTest pure (readColumnIndex pf rg col)
        expect "generated index includes page null counts"
          ((ci >>= ciNullCounts) == Just (V.singleton (if col == 1 then 1 else 0)))
        if col == 0 then do
          selected <- either failTest pure (Parquet.Read.readGenericInt32SelectedPages codec bytes
            (V.singleton location) (V.singleton True))
          expect "generated indexes select required values" (selected == VP.fromList [1,2,3])
        else do
          selected <- either failTest pure (Parquet.Read.readGenericInt64OptionalSelectedPages codec 0 1 bytes
            (V.singleton location) (V.singleton True))
          expect "generated indexes select nullable values" (selected == V.fromList [Just 10,Nothing,Just 30])) [0,1]) [0,1]
    let native = PArrow.parquetFileArrowSchema pf
    columns <- either (failTest . show) pure (PArrow.parquetRowGroupToArrow native pf 1)
    expect "mixed options preserve nullable values"
      (columns == V.fromList [AC.primColumn AC.PInt32 (VS.fromList [1,2,3]),
        optionalExpected])) [False,True]) [Uncompressed,GZip,LZO]) [PageV1,PageV2]
  rejected <- try (evaluate (BS.length (PHL.encodeParquetMixed
    (PHL.defaultWriteOptions {PHL.writeCompression = LZ4}) schema [rows]))) :: IO (Either SomeException Int)
  expect "mixed writer rejects unsupported codec instead of silently falling back" (isLeft rejected)
  let requiredSchema = V.fromList [SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing, required]
      badOptions = PHL.defaultWriteOptions {PHL.writeCompression = LZ4}
  mapM_ (\groups -> do
    failure <- try (evaluate (BS.length (PHL.encodeParquet badOptions requiredSchema groups))) :: IO (Either SomeException Int)
    expect "required writer rejects unavailable codec including empty files" (isLeft failure)
    checked <- PHL.encodeParquetIO badOptions requiredSchema groups
    expect "checked writer reports unavailable codec through Left" (isLeft checked))
    [[], [V.singleton (ColInt32 (VP.fromList [1,2,3]))]]
  mismatch <- pure (PHL.encodeParquetMixed (PHL.defaultWriteOptions {PHL.writeCompression = Uncompressed}) schema
    [V.singleton (PCRequired (ColInt32 (VP.fromList [1,2,3])))])
  malformed <- try (evaluate (BS.length mismatch)) :: IO (Either SomeException Int)
  expect "mixed writer rejects incomplete column groups" (isLeft malformed)
  let zeros = ColInt32 (VP.replicate 257 0)
      compressed = PHL.encodeParquetMixed (PHL.defaultWriteOptions
        {PHL.writeCompression = GZip, PHL.writePageVersion = PageV1}) requiredSchema
        [V.singleton (PCRequired zeros)]
  pf <- either failTest pure (loadParquetFile compressed)
  let rg = V.head (fmRowGroups (pfFooter pf))
  meta <- maybe (failTest "missing compressed size regression metadata") pure
    (ccMetadata (V.head (rgColumns rg)))
  (header, bodyOffset) <- either failTest pure
    (readPageHeaderCheckedAt compressed (fromIntegral (cmDataPageOffset meta)))
  let uncompressed = fromIntegral (bodyOffset - fromIntegral (cmDataPageOffset meta))
        + maybe 0 fromIntegral (phUncompressedPageSize header)
  expect "mixed uncompressed size uses the actual compressed-page header length"
    (cmTotalUncompressedSize meta == uncompressed)
  expect "row-group total byte size is uncompressed even for compressed files"
    (rgTotalByteSize rg == uncompressed && uncompressed > cmTotalCompressedSize meta)


wideTimestampStorageRegression :: IO ()
wideTimestampStorageRegression = do
  let schema unit =
        V.fromList
          [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
          , SchemaElement
              "timestamp"
              (Just Required)
              (Just PTFixedLenByteArray)
              Nothing
              Nothing
              (Just (LTTimestamp True unit))
              Nothing
              (Just 12)
          ]
      footer unit = FileMetadata 1 (schema unit) 0 V.empty Nothing Nothing Nothing
      file unit = parquetMagic <> Footer.writeFooter (footer unit)
  mapM_
    ( \unit -> do
        expect
          "wide timestamps retain their lossless type and parameters"
          ( case PArrow.parquetFileArrowSchemaChecked (Parquet.Read.ParquetFile (file unit) (footer unit)) of
              Right result -> V.length (AT.arrowFields result) == 1 && Timestamp.timestampEncoding (V.head (AT.arrowFields result)) == Right (Just (Timestamp.SignedTimestamp96 True unit))
              Left _ -> False
          )
        expect
          "public decode accepts valid wide timestamp storage"
          (either (const False) (const True) (PHL.decodeParquet PHL.defaultReadOptions (file unit)))
    )
    [LtMillis, LtMicros, LtNanos]


legacyDictionarySizeRegression :: IO ()
legacyDictionarySizeRegression = do
  let values = V.fromList ["alpha", "beta", "alpha", "gamma", "beta", "alpha"]
      schema = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
        , SchemaElement "x" (Just Required) (Just PTByteArray) Nothing Nothing Nothing Nothing Nothing ]
      dictionary = buildDictionary (ColByteArray values)
      dictionaryPage = encodeDictPage dictionary
      pages = dictionaryPage <> encodeDictDataPage dictionary
  base <- either failTest pure (loadParquetFile (buildParquetFile schema
    (V.singleton (V.singleton (ColByteArray values)))))
  (_, dictionaryHeaderSize) <- either failTest pure (readPageHeaderAt dictionaryPage 0)
  let footer = pfFooter base
      row = V.head (fmRowGroups footer)
      column = V.head (rgColumns row)
  metadata <- maybe (failTest "missing dictionary regression metadata") pure (ccMetadata column)
  let shortSize = BS.length pages - dictionaryHeaderSize
      legacyMeta = metadata
        { cmEncodings = V.empty, cmDataPageOffset = 4, cmDictionaryPageOffset = Nothing
        , cmTotalCompressedSize = fromIntegral shortSize
        , cmTotalUncompressedSize = fromIntegral shortSize }
      legacyColumn = column {ccMetadata = Just legacyMeta}
      makeFile col =
        let fm = footer {fmCreatedBy = Just "parquet-mr",
              fmRowGroups = V.singleton (row {rgColumns = V.singleton col})}
        in Parquet.Read.ParquetFile (parquetMagic <> pages <> Footer.writeFooter fm) fm
      legacy = makeFile legacyColumn
  corrected <- either failTest pure (Parquet.Read.columnChunkSlice legacy 0 0)
  expect "legacy dictionary size repair includes exactly the missing header" (corrected == pages)
  decoded <- either failTest pure (Parquet.Read.readGenericByteArrayColumnChunk Uncompressed corrected)
  expect "legacy dictionary size repair preserves every value" (decoded == values)
  let blocked = makeFile (legacyColumn {ccColumnIndexOffset = Just (fromIntegral (4 + BS.length pages - 1))})
  expect "legacy repair never crosses an auxiliary index boundary"
    (isLeft (Parquet.Read.columnChunkSlice blocked 0 0))
  let wrongCount = makeFile (legacyColumn {ccMetadata = Just (legacyMeta {cmNumValues = 7})})
  expect "legacy repair validates the full declared value count"
    (isLeft (Parquet.Read.columnChunkSlice wrongCount 0 0))
  let wrongLength = makeFile (legacyColumn {ccMetadata = Just
        (legacyMeta {cmTotalCompressedSize = fromIntegral (shortSize - 1)})})
  expect "legacy repair rejects lengths other than one missing header"
    (isLeft (Parquet.Read.columnChunkSlice wrongLength 0 0))


pageChecksumRegression :: IO ()
pageChecksumRegression = do
  -- IEEE CRC32's independent check vector; its high bit also exercises the
  -- signed i32 Thrift representation. Checksums cover the body, never headers.
  let payload = "123456789"
      page ty extra crc body = Thrift.encodeCompact (TV.Struct (V.fromList
        ([ TS.PageHeader_Type ty, TS.PageHeader_UncompressedSize 9
         , TS.PageHeader_CompressedSize 9, TS.PageHeader_Crc crc ] ++ extra))) <> body
      v1 = [TS.PageHeader_DataPageHeader (V.fromList
        [TS.DataPageHeader_NumValues 1, TS.DataPageHeader_Encoding 0])]
      v2 = [TS.PageHeader_DataPageHeaderV2 (V.fromList
        [ TS.DataPageHeaderV2_NumValues 1, TS.DataPageHeaderV2_NumNulls 0
        , TS.DataPageHeaderV2_NumRows 1, TS.DataPageHeaderV2_Encoding 0
        , TS.DataPageHeaderV2_DefinitionLevelsByteLength 0, TS.DataPageHeaderV2_RepetitionLevelsByteLength 0 ])]
      dictionary = [TS.PageHeader_DictionaryPageHeader (V.fromList
        [TS.DictionaryPageHeader_NumValues 1, TS.DictionaryPageHeader_Encoding 0])]
      checksum = fromIntegral (0xcbf43926 :: Word32)
  mapM_ (\(ty, extra) -> do
    expect "checksums accept the IEEE check vector for every page kind"
      (not (isLeft (readPageHeaderCheckedAt (page ty extra checksum payload) 0)))
    expect "checksums reject damaged bodies before decompression"
      (isLeft (readPageHeaderCheckedAt (page ty extra checksum "123456780") 0))
    expect "checksums reject a changed CRC"
      (isLeft (readPageHeaderCheckedAt (page ty extra 0 payload) 0))
    expect "checksums reject truncated bodies"
      (isLeft (readPageHeaderCheckedAt (page ty extra checksum "12345678") 0)))
    [(0, v1), (1, []), (2, dictionary), (3, v2)]
  let bytes = page 0 v1 checksum payload
      headerOnly = BS.take (BS.length bytes - BS.length payload) bytes
  expect "header-only parsing remains available for encryption framing"
    (not (isLeft (readPageHeaderAt headerOnly 0)))
  expect "data readers verify checksums instead of exposing damaged bytes"
    (isLeft (Parquet.Read.decompressDataPageBody Uncompressed (page 0 v1 checksum "123456780") 0))
  expect "CRC validation handles a page at a nonzero file offset"
    (not (isLeft (readPageHeaderCheckedAt ("prefix" <> bytes <> "suffix") 6)))


malformedCountRegression :: IO ()
malformedCountRegression = do
  let check label decoded = expect label (isLeft decoded)
      encodedInts = encodeDeltaBinaryPackedInt64 (VP.fromList [1, 2, 3])
      encodedBytes = V.fromList ["first", "second", "third"]
  mapM_ (\count -> do
    check "DELTA_BINARY_PACKED checks every expected count"
      (decodeDeltaBinaryPackedInt64 count encodedInts)
    check "DELTA_LENGTH_BYTE_ARRAY checks every expected count"
      (decodeDeltaLengthByteArray count (encodeDeltaLengthByteArray encodedBytes))
    check "DELTA_BYTE_ARRAY checks every expected count"
      (decodeDeltaByteArray count (encodeDeltaByteArray encodedBytes))) [-1, 0, 2, 4]
  check "DELTA rejects a huge truncated count before allocating its output"
    (decodeDeltaBinaryPackedInt64 2147483647 (BS.pack [0x80, 1, 4, 0xff, 0xff, 0xff, 0xff, 7, 0]))
  mapM_ (\count -> do
    check "PLAIN Int32 rejects invalid counts" (Parquet.Read.decodePlainInt32 count BS.empty)
    check "PLAIN Int64 rejects invalid counts" (Parquet.Read.decodePlainInt64 count BS.empty)
    check "PLAIN Float rejects invalid counts" (Parquet.Read.decodePlainFloat count BS.empty)
    check "PLAIN Double rejects invalid counts" (Parquet.Read.decodePlainDouble count BS.empty)
    check "PLAIN Bool rejects invalid counts" (Parquet.Read.decodePlainBool count BS.empty)
    check "PLAIN Int96 rejects invalid counts" (Parquet.Read.decodePlainInt96 count BS.empty)
    check "PLAIN fixed bytes rejects invalid counts" (Parquet.Read.decodePlainFixedLenByteArray 16 count BS.empty)
    check "BYTE_STREAM_SPLIT Float rejects invalid counts" (Parquet.Read.decodeByteStreamSplitFloat count BS.empty)
    check "BYTE_STREAM_SPLIT Double rejects invalid counts" (Parquet.Read.decodeByteStreamSplitDouble count BS.empty))
    [-1, maxBound]
  let base = PageHeader (PtDataPage (DataPageHeader 0 0)) (Just 0) (Just 0)
      badHeaders =
        [ base {phUncompressedPageSize = Just (-1)}
        , base {phCompressedPageSize = Just (-1)}
        , base {phUncompressedPageSize = Nothing}
        , base {phCompressedPageSize = Nothing}
        , base {phType = PtDataPage (DataPageHeader (-1) 0)}
        , base {phType = PtDictionaryPage (DictionaryPageHeader (-1) 0)}
        , base {phType = PtDataPageV2 (DataPageHeaderV2 (-1) 0 0 0 0 0 False)}
        , base {phType = PtDataPageV2 (DataPageHeaderV2 1 2 1 0 0 0 False)}
        , base {phType = PtDataPageV2 (DataPageHeaderV2 1 0 2 0 0 0 False)}
        , base {phType = PtDataPageV2 (DataPageHeaderV2 1 0 1 0 (-1) 0 False)}
        , base {phType = PtDataPageV2 (DataPageHeaderV2 1 0 1 0 0 (-1) False)}
        , base {phType = PtDataPageV2 (DataPageHeaderV2 1 0 1 0 maxBound maxBound False)}
        ]
  mapM_ (check "malformed page counts/sizes reject" . (`readPageHeaderAt` 0) . encodePageHeader) badHeaders
  check "negative page-header offset rejects" (readPageHeaderAt (encodePageHeader base) (-1))
  check "page-header offset at EOF rejects" (readPageHeaderAt BS.empty 0)
  let metadata extra = TV.Struct (V.fromList
        [ TS.FileMetadata_Version 1
        , TS.FileMetadata_Schema (V.fromList
            [ TV.Struct (V.fromList [TS.SchemaElement_Name "schema", TS.SchemaElement_NumChildren 1])
            , TV.Struct (V.fromList (TS.SchemaElement_Name "x" : extra))
            ])
        , TS.FileMetadata_NumRows 0
        , TS.FileMetadata_RowGroups V.empty
        ])
  check "footer rejects invalid repetition enum without toEnum exception"
    (Footer.readFooterRaw (Thrift.encodeCompact (metadata [TS.SchemaElement_Type 1, TS.SchemaElement_RepetitionType 99])))
  check "footer rejects unknown physical type"
    (Footer.readFooterRaw (Thrift.encodeCompact (metadata [TS.SchemaElement_Type 99, TS.SchemaElement_RepetitionType 0])))
  check "footer rejects negative child count"
    (Footer.readFooterRaw (Thrift.encodeCompact (metadata [TS.SchemaElement_NumChildren (-1)])))


dataPageV2CompressionRegression :: IO ()
dataPageV2CompressionRegression = do
  let definitions = BS.pack [2, 0] -- One null, RLE level 0 at bit width 1.
      header compressedSize = encodePageHeader PageHeader
        { phType = PtDataPageV2 (DataPageHeaderV2 1 1 1 0 2 0 True)
        , phUncompressedPageSize = Just 2
        , phCompressedPageSize = Just compressedSize
        }
      emptySnappy = header 2 <> definitions
  expect "V2 all-null page permits absent Snappy values"
    (Parquet.Read.readGenericFloatOptionalColumnChunk Snappy 0 1 emptySnappy == Right (V.singleton Nothing))
  case compressPageBytes ZSTD BS.empty of
    Left e -> failTest e
    Right compressed -> do
      let page = header (fromIntegral (BS.length definitions + BS.length compressed)) <> definitions <> compressed
      expect "V2 all-null page permits compressed empty ZSTD values"
        (Parquet.Read.readGenericInt32OptionalColumnChunk ZSTD 0 1 page == Right (V.singleton Nothing))
      let dictionaryHeader = encodePageHeader PageHeader
            { phType = PtDictionaryPage (DictionaryPageHeader 0 0)
            , phUncompressedPageSize = Just 0
            , phCompressedPageSize = Just (fromIntegral (BS.length compressed))
            }
          dictionaryData = encodePageHeader PageHeader
            { phType = PtDataPageV2 (DataPageHeaderV2 1 1 1 8 2 0 True)
            , phUncompressedPageSize = Just 2
            , phCompressedPageSize = Just 2
            }
          allNullDictionary = dictionaryHeader <> compressed <> dictionaryData <> definitions
      expect "ZSTD empty dictionary and absent all-null indices decode"
        (Parquet.Read.readGenericInt32OptionalColumnChunk ZSTD 0 1 allNullDictionary == Right (V.singleton Nothing))
      expect "public ZSTD decoder accepts a valid empty frame"
        (Parquet.Read.decompressChunk ZSTD compressed == Right BS.empty)
  -- Zstandard frame: no frame-content-size field, 1 KiB window,
  -- one final raw block containing "abc". It is independent of our writer.
  let unknownSize = BS.pack [0x28, 0xb5, 0x2f, 0xfd, 0x00, 0x00, 0x19, 0x00, 0x00, 0x61, 0x62, 0x63]
      rawHeader expected = encodePageHeader PageHeader
        { phType = PtDataPage (DataPageHeader 1 0)
        , phUncompressedPageSize = Just expected
        , phCompressedPageSize = Just (fromIntegral (BS.length unknownSize))
        }
  expect "public ZSTD decoder accepts a frame without content size"
    (Parquet.Read.decompressChunk ZSTD unknownSize == Right "abc")
  expect "public ZSTD decoder rejects truncated unknown-size frames"
    (isLeft (Parquet.Read.decompressChunk ZSTD (BS.take 11 unknownSize)))
  case Parquet.Read.decompressDataPageBody ZSTD (rawHeader 3 <> unknownSize) 0 of
    Left e -> failTest e
    Right (_, _, body, _) -> expect "page-sized ZSTD decoding accepts omitted frame size" (body == "abc")
  expect "page-sized ZSTD decoding rejects size mismatch"
    (isLeft (Parquet.Read.decompressDataPageBody ZSTD (rawHeader 4 <> unknownSize) 0))
  case encodeOptionalColumnPageV2Parts LZ4Raw (OptInt32 (V.fromList [Just 1, Nothing, Just 3])) of
    Left e -> failTest e
    Right (hdr, reps, defs, values) ->
      expect "V2 LZ4 uses values size without level bytes"
        (Parquet.Read.readGenericInt32OptionalColumnChunk LZ4Raw 0 1 (hdr <> reps <> defs <> values)
          == Right (V.fromList [Just 1, Nothing, Just 3]))


deltaWideIntegerRegression :: IO ()
deltaWideIntegerRegression = do
  let header = [0x80, 0x01, 0x04, 0x01]
      minimumValue = BS.pack (header ++ replicate 9 0xff ++ [0x01])
      maximumValue = BS.pack (header ++ [0xfe] ++ replicate 8 0xff ++ [0x01])
  expect "delta zigzag preserves minBound Int64"
    (decodeDeltaBinaryPackedInt64 1 minimumValue == Right (VP.singleton minBound))
  expect "delta zigzag preserves maxBound Int64"
    (decodeDeltaBinaryPackedInt64 1 maximumValue == Right (VP.singleton maxBound))
  expect "delta rejects overflowing 64-bit varint"
    (isLeft (decodeDeltaBinaryPackedInt64 1 (BS.pack (header ++ replicate 9 0xff ++ [0x02]))))
  expect "delta rejects mismatched page count"
    (isLeft (decodeDeltaBinaryPackedInt64 2 maximumValue))
  expect "delta rejects zero-sized miniblocks"
    (isLeft (decodeDeltaBinaryPackedInt64 2 (BS.pack [0x80, 0x01, 0x80, 0x02, 2, 0])))


arrowParquetIntegerTypes :: IO ()
arrowParquetIntegerTypes = do
  check "Int8" (AT.AInt 8 True) (AC.fromMaybes AC.PInt8 (V.fromList [Just (minBound :: Int8), Nothing, Just maxBound]))
  check "Int16" (AT.AInt 16 True) (AC.fromMaybes AC.PInt16 (V.fromList [Just (minBound :: Int16), Nothing, Just maxBound]))
  check "UInt8" (AT.AInt 8 False) (AC.fromMaybes AC.PUInt8 (V.fromList [Just (0 :: Word8), Nothing, Just maxBound]))
  check "UInt16" (AT.AInt 16 False) (AC.fromMaybes AC.PUInt16 (V.fromList [Just (0 :: Word16), Nothing, Just maxBound]))
  check "UInt32" (AT.AInt 32 False) (AC.fromMaybes AC.PUInt32 (V.fromList [Just (0 :: Word32), Nothing, Just maxBound]))
  check "UInt64" (AT.AInt 64 False) (AC.fromMaybes AC.PUInt64 (V.fromList [Just (0 :: Word64), Nothing, Just maxBound]))
  where
    check label ty col = do
      let field = AT.Field "x" True ty V.empty Nothing V.empty
          schema = AT.Schema (V.singleton field) AT.Little V.empty V.empty
      case PArrow.arrowToParquetMixed schema [V.singleton col] of
        Left e -> failTest (label ++ " encode bridge: " ++ e)
        Right (ps, groups) -> do
          let bytes = PHL.encodeParquetMixed PHL.defaultWriteOptions ps groups
          case PHL.decodeParquet PHL.defaultReadOptions bytes of
            Left e -> failTest (label ++ " footer: " ++ e)
            Right pf -> do
              let recovered = PArrow.parquetFileArrowSchema pf
              expect (label ++ " retains logical integer type") (AT.arrowFields recovered == AT.arrowFields schema)
              expect (label ++ " retains unsigned/narrow values and nulls")
                (PArrow.parquetRowGroupToArrow recovered pf 0 == Right (V.singleton col))
              case ty of
                AT.AInt width False -> do
                  let storageBytes = if width == 64 then 8 else 4
                      expectedMax = BS.pack (replicate (width `quot` 8) 0xff ++ replicate (storageBytes - width `quot` 8) 0)
                      chunk = rgColumns (fmRowGroups (pfFooter pf) V.! 0) V.! 0
                      statistics = ccMetadata chunk >>= cmStatistics
                  expect (label ++ " statistics use unsigned ordering")
                    (maybe False (\s -> statMinValue s == Just (BS.replicate storageBytes 0)
                                      && statMaxValue s == Just expectedMax
                                      && statMin s == Nothing && statMax s == Nothing
                                      && statNullCount s == Just 1) statistics)
                _ -> pure ()


predicateRowGroupSkipping :: IO ()
predicateRowGroupSkipping = do
  let schema = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
        , SchemaElement "n" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
        ]
      vs    = VP.fromList [(10 :: Int32), 20, 30, 40, 50]
      fbs   = buildParquetFile schema (V.singleton (V.singleton (ColInt32 vs)))
  case loadParquetFile fbs of
    Left e -> failTest ("predicate: loadParquetFile: " ++ e)
    Right pf -> do
      let !rg = V.unsafeIndex (fmRowGroups (pfFooter pf)) 0
          fileMetadata = pfFooter pf
          mkPred cp = Pred.PCol "n" cp
      expect "PEq 25 outside [10,50] -> Skip"
        (Pred.evalRowGroup fileMetadata (mkPred (Pred.PEq (Pred.PVInt32 25)))
                           rg == Pred.PMaybeKeep)
      expect "PEq 5 below min -> Skip"
        (Pred.evalRowGroup fileMetadata (mkPred (Pred.PEq (Pred.PVInt32 5)))
                           rg == Pred.PSkip)
      expect "PEq 100 above max -> Skip"
        (Pred.evalRowGroup fileMetadata (mkPred (Pred.PEq (Pred.PVInt32 100)))
                           rg == Pred.PSkip)
      expect "PLt 5 -> Skip"
        (Pred.evalRowGroup fileMetadata (mkPred (Pred.PLt (Pred.PVInt32 5)))
                           rg == Pred.PSkip)
      expect "PGtEq 30 inside range -> MaybeKeep"
        (Pred.evalRowGroup fileMetadata (mkPred (Pred.PGtEq (Pred.PVInt32 30)))
                           rg == Pred.PMaybeKeep)
      -- AND short-circuits on a Skip side
      let pAnd = Pred.PAnd
                   (mkPred (Pred.PLt (Pred.PVInt32 5)))
                   (mkPred (Pred.PGt (Pred.PVInt32 0)))
      expect "AND with skipping side -> Skip"
        (Pred.evalRowGroup fileMetadata pAnd rg == Pred.PSkip)
      -- IsNull on no-null column => Skip
      expect "PIsNull when null_count=0 -> Skip"
        (Pred.evalRowGroup fileMetadata (mkPred Pred.PIsNull) rg == Pred.PSkip)

-- File-level order is part of the interpretation of modern statistics.
statisticsOrderingRegression :: IO ()
statisticsOrderingRegression = do
  let field = physicalField PTInt32
      schema = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
        , field
        ]
      values = V.singleton (ColInt32 (VP.fromList [-1, 0]))
  file <- either failTest pure (loadParquetFile (buildParquetFile schema (V.singleton values)))
  let metadata = pfFooter file
      group = V.head (fmRowGroups metadata)
      column = V.head (rgColumns group)
      stats = columnDataStatistics (ColInt32 (VP.fromList [-1, 0]))
      modernOnly = stats {statMin = Nothing, statMax = Nothing}
      noOrder = Pred.StatisticsContext field Nothing
      cannotMatch = Pred.PEq (Pred.PVInt32 10)
      replaceStats st = column {ccMetadata = fmap (\md -> md {cmStatistics = Just st}) (ccMetadata column)}
      modernGroup = group {rgColumns = V.singleton (replaceStats modernOnly)}
      modernMetadata = metadata {fmColumnOrders = Nothing, fmRowGroups = V.singleton modernGroup}
  expect "modern-only bounds without column_orders cannot prune"
    (Pred.evalColumnChunk noOrder (replaceStats modernOnly) cannotMatch == Pred.PMaybeKeep
      && Pred.evalRowGroup modernMetadata (Pred.PCol (seName field) cannotMatch) modernGroup == Pred.PMaybeKeep
      && PAgg.columnMin modernMetadata 0 == Nothing && PAgg.columnMax modernMetadata 0 == Nothing)
  expect "legacy signed numeric bounds remain usable without column_orders"
    (Pred.evalColumnChunk noOrder column cannotMatch == Pred.PSkip
      && Pred.minimumStatistic noOrder stats == Just (Pred.PVInt32 (-1)))
  let binaryField = physicalField PTByteArray
      legacyBinary = columnDataStatistics (ColByteArray (V.fromList ["a", "z"]))
  expect "legacy signed byte bounds cannot establish unsigned binary order"
    (Pred.minimumStatistic (Pred.StatisticsContext binaryField Nothing) legacyBinary == Nothing)
  let index = ColumnIndex
        { ciNanCounts = Nothing
        , ciNullPages = V.fromList [False, True]
        , ciMinValues = V.fromList [BS.pack [255,255,255,255], BS.empty]
        , ciMaxValues = V.fromList [BS.replicate 4 0, BS.empty]
        , ciBoundaryOrder = OrderAscending
        , ciNullCounts = Just (V.fromList [0, 2])
        , ciRepetitionLevelHistograms = Nothing
        , ciDefinitionLevelHistograms = Nothing
        }
  expect "page bounds need an order while all-null pages remain skippable"
    (Pred.evalPagesByColumnIndex noOrder index cannotMatch == V.fromList [Pred.PMaybeKeep, Pred.PSkip])
  let missingOrderRaw = case Footer.fileMetadataToThrift modernMetadata of
        TV.Struct fields -> Thrift.encodeCompact (TV.Struct fields)
        _ -> error "expected footer struct"
  parsedMissing <- either failTest pure (Footer.readFooterRaw missingOrderRaw)
  expect "reading a footer without orders preserves raw modern metadata"
    ((ccMetadata (V.head (rgColumns (V.head (fmRowGroups parsedMissing)))) >>= cmStatistics) == Just modernOnly)
  let unknownAnnotations =
        [ V.singleton (127, TV.Struct V.empty)
        , V.empty
        , V.fromList [(1, TV.Struct V.empty), (127, TV.Struct V.empty)]
        , V.singleton (8, TV.Struct (V.fromList
            [(1, TV.Bool True), (2, TV.Struct (V.singleton (127, TV.Struct V.empty)))]))
        ]
      replaceAnnotation annotation = case Footer.fileMetadataToThrift metadata of
        TV.Struct fields -> TV.Struct (V.map (\entry -> case entry of
          TS.FileMetadata_Schema elements -> TS.FileMetadata_Schema (V.imap (\i element ->
            if i == 0 then element else case element of
              TV.Struct fields' -> TV.Struct (V.filter (\entry' -> case entry' of
                TS.SchemaElement_LogicalType _ -> False
                TS.SchemaElement_ConvertedType _ -> False
                _ -> True) fields' V.++ V.fromList
                  [TS.SchemaElement_ConvertedType 13, TS.SchemaElement_LogicalType annotation])
              other -> other) elements)
          other -> other) fields)
        other -> other
  mapM_ (\annotation -> do
    parsed <- either failTest pure (Footer.readFooterRaw (Thrift.encodeCompact (replaceAnnotation annotation)))
    let decodedField = fmSchema parsed V.! 1
        decodedColumn = V.head (rgColumns (V.head (fmRowGroups parsed)))
        arrowSchema = PArrow.parquetFileArrowSchema (file {pfFooter = parsed})
    expect "unknown logical annotations cannot inherit obsolete unsigned semantics"
      (seLogicalType decodedField == Just LTUnsupported && seConvertedType decodedField == Nothing
        && AT.fieldType (V.head (AT.arrowFields arrowSchema)) == AT.AInt 32 True)
    expect "unknown annotations never provide ordered bounds"
      (Pred.decodePValueForSchema decodedField (BS.replicate 4 0) == Nothing
        && (ccMetadata decodedColumn >>= cmStatistics >>= statMinValue) == Nothing)
    checked <- buildParquetFileWithIndexIO (fmSchema parsed) (V.singleton values)
      (V.singleton (V.singleton emptyColumnAux))
    expect "writer rejects unsupported logical annotations" (isLeft checked)
    emptyChecked <- buildParquetFileWithIndexIO (fmSchema parsed) V.empty V.empty
    expect "empty writer also rejects unsupported logical annotations" (isLeft emptyChecked)) unknownAnnotations
  let externalFile = file {pfFooter = metadata {fmRowGroups = V.singleton
        (group {rgColumns = V.singleton (column {ccFilePath = Just "part-00001.parquet"})})}}
  expect "external chunks cannot read unrelated bytes from the metadata file"
    (isLeft (Parquet.Read.columnChunkSlice externalFile 0 0))
  expect "external chunks cannot read unrelated page indexes"
    (isLeft (readOffsetIndex externalFile 0 0) && isLeft (readColumnIndex externalFile 0 0))
  let emptyPathFile = file {pfFooter = metadata {fmRowGroups = V.singleton
        (group {rgColumns = V.singleton (column {ccFilePath = Just mempty})})}}
  expect "empty external path retains local column reading"
    (Parquet.Read.columnChunkSlice emptyPathFile 0 0 == Parquet.Read.columnChunkSlice file 0 0)


predicatePageSkipping :: IO ()
predicatePageSkipping = do
  -- Build a synthetic ColumnIndex for an int32 column with two
  -- pages: page 0 has [1..10], page 1 has [50..60].
  let mins = V.fromList
               [ BS.pack [1, 0, 0, 0]
               , BS.pack [50, 0, 0, 0]
               ]
      maxs = V.fromList
               [ BS.pack [10, 0, 0, 0]
               , BS.pack [60, 0, 0, 0]
               ]
      ci = ColumnIndex
             { ciNanCounts = Nothing
             , ciNullPages = V.fromList [False, False]
             , ciMinValues = mins
             , ciMaxValues = maxs
             , ciBoundaryOrder = OrderAscending
             , ciNullCounts   = Nothing
             , ciRepetitionLevelHistograms = Nothing
             , ciDefinitionLevelHistograms = Nothing
             }
      decisions = Pred.evalPagesByColumnIndex (knownContext (physicalField PTInt32)) ci
                    (Pred.PEq (Pred.PVInt32 30))
  expect "page 0 ([1,10]) skips for =30"
    (V.unsafeIndex decisions 0 == Pred.PSkip)
  expect "page 1 ([50,60]) skips for =30"
    (V.unsafeIndex decisions 1 == Pred.PSkip)
  let decisions2 = Pred.evalPagesByColumnIndex (knownContext (physicalField PTInt32)) ci
                     (Pred.PEq (Pred.PVInt32 5))
  expect "page 0 keeps for =5"
    (V.unsafeIndex decisions2 0 == Pred.PMaybeKeep)
  expect "page 1 skips for =5"
    (V.unsafeIndex decisions2 1 == Pred.PSkip)
  let malformed = ci {ciMinValues = V.take 1 mins}
  expect "malformed page bounds never reach unchecked indexing"
    (Pred.evalPagesByColumnIndex (knownContext (physicalField PTInt32)) malformed (Pred.PEq (Pred.PVInt32 30))
      == V.replicate 2 Pred.PMaybeKeep)
  mapM_ (\broken -> expect "malformed column-index vectors reject during decoding"
    (isLeft (decodeColumnIndex (encodeColumnIndex broken))))
    [ malformed
    , ci {ciMaxValues = V.empty}
    , ci {ciNullCounts = Just (V.singleton 0)}
    , ci {ciNanCounts = Just (V.singleton 0)}
    , ci {ciNanCounts = Just (V.fromList [-1, 0])}
    ]

logicalTypeRoundTrip :: IO ()
logicalTypeRoundTrip = do
  -- Build a footer that exercises a handful of LogicalType
  -- variants (the modern slot, field 10) and assert they
  -- survive a thrift round-trip through readFooterRaw.
  let cases =
        [ ("string-col",  PTByteArray, Just LTString)
        , ("date-col",    PTInt32,     Just LTDate)
        , ("time-millis", PTInt32,     Just (LTTime False LtMillis))
        , ("time-micros", PTInt64,     Just (LTTime True  LtMicros))
        , ("ts-nanos",    PTInt64,     Just (LTTimestamp True  LtNanos))
        , ("dec-12-3",    PTFixedLenByteArray, Just (LTDecimal 12 3))
        , ("uuid-col",    PTFixedLenByteArray, Just LTUUID)
        , ("uint8-col",   PTInt32,     Just (LTInteger 8 False))
        , ("float16-col", PTFixedLenByteArray, Just LTFloat16)
        , ("variant-col", PTByteArray, Just (LTVariant 1))
        , ("geom-col",    PTByteArray, Just (LTGeometry defaultGeometryParameters))
        , ("geog-col",    PTByteArray, Just (LTGeography defaultGeographyParameters))
        ]
      mkSe (nm, pt, lt) =
        SchemaElement
          { seTypeLength = if pt == PTFixedLenByteArray then Just (if lt == Just LTFloat16 then 2 else 16) else Nothing
            , seName          = nm
          , seRepetition    = Just Required
          , seType          = Just pt
          , seNumChildren   = Nothing
          , seConvertedType = Nothing
          , seLogicalType   = lt
          , seFieldId       = Nothing
          }
      schema = V.fromList $
        SchemaElement "schema" Nothing Nothing
          (Just (fromIntegral (length cases))) Nothing Nothing Nothing Nothing
        : map mkSe cases
      vs = VP.fromList [(0 :: Int32)]
      cdata = ColInt32 vs
      -- Reuse the buildParquetFile entry for one stripe; the
      -- LogicalType slots ride along the schema regardless of
      -- which physical types the column data actually use.
      fbs = buildParquetFile
              (V.fromList
                 [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
                 , SchemaElement "n" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
                 ])
              (V.singleton (V.singleton cdata))
  -- The builder above ignores schema beyond the data layout; we
  -- exercise the LogicalType round-trip by serialising a
  -- standalone footer instead.
  _ <- pure fbs
  let fmRaw = FileMetadata
                { fmVersion   = 1
                , fmSchema    = schema
                , fmNumRows   = 0
                , fmRowGroups = V.empty
                , fmCreatedBy = Just "wireform"
                , fmKeyValueMetadata = Nothing
                , fmColumnOrders = Nothing
                }
  let bytes = Footer.writeFooter fmRaw
  -- buildParquetFile pads with magic; readFooter wants the
  -- same envelope. Just write a self-contained PAR1 footer
  -- and round-trip via readFooterRaw on the inner thrift bytes.
  let trailer = either error id (Footer.readFooterTrailer (parquetMagic <> bytes))
      raw     = ftBytes trailer
  case Footer.readFooterRaw raw of
    Left e -> failTest $ "logicalType: readFooterRaw: " ++ e
    Right fmDecoded -> do
      let recovered = V.toList (V.drop 1 (fmSchema fmDecoded))
      flip mapM_ (zip cases recovered) $ \((nm, _, expected), got) -> do
        expect ("LogicalType " ++ T.unpack nm ++ " round-trip")
               (seLogicalType got == expected)

-- Explicit Thrift union tags/parameter fields provide an independent wire
-- oracle. Re-encoding must retain optional-field presence, not just defaults.
logicalParameterRegression :: IO ()
logicalParameterRegression = do
  let geometry = LTGeometry . GeometryParameters
      geography crs algorithm = LTGeography (GeographyParameters crs algorithm)
      annotation tag fields = TV.Struct (V.singleton (tag, TV.Struct (V.fromList fields)))
      cases =
        [ (annotation 17 [], geometry Nothing)
        , (annotation 17 [(1, TV.Binary "EPSG:3857")], geometry (Just "EPSG:3857"))
        , (annotation 17 [(1, TV.Binary BS.empty)], geometry (Just ""))
        , (annotation 18 [], geography Nothing Nothing)
        , (annotation 18 [(1, TV.Binary "EPSG:4326")], geography (Just "EPSG:4326") Nothing)
        ]
      algorithms =
        [ (minBound, UnrecognizedInterpolation minBound)
        , (-1, UnrecognizedInterpolation (-1))
        , (0, Spherical)
        , (1, Vincenty)
        , (2, Thomas)
        , (3, Andoyer)
        , (4, Karney)
        , (127, UnrecognizedInterpolation 127)
        , (maxBound, UnrecognizedInterpolation maxBound)
        ]
      algorithmCase (code, algorithm) =
        ( annotation 18 [(1, TV.Binary "projjson:custom"), (2, TV.I32 code)]
        , geography (Just "projjson:custom") (Just algorithm)
        )
      checkCase (raw, expected) = do
        decoded <- decodeFixture False raw
        expect
          "geospatial parameters retain optional presence and full-width enums"
          (seLogicalType (fmSchema decoded V.! 1) == Just expected)
        expect
          "geospatial writer emits independent CRS/interpolation wire fields"
          ((Thrift.encodeCompact <$> findAnnotation decoded) == Just (Thrift.encodeCompact raw))
        expect
          "parameterized geospatial annotations retain undefined min/max order"
          (not (hasDefinedStatisticsOrder (fmSchema decoded V.! 1)))
  mapM_ checkCase (cases ++ map algorithmCase algorithms)
  mapM_
    ( \(code, algorithm) ->
        checkCase
          (annotation 18 [(2, TV.I32 code)], geography Nothing (Just algorithm))
    )
    algorithms
  mapM_
    ( \raw -> do
        decoded <- decodeFixture False raw
        expect
          "malformed geospatial parameters use physical fallback instead of defaults"
          (seLogicalType (fmSchema decoded V.! 1) == Just LTUnsupported)
    )
    [ annotation 17 [(1, TV.I32 1)]
    , annotation 17 [(1, TV.Binary (BS.singleton 255))]
    , annotation 17 [(1, TV.Binary "a"), (1, TV.Binary "b")]
    , annotation 18 [(2, TV.Binary "0")]
    , annotation 18 [(2, TV.I32 0), (2, TV.I32 1)]
    ]
  extended <- decodeFixture False (annotation 18 [(2, TV.I32 4), (99, TV.I32 7)])
  expect
    "unknown future geospatial fields do not discard known parameters"
    (seLogicalType (fmSchema extended V.! 1) == Just (geography Nothing (Just Karney)))
  file <- decodeFixture True (annotation 19 [])
  let fileGroup = fmSchema file V.! 1
  expect
    "FILE union tag19 retains its group annotation"
    (seLogicalType fileGroup == Just LTFile && hasCompatiblePhysicalType fileGroup)
  expect
    "FILE annotation writer emits the independent tag19 bytes"
    ((Thrift.encodeCompact <$> findAnnotation file) == Just (Thrift.encodeCompact (annotation 19 [])))
  expect
    "FILE cannot annotate scalar storage"
    (not (hasCompatiblePhysicalType (fileGroup {seType = Just PTByteArray})))
  where
    decodeFixture group raw =
      let root = SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
          field =
            SchemaElement
              "x"
              (Just Optional)
              (if group then Nothing else Just PTByteArray)
              (if group then Just 1 else Nothing)
              Nothing
              Nothing
              Nothing
              Nothing
          inline = SchemaElement "inline" (Just Optional) (Just PTByteArray) Nothing Nothing Nothing Nothing Nothing
          schema = V.fromList (if group then [root, field, inline] else [root, field])
          metadata = FileMetadata 1 schema 0 V.empty Nothing Nothing Nothing
          update (2, TV.List ty fields) =
            ( 2
            , TV.List
                ty
                ( V.imap
                    ( \index value -> case value of
                        TV.Struct body | index == 1 -> TV.Struct (V.snoc body (10, raw))
                        _ -> value
                    )
                    fields
                )
            )
          update value = value
          wire = case Footer.fileMetadataToThrift metadata of
            TV.Struct fields -> TV.Struct (V.map update fields)
            _ -> error "expected footer struct"
      in either failTest pure (Footer.readFooterRaw (Thrift.encodeCompact wire))
    findAnnotation metadata = do
      TV.Struct fields <- Just (Footer.fileMetadataToThrift metadata)
      TV.List _ schema <- lookup 2 (V.toList fields)
      TV.Struct field <- schema V.!? 1
      lookup 10 (V.toList field)
emptyColumnChunkRegression :: IO ()
emptyColumnChunkRegression = do
  let schema = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
        , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
        ]
      bytes = buildParquetFile schema (V.singleton (V.singleton (ColInt32 VP.empty)))
  case loadParquetFile bytes of
    Left err -> failTest err
    Right pf -> do
      let metadata = pfFooter pf
          rg = fmRowGroups metadata V.! 0
          chunk = rgColumns rg V.! 0
          -- Some zero-row writers advertise offsets at zero and nonzero
          -- compressed sizes; those bytes are not pages when num_values=0.
          changed = chunk {ccMetadata = fmap
            (\md -> md {cmNumValues = 0, cmDataPageOffset = 0, cmTotalCompressedSize = 14})
            (ccMetadata chunk)}
          file = pf {Parquet.Read.pfFooter = metadata {fmRowGroups = V.singleton
            (rg {rgColumns = V.singleton changed})}}
      expect "zero-value column chunks do not parse file magic as pages"
        (Parquet.Read.columnChunkSlice file 0 0 == Right BS.empty)
      expect "zero-value columns retain their empty Arrow shape"
        (PArrow.parquetRowGroupToArrow (PArrow.parquetFileArrowSchema file) file 0
          == Right (V.singleton (AC.primColumn AC.PInt32 VS.empty)))


fixedDecimalMetadataRegression :: IO ()
fixedDecimalMetadataRegression = do
  let schema fields = V.fromList
        [ TV.Struct (V.fromList [TS.SchemaElement_Name "schema", TS.SchemaElement_NumChildren 1])
        , TV.Struct (V.fromList (TS.SchemaElement_Name "x" : TS.SchemaElement_RepetitionType 0 : fields))
        ]
      footer fields = TV.Struct $ V.fromList
        [ TS.FileMetadata_Version 1
        , TS.FileMetadata_Schema (schema fields)
        , TS.FileMetadata_NumRows 0
        , TS.FileMetadata_RowGroups V.empty
        ]
      decode = Footer.thriftToFileMetadata . footer
      legacy = [ TS.SchemaElement_Type 7, TS.SchemaElement_TypeLength 7
               , TS.SchemaElement_ConvertedType 5, TS.SchemaElement_Precision 16
               , TS.SchemaElement_Scale 4 ]
  case decode legacy of
    Left err -> failTest err
    Right metadata -> do
      let leaf = fmSchema metadata V.! 1
      expect "legacy DECIMAL precision and scale are restored"
        (seLogicalType leaf == Just (LTDecimal 16 4))
      expect "fixed byte width survives decoding" (seTypeLength leaf == Just 7)
      case Footer.readFooterRaw (Thrift.encodeCompact (Footer.fileMetadataToThrift metadata)) of
        Left err -> failTest err
        Right restored -> expect "fixed DECIMAL width survives a footer round-trip"
          (fmSchema restored == fmSchema metadata)
      case Footer.fileMetadataToThrift metadata of
        TV.Struct fields ->
          let schemas = V.mapMaybe (\case TS.FileMetadata_Schema xs -> Just xs; _ -> Nothing) fields
              leaves = V.concatMap (V.drop 1) schemas
              fieldsOf = V.concatMap (\case TV.Struct xs -> xs; _ -> V.empty) leaves
          in expect "modern DECIMAL also emits legacy precision and scale"
            (V.elem (TS.SchemaElement_Precision 16) fieldsOf
             && V.elem (TS.SchemaElement_Scale 4) fieldsOf)
        _ -> failTest "footer did not encode as a struct"
  mapM_ (\fields -> expect "invalid fixed/DECIMAL metadata is rejected" (isLeft (decode fields)))
    [ [TS.SchemaElement_Type 7]
    , [TS.SchemaElement_Type 7, TS.SchemaElement_TypeLength 0]
    , [TS.SchemaElement_Type 7, TS.SchemaElement_TypeLength (-1)]
    , [TS.SchemaElement_Type 1, TS.SchemaElement_TypeLength (-1)]
    , [TS.SchemaElement_Type 1, TS.SchemaElement_ConvertedType 5]
    , [TS.SchemaElement_Type 1, TS.SchemaElement_ConvertedType 5, TS.SchemaElement_Precision 10]
    , [TS.SchemaElement_Type 2, TS.SchemaElement_ConvertedType 5, TS.SchemaElement_Precision 19]
    , [TS.SchemaElement_Type 7, TS.SchemaElement_TypeLength 1, TS.SchemaElement_ConvertedType 5, TS.SchemaElement_Precision 3]
    , [TS.SchemaElement_Type 6, TS.SchemaElement_ConvertedType 5, TS.SchemaElement_Precision 2, TS.SchemaElement_Scale 3]
    ]


-- | Random-input round-trips for every encoder/decoder pair.
-- Uses a fixed seed so failures reproduce; covers
-- DELTA_BINARY_PACKED Int32/Int64, DELTA_LENGTH_BYTE_ARRAY,
-- DELTA_BYTE_ARRAY, BYTE_STREAM_SPLIT Float/Double, and the
-- predicate evaluator's range arithmetic.
encodingRoundTripProperties :: IO ()
encodingRoundTripProperties = do
  let seed = 0xC0FFEE :: Int
      cases = 50
      -- Cheap LCG; we don't need cryptographic quality.
      lcg s = (s * 1103515245 + 12345) `mod` 0x100000000
      mkInts32 s n = take n (drop 1 (iterate lcg s))
        & map (\x -> fromIntegral (x `mod` 0x100000000) :: Int32)
      mkInts64 s n = take n (drop 1 (iterate lcg s))
        & map fromIntegral
      mkFloats s n = map (\x -> fromIntegral x / 1000.0)
                       (mkInts32 s n)
      mkDoubles s n = map (\x -> fromIntegral (x :: Int64) / 1000000.0)
                        (mkInts64 s n)
      (&) :: a -> (a -> b) -> b
      a & f = f a

  -- DELTA_BINARY_PACKED Int32
  do
    let pass i =
          let !n = (i `mod` 32) + 1
              !s = seed + i
              !vs = VP.fromList (mkInts32 s n)
          in case decodeDeltaBinaryPackedInt64
                   (VP.length vs)
                   (encodeDeltaBinaryPackedInt32 vs) of
               Right got
                 | VP.toList (VP.map (fromIntegral :: Int64 -> Int32) got)
                     == VP.toList vs -> True
               _ -> False
    let !okCount = length (filter pass [1 .. cases])
    expect ("DELTA_BINARY_PACKED Int32 property: "
              ++ show okCount ++ "/" ++ show cases)
           (okCount == cases)

  -- DELTA_BINARY_PACKED Int64
  do
    let pass i =
          let !n = (i `mod` 32) + 1
              !s = seed + i + 1
              !vs = VP.fromList (mkInts64 s n)
          in case decodeDeltaBinaryPackedInt64
                   (VP.length vs)
                   (encodeDeltaBinaryPackedInt64 vs) of
               Right got | VP.toList got == VP.toList vs -> True
               _ -> False
    let !okCount = length (filter pass [1 .. cases])
    expect ("DELTA_BINARY_PACKED Int64 property: "
              ++ show okCount ++ "/" ++ show cases)
           (okCount == cases)

  -- BYTE_STREAM_SPLIT Float
  do
    let pass i =
          let !n = (i `mod` 16) + 1
              !s = seed + i + 2
              !vs = VP.fromList (mkFloats s n)
          in case decodeByteStreamSplitFloat
                   (VP.length vs)
                   (encodeByteStreamSplitFloat vs) of
               Right got | VP.toList got == VP.toList vs -> True
               _ -> False
    let !okCount = length (filter pass [1 .. cases])
    expect ("BYTE_STREAM_SPLIT Float property: "
              ++ show okCount ++ "/" ++ show cases)
           (okCount == cases)

  -- BYTE_STREAM_SPLIT Double
  do
    let pass i =
          let !n = (i `mod` 16) + 1
              !s = seed + i + 3
              !vs = VP.fromList (mkDoubles s n)
          in case decodeByteStreamSplitDouble
                   (VP.length vs)
                   (encodeByteStreamSplitDouble vs) of
               Right got | VP.toList got == VP.toList vs -> True
               _ -> False
    let !okCount = length (filter pass [1 .. cases])
    expect ("BYTE_STREAM_SPLIT Double property: "
              ++ show okCount ++ "/" ++ show cases)
           (okCount == cases)

  -- DELTA_LENGTH_BYTE_ARRAY
  do
    let pass i =
          let !n = (i `mod` 8) + 1
              !vs = V.fromList
                [ BSC.pack ("v_" ++ show j ++ "_" ++ replicate (j `mod` 5) 'x')
                | j <- [0 .. n - 1]
                ]
          in case decodeDeltaLengthByteArray
                   (V.length vs)
                   (encodeDeltaLengthByteArray vs) of
               Right got | V.toList got == V.toList vs -> True
               _ -> False
    let !okCount = length (filter pass [1 .. cases])
    expect ("DELTA_LENGTH_BYTE_ARRAY property: "
              ++ show okCount ++ "/" ++ show cases)
           (okCount == cases)

  -- DELTA_BYTE_ARRAY
  do
    let pass i =
          let !n = (i `mod` 8) + 1
              -- Build a sequence with non-trivial common
              -- prefixes so the prefix-compression actually
              -- exercises the front-coding path.
              !vs = V.fromList
                [ BSC.pack ("alphabet" ++ replicate (j `mod` 4) 'z')
                | j <- [0 .. n - 1]
                ]
          in case decodeDeltaByteArray
                   (V.length vs)
                   (encodeDeltaByteArray vs) of
               Right got | V.toList got == V.toList vs -> True
               _ -> False
    let !okCount = length (filter pass [1 .. cases])
    expect ("DELTA_BYTE_ARRAY property: "
              ++ show okCount ++ "/" ++ show cases)
           (okCount == cases)

-- | encodeParquet with writeBloomFilters set should emit a
-- bloom-filter region for the named column and stamp
-- (offset, length) on its column metadata. The resulting
-- filter should pass membership tests for every value the
-- writer saw and reject obviously-absent values (modulo the
-- nominal false-positive rate).
highLevelBloomFilters :: IO ()
highLevelBloomFilters = do
  let !schema = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
        , SchemaElement "sku" (Just Required) (Just PTByteArray) Nothing
            (Just CTUtf8) Nothing Nothing Nothing
        ]
      !skus = V.fromList
        [ BSC.pack "AAA001", BSC.pack "AAA002", BSC.pack "BBB100"
        , BSC.pack "CCC999", BSC.pack "AAA001"
        ]
      !batch = V.singleton (ColByteArray skus)
      !opts  = PHL.defaultWriteOptions
                 { PHL.writePageVersion  = PageV1
                 , PHL.writeCompression  = Uncompressed
                 , PHL.writeBloomFilters = ["sku"]
                 }
      !bytes = PHL.encodeParquet opts schema [batch]
  case loadParquetFile bytes of
    Left e  -> failTest $ "highLevelBloom loadParquetFile: " ++ e
    Right pf -> do
      let !rg = V.unsafeIndex (fmRowGroups (pfFooter pf)) 0
          !cc = V.unsafeIndex (rgColumns rg) 0
      case ccMetadata cc of
        Nothing -> failTest "highLevelBloom: column metadata missing"
        Just md -> do
          expect "highLevelBloom: writes bloom-filter offset"
            (cmBloomFilterOffset md /= Nothing)
          expect "highLevelBloom: writes bloom-filter length"
            (cmBloomFilterLength md /= Nothing)
          case (cmBloomFilterOffset md, cmBloomFilterLength md) of
            (Just off, Just len) -> do
              let !o   = fromIntegral off :: Int
                  !l   = fromIntegral len :: Int
                  !blob = BS.take l (BS.drop o bytes)
              case decodeBloomFilter blob of
                Left e -> failTest $ "highLevelBloom decodeBloomFilter: " ++ e
                Right (_, sbbf) -> do
                  expect "highLevelBloom: present sku passes filter"
                    (sbbfCheck (BSC.pack "AAA001") sbbf)
                  expect "highLevelBloom: present sku BBB100 passes filter"
                    (sbbfCheck (BSC.pack "BBB100") sbbf)
                  -- A sku that wasn't inserted /almost certainly/
                  -- doesn't pass; with a 1% FPP at ~5 distinct
                  -- entries the chance of a false positive on
                  -- this one literal is negligible.
                  expect "highLevelBloom: absent sku rejected"
                    (not (sbbfCheck (BSC.pack "ZZZZZ") sbbf))
            _ -> failTest "highLevelBloom: missing offset/length"

-- | Synthesize column chunks that exercise encoding paths the
-- wireform writer doesn't naturally produce — DELTA_BINARY_PACKED
-- INT32, BYTE_STREAM_SPLIT FLOAT, DELTA_BYTE_ARRAY and
-- DATA_PAGE_V2 — and round-trip them through the generic chunk
-- readers.
jsonExtensionRegression :: IO ()
jsonExtensionRegression = do
  let field = AT.Field "doc" True AT.AUtf8 V.empty Nothing
        (V.fromList [("ARROW:extension:name", "arrow.json"), ("ARROW:extension:metadata", "{}")])
      schema = AT.Schema (V.singleton field) AT.Little V.empty V.empty
      column = AC.fromMaybeTexts (V.fromList [Just "{\"k\":1}", Nothing, Just "[true,null]"])
  (physical, groups) <- either failTest pure (PArrow.arrowToParquetMixed schema [V.singleton column])
  expect "Arrow JSON writes Parquet JSON annotations"
    (seLogicalType (physical V.! 1) == Just LTJson && seConvertedType (physical V.! 1) == Just CTJson)
  file <- either failTest pure (loadParquetFile (PHL.encodeParquetMixed PHL.defaultWriteOptions physical groups))
  expect "Arrow JSON metadata survives a file round-trip"
    (PArrow.parquetFileArrowSchemaChecked file == Right schema)
  expect "Arrow JSON storage survives a file round-trip"
    (PArrow.parquetRowGroupToArrow schema file 0 == Right (V.singleton column))
  nestedSchema <- either failTest pure (PArrow.arrowFieldToNestedSchema field)
  rows <- either failTest pure (PArrow.columnArrayToNestedRows field column)
  nestedBytes <- either failTest pure (Nested.buildNestedFile (V.singleton ("doc", nestedSchema)) (V.singleton rows))
  nested <- either failTest pure (loadParquetFile nestedBytes)
  expect "nested writer retains JSON annotations"
    (PArrow.parquetFileArrowSchemaChecked nested == Right schema)


nestedReadRegression :: IO ()
nestedReadRegression = do
  expect "nested writer rejects values of the wrong physical type"
    (isLeft (Nested.buildNestedFile
      (V.singleton ("x", Nested.NSPrimitive Nested.LtInt32))
      (V.singleton (V.singleton (Nested.NRLeaf (Nested.LvDouble 1.5))))))
  let primitive name ty nullable = AT.Field name nullable ty V.empty Nothing V.empty
      childI = primitive "value" (AT.AInt 32 True) False
      childD = primitive "value" (AT.AFloatingPoint AT.DoublePrecision) False
      struct name child = AT.Field name False AT.AStruct (V.singleton child) Nothing V.empty
      fields = V.fromList [struct "left" childI, struct "right" childD]
      schema = AT.Schema fields AT.Little V.empty V.empty
      integers = AC.primColumn AC.PInt32 (VS.fromList [10,20])
      doubles = AC.primColumn AC.PDouble (VS.fromList [1.5,2.5])
  left <- either failTest pure (AC.mkStruct 2 Nothing (V.singleton ("value", integers)))
  right <- either failTest pure (AC.mkStruct 2 Nothing (V.singleton ("value", doubles)))
  nested <- either failTest pure (V.mapM PArrow.arrowFieldToNestedSchema fields)
  columns <- either failTest pure (V.zipWithM PArrow.columnArrayToNestedRows fields (V.fromList [left,right]))
  bytes <- either failTest pure (Nested.buildNestedFile
    (V.zip (AT.fieldName <$> fields) nested)
    columns)
  pf <- either failTest pure (loadParquetFile bytes)
  inferred <- either failTest pure (PArrow.parquetFileArrowSchemaChecked pf)
  expect "nested schema preserves parents and duplicate leaf names" (inferred == schema)
  let malformed = pf {Parquet.Read.pfFooter = (pfFooter pf)
        {fmSchema = V.empty}}
      duplicates = pf {Parquet.Read.pfFooter = (pfFooter pf)
        {fmSchema = V.map (\node -> if seName node == "right" then node {seName = "left"} else node)
          (fmSchema (pfFooter pf))}}
  expect "checked schema inference rejects a missing root"
    (isLeft (PArrow.parquetFileArrowSchemaChecked malformed))
  expect "high-level decode reports malformed schema trees before returning a file"
    (case PHL.decodeParquet PHL.defaultReadOptions (parquetMagic <> Footer.writeFooter (pfFooter malformed)) of
      Left err -> "missing its root" `isInfixOf` err
      Right _ -> False)
  expect "checked schema inference rejects duplicate siblings"
    (isLeft (PArrow.parquetFileArrowSchemaChecked duplicates))
  let decoded = PArrow.parquetRowGroupToArrow inferred pf 0
  unless (decoded == Right (V.fromList [left,right]))
    (failTest ("nested struct leaves keep their own physical column ordinals: " ++ show decoded))
  let mapSchema = Nested.NSRequired (Nested.NSMap
        (Nested.NSRequired (Nested.NSPrimitive Nested.LtInt32))
        (Nested.NSOptional (Nested.NSPrimitive Nested.LtInt32)))
      pair k v = (Nested.NRLeaf (Nested.LvInt32 k), Nested.NRLeaf (Nested.LvInt32 v))
      rows = V.singleton (V.fromList
        [ Nested.NRMapEntries (V.fromList [pair 1 10, pair 2 20])
        , Nested.NRMapEntries (V.singleton (pair 3 30)) ])
  mapBytes <- either failTest pure (Nested.buildNestedFile (V.singleton ("m",mapSchema)) rows)
  mapFile <- either failTest pure (loadParquetFile mapBytes)
  expect "direct physical-leaf reads reject repeated containers"
    (isLeft (PArrow.readParquetColumn mapFile 0 0
      (primitive "key" (AT.AInt 32 True) True)))
  let nullableStruct = Nested.NSOptional (Nested.NSStruct (V.singleton
        ("value", Nested.NSRequired (Nested.NSPrimitive Nested.LtInt32))))
  nullableBytes <- either failTest pure (Nested.buildNestedFile (V.singleton ("parent", nullableStruct))
    (V.singleton (V.fromList [Nested.NRNull, Nested.NRStruct (V.singleton (Nested.NRLeaf (Nested.LvInt32 42)))])))
  nullableFile <- either failTest pure (loadParquetFile nullableBytes)
  nullableValue <- either failTest pure (PArrow.readParquetColumn nullableFile 0 0
    (primitive "value" (AT.AInt 32 True) True))
  expect "direct required-leaf reads preserve nullable ancestors"
    (case AC.asPrim AC.PInt32 nullableValue of
      Just array -> AC.primAt array 0 == Nothing && AC.primAt array 1 == Just 42
      Nothing -> False)
  expect "direct required-leaf reads cannot hide null ancestors from a required target"
    (isLeft (PArrow.readParquetColumn nullableFile 0 0 (primitive "value" (AT.AInt 32 True) False)))
  let metadata = (rgColumns (fmRowGroups (pfFooter mapFile) V.! 0) V.! 1)
  valueMetadata <- maybe (failTest "missing map value metadata") pure (ccMetadata metadata)
  (header, body) <- either failTest pure (readPageHeaderAt mapBytes (fromIntegral (cmDataPageOffset valueMetadata)))
  case phType header of
    PtDataPageV2 page -> do
      expect "nested V2 page counts rows independently of repeated events"
        (dph2NumRows page == 2 && dph2NumValues page == 3)
      let changed = LE.encodeRLEHybrid 1 (VP.fromList [0,0,1])
          repLength = fromIntegral (dph2RepLevelsLen page)
      expect "map mismatch regression preserves page lengths" (BS.length changed == repLength)
      corrupt <- either failTest pure (loadParquetFile (BS.take body mapBytes <> changed <> BS.drop (body + repLength) mapBytes))
      let result = PArrow.parquetRowGroupToArrow (PArrow.parquetFileArrowSchema corrupt) corrupt 0
      expect "map siblings with equal totals but different row boundaries are rejected"
        (case result of Left err -> "siblings disagree" `isInfixOf` show err; Right _ -> False)
    _ -> failTest "expected a V2 map page"


legacyLevelsRegression :: IO ()
legacyLevelsRegression = do
  let chunk count levelBytes valueBytes encoding =
        let body = levelBytes <> valueBytes
        in Thrift.encodeCompact (TV.Struct (V.fromList
          [ TS.PageHeader_Type 0
          , TS.PageHeader_UncompressedSize (fromIntegral (BS.length body))
          , TS.PageHeader_CompressedSize (fromIntegral (BS.length body))
          , TS.PageHeader_DataPageHeader (V.fromList
              [ TS.DataPageHeader_NumValues count, TS.DataPageHeader_Encoding 0
              , TS.DataPageHeader_DefinitionLevelEncoding encoding
              , TS.DataPageHeader_RepetitionLevelEncoding 3 ]) ])) <> body
      int n = BS.pack [n,0,0,0]
  expect "legacy MSB-first one-bit definition levels"
    (Parquet.Read.readGenericInt32OptionalColumnChunk Uncompressed 0 1
      (chunk 5 (BS.singleton 0xb0) (int 10 <> int 20 <> int 30) 4)
      == Right (V.fromList [Just 10, Nothing, Just 20, Just 30, Nothing]))
  levels <- either failTest pure (Parquet.Read.readInt32Levels Uncompressed 0 7
    (chunk 8 (BS.pack [0x05,0x39,0x77]) (int 42) 4))
  expect "legacy three-bit levels use the Apache MSB-first golden"
    (Parquet.Read.clDefinition levels == VP.fromList [0..7]
      && Parquet.Read.clValues levels == VP.singleton 42)
  grouped <- either failTest pure (Parquet.Read.readInt32Levels Uncompressed 0 7
    (chunk 17 (BS.pack [0x05,0x39,0x77,0x05,0x39,0x77,0]) (int 42 <> int 43) 4))
  expect "legacy levels cross complete groups and a partial trailing byte"
    (Parquet.Read.clDefinition grouped == VP.generate 17 (\i -> fromIntegral (i `rem` 8))
      && Parquet.Read.clValues grouped == VP.fromList [42,43])
  expect "truncated legacy levels are rejected"
    (isLeft (Parquet.Read.readInt32Levels Uncompressed 0 7 (chunk 8 (BS.singleton 0) BS.empty 4)))
  expect "unknown nonzero-width level encodings are rejected"
    (isLeft (Parquet.Read.readInt32Levels Uncompressed 0 1 (chunk 1 BS.empty BS.empty 5)))
  let v2 reps defs values count =
        let body = reps <> defs <> values
            page = PageHeader (PtDataPageV2 (DataPageHeaderV2 count 0 count 0
              (fromIntegral (BS.length defs)) (fromIntegral (BS.length reps)) False))
              (Just (fromIntegral (BS.length body))) (Just (fromIntegral (BS.length body)))
        in encodePageHeader page <> body
  expect "V2 optional pages cannot omit their definition levels"
    (isLeft (Parquet.Read.readInt32Levels Uncompressed 0 1 (v2 BS.empty BS.empty (int 42) 1)))
  expect "a repeated column must begin at repetition level zero"
    (isLeft (Parquet.Read.readInt32Levels Uncompressed 1 1
      (v2 (LE.encodeRLEHybrid 1 (VP.fromList [1,0,1]))
          (LE.encodeRLEHybrid 1 (VP.replicate 3 1)) (int 1 <> int 2 <> int 3) 3)))


manyPageRegression :: IO ()
manyPageRegression = do
  let nPages = 97
      rowsPerPage = 17
      required page = VP.generate rowsPerPage (\i -> fromIntegral (page * rowsPerPage + i) :: Int32)
      optional page = V.generate rowsPerPage (\i ->
        if i `rem` 3 == 0 then Nothing else Just (fromIntegral (page * rowsPerPage + i) :: Int32))
      requiredPage version page = case version of
        PageV1 -> let (header, payload) = encodeColumnDataPageParts (ColInt32 (required page)) in header <> payload
        PageV2 -> either error id (encodeColumnDataPageV2 Uncompressed (ColInt32 (required page)))
      optionalPage version page = case version of
        PageV1 -> encodeOptionalColumnPage (OptInt32 (optional page))
        PageV2 -> case encodeOptionalColumnPageV2Parts Uncompressed (OptInt32 (optional page)) of
          Left err -> error err
          Right (header, reps, defs, values) -> BS.concat [header, reps, defs, values]
      checkPages version makePage decode select expected = do
        let pages = V.generate nPages (makePage version)
            offsets = V.scanl' (+) 0 (BS.length <$> pages)
            file = BS.concat (V.toList pages)
            locations = V.imap (\i page -> PageLocation
              (fromIntegral (offsets V.! i)) (fromIntegral (BS.length page))
              (fromIntegral (i * rowsPerPage))) pages
            keep = V.generate nPages (\i -> i `rem` 3 /= 1)
        expect ("many-page decode preserves order " ++ show version)
          (decode file == Right (V.concat (V.toList (V.generate nPages expected))))
        expect ("many-page selection preserves order " ++ show version)
          (select file locations keep == Right (V.concat (V.toList
            (V.imapMaybe (\i yes -> if yes then Just (expected i) else Nothing) keep))))
  mapM_ (\version -> do
    checkPages version requiredPage
      (fmap VP.convert . Parquet.Read.readGenericInt32ColumnChunk Uncompressed)
      (\file locations keep -> VP.convert <$>
        Parquet.Read.readGenericInt32SelectedPages Uncompressed file locations keep)
      (VP.convert . required)
    checkPages version optionalPage
      (Parquet.Read.readGenericInt32OptionalColumnChunk Uncompressed 0 1)
      (Parquet.Read.readGenericInt32OptionalSelectedPages Uncompressed 0 1)
      optional) [PageV1, PageV2]


genericPageDispatch :: IO ()
genericPageDispatch = do
  -- DELTA_BINARY_PACKED INT32 in a DATA_PAGE V1.
  do
    let !values = VP.fromList ([10, 20, 30, 40, 50] :: [Int32])
        !payload = encodeDeltaBinaryPackedInt32 values
        !numVals = VP.length values
        !hdr = PageHeader
          { phType = PtDataPage
              (DataPageHeader { dphNumValues = fromIntegral numVals
                              , dphEncoding  = 5  -- DELTA_BINARY_PACKED
                              })
          , phUncompressedPageSize = Just (fromIntegral (BS.length payload))
          , phCompressedPageSize   = Just (fromIntegral (BS.length payload))
          }
        !chunk = encodePageHeader hdr <> payload
    case Parquet.Read.readGenericInt32ColumnChunk Uncompressed chunk of
      Right v | VP.toList v == VP.toList values ->
        putStrLn "OK: generic INT32 dispatch handles DELTA_BINARY_PACKED"
      Right v -> failTest $ "DELTA_BINARY_PACKED Int32 mismatch: got "
                            ++ show (VP.toList v)
      Left e  -> failTest $ "DELTA_BINARY_PACKED Int32: " ++ e

  -- BYTE_STREAM_SPLIT FLOAT in DATA_PAGE V1.
  do
    let !values = VP.fromList ([1.0, -2.5, 3.14, 99.9] :: [Float])
        !payload = encodeByteStreamSplitFloat values
        !hdr = PageHeader
          { phType = PtDataPage
              (DataPageHeader { dphNumValues = fromIntegral (VP.length values)
                              , dphEncoding  = 9  -- BYTE_STREAM_SPLIT
                              })
          , phUncompressedPageSize = Just (fromIntegral (BS.length payload))
          , phCompressedPageSize   = Just (fromIntegral (BS.length payload))
          }
        !chunk = encodePageHeader hdr <> payload
    case Parquet.Read.readGenericFloatColumnChunk Uncompressed chunk of
      Right v | VP.toList v == VP.toList values ->
        putStrLn "OK: generic FLOAT dispatch handles BYTE_STREAM_SPLIT"
      Right v -> failTest $ "BYTE_STREAM_SPLIT Float mismatch: got "
                            ++ show (VP.toList v)
      Left e  -> failTest $ "BYTE_STREAM_SPLIT Float: " ++ e

  -- DELTA_BYTE_ARRAY for BYTE_ARRAY in DATA_PAGE V1.
  do
    let !values = V.fromList
          [ BSC.pack "alpha"
          , BSC.pack "alphanumeric"
          , BSC.pack "alphabet"
          ]
        !payload = encodeDeltaByteArray values
        !hdr = PageHeader
          { phType = PtDataPage
              (DataPageHeader { dphNumValues = fromIntegral (V.length values)
                              , dphEncoding  = 7  -- DELTA_BYTE_ARRAY
                              })
          , phUncompressedPageSize = Just (fromIntegral (BS.length payload))
          , phCompressedPageSize   = Just (fromIntegral (BS.length payload))
          }
        !chunk = encodePageHeader hdr <> payload
    case Parquet.Read.readGenericByteArrayColumnChunk Uncompressed chunk of
      Right v | V.toList v == V.toList values ->
        putStrLn "OK: generic BYTE_ARRAY dispatch handles DELTA_BYTE_ARRAY"
      Right v -> failTest $ "DELTA_BYTE_ARRAY mismatch: got "
                            ++ show (V.toList v)
      Left e  -> failTest $ "DELTA_BYTE_ARRAY: " ++ e

  -- PLAIN INT64 wrapped in a DATA_PAGE_V2 (no level streams,
  -- uncompressed). This exercises the V2 page header dispatch.
  do
    let !values = VP.fromList ([100, 200, 300] :: [Int64])
        !payload = mconcat
          [ BS.pack
              [ fromIntegral (v `mod` 256)
              , fromIntegral ((v `div` 256) `mod` 256)
              , fromIntegral ((v `div` 65536) `mod` 256)
              , 0, 0, 0, 0, 0
              ]
          | v <- VP.toList values
          ]
        !hdr = PageHeader
          { phType = PtDataPageV2
              DataPageHeaderV2
                { dph2NumValues    = fromIntegral (VP.length values)
                , dph2NumNulls     = 0
                , dph2NumRows      = fromIntegral (VP.length values)
                , dph2Encoding     = 0  -- PLAIN
                , dph2DefLevelsLen = 0
                , dph2RepLevelsLen = 0
                , dph2IsCompressed = False
                }
          , phUncompressedPageSize = Just (fromIntegral (BS.length payload))
          , phCompressedPageSize   = Just (fromIntegral (BS.length payload))
          }
        !chunk = encodePageHeader hdr <> payload
    case Parquet.Read.readGenericInt64ColumnChunk Uncompressed chunk of
      Right v | VP.toList v == VP.toList values ->
        putStrLn "OK: generic INT64 dispatch handles DATA_PAGE_V2"
      Right v -> failTest $ "DATA_PAGE_V2 Int64 mismatch: got "
                            ++ show (VP.toList v)
      Left e  -> failTest $ "DATA_PAGE_V2 Int64: " ++ e

predicateBloomSkipping :: IO ()
predicateBloomSkipping = do
  let -- Bloom filter built from i32 PLAIN payloads of 10, 20, 30
      bf0 = newSbbf (optimalNumBytes 1024 0.01)
      bf  = foldr sbbfInsert bf0
              [ BS.pack [10, 0, 0, 0]
              , BS.pack [20, 0, 0, 0]
              , BS.pack [30, 0, 0, 0]
              ]
  expect "bloom keeps known-present 20"
    (Pred.evalBloomChunk (physicalField PTInt32) bf
       (Pred.PEq (Pred.PVInt32 20)) == Pred.PMaybeKeep)
  expect "bloom skips known-absent 99"
    (Pred.evalBloomChunk (physicalField PTInt32) bf
       (Pred.PEq (Pred.PVInt32 99)) == Pred.PSkip)
  expect "bloom keeps PIn that contains a present"
    (Pred.evalBloomChunk (physicalField PTInt32) bf
       (Pred.PIn [Pred.PVInt32 99, Pred.PVInt32 20]) == Pred.PMaybeKeep)
  expect "bloom skips PIn of all-absent"
    (Pred.evalBloomChunk (physicalField PTInt32) bf
       (Pred.PIn [Pred.PVInt32 99, Pred.PVInt32 100]) == Pred.PSkip)

arrowParquetProjection :: IO ()
arrowParquetProjection = do
  -- Write a 3-column file (a=int32, b=int32, c=utf8), then read
  -- it back with target schema (c, a) (reordered + narrowed).
  let !fullSchema = AT.Schema
        { AT.arrowFields = V.fromList
            [ AT.Field "a" False (AT.AInt 32 True) V.empty Nothing V.empty
            , AT.Field "b" False (AT.AInt 32 True) V.empty Nothing V.empty
            , AT.Field "c" False AT.AUtf8          V.empty Nothing V.empty
            ]
        , AT.arrowEndianness = AT.Little
        , AT.arrowMetadata   = V.empty
        , AT.arrowFeatures = V.empty
        }
      !batch = V.fromList
        [ AC.primColumn AC.PInt32 (VS.fromList [10, 20, 30 :: Int32])
        , AC.primColumn AC.PInt32 (VS.fromList [40, 50, 60 :: Int32])
        , AC.fromTexts (V.fromList ["alpha", "beta", "gamma"])
        ]
  case PArrow.arrowToParquet fullSchema [batch] of
    Left  e -> failTest $ "projection arrowToParquet: " ++ e
    Right (pSchema, pRgs) -> do
      let !opts = PHL.defaultWriteOptions
                    { PHL.writePageVersion = PageV1
                    , PHL.writeCompression = Uncompressed
                    }
          !bytes = PHL.encodeParquet opts pSchema pRgs
      case PHL.decodeParquet PHL.defaultReadOptions bytes of
        Left  e -> failTest $ "projection decodeParquet: " ++ e
        Right pf -> do
          -- Target: (c, a), reordered and projected.
          let !target = AT.Schema
                { AT.arrowFields = V.fromList
                    [ AT.Field "c" False AT.AUtf8          V.empty Nothing V.empty
                    , AT.Field "a" False (AT.AInt 32 True) V.empty Nothing V.empty
                    ]
                , AT.arrowEndianness = AT.Little
                , AT.arrowMetadata   = V.empty
                , AT.arrowFeatures = V.empty
                }
          case PArrow.parquetRowGroupToArrow target pf 0 of
            Left  e    -> failTest $ "parquetRowGroupToArrow: " ++ show e
            Right cols -> do
              let !expected = V.fromList
                    [ AC.fromTexts (V.fromList ["alpha", "beta", "gamma"])
                    , AC.primColumn AC.PInt32 (VS.fromList [10, 20, 30 :: Int32])
                    ]
              if cols == expected
                then putStrLn "OK: Parquet projection + reorder (c, a <- a, b, c)"
                else failTest $ "projection mismatch:\n got "
                                  ++ show (V.toList cols)

          -- Test a missing column.
          let !missing = AT.Schema
                { AT.arrowFields = V.singleton
                    (AT.Field "z" False (AT.AInt 32 True) V.empty Nothing V.empty)
                , AT.arrowEndianness = AT.Little
                , AT.arrowMetadata   = V.empty
                , AT.arrowFeatures = V.empty
                }
          case PArrow.parquetRowGroupToArrow missing pf 0 of
            Left (PArrow.MissingColumn "z") ->
              putStrLn "OK: Parquet projection surfaces MissingColumn"
            other -> failTest $ "expected MissingColumn, got " ++ show other

          -- Test coercion: Int32 -> Int64.
          let !widen = AT.Schema
                { AT.arrowFields = V.singleton
                    (AT.Field "a" False (AT.AInt 64 True) V.empty Nothing V.empty)
                , AT.arrowEndianness = AT.Little
                , AT.arrowMetadata   = V.empty
                , AT.arrowFeatures = V.empty
                }
          case PArrow.parquetRowGroupToArrow widen pf 0 of
            Left e -> failTest $ "projection coercion: " ++ show e
            Right cols ->
              if cols == V.singleton (AC.primColumn AC.PInt64 (VS.fromList [10, 20, 30 :: Int64]))
                then putStrLn "OK: Parquet projection coerces Int32 -> Int64"
                else failTest $ "coercion mismatch: " ++ show (V.toList cols)

arrowParquetNestedBridge :: IO ()
arrowParquetNestedBridge = do
  -- Build an Arrow struct<i32, utf8> column + wrap it in a nested
  -- Parquet file. Verifies that arrowFieldToNestedSchema +
  -- columnArrayToNestedRows produce a valid NestedRow tree and
  -- encodeParquetNested accepts it.
  let structField = AT.Field "point" False AT.AStruct
        (V.fromList
           [ AT.Field "x" False (AT.AInt 32 True) V.empty Nothing V.empty
           , AT.Field "name" False AT.AUtf8        V.empty Nothing V.empty
           ])
        Nothing
        V.empty
      structCol = either error id $ AC.mkStruct 3 Nothing (V.fromList
        [ ("x",    AC.primColumn AC.PInt32 (VS.fromList [1, 2, 3 :: Int32]))
        , ("name", AC.fromTexts (V.fromList ["a", "b", "c"]))
        ])
  case PArrow.arrowFieldToNestedSchema structField of
    Left  e   -> failTest $ "arrowFieldToNestedSchema: " ++ e
    Right sch -> case PArrow.columnArrayToNestedRows structField structCol of
      Left  e   -> failTest $ "columnArrayToNestedRows: " ++ e
      Right rows ->
        let !cols = V.singleton ("point", sch)
            !cr   = V.singleton rows
        in  case PHL.encodeParquetNested cols cr of
              Left  e  -> failTest $ "encodeParquetNested: " ++ e
              Right bs ->
                if BS.length bs > 4 && BS.take 4 bs == BSC.pack "PAR1"
                  then putStrLn "OK: Arrow <-> Parquet nested bridge"
                  else failTest $ "nested bridge produced invalid Parquet file"

arrowParquetBridge :: IO ()
arrowParquetBridge = do
  let !arrowSchema = AT.Schema
        { AT.arrowFields = V.fromList
            [ AT.Field "i" False (AT.AInt 32 True) V.empty Nothing V.empty
            , AT.Field "s" False AT.AUtf8           V.empty Nothing V.empty
            ]
        , AT.arrowEndianness = AT.Little
        , AT.arrowMetadata   = V.empty
        , AT.arrowFeatures = V.empty
        }
      !batch = V.fromList
        [ AC.primColumn AC.PInt32 (VS.fromList ([10, 20, 30] :: [Int32]))
        , AC.fromTexts (V.fromList ["alpha", "beta", "gamma"])
        ]
  case PArrow.arrowToParquet arrowSchema [batch] of
    Left  e  -> failTest $ "arrowToParquet: " ++ e
    Right (psSchema, rgs) -> do
      let !opts = PHL.defaultWriteOptions
                    { PHL.writePageVersion = PageV1
                    , PHL.writeCompression = Uncompressed
                    }
          !bytes = PHL.encodeParquet opts psSchema rgs
      case PHL.decodeParquet PHL.defaultReadOptions bytes of
        Left  e -> failTest $ "decodeParquet (bridge): " ++ e
        Right pf ->
          case PArrow.parquetRowGroupToArrow arrowSchema pf 0 of
            Left  e    -> failTest $ "parquetRowGroupToArrow: " ++ show e
            Right cols ->
              if cols == batch
                then putStrLn "OK: Arrow ↔ Parquet bridge round-trip"
                else failTest $ "bridge round-trip mismatch:\n got "
                                 ++ show (V.toList cols)
                                 ++ "\n exp " ++ show (V.toList batch)

  -- streamRowGroups: drain the same single-row-group file as a
  -- list and assert every row group materialises correctly.
  case PArrow.arrowToParquet arrowSchema [batch] of
    Left  e -> failTest $ "arrowToParquet (stream): " ++ e
    Right (psSchema, rgs) -> do
      let !opts = PHL.defaultWriteOptions
                    { PHL.writePageVersion = PageV1
                    , PHL.writeCompression = Uncompressed
                    }
          !bytes = PHL.encodeParquet opts psSchema rgs
      case PHL.decodeParquet PHL.defaultReadOptions bytes of
        Left  e -> failTest $ "decodeParquet (stream): " ++ e
        Right pf -> do
          let results = PArrow.streamRowGroups arrowSchema pf
          case results of
            [Left e] -> failTest $ "streamRowGroups (rg 0): " ++ e
            [Right cols] ->
              if cols == batch
                then putStrLn "OK: streamRowGroups iterates row groups"
                else failTest $ "streamRowGroups mismatch"
            _ ->
              failTest $ "streamRowGroups: expected 1 row group, got "
                ++ show (length results)

  -- Temporal bridge: Date32, Timestamp round-trip through the
  -- Arrow -> Parquet -> Arrow path.
  let !tempSchema = AT.Schema
        { AT.arrowFields = V.fromList
            [ AT.Field "d"  False (AT.ADate AT.DateDay) V.empty Nothing V.empty
            , AT.Field "ts" False (AT.ATimestamp AT.Microsecond Nothing) V.empty Nothing V.empty
            ]
        , AT.arrowEndianness = AT.Little
        , AT.arrowMetadata   = V.empty
        , AT.arrowFeatures = V.empty
        }
      !tempBatch = V.fromList
        [ AC.primColumn AC.PDate32    (VS.fromList ([19000, 19001, 19002] :: [Int32]))
        , AC.primColumn AC.PTimestamp (VS.fromList ([1700000000000000, 1700001000000000, 1700002000000000] :: [Int64]))
        ]
  case PArrow.arrowToParquet tempSchema [tempBatch] of
    Left  e -> failTest $ "temporal arrowToParquet: " ++ e
    Right (tSchema, tRgs) -> do
      let !tOpts = PHL.defaultWriteOptions
                      { PHL.writePageVersion = PageV1
                      , PHL.writeCompression = Uncompressed
                      }
          !tBytes = PHL.encodeParquet tOpts tSchema tRgs
      case PHL.decodeParquet PHL.defaultReadOptions tBytes of
        Left  e -> failTest $ "temporal decodeParquet: " ++ e
        Right pf ->
          case PArrow.parquetRowGroupToArrow tempSchema pf 0 of
            Left  e    -> failTest $ "temporal parquetRowGroupToArrow: " ++ show e
            Right cols ->
              if cols == tempBatch
                then putStrLn "OK: Arrow <-> Parquet temporal round-trip"
                else failTest $ "temporal mismatch:\n got "
                                  ++ show (V.toList cols)
                                  ++ "\n exp " ++ show (V.toList tempBatch)

  -- Nested bridge + Parquet.HighLevel.encodeParquetNested:
  -- struct + list Arrow columns lower to Parquet.Nested.NestedRow
  -- trees and the nested writer produces a valid Parquet file.
  arrowParquetNestedBridge

  -- Projection / schema evolution: read a 3-column file back with
  -- a 2-column target schema in a different order.
  arrowParquetProjection

expectHash :: String -> String -> IO ()
expectHash s expected = expectHashBs (BSC.pack s) expected

expectHashBs :: BS.ByteString -> String -> IO ()
expectHashBs bs expected =
  let actual = pad16 (showHex (Hash.xxh64 0 bs) "")
  in unless (actual == expected) $
       failTest ("xxh64 " ++ show bs ++ " expected " ++ expected
                  ++ " got " ++ actual)

pad16 :: String -> String
pad16 s = replicate (16 - length s) '0' ++ s

expect :: String -> Bool -> IO ()
expect what ok = do
  if ok
    then putStrLn ("OK: " ++ what)
    else failTest what

failTest :: String -> IO a
failTest msg = do
  putStrLn ("FAIL: " ++ msg)
  exitFailure

-- ============================================================
-- Pyarrow availability + nested-list cross-language check
-- ============================================================

pyarrowAvailable :: IO Bool
pyarrowAvailable = do
  (code, _, _) <- Proc.readProcessWithExitCode
                    "python3" ["-c", "import pyarrow.parquet"] ""
  pure (code == ExitSuccess)

validateNestedAgainstPyarrow :: IO ()
validateNestedAgainstPyarrow = do
  let rows = V.fromList
        [ Just (V.fromList [Just (1 :: Int32), Nothing, Just 3])
        , Just V.empty
        , Nothing
        ]
      leaf = Nested.encodeOptionalListOptionalI32 "xs" rows
      bytes = Nested.buildOptionalListFile PTInt32 leaf 3
      filePath = "/tmp/wireform-nested-list.parquet"
  BS.writeFile filePath bytes
  pyarrowAssert "nested optional-list<optional INT32> reads back via pyarrow"
    [ "t = pq.read_table('" ++ filePath ++ "').to_pylist()"
    , "exp = [{'xs': [1, None, 3]}, {'xs': []}, {'xs': None}]"
    , "assert t == exp, f'roundtrip mismatch: {t!r}'"
    ]

-- | Run a python snippet under pyarrow and assert that it prints
-- 'PYARROW_OK'. Snippet header (importing pyarrow.parquet as pq) is
-- prepended automatically.
pyarrowAssert :: String -> [String] -> IO ()
pyarrowAssert label snippet = do
  (code, out, err) <- Proc.readProcessWithExitCode "python3"
    [ "-c"
    , unlines
        ( "import pyarrow.parquet as pq"
        : snippet
       ++ ["print('PYARROW_OK')"]
        )
    ] ""
  case code of
    ExitSuccess
      | "PYARROW_OK" `isInfixOf` out ->
          expect label True
      | otherwise -> failTest ("pyarrow: unexpected output: " ++ out)
    _ -> failTest ("pyarrow rejected our nested file:\nstdout=" ++ out
                    ++ "\nstderr=" ++ err)

finiteVectorListFallback :: IO ()
finiteVectorListFallback = do
  let fieldSchema = Nested.NSOptional (Nested.NSList (Nested.NSPrimitive Nested.LtInt32))
      vector x y = Nested.NRList (V.fromList (map (Nested.NRLeaf . Nested.LvInt32) [x, y]))
      rows = V.fromList [vector 1 2, Nested.NRNull, vector 3 4]
  listFile <- either failTest pure (Nested.buildNestedFile (V.singleton ("embedding", fieldSchema)) (V.singleton rows))
  footer <- either failTest pure (Footer.readFooter listFile)
  trailer <- either failTest pure (Footer.readFooterTrailer listFile)
  let prefix = BS.take (BS.length listFile - BS.length (Footer.ftBytes trailer) - 8) listFile
      annotation n = TS.SchemaElement_LogicalType (V.singleton (20, TV.Struct (V.singleton (1, TV.I32 n))))
      stripList = V.filter (\case
        TS.SchemaElement_LogicalType _ -> False
        TS.SchemaElement_ConvertedType _ -> False
        _ -> True)
      replaceAnnotation newFields = \case
        TS.FileMetadata_Schema schema ->
          TS.FileMetadata_Schema (schema V.// [(1, case schema V.! 1 of
            TV.Struct fields -> TV.Struct (stripList fields V.++ V.fromList newFields)
            other -> other)])
        entry -> entry
      withAnnotation newFields = case Footer.fileMetadataToThrift footer of
        TV.Struct fields ->
          prefix <> Footer.writeRawFooter parquetMagic (Thrift.encodeCompact (TV.Struct (V.map (replaceAnnotation newFields) fields)))
        _ -> error "expected footer struct"
  mapM_ (\converted -> do
    file <- either failTest pure (loadParquetFile (withAnnotation (annotation 2 : converted)))
    arrowSchema <- either failTest pure (PArrow.parquetFileArrowSchemaChecked file)
    let field = V.head (AT.arrowFields arrowSchema)
    expect "FINITE_VECTOR falls back to a nullable LIST"
      (AT.fieldType field == AT.AList && AT.fieldNullable field)
    columns <- either (failTest . show) pure (PArrow.parquetRowGroupToArrow arrowSchema file 0)
    decoded <- either failTest pure (PArrow.columnArrayToNestedRows field (V.head columns))
    expect "FINITE_VECTOR fallback preserves vector elements and null parents" (decoded == rows))
    [[], [TS.SchemaElement_ConvertedType 3]]
  malformed <- either failTest pure (Footer.readFooter (withAnnotation [annotation 0]))
  expect "malformed FINITE_VECTOR does not become LIST"
    (seLogicalType (fmSchema malformed V.! 1) == Just LTUnsupported)


validateListStructAgainstPyarrow :: IO ()
validateListStructAgainstPyarrow = do
  -- Schema: optional list<optional struct<a: int32, b: string>>
  let schema = Nested.NSOptional
                 (Nested.NSList
                   (Nested.NSOptional
                     (Nested.NSStruct (V.fromList
                       [ ("a", Nested.NSOptional (Nested.NSPrimitive Nested.LtInt32))
                       , ("b", Nested.NSOptional (Nested.NSPrimitive Nested.LtString))
                       ]))))
      mkRec :: Maybe (Int32, T.Text) -> Nested.NestedRow
      mkRec Nothing = Nested.NRNull
      mkRec (Just (i, s)) = Nested.NRStruct (V.fromList
        [ Nested.NRLeaf (Nested.LvInt32 i)
        , Nested.NRLeaf (Nested.LvString s)
        ])
      rows = V.fromList
        [ Nested.NRList (V.fromList
            [ mkRec (Just (1, "x"))
            , mkRec Nothing
            , mkRec (Just (2, "y"))
            ])
        , Nested.NRList V.empty
        , Nested.NRNull
        , Nested.NRList (V.singleton (mkRec (Just (3, "z"))))
        ]
  case Nested.buildNestedFile (V.singleton ("xs", schema))
                              (V.singleton rows) of
    Left e -> failTest ("buildNestedFile (list<struct>): " ++ e)
    Right bytes -> do
      let filePath = "/tmp/wireform-list-struct.parquet"
      BS.writeFile filePath bytes
      pyarrowAssert "nested list<struct<a:int32,b:string>> reads back via pyarrow"
        [ "t = pq.read_table('" ++ filePath ++ "').to_pylist()"
        , "exp = [{'xs': [{'a': 1, 'b': 'x'}, None, {'a': 2, 'b': 'y'}]},"
        , "       {'xs': []},"
        , "       {'xs': None},"
        , "       {'xs': [{'a': 3, 'b': 'z'}]}]"
        , "assert t == exp, f'roundtrip mismatch: {t!r}'"
        ]

validateListListAgainstPyarrow :: IO ()
validateListListAgainstPyarrow = do
  -- Schema: optional list<optional list<optional int32>>
  let schema = Nested.NSOptional
                 (Nested.NSList
                   (Nested.NSOptional
                     (Nested.NSList
                       (Nested.NSOptional
                         (Nested.NSPrimitive Nested.LtInt32)))))
      lit :: Int32 -> Nested.NestedRow
      lit = Nested.NRLeaf . Nested.LvInt32
      inner :: [Int32] -> Nested.NestedRow
      inner xs = Nested.NRList (V.fromList (map lit xs))
      rows = V.fromList
        [ Nested.NRList (V.fromList [inner [1, 2], inner [3]])
        , Nested.NRList (V.singleton (Nested.NRList V.empty))
        , Nested.NRNull
        , Nested.NRList (V.singleton (inner [4, 5, 6]))
        ]
  case Nested.buildNestedFile (V.singleton ("xs", schema))
                              (V.singleton rows) of
    Left e -> failTest ("buildNestedFile (list<list>): " ++ e)
    Right bytes -> do
      let filePath = "/tmp/wireform-list-list.parquet"
      BS.writeFile filePath bytes
      pyarrowAssert "nested list<list<int32>> reads back via pyarrow"
        [ "t = pq.read_table('" ++ filePath ++ "').to_pylist()"
        , "exp = [{'xs': [[1, 2], [3]]},"
        , "       {'xs': [[]]},"
        , "       {'xs': None},"
        , "       {'xs': [[4, 5, 6]]}]"
        , "assert t == exp, f'roundtrip mismatch: {t!r}'"
        ]

validateMapAgainstPyarrow :: IO ()
validateMapAgainstPyarrow = do
  -- Schema: optional map<string, int32> with optional value.
  let schema = Nested.NSOptional
                 (Nested.NSMap
                   (Nested.NSRequired (Nested.NSPrimitive Nested.LtString))
                   (Nested.NSOptional (Nested.NSPrimitive Nested.LtInt32)))
      pair :: T.Text -> Maybe Int32 -> (Nested.NestedRow, Nested.NestedRow)
      pair k mv =
        ( Nested.NRLeaf (Nested.LvString k)
        , maybe Nested.NRNull (Nested.NRLeaf . Nested.LvInt32) mv
        )
      rows = V.fromList
        [ Nested.NRMapEntries (V.fromList
            [ pair "k1" (Just 10)
            , pair "k2" (Just 20)
            ])
        , Nested.NRMapEntries V.empty
        , Nested.NRNull
        , Nested.NRMapEntries (V.singleton (pair "k3" (Just 30)))
        ]
  case Nested.buildNestedFile (V.singleton ("xs", schema))
                              (V.singleton rows) of
    Left e -> failTest ("buildNestedFile (map): " ++ e)
    Right bytes -> do
      let filePath = "/tmp/wireform-map.parquet"
      BS.writeFile filePath bytes
      -- pyarrow reads MAP columns as a list of (key, value) tuples;
      -- the dict ordering is preserved.
      pyarrowAssert "nested map<string, int32> reads back via pyarrow"
        [ "t = pq.read_table('" ++ filePath ++ "').to_pylist()"
        , "exp = [{'xs': [('k1', 10), ('k2', 20)]},"
        , "       {'xs': []},"
        , "       {'xs': None},"
        , "       {'xs': [('k3', 30)]}]"
        , "assert t == exp, f'roundtrip mismatch: {t!r}'"
        ]

-- ============================================================
-- Golden pyarrow fixtures
-- ============================================================

goldenSimpleInt :: IO ()
goldenSimpleInt = do
  bs <- BS.readFile "test/fixtures/simple_int.parquet"
  case loadParquetFile bs of
    Left e -> failTest ("golden simple_int: " ++ e)
    Right pf -> do
      let !rgs = fmRowGroups (pfFooter pf)
          !rg  = V.unsafeIndex rgs 0
          !cc  = V.unsafeIndex (rgColumns rg) 0
      expect "golden simple_int: 5 rows" (rgNumRows rg == 5)
      case ccMetadata cc of
        Just m -> do
          expect "golden simple_int: column type INT64"
            (cmType m == PTInt64)
          expect "golden simple_int: numValues == 5"
            (cmNumValues m == 5)
        Nothing -> failTest "golden simple_int: no metadata"

goldenMixedTypes :: IO ()
goldenMixedTypes = do
  bs <- BS.readFile "test/fixtures/mixed_types.parquet"
  case loadParquetFile bs of
    Left e -> failTest ("golden mixed_types: " ++ e)
    Right pf -> do
      let !rgs = fmRowGroups (pfFooter pf)
          !rg  = V.unsafeIndex rgs 0
          !cols = rgColumns rg
      expect "golden mixed_types: 3 columns" (V.length cols == 3)
      let typeAt i = case ccMetadata (V.unsafeIndex cols i) of
            Just m  -> Just (cmType m)
            Nothing -> Nothing
      expect "golden mixed_types: id is INT64"   (typeAt 0 == Just PTInt64)
      expect "golden mixed_types: name is BYTE_ARRAY" (typeAt 1 == Just PTByteArray)
      expect "golden mixed_types: val is DOUBLE" (typeAt 2 == Just PTDouble)
      let codecAt i = case ccMetadata (V.unsafeIndex cols i) of
            Just m  -> Just (cmCodec m)
            Nothing -> Nothing
      expect "golden mixed_types: gzip codec on every column"
        (all (== Just GZip) (map codecAt [0, 1, 2]))

goldenBloomIndex :: IO ()
goldenBloomIndex = do
  bs <- BS.readFile "test/fixtures/bloom_and_index.parquet"
  case loadParquetFile bs of
    Left e -> failTest ("golden bloom_and_index: " ++ e)
    Right pf -> do
      let !rgs = fmRowGroups (pfFooter pf)
          !rg  = V.unsafeIndex rgs 0
          !cc  = V.unsafeIndex (rgColumns rg) 0
      expect "golden bloom_and_index: 100 rows" (rgNumRows rg == 100)
      expect "golden bloom_and_index: page index offsets present"
        (ccOffsetIndexOffset cc /= Nothing && ccColumnIndexOffset cc /= Nothing)


physicalField :: ParquetType -> SchemaElement
physicalField ty = SchemaElement "value" (Just Required) (Just ty) Nothing Nothing Nothing Nothing Nothing


knownContext :: SchemaElement -> Pred.StatisticsContext
knownContext field = Pred.StatisticsContext field (Just TypeDefinedOrder)
