module NestedOptionsRegression (tests) where

import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
import Data.Text.Encoding qualified
import Data.Vector qualified as V
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat)
import Parquet.Arrow qualified as A
import Parquet.BloomFilter qualified as Bloom
import Parquet.Compress qualified as Compress
import Parquet.HighLevel qualified as H
import Parquet.Nested qualified as N
import Parquet.Page qualified as Page
import Parquet.PageIndex qualified as Index
import Parquet.Read qualified as R
import Parquet.Types
import Parquet.Write qualified as W
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>))


check :: String -> Bool -> IO ()
check label okay = unless okay (fail ("nested options: " ++ label))


right :: Either String a -> IO a
right = either fail pure


tests :: IO ()
tests = do
  let primitive = N.NSPrimitive N.LtInt32
      schema = V.singleton ("v", N.NSOptional (N.NSList (N.NSOptional primitive)))
      rows = V.singleton (V.fromList [N.NRNull, N.NRList V.empty, N.NRList (V.fromList [N.NRLeaf (N.LvInt32 42), N.NRNull, N.NRLeaf (N.LvInt32 (-7))]), N.NRList (V.singleton (N.NRLeaf (N.LvInt32 9)))])
  baseline <- right (N.buildNestedFile schema rows) >>= right . R.loadParquetFile
  native <- right (A.parquetFileArrowSchemaChecked baseline)
  expected <- either (fail . show) pure (A.parquetRowGroupToArrow native baseline 0)
  forM_ [Uncompressed, GZip, Snappy, ZSTD, LZ4Raw, LZO, Brotli] $ \codec ->
    case Compress.validateCompression codec of
      Left _ -> pure ()
      Right () -> forM_ [W.PageV1, W.PageV2] $ \version -> do
        bytes <- right (H.encodeParquetNestedWith H.defaultWriteOptions {H.writeCompression = codec, H.writePageVersion = version} schema rows)
        file <- right (R.loadParquetFile bytes)
        actual <- either (fail . show) pure (A.parquetRowGroupToArrow native file 0)
        check ("values: " ++ show codec ++ "/" ++ show version) (actual == expected)
        let group = V.head (fmRowGroups (R.pfFooter file))
            column = V.head (rgColumns group)
        metadata <- maybe (fail "nested options: missing metadata") pure (ccMetadata column)
        check "requested compression is recorded" (cmCodec metadata == codec)
        check "row-group total is the uncompressed total" (rgTotalByteSize group == cmTotalUncompressedSize metadata)
        chunk <- right (R.columnChunkSlice file 0 0)
        (header, _) <- right (Page.readPageHeaderAt chunk 0)
        check "requested page version is emitted" (case (version, Page.phType header) of (W.PageV1, Page.PtDataPage _) -> True; (W.PageV2, Page.PtDataPageV2 _) -> True; _ -> False)
        index <- right (Index.readOffsetIndex file 0 0)
        check "offset index describes actual page bytes" $ case index of
          Just value -> case V.toList (oiPageLocations value) of
            [location] -> plOffset location == cmDataPageOffset metadata && fromIntegral (plCompressedPageSize location) == BS.length chunk && plFirstRowIndex location == 0
            _ -> False
          Nothing -> False
  let bloom = V.foldl' (\filter_ value -> Bloom.sbbfInsert (plainBloomValue (N.LvInt32 value)) filter_) (Bloom.newSbbf 256) (V.fromList [42, -7, 9])
      aux = W.emptyColumnAux {W.caPageVersion = W.PageV2, W.caGeneratePageIndex = True, W.caBloomFilter = Just bloom}
  bytes <- right (N.buildNestedFileWithAux (V.singleton aux) schema rows)
  file <- right (R.loadParquetFile bytes)
  restored <- right (Bloom.readBloomFilter file 0 0)
  check "pre-built bloom filter is emitted" (restored == Just bloom)
  let nullSchema = V.singleton ("v", N.NSOptional primitive)
      nullRows = V.singleton (V.replicate 3 N.NRNull)
  allNull <- right (N.buildNestedFileWithAux (V.singleton aux) nullSchema nullRows) >>= right . R.loadParquetFile
  output <- lookupEnv "WIREFORM_PARQUET_PRIMITIVE_OUTPUT"
  forM_ output $ \directory -> do
    createDirectoryIfMissing True directory
    BS.writeFile (directory </> "nested-bloom-with-options.parquet") bytes
    let allNullBytes = N.buildNestedFileWithAux (V.singleton aux) nullSchema nullRows
    right allNullBytes >>= BS.writeFile (directory </> "nested-all-null-with-options.parquet")
  bounds <- right (Index.readColumnIndex allNull 0 0)
  check "all-null page gets a truthful column index" (case bounds of Just value -> ciNullPages value == V.singleton True && ciNullCounts value == Just (V.singleton 3); _ -> False)
  namedBloomTests
  check "nested footer encryption cannot be silently ignored" (isLeft (H.encodeParquetNestedWith H.defaultWriteOptions {H.writeFooterEncryption = Just (W.FooterEncryption "invalid" "file" "" "")} schema rows))
  check "wrong per-leaf auxiliary count is rejected" (isLeft (N.buildNestedFileWithAux (V.replicate 2 aux) schema rows))
  check "deprecated LZ4 codec does not fall back to plaintext" (isLeft (H.encodeParquetNestedWith H.defaultWriteOptions {H.writeCompression = LZ4} schema rows))
  let group = V.head (fmRowGroups (R.pfFooter file))
  check "auxiliary layout detects final offset overflow" (isLeft (W.layoutColumnAuxiliaries (V.singleton group) (V.singleton (V.singleton aux)) maxBound))
  putStrLn "Nested codec, page-version, index and option-boundary regressions passed."


-- Mixed required/nullable and repeated data exercise every physical carrier.
-- Expected filter bits are built from individual values, independently of
-- the page's PLAIN framing and packed BOOLEAN representation.
namedBloomTests :: IO ()
namedBloomTests = do
  output <- lookupEnv "WIREFORM_PARQUET_PRIMITIVE_OUTPUT"
  let cases =
        V.fromList
          [ BloomCase N.LtInt32 (V.fromList [N.LvInt32 (-7), N.LvInt32 0, N.LvInt32 42])
          , BloomCase N.LtInt64 (V.fromList [N.LvInt64 (-1099511627776), N.LvInt64 1099511627776])
          , BloomCase N.LtFloat (V.fromList [N.LvFloat (castWord32ToFloat 0x7fc00001), N.LvFloat (-0), N.LvFloat 0, N.LvFloat (1 / 0)])
          , BloomCase N.LtDouble (V.fromList [N.LvDouble (-0), N.LvDouble 0, N.LvDouble (1 / 0), N.LvDouble (-3.5)])
          , BloomCase N.LtBool (V.generate 17 (N.LvBool . odd))
          , BloomCase N.LtString (V.fromList [N.LvString "", N.LvString "alpha", N.LvString "λ\NULβ"])
          , BloomCase N.LtBinary (V.fromList [N.LvBinary "", N.LvBinary "abc", N.LvBinary (BS.pack [0, 255, 4, 0, 0, 0])])
          , BloomCase (N.LtFixedBinary 9) (V.fromList [N.LvFixedBinary (BS.replicate 9 0), N.LvFixedBinary (BS.replicate 9 255)])
          , BloomCase N.LtUuid (V.singleton (N.LvFixedBinary (BS.pack [0 .. 15])))
          , BloomCase N.LtInt96 (V.singleton (N.LvFixedBinary (BS.pack [0 .. 11])))
          , BloomCase (N.LtTimestamp96 True LtNanos) (V.singleton (N.LvFixedBinary (BS.replicate 12 255)))
          , BloomCase (N.LtDecimal 38 2) (V.fromList [N.LvFixedBinary (BS.replicate 16 255), N.LvFixedBinary (BS.replicate 16 0)])
          , BloomCase (N.LtDecimal 76 2) (V.singleton (N.LvFixedBinary (BS.replicate 32 255)))
          , BloomCase N.LtInt32 V.empty
          , BloomCase N.LtBool (V.singleton (N.LvBool True))
          , BloomCase N.LtBool (V.replicate 8 (N.LvBool False))
          ]
  V.imapM_
    ( \index config -> forM_ [Uncompressed, GZip, Snappy, ZSTD, LZ4Raw, LZO, Brotli] $ \codec ->
        case Compress.validateCompression codec of
          Left _ -> pure ()
          Right () -> forM_ [W.PageV1, W.PageV2] $ \version -> do
            let name = "values.with.dot"
                schema = V.fromList [(name, N.NSOptional (N.NSList (N.NSOptional (N.NSPrimitive (bloomCaseType config))))), ("unselected", N.NSPrimitive N.LtInt32)]
                rows = V.fromList [V.fromList [N.NRNull, N.NRList V.empty, N.NRList (V.cons N.NRNull (V.map N.NRLeaf (bloomCaseValues config)))], V.fromList [N.NRLeaf (N.LvInt32 1), N.NRLeaf (N.LvInt32 2), N.NRLeaf (N.LvInt32 3)]]
                options = H.defaultWriteOptions {H.writeCompression = codec, H.writePageVersion = version, H.writeBloomFilters = [name]}
            bytes <- right (H.encodeParquetNestedWith options schema rows)
            file <- right (R.loadParquetFile bytes)
            found <- right (Bloom.readBloomFilter file 0 0) >>= maybe (fail "missing generated nested bloom") pure
            let expected = V.foldl' (\filter_ value -> Bloom.sbbfInsert (plainBloomValue value) filter_) (Bloom.newSbbf (Bloom.sbbfNumBytes found)) (bloomCaseValues config)
            check "nested bloom hashes every present value without page framing" (found == expected)
            other <- right (Bloom.readBloomFilter file 0 1)
            check "unrequested nested fields have no bloom" (other == Nothing)
            forM_ output $ \directory -> BS.writeFile (directory </> ("nested-generated-bloom-" ++ show index ++ "-" ++ show codec ++ "-" ++ show version ++ ".parquet")) bytes
            check "unknown nested bloom field is rejected" (isLeft (H.encodeParquetNestedWith options {H.writeBloomFilters = ["missing"]} schema rows))
    )
    cases
  let struct = V.singleton ("record", N.NSStruct (V.fromList [("id", N.NSPrimitive N.LtInt32), ("flag", N.NSPrimitive N.LtBool)]))
      rows = V.singleton (V.singleton (N.NRStruct (V.fromList [N.NRLeaf (N.LvInt32 42), N.NRLeaf (N.LvBool True)])))
  file <- right (H.encodeParquetNestedWith H.defaultWriteOptions {H.writeCompression = Uncompressed, H.writeBloomFilters = ["record"]} struct rows) >>= right . R.loadParquetFile
  forM_ [0, 1] $ \index -> right (Bloom.readBloomFilter file 0 index) >>= \value -> check "top-level field selects all physical descendants" (value /= Nothing)
  let explicit = Bloom.sbbfInsert "caller-provided" (Bloom.sbbfInsert (plainBloomValue (N.LvInt32 42)) (Bloom.newSbbf 96))
      auxes = V.fromList [W.emptyColumnAux {W.caBloomFilter = Just explicit}, W.emptyColumnAux]
  custom <- right (N.buildNestedFileWithBloomFilters ["record"] auxes struct rows) >>= right . R.loadParquetFile
  first <- right (Bloom.readBloomFilter custom 0 0)
  check "explicit leaf bloom overrides name-based generation" (first == Just explicit)


data BloomCase = BloomCase
  { bloomCaseType :: !N.LeafType
  , bloomCaseValues :: !(V.Vector N.LeafValue)
  }


plainBloomValue :: N.LeafValue -> BS.ByteString
plainBloomValue value = case value of
  N.LvInt32 x -> encode (Builder.int32LE x)
  N.LvInt64 x -> encode (Builder.int64LE x)
  N.LvFloat16 x -> encode (Builder.word16LE x)
  N.LvFloat x -> encode (Builder.word32LE (castFloatToWord32 x))
  N.LvDouble x -> encode (Builder.word64LE (castDoubleToWord64 x))
  N.LvBool x -> BS.singleton (if x then 1 else 0)
  N.LvString x -> Data.Text.Encoding.encodeUtf8 x
  N.LvBinary x -> x
  N.LvFixedBinary x -> x
  where
    encode = BL.toStrict . Builder.toLazyByteString
