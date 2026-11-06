{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | wireform-parquet -> pyarrow / duckdb / polars round-trip
probe.

Writes a fixed catalogue of one-file-per-shape Parquet files
under the directory passed as @argv[1]@. The companion
'scripts/parquet_interop.py' driver opens each one with
pyarrow, then with duckdb, then with polars and asserts the
contents match what we expect.

The default catalogue uses 'Parquet.HighLevel.encodeParquet' /
'encodeParquetMixed'. The @--delta-int32@ / @--delta-int64@ modes export
raw codec output for independently framed cross-reader checks.
-}
module Main (main) where

import Arrow.Column qualified as AC
import Arrow.File qualified as ArrowFile
import Arrow.Read.Columns qualified as ArrowRead
import Arrow.Types qualified as AT
import Control.Monad (forM_, when)
import Crypto.Random.Types qualified as RNG
import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Data.Vector.Storable qualified as VS
import Data.Word (Word32, Word64, Word8)
import Foreign.Storable (Storable)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Parquet qualified as PHL
import Parquet.Compress qualified as Compress
import Parquet.DeltaEncode qualified as Delta
import Parquet.Encryption qualified as Enc
import Parquet.File qualified as File
import Parquet.Footer qualified as Footer
import Parquet.Nested qualified as PN
import Parquet.PageIndex qualified as PageIndex
import Parquet.Read qualified as Read
import Parquet.Types qualified as P
import Parquet.Variant qualified as Variant
import Parquet.Variant.Arrow qualified as VariantArrow
import Parquet.Variant.Parquet qualified as VariantParquet
import Parquet.Variant.Shred qualified as VariantShred
import Parquet.Write (
  ColumnData (..),
  OptionalColumn (..),
  ParquetColumn (..),
 )
import Parquet.Write qualified as PW
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, takeExtension, (</>))


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--variant-from-binary", sourceDir, outDir] -> writeVariantInputs sourceDir outDir
    ["--file", outDir] -> writeFileProbes outDir
    ["--file-check", sourceDir] -> checkFileReferences sourceDir
    ["--from-arrow", sourceDir, outDir] -> writeArrowInputs sourceDir outDir
    ["--nested-from-arrow", sourceDir, outDir] -> writeNestedArrowInputs sourceDir outDir
    ["--nested-options-from-arrow", sourceDir, outDir] -> writeNestedOptionsArrowInputs sourceDir outDir
    ["--variant-shred-from-arrow", sourceDir, outDir] -> writeShreddedVariantInputs sourceDir outDir
    ["--metadata", outDir] -> writeMetadataProbes outDir
    ["--copy-footers", sourceDir, outDir] -> copyMetadataFooters sourceDir outDir
    ["--lzo", outDir] -> writeLzoProbes outDir
    ["--codec-profiles", outDir] -> writeCodecProfiles outDir
    ["--logical-metadata", outDir] -> writeLogicalMetadataProbes outDir
    ["--nested-types", outDir] -> writeNestedTypeProbes outDir
    ["--dictionary-bytes", outDir] -> writeDictionaryBytesProbes outDir
    ["--dictionary-ieee", outDir] -> writeDictionaryIEEEProbes outDir
    ["--delta-int32", outDir] -> writeDeltaInt32Probes outDir
    ["--delta-int64", outDir] -> writeDeltaInt64Probes outDir
    ["--decimal", outDir] -> writeDecimalProbes outDir
    ["--writer-options", outDir] -> writeOptionsProbes outDir
    ["--statistics", outDir] -> writeStatisticsProbes outDir
    ["--encrypted", outDir, metadataPath, keyWidth] -> do
      width <- case keyWidth of
        "16" -> pure 16
        "24" -> pure 24
        "32" -> pure 32
        _ -> fail "fixture AES key width must be 16, 24 or 32 bytes"
      createDirectoryIfMissing True outDir
      metadata <- BS.readFile metadataPath
      writeEncryptedProbes width outDir metadata
    ["--encrypted", outDir, metadataPath] -> do
      metadata <- BS.readFile metadataPath
      writeEncryptedProbes 16 outDir metadata
    [outDir] -> do
      writeAllProbes outDir
      putStrLn ("wrote parquet probe outputs to " ++ outDir)
    _ -> do
      putStrLn "usage: wireform-parquet-interop-probe [--encrypted] <output-dir> [test-key-metadata-json]"
      exitFailure


writeVariantInputs :: FilePath -> FilePath -> IO ()
writeVariantInputs sourceDir outDir = do
  createDirectoryIfMissing True outDir
  names <- filter ((== ".meta") . takeExtension) <$> listDirectory sourceDir
  mapM_
    ( \name -> do
        metadata <- BS.readFile (sourceDir </> name)
        payload <- BS.readFile (sourceDir </> takeBaseName name ++ ".value")
        value <- either fail pure (Variant.decodeVariant metadata payload)
        bytes <- either fail pure (VariantParquet.buildVariantParquetFile "v" (V.fromList [Just value, Nothing, Just Variant.VNull]))
        BS.writeFile (outDir </> takeBaseName name ++ ".parquet") bytes
    )
    names


writeArrowInputs :: FilePath -> FilePath -> IO ()
writeArrowInputs sourceDir outDir = do
  createDirectoryIfMissing True outDir
  names <- listDirectory sourceDir
  mapM_
    ( \name -> when (takeExtension name == ".arrow") $ do
        bytes <- BS.readFile (sourceDir </> name)
        (schema, batches) <- either fail pure (ArrowFile.readArrowFileColumns ArrowRead.defaultReadOptions bytes)
        (target, groups) <- either fail pure (PHL.arrowToParquetMixed schema (V.toList batches))
        let options = PHL.defaultWriteOptions {PHL.writeCompression = P.Uncompressed, PHL.writePageVersion = PW.PageV2}
        BS.writeFile (outDir </> takeBaseName name <> ".parquet") (PHL.encodeParquetMixed options target groups)
    )
    names


writeMetadataProbes :: FilePath -> IO ()
writeMetadataProbes outDir = do
  createDirectoryIfMissing True outDir
  let schema =
        V.fromList
          [ P.SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
          , P.SchemaElement "x" (Just P.Required) (Just P.PTInt32) Nothing Nothing Nothing Nothing Nothing
          ]
      bytes =
        PHL.encodeParquet
          (PHL.defaultWriteOptions {PHL.writeCompression = P.Uncompressed})
          schema
          [V.singleton (ColInt32 (VP.fromList [minBound, 0, maxBound]))]
      entries =
        V.fromList
          [ P.KeyValue "same" Nothing
          , P.KeyValue "same" (Just "")
          , P.KeyValue "same" (Just "last")
          , P.KeyValue (TE.encodeUtf8 "κλειδί") (Just (TE.encodeUtf8 "世界"))
          , P.KeyValue "opaque" (Just (BS.pack [255, 0, 128]))
          ]
  mapM_
    ( \(name, values) -> do
        metadata <- either fail pure (Footer.readFooter bytes)
        trailer <- either fail pure (Footer.readFooterTrailer bytes)
        let prefix = BS.take (BS.length bytes - 8 - BS.length (Footer.ftBytes trailer)) bytes
        BS.writeFile
          (outDir </> name <> ".parquet")
          (prefix <> Footer.writeFooter (metadata {P.fmKeyValueMetadata = values}))
    )
    [("absent", Nothing), ("empty", Just V.empty), ("entries", Just entries)]


copyMetadataFooters :: FilePath -> FilePath -> IO ()
copyMetadataFooters sourceDir outDir = do
  createDirectoryIfMissing True outDir
  names <- listDirectory sourceDir
  mapM_
    ( \name -> when (takeExtension name == ".parquet") $ do
        bytes <- BS.readFile (sourceDir </> name)
        metadata <- either fail pure (Footer.readFooter bytes)
        trailer <- either fail pure (Footer.readFooterTrailer bytes)
        let prefix = BS.take (BS.length bytes - 8 - BS.length (Footer.ftBytes trailer)) bytes
        BS.writeFile (outDir </> name) (prefix <> Footer.writeFooter metadata)
    )
    names


-- Broad native codec coverage; availability is checked by the external
-- driver against its explicitly requested build profile.
writeCodecProfiles :: FilePath -> IO ()
writeCodecProfiles directory = do
  createDirectoryIfMissing True directory
  let many = V.fromList . concat . replicate 16
      schema repetition =
        V.fromList
          [ P.SchemaElement "schema" Nothing Nothing (Just 4) Nothing Nothing Nothing Nothing
          , P.SchemaElement "i" (Just repetition) (Just P.PTInt32) Nothing Nothing Nothing Nothing Nothing
          , P.SchemaElement "s" (Just repetition) (Just P.PTByteArray) Nothing (Just P.CTUtf8) (Just P.LTString) Nothing Nothing
          , P.SchemaElement "b" (Just repetition) (Just P.PTBoolean) Nothing Nothing Nothing Nothing Nothing
          , P.SchemaElement "d" (Just repetition) (Just P.PTDouble) Nothing Nothing Nothing Nothing Nothing
          ]
      ordinary =
        V.fromList
          [ PCOptional (OptInt32 (many [Just minBound, Nothing, Just 0, Just maxBound, Nothing]))
          , PCOptional (OptByteArray (many [Just (TE.encodeUtf8 "κόσμε"), Nothing, Just "", Just "alpha", Nothing]))
          , PCOptional (OptBool (many [Just True, Nothing, Just False, Just True, Nothing]))
          , PCOptional (OptDouble (many [Just (-1.5), Nothing, Just 0, Just 1.25, Nothing]))
          ]
      required =
        V.fromList
          [ PCRequired (ColInt32 (VP.fromList (concat (replicate 16 [minBound, -1, 0, maxBound, 1]))))
          , PCRequired (ColByteArray (many [TE.encodeUtf8 "κόσμε", "beta", "", "alpha", "omega"]))
          , PCRequired (ColBool (many [True, False, False, True, False]))
          , PCRequired (ColDouble (VP.fromList (concat (replicate 16 [-1.5, -1, 0, 1.25, 2]))))
          ]
      nulls n = V.fromList [PCOptional (OptInt32 (V.replicate n Nothing)), PCOptional (OptByteArray (V.replicate n Nothing)), PCOptional (OptBool (V.replicate n Nothing)), PCOptional (OptDouble (V.replicate n Nothing))]
  mapM_
    ( \(label, codec) -> case Compress.validateCompression codec of
        Left _ -> pure ()
        Right () ->
          mapM_
            ( \(versionName, version) ->
                mapM_
                  ( \indexed ->
                      mapM_
                        ( \(shape, repetition, columns) -> do
                            let options = PHL.defaultWriteOptions {PHL.writeCompression = codec, PHL.writePageVersion = version, PHL.writePageIndex = indexed}
                                name = label ++ "-" ++ versionName ++ "-" ++ shape ++ (if indexed then "-indexed" else "-plain") ++ ".parquet"
                            BS.writeFile (directory </> name) (PHL.encodeParquetMixed options (schema repetition) [columns, columns])
                        )
                        [("required", P.Required, required), ("mixed", P.Optional, ordinary), ("allnull", P.Optional, nulls 80), ("empty", P.Optional, nulls 0)]
                  )
                  [False, True]
            )
            [("v1", PW.PageV1), ("v2", PW.PageV2)]
    )
    [("uncompressed", P.Uncompressed), ("gzip", P.GZip), ("snappy", P.Snappy), ("zstd", P.ZSTD), ("lz4", P.LZ4Raw), ("brotli", P.Brotli)]


writeLzoProbes :: FilePath -> IO ()
writeLzoProbes outDir = do
  createDirectoryIfMissing True outDir
  let many values = V.fromList (concat (replicate 20 values))
      fields repetition =
        V.fromList
          [ P.SchemaElement "schema" Nothing Nothing (Just 3) Nothing Nothing Nothing Nothing
          , P.SchemaElement "i32" (Just repetition) (Just P.PTInt32) Nothing Nothing Nothing Nothing Nothing
          , P.SchemaElement "flag" (Just repetition) (Just P.PTBoolean) Nothing Nothing Nothing Nothing Nothing
          , P.SchemaElement "bytes" (Just repetition) (Just P.PTByteArray) Nothing Nothing Nothing Nothing Nothing
          ]
      required =
        V.fromList
          [ PCRequired (ColInt32 (VP.fromList (concat (replicate 20 [minBound, -1, 0, 1, maxBound]))))
          , PCRequired (ColBool (many [True, False, False, True, True]))
          , PCRequired (ColByteArray (many ["alpha", "", BS.singleton 0, BS.singleton 255, BS.replicate 128 97]))
          ]
      optional =
        V.fromList
          [ PCOptional (OptInt32 (many [Just minBound, Nothing, Just 0, Just maxBound, Nothing]))
          , PCOptional (OptBool (many [Just True, Nothing, Just False, Just True, Nothing]))
          , PCOptional (OptByteArray (many [Just "alpha", Nothing, Just "", Just (BS.singleton 255), Nothing]))
          ]
      allNull = V.fromList [PCOptional (OptInt32 (V.replicate 100 Nothing)), PCOptional (OptBool (V.replicate 100 Nothing)), PCOptional (OptByteArray (V.replicate 100 Nothing))]
      empty = V.fromList [PCRequired (ColInt32 VP.empty), PCRequired (ColBool V.empty), PCRequired (ColByteArray V.empty)]
  mapM_
    ( \(label, version) ->
        mapM_
          ( \(name, repetition, columns) -> do
              let options = PHL.defaultWriteOptions {PHL.writeCompression = P.LZO, PHL.writePageVersion = version, PHL.writePageIndex = False}
              BS.writeFile
                (outDir </> label <> "-" <> name <> ".parquet")
                (PHL.encodeParquetMixed options (fields repetition) [columns, columns])
          )
          [("required", P.Required, required), ("nullable", P.Optional, optional), ("all-null", P.Optional, allNull), ("empty", P.Required, empty)]
    )
    [("v1", PW.PageV1), ("v2", PW.PageV2)]


writeLogicalMetadataProbes :: FilePath -> IO ()
writeLogicalMetadataProbes outDir = do
  createDirectoryIfMissing True outDir
  let point = BS.pack [1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 248, 63, 0, 0, 0, 0, 0, 0, 4, 64]
      cases =
        [ ("geometry-default", P.LTGeometry P.defaultGeometryParameters)
        , ("geometry-custom", P.LTGeometry (P.GeometryParameters (Just "EPSG:3857")))
        , ("geography-default", P.LTGeography P.defaultGeographyParameters)
        ]
          ++ map
            ( \(name, algorithm) ->
                ( "geography-" <> name
                , P.LTGeography (P.GeographyParameters (Just "EPSG:4326") (Just algorithm))
                )
            )
            [("spherical", P.Spherical), ("vincenty", P.Vincenty), ("thomas", P.Thomas), ("andoyer", P.Andoyer), ("karney", P.Karney)]
  mapM_
    ( \(name, logical) -> do
        let schema =
              V.fromList
                [ P.SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
                , P.SchemaElement "geometry" (Just P.Required) (Just P.PTByteArray) Nothing Nothing (Just logical) Nothing Nothing
                ]
        result <-
          PHL.encodeParquetIO
            (PHL.defaultWriteOptions {PHL.writeCompression = P.Uncompressed})
            schema
            [V.singleton (ColByteArray (V.singleton point))]
        either (fail . ("logical metadata: " <>)) (BS.writeFile (outDir </> name <> ".parquet")) result
    )
    cases


writeDictionaryIEEEProbes :: FilePath -> IO ()
writeDictionaryIEEEProbes outDir = do
  createDirectoryIfMissing True outDir
  let bits32 = [0x3f800000, 0x7fc01234, 0xffa05678, 0x7fc01234, 0, 0x80000000, 0, 0x80000000, 0x7f800000, 0xff800000, 1, 0xffffffff]
      bits64 = [0x3ff0000000000000, 0x7ff8123456789abc, 0xfff056789abcdef0, 0x7ff8123456789abc, 0, 0x8000000000000000, 0, 0x8000000000000000, 0x7ff0000000000000, 0xfff0000000000000, 1, 0xffffffffffffffff]
      values32 = bits32 ++ take 129 (iterate (\value -> value * 1664525 + 1013904223) (0x12345678 :: Word32))
      values64 = bits64 ++ take 129 (iterate (\value -> value * 6364136223846793005 + 1442695040888963407) (0x123456789abcdef0 :: Word64))
  writeDictionaryFragments outDir "f32" (ColFloat (VP.fromList (map castWord32ToFloat (values32 ++ reverse values32))))
  writeDictionaryFragments outDir "f64" (ColDouble (VP.fromList (map castWord64ToDouble (values64 ++ reverse values64))))


writeDictionaryFragments :: FilePath -> FilePath -> ColumnData -> IO ()
writeDictionaryFragments outDir name column = do
  let dictionary = PW.buildDictionary column
  BS.writeFile (outDir </> name ++ ".dictionary.bin") (PW.encodeDictPage dictionary)
  BS.writeFile (outDir </> name ++ ".data.bin") (PW.encodeDictDataPage dictionary)


writeDictionaryBytesProbes :: FilePath -> IO ()
writeDictionaryBytesProbes outDir = do
  createDirectoryIfMissing True outDir
  let shortValues maximumSize = map (\size -> BS.replicate size 0) [0 .. maximumSize] ++ map (\size -> BS.pack (take size (cycle [0 .. 255]))) [0 .. maximumSize]
      allRandomValues = take 257 (iterate (\value -> value * 6364136223846793005 + 1442695040888963407) (0x123456789abcdef0 :: Word64))
      randomValues = take 129 allRandomValues
      wordBytes :: Word64 -> ByteString
      wordBytes value = BS.pack (map (\shift -> fromIntegral (value `shiftR` shift)) [0, 8 .. 56])
      wide i value = let width = [64, 256, 4096] !! (i `rem` 3) in wordBytes value <> BS.replicate (width - 8) 97
      sliced i bytes = let prefix = i `rem` 8 in BS.take (BS.length bytes) (BS.drop prefix (BS.replicate prefix 0xa5 <> bytes <> BS.replicate 8 0x5a))
      write name values = writeDictionaryFragments outDir name (ColByteArray (V.fromList (zipWith sliced [0 ..] (values ++ reverse values))))
  write "short" (shortValues 8)
  write "tagged-short" (shortValues 7)
  write "mixed" (shortValues 40)
  write "sparse8" (map wordBytes randomValues)
  write "wide" (zipWith wide [0 ..] randomValues)
  write "common-ends" (map (\value -> BS.replicate 24 97 <> wordBytes value <> BS.replicate 32 97) randomValues)
  let ordinary value = wordBytes value <> BS.replicate 56 97
      ambiguous = wordBytes 0x123456789abcdef0 <> BS.replicate 16 97 <> BS.replicate 8 98 <> BS.replicate 32 97
      secondAmbiguous = wordBytes 0x123456789abcdef0 <> BS.replicate 32 97 <> BS.replicate 8 99 <> BS.replicate 16 97
  write "late-promotion" (map ordinary randomValues ++ [ambiguous] ++ map ordinary (drop 129 allRandomValues) ++ [secondAmbiguous])
  let mixed cardinality =
        map (wordBytes . fromIntegral) [0 .. cardinality - 1]
          ++ concat
            (zipWith (\key count -> replicate count (wordBytes (fromIntegral (key `rem` cardinality))) ++ map (wordBytes . fromIntegral) [0, 1, 0]) [0 ..] [7, 8, 9, 15, 16, 17, 23, 24, 31, 32, 33, 63, 64, 65])
  write "rle-binary" (mixed (2 :: Int))
  write "rle-mixed" (mixed (16 :: Int))
  write "rle-wide" (mixed (257 :: Int))


data DeltaProbe = DeltaProbe
  { deltaProbeName :: !FilePath
  , deltaProbeValues :: ![Integer]
  }


-- Raw encoder output lets Apache Thrift supply independent page/footer
-- framing in parquet_delta_interop.py, isolating the encoding contract.
writeDeltaInt32Probes :: FilePath -> IO ()
writeDeltaInt32Probes outDir =
  writeDeltaProbes
    outDir
    (toInteger (minBound :: Int32))
    (toInteger (maxBound :: Int32))
    randomValues
    (Delta.encodeDeltaBinaryPackedInt32 . VP.fromList . map fromInteger)
  where
    randomValues = map (toInteger . (fromIntegral :: Word32 -> Int32)) (take 513 (iterate (\value -> value * 1664525 + 1013904223) (0x12345678 :: Word32)))


writeDeltaInt64Probes :: FilePath -> IO ()
writeDeltaInt64Probes outDir =
  writeDeltaProbes
    outDir
    (toInteger (minBound :: Int64))
    (toInteger (maxBound :: Int64))
    randomValues
    (Delta.encodeDeltaBinaryPackedInt64 . VP.fromList . map fromInteger)
  where
    randomValues = map (toInteger . (fromIntegral :: Word64 -> Int64)) (take 513 (iterate (\value -> value * 6364136223846793005 + 1442695040888963407) (0x123456789abcdef0 :: Word64)))


writeDeltaProbes :: FilePath -> Integer -> Integer -> [Integer] -> ([Integer] -> ByteString) -> IO ()
writeDeltaProbes outDir low high randomValues encode = do
  createDirectoryIfMissing True outDir
  let series name count patternValues = DeltaProbe name (take count (cycle patternValues))
      fixed =
        [ DeltaProbe "wrap-up" [low, high, low]
        , DeltaProbe "wrap-down" [high, low, high]
        , DeltaProbe "step-up" [low, high]
        , DeltaProbe "step-down" [high, low]
        , DeltaProbe "minimum" [low]
        , DeltaProbe "maximum" [high]
        , DeltaProbe "mixed-sign-delta" [0, low, low + 1]
        , series "edges-257" 257 [low, low + 1, high, high - 1, 0, -1, 1]
        , series "constant-129" 129 [low]
        , DeltaProbe "random-513" randomValues
        ]
      alternating count = series ("alternating-" ++ show count) count [low, high]
      bits = if high <= toInteger (maxBound :: Int32) then 32 else 64
      packed width =
        let mask = (1 `shiftL` width) - 1
            count = 2 + width `rem` 31
            offsets = take count ([0, 1 `shiftL` (width - 1), mask] ++ cycle [104729 .&. mask, 7 .&. mask])
            wrapped value = (value - low) `mod` (high - low + 1) + low
            rows = map wrapped (scanl (+) 0 (map (low +) offsets))
        in DeltaProbe ("bitwidth-" ++ show width) rows
      probes = fixed ++ map alternating [31, 32, 33, 127, 128, 129, 130] ++ map packed [1 .. bits]
  mapM_ (\probe -> BS.writeFile (outDir </> deltaProbeName probe ++ ".bin") (encode (deltaProbeValues probe))) probes


writeDecimalProbes :: FilePath -> IO ()
writeDecimalProbes outDir = do
  createDirectoryIfMissing True outDir
  let groups =
        [ [(1, [1]), (127, [127]), (128, [0, 128]), (255, [0, 255]), (256, [1, 0]), (1000, [3, 232])]
        , [(-1000, [252, 24]), (-256, [255, 0]), (-129, [255, 127]), (-128, [128]), (-1, [255, 255])]
        , [(-999999, [240, 189, 193]), (0, [0]), (999999, [15, 66, 63])]
        ]
          :: [[(Int64, [Word8])]]
  mapM_
    ( \storage ->
        mapM_
          ( \nullable ->
              mapM_
                ( \version ->
                    mapM_
                      ( \(codecName, codec) -> do
                          let physical = case storage of "i32" -> P.PTInt32; "i64" -> P.PTInt64; _ -> P.PTByteArray
                              schema =
                                V.fromList
                                  [ P.SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
                                  , P.SchemaElement
                                      "x"
                                      (Just (if nullable then P.Optional else P.Required))
                                      (Just physical)
                                      Nothing
                                      (Just P.CTDecimal)
                                      (Just (P.LTDecimal 6 2))
                                      Nothing
                                      Nothing
                                  ]
                              required values = case storage of
                                "i32" -> PW.ColInt32 (VP.fromList (fromIntegral . fst <$> values))
                                "i64" -> PW.ColInt64 (VP.fromList (fst <$> values))
                                _ -> PW.ColByteArray (V.fromList (BS.pack . snd <$> values))
                              optional values =
                                let rows convert = V.fromList (Nothing : ((Just . convert) <$> values) ++ [Nothing])
                                in case storage of
                                     "i32" -> PW.OptInt32 (rows (fromIntegral . fst))
                                     "i64" -> PW.OptInt64 (rows fst)
                                     _ -> PW.OptByteArray (rows (BS.pack . snd))
                              opts =
                                PHL.defaultWriteOptions
                                  { PHL.writeCompression = codec
                                  , PHL.writePageVersion = version
                                  , PHL.writeBloomFilters = ["x"]
                                  }
                              bytes =
                                if nullable
                                  then
                                    PHL.encodeParquetMixed
                                      opts
                                      schema
                                      ((\values -> V.singleton (PW.PCOptional (optional values))) <$> groups)
                                  else PHL.encodeParquet opts schema ((V.singleton . required) <$> groups)
                              name =
                                storage
                                  <> (if nullable then "-optional" else "-required")
                                  <> (if version == PW.PageV1 then "-v1-" else "-v2-")
                                  <> codecName
                                  <> ".parquet"
                          BS.writeFile (outDir </> name) bytes
                      )
                      [("uncompressed", P.Uncompressed), ("gzip", P.GZip)]
                )
                [PW.PageV1, PW.PageV2]
          )
          [False, True]
    )
    ["i32", "i64", "bytes"]


writeOptionsProbes :: FilePath -> IO ()
writeOptionsProbes outDir = do
  createDirectoryIfMissing True outDir
  mapM_
    ( \nullable ->
        mapM_
          ( \version ->
              mapM_
                ( \(codecName, codec) ->
                    mapM_
                      ( \indexed -> do
                          let schema =
                                V.fromList
                                  [ P.SchemaElement "schema" Nothing Nothing (Just 2) Nothing Nothing Nothing Nothing
                                  , P.SchemaElement "r" (Just P.Required) (Just P.PTInt32) Nothing Nothing Nothing Nothing Nothing
                                  , P.SchemaElement "o" (Just (if nullable then P.Optional else P.Required)) (Just P.PTInt64) Nothing Nothing Nothing Nothing Nothing
                                  ]
                              required = PW.ColInt32 (VP.fromList [1, 2, 3])
                              values = PW.ColInt64 (VP.fromList [10, 20, 30])
                              mixed =
                                V.fromList
                                  [ PW.PCRequired required
                                  , PW.PCOptional (PW.OptInt64 (V.fromList [Just 10, Nothing, Just 30]))
                                  ]
                              opts =
                                PHL.defaultWriteOptions
                                  { PHL.writeCompression = codec
                                  , PHL.writePageVersion = version
                                  , PHL.writePageIndex = indexed
                                  , PHL.writeBloomFilters = ["o"]
                                  }
                              bytes =
                                if nullable
                                  then PHL.encodeParquetMixed opts schema [mixed, mixed]
                                  else PHL.encodeParquet opts schema (replicate 2 (V.fromList [required, values]))
                              name =
                                (if nullable then "mixed" else "required")
                                  <> "-"
                                  <> (if version == PW.PageV1 then "v1" else "v2")
                                  <> "-"
                                  <> codecName
                                  <> (if indexed then "-index.parquet" else "-noindex.parquet")
                          BS.writeFile (outDir </> name) bytes
                      )
                      [False, True]
                )
                [("uncompressed", P.Uncompressed), ("gzip", P.GZip), ("snappy", P.Snappy), ("zstd", P.ZSTD)]
          )
          [PW.PageV1, PW.PageV2]
    )
    [False, True]


writeStatisticsProbes :: FilePath -> IO ()
writeStatisticsProbes outDir =
  mapM_
    ( \precision ->
        mapM_
          ( \(name, values) -> do
              let physical = if precision == "float" then P.PTFloat else P.PTDouble
                  column =
                    if precision == "float"
                      then ColFloat (VP.fromList (realToFrac <$> values))
                      else ColDouble (VP.fromList values)
                  schema =
                    V.fromList
                      [ P.SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
                      , P.SchemaElement "x" (Just P.Required) (Just physical) Nothing Nothing Nothing Nothing Nothing
                      ]
                  bytes = PHL.encodeParquet PHL.defaultWriteOptions schema [V.singleton column]
              BS.writeFile (outDir </> precision <> "-" <> name <> ".parquet") bytes
          )
          [ ("mixed", [0 / 0, -2, 3, 0 / 0])
          , ("nan", [0 / 0, 0 / 0])
          , ("positive-zero", [0])
          , ("negative-zero", [-0.0])
          , ("infinities", [negate (1 / 0), 1 / 0, 0 / 0])
          , ("empty", [])
          ]
    )
    ["float", "double"]
    >> BS.writeFile
      (outDir </> "statistics-index.bin")
      ( PageIndex.encodeColumnIndex
          P.ColumnIndex
            { P.ciNullPages = V.fromList [False, False, True]
            , P.ciMinValues = V.fromList [BS.pack [0, 0, 0, 0x80], BS.pack [0, 0, 0, 0xc0], BS.empty]
            , P.ciMaxValues = V.fromList [BS.replicate 4 0, BS.pack [0, 0, 0x40, 0x40], BS.empty]
            , P.ciBoundaryOrder = P.OrderUnordered
            , P.ciNullCounts = Just (V.fromList [0, 0, 4])
            , P.ciNanCounts = Just (V.fromList [1, 0, 0])
            , P.ciRepetitionLevelHistograms = Nothing
            , P.ciDefinitionLevelHistograms = Nothing
            }
      )
    >> writeReq
      outDir
      "binary-boundaries.parquet"
      ( [rootElem 1, leafElem P.PTByteArray Nothing "x"]
      , V.singleton (ColByteArray (V.fromList [BS.pack [0x80], BS.pack [0x7f], BS.empty, BS.pack [0xff]]))
      )
    >> writeReq
      outDir
      "utf8-boundaries.parquet"
      ( [rootElem 1, leafElem P.PTByteArray (Just P.CTUtf8) "x"]
      , V.singleton (ColByteArray (TE.encodeUtf8 <$> V.fromList ["é", "z", "", "🙂"]))
      )
    >> writeStatisticsTraversalProbes outDir


writeStatisticsTraversalProbes :: FilePath -> IO ()
writeStatisticsTraversalProbes outDir = do
  let write name physical column =
        writeReq outDir name ([rootElem 1, leafElem physical Nothing "x"], V.singleton column)
  mapM_
    (\(name, values) -> write ("int32-" <> name <> ".parquet") P.PTInt32 (ColInt32 (VP.fromList values)))
    [ ("empty", [])
    , ("bounds", [maxBound, minBound, 0, -1, 1, maxBound])
    , ("descending", [19, 7, 0, -5, -23])
    ]
  mapM_
    (\(name, values) -> write ("int64-" <> name <> ".parquet") P.PTInt64 (ColInt64 (VP.fromList values)))
    [ ("empty", [])
    , ("bounds", [maxBound, minBound, 0, -1, 1, (1 `shiftL` 54) + 1, maxBound])
    , ("descending", [19, 7, 0, -5, -23])
    ]
  let initial = take 40 (cycle ["name_0", "name_7", "name_19"])
      short = [BS.empty, BS.pack [0], BS.pack [0, 0], BS.pack [1], BS.pack [1, 0], BS.pack [255], BS.pack [255, 0], "abcdefgh"]
      sliced ordinal bytes = BS.take (BS.length bytes) (BS.drop 5 (BS.replicate 5 ordinal <> bytes <> BS.singleton ordinal))
  mapM_
    (\(name, values) -> write ("binary-" <> name <> ".parquet") P.PTByteArray (ColByteArray (V.fromList values)))
    [ ("empty", [])
    , ("short-prefix", short)
    , ("zero-prefix", map (`BS.replicate` 0) [0 .. 16])
    , ("late-min", initial ++ [BS.replicate 9 0, "m", "z"])
    , ("late-max", initial ++ [BS.replicate 9 255, "a", "m"])
    , ("long-first", map (\key -> BS.replicate 32 97 <> BS.singleton key) [255, 0, 128, 1, 127])
    , ("short-slice", zipWith sliced [0 ..] short)
    ]


-- Deliberately public test key: no production credentials or KMS are used.
-- Two row groups and one plaintext column exercise ordinals and selective
-- encryption independently of our own encrypted-reader implementation.
writeEncryptedProbes :: Int -> FilePath -> ByteString -> IO ()
writeEncryptedProbes keyBytes outDir metadata =
  mapM_
    ( \algorithm ->
        mapM_
          ( \version ->
              mapM_
                ( \encrypted -> do
                    fileId <- RNG.getRandomBytes 8
                    let key = BS.replicate keyBytes 0x42
                        footer = PW.FooterEncryption key fileId BS.empty metadata
                        column = PW.ColumnEncryption algorithm key fileId BS.empty BS.empty 0
                        schema =
                          V.fromList
                            [ P.SchemaElement "schema" Nothing Nothing (Just 2) Nothing Nothing Nothing Nothing
                            , P.SchemaElement "x" (Just P.Required) (Just P.PTInt32) Nothing Nothing Nothing Nothing Nothing
                            , P.SchemaElement "y" (Just P.Required) (Just P.PTByteArray) Nothing (Just P.CTUtf8) (Just P.LTString) Nothing Nothing
                            ]
                        groups =
                          [ V.fromList [ColInt32 (VP.fromList [1, 2, 3]), ColByteArray (V.fromList ["a", "b", "c"])]
                          , V.fromList [ColInt32 (VP.fromList [4, 5]), ColByteArray (V.fromList ["d", "e"])]
                          ]
                        opts =
                          PHL.defaultWriteOptions
                            { PHL.writeCompression = P.Snappy
                            , PHL.writePageVersion = version
                            , PHL.writeFooterEncryption = Just footer
                            , PHL.writeColumnEncryption = V.fromList [if encrypted then Just column else Nothing, Nothing]
                            , PHL.writeBloomFilters = ["x"]
                            }
                        filename = show algorithm <> "-" <> show version <> (if encrypted then "-column" else "-footer") <> ".parquet"
                    result <- PHL.encodeParquetIO opts schema groups
                    bytes <- either (ioError . userError) pure result
                    BS.writeFile (outDir </> filename) bytes
                )
                [False, True]
          )
          [PW.PageV1, PW.PageV2]
    )
    [Enc.AesGcmV1, Enc.AesGcmCtrV1]


-- ============================================================
-- Probe catalogue
-- ============================================================

writeAllProbes :: FilePath -> IO ()
writeAllProbes outDir = do
  -- Required-only columns through encodeParquet (PageV1).
  writeReq outDir "int32_required.parquet" reqInt32
  writeReq outDir "int64_required.parquet" reqInt64
  writeReq outDir "float_required.parquet" reqFloat
  writeReq outDir "double_required.parquet" reqDouble
  writeReq outDir "bool_required.parquet" reqBool
  writeReq outDir "byte_array_required.parquet" reqByteArray
  writeReq outDir "utf8_required.parquet" reqUtf8

  -- Logical-type-annotated primitives. The physical type is
  -- the underlying integer / binary; the ConvertedType
  -- annotation tells the reader to surface it as a
  -- date / time / decimal / etc. Every modern engine has to
  -- reconstruct these annotations correctly from our footer.
  writeReq outDir "date32_required.parquet" reqDate32
  writeReq outDir "time_millis_required.parquet" reqTimeMillis
  writeReq outDir "timestamp_millis_required.parquet" reqTimestampMillis
  writeReq outDir "uint32_required.parquet" reqUInt32
  writeReq outDir "json_required.parquet" reqJson

  -- Compressed required-only.
  writeReqCompressed outDir "int64_zstd.parquet" P.ZSTD reqInt64
  writeReqCompressed outDir "int64_snappy.parquet" P.Snappy reqInt64
  writeReqCompressed outDir "int64_gzip.parquet" P.GZip reqInt64
  writeReqCompressed outDir "int64_lz4_raw.parquet" P.LZ4Raw reqInt64

  -- Mixed required + optional through encodeParquetMixed.
  writeMixed outDir "mixed_optional.parquet" mixedSchema mixedRow

  -- All-nullable optional columns covering each OptionalColumn
  -- variant, exercising definition-level streams for every
  -- physical type.
  writeMixed outDir "optional_int32.parquet" optInt32Schema optInt32Row
  writeMixed outDir "optional_int64.parquet" optInt64Schema optInt64Row
  writeMixed outDir "optional_float.parquet" optFloatSchema optFloatRow
  writeMixed outDir "optional_bool.parquet" optBoolSchema optBoolRow

  -- Multi-row-group: same column twice in two row groups.
  writeReqMultiRG outDir "int64_two_row_groups.parquet" reqInt64Twice
  writeTemporalProbes outDir


writeTemporalProbes :: FilePath -> IO ()
writeTemporalProbes outDir =
  mapM_
    ( \version ->
        mapM_
          ( \nullable -> do
              let field name typ = AT.Field name nullable typ V.empty Nothing V.empty
                  schema =
                    AT.Schema
                      ( V.fromList
                          [ field "date" (AT.ADate AT.DateMillisecond)
                          , field "time" (AT.ATime AT.Second 32)
                          , field "stamp" (AT.ATimestamp AT.Second Nothing)
                          ]
                      )
                      AT.Little
                      V.empty
                      V.empty
                  column tag values =
                    if nullable
                      then AC.fromMaybes tag (V.imap (\i value -> if i == 1 then Nothing else Just value) (V.fromList values))
                      else AC.primColumn tag (VS.fromList values)
                  batch =
                    V.fromList
                      [ column AC.PDate64 [-86400000, 0, 86400000]
                      , column AC.PTime32 [0, 1, 86399]
                      , column AC.PTimestamp [-1, 0, 1700000000]
                      ]
                  opts = PHL.defaultWriteOptions {PHL.writePageVersion = version}
                  name =
                    "arrow_temporal_"
                      <> (if nullable then "optional_" else "required_")
                      <> (if version == PW.PageV1 then "v1" else "v2")
                      <> ".parquet"
                  encoded =
                    if nullable
                      then do
                        (parquetSchema, groups) <- PHL.arrowToParquetMixed schema [batch]
                        Right (PHL.encodeParquetMixed opts parquetSchema groups)
                      else do
                        (parquetSchema, groups) <- PHL.arrowToParquet schema [batch]
                        Right (PHL.encodeParquet opts parquetSchema groups)
              case encoded of
                Left err -> fail err
                Right bytes -> BS.writeFile (outDir </> name) bytes
          )
          [False, True]
    )
    [PW.PageV1, PW.PageV2]


writeReq
  :: FilePath
  -> FilePath
  -> ([P.SchemaElement], V.Vector ColumnData)
  -> IO ()
writeReq outDir fname (schema, cols) = do
  let !bs =
        PHL.encodeParquet
          PHL.defaultWriteOptions
          (V.fromList schema)
          [cols]
  BS.writeFile (outDir </> fname) bs


writeReqMultiRG
  :: FilePath
  -> FilePath
  -> ([P.SchemaElement], [V.Vector ColumnData])
  -> IO ()
writeReqMultiRG outDir fname (schema, rgs) = do
  let !bs =
        PHL.encodeParquet
          PHL.defaultWriteOptions
          (V.fromList schema)
          rgs
  BS.writeFile (outDir </> fname) bs


writeReqCompressed
  :: FilePath
  -> FilePath
  -> P.Compression
  -> ([P.SchemaElement], V.Vector ColumnData)
  -> IO ()
writeReqCompressed outDir fname codec (schema, cols) = do
  let !bs =
        PHL.encodeParquet
          PHL.defaultWriteOptions {PHL.writeCompression = codec}
          (V.fromList schema)
          [cols]
  BS.writeFile (outDir </> fname) bs


writeMixed
  :: FilePath
  -> FilePath
  -> [P.SchemaElement]
  -> V.Vector ParquetColumn
  -> IO ()
writeMixed outDir fname schema cols = do
  let !bs =
        PHL.encodeParquetMixed
          PHL.defaultWriteOptions
          (V.fromList schema)
          [cols]
  BS.writeFile (outDir </> fname) bs


-- ============================================================
-- Schemas + payloads
-- ============================================================

rootElem :: Int32 -> P.SchemaElement
rootElem n =
  P.SchemaElement
    "schema"
    Nothing
    Nothing
    (Just n)
    Nothing
    Nothing
    Nothing
    Nothing


leafElem :: P.ParquetType -> Maybe P.ConvertedType -> Text -> P.SchemaElement
leafElem ty conv name =
  P.SchemaElement name (Just P.Required) (Just ty) Nothing conv Nothing Nothing Nothing


reqInt32 :: ([P.SchemaElement], V.Vector ColumnData)
reqInt32 =
  ( [rootElem 1, leafElem P.PTInt32 Nothing "x"]
  , V.singleton (ColInt32 (VP.fromList [1, 2, 3, 4, 5 :: Int32]))
  )


reqInt64 :: ([P.SchemaElement], V.Vector ColumnData)
reqInt64 =
  ( [rootElem 1, leafElem P.PTInt64 Nothing "x"]
  , V.singleton (ColInt64 (VP.fromList [10, 20, 30, 40, 50 :: Int64]))
  )


reqInt64Twice :: ([P.SchemaElement], [V.Vector ColumnData])
reqInt64Twice =
  ( [rootElem 1, leafElem P.PTInt64 Nothing "x"]
  ,
    [ V.singleton (ColInt64 (VP.fromList [1, 2, 3 :: Int64]))
    , V.singleton (ColInt64 (VP.fromList [4, 5, 6 :: Int64]))
    ]
  )


reqFloat :: ([P.SchemaElement], V.Vector ColumnData)
reqFloat =
  ( [rootElem 1, leafElem P.PTFloat Nothing "x"]
  , V.singleton (ColFloat (VP.fromList [1.5, 2.5, 3.5 :: Float]))
  )


reqDouble :: ([P.SchemaElement], V.Vector ColumnData)
reqDouble =
  ( [rootElem 1, leafElem P.PTDouble Nothing "x"]
  , V.singleton (ColDouble (VP.fromList [1.5, -2.5, 3.14159 :: Double]))
  )


reqBool :: ([P.SchemaElement], V.Vector ColumnData)
reqBool =
  ( [rootElem 1, leafElem P.PTBoolean Nothing "x"]
  , V.singleton (ColBool (V.fromList [True, False, True, True, False]))
  )


reqByteArray :: ([P.SchemaElement], V.Vector ColumnData)
reqByteArray =
  ( [rootElem 1, leafElem P.PTByteArray Nothing "x"]
  , V.singleton
      (ColByteArray (V.fromList ["alpha", "beta", "gamma" :: ByteString]))
  )


reqUtf8 :: ([P.SchemaElement], V.Vector ColumnData)
reqUtf8 =
  -- Use Text -> UTF-8 explicitly. ByteString's IsString
  -- truncates each Char to 8 bits which silently mangles
  -- non-ASCII string literals; downstream Parquet readers
  -- (pyarrow / duckdb / polars) then refuse the file because
  -- the bytes aren't valid UTF-8.
  ( [rootElem 1, leafElem P.PTByteArray (Just P.CTUtf8) "name"]
  , V.singleton
      ( ColByteArray
          ( V.fromList
              [ TE.encodeUtf8 "Alice"
              , TE.encodeUtf8 "Bob"
              , TE.encodeUtf8 "Carol"
              , TE.encodeUtf8 "Δοε" -- Greek "doe"
              ]
          )
      )
  )


-- Mixed required + optional in one row group.
mixedSchema :: [P.SchemaElement]
mixedSchema =
  [ rootElem 3
  , P.SchemaElement "id" (Just P.Required) (Just P.PTInt64) Nothing Nothing Nothing Nothing Nothing
  , P.SchemaElement "name" (Just P.Optional) (Just P.PTByteArray) Nothing (Just P.CTUtf8) Nothing Nothing Nothing
  , P.SchemaElement "score" (Just P.Optional) (Just P.PTDouble) Nothing Nothing Nothing Nothing Nothing
  ]


mixedRow :: V.Vector ParquetColumn
mixedRow =
  V.fromList
    [ PCRequired (ColInt64 (VP.fromList [10, 20, 30 :: Int64]))
    , PCOptional
        ( OptByteArray
            ( V.fromList
                [Just "alice", Nothing, Just "carol" :: Maybe ByteString]
            )
        )
    , PCOptional
        ( OptDouble
            ( V.fromList
                [Just 1.5, Just 2.5, Nothing :: Maybe Double]
            )
        )
    ]


-- ============================================================
-- Logical-type variants (Int32 / Int64 / ByteArray with
-- ConvertedType annotations). Every modern Parquet reader
-- has to honour the annotation and surface the column as a
-- date / time / decimal / unsigned int / json string.
-- ============================================================

reqDate32 :: ([P.SchemaElement], V.Vector ColumnData)
reqDate32 =
  ( [rootElem 1, leafElem P.PTInt32 (Just P.CTDate) "d"]
  , -- Days since 1970-01-01. 18000 days ~ 2019-04-13.
    V.singleton (ColInt32 (VP.fromList [0, 18000, 19000 :: Int32]))
  )


reqTimeMillis :: ([P.SchemaElement], V.Vector ColumnData)
reqTimeMillis =
  ( [rootElem 1, leafElem P.PTInt32 (Just P.CTTimeMillis) "t"]
  , V.singleton
      (ColInt32 (VP.fromList [0, 12345, 86400000 - 1 :: Int32]))
  )


reqTimestampMillis :: ([P.SchemaElement], V.Vector ColumnData)
reqTimestampMillis =
  ( [rootElem 1, leafElem P.PTInt64 (Just P.CTTimestampMillis) "ts"]
  , V.singleton
      (ColInt64 (VP.fromList [0, 1700000000000 :: Int64]))
  )


reqUInt32 :: ([P.SchemaElement], V.Vector ColumnData)
reqUInt32 =
  ( [rootElem 1, leafElem P.PTInt32 (Just P.CTUInt32) "u"]
  , -- Stored as Int32 with an unsigned reinterpretation;
    -- maxBound :: Int32 = 2_147_483_647, fromIntegral
    -- (maxBound :: Word32) wraps to -1.
    V.singleton
      ( ColInt32
          ( VP.fromList
              [ 0
              , 1
              , maxBound :: Int32 -- = 2_147_483_647 unsigned
              , -1 :: Int32 -- = 4_294_967_295 unsigned
              ]
          )
      )
  )


reqJson :: ([P.SchemaElement], V.Vector ColumnData)
reqJson =
  ( [rootElem 1, leafElem P.PTByteArray (Just P.CTJson) "doc"]
  , V.singleton
      ( ColByteArray
          ( V.fromList
              [ TE.encodeUtf8 "{\"k\":1}"
              , TE.encodeUtf8 "{\"k\":2,\"j\":\"hi\"}"
              ]
          )
      )
  )


-- ============================================================
-- Optional-column-only schemas (one column each).
-- ============================================================

optColSchema :: P.ParquetType -> Maybe P.ConvertedType -> Text -> [P.SchemaElement]
optColSchema ty conv name =
  [ rootElem 1
  , P.SchemaElement name (Just P.Optional) (Just ty) Nothing conv Nothing Nothing Nothing
  ]


optInt32Schema, optInt64Schema, optFloatSchema, optBoolSchema :: [P.SchemaElement]
optInt32Schema = optColSchema P.PTInt32 Nothing "x"
optInt64Schema = optColSchema P.PTInt64 Nothing "x"
optFloatSchema = optColSchema P.PTFloat Nothing "x"
optBoolSchema = optColSchema P.PTBoolean Nothing "x"


optInt32Row, optInt64Row, optFloatRow, optBoolRow :: V.Vector ParquetColumn
optInt32Row =
  V.singleton $
    PCOptional $
      OptInt32
        ( V.fromList
            [Just 1, Nothing, Just 3, Just (-1)]
        )
optInt64Row =
  V.singleton $
    PCOptional $
      OptInt64
        ( V.fromList
            [Just 100, Nothing, Just 300]
        )
optFloatRow =
  V.singleton $
    PCOptional $
      OptFloat
        ( V.fromList
            [Just 1.5, Just 2.5, Nothing, Just 4.5]
        )
optBoolRow =
  V.singleton $
    PCOptional $
      OptBool
        ( V.fromList
            [Just True, Nothing, Just False, Just True, Nothing]
        )


data NestedProbeCase = NestedProbeCase
  { nestedProbeName :: !String
  , nestedProbeType :: !AT.ArrowType
  , nestedProbeColumn :: !AC.ColumnArray
  }


writeNestedTypeProbes :: FilePath -> IO ()
writeNestedTypeProbes outDir = do
  createDirectoryIfMissing True outDir
  let scalar name typ = AT.Field name False typ V.empty Nothing V.empty
      sample = NestedProbeCase
      cases =
        [ sample "i8" (AT.AInt 8 True) (AC.primColumn AC.PInt8 (VS.fromList [-128, -1, 0, 127]))
        , sample "i16" (AT.AInt 16 True) (AC.primColumn AC.PInt16 (VS.fromList [-32768, -1, 0, 32767]))
        , sample "i32" (AT.AInt 32 True) (AC.primColumn AC.PInt32 (VS.fromList [minBound, -1, 0, maxBound]))
        , sample "i64" (AT.AInt 64 True) (AC.primColumn AC.PInt64 (VS.fromList [minBound, -1, 0, maxBound]))
        , sample "u8" (AT.AInt 8 False) (AC.primColumn AC.PUInt8 (VS.fromList [0, 127, 128, 255]))
        , sample "u16" (AT.AInt 16 False) (AC.primColumn AC.PUInt16 (VS.fromList [0, 32767, 32768, 65535]))
        , sample "u32" (AT.AInt 32 False) (AC.primColumn AC.PUInt32 (VS.fromList [0, 2147483647, 2147483648, maxBound]))
        , sample "u64" (AT.AInt 64 False) (AC.primColumn AC.PUInt64 (VS.fromList [0, 9223372036854775807, 9223372036854775808, maxBound]))
        , sample "date32" (AT.ADate AT.DateDay) (AC.primColumn AC.PDate32 (VS.fromList [-1, 0, 1, 20000]))
        , sample "date64" (AT.ADate AT.DateMillisecond) (AC.primColumn AC.PDate64 (VS.fromList [-86400000, 0, 86400000, 172800000]))
        , sample "time_s" (AT.ATime AT.Second 32) (AC.primColumn AC.PTime32 (VS.fromList [0, 1, 43200, 86399]))
        , sample "time_ms" (AT.ATime AT.Millisecond 32) (AC.primColumn AC.PTime32 (VS.fromList [0, 1, 43200000, 86399999]))
        , sample "time_us" (AT.ATime AT.Microsecond 64) (AC.primColumn AC.PTime64 (VS.fromList [0, 1, 43200000000, 86399999999]))
        , sample "time_ns" (AT.ATime AT.Nanosecond 64) (AC.primColumn AC.PTime64 (VS.fromList [0, 1, 43200000000000, 86399999999999]))
        , sample "bool" AT.ABool (AC.fromBools (V.fromList [True, False, True, True]))
        ]
          ++ concatMap
            ( \(zoneName, zone) ->
                map
                  ( \(unitName, unit) ->
                      sample
                        ("timestamp_" ++ unitName ++ "_" ++ zoneName)
                        (AT.ATimestamp unit zone)
                        (AC.primColumn AC.PTimestamp (VS.fromList [-1, 0, 1, 1700000000]))
                  )
                  [("s", AT.Second), ("ms", AT.Millisecond), ("us", AT.Microsecond), ("ns", AT.Nanosecond)]
            )
            [("local", Nothing), ("utc", Just "UTC")]
      emit name field column = do
        schema <- either fail pure (PHL.arrowFieldToNestedSchema field)
        rows <- either fail pure (PHL.columnArrayToNestedRows field column)
        bytes <- either fail pure (PHL.encodeParquetNested (V.singleton ("v", schema)) (V.singleton rows))
        BS.writeFile (outDir </> name ++ ".parquet") bytes
  mapM_
    ( \entry -> do
        let name = nestedProbeName entry
            typ = nestedProbeType entry
            column = nestedProbeColumn entry
            valueField = scalar "v" typ
        emit (name ++ "_scalar") valueField column
        let parentMask = AC.validityFromBools (V.fromList [True, False, True, True])
            parentField = AT.Field "v" True AT.AStruct (V.singleton valueField) Nothing V.empty
        parent <- either fail pure (AC.mkStruct 4 parentMask (V.singleton ("v", column)))
        emit (name ++ "_struct") parentField parent
        let element = scalar "element" typ
            listField = AT.Field "v" False AT.AList (V.singleton element) Nothing V.empty
        list <- either fail pure (AC.mkList Nothing (VS.fromList [1, 3, 3, 4]) column)
        emit (name ++ "_list") listField list
        let key = scalar "key" (AT.AInt 32 False)
            value = scalar "value" typ
            mapField =
              AT.Field
                "v"
                False
                (AT.AMap False)
                (V.singleton (AT.Field "entries" False AT.AStruct (V.fromList [key, value]) Nothing V.empty))
                Nothing
                V.empty
            keys = AC.primColumn AC.PUInt32 (VS.fromList [1, 2, 3, 4])
        mapColumn <- either fail pure (AC.mkMap Nothing (VS.fromList [1, 3, 3, 4]) keys column)
        emit (name ++ "_map") mapField mapColumn
    )
    cases
  mapM_
    ( \count -> do
        let values = V.generate count (\i -> if i `rem` 4 == 0 then Nothing else Just (i `rem` 3 == 0))
        emit ("bool_tail_" ++ show count) ((scalar "v" AT.ABool) {AT.fieldNullable = True}) (AC.fromMaybeBools values)
    )
    [0 .. 65]
  let parentMask = AC.validityFromBools (V.fromList [True, False, True])
      dateField = scalar "date" (AT.ADate AT.DateMillisecond)
      parentField = AT.Field "v" True AT.AStruct (V.singleton dateField) Nothing V.empty
  dates <- either fail pure (AC.mkPrim AC.PDate64 parentMask (VS.fromList [-86400000, 1, 86400000]))
  parent <- either fail pure (AC.mkStruct 3 parentMask (V.singleton ("date", dates)))
  emit "poison_struct" parentField parent
  let timeField = scalar "element" (AT.ATime AT.Second 32)
      listField = AT.Field "v" False AT.AList (V.singleton timeField) Nothing V.empty
  times <- either fail pure (AC.mkList Nothing (VS.fromList [1, 3]) (AC.primColumn AC.PTime32 (VS.fromList [maxBound, 0, 1, minBound])))
  emit "poison_list" listField times
  let keys = AC.primColumn AC.PInt32 (VS.fromList [1, 2])
      keyField = scalar "key" (AT.AInt 32 True)
      valueField = scalar "value" (AT.ADate AT.DateMillisecond)
      mapField =
        AT.Field
          "v"
          True
          (AT.AMap False)
          (V.singleton (AT.Field "entries" False AT.AStruct (V.fromList [keyField, valueField]) Nothing V.empty))
          Nothing
          V.empty
      mapMask = AC.validityFromBools (V.fromList [False, True])
  hiddenKeys <- either fail pure (AC.maskValidity mapMask keys)
  hiddenMap <- either fail pure (AC.mkMap mapMask (VS.fromList [0, 1, 2]) hiddenKeys (AC.primColumn AC.PDate64 (VS.fromList [1, 86400000])))
  emit "poison_map" mapField hiddenMap
  let optionalMapField =
        AT.Field
          "v"
          False
          (AT.AMap False)
          ( V.singleton
              ( AT.Field
                  "entries"
                  False
                  AT.AStruct
                  (V.fromList [scalar "key" (AT.AInt 32 False), (scalar "value" (AT.AInt 64 False)) {AT.fieldNullable = True}])
                  Nothing
                  V.empty
              )
          )
          Nothing
          V.empty
  optionalValues <- either fail pure (AC.mkPrim AC.PUInt64 (AC.validityFromBools (V.fromList [True, False, True])) (VS.fromList [maxBound, 0, 9223372036854775808]))
  optionalMap <- either fail pure (AC.mkMap Nothing (VS.fromList [0, 2, 2, 3]) (AC.primColumn AC.PUInt32 (VS.fromList [1, 2, 3])) optionalValues)
  emit "map_optional_values" optionalMapField optionalMap
  let nestedListSchema = PN.NSOptional (PN.NSList (PN.NSOptional (PN.NSList (PN.NSOptional (PN.NSPrimitive PN.LtInt32)))))
      int = PN.NRLeaf . PN.LvInt32
      list = PN.NRList . V.fromList
      nestedListRows =
        V.fromList
          [ list [list [int 1, PN.NRNull], PN.NRNull, list [], list [int 2]]
          , PN.NRNull
          , list []
          , list [list [], list [PN.NRNull]]
          ]
  nestedListBytes <- either fail pure (PHL.encodeParquetNested (V.singleton ("v", nestedListSchema)) (V.singleton nestedListRows))
  BS.writeFile (outDir </> "list_of_lists.parquet") nestedListBytes
  mapM_
    ( \entry -> do
        let name = nestedProbeName entry
            typ = nestedProbeType entry
        primitive <- either fail pure (PHL.arrowFieldToNestedSchema (scalar "element" typ))
        core <- case primitive of
          PN.NSRequired child -> pure child
          _ -> fail "legacy probe needs a required primitive field"
        values <- either fail pure (PHL.columnArrayToNestedRows (scalar "element" typ) (nestedProbeColumn entry))
        let schema = PN.NSOptional (PN.NSList (PN.NSOptional core))
            rows =
              V.fromList
                [ PN.NRList (V.fromList [values V.! 0, PN.NRNull, values V.! 1])
                , PN.NRNull
                , PN.NRList V.empty
                , PN.NRList (V.fromList [values V.! 2, values V.! 3])
                ]
        leaves <- either fail pure (PN.shred schema rows)
        let physicalSchema = V.last (PN.nestedSchemaToFlatSchema (V.singleton ("v", primitive)))
        physical <- maybe (fail "legacy probe needs a physical type") pure (P.seType physicalSchema)
        let leaf = (V.head leaves) {PN.nlPath = V.fromList ["v", "list", "element"]}
        bytes <- either fail pure (PN.buildOptionalListFileEither physical leaf 4)
        BS.writeFile (outDir </> name ++ ".parquet") bytes
    )
    [ sample "legacy_i32" (AT.AInt 32 True) (AC.primColumn AC.PInt32 (VS.fromList [minBound, -1, 0, maxBound]))
    , sample "legacy_i64" (AT.AInt 64 True) (AC.primColumn AC.PInt64 (VS.fromList [minBound, -1, 0, maxBound]))
    , sample "legacy_double" (AT.AFloatingPoint AT.DoublePrecision) (AC.primColumn AC.PDouble (VS.fromList [-1.5, 0, 1.5, 1e100]))
    , sample "legacy_bool" AT.ABool (AC.fromBools (V.fromList [True, False, True, True]))
    , sample "legacy_binary" AT.ABinary (AC.fromByteStrings (V.fromList ["", "abc", BS.pack [0, 255], "tail"]))
    ]


-- FILE logical groups: subset schemas, reordered names/IDs, nulls, and nesting.
writeFileProbes :: FilePath -> IO ()
writeFileProbes outDir = do
  createDirectoryIfMissing True outDir
  let empty = File.emptyFileReference
      inline = empty {File.fileInline = Just ""}
      uri = empty {File.fileUri = Just "../images/a%20b.png?raw=1#part"}
      both = uri {File.fileOffset = Just 0, File.fileSize = Just (fromIntegral (BS.length "whole inline payload")), File.fileInline = Just "whole inline payload"}
      maximum = uri {File.fileOffset = Just maxBound, File.fileSize = Just maxBound}
      fullRows = V.fromList [PN.NRNull, PN.NRFile inline, PN.NRFile uri, PN.NRFile both, PN.NRFile maximum, PN.NRFile inline {File.fileUri = Just "", File.fileContentType = Just "", File.fileChecksum = Just ""}]
      emit name fields rows = do
        schema <- either fail pure (File.mkFileSchema (V.fromList fields))
        bytes <- either fail pure (PN.buildNestedFile (V.singleton ("f", PN.NSOptional (PN.NSFile schema))) (V.singleton rows))
        BS.writeFile (outDir </> name <> ".parquet") bytes
      specs = map (\field -> File.FileFieldSpec field (Just (fromIntegral (fromEnum field + 10)))) [minBound .. maxBound]
  emit "full" specs fullRows
  emit "reordered" (reverse specs) fullRows
  emit "inline" [File.FileFieldSpec File.FileInline Nothing] (V.fromList [PN.NRNull, PN.NRFile inline, PN.NRFile inline {File.fileInline = Just "bytes"}])
  emit "uri" [File.FileFieldSpec File.FileUri Nothing] (V.fromList [PN.NRNull, PN.NRFile uri, PN.NRFile empty {File.fileUri = Just "uri.parquet"}])
  emit "metadata-only" [File.FileFieldSpec File.FileContentType Nothing] (V.fromList [PN.NRNull, PN.NRNull])
  schema <- either fail pure (File.mkFileSchema (V.fromList specs))
  let list = PN.NSOptional (PN.NSList (PN.NSOptional (PN.NSFile schema)))
      rows = V.fromList [PN.NRNull, PN.NRList V.empty, PN.NRList (V.fromList [PN.NRNull, PN.NRFile uri, PN.NRFile inline])]
  bytes <- either fail pure (PN.buildNestedFile (V.singleton ("files", list)) (V.singleton rows))
  BS.writeFile (outDir </> "list.parquet") bytes


checkFileReferences :: FilePath -> IO ()
checkFileReferences sourceDir = do
  names <- listDirectory sourceDir
  V.forM_ (V.fromList (filter ((== ".parquet") . takeExtension) names)) $ \name -> do
    bytes <- BS.readFile (sourceDir </> name)
    file <- either fail pure (Read.loadParquetFile bytes)
    let elements = P.fmSchema (Read.pfFooter file)
    schema <- either fail pure (File.parseFileSchema (elements V.! 1) (V.drop 2 elements))
    arrow <- either fail pure (PHL.parquetFileArrowSchemaChecked file)
    columns <- either (fail . show) pure (PHL.parquetRowGroupToArrow arrow file 0)
    reader <- either fail pure (File.compileFileReader schema (V.head columns))
    V.forM_ (V.generate (AC.columnLength (V.head columns)) id) $ \index -> do
      reference <- either fail pure (File.fileReferenceAt reader index)
      case reference of
        Nothing -> pure ()
        Just value -> either fail (const (pure ())) (File.resolveFileReference value)
    putStrLn ("FILE checked " ++ name)


writeNestedArrowInputs :: FilePath -> FilePath -> IO ()
writeNestedArrowInputs = writeNestedArrowInputsWith Nothing


writeNestedOptionsArrowInputs :: FilePath -> FilePath -> IO ()
writeNestedOptionsArrowInputs sourceDir outDir =
  forM_ [P.Uncompressed, P.GZip, P.Snappy, P.ZSTD, P.LZ4Raw, P.LZO, P.Brotli] $ \codec ->
    forM_ [PW.PageV1, PW.PageV2] $ \version -> do
      let options = PHL.defaultWriteOptions {PHL.writeCompression = codec, PHL.writePageVersion = version}
      writeNestedArrowInputsWith (Just options) sourceDir (outDir </> show codec <> "-" <> show version)


writeNestedArrowInputsWith :: Maybe PHL.WriteOptions -> FilePath -> FilePath -> IO ()
writeNestedArrowInputsWith options sourceDir outDir = do
  createDirectoryIfMissing True outDir
  names <- listDirectory sourceDir
  mapM_
    ( \name -> when (takeExtension name == ".arrow") $ do
        bytes <- BS.readFile (sourceDir </> name)
        (schema, batches) <- either fail pure (ArrowFile.readArrowFileColumns ArrowRead.defaultReadOptions bytes)
        let fields = AT.arrowFields schema
        nested <- either fail pure (V.mapM PHL.arrowFieldToNestedSchema fields)
        rows <- either fail pure (V.imapM (\index field -> V.concatMap id <$> V.mapM (\batch -> PHL.columnArrayToNestedRows field (batch V.! index)) batches) fields)
        let columns = V.zip (AT.fieldName <$> fields) nested
        output <- either fail pure (case options of Nothing -> PHL.encodeParquetNested columns rows; Just opts -> PHL.encodeParquetNestedWith opts columns rows)
        BS.writeFile (outDir </> takeBaseName name <> ".parquet") output
    )
    names


-- Semantic shreds are checked separately from physical storage forwarding.
-- Keep the source metadata bytes so fallback fragments cannot accidentally
-- acquire a fresh, incompatible dictionary.
writeShreddedVariantInputs :: FilePath -> FilePath -> IO ()
writeShreddedVariantInputs sourceDir outDir = do
  createDirectoryIfMissing True outDir
  names <- listDirectory sourceDir
  mapM_
    ( \name -> when (takeExtension name == ".arrow") $ do
        bytes <- BS.readFile (sourceDir </> name)
        (schema, batches) <- either fail pure (ArrowFile.readArrowFileColumns ArrowRead.defaultReadOptions bytes)
        when (V.length (AT.arrowFields schema) /= 1) (fail "Variant semantic probe expects one column")
        let field = V.head (AT.arrowFields schema)
        shredder <- either fail pure (VariantShred.compileVariantShredder field)
        rows <-
          V.concat . V.toList
            <$> V.mapM
              ( \columns -> do
                  let column = V.head columns
                  reader <- either fail pure (VariantArrow.compileVariantArrowReader field column)
                  metadata <- case column of
                    AC.ColStruct _ _ children -> maybe (fail "missing metadata") (pure . snd) (V.find ((== "metadata") . fst) children)
                    _ -> fail "expected Variant struct"
                  V.generateM
                    (AC.columnLength column)
                    ( \index -> do
                        value <- either fail pure (VariantArrow.variantArrowAt reader index)
                        case value of
                          Nothing -> either fail pure (VariantShred.shredVariant shredder Nothing)
                          Just present -> do
                            raw <- maybe (fail "missing metadata bytes") pure (probeMetadata metadata index)
                            dictionary <- either fail pure (Variant.compileVariantMetadata raw)
                            either fail pure (VariantShred.shredVariantWithMetadata shredder dictionary (Just present))
                    )
              )
              batches
        output <- either fail pure (PHL.encodeParquetNested (V.singleton (AT.fieldName field, VariantShred.variantShredSchema shredder)) (V.singleton rows))
        BS.writeFile (outDir </> takeBaseName name <> ".parquet") output
    )
    names


probeMetadata :: AC.ColumnArray -> Int -> Maybe ByteString
probeMetadata column index = case column of
  AC.ColDictionary _ _ values -> AC.dictKeyAt column index >>= probeMetadata values
  AC.ColRunEndEncoded offset _ ends values -> do
    run <- case ends of
      AC.ColInt16 _ vector -> findRun vector (offset + index)
      AC.ColInt32 _ vector -> findRun vector (offset + index)
      AC.ColInt64 _ vector -> findRun vector (offset + index)
      _ -> Nothing
    probeMetadata values run
  _ -> AC.anyBytesAt column index
  where
    findRun :: (Storable a, Integral a) => VS.Vector a -> Int -> Maybe Int
    findRun ends position = search 0 (VS.length ends)
      where
        search low high
          | low == high = if low < VS.length ends then Just low else Nothing
          | fromIntegral (VS.unsafeIndex ends middle) <= position = search (middle + 1) high
          | otherwise = search low middle
          where
            middle = low + (high - low) `quot` 2
