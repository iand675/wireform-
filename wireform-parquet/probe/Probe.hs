{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | wireform-parquet -> pyarrow / duckdb / polars round-trip
-- probe.
--
-- Writes a fixed catalogue of one-file-per-shape Parquet files
-- under the directory passed as @argv[1]@. The companion
-- 'scripts/parquet_interop.py' driver opens each one with
-- pyarrow, then with duckdb, then with polars and asserts the
-- contents match what we expect.
--
-- Everything written here uses our 'Parquet.HighLevel.encodeParquet'
-- / 'encodeParquetMixed' so the probe exercises the same code path
-- that real users hit.
module Main (main) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Arrow.Column as AC
import qualified Arrow.View as AV
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import qualified Parquet.HighLevel as PHL
import qualified Parquet.Types as P
import Parquet.Write
  ( ColumnData (..)
  , ParquetColumn (..)
  , OptionalColumn (..)
  )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [outDir] -> do
      writeAllProbes outDir
      putStrLn ("wrote parquet probe outputs to " ++ outDir)
    _ -> do
      putStrLn "usage: wireform-parquet-interop-probe <output-dir>"
      exitFailure

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
  writeReq outDir "date32_required.parquet"        reqDate32
  writeReq outDir "time_millis_required.parquet"   reqTimeMillis
  writeReq outDir "timestamp_millis_required.parquet" reqTimestampMillis
  writeReq outDir "uint32_required.parquet"        reqUInt32
  writeReq outDir "json_required.parquet"          reqJson

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
  writeMixed outDir "optional_bool.parquet"  optBoolSchema  optBoolRow

  -- Multi-row-group: same column twice in two row groups.
  writeReqMultiRG outDir "int64_two_row_groups.parquet" reqInt64Twice

writeReq
  :: FilePath -> FilePath
  -> ([P.SchemaElement], V.Vector ColumnData)
  -> IO ()
writeReq outDir fname (schema, cols) = do
  let !bs = PHL.encodeParquet PHL.defaultWriteOptions
              (V.fromList schema) [cols]
  BS.writeFile (outDir </> fname) bs

writeReqMultiRG
  :: FilePath -> FilePath
  -> ([P.SchemaElement], [V.Vector ColumnData])
  -> IO ()
writeReqMultiRG outDir fname (schema, rgs) = do
  let !bs = PHL.encodeParquet PHL.defaultWriteOptions
              (V.fromList schema) rgs
  BS.writeFile (outDir </> fname) bs

writeReqCompressed
  :: FilePath -> FilePath -> P.Compression
  -> ([P.SchemaElement], V.Vector ColumnData)
  -> IO ()
writeReqCompressed outDir fname codec (schema, cols) = do
  let !bs = PHL.encodeParquet
              PHL.defaultWriteOptions { PHL.writeCompression = codec }
              (V.fromList schema) [cols]
  BS.writeFile (outDir </> fname) bs

writeMixed
  :: FilePath -> FilePath
  -> [P.SchemaElement] -> V.Vector ParquetColumn
  -> IO ()
writeMixed outDir fname schema cols = do
  let !bs = PHL.encodeParquetMixed PHL.defaultWriteOptions
              (V.fromList schema) [cols]
  BS.writeFile (outDir </> fname) bs

-- ============================================================
-- Schemas + payloads
-- ============================================================

rootElem :: Int32 -> P.SchemaElement
rootElem n = P.SchemaElement "schema" Nothing Nothing (Just n)
  Nothing Nothing Nothing

leafElem :: P.ParquetType -> Maybe P.ConvertedType -> Text -> P.SchemaElement
leafElem ty conv name =
  P.SchemaElement name (Just P.Required) (Just ty) Nothing conv Nothing Nothing

reqInt32 :: ([P.SchemaElement], V.Vector ColumnData)
reqInt32 =
  ( [rootElem 1, leafElem P.PTInt32 Nothing "x"]
  , V.singleton (ColInt32 (VS.fromList [1, 2, 3, 4, 5 :: Int32]))
  )

reqInt64 :: ([P.SchemaElement], V.Vector ColumnData)
reqInt64 =
  ( [rootElem 1, leafElem P.PTInt64 Nothing "x"]
  , V.singleton (ColInt64 (VS.fromList [10, 20, 30, 40, 50 :: Int64]))
  )

reqInt64Twice :: ([P.SchemaElement], [V.Vector ColumnData])
reqInt64Twice =
  ( [rootElem 1, leafElem P.PTInt64 Nothing "x"]
  ,   [ V.singleton (ColInt64 (VS.fromList [1, 2, 3 :: Int64]))
      , V.singleton (ColInt64 (VS.fromList [4, 5, 6 :: Int64]))
      ]
  )

reqFloat :: ([P.SchemaElement], V.Vector ColumnData)
reqFloat =
  ( [rootElem 1, leafElem P.PTFloat Nothing "x"]
  , V.singleton (ColFloat (VS.fromList [1.5, 2.5, 3.5 :: Float]))
  )

reqDouble :: ([P.SchemaElement], V.Vector ColumnData)
reqDouble =
  ( [rootElem 1, leafElem P.PTDouble Nothing "x"]
  , V.singleton (ColDouble (VS.fromList [1.5, -2.5, 3.14159 :: Double]))
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
      (ColByteArray (V.fromList
         [ TE.encodeUtf8 "Alice"
         , TE.encodeUtf8 "Bob"
         , TE.encodeUtf8 "Carol"
         , TE.encodeUtf8 "Δοε"  -- Greek "doe"
         ]))
  )

-- Mixed required + optional in one row group.
mixedSchema :: [P.SchemaElement]
mixedSchema =
  [ rootElem 3
  , P.SchemaElement "id"   (Just P.Required) (Just P.PTInt64)     Nothing Nothing Nothing Nothing
  , P.SchemaElement "name" (Just P.Optional) (Just P.PTByteArray) Nothing (Just P.CTUtf8) Nothing Nothing
  , P.SchemaElement "score" (Just P.Optional) (Just P.PTDouble)   Nothing Nothing Nothing Nothing
  ]

mixedRow :: V.Vector ParquetColumn
mixedRow = V.fromList
  [ PCRequired (ColInt64 (VS.fromList [10, 20, 30 :: Int64]))
  , PCOptional (OptByteArray
      (AV.nullableBinaryViewFromMaybeVector (V.fromList
        [ Just "alice", Nothing, Just "carol" :: Maybe ByteString ])))
  , PCOptional (OptDouble (AC.nvFromMaybeList 0
      [ Just 1.5, Just 2.5, Nothing :: Maybe Double ]))
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
    -- Days since 1970-01-01. 18000 days ~ 2019-04-13.
  , V.singleton (ColInt32 (VS.fromList [0, 18000, 19000 :: Int32]))
  )

reqTimeMillis :: ([P.SchemaElement], V.Vector ColumnData)
reqTimeMillis =
  ( [rootElem 1, leafElem P.PTInt32 (Just P.CTTimeMillis) "t"]
  , V.singleton
      (ColInt32 (VS.fromList [0, 12345, 86400000 - 1 :: Int32]))
  )

reqTimestampMillis :: ([P.SchemaElement], V.Vector ColumnData)
reqTimestampMillis =
  ( [rootElem 1, leafElem P.PTInt64 (Just P.CTTimestampMillis) "ts"]
  , V.singleton
      (ColInt64 (VS.fromList [0, 1700000000000 :: Int64]))
  )

reqUInt32 :: ([P.SchemaElement], V.Vector ColumnData)
reqUInt32 =
  ( [rootElem 1, leafElem P.PTInt32 (Just P.CTUInt32) "u"]
    -- Stored as Int32 with an unsigned reinterpretation;
    -- maxBound :: Int32 = 2_147_483_647, fromIntegral
    -- (maxBound :: Word32) wraps to -1.
  , V.singleton (ColInt32 (VS.fromList
      [ 0, 1
      , maxBound :: Int32   -- = 2_147_483_647 unsigned
      , -1 :: Int32         -- = 4_294_967_295 unsigned
      ]))
  )

reqJson :: ([P.SchemaElement], V.Vector ColumnData)
reqJson =
  ( [rootElem 1, leafElem P.PTByteArray (Just P.CTJson) "doc"]
  , V.singleton (ColByteArray (V.fromList
      [ TE.encodeUtf8 "{\"k\":1}"
      , TE.encodeUtf8 "{\"k\":2,\"j\":\"hi\"}"
      ]))
  )

-- ============================================================
-- Optional-column-only schemas (one column each).
-- ============================================================

optColSchema :: P.ParquetType -> Maybe P.ConvertedType -> Text -> [P.SchemaElement]
optColSchema ty conv name =
  [ rootElem 1
  , P.SchemaElement name (Just P.Optional) (Just ty) Nothing conv Nothing Nothing
  ]

optInt32Schema, optInt64Schema, optFloatSchema, optBoolSchema :: [P.SchemaElement]
optInt32Schema = optColSchema P.PTInt32   Nothing "x"
optInt64Schema = optColSchema P.PTInt64   Nothing "x"
optFloatSchema = optColSchema P.PTFloat   Nothing "x"
optBoolSchema  = optColSchema P.PTBoolean Nothing "x"

optInt32Row, optInt64Row, optFloatRow, optBoolRow :: V.Vector ParquetColumn
optInt32Row = V.singleton $ PCOptional $ OptInt32 (AC.nvFromMaybeList 0
  [ Just 1, Nothing, Just 3, Just (-1) ])
optInt64Row = V.singleton $ PCOptional $ OptInt64 (AC.nvFromMaybeList 0
  [ Just 100, Nothing, Just 300 ])
optFloatRow = V.singleton $ PCOptional $ OptFloat (AC.nvFromMaybeList 0
  [ Just 1.5, Just 2.5, Nothing, Just 4.5 ])
optBoolRow = V.singleton $ PCOptional $ OptBool
  (AV.nullableBoolViewFromMaybeVector (V.fromList
    [ Just True, Nothing, Just False, Just True, Nothing ]))
