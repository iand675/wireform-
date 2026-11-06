{-# LANGUAGE BangPatterns #-}

{- | End-to-end Parquet write + read throughput baseline for
wireform-parquet, measured on a synthetic dataset of
realistic shape and size. Output goes through criterion +
a JSON report so the matching Python driver can compare
against pyarrow on the same shape.

Usage:

@
cabal bench wireform-parquet:parquet-throughput

# or, with explicit options for the JSON reporter:
cabal bench wireform-parquet:parquet-throughput \\
  -- --json wireform_throughput.json
@

The dataset:

  * 100,000 rows
  * 4 columns: int64 id, double score, utf8 names drawn from 1000 values, bool active
  * single row group and one required PLAIN V2 page per column
  * uncompressed, Snappy and Zstd at level 3

Each measurement is one full encode (or decode) of the
complete dataset; criterion divides the wall-clock time by
the iteration count so the raw seconds are per-encode.
-}
module Main (main) where

import Arrow.Column qualified as AC
import Arrow.Types qualified as AT
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad (unless)
import Criterion.Main (bench, bgroup, defaultMain, env, nf, whnf)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Data.Vector.Storable qualified as VS
import Parquet.Arrow qualified as PArrow
import Parquet.HighLevel qualified as PHL
import Parquet.Page qualified as Page
import Parquet.Read qualified as Read
import Parquet.Types qualified as P
import Parquet.Write (ColumnData (..))
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))


{- | 100k rows is enough to push past per-call overhead while
keeping each criterion sample under a second.
-}
nRows :: Int
nRows = 100_000


{- | Generate the same 4-column dataset every run.

id      = increasing Int64 0..N-1
score   = i * 0.5
name    = "name_<i_mod_1000>" (so dict encoding has work to do)
active  = i `rem` 2 == 0
-}
mkDataset :: Int -> V.Vector ColumnData
mkDataset n =
  V.fromList
    [ ColInt64 (VP.generate n fromIntegral)
    , ColDouble (VP.generate n (\i -> fromIntegral i * 0.5))
    , ColByteArray $ V.generate n $ \i -> V.unsafeIndex names (i `rem` 1000)
    , ColBool (V.generate n (\i -> i `rem` 2 == 0))
    ]
  where
    names = V.generate 1000 (BL.toStrict . BB.toLazyByteString . ("name_" <>) . BB.intDec)


mkSchema :: V.Vector P.SchemaElement
mkSchema =
  V.fromList
    [ P.SchemaElement "schema" Nothing Nothing (Just 4) Nothing Nothing Nothing Nothing
    , P.SchemaElement "id" (Just P.Required) (Just P.PTInt64) Nothing Nothing Nothing Nothing Nothing
    , P.SchemaElement "score" (Just P.Required) (Just P.PTDouble) Nothing Nothing Nothing Nothing Nothing
    , P.SchemaElement "name" (Just P.Required) (Just P.PTByteArray) Nothing (Just P.CTUtf8) Nothing Nothing Nothing
    , P.SchemaElement "active" (Just P.Required) (Just P.PTBoolean) Nothing Nothing Nothing Nothing Nothing
    ]


writeFile_ :: V.Vector ColumnData -> BS.ByteString
writeFile_ cols =
  PHL.encodeParquet
    PHL.defaultWriteOptions {PHL.writeCompression = P.Uncompressed, PHL.writePageIndex = False}
    mkSchema
    [cols]


writeFileZstd :: V.Vector ColumnData -> BS.ByteString
writeFileZstd cols =
  PHL.encodeParquet
    PHL.defaultWriteOptions {PHL.writeCompression = P.ZSTD, PHL.writePageIndex = False}
    mkSchema
    [cols]


writeFileSnappy :: V.Vector ColumnData -> BS.ByteString
writeFileSnappy cols =
  PHL.encodeParquet
    PHL.defaultWriteOptions {PHL.writeCompression = P.Snappy, PHL.writePageIndex = False}
    mkSchema
    [cols]


-- Retain the decoded batches just as the Rust/C++ public readers do.
-- Criterion forces every buffer; timing a row count could discard columns
-- earlier and would not check that the requested values were reconstructed.
data DecodedFixture = DecodedFixture
  { fixtureSchema :: !AT.Schema
  , fixtureBatches :: ![V.Vector AC.ColumnArray]
  }


decodeFixture :: BS.ByteString -> Either String DecodedFixture
decodeFixture bs = do
  file <- PHL.decodeParquet PHL.defaultReadOptions bs
  schema <- PArrow.parquetFileArrowSchemaChecked file
  batches <-
    mapM
      (\i -> either (Left . show) Right (PArrow.parquetRowGroupToArrow schema file i))
      [0 .. PArrow.numRowGroups file - 1]
  Right (DecodedFixture schema batches)


readBack :: BS.ByteString -> [V.Vector AC.ColumnArray]
readBack = either error fixtureBatches . decodeFixture


data FileCase = FileCase
  { fileLabel :: !FilePath
  , fileCodec :: !P.Compression
  , fileEncode :: !(V.Vector ColumnData -> BS.ByteString)
  }


fileCases :: [FileCase]
fileCases = [FileCase "uncompressed" P.Uncompressed writeFile_, FileCase "snappy" P.Snappy writeFileSnappy, FileCase "zstd" P.ZSTD writeFileZstd]


expectedSchema :: AT.Schema
expectedSchema =
  AT.Schema
    (V.fromList [field "id" (AT.AInt 64 True), field "score" (AT.AFloatingPoint AT.DoublePrecision), field "name" AT.AUtf8, field "active" AT.ABool])
    AT.Little
    V.empty
    V.empty
  where
    field name typ = AT.Field name False typ V.empty Nothing V.empty


expectedColumns :: V.Vector AC.ColumnArray
expectedColumns =
  V.fromList
    [ AC.primColumn AC.PInt64 (VS.generate nRows fromIntegral)
    , AC.primColumn AC.PDouble (VS.generate nRows (\i -> fromIntegral i * 0.5))
    , AC.fromTexts (V.generate nRows (\i -> V.unsafeIndex names (i `rem` 1000)))
    , AC.fromBools (V.generate nRows even)
    ]
  where
    names = V.generate 1000 (TE.decodeUtf8 . BL.toStrict . BB.toLazyByteString . ("name_" <>) . BB.intDec)


validateFixture :: P.Compression -> BS.ByteString -> Either String ()
validateFixture codec bytes = do
  decoded <- decodeFixture bytes
  require (AT.schemaEquivalent (fixtureSchema decoded) expectedSchema) "schema differs from the required four-column model"
  require (fixtureBatches decoded == [expectedColumns]) "decoded values differ from the 100000-row model"
  file <- PHL.decodeParquet PHL.defaultReadOptions bytes
  let footer = Read.pfFooter file
      groups = P.fmRowGroups footer
  require (P.fmNumRows footer == fromIntegral nRows && V.length groups == 1) "layout must have one 100000-row group"
  group <- maybe (Left "fixture: missing row group") Right (groups V.!? 0)
  require (P.rgNumRows group == fromIntegral nRows && V.length (P.rgColumns group) == 4) "layout must have four required columns"
  V.imapM_ (validateColumn file) (P.rgColumns group)
  where
    require condition message = unless condition (Left ("benchmark fixture: " ++ message))
    validateColumn file index chunk = do
      metadata <- maybe (Left "benchmark fixture: missing column metadata") Right (P.ccMetadata chunk)
      require (P.cmCodec metadata == codec) "codec differs from benchmark label"
      require (P.cmNumValues metadata == fromIntegral nRows) "column event count differs from model"
      require (P.cmDictionaryPageOffset metadata == Nothing && not (V.any (`elem` [P.PlainDictionary, P.RLEDictionary]) (P.cmEncodings metadata))) "layout must disable dictionary encoding"
      require (P.ccOffsetIndexOffset chunk == Nothing && P.ccColumnIndexOffset chunk == Nothing) "layout must disable page indexes"
      body <- Read.columnChunkSlice file 0 index
      (header, offset) <- Page.readPageHeaderAt body 0
      case Page.phType header of
        Page.PtDataPageV2 page -> do
          require (Page.dph2NumValues page == fromIntegral nRows && Page.dph2NumRows page == fromIntegral nRows && Page.dph2NumNulls page == 0) "V2 page row/null counts differ from model"
          require (Page.dph2Encoding page == 0 && Page.dph2DefLevelsLen page == 0 && Page.dph2RepLevelsLen page == 0) "layout must use required PLAIN V2 values without level streams"
          require (Page.dph2IsCompressed page == (codec /= P.Uncompressed)) "page compression flag differs from benchmark label"
        _ -> Left "benchmark fixture: layout must use V2 data pages"
      size <- maybe (Left "benchmark fixture: missing page size") (Right . fromIntegral) (Page.phCompressedPageSize header)
      require (offset + size == BS.length body) "layout must have exactly one page per column"


verifyGenerated :: V.Vector ColumnData -> IO ()
verifyGenerated dataset = do
  dataset `deepseq` pure ()
  mapM_ (\sample -> either (fail . ((fileLabel sample ++ ": ") ++)) pure (validateFixture (fileCodec sample) (fileEncode sample dataset))) fileCases


verifyFiles :: FilePath -> IO ()
verifyFiles dir =
  mapM_
    ( \sample -> do
        bytes <- BS.readFile (dir </> fileLabel sample ++ ".parquet")
        either (fail . ((fileLabel sample ++ ": ") ++)) pure (validateFixture (fileCodec sample) bytes)
    )
    fileCases


instance NFData ColumnData where
  rnf (ColInt32 v) = v `seq` ()
  rnf (ColInt64 v) = v `seq` ()
  rnf (ColFloat v) = v `seq` ()
  rnf (ColDouble v) = v `seq` ()
  rnf (ColBool v) = v `deepseq` ()
  rnf (ColByteArray v) = v `deepseq` ()


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--verify-fixtures", dir] -> verifyFiles dir
    ["--write-fixtures", dir] -> do
      createDirectoryIfMissing True dir
      let !dataset = mkDataset nRows
      verifyGenerated dataset
      mapM_ (\sample -> BS.writeFile (dir </> fileLabel sample ++ ".parquet") (fileEncode sample dataset)) fileCases
    _ -> do
      verifyGenerated (mkDataset nRows)
      runBenchmarks


runBenchmarks :: IO ()
runBenchmarks =
  let !dataset = mkDataset nRows
  in defaultMain
       [ env (pure dataset) $ \cols ->
           bgroup
             ("write " ++ show nRows ++ " rows x 4 cols")
             [ bench "uncompressed" $ whnf (BS.length . writeFile_) cols
             , bench "snappy" $ whnf (BS.length . writeFileSnappy) cols
             , bench "zstd" $ whnf (BS.length . writeFileZstd) cols
             ]
       , env (pure (writeFile_ dataset)) $ \bs ->
           bgroup
             ("read " ++ show nRows ++ " rows x 4 cols (uncompressed)")
             [ bench "decode" $ nf readBack bs
             ]
       , env (pure (writeFileSnappy dataset)) $ \bs ->
           bgroup
             ("read " ++ show nRows ++ " rows x 4 cols (snappy)")
             [ bench "decode" $ nf readBack bs
             ]
       , env (pure (writeFileZstd dataset)) $ \bs ->
           bgroup
             ("read " ++ show nRows ++ " rows x 4 cols (zstd)")
             [ bench "decode" $ nf readBack bs
             ]
       ]
