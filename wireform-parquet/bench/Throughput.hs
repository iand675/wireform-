{-# LANGUAGE BangPatterns #-}
-- | End-to-end Parquet write + read throughput baseline for
-- wireform-parquet, measured on a synthetic dataset of
-- realistic shape and size. Output goes through criterion +
-- a JSON report so the matching Python driver can compare
-- against pyarrow on the same shape.
--
-- Usage:
--
-- @
-- cabal bench wireform-parquet:parquet-throughput
--
-- # or, with explicit options for the JSON reporter:
-- cabal bench wireform-parquet:parquet-throughput \\
--   -- --json wireform_throughput.json
-- @
--
-- The dataset:
--
--   * 1,000,000 rows
--   * 4 columns: int64 id, double score, utf8 name (8-byte
--     fixed strings), bool active
--   * single row group, default codec
--
-- Each measurement is one full encode (or decode) of the
-- complete dataset; criterion divides the wall-clock time by
-- the iteration count so the raw seconds are per-encode.
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad (forM_, replicateM)
import Control.Parallel.Strategies (parMap, rseq)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)

import Criterion.Main (defaultMain, bench, bgroup, env, nf, whnf)

import qualified Parquet.HighLevel as PHL
import qualified Parquet.Read as PR
import qualified Parquet.Arrow as PArrow
import qualified Parquet.Types as P
import qualified Arrow.Types as AT
import qualified Arrow.Column as AC
import Parquet.Write (ColumnData (..))

-- | Default 100k rows is enough to push past per-call
-- overhead while keeping each criterion sample under a
-- second. Override with @--rows N@ at the command line via
-- a custom envparam if needed.
nRows :: Int
nRows = 100_000

-- | Generate the same 4-column dataset every run.
--
-- id      = increasing Int64 0..N-1
-- score   = i * 0.5
-- name    = "name_<i_mod_1000>" (so dict encoding has work to do)
-- active  = i `rem` 2 == 0
mkDataset :: Int -> V.Vector ColumnData
mkDataset n = V.fromList
  [ ColInt64  (VP.generate n fromIntegral)
  , ColDouble (VP.generate n (\i -> fromIntegral i * 0.5))
  , ColByteArray $ V.generate n $ \i ->
      TE.encodeUtf8 (T.pack ("name_" ++ show (i `rem` 1000)))
  , ColBool   (V.generate n (\i -> i `rem` 2 == 0))
  ]

mkSchema :: V.Vector P.SchemaElement
mkSchema = V.fromList
  [ P.SchemaElement "schema" Nothing Nothing (Just 4) Nothing Nothing Nothing
  , P.SchemaElement "id"     (Just P.Required) (Just P.PTInt64)     Nothing Nothing Nothing Nothing
  , P.SchemaElement "score"  (Just P.Required) (Just P.PTDouble)    Nothing Nothing Nothing Nothing
  , P.SchemaElement "name"   (Just P.Required) (Just P.PTByteArray) Nothing (Just P.CTUtf8) Nothing Nothing
  , P.SchemaElement "active" (Just P.Required) (Just P.PTBoolean)   Nothing Nothing Nothing Nothing
  ]

writeFile_ :: V.Vector ColumnData -> BS.ByteString
writeFile_ cols =
  PHL.encodeParquet
    PHL.defaultWriteOptions { PHL.writeCompression = P.Uncompressed }
    mkSchema [cols]

writeFileZstd :: V.Vector ColumnData -> BS.ByteString
writeFileZstd cols =
  PHL.encodeParquet
    PHL.defaultWriteOptions { PHL.writeCompression = P.ZSTD }
    mkSchema [cols]

writeFileSnappy :: V.Vector ColumnData -> BS.ByteString
writeFileSnappy cols =
  PHL.encodeParquet
    PHL.defaultWriteOptions { PHL.writeCompression = P.Snappy }
    mkSchema [cols]

-- | Decode the file's footer + every column, forcing each
-- decoded value to NF so the per-page allocation work and any
-- deferred per-element decoding is captured by the benchmark.
--
-- Single-threaded. Returns a checksum that depends on every
-- value, so neither GHC nor criterion can drop the work.
readBack :: BS.ByteString -> Int
readBack bs = case PHL.decodeParquet PHL.defaultReadOptions bs of
  Left err -> error err
  Right pf ->
    let !sch    = PArrow.parquetFileArrowSchema pf
        !nRG    = PArrow.numRowGroups pf
        !nCols  = V.length (AT.arrowFields sch)
        !total  = sum
                    [ forceCol col
                    | rg <- [0 .. nRG - 1]
                    , c  <- [0 .. nCols - 1]
                    , let !fld = V.unsafeIndex (AT.arrowFields sch) c
                    , col <- case PArrow.readParquetColumn pf rg c fld of
                               Right cv -> [cv]
                               Left  _  -> []
                    ]
    in total

-- | Like 'readBack', but decodes column chunks in parallel
-- across the GHC thread pool — this is the apples-to-apples
-- analogue of pyarrow's @pq.read_table(use_threads=True)@.
--
-- Uses 'Control.Parallel.Strategies.parMap' (sparks) rather
-- than @forConcurrently@: the per-task work is on the order of
-- hundreds of microseconds, and async's @forkIO@-per-task
-- overhead eats most of the win at that granularity. Sparks
-- are essentially free to create and the runtime work-steals
-- onto idle capabilities.
-- | Parallel column-chunk decode — the apples-to-apples
-- analogue of pyarrow's @pq.read_table(use_threads=True)@.
--
-- Uses explicit 'forkIO' + 'MVar' rather than sparks: GHC
-- sparks can be skipped or evaluated serially if the runtime
-- doesn't push them onto idle capabilities, and at this work
-- granularity (4 columns × ~600 µs each) we lose meaningful
-- parallelism that way. Explicit threads guarantee one OS
-- thread per task as long as @+RTS -N>=ntasks@.
--
-- Compared to spark-based 'readBackParSpark' on a 4-core box:
--   read uncompressed:  spark 2.64 ms → fork 2.27 ms
--   read snappy:        spark 3.97 ms → fork 3.18 ms
--   read zstd:          spark 3.35 ms → fork 2.61 ms
readBackPar :: BS.ByteString -> Int
readBackPar bs = unsafePerformIO $
  case PHL.decodeParquet PHL.defaultReadOptions bs of
    Left err -> error err
    Right pf -> do
      let !sch    = PArrow.parquetFileArrowSchema pf
          !nRG    = PArrow.numRowGroups pf
          !flds   = AT.arrowFields sch
          !nCols  = V.length flds
          !work   =
            [ (rg, c)
            | rg <- [0 .. nRG - 1]
            , c  <- [0 .. nCols - 1]
            ]
          !nTasks = length work
      mvars <- replicateM nTasks newEmptyMVar
      forM_ (zip mvars work) $ \(mv, (rg, c)) -> forkIO $ do
        let !fld = V.unsafeIndex flds c
            !x  = case PArrow.readParquetColumn pf rg c fld of
                    Right cv -> forceCol cv
                    Left  _  -> 0
        putMVar mv x
      results <- mapM takeMVar mvars
      pure $! sum results

-- | Spark-based parallel reader; kept as a comparison point.
readBackParSpark :: BS.ByteString -> Int
readBackParSpark bs = case PHL.decodeParquet PHL.defaultReadOptions bs of
  Left err -> error err
  Right pf ->
    let !sch    = PArrow.parquetFileArrowSchema pf
        !nRG    = PArrow.numRowGroups pf
        !nCols  = V.length (AT.arrowFields sch)
        !work   =
          [ (rg, c)
          | rg <- [0 .. nRG - 1]
          , c  <- [0 .. nCols - 1]
          ]
        decodeOne (rg, c) =
          let !fld = V.unsafeIndex (AT.arrowFields sch) c
          in case PArrow.readParquetColumn pf rg c fld of
               Right cv -> forceCol cv
               Left  _  -> 0
    in sum (parMap rseq decodeOne work)

-- | Like 'readBackPar' but kept as an alias; used by older bench labels.
readBackForkIO :: BS.ByteString -> Int
readBackForkIO bs = unsafePerformIO $
  case PHL.decodeParquet PHL.defaultReadOptions bs of
    Left err -> error err
    Right pf -> do
      let !sch    = PArrow.parquetFileArrowSchema pf
          !nRG    = PArrow.numRowGroups pf
          !flds   = AT.arrowFields sch
          !nCols  = V.length flds
          !work   =
            [ (rg, c)
            | rg <- [0 .. nRG - 1]
            , c  <- [0 .. nCols - 1]
            ]
          !nTasks = length work
      mvars <- replicateM nTasks newEmptyMVar
      forM_ (zip mvars work) $ \(mv, (rg, c)) -> forkIO $ do
        let !fld = V.unsafeIndex flds c
            !x  = case PArrow.readParquetColumn pf rg c fld of
                    Right cv -> forceCol cv
                    Left  _  -> 0
        putMVar mv x
      results <- mapM takeMVar mvars
      pure $! sum results

-- | Force a 'ColumnArray' so the bench measures real decode
-- cost — and only that.
--
-- For primitive vectors ('ColInt32' / 'ColInt64' / 'ColFloat'
-- / 'ColDouble') the data lives in a strict 'ByteArray', so
-- the decode is fully done as soon as the vector constructor
-- is reached: 'VP.length' suffices to force the work.
-- @VP.foldl' (+)@ would also sum every value, which pyarrow's
-- @read_table@ does not do — including it would unfairly
-- penalise wireform.
--
-- For boxed vectors ('Bool' / 'Utf8' / 'Binary') the elements
-- can still be lazy thunks, so we walk the spine and force
-- each element to WHNF — which is what pyarrow's
-- @read_table@ effectively does (it materialises Arrow
-- buffers).
forceCol :: AC.ColumnArray -> Int
forceCol = \case
  AC.ColInt32  v -> VP.length v
  AC.ColInt64  v -> VP.length v
  AC.ColFloat  v -> VP.length v
  AC.ColDouble v -> VP.length v
  AC.ColBool   v -> V.foldl'  (\a !_ -> a + 1) 0 v
  AC.ColUtf8   v -> V.foldl'  (\a !_ -> a + 1) 0 v
  AC.ColBinary v -> V.foldl'  (\a !_ -> a + 1) 0 v
  c              -> AC.columnLength c

instance NFData ColumnData where
  rnf (ColInt32 v)     = v `seq` ()
  rnf (ColInt64 v)     = v `seq` ()
  rnf (ColFloat v)     = v `seq` ()
  rnf (ColDouble v)    = v `seq` ()
  rnf (ColBool v)      = v `seq` ()
  rnf (ColByteArray v) = v `deepseq` ()

main :: IO ()
main =
  let !dataset = mkDataset nRows
  in defaultMain
       [ env (pure dataset) $ \cols ->
           bgroup ("write " ++ show nRows ++ " rows x 4 cols")
             [ bench "uncompressed"   $ whnf (BS.length . writeFile_)        cols
             , bench "snappy"         $ whnf (BS.length . writeFileSnappy)  cols
             , bench "zstd"           $ whnf (BS.length . writeFileZstd)    cols
             ]
       , env (pure (writeFile_ dataset)) $ \bs ->
           bgroup ("read " ++ show nRows ++ " rows x 4 cols (uncompressed)")
             [ bench "decode"        $ nf readBack       bs
             , bench "decode-par"    $ nf readBackPar    bs
             ]
       , env (pure (writeFileSnappy dataset)) $ \bs ->
           bgroup ("read " ++ show nRows ++ " rows x 4 cols (snappy)")
             [ bench "decode"        $ nf readBack       bs
             , bench "decode-par"    $ nf readBackPar    bs
             ]
       , env (pure (writeFileZstd dataset)) $ \bs ->
           bgroup ("read " ++ show nRows ++ " rows x 4 cols (zstd)")
             [ bench "decode"        $ nf readBack       bs
             , bench "decode-par"    $ nf readBackPar    bs
             ]
       ]
