{-# LANGUAGE BangPatterns #-}
-- | Profiling-friendly entry point: write the throughput
-- dataset, then read it back N times in a tight loop. Run
-- with @+RTS -p -hT -s@ to get a cost-centre + alloc-by-type
-- + GC-stats report.
--
-- @
-- cabal build wireform-parquet:parquet-profile \\
--   --enable-profiling --profiling-detail=late
-- $(cabal list-bin wireform-parquet:parquet-profile) +RTS -p -s
-- @
module Main (main) where

import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad (replicateM_)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.IORef
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

import qualified Parquet.HighLevel as PHL
import qualified Parquet.Read as PR
import qualified Parquet.Arrow as PArrow
import qualified Parquet.Types as P
import qualified Arrow.Types as AT
import qualified Arrow.Column as AC
import Parquet.Write (ColumnData (..))

nRows :: Int
nRows = 100_000

nIters :: Int
nIters = 5

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

writeF :: V.Vector ColumnData -> BS.ByteString
writeF cols = PHL.encodeParquet PHL.defaultWriteOptions mkSchema [cols]

-- | Force every column.
readBack :: BS.ByteString -> Int
readBack bs = case PHL.decodeParquet PHL.defaultReadOptions bs of
  Left err -> error err
  Right pf ->
    let !sch    = PArrow.parquetFileArrowSchema pf
        !nRG    = PArrow.numRowGroups pf
        !nCols  = V.length (AT.arrowFields sch)
        loopRG !rg !acc
          | rg >= nRG = acc
          | otherwise =
              let !acc' = loopCol rg 0 acc
              in  loopRG (rg + 1) acc'
        loopCol !rg !c !acc
          | c >= nCols = acc
          | otherwise =
              let !fld = V.unsafeIndex (AT.arrowFields sch) c
              in case PArrow.readParquetColumn pf rg c fld of
                   Right cv ->
                     -- Force length so the decode actually runs.
                     let !n = AC.columnLength cv
                     in  loopCol rg (c + 1) (acc + n)
                   Left  _  -> loopCol rg (c + 1) acc
    in loopRG 0 0

instance NFData ColumnData where
  rnf c = c `seq` ()

main :: IO ()
main = do
  let !dataset = mkDataset nRows
      !bs      = writeF dataset
  putStrLn $ "wrote " ++ show (BS.length bs) ++ " bytes"
  ref <- newIORef (0 :: Int)
  replicateM_ nIters $ do
    let !n = readBack bs
    modifyIORef' ref (+ n)
  total <- readIORef ref
  putStrLn $ show nIters ++ " iters; sum-of-column-lengths = " ++ show total
  -- Reference dataset to suppress unused warning.
  dataset `deepseq` pure ()
