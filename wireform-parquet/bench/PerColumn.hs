{-# LANGUAGE BangPatterns #-}
-- | Per-column read-time decomposition. Same dataset shape as
-- the throughput benchmark, but reads each column in isolation
-- so we can see which one dominates.
module Main (main) where

import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import qualified Parquet.HighLevel as PHL
import qualified Parquet.Read as PR
import qualified Parquet.Arrow as PArrow
import qualified Parquet.Types as P
import qualified Arrow.Types as AT
import qualified Arrow.Column as AC
import Parquet.Write (ColumnData (..))

nRows :: Int
nRows = 100_000

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

time :: String -> IO Int -> IO ()
time label act = do
  t0 <- getCurrentTime
  !n <- act
  t1 <- getCurrentTime
  let !ms = realToFrac (diffUTCTime t1 t0) * 1000 :: Double
      !rps = (fromIntegral nRows / realToFrac (diffUTCTime t1 t0)) :: Double
  putStrLn $ "  " ++ label ++ ": " ++ show ms ++ " ms (" ++ show n ++ " values, "
              ++ show (round rps :: Int) ++ " rows/s)"

main :: IO ()
main = do
  let !dataset = mkDataset nRows
      !bs      = writeF dataset
  putStrLn $ "wrote " ++ show (BS.length bs) ++ " bytes"
  case PHL.decodeParquet PHL.defaultReadOptions bs of
    Left e -> error e
    Right pf -> do
      let !sch    = PArrow.parquetFileArrowSchema pf
          !flds   = AT.arrowFields sch
          rd c =
            let !fld = V.unsafeIndex flds c
            in case PArrow.readParquetColumn pf 0 c fld of
                 Right cv -> AC.columnLength cv
                 Left  _  -> 0
      -- Warm up
      _ <- pure (rd 0)
      putStrLn ""
      putStrLn "=== Single-pass per-column reads (100k rows) ==="
      time "id     (Int64)"      (pure (rd 0))
      time "score  (Double)"     (pure (rd 1))
      time "name   (Utf8 dict)"  (pure (rd 2))
      time "active (Bool)"       (pure (rd 3))
      putStrLn ""
      putStrLn "=== Repeated reads (5x each) ==="
      let bench label c =
            time (label ++ " 5x") $ do
              let !s = rd c + rd c + rd c + rd c + rd c
              pure s
      bench "id    " 0
      bench "score " 1
      bench "name  " 2
      bench "active" 3
