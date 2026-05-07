{-# LANGUAGE BangPatterns #-}
-- | Per-column read-time decomposition. Same dataset shape as
-- the throughput benchmark, but reads each column in isolation
-- so we can see which one dominates.
module Main (main) where

import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.IORef
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import qualified Parquet.HighLevel as PHL
import qualified Parquet.Read as PR
import qualified Parquet.Arrow as PArrow
import qualified Parquet.Types as P
import qualified Arrow.Types as AT
import qualified Arrow.Column as AC
import qualified Arrow.View as AV
import Parquet.Write (ColumnData (..))

-- | Force every element of a 'ColumnArray' so the per-column
-- timing actually measures decode work, not just thunk
-- allocation. (V.map and friends are lazy in their elements
-- by default; without this the deferred Text construction
-- never runs and the bench misreports.)
forceCol :: AC.ColumnArray -> Int
forceCol = \case
  AC.ColInt32  v -> VS.length v
  AC.ColInt64  v -> VS.length v
  AC.ColFloat  v -> VS.length v
  AC.ColDouble v -> VS.length v
  AC.ColBool   v -> VU.length v
  AC.ColUtf8   v -> AV.vLength v
  AC.ColBinary v -> AV.vLength v
  c              -> AC.columnLength c

nRows :: Int
nRows = 100_000

mkDataset :: Int -> V.Vector ColumnData
mkDataset n = V.fromList
  [ ColInt64  (VS.generate n fromIntegral)
  , ColDouble (VS.generate n (\i -> fromIntegral i * 0.5))
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
writeF cols = PHL.encodeParquet
  PHL.defaultWriteOptions { PHL.writeCompression = P.Uncompressed }
  mkSchema [cols]

writeSnappyF :: V.Vector ColumnData -> BS.ByteString
writeSnappyF cols = PHL.encodeParquet
  PHL.defaultWriteOptions { PHL.writeCompression = P.Snappy }
  mkSchema [cols]

time :: String -> Int -> IO Int -> IO ()
time label iters act = do
  -- Warm up
  _ <- act
  t0 <- getCurrentTime
  let loop !k !acc
        | k >= iters = pure acc
        | otherwise  = do !x <- act; loop (k + 1) (acc + x)
  !n <- loop 0 0
  t1 <- getCurrentTime
  let !secs    = realToFrac (diffUTCTime t1 t0) :: Double
      !msEach  = secs * 1000 / fromIntegral iters
      !rpsEach = fromIntegral nRows / (secs / fromIntegral iters) :: Double
  putStrLn $ "  " ++ label ++ ": " ++ show msEach ++ " ms (" ++ show n ++ " sum, "
              ++ show (round rpsEach :: Int) ++ " rows/s)"

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
                 Right cv -> forceCol cv
                 Left  _  -> 0
      _ <- pure (rd 0)
      putStrLn ""
      putStrLn "=== Footer parse (decodeParquet only, 5x) ==="
      time "decodeParquet" 5 $ do
        case PHL.decodeParquet PHL.defaultReadOptions bs of
          Left e -> error e
          Right pf' ->
            let !s = PArrow.numRowGroups pf'
                  + V.length (AT.arrowFields (PArrow.parquetFileArrowSchema pf'))
            in pure s
      putStrLn ""
      putStrLn "=== Single-pass per-column reads (100k rows) ==="
      time "id     (Int64)" 1     (pure (rd 0))
      time "score  (Double)" 1    (pure (rd 1))
      time "name   (Utf8 dict)" 1 (pure (rd 2))
      time "active (Bool)" 1      (pure (rd 3))
      putStrLn ""
      putStrLn "=== Repeated reads (5x each, footer pre-parsed) ==="
      time "id     (Int64)" 5 (pure (rd 0))
      time "score  (Double)" 5 (pure (rd 1))
      time "name   (Utf8)" 5 (pure (rd 2))
      time "active (Bool)" 5 (pure (rd 3))
      putStrLn ""
      putStrLn "=== Stage breakdown for one Int64 column (100x, fresh bs to defeat CSE) ==="
      -- Pull the column chunk bytes once; time downstream stages.
      case PR.columnChunkSlice pf 0 0 of
        Right chunkBs -> do
          let !codec = P.Uncompressed
          time "  columnChunkSlice (just slice)" 100 $
            case PR.columnChunkSlice pf 0 0 of
              Right b -> pure (BS.length b)
              Left _  -> pure 0
          time "  readGenericInt64ColumnChunk (Uncompressed)" 100 $
            case PR.readGenericInt64ColumnChunk codec chunkBs of
              Right v -> pure (VP.length v)
              Left _  -> pure 0
        Left _ -> pure ()

      -- Repeat with snappy data so we can isolate decompression cost.
      let !bsSnappy = writeSnappyF dataset
      case PHL.decodeParquet PHL.defaultReadOptions bsSnappy of
        Right pfS -> case PR.columnChunkSlice pfS 0 0 of
          Right snappyChunk -> do
            putStrLn ""
            putStrLn "=== Snappy stage breakdown for one Int64 column ==="
            time "  readGenericInt64ColumnChunk (Snappy)" 100 $
              case PR.readGenericInt64ColumnChunk P.Snappy snappyChunk of
                Right v -> pure (VP.length v)
                Left _  -> pure 0
          Left _ -> pure ()
        Left _ -> pure ()
      putStrLn ""
      putStrLn "=== End-to-end (decodeParquet + 4 cols, fresh bs each iter) ==="
      -- Make 5 distinct ByteStrings so GHC can't CSE the whole
      -- decoded result across iterations.
      let !bsList = take 5 (iterate (\b -> BS.copy b) bs)
      ref <- newIORef (0 :: Int)
      let one !b = case PHL.decodeParquet PHL.defaultReadOptions b of
            Left e   -> error e
            Right pf' ->
              let !sch'  = PArrow.parquetFileArrowSchema pf'
                  !flds' = AT.arrowFields sch'
                  rd' c =
                    let !fld = V.unsafeIndex flds' c
                    in case PArrow.readParquetColumn pf' 0 c fld of
                         Right cv -> forceCol cv
                         Left  _  -> 0
              in rd' 0 + rd' 1 + rd' 2 + rd' 3
      -- warm up
      modifyIORef' ref (+ one bs)
      t0 <- getCurrentTime
      mapM_ (\b -> modifyIORef' ref (+ one b)) bsList
      t1 <- getCurrentTime
      total <- readIORef ref
      let !secs = realToFrac (diffUTCTime t1 t0) :: Double
          !msEach = secs * 1000 / fromIntegral (length bsList)
      putStrLn $ "  full read: " ++ show msEach ++ " ms (sum=" ++ show total ++ ")"
