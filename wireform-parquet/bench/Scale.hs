{-# LANGUAGE BangPatterns #-}
-- | Scaling benchmarks: how does our perf change with row
-- count, column count, and column type mix? Helps spot
-- per-call vs per-byte overhead.
module Main (main) where

import Control.DeepSeq (NFData (..), deepseq)
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

import Criterion.Main (defaultMain, bench, bgroup, env, nf, whnf)

import qualified Parquet.HighLevel as PHL
import qualified Parquet.Read as PR
import qualified Parquet.Arrow as PArrow
import qualified Parquet.Types as P
import qualified Arrow.Types as AT
import qualified Arrow.Column as AC
import Parquet.Write (ColumnData (..))

-- ----------------------------------------------------------------------
-- Schema variants

-- | Mixed schema: int64, double, utf8, bool (matches Throughput.hs).
mixedSchema :: Int -> V.Vector P.SchemaElement
mixedSchema n = V.fromList
  [ P.SchemaElement "schema" Nothing Nothing (Just (fromIntegral n)) Nothing Nothing Nothing
  , P.SchemaElement "id"     (Just P.Required) (Just P.PTInt64)     Nothing Nothing Nothing Nothing
  , P.SchemaElement "score"  (Just P.Required) (Just P.PTDouble)    Nothing Nothing Nothing Nothing
  , P.SchemaElement "name"   (Just P.Required) (Just P.PTByteArray) Nothing (Just P.CTUtf8) Nothing Nothing
  , P.SchemaElement "active" (Just P.Required) (Just P.PTBoolean)   Nothing Nothing Nothing Nothing
  ]

mixedDataset :: Int -> V.Vector ColumnData
mixedDataset n = V.fromList
  [ ColInt64  (VP.generate n fromIntegral)
  , ColDouble (VP.generate n (\i -> fromIntegral i * 0.5))
  , ColByteArray $ V.generate n $ \i ->
      TE.encodeUtf8 (T.pack ("name_" ++ show (i `rem` 1000)))
  , ColBool   (V.generate n (\i -> i `rem` 2 == 0))
  ]

-- | All-Int64 schema with @nCols@ columns; useful for stressing
-- the per-column dispatch overhead.
intSchema :: Int -> V.Vector P.SchemaElement
intSchema nCols = V.fromList $
  P.SchemaElement "schema" Nothing Nothing (Just (fromIntegral nCols)) Nothing Nothing Nothing
  : [ P.SchemaElement (T.pack ("c" ++ show i))
                      (Just P.Required)
                      (Just P.PTInt64)
                      Nothing Nothing Nothing Nothing
    | i <- [0 .. nCols - 1]
    ]

intDataset :: Int -> Int -> V.Vector ColumnData
intDataset nCols nRows = V.generate nCols $ \c ->
  ColInt64 (VP.generate nRows (\i -> fromIntegral (c * nRows + i)))

-- ----------------------------------------------------------------------
-- Helpers

forceCol :: AC.ColumnArray -> Int
forceCol = \case
  AC.ColInt32  v -> VP.length v
  AC.ColInt64  v -> VP.length v
  AC.ColFloat  v -> VP.length v
  AC.ColDouble v -> VP.length v
  AC.ColBool   v -> V.foldl' (\a !_ -> a + 1) 0 v
  AC.ColUtf8   v -> V.foldl' (\a !_ -> a + 1) 0 v
  AC.ColBinary v -> V.foldl' (\a !_ -> a + 1) 0 v
  c              -> AC.columnLength c

writeBs :: V.Vector P.SchemaElement -> V.Vector ColumnData -> BS.ByteString
writeBs sch cols =
  PHL.encodeParquet
    PHL.defaultWriteOptions { PHL.writeCompression = P.Uncompressed }
    sch [cols]

readBack :: BS.ByteString -> Int
readBack bs = case PHL.decodeParquet PHL.defaultReadOptions bs of
  Left err -> error err
  Right pf ->
    let !sch    = PArrow.parquetFileArrowSchema pf
        !nRG    = PArrow.numRowGroups pf
        !nCols  = V.length (AT.arrowFields sch)
    in sum
         [ forceCol col
         | rg <- [0 .. nRG - 1]
         , c  <- [0 .. nCols - 1]
         , let !fld = V.unsafeIndex (AT.arrowFields sch) c
         , col <- case PArrow.readParquetColumn pf rg c fld of
                    Right cv -> [cv]
                    Left  _  -> []
         ]

instance NFData ColumnData where
  rnf (ColInt32 v)     = v `seq` ()
  rnf (ColInt64 v)     = v `seq` ()
  rnf (ColFloat v)     = v `seq` ()
  rnf (ColDouble v)    = v `seq` ()
  rnf (ColBool v)      = v `seq` ()
  rnf (ColByteArray v) = v `deepseq` ()

main :: IO ()
main = defaultMain
  [ bgroup "mixed (Int64+Double+Utf8+Bool)"
      [ env (pure (mixedDataset n)) $ \d -> bgroup (show n ++ " rows")
        [ bench "write" $ whnf (BS.length . writeBs (mixedSchema 4)) d
        , env (pure (writeBs (mixedSchema 4) d)) $ \bs ->
            bench "read"  $ nf readBack bs
        ]
      | n <- [10_000, 100_000, 1_000_000]
      ]
  , bgroup "wide-int64 (all Int64 cols)"
      [ env (pure (intDataset nC nR)) $ \d -> bgroup
          (show nC ++ " cols x " ++ show nR ++ " rows")
        [ bench "write" $ whnf (BS.length . writeBs (intSchema nC)) d
        , env (pure (writeBs (intSchema nC) d)) $ \bs ->
            bench "read"  $ nf readBack bs
        ]
      | (nC, nR) <- [ (4, 100_000), (16, 25_000), (64, 6_250), (256, 1_563) ]
        -- All have ~400k total values, so changes between the
        -- shapes show per-column overhead, not raw data work.
      ]
  ]
