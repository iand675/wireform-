{-# LANGUAGE BangPatterns #-}
-- | Dictionary-build microbench. Specific to ColInt64 with a
-- known small distinct-value count (1000). The previous
-- buildDictionary was O(n²) in the column length plus
-- O(n × |uniques|) for the index assignment.
module Main (main) where

import Control.DeepSeq (NFData (..))
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import Data.Int (Int64)

import Criterion.Main (defaultMain, bench, bgroup, env, nf)

import qualified Parquet.Write as PW

instance NFData PW.ColumnData where
  rnf c = c `seq` ()

mkColumn :: Int -> Int -> PW.ColumnData
mkColumn n distinct =
  PW.ColInt64 (VP.generate n (\i -> fromIntegral (i `rem` distinct)))

main :: IO ()
main = defaultMain
  [ env (pure (mkColumn n distinct)) $ \cd ->
      bgroup (show n ++ " rows / " ++ show distinct ++ " distinct")
        [ bench "buildDictionary" $
            nf (\c -> let d = PW.buildDictionary c
                      in VP.length (PW.dictIndices d)) cd
        ]
  | (n, distinct) <-
      [ (10_000,   100)
      , (10_000,  1000)
      , (100_000,  100)
      , (100_000, 1000)
      ]
  ]
