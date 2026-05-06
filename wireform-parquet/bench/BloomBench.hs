{-# LANGUAGE BangPatterns #-}
-- | Bloom-filter build microbench.
--
-- Compares the old per-value 'sbbfInsert' shape (which copied
-- the entire bitset on every insert) with the new bulk
-- 'buildSbbfFromHashes' shape (one allocation, one ST pass).
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)

import Criterion.Main (defaultMain, bench, bgroup, nf)

import qualified Parquet.BloomFilter as Bloom
import qualified Wireform.Hash as WHash

i64LE :: Int64 -> BS.ByteString
i64LE v = BL.toStrict (B.toLazyByteString (B.int64LE v))

mkHashes :: Int -> VU.Vector Word64
mkHashes n =
  VU.generate n (\i -> WHash.xxh64 0 (i64LE (fromIntegral i)))

oldBuild :: Int -> VU.Vector Word64 -> Bloom.Sbbf
oldBuild nBytes hashes =
  let !empty0 = Bloom.newSbbf nBytes
  in VU.foldl' (\acc h -> Bloom.sbbfInsertHash h acc) empty0 hashes

newBuild :: Int -> VU.Vector Word64 -> Bloom.Sbbf
newBuild = Bloom.buildSbbfFromHashes

main :: IO ()
main = defaultMain
  [ bgroup (show n ++ " inserts")
      [ bench "old (sbbfInsertHash fold)" $
          nf (Bloom.sbbfNumBytes . oldBuild nBytes) hs
      , bench "new (buildSbbfFromHashes)" $
          nf (Bloom.sbbfNumBytes . newBuild nBytes) hs
      ]
  | n <- [10_000, 100_000]
  , let !hs = mkHashes n
  , let !nBytes = Bloom.optimalNumBytes (max 1 n) 0.01
  ]
