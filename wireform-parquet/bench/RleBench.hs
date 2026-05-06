{-# LANGUAGE BangPatterns #-}
-- | Microbenchmarks for the hybrid-RLE / bit-packed decoder
-- and a real-world dict-encoded column read. The dict path
-- is the typical shape for files written by pyarrow / parquet-mr
-- (low-cardinality string columns get RLE_DICTIONARY by default).
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Bits ((.|.), shiftL, shiftR)
import Data.Word (Word8, Word32)
import qualified Data.Vector.Primitive as VP

import Criterion.Main (defaultMain, bench, bgroup, env, nf, whnf)

import qualified Parquet.RLE as RLE

-- | Build a hybrid-RLE-encoded byte stream of @n@ values
-- using @bw@-bit indices, all bit-packed (worst case for the
-- decoder). Layout matches what 'decodeHybridRleUnsigned32'
-- accepts: leading bit width is NOT included; caller passes
-- @bw@ separately.
mkBitPackedStream :: Int -> Int -> BS.ByteString
mkBitPackedStream bw n =
  -- One header per chunk: header = (numGroups << 1) | 1
  -- The simplest single-header form puts everything in one run.
  let !numGroups = (n + 7) `quot` 8
      !header   = fromIntegral (numGroups `shiftL` 1) .|. 1 :: Word32
      headerBs  = encodeULEB header
      -- Body: numGroups groups of bw bytes, all zero-filled (the
      -- decode work is the same — we measure the unpack inner
      -- loop, not the value distribution).
      bodyBs    = BS.replicate (numGroups * bw) 0
  in  headerBs <> bodyBs

encodeULEB :: Word32 -> BS.ByteString
encodeULEB w0 = BL.toStrict $ B.toLazyByteString $ go w0
  where
    go w
      | w < 0x80  = B.word8 (fromIntegral w)
      | otherwise = B.word8 (fromIntegral (w .|. 0x80))
                      <> go (w `shiftR` 7)

main :: IO ()
main = defaultMain
  [ bgroup "decodeHybridRleUnsigned32 (all bit-packed)"
      [ env (pure (mkBitPackedStream bw n)) $ \bs ->
          bench (show n ++ " vals, bw=" ++ show bw) $
            nf (\b -> case RLE.decodeHybridRleUnsigned32 bw n b of
                        Right v -> VP.length v
                        Left _  -> 0) bs
      | (bw, n) <-
          [ (10, 100_000)   -- typical dict-index width for ≤ 1024 distinct values
          , (4,  100_000)
          , (1,  100_000)
          , (8,  100_000)
          ]
      ]
  ]
