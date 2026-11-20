module Main (main) where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Control.Monad (unless)
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, env, whnf)
import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.Vector.Primitive qualified as P
import Data.Word (Word32, Word64)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)
import Parquet.ByteStreamSplit qualified as Write
import Parquet.Read qualified as Read
import System.Environment (getArgs)


data Fixture a = Fixture
  { fixtureValues :: !(P.Vector a)
  , fixtureEncoded :: !BS.ByteString
  }


instance NFData (Fixture a) where
  rnf (Fixture values encoded) = rnf values `seq` rnf encoded


counts :: [Int]
counts = [17, 1024, 65536]


word32At :: Int -> Word32
word32At i = case i of
  0 -> 0
  1 -> 0x80000000
  2 -> 0x7f800000
  3 -> 0xff800000
  4 -> 0x7fc12345
  5 -> 0x7f812345
  6 -> 1
  7 -> 0x7f7fffff
  _ -> fromIntegral i * 1664525 + 1013904223


word64At :: Int -> Word64
word64At i = case i of
  0 -> 0
  1 -> 0x8000000000000000
  2 -> 0x7ff0000000000000
  3 -> 0xfff0000000000000
  4 -> 0x7ff8123456789abc
  5 -> 0x7ff0123456789abc
  6 -> 1
  7 -> 0x7fefffffffffffff
  _ -> fromIntegral i * 6364136223846793005 + 1442695040888963407


-- Independent specification model: byte lane first, then value index.
golden32 :: Int -> BS.ByteString
golden32 n = BS.pack (concatMap (\lane -> map (\i -> fromIntegral (word32At i `shiftR` (lane * 8))) [0 .. n - 1]) [0 .. 3])


golden64 :: Int -> BS.ByteString
golden64 n = BS.pack (concatMap (\lane -> map (\i -> fromIntegral (word64At i `shiftR` (lane * 8))) [0 .. n - 1]) [0 .. 7])


decode32 :: Int -> BS.ByteString -> P.Vector Float
decode32 n = either error id . Read.decodeByteStreamSplitFloat n


decode64 :: Int -> BS.ByteString -> P.Vector Double
decode64 n = either error id . Read.decodeByteStreamSplitDouble n


fixture32 :: Int -> IO (Fixture Float)
fixture32 n = do
  values <- evaluate (P.generate n (castWord32ToFloat . word32At))
  encoded <- evaluate (Write.encodeByteStreamSplitFloat values)
  unless (encoded == golden32 n) (fail "FLOAT byte-stream-split bytes differ from specification model")
  unless (P.map castFloatToWord32 (decode32 n encoded) == P.generate n word32At) (fail "FLOAT byte-stream-split bits differ")
  pure (Fixture values encoded)


fixture64 :: Int -> IO (Fixture Double)
fixture64 n = do
  values <- evaluate (P.generate n (castWord64ToDouble . word64At))
  encoded <- evaluate (Write.encodeByteStreamSplitDouble values)
  unless (encoded == golden64 n) (fail "DOUBLE byte-stream-split bytes differ from specification model")
  unless (P.map castDoubleToWord64 (decode64 n encoded) == P.generate n word64At) (fail "DOUBLE byte-stream-split bits differ")
  pure (Fixture values encoded)


case32 :: Int -> Benchmark
case32 n = env (fixture32 n) $ \fixture ->
  bgroup
    ("FLOAT " ++ show n ++ " values")
    [ bench "encode" (whnf Write.encodeByteStreamSplitFloat (fixtureValues fixture))
    , bench "decode" (whnf (decode32 n) (fixtureEncoded fixture))
    ]


case64 :: Int -> Benchmark
case64 n = env (fixture64 n) $ \fixture ->
  bgroup
    ("DOUBLE " ++ show n ++ " values")
    [ bench "encode" (whnf Write.encodeByteStreamSplitDouble (fixtureValues fixture))
    , bench "decode" (whnf (decode64 n) (fixtureEncoded fixture))
    ]


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--check"] -> do
      mapM_ fixture32 counts
      mapM_ fixture64 counts
      putStrLn "6 BYTE_STREAM_SPLIT fixtures match independent encoded-byte and IEEE bit models"
    _ -> defaultMain [bgroup "byte stream split" (map case32 counts ++ map case64 counts)]
