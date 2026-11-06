module Main (main) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad (unless)
import Criterion.Main (bench, bgroup, defaultMain, nf)
import Data.Bits ((.&.))
import Data.Int (Int32, Int64)
import Data.Vector.Primitive qualified as P
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Parquet.Delta qualified as Read
import Parquet.DeltaEncode qualified as Write


data Inputs = Inputs
  { ascending32 :: !(P.Vector Int32)
  , bounded32 :: !(P.Vector Int32)
  , full32 :: !(P.Vector Int32)
  , wrapping32 :: !(P.Vector Int32)
  , ascending64 :: !(P.Vector Int64)
  , bounded64 :: !(P.Vector Int64)
  , full64 :: !(P.Vector Int64)
  , wrapping64 :: !(P.Vector Int64)
  }
  deriving stock (Generic)
  deriving anyclass (NFData)


rowCount :: Int
rowCount = 65536


inputs :: Inputs
inputs =
  Inputs
    (P.generate rowCount fromIntegral)
    (P.generate rowCount (fromIntegral . bounded))
    (P.map fromIntegral (P.iterateN rowCount (\value -> value * 1664525 + 1013904223) (0x12345678 :: Word32)))
    (P.generate rowCount (\i -> if even i then minBound else maxBound))
    (P.generate rowCount fromIntegral)
    (P.generate rowCount (fromIntegral . bounded))
    (P.map fromIntegral (P.iterateN rowCount (\value -> value * 6364136223846793005 + 1442695040888963407) (0x123456789abcdef0 :: Word64)))
    (P.generate rowCount (\i -> if even i then minBound else maxBound))
  where
    bounded i = (fromIntegral i * 1103515245 + 12345) .&. 0xffff :: Word32


main :: IO ()
main = do
  values <- evaluate (force inputs)
  let cases32 = [("INT32 ascending", ascending32 values), ("INT32 bounded", bounded32 values), ("INT32 full", full32 values), ("INT32 wrapping", wrapping32 values)]
      cases64 = [("INT64 ascending", ascending64 values), ("INT64 bounded", bounded64 values), ("INT64 full", full64 values), ("INT64 wrapping", wrapping64 values)]
  mapM_
    ( \(name, column) -> do
        let encoded = Write.encodeDeltaBinaryPackedInt32 column
        decoded <- either fail (evaluate . force) (Read.decodeDeltaBinaryPackedInt32 rowCount encoded)
        unless (decoded == column) (fail (name ++ ": encoder values differ"))
    )
    cases32
  mapM_
    ( \(name, column) -> do
        let encoded = Write.encodeDeltaBinaryPackedInt64 column
        decoded <- either fail (evaluate . force) (Read.decodeDeltaBinaryPackedInt64 rowCount encoded)
        unless (decoded == column) (fail (name ++ ": encoder values differ"))
    )
    cases64
  defaultMain
    [ bgroup "delta encode" $
        map (\(name, column) -> bench name (nf Write.encodeDeltaBinaryPackedInt32 column)) cases32
          ++ map (\(name, column) -> bench name (nf Write.encodeDeltaBinaryPackedInt64 column)) cases64
    ]
