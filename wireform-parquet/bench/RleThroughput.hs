module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (unless)
import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.Bits (shiftL, (.&.))
import Data.ByteString qualified as BS
import Data.Int (Int32)
import Data.Vector.Primitive qualified as P
import Data.Word (Word32, Word64)
import Parquet.LevelsEncode qualified as Levels
import Parquet.Read qualified as Read
import System.Environment (getArgs)


rowCount :: Int
rowCount = 65536


data Input = Input
  { inputName :: !String
  , inputWidth :: {-# UNPACK #-} !Int
  , inputValues :: !(P.Vector Int32)
  }


inputs :: [Input]
inputs = concatMap forWidth [0, 1, 2, 4, 8, 9, 16, 24, 31, 32]
  where
    forWidth width =
      let mask = fromIntegral (((1 :: Word64) `shiftL` width) - 1) :: Word32
          sample :: String -> (Word32 -> Word32) -> Input
          sample label value = Input ("width " ++ show width ++ "/" ++ label ++ " 65536 values") width (P.generate rowCount (\i -> fromIntegral (value (fromIntegral i) .&. mask)))
          constant = sample "constant" (const mask)
      in if width == 0
           then [constant]
           else
             [ constant
             , sample "cycle" id
             , sample "random" (\i -> i * 1664525 + 1013904223)
             , sample "blocks8" (`quot` 8)
             , sample "blocks24" (`quot` 24)
             , sample "blocks64" (`quot` 64)
             , sample "prefix" (\i -> if i == fromIntegral rowCount - 1 then mask else 0)
             , sample "misaligned64" (\i -> if i < 3 then i else (i - 3) `quot` 64)
             ]


main :: IO ()
main = do
  mapM_
    ( \sample -> do
        let values = inputValues sample
            width = inputWidth sample
        _ <- evaluate (P.foldl' (+) 0 values)
        let encoded = Levels.encodeRLEHybrid width values
        unless
          (Read.decodeDictionaryIndices rowCount (BS.cons (fromIntegral width) encoded) == Right values)
          (fail (inputName sample ++ ": hybrid round trip failed"))
    )
    inputs
  args <- getArgs
  case args of
    ["--sizes"] -> mapM_ (\sample -> putStrLn ("hybrid-size " ++ inputName sample ++ ": " ++ show (BS.length (Levels.encodeRLEHybrid (inputWidth sample) (inputValues sample))))) inputs
    _ -> defaultMain [bgroup "hybrid encode" (map (\sample -> bench (inputName sample) (whnf (Levels.encodeRLEHybrid (inputWidth sample)) (inputValues sample))) inputs)]
