module Main (main) where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Control.Monad (unless)
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, whnf)
import Data.Bits (shiftL, (.|.))
import Data.ByteString qualified as BS
import Data.List (isPrefixOf)
import Data.Vector.Primitive qualified as P
import Data.Word (Word32, Word64)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Parquet.Alp qualified as Alp
import System.Environment (getArgs, getEnv)
import System.FilePath ((</>))


data Fixture = Fixture
  { fixtureName :: !String
  , fixtureWidth :: {-# UNPACK #-} !Int
  , fixtureCount :: {-# UNPACK #-} !Int
  , fixtureEncoded :: !BS.ByteString
  }


instance NFData Fixture where
  rnf (Fixture name width count encoded) = rnf name `seq` rnf width `seq` rnf count `seq` rnf encoded


decode32 :: Int -> BS.ByteString -> P.Vector Float
decode32 count = either error id . Alp.decodeAlpFloat count


decode64 :: Int -> BS.ByteString -> P.Vector Double
decode64 count = either error id . Alp.decodeAlpDouble count


loadFixture :: FilePath -> String -> IO Fixture
loadFixture root line = case splitTab line of
  [stem, widthText, countText, name] -> do
    let width = read widthText
        count = read countText
    encoded <- BS.readFile (root </> stem ++ ".alp")
    expected <- BS.readFile (root </> stem ++ ".plain")
    unless (BS.length expected == count * width) (fail "ALP benchmark oracle size differs")
    if width == 4
      then
        unless
          (P.map castFloatToWord32 (decode32 count encoded) == P.generate count (read32 expected))
          (fail (name ++ ": FLOAT IEEE bits differ from independent PLAIN oracle"))
      else do
        unless (width == 8) (fail "ALP benchmark physical width differs")
        unless
          (P.map castDoubleToWord64 (decode64 count encoded) == P.generate count (read64 expected))
          (fail (name ++ ": DOUBLE IEEE bits differ from independent PLAIN oracle"))
    evaluate $ Fixture name width count encoded
  _ -> fail "ALP benchmark fixture plan must have four tab-separated fields"
  where
    splitTab input = case break (== '\t') input of
      (field, []) -> [field]
      (field, _ : rest) -> field : splitTab rest


read32 :: BS.ByteString -> Int -> Word32
read32 bytes row = foldl' (\value lane -> value .|. (fromIntegral (BS.index bytes (row * 4 + lane)) `shiftL` (lane * 8))) 0 [0 .. 3]


read64 :: BS.ByteString -> Int -> Word64
read64 bytes row = foldl' (\value lane -> value .|. (fromIntegral (BS.index bytes (row * 8 + lane)) `shiftL` (lane * 8))) 0 [0 .. 7]


benchmark :: Fixture -> Benchmark
benchmark fixture =
  bgroup
    (fixtureName fixture)
    [ if fixtureWidth fixture == 4
        then bench "decode" (whnf (decode32 (fixtureCount fixture)) (fixtureEncoded fixture))
        else bench "decode" (whnf (decode64 (fixtureCount fixture)) (fixtureEncoded fixture))
    ]


main :: IO ()
main = do
  root <- getEnv "WIREFORM_PARQUET_ALP_FIXTURES"
  plan <- readFile (root </> "cases.tsv")
  fixtures <- mapM (loadFixture root) (lines plan)
  unless
    (length fixtures == 30 && all (\f -> "FLOAT " `isPrefixOf` fixtureName f || "DOUBLE " `isPrefixOf` fixtureName f) fixtures)
    (fail "ALP benchmark fixture set differs")
  args <- getArgs
  case args of
    ["--check"] -> putStrLn "30 ALP pages match independent PLAIN IEEE bit oracles"
    _ -> defaultMain [bgroup "ALP" (map benchmark fixtures)]
