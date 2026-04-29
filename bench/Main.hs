{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import System.CPUTime

import Proto.Wire (Tag (..))
import Proto.Wire.Encode
import Proto.Wire.Decode
import Proto.Encode
import Proto.Decode
import qualified Proto.SizedBuilder as SB

main :: IO ()
main = do
  putStrLn "wireform benchmarks"
  putStrLn (replicate 60 '=')

  benchVarintEncode
  benchVarintDecode
  benchMessageEncode
  benchMessageDecode
  benchPackedEncode
  benchPackedDecode
  benchSizeCalculation
  benchSizedBuilderEncode

benchVarintEncode :: IO ()
benchVarintEncode = do
  let n = 100000 :: Int
  putStrLn "\nVarint encode (100k iterations):"

  t1 <- getCPUTime
  let !bs = BL.toStrict $ B.toLazyByteString $ go n mempty
  t2 <- getCPUTime

  putStrLn $ "  Time: " <> show ((t2 - t1) `div` 1000000000) <> " ms"
  putStrLn $ "  Output size: " <> show (BS.length bs) <> " bytes"
  where
    go 0 !acc = acc
    go !i !acc = go (i - 1) (acc <> putVarint (fromIntegral i))

benchVarintDecode :: IO ()
benchVarintDecode = do
  let n = 100000 :: Int
      encoded = BL.toStrict $ B.toLazyByteString $
        foldMap (\i -> putVarint (fromIntegral (i :: Int))) [1..n]

  putStrLn "\nVarint decode (100k varints):"

  t1 <- getCPUTime
  let !count = countVarints encoded :: Int
  t2 <- getCPUTime

  putStrLn $ "  Time: " <> show ((t2 - t1) `div` 1000000000) <> " ms"
  putStrLn $ "  Decoded: " <> show count <> " varints"
  where
    countVarints bs = go 0 0
      where
        len = BS.length bs
        go !count !off
          | off >= len = count
          | otherwise = case runDecoder' getVarint bs off of
              DecodeOK _ off' -> go (count + 1) off'
              DecodeFail _    -> count

benchMessageEncode :: IO ()
benchMessageEncode = do
  let n = 10000 :: Int

  putStrLn "\nMessage encode (10k iterations):"

  t1 <- getCPUTime
  let !totalSize = go n 0
  t2 <- getCPUTime

  putStrLn $ "  Time: " <> show ((t2 - t1) `div` 1000000000) <> " ms"
  putStrLn $ "  Total bytes: " <> show totalSize
  where
    msg = BenchMsg 42 "hello world benchmark" True
    go 0 !acc = acc
    go !i !acc = go (i - 1) (acc + BS.length (encodeMessage msg))

benchMessageDecode :: IO ()
benchMessageDecode = do
  let n = 10000 :: Int

  putStrLn "\nMessage decode (10k iterations):"

  t1 <- getCPUTime
  let !count = go encoded n (0 :: Int)
  t2 <- getCPUTime

  putStrLn $ "  Time: " <> show ((t2 - t1) `div` 1000000000) <> " ms"
  putStrLn $ "  Successful decodes: " <> show count
  where
    encoded = encodeMessage (BenchMsg 42 "hello world benchmark" True)
    go _ 0 !acc = acc
    go enc !i !acc = case decodeMessage enc :: Either DecodeError BenchMsg of
      Right _ -> go enc (i - 1) (acc + 1)
      Left _  -> go enc (i - 1) acc

benchPackedEncode :: IO ()
benchPackedEncode = do
  let vals = VU.fromList [1..10000 :: Word64]

  putStrLn "\nPacked varint encode (10k values):"

  t1 <- getCPUTime
  let !bs = BL.toStrict $ B.toLazyByteString $ encodePackedWord64 1 vals
  t2 <- getCPUTime

  putStrLn $ "  Time: " <> show ((t2 - t1) `div` 1000000000) <> " ms"
  putStrLn $ "  Output size: " <> show (BS.length bs) <> " bytes"

benchPackedDecode :: IO ()
benchPackedDecode = do
  let vals = VU.fromList [1..10000 :: Word64]
      encoded = BL.toStrict $ B.toLazyByteString $ encodePackedWord64 1 vals

  putStrLn "\nPacked varint decode (10k values):"

  t1 <- getCPUTime
  case runDecoder (getTag >> decodePackedVarint) encoded of
    Right !decoded -> do
      t2 <- getCPUTime
      putStrLn $ "  Time: " <> show ((t2 - t1) `div` 1000000000) <> " ms"
      putStrLn $ "  Decoded: " <> show (VU.length decoded) <> " values"
    Left e -> putStrLn $ "  Error: " <> show e

benchSizeCalculation :: IO ()
benchSizeCalculation = do
  let n = 100000 :: Int

  putStrLn "\nSize calculation (100k iterations):"

  t1 <- getCPUTime
  let !totalSize = go n 0
  t2 <- getCPUTime

  putStrLn $ "  Time: " <> show ((t2 - t1) `div` 1000000000) <> " ms"
  putStrLn $ "  Sum of sizes: " <> show totalSize
  where
    msg = BenchMsg 42 "hello world benchmark" True
    go 0 !acc = acc
    go !i !acc = go (i - 1) (acc + messageSize msg)

-- Benchmark message type

data BenchMsg = BenchMsg
  { bmValue  :: {-# UNPACK #-} !Word64
  , bmName   :: !Text
  , bmActive :: !Bool
  } deriving stock (Show, Eq)

instance MessageEncode BenchMsg where
  buildMessage msg =
    (if bmValue msg /= 0 then encodeFieldVarint 1 (bmValue msg) else mempty) <>
    (if bmName msg /= "" then encodeFieldString 2 (bmName msg) else mempty) <>
    (if bmActive msg then encodeFieldBool 3 True else mempty)

instance MessageSize BenchMsg where
  messageSize msg =
    (if bmValue msg /= 0 then fieldVarintSize 1 (bmValue msg) else 0) +
    (if bmName msg /= "" then fieldTextSize 2 (bmName msg) else 0) +
    (if bmActive msg then fieldBoolSize 3 else 0)

buildSizedBenchMsg :: BenchMsg -> SB.SizedBuilder
buildSizedBenchMsg msg =
  (if bmValue msg /= 0 then sizedFieldVarint 1 (bmValue msg) else mempty) <>
  (if bmName msg /= "" then sizedFieldString 2 (bmName msg) else mempty) <>
  (if bmActive msg then sizedFieldBool 3 True else mempty)

benchSizedBuilderEncode :: IO ()
benchSizedBuilderEncode = do
  let n = 10000 :: Int

  putStrLn "\nSizedBuilder encode (10k iterations, fused size+build):"

  t1 <- getCPUTime
  let !totalSize = go n 0
  t2 <- getCPUTime

  putStrLn $ "  Time: " <> show ((t2 - t1) `div` 1000000000) <> " ms"
  putStrLn $ "  Total bytes: " <> show totalSize
  where
    msg = BenchMsg 42 "hello world benchmark" True
    go 0 !acc = acc
    go !i !acc = go (i - 1) (acc + BS.length (SB.toByteString (buildSizedBenchMsg msg)))

instance MessageDecode BenchMsg where
  messageDecoder = loop 0 "" False
    where
      loop !val !name !active = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure (BenchMsg val name active)
          UJust (Tag fn wt) -> case fn of
            1 -> getVarint >>= \v -> loop v name active
            2 -> getText >>= \v -> loop val v active
            3 -> getVarint >>= \v -> loop val name (v /= 0)
            _ -> skipField wt >> loop val name active
