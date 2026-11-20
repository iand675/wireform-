module BloomBuilderRegression (tests) where

import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Int (Int32)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import GHC.Conc (getAllocationCounter)
import Hedgehog (Group (..), assert, checkParallel, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Parquet.BloomFilter qualified as B
import System.Mem (performGC)


check :: String -> Bool -> IO ()
check label success = unless success (fail ("bloom builder: " ++ label))


bulk :: Int -> V.Vector BS.ByteString -> B.Sbbf
bulk size values = B.buildSbbf size (\builder -> V.forM_ values (B.sbbfBuilderInsert builder))


incremental :: Int -> V.Vector BS.ByteString -> B.Sbbf
incremental size = V.foldl' (flip B.sbbfInsert) (B.newSbbf size)


encoded :: B.Sbbf -> BS.ByteString
encoded = B.encodeBloomFilter


allocation :: BS.ByteString -> IO Int
allocation bytes = do
  performGC
  before <- getAllocationCounter
  _ <- evaluate (BS.length bytes)
  after <- getAllocationCounter
  pure (fromIntegral (before - after))


tests :: IO ()
tests = do
  okay <-
    checkParallel $
      Group
        "bulk bloom construction"
        [
          ( "same wire bits and membership"
          , property $ do
              size <- forAll (Gen.element [32, 64, 96, 256, 4096])
              values <- V.fromList <$> forAll (Gen.list (Range.linear 0 180) (Gen.bytes (Range.linear 0 80)))
              let built = bulk size values
                  original = incremental size values
              encoded built === encoded original
              assert (V.all (`B.sbbfCheck` built) values)
              assert (B.sbbfNumBytes built == size)
          )
        ,
          ( "eight-hash sizing estimate"
          , property $ do
              count <- forAll (Gen.int (Range.linear 1 100000))
              probability <- forAll (Gen.element [0.000001, 0.0001, 0.01, 0.1, 0.5])
              let bytes = B.optimalNumBytes count probability
                  estimated = (1 - exp (negate (fromIntegral count / fromIntegral bytes))) ** 8
              assert (bytes >= 32 && bytes `rem` 32 == 0)
              assert (estimated <= probability + 1e-12)
          )
        ]
  check "properties" okay
  forM_ [BS.empty, BS.replicate 1 0, BS.replicate 31 0, BS.replicate 33 0] $ \bytes ->
    check "malformed raw bitsets are rejected before unsafe lane access" (isLeft (B.newSbbfFromBytesChecked bytes))
  forM_ [0, 31, 33, maxBound] $ \size ->
    check "invalid exact-size constructors are rejected" (isLeft (B.newSbbfBytesChecked size U.empty))
  check "decoded word count must match declared size" (isLeft (B.newSbbfBytesChecked 32 (U.singleton 0)))
  check "oversized allocation request is rejected before rounding" (isLeft (B.newSbbfChecked maxBound))
  check "oversized bulk request is rejected before allocation" (isLeft (B.buildSbbfChecked maxBound (\_ -> pure ())))
  forM_ [-1, 0, 1, 31, 32, 33] $ \size -> do
    emptyFilter <- either fail pure (B.newSbbfChecked size)
    let rounded = if size <= 32 then 32 else 64
    check "normal allocation preserves minimum/rounding rules" (B.sbbfNumBytes emptyFilter == rounded)
  let rawWords = [0x0123456789abcdef, 0xfedcba9876543210, 0x8000000000000001, 0x0102030405060708]
      expectedBits = BS.pack [0xef, 0xcd, 0xab, 0x89, 0x67, 0x45, 0x23, 0x01, 0x10, 0x32, 0x54, 0x76, 0x98, 0xba, 0xdc, 0xfe, 0x01, 0, 0, 0, 0, 0, 0, 0x80, 0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01]
  forM_ [0, 1, 3, 8] $ \offset -> do
    let guarded = U.fromList (replicate offset 0xdeadbeef ++ rawWords ++ replicate 4 0xdeadbeef)
    filter_ <- either fail pure (B.newSbbfBytesChecked 32 (U.slice offset 4 guarded))
    let bytes = encoded filter_
    check "serialization respects word-vector slices and little-endian layout" (BS.drop (BS.length bytes - 32) bytes == expectedBits)
    (_, decoded) <- either fail pure (B.decodeBloomFilter bytes)
    check "sliced bitsets round-trip without neighboring guard words" (decoded == filter_)
  let limit = fromIntegral (maxBound :: Int32) - 31
  -- Independent100-digit Decimal evaluation gives25762.411444445775...
  -- bytes before rounding. Computing the eighth root first rounds it to1.
  let belowOne = encodeFloat (2 ^ (53 :: Int) - 1) (-53) :: Double
  check "probability immediately below1 keeps its finite sizing estimate" (B.optimalNumBytes 1000000 belowOne == 25792)
  check "huge NDV sizing saturates without integer overflow" (B.optimalNumBytes maxBound 0.01 == limit)
  check "tiny probability keeps a finite representable size" (B.optimalNumBytes 1 5e-324 == limit)
  forM_ [0, 1, -1, 0 / 0, 1 / 0, -1 / 0] $ \probability ->
    check "invalid probability has a deterministic fallback" (B.optimalNumBytes 100 probability == 32)
  let values = V.generate 4096 (\index -> BS.pack [fromIntegral index, fromIntegral (index `quot` 256), 0, 0])
  _ <- evaluate (V.foldl' (\total bytes -> total + BS.length bytes) 0 values)
  bulkBytes <- allocation (encoded (bulk 32768 values))
  oldBytes <- allocation (encoded (incremental 32768 values))
  check "whole-column construction avoids a bitset copy per row" (bulkBytes * 8 < oldBytes)
  putStrLn ("Bloom allocation regression: bulk=" ++ show bulkBytes ++ ", incremental=" ++ show oldBytes)
