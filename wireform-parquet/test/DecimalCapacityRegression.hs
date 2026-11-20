{-# LANGUAGE OverloadedStrings #-}

module DecimalCapacityRegression (tests) where

import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.Either (isRight)
import Data.Int (Int32)
import Data.Vector qualified as V
import GHC.Conc (getAllocationCounter)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Parquet.Footer qualified as F
import Parquet.Types qualified as P
import Thrift.Encode qualified as T
import Thrift.Value qualified as TV
import Thrift.Wire (ThriftType (..))


-- Raw field numbers make these fixtures independent of the footer encoder.
metadata :: Int32 -> Maybe Int32 -> Bool -> Int32 -> TV.Value
metadata precision width modern physical =
  let decimal = TV.Struct (V.fromList [(1, TV.I32 0), (2, TV.I32 precision)])
      leaf = TV.Struct (V.fromList ([(1, TV.I32 physical), (3, TV.I32 0), (4, TV.String "d"), (6, TV.I32 5), (7, TV.I32 0), (8, TV.I32 precision)] ++ maybe [] (\n -> [(2, TV.I32 n)]) width ++ if modern then [(10, TV.Struct (V.singleton (5, decimal)))] else []))
      root = TV.Struct (V.fromList [(4, TV.String "schema"), (5, TV.I32 1)])
  in TV.Struct (V.fromList [(1, TV.I32 1), (2, TV.List TT_STRUCT (V.fromList [root, leaf])), (3, TV.I64 0), (4, TV.List TT_STRUCT V.empty)])


accepted :: Int32 -> Int32 -> Bool -> Bool
accepted precision width modern = isRight (F.readFooterRaw (T.encodeCompact (metadata precision (Just width) modern 7)))


compatible :: Int32 -> Int32 -> Bool
compatible precision width =
  P.hasCompatiblePhysicalType
    P.SchemaElement
      { P.seName = "d"
      , P.seRepetition = Just P.Required
      , P.seType = Just P.PTFixedLenByteArray
      , P.seTypeLength = Just width
      , P.seNumChildren = Nothing
      , P.seConvertedType = Just P.CTDecimal
      , P.seLogicalType = Just (P.LTDecimal precision 0)
      , P.seFieldId = Nothing
      }


exactCapacity :: Int -> Int
exactCapacity width = search 0 (8 * width)
  where
    limit = (2 :: Integer) ^ (8 * width - 1)
    search low high
      | high - low <= 1 = low
      | (10 :: Integer) ^ middle <= limit = search middle high
      | otherwise = search low middle
      where
        middle = (low + high) `quot` 2


check :: String -> Bool -> IO ()
check name ok = unless ok (fail ("DECIMAL capacity regression: " ++ name))


tests :: IO ()
tests = do
  forM_ [1 .. 512] $ \width -> do
    let capacity = fromIntegral (exactCapacity width)
        physicalWidth = fromIntegral width
    forM_ [False, True] $ \modern -> do
      check "exact capacity is accepted" (accepted capacity physicalWidth modern)
      check "one digit past capacity is rejected" (not (accepted (capacity + 1) physicalWidth modern))
    check "physical compatibility accepts exact capacity" (compatible capacity physicalWidth)
    check "physical compatibility rejects excess precision" (not (compatible (capacity + 1) physicalWidth))
  forM_ [(77, 1), (77, 32), (80, 33), (1000, 33), (maxBound, 32)] $ \(precision, width) -> do
    check "large declared precision cannot bypass fixed capacity" (not (accepted precision width True) && not (accepted precision width False) && not (compatible precision width))
  forM_ [(79, 33), (1000, 416), (maxBound, maxBound), (maxBound, 1000000000), (9, 536870913)] $ \(precision, width) -> do
    check "large valid metadata is accepted without Arrow's76-digit cap" (accepted precision width True && accepted precision width False && compatible precision width)
  forM_ [(0, 1), (-1, 1), (1, 0), (1, -1)] $ \(precision, width) ->
    check "nonpositive precision/width is invalid" (not (accepted precision width True) && not (compatible precision width))
  forM_ [77, 1000, maxBound] $ \precision -> do
    check "BYTE_ARRAY precision remains unbounded by its storage type" (isRight (F.readFooterRaw (T.encodeCompact (metadata precision Nothing True 6))))
    check "legacy BYTE_ARRAY precision remains unbounded" (isRight (F.readFooterRaw (T.encodeCompact (metadata precision Nothing False 6))))
  passed <- H.check $ H.withTests 1000 $ H.property $ do
    width <- H.forAll (Gen.int (Range.linear 1 4096))
    precision <- H.forAll (Gen.int (Range.linear 1 10000))
    let expected = (10 :: Integer) ^ precision <= (2 :: Integer) ^ (8 * width - 1)
    accepted (fromIntegral precision) (fromIntegral width) True H.=== expected
    compatible (fromIntegral precision) (fromIntegral width) H.=== expected
  check "generated widths/precisions agree with independent exact powers" passed
  -- Only metadata is parsed. Declared byte widths must never drive a value
  -- allocation or an enormous exponent before normal read limits can apply.
  let bytes = T.encodeCompact (metadata maxBound (Just maxBound) True 7)
  _ <- evaluate (BS.length bytes)
  let warm = T.encodeCompact (metadata 1 (Just maxBound) True 7)
  _ <- evaluate (V.length (case F.readFooterRaw warm of Right file -> P.fmSchema file; Left message -> error message))
  before <- getAllocationCounter
  value <- evaluate (case F.readFooterRaw bytes of Right file -> V.length (P.fmSchema file); Left message -> error message)
  after <- getAllocationCounter
  check "huge valid width uses bounded metadata memory" (value == 2 && before - after < 1024 * 1024)
  putStrLn "DECIMAL full-width capacity, legacy/modern, exact-boundary and metadata-memory regressions passed"
