{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector as V
import Numeric (showHex)
import System.Exit (exitFailure)

import qualified Data.Vector.Primitive as VP
import Data.Int (Int32, Int64)

import Parquet.BloomFilter
import Parquet.PageIndex
import Parquet.Read (loadParquetFile, pfFooter)
import Parquet.Types
import Parquet.Write
  ( buildParquetFile
  , buildParquetFileWithIndex
  , statisticsForByteArray
  , statisticsForInt32
  , statisticsForInt64
  )
import Parquet.XXH64

main :: IO ()
main = do
  -- XXH64 reference vectors.
  expectHash "" "ef46db3751d8e999"
  expectHash "abc" "44bc2cf5ad770999"
  expectHash "Nobody inspects the spammish repetition" "fbcea83c8a378bf1"
  -- 32 bytes of 'a' is exactly one bulk stripe (xxhsum -H1 reference).
  expectHashBs (BS.replicate 32 0x61) "856e843298f99ad7"
  -- 64 bytes (two stripes) — exercises the bulk-phase merge.
  expectHashBs (BS.replicate 64 0x62) "ecbaf4bdf26b6349"

  -- OffsetIndex round-trip.
  let oi = OffsetIndex
        { oiPageLocations = V.fromList
            [ PageLocation 100 200 0
            , PageLocation 300 250 50
            ]
        , oiUnencodedByteArrayDataBytes = Just (V.fromList [42, 99])
        }
  expect "OffsetIndex round-trip"
    (decodeOffsetIndex (encodeOffsetIndex oi) == Right oi)

  -- ColumnIndex round-trip with all optional fields.
  let ci = ColumnIndex
        { ciNullPages = V.fromList [False, False, True]
        , ciMinValues = V.fromList [BSC.pack "a", BSC.pack "b", BS.empty]
        , ciMaxValues = V.fromList [BSC.pack "z", BSC.pack "y", BS.empty]
        , ciBoundaryOrder = OrderAscending
        , ciNullCounts = Just (V.fromList [0, 0, 100])
        , ciRepetitionLevelHistograms = Just (V.fromList [10, 5])
        , ciDefinitionLevelHistograms = Just (V.fromList [3, 8])
        }
  expect "ColumnIndex round-trip"
    (decodeColumnIndex (encodeColumnIndex ci) == Right ci)

  -- Bloom filter membership.
  let sbbf0 = newSbbf 1024
      values = ["alpha", "beta", "gamma", "delta", "epsilon"]
      sbbf  = foldr (sbbfInsert . BSC.pack) sbbf0 values
  mapM_ (\v -> expect ("bloom contains " ++ v)
                 (sbbfCheck (BSC.pack v) sbbf)) values

  -- Golden vector from arrow-rs / parquet-mr: a 32-byte bitset produced
  -- by parquet-mr for the strings "a0".."a9" must report all of them
  -- present.  This proves byte-compatibility of our XXH64 + block layout
  -- with the reference writer.
  let goldenBits = BS.pack
        [ 200, 1, 80, 20, 64, 68, 8, 109, 6, 37, 4, 67, 144, 80, 96, 32
        , 8, 132, 43, 33, 0, 5, 99, 65, 2, 0, 224, 44, 64, 78, 96, 4 ]
      goldenSbbf = newSbbfFromBytes goldenBits
  mapM_ (\i -> let v = "a" <> show i in
                 expect ("golden contains " ++ v)
                   (sbbfCheck (BSC.pack v) goldenSbbf))
        [(0 :: Int) .. 9]

  -- Bloom filter false-positive sanity.
  let sbbfBig0 = newSbbf 2048
      inserted = map (BSC.pack . ("inserted-" <>) . show) [0 .. 255 :: Int]
      probes   = map (BSC.pack . ("probe-" <>) . show)   [0 .. 255 :: Int]
      sbbfBig  = foldr sbbfInsert sbbfBig0 inserted
      fp = length (filter (`sbbfCheck` sbbfBig) probes)
  expect ("bloom FP rate (got " ++ show fp ++ ")") (fp <= 16)

  -- Bloom encode/decode round-trip.
  let bs = encodeBloomFilter sbbf
  case decodeBloomFilter bs of
    Left e -> failTest ("decodeBloomFilter: " ++ e)
    Right (_hdr, sbbf') -> do
      expect "decoded numBytes" (sbbfNumBytes sbbf' == sbbfNumBytes sbbf)
      mapM_ (\v -> expect ("decoded contains " ++ v)
                     (sbbfCheck (BSC.pack v) sbbf')) values

  -- Statistics
  let s32 = statisticsForInt32 (VP.fromList [3, -1, 7, 0, 4 :: Int32])
  expect "Int32 stats min"
    (statMinValue s32 == Just (BS.pack [0xFF, 0xFF, 0xFF, 0xFF]))   -- -1 LE
  expect "Int32 stats max"
    (statMaxValue s32 == Just (BS.pack [0x07, 0x00, 0x00, 0x00]))
  expect "Int32 stats nullCount"
    (statNullCount s32 == Just 0)
  let s64 = statisticsForInt64 (VP.fromList [10, 5, -3, 100 :: Int64])
  expect "Int64 stats min/max present"
    (statMinValue s64 /= Nothing && statMaxValue s64 /= Nothing)
  let sBA = statisticsForByteArray
              (V.fromList [BSC.pack "banana", BSC.pack "apple", BSC.pack "cherry"])
  expect "ByteArray stats min == 'apple'"
    (statMinValue sBA == Just (BSC.pack "apple"))
  expect "ByteArray stats max == 'cherry'"
    (statMaxValue sBA == Just (BSC.pack "cherry"))
  let sEmpty = statisticsForInt32 VP.empty
  expect "empty Int32 stats has no min/max"
    (statMinValue sEmpty == Nothing && statMaxValue sEmpty == Nothing)

  -- Writer attaches statistics that round-trip through readFooter.
  let schema = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing
        , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing
        ]
      vs   = VP.fromList [(3 :: Int32), -1, 7, 0, 4]
      fbs  = buildParquetFile schema (V.singleton (V.singleton vs))
  case loadParquetFile fbs of
    Left e -> failTest ("loadParquetFile: " ++ e)
    Right pf -> do
      let !rgs = fmRowGroups (pfFooter pf)
          !cm = ccMetadata
                  (V.unsafeIndex (rgColumns (V.unsafeIndex rgs 0)) 0)
      case cm of
        Nothing -> failTest "expected ColumnMetadata"
        Just m -> case cmStatistics m of
          Nothing -> failTest "writer omitted statistics"
          Just st -> do
            expect "writer min == -1"
              (statMinValue st == Just (BS.pack [0xFF, 0xFF, 0xFF, 0xFF]))
            expect "writer max == 7"
              (statMaxValue st == Just (BS.pack [0x07, 0x00, 0x00, 0x00]))

  -- buildParquetFileWithIndex: produces files with OffsetIndex,
  -- ColumnIndex, and bloom filter footers populated.
  let schemaWI = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 2) Nothing Nothing
        , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing
        , SchemaElement "y" (Just Required) (Just PTInt32) Nothing Nothing Nothing
        ]
      colX = VP.fromList [(0 :: Int32) .. 9]
      colY = VP.fromList [(100 :: Int32) .. 109]
      fbsWI = buildParquetFileWithIndex 4 schemaWI
                (V.singleton (V.fromList [colX, colY]))
  case loadParquetFile fbsWI of
    Left e -> failTest ("loadParquetFile (with index): " ++ e)
    Right pfWI -> do
      let !rgs = fmRowGroups (pfFooter pfWI)
          !rg = V.unsafeIndex rgs 0
          !cc0 = V.unsafeIndex (rgColumns rg) 0
          !cc1 = V.unsafeIndex (rgColumns rg) 1
      expect "with-index writer: 2 columns" (V.length (rgColumns rg) == 2)
      expect "with-index col 0: OffsetIndex pointer set"
        (ccOffsetIndexOffset cc0 /= Nothing && ccOffsetIndexLength cc0 /= Nothing)
      expect "with-index col 0: ColumnIndex pointer set"
        (ccColumnIndexOffset cc0 /= Nothing && ccColumnIndexLength cc0 /= Nothing)
      case ccMetadata cc0 of
        Nothing -> failTest "with-index col 0: ColumnMetadata missing"
        Just m  -> do
          expect "with-index col 0: bloom_filter_offset set"
            (cmBloomFilterOffset m /= Nothing && cmBloomFilterLength m /= Nothing)
      case readOffsetIndex pfWI 0 0 of
        Left e -> failTest ("readOffsetIndex col 0: " ++ e)
        Right Nothing -> failTest "readOffsetIndex col 0: Nothing"
        Right (Just oi0) -> do
          -- 10 values / 4 per page = 3 pages (4 + 4 + 2)
          expect "with-index col 0: 3 pages"
            (V.length (oiPageLocations oi0) == 3)
          expect "with-index col 0: page 0 starts at row 0"
            (plFirstRowIndex (V.head (oiPageLocations oi0)) == 0)
      case readColumnIndex pfWI 0 0 of
        Left e -> failTest ("readColumnIndex col 0: " ++ e)
        Right Nothing -> failTest "readColumnIndex col 0: Nothing"
        Right (Just ci0) -> do
          expect "with-index col 0: ColumnIndex has 3 entries"
            (V.length (ciMinValues ci0) == 3 && V.length (ciMaxValues ci0) == 3)
      case readOffsetIndex pfWI 0 1 of
        Left e -> failTest ("readOffsetIndex col 1: " ++ e)
        Right Nothing -> failTest "readOffsetIndex col 1: Nothing"
        Right (Just _)  -> putStrLn "OK: with-index col 1: OffsetIndex parsed"
      -- The bloom filter slice should be parseable as a BloomFilter.
      case ccMetadata cc1 of
        Nothing -> pure ()
        Just m  -> case (cmBloomFilterOffset m, cmBloomFilterLength m) of
          (Just o, Just l) -> do
            let slice = BS.take (fromIntegral l) (BS.drop (fromIntegral o) fbsWI)
            case decodeBloomFilter slice of
              Left e -> failTest ("decodeBloomFilter col 1: " ++ e)
              Right (_, sbbf) -> do
                expect "with-index col 1: bloom contains a known value"
                  (sbbfCheck (i32LEBs (105 :: Int32)) sbbf)
                expect "with-index col 1: bloom does not contain a far value"
                  (not (sbbfCheck (i32LEBs (999999 :: Int32)) sbbf))
          _ -> failTest "with-index col 1: bloom pointers missing"

  putStrLn "All Parquet page-index / bloom-filter / statistics tests passed."

i32LEBs :: Int32 -> BS.ByteString
i32LEBs n = BS.pack
  [ fromIntegral n
  , fromIntegral (n `div` 256)
  , fromIntegral (n `div` 65536)
  , fromIntegral (n `div` 16777216)
  ]

expectHash :: String -> String -> IO ()
expectHash s expected = expectHashBs (BSC.pack s) expected

expectHashBs :: BS.ByteString -> String -> IO ()
expectHashBs bs expected =
  let actual = pad16 (showHex (xxh64 bs) "")
  in unless (actual == expected) $
       failTest ("xxh64 " ++ show bs ++ " expected " ++ expected
                  ++ " got " ++ actual)

pad16 :: String -> String
pad16 s = replicate (16 - length s) '0' ++ s

expect :: String -> Bool -> IO ()
expect what ok = do
  if ok
    then putStrLn ("OK: " ++ what)
    else failTest what

failTest :: String -> IO ()
failTest msg = do
  putStrLn ("FAIL: " ++ msg)
  exitFailure
