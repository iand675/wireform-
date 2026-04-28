{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (unless)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector as V
import Numeric (showHex)
import System.Exit (exitFailure)

import qualified Data.Vector.Primitive as VP
import Data.Int (Int32, Int64)

import Parquet.BloomFilter
import Parquet.Levels
  ( materializeRepeatedBool
  , materializeRepeatedDouble
  , materializeRepeatedFloat
  , materializeRepeatedInt32
  , materializeRepeatedInt64
  )
import Parquet.Footer (readFooter, writeFooter)
import Parquet.PageIndex
import Parquet.Read (loadParquetFile, pfFooter)
import Parquet.Types
import Parquet.Write
  ( buildParquetFile
  , buildParquetFileWithBloom
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

  -- ColumnMetaData round-trip with index_page_offset, dictionary_page_offset,
  -- key_value_metadata, encoding_stats (parquet.thrift fields 10-13).
  let cm = ColumnMetadata
        { cmType                  = PTByteArray
        , cmEncodings             = V.fromList [Plain, RLEDictionary]
        , cmPathInSchema          = V.singleton "name"
        , cmCodec                 = GZip
        , cmNumValues             = 4096
        , cmTotalUncompressedSize = 12345
        , cmTotalCompressedSize   = 6789
        , cmDataPageOffset        = 1024
        , cmStatistics            = Nothing
        , cmIndexPageOffset       = Just 800
        , cmDictionaryPageOffset  = Just 100
        , cmKeyValueMetadata      = V.fromList
            [ KeyValue "writer.version" (Just "wireform-1")
            , KeyValue "comment" Nothing
            ]
        , cmEncodingStats         = V.fromList
            [ PageEncodingStats { pesPageType = 2, pesEncoding = Plain, pesCount = 1 }
            , PageEncodingStats { pesPageType = 0, pesEncoding = RLEDictionary, pesCount = 7 }
            ]
        , cmBloomFilterOffset     = Just 9001
        , cmBloomFilterLength     = Just 256
        }
      ccF = ColumnChunk Nothing 1024 (Just cm) Nothing Nothing Nothing Nothing
      rgF = RowGroup (V.singleton ccF) 6789 4096
      fmF = FileMetadata 2
              (V.fromList
                [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing
                , SchemaElement "name" (Just Required) (Just PTByteArray) Nothing Nothing Nothing
                ])
              4096 (V.singleton rgF) Nothing
  case readFooter (writeFooter fmF) of
    Left e -> failTest ("ColumnMetaData fields 10-15 round-trip: " ++ e)
    Right got
      | got /= fmF -> failTest "ColumnMetaData fields 10-15 mismatch"
      | otherwise ->
          putStrLn "OK: ColumnMetaData fields 10-15 round-trip (index/dict/key-value/encoding-stats/bloom)"

  -- Repeated-column materialization (A.9). rep=0 starts a new row,
  -- rep>0 continues; def==maxDef means a defined element, def<maxDef
  -- a null element in the same row.
  let reps = VP.fromList [(0 :: Int32), 1, 1]
      defs = VP.fromList [(2 :: Int32), 2, 1]
      bodyBool = BS.pack [0x01]   -- bits 1 then 0 for the two defined rows
  case materializeRepeatedBool reps defs 2 bodyBool of
    Left e -> failTest ("repeated BOOL: " ++ e)
    Right got
      | got == V.singleton (V.fromList [Just True, Just False, Nothing]) ->
          putStrLn "OK: materializeRepeatedBool"
      | otherwise -> failTest $ "repeated BOOL mismatch: " ++ show got

  -- Repeated INT64: two rows, second has one defined and one null element.
  let reps64 = VP.fromList [(0 :: Int32), 0, 1]
      defs64 = VP.fromList [(2 :: Int32), 2, 1]
      body64 = BS.pack
        [ 0x64, 0, 0, 0, 0, 0, 0, 0    -- 100
        , 0x07, 0, 0, 0, 0, 0, 0, 0    -- 7
        ]
  case materializeRepeatedInt64 reps64 defs64 2 body64 of
    Left e -> failTest ("repeated INT64: " ++ e)
    Right got
      | got == V.fromList
          [ V.singleton (Just (100 :: Int64))
          , V.fromList [Just 7, Nothing]
          ] -> putStrLn "OK: materializeRepeatedInt64"
      | otherwise -> failTest $ "repeated INT64 mismatch: " ++ show got

  -- Repeated DOUBLE smoke test.
  let repsD = VP.fromList [(0 :: Int32), 1]
      defsD = VP.fromList [(2 :: Int32), 2]
      bodyD = BS.pack
        [ 0, 0, 0, 0, 0, 0, 0xF0, 0x3F   -- 1.0
        , 0, 0, 0, 0, 0, 0, 0x00, 0x40   -- 2.0
        ]
  case materializeRepeatedDouble repsD defsD 2 bodyD of
    Left e -> failTest ("repeated DOUBLE: " ++ e)
    Right got
      | V.length got == 1 && V.length (V.head got) == 2 ->
          putStrLn "OK: materializeRepeatedDouble"
      | otherwise -> failTest $ "repeated DOUBLE shape: " ++ show got

  -- buildParquetFileWithBloom: round-trip a file whose column chunk
  -- carries a bloom filter, and verify that loading it back finds
  -- every inserted value.
  let schemaB = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing
        , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing
        ]
      vsB = VP.fromList [(7 :: Int32), 11, 13, 17, 19, 23, 29]
      bfile = buildParquetFileWithBloom 256 schemaB
                (V.singleton (V.singleton vsB))
  case loadParquetFile bfile of
    Left e -> failTest ("loadParquetFile (with bloom): " ++ e)
    Right pf -> do
      let !rgs = fmRowGroups (pfFooter pf)
          !cm = ccMetadata (V.unsafeIndex (rgColumns (V.unsafeIndex rgs 0)) 0)
      case cm of
        Nothing -> failTest "expected ColumnMetadata"
        Just m -> case (cmBloomFilterOffset m, cmBloomFilterLength m) of
          (Just bfOff, Just bfLen) -> do
            let !sl = BS.take (fromIntegral bfLen) (BS.drop (fromIntegral bfOff) bfile)
            case decodeBloomFilter sl of
              Left e -> failTest ("decodeBloomFilter: " ++ e)
              Right (_, sbbf) -> do
                -- Every inserted value should report present.
                let allPresent = VP.foldl' (\ok v ->
                      ok && sbbfCheck (i32LE v) sbbf) True vsB
                if allPresent
                  then putStrLn "OK: buildParquetFileWithBloom + decodeBloomFilter"
                  else failTest "bloom did not contain every inserted value"
                -- Spot-check a value we did not insert.
                if sbbfCheck (i32LE 12345) sbbf
                  then putStrLn "Note: bloom false positive for 12345 (acceptable)"
                  else putStrLn "OK: bloom does not falsely contain 12345"
          _ -> failTest "writer did not populate bloom_filter_offset/length"

  putStrLn "All Parquet page-index / bloom-filter / statistics tests passed."

-- | Helper: little-endian 4-byte encoding of an Int32, used to feed the
-- bloom filter exactly the same way 'buildParquetFileWithBloom' does.
i32LE :: Int32 -> BS.ByteString
i32LE v = BS.pack
  [ fromIntegral (v .&. 0xFF)
  , fromIntegral ((v `shiftR` 8) .&. 0xFF)
  , fromIntegral ((v `shiftR` 16) .&. 0xFF)
  , fromIntegral ((v `shiftR` 24) .&. 0xFF)
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
