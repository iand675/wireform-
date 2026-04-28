{-# LANGUAGE OverloadedStrings #-}
-- | Microbenchmarks for the Parquet footer encoder.
--
-- Run with:
--
--   cabal bench wireform-parquet:wireform-parquet-footer-bench
module Main (main) where

import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Vector as V

import Criterion.Main (bench, bgroup, defaultMain, nf, whnf)

import Parquet.Footer (writeFooter)
import Parquet.Types

main :: IO ()
main =
  defaultMain
    [ bgroup "writeFooter"
        [ bench "1 RG x 5 cols (all optionals populated)" $
            nf writeFooter (mkFooter 1 5 True)
        , bench "1 RG x 5 cols (no optionals)" $
            nf writeFooter (mkFooter 1 5 False)
        , bench "8 RG x 16 cols (all optionals populated)" $
            nf writeFooter (mkFooter 8 16 True)
        , bench "32 RG x 64 cols (all optionals populated)" $
            nf writeFooter (mkFooter 32 64 True)
        ]
    , bgroup "writeFooter byte length"
        [ bench "1 RG x 5 cols (no optionals) length" $
            whnf (BS.length . writeFooter) (mkFooter 1 5 False)
        ]
    ]

mkFooter :: Int -> Int -> Bool -> FileMetadata
mkFooter !nRowGroups !nCols populateOptionals =
  let !schema = mkSchema nCols
      !rgs = V.fromList
        [ mkRowGroup nCols populateOptionals (fromIntegral (i * 100))
        | i <- [0 .. nRowGroups - 1]
        ]
  in FileMetadata
       { fmVersion = 2
       , fmSchema = schema
       , fmNumRows = fromIntegral (nRowGroups * 1000)
       , fmRowGroups = rgs
       , fmCreatedBy = Just "wireform-bench"
       }

mkSchema :: Int -> V.Vector SchemaElement
mkSchema !nCols = V.fromList $
  SchemaElement "schema" Nothing Nothing
    (Just (fromIntegral nCols)) Nothing Nothing
  : [ SchemaElement (T.pack ("c" ++ show i)) (Just Required) (Just PTInt64)
        Nothing Nothing Nothing
    | i <- [0 .. nCols - 1]
    ]

mkRowGroup :: Int -> Bool -> Int64 -> RowGroup
mkRowGroup !nCols populateOptionals !startOff = RowGroup
  { rgColumns = V.fromList
      [ mkColumnChunk i populateOptionals (startOff + fromIntegral i * 1024)
      | i <- [0 .. nCols - 1]
      ]
  , rgTotalByteSize = fromIntegral nCols * 1024
  , rgNumRows = 1000
  }

mkColumnChunk :: Int -> Bool -> Int64 -> ColumnChunk
mkColumnChunk !_i populateOptionals !off = ColumnChunk
  { ccFilePath = if populateOptionals then Just "data/part-0.parquet" else Nothing
  , ccFileOffset = off
  , ccMetadata = Just (mkColumnMetadata _i populateOptionals off)
  , ccOffsetIndexOffset = if populateOptionals then Just (off + 800) else Nothing
  , ccOffsetIndexLength = if populateOptionals then Just 80     else Nothing
  , ccColumnIndexOffset = if populateOptionals then Just (off + 880) else Nothing
  , ccColumnIndexLength = if populateOptionals then Just 200    else Nothing
  }

mkColumnMetadata :: Int -> Bool -> Int64 -> ColumnMetadata
mkColumnMetadata !i populateOptionals !off = ColumnMetadata
  { cmType                  = PTInt64
  , cmEncodings             = V.fromList [Plain, RLEDictionary]
  , cmPathInSchema          = V.singleton (T.pack ("c" ++ show i))
  , cmCodec                 = GZip
  , cmNumValues             = 4096
  , cmTotalUncompressedSize = 12345
  , cmTotalCompressedSize   = 6789
  , cmDataPageOffset        = off
  , cmStatistics            = if populateOptionals then Just stats else Nothing
  , cmIndexPageOffset       = if populateOptionals then Just (off + 600) else Nothing
  , cmDictionaryPageOffset  = if populateOptionals then Just (off + 100) else Nothing
  , cmKeyValueMetadata      = if populateOptionals then kvs else V.empty
  , cmEncodingStats         = if populateOptionals then encStats else V.empty
  , cmBloomFilterOffset     = if populateOptionals then Just (off + 9001) else Nothing
  , cmBloomFilterLength     = if populateOptionals then Just 256 else Nothing
  }
  where
    stats = Statistics
      { statMin = Just (BS.pack [0,0,0,0,0,0,0,0])
      , statMax = Just (BS.pack [0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff])
      , statNullCount = Just 0
      , statDistinctCount = Nothing
      , statMinValue = Just (BS.pack [0,0,0,0,0,0,0,0])
      , statMaxValue = Just (BS.pack [0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff])
      , statIsMaxValueExact = Just True
      , statIsMinValueExact = Just True
      }
    kvs = V.fromList
      [ KeyValue "writer.version" (Just "wireform")
      , KeyValue "writer.lang" (Just "haskell")
      ]
    encStats = V.fromList
      [ PageEncodingStats { pesPageType = 2, pesEncoding = Plain, pesCount = 1 }
      , PageEncodingStats { pesPageType = 0, pesEncoding = RLEDictionary, pesCount = 8 }
      ]
