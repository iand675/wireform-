{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the @Iceberg.Parquet@ bridge: deriving a populated 'DataFile'
from a 'P.FileMetadata' and computing page overlap with a deletion vector.
-}
module Test.Iceberg.Parquet (tests) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Iceberg.DeletionVector qualified as DV
import Iceberg.Parquet qualified as IP
import Iceberg.Types
import Parquet.Encryption qualified as Enc
import Parquet.Types qualified as P
import Test.Syd


-- A two-column schema: id (long) and name (string).
schema :: Schema
schema =
  Schema
    { schemaId = 0
    , schemaFields =
        V.fromList
          [ StructField 1 "id" True TLong Nothing Nothing Nothing
          , StructField 2 "name" True TString Nothing Nothing Nothing
          ]
    , schemaIdentifierFieldIds = V.empty
    }


-- A single-row-group Parquet metadata covering the same schema, with min/max
-- statistics on each column and 100 rows total.
mkParquetFm :: P.FileMetadata
mkParquetFm =
  P.FileMetadata
    { P.fmVersion = 2
    , P.fmSchema = V.empty -- root + children would normally live here; the
    -- bridge doesn't consult fmSchema.
    , P.fmNumRows = 100
    , P.fmRowGroups =
        V.singleton
          ( P.RowGroup
              { P.rgColumns = V.fromList [idChunk, nameChunk]
              , P.rgTotalByteSize = 4096
              , P.rgNumRows = 100
              , P.rgSortingColumns = Nothing
              }
          )
    , P.fmCreatedBy = Just "wireform-parquet test"
    , P.fmKeyValueMetadata = Nothing
    , P.fmColumnOrders = Nothing
    }
  where
    idChunk =
      P.ColumnChunk
        { P.ccFilePath = Nothing
        , P.ccFileOffset = 0
        , P.ccMetadata =
            Just
              ( P.ColumnMetadata
                  { P.cmType = P.PTInt64
                  , P.cmEncodings = V.empty
                  , P.cmPathInSchema = V.singleton "id"
                  , P.cmCodec = P.Uncompressed
                  , P.cmNumValues = 100
                  , P.cmTotalUncompressedSize = 1024
                  , P.cmTotalCompressedSize = 800
                  , P.cmDataPageOffset = 4
                  , P.cmDictionaryPageOffset = Nothing
                  , P.cmStatistics =
                      Just
                        ( P.Statistics
                            { P.statMinValueExact = Nothing
                            , P.statMaxValueExact = Nothing
                            , P.statNanCount = Nothing
                            , P.statMin = Nothing
                            , P.statMax = Nothing
                            , P.statNullCount = Just 1
                            , P.statDistinctCount = Nothing
                            , P.statMinValue = Just (BS.pack [1, 0, 0, 0, 0, 0, 0, 0]) -- 1
                            , P.statMaxValue = Just (BS.pack [99, 0, 0, 0, 0, 0, 0, 0]) -- 99
                            }
                        )
                  , P.cmBloomFilterOffset = Nothing
                  , P.cmBloomFilterLength = Nothing
                  }
              )
        , P.ccOffsetIndexOffset = Nothing
        , P.ccOffsetIndexLength = Nothing
        , P.ccColumnIndexOffset = Nothing
        , P.ccColumnIndexLength = Nothing
        }
    nameChunk =
      P.ColumnChunk
        { P.ccFilePath = Nothing
        , P.ccFileOffset = 1024
        , P.ccMetadata =
            Just
              ( P.ColumnMetadata
                  { P.cmType = P.PTByteArray
                  , P.cmEncodings = V.empty
                  , P.cmPathInSchema = V.singleton "name"
                  , P.cmCodec = P.Uncompressed
                  , P.cmNumValues = 100
                  , P.cmTotalUncompressedSize = 2048
                  , P.cmTotalCompressedSize = 1500
                  , P.cmDataPageOffset = 1024
                  , P.cmDictionaryPageOffset = Nothing
                  , P.cmStatistics =
                      Just
                        ( P.Statistics
                            { P.statMinValueExact = Nothing
                            , P.statMaxValueExact = Nothing
                            , P.statNanCount = Nothing
                            , P.statMin = Nothing
                            , P.statMax = Nothing
                            , P.statNullCount = Just 0
                            , P.statDistinctCount = Nothing
                            , P.statMinValue = Just "alpha"
                            , P.statMaxValue = Just "zulu"
                            }
                        )
                  , P.cmBloomFilterOffset = Nothing
                  , P.cmBloomFilterLength = Nothing
                  }
              )
        , P.ccOffsetIndexOffset = Nothing
        , P.ccOffsetIndexLength = Nothing
        , P.ccColumnIndexOffset = Nothing
        , P.ccColumnIndexLength = Nothing
        }


mkOffsetIndex :: P.OffsetIndex
mkOffsetIndex =
  P.OffsetIndex
    { P.oiPageLocations =
        V.fromList
          [ P.PageLocation {P.plOffset = 0, P.plCompressedPageSize = 256, P.plFirstRowIndex = 0}
          , P.PageLocation {P.plOffset = 256, P.plCompressedPageSize = 256, P.plFirstRowIndex = 50}
          , P.PageLocation {P.plOffset = 512, P.plCompressedPageSize = 256, P.plFirstRowIndex = 75}
          ]
    , P.oiUnencodedByteArrayDataBytes = Nothing
    }


tests :: Spec
tests =
  describe "Iceberg.Parquet bridge" $
    sequence_
      [ it "dataFileFromParquet records column sizes by Iceberg field id" $ do
          let df =
                IP.dataFileFromParquet
                  mkParquetFm
                  schema
                  Map.empty
                  "s3://b/data.parquet"
                  4096
                  V.empty
                  Nothing
          Map.lookup 1 (dataFileColumnSizes df) `shouldBe` Just 800
          Map.lookup 2 (dataFileColumnSizes df) `shouldBe` Just 1500
          dataFileRecordCount df `shouldBe` 100
          dataFileFileSize df `shouldBe` 4096
      , it "dataFileFromParquet records value/null counts" $ do
          let df =
                IP.dataFileFromParquet
                  mkParquetFm
                  schema
                  Map.empty
                  "s3://b/data.parquet"
                  4096
                  V.empty
                  Nothing
          Map.lookup 1 (dataFileValueCounts df) `shouldBe` Just 100
          Map.lookup 2 (dataFileValueCounts df) `shouldBe` Just 100
          Map.lookup 1 (dataFileNullValueCounts df) `shouldBe` Just 1
          Map.lookup 2 (dataFileNullValueCounts df) `shouldBe` Just 0
      , it "dataFileFromParquet records min/max bounds (truncated)" $ do
          let df =
                IP.dataFileFromParquet
                  mkParquetFm
                  schema
                  Map.empty
                  "s3://b/data.parquet"
                  4096
                  V.empty
                  Nothing
          -- id column: 8-byte little-endian 1 / 99 - shorter than truncation
          -- threshold so they pass through.
          Map.lookup 1 (dataFileLowerBounds df) `shouldBe` Just (BS.pack [1, 0, 0, 0, 0, 0, 0, 0])
          Map.lookup 1 (dataFileUpperBounds df) `shouldBe` Just (BS.pack [99, 0, 0, 0, 0, 0, 0, 0])
          -- name column: bound truncation rounds the upper bound up.
          Map.lookup 2 (dataFileLowerBounds df) `shouldBe` Just "alpha"
          case Map.lookup 2 (dataFileUpperBounds df) of
            Just b -> (b >= "zulu") `shouldBe` True
            Nothing -> expectationFailure "expected upper bound for column 2"
      , it "dataFileFromParquet records split offsets per row group" $ do
          let df =
                IP.dataFileFromParquet
                  mkParquetFm
                  schema
                  Map.empty
                  "s3://b/data.parquet"
                  4096
                  V.empty
                  Nothing
          V.toList (dataFileSplitOffsets df) `shouldBe` [4, 1024]
      , it "metrics-mode 'none' suppresses stats for that column" $ do
          let props = Map.singleton "write.metadata.metrics.column.id" "none"
              df =
                IP.dataFileFromParquet
                  mkParquetFm
                  schema
                  props
                  "s3://b/data.parquet"
                  4096
                  V.empty
                  Nothing
          Map.lookup 1 (dataFileColumnSizes df) `shouldBe` Nothing
          Map.lookup 2 (dataFileColumnSizes df) `shouldBe` Just 1500
      , it "pagesOverlappingDeletes: deletes inside one page" $ do
          let dv = DV.addPositions [25, 60] DV.emptyDV
              hits = IP.pagesOverlappingDeletes dv mkOffsetIndex
          -- Row 25 is in page 0 (rows 0..49); row 60 is in page 1 (rows 50..74).
          V.toList hits `shouldBe` [0, 1]
      , it "pagesOverlappingDeletes: empty deletion vector returns no pages" $ do
          let hits = IP.pagesOverlappingDeletes DV.emptyDV mkOffsetIndex
          V.toList hits `shouldBe` []
      , it "filterDeletedPages drops fully-deleted pages" $ do
          -- Delete every row in page 1 (rows 50..74).
          let dv = DV.addPositions [fromIntegral i | i <- [50 .. 74 :: Int]] DV.emptyDV
              surviving = IP.filterDeletedPages dv mkOffsetIndex 100
          V.toList surviving `shouldBe` [0, 2]
      , it "encryptionConfigFromTable copies keyId into encKeyMetadata" $ do
          let baseTable =
                TableMetadata
                  { tmFormatVersion = 3
                  , tmTableUuid = "u"
                  , tmLocation = "s3://b"
                  , tmLastSequenceNumber = 0
                  , tmLastUpdatedMs = 0
                  , tmLastColumnId = 0
                  , tmCurrentSchemaId = 0
                  , tmSchemas = V.singleton schema
                  , tmCurrentSnapshotId = Nothing
                  , tmSnapshots = V.empty
                  , tmPartitionSpecs = V.singleton (PartitionSpec 0 V.empty)
                  , tmDefaultSpecId = 0
                  , tmLastPartitionId = 0
                  , tmSortOrders = V.singleton (SortOrder 0 V.empty)
                  , tmDefaultSortOrderId = 0
                  , tmProperties = Map.empty
                  , tmSnapshotLog = V.empty
                  , tmMetadataLog = V.empty
                  , tmSnapshotRefs = Map.empty
                  , tmStatistics = V.empty
                  , tmPartitionStatistics = V.empty
                  , tmNextRowId = Nothing
                  , tmEncryptionKeys = Map.singleton "kek-1" "arn:aws:kms:::alias/iceberg"
                  }
              cfg =
                IP.encryptionConfigFromTable
                  baseTable
                  "kek-1"
                  (BS.replicate 16 0x01)
                  Map.empty
                  (BS.replicate 8 0x02)
          Enc.encKeyMetadata cfg `shouldBe` "arn:aws:kms:::alias/iceberg"
          let df =
                ( DataFile
                    DataContent
                    "s3://b/data.parquet"
                    ParquetFormat
                    V.empty
                    0
                    0
                    Map.empty
                    Map.empty
                    Map.empty
                    Map.empty
                    Map.empty
                    Map.empty
                    Nothing
                    V.empty
                    V.empty
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                )
              stamped = IP.withEncryptionKeyMetadata cfg df
          dataFileKeyMetadata stamped `shouldBe` Just "arn:aws:kms:::alias/iceberg"
      ]
