module Test.Iceberg (icebergTests) where

import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Avro.Container (writeContainer)
import Avro.Schema (AvroType (..))
import qualified Avro.Value as AV
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AKM
import Iceberg.JSON (metadataFromJSON, metadataToJSON)
import qualified Iceberg.RestCatalog as Rest
import Iceberg.Manifest (manifestEntrySchema, manifestFileSchema)
import Iceberg.Read
  ( applyPositionDeletes
  , dataManifestPaths
  , deleteManifestPaths
  , positionDeletesFromColumns
  , readManifestEntries
  , readManifestList
  , ScanPlan(..)
  , planScan
  )
import Iceberg.SchemaEvolution
  (currentSchema, findFieldById, projectSchema, schemaById)
import Iceberg.Snapshot
  ( applicableDeletes
  , currentPartitionSpec
  , currentSnapshot
  , filterBySequenceNumber
  , snapshotById
  , snapshotParentChain
  )
import Iceberg.Types

icebergTests :: TestTree
icebergTests = testGroup "Iceberg"
  [ jsonRoundtripTests
  , schemaTypeTests
  , partitionSpecTests
  , manifestSchemaTests
  , manifestAvroTests
  , snapshotTests
  , schemaEvolutionTests
  , deleteFileTests
  , positionDeleteTests
  , manifestPathFilterTests
  , sequenceNumberTests
  , scanPlanDeleteTests
  , restCatalogTests
  , snapshotRefTests
  ]

-- ============================================================
-- JSON roundtrip
-- ============================================================

jsonRoundtripTests :: TestTree
jsonRoundtripTests = testGroup "JSON roundtrip"
  [ testCase "Minimal table metadata roundtrip" $ do
      let tm = minimalMetadata
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Table metadata with snapshots" $ do
      let snap = Snapshot
            { snapId = 100
            , snapParentId = Nothing
            , snapSequenceNumber = 1
            , snapTimestampMs = 1672531200000
            , snapManifestList = "s3://bucket/manifest-list.avro"
            , snapSummary = Map.fromList [("operation", "append")]
            }
          tm = minimalMetadata
            { tmCurrentSnapshotId = Just 100
            , tmSnapshots = V.singleton snap
            , tmSnapshotLog = V.singleton (SnapshotLogEntry 1672531200000 100)
            }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Table metadata with snapshot parent" $ do
      let snap = Snapshot
            { snapId = 200
            , snapParentId = Just 100
            , snapSequenceNumber = 2
            , snapTimestampMs = 1672531300000
            , snapManifestList = "s3://bucket/manifest-list-2.avro"
            , snapSummary = Map.fromList [("operation", "overwrite")]
            }
          tm = minimalMetadata
            { tmCurrentSnapshotId = Just 200
            , tmSnapshots = V.singleton snap
            }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Table metadata with properties" $ do
      let tm = minimalMetadata
            { tmProperties = Map.fromList
                [ ("write.format.default", "parquet")
                , ("commit.retry.num-retries", "4")
                ]
            }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm
  ]

-- ============================================================
-- Schema with various types
-- ============================================================

schemaTypeTests :: TestTree
schemaTypeTests = testGroup "Schema types"
  [ testCase "Primitive types roundtrip" $ do
      let fields = V.fromList
            [ StructField 1 "bool_col" True TBoolean Nothing
            , StructField 2 "int_col" True TInt Nothing
            , StructField 3 "long_col" True TLong Nothing
            , StructField 4 "float_col" True TFloat Nothing
            , StructField 5 "double_col" True TDouble Nothing
            , StructField 6 "string_col" True TString Nothing
            , StructField 7 "binary_col" True TBinary Nothing
            , StructField 8 "uuid_col" False TUuid (Just "A UUID column")
            ]
          schema = Schema 0 fields
          tm = minimalMetadata { tmSchemas = V.singleton schema }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Date/time types roundtrip" $ do
      let fields = V.fromList
            [ StructField 1 "date_col" True TDate Nothing
            , StructField 2 "time_col" True TTime Nothing
            , StructField 3 "ts_col" True TTimestamp Nothing
            , StructField 4 "tstz_col" True TTimestampTz Nothing
            ]
          schema = Schema 0 fields
          tm = minimalMetadata { tmSchemas = V.singleton schema }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Fixed and Decimal types roundtrip" $ do
      let fields = V.fromList
            [ StructField 1 "fixed_col" True (TFixed 16) Nothing
            , StructField 2 "dec_col" True (TDecimal 10 2) Nothing
            ]
          schema = Schema 0 fields
          tm = minimalMetadata { tmSchemas = V.singleton schema }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Nested struct type roundtrip" $ do
      let innerFields = V.fromList
            [ StructField 10 "x" True TInt Nothing
            , StructField 11 "y" True TInt Nothing
            ]
          fields = V.fromList
            [ StructField 1 "point" True (TStruct innerFields) Nothing
            ]
          schema = Schema 0 fields
          tm = minimalMetadata { tmSchemas = V.singleton schema }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "List type roundtrip" $ do
      let fields = V.fromList
            [ StructField 1 "tags" True (TList 100 TString) Nothing
            ]
          schema = Schema 0 fields
          tm = minimalMetadata { tmSchemas = V.singleton schema }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Map type roundtrip" $ do
      let fields = V.fromList
            [ StructField 1 "attrs" True (TMap 100 TString 101 TLong) Nothing
            ]
          schema = Schema 0 fields
          tm = minimalMetadata { tmSchemas = V.singleton schema }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm
  ]

-- ============================================================
-- PartitionSpec with transforms
-- ============================================================

partitionSpecTests :: TestTree
partitionSpecTests = testGroup "PartitionSpec"
  [ testCase "Identity transform" $ do
      let ps = PartitionSpec 0 (V.singleton (PartitionField 1 1000 "id_part" Identity))
          tm = minimalMetadata { tmPartitionSpecs = V.singleton ps }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Bucket transform" $ do
      let ps = PartitionSpec 0 (V.singleton (PartitionField 1 1000 "bucket_part" (Bucket 16)))
          tm = minimalMetadata { tmPartitionSpecs = V.singleton ps }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Truncate transform" $ do
      let ps = PartitionSpec 0 (V.singleton (PartitionField 2 1001 "trunc_part" (Truncate 10)))
          tm = minimalMetadata { tmPartitionSpecs = V.singleton ps }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Time-based transforms" $ do
      let ps = PartitionSpec 0 (V.fromList
            [ PartitionField 1 1000 "year_part" Year
            , PartitionField 2 1001 "month_part" Month
            , PartitionField 3 1002 "day_part" Day
            , PartitionField 4 1003 "hour_part" Hour
            ])
          tm = minimalMetadata { tmPartitionSpecs = V.singleton ps }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Void transform" $ do
      let ps = PartitionSpec 0 (V.singleton (PartitionField 1 1000 "void_part" Void))
          tm = minimalMetadata { tmPartitionSpecs = V.singleton ps }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "Sort orders roundtrip" $ do
      let so = SortOrder 1 (V.fromList
            [ SortField 1 Identity Asc NullsFirst
            , SortField 2 (Bucket 8) Desc NullsLast
            ])
          tm = minimalMetadata { tmSortOrders = V.singleton so }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm
  ]

-- ============================================================
-- Manifest schema construction
-- ============================================================

manifestSchemaTests :: TestTree
manifestSchemaTests = testGroup "Manifest schemas"
  [ testCase "manifestEntrySchema is an AvroRecord" $
      case manifestEntrySchema of
        AvroRecord{avroRecordName = name} -> name @?= "manifest_entry"
        _ -> assertFailure "expected AvroRecord"

  , testCase "manifestFileSchema is an AvroRecord" $
      case manifestFileSchema of
        AvroRecord{avroRecordName = name} -> name @?= "manifest_file"
        _ -> assertFailure "expected AvroRecord"

  , testCase "manifestEntrySchema has correct namespace" $
      case manifestEntrySchema of
        AvroRecord{avroRecordNamespace = ns} -> ns @?= Just "org.apache.iceberg"
        _ -> assertFailure "expected AvroRecord"

  , testCase "manifestFileSchema has expected fields" $
      case manifestFileSchema of
        AvroRecord{avroRecordFields = fields} ->
          V.length fields > 0 @?= True
        _ -> assertFailure "expected AvroRecord"
  ]

-- ============================================================
-- Avro manifest / manifest-list containers
-- ============================================================

manifestAvroTests :: TestTree
manifestAvroTests = testGroup "Manifest Avro"
  [ testCase "roundtrip manifest entry container" $ do
      let u0 = AV.Union 0 AV.Null
          dataFile =
            AV.Record $
              V.fromList
                [ AV.String "s3://b/f.parquet"
                , AV.String "parquet"
                , AV.Record V.empty
                , AV.Long 42
                , AV.Long 2048
                , AV.Long 67108864
                , u0
                , u0
                , u0
                , u0
                , u0
                ]
          entry =
            AV.Record $
              V.fromList
                [ AV.Int 1
                , AV.Union 1 (AV.Long 99)
                , u0
                , AV.Union 1 (AV.Long 7)
                , dataFile
                ]
          bs = writeContainer manifestEntrySchema (V.singleton entry)
      case readManifestEntries bs of
        Left e -> assertFailure e
        Right (_, vec) -> do
          V.length vec @?= 1
          let me = V.unsafeIndex vec 0
          meStatus me @?= Added
          meSnapshotId me @?= Just 99
          meSequenceNumber me @?= Nothing
          meFileSequenceNumber me @?= Just 7
          meFilePath me @?= "s3://b/f.parquet"
          meFileFormat me @?= ParquetFormat
          meRecordCount me @?= 42
          meFileSizeBytes me @?= 2048
          V.length (mePartition me) @?= 0

  , testCase "roundtrip manifest list container" $ do
      let u0 = AV.Union 0 AV.Null
          mf =
            AV.Record $
              V.fromList
                [ AV.String "s3://bucket/m1.avro"
                , AV.Long 500
                , AV.Int 0
                , AV.Int 0
                , AV.Long 10
                , AV.Long 5
                , AV.Long 99
                , u0
                , u0
                , u0
                , u0
                , u0
                , u0
                ]
          bs = writeContainer manifestFileSchema (V.singleton mf)
      case readManifestList bs of
        Left e -> assertFailure e
        Right (_, vec) -> do
          V.length vec @?= 1
          let m = V.unsafeIndex vec 0
          mfPath m @?= "s3://bucket/m1.avro"
          mfLength m @?= 500
          mfPartitionSpecId m @?= 0
          mfContent m @?= DataContent
          mfSequenceNumber m @?= 10
          mfMinSequenceNumber m @?= 5
          mfAddedSnapshotId m @?= 99
          mfAddedDataFilesCount m @?= Nothing
          mfExistingDataFilesCount m @?= Nothing
          mfDeletedDataFilesCount m @?= Nothing
          mfAddedRowsCount m @?= Nothing
          mfExistingRowsCount m @?= Nothing
          mfDeletedRowsCount m @?= Nothing
  ]

-- ============================================================
-- Snapshot operations
-- ============================================================

snapshotTests :: TestTree
snapshotTests = testGroup "Snapshot operations"
  [ testCase "currentSnapshot returns matching snapshot" $ do
      let snap = Snapshot
            { snapId = 42
            , snapParentId = Nothing
            , snapSequenceNumber = 1
            , snapTimestampMs = 1000
            , snapManifestList = "s3://bucket/ml.avro"
            , snapSummary = Map.singleton "operation" "append"
            }
          tm = minimalMetadata
            { tmCurrentSnapshotId = Just 42
            , tmSnapshots = V.singleton snap
            }
      currentSnapshot tm @?= Just snap

  , testCase "currentSnapshot returns Nothing when no current ID" $ do
      let tm = minimalMetadata { tmCurrentSnapshotId = Nothing }
      currentSnapshot tm @?= Nothing

  , testCase "currentSnapshot returns Nothing for missing ID" $ do
      let snap = Snapshot 1 Nothing 1 1000 "ml.avro" Map.empty
          tm = minimalMetadata
            { tmCurrentSnapshotId = Just 999
            , tmSnapshots = V.singleton snap
            }
      currentSnapshot tm @?= Nothing

  , testCase "snapshotById finds the right snapshot" $ do
      let s1 = Snapshot 10 Nothing 1 1000 "ml1.avro" Map.empty
          s2 = Snapshot 20 (Just 10) 2 2000 "ml2.avro" Map.empty
          tm = minimalMetadata { tmSnapshots = V.fromList [s1, s2] }
      snapshotById tm 20 @?= Just s2
      snapshotById tm 10 @?= Just s1
      snapshotById tm 99 @?= Nothing

  , testCase "snapshotParentChain with 3 snapshots" $ do
      let s1 = Snapshot 1 Nothing   1 1000 "ml1.avro" Map.empty
          s2 = Snapshot 2 (Just 1)  2 2000 "ml2.avro" Map.empty
          s3 = Snapshot 3 (Just 2)  3 3000 "ml3.avro" Map.empty
          tm = minimalMetadata
            { tmSnapshots = V.fromList [s1, s2, s3] }
          chain = snapshotParentChain tm s3
      case chain of
        [p1, p2] -> do
          snapId p1 @?= 2
          snapId p2 @?= 1
        _ -> assertFailure ("expected 2-element chain, got " ++ show (length chain))

  , testCase "snapshotParentChain stops at root" $ do
      let s1 = Snapshot 1 Nothing 1 1000 "ml1.avro" Map.empty
          tm = minimalMetadata { tmSnapshots = V.singleton s1 }
      snapshotParentChain tm s1 @?= []

  , testCase "currentPartitionSpec returns default spec" $ do
      let ps = PartitionSpec 5 (V.singleton (PartitionField 1 1000 "p" Identity))
          tm = minimalMetadata
            { tmPartitionSpecs = V.fromList
                [ PartitionSpec 0 V.empty
                , ps
                ]
            , tmDefaultSpecId = 5
            }
      currentPartitionSpec tm @?= Just ps

  , testCase "currentPartitionSpec returns Nothing for missing ID" $ do
      let tm = minimalMetadata { tmDefaultSpecId = 99 }
      currentPartitionSpec tm @?= Nothing
  ]

-- ============================================================
-- Schema evolution
-- ============================================================

schemaEvolutionTests :: TestTree
schemaEvolutionTests = testGroup "Schema evolution"
  [ testCase "schemaById lookup" $ do
      let s0 = Schema 0 V.empty
          s1 = Schema 1 (V.singleton (StructField 1 "x" True TInt Nothing))
          tm = minimalMetadata { tmSchemas = V.fromList [s0, s1] }
      schemaById tm 1 @?= Just s1
      schemaById tm 0 @?= Just s0
      schemaById tm 99 @?= Nothing

  , testCase "currentSchema matches tmCurrentSchemaId" $ do
      let s0 = Schema 0 V.empty
          s1 = Schema 1 (V.singleton (StructField 1 "x" True TLong Nothing))
          tm = minimalMetadata
            { tmSchemas = V.fromList [s0, s1]
            , tmCurrentSchemaId = 1
            }
      fmap schemaId (currentSchema tm) @?= Just 1

  , testCase "findFieldById at top level" $ do
      let fields = V.fromList
            [ StructField 1 "a" True TInt Nothing
            , StructField 2 "b" True TString Nothing
            ]
          schema = Schema 0 fields
      fmap sfName (findFieldById schema 2) @?= Just "b"
      findFieldById schema 99 @?= Nothing

  , testCase "findFieldById in nested struct" $ do
      let inner = V.fromList
            [ StructField 10 "x" True TInt Nothing
            , StructField 11 "y" True TInt Nothing
            ]
          fields = V.fromList
            [ StructField 1 "id" True TLong Nothing
            , StructField 2 "point" True (TStruct inner) Nothing
            ]
          schema = Schema 0 fields
      fmap sfName (findFieldById schema 11) @?= Just "y"
      fmap sfName (findFieldById schema 10) @?= Just "x"
      fmap sfName (findFieldById schema 1) @?= Just "id"

  , testCase "findFieldById in list element struct" $ do
      let elemFields = V.fromList
            [ StructField 20 "name" True TString Nothing
            , StructField 21 "val" True TDouble Nothing
            ]
          fields = V.singleton
            (StructField 1 "items" True (TList 100 (TStruct elemFields)) Nothing)
          schema = Schema 0 fields
      fmap sfName (findFieldById schema 20) @?= Just "name"
      fmap sfName (findFieldById schema 21) @?= Just "val"

  , testCase "findFieldById in map value struct" $ do
      let valFields = V.singleton (StructField 30 "score" True TFloat Nothing)
          fields = V.singleton
            (StructField 1 "scores" True
              (TMap 100 TString 101 (TStruct valFields)) Nothing)
          schema = Schema 0 fields
      fmap sfName (findFieldById schema 30) @?= Just "score"

  , testCase "projectSchema keeping subset of fields" $ do
      let fields = V.fromList
            [ StructField 1 "a" True TInt Nothing
            , StructField 2 "b" True TString Nothing
            , StructField 3 "c" True TLong Nothing
            , StructField 4 "d" False TDouble Nothing
            ]
          schema = Schema 0 fields
      case projectSchema schema [2, 4] of
        Left e -> assertFailure e
        Right projected -> do
          V.length (schemaFields projected) @?= 2
          sfName (V.unsafeIndex (schemaFields projected) 0) @?= "b"
          sfName (V.unsafeIndex (schemaFields projected) 1) @?= "d"

  , testCase "projectSchema with empty field list" $ do
      let schema = Schema 0 (V.singleton (StructField 1 "a" True TInt Nothing))
      case projectSchema schema [] of
        Left _  -> assertFailure "empty field list should succeed"
        Right s -> V.length (schemaFields s) @?= 0

  , testCase "projectSchema with no matching IDs fails" $ do
      let schema = Schema 0 (V.singleton (StructField 1 "a" True TInt Nothing))
      case projectSchema schema [99, 100] of
        Left _  -> pure ()
        Right _ -> assertFailure "expected Left for no matching IDs"

  , testCase "JSON roundtrip -> currentSnapshot -> verify" $ do
      let snap1 = Snapshot 10 Nothing  1 1000 "s3://b/ml1.avro"
                    (Map.singleton "operation" "append")
          snap2 = Snapshot 20 (Just 10) 2 2000 "s3://b/ml2.avro"
                    (Map.singleton "operation" "overwrite")
          s0 = Schema 0 (V.singleton (StructField 1 "id" True TLong Nothing))
          s1 = Schema 1 (V.fromList
                 [ StructField 1 "id" True TLong Nothing
                 , StructField 2 "name" True TString Nothing
                 ])
          tm = minimalMetadata
            { tmCurrentSnapshotId = Just 20
            , tmSnapshots = V.fromList [snap1, snap2]
            , tmCurrentSchemaId = 1
            , tmSchemas = V.fromList [s0, s1]
            , tmSnapshotLog = V.fromList
                [ SnapshotLogEntry 1000 10
                , SnapshotLogEntry 2000 20
                ]
            }
          json = metadataToJSON tm
      case metadataFromJSON json of
        Left e -> assertFailure e
        Right tm' -> do
          tm' @?= tm
          let mSnap = currentSnapshot tm'
          fmap snapId mSnap @?= Just 20
          fmap snapParentId mSnap @?= Just (Just 10)
          let mSchema = currentSchema tm'
          fmap schemaId mSchema @?= Just 1
          fmap (V.length . schemaFields) mSchema @?= Just 2
  ]

-- ============================================================
-- Delete file types
-- ============================================================

deleteFileTests :: TestTree
deleteFileTests = testGroup "Delete file types"
  [ testCase "DeleteFile construction" $ do
      let df = DeleteFile
            { dfFilePath = "s3://b/del.parquet"
            , dfFileFormat = ParquetFormat
            , dfContent = PositionDeletes
            , dfRecordCount = 100
            , dfFileSizeInBytes = 4096
            , dfEqualityFieldIds = V.empty
            , dfPartition = Map.empty
            , dfSequenceNumber = Just 5
            }
      dfFilePath df @?= "s3://b/del.parquet"
      dfContent df @?= PositionDeletes
      dfRecordCount df @?= 100

  , testCase "EqualityDeletes variant" $ do
      let df = DeleteFile
            { dfFilePath = "s3://b/eq-del.parquet"
            , dfFileFormat = ParquetFormat
            , dfContent = EqualityDeletes
            , dfRecordCount = 50
            , dfFileSizeInBytes = 2048
            , dfEqualityFieldIds = V.fromList [1, 2, 3 :: Int32]
            , dfPartition = Map.singleton "region" (AV.String "us-east")
            , dfSequenceNumber = Nothing
            }
      dfContent df @?= EqualityDeletes
      V.length (dfEqualityFieldIds df) @?= 3
  ]

-- ============================================================
-- Position deletes
-- ============================================================

positionDeleteTests :: TestTree
positionDeleteTests = testGroup "Position deletes"
  [ testCase "positionDeletesFromColumns zips correctly" $ do
      let paths = V.fromList ["f1.parquet", "f1.parquet", "f2.parquet"]
          positions = V.fromList [0, 5, 3]
          pds = positionDeletesFromColumns paths positions
      V.length pds @?= 3
      pdFilePath (V.unsafeIndex pds 0) @?= "f1.parquet"
      pdPosition (V.unsafeIndex pds 0) @?= 0
      pdFilePath (V.unsafeIndex pds 2) @?= "f2.parquet"
      pdPosition (V.unsafeIndex pds 2) @?= 3

  , testCase "applyPositionDeletes removes correct rows" $ do
      let deletes = V.fromList
            [ PositionDelete "data.parquet" 1
            , PositionDelete "data.parquet" 3
            , PositionDelete "other.parquet" 0
            ]
          rows = V.fromList ["a", "b", "c", "d", "e" :: String]
          result = applyPositionDeletes deletes "data.parquet" rows
      result @?= V.fromList ["a", "c", "e"]

  , testCase "applyPositionDeletes with no matching deletes" $ do
      let deletes = V.fromList [PositionDelete "other.parquet" 0]
          rows = V.fromList [10, 20, 30 :: Int]
      applyPositionDeletes deletes "data.parquet" rows @?= rows

  , testCase "applyPositionDeletes with empty deletes" $ do
      let rows = V.fromList [1, 2, 3 :: Int]
      applyPositionDeletes V.empty "data.parquet" rows @?= rows
  ]

-- ============================================================
-- Manifest path filtering
-- ============================================================

manifestPathFilterTests :: TestTree
manifestPathFilterTests = testGroup "Manifest path filtering"
  [ testCase "deleteManifestPaths filters to DeletesContent" $ do
      let mfs = V.fromList
            [ mkManifestFile "m1.avro" DataContent 10
            , mkManifestFile "m2.avro" DeletesContent 10
            , mkManifestFile "m3.avro" DataContent 10
            , mkManifestFile "m4.avro" DeletesContent 10
            ]
      deleteManifestPaths mfs @?= V.fromList ["m2.avro", "m4.avro"]

  , testCase "dataManifestPaths filters to DataContent" $ do
      let mfs = V.fromList
            [ mkManifestFile "m1.avro" DataContent 10
            , mkManifestFile "m2.avro" DeletesContent 10
            ]
      dataManifestPaths mfs @?= V.fromList ["m1.avro"]

  , testCase "both filters on empty input" $ do
      deleteManifestPaths V.empty @?= V.empty
      dataManifestPaths V.empty @?= V.empty
  ]

-- ============================================================
-- Sequence number filtering
-- ============================================================

sequenceNumberTests :: TestTree
sequenceNumberTests = testGroup "Sequence number filtering"
  [ testCase "filterBySequenceNumber keeps entries <= threshold" $ do
      let entries = V.fromList
            [ mkEntry (Just 3) "f1.parquet"
            , mkEntry (Just 5) "f2.parquet"
            , mkEntry (Just 7) "f3.parquet"
            , mkEntry Nothing  "f4.parquet"
            ]
          result = filterBySequenceNumber 5 entries
      V.length result @?= 3
      V.map meFilePath result @?= V.fromList ["f1.parquet", "f2.parquet", "f4.parquet"]

  , testCase "applicableDeletes filters delete manifests by sequence number" $ do
      let snap = Snapshot 1 Nothing 5 1000 "ml.avro" Map.empty
          mfs = V.fromList
            [ mkManifestFile "d1.avro" DeletesContent 3
            , mkManifestFile "d2.avro" DeletesContent 7
            , mkManifestFile "d3.avro" DataContent 3
            , mkManifestFile "d4.avro" DeletesContent 5
            ]
          result = applicableDeletes snap mfs
      V.length result @?= 2
      V.map mfPath result @?= V.fromList ["d1.avro", "d4.avro"]
  ]

-- ============================================================
-- ScanPlan with delete file paths
-- ============================================================

scanPlanDeleteTests :: TestTree
scanPlanDeleteTests = testGroup "ScanPlan with deletes"
  [ testCase "planScan sets empty delete file paths" $ do
      let snap = Snapshot 1 Nothing 1 1000 "s3://b/ml.avro" (Map.singleton "operation" "append")
          tm = minimalMetadata
            { tmCurrentSnapshotId = Just 1
            , tmSnapshots = V.singleton snap
            , tmSchemas = V.singleton (Schema 0 V.empty)
            }
          u0 = AV.Union 0 AV.Null
          dataFile = AV.Record $ V.fromList
            [ AV.String "s3://b/data.parquet", AV.String "parquet"
            , AV.Record V.empty, AV.Long 10, AV.Long 1024
            , AV.Long 67108864, u0, u0, u0, u0, u0
            ]
          entry = AV.Record $ V.fromList [AV.Int 1, u0, u0, u0, dataFile]
          manifestBs = writeContainer manifestEntrySchema (V.singleton entry)
          mfRec = AV.Record $ V.fromList
            [ AV.String "manifest.avro", AV.Long 500, AV.Int 0, AV.Int 0
            , AV.Long 1, AV.Long 1, AV.Long 1, u0, u0, u0, u0, u0, u0
            ]
          mlBs = writeContainer manifestFileSchema (V.singleton mfRec)
          readManifest _ = Right manifestBs
      case planScan tm mlBs readManifest of
        Left e -> assertFailure e
        Right sp -> do
          spDeleteFilePaths sp @?= V.empty
          V.length (spDataFilePaths sp) @?= 1
  ]

-- ============================================================
-- SnapshotRef JSON roundtrip
-- ============================================================

snapshotRefTests :: TestTree
snapshotRefTests = testGroup "SnapshotRef"
  [ testCase "JSON roundtrip with snapshot refs" $ do
      let ref = SnapshotRef
            { srSnapshotId = 100
            , srType = "branch"
            , srMaxRefAgeMs = Just 86400000
            , srMaxSnapshotAgeMs = Nothing
            , srMinSnapshotsToKeep = Just 5
            }
          tm = minimalMetadata
            { tmSnapshotRefs = Map.singleton "main" ref }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "JSON roundtrip with empty snapshot refs" $ do
      let tm = minimalMetadata
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm

  , testCase "JSON roundtrip with format version 2 and all v2 fields" $ do
      let ref = SnapshotRef
            { srSnapshotId = 200
            , srType = "tag"
            , srMaxRefAgeMs = Nothing
            , srMaxSnapshotAgeMs = Just 172800000
            , srMinSnapshotsToKeep = Nothing
            }
          snap = Snapshot 200 Nothing 10 2000 "s3://b/ml.avro" Map.empty
          tm = minimalMetadata
            { tmFormatVersion = 2
            , tmLastSequenceNumber = 10
            , tmCurrentSnapshotId = Just 200
            , tmSnapshots = V.singleton snap
            , tmSnapshotRefs = Map.fromList
                [ ("main", ref { srSnapshotId = 200, srType = "branch" })
                , ("v1.0", ref)
                ]
            }
          json = metadataToJSON tm
      metadataFromJSON json @?= Right tm
  ]

-- ============================================================
-- Helpers
-- ============================================================

mkManifestFile :: Text -> ManifestContent -> Int64 -> ManifestFile
mkManifestFile path content seqNum = ManifestFile
  { mfPath = path
  , mfLength = 1000
  , mfPartitionSpecId = 0
  , mfContent = content
  , mfSequenceNumber = seqNum
  , mfMinSequenceNumber = 0
  , mfAddedSnapshotId = 1
  , mfAddedDataFilesCount = Nothing
  , mfExistingDataFilesCount = Nothing
  , mfDeletedDataFilesCount = Nothing
  , mfAddedRowsCount = Nothing
  , mfExistingRowsCount = Nothing
  , mfDeletedRowsCount = Nothing
  }

mkEntry :: Maybe Int64 -> Text -> ManifestEntry
mkEntry seqNo path = ManifestEntry
  { meStatus = Added
  , meSnapshotId = Just 1
  , meSequenceNumber = seqNo
  , meFileSequenceNumber = Nothing
  , meFilePath = path
  , meFileFormat = ParquetFormat
  , mePartition = V.empty
  , meRecordCount = 100
  , meFileSizeBytes = 4096
  }

minimalMetadata :: TableMetadata
minimalMetadata = TableMetadata
  { tmFormatVersion      = 2
  , tmTableUuid          = "550e8400-e29b-41d4-a716-446655440000"
  , tmLocation           = "s3://bucket/table"
  , tmLastSequenceNumber = 0
  , tmLastUpdatedMs      = 1672531200000
  , tmLastColumnId       = 0
  , tmCurrentSchemaId    = 0
  , tmSchemas            = V.singleton (Schema 0 V.empty)
  , tmCurrentSnapshotId  = Nothing
  , tmSnapshots          = V.empty
  , tmPartitionSpecs     = V.singleton (PartitionSpec 0 V.empty)
  , tmDefaultSpecId      = 0
  , tmSortOrders         = V.singleton (SortOrder 0 V.empty)
  , tmDefaultSortOrderId = 0
  , tmProperties         = Map.empty
  , tmSnapshotLog        = V.empty
  , tmSnapshotRefs       = Map.empty
  }

-- ============================================================
-- REST catalog protocol tests
-- ============================================================

restCatalogTests :: TestTree
restCatalogTests = testGroup "REST catalog protocol"
  [ testCase "namespacePath / tablePath" $ do
      let ns = Rest.Namespace (V.fromList ["accounting", "tax"])
          ti = Rest.TableIdentifier ns "ledger"
      Rest.namespacePath ns @?= "/v1/namespaces/accounting%1Ftax"
      Rest.tablePath ti     @?= "/v1/namespaces/accounting%1Ftax/tables/ledger"
      Rest.tablesPath ns    @?= "/v1/namespaces/accounting%1Ftax/tables"

  , testCase "ListNamespacesResp parses page-token form" $ do
      let body = "{\"namespaces\":[[\"a\"],[\"b\",\"c\"]],\"next-page-token\":\"tok\"}"
      case Aeson.eitherDecodeStrict body of
        Left e -> assertFailure e
        Right r -> do
          V.length (Rest.lnpNamespaces r) @?= 2
          Rest.lnpNextToken r @?= Just "tok"

  , testCase "createNamespace request body shape" $ do
      let req = Rest.CreateNamespaceReq
                  { Rest.cnrNamespace  = Rest.Namespace (V.singleton "books")
                  , Rest.cnrProperties = Map.fromList [("owner","alice")]
                  }
          body = Aeson.encode req
      case Aeson.eitherDecode body of
        Left e -> assertFailure ("decode: " ++ e)
        Right (Aeson.Object o) -> do
          AKM.lookup "namespace"  o /= Nothing @? "namespace field present"
          AKM.lookup "properties" o /= Nothing @? "properties field present"
        Right _ -> assertFailure "expected object"

  , testCase "TableUpdate.AddSchema serializes with action tag" $ do
      let upd = Rest.AddSchema
                  (Schema 7 (V.singleton (StructField 1 "x" True TInt Nothing)))
                  (Just 1)
          body = Aeson.encode upd
      case Aeson.eitherDecode body of
        Left e -> assertFailure ("decode: " ++ e)
        Right (Aeson.Object o) ->
          (AKM.lookup "action" o == Just (Aeson.String "add-schema"))
            @? "action == add-schema"
        Right _ -> assertFailure "expected object"

  , testCase "TableRequirement.AssertCreate serializes with type tag" $ do
      let body = Aeson.encode Rest.AssertCreate
      case Aeson.eitherDecode body of
        Left e -> assertFailure ("decode: " ++ e)
        Right (Aeson.Object o) ->
          (AKM.lookup "type" o == Just (Aeson.String "assert-create"))
            @? "type == assert-create"
        Right _ -> assertFailure "expected object"

  , testCase "Pure-IO mock: dropTable round-trips through CatalogClient" $ do
      let mockPerform req
            | Rest.reqMethod req == Rest.DELETE
            , Rest.reqPath req == "/v1/namespaces/foo/tables/bar"
            = pure (Rest.Response 204 "")
            | otherwise
            = pure (Rest.Response 500 "{\"error\":{\"message\":\"unexpected\",\"type\":\"x\",\"code\":500}}")
          cc = Rest.CatalogClient
                 { Rest.ccBaseUrl = "https://example/"
                 , Rest.ccAuth    = Rest.AuthBearer "abc"
                 , Rest.ccPerform = mockPerform
                 }
          ti = Rest.TableIdentifier (Rest.Namespace (V.singleton "foo")) "bar"
      result <- Rest.dropTable cc ti
      case result of
        Right () -> pure ()
        Left e -> assertFailure ("dropTable failed: " ++ show e)
  ]
