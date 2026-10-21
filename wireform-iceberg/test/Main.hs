module Main (main) where

import Hedgehog.Internal.Config (resolveSkip)
import Test.Iceberg.BoundTrunc qualified
import Test.Iceberg.CatalogGlue qualified
import Test.Iceberg.CatalogHadoop qualified
import Test.Iceberg.CatalogSql qualified
import Test.Iceberg.Delete qualified
import Test.Iceberg.DeletionVector qualified
import Test.Iceberg.EncryptionProperty qualified
import Test.Iceberg.Expression qualified
import Test.Iceberg.Geometry qualified
import Test.Iceberg.Hash qualified
import Test.Iceberg.Incremental qualified
import Test.Iceberg.Maintenance qualified
import Test.Iceberg.ManifestMerge qualified
import Test.Iceberg.MetricsConfig qualified
import Test.Iceberg.Murmur3 qualified
import Test.Iceberg.NameMapping qualified
import Test.Iceberg.NestedProperty qualified
import Test.Iceberg.Parquet qualified
import Test.Iceberg.Partition qualified
import Test.Iceberg.Puffin qualified
import Test.Iceberg.RESTCatalog qualified
import Test.Iceberg.RESTClient qualified
import Test.Iceberg.ScanPlan qualified
import Test.Iceberg.SchemaCompat qualified
import Test.Iceberg.SequenceInheritance qualified
import Test.Iceberg.SingleValue qualified
import Test.Iceberg.SnapshotHistory qualified
import Test.Iceberg.SnapshotSummary qualified
import Test.Iceberg.Sort qualified
import Test.Iceberg.Transform qualified
import Test.Iceberg.Update qualified
import Test.Iceberg.Validate qualified
import Test.Iceberg.Variant qualified
import Test.Iceberg.VariantParquet qualified
import Test.Iceberg.VariantProperty qualified
import Test.Iceberg.VariantShredding qualified
import Test.Iceberg.View qualified
import Test.Iceberg.Write qualified
import Test.Syd


main :: IO ()
main = do
  -- Force Hedgehog's "HEDGEHOG_SKIP" CAF before sydtest starts its workers.
  -- Every property forces it first; when many capabilities hit it at once,
  -- GHC 9.8.4's RTS can lose the wakeup of a thread blocked on it, leaving
  -- one property hung until the per-test timeout.
  _ <- resolveSkip Nothing
  sydTest $
    describe "wireform-iceberg" $
      sequence_
        [ Test.Iceberg.Murmur3.tests
        , Test.Iceberg.SingleValue.tests
        , Test.Iceberg.Transform.tests
        , Test.Iceberg.Expression.tests
        , Test.Iceberg.Incremental.tests
        , Test.Iceberg.Maintenance.tests
        , Test.Iceberg.Write.tests
        , Test.Iceberg.Update.tests
        , Test.Iceberg.View.tests
        , Test.Iceberg.Puffin.tests
        , Test.Iceberg.DeletionVector.tests
        , Test.Iceberg.RESTCatalog.tests
        , Test.Iceberg.RESTClient.tests
        , Test.Iceberg.NameMapping.tests
        , Test.Iceberg.Parquet.tests
        , Test.Iceberg.ScanPlan.tests
        , Test.Iceberg.SequenceInheritance.tests
        , Test.Iceberg.SnapshotHistory.tests
        , Test.Iceberg.SnapshotSummary.tests
        , Test.Iceberg.Partition.tests
        , Test.Iceberg.Sort.tests
        , Test.Iceberg.ManifestMerge.tests
        , Test.Iceberg.MetricsConfig.tests
        , Test.Iceberg.BoundTrunc.tests
        , Test.Iceberg.CatalogGlue.tests
        , Test.Iceberg.CatalogHadoop.tests
        , Test.Iceberg.CatalogSql.tests
        , Test.Iceberg.Delete.tests
        , Test.Iceberg.Geometry.tests
        , Test.Iceberg.Hash.tests
        , Test.Iceberg.SchemaCompat.tests
        , Test.Iceberg.Validate.tests
        , Test.Iceberg.EncryptionProperty.tests
        , Test.Iceberg.NestedProperty.tests
        , Test.Iceberg.Variant.tests
        , Test.Iceberg.VariantParquet.tests
        , Test.Iceberg.VariantProperty.tests
        , Test.Iceberg.VariantShredding.tests
        ]
