# wireform-iceberg

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

[Apache Iceberg](https://iceberg.apache.org/) for Haskell. Table
metadata ([`Iceberg.Types`](src/Iceberg/Types.hs)), snapshots
([`Iceberg.Snapshot`](src/Iceberg/Snapshot.hs)) and time-travel,
manifest list and manifest readers / writers
([`Iceberg.Manifest`](src/Iceberg/Manifest.hs),
[`Iceberg.ManifestMerge`](src/Iceberg/ManifestMerge.hs)),
partition specs and transforms
([`Iceberg.Partition`](src/Iceberg/Partition.hs),
[`Iceberg.Transform`](src/Iceberg/Transform.hs)),
the expression evaluator
([`Iceberg.Expression`](src/Iceberg/Expression.hs)),
schema compatibility and evolution rules
([`Iceberg.SchemaCompat`](src/Iceberg/SchemaCompat.hs),
[`Iceberg.SchemaEvolution`](src/Iceberg/SchemaEvolution.hs)),
the row-level delete surface
([`Iceberg.Delete`](src/Iceberg/Delete.hs),
[`Iceberg.DeletionVector`](src/Iceberg/DeletionVector.hs)),
the Variant binary type
([`Iceberg.Variant`](src/Iceberg/Variant.hs)) with Parquet and
shredding integrations, the Puffin index format
([`Iceberg.Puffin`](src/Iceberg/Puffin.hs)),
table maintenance and compaction
([`Iceberg.Maintenance`](src/Iceberg/Maintenance.hs)),
JSON serialisation
([`Iceberg.JSON`](src/Iceberg/JSON.hs)),
read / write entry points
([`Iceberg.Read`](src/Iceberg/Read.hs),
[`Iceberg.Write`](src/Iceberg/Write.hs)),
catalogs for Glue / Hadoop / REST / SQL
([`Iceberg.Catalog.*`](src/Iceberg/Catalog/)),
and an annotation-driven deriver
([`Iceberg.Derive`](src/Iceberg/Derive.hs)).

Iceberg is the table format that data warehouses adopted to make
data lakes act like tables: ACID transactions, schema evolution,
hidden partitioning, time travel, snapshot isolation, and
unique-id-driven schema versioning, all on top of object storage.
The implementation is a metadata layer sitting on top of Parquet,
Avro, ORC, and JSON files: a Parquet data file, a manifest entry
referencing it (in Avro), a manifest list referencing the manifest
(also Avro), a snapshot referencing the manifest list, and a
table metadata JSON document referencing the snapshot.
wireform-iceberg implements every layer of that stack and the
catalogs that point at it.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-iceberg,
  wireform-parquet,     -- data files are Parquet
  wireform-avro,        -- manifests + manifest lists are Avro
  wireform-derive,      -- only if you want the cross-format annotation deriver
```

The REST catalog HTTP client is on by default, behind the
`+rest-client` flag (which is `default: True`). Disable with
`-f-rest-client` if you want to skip the `http-client` /
`http-client-tls` / `http-types` / `case-insensitive` dep tree.

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-iceberg` to
compile locally.

## Hello world

Build an Iceberg `TableMetadata`, serialise it to JSON, parse it back:

```haskell
import qualified Data.Map.Strict as Map
import qualified Data.Vector     as V
import qualified Iceberg.Types as Ice
import qualified Iceberg.JSON  as IJ
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  let schema = Ice.Schema
        { Ice.schemaId = 0
        , Ice.schemaFields = V.fromList
            [ mkField 1 "id"   True  Ice.TLong
            , mkField 2 "name" True  Ice.TString
            , mkField 3 "ts"   False Ice.TTimestamp
            ]
        , Ice.schemaIdentifierFieldIds = V.empty
        }
      metadata = Ice.TableMetadata
        { Ice.tmFormatVersion = 2
        , Ice.tmTableUuid     = "550e8400-e29b-41d4-a716-446655440000"
        , Ice.tmLocation      = "s3://bucket/warehouse/db/table"
        , Ice.tmCurrentSchemaId = 0
        , Ice.tmSchemas       = V.singleton schema
        , Ice.tmCurrentSnapshotId = Nothing
        , Ice.tmSnapshots     = V.empty
        , Ice.tmPartitionSpecs = V.singleton (Ice.PartitionSpec 0 V.empty)
        , Ice.tmDefaultSpecId  = 0
        , Ice.tmSortOrders     = V.singleton (Ice.SortOrder 0 V.empty)
        , Ice.tmDefaultSortOrderId = 0
        , Ice.tmProperties     = Map.singleton "owner" "analytics"
        , -- ... abbreviated
          Ice.tmLastSequenceNumber = 0, Ice.tmLastUpdatedMs = 0
        , Ice.tmLastColumnId   = 3, Ice.tmLastPartitionId = 0
        , Ice.tmSnapshotLog    = V.empty, Ice.tmMetadataLog = V.empty
        , Ice.tmSnapshotRefs   = Map.empty, Ice.tmStatistics = V.empty
        , Ice.tmPartitionStatistics = V.empty, Ice.tmNextRowId = Nothing
        , Ice.tmEncryptionKeys = Map.empty
        }
      json = IJ.metadataToJSON metadata
  case IJ.metadataFromJSON json of
    Right tm  -> putStrLn $ "format-version=" ++ show (Ice.tmFormatVersion tm)
    Left  err -> putStrLn err
  where
    mkField i n req t = Ice.StructField i n req t Nothing Nothing Nothing
```

The runnable version lives in [`examples/IcebergExample.hs`](../examples/IcebergExample.hs).

For an end-to-end pipeline that builds a real table from Parquet
data files, see [`examples/IcebergPipeline.hs`](../examples/IcebergPipeline.hs).

## What's in here

| Module                         | Role                                                      |
|--------------------------------|-----------------------------------------------------------|
| `Iceberg.Types`                | Table metadata AST: `TableMetadata`, `Schema`, `StructField`, `Snapshot`, `PartitionSpec`, `SortOrder`, `Type` |
| `Iceberg.JSON`                 | `metadataToJSON` / `metadataFromJSON`: round-trip table metadata through Iceberg's canonical JSON form |
| `Iceberg.Snapshot`             | `currentSnapshot`, `snapshotById`, `snapshotByRef`, `snapshotAsOfTime` (time travel), `snapshotParentChain`, `ancestorsOf`, `currentAncestors`, `snapshotsBetween`, `isAncestor`, `snapshotManifestListPath` |
| `Iceberg.Manifest`             | Manifest entry reader / writer (Avro-encoded) |
| `Iceberg.ManifestMerge`        | Manifest compaction and merging (the half of table maintenance that touches manifests) |
| `Iceberg.Partition`            | Partition spec evaluation, `evaluatePartitionFilter` |
| `Iceberg.Transform`            | Iceberg's partition transforms: `identity`, `bucket[N]`, `truncate[W]`, `year`, `month`, `day`, `hour`, `void` |
| `Iceberg.Expression`           | Iceberg expression AST + evaluator (the Iceberg-shaped predicate language; consumes the same shape as `Columnar.Predicate` underneath) |
| `Iceberg.Update`               | Table-update primitives: schema updates, snapshot adds, partition spec changes |
| `Iceberg.Validate`             | Spec-conformance checks against `TableMetadata` |
| `Iceberg.Read`                 | Read entry points: scan planning, manifest walking, file selection |
| `Iceberg.Write`                | Write entry points: snapshot construction, manifest emission, metadata commit |
| `Iceberg.Maintenance`          | `expireSnapshots`, file compaction planning, orphan-file detection |
| `Iceberg.MetricsConfig`        | Per-column metrics-mode configuration (`full`, `truncate(N)`, `counts`, `none`) |
| `Iceberg.SchemaCompat`         | Schema-compatibility rules between two schema versions |
| `Iceberg.SchemaEvolution`      | Allowed evolutions: add column, drop column, rename, promote |
| `Iceberg.SingleValue`          | Iceberg's single-value binary serialisation (the format Iceberg uses for partition-value bytes) |
| `Iceberg.BoundTrunc`           | Truncation rules for partition lower / upper bound serialisation |
| `Iceberg.Murmur3`              | MurmurHash3-x86-32 (the hash Iceberg's `bucket[N]` partition transform uses) |
| `Iceberg.Geometry`             | Geo type encoding (Iceberg v3 / spatial extensions) |
| `Iceberg.Variant`              | Iceberg Variant binary type encoder / decoder |
| `Iceberg.Variant.Parquet`      | Parquet representation of Variant columns |
| `Iceberg.Variant.Shredding`    | Variant shredding rules (Iceberg v3) |
| `Iceberg.Puffin`               | Puffin index file format (Iceberg's auxiliary index container, used for stats / sketches / NDV estimates) |
| `Iceberg.Delete`               | Equality and position deletes |
| `Iceberg.DeletionVector`       | Iceberg v3 deletion-vector encoding (a compact bitmap of deleted rows) |
| `Iceberg.View`                 | Iceberg view metadata (the SQL-view sibling of the table format) |
| `Iceberg.Parquet`              | Bridge between Iceberg's Parquet data files and `wireform-parquet` |
| `Iceberg.Catalog.Glue`         | AWS Glue Data Catalog binding |
| `Iceberg.Catalog.Hadoop`       | Hadoop-style filesystem catalog (the original Iceberg catalog) |
| `Iceberg.Catalog.REST`         | Iceberg REST catalog protocol types |
| `Iceberg.Catalog.REST.Client`  | HTTP client for the REST catalog (behind `+rest-client`) |
| `Iceberg.Catalog.Sql`          | SQL-backed catalog (PostgreSQL / etc.) |
| `Iceberg.Derive`               | `deriveIceberg` Template Haskell entry point: maps Haskell records onto Iceberg `Schema`s with field IDs |

## Snapshots and time travel

`Iceberg.Snapshot` is the working surface for time-travel queries.
Iceberg snapshots form a DAG (typically a chain in the steady state,
but rollbacks and branches create siblings); the helpers cover the
queries that actually come up:

```haskell
currentSnapshot     :: TableMetadata -> Maybe Snapshot
snapshotById        :: TableMetadata -> Int64 -> Maybe Snapshot
snapshotByRef       :: TableMetadata -> Text  -> Maybe Snapshot   -- "main", branch / tag refs
snapshotAsOfTime    :: TableMetadata -> Int64 -> Maybe Snapshot   -- millis-since-epoch
snapshotParentChain :: TableMetadata -> Snapshot -> [Snapshot]
ancestorsOf         :: TableMetadata -> Int64    -> [Snapshot]
currentAncestors    :: TableMetadata -> [Snapshot]
snapshotsBetween    :: TableMetadata -> Int64 -> Int64 -> Maybe [Snapshot]
isAncestor          :: TableMetadata -> Int64 -> Int64 -> Bool
```

## Annotation-driven deriving

`Iceberg.Derive` consumes the cross-format `Wireform.Derive.Modifier`
vocabulary from [`wireform-derive`](../wireform-derive/README.md). The
field IDs Iceberg requires come from the same `tag N` annotation
proto / Thrift / Bond use:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified Iceberg.Derive as DIce
import Wireform.Derive (tag)

data Trade = Trade
  { tradeId    :: !Int64
  , tradeTicker :: !Text
  , tradePrice :: !Double
  } deriving stock (Show, Eq, Generic)

{-# ANN type Trade ("Trade" :: String) #-}
{-# ANN tradeId    (tag 1) #-}
{-# ANN tradeTicker (tag 2) #-}
{-# ANN tradePrice (tag 3) #-}

DIce.deriveIceberg ''Trade
```

Generates a `Schema` with stable field IDs that survive Iceberg's
schema-evolution rules.

## Catalogs

`Iceberg.Catalog.*` covers the four catalog flavours in common use:

- `Iceberg.Catalog.Glue`: AWS Glue Data Catalog. Read-only by
  default; writes go via the AWS SDK.
- `Iceberg.Catalog.Hadoop`: filesystem-backed catalog, the
  original Iceberg catalog flavour. Backed by direct path
  operations against the metadata location.
- `Iceberg.Catalog.REST` / `.REST.Client`: the
  [Iceberg REST catalog protocol](https://iceberg.apache.org/docs/latest/configuration/#catalogs)
  shared between Tabular, Polaris, Lakekeeper, and others. The
  HTTP client is behind the `+rest-client` flag.
- `Iceberg.Catalog.Sql`: SQL-backed catalog (PostgreSQL / MySQL,
  the schema the Java reference catalog uses).

## Testing

The per-format suite covers the table metadata round-trip, the
manifest reader / writer, the expression evaluator, schema-compat
rules, the in-process catalogs, partition transforms, the Variant
type, the Puffin index, the deletion vector, and Iceberg's MurmurHash3
implementation:

```bash
cabal test wireform-iceberg:iceberg-test
cabal test wireform-iceberg:wireform-iceberg-derive-test
```

### Cross-language interop with `pyiceberg`

[`probe/Probe.hs`](probe/Probe.hs) (paired with
[`scripts/iceberg_interop.py`](scripts/iceberg_interop.py)) round-
trips manifests, manifest lists, and table metadata through pyiceberg
and fastavro. The probe writes payloads from the wireform side, the
Python script reads them back through the reference implementation,
and the comparison catches wire-level deviations that an in-process
round-trip wouldn't see.

```bash
pip install pyiceberg fastavro
cabal run wireform-iceberg-pyiceberg-probe
```

The in-process catalog implementations (`Glue` / `Hadoop` / `REST` /
`Sql`) are exercised separately by the `Test.Iceberg.Catalog*` suites.

## Benchmarks

A criterion harness in [`bench/Bench.hs`](bench/Bench.hs) measures
the C-vs-pure speedup on the hot paths Iceberg uses internally:
Murmur3 (the hash function `bucket[N]` partition transforms call),
deletion-vector decode + contains, Roaring container encode/decode.

```bash
cabal bench wireform-iceberg:iceberg-bench
```

<!-- BEGIN_AUTOGEN bench:iceberg-murmur3-c-vs-pure -->
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="bench-results/charts/iceberg-murmur3-c-vs-pure-dark.svg">
  <img src="bench-results/charts/iceberg-murmur3-c-vs-pure-light.svg" alt="Iceberg Murmur3 hash: C kernel vs pure Haskell across input sizes">
</picture>

| Operation | C kernel | pure Haskell | ratio |
| :-------- | -------: | -----------: | ----: |
| 8 B       |  11.5 ns |      20.7 ns | 1.80x |
| 64 B      |  22.9 ns |       110 ns | 4.80x |
| 1 KiB     |   391 ns |      1551 ns | 3.96x |
| 64 KiB    | 26010 ns |     97717 ns | 3.76x |

<sub>Last run 2026-06-27 11:45:59 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:iceberg-murmur3-c-vs-pure -->

<!-- BEGIN_AUTOGEN bench:iceberg-deletion-vector -->
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="bench-results/charts/iceberg-deletion-vector-dark.svg">
  <img src="bench-results/charts/iceberg-deletion-vector-light.svg" alt="Iceberg deletion vector hot paths: C vs pure Haskell">
</picture>

| Operation             | C kernel | pure Haskell |  ratio |
| :-------------------- | -------: | -----------: | -----: |
| decode 1001 positions | 10081 ns |     27392 ns |  2.72x |
| contains check        |  11.9 ns |      1054 ns | 88.30x |

<sub>Last run 2026-06-27 11:45:59 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:iceberg-deletion-vector -->

For cross-language comparisons (table-format-level):

- Java: the
  [Apache Iceberg reference Java library](https://github.com/apache/iceberg),
  the canonical implementation. Used by Hive, Spark, Flink, Trino.
- Python: [pyiceberg](https://py.iceberg.apache.org/), the same
  binding the interop probe uses.
- Rust: [`iceberg-rust`](https://github.com/apache/iceberg-rust),
  the in-incubation Rust binding.
- Go: [`go-iceberg`](https://github.com/apache/iceberg-go).

## License

BSD-3-Clause.

## References

- [Apache Iceberg specification](https://iceberg.apache.org/spec/)
- [Iceberg REST catalog protocol](https://iceberg.apache.org/docs/latest/configuration/#catalogs)
- [Iceberg view spec](https://iceberg.apache.org/view-spec/)
- [Iceberg Puffin spec](https://iceberg.apache.org/puffin-spec/)
- [Apache Iceberg project](https://iceberg.apache.org/)
