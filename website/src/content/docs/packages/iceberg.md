---
title: wireform-iceberg
description: "Apache Iceberg table format with metadata JSON, Avro manifests, scan planning, schema evolution, partition transforms, deletion vectors, catalog clients, and time travel."
sidebar:
  order: 43
---

`wireform-iceberg` implements the Apache Iceberg open table format. Iceberg
adds ACID transactions, hidden partitioning, schema evolution, and time
travel on top of object-storage data files. Use this package when you need
to read table metadata, plan scans with predicate pushdown, or integrate
with Iceberg REST, Glue, Hadoop, or SQL catalogs from Haskell.

## Key features

- **Table metadata** as canonical JSON via `Iceberg.JSON`
- **Manifest and manifest-list** readers and writers (Avro-encoded)
- **Scan planning** with manifest pruning and file selection
- **Schema evolution** rules and compatibility checks
- **Partition transforms** (`identity`, `bucket`, `truncate`, time transforms)
- **Deletion vectors** and position/equality delete file handling
- **Puffin statistics** index format support
- **Catalog clients** for REST, AWS Glue, Hadoop filesystem, and SQL backends
- **Time travel** via snapshot refs, snapshot IDs, and timestamp lookup
- **Interop-tested** against pyiceberg

## Basic usage

Open a table by parsing its metadata JSON, then plan a scan over the
current snapshot. The scan planner resolves the manifest list, reads
each manifest, and collects the data file paths your reader should open:

```haskell
import qualified Data.Aeson              as Aeson
import qualified Data.ByteString         as BS
import qualified Data.Map.Strict         as Map
import qualified Iceberg.Expression      as Expr
import qualified Iceberg.JSON            as IJ
import qualified Iceberg.Read            as IR
import           Iceberg.Snapshot          (currentSnapshot)

openTableMetadata :: FilePath -> IO (Either String Iceberg.Types.TableMetadata)
openTableMetadata metadataPath = do
  jsonBytes <- BS.readFile metadataPath
  pure $ case Aeson.eitherDecodeStrict jsonBytes of
    Left err  -> Left err
    Right val -> IJ.metadataFromJSON val

planScanWithLocalManifests
  :: Iceberg.Types.TableMetadata
  -> ByteString
  -> Map Text ByteString
  -> Either String IR.ScanPlan
planScanWithLocalManifests tm manifestListBytes manifests =
  let filterExpr =
        Expr.and_
          (Expr.greaterThanOrEq "event_time" (Expr.LLong 1700000000000))
          (Expr.equal "region" (Expr.LString "us-west"))
      readManifest path =
        maybe (Left ("missing manifest: " ++ T.unpack path)) Right
          (Map.lookup path manifests)
  in case currentSnapshot tm of
       Nothing -> Left "table has no current snapshot"
       Just _  -> IR.planScanWithFilter tm manifestListBytes readManifest filterExpr
```

The resulting `ScanPlan` carries the resolved snapshot, schema, manifest
paths, and data file paths. Pass each data file path to `wireform-parquet`
(or another format reader) to materialize rows.

For catalog-backed tables, use `Iceberg.Catalog.REST.Client` or
`Iceberg.Catalog.Glue` to load metadata before calling the scan planner.

## Performance

### Hot-path microbenchmarks: C kernel vs pure Haskell

#### Deletion vector

<!-- BEGIN_AUTOGEN bench:iceberg-deletion-vector -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>Iceberg deletion vector hot paths: C vs pure Haskell</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">Iceberg deletion vector hot paths: C vs pure Haskell</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">12500</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">25000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">37500</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">50000</text>
    </g>
    <g>
      <rect x="171" y="267.6" width="62" height="52.4" rx="2" fill="#0969da"/>
      <rect x="235" y="177.6" width="62" height="142.4" rx="2" fill="#cf222e"/>
      <rect x="481" y="319.9" width="62" height="0.1" rx="2" fill="#0969da"/>
      <rect x="545" y="314.5" width="62" height="5.5" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="202" y="263.6" text-anchor="middle" font-size="10" fill="#1f2328">10081</text>
      <text x="266" y="173.6" text-anchor="middle" font-size="10" fill="#1f2328">27392</text>
      <text x="512" y="315.9" text-anchor="middle" font-size="10" fill="#1f2328">11.9</text>
      <text x="576" y="310.5" text-anchor="middle" font-size="10" fill="#1f2328">1054</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode 1001 positions</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#1f2328">contains check</text>
    </g>
    <g>
      <g transform="translate(264, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">C kernel</text>
      </g>
      <g transform="translate(354, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">pure Haskell</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">Iceberg deletion vector hot paths: C vs pure Haskell</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">12500</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">25000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">37500</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">50000</text>
    </g>
    <g>
      <rect x="171" y="267.6" width="62" height="52.4" rx="2" fill="#58a6ff"/>
      <rect x="235" y="177.6" width="62" height="142.4" rx="2" fill="#ff7b72"/>
      <rect x="481" y="319.9" width="62" height="0.1" rx="2" fill="#58a6ff"/>
      <rect x="545" y="314.5" width="62" height="5.5" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="202" y="263.6" text-anchor="middle" font-size="10" fill="#e6edf3">10081</text>
      <text x="266" y="173.6" text-anchor="middle" font-size="10" fill="#e6edf3">27392</text>
      <text x="512" y="315.9" text-anchor="middle" font-size="10" fill="#e6edf3">11.9</text>
      <text x="576" y="310.5" text-anchor="middle" font-size="10" fill="#e6edf3">1054</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode 1001 positions</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">contains check</text>
    </g>
    <g>
      <g transform="translate(264, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">C kernel</text>
      </g>
      <g transform="translate(354, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">pure Haskell</text>
      </g>
    </g>
  </g>
</svg>


| Operation             | C kernel | pure Haskell |  ratio |
| :-------------------- | -------: | -----------: | -----: |
| decode 1001 positions | 10081 ns |     27392 ns |  2.72x |
| contains check        |  11.9 ns |      1054 ns | 88.30x |

<sub>Last run 2026-06-27 11:45:59 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:iceberg-deletion-vector -->

#### Murmur3 hash

<!-- BEGIN_AUTOGEN bench:iceberg-murmur3-c-vs-pure -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>Iceberg Murmur3 hash: C kernel vs pure Haskell across input sizes</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">Iceberg Murmur3 hash: C kernel vs pure Haskell across input sizes</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">25000</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">50000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">75000</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">100000</text>
    </g>
    <g>
      <rect x="95.5" y="320.0" width="60" height="0.0" rx="2" fill="#0969da"/>
      <rect x="157.5" y="319.9" width="60" height="0.1" rx="2" fill="#cf222e"/>
      <rect x="250.5" y="319.9" width="60" height="0.1" rx="2" fill="#0969da"/>
      <rect x="312.5" y="319.7" width="60" height="0.3" rx="2" fill="#cf222e"/>
      <rect x="405.5" y="319.0" width="60" height="1.0" rx="2" fill="#0969da"/>
      <rect x="467.5" y="316.0" width="60" height="4.0" rx="2" fill="#cf222e"/>
      <rect x="560.5" y="252.4" width="60" height="67.6" rx="2" fill="#0969da"/>
      <rect x="622.5" y="65.9" width="60" height="254.1" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="125.5" y="316.0" text-anchor="middle" font-size="10" fill="#1f2328">11.5</text>
      <text x="187.5" y="315.9" text-anchor="middle" font-size="10" fill="#1f2328">20.7</text>
      <text x="280.5" y="315.9" text-anchor="middle" font-size="10" fill="#1f2328">22.9</text>
      <text x="342.5" y="315.7" text-anchor="middle" font-size="10" fill="#1f2328">110</text>
      <text x="435.5" y="315.0" text-anchor="middle" font-size="10" fill="#1f2328">391</text>
      <text x="497.5" y="312.0" text-anchor="middle" font-size="10" fill="#1f2328">1551</text>
      <text x="590.5" y="248.4" text-anchor="middle" font-size="10" fill="#1f2328">26010</text>
      <text x="652.5" y="61.9" text-anchor="middle" font-size="10" fill="#1f2328">97717</text>
    </g>
    <g>
      <text x="157.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">8 B</text>
      <text x="312.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">64 B</text>
      <text x="467.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">1 KiB</text>
      <text x="622.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">64 KiB</text>
    </g>
    <g>
      <g transform="translate(264, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">C kernel</text>
      </g>
      <g transform="translate(354, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">pure Haskell</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">Iceberg Murmur3 hash: C kernel vs pure Haskell across input sizes</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">25000</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">50000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">75000</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">100000</text>
    </g>
    <g>
      <rect x="95.5" y="320.0" width="60" height="0.0" rx="2" fill="#58a6ff"/>
      <rect x="157.5" y="319.9" width="60" height="0.1" rx="2" fill="#ff7b72"/>
      <rect x="250.5" y="319.9" width="60" height="0.1" rx="2" fill="#58a6ff"/>
      <rect x="312.5" y="319.7" width="60" height="0.3" rx="2" fill="#ff7b72"/>
      <rect x="405.5" y="319.0" width="60" height="1.0" rx="2" fill="#58a6ff"/>
      <rect x="467.5" y="316.0" width="60" height="4.0" rx="2" fill="#ff7b72"/>
      <rect x="560.5" y="252.4" width="60" height="67.6" rx="2" fill="#58a6ff"/>
      <rect x="622.5" y="65.9" width="60" height="254.1" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="125.5" y="316.0" text-anchor="middle" font-size="10" fill="#e6edf3">11.5</text>
      <text x="187.5" y="315.9" text-anchor="middle" font-size="10" fill="#e6edf3">20.7</text>
      <text x="280.5" y="315.9" text-anchor="middle" font-size="10" fill="#e6edf3">22.9</text>
      <text x="342.5" y="315.7" text-anchor="middle" font-size="10" fill="#e6edf3">110</text>
      <text x="435.5" y="315.0" text-anchor="middle" font-size="10" fill="#e6edf3">391</text>
      <text x="497.5" y="312.0" text-anchor="middle" font-size="10" fill="#e6edf3">1551</text>
      <text x="590.5" y="248.4" text-anchor="middle" font-size="10" fill="#e6edf3">26010</text>
      <text x="652.5" y="61.9" text-anchor="middle" font-size="10" fill="#e6edf3">97717</text>
    </g>
    <g>
      <text x="157.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">8 B</text>
      <text x="312.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">64 B</text>
      <text x="467.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">1 KiB</text>
      <text x="622.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">64 KiB</text>
    </g>
    <g>
      <g transform="translate(264, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">C kernel</text>
      </g>
      <g transform="translate(354, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">pure Haskell</text>
      </g>
    </g>
  </g>
</svg>


| Operation | C kernel | pure Haskell | ratio |
| :-------- | -------: | -----------: | ----: |
| 8 B       |  11.5 ns |      20.7 ns | 1.80x |
| 64 B      |  22.9 ns |       110 ns | 4.80x |
| 1 KiB     |   391 ns |      1551 ns | 3.96x |
| 64 KiB    | 26010 ns |     97717 ns | 3.76x |

<sub>Last run 2026-06-27 11:45:59 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:iceberg-murmur3-c-vs-pure -->

The C kernels for deletion-vector bitmap operations and Murmur3 hashing are several times faster than the pure Haskell fallbacks. The contains check is the most dramatic: a single bitmap probe takes nanoseconds in C vs over 1 µs in pure Haskell. Both kernels are used by default.

The charts and tables above are regenerated by [`wireform-stats`](../stats/) from `wireform-iceberg/bench-results/summary/iceberg-{deletion-vector,murmur3-c-vs-pure}.json` — the same source the README charts are built from.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Iceberg.Types` | `TableMetadata`, `Schema`, `Snapshot`, partition specs |
| `Iceberg.JSON` | `metadataToJSON` / `metadataFromJSON` |
| `Iceberg.Snapshot` | Snapshot lookup, refs, time travel, ancestry |
| `Iceberg.Read` | `planScan`, `planScanWithFilter`, manifest readers |
| `Iceberg.Write` | Snapshot and manifest emission |
| `Iceberg.Expression` | Predicate AST and manifest pruning evaluators |
| `Iceberg.Partition` / `Iceberg.Transform` | Partition spec evaluation |
| `Iceberg.SchemaEvolution` | Allowed schema changes |
| `Iceberg.Delete` / `Iceberg.DeletionVector` | Row-level delete handling |
| `Iceberg.Puffin` | Puffin auxiliary index files |
| `Iceberg.Catalog.*` | REST, Glue, Hadoop, and SQL catalog bindings |
| `Iceberg.Parquet` | Iceberg Parquet data file bridge |

## Interop

The probe suite round-trips table metadata, manifest lists, and manifest
files against pyiceberg and fastavro fixtures captured from real tables.
