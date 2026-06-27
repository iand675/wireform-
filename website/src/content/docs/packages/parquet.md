---
title: wireform-parquet
description: "Apache Parquet reader and writer with full encoding support, compression, bloom filters, page indexes, column encryption, predicate pushdown, and an Arrow bridge."
sidebar:
  order: 40
---

`wireform-parquet` implements the Apache Parquet columnar file format. Parquet
is the on-disk format behind most data warehouses and lakehouse table formats
(Iceberg, Delta Lake, Hudi). Use this package when you need to read or write
Parquet files directly in Haskell, with support for the encodings and
compression codecs that real-world writers emit.

## Key features

- **Full read and write** via `Parquet.HighLevel` and lower-level page APIs
- **All major encodings**: PLAIN, dictionary, DELTA_BINARY_PACKED,
  BYTE_STREAM_SPLIT, and hybrid RLE index pages
- **Compression codecs** behind Cabal flags: Snappy, Zstd, LZ4, Gzip, and
  Brotli
- **Bloom filters** and **page indexes** for sub-row-group predicate pruning
- **Modular column encryption** (AES-GCM) per the Parquet Modular Encryption
  spec
- **Predicate pushdown** over footer statistics, page indexes, and bloom filters
- **Nested columns** (lists, maps, structs) via `Parquet.Nested`
- **Arrow bridge** for typed record batches through `Parquet.Arrow`
- **Template Haskell deriver** via `Parquet.Derive`
- **Interop-tested** against pyarrow

## Basic usage

Most callers start with the high-level decode API for in-memory bytes, or
`openParquetReader` for mmap-aware streaming over large files on disk:

```haskell
import qualified Data.Vector        as V
import qualified Parquet.HighLevel  as PH
import qualified Parquet.Read       as PR
import qualified Parquet.Types      as PT

readParquetBytes :: ByteString -> IO ()
readParquetBytes bytes =
  case PH.decodeParquet PH.defaultReadOptions bytes of
    Left err ->
      putStrLn err
    Right pf ->
      let fm = PR.pfFooter pf
      in putStrLn $
           "rows="
             ++ show (PT.fmNumRows fm)
             ++ " rowGroups="
             ++ show (V.length (PT.fmRowGroups fm))

readParquetFile :: FilePath -> IO ()
readParquetFile path = do
  result <- PR.openParquetReader path
  case result of
    Left err ->
      putStrLn err
    Right (pf, _rowGroupIter) ->
      let fm = PR.pfFooter pf
      in putStrLn $
           "rows="
             ++ show (PT.fmNumRows fm)
             ++ " rowGroups="
             ++ show (V.length (PT.fmRowGroups fm))
```

For writing, pass an Arrow-shaped schema and column batches to
`encodeParquet`:

```haskell
import qualified Parquet.HighLevel as PH

writeParquet :: PH.Schema -> [V.Vector PH.ColumnData] -> ByteString
writeParquet schema rowGroups =
  PH.encodeParquet PH.defaultWriteOptions schema rowGroups
```

When you need projection or filter pushdown without loading every column,
use the predicate and aggregate modules together with the Arrow bridge or
the cross-format `Wireform.Columnar` facade.

## Performance

### XXH64 hash: C kernel vs pure Haskell

<!-- BEGIN_AUTOGEN bench:parquet-xxh64-c-vs-pure -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>Parquet XXH64 hash: C kernel vs pure Haskell across input sizes</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">Parquet XXH64 hash: C kernel vs pure Haskell across input sizes</text>
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
      <rect x="95.5" y="319.9" width="60" height="0.1" rx="2" fill="#0969da"/>
      <rect x="157.5" y="319.9" width="60" height="0.1" rx="2" fill="#cf222e"/>
      <rect x="250.5" y="319.9" width="60" height="0.1" rx="2" fill="#0969da"/>
      <rect x="312.5" y="319.8" width="60" height="0.2" rx="2" fill="#cf222e"/>
      <rect x="405.5" y="319.6" width="60" height="0.4" rx="2" fill="#0969da"/>
      <rect x="467.5" y="317.6" width="60" height="2.4" rx="2" fill="#cf222e"/>
      <rect x="560.5" y="297.2" width="60" height="22.8" rx="2" fill="#0969da"/>
      <rect x="622.5" y="172.9" width="60" height="147.1" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="125.5" y="315.9" text-anchor="middle" font-size="10" fill="#1f2328">12.1</text>
      <text x="187.5" y="315.9" text-anchor="middle" font-size="10" fill="#1f2328">10.3</text>
      <text x="280.5" y="315.9" text-anchor="middle" font-size="10" fill="#1f2328">18.1</text>
      <text x="342.5" y="315.8" text-anchor="middle" font-size="10" fill="#1f2328">43.0</text>
      <text x="435.5" y="315.6" text-anchor="middle" font-size="10" fill="#1f2328">79.9</text>
      <text x="497.5" y="313.6" text-anchor="middle" font-size="10" fill="#1f2328">457</text>
      <text x="590.5" y="293.2" text-anchor="middle" font-size="10" fill="#1f2328">4387</text>
      <text x="652.5" y="168.9" text-anchor="middle" font-size="10" fill="#1f2328">28291</text>
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
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">Parquet XXH64 hash: C kernel vs pure Haskell across input sizes</text>
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
      <rect x="95.5" y="319.9" width="60" height="0.1" rx="2" fill="#58a6ff"/>
      <rect x="157.5" y="319.9" width="60" height="0.1" rx="2" fill="#ff7b72"/>
      <rect x="250.5" y="319.9" width="60" height="0.1" rx="2" fill="#58a6ff"/>
      <rect x="312.5" y="319.8" width="60" height="0.2" rx="2" fill="#ff7b72"/>
      <rect x="405.5" y="319.6" width="60" height="0.4" rx="2" fill="#58a6ff"/>
      <rect x="467.5" y="317.6" width="60" height="2.4" rx="2" fill="#ff7b72"/>
      <rect x="560.5" y="297.2" width="60" height="22.8" rx="2" fill="#58a6ff"/>
      <rect x="622.5" y="172.9" width="60" height="147.1" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="125.5" y="315.9" text-anchor="middle" font-size="10" fill="#e6edf3">12.1</text>
      <text x="187.5" y="315.9" text-anchor="middle" font-size="10" fill="#e6edf3">10.3</text>
      <text x="280.5" y="315.9" text-anchor="middle" font-size="10" fill="#e6edf3">18.1</text>
      <text x="342.5" y="315.8" text-anchor="middle" font-size="10" fill="#e6edf3">43.0</text>
      <text x="435.5" y="315.6" text-anchor="middle" font-size="10" fill="#e6edf3">79.9</text>
      <text x="497.5" y="313.6" text-anchor="middle" font-size="10" fill="#e6edf3">457</text>
      <text x="590.5" y="293.2" text-anchor="middle" font-size="10" fill="#e6edf3">4387</text>
      <text x="652.5" y="168.9" text-anchor="middle" font-size="10" fill="#e6edf3">28291</text>
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
| 8 B       |  12.1 ns |      10.3 ns | 0.85x |
| 64 B      |  18.1 ns |     42.10 ns | 2.37x |
| 1 KiB     |  79.9 ns |       457 ns | 5.71x |
| 64 KiB    |  4387 ns |     28291 ns | 6.45x |

<sub>Last run 2026-06-27 11:45:59 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:parquet-xxh64-c-vs-pure -->

The C kernel pulls ahead of the pure Haskell fallback from 64 bytes up. At 8 bytes the pure path wins slightly due to call overhead. The C path is the default when the FFI is available. Page-level decode throughput depends heavily on encoding (PLAIN, DELTA_BINARY_PACKED, RLE/bitpacked) and compression codec.

The chart and table above are regenerated by [`wireform-stats`](../stats/) from `wireform-parquet/bench-results/summary/parquet-xxh64-c-vs-pure.json` — the same source the README chart is built from.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Parquet.HighLevel` | `encodeParquet`, `decodeParquet`, `WriteOptions`, `ReadOptions` |
| `Parquet.Read` | `loadParquetFilePath`, `openParquetReader`, column chunk decoders |
| `Parquet.Write` | Page encoders, row group assembly, `buildParquetFile` |
| `Parquet.Footer` | Thrift-encoded footer parse and emit |
| `Parquet.Page` / `Parquet.PageIndex` | Data page headers and per-page statistics |
| `Parquet.BloomFilter` | Split-block bloom filter decode |
| `Parquet.Encryption` | Column-level and footer encryption (PME, AES-GCM) |
| `Parquet.Predicate` | Statistics and bloom-filter predicate evaluation |
| `Parquet.Aggregate` | `count(*)`, `count(col)`, `min`, `max` from footer stats |
| `Parquet.Arrow` | Parquet columns to Arrow `ColumnArray` bridge |
| `Parquet.Derive` | Template Haskell deriver with `wireform-derive` annotations |

## Interop

The reader handles files produced by pyarrow, parquet-cpp, and arrow-rs,
including dictionary-encoded strings, delta-packed integers, and
BYTE_STREAM_SPLIT floats. Cross-language round-trip tests live in the
package probe suite.
