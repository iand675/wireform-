# wireform-parquet

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

[Apache Parquet](https://parquet.apache.org/) for Haskell. The
metadata footer ([`Parquet.Footer`](src/Parquet/Footer.hs),
[`Parquet.Types`](src/Parquet/Types.hs),
[`Parquet.Thrift.Schema`](src/Parquet/Thrift/Schema.hs)), column page
read and write paths
([`Parquet.Page`](src/Parquet/Page.hs),
[`Parquet.Read`](src/Parquet/Read.hs),
[`Parquet.Write`](src/Parquet/Write.hs)), the rep / def level
materializer ([`Parquet.Levels`](src/Parquet/Levels.hs),
[`Parquet.LevelsEncode`](src/Parquet/LevelsEncode.hs)), every
encoding the Parquet format supports
([`Parquet.Delta`](src/Parquet/Delta.hs),
[`Parquet.DeltaEncode`](src/Parquet/DeltaEncode.hs),
[`Parquet.ByteStreamSplit`](src/Parquet/ByteStreamSplit.hs)),
the page index and bloom filter
([`Parquet.PageIndex`](src/Parquet/PageIndex.hs),
[`Parquet.BloomFilter`](src/Parquet/BloomFilter.hs)),
column-level encryption
([`Parquet.Encryption`](src/Parquet/Encryption.hs)),
the high-level reader / writer
([`Parquet.HighLevel`](src/Parquet/HighLevel.hs)),
predicate pushdown ([`Parquet.Predicate`](src/Parquet/Predicate.hs),
[`Parquet.Aggregate`](src/Parquet/Aggregate.hs)), an Arrow bridge
([`Parquet.Arrow`](src/Parquet/Arrow.hs)), and an annotation-driven
deriver ([`Parquet.Derive`](src/Parquet/Derive.hs)).

Parquet is the on-disk columnar format that won at scale: every
data warehouse from Snowflake to Redshift to BigQuery to Databricks
reads it, every data-lake table format (Iceberg, Delta Lake, Hudi)
stores its data files in it, and every modern dataframe runtime
ships a reader for it. The wire format is dense (per-column
encoding selected by the writer, with optional dictionary, delta,
or byte-stream-split encodings on top, optional Snappy / Zstd /
LZ4 / Brotli compression on top of that, optional column-level
encryption on top of that, plus a Thrift-encoded footer at the end
of the file with all the row-group statistics needed for
predicate pushdown).

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-parquet,
  wireform-arrow,       -- for the Arrow bridge / typed record surface
  wireform-columnar,    -- iterator surface + predicate vocabulary
  wireform-derive,      -- only if you want the cross-format annotation deriver
```

The package supports four compression flags. Snappy, Zstandard, and
LZ4_RAW are on by default because virtually every Parquet file in the
wild uses one of them (pyarrow + DuckDB default to Snappy, Polars
defaults to ZSTD, Spark frequently emits LZ4_RAW). Brotli (codec 4)
is off by default since it shows up much less often:

```cabal
flags: +snappy +zstd +lz4 -brotli
```

Disable individually with `-f-snappy`, `-f-zstd`, etc., if you want
a smaller dep tree.

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-parquet` to compile
locally.

## Hello world

Round-tripping a Parquet file footer:

```haskell
import qualified Data.ByteString as BS
import qualified Data.Vector     as V
import qualified Parquet.Types  as P
import qualified Parquet.Footer as PF

main :: IO ()
main = do
  let schema = V.fromList
        [ P.SchemaElement "schema" Nothing               Nothing               (Just 2) Nothing Nothing Nothing
        , P.SchemaElement "id"     (Just P.Required)     (Just P.PTInt64)      Nothing  Nothing Nothing Nothing
        , P.SchemaElement "name"   (Just P.Optional)     (Just P.PTByteArray)  Nothing  Nothing Nothing Nothing
        ]
      metadata = P.FileMetadata
        { P.fmVersion       = 2
        , P.fmSchema        = schema
        , P.fmNumRows       = 1000
        , P.fmRowGroups     = V.empty
        , P.fmCreatedBy     = Just "wireform"
        , P.fmColumnOrders  = Nothing
        }
      footer = PF.writeFooter metadata
  case PF.readFooter footer of
    Right fm -> putStrLn $ "Read back: rows=" ++ show (P.fmNumRows fm)
    Left  err -> putStrLn err
```

The runnable version lives in [`examples/ParquetExample.hs`](../examples/ParquetExample.hs).

For end-to-end file reading and writing, the high-level entry points
are in `Parquet.HighLevel` and through the Arrow bridge in
`Parquet.Arrow`. The cross-format `Wireform.Columnar` facade in the
umbrella package layers a uniform Arrow-shaped surface on top.

## What's in here

| Module                  | Role                                                      |
|-------------------------|-----------------------------------------------------------|
| `Parquet.Types`         | Parquet schema and metadata AST (`FileMetadata`, `SchemaElement`, `RowGroup`, `ColumnChunk`, the Parquet logical type system) |
| `Parquet.Thrift.Schema` | The Thrift binding for `parquet.thrift` (Parquet's metadata is Thrift-encoded inside the footer) |
| `Parquet.Footer`        | `readFooter` / `writeFooter`: parse and emit the magic-prefixed Thrift-encoded footer |
| `Parquet.Page`          | Per-page header decoder (DataPageV1, DataPageV2, DictionaryPage), page-body slicing |
| `Parquet.PageIndex`     | Column index and offset index decoders (Parquet's per-page min / max statistics for sub-row-group pushdown) |
| `Parquet.BloomFilter`   | Split-block bloom filter decoder (Parquet 2.4+ probabilistic bloom for equality predicates) |
| `Parquet.Levels`        | Materialise definition / repetition levels into a column shape (`readPlain*OptionalColumnChunk`) |
| `Parquet.LevelsEncode`  | The encode side of definition / repetition levels |
| `Parquet.Nested`        | Nested-type materialisation: lists, maps, and struct columns built out of the leaf-column reads |
| `Parquet.NullPagesBitmap` | Validity bitmap construction from definition levels |
| `Parquet.Delta`         | DELTA_BINARY_PACKED, DELTA_LENGTH_BYTE_ARRAY, DELTA_BYTE_ARRAY decoders |
| `Parquet.DeltaEncode`   | DELTA_BINARY_PACKED encoder |
| `Parquet.ByteStreamSplit` | BYTE_STREAM_SPLIT decoder (Parquet 2.8+ float-friendly encoding) |
| `Parquet.Compress`      | Snappy / Zstandard / LZ4_RAW / Brotli / Gzip codec dispatch (each behind its own Cabal flag) |
| `Parquet.Encryption`    | Column-level encryption (PME, AES-GCM-CTR) per the Parquet Modular Encryption spec |
| `Parquet.Read`          | `loadParquetFilePath`, `openParquetReader` (path + handle helpers), and the pull-based reader entry points |
| `Parquet.Write`         | PLAIN / RLE / dictionary page encoders, page header encode, `buildParquetFile` |
| `Parquet.HighLevel`     | High-level entry points: `decodeParquet`, `WriteOptions`, `defaultWriteOptions`, `defaultReadOptions` |
| `Parquet.Predicate`     | Pushdown predicate evaluator over column statistics + page index + bloom filter |
| `Parquet.Aggregate`     | Statistics-driven aggregate pushdown: `count(*)` / `count(col)` / `min` / `max` directly from the footer when statistics are present |
| `Parquet.Arrow`         | Bridge between `Parquet` columns and Arrow `ColumnArray` (used by the `Wireform.Columnar` facade) |
| `Parquet.Derive`        | `deriveParquet` Template Haskell entry point |

## Encoding coverage

The reader implements every encoding the Parquet 2.x format defines:

- PLAIN, RLE, RLE_DICTIONARY, BIT_PACKED.
- DELTA_BINARY_PACKED, DELTA_LENGTH_BYTE_ARRAY, DELTA_BYTE_ARRAY.
- BYTE_STREAM_SPLIT.

Compression codecs (each behind its Cabal flag): UNCOMPRESSED, SNAPPY,
ZSTD, LZ4_RAW, BROTLI, GZIP.

The writer covers PLAIN page encoders for every leaf type plus
RLE / dictionary pages, the right defaults for nullable columns
(`Parquet.LevelsEncode`), and bloom filter generation
(`Parquet.HighLevel.buildBloomFilterFor`).

## Predicate pushdown

`Parquet.Predicate` evaluates a `Columnar.Predicate.Predicate` against:

- per-row-group column statistics from the footer,
- per-page column / offset index entries (Parquet 2.5+),
- split-block bloom filters where available.

The `Wireform.Columnar.decodeFilteredIter` facade entry point in the
umbrella package combines this evaluator with the streaming reader so
callers get only the row groups (and pages, with `PageIndex`) that
the predicate didn't rule out.

`Parquet.Aggregate` short-circuits the entire scan for `count(*)`,
`count(<col>)`, `min(<col>)`, and `max(<col>)` queries when the
required statistics are present in the footer; the reader returns
the aggregate without reading any column data.

## Arrow bridge

`Parquet.Arrow` converts Parquet column reads to Arrow `ColumnArray`s
and vice versa. The `Wireform.Columnar` facade in the umbrella package
layers a uniform Arrow-shaped API on top of both formats, so a
caller working with Arrow `ColumnArray`s can pick Parquet (or ORC, or
Arrow IPC) by switching the `Format` argument.

## Annotation-driven deriving

`Parquet.Derive` consumes the cross-format `Wireform.Derive.Modifier`
vocabulary from [`wireform-derive`](../wireform-derive/README.md):

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified Parquet.Derive as DPq
import Wireform.Derive (renameStyle, SnakeCase)

data Trade = Trade
  { tradeTicker :: !Text
  , tradePrice  :: !Double
  } deriving stock (Show, Eq, Generic)

{-# ANN type Trade ("Trade" :: String) #-}
{-# ANN tradeTicker (renameStyle SnakeCase) #-}
{-# ANN tradePrice  (renameStyle SnakeCase) #-}

DPq.deriveParquet ''Trade
```

## Testing

The per-format Hedgehog + HUnit suites live in `test/` and `test-derive/`:

```bash
cabal test wireform-parquet:wireform-parquet-test
cabal test wireform-parquet:wireform-parquet-derive-test
```

They cover footer round-trips, every encoding, every compression
codec (when its flag is enabled), the page index, the bloom filter,
the encryption surface, and the Arrow bridge.

A separate pyarrow round-trip suite exercises the writer against
pyarrow's reader (and vice versa) to catch wire-level deviations
that an in-process round-trip wouldn't.

## Benchmarks

A criterion harness in [`bench/Bench.hs`](bench/Bench.hs) measures
the C-vs-pure speedup on the XXH64 hash that Parquet bloom filters
key off:

```bash
cabal bench wireform-parquet:parquet-bench
```

<!-- BEGIN_AUTOGEN bench:parquet-xxh64-c-vs-pure -->
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="bench-results/charts/parquet-xxh64-c-vs-pure-dark.svg">
  <img src="bench-results/charts/parquet-xxh64-c-vs-pure-light.svg" alt="Parquet XXH64 hash: C kernel vs pure Haskell across input sizes">
</picture>

| Operation | C kernel | pure Haskell | ratio |
| :-------- | -------: | -----------: | ----: |
| 8 B       |  12.1 ns |      10.3 ns | 0.85x |
| 64 B      |  18.1 ns |     42.10 ns | 2.37x |
| 1 KiB     |  79.9 ns |       457 ns | 5.71x |
| 64 KiB    |  4387 ns |     28291 ns | 6.45x |

<sub>Last run 2026-06-27 11:45:59 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:parquet-xxh64-c-vs-pure -->

For cross-language comparisons (file-format-level):

- Haskell:
  [`parquet-hs`](https://hackage.haskell.org/package/parquet-hs) (the
  established Hackage Parquet library) and
  [`parquet`](https://hackage.haskell.org/package/parquet).
- C++: [arrow-cpp](https://github.com/apache/arrow/tree/main/cpp/src/parquet),
  the de facto reference. DuckDB also ships a high-quality reader.
- Rust: [`parquet`](https://crates.io/crates/parquet), used by
  Datafusion and Polars.
- Python: [`pyarrow.parquet`](https://arrow.apache.org/docs/python/parquet.html).

## License

BSD-3-Clause.

## References

- [Apache Parquet specification](https://github.com/apache/parquet-format)
- [Parquet Thrift schema](https://github.com/apache/parquet-format/blob/master/src/main/thrift/parquet.thrift)
- [Parquet modular encryption spec](https://github.com/apache/parquet-format/blob/master/Encryption.md)
- [Parquet bloom filter spec](https://github.com/apache/parquet-format/blob/master/BloomFilter.md)
