# wireform-arrow

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

[Apache Arrow](https://arrow.apache.org/) IPC for Haskell. The Arrow
schema and type system ([`Arrow.Types`](src/Arrow/Types.hs)), the
columnar value representation ([`Arrow.Column`](src/Arrow/Column.hs)),
the IPC message envelope ([`Arrow.IPC`](src/Arrow/IPC.hs)) including
the FlatBuffer-encoded schema and record batch headers
([`Arrow.FlatBufferIPC`](src/Arrow/FlatBufferIPC.hs)), file
([`Arrow.File`](src/Arrow/File.hs)) and stream
([`Arrow.Stream`](src/Arrow/Stream.hs)) framings, a typed record
surface ([`Arrow.Record`](src/Arrow/Record.hs),
[`Arrow.Record.Generic`](src/Arrow/Record/Generic.hs),
[`Arrow.Record.TH`](src/Arrow/Record/TH.hs)), the encode side
([`Arrow.Write`](src/Arrow/Write.hs)), and the annotation-driven
deriver ([`Arrow.Derive`](src/Arrow/Derive.hs)).

Arrow is the in-memory columnar layout that anchors the modern
analytics stack: pandas, polars, DuckDB, Velox, Datafusion, and most
modern dataframe and analytics engines. The IPC format is what those
engines exchange when they ship a record batch over a socket, an
HTTP stream, or a file. The wire payload is a FlatBuffer-encoded
schema header followed by a sequence of record batches, with
validity bitmaps and column buffers laid out exactly as Arrow's
in-memory format requires, so a compliant reader can mmap the file
and skip the parse step entirely.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-arrow,
  wireform-columnar,    -- iterator surface + predicate vocabulary
  wireform-derive,      -- only if you want the cross-format annotation deriver
```

The package supports two optional compression flags for IPC body
buffers, both off by default:

```cabal
flags: +zstd +lz4
```

`+zstd` adds Zstandard via the [`zstd`](https://hackage.haskell.org/package/zstd)
binding. `+lz4` adds LZ4 frame format via
[`lz4-hs`](https://hackage.haskell.org/package/lz4-hs) (the older
Hackage `lz4` package implements only the block format, which is
incompatible with arrow-cpp). Both must match what the producer used.

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-arrow -fzstd -flz4`
to compile locally with both codecs.

## Hello world

Encode an Arrow `Schema` as an IPC message and round-trip it:

```haskell
import qualified Data.ByteString as BS
import qualified Data.Vector     as V
import qualified Arrow.Types as A
import qualified Arrow.IPC   as AIPC

main :: IO ()
main = do
  let schema = A.Schema
        { A.arrowFields = V.fromList
            [ A.Field "id"     False (A.AInt 64 True)               V.empty Nothing V.empty
            , A.Field "name"   True  A.AUtf8                        V.empty Nothing V.empty
            , A.Field "score"  False (A.AFloatingPoint A.DoublePrecision) V.empty Nothing V.empty
            , A.Field "active" True  A.ABool                        V.empty Nothing V.empty
            ]
        , A.arrowEndianness = A.Little
        , A.arrowMetadata   = V.empty
        , A.arrowFeatures   = V.empty
        }
      bytes = AIPC.encodeIPCMessage (A.SchemaMessage schema)
  case AIPC.decodeIPCMessage bytes of
    Right (A.SchemaMessage s) ->
      putStrLn $ "Decoded schema: " ++ show (V.length (A.arrowFields s)) ++ " fields"
    Right other -> print other
    Left  err   -> putStrLn err
```

The runnable version lives in [`examples/ArrowExample.hs`](../examples/ArrowExample.hs).

For typed records, derive the `Arrow.Derive` typeclasses against a
record:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified Arrow.Derive as DArrow

data Trade = Trade
  { tradeId    :: !Int64
  , tradeTicker :: !Text
  , tradePrice :: !Double
  } deriving stock (Show, Eq, Generic)

DArrow.deriveArrow ''Trade
```

## What's in here

| Module                   | Role                                                      |
|--------------------------|-----------------------------------------------------------|
| `Arrow.Types`            | Arrow schema AST: `Schema`, `Field`, `ArrowType` (`AInt`, `AFloatingPoint`, `AUtf8`, `ABool`, `AStruct`, `AList`, `AMap`, `ADictionary`, ...), endianness, metadata, schema fingerprinting (`schemaFingerprint`, `schemaEquivalent`). |
| `Arrow.Column`           | `ColumnArray`: the in-memory columnar representation (one constructor per Arrow type, `*Maybe` variants for nullable columns, `ColDictionary` / `ColDictionaryMaybe` for dictionary encoding, union type ids as child indices). Total `sliceColumnArray`, `concatColumnArray`, `takeColumnArray`, `expandDictionary`, and `validateMapKeysSorted` for spec-required map ordering. |
| `Arrow.IPC`              | Single-message framing: `encodeIPCMessage`, `decodeIPCMessage` for the spec FlatBuffers `Message` (Schema, DictionaryBatch, RecordBatch); bodies are appended by the caller. |
| `Arrow.FlatBufferIPC`    | Arrow's FlatBuffer schema and record batch headers (Arrow IPC's wire layer). |
| `Arrow.Stream`           | Stream and file writers / readers (`encodeArrowStream`, `decodeArrowStream`, `encodeArrowFile`, `decodeArrowFile`, `openStreamReader`, `streamReaderIter`, projection helpers), with dictionary batches (replacement and delta), and body compression. The `Iter` from [`wireform-columnar`](../wireform-columnar/) is the yield type. |
| `Arrow.File`             | Arrow file and stream readers over the same spec format (`readArrowFile`, `readArrowFileColumns`, `readArrowStream`, `readIPCMessage`). |
| `Arrow.Write`            | Column encoders and validity bitmaps (from `Arrow.Write.Columns`), plus `writeArrowStream` and `writeArrowFile`, which emit the standard Arrow IPC format via `Arrow.Stream`. |
| `Arrow.Record`           | Typed record surface (`Table`, `structE`, `structEMaybe`, `structD`, `structDMaybe`, `columnDWithDefault`, `subsetTable`, `projectTable`, `NameStrategy`). |
| `Arrow.Record.Generic`   | `GHC.Generics`-driven default `Table` derivation. |
| `Arrow.Record.TH`        | `Template Haskell` driver for explicit `Table` derivation when `Generic` doesn't fit. |
| `Arrow.Derive`           | `deriveArrow` Template Haskell entry point that consumes the `Wireform.Derive.Modifier` vocabulary. |

## Streaming reader

`Arrow.Stream.openStreamReader` returns a `StreamReader` that yields
record batches via the [`wireform-columnar`](../wireform-columnar/)
`Iter` interface. Callers can chain the standard combinators
(`iterMap`, `iterFilter`, `iterTake`, `iterIOPrefetch`,
`iterParallelMap`) onto the returned iterator:

```haskell
import qualified Arrow.Stream as AS
import qualified Columnar.Stream as IS

case AS.openStreamReader bytes of
  Right rdr -> do
    let sch     = AS.streamReaderSchema rdr
        batches = AS.streamReaderIter   rdr
    -- consume one batch at a time
    IS.iterTraverse_ batches $ \batch -> ...
  Left err -> putStrLn err
```

`resolveProjectionIndices` + `projectSchema` + `projectColumns`
implement column-projection pushdown so consumers that only want a
subset of fields can avoid materialising the rest.

## File reader and writer

`Arrow.File` covers the Arrow file format: the IPC stream framing
between `ARROW1` magic markers, followed by a FlatBuffers footer, as
written by pyarrow, arrow-cpp, `Arrow.Stream.encodeArrowFile`, and
`Arrow.Write.writeArrowFile`. `readArrowFileColumns` returns every
batch materialized (decompressed, dictionaries resolved).

## Annotation-driven deriving

`Arrow.Derive` consumes the cross-format `Wireform.Derive.Modifier`
vocabulary from [`wireform-derive`](../wireform-derive/README.md). The
typed record surface in `Arrow.Record` is the columnar equivalent of
what `<Format>.Class` is for the row-oriented formats: a Haskell
record is mapped to a struct column, with each field becoming a
child column.

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified Arrow.Derive as DArrow
import Wireform.Derive (renameStyle, SnakeCase)

data Trade = Trade
  { tradeTicker :: !Text
  , tradePrice  :: !Double
  } deriving stock (Show, Eq, Generic)

{-# ANN type Trade ("Trade" :: String) #-}
{-# ANN tradeTicker (renameStyle SnakeCase) #-}
{-# ANN tradePrice  (renameStyle SnakeCase) #-}

DArrow.deriveArrow ''Trade
```

The `Wireform.Columnar` facade in the umbrella package layers an
Arrow-shaped API on top of all three columnar formats (Arrow,
Parquet, ORC), so the same `[Trade]` value can be encoded to any of
them by switching the `Format` argument.

## Testing

Three suites, all run in CI:

```bash
cabal test wireform-arrow:wireform-arrow-test
cabal test wireform-arrow:wireform-arrow-derive-test
cabal test wireform-arrow:wireform-arrow-pyarrow-interop
```

`wireform-arrow-test` has three layers:

- Example round-trips for every column type, nullable and not, plus
  pyarrow-written goldens in `test/golden/` (stream and file format).
- Hedgehog properties (`test/Test/Arrow/Props.hs`) over generated
  schemas and multi-batch tables (`test/Test/Arrow/Gen.hs`): every
  leaf type with edge values, nesting to depth 3, dictionaries over
  every index type, run-end encoding, list views, utf8/binary views,
  zero-row batches. Stream and file round-trips, body compression,
  replacement and delta dictionaries, projection, slicing and
  concatenation laws, map key ordering.
- Malformed-input properties (`test/Test/Arrow/Malformed.hs`):
  truncations, bit flips, byte overwrites and corrupted buffer
  descriptors of valid streams and files, plus random bytes. Every
  decoder entry point must return `Left` or a fully forceable `Right`,
  never throw or crash.

`wireform-arrow-pyarrow-interop` (`test-interop/`) checks both
directions against pyarrow over the same case matrix: wireform-arrow
writes and pyarrow validates (`validate(full=True)` plus value
equality), and pyarrow writes (plain, sliced at an unaligned offset,
zstd, lz4) and wireform-arrow decodes. It needs a python with
pyarrow:

- `WIREFORM_ARROW_PYTHON`: python interpreter (default `python3`).
- `WIREFORM_ARROW_REQUIRE_PYARROW=1`: fail instead of skipping when
  pyarrow is missing (CI sets this).

`cabal run wireform-arrow:test:wireform-arrow-pyarrow-interop -- --write DIR`
only writes the wireform-side files, for checking other readers such
as arrow-rs (`scripts/run_columnar_interop.sh`). Regenerate the
pyarrow goldens with
`python3 wireform-arrow/test-interop/pyarrow_interop.py regen-goldens`.

## Benchmarks

A criterion harness in [`bench/Bench.hs`](bench/Bench.hs):

```bash
cabal bench wireform-arrow:wireform-arrow-bench
```

Each workload is one record batch encoded with
`Arrow.Stream.encodeArrowStream` and decoded (fully materialized) with
`Arrow.Stream.decodeArrowStream`. Inputs are built outside the timed
region. Nullable columns carry 10% nulls; `list<int32>` has four
elements per row; `dictionary<utf8>` has 16 distinct values.

<!-- BEGIN_AUTOGEN bench:arrow-encode-decode -->
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="bench-results/charts/arrow-encode-decode-dark.svg">
  <img src="bench-results/charts/arrow-encode-decode-light.svg" alt="wireform-arrow IPC stream encode + decode (100k rows)">
</picture>

| Operation                 |   encode |   decode | ratio |
| :------------------------ | -------: | -------: | ----: |
| int64                     |  1571 µs |   153 µs | 0.10x |
| double                    |  1707 µs |   189 µs | 0.11x |
| nullable int64            |  3007 µs |  5963 µs | 1.98x |
| utf8                      | 12022 µs |  6756 µs | 0.56x |
| nullable utf8             | 15433 µs |  8658 µs | 0.56x |
| mixed 6-col               | 38249 µs | 29993 µs | 0.78x |
| list<int32>               | 15348 µs |  1206 µs | 0.08x |
| struct<int32,double,bool> |  3810 µs |  1133 µs | 0.30x |
| dictionary<utf8>          |  1631 µs |   273 µs | 0.17x |

<sub>Last run 2026-10-07 23:32:59 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:arrow-encode-decode -->

The same workloads as a small (100-row) batch, where per-message framing
dominates:

<!-- BEGIN_AUTOGEN bench:arrow-encode-decode-small -->
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="bench-results/charts/arrow-encode-decode-small-dark.svg">
  <img src="bench-results/charts/arrow-encode-decode-small-light.svg" alt="wireform-arrow IPC stream encode + decode (100-row batch)">
</picture>

| Operation                 | encode (100 rows) | decode (100 rows) | ratio |
| :------------------------ | ----------------: | ----------------: | ----: |
| int64                     |           4.06 µs |           0.76 µs | 0.19x |
| double                    |           4.17 µs |           0.79 µs | 0.19x |
| nullable int64            |           5.16 µs |           1.90 µs | 0.37x |
| utf8                      |           5.85 µs |           3.93 µs | 0.67x |
| nullable utf8             |           7.12 µs |           4.13 µs | 0.58x |
| mixed 6-col               |           19.3 µs |           11.6 µs | 0.60x |
| list<int32>               |           6.60 µs |           2.11 µs | 0.32x |
| struct<int32,double,bool> |           8.90 µs |           2.14 µs | 0.24x |
| dictionary<utf8>          |           7.86 µs |           1.94 µs | 0.25x |

<sub>Last run 2026-10-07 23:32:59 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:arrow-encode-decode-small -->

The mixed 6-column table through each public entry point: the
`Arrow.Stream` and `Arrow.Write` stream writers, the file format,
the `Arrow.File` reader, and a typed `Arrow.Record` table derived with
`Arrow.Record.TH.deriveTable` (records to bytes, and bytes to records):

<!-- BEGIN_AUTOGEN bench:arrow-api-paths -->
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="bench-results/charts/arrow-api-paths-dark.svg">
  <img src="bench-results/charts/arrow-api-paths-light.svg" alt="wireform-arrow entry points, mixed 6-column table (100k rows)">
</picture>

| Operation                    | mixed 6-col | ratio |
| :--------------------------- | ----------: | ----: |
| stream encode (Arrow.Stream) |    37139 µs |     - |
| stream encode (Arrow.Write)  |    37771 µs |     - |
| stream decode (Arrow.Stream) |    27818 µs |     - |
| file encode (Arrow.Stream)   |    36653 µs |     - |
| file decode (Arrow.Stream)   |    28210 µs |     - |
| file read (Arrow.File)       |    29074 µs |     - |
| typed encode (Arrow.Record)  |    48048 µs |     - |
| typed decode (Arrow.Record)  |    61321 µs |     - |

<sub>Last run 2026-10-07 23:32:59 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:arrow-api-paths -->

External comparisons are not yet measured. The benchmark pipeline
(`scripts/run-benchmarks.py`) only distills in-process criterion runs,
and none of the candidate baselines is available as a Haskell
dependency here. Candidates:

- Haskell:
  [`arrow`](https://hackage.haskell.org/package/arrow) (the long-
  standing Hackage Arrow library, primarily for FFI to arrow-cpp).
- C++: the [arrow-cpp](https://github.com/apache/arrow/tree/main/cpp)
  reference implementation, the canonical baseline.
- Rust: [`arrow`](https://crates.io/crates/arrow), the
  Apache-blessed Rust implementation used by Datafusion and Polars.
- Python: [`pyarrow`](https://pypi.org/project/pyarrow/) (already the
  correctness oracle in `wireform-arrow-pyarrow-interop`).

## License

BSD-3-Clause.

## References

- [Apache Arrow specification](https://arrow.apache.org/docs/format/Columnar.html)
- [Apache Arrow IPC format](https://arrow.apache.org/docs/format/Columnar.html#serialization-and-interprocess-communication-ipc)
- [Apache Arrow project](https://arrow.apache.org/)
