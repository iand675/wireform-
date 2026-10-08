# wireform-columnar

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

Format-agnostic columnar primitives shared by the
[`wireform-arrow`](../wireform-arrow/), [`wireform-parquet`](../wireform-parquet/),
and [`wireform-orc`](../wireform-orc/) packages. A pull-based iterator
surface (`Columnar.Stream`), a pushdown predicate vocabulary
(`Columnar.Predicate`), an mmap-aware file loader (`Columnar.IO`), an
LZ4 codec wrapper (`Columnar.LZ4`), and SIMD-accelerated bit-unpacking
and popcount kernels (`Columnar.SIMD`).

The columnar tier in wireform is structured so that the formats share
everything that isn't format-specific. The pull-based `Iter` surface
in `Columnar.Stream` is what `Arrow.Stream`, `Parquet.HighLevel`, and
`ORC` all yield record batches into. The `Predicate` vocabulary in
`Columnar.Predicate` is what each format's pushdown evaluator
consumes. The mmap-aware file loader in `Columnar.IO` is what their
`open*` entry points use. The bit-unpacking kernels in `Columnar.SIMD`
are what Arrow validity bitmaps, Parquet bit-packed run-length
encoding, and ORC RLE v2 share. This package contains zero
format-specific code, exists so the format packages can stay
independent, and almost nobody depends on it directly.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

This package is mostly an internal dependency of the columnar format
packages. Most users will pick it up transitively by depending on
`wireform-arrow`, `wireform-parquet`, `wireform-orc`, or
`wireform-iceberg`. To use the streaming primitives directly:

```cabal
build-depends:
  base,
  wireform-columnar,
```

The package needs `liblz4` available at link time for `Columnar.LZ4`.
On Debian / Ubuntu: `apt install liblz4-dev`. On macOS:
`brew install lz4`.

## What's in here

| Module               | Role                                                      |
|----------------------|-----------------------------------------------------------|
| `Columnar.Stream`    | Pull-based `Iter` / `IterIO` plus combinators (`iterMap`, `iterFilter`, `iterTake`, `iterChunk`, `iterScan`, `iterMergeBy`, `iterIOPrefetch`, `iterParallelMap`). The yield type that Arrow / Parquet / ORC all produce row batches into. |
| `Columnar.Predicate` | Shared pushdown vocabulary: `PValue`, `PColPredicate`, `Predicate`. Per-format evaluators (`Parquet.Statistics`, `ORC.Statistics`, Iceberg expression evaluator) all consume this. |
| `Columnar.IO`        | mmap-aware file loader: `loadFile` (mmap above 64 KiB, eager below), `loadFileMmap`, `loadFileEager`. |
| `Columnar.LZ4`       | LZ4 frame and block-format codec wrapper around the system `liblz4`. |
| `Columnar.SIMD`      | SIMD-accelerated bit-unpacking, popcount, LSB-first packed-bool unpack (Arrow / Parquet validity bitmaps). C kernel in `cbits/columnar_simd.c` using vendored simde for SSE2 / AVX2 / NEON portability. |

## Pull-based iterators

`Columnar.Stream.Iter` is a step-pull iterator that returns either a
new value plus a continuation, end-of-stream, or an error:

```haskell
data IterStep a where ...

iterFromVector :: V.Vector a -> Iter a
iterMap        :: (a -> b)   -> Iter a -> Iter b
iterFilter     :: (a -> Bool) -> Iter a -> Iter a
iterTake       :: Int        -> Iter a -> Iter a
```

The `IterIO` variant carries an `IO` action per step, which is what
the file-backed columnar readers actually use:

```haskell
iterIOPrefetch :: Int -> IterIO a -> IterIO a
iterParallelMap :: Int -> (a -> IO b) -> IterIO a -> IterIO b
```

`iterIOPrefetch n` reads `n` batches ahead so I/O overlaps with the
consumer's work; `iterParallelMap n` runs `n` worker threads applying
the per-batch function in parallel and yields results in input order.

## Predicate vocabulary

`Columnar.Predicate` is the shape pushdown filters take across the
columnar stack. Per-format evaluators evaluate a `Predicate` against
column statistics (Parquet), bloom filters (Parquet, Iceberg, ORC),
or row indexes (ORC), and decide which row groups / stripes / row
ranges can be skipped:

```haskell
data PValue        = PInt64 Int64 | PDouble Double | PText Text | ...
data PColPredicate = Eq PValue | Lt PValue | ... | In [PValue] | NotNull
data Predicate     = ColP Text PColPredicate
                   | And  Predicate Predicate
                   | Or   Predicate Predicate
                   | Not  Predicate
```

The Iceberg, Parquet, and ORC packages all consume the same vocabulary
so a query planner can build one `Predicate` and have every format
evaluate it.

## mmap-aware loading

`Columnar.IO.loadFile` picks between mmap and an eager `ByteString`
read based on file size: above 64 KiB it mmaps, below it reads. The
eager path is faster for small files (mmap setup costs more than the
read itself); the mmap path keeps RSS flat for the large files
columnar formats are usually applied to. Callers that want to force
the choice use `loadFileMmap` or `loadFileEager` directly.

## SIMD kernels

`Columnar.SIMD` exposes the Haskell side of the C kernel in
[`cbits/columnar_simd.c`](cbits/columnar_simd.c). Two kernels share
this code:

- LSB-first bit-unpacking for Arrow validity bitmaps and Parquet
  bit-packed run-length encoding.
- Popcount over Arrow validity bitmaps for null-counting and
  null-mask arithmetic.

The kernel uses vendored simde headers (under `include/simde/`) so
it compiles to SSE2 / AVX2 on x86 and NEON on aarch64 without any
target-specific Haskell. Build flag is `-march=native`, applied at
the C layer only.

## Testing

```bash
cabal test wireform-columnar:wireform-columnar-test
```

Property-based tests cover the iterator combinators (idempotence,
fusion equivalence, prefetch / parallelism preserving order),
predicate evaluation, the LZ4 frame round-trip, and the SIMD kernels
against scalar reference implementations.

## Benchmarks

No per-package criterion harness in tree yet. The columnar primitives
are exercised through the format packages' own benchmarks (Arrow IPC
read + write, Parquet column reads, ORC stripe reads). Standalone
micro-benchmarks against scalar Haskell baselines for the SIMD
kernels and against `Streaming` / `pipes` / `conduit` for the
iterator surface are planned.

> Numbers TBD: harness pending.

## Improvement ideas

- **SIMD bulk copy kernel.** Add a 16-byte-chunked SIMDe copy
  (load/store `__m128i` with a scalar `memcpy` tail) to
  `cbits/columnar_simd.c` for runs of values inside RLE / dictionary
  pages. Should land together with a micro-benchmark showing a win
  over libc `memcpy` on real Parquet / Arrow page bodies — and with
  actual call sites, since an earlier version existed but was never
  wired into any reader.

## License

BSD-3-Clause. Vendored [simde](https://github.com/simd-everywhere/simde)
headers under `include/simde/` carry their own MIT license.
