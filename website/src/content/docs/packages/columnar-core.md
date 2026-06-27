---
title: wireform-columnar-core
description: "Format-agnostic columnar primitives: pull-based iterators, pushdown predicates, mmap-aware loading, an LZ4 codec, and SIMD bit-unpacking kernels."
sidebar:
  order: 48
---

`wireform-columnar-core` is where the shared, format-agnostic columnar
primitives actually live. The Arrow, Parquet, ORC, and Iceberg packages
all build on it so that everything which isn't format-specific is
implemented and tuned once. It contains zero format-specific code and
almost nobody depends on it directly; you usually pick it up
transitively through `wireform-arrow`, `wireform-parquet`,
`wireform-orc`, or `wireform-iceberg`.

## Not to be confused with `wireform-columnar`

This package and [`wireform-columnar`](../columnar/) are different
things:

- **`wireform-columnar-core`** (this package) holds the real
  `Columnar.*` primitive modules: `Columnar.IO`, `Columnar.LZ4`,
  `Columnar.Predicate`, `Columnar.SIMD`, and `Columnar.Stream`.
- **`wireform-columnar`** is a thin facade exposing the single module
  `Wireform.Columnar`, which re-exports the per-format decode entry
  points (the unified encode/decode API across Parquet, Arrow IPC, and
  ORC).

If you want the unified cross-format API, see
[`wireform-columnar`](../columnar/). If you want the building blocks it
and the format packages share, you're in the right place.

## What's inside

| Module | Role |
|--------|------|
| `Columnar.Stream` | Pull-based `Iter` / `IterIO` plus combinators; the yield type Arrow / Parquet / ORC produce row batches into |
| `Columnar.Predicate` | Shared pushdown vocabulary (`PValue`, `PColPredicate`, `Predicate`) consumed by each format's evaluator |
| `Columnar.IO` | mmap-aware file loader: `loadFile`, `loadFileMmap`, `loadFileEager` |
| `Columnar.LZ4` | LZ4 frame and block-format codec wrapper around the system `liblz4` |
| `Columnar.SIMD` | SIMD-accelerated bit-unpacking, popcount, and bulk-copy kernels |

## Pull-based iterators

`Columnar.Stream` is a step-pull iterator surface. `Iter` returns
either a new value plus a continuation, end-of-stream, or an error;
the `IterIO` variant carries an `IO` action per step, which is what the
file-backed columnar readers use. Combinators include `iterMap`,
`iterFilter`, `iterTake`, `iterChunk`, `iterScan`, `iterMergeBy`,
`iterIOPrefetch`, and `iterParallelMap`. `iterIOPrefetch n` reads `n`
batches ahead so I/O overlaps with the consumer's work;
`iterParallelMap n` runs `n` worker threads applying the per-batch
function in parallel and yields results in input order.

## Predicate vocabulary

`Columnar.Predicate` is the shape pushdown filters take across the
columnar stack. Per-format evaluators (`Parquet.Statistics`,
`ORC.Statistics`, the Iceberg expression evaluator) evaluate a
`Predicate` against column statistics, bloom filters, or row indexes
and decide which row groups, stripes, or row ranges can be skipped. The
Iceberg, Parquet, and ORC packages all consume the same vocabulary so a
query planner can build one `Predicate` and have every format evaluate
it.

## mmap-aware loading

`Columnar.IO.loadFile` picks between mmap and an eager `ByteString`
read based on file size: above 64 KiB it mmaps, below it reads eagerly.
The eager path is faster for small files (mmap setup costs more than the
read itself); the mmap path keeps RSS flat for the large files columnar
formats are usually applied to. Callers that want to force the choice
use `loadFileMmap` or `loadFileEager` directly.

## SIMD kernels

`Columnar.SIMD` exposes the Haskell side of the C kernel in
`cbits/columnar_simd.c`. Three kernels share this code:

- LSB-first bit-unpacking for Arrow validity bitmaps and Parquet
  bit-packed run-length encoding.
- Popcount over Arrow validity bitmaps for null-counting and null-mask
  arithmetic.
- 16-byte bulk `memcpy` for runs of values inside RLE / dictionary
  pages.

The kernel uses vendored [simde](https://github.com/simd-everywhere/simde)
headers (under `include/simde/`) so it compiles to SSE2 / AVX2 on x86
and NEON on aarch64 without any target-specific Haskell. The build flag
is `-march=native`, applied at the C layer only.

## System dependencies

The package needs `liblz4` available at link time for `Columnar.LZ4`.
On Debian / Ubuntu: `apt install liblz4-dev`. On macOS: `brew install
lz4`. It is light enough (< 100 KiB) that it is not gated behind a Cabal
flag; if it's missing, the link fails with a clear `cannot find -llz4`
message.
