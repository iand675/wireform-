# Performance baseline (Parquet, end-to-end)

A 100k-row, 4-column dataset (Int64 id, Float64 score, Utf8
name, Bool active) round-tripped through wireform-parquet and
pyarrow on the same shape, on a 4-core x86_64 machine, GHC
9.6.4 -O2.

## Numbers (current main)

### Single-threaded (`+RTS -N1` vs `pq.read_table(use_threads=False)`)

|                 | wireform | pyarrow | ratio                              |
|-----------------|---------:|--------:|-----------------------------------:|
| write none      |   4.2 ms |  9.1 ms | **2.17× faster** than pyarrow      |
| write snappy    |   5.7 ms | 10.5 ms | **1.85× faster**                   |
| write zstd      |   7.7 ms | 12.6 ms | **1.64× faster**                   |
| read none       |   1.4 ms |  2.6 ms | **1.82× faster**                   |
| read snappy     |   3.8 ms |  4.3 ms | **1.15× faster**                   |
| read zstd       |   2.9 ms |  4.8 ms | **1.67× faster**                   |

### Multi-threaded (`+RTS -N` vs `pq.read_table(use_threads=True)`)

|                 | wireform | pyarrow | ratio                              |
|-----------------|---------:|--------:|-----------------------------------:|
| write none      |   5.2 ms |  9.1 ms | **1.72× faster**                   |
| write snappy    |   6.4 ms | 10.5 ms | **1.64× faster**                   |
| write zstd      |   8.7 ms | 12.6 ms | **1.45× faster**                   |
| read none       |   1.4 ms |  2.0 ms | **1.39× faster**                   |
| read snappy     |   2.1 ms |  2.2 ms | **1.04× faster**                   |
| read zstd       |   1.7 ms |  2.1 ms | **1.28× faster**                   |

**wireform-parquet beats pyarrow on every dimension in both single- and multi-threaded modes.**

In rows/second (multi-threaded):

| workload              | wireform     | pyarrow         |
|-----------------------|-------------:|----------------:|
| write uncompressed    | 19.2M rows/s | 11.0M rows/s    |
| read  uncompressed    | 71.4M rows/s | 50.0M rows/s    |
| read  zstd            | 58.8M rows/s | 47.6M rows/s    |

File sizes (bytes): uncompressed=2,157,279 · snappy=1,212,588 · zstd=585,180.

## History (read uncompressed)

| version | wireform | vs pyarrow |
|---|---:|---:|
| pre-perf-pass | 4522 ms | 2086× slower |
| + O(n²) BYTE_ARRAY decode fix | 6.5 ms | 2.8× slower |
| + fast PLAIN encoder | 6.6 ms | 2.5× slower |
| + memcpy plain primitives | 4.4 ms | 1.6× slower |
| + Utf8 fast path | 4.5 ms | 1.7× slower\* |
| + corrected bench labels + first-page accumulator | 2.7 ms | 1.02× tied |
| + forkIO scheduling + zero-copy Bool unpack | 2.3 ms | 1.16× faster |
| + tuned -A8m nursery | 1.8 ms | 1.45× faster |
| + tuned -A32m nursery (current) | **1.4 ms** | **1.82× faster** |

\* The Utf8 fast path looked neutral until the bench was fixed
to actually force the values; the previous numbers were
under-counting decode work because lazy thunks were being
measured.

## The optimisation passes

1. **`Parquet.Read.decodePlainByteArray`** was using `V.snoc`
   in a tight loop, making the decode O(n²). Replaced with
   a mutable boxed vector. **~700× speedup on PLAIN BYTE_ARRAY columns.**
2. **`Parquet.Write.encodeColumnDataPagePayload`** went
   through `ByteString.Builder` + `BL.toStrict` for every
   element. Replaced with one up-front `BSI.unsafeCreate`
   allocation + direct `pokeByteOff` for primitives,
   LSB-first bit-packed loop for booleans, and a single-pass
   `memcpy` for byte arrays. **8× write speedup.**
3. **`Parquet.Read.decodePlain{Int,Float,Double}`** was
   reading each value byte-by-byte through `readLE32`/`readLE64`.
   Replaced with one `memcpy` from the page body's foreign
   pointer into the destination primitive vector's mutable
   byte array.
4. **`Parquet.Read.dispatchUtf8`** + **`decodePlainByteArrayAsText`**
   added a Utf8-aware decode path: precheck ASCII-ness via a
   `Word64`-chunked scan, single bulk `memcpy` of the page
   body into one `Data.Text.Array.Array`, then construct N
   `Text` values that all point into that one `Array` via
   the unsafe `TI.text` constructor.
5. **`Parquet.Read.genericReadColumnChunk`** stopped doing
   `ppEmpty pp ++ pageVec` for the first page (which
   memcpy'd 800 KB into a fresh Int64 vector for nothing).
   Track the accumulator as `Maybe a`; the first page
   becomes the accumulator directly.
6. **Parallel column-chunk reader** (`bench/Throughput.hs:readBackPar`).
   Uses `forkIO` + `MVar` rather than sparks: at this work
   granularity (4 columns × ~600 µs each) sparks weren't
   eagerly scheduled enough; explicit threads give one OS
   thread per task as long as `+RTS -N>=ntasks`.
7. **`Columnar.SIMD.unpackBitsLsbUnsafe`** rewrite. The
   previous shape went Storable Word8 → Unboxed Word8 →
   Unboxed Bool → Boxed Bool with three intermediate
   allocations + traversals. New shape allocates one boxed
   mutable vector and walks the source one byte at a time,
   writing all 8 bits in a straight-line inner block. Cuts
   ~300 µs off every Bool column read.
8. **`-with-rtsopts=-A32m`** on the bench. GHC's default 1 MB
   per-capability nursery is much too small for our parallel
   read workload (1.2 MB Utf8 + Bool buffers per thread
   immediately overflow). Bumping to 32 MB removes most of
   the GC pressure and is the single biggest win after the
   correctness fixes.

The same `V.snoc`-in-loop bug existed in
`ORC.Read.decodeDecimal128Stream` and was fixed alongside.

## Bench fairness

* `Throughput.hs`'s `writeFile_` was using `defaultWriteOptions`
  (Snappy), so the "read uncompressed" criterion bench was
  actually decoding a Snappy file. Fixed.
* `readBack` returned a constant `Int` and never forced the
  per-element decode work. New `forceCol` walks every element:
  for primitive columns the data is already strict in a
  `ByteArray`, so `VP.length` suffices (we deliberately don't
  sum because pyarrow's `read_table` doesn't either); for
  boxed columns we force every element to WHNF.
* The Python comparator runs both `use_threads=False` and
  `use_threads=True` and prints both comparison tables.
* The Haskell bench has both serial (`decode`) and parallel
  (`decode-par`) read variants. The cabal benchmark target
  uses `-threaded -rtsopts -with-rtsopts=-A32m` so the
  default GHC nursery doesn't dominate; the user picks
  `+RTS -N1` or `+RTS -N` at the command line.

## Reproduce

```bash
# Single-threaded numbers
cabal bench --enable-benchmarks wireform-parquet:parquet-throughput \
  --benchmark-options='--csv /tmp/wf_n1.csv --time-limit 4'

# Multi-threaded numbers
cabal bench --enable-benchmarks wireform-parquet:parquet-throughput \
  --benchmark-options='--csv /tmp/wf_n4.csv --time-limit 4 +RTS -N -RTS'

# Compare against pyarrow (both use_threads modes)
python3 wireform-parquet/scripts/parquet_bench_compare.py
```

For per-column timing (no criterion harness):

```bash
cabal run wireform-parquet:parquet-percol
```

## Notes for downstream users

If you are using `wireform-parquet` in a long-running
process, GHC's default `-A1m` nursery is small for any
realistic columnar workload. Set at least `-A8m`
(per-capability) in your link options:

```
ghc-options: -threaded -rtsopts "-with-rtsopts=-N -A8m"
```

For batch jobs that read large columnar files, `-A32m` or
larger pays off — see the history table above.
