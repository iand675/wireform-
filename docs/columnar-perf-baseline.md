# Performance baseline (Parquet, end-to-end)

A 100k-row, 4-column dataset (Int64 id, Float64 score, Utf8
name, Bool active) round-tripped through wireform-parquet and
**single-threaded** pyarrow on the same shape.

## Numbers (current main, GHC 9.6.4 -O2, single thread)

|                 | wireform | pyarrow | ratio                              |
|-----------------|---------:|--------:|-----------------------------------:|
| write none      |   4.9 ms |  8.5 ms | **1.74× faster** than pyarrow      |
| write snappy    |   5.9 ms |  9.9 ms | **1.68× faster**                   |
| write zstd      |   8.1 ms | 12.0 ms | **1.48× faster**                   |
| read none       |   2.7 ms |  2.6 ms | **1.02× — tied** (within noise)    |
| read snappy     |   4.5 ms |  4.5 ms | **1.00× — tied** (within noise)    |
| read zstd       |   3.6 ms |  4.6 ms | **1.28× faster**                   |

File sizes (bytes): uncompressed=2,157,279 · snappy=1,212,588 · zstd=585,180.

In rows/second:

| workload                   | wireform     | pyarrow         |
|----------------------------|-------------:|----------------:|
| write uncompressed         | 20.4M rows/s | 11.7M rows/s    |
| read  uncompressed         | 37.5M rows/s | 38.5M rows/s    |
| read  zstd                 | 27.8M rows/s | 21.7M rows/s    |

**wireform-parquet now ties or beats single-threaded pyarrow on every dimension of this workload.**

## History

| version | write none | read none | vs pyarrow read     |
|---------|-----------:|----------:|--------------------:|
| pre-perf-pass                              | 47.9 ms | 4522 ms | 2086× slower |
| + O(n²) BYTE_ARRAY decode fix              | 47.9 ms |  6.5 ms | 2.8× slower  |
| + fast PLAIN encoder                       |  6.0 ms |  6.6 ms | 2.5× slower  |
| + memcpy plain primitives                  |  6.1 ms |  4.4 ms | 1.6× slower  |
| + Utf8 fast path (shared ByteArray)        |  6.0 ms |  4.5 ms | 1.7× slower\* |
| + corrected bench labels (was reading snappy) |  4.9 ms |  2.7 ms | **1.02× tied** |

\* Earlier "4.4 ms" reads were misreported because the bench
forced only WHNF on the lazy column wrappers, not the
underlying decoded values; the Utf8-fast-path change pushed
the actual decode cost forward and the new bench captures it
honestly. The corrected bench then revealed a label mistake
where `writeFile_` was using the default Snappy compression,
so the "uncompressed read" benchmark had been measuring a
Snappy-compressed file all along.

The five fixes applied during this perf pass:

1. **`Parquet.Read.decodePlainByteArray`** was using `V.snoc`
   in a tight loop, making the decode O(n²). For a 100k-row
   PLAIN BYTE_ARRAY page that is ~5 billion copies and
   accounted for ~99% of read time. Replaced with a mutable
   boxed vector.
2. **`Parquet.Write.encodeColumnDataPagePayload`** went
   through `ByteString.Builder` + `BL.toStrict` for every
   element. Replaced with a single up-front allocation
   (`BSI.unsafeCreate`) and direct `pokeByteOff` for
   primitives, plus an LSB-first bit-packed loop for
   booleans and a fold + `memcpy` for byte arrays.
3. **`Parquet.Read.decodePlain{Int,Float,Double}`** used
   `readLE32`/`readLE64` per element, which performed
   bounds-checked `BS.index` byte-by-byte and reassembled
   words with shifts. Replaced with a single `memcpy` from
   the source bytestring's foreign pointer into the
   destination primitive vector's mutable byte array.
4. **`Parquet.Read.dispatchUtf8`** + **`decodePlainByteArrayAsText`**
   added a Utf8-aware decode path: instead of decoding to
   `V.Vector ByteString` then `V.map decodeUtf8Lossy`
   (which allocates one `Text` + underlying `ByteArray` per
   value), one bulk `memcpy` of the page body produces a
   shared `Data.Text.Array.Array`, then N `Text` values
   point into it via the unsafe `TI.text` constructor.
   ASCII-precheck via a `Word64`-chunked scan; non-ASCII
   data falls back to per-value lossy decode.
5. **`Parquet.Read.genericReadColumnChunk`** stopped doing
   `ppEmpty pp ++ pageVec` for the first page (which
   memcpy'd 800 KB into a fresh Int64 vector for nothing).
   Track the accumulator as `Maybe a`; the first page
   becomes the accumulator directly.

The same `V.snoc`-in-loop bug existed in
`ORC.Read.decodeDecimal128Stream` and was fixed alongside.

## Bench fairness

The criterion benchmark was rewritten to actually force the
decoded values:

* For primitive columns (`ColInt32`/`ColInt64`/`ColFloat`/
  `ColDouble`) the data lives in a strict `ByteArray`, so
  the decode is fully done as soon as the vector
  constructor is reached. `VP.length` suffices to force
  the work. We don't sum the values because pyarrow's
  `read_table` doesn't either.
* For boxed columns (`ColBool`/`ColUtf8`/`ColBinary`)
  elements can be lazy thunks, so we walk the spine and
  force each element to WHNF. This matches what pyarrow's
  `read_table` does (it materialises the Arrow buffers).

The Python comparator runs `pq.read_table(reader, use_threads=False)`
to keep the comparison honest (single-thread vs
single-thread). With pyarrow's default thread pool the
unfair-but-typical measurement still has wireform ahead on
writes and behind by ~2× on uncompressed reads.

## Reproduce

```bash
cabal bench wireform-parquet:parquet-throughput \
  --benchmark-options='--csv /tmp/wireform_throughput.csv \
                        --time-limit 3'
python3 wireform-parquet/scripts/parquet_bench_compare.py
```

For per-column timing (no criterion harness):

```bash
cabal run wireform-parquet:parquet-percol
```
