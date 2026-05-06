# Performance baseline (Parquet, end-to-end)

A 100k-row, 4-column dataset (Int64 id, Float64 score, Utf8
name, Bool active) round-tripped through wireform-parquet and
pyarrow on the same shape.

## Numbers (current main, GHC 9.6.4 -O2, single thread)

|                 | wireform | pyarrow  | ratio                     |
|-----------------|---------:|---------:|--------------------------:|
| write none      |   6.1 ms |   8.6 ms | **1.41× faster** than pyarrow |
| write snappy    |   6.1 ms |  10.0 ms | **1.64× faster**              |
| write zstd      |   8.3 ms |  12.2 ms | **1.47× faster**              |
| read none       |   4.4 ms |   2.7 ms | 1.6× slower                  |
| read snappy     |   4.4 ms |   2.2 ms | 2.0× slower                  |
| read zstd       |   3.6 ms |   2.1 ms | 1.7× slower                  |

File sizes (bytes): uncompressed=2,157,279 · snappy=1,212,588 · zstd=585,180.

In rows/second:

| workload                   | wireform     | pyarrow         |
|----------------------------|-------------:|----------------:|
| write uncompressed         | 16.4M rows/s | 11.6M rows/s    |
| read  uncompressed         | 22.6M rows/s | 36.8M rows/s    |

## History

The numbers above are the result of three perf fixes applied
together:

| version | write none | read none | vs pyarrow read |
|---------|-----------:|----------:|----------------:|
| pre-perf-pass            | 47.9 ms | 4522 ms | 2086× slower |
| + O(n²) BYTE_ARRAY decode fix | 47.9 ms | 6.5 ms  | 2.8× slower  |
| + fast PLAIN encoder         |  6.0 ms | 6.6 ms  | 2.5× slower  |
| + memcpy plain primitives    |  6.1 ms | 4.4 ms  | 1.6× slower  |

The three fixes:

1. **`Parquet.Read.decodePlainByteArray`** was using `V.snoc`
   in a tight loop, making the decode O(n²). For a 100k-row
   PLAIN BYTE_ARRAY page that is ~5 billion copies and
   accounted for ~99% of read time. Replaced with a mutable
   boxed vector. **~700× speedup on PLAIN BYTE_ARRAY columns.**
2. **`Parquet.Write.encodeColumnDataPagePayload`** went
   through `ByteString.Builder` + `BL.toStrict` for every
   element. Replaced with a single up-front allocation
   (`BSI.unsafeCreate`) and direct `pokeByteOff` for
   primitives, plus an LSB-first bit-packed loop for
   booleans and a fold + `memcpy` for byte arrays.
   **~8× write speedup, faster than pyarrow.**
3. **`Parquet.Read.decodePlain{Int,Float,Double}`** used
   `readLE32`/`readLE64` per element, which performed
   bounds-checked `BS.index` byte-by-byte and reassembled
   words with shifts. Replaced with a single `memcpy` from
   the source bytestring's foreign pointer into the
   destination primitive vector's mutable byte array
   (LE wire format = host byte order on x86_64 / aarch64).
   **~1.5–2× read speedup on numeric columns.**

The same `V.snoc`-in-loop bug existed in
`ORC.Read.decodeDecimal128Stream` and was fixed alongside the
Parquet one.

## Remaining read gap (~1.6–2× vs pyarrow)

What's left is small per-call overhead, none of it dominant
in the profile:

* Page header parse goes through an intermediate Thrift
  `TV.Value` `Vector` of fields before being destructured
  into a `PageHeader` record. A direct field-projecting
  decoder would cut a few hundred microseconds per column.
* The dictionary path for UTF-8 still does a
  `V.map decodeUtf8Lossy` over the gathered byte slices
  (one `Text` allocation per value). pyarrow keeps the
  underlying buffer and lets you index into it.
* `genericReadColumnChunk` calls `BS.take`/`BS.drop` per page
  to slice the page body; switching to offset+length pairs
  and a single `BS.take` at the end of the chunk would
  remove a small per-page allocation.

None are large enough to be worth a follow-up on their own;
together they would close most of the remaining gap.

## Reproduce

```bash
# Run the wireform end-to-end Parquet benchmark
cabal bench wireform-parquet:parquet-throughput \
  --benchmark-options='--csv /tmp/wireform_throughput.csv \
                        --time-limit 3'

# Compare against pyarrow on the same shape
python3 wireform-parquet/scripts/parquet_bench_compare.py
```

For per-column timing without criterion's harness:

```bash
cabal run wireform-parquet:parquet-percol
```
