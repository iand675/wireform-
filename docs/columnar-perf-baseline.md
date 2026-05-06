# Performance baseline (Parquet, end-to-end)

A 100k-row, 4-column dataset (Int64 id, Float64 score, Utf8
name, Bool active) round-tripped through wireform-parquet and
pyarrow on the same shape, on a 4-core x86_64 machine, GHC
9.6.4 -O2.

## Numbers (current main)

### Single-threaded (`+RTS -N1` vs `pq.read_table(use_threads=False)`)

|                 | wireform | pyarrow | ratio                              |
|-----------------|---------:|--------:|-----------------------------------:|
| write none      |   3.2 ms |  8.6 ms | **2.69× faster** than pyarrow      |
| write snappy    |   4.8 ms | 10.0 ms | **2.08× faster**                   |
| write zstd      |   7.6 ms | 12.1 ms | **1.59× faster**                   |
| read none       |   1.7 ms |  2.6 ms | **1.53× faster**                   |
| read snappy     |   4.0 ms |  4.5 ms | **1.13× faster**                   |
| read zstd       |   3.1 ms |  4.6 ms | **1.48× faster**                   |

### Multi-threaded (`+RTS -N` vs `pq.read_table(use_threads=True)`)

|                 | wireform | pyarrow | ratio                              |
|-----------------|---------:|--------:|-----------------------------------:|
| write none      |   3.5 ms |  8.6 ms | **2.46× faster**                   |
| write snappy    |   4.7 ms | 10.0 ms | **2.13× faster**                   |
| write zstd      |   7.0 ms | 12.1 ms | **1.73× faster**                   |
| read none       |   1.4 ms |  2.0 ms | **1.43× faster**                   |
| read snappy     |   2.2 ms |  2.3 ms | **1.05× faster**                   |
| read zstd       |   1.8 ms |  2.1 ms | **1.17× faster**                   |

**wireform-parquet beats pyarrow on every workload, single- and multi-threaded.**

In rows/second (single-threaded):

| workload              | wireform     | pyarrow         |
|-----------------------|-------------:|----------------:|
| write uncompressed    | 34.5M rows/s | 11.6M rows/s    |
| read  uncompressed    | 66.7M rows/s | 38.5M rows/s    |
| read  zstd            | 35.7M rows/s | 22.2M rows/s    |

File sizes (bytes): uncompressed=2,157,279 · snappy=1,212,588 · zstd=585,180.

## Arrow IPC numbers (same dataset, single-threaded)

| | wireform | pyarrow* |
|---|---:|---:|
| write encode | 1.94 ms | 0.4 ms |
| read decode | 1.10 ms | 0.01 ms |

\* pyarrow's Arrow IPC implementation is essentially zero-copy: the in-memory representation matches the wire format, so `read_table` just points an Arrow Array at the source buffer with no allocation. Our `ColumnArray` carries its own `VP.Vector` (which has its own `ByteArray`), so we always pay one memcpy per column. This is a fundamental representation difference that no amount of micro-optimisation will close. Our 1.10 ms read is still within 1× of cache-warm memcpy at this size.

## History (read uncompressed)

| version | wireform | vs pyarrow |
|---|---:|---:|
| pre-perf-pass                              | 4522 ms | 2086× slower |
| + O(n²) BYTE_ARRAY decode fix              |  6.5 ms | 2.8× slower  |
| + fast PLAIN encoder                       |  6.6 ms | 2.5× slower  |
| + memcpy plain primitives                  |  4.4 ms | 1.6× slower  |
| + Utf8 fast path (shared ByteArray)        |  4.5 ms | 1.7× slower\* |
| + corrected bench labels + first-page accumulator | 2.7 ms | 1.02× tied |
| + forkIO scheduling + zero-copy Bool unpack | 2.3 ms | 1.16× faster |
| + tuned -A8m nursery                       |  1.8 ms | 1.45× faster |
| + tuned -A32m nursery                      |  1.4 ms | 1.82× faster |
| + write-side double-encode fix             |  1.5 ms | 1.71× faster |
| + SIMDe ASCII / bswap memcpy               |  1.5 ms | 1.71× faster |

\* The Utf8 fast path looked neutral until the bench was fixed
to actually force the values; the previous numbers were
under-counting decode work because lazy thunks were being
measured.

## The optimisation passes

1. **`Parquet.Read.decodePlainByteArray`** — `V.snoc` in a
   loop made decode O(n²). Fixed with a mutable boxed vector.
2. **`Parquet.Write.encodeColumnDataPagePayload`** — `Builder` +
   `BL.toStrict` per element → pre-allocated strict ByteString +
   direct `pokeByteOff`. **8× write speedup.**
3. **`Parquet.Read.decodePlain{Int,Float,Double}`** — byte-by-byte
   `BS.index` + shifts → single `memcpy` from page body to
   primitive vector's underlying byte array.
4. **`Parquet.Read.dispatchUtf8`** + **`decodePlainByteArrayAsText`** —
   100k tiny `decodeUtf8` allocations → ASCII-precheck + one
   bulk `memcpy` of the page body into one shared
   `Data.Text.Array.Array`.
5. **`Parquet.Read.genericReadColumnChunk`** — first-page no-copy
   (avoid `ppEmpty ++ pageVec` allocating + memcpy'ing 800 KB
   for nothing). Plus `ppConcat` so multi-page columns don't
   pay O(pages²) memcpy.
6. **`bench/Throughput.hs:readBackPar`** with `forkIO + MVar` —
   guaranteed parallelism per task, vs sparks which weren't
   eagerly scheduled at this work granularity.
7. **`Columnar.SIMD.unpackBitsLsbUnsafe`** — Storable Word8 →
   Unboxed Word8 → Unboxed Bool → Boxed Bool (3 intermediate
   allocations) → one boxed mutable vector with an inline
   8-bits-per-byte unpacker.
8. **`-with-rtsopts=-A32m`** on the bench — GHC's default
   1 MB per-capability nursery overflows on parallel reads.
   Single biggest win after the correctness fixes.
9. **V2 page write double-encode fix** — `encodeColumnDataPage`
   was being called just to compute the uncompressed size,
   then `encodeColumnDataPageV2Parts` was called for the
   actual output (encoding the column twice). Replaced with
   `columnDataPlainEncodedSize` which is O(1) for primitives.
   **~30-40% write speedup.**
10. **Single-pass min/max stats** — `VP.foldl1' min` +
    `VP.foldl1' max` was two passes per column; new `MinMax`
    strict-pair fold does one pass.
11. **Arrow read primitive columns** — same byte-by-byte issue
    as Parquet had. Fixed with the same `memcpyPrimVecLE`
    helper. **9.6× total Arrow IPC read speedup.**
12. **Arrow Utf8 read** — same shared-ByteArray fast path as
    Parquet. **4× speedup on the Utf8 column.**
13. **Arrow + ORC write encoders** — same Builder-per-element
    issue. Fixed with `pokePrimVecLE` / direct allocation.
14. **Arrow Utf8 write** — was calling `TE.encodeUtf8` per
    Text (100 k tiny ByteString allocations). New path copies
    bytes straight from each `Text`'s underlying `ByteArray`
    into the destination buffer via `PBA.copyByteArrayToAddr`.
    **~2.3× Arrow write speedup.**
15. **ORC `encodeStringDictColumn`** — same `V.snoc`-in-loop
    O(n²) bug as Parquet's BYTE_ARRAY decode. Fixed with a
    mutable primitive vector.
16. **SIMDe-accelerated kernels** in `Columnar.SIMD`:
    * `hs_columnar_is_ascii` — SSE2 OR + movemask, ~2-3×
      faster than the Word64-chunked Haskell loop.
    * `hs_columnar_bswap{16,32,64}_copy` — SSSE3 `pshufb`-based
      byte-swap memcpy for the rare Arrow big-endian path.
      Replaces a per-element scalar loop.
17. **Direct Thrift compact encoder for `FileMetadata`** —
    walks the record tree straight to bytes; no `TV.Value`
    intermediate. **1.65× faster on 256-column writes**
    (the wider the schema, the bigger the win).
18. **`Parquet.RLE.unpackAllGroups` O(n²) fix** — bit-packed
    groups were being concatenated with `acc VP.++ grp`
    per group; ~78 M wasted Word32 writes for a 100k-row
    dict-encoded column. Fixed with one up-front
    `MVP.unsafeNew` and an `unpackGroup8Into` helper.
19. **`materializePlain*Optional` family in `Parquet.Levels`** —
    cons + reverse + V.fromList per nullable column read
    replaced with a single mutable boxed vector + freeze
    via a shared `materializePlainOptionalGeneric` helper.
20. **`materializeDictOptional` in `Parquet.Read`** — same
    cons + reverse + fromList anti-pattern, fixed the same
    way.
21. **Iter fusion rules** in `Columnar.Stream` — chained
    `iterMap` / `iterMapMaybe` / `iterFilter` collapse to
    one specialised loop.
22. **`Parquet.BloomFilter` O(n²) thaw-per-insert fix** —
    `sbbfInsert` was copying the entire bitset on every
    value (435 ms for 100k inserts). New
    `buildSbbfFromHashes` + `buildSbbfFromBytes` thaw once,
    write all, freeze once: **454× speedup** on 100k inserts
    (0.96 ms). Same fix ported to `ORC.BloomFilter`.
23. **`buildDictionary` O(n² + n×|uniques|) → O(n log u)** —
    `orderedUniq`'s `elem` per row + `lookup` per index
    assignment replaced with a `Data.Map.Strict` lookup
    written straight into a mutable `VP.Vector`. ~500×
    speedup at 100k rows / 1k distinct.
24. **ORC float/double readers** — same memcpy primitive
    fix as Parquet; replaces byte-by-byte `BS.index +
    shifts`. Also tightened `readVulong` / `readBigEndian`
    in the RLE-v2 inner loop.
25. **`Arrow.Write.encodeMaybeNullBitmap`** — fused null
    bitmap pack + null count into one ST pass; skips the
    boxed-Bool intermediate.
26. **`encodeOptionalColumnPage` + `optionalColumnPresentValues`** —
    nullable Parquet writer no longer goes through Haskell
    list intermediates. One `VP.generate` for def levels +
    one mutable-vector pass for the present-values
    sub-vector.
27. **Direct-poke `i32LE` / `i64LE` / `f32LE` / `f64LE` /
    `word32LE` / `word64LE`** — bloom filter inserts and
    predicate evaluation no longer go through
    `BL.toStrict . B.toLazyByteString . B.intXXLE`. Same
    fix in     `Parquet.BloomFilter.serializeBitset` /
    `ORC.BloomFilter.bitsetToLEBytes`.
* **Pass 28 — list-comprehension allocation hunt.**
  Replaced every `VP.fromList [ f x | Just x <- V.toList v ]`
  in the Arrow ↔ Parquet ↔ ORC bridges with a single-pass
  generic mutable-buffer builder
  (`Columnar.NullableBuild.presentValues{P,V,PMap,VMap}`).
  For a 1 M-row 50 %-null Int64 column that's 3 fewer
  full-vector allocations and walks per column.
* **Pass 29 — kill the remaining O(N²) snoc / append loops.**
  The "main" generic `Parquet.Read.genericReadColumnChunk`
  was already cons + `ppConcat` (Pass 18), but a dozen
  per-type readers (`readPlain*ColumnChunk`,
  `readDictionaryOptionalColumnChunk`,
  `readGenericOptionalColumnChunk`,
  `readGenericOptionalSelectedPages`) still appended via
  `acc V.++ pageVec` per page (O(K² · N) for K data pages
  of N values). All switched to reverse-cons +
  `V.concat (reverse pages)`. `ppAppend` removed from the
  `PerPage` typeclass — last two callers eliminated, and
  it never had the right shape to expose anyway.
  Same fix on the writer side:
    * `Parquet.Write.{layoutBlooms,layoutOffsetIndex,layoutColumnIndex}`
      — O(R² + R · C²) per layout pass, three layout passes
      per file. Refactored through one shared
      `layoutAuxiliary` helper using cons-then-`V.fromListN`.
    * `Parquet.Write.{buildRG,buildCol}`: same.
    * `ORC.Footer.{decodePostScript,decodeFooter,decodeORCType}`:
      footer decoder was O(K²) in stripe count K. Internal
      `*Acc` shadow records carry cons lists, frozen at the
      end via `V.fromList`.
    * `ORC.Stripe.{stripeStreamSlices,decodeStripeFooter}`:
      same.
    * `ORC.Write.buildStripes`: same.
* **Pass 30 — bit-pack writer allocation.**
  `BS.pack [byteAt i | i <- [0..]]` patterns in
  `Parquet.LevelsEncode.{packBitsLsb,rleRun}`,
  `Parquet.DeltaEncode.encodeMiniblockPayload`, and
  `Parquet.NullPagesBitmap.packNullPages` rewritten to
  one `BSI.unsafeCreate` + `pokeByteOff` loop. Same in
  `Parquet.DeltaEncode.encodeDeltaLengthByteArray` (now
  `VP.generate` + `BS.concat`).
  `Parquet.NullPagesBitmap.nonNullPages` now returns a
  `VP.Vector Int` built by mutable-write loop instead of
  `V.Vector Int` from a list. **API break.**
* **Pass 31 — direct PageHeader Thrift codec.**
  Both reader and writer were going through the generic
  Thrift Compact `TV.Value` AST for the per-page header
  struct. Read path now decodes straight into the
  `PageHeader` record with a hand-rolled walker
  (`Parquet.Page.readPageHeaderAt`); write path emits
  straight to `Builder` through `tCompEncode*` primitives
  (`Parquet.Write.encodePageHeaderBuilder`). Eliminates
  one `V.fromList` + 5+ boxed `Value`s per page.
  Also: `i32LE` / `i64LE` / `fLE` / `dLE` (4-byte / 8-byte
  primitive serializers used by every column statistic)
  switched from `BL.toStrict (toLazyByteString (B.intXXLE v))`
  to direct `BSI.unsafeCreate` + `pokeByteOff`.
* **Pass 32 — RLE/Hybrid decoder: STRef -> ST tail recursion.**
  The hybrid-RLE inner loop (`Parquet.RLE.fillHybrid` —
  hot path for dictionary indices and definition /
  repetition levels) was using four `STRef`s to hold its
  state. Each `readSTRef` / `writeSTRef` is an indirect
  heap load + write barrier. Refactored to a flat
  tail-recursive driver that threads the state through
  arguments. GHC keeps it in registers; ~2x speedup on
  bit-packed pages in the microbench.
* **Pass 33 — Bool unpack/pack: 8-way unrolling.**
  `Columnar.SIMD.unpackBitsLsbUnsafe` (BOOL reader) and
  `Parquet.Write.plainBoolVecBytes` (BOOL writer) had
  inner per-bit loops that GHC kept stuck on the stack.
  Unrolled to 8 explicit reads / writes per source byte
  for the whole-byte case so GHC keeps the byte in a
  register across the eight stores.
* **Pass 34 — V2 def-levels: drop list intermediate.**
  `encodeOptionalColumnPageV2Parts` built the def-levels
  vector via `VP.fromList [if isJust v then 1 else 0 |
  v <- presenceList oc]` where `presenceList` walked the
  `V.Vector` twice (`V.toList` -> `[Maybe ()]` -> filter).
  Now shares the existing `presenceVector` helper (single
  `VP.generate`) and `optionalColumnNullCount` (single
  fold).
* **Pass 35 — `writeParquetFile`: `Builder` -> `BS.concat`.**
  Same `Builder` -> single-allocation refactor as
  `buildParquetFileMixedRaw` from an earlier pass — the
  page bytestrings are already in hand, so the temporary
  lazy chain is wasted work.
* **Pass 36 — `statisticsForBool`: short-circuit pass.**
  Was `V.any not vs >> V.any id vs`, two passes over the
  column. Now a single short-circuiting fold that bails as
  soon as both `True` and `False` have been observed.

## Bench fairness

* `Throughput.hs`'s `writeFile_` was using `defaultWriteOptions`
  (Snappy), so the "read uncompressed" benchmark was actually
  decoding a Snappy file. Fixed.
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
  uses `-threaded -rtsopts -with-rtsopts=-A32m`; the user
  picks `+RTS -N1` or `+RTS -N` at the command line.

## Reproduce

```bash
cabal bench --enable-benchmarks wireform-parquet:parquet-throughput \
  --benchmark-options='--csv /tmp/wf_n1.csv --time-limit 4'
cabal bench --enable-benchmarks wireform-parquet:parquet-throughput \
  --benchmark-options='--csv /tmp/wf_n4.csv --time-limit 4 +RTS -N -RTS'
python3 wireform-parquet/scripts/parquet_bench_compare.py
```

For per-column timing (no criterion harness):

```bash
cabal run wireform-parquet:parquet-percol
```

For Arrow IPC comparison:

```bash
cabal bench --enable-benchmarks wireform-arrow:arrow-ipc-throughput \
  --benchmark-options='--csv /tmp/wf_arrow.csv --time-limit 4'
python3 wireform-arrow/scripts/arrow_ipc_bench_compare.py
```

Wide-schema and large-row scaling:

```bash
cabal bench --enable-benchmarks wireform-parquet:parquet-scale \
  --benchmark-options='--time-limit 3'
```

## Notes for downstream users

If you are using `wireform-parquet` in a long-running process,
GHC's default `-A1m` nursery is small for any realistic
columnar workload. Set at least `-A8m` (per-capability) in
your link options:

```
ghc-options: -threaded -rtsopts "-with-rtsopts=-N -A8m"
```

For batch jobs that read large columnar files, `-A32m` or
larger pays off — see the history table above.
