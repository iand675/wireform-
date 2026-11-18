# wireform-hashtables

Fast mutable hash tables for Haskell, with the API shape of
[`vector-hashtables`](https://hackage.haskell.org/package/vector-hashtables)
(`Data.Vector.Hashtables`) on a new engine.

```haskell
import qualified Hashtable as H

type Counts = H.Dictionary H.RealWorld H.Unboxed Int H.Unboxed Int

main :: IO ()
main = do
  t <- H.initialize 1024 :: IO Counts
  mapM_ (H.upsert t (maybe 1 (+ 1))) [1, 2, 2, 3, 3, 3]
  print =<< H.lookup t 3        -- Just 3
```

## Engine

Boost `unordered_flat_map`'s design, squeezed into 64-bit SWAR words
(GHC has no `pmovmskb`, so there is no 16-byte SIMD group to be had):

```
 group word:  byte 0  1  2  3  4  5  6 | 7
             tag tag tag tag tag tag tag | overflow bits
```

* **Seven 8-bit tags per word**, tags in `[2, 255]`, empty = 0. Keeping tags
  off 1 makes the cheap 3-op `haszero` test exact on empty lanes, so a
  match never lands on an `Unboxed` slot's garbage.
* **Overflow Bloom byte.** Bit `tag & 7` is set when an insertion probes
  past a full group, so a miss stops at the first group whose bit is clear —
  usually the first.
* **No tombstones.** Deleting zeroes the tag. A delete from an overflowed
  group lowers the load budget instead, so heavy churn brings the next rehash
  forward rather than silently lengthening probes. (The test suite pins this:
  without it, after 400k delete/insert cycles at full load, misses scan the
  whole table; with it, 1.11 groups on average.)
* **One hop.** The handle is a `MutVar#` holding an *unlifted* `Table`
  record whose fields are the raw arrays — no `MVector` boxes, offsets or
  lengths in between.

See `Hashtable.Internal` for the full layout notes.

## Storage

Keys and values each choose a storage tag, the role `ks` / `vs` vector types
play in `vector-hashtables`:

| Tag       | Representation                        | Use for                        |
|-----------|---------------------------------------|--------------------------------|
| `Unboxed` | `MutableByteArray#` via `Prim`        | `Int`, `Word`, `Double`, …     |
| `Strict`  | `MutableArray#`, forced to WHNF       | boxed values (no thunk chains) |
| `Boxed`   | `MutableArray#`, lazy                 | when you want laziness         |

`Strict` writes force the value *before* anything is mutated, so an
exception while forcing leaves the table untouched.

## API

Everything from `Data.Vector.Hashtables` (`initialize`, `clone`, `size`,
`keys`, `values`, `lookup`, `lookup'`, `at`, `at'`, `member`,
`findWithDefault`, `findEntry`, `lookupIndex`, `insert`, `delete`, `upsert`,
`alter`, `alterM`, `union[With[Key]]`, `difference[With]`,
`intersection[With[Key]]`, `fromList`, `toList`, `unsafeFreeze`,
`unsafeThaw`, `findElem`), with the same table–function–key argument order,
plus:

* `adjust`, `adjustM`, `update`, `insertWith`, `insertIfAbsent`,
  `mutate` / `mutateM` (the `hashtables` shape, returning a result);
* `mapInPlace`, `filterInPlace` (never rehashes), `clear`, `reserve`,
  `capacity`, `foldM`, `forWithKey_`, `fromListN`, `fromListWith`, `newSized`;
* `freeze` / `thaw` (copying) and `frozenLookup`, `frozenMember`,
  `frozenKeyAt`, `frozenValueAt`, `frozenToList`;
* **batched, software-prefetched** `lookupBatch`, `insertBatch`,
  `upsertBatch` over vectors of keys (below).

### Differences from `vector-hashtables`

* Storage is chosen by tag (`Unboxed` / `Strict` / `Boxed`) rather than by
  `MVector` type; `keys` / `values` return any `Data.Vector.Generic` vector.
* Iteration order is slot order: unspecified, and it changes on growth.
  (Upstream happens to iterate in insertion order.)
* `alterM`, `adjustM` and `mutateM` look the key up again after the callback
  runs, because the callback may use the table. Two probes, always correct.
* Deleting through `alter` / `update` / `mutate` is the same operation as
  `delete`. (Upstream issue
  [#43](https://github.com/klapaucius/vector-hashtables/issues/43): deleting
  through `alter` dropped colliding keys. The suite runs every mutation path
  against `Data.Map` with constant-hash keys.)
* `fromList` grows as it goes instead of forcing the list spine for its
  length; use `fromListN` when you know the size.

### Batched operations

On tables bigger than cache, `lookupBatch` / `insertBatch` / `upsertBatch`
run keys through a three-stage pipeline: hash and prefetch the home group
`2·D` keys ahead, prefetch the exact key and value lines `D` keys ahead, then
probe against warm lines. Each key is hashed once. Below about 229k slots
the pipeline is skipped (it would be pure overhead). On the bench machine,
1M random `Int` keys (criterion, table below): inserts 58 → 33 ns, hits
40 → 29 ns. 16M keys, past the 260 MB L3 (one-shot timing driver, since the
suite stops at 1M): inserts 93 → 48 ns, hits 89 → 56 ns. Misses gain nothing,
since they only touch the control bytes.

## Performance

ns per operation, `Int` keys and values (`Unboxed`/`Unboxed`), GHC 9.10.3
NCG `-O2`, criterion `--time-limit 2`, on a 4-core 2.1 GHz Xeon VM (192 KiB
L1d, 2 MiB L2 per core, **260 MiB shared L3** — so the 1M tables are
L3-resident; on a typical server they would be in DRAM, where the batched
operations gain more: see above). `seq` keys are `0 .. n-1`, looked up in
insertion order — the best case for an identity-hash chaining table; `rand`
keys are 63-bit pseudo-random. "batch" is `insertBatch` / `lookupBatch` /
`upsertBatch`; on `insert-grow` it presizes (it knows `n`), so it is never
counted as the best there. Speedups use the faster of our two modes.

| op | keys | n | ours | ours (batch) | vector-hashtables | hashtables basic | hashtables cuckoo | unordered-containers | vs vector-hashtables | vs best other |
|---|---|---|---|---|---|---|---|---|---|---|
| insert | seq | 1,000 | 12.3 | 12.3 | 16.1 | 29.4 | 78.3 | 74.3 | 1.31× | 1.31× |
| insert | seq | 100,000 | 11.7 | 12.3 | 12.9 | 33.3 | 88.7 | 332.6 | 1.10× | 1.10× |
| insert | seq | 1,000,000 | 21.3 | 18.0 | 16.3 | 37.9 | 184.9 | 753.9 | 0.90× (batch) | 0.90× (batch) |
| insert | rand | 1,000 | 11.7 | 12.3 | 15.4 | 38.0 | 84.3 | 93.9 | 1.32× | 1.32× |
| insert | rand | 100,000 | 23.9 | 25.1 | 35.6 | 92.2 | 99.1 | 453.9 | 1.49× | 1.49× |
| insert | rand | 1,000,000 | 58.4 | 33.3 | 68.8 | 150.6 | 227.8 | 1142.2 | 2.06× (batch) | 2.06× (batch) |
| insert-grow | seq | 1,000 | 26.7 | 11.7 | 26.2 | 112.6 | 207.4 | 85.4 | 0.98× | 0.98× |
| insert-grow | seq | 100,000 | 25.8 | 14.2 | 20.7 | 90.6 | 404.3 | 293.1 | 0.80× | 0.80× |
| insert-grow | seq | 1,000,000 | 37.6 | 18.3 | 29.2 | 124.8 | 390.4 | 707.9 | 0.78× | 0.78× |
| insert-grow | rand | 1,000 | 26.5 | 13.7 | 28.3 | 159.5 | 271.6 | 99.6 | 1.06× | 1.06× |
| insert-grow | rand | 100,000 | 31.7 | 26.1 | 54.7 | 222.8 | 587.2 | 502.6 | 1.72× | 1.72× |
| insert-grow | rand | 1,000,000 | 66.5 | 36.1 | 90.5 | 419.1 | 567.7 | 1169.4 | 1.36× | 1.36× |
| lookup-hit | seq | 1,000 | 4.0 | 6.4 | 4.3 | 14.5 | 37.4 | 9.3 | 1.07× | 1.07× |
| lookup-hit | seq | 100,000 | 6.4 | 8.4 | 5.3 | 19.2 | 48.1 | 70.0 | 0.83× | 0.83× |
| lookup-hit | seq | 1,000,000 | 7.1 | 12.2 | 4.6 | 14.3 | 44.2 | 206.0 | 0.65× | 0.65× |
| lookup-hit | rand | 1,000 | 4.5 | 6.3 | 5.0 | 18.9 | 35.6 | 16.3 | 1.10× | 1.10× |
| lookup-hit | rand | 100,000 | 17.6 | 23.4 | 24.9 | 51.1 | 74.8 | 112.4 | 1.41× | 1.41× |
| lookup-hit | rand | 1,000,000 | 39.9 | 28.6 | 53.0 | 107.3 | 242.0 | 348.0 | 1.85× (batch) | 1.85× (batch) |
| lookup-miss | seq | 1,000 | 4.0 | 5.8 | 3.9 | 259.4 | 59.0 | 8.4 | 0.98× | 0.98× |
| lookup-miss | seq | 100,000 | 8.1 | 7.7 | 4.2 | 18191.8 | 59.9 | 8.1 | 0.54× (batch) | 0.54× (batch) |
| lookup-miss | seq | 1,000,000 | 4.8 | 10.6 | 4.3 | DNF¹ | 95.5 | 85.9 | 0.90× | 0.90× |
| lookup-miss | rand | 1,000 | 4.4 | 7.2 | 4.3 | 31.3 | 58.6 | 9.3 | 0.98× | 0.98× |
| lookup-miss | rand | 100,000 | 7.7 | 11.1 | 27.1 | 33.8 | 65.7 | 15.1 | 3.50× | 1.95× |
| lookup-miss | rand | 1,000,000 | 13.2 | 18.3 | 57.8 | 49.6 | 118.2 | 134.3 | 4.39× | 3.77× |
| insert-delete | seq | 1,000 | 17.7 | — | 20.8 | 56.8 | 133.1 | 93.2 | 1.17× | 1.17× |
| insert-delete | seq | 100,000 | 21.5 | — | 17.7 | 52.1 | 133.1 | 438.6 | 0.82× | 0.82× |
| insert-delete | seq | 1,000,000 | 27.7 | — | 22.4 | 60.1 | 299.1 | 858.1 | 0.81× | 0.81× |
| insert-delete | rand | 1,000 | 18.9 | — | 24.9 | 87.5 | 130.0 | 109.2 | 1.32× | 1.32× |
| insert-delete | rand | 100,000 | 39.0 | — | 61.6 | 144.6 | 180.8 | 684.9 | 1.58× | 1.58× |
| insert-delete | rand | 1,000,000 | 98.3 | — | 119.8 | 290.6 | 519.8 | 1115.9 | 1.22× | 1.22× |
| upsert-count | seq | 1,000 | 8.3 | 9.2 | 21.6 | 68.8 | 84.9 | 72.5 | 2.60× | 2.60× |
| upsert-count | seq | 100,000 | 7.6 | 8.8 | 17.7 | 60.2 | 108.3 | 142.3 | 2.34× | 2.34× |
| upsert-count | seq | 1,000,000 | 11.2 | 11.3 | 20.0 | 75.3 | 165.0 | 662.7 | 1.79× | 1.79× |
| upsert-count | rand | 1,000 | 10.1 | 11.6 | 21.2 | 71.0 | 88.1 | 71.0 | 2.11× | 2.11× |
| upsert-count | rand | 100,000 | 10.0 | 11.7 | 24.8 | 135.1 | 173.1 | 273.7 | 2.48× | 2.48× |
| upsert-count | rand | 1,000,000 | 22.6 | 23.5 | 61.4 | 413.1 | 288.7 | 865.5 | 2.72× | 2.72× |
| toList | seq | 1,000 | 15.8 | — | 15.9 | 10.8 | 10.6 | 19.3 | 1.01× | 0.67× |
| toList | seq | 100,000 | 46.5 | — | 37.1 | 21.7 | 18.9 | 23.6 | 0.80× | 0.41× |
| toList | seq | 1,000,000 | 64.5 | — | 64.5 | 30.3 | 29.1 | 24.7 | 1.00× | 0.38× |
| toList | rand | 1,000 | 18.0 | — | 16.5 | 12.0 | 11.2 | 26.0 | 0.92× | 0.62× |
| toList | rand | 100,000 | 41.7 | — | 36.6 | 21.7 | 21.1 | 29.4 | 0.88× | 0.51× |
| toList | rand | 1,000,000 | 66.6 | — | 63.1 | 29.8 | 29.8 | 34.3 | 0.95× | 0.45× |

¹ `hashtables`' basic table probes linearly under `hashable`'s identity
`Int` hash, so sequential keys form one occupied run and misses next to it
cost O(n): 259 ns per miss at 1k, 18.2 µs at 100k; at 1M one pass would
take minutes, so the cell was stopped.

Where it loses, and why:

* **Sequential keys looked up in insertion order** (`lookup-hit/seq` at
  100k+, `insert-grow/seq`, `insert-delete/seq`): `vector-hashtables` keeps
  entries in insertion-ordered dense arrays and hashes `Int` as the identity,
  so this access pattern streams 8 keys per cache line. `hashKey#` keeps
  256-key blocks contiguous (consecutive home groups) to get most of that
  locality back — 41 → 7 ns per hit at 1M — but a 7-lane group still
  touches about a line per key.
* **Growing from empty at 1k** is at parity (0.98–1.06×, within noise):
  tables under 64 groups grow 4× instead of 2×, halving the tiny rehashes
  (1.10–1.23× over 2× growth in an interleaved A/B).
* **`toList`**: `hashtables` and `unordered-containers` store boxed `Int`s
  and cons pointers to them; an `Unboxed` column must allocate two `I#`s per
  entry on the way out. `foldM` does not allocate.

### Writing fast code against it

* Use the table at concrete types. Every operation is `INLINE`, so the probe
  loop is compiled for your key type with no dictionary passing; if you wrap
  it in your own polymorphic functions, `SPECIALIZE` them.
* `findWithDefault` never allocates; `lookup`'s `Just` disappears if you
  scrutinise it straight away.
* `foldM` is allocation-free; `toList` must box every unboxed key and value.
* Not thread-safe.

## Benchmarks

```bash
cabal bench wireform-hashtables-bench --benchmark-options='--time-limit 2'
# or, per repo convention, sequentially through the manifest:
python3 scripts/run-benchmarks.py --only hashtables
```
