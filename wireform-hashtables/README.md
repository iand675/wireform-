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
the pipeline is skipped (it would be pure overhead). Measured on the bench
machine, 1M random `Int` keys: inserts 73 → 37 ns, hits 45 → 27 ns; 16M keys
(past the 260 MB L3): inserts 93 → 48 ns, hits 89 → 56 ns. Misses gain
nothing, since they only touch the control bytes.

## Performance

<!-- results table: see "Benchmarks" below -->

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
