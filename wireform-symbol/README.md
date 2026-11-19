# wireform-symbol

Interned strings for the whole process. While a `Symbol` for some text is
alive, every `intern` of that text returns the same canonical `ByteArray#`.
So `==` is a pointer comparison, `hash` is a field read, and repeated
strings are stored once. Unused symbols are reclaimed by the garbage
collector.

```haskell
import qualified Symbol

let a = Symbol.intern ("content-type" :: Text)
a == Symbol.intern ("content-type" :: ByteString)   -- True, O(1)
Symbol.unintern a :: Text                            -- zero-copy
```

hermes uses it for `HeaderFieldName` and non-standard `Method`s. It replaces
`symbolize`, which deletes dead symbols from a finalizer through
`vector-hashtables`' `alter`. That delete can drop *colliding live* keys.
Re-interning a live symbol's text then minted a second copy, and pointer
equality between the two broke.

## Design

The entry for a given content is a small state machine. Every transition
except the garbage collector's happens with its shard's lock held:

```
Absent ──intern──▶ Live w ──GC──▶ Dead w ──┬─ sweep ──▶ Absent
                                           └─ intern ─▶ Live w'
```

* **No finalizers.** Weak pointers are made with `mkWeakNoFinalizer#`. A
  dead entry is replaced when a lookup reaches it, or swept just before its
  shard would grow. After a sweep that leaves the shard more than half
  full, the shard doubles, so sweeps are O(1) amortised per insert. There is
  no finalizer thread, so a late finalizer can never race a re-intern.
* **The hash lives in the symbol.** `data Symbol = Symbol ByteArray# Int#`.
  `hash` is the stored SipHash, so there's no `StableName` lookup in the RTS
  table on every `HashMap` operation.
* **HashDoS-safe.** The table is keyed by SipHash-1-3 (as in Rust's and
  CPython's hash tables) under a 128-bit key from the OS entropy source.
  Header names are attacker-controlled.
* **Nothing is copied on a hit.** `Text` and valid `ShortByteString` are
  hashed and compared in place as array slices, and a valid `ByteString`
  through its buffer pointer. Bytes are copied only into a new symbol.
  Invalid UTF-8 is first replaced as `decodeUtf8Lenient` does, so
  `unintern` to `Text` is always valid. That matches symbolize's
  behaviour.
* **32 independently locked shards** (`MVar` +
  [`wireform-hashtables`](../wireform-hashtables)), picked by the hash's
  top bits, so independent interns rarely contend.

`Ord` is byte-lexicographic on the UTF-8, which is code-point order and the
same as `Text`'s `Ord`. `Binary` writes the content as a length-prefixed
`ShortByteString`, the same bytes symbolize writes.

## Benchmarks

`cabal bench wireform-symbol-bench` compares against `symbolize`, which is
kept as a bench-only dependency. Measured on GHC 9.10.3, `-O2`, a 4-core
2.1 GHz Xeon VM. Each figure is the minimum over two runs. ns per operation,
over 24 common header names, or 1000 fresh names for `intern-fresh`:

| op | wireform-symbol | symbolize | speedup |
|---|---|---|---|
| intern, already live, `ByteString` | 85.2 | 156.3 | 1.84× |
| intern, already live, `Text` | 61.1 | 118.6 | 1.94× |
| intern, fresh names | 91.6 | 293.5 | 3.21× |
| `hash` | 2.0 | 25.5 | 12.7× |
| `==` | 2.6 | 2.1 | (both a pointer compare) |

hermes' own `HeaderMap` bench (`cabal bench hermes-bench`), run three times
interleaved against the symbolize build, minimum of three:

| fixture | op | symbolize (ns) | wireform-symbol (ns) | speedup |
|---|---|---|---|---|
| small (5 headers) | `headerMapFromList` | 884.6 | 580.1 | 1.52× |
| typical (14) | `headerMapFromList` | 2620.6 | 1966.9 | 1.33× |
| repeated (22) | `headerMapFromList` | 3624.4 | 2637.1 | 1.37× |
| small | `lookupRawHeader` hit / miss | 41.5 / 29.0 | 32.6 / 19.0 | 1.27× / 1.53× |
| typical | `lookupRawHeader` hit / miss | 48.0 / 28.7 | 39.1 / 18.4 | 1.23× / 1.56× |
| repeated | `lookupRawHeader` hit / miss | 82–90¹ / 28.5 | 80–92 / 18.5 | parity / 1.54× |
| all three | `lookupHeader @CacheControl` | | | 1.02–1.09× |
| all three | `headerMapToList` | | | 0.89–0.97× |

¹ In two of five runs, symbolize's `repeated` "hit" took about 30 ns,
which is its miss latency. The hit key is a top-level interned
`"set-cookie"` and the map holds the same text interned from the wire. If
the live entry is evicted in between, the two become distinct symbols and
the lookup misses. That's the failure this package exists to remove, though
this is inferred from the timings, not instrumented.

`headerMapToList` interns nothing. It copies each name out to a
`ByteString`, and the 3–11% gap (about 3 ns per header) there is not yet
explained.
