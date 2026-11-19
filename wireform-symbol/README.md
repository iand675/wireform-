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
kept as a bench-only dependency.
