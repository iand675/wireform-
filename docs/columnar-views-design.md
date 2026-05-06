# Column-views design: closing the C/Rust gap

## Status

`docs/columnar-perf-baseline.md` ends with a "why we're not at C/Rust speed" section. This document is the concrete plan for making the changes that section gestures at — replacing the GC-managed, fully-materialised `ColumnArray` constructors with a /view/ representation that can wrap a `ForeignPtr` slice into the source buffer.

The first piece is already shipped as `Columnar.Bit`. The rest is described below as discrete units of work in roughly the order they need to land.

## Why bother

Today the read path for primitive Arrow / Parquet columns does:

1. mmap the file → `ByteString` (one `ForeignPtr`).
2. Slice out a column chunk (`BS.take + BS.drop` — still one `ForeignPtr`, free).
3. Decompress if needed (`Snappy.decompress` etc. — produces a fresh `ByteString`).
4. **Memcpy** the decompressed bytes into a fresh `VP.Vector Word32` / `Int64` / etc. (`ColumnArray` constructors hold `VP.Vector`, which wraps a `ByteArray`, which is GC-managed and not interchangeable with `ForeignPtr`).
5. Return that `ColumnArray` to the caller.

For uncompressed reads, step 4 is the dominant cost — we read 800 KB of int64s at maybe 8 GB/s into a fresh nursery allocation when arrow-rs would just hand back a slice descriptor pointing into the mmap.

For compressed reads the decompressor itself produces a fresh buffer, so step 4 doesn't add a second copy — but the buffer is still a `ByteString` (`ForeignPtr`), and the conversion to `VP.Vector` is still a copy.

Bool and Utf8 columns have an additional structural cost: `V.Vector Bool` / `V.Vector Text` are /boxed/ vectors. Each cell is a pointer; for an N-row column the spine alone is `8N` bytes regardless of how many actual bytes the bool / text data takes. The wire format is bit-packed `Bool` and slice-of-bytes `Text`; we throw that compactness away on read.

## End state

`Arrow.Column.ColumnArray` becomes a sum where every primitive constructor carries a /view/ instead of a `VP.Vector`. The view types are:

```haskell
-- Numeric primitives: same shape as today's VP.Vector but
-- backed by VS.Vector (which carries a ForeignPtr instead of
-- a ByteArray).
type Int64View = VS.Vector Int64
type DoubleView = VS.Vector Double
-- ... etc

-- Bool: bit-packed.
type BoolView = VU.Vector Bit

-- Utf8 / Binary: offsets buffer + data buffer, both views.
data Utf8View  = Utf8View  !(VS.Vector Int32) !(VS.Vector Word8)
data BinaryView = BinaryView !(VS.Vector Int32) !(VS.Vector Word8)
```

The constructors look like:

```haskell
data ColumnArray
  = ColInt32  !Int32View
  | ColInt64  !Int64View
  | ColFloat  !FloatView
  | ColDouble !DoubleView
  | ColBool   !BoolView
  | ColUtf8   !Utf8View
  | ColBinary !BinaryView
  -- ... plus the existing Maybe / nested / dictionary
  --     constructors, similarly view-ified.
```

Reads become:

1. mmap the file.
2. Slice out the column chunk.
3. Decompress (still one allocation, unavoidable).
4. **Wrap** the decompressed `ByteString` as a `VS.Vector Int64` etc. — `VS.Vector` adopts a `ForeignPtr` directly, so this is *zero copies*.
5. Return.

For uncompressed reads steps 3 and 4 collapse: the decompressor is a no-op, and step 4 just adopts the original mmap slice. Round trip is one syscall + a few tens of cycles per column.

## Work units

Each section below is a self-contained PR.

### 1. `Columnar.Bit` (DONE)

Bit-packed `Bool` newtype with `Data.Vector.Unboxed` instance. `VU.Vector Bit` packs eight values per byte (LSB-first, matching Arrow / Parquet on the wire).

The full `vector` API works: `length`, `slice`, `(!)`, `map`, `foldl'`, `zip`, `toList`, `fromList`, mutable `read` / `write`, `freeze` / `thaw`. Slicing supports arbitrary bit boundaries by carrying `(bitOffset, bitLength)` alongside the byte vector.

Convenience helpers:
- `fromByteString` / `fromByteStringN` — wrap an Arrow / Parquet bitmap (currently with a copy; zero-copy needs the `Storable` migration in unit 2).
- `toByteString` — emit the bit-packed bytes for the wire format.
- `countOnes` — popcount, with a SIMD-accelerated whole-buffer fast path.

### 2. Switch primitive `ColumnArray` constructors from `VP.Vector` to `VS.Vector`

`Data.Vector.Storable.Vector a` is `(ForeignPtr a, Int)` underneath. It supports `unsafeFromForeignPtr` / `unsafeToForeignPtr` for zero-copy adoption / extraction.

Roughly the same API surface as `VP.Vector` from a consumer point of view — `VS.length`, `VS.unsafeIndex`, `VS.foldl'`, `VS.map`, `VS.unsafeFreeze`, `VS.fromList`, etc. The differences:
- `VS.Vector a` requires `Storable a` rather than `Prim a`. All our primitive types (`Int8`/`Int16`/`Int32`/`Int64`, `Word*`, `Float`, `Double`) already have `Storable` instances.
- `VS.Vector` doesn't track strictness the same way; the underlying `ForeignPtr` is always evaluated. (No semantic difference for our use case.)

Migration:
1. In `Arrow.Column`, change every `ColInt32 !(VP.Vector Int32)` constructor to `ColInt32 !(VS.Vector Int32)`. Same for the rest of the primitive numeric / temporal constructors.
2. Update every `VP.length` / `VP.unsafeIndex` / etc. callsite. (`grep` for `VP\.` should give a complete list — most uses are in `Arrow.Column`, `Parquet.Arrow`, `ORC.Arrow`, and the Read / Write modules.)
3. Replace `VP.create`-based mutable builds with `VS.create`. `VSM.MVector` has the same shape as `VPM.MVector`; the migration is largely mechanical.
4. New constructor `Arrow.Column.fromForeignPtr :: ForeignPtr a -> Int -> Int -> VS.Vector a` (this is just `VS.unsafeFromForeignPtr` re-exported for consumers).

The performance win is realised in `Arrow.Column.readInts32` and friends: they already drive the read through a single `memcpy` from the source `ByteString` into a freshly-allocated `VP.Vector` — once `ColInt32` holds a `VS.Vector`, the helper can adopt the source `ByteString`'s `ForeignPtr` directly via `BS.toForeignPtr`. No allocation, no memcpy.

### 3. `Utf8View` / `BinaryView` / `LargeUtf8View` / `LargeBinaryView`

Same idea applied to variable-length strings. The view type is the offsets buffer + the data buffer, both `VS.Vector`s wrapping the source `ByteString`'s `ForeignPtr`.

```haskell
data Utf8View = Utf8View
  { uvOffsets :: !(VS.Vector Int32)
    -- (n+1) Int32 offsets, prefix-sum style.
  , uvData    :: !(VS.Vector Word8)
    -- Validated UTF-8 bytes; consumer can slice without
    -- per-element decoding.
  }
```

Per-element access:

```haskell
uvIndex :: Utf8View -> Int -> Text
uvIndex (Utf8View offs dat) i =
  let !s   = fromIntegral (VS.unsafeIndex offs i)
      !e   = fromIntegral (VS.unsafeIndex offs (i + 1))
      !len = e - s
  in TI.text (vsToTextArray (VS.unsafeSlice s len dat)) 0 len
```

`vsToTextArray :: VS.Vector Word8 -> TA.Array` is a tiny FFI helper that exposes the view's `ForeignPtr` as a text `Array`. Per-element `Text` is a (Array, offset, length) triple — no copy.

`Utf8View` is iterable, foldable, etc. via a `View` typeclass (see unit 6). Eager materialisation to `V.Vector Text` (today's representation) is still available for consumers that want it, behind `materializeUtf8 :: Utf8View -> V.Vector Text`.

UTF-8 validation moves from per-element to per-buffer: a single SIMD ASCII check (`Columnar.SIMD.isAsciiBS`) on the data buffer establishes the fast path; the slow path runs `Data.Text.Encoding.streamDecodeUtf8'` over the whole buffer once instead of per-string.

### 4. `Arrow.Column.NullableView` (replace `Maybe`-vector constructors)

Today `ColInt32Maybe :: V.Vector (Maybe Int32)` is a boxed vector of `Maybe Int32`. Each cell is a pointer to either `Nothing` or `Just !Int32`. For an N-row column the spine is `8N` bytes regardless of how many nulls there actually are.

Arrow already has the right representation: a validity bitmap (one bit per element) plus the dense values buffer. Mirror that:

```haskell
data NullableView a = NullableView
  { nvValidity :: !(VU.Vector Bit)    -- bit i = 1 iff value i is present
  , nvValues   :: !(VS.Vector a)      -- always full length; null slots
                                      -- carry whatever was on the wire
  }
```

`nvIndex :: NullableView a -> Int -> Maybe a` is one `VU.unsafeIndex` on the validity bitmap + one `VS.unsafeIndex` on the values. For all-non-null and all-null columns the validity bitmap is a constant — emit `nvValidity = VU.empty` and a flag, save the `ceil(N/8)` byte allocation.

Migration path is the same as unit 2: change every `ColIntXMaybe !(V.Vector (Maybe a))` to `ColIntXMaybe !(NullableView a)`, update callsites. The Arrow IPC reader already produces this shape on the wire, so the decode side is a literal pointer-adoption (zero copy). The Parquet reader needs to map `definition_levels`-style validity to a bit vector — `Columnar.Bit.fromByteString` already exists.

### 5. Read-path adoption: `Arrow.Column.readInts*` to use `VS`

Once unit 2 lands, the `Arrow.Column.readIntsN` / `readUInts*` / `readFloat*` / `readTimestamp*` / etc. helpers can stop calling `memcpyPrimVecLE` (which mallocs a `VP.Vector`) and instead:

```haskell
readInts32 endian len rb body bufIdx nodeIdx = do
  buf <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  let (fp, off, _) = BSI.toForeignPtr buf
  pure ( ColInt32 (VS.unsafeFromForeignPtr (castForeignPtr fp)
                                           off len)
       , nodeIdx + 1, bufIdx + 1)
```

That's one `BS.toForeignPtr` (free, returns existing pointer) + one `VS.unsafeFromForeignPtr` (constructor, no allocation). Compare to today's `memcpyPrimVecLE` which allocates `len * 4` bytes.

Same shape applies to `Parquet.Read.decodePlainPrimLEMemcpy` once `ColInt32` etc. carry `VS.Vector`.

The big-endian path still has to copy (and byte-swap), but big-endian Arrow / Parquet is rare. The host-LE common path becomes free.

### 6. `View` typeclass: shared iterator surface

Consumers should be able to write polymorphic loops over column views without caring whether the column is a slice-view or a freshly-materialised vector:

```haskell
class View v where
  type Element v
  vLength    :: v -> Int
  vIndex     :: v -> Int -> Element v
  vUnsafeIndex :: v -> Int -> Element v
  vSlice     :: Int -> Int -> v -> v
  vToList    :: v -> [Element v]
  vForM_     :: Applicative f => v -> (Element v -> f ()) -> f ()
```

Default implementations cover most of these; concrete views (`Int64View`, `BoolView`, `Utf8View`, `NullableView a`) provide the primitive `vLength` / `vUnsafeIndex` only.

This typeclass is also where the `Columnar.Stream.Iter` consumers should land: `iterColumn :: View v => v -> Iter (Element v)`.

### 7. Write path: accept either views or eager vectors

Writes are the inverse. `Parquet.Write.encodeColumnDataPagePayload` already takes a `ColumnData` that holds `VP.Vector`s. Once unit 2 changes the constructors, the writers automatically see `VS.Vector`s — same API surface, same `pokeByteOff` loop, same single allocation per page.

For the `Utf8View` write path: the offsets buffer is the wire format, so we can pass it through directly. If the writer's destination buffer happens to be the same size as the view, `BSI.memcpy` from the view's `ForeignPtr` straight into the page body — one syscall, no per-string allocation.

### 8. Backward compatibility helpers

A subset of consumers (notably the `derive` test suites, fixture builders, `Arrow.Record`) want to construct columns from Haskell lists. Provide:

```haskell
fromList :: VS.Storable a => [a] -> ColumnArray a   -- existing API, just with VS instead of VP
toList   :: ColumnArray a -> [a]
materializeColumn :: ColumnArray -> [some boxed shape]  -- worst-case eager realiser
```

The `*Maybe` constructors retain a `fromMaybeList :: [Maybe a] -> NullableView a` constructor that builds the validity bitmap + dense values in one pass.

### 9. Tests / property work

- `Columnar.Bit` round-trip + slicing properties (DONE for the basics; expand to `VU.zip`, `VU.imap`, `VU.unsafeMove` once they're exercised in real code).
- `View` instances obey the law `vLength v == length (vToList v)` and `vToList (vSlice s l v) == take l (drop s (vToList v))`.
- Parquet / Arrow / ORC interop tests stay green: every write-then-read-then-compare round-trip should be byte-equivalent before / after the migration.
- `parquet-throughput` benchmark with the same dataset before / after each unit lands; expect read-uncompressed to drop from 1.7 ms to "decompress + footer parse" cost only (a few hundred µs).

### 10. Documentation

- Update `docs/columnar-perf-baseline.md` with the new "view-based reads" numbers once unit 5 lands.
- Update `docs/columnar-package-overviews.md` to mention `Columnar.Bit` + the `View` typeclass.
- Add a short "view vs eager vector" section to the user-facing `getting-started.md` so new consumers know which API to reach for.

## Risks and trade-offs

- **Lifetimes.** `VS.Vector` shares a `ForeignPtr`. When the source `ByteString` is freed (closed mmap, dropped reference), every view derived from it becomes dangling. Today's code is safe by construction because we always copy. The migration needs documentation + ideally a `force` API for callers that want to detach a view from its source. (`VS.force` already exists; it copies into a fresh `ForeignPtr`.)
- **Slicing semantics changes.** `VP.Vector` and `VS.Vector` both support `unsafeSlice`; consumers shouldn't notice. But anything that used `VP.toList` and expected the underlying `ByteArray` not to escape needs review — `VS.toList` materialises through pointers.
- **`Storable` vs `Prim` instance churn.** Our derived `ToColumn` / `FromColumn` typeclasses default to `Prim`. They'll need a `Storable` shim, or a switch. Easiest path: have the typeclass require `(Prim a, Storable a)` for the next few releases and let consumers gate on either constraint.
- **Bit-vector slicing complexity.** `Columnar.Bit` carries `(bitOffset, bitLength)` so misaligned slices are correct, but `basicUnsafeCopy` for misaligned slices falls back to a per-bit walk. For the common case (whole columns, or slices on byte boundaries) the byte-aligned fast path kicks in.

## Sequencing

1. `Columnar.Bit` — done.
2. `VS.Vector` migration for primitive numeric constructors (unit 2). Largest surface, most test coverage.
3. `NullableView` (unit 4) — depends on `Columnar.Bit` + `VS.Vector` for the underlying values buffer.
4. `Utf8View` / `BinaryView` (unit 3) — depends on `VS.Vector`.
5. Read-path zero-copy adoption (unit 5) — depends on units 2/3/4.
6. `View` typeclass (unit 6) — depends on all the view types existing.
7. Write path (unit 7) — automatic once unit 2 lands; minor follow-ups for the variable-length writers.
8. Backward-compat helpers + tests + docs (units 8/9/10) — ride along with each unit.

Each unit is a discrete PR. Steps 2 onwards involve a lot of mechanical edits; the migration is best done one constructor family at a time so test failures stay scoped.
