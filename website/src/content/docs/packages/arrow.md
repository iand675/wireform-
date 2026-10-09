---
title: wireform-arrow
description: "Apache Arrow IPC with schema framing, record batch encode/decode, typed record APIs, table projection, and optional zstd/lz4 compression."
sidebar:
  order: 41
---

`wireform-arrow` implements Apache Arrow IPC and the Arrow columnar data
model in Haskell. Arrow is the interchange format between analytics engines,
dataframe libraries, and columnar storage. Use this package when you need
typed record batches, schema-aware encoding, or a shared column vocabulary
that Parquet and ORC readers in wireform can target.

## Key features

- **Schema framing** and IPC message encode/decode via `Arrow.IPC`
- **Record batch** encode and decode for in-memory columnar data
- **Typed record API** with Template Haskell and Generic support
- **Table projection and subsetting** to read only the columns you need
- **Optional compression** (Zstd and LZ4 behind Cabal flags)
- **Zero-copy decode**: columns are Arrow-native buffers that alias the
  input; only offsets, UTF-8 and dictionary keys are validated (C kernels)
- **Single-allocation encode**, plus lazy encoders whose chunks alias the
  column buffers
- **Streaming reader** for framed IPC streams in `Arrow.Stream`

## Basic usage

Define a record type, build a `Table`, encode a vector of records to an
Arrow IPC stream with `Arrow.Stream`, and decode it back:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Trades where

import Arrow.Record
import Arrow.Stream (decodeArrowStream, defaultWriteOptions, encodeArrowStream)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Vector as V

data Trade = Trade
  { tradeSym  :: !Text
  , tradeQty  :: !Int32
  , tradeNote :: !(Maybe Text)
  }
  deriving stock (Show, Eq)

tradeTable :: Table Trade
tradeTable = table enc dec
  where
    enc =
      fieldE "sym"  tradeSym  utf8E
        <> fieldE "qty"  tradeQty  int32E
        <> fieldE "note" tradeNote (nullable utf8E)
    dec =
      Trade
        <$> columnD "sym"  utf8D
        <*> columnD "qty"  int32D
        <*> columnD "note" (nullableD utf8D)

-- | One record batch in an IPC stream. 'Left' when the columns do not
-- fit the schema (they always do for a 'Table').
encodeTrades :: V.Vector Trade -> Either String ByteString
encodeTrades trades =
  let (schema, columns) = encodeTable tradeTable trades
  in encodeArrowStream defaultWriteOptions schema [columns]

-- | Every record of every batch in the stream.
decodeTrades :: ByteString -> Either String (V.Vector Trade)
decodeTrades bytes = do
  (schema, batches) <- decodeArrowStream bytes
  V.concat <$> traverse (decodeTable tradeTable schema) batches

roundTripTrades :: V.Vector Trade -> Either String (V.Vector Trade)
roundTripTrades trades = encodeTrades trades >>= decodeTrades
```

`encodeArrowFile` / `decodeArrowFile` do the same for the Arrow file
format. The writers check every batch against the schema and return
`Left` for a mismatch, or for dictionaries they cannot write (an
emit-once dictionary larger than its index type can address).

Project column batches down to a subset of fields when the full schema is
larger than what your query needs:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Arrow.Column (ColumnArray)
import Arrow.Record (projectTable)
import Arrow.Types (Schema)
import qualified Data.Vector as V

projectSymQty :: Schema -> V.Vector ColumnArray -> Maybe (Schema, V.Vector ColumnArray)
projectSymQty schema cols =
  projectTable ["sym", "qty"] schema cols
```

`Arrow.File` reads the file format and exposes the raw record batches;
`Arrow.Stream.openStreamReader` reads a stream one batch at a time.

## Columns

A `ColumnArray` holds Arrow's own buffers: storable vectors for
fixed-width values and offsets, LSB-first bitmaps for validity and
booleans, and `ByteString` regions for variable-length data. Nullability
is per value: every array with a validity slot carries `Maybe Validity`,
`Nothing` meaning no nulls. The patterns (`ColPrim`, `ColInt64`,
`ColUtf8`, ...) only match; build with `primColumn`, the `from*`
conversions, the builders, or the validating `mk*` constructors:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Build where

import Arrow.Column
import Control.Monad.ST (runST)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

-- O(1): the vector becomes the values buffer.
prices :: ColumnArray
prices = primColumn PDouble (VS.fromList [101.5, 99.25, 100])

-- A builder appends into pinned, growable buffers; the validity bitmap
-- is allocated on the first null.
quantities :: ColumnArray
quantities = runST $ do
  b <- newPrimBuilder PInt64 3
  appendPrim b 10
  appendNull b
  appendPrim b 30
  freezeBuilder b

-- Convenience constructors from boxed vectors (O(n)).
notes :: ColumnArray
notes = fromMaybeTexts (V.fromList [Just "filled", Nothing, Just "late"])

-- Nested columns: build the children, then validate with mk*.
fills :: Either String ColumnArray
fills = mkList Nothing (VS.fromList [0, 2, 2, 3]) (primColumn PInt32 (VS.fromList [5, 7, 9]))
```

Read through typed views: bind `asPrim`, `asUtf8`, `asBinary` or
`asBool` once per column, then index with `primAt`, `textAt`, `bytesAt`
or `boolArrayAt` (`listRange` for list and map rows, `dictKeyAt` for
dictionary rows).

For a whole column of Haskell values, the `to*Vector` conversions
return an `Arrow.Vector.Vector`: a data family (like
`Data.Vector.Unboxed`) used through `Data.Vector.Generic` with element
types such as `Maybe Int64` or `Maybe Text`, stored the way the column
is. A `Vector (Maybe Int64)` is the column's validity bitmap plus its
storable values (8 bytes and one bit per row, no heap object per row),
and fused folds over it compile to a loop that allocates nothing per
row. `toMaybeVector` and `toBoolVector` are O(1) (they alias the
column; a column without nulls gets a fresh all-set bitmap);
`toTextVector`, `toBytesVector` and `toListVector` are O(rows) (offsets
widened to `Int`), with every row a slice of the column's data:
`Text` rows are neither copied nor re-validated (data outside the GHC
heap, such as an mmapped file, is copied once), views copy their bytes
once, and dictionaries convert their values once and share them. The
inverses `fromMaybeVector` and `fromBoolVector` are O(1) plus a
popcount. `G.convert` gives a boxed `Data.Vector`; `G.force` copies the
rows into fresh buffers, detaching them from the input.

```haskell
{-# LANGUAGE BangPatterns #-}
module Read where

import Arrow.Column
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G

-- Bind the typed view once, then index; the loop does not allocate.
sumQuantities :: ColumnArray -> Maybe Int64
sumQuantities c = do
  p <- asPrim PInt64 c
  let n = primArrayLength p
      go !acc i
        | i >= n = acc
        | otherwise = go (acc + maybe 0 id (primAt p i)) (i + 1)
  pure (go 0 0)

-- The same sum over the whole column: O(1) conversion, fused fold.
sumQuantities' :: ColumnArray -> Maybe Int64
sumQuantities' c = G.foldl' (\acc m -> maybe acc (+ acc) m) 0 . toMaybeVector <$> asPrim PInt64 c

-- One copy into a Text, no UTF-8 re-validation.
noteAt :: ColumnArray -> Int -> Maybe Text
noteAt c i = asUtf8 c >>= \u -> textAt u i

-- A boxed Data.Vector when an API wants one.
quantityList :: ColumnArray -> Maybe (V.Vector (Maybe Int64))
quantityList c = G.convert . toMaybeVector <$> asPrim PInt64 c
```

Decoded columns alias the input bytes and keep them alive. Holding a
small column from a large message pins the whole message; `copyColumn`
copies only the bytes the column references:

```haskell
module Lifetime where

import Arrow.Column (ColumnArray, copyColumn)
import Arrow.Stream (decodeArrowStream)
import Data.ByteString (ByteString)
import qualified Data.Vector as V

-- Decoded columns alias 'bytes'. Keeping one small column from a large
-- message would keep the whole message alive; copyColumn detaches it.
firstColumn :: ByteString -> Either String ColumnArray
firstColumn bytes = do
  (_, batches) <- decodeArrowStream bytes
  case batches of
    cols : _ | not (V.null cols) -> Right (copyColumn (V.head cols))
    _ -> Left "no columns"
```

Dictionary keys are an integer column at the wire width of the field's
index type and carry the row validity. `Eq` is logical: same type, same
length, the same validity per row and equal values in valid rows; null
slot contents, offsets, slices and dictionary layouts do not matter.

The eager encoders write the whole output into one allocation. The lazy
encoders return chunks that alias the column buffers, so their cost
follows the buffer count rather than the data size:

```haskell
module Lazy where

import Arrow.Column (ColumnArray)
import Arrow.Stream (defaultWriteOptions, encodeArrowStreamLazy)
import Arrow.Types (Schema)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import System.IO (Handle)

-- The chunks are the message headers and the column buffers themselves:
-- no body copy, so the cost tracks the buffer count, not the data size.
writeBatches :: Handle -> Schema -> [V.Vector ColumnArray] -> IO (Either String ())
writeBatches h schema batches =
  traverse (BL.hPut h) (encodeArrowStreamLazy defaultWriteOptions schema batches)
```

### Migrating from the boxed constructors

The boxed constructors and their `*Maybe` twins (`ColInt64Maybe (V.Vector
(Maybe Int64))`, `ColUtf8 (V.Vector Text)`, `ColListMaybe valid offsets
child`, ...) are gone. `ColInt64Maybe v` becomes `fromMaybes PInt64 v`;
`ColUtf8Maybe v` becomes `fromMaybeTexts v`; `ColListMaybe valid offs
child` becomes `mkList (validityFromBools valid) offs child`;
`ColDictionary i keys vals` becomes `mkDictionary i keysColumn vals`;
matching on `ColInt64Maybe v` becomes matching `ColInt64 validity v` or
reading through `asPrim PInt64`; `isNullableColumn c` becomes
`nullCount c > 0`.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Migrate where

import Arrow.Column
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

-- Before: ColInt64Maybe (V.fromList [Just 1, Nothing])
nullableIds :: ColumnArray
nullableIds = fromMaybes PInt64 (V.fromList [Just 1, Nothing])

-- Before: ColUtf8 (V.fromList ["a", "b"])
names :: ColumnArray
names = fromTexts (V.fromList ["a", "b"])

-- Before: ColDictionary 0 (VP.fromList [0, 1, 0]) (ColUtf8 ...)
-- Keys are an integer column at the wire width and carry the row nulls.
colours :: Either String ColumnArray
colours =
  mkDictionary 0 (fromMaybes PInt32 (V.fromList [Just 0, Nothing, Just 1]))
    (fromTexts (V.fromList ["red", "green"]))

-- Before: case c of ColInt64Maybe v -> ...; ColInt64 v -> ...
-- After: one pattern; the validity is Nothing when there are no nulls.
describe :: ColumnArray -> String
describe c = case c of
  ColInt64 Nothing v -> "dense int64 x" <> show (VS.length v)
  ColInt64 (Just m) _ -> show (validityNullCount m) <> " nulls"
  _ -> columnTag c

-- Before: ColListMaybe valid offsets child
nullableLists :: Either String ColumnArray
nullableLists =
  mkList (validityFromBools (V.fromList [True, False])) (VS.fromList [0, 1, 1])
    (primColumn PInt32 (VS.fromList [7 :: Int32]))

-- Equality is logical: layout differences that do not change the rows
-- (slices, offset bases, null slot contents) compare equal.
sameRows :: Bool
sameRows = sliceColumnArray 1 1 names == fromTexts (V.fromList ["b" :: Text])
```

## Notable modules

| Module | Purpose |
|--------|---------|
| `Arrow.Types` | Schema, field, and buffer types; `schemaFingerprint` |
| `Arrow.Column` | `ColumnArray` (Arrow-native buffers, `Maybe Validity`), patterns, typed accessors, `from*`/`mk*` construction, `copyColumn`, slice/concat/take, logical `Eq` |
| `Arrow.Column.Builder` | Growable pinned builders in `ST`/`IO` (re-exported by `Arrow.Column`) |
| `Arrow.Column.Buffer` | `Bitmap`, `Validity`, fixed-width element types, aligned allocation |
| `Arrow.Column.Internal` | Raw constructors and structural validators, for readers, writers and malformed-input tests |
| `Arrow.Vector` | `Vector a` data family behind the `Data.Vector.Generic` API in Arrow layout (storable values, bitmaps, validity plus values for `Maybe a`, offsets into one store for `Text`, `ByteString` and lists); what the `to*Vector` conversions return |
| `Arrow.Read.Columns` | Record batch to columns: buffer slicing, validation, zero-copy aliasing |
| `Arrow.Record` | Typed `Table`, `Encoder`, `Decoder`, projection helpers |
| `Arrow.Record.Generic` / `Arrow.Record.TH` | Generic and TH record derivation |
| `Arrow.Derive` | Annotation-driven deriver |
| `Arrow.IPC` | IPC message framing encode/decode |
| `Arrow.Stream` | Stream and file encode / decode (with dictionaries, including nested ones), lazy encoders, pull-based streaming reader |
| `Arrow.File` | Arrow file format readers |
| `Arrow.FlatBufferIPC` | FlatBuffer-backed IPC metadata path (re-exports `.Common`, `.Read`, `.Write`) |
| `Arrow.Write` | `writeArrowStream` / `writeArrowFile`, column encoders, `validateColumns` |

## Compression

Enable Zstd or LZ4 with Cabal flags (`+zstd`, `+lz4`). Compressed IPC
streams follow the standard Arrow body compression layout; uncompressed
IPC remains the default for maximum interoperability.

## Performance

One record batch per workload through `Arrow.Stream`, set against
[arrow-rs](https://crates.io/crates/arrow) 58 building the same batches
from the same generators. Four groups per workload: `encode` (one
allocation; arrow-rs `StreamWriter` into a `Vec<u8>`), `encode lazy`
(chunks aliasing the column buffers; arrow-rs has no lazy writer, so
it is set against `StreamWriter`), `decode` (zero copy; arrow-rs
`StreamReader` with its default validation on), and `decode + toVector`
(decode, convert every column with the `to*Vector` conversions, then
`G.force` each result into fresh buffers so it owns its data; arrow-rs
copies into owned `Vec<Option<T>>`, `String` and nested `Vec` values).
Dictionary rows share one owned copy of the values instead of one
string per row. Inputs are built outside the
timed region. The ratio column is wireform-arrow time over arrow-rs
time, so `2.00x` means wireform-arrow takes twice as long. Run both,
sequentially, with
`python3 scripts/run-benchmarks.py --only arrow --render`; the harnesses
are `wireform-arrow/bench/Bench.hs` and
`interop/arrow-rs/benches/arrow_ipc.rs`.

The bench binary runs with a 64 MB nursery (`-with-rtsopts=-A64m`).
Decoding, lazy encoding and the `Arrow.Vector` conversions barely
allocate, but code that builds boxed values (the typed `Arrow.Record`
rows, `G.convert` to a boxed vector, your own conversions) is dominated
by minor collections at GHC's default 4 MB nursery. Programs that move
large batches should do the same: link with `-rtsopts` and run with
`+RTS -A64m`, or bake it in with `-with-rtsopts`.

### Encode/decode, 100k rows

<!-- BEGIN_AUTOGEN bench:arrow-encode-decode -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-arrow vs arrow-rs, IPC stream encode + decode (100k rows)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-arrow vs arrow-rs, IPC stream encode + decode (100k rows)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5; arrow-rs 58.2.0, rustc 1.98.1, criterion.rs 0.7.0</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">1250</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">2500</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">3750</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">5000</text>
    </g>
    <g>
      <rect x="81.7" y="318.0" width="4.9" height="2.0" rx="2" fill="#0969da"/>
      <rect x="88.6" y="319.4" width="4.9" height="0.6" rx="2" fill="#cf222e"/>
      <rect x="98.9" y="318.0" width="4.9" height="2.0" rx="2" fill="#0969da"/>
      <rect x="105.8" y="319.4" width="4.9" height="0.6" rx="2" fill="#cf222e"/>
      <rect x="116.2" y="318.0" width="4.9" height="2.0" rx="2" fill="#0969da"/>
      <rect x="123.1" y="319.4" width="4.9" height="0.6" rx="2" fill="#cf222e"/>
      <rect x="133.4" y="318.0" width="4.9" height="2.0" rx="2" fill="#0969da"/>
      <rect x="140.3" y="319.3" width="4.9" height="0.7" rx="2" fill="#cf222e"/>
      <rect x="150.6" y="318.0" width="4.9" height="2.0" rx="2" fill="#0969da"/>
      <rect x="157.5" y="319.3" width="4.9" height="0.7" rx="2" fill="#cf222e"/>
      <rect x="167.8" y="294.7" width="4.9" height="25.3" rx="2" fill="#0969da"/>
      <rect x="174.7" y="316.8" width="4.9" height="3.2" rx="2" fill="#cf222e"/>
      <rect x="185.1" y="315.0" width="4.9" height="5.0" rx="2" fill="#0969da"/>
      <rect x="191.9" y="318.6" width="4.9" height="1.4" rx="2" fill="#cf222e"/>
      <rect x="202.3" y="316.7" width="4.9" height="3.3" rx="2" fill="#0969da"/>
      <rect x="209.2" y="319.1" width="4.9" height="0.9" rx="2" fill="#cf222e"/>
      <rect x="219.5" y="318.5" width="4.9" height="1.5" rx="2" fill="#0969da"/>
      <rect x="226.4" y="319.7" width="4.9" height="0.3" rx="2" fill="#cf222e"/>
      <rect x="236.7" y="318.0" width="4.9" height="2.0" rx="2" fill="#0969da"/>
      <rect x="243.6" y="320.0" width="4.9" height="0.0" rx="2" fill="#cf222e"/>
      <rect x="253.9" y="318.0" width="4.9" height="2.0" rx="2" fill="#0969da"/>
      <rect x="260.8" y="320.0" width="4.9" height="0.0" rx="2" fill="#cf222e"/>
      <rect x="271.2" y="318.0" width="4.9" height="2.0" rx="2" fill="#0969da"/>
      <rect x="278.1" y="320.0" width="4.9" height="0.0" rx="2" fill="#cf222e"/>
      <rect x="288.4" y="318.0" width="4.9" height="2.0" rx="2" fill="#0969da"/>
      <rect x="295.3" y="320.0" width="4.9" height="0.0" rx="2" fill="#cf222e"/>
      <rect x="305.6" y="318.0" width="4.9" height="2.0" rx="2" fill="#0969da"/>
      <rect x="312.5" y="320.0" width="4.9" height="0.0" rx="2" fill="#cf222e"/>
      <rect x="322.8" y="294.7" width="4.9" height="25.3" rx="2" fill="#0969da"/>
      <rect x="329.7" y="319.9" width="4.9" height="0.1" rx="2" fill="#cf222e"/>
      <rect x="340.1" y="315.0" width="4.9" height="5.0" rx="2" fill="#0969da"/>
      <rect x="346.9" y="320.0" width="4.9" height="0.0" rx="2" fill="#cf222e"/>
      <rect x="357.3" y="316.7" width="4.9" height="3.3" rx="2" fill="#0969da"/>
      <rect x="364.2" y="319.9" width="4.9" height="0.1" rx="2" fill="#cf222e"/>
      <rect x="374.5" y="318.5" width="4.9" height="1.5" rx="2" fill="#0969da"/>
      <rect x="381.4" y="319.9" width="4.9" height="0.1" rx="2" fill="#cf222e"/>
      <rect x="391.7" y="319.3" width="4.9" height="0.7" rx="2" fill="#0969da"/>
      <rect x="398.6" y="320.0" width="4.9" height="0.0" rx="2" fill="#cf222e"/>
      <rect x="408.9" y="319.3" width="4.9" height="0.7" rx="2" fill="#0969da"/>
      <rect x="415.8" y="320.0" width="4.9" height="0.0" rx="2" fill="#cf222e"/>
      <rect x="426.2" y="319.3" width="4.9" height="0.7" rx="2" fill="#0969da"/>
      <rect x="433.1" y="320.0" width="4.9" height="0.0" rx="2" fill="#cf222e"/>
      <rect x="443.4" y="313.6" width="4.9" height="6.4" rx="2" fill="#0969da"/>
      <rect x="450.3" y="317.5" width="4.9" height="2.5" rx="2" fill="#cf222e"/>
      <rect x="460.6" y="313.5" width="4.9" height="6.5" rx="2" fill="#0969da"/>
      <rect x="467.5" y="317.6" width="4.9" height="2.4" rx="2" fill="#cf222e"/>
      <rect x="477.8" y="304.6" width="4.9" height="15.4" rx="2" fill="#0969da"/>
      <rect x="484.7" y="315.0" width="4.9" height="5.0" rx="2" fill="#cf222e"/>
      <rect x="495.1" y="314.4" width="4.9" height="5.6" rx="2" fill="#0969da"/>
      <rect x="501.9" y="319.7" width="4.9" height="0.3" rx="2" fill="#cf222e"/>
      <rect x="512.3" y="318.9" width="4.9" height="1.1" rx="2" fill="#0969da"/>
      <rect x="519.2" y="319.9" width="4.9" height="0.1" rx="2" fill="#cf222e"/>
      <rect x="529.5" y="318.1" width="4.9" height="1.9" rx="2" fill="#0969da"/>
      <rect x="536.4" y="318.9" width="4.9" height="1.1" rx="2" fill="#cf222e"/>
      <rect x="546.7" y="311.9" width="4.9" height="8.1" rx="2" fill="#0969da"/>
      <rect x="553.6" y="319.2" width="4.9" height="0.8" rx="2" fill="#cf222e"/>
      <rect x="563.9" y="311.3" width="4.9" height="8.7" rx="2" fill="#0969da"/>
      <rect x="570.8" y="319.2" width="4.9" height="0.8" rx="2" fill="#cf222e"/>
      <rect x="581.2" y="311.9" width="4.9" height="8.1" rx="2" fill="#0969da"/>
      <rect x="588.1" y="319.2" width="4.9" height="0.8" rx="2" fill="#cf222e"/>
      <rect x="598.4" y="246.0" width="4.9" height="74.0" rx="2" fill="#0969da"/>
      <rect x="605.3" y="307.4" width="4.9" height="12.6" rx="2" fill="#cf222e"/>
      <rect x="615.6" y="244.3" width="4.9" height="75.7" rx="2" fill="#0969da"/>
      <rect x="622.5" y="306.5" width="4.9" height="13.5" rx="2" fill="#cf222e"/>
      <rect x="632.8" y="140.0" width="4.9" height="180.0" rx="2" fill="#0969da"/>
      <rect x="639.7" y="291.1" width="4.9" height="28.9" rx="2" fill="#cf222e"/>
      <rect x="650.1" y="129.2" width="4.9" height="190.8" rx="2" fill="#0969da"/>
      <rect x="656.9" y="310.1" width="4.9" height="9.9" rx="2" fill="#cf222e"/>
      <rect x="667.3" y="303.0" width="4.9" height="17.0" rx="2" fill="#0969da"/>
      <rect x="674.2" y="318.6" width="4.9" height="1.4" rx="2" fill="#cf222e"/>
      <rect x="684.5" y="247.9" width="4.9" height="72.1" rx="2" fill="#0969da"/>
      <rect x="691.4" y="297.5" width="4.9" height="22.5" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="84.2" y="314.0" text-anchor="middle" font-size="10" fill="#1f2328">38.6</text>
      <text x="91.1" y="315.4" text-anchor="middle" font-size="10" fill="#1f2328">10.9</text>
      <text x="101.4" y="314.0" text-anchor="middle" font-size="10" fill="#1f2328">38.5</text>
      <text x="108.3" y="315.4" text-anchor="middle" font-size="10" fill="#1f2328">10.6</text>
      <text x="118.6" y="314.0" text-anchor="middle" font-size="10" fill="#1f2328">37.7</text>
      <text x="125.5" y="315.4" text-anchor="middle" font-size="10" fill="#1f2328">10.9</text>
      <text x="135.8" y="314.0" text-anchor="middle" font-size="10" fill="#1f2328">38.9</text>
      <text x="142.7" y="315.3" text-anchor="middle" font-size="10" fill="#1f2328">13.1</text>
      <text x="153.1" y="314.0" text-anchor="middle" font-size="10" fill="#1f2328">38.2</text>
      <text x="159.9" y="315.3" text-anchor="middle" font-size="10" fill="#1f2328">12.7</text>
      <text x="170.3" y="290.7" text-anchor="middle" font-size="10" fill="#1f2328">486</text>
      <text x="177.2" y="312.8" text-anchor="middle" font-size="10" fill="#1f2328">61.9</text>
      <text x="187.5" y="311.0" text-anchor="middle" font-size="10" fill="#1f2328">96.4</text>
      <text x="194.4" y="314.6" text-anchor="middle" font-size="10" fill="#1f2328">27.5</text>
      <text x="204.7" y="312.7" text-anchor="middle" font-size="10" fill="#1f2328">63.6</text>
      <text x="211.6" y="315.1" text-anchor="middle" font-size="10" fill="#1f2328">16.8</text>
      <text x="221.9" y="314.5" text-anchor="middle" font-size="10" fill="#1f2328">28.6</text>
      <text x="228.8" y="315.7" text-anchor="middle" font-size="10" fill="#1f2328">6.14</text>
      <text x="239.2" y="314.0" text-anchor="middle" font-size="10" fill="#1f2328">38.6</text>
      <text x="246.1" y="316.0" text-anchor="middle" font-size="10" fill="#1f2328">0.640</text>
      <text x="256.4" y="314.0" text-anchor="middle" font-size="10" fill="#1f2328">38.5</text>
      <text x="263.3" y="316.0" text-anchor="middle" font-size="10" fill="#1f2328">0.620</text>
      <text x="273.6" y="314.0" text-anchor="middle" font-size="10" fill="#1f2328">37.7</text>
      <text x="280.5" y="316.0" text-anchor="middle" font-size="10" fill="#1f2328">0.660</text>
      <text x="290.8" y="314.0" text-anchor="middle" font-size="10" fill="#1f2328">38.9</text>
      <text x="297.7" y="316.0" text-anchor="middle" font-size="10" fill="#1f2328">0.680</text>
      <text x="308.1" y="314.0" text-anchor="middle" font-size="10" fill="#1f2328">38.2</text>
      <text x="314.9" y="316.0" text-anchor="middle" font-size="10" fill="#1f2328">0.700</text>
      <text x="325.3" y="290.7" text-anchor="middle" font-size="10" fill="#1f2328">486</text>
      <text x="332.2" y="315.9" text-anchor="middle" font-size="10" fill="#1f2328">2.06</text>
      <text x="342.5" y="311.0" text-anchor="middle" font-size="10" fill="#1f2328">96.4</text>
      <text x="349.4" y="316.0" text-anchor="middle" font-size="10" fill="#1f2328">0.920</text>
      <text x="359.7" y="312.7" text-anchor="middle" font-size="10" fill="#1f2328">63.6</text>
      <text x="366.6" y="315.9" text-anchor="middle" font-size="10" fill="#1f2328">1.39</text>
      <text x="376.9" y="314.5" text-anchor="middle" font-size="10" fill="#1f2328">28.6</text>
      <text x="383.8" y="315.9" text-anchor="middle" font-size="10" fill="#1f2328">1.14</text>
      <text x="394.2" y="315.3" text-anchor="middle" font-size="10" fill="#1f2328">13.2</text>
      <text x="401.1" y="316.0" text-anchor="middle" font-size="10" fill="#1f2328">0.690</text>
      <text x="411.4" y="315.3" text-anchor="middle" font-size="10" fill="#1f2328">12.8</text>
      <text x="418.3" y="316.0" text-anchor="middle" font-size="10" fill="#1f2328">0.670</text>
      <text x="428.6" y="315.3" text-anchor="middle" font-size="10" fill="#1f2328">13.1</text>
      <text x="435.5" y="316.0" text-anchor="middle" font-size="10" fill="#1f2328">0.960</text>
      <text x="445.8" y="309.6" text-anchor="middle" font-size="10" fill="#1f2328">122</text>
      <text x="452.7" y="313.5" text-anchor="middle" font-size="10" fill="#1f2328">47.1</text>
      <text x="463.1" y="309.5" text-anchor="middle" font-size="10" fill="#1f2328">125</text>
      <text x="469.9" y="313.6" text-anchor="middle" font-size="10" fill="#1f2328">47.0</text>
      <text x="480.3" y="300.6" text-anchor="middle" font-size="10" fill="#1f2328">296</text>
      <text x="487.2" y="311.0" text-anchor="middle" font-size="10" fill="#1f2328">95.3</text>
      <text x="497.5" y="310.4" text-anchor="middle" font-size="10" fill="#1f2328">108</text>
      <text x="504.4" y="315.7" text-anchor="middle" font-size="10" fill="#1f2328">6.24</text>
      <text x="514.7" y="314.9" text-anchor="middle" font-size="10" fill="#1f2328">20.4</text>
      <text x="521.6" y="315.9" text-anchor="middle" font-size="10" fill="#1f2328">1.37</text>
      <text x="531.9" y="314.1" text-anchor="middle" font-size="10" fill="#1f2328">36.6</text>
      <text x="538.8" y="314.9" text-anchor="middle" font-size="10" fill="#1f2328">20.4</text>
      <text x="549.2" y="307.9" text-anchor="middle" font-size="10" fill="#1f2328">155</text>
      <text x="556.1" y="315.2" text-anchor="middle" font-size="10" fill="#1f2328">15.5</text>
      <text x="566.4" y="307.3" text-anchor="middle" font-size="10" fill="#1f2328">167</text>
      <text x="573.3" y="315.2" text-anchor="middle" font-size="10" fill="#1f2328">15.2</text>
      <text x="583.6" y="307.9" text-anchor="middle" font-size="10" fill="#1f2328">157</text>
      <text x="590.5" y="315.2" text-anchor="middle" font-size="10" fill="#1f2328">14.9</text>
      <text x="600.8" y="242.0" text-anchor="middle" font-size="10" fill="#1f2328">1422</text>
      <text x="607.7" y="303.4" text-anchor="middle" font-size="10" fill="#1f2328">242</text>
      <text x="618.1" y="240.3" text-anchor="middle" font-size="10" fill="#1f2328">1455</text>
      <text x="624.9" y="302.5" text-anchor="middle" font-size="10" fill="#1f2328">259</text>
      <text x="635.3" y="136.0" text-anchor="middle" font-size="10" fill="#1f2328">3462</text>
      <text x="642.2" y="287.1" text-anchor="middle" font-size="10" fill="#1f2328">556</text>
      <text x="652.5" y="125.2" text-anchor="middle" font-size="10" fill="#1f2328">3669</text>
      <text x="659.4" y="306.1" text-anchor="middle" font-size="10" fill="#1f2328">190</text>
      <text x="669.7" y="299.0" text-anchor="middle" font-size="10" fill="#1f2328">326</text>
      <text x="676.6" y="314.6" text-anchor="middle" font-size="10" fill="#1f2328">26.1</text>
      <text x="686.9" y="243.9" text-anchor="middle" font-size="10" fill="#1f2328">1387</text>
      <text x="693.8" y="293.5" text-anchor="middle" font-size="10" fill="#1f2328">433</text>
    </g>
    <g>
      <text x="88.6" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode int64</text>
      <text x="105.8" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode double</text>
      <text x="123.1" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode nullable int64</text>
      <text x="140.3" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode utf8</text>
      <text x="157.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode nullable utf8</text>
      <text x="174.7" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode mixed 6-col</text>
      <text x="191.9" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode list&lt;int32&gt;</text>
      <text x="209.2" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode struct&lt;int32,double,bool&gt;</text>
      <text x="226.4" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode dictionary&lt;utf8&gt;</text>
      <text x="243.6" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode lazy int64</text>
      <text x="260.8" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode lazy double</text>
      <text x="278.1" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode lazy nullable int64</text>
      <text x="295.3" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode lazy utf8</text>
      <text x="312.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode lazy nullable utf8</text>
      <text x="329.7" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode lazy mixed 6-col</text>
      <text x="346.9" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode lazy list&lt;int32&gt;</text>
      <text x="364.2" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode lazy struct&lt;int32,double,bool&gt;</text>
      <text x="381.4" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode lazy dictionary&lt;utf8&gt;</text>
      <text x="398.6" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode int64</text>
      <text x="415.8" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode double</text>
      <text x="433.1" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode nullable int64</text>
      <text x="450.3" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode utf8</text>
      <text x="467.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode nullable utf8</text>
      <text x="484.7" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode mixed 6-col</text>
      <text x="501.9" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode list&lt;int32&gt;</text>
      <text x="519.2" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode struct&lt;int32,double,bool&gt;</text>
      <text x="536.4" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode dictionary&lt;utf8&gt;</text>
      <text x="553.6" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode + toVector int64</text>
      <text x="570.8" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode + toVector double</text>
      <text x="588.1" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode + toVector nullable int64</text>
      <text x="605.3" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode + toVector utf8</text>
      <text x="622.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode + toVector nullable utf8</text>
      <text x="639.7" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode + toVector mixed 6-col</text>
      <text x="656.9" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode + toVector list&lt;int32&gt;</text>
      <text x="674.2" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode + toVector struct&lt;int32,double,bool&gt;</text>
      <text x="691.4" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode + toVector dictionary&lt;utf8&gt;</text>
    </g>
    <g>
      <g transform="translate(257, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">arrow-rs</text>
      </g>
      <g transform="translate(347, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">wireform-arrow</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-arrow vs arrow-rs, IPC stream encode + decode (100k rows)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5; arrow-rs 58.2.0, rustc 1.98.1, criterion.rs 0.7.0</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">1250</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">2500</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">3750</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">5000</text>
    </g>
    <g>
      <rect x="81.7" y="318.0" width="4.9" height="2.0" rx="2" fill="#58a6ff"/>
      <rect x="88.6" y="319.4" width="4.9" height="0.6" rx="2" fill="#ff7b72"/>
      <rect x="98.9" y="318.0" width="4.9" height="2.0" rx="2" fill="#58a6ff"/>
      <rect x="105.8" y="319.4" width="4.9" height="0.6" rx="2" fill="#ff7b72"/>
      <rect x="116.2" y="318.0" width="4.9" height="2.0" rx="2" fill="#58a6ff"/>
      <rect x="123.1" y="319.4" width="4.9" height="0.6" rx="2" fill="#ff7b72"/>
      <rect x="133.4" y="318.0" width="4.9" height="2.0" rx="2" fill="#58a6ff"/>
      <rect x="140.3" y="319.3" width="4.9" height="0.7" rx="2" fill="#ff7b72"/>
      <rect x="150.6" y="318.0" width="4.9" height="2.0" rx="2" fill="#58a6ff"/>
      <rect x="157.5" y="319.3" width="4.9" height="0.7" rx="2" fill="#ff7b72"/>
      <rect x="167.8" y="294.7" width="4.9" height="25.3" rx="2" fill="#58a6ff"/>
      <rect x="174.7" y="316.8" width="4.9" height="3.2" rx="2" fill="#ff7b72"/>
      <rect x="185.1" y="315.0" width="4.9" height="5.0" rx="2" fill="#58a6ff"/>
      <rect x="191.9" y="318.6" width="4.9" height="1.4" rx="2" fill="#ff7b72"/>
      <rect x="202.3" y="316.7" width="4.9" height="3.3" rx="2" fill="#58a6ff"/>
      <rect x="209.2" y="319.1" width="4.9" height="0.9" rx="2" fill="#ff7b72"/>
      <rect x="219.5" y="318.5" width="4.9" height="1.5" rx="2" fill="#58a6ff"/>
      <rect x="226.4" y="319.7" width="4.9" height="0.3" rx="2" fill="#ff7b72"/>
      <rect x="236.7" y="318.0" width="4.9" height="2.0" rx="2" fill="#58a6ff"/>
      <rect x="243.6" y="320.0" width="4.9" height="0.0" rx="2" fill="#ff7b72"/>
      <rect x="253.9" y="318.0" width="4.9" height="2.0" rx="2" fill="#58a6ff"/>
      <rect x="260.8" y="320.0" width="4.9" height="0.0" rx="2" fill="#ff7b72"/>
      <rect x="271.2" y="318.0" width="4.9" height="2.0" rx="2" fill="#58a6ff"/>
      <rect x="278.1" y="320.0" width="4.9" height="0.0" rx="2" fill="#ff7b72"/>
      <rect x="288.4" y="318.0" width="4.9" height="2.0" rx="2" fill="#58a6ff"/>
      <rect x="295.3" y="320.0" width="4.9" height="0.0" rx="2" fill="#ff7b72"/>
      <rect x="305.6" y="318.0" width="4.9" height="2.0" rx="2" fill="#58a6ff"/>
      <rect x="312.5" y="320.0" width="4.9" height="0.0" rx="2" fill="#ff7b72"/>
      <rect x="322.8" y="294.7" width="4.9" height="25.3" rx="2" fill="#58a6ff"/>
      <rect x="329.7" y="319.9" width="4.9" height="0.1" rx="2" fill="#ff7b72"/>
      <rect x="340.1" y="315.0" width="4.9" height="5.0" rx="2" fill="#58a6ff"/>
      <rect x="346.9" y="320.0" width="4.9" height="0.0" rx="2" fill="#ff7b72"/>
      <rect x="357.3" y="316.7" width="4.9" height="3.3" rx="2" fill="#58a6ff"/>
      <rect x="364.2" y="319.9" width="4.9" height="0.1" rx="2" fill="#ff7b72"/>
      <rect x="374.5" y="318.5" width="4.9" height="1.5" rx="2" fill="#58a6ff"/>
      <rect x="381.4" y="319.9" width="4.9" height="0.1" rx="2" fill="#ff7b72"/>
      <rect x="391.7" y="319.3" width="4.9" height="0.7" rx="2" fill="#58a6ff"/>
      <rect x="398.6" y="320.0" width="4.9" height="0.0" rx="2" fill="#ff7b72"/>
      <rect x="408.9" y="319.3" width="4.9" height="0.7" rx="2" fill="#58a6ff"/>
      <rect x="415.8" y="320.0" width="4.9" height="0.0" rx="2" fill="#ff7b72"/>
      <rect x="426.2" y="319.3" width="4.9" height="0.7" rx="2" fill="#58a6ff"/>
      <rect x="433.1" y="320.0" width="4.9" height="0.0" rx="2" fill="#ff7b72"/>
      <rect x="443.4" y="313.6" width="4.9" height="6.4" rx="2" fill="#58a6ff"/>
      <rect x="450.3" y="317.5" width="4.9" height="2.5" rx="2" fill="#ff7b72"/>
      <rect x="460.6" y="313.5" width="4.9" height="6.5" rx="2" fill="#58a6ff"/>
      <rect x="467.5" y="317.6" width="4.9" height="2.4" rx="2" fill="#ff7b72"/>
      <rect x="477.8" y="304.6" width="4.9" height="15.4" rx="2" fill="#58a6ff"/>
      <rect x="484.7" y="315.0" width="4.9" height="5.0" rx="2" fill="#ff7b72"/>
      <rect x="495.1" y="314.4" width="4.9" height="5.6" rx="2" fill="#58a6ff"/>
      <rect x="501.9" y="319.7" width="4.9" height="0.3" rx="2" fill="#ff7b72"/>
      <rect x="512.3" y="318.9" width="4.9" height="1.1" rx="2" fill="#58a6ff"/>
      <rect x="519.2" y="319.9" width="4.9" height="0.1" rx="2" fill="#ff7b72"/>
      <rect x="529.5" y="318.1" width="4.9" height="1.9" rx="2" fill="#58a6ff"/>
      <rect x="536.4" y="318.9" width="4.9" height="1.1" rx="2" fill="#ff7b72"/>
      <rect x="546.7" y="311.9" width="4.9" height="8.1" rx="2" fill="#58a6ff"/>
      <rect x="553.6" y="319.2" width="4.9" height="0.8" rx="2" fill="#ff7b72"/>
      <rect x="563.9" y="311.3" width="4.9" height="8.7" rx="2" fill="#58a6ff"/>
      <rect x="570.8" y="319.2" width="4.9" height="0.8" rx="2" fill="#ff7b72"/>
      <rect x="581.2" y="311.9" width="4.9" height="8.1" rx="2" fill="#58a6ff"/>
      <rect x="588.1" y="319.2" width="4.9" height="0.8" rx="2" fill="#ff7b72"/>
      <rect x="598.4" y="246.0" width="4.9" height="74.0" rx="2" fill="#58a6ff"/>
      <rect x="605.3" y="307.4" width="4.9" height="12.6" rx="2" fill="#ff7b72"/>
      <rect x="615.6" y="244.3" width="4.9" height="75.7" rx="2" fill="#58a6ff"/>
      <rect x="622.5" y="306.5" width="4.9" height="13.5" rx="2" fill="#ff7b72"/>
      <rect x="632.8" y="140.0" width="4.9" height="180.0" rx="2" fill="#58a6ff"/>
      <rect x="639.7" y="291.1" width="4.9" height="28.9" rx="2" fill="#ff7b72"/>
      <rect x="650.1" y="129.2" width="4.9" height="190.8" rx="2" fill="#58a6ff"/>
      <rect x="656.9" y="310.1" width="4.9" height="9.9" rx="2" fill="#ff7b72"/>
      <rect x="667.3" y="303.0" width="4.9" height="17.0" rx="2" fill="#58a6ff"/>
      <rect x="674.2" y="318.6" width="4.9" height="1.4" rx="2" fill="#ff7b72"/>
      <rect x="684.5" y="247.9" width="4.9" height="72.1" rx="2" fill="#58a6ff"/>
      <rect x="691.4" y="297.5" width="4.9" height="22.5" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="84.2" y="314.0" text-anchor="middle" font-size="10" fill="#e6edf3">38.6</text>
      <text x="91.1" y="315.4" text-anchor="middle" font-size="10" fill="#e6edf3">10.9</text>
      <text x="101.4" y="314.0" text-anchor="middle" font-size="10" fill="#e6edf3">38.5</text>
      <text x="108.3" y="315.4" text-anchor="middle" font-size="10" fill="#e6edf3">10.6</text>
      <text x="118.6" y="314.0" text-anchor="middle" font-size="10" fill="#e6edf3">37.7</text>
      <text x="125.5" y="315.4" text-anchor="middle" font-size="10" fill="#e6edf3">10.9</text>
      <text x="135.8" y="314.0" text-anchor="middle" font-size="10" fill="#e6edf3">38.9</text>
      <text x="142.7" y="315.3" text-anchor="middle" font-size="10" fill="#e6edf3">13.1</text>
      <text x="153.1" y="314.0" text-anchor="middle" font-size="10" fill="#e6edf3">38.2</text>
      <text x="159.9" y="315.3" text-anchor="middle" font-size="10" fill="#e6edf3">12.7</text>
      <text x="170.3" y="290.7" text-anchor="middle" font-size="10" fill="#e6edf3">486</text>
      <text x="177.2" y="312.8" text-anchor="middle" font-size="10" fill="#e6edf3">61.9</text>
      <text x="187.5" y="311.0" text-anchor="middle" font-size="10" fill="#e6edf3">96.4</text>
      <text x="194.4" y="314.6" text-anchor="middle" font-size="10" fill="#e6edf3">27.5</text>
      <text x="204.7" y="312.7" text-anchor="middle" font-size="10" fill="#e6edf3">63.6</text>
      <text x="211.6" y="315.1" text-anchor="middle" font-size="10" fill="#e6edf3">16.8</text>
      <text x="221.9" y="314.5" text-anchor="middle" font-size="10" fill="#e6edf3">28.6</text>
      <text x="228.8" y="315.7" text-anchor="middle" font-size="10" fill="#e6edf3">6.14</text>
      <text x="239.2" y="314.0" text-anchor="middle" font-size="10" fill="#e6edf3">38.6</text>
      <text x="246.1" y="316.0" text-anchor="middle" font-size="10" fill="#e6edf3">0.640</text>
      <text x="256.4" y="314.0" text-anchor="middle" font-size="10" fill="#e6edf3">38.5</text>
      <text x="263.3" y="316.0" text-anchor="middle" font-size="10" fill="#e6edf3">0.620</text>
      <text x="273.6" y="314.0" text-anchor="middle" font-size="10" fill="#e6edf3">37.7</text>
      <text x="280.5" y="316.0" text-anchor="middle" font-size="10" fill="#e6edf3">0.660</text>
      <text x="290.8" y="314.0" text-anchor="middle" font-size="10" fill="#e6edf3">38.9</text>
      <text x="297.7" y="316.0" text-anchor="middle" font-size="10" fill="#e6edf3">0.680</text>
      <text x="308.1" y="314.0" text-anchor="middle" font-size="10" fill="#e6edf3">38.2</text>
      <text x="314.9" y="316.0" text-anchor="middle" font-size="10" fill="#e6edf3">0.700</text>
      <text x="325.3" y="290.7" text-anchor="middle" font-size="10" fill="#e6edf3">486</text>
      <text x="332.2" y="315.9" text-anchor="middle" font-size="10" fill="#e6edf3">2.06</text>
      <text x="342.5" y="311.0" text-anchor="middle" font-size="10" fill="#e6edf3">96.4</text>
      <text x="349.4" y="316.0" text-anchor="middle" font-size="10" fill="#e6edf3">0.920</text>
      <text x="359.7" y="312.7" text-anchor="middle" font-size="10" fill="#e6edf3">63.6</text>
      <text x="366.6" y="315.9" text-anchor="middle" font-size="10" fill="#e6edf3">1.39</text>
      <text x="376.9" y="314.5" text-anchor="middle" font-size="10" fill="#e6edf3">28.6</text>
      <text x="383.8" y="315.9" text-anchor="middle" font-size="10" fill="#e6edf3">1.14</text>
      <text x="394.2" y="315.3" text-anchor="middle" font-size="10" fill="#e6edf3">13.2</text>
      <text x="401.1" y="316.0" text-anchor="middle" font-size="10" fill="#e6edf3">0.690</text>
      <text x="411.4" y="315.3" text-anchor="middle" font-size="10" fill="#e6edf3">12.8</text>
      <text x="418.3" y="316.0" text-anchor="middle" font-size="10" fill="#e6edf3">0.670</text>
      <text x="428.6" y="315.3" text-anchor="middle" font-size="10" fill="#e6edf3">13.1</text>
      <text x="435.5" y="316.0" text-anchor="middle" font-size="10" fill="#e6edf3">0.960</text>
      <text x="445.8" y="309.6" text-anchor="middle" font-size="10" fill="#e6edf3">122</text>
      <text x="452.7" y="313.5" text-anchor="middle" font-size="10" fill="#e6edf3">47.1</text>
      <text x="463.1" y="309.5" text-anchor="middle" font-size="10" fill="#e6edf3">125</text>
      <text x="469.9" y="313.6" text-anchor="middle" font-size="10" fill="#e6edf3">47.0</text>
      <text x="480.3" y="300.6" text-anchor="middle" font-size="10" fill="#e6edf3">296</text>
      <text x="487.2" y="311.0" text-anchor="middle" font-size="10" fill="#e6edf3">95.3</text>
      <text x="497.5" y="310.4" text-anchor="middle" font-size="10" fill="#e6edf3">108</text>
      <text x="504.4" y="315.7" text-anchor="middle" font-size="10" fill="#e6edf3">6.24</text>
      <text x="514.7" y="314.9" text-anchor="middle" font-size="10" fill="#e6edf3">20.4</text>
      <text x="521.6" y="315.9" text-anchor="middle" font-size="10" fill="#e6edf3">1.37</text>
      <text x="531.9" y="314.1" text-anchor="middle" font-size="10" fill="#e6edf3">36.6</text>
      <text x="538.8" y="314.9" text-anchor="middle" font-size="10" fill="#e6edf3">20.4</text>
      <text x="549.2" y="307.9" text-anchor="middle" font-size="10" fill="#e6edf3">155</text>
      <text x="556.1" y="315.2" text-anchor="middle" font-size="10" fill="#e6edf3">15.5</text>
      <text x="566.4" y="307.3" text-anchor="middle" font-size="10" fill="#e6edf3">167</text>
      <text x="573.3" y="315.2" text-anchor="middle" font-size="10" fill="#e6edf3">15.2</text>
      <text x="583.6" y="307.9" text-anchor="middle" font-size="10" fill="#e6edf3">157</text>
      <text x="590.5" y="315.2" text-anchor="middle" font-size="10" fill="#e6edf3">14.9</text>
      <text x="600.8" y="242.0" text-anchor="middle" font-size="10" fill="#e6edf3">1422</text>
      <text x="607.7" y="303.4" text-anchor="middle" font-size="10" fill="#e6edf3">242</text>
      <text x="618.1" y="240.3" text-anchor="middle" font-size="10" fill="#e6edf3">1455</text>
      <text x="624.9" y="302.5" text-anchor="middle" font-size="10" fill="#e6edf3">259</text>
      <text x="635.3" y="136.0" text-anchor="middle" font-size="10" fill="#e6edf3">3462</text>
      <text x="642.2" y="287.1" text-anchor="middle" font-size="10" fill="#e6edf3">556</text>
      <text x="652.5" y="125.2" text-anchor="middle" font-size="10" fill="#e6edf3">3669</text>
      <text x="659.4" y="306.1" text-anchor="middle" font-size="10" fill="#e6edf3">190</text>
      <text x="669.7" y="299.0" text-anchor="middle" font-size="10" fill="#e6edf3">326</text>
      <text x="676.6" y="314.6" text-anchor="middle" font-size="10" fill="#e6edf3">26.1</text>
      <text x="686.9" y="243.9" text-anchor="middle" font-size="10" fill="#e6edf3">1387</text>
      <text x="693.8" y="293.5" text-anchor="middle" font-size="10" fill="#e6edf3">433</text>
    </g>
    <g>
      <text x="88.6" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode int64</text>
      <text x="105.8" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode double</text>
      <text x="123.1" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode nullable int64</text>
      <text x="140.3" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode utf8</text>
      <text x="157.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode nullable utf8</text>
      <text x="174.7" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode mixed 6-col</text>
      <text x="191.9" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode list&lt;int32&gt;</text>
      <text x="209.2" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode struct&lt;int32,double,bool&gt;</text>
      <text x="226.4" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode dictionary&lt;utf8&gt;</text>
      <text x="243.6" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode lazy int64</text>
      <text x="260.8" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode lazy double</text>
      <text x="278.1" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode lazy nullable int64</text>
      <text x="295.3" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode lazy utf8</text>
      <text x="312.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode lazy nullable utf8</text>
      <text x="329.7" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode lazy mixed 6-col</text>
      <text x="346.9" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode lazy list&lt;int32&gt;</text>
      <text x="364.2" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode lazy struct&lt;int32,double,bool&gt;</text>
      <text x="381.4" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode lazy dictionary&lt;utf8&gt;</text>
      <text x="398.6" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode int64</text>
      <text x="415.8" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode double</text>
      <text x="433.1" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode nullable int64</text>
      <text x="450.3" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode utf8</text>
      <text x="467.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode nullable utf8</text>
      <text x="484.7" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode mixed 6-col</text>
      <text x="501.9" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode list&lt;int32&gt;</text>
      <text x="519.2" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode struct&lt;int32,double,bool&gt;</text>
      <text x="536.4" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode dictionary&lt;utf8&gt;</text>
      <text x="553.6" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode + toVector int64</text>
      <text x="570.8" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode + toVector double</text>
      <text x="588.1" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode + toVector nullable int64</text>
      <text x="605.3" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode + toVector utf8</text>
      <text x="622.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode + toVector nullable utf8</text>
      <text x="639.7" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode + toVector mixed 6-col</text>
      <text x="656.9" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode + toVector list&lt;int32&gt;</text>
      <text x="674.2" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode + toVector struct&lt;int32,double,bool&gt;</text>
      <text x="691.4" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode + toVector dictionary&lt;utf8&gt;</text>
    </g>
    <g>
      <g transform="translate(257, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">arrow-rs</text>
      </g>
      <g transform="translate(347, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">wireform-arrow</text>
      </g>
    </g>
  </g>
</svg>


| Operation                                   | arrow-rs | wireform-arrow | ratio |
| :------------------------------------------ | -------: | -------------: | ----: |
| encode int64                                |  38.6 µs |        10.9 µs | 0.28x |
| encode double                               |  38.5 µs |        10.6 µs | 0.28x |
| encode nullable int64                       |  37.7 µs |        10.9 µs | 0.29x |
| encode utf8                                 |  38.9 µs |        13.1 µs | 0.34x |
| encode nullable utf8                        |  38.1 µs |        12.7 µs | 0.33x |
| encode mixed 6-col                          |   486 µs |        61.9 µs | 0.13x |
| encode list<int32>                          |  96.4 µs |        27.5 µs | 0.28x |
| encode struct<int32,double,bool>            |  63.6 µs |        16.8 µs | 0.26x |
| encode dictionary<utf8>                     |  28.6 µs |        6.14 µs | 0.21x |
| encode lazy int64                           |  38.6 µs |        0.64 µs | 0.02x |
| encode lazy double                          |  38.5 µs |        0.62 µs | 0.02x |
| encode lazy nullable int64                  |  37.7 µs |        0.66 µs | 0.02x |
| encode lazy utf8                            |  38.9 µs |        0.68 µs | 0.02x |
| encode lazy nullable utf8                   |  38.1 µs |        0.70 µs | 0.02x |
| encode lazy mixed 6-col                     |   486 µs |        2.06 µs | 0.00x |
| encode lazy list<int32>                     |  96.4 µs |        0.92 µs | 0.01x |
| encode lazy struct<int32,double,bool>       |  63.6 µs |        1.39 µs | 0.02x |
| encode lazy dictionary<utf8>                |  28.6 µs |        1.14 µs | 0.04x |
| decode int64                                |  13.2 µs |        0.69 µs | 0.05x |
| decode double                               |  12.8 µs |        0.67 µs | 0.05x |
| decode nullable int64                       |  13.1 µs |        0.96 µs | 0.07x |
| decode utf8                                 |   122 µs |        47.1 µs | 0.38x |
| decode nullable utf8                        |   125 µs |        47.0 µs | 0.38x |
| decode mixed 6-col                          |   296 µs |        95.3 µs | 0.32x |
| decode list<int32>                          |   108 µs |        6.24 µs | 0.06x |
| decode struct<int32,double,bool>            |  20.4 µs |        1.37 µs | 0.07x |
| decode dictionary<utf8>                     |  36.6 µs |        20.4 µs | 0.56x |
| decode + toVector int64                     |   155 µs |        15.5 µs | 0.10x |
| decode + toVector double                    |   167 µs |        15.2 µs | 0.09x |
| decode + toVector nullable int64            |   157 µs |        14.9 µs | 0.10x |
| decode + toVector utf8                      |  1422 µs |         242 µs | 0.17x |
| decode + toVector nullable utf8             |  1455 µs |         259 µs | 0.18x |
| decode + toVector mixed 6-col               |  3462 µs |         556 µs | 0.16x |
| decode + toVector list<int32>               |  3669 µs |         190 µs | 0.05x |
| decode + toVector struct<int32,double,bool> |   326 µs |        26.1 µs | 0.08x |
| decode + toVector dictionary<utf8>          |  1387 µs |         433 µs | 0.31x |

<sub>Last run 2026-10-09 07:00:11 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5; arrow-rs 58.2.0, rustc 1.98.1, criterion.rs 0.7.0.</sub>
<!-- END_AUTOGEN bench:arrow-encode-decode -->

### Encode/decode, 100-row batch

<!-- BEGIN_AUTOGEN bench:arrow-encode-decode-small -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-arrow vs arrow-rs, IPC stream encode + decode (100-row batch)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-arrow vs arrow-rs, IPC stream encode + decode (100-row batch)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5; arrow-rs 58.2.0, rustc 1.98.1, criterion.rs 0.7.0</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">2.50</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">5.00</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">7.50</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">10.0</text>
    </g>
    <g>
      <rect x="81.7" y="294.8" width="4.9" height="25.2" rx="2" fill="#0969da"/>
      <rect x="88.6" y="303.4" width="4.9" height="16.6" rx="2" fill="#cf222e"/>
      <rect x="98.9" y="294.5" width="4.9" height="25.5" rx="2" fill="#0969da"/>
      <rect x="105.8" y="303.6" width="4.9" height="16.4" rx="2" fill="#cf222e"/>
      <rect x="116.2" y="295.0" width="4.9" height="25.0" rx="2" fill="#0969da"/>
      <rect x="123.1" y="303.1" width="4.9" height="16.9" rx="2" fill="#cf222e"/>
      <rect x="133.4" y="292.7" width="4.9" height="27.3" rx="2" fill="#0969da"/>
      <rect x="140.3" y="302.3" width="4.9" height="17.7" rx="2" fill="#cf222e"/>
      <rect x="150.6" y="292.4" width="4.9" height="27.6" rx="2" fill="#0969da"/>
      <rect x="157.5" y="302.8" width="4.9" height="17.2" rx="2" fill="#cf222e"/>
      <rect x="167.8" y="253.4" width="4.9" height="66.6" rx="2" fill="#0969da"/>
      <rect x="174.7" y="263.3" width="4.9" height="56.7" rx="2" fill="#cf222e"/>
      <rect x="185.1" y="283.3" width="4.9" height="36.7" rx="2" fill="#0969da"/>
      <rect x="191.9" y="295.6" width="4.9" height="24.4" rx="2" fill="#cf222e"/>
      <rect x="202.3" y="270.1" width="4.9" height="49.9" rx="2" fill="#0969da"/>
      <rect x="209.2" y="283.9" width="4.9" height="36.1" rx="2" fill="#cf222e"/>
      <rect x="219.5" y="263.6" width="4.9" height="56.4" rx="2" fill="#0969da"/>
      <rect x="226.4" y="291.7" width="4.9" height="28.3" rx="2" fill="#cf222e"/>
      <rect x="236.7" y="294.8" width="4.9" height="25.2" rx="2" fill="#0969da"/>
      <rect x="243.6" y="303.4" width="4.9" height="16.6" rx="2" fill="#cf222e"/>
      <rect x="253.9" y="294.5" width="4.9" height="25.5" rx="2" fill="#0969da"/>
      <rect x="260.8" y="303.9" width="4.9" height="16.1" rx="2" fill="#cf222e"/>
      <rect x="271.2" y="295.0" width="4.9" height="25.0" rx="2" fill="#0969da"/>
      <rect x="278.1" y="303.1" width="4.9" height="16.9" rx="2" fill="#cf222e"/>
      <rect x="288.4" y="292.7" width="4.9" height="27.3" rx="2" fill="#0969da"/>
      <rect x="295.3" y="302.1" width="4.9" height="17.9" rx="2" fill="#cf222e"/>
      <rect x="305.6" y="292.4" width="4.9" height="27.6" rx="2" fill="#0969da"/>
      <rect x="312.5" y="301.3" width="4.9" height="18.7" rx="2" fill="#cf222e"/>
      <rect x="322.8" y="253.4" width="4.9" height="66.6" rx="2" fill="#0969da"/>
      <rect x="329.7" y="266.4" width="4.9" height="53.6" rx="2" fill="#cf222e"/>
      <rect x="340.1" y="283.3" width="4.9" height="36.7" rx="2" fill="#0969da"/>
      <rect x="346.9" y="296.3" width="4.9" height="23.7" rx="2" fill="#cf222e"/>
      <rect x="357.3" y="270.1" width="4.9" height="49.9" rx="2" fill="#0969da"/>
      <rect x="364.2" y="284.1" width="4.9" height="35.9" rx="2" fill="#cf222e"/>
      <rect x="374.5" y="263.6" width="4.9" height="56.4" rx="2" fill="#0969da"/>
      <rect x="381.4" y="290.4" width="4.9" height="29.6" rx="2" fill="#cf222e"/>
      <rect x="391.7" y="303.9" width="4.9" height="16.1" rx="2" fill="#0969da"/>
      <rect x="398.6" y="302.1" width="4.9" height="17.9" rx="2" fill="#cf222e"/>
      <rect x="408.9" y="304.1" width="4.9" height="15.9" rx="2" fill="#0969da"/>
      <rect x="415.8" y="302.6" width="4.9" height="17.4" rx="2" fill="#cf222e"/>
      <rect x="426.2" y="303.1" width="4.9" height="16.9" rx="2" fill="#0969da"/>
      <rect x="433.1" y="301.5" width="4.9" height="18.5" rx="2" fill="#cf222e"/>
      <rect x="443.4" y="301.0" width="4.9" height="19.0" rx="2" fill="#0969da"/>
      <rect x="450.3" y="300.0" width="4.9" height="20.0" rx="2" fill="#cf222e"/>
      <rect x="460.6" y="300.2" width="4.9" height="19.8" rx="2" fill="#0969da"/>
      <rect x="467.5" y="299.7" width="4.9" height="20.3" rx="2" fill="#cf222e"/>
      <rect x="477.8" y="262.3" width="4.9" height="57.7" rx="2" fill="#0969da"/>
      <rect x="484.7" y="268.5" width="4.9" height="51.5" rx="2" fill="#cf222e"/>
      <rect x="495.1" y="292.4" width="4.9" height="27.6" rx="2" fill="#0969da"/>
      <rect x="501.9" y="295.3" width="4.9" height="24.7" rx="2" fill="#cf222e"/>
      <rect x="512.3" y="287.0" width="4.9" height="33.0" rx="2" fill="#0969da"/>
      <rect x="519.2" y="284.1" width="4.9" height="35.9" rx="2" fill="#cf222e"/>
      <rect x="529.5" y="285.2" width="4.9" height="34.8" rx="2" fill="#0969da"/>
      <rect x="536.4" y="287.8" width="4.9" height="32.2" rx="2" fill="#cf222e"/>
      <rect x="546.7" y="301.3" width="4.9" height="18.7" rx="2" fill="#0969da"/>
      <rect x="553.6" y="299.5" width="4.9" height="20.5" rx="2" fill="#cf222e"/>
      <rect x="563.9" y="301.5" width="4.9" height="18.5" rx="2" fill="#0969da"/>
      <rect x="570.8" y="299.7" width="4.9" height="20.3" rx="2" fill="#cf222e"/>
      <rect x="581.2" y="299.7" width="4.9" height="20.3" rx="2" fill="#0969da"/>
      <rect x="588.1" y="299.2" width="4.9" height="20.8" rx="2" fill="#cf222e"/>
      <rect x="598.4" y="270.6" width="4.9" height="49.4" rx="2" fill="#0969da"/>
      <rect x="605.3" y="287.8" width="4.9" height="32.2" rx="2" fill="#cf222e"/>
      <rect x="615.6" y="269.8" width="4.9" height="50.2" rx="2" fill="#0969da"/>
      <rect x="622.5" y="287.8" width="4.9" height="32.2" rx="2" fill="#cf222e"/>
      <rect x="632.8" y="188.2" width="4.9" height="131.8" rx="2" fill="#0969da"/>
      <rect x="639.7" y="235.5" width="4.9" height="84.5" rx="2" fill="#cf222e"/>
      <rect x="650.1" y="200.1" width="4.9" height="119.9" rx="2" fill="#0969da"/>
      <rect x="656.9" y="279.2" width="4.9" height="40.8" rx="2" fill="#cf222e"/>
      <rect x="667.3" y="280.0" width="4.9" height="40.0" rx="2" fill="#0969da"/>
      <rect x="674.2" y="276.3" width="4.9" height="43.7" rx="2" fill="#cf222e"/>
      <rect x="684.5" y="252.9" width="4.9" height="67.1" rx="2" fill="#0969da"/>
      <rect x="691.4" y="268.5" width="4.9" height="51.5" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="84.2" y="290.8" text-anchor="middle" font-size="10" fill="#1f2328">0.970</text>
      <text x="91.1" y="299.4" text-anchor="middle" font-size="10" fill="#1f2328">0.640</text>
      <text x="101.4" y="290.5" text-anchor="middle" font-size="10" fill="#1f2328">0.980</text>
      <text x="108.3" y="299.6" text-anchor="middle" font-size="10" fill="#1f2328">0.630</text>
      <text x="118.6" y="291.0" text-anchor="middle" font-size="10" fill="#1f2328">0.960</text>
      <text x="125.5" y="299.1" text-anchor="middle" font-size="10" fill="#1f2328">0.650</text>
      <text x="135.8" y="288.7" text-anchor="middle" font-size="10" fill="#1f2328">1.05</text>
      <text x="142.7" y="298.3" text-anchor="middle" font-size="10" fill="#1f2328">0.680</text>
      <text x="153.1" y="288.4" text-anchor="middle" font-size="10" fill="#1f2328">1.06</text>
      <text x="159.9" y="298.8" text-anchor="middle" font-size="10" fill="#1f2328">0.660</text>
      <text x="170.3" y="249.4" text-anchor="middle" font-size="10" fill="#1f2328">2.56</text>
      <text x="177.2" y="259.3" text-anchor="middle" font-size="10" fill="#1f2328">2.18</text>
      <text x="187.5" y="279.3" text-anchor="middle" font-size="10" fill="#1f2328">1.41</text>
      <text x="194.4" y="291.6" text-anchor="middle" font-size="10" fill="#1f2328">0.940</text>
      <text x="204.7" y="266.1" text-anchor="middle" font-size="10" fill="#1f2328">1.92</text>
      <text x="211.6" y="279.9" text-anchor="middle" font-size="10" fill="#1f2328">1.39</text>
      <text x="221.9" y="259.6" text-anchor="middle" font-size="10" fill="#1f2328">2.17</text>
      <text x="228.8" y="287.7" text-anchor="middle" font-size="10" fill="#1f2328">1.09</text>
      <text x="239.2" y="290.8" text-anchor="middle" font-size="10" fill="#1f2328">0.970</text>
      <text x="246.1" y="299.4" text-anchor="middle" font-size="10" fill="#1f2328">0.640</text>
      <text x="256.4" y="290.5" text-anchor="middle" font-size="10" fill="#1f2328">0.980</text>
      <text x="263.3" y="299.9" text-anchor="middle" font-size="10" fill="#1f2328">0.620</text>
      <text x="273.6" y="291.0" text-anchor="middle" font-size="10" fill="#1f2328">0.960</text>
      <text x="280.5" y="299.1" text-anchor="middle" font-size="10" fill="#1f2328">0.650</text>
      <text x="290.8" y="288.7" text-anchor="middle" font-size="10" fill="#1f2328">1.05</text>
      <text x="297.7" y="298.1" text-anchor="middle" font-size="10" fill="#1f2328">0.690</text>
      <text x="308.1" y="288.4" text-anchor="middle" font-size="10" fill="#1f2328">1.06</text>
      <text x="314.9" y="297.3" text-anchor="middle" font-size="10" fill="#1f2328">0.720</text>
      <text x="325.3" y="249.4" text-anchor="middle" font-size="10" fill="#1f2328">2.56</text>
      <text x="332.2" y="262.4" text-anchor="middle" font-size="10" fill="#1f2328">2.06</text>
      <text x="342.5" y="279.3" text-anchor="middle" font-size="10" fill="#1f2328">1.41</text>
      <text x="349.4" y="292.3" text-anchor="middle" font-size="10" fill="#1f2328">0.910</text>
      <text x="359.7" y="266.1" text-anchor="middle" font-size="10" fill="#1f2328">1.92</text>
      <text x="366.6" y="280.1" text-anchor="middle" font-size="10" fill="#1f2328">1.38</text>
      <text x="376.9" y="259.6" text-anchor="middle" font-size="10" fill="#1f2328">2.17</text>
      <text x="383.8" y="286.4" text-anchor="middle" font-size="10" fill="#1f2328">1.14</text>
      <text x="394.2" y="299.9" text-anchor="middle" font-size="10" fill="#1f2328">0.620</text>
      <text x="401.1" y="298.1" text-anchor="middle" font-size="10" fill="#1f2328">0.690</text>
      <text x="411.4" y="300.1" text-anchor="middle" font-size="10" fill="#1f2328">0.610</text>
      <text x="418.3" y="298.6" text-anchor="middle" font-size="10" fill="#1f2328">0.670</text>
      <text x="428.6" y="299.1" text-anchor="middle" font-size="10" fill="#1f2328">0.650</text>
      <text x="435.5" y="297.5" text-anchor="middle" font-size="10" fill="#1f2328">0.710</text>
      <text x="445.8" y="297.0" text-anchor="middle" font-size="10" fill="#1f2328">0.730</text>
      <text x="452.7" y="296.0" text-anchor="middle" font-size="10" fill="#1f2328">0.770</text>
      <text x="463.1" y="296.2" text-anchor="middle" font-size="10" fill="#1f2328">0.760</text>
      <text x="469.9" y="295.7" text-anchor="middle" font-size="10" fill="#1f2328">0.780</text>
      <text x="480.3" y="258.3" text-anchor="middle" font-size="10" fill="#1f2328">2.22</text>
      <text x="487.2" y="264.5" text-anchor="middle" font-size="10" fill="#1f2328">1.98</text>
      <text x="497.5" y="288.4" text-anchor="middle" font-size="10" fill="#1f2328">1.06</text>
      <text x="504.4" y="291.3" text-anchor="middle" font-size="10" fill="#1f2328">0.950</text>
      <text x="514.7" y="283.0" text-anchor="middle" font-size="10" fill="#1f2328">1.27</text>
      <text x="521.6" y="280.1" text-anchor="middle" font-size="10" fill="#1f2328">1.38</text>
      <text x="531.9" y="281.2" text-anchor="middle" font-size="10" fill="#1f2328">1.34</text>
      <text x="538.8" y="283.8" text-anchor="middle" font-size="10" fill="#1f2328">1.24</text>
      <text x="549.2" y="297.3" text-anchor="middle" font-size="10" fill="#1f2328">0.720</text>
      <text x="556.1" y="295.5" text-anchor="middle" font-size="10" fill="#1f2328">0.790</text>
      <text x="566.4" y="297.5" text-anchor="middle" font-size="10" fill="#1f2328">0.710</text>
      <text x="573.3" y="295.7" text-anchor="middle" font-size="10" fill="#1f2328">0.780</text>
      <text x="583.6" y="295.7" text-anchor="middle" font-size="10" fill="#1f2328">0.780</text>
      <text x="590.5" y="295.2" text-anchor="middle" font-size="10" fill="#1f2328">0.800</text>
      <text x="600.8" y="266.6" text-anchor="middle" font-size="10" fill="#1f2328">1.90</text>
      <text x="607.7" y="283.8" text-anchor="middle" font-size="10" fill="#1f2328">1.24</text>
      <text x="618.1" y="265.8" text-anchor="middle" font-size="10" fill="#1f2328">1.93</text>
      <text x="624.9" y="283.8" text-anchor="middle" font-size="10" fill="#1f2328">1.24</text>
      <text x="635.3" y="184.2" text-anchor="middle" font-size="10" fill="#1f2328">5.07</text>
      <text x="642.2" y="231.5" text-anchor="middle" font-size="10" fill="#1f2328">3.25</text>
      <text x="652.5" y="196.1" text-anchor="middle" font-size="10" fill="#1f2328">4.61</text>
      <text x="659.4" y="275.2" text-anchor="middle" font-size="10" fill="#1f2328">1.57</text>
      <text x="669.7" y="276.0" text-anchor="middle" font-size="10" fill="#1f2328">1.54</text>
      <text x="676.6" y="272.3" text-anchor="middle" font-size="10" fill="#1f2328">1.68</text>
      <text x="686.9" y="248.9" text-anchor="middle" font-size="10" fill="#1f2328">2.58</text>
      <text x="693.8" y="264.5" text-anchor="middle" font-size="10" fill="#1f2328">1.98</text>
    </g>
    <g>
      <text x="88.6" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode int64</text>
      <text x="105.8" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode double</text>
      <text x="123.1" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode nullable int64</text>
      <text x="140.3" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode utf8</text>
      <text x="157.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode nullable utf8</text>
      <text x="174.7" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode mixed 6-col</text>
      <text x="191.9" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode list&lt;int32&gt;</text>
      <text x="209.2" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode struct&lt;int32,double,bool&gt;</text>
      <text x="226.4" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode dictionary&lt;utf8&gt;</text>
      <text x="243.6" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode lazy int64</text>
      <text x="260.8" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode lazy double</text>
      <text x="278.1" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode lazy nullable int64</text>
      <text x="295.3" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode lazy utf8</text>
      <text x="312.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode lazy nullable utf8</text>
      <text x="329.7" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode lazy mixed 6-col</text>
      <text x="346.9" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode lazy list&lt;int32&gt;</text>
      <text x="364.2" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode lazy struct&lt;int32,double,bool&gt;</text>
      <text x="381.4" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode lazy dictionary&lt;utf8&gt;</text>
      <text x="398.6" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode int64</text>
      <text x="415.8" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode double</text>
      <text x="433.1" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode nullable int64</text>
      <text x="450.3" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode utf8</text>
      <text x="467.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode nullable utf8</text>
      <text x="484.7" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode mixed 6-col</text>
      <text x="501.9" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode list&lt;int32&gt;</text>
      <text x="519.2" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode struct&lt;int32,double,bool&gt;</text>
      <text x="536.4" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode dictionary&lt;utf8&gt;</text>
      <text x="553.6" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode + toVector int64</text>
      <text x="570.8" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode + toVector double</text>
      <text x="588.1" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode + toVector nullable int64</text>
      <text x="605.3" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode + toVector utf8</text>
      <text x="622.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode + toVector nullable utf8</text>
      <text x="639.7" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode + toVector mixed 6-col</text>
      <text x="656.9" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode + toVector list&lt;int32&gt;</text>
      <text x="674.2" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode + toVector struct&lt;int32,double,bool&gt;</text>
      <text x="691.4" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode + toVector dictionary&lt;utf8&gt;</text>
    </g>
    <g>
      <g transform="translate(257, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">arrow-rs</text>
      </g>
      <g transform="translate(347, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">wireform-arrow</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-arrow vs arrow-rs, IPC stream encode + decode (100-row batch)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5; arrow-rs 58.2.0, rustc 1.98.1, criterion.rs 0.7.0</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">2.50</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">5.00</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">7.50</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">10.0</text>
    </g>
    <g>
      <rect x="81.7" y="294.8" width="4.9" height="25.2" rx="2" fill="#58a6ff"/>
      <rect x="88.6" y="303.4" width="4.9" height="16.6" rx="2" fill="#ff7b72"/>
      <rect x="98.9" y="294.5" width="4.9" height="25.5" rx="2" fill="#58a6ff"/>
      <rect x="105.8" y="303.6" width="4.9" height="16.4" rx="2" fill="#ff7b72"/>
      <rect x="116.2" y="295.0" width="4.9" height="25.0" rx="2" fill="#58a6ff"/>
      <rect x="123.1" y="303.1" width="4.9" height="16.9" rx="2" fill="#ff7b72"/>
      <rect x="133.4" y="292.7" width="4.9" height="27.3" rx="2" fill="#58a6ff"/>
      <rect x="140.3" y="302.3" width="4.9" height="17.7" rx="2" fill="#ff7b72"/>
      <rect x="150.6" y="292.4" width="4.9" height="27.6" rx="2" fill="#58a6ff"/>
      <rect x="157.5" y="302.8" width="4.9" height="17.2" rx="2" fill="#ff7b72"/>
      <rect x="167.8" y="253.4" width="4.9" height="66.6" rx="2" fill="#58a6ff"/>
      <rect x="174.7" y="263.3" width="4.9" height="56.7" rx="2" fill="#ff7b72"/>
      <rect x="185.1" y="283.3" width="4.9" height="36.7" rx="2" fill="#58a6ff"/>
      <rect x="191.9" y="295.6" width="4.9" height="24.4" rx="2" fill="#ff7b72"/>
      <rect x="202.3" y="270.1" width="4.9" height="49.9" rx="2" fill="#58a6ff"/>
      <rect x="209.2" y="283.9" width="4.9" height="36.1" rx="2" fill="#ff7b72"/>
      <rect x="219.5" y="263.6" width="4.9" height="56.4" rx="2" fill="#58a6ff"/>
      <rect x="226.4" y="291.7" width="4.9" height="28.3" rx="2" fill="#ff7b72"/>
      <rect x="236.7" y="294.8" width="4.9" height="25.2" rx="2" fill="#58a6ff"/>
      <rect x="243.6" y="303.4" width="4.9" height="16.6" rx="2" fill="#ff7b72"/>
      <rect x="253.9" y="294.5" width="4.9" height="25.5" rx="2" fill="#58a6ff"/>
      <rect x="260.8" y="303.9" width="4.9" height="16.1" rx="2" fill="#ff7b72"/>
      <rect x="271.2" y="295.0" width="4.9" height="25.0" rx="2" fill="#58a6ff"/>
      <rect x="278.1" y="303.1" width="4.9" height="16.9" rx="2" fill="#ff7b72"/>
      <rect x="288.4" y="292.7" width="4.9" height="27.3" rx="2" fill="#58a6ff"/>
      <rect x="295.3" y="302.1" width="4.9" height="17.9" rx="2" fill="#ff7b72"/>
      <rect x="305.6" y="292.4" width="4.9" height="27.6" rx="2" fill="#58a6ff"/>
      <rect x="312.5" y="301.3" width="4.9" height="18.7" rx="2" fill="#ff7b72"/>
      <rect x="322.8" y="253.4" width="4.9" height="66.6" rx="2" fill="#58a6ff"/>
      <rect x="329.7" y="266.4" width="4.9" height="53.6" rx="2" fill="#ff7b72"/>
      <rect x="340.1" y="283.3" width="4.9" height="36.7" rx="2" fill="#58a6ff"/>
      <rect x="346.9" y="296.3" width="4.9" height="23.7" rx="2" fill="#ff7b72"/>
      <rect x="357.3" y="270.1" width="4.9" height="49.9" rx="2" fill="#58a6ff"/>
      <rect x="364.2" y="284.1" width="4.9" height="35.9" rx="2" fill="#ff7b72"/>
      <rect x="374.5" y="263.6" width="4.9" height="56.4" rx="2" fill="#58a6ff"/>
      <rect x="381.4" y="290.4" width="4.9" height="29.6" rx="2" fill="#ff7b72"/>
      <rect x="391.7" y="303.9" width="4.9" height="16.1" rx="2" fill="#58a6ff"/>
      <rect x="398.6" y="302.1" width="4.9" height="17.9" rx="2" fill="#ff7b72"/>
      <rect x="408.9" y="304.1" width="4.9" height="15.9" rx="2" fill="#58a6ff"/>
      <rect x="415.8" y="302.6" width="4.9" height="17.4" rx="2" fill="#ff7b72"/>
      <rect x="426.2" y="303.1" width="4.9" height="16.9" rx="2" fill="#58a6ff"/>
      <rect x="433.1" y="301.5" width="4.9" height="18.5" rx="2" fill="#ff7b72"/>
      <rect x="443.4" y="301.0" width="4.9" height="19.0" rx="2" fill="#58a6ff"/>
      <rect x="450.3" y="300.0" width="4.9" height="20.0" rx="2" fill="#ff7b72"/>
      <rect x="460.6" y="300.2" width="4.9" height="19.8" rx="2" fill="#58a6ff"/>
      <rect x="467.5" y="299.7" width="4.9" height="20.3" rx="2" fill="#ff7b72"/>
      <rect x="477.8" y="262.3" width="4.9" height="57.7" rx="2" fill="#58a6ff"/>
      <rect x="484.7" y="268.5" width="4.9" height="51.5" rx="2" fill="#ff7b72"/>
      <rect x="495.1" y="292.4" width="4.9" height="27.6" rx="2" fill="#58a6ff"/>
      <rect x="501.9" y="295.3" width="4.9" height="24.7" rx="2" fill="#ff7b72"/>
      <rect x="512.3" y="287.0" width="4.9" height="33.0" rx="2" fill="#58a6ff"/>
      <rect x="519.2" y="284.1" width="4.9" height="35.9" rx="2" fill="#ff7b72"/>
      <rect x="529.5" y="285.2" width="4.9" height="34.8" rx="2" fill="#58a6ff"/>
      <rect x="536.4" y="287.8" width="4.9" height="32.2" rx="2" fill="#ff7b72"/>
      <rect x="546.7" y="301.3" width="4.9" height="18.7" rx="2" fill="#58a6ff"/>
      <rect x="553.6" y="299.5" width="4.9" height="20.5" rx="2" fill="#ff7b72"/>
      <rect x="563.9" y="301.5" width="4.9" height="18.5" rx="2" fill="#58a6ff"/>
      <rect x="570.8" y="299.7" width="4.9" height="20.3" rx="2" fill="#ff7b72"/>
      <rect x="581.2" y="299.7" width="4.9" height="20.3" rx="2" fill="#58a6ff"/>
      <rect x="588.1" y="299.2" width="4.9" height="20.8" rx="2" fill="#ff7b72"/>
      <rect x="598.4" y="270.6" width="4.9" height="49.4" rx="2" fill="#58a6ff"/>
      <rect x="605.3" y="287.8" width="4.9" height="32.2" rx="2" fill="#ff7b72"/>
      <rect x="615.6" y="269.8" width="4.9" height="50.2" rx="2" fill="#58a6ff"/>
      <rect x="622.5" y="287.8" width="4.9" height="32.2" rx="2" fill="#ff7b72"/>
      <rect x="632.8" y="188.2" width="4.9" height="131.8" rx="2" fill="#58a6ff"/>
      <rect x="639.7" y="235.5" width="4.9" height="84.5" rx="2" fill="#ff7b72"/>
      <rect x="650.1" y="200.1" width="4.9" height="119.9" rx="2" fill="#58a6ff"/>
      <rect x="656.9" y="279.2" width="4.9" height="40.8" rx="2" fill="#ff7b72"/>
      <rect x="667.3" y="280.0" width="4.9" height="40.0" rx="2" fill="#58a6ff"/>
      <rect x="674.2" y="276.3" width="4.9" height="43.7" rx="2" fill="#ff7b72"/>
      <rect x="684.5" y="252.9" width="4.9" height="67.1" rx="2" fill="#58a6ff"/>
      <rect x="691.4" y="268.5" width="4.9" height="51.5" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="84.2" y="290.8" text-anchor="middle" font-size="10" fill="#e6edf3">0.970</text>
      <text x="91.1" y="299.4" text-anchor="middle" font-size="10" fill="#e6edf3">0.640</text>
      <text x="101.4" y="290.5" text-anchor="middle" font-size="10" fill="#e6edf3">0.980</text>
      <text x="108.3" y="299.6" text-anchor="middle" font-size="10" fill="#e6edf3">0.630</text>
      <text x="118.6" y="291.0" text-anchor="middle" font-size="10" fill="#e6edf3">0.960</text>
      <text x="125.5" y="299.1" text-anchor="middle" font-size="10" fill="#e6edf3">0.650</text>
      <text x="135.8" y="288.7" text-anchor="middle" font-size="10" fill="#e6edf3">1.05</text>
      <text x="142.7" y="298.3" text-anchor="middle" font-size="10" fill="#e6edf3">0.680</text>
      <text x="153.1" y="288.4" text-anchor="middle" font-size="10" fill="#e6edf3">1.06</text>
      <text x="159.9" y="298.8" text-anchor="middle" font-size="10" fill="#e6edf3">0.660</text>
      <text x="170.3" y="249.4" text-anchor="middle" font-size="10" fill="#e6edf3">2.56</text>
      <text x="177.2" y="259.3" text-anchor="middle" font-size="10" fill="#e6edf3">2.18</text>
      <text x="187.5" y="279.3" text-anchor="middle" font-size="10" fill="#e6edf3">1.41</text>
      <text x="194.4" y="291.6" text-anchor="middle" font-size="10" fill="#e6edf3">0.940</text>
      <text x="204.7" y="266.1" text-anchor="middle" font-size="10" fill="#e6edf3">1.92</text>
      <text x="211.6" y="279.9" text-anchor="middle" font-size="10" fill="#e6edf3">1.39</text>
      <text x="221.9" y="259.6" text-anchor="middle" font-size="10" fill="#e6edf3">2.17</text>
      <text x="228.8" y="287.7" text-anchor="middle" font-size="10" fill="#e6edf3">1.09</text>
      <text x="239.2" y="290.8" text-anchor="middle" font-size="10" fill="#e6edf3">0.970</text>
      <text x="246.1" y="299.4" text-anchor="middle" font-size="10" fill="#e6edf3">0.640</text>
      <text x="256.4" y="290.5" text-anchor="middle" font-size="10" fill="#e6edf3">0.980</text>
      <text x="263.3" y="299.9" text-anchor="middle" font-size="10" fill="#e6edf3">0.620</text>
      <text x="273.6" y="291.0" text-anchor="middle" font-size="10" fill="#e6edf3">0.960</text>
      <text x="280.5" y="299.1" text-anchor="middle" font-size="10" fill="#e6edf3">0.650</text>
      <text x="290.8" y="288.7" text-anchor="middle" font-size="10" fill="#e6edf3">1.05</text>
      <text x="297.7" y="298.1" text-anchor="middle" font-size="10" fill="#e6edf3">0.690</text>
      <text x="308.1" y="288.4" text-anchor="middle" font-size="10" fill="#e6edf3">1.06</text>
      <text x="314.9" y="297.3" text-anchor="middle" font-size="10" fill="#e6edf3">0.720</text>
      <text x="325.3" y="249.4" text-anchor="middle" font-size="10" fill="#e6edf3">2.56</text>
      <text x="332.2" y="262.4" text-anchor="middle" font-size="10" fill="#e6edf3">2.06</text>
      <text x="342.5" y="279.3" text-anchor="middle" font-size="10" fill="#e6edf3">1.41</text>
      <text x="349.4" y="292.3" text-anchor="middle" font-size="10" fill="#e6edf3">0.910</text>
      <text x="359.7" y="266.1" text-anchor="middle" font-size="10" fill="#e6edf3">1.92</text>
      <text x="366.6" y="280.1" text-anchor="middle" font-size="10" fill="#e6edf3">1.38</text>
      <text x="376.9" y="259.6" text-anchor="middle" font-size="10" fill="#e6edf3">2.17</text>
      <text x="383.8" y="286.4" text-anchor="middle" font-size="10" fill="#e6edf3">1.14</text>
      <text x="394.2" y="299.9" text-anchor="middle" font-size="10" fill="#e6edf3">0.620</text>
      <text x="401.1" y="298.1" text-anchor="middle" font-size="10" fill="#e6edf3">0.690</text>
      <text x="411.4" y="300.1" text-anchor="middle" font-size="10" fill="#e6edf3">0.610</text>
      <text x="418.3" y="298.6" text-anchor="middle" font-size="10" fill="#e6edf3">0.670</text>
      <text x="428.6" y="299.1" text-anchor="middle" font-size="10" fill="#e6edf3">0.650</text>
      <text x="435.5" y="297.5" text-anchor="middle" font-size="10" fill="#e6edf3">0.710</text>
      <text x="445.8" y="297.0" text-anchor="middle" font-size="10" fill="#e6edf3">0.730</text>
      <text x="452.7" y="296.0" text-anchor="middle" font-size="10" fill="#e6edf3">0.770</text>
      <text x="463.1" y="296.2" text-anchor="middle" font-size="10" fill="#e6edf3">0.760</text>
      <text x="469.9" y="295.7" text-anchor="middle" font-size="10" fill="#e6edf3">0.780</text>
      <text x="480.3" y="258.3" text-anchor="middle" font-size="10" fill="#e6edf3">2.22</text>
      <text x="487.2" y="264.5" text-anchor="middle" font-size="10" fill="#e6edf3">1.98</text>
      <text x="497.5" y="288.4" text-anchor="middle" font-size="10" fill="#e6edf3">1.06</text>
      <text x="504.4" y="291.3" text-anchor="middle" font-size="10" fill="#e6edf3">0.950</text>
      <text x="514.7" y="283.0" text-anchor="middle" font-size="10" fill="#e6edf3">1.27</text>
      <text x="521.6" y="280.1" text-anchor="middle" font-size="10" fill="#e6edf3">1.38</text>
      <text x="531.9" y="281.2" text-anchor="middle" font-size="10" fill="#e6edf3">1.34</text>
      <text x="538.8" y="283.8" text-anchor="middle" font-size="10" fill="#e6edf3">1.24</text>
      <text x="549.2" y="297.3" text-anchor="middle" font-size="10" fill="#e6edf3">0.720</text>
      <text x="556.1" y="295.5" text-anchor="middle" font-size="10" fill="#e6edf3">0.790</text>
      <text x="566.4" y="297.5" text-anchor="middle" font-size="10" fill="#e6edf3">0.710</text>
      <text x="573.3" y="295.7" text-anchor="middle" font-size="10" fill="#e6edf3">0.780</text>
      <text x="583.6" y="295.7" text-anchor="middle" font-size="10" fill="#e6edf3">0.780</text>
      <text x="590.5" y="295.2" text-anchor="middle" font-size="10" fill="#e6edf3">0.800</text>
      <text x="600.8" y="266.6" text-anchor="middle" font-size="10" fill="#e6edf3">1.90</text>
      <text x="607.7" y="283.8" text-anchor="middle" font-size="10" fill="#e6edf3">1.24</text>
      <text x="618.1" y="265.8" text-anchor="middle" font-size="10" fill="#e6edf3">1.93</text>
      <text x="624.9" y="283.8" text-anchor="middle" font-size="10" fill="#e6edf3">1.24</text>
      <text x="635.3" y="184.2" text-anchor="middle" font-size="10" fill="#e6edf3">5.07</text>
      <text x="642.2" y="231.5" text-anchor="middle" font-size="10" fill="#e6edf3">3.25</text>
      <text x="652.5" y="196.1" text-anchor="middle" font-size="10" fill="#e6edf3">4.61</text>
      <text x="659.4" y="275.2" text-anchor="middle" font-size="10" fill="#e6edf3">1.57</text>
      <text x="669.7" y="276.0" text-anchor="middle" font-size="10" fill="#e6edf3">1.54</text>
      <text x="676.6" y="272.3" text-anchor="middle" font-size="10" fill="#e6edf3">1.68</text>
      <text x="686.9" y="248.9" text-anchor="middle" font-size="10" fill="#e6edf3">2.58</text>
      <text x="693.8" y="264.5" text-anchor="middle" font-size="10" fill="#e6edf3">1.98</text>
    </g>
    <g>
      <text x="88.6" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode int64</text>
      <text x="105.8" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode double</text>
      <text x="123.1" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode nullable int64</text>
      <text x="140.3" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode utf8</text>
      <text x="157.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode nullable utf8</text>
      <text x="174.7" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode mixed 6-col</text>
      <text x="191.9" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode list&lt;int32&gt;</text>
      <text x="209.2" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode struct&lt;int32,double,bool&gt;</text>
      <text x="226.4" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode dictionary&lt;utf8&gt;</text>
      <text x="243.6" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode lazy int64</text>
      <text x="260.8" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode lazy double</text>
      <text x="278.1" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode lazy nullable int64</text>
      <text x="295.3" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode lazy utf8</text>
      <text x="312.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode lazy nullable utf8</text>
      <text x="329.7" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode lazy mixed 6-col</text>
      <text x="346.9" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode lazy list&lt;int32&gt;</text>
      <text x="364.2" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode lazy struct&lt;int32,double,bool&gt;</text>
      <text x="381.4" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode lazy dictionary&lt;utf8&gt;</text>
      <text x="398.6" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode int64</text>
      <text x="415.8" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode double</text>
      <text x="433.1" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode nullable int64</text>
      <text x="450.3" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode utf8</text>
      <text x="467.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode nullable utf8</text>
      <text x="484.7" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode mixed 6-col</text>
      <text x="501.9" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode list&lt;int32&gt;</text>
      <text x="519.2" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode struct&lt;int32,double,bool&gt;</text>
      <text x="536.4" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode dictionary&lt;utf8&gt;</text>
      <text x="553.6" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode + toVector int64</text>
      <text x="570.8" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode + toVector double</text>
      <text x="588.1" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode + toVector nullable int64</text>
      <text x="605.3" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode + toVector utf8</text>
      <text x="622.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode + toVector nullable utf8</text>
      <text x="639.7" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode + toVector mixed 6-col</text>
      <text x="656.9" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode + toVector list&lt;int32&gt;</text>
      <text x="674.2" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode + toVector struct&lt;int32,double,bool&gt;</text>
      <text x="691.4" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode + toVector dictionary&lt;utf8&gt;</text>
    </g>
    <g>
      <g transform="translate(257, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">arrow-rs</text>
      </g>
      <g transform="translate(347, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">wireform-arrow</text>
      </g>
    </g>
  </g>
</svg>


| Operation                                   | arrow-rs | wireform-arrow | ratio |
| :------------------------------------------ | -------: | -------------: | ----: |
| encode int64                                |  0.97 µs |        0.64 µs | 0.66x |
| encode double                               |  0.98 µs |        0.63 µs | 0.64x |
| encode nullable int64                       |  0.96 µs |        0.65 µs | 0.68x |
| encode utf8                                 |  1.05 µs |        0.68 µs | 0.65x |
| encode nullable utf8                        |  1.06 µs |        0.66 µs | 0.62x |
| encode mixed 6-col                          |  2.56 µs |        2.18 µs | 0.85x |
| encode list<int32>                          |  1.41 µs |        0.94 µs | 0.67x |
| encode struct<int32,double,bool>            |  1.92 µs |        1.39 µs | 0.72x |
| encode dictionary<utf8>                     |  2.17 µs |        1.09 µs | 0.50x |
| encode lazy int64                           |  0.97 µs |        0.64 µs | 0.66x |
| encode lazy double                          |  0.98 µs |        0.62 µs | 0.63x |
| encode lazy nullable int64                  |  0.96 µs |        0.65 µs | 0.68x |
| encode lazy utf8                            |  1.05 µs |        0.69 µs | 0.66x |
| encode lazy nullable utf8                   |  1.06 µs |        0.72 µs | 0.68x |
| encode lazy mixed 6-col                     |  2.56 µs |        2.06 µs | 0.80x |
| encode lazy list<int32>                     |  1.41 µs |        0.91 µs | 0.65x |
| encode lazy struct<int32,double,bool>       |  1.92 µs |        1.38 µs | 0.72x |
| encode lazy dictionary<utf8>                |  2.17 µs |        1.14 µs | 0.53x |
| decode int64                                |  0.62 µs |        0.69 µs | 1.11x |
| decode double                               |  0.61 µs |        0.67 µs | 1.10x |
| decode nullable int64                       |  0.65 µs |        0.71 µs | 1.09x |
| decode utf8                                 |  0.73 µs |        0.77 µs | 1.05x |
| decode nullable utf8                        |  0.76 µs |        0.78 µs | 1.03x |
| decode mixed 6-col                          |  2.22 µs |        1.98 µs | 0.89x |
| decode list<int32>                          |  1.06 µs |        0.95 µs | 0.90x |
| decode struct<int32,double,bool>            |  1.27 µs |        1.38 µs | 1.09x |
| decode dictionary<utf8>                     |  1.34 µs |        1.24 µs | 0.93x |
| decode + toVector int64                     |  0.72 µs |        0.79 µs | 1.10x |
| decode + toVector double                    |  0.71 µs |        0.78 µs | 1.10x |
| decode + toVector nullable int64            |  0.78 µs |        0.80 µs | 1.03x |
| decode + toVector utf8                      |  1.90 µs |        1.24 µs | 0.65x |
| decode + toVector nullable utf8             |  1.93 µs |        1.24 µs | 0.64x |
| decode + toVector mixed 6-col               |  5.07 µs |        3.25 µs | 0.64x |
| decode + toVector list<int32>               |  4.61 µs |        1.57 µs | 0.34x |
| decode + toVector struct<int32,double,bool> |  1.54 µs |        1.68 µs | 1.09x |
| decode + toVector dictionary<utf8>          |  2.58 µs |        1.98 µs | 0.77x |

<sub>Last run 2026-10-09 07:00:11 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5; arrow-rs 58.2.0, rustc 1.98.1, criterion.rs 0.7.0.</sub>
<!-- END_AUTOGEN bench:arrow-encode-decode-small -->

### Entry points, mixed 6-column table

<!-- BEGIN_AUTOGEN bench:arrow-api-paths -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-arrow entry points vs arrow-rs, mixed 6-column table (100k rows)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-arrow entry points vs arrow-rs, mixed 6-column table (100k rows)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5; arrow-rs 58.2.0, rustc 1.98.1, criterion.rs 0.7.0</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">1250</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">2500</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">3750</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">5000</text>
    </g>
    <g>
      <rect x="86.2" y="297.9" width="22.8" height="22.1" rx="2" fill="#0969da"/>
      <rect x="111" y="316.8" width="22.8" height="3.2" rx="2" fill="#cf222e"/>
      <rect x="148.2" y="297.9" width="22.8" height="22.1" rx="2" fill="#0969da"/>
      <rect x="173" y="319.9" width="22.8" height="0.1" rx="2" fill="#cf222e"/>
      <rect x="210.2" y="297.9" width="22.8" height="22.1" rx="2" fill="#0969da"/>
      <rect x="235" y="316.8" width="22.8" height="3.2" rx="2" fill="#cf222e"/>
      <rect x="272.2" y="304.8" width="22.8" height="15.2" rx="2" fill="#0969da"/>
      <rect x="297" y="315.0" width="22.8" height="5.0" rx="2" fill="#cf222e"/>
      <rect x="334.2" y="296.5" width="22.8" height="23.5" rx="2" fill="#0969da"/>
      <rect x="359" y="316.6" width="22.8" height="3.4" rx="2" fill="#cf222e"/>
      <rect x="396.2" y="296.5" width="22.8" height="23.5" rx="2" fill="#0969da"/>
      <rect x="421" y="319.8" width="22.8" height="0.2" rx="2" fill="#cf222e"/>
      <rect x="458.2" y="304.6" width="22.8" height="15.4" rx="2" fill="#0969da"/>
      <rect x="483" y="315.0" width="22.8" height="5.0" rx="2" fill="#cf222e"/>
      <rect x="520.2" y="304.6" width="22.8" height="15.4" rx="2" fill="#0969da"/>
      <rect x="545" y="314.8" width="22.8" height="5.2" rx="2" fill="#cf222e"/>
      <rect x="582.2" y="231.0" width="22.8" height="89.0" rx="2" fill="#0969da"/>
      <rect x="607" y="207.8" width="22.8" height="112.2" rx="2" fill="#cf222e"/>
      <rect x="644.2" y="153.8" width="22.8" height="166.2" rx="2" fill="#0969da"/>
      <rect x="669" y="122.2" width="22.8" height="197.8" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="97.6" y="293.9" text-anchor="middle" font-size="10" fill="#1f2328">425</text>
      <text x="122.4" y="312.8" text-anchor="middle" font-size="10" fill="#1f2328">61.6</text>
      <text x="159.6" y="293.9" text-anchor="middle" font-size="10" fill="#1f2328">425</text>
      <text x="184.4" y="315.9" text-anchor="middle" font-size="10" fill="#1f2328">2.11</text>
      <text x="221.6" y="293.9" text-anchor="middle" font-size="10" fill="#1f2328">425</text>
      <text x="246.4" y="312.8" text-anchor="middle" font-size="10" fill="#1f2328">62.0</text>
      <text x="283.6" y="300.8" text-anchor="middle" font-size="10" fill="#1f2328">293</text>
      <text x="308.4" y="311.0" text-anchor="middle" font-size="10" fill="#1f2328">96.8</text>
      <text x="345.6" y="292.5" text-anchor="middle" font-size="10" fill="#1f2328">452</text>
      <text x="370.4" y="312.6" text-anchor="middle" font-size="10" fill="#1f2328">65.3</text>
      <text x="407.6" y="292.5" text-anchor="middle" font-size="10" fill="#1f2328">452</text>
      <text x="432.4" y="315.8" text-anchor="middle" font-size="10" fill="#1f2328">3.72</text>
      <text x="469.6" y="300.6" text-anchor="middle" font-size="10" fill="#1f2328">297</text>
      <text x="494.4" y="311.0" text-anchor="middle" font-size="10" fill="#1f2328">97.1</text>
      <text x="531.6" y="300.6" text-anchor="middle" font-size="10" fill="#1f2328">297</text>
      <text x="556.4" y="310.8" text-anchor="middle" font-size="10" fill="#1f2328">100</text>
      <text x="593.6" y="227.0" text-anchor="middle" font-size="10" fill="#1f2328">1712</text>
      <text x="618.4" y="203.8" text-anchor="middle" font-size="10" fill="#1f2328">2158</text>
      <text x="655.6" y="149.8" text-anchor="middle" font-size="10" fill="#1f2328">3196</text>
      <text x="680.4" y="118.2" text-anchor="middle" font-size="10" fill="#1f2328">3803</text>
    </g>
    <g>
      <text x="111" y="338" text-anchor="middle" font-size="11" fill="#1f2328">stream encode (Arrow.Stream)</text>
      <text x="173" y="338" text-anchor="middle" font-size="11" fill="#1f2328">stream encode lazy (Arrow.Stream)</text>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#1f2328">stream encode (Arrow.Write)</text>
      <text x="297" y="338" text-anchor="middle" font-size="11" fill="#1f2328">stream decode (Arrow.Stream)</text>
      <text x="359" y="338" text-anchor="middle" font-size="11" fill="#1f2328">file encode (Arrow.Stream)</text>
      <text x="421" y="338" text-anchor="middle" font-size="11" fill="#1f2328">file encode lazy (Arrow.Stream)</text>
      <text x="483" y="338" text-anchor="middle" font-size="11" fill="#1f2328">file decode (Arrow.Stream)</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#1f2328">file read (Arrow.File)</text>
      <text x="607" y="338" text-anchor="middle" font-size="11" fill="#1f2328">typed encode (Arrow.Record)</text>
      <text x="669" y="338" text-anchor="middle" font-size="11" fill="#1f2328">typed decode (Arrow.Record)</text>
    </g>
    <g>
      <g transform="translate(257, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">arrow-rs</text>
      </g>
      <g transform="translate(347, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">wireform-arrow</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-arrow entry points vs arrow-rs, mixed 6-column table (100k rows)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5; arrow-rs 58.2.0, rustc 1.98.1, criterion.rs 0.7.0</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">1250</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">2500</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">3750</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">5000</text>
    </g>
    <g>
      <rect x="86.2" y="297.9" width="22.8" height="22.1" rx="2" fill="#58a6ff"/>
      <rect x="111" y="316.8" width="22.8" height="3.2" rx="2" fill="#ff7b72"/>
      <rect x="148.2" y="297.9" width="22.8" height="22.1" rx="2" fill="#58a6ff"/>
      <rect x="173" y="319.9" width="22.8" height="0.1" rx="2" fill="#ff7b72"/>
      <rect x="210.2" y="297.9" width="22.8" height="22.1" rx="2" fill="#58a6ff"/>
      <rect x="235" y="316.8" width="22.8" height="3.2" rx="2" fill="#ff7b72"/>
      <rect x="272.2" y="304.8" width="22.8" height="15.2" rx="2" fill="#58a6ff"/>
      <rect x="297" y="315.0" width="22.8" height="5.0" rx="2" fill="#ff7b72"/>
      <rect x="334.2" y="296.5" width="22.8" height="23.5" rx="2" fill="#58a6ff"/>
      <rect x="359" y="316.6" width="22.8" height="3.4" rx="2" fill="#ff7b72"/>
      <rect x="396.2" y="296.5" width="22.8" height="23.5" rx="2" fill="#58a6ff"/>
      <rect x="421" y="319.8" width="22.8" height="0.2" rx="2" fill="#ff7b72"/>
      <rect x="458.2" y="304.6" width="22.8" height="15.4" rx="2" fill="#58a6ff"/>
      <rect x="483" y="315.0" width="22.8" height="5.0" rx="2" fill="#ff7b72"/>
      <rect x="520.2" y="304.6" width="22.8" height="15.4" rx="2" fill="#58a6ff"/>
      <rect x="545" y="314.8" width="22.8" height="5.2" rx="2" fill="#ff7b72"/>
      <rect x="582.2" y="231.0" width="22.8" height="89.0" rx="2" fill="#58a6ff"/>
      <rect x="607" y="207.8" width="22.8" height="112.2" rx="2" fill="#ff7b72"/>
      <rect x="644.2" y="153.8" width="22.8" height="166.2" rx="2" fill="#58a6ff"/>
      <rect x="669" y="122.2" width="22.8" height="197.8" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="97.6" y="293.9" text-anchor="middle" font-size="10" fill="#e6edf3">425</text>
      <text x="122.4" y="312.8" text-anchor="middle" font-size="10" fill="#e6edf3">61.6</text>
      <text x="159.6" y="293.9" text-anchor="middle" font-size="10" fill="#e6edf3">425</text>
      <text x="184.4" y="315.9" text-anchor="middle" font-size="10" fill="#e6edf3">2.11</text>
      <text x="221.6" y="293.9" text-anchor="middle" font-size="10" fill="#e6edf3">425</text>
      <text x="246.4" y="312.8" text-anchor="middle" font-size="10" fill="#e6edf3">62.0</text>
      <text x="283.6" y="300.8" text-anchor="middle" font-size="10" fill="#e6edf3">293</text>
      <text x="308.4" y="311.0" text-anchor="middle" font-size="10" fill="#e6edf3">96.8</text>
      <text x="345.6" y="292.5" text-anchor="middle" font-size="10" fill="#e6edf3">452</text>
      <text x="370.4" y="312.6" text-anchor="middle" font-size="10" fill="#e6edf3">65.3</text>
      <text x="407.6" y="292.5" text-anchor="middle" font-size="10" fill="#e6edf3">452</text>
      <text x="432.4" y="315.8" text-anchor="middle" font-size="10" fill="#e6edf3">3.72</text>
      <text x="469.6" y="300.6" text-anchor="middle" font-size="10" fill="#e6edf3">297</text>
      <text x="494.4" y="311.0" text-anchor="middle" font-size="10" fill="#e6edf3">97.1</text>
      <text x="531.6" y="300.6" text-anchor="middle" font-size="10" fill="#e6edf3">297</text>
      <text x="556.4" y="310.8" text-anchor="middle" font-size="10" fill="#e6edf3">100</text>
      <text x="593.6" y="227.0" text-anchor="middle" font-size="10" fill="#e6edf3">1712</text>
      <text x="618.4" y="203.8" text-anchor="middle" font-size="10" fill="#e6edf3">2158</text>
      <text x="655.6" y="149.8" text-anchor="middle" font-size="10" fill="#e6edf3">3196</text>
      <text x="680.4" y="118.2" text-anchor="middle" font-size="10" fill="#e6edf3">3803</text>
    </g>
    <g>
      <text x="111" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">stream encode (Arrow.Stream)</text>
      <text x="173" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">stream encode lazy (Arrow.Stream)</text>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">stream encode (Arrow.Write)</text>
      <text x="297" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">stream decode (Arrow.Stream)</text>
      <text x="359" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">file encode (Arrow.Stream)</text>
      <text x="421" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">file encode lazy (Arrow.Stream)</text>
      <text x="483" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">file decode (Arrow.Stream)</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">file read (Arrow.File)</text>
      <text x="607" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">typed encode (Arrow.Record)</text>
      <text x="669" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">typed decode (Arrow.Record)</text>
    </g>
    <g>
      <g transform="translate(257, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">arrow-rs</text>
      </g>
      <g transform="translate(347, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">wireform-arrow</text>
      </g>
    </g>
  </g>
</svg>


| Operation                         | arrow-rs | wireform-arrow | ratio |
| :-------------------------------- | -------: | -------------: | ----: |
| stream encode (Arrow.Stream)      |   425 µs |        61.6 µs | 0.14x |
| stream encode lazy (Arrow.Stream) |   425 µs |        2.11 µs | 0.00x |
| stream encode (Arrow.Write)       |   425 µs |        62.0 µs | 0.15x |
| stream decode (Arrow.Stream)      |   293 µs |        96.8 µs | 0.33x |
| file encode (Arrow.Stream)        |   452 µs |        65.3 µs | 0.14x |
| file encode lazy (Arrow.Stream)   |   452 µs |        3.72 µs | 0.01x |
| file decode (Arrow.Stream)        |   297 µs |        97.1 µs | 0.33x |
| file read (Arrow.File)            |   297 µs |         100 µs | 0.34x |
| typed encode (Arrow.Record)       |  1712 µs |        2158 µs | 1.26x |
| typed decode (Arrow.Record)       |  3196 µs |        3803 µs | 1.19x |

<sub>Last run 2026-10-09 07:00:11 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5; arrow-rs 58.2.0, rustc 1.98.1, criterion.rs 0.7.0.</sub>
<!-- END_AUTOGEN bench:arrow-api-paths -->

The entry-point rows map to the closest arrow-rs path: `StreamWriter`
for every stream writer (eager, lazy, `Arrow.Write`), `FileWriter` for
both file writers, `FileReader` for both file readers, and for the
typed rows a `Vec` of row structs turned into one array per field (or
read back into owned row structs), since arrow-rs has no record
deriving. Comparisons against pyarrow and arrow-cpp are not yet
measured.
