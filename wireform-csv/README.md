# wireform-csv

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

CSV, TSV, and pipe-separated text for Haskell. Encode and decode the
dynamic [`CSV.Value`](src/CSV/Value.hs), derive typeclass instances
generically or via Template Haskell, and parse with a SIMD-accelerated
delimiter scanner that pulls bytes off the input four to sixteen
characters at a time depending on what the host CPU supports.

CSV is the format that won't go away. Spreadsheets export it,
analytics pipelines ingest it, every database has a `COPY ... FROM
'file.csv'` knob, and you can't go a full quarter without somebody
emailing you a `report.csv`. The grammar is almost simple, except
for quoting, except for embedded delimiters, except for line endings,
except for the BOM, except for trailing whitespace, and so on. This
package handles those cases via the [RFC 4180](https://www.rfc-editor.org/rfc/rfc4180)
shape with a configurable `CSVConfig` for the actually-non-standard
parts.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-csv,
  wireform-derive,    -- only if you want the cross-format annotation deriver
```

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-csv` to compile
locally. Compiling with the LLVM backend (`-fllvm`) adds compile time
but measurably improves runtime performance.

## Hello world

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Vector as V
import CSV.Class (ToCSV, FromCSV)
import CSV.Encode (encodeRecords)
import CSV.Decode (decodeRecords)
import CSV.Value  (defaultCSV)

data Sale = Sale
  { saleProduct :: !Text
  , saleUnits   :: !Int
  , salePrice   :: !Double
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToCSV, FromCSV)

main :: IO ()
main = do
  let rows  = V.fromList
        [ Sale "widget"  3 9.99
        , Sale "gadget" 12 4.50
        ]
      bytes = encodeRecords defaultCSV rows
  BS8.putStrLn bytes
  case decodeRecords defaultCSV bytes of
    Right (decoded :: V.Vector Sale) -> mapM_ print decoded
    Left  err                        -> putStrLn err
```

`encodeRecords defaultCSV rows` renders to:

```csv
saleProduct,saleUnits,salePrice
widget,3,9.99
gadget,12,4.5
```

## What's in here

| Module          | Role                                                      |
|-----------------|-----------------------------------------------------------|
| `CSV.Value`     | `CSVDocument` (header + body), `CSVConfig` (delimiter, quote, line ending, header policy), `defaultCSV`, `defaultTSV` |
| `CSV.Encode`    | `encode :: CSVConfig -> CSVDocument -> ByteString` and `encodeRecords :: ToCSV a => CSVConfig -> Vector a -> ByteString` |
| `CSV.Decode`    | `decode`, `decodeStream` (streaming row-at-a-time callback), `decodeRecords` (typed); SIMD-accelerated delimiter / quote / newline scanning via `Wireform.FFI.findByteBS` |
| `CSV.Class`     | Public `ToCSV` / `FromCSV` typeclasses + the `CSVField` typeclass for individual cells |
| `CSV.Derive`    | `deriveCSV` / `deriveToCSV` / `deriveFromCSV` Template Haskell entry points |

## Encode and decode

The most common entry points are the typed `encodeRecords` /
`decodeRecords`, which work over a `Vector` of `ToCSV` / `FromCSV`
records:

```haskell
encodeRecords :: ToCSV   a => CSVConfig -> Vector a    -> ByteString
decodeRecords :: FromCSV a => CSVConfig -> ByteString -> Either String (Vector a)
```

Below them, `encode` / `decode` work in terms of `CSVDocument` (which
splits the header row from the body) for cases where you need direct
control over header handling or you're working with dynamic columns.

`decodeStream` calls back with one `Vector Text` per row as it parses,
useful for inputs that don't fit in memory:

```haskell
import qualified CSV.Decode as CSVD

CSVD.decodeStream defaultCSV input $ \row -> do
  -- one row at a time, header-stripped
  print row
```

`CSVConfig` covers the actually-non-standard parts: the field
delimiter (`,` / `\t` / `|` / `;`), the quoting character, the line
ending, and whether the input has a header row. `defaultCSV` and
`defaultTSV` are the obvious starting points.

## Annotation-driven deriving

`CSV.Derive` consumes the cross-format `Wireform.Derive.Modifier`
vocabulary from [`wireform-derive`](../wireform-derive/README.md). CSV
column headers are conventionally lowercase and underscored, which
the `renameStyle SnakeCase` annotation produces:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified CSV.Derive as DCSV
import Wireform.Derive (renameStyle, SnakeCase)

data Sale = Sale
  { saleProduct :: !Text
  , saleUnits   :: !Int
  , salePrice   :: !Double
  } deriving stock (Show, Eq, Generic)

{-# ANN type Sale ("Sale" :: String) #-}
{-# ANN saleProduct (renameStyle SnakeCase) #-}
{-# ANN saleUnits   (renameStyle SnakeCase) #-}
{-# ANN salePrice   (renameStyle SnakeCase) #-}

DCSV.deriveCSV ''Sale
```

Headers become `sale_product`, `sale_units`, `sale_price`.

## Performance

The decoder uses `Wireform.FFI.findByteBS` from `wireform-core` to
scan past delimiters, quotes, and newlines in 16-byte SIMD chunks
(SSE2, AVX2, or NEON depending on the host). Quoted fields fall back
to a scalar inner loop because of the embedded-delimiter / escaped-
quote rules, but unquoted fields stay on the SIMD fast path.

## Testing

The per-format Hedgehog suite lives in `test/`:

```bash
cabal test wireform-csv:wireform-csv-derive-test
```

It covers the typeclass instances, the deriver, generic and
TH-derived round-trips, header-handling edge cases, embedded-quote
escaping, and the alternate delimiter configurations.

## Benchmarks

A criterion harness in [`bench/Bench.hs`](bench/Bench.hs):

```bash
cabal bench wireform-csv:wireform-csv-bench
```

<!-- BEGIN_AUTOGEN bench:csv-encode-decode -->
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="bench-results/charts/csv-encode-decode-dark.svg">
  <img src="bench-results/charts/csv-encode-decode-light.svg" alt="wireform-csv encode + decode (Sale record)">
</picture>

| Operation |  encode |  decode | ratio |
| :-------- | ------: | ------: | ----: |
| 10 rows   | 5.20 µs | 10.8 µs | 2.08x |
| 1000 rows |  685 µs | 1432 µs | 2.09x |

<sub>Last run 2026-06-27 11:35:54 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:csv-encode-decode -->

For cross-language comparisons:

- Haskell: [`cassava`](https://hackage.haskell.org/package/cassava)
  (the established Hackage CSV library) and
  [`csv-conduit`](https://hackage.haskell.org/package/csv-conduit).
- Rust: [`csv`](https://crates.io/crates/csv) crate, used by `xsv`
  and most Rust-side ETL.
- C: [libcsv](https://github.com/rgamble/libcsv).
- Python: [`pandas.read_csv`](https://pandas.pydata.org/docs/reference/api/pandas.read_csv.html)
  for an end-to-end ingest comparison.

## License

BSD-3-Clause.

## References

- [RFC 4180: Common format and MIME type for CSV files](https://www.rfc-editor.org/rfc/rfc4180)
