# wireform-parquet

Read and write [Apache Parquet](https://parquet.apache.org/) files in Haskell.
Work with records or Arrow columns, including nullable fields, structs, lists
and maps. Compression, column projection and statistics-based filtering are
available for analytical workloads.

## Install

Requires **GHC 9.10 or newer**. Add `wireform-parquet` to your application's dependencies. The example below
also uses `wireform-arrow` for record conversion:

```cabal
build-depends:
  base,
  bytestring,
  text,
  vector,
  wireform-arrow,
  wireform-parquet
```

The default build enables Snappy, Zstandard and LZ4 and needs their native
development libraries, together with zlib. Brotli is optional:

```sh
cabal build wireform-parquet -fbrotli
```

Brotli also requires the native Brotli 1.x development libraries and
`pkg-config`. Individual optional codecs can be disabled with `-f-snappy`,
`-f-zstd`, `-f-lz4` and `-f-brotli`. Uncompressed, GZip and LZO support is
always available. If you disable Snappy, set `writeCompression` to another
enabled codec.

## Write and read a file

This example derives an Arrow table for a Haskell record, writes it to
Parquet with the default Snappy compression, then reads the records back.
Nullable fields are preserved.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Arrow.Derive as Derive
import qualified Arrow.Record as Arrow
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Parquet as P

data Person = Person
  { personId :: Int64
  , personName :: Text
  , personEmail :: Maybe Text
  } deriving (Eq, Show)

Derive.deriveArrow ''Person

main :: IO ()
main = do
  let people = V.fromList
        [ Person 1 "Ada" (Just "ada@example.com")
        , Person 2 "Grace" Nothing
        ]
      (schema, columns) = Arrow.encodeTable Derive.hasTable people

  (fileSchema, rowGroups) <- either fail pure $
    P.arrowToParquetMixed schema [columns]
  BS.writeFile "people.parquet" $
    P.encodeParquetMixed P.defaultWriteOptions fileSchema rowGroups

  bytes <- BS.readFile "people.parquet"
  file <- either fail pure (P.decodeParquet P.defaultReadOptions bytes)
  storedSchema <- either fail pure (P.parquetFileArrowSchemaChecked file)
  storedColumns <- either (fail . show) pure $
    P.parquetRowGroupToArrow storedSchema file 0
  decoded <- either fail pure $
    Arrow.decodeTable Derive.hasTable storedSchema storedColumns
  print (decoded :: V.Vector Person)
```

If you already have Arrow columns, start with `arrowToParquetMixed`.
Use `streamRowGroupsIter` to decode row groups as they are consumed.
For nested data, `encodeParquetNestedWith` accepts writer options for
compression, page version and indexes, with automatic min/max statistics.
Set `writeBloomFilters` to top-level field names to generate filters for all of their physical leaves. For
individual leaf control, use `buildNestedFileWithAux`.

## Choosing an API

Import `Parquet` for the main file, Arrow and record APIs. More specialized
modules are available when you need them:

| Task | API |
| --- | --- |
| Read or write files and choose compression | [`Parquet.HighLevel`](https://hackage.haskell.org/package/wireform-parquet/docs/Parquet-HighLevel.html) |
| Convert Arrow columns, project fields or read row groups | [`Parquet.Arrow`](https://hackage.haskell.org/package/wireform-parquet/docs/Parquet-Arrow.html) |
| Write nested structs, lists and maps | [`Parquet.Nested`](https://hackage.haskell.org/package/wireform-parquet/docs/Parquet-Nested.html) |
| Derive Parquet schemas and row conversions | [`Parquet.Derive`](https://hackage.haskell.org/package/wireform-parquet/docs/Parquet-Derive.html) |
| Skip data using predicates, indexes and bloom filters | [`Parquet.Predicate`](https://hackage.haskell.org/package/wireform-parquet/docs/Parquet-Predicate.html) |
| Answer counts and extrema from available statistics | [`Parquet.Aggregate`](https://hackage.haskell.org/package/wireform-parquet/docs/Parquet-Aggregate.html) |
| Work with Variant values | [`Parquet.Variant`](https://hackage.haskell.org/package/wireform-parquet/docs/Parquet-Variant.html), [`Parquet.Variant.Parquet`](https://hackage.haskell.org/package/wireform-parquet/docs/Parquet-Variant-Parquet.html) |

The Arrow bridge supports decimals, UUIDs, dates and timestamps alongside
ordinary numeric, Boolean, text and binary columns. The nested bridge also
preserves binary16 values. Wide and legacy timestamps
can be preserved losslessly; conversions to ordinary Arrow timestamps check
range and precision.

Target schemas can select and reorder fields, widen numeric types, and request
large strings, binary columns or lists. Filtering with page indexes retains
whole matching pages; apply a row filter when you need exact matching rows.

Use `encodeParquetIO`, `encodeParquetMixedIO` or `encodeParquetNestedIO` to
write encrypted files with required, nullable or nested columns. Set
`writeFooterEncryption` and select columns with `writeColumnEncryption`;
columns can share the footer key or use separate keys. Set `writeFooterMode`
to `SignedPlaintextFooter` to keep the footer readable and authenticate it
with the supplied signing key. Generate a fresh footer configuration with
`newFooterEncryption` for every file.

Use `readDecryptionConfig` in the read options to supply footer and column
keys for encrypted files and to verify signed plaintext footers. See
[`Parquet.Decryption`](https://hackage.haskell.org/package/wireform-parquet/docs/Parquet-Decryption.html) for key routing.

For applications working across Parquet, Arrow IPC and ORC, the separate
[`wireform-columnar`](https://github.com/iand675/wireform-/tree/main/wireform-columnar) package provides a shared API.

## Current limitations

- Readers do not follow column chunks into external files. Read the individual
  data files rather than a metadata-only file that refers to them.
- For a text or binary column with 2 GiB or more of decoded value bytes in a
  row group, request `ALargeUtf8` or `ALargeBinary` in the target Arrow schema.

## Development

See the [developer guide](https://hackage.haskell.org/package/wireform-parquet-0.1.0.0/src/DEVELOPING.md) for build and test commands,
compatibility checks, benchmarks and implementation notes.

## License

[BSD-3-Clause](https://hackage.haskell.org/package/wireform-parquet-0.1.0.0/src/LICENSE).
