# wireform-fory

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

[Apache Fory](https://fory.apache.org/) (formerly Fury) for Haskell.
Encode and decode the dynamic [`Fory.Value`](src/Fory/Value.hs), derive
typeclass instances generically or via Template Haskell, and stay
byte-for-byte wire-compatible with `pyfory` 0.17 across the implemented
type set (verified by an opt-in
[interop test suite](test-interop/Main.hs) against a real `pyfory`
process).

Fory is Apache's cross-language serialization framework. It targets
the same niche as protobuf, MessagePack, and Avro, but with a focus on
xlang structural types: a `NAMED_STRUCT` written in Java should
deserialize verbatim in Python, Go, Rust, JavaScript, or Haskell. The
wire format covers `null`, `bool`, every signed and unsigned integer
width with both fixed and varint encodings, IEEE 754 floats, length-
or NUL-terminated strings (with LATIN-1 / UTF-8 selection), binary,
chunked `LIST` / `SET` (with reference tracking), chunked `MAP`,
`NAMED_STRUCT` with a 4-byte fingerprint hash, primitive arrays, and
the five MetaString encodings (LowerSpecial, LowerUpperDigitSpecial,
FirstToLowerSpecial, AllToLowerSpecial, UTF-8) with MurmurHash3 hashes
for strings over 16 bytes.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-fory,
  wireform-derive,    -- only if you want the cross-format annotation deriver
```

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-fory` to compile
locally. Compiling with the LLVM backend (`-fllvm`) adds compile time
but measurably improves runtime performance.

## Hello world

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

import GHC.Generics (Generic)
import Data.Text (Text)
import Fory.Class (ToFory, FromFory, encodeFory, decodeFory)

data Order = Order
  { orderId   :: !Int
  , orderItem :: !Text
  , orderQty  :: !Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToFory, FromFory)

main :: IO ()
main = do
  let o     = Order 42 "widget" 3
      bytes = encodeFory o
  case decodeFory bytes of
    Right (decoded :: Order) -> print decoded
    Left  err                -> putStrLn err
```

## What's in here

| Module                       | Role                                                      |
|------------------------------|-----------------------------------------------------------|
| `Fory.Value`                 | Dynamic untyped `Value` ADT covering every implemented Fory type |
| `Fory.Encoding`              | The `Encoding` builder type used by `ToFory` instances    |
| `Fory.Encode`                | Low-level encoding primitives, with detailed haddock about which subset of the spec is implemented |
| `Fory.Decode`                | Low-level decoding primitives                             |
| `Fory.Direct`                | Direct-write encode path for predictable-size payloads    |
| `Fory.Class`                 | Public `ToFory` / `FromFory` typeclasses + `encodeFory` / `decodeFory` |
| `Fory.Derive`                | `deriveFory` / `deriveToFory` / `deriveFromFory` Template Haskell entry points |
| `Fory.Bulk`                  | Bulk encoding helpers for primitive arrays (`BoolArray`, `Float64Array`, ...) |
| `Fory.IO`                    | I/O helpers around the encode buffer                      |
| `Fory.Options`               | Encoder / decoder configuration (reference tracking, scoped tracking, etc.) |
| `Fory.Struct`                | Schema registry for `NAMED_STRUCT` types (mirrors pyfory's `StructSchema`) |
| `Fory.TypeId`                | The Fory type-ID enumeration                              |
| `Fory.MetaString`            | MetaString encoder / decoder for string deduplication     |
| `Fory.MetaString.Encoder`    | The five MetaString encodings (LowerSpecial / ... / UTF-8) |
| `Fory.MetaString.Hash`       | MurmurHash3-x64-128 hashcodes for MetaString interning    |
| `Fory.TextHelpers`           | UTF-8 / LATIN-1 selection helpers                         |

## Encode and decode

The typeclass entry points are the usual shape:

```haskell
encodeFory :: ToFory   a => a          -> ByteString
decodeFory :: FromFory a => ByteString -> Either String a
```

For dynamic values without a Haskell type to mirror them, work with
[`Fory.Value`](src/Fory/Value.hs) directly. The `Value` ADT carries
every implemented Fory type, including chunked collections with
`TRACKING_REF` flag, primitive arrays, and `NAMED_STRUCT` envelopes
with their fingerprint hash.

`Fory.Options` controls the encoder / decoder behavior:

```haskell
import qualified Fory.Options as O

let opts = O.defaultEncodeOptions { O.eoRefTracking = True }
```

Reference tracking detects structural sharing in `Hashable`-keyed
graphs and emits Fory `REF` shortcuts; the decoder reconstructs the
shared substructures. Off by default to match the most common
cross-language encoder configuration.

## Annotation-driven deriving

`Fory.Derive` consumes the cross-format `Wireform.Derive.Modifier`
vocabulary from [`wireform-derive`](../wireform-derive/README.md). The
field renaming Fory expects (camelCase by default in pyfory) lines up
with the `renameStyle CamelCase` annotation:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified Fory.Derive          as DFory
import qualified Wireform.Derive.Aeson as DAeson
import Wireform.Derive (renameStyle, CamelCase)

data Person = Person
  { personFullName :: !Text
  , personAge      :: !Word32
  } deriving stock (Show, Eq, Generic)

{-# ANN type Person ("Person" :: String) #-}
{-# ANN personFullName (renameStyle CamelCase) #-}
{-# ANN personAge      (renameStyle CamelCase) #-}

DFory.deriveFory  ''Person
DAeson.deriveJSON ''Person
```

The deriver also registers a `Fory.Struct.StructSchema` for the type
on the side, which is what pyfory needs to materialize the
`NAMED_STRUCT` payload back into its own struct type. Both sides see
the same fingerprint hash and the same canonical field order.

## Cross-language interop with `pyfory`

The `wireform-fory-interop` test suite (behind the `+python-interop`
Cabal flag) shells out to a real `pyfory` install and verifies that
every implemented type round-trips byte-for-byte:

```bash
pip install pyfory==0.17.*
cabal test wireform-fory:wireform-fory-interop -fpython-interop
```

Current status: 45 / 45 implemented cases passing. The remaining gap
is `NAMED_COMPATIBLE_STRUCT` (Fory's schema-evolution variant, with a
bit-packed `TypeDef` field-info layout). The in-package
`CompatibleStructVal` round-trips fine inside wireform-fory; only
cross-language interop for it is unfinished.

A Hedgehog-driven fuzzer (`wireform-fory-interop-fuzz`) generates
random `Value`s and asks pyfory to deserialize them, surfacing any
case where the wire format diverges:

```bash
cabal test wireform-fory:wireform-fory-interop-fuzz -fpython-interop
```

## Testing

The per-format suite (Hedgehog + HUnit) lives in `test/`:

```bash
cabal test wireform-fory:wireform-fory-test
```

It covers the dynamic `Value` ADT, the encoder / decoder, MetaString
interop edge cases, and the spec extensions for each implemented type.

## Benchmarks

A criterion harness in [`bench/Bench.hs`](bench/Bench.hs):

```bash
cabal bench wireform-fory:wireform-fory-bench
```

<!-- BEGIN_AUTOGEN bench:fory-encode-decode -->
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="bench-results/charts/fory-encode-decode-dark.svg">
  <img src="bench-results/charts/fory-encode-decode-light.svg" alt="wireform-fory encode + decode across representative shapes">
</picture>

| Operation        |  encode |   decode | ratio |
| :--------------- | ------: | -------: | ----: |
| int              | 75.7 ns |  62.5 ns | 0.83x |
| string           | 85.3 ns |  71.8 ns | 0.84x |
| bytes 1KB        |  127 ns | 60.10 ns | 0.48x |
| Person struct    |  266 ns |   440 ns | 1.65x |
| list[Person]*100 | 6630 ns | 10783 ns | 1.63x |

<sub>Last run 2026-06-27 11:35:55 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:fory-encode-decode -->

For cross-language comparisons:

- Java: [Apache Fory's reference Java implementation](https://github.com/apache/fory).
- Python: [pyfory](https://pypi.org/project/pyfory/) (the same
  binding the interop test shells out to).
- Rust: Apache Fory ships an in-repo Rust binding under the same
  monorepo.

## License

BSD-3-Clause.

## References

- [Apache Fory xlang serialization spec](https://fory.apache.org/docs/specification/xlang_serialization_spec)
- [Apache Fory project](https://fory.apache.org/)
- [pyfory PyPI](https://pypi.org/project/pyfory/)
