# wireform-edn

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

[Extensible Data Notation (EDN)](https://github.com/edn-format/edn) for
Haskell. Encode and decode the dynamic
[`EDN.Value`](src/EDN/Value.hs), derive typeclass instances generically
or via Template Haskell, and bridge to JSON.

EDN is the data subset of Clojure's reader syntax: nil, booleans,
integers, floats, strings, characters, keywords (`:foo`), symbols
(`foo`), lists (`(...)`), vectors (`[...]`), maps (`{...}`), sets
(`#{...}`), and tagged literals (`#inst "..."`, `#uuid "..."`, plus
user-defined tags). It's the natural choice when you're talking to a
Clojure or ClojureScript service, and a useful config format when you
want first-class set / keyword / tagged-literal support on top of a
JSON-shaped data model.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-edn,
  wireform-derive,    -- only if you want the cross-format annotation deriver
```

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-edn` to compile
locally. Compiling with the LLVM backend (`-fllvm`) adds compile time
but measurably improves runtime performance.

## Hello world

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import EDN.Class (ToEDN, FromEDN, encodeEDN, decodeEDN)

data Config = Config
  { host  :: !Text
  , port  :: !Int
  , debug :: !Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToEDN, FromEDN)

main :: IO ()
main = do
  let cfg  = Config "localhost" 8080 True
      text = encodeEDN cfg
  putStrLn (T.unpack text)
  case decodeEDN text of
    Right (decoded :: Config) -> print decoded
    Left  err                 -> putStrLn err
```

`encodeEDN cfg` renders to:

```edn
{:host "localhost", :port 8080, :debug true}
```

The runnable version lives in [`examples/EDNExample.hs`](../examples/EDNExample.hs).

## What's in here

| Module           | Role                                                      |
|------------------|-----------------------------------------------------------|
| `EDN.Value`      | Dynamic untyped `Value` ADT (`VNil`, `VBool`, `VInt`, `VDouble`, `VString`, `VKeyword`, `VSymbol`, `VList`, `VVector`, `VSet`, `VMap`, `VTagged`, ...) |
| `EDN.Encoding`   | The `Encoding` builder type used by `ToEDN` instances     |
| `EDN.Encode`     | Pretty-printer that produces canonical EDN text           |
| `EDN.Decode`     | Megaparsec-based parser that consumes EDN text into `Value` or a typed Haskell value |
| `EDN.Class`      | Public `ToEDN` / `FromEDN` typeclasses + `encodeEDN` / `decodeEDN` |
| `EDN.Derive`     | `deriveEDN` / `deriveToEDN` / `deriveFromEDN` Template Haskell entry points |
| `EDN.JSON`       | Bridge to and from `aeson`'s `Value`                      |

## Encode and decode

The typeclass entry points produce and consume `Text` (EDN is a text
format, not a binary one):

```haskell
encodeEDN :: ToEDN   a => a    -> Text
decodeEDN :: FromEDN a => Text -> Either String a
```

For dynamic values without a Haskell type to mirror them, work with
[`EDN.Value`](src/EDN/Value.hs) directly. The `Value` ADT distinguishes
keywords (`:foo`) from symbols (`foo`), sets from vectors from lists,
and tagged literals from their underlying values, all of which JSON
flattens or loses.

## Annotation-driven deriving

`EDN.Derive` consumes the cross-format `Wireform.Derive.Modifier`
vocabulary from [`wireform-derive`](../wireform-derive/README.md). EDN
keys are conventionally kebab-case keywords, which the `Idiomatic`
naming convention picks up automatically:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified EDN.Derive            as DEDN
import qualified Wireform.Derive.Aeson as DAeson
import Wireform.Derive (renameIdiomatic)

data Person = Person
  { personFullName :: !Text
  , personAge      :: !Word32
  } deriving stock (Show, Eq, Generic)

{-# ANN type Person ("Person" :: String) #-}
{-# ANN personFullName renameIdiomatic #-}
{-# ANN personAge      renameIdiomatic #-}

DEDN.deriveEDN    ''Person
DAeson.deriveJSON ''Person
```

`personFullName` lands as `:person-full-name` in EDN and `personFullName`
in JSON, both driven by the same `Idiomatic` annotation.

## JSON bridge

`EDN.JSON` round-trips between `EDN.Value` and `Data.Aeson.Value`.
Mostly obvious; the EDN-specific shapes follow conventional fallbacks:
keywords as JSON strings (`"foo"`), symbols as
`{"#sym": "foo"}` envelopes, sets as
`{"#set": [...]}` envelopes, tagged literals as
`{"#tag": "inst", "value": "..."}` envelopes.

## Testing

The per-format Hedgehog suite lives in `test/`:

```bash
cabal test wireform-edn:wireform-edn-derive-test
```

It covers the typeclass instances, the deriver, generic and
TH-derived round-trips, and the dynamic `Value` ADT.

## Benchmarks

A criterion harness in [`bench/Bench.hs`](bench/Bench.hs):

```bash
cabal bench wireform-edn:wireform-edn-bench
```

<!-- BEGIN_AUTOGEN bench:edn-encode-decode -->
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="bench-results/charts/edn-encode-decode-dark.svg">
  <img src="bench-results/charts/edn-encode-decode-light.svg" alt="wireform-edn encode + decode (Person record, text format)">
</picture>

| Operation      |   encode |    decode | ratio |
| :------------- | -------: | --------: | ----: |
| single Person  |   787 ns |   2010 ns | 2.55x |
| [Person] x 100 | 84916 ns | 244287 ns | 2.88x |

<sub>Last run 2026-06-27 11:35:54 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:edn-encode-decode -->

For cross-language comparisons:

- Haskell: [`hedn`](https://hackage.haskell.org/package/hedn) (the
  established Haskell EDN parser).
- JVM: [edn-java](https://github.com/bpsm/edn-java) and Clojure's
  built-in `clojure.edn` reader.

## License

BSD-3-Clause.

## References

- [EDN specification](https://github.com/edn-format/edn)
- [Clojure reader literals](https://clojure.org/reference/reader)
