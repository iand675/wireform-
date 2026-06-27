# wireform-toml

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

[TOML](https://toml.io/) for Haskell. Encode and decode the dynamic
[`TOML.Value`](src/TOML/Value.hs), derive typeclass instances
generically or via Template Haskell, and pass the official
[toml-test](https://github.com/toml-lang/toml-test) suite when you
point the test harness at a clone of it.

TOML is a configuration file format with an intentionally minimal
surface: scalars (`integer`, `float`, `bool`, `string`), four
datetime variants (offset datetime, local datetime, local date, local
time), tables (sections), arrays, and arrays-of-tables. The grammar
is unambiguous, the spec is short, and the format is the de facto
config language for Cargo, `pyproject.toml`, Hugo, and a number of
other modern toolchains.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-toml,
  wireform-derive,    -- only if you want the cross-format annotation deriver
```

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-toml` to compile
locally. Compiling with the LLVM backend (`-fllvm`) adds compile time
but measurably improves runtime performance.

## Hello world

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import TOML.Class (ToTOML, FromTOML, encodeTOML, decodeTOML)

data Server = Server
  { host :: !Text
  , port :: !Int
  , tls  :: !Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToTOML, FromTOML)

main :: IO ()
main = do
  let s    = Server "localhost" 8080 True
      text = encodeTOML s
  putStrLn (T.unpack text)
  case decodeTOML text of
    Right (decoded :: Server) -> print decoded
    Left  err                 -> putStrLn err
```

`encodeTOML s` renders to:

```toml
host = "localhost"
port = 8080
tls = true
```

## What's in here

| Module           | Role                                                      |
|------------------|-----------------------------------------------------------|
| `TOML.Value`     | Dynamic untyped `Value` ADT (scalars, tables, arrays of tables, the four datetime variants) |
| `TOML.Encoding`  | The `Encoding` builder type used by `ToTOML` instances    |
| `TOML.Encode`    | Pretty-printer that produces canonical TOML 1.0 / 1.1 output |
| `TOML.Decode`    | Megaparsec-based parser that consumes TOML text into `Value` or a typed Haskell value |
| `TOML.Class`     | Public `ToTOML` / `FromTOML` typeclasses + `encodeTOML` / `decodeTOML` / `encodeTOMLDirect` |
| `TOML.Derive`    | `deriveTOML` / `deriveToTOML` / `deriveFromTOML` Template Haskell entry points |

## Encode and decode

The typeclass entry points produce and consume `Text`:

```haskell
encodeTOML       :: ToTOML   a => a    -> Text
encodeTOMLDirect :: ToTOML   a => a    -> Text   -- direct-write path
decodeTOML       :: FromTOML a => Text -> Either String a
```

Both TOML 1.0.0 and the in-progress 1.1.0 surface (notably extended
key syntax) are handled by the parser.

For dynamic values without a Haskell type to mirror them, work with
[`TOML.Value`](src/TOML/Value.hs) directly. The `Value` ADT preserves
the four TOML datetime variants (`OffsetDateTime`, `LocalDateTime`,
`LocalDate`, `LocalTime`) as separate constructors, so round-tripping
through the dynamic representation doesn't lose timezone information.

## Annotation-driven deriving

`TOML.Derive` consumes the cross-format `Wireform.Derive.Modifier`
vocabulary from [`wireform-derive`](../wireform-derive/README.md):

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified TOML.Derive          as DTOML
import Wireform.Derive (rename, renameStyle, KebabCase)

data DatabaseConfig = DatabaseConfig
  { dbConnectionString :: !Text
  , dbMaxOpenConns     :: !Int
  } deriving stock (Show, Eq, Generic)

{-# ANN type DatabaseConfig ("DatabaseConfig" :: String) #-}
{-# ANN dbConnectionString (renameStyle KebabCase) #-}
{-# ANN dbMaxOpenConns     (renameStyle KebabCase) #-}

DTOML.deriveTOML ''DatabaseConfig
```

Renders as `db-connection-string` and `db-max-open-conns` keys, which
matches the convention TOML configs in the wild tend to use.

## Testing

The per-format Hedgehog suite lives in `test/`:

```bash
cabal test wireform-toml:wireform-toml-derive-test
```

### Conformance against `toml-test`

The test binary also runs an opt-in conformance harness against the
official [toml-lang/toml-test](https://github.com/toml-lang/toml-test)
suite. Point either `TOML_TEST_SUITE` or `TOML_TEST_DIR` at a local
clone and the suite will walk every `tests/valid/` and `tests/invalid/`
fixture:

```bash
git clone https://github.com/toml-lang/toml-test /tmp/toml-test
TOML_TEST_SUITE=/tmp/toml-test \
  cabal test wireform-toml:wireform-toml-derive-test
```

When the env var is unset the harness reports a no-op skip group so
CI stays green out of the box. A built-in mini-suite drawn from the
TOML 1.0 spec examples always runs, so core compliance is exercised
even without the external clone.

## Benchmarks

A criterion harness in [`bench/Bench.hs`](bench/Bench.hs):

```bash
cabal bench wireform-toml:wireform-toml-bench
```

<!-- BEGIN_AUTOGEN bench:toml-encode-decode -->
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="bench-results/charts/toml-encode-decode-dark.svg">
  <img src="bench-results/charts/toml-encode-decode-light.svg" alt="wireform-toml encode + decode (Person record)">
</picture>

| Operation      |   encode |    decode |  ratio |
| :------------- | -------: | --------: | -----: |
| single Person  |   730 ns |   2383 ns |  3.26x |
| [Person] x 100 | 86012 ns | 343876 ns | 3.100x |

<sub>Last run 2026-06-27 11:35:55 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5. Decode is now linear in input size (was previously O(N²) due to T.index/T.length on the full source); 100-record decode dropped from 240 ms to 335 µs..</sub>
<!-- END_AUTOGEN bench:toml-encode-decode -->

For cross-language comparisons:

- Haskell:
  [`toml-parser`](https://hackage.haskell.org/package/toml-parser),
  [`tomland`](https://hackage.haskell.org/package/tomland), and
  [`htoml-megaparsec`](https://hackage.haskell.org/package/htoml-megaparsec).
- C: [tomlc99](https://github.com/cktan/tomlc99).
- Rust: [`toml`](https://crates.io/crates/toml) and
  [`toml_edit`](https://crates.io/crates/toml_edit) (the latter
  preserves formatting, so it's the right comparison if you care
  about lossless round-trips).

## License

BSD-3-Clause.

## References

- [TOML 1.0.0 specification](https://toml.io/en/v1.0.0)
- [TOML 1.1.0 (in progress)](https://toml.io/en/v1.1.0)
- [toml-lang/toml-test](https://github.com/toml-lang/toml-test) (the official conformance suite)
