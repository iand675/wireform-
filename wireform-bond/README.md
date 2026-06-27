# wireform-bond

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

[Microsoft Bond](https://github.com/microsoft/bond) compact-binary
serialization for Haskell. Encode and decode the dynamic
[`Bond.Value`](src/Bond/Value.hs), derive typeclass instances generically
or via Template Haskell, parse `.bond` schema files, generate Haskell
types from them, and use a `[bond| ... |]` quasiquoter for inline
schemas.

Bond is a schema-driven binary format Microsoft developed and uses
heavily in Bing, Cosmos DB, and other internal systems before
open-sourcing the framework. Field IDs are explicit and numeric (so
the wire is forward / backward compatible like protobuf), but the
type system also supports `nullable<T>`, `bonded<T>` (carrying a
sub-tree without forcing a deserialize), inheritance, and custom
attributes that survive the schema round trip. The default wire
encoding is Compact Binary, which is roughly the size of protobuf for
similar-shaped data.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-bond,
  wireform-derive,    -- only if you want the cross-format annotation deriver
```

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-bond` to compile
locally. Compiling with the LLVM backend (`-fllvm`) adds compile time
but measurably improves runtime performance.

## Hello world

Working directly with `Bond.Value`, building a small struct and
round-tripping it through Compact Binary:

```haskell
import qualified Data.ByteString as BS
import qualified Data.Vector     as V
import qualified Bond.Value  as B
import qualified Bond.Encode as BE
import qualified Bond.Decode as BD

main :: IO ()
main = do
  let person = B.Struct V.empty $ V.fromList
        [ (1, B.BT_STRING, B.String "Frank")
        , (2, B.BT_INT32,  B.Int32 45)
        , (3, B.BT_BOOL,   B.Bool True)
        ]
      bytes  = BE.encode person
  case BD.decode B.BT_STRUCT bytes of
    Right val -> print val
    Left  err -> putStrLn err
```

The runnable version (with a longer struct including a list field)
lives in [`examples/BondExample.hs`](../examples/BondExample.hs).

## What's in here

| Module           | Role                                                      |
|------------------|-----------------------------------------------------------|
| `Bond.Value`     | Dynamic untyped `Value` ADT keyed by Bond type tags (`BT_BOOL`, `BT_INT*`, `BT_FLOAT`, `BT_STRING`, `BT_STRUCT`, `BT_LIST`, `BT_SET`, `BT_MAP`, `BT_BONDED`, ...) |
| `Bond.Encode`    | Low-level Compact Binary encoder: `encode :: Value -> ByteString` |
| `Bond.Decode`    | Low-level Compact Binary decoder                          |
| `Bond.Derive`    | `deriveBond` / `deriveToBond` / `deriveFromBond` Template Haskell entry points |
| `Bond.Schema`    | Bond schema AST (`BondSchema`, `BondStruct`, `BondField`, `BondAttribute`, ...) |
| `Bond.Parser`    | `parseBond :: Text -> Either String BondSchema` for `.bond` schema files |
| `Bond.CodeGen`   | Generate Haskell types and `ToBond` / `FromBond` instances from a Bond schema |
| `Bond.QQ`        | `[bond| ... |]` quasiquoter                               |
| `Bond.Registry`  | Runtime struct schema registry                            |

## Encode and decode

The low-level entry points work with `Bond.Value`:

```haskell
Bond.Encode.encode :: Value           -> ByteString
Bond.Decode.decode :: BondType -> ByteString -> Either String Value
```

The decoder takes the expected top-level Bond type because Bond's
Compact Binary doesn't tag the root, only the fields inside it.

## Annotation-driven deriving

`Bond.Derive` consumes the cross-format `Wireform.Derive.Modifier`
vocabulary from [`wireform-derive`](../wireform-derive/README.md). Bond
field IDs are required and come from the same `tag N` annotation the
proto and Thrift derivers use:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified Bond.Derive as DBond
import Wireform.Derive (tag)

data Person = Person
  { personName :: !Text
  , personAge  :: !Int32
  } deriving stock (Show, Eq, Generic)

{-# ANN type Person ("Person" :: String) #-}
{-# ANN personName (tag 1) #-}
{-# ANN personAge  (tag 2) #-}

DBond.deriveBond ''Person
```

## Bond schema and code generation

`.bond` schema files go through `Bond.Parser.parseBond` to produce a
`BondSchema`, and through `Bond.CodeGen` to emit Haskell types +
`ToBond` / `FromBond` instances:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Bond.QQ (bond)

[bond|
  struct Person {
    0: string name;
    1: int32  age;
  }
|]
-- Generates: data Person = Person { name :: Text, age :: Int32 }
--            instance ToBond Person ; instance FromBond Person
```

For external `.bond` files, the `wireform-gen` CLI in the umbrella
package wraps the same codegen:

```bash
wireform-gen bond -i schema.bond -o src/Gen/
```

## Testing

The per-format Hedgehog suite lives in `test/`:

```bash
cabal test wireform-bond:wireform-bond-derive-test
```

It covers the typeclass instances, the deriver, the dynamic `Value`
ADT, the schema parser, and the code generator output.

## Benchmarks

A criterion harness in [`bench/Bench.hs`](bench/Bench.hs):

```bash
cabal bench wireform-bond:wireform-bond-bench
```

<!-- BEGIN_AUTOGEN bench:bond-encode-decode -->
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="bench-results/charts/bond-encode-decode-dark.svg">
  <img src="bench-results/charts/bond-encode-decode-light.svg" alt="wireform-bond encode + decode (Compact Binary, Person record)">
</picture>

| Operation      |   encode |  decode | ratio |
| :------------- | -------: | ------: | ----: |
| single Person  |   168 ns |  192 ns | 1.14x |
| [Person] x 100 | 15742 ns | 2140 ns | 0.14x |

<sub>Last run 2026-06-27 11:35:54 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5. Decode is structurally lazy at the Value layer; the [Person] x 100 number reflects only the outer container resolution..</sub>
<!-- END_AUTOGEN bench:bond-encode-decode -->

For cross-language comparisons:

- Haskell: no comparable Bond library on Hackage.
- C++: [Microsoft's reference Bond library](https://github.com/microsoft/bond)
  (Compact Binary v1 + v2, Simple Binary, Fast Binary).
- C#: the same reference library's .NET binding.
- Python: the same reference library's Python binding.

## License

BSD-3-Clause.

## References

- [Microsoft Bond](https://github.com/microsoft/bond)
- [Bond user manual](https://microsoft.github.io/bond/manual/bond_cpp.html)
- [Bond schema definition language](https://microsoft.github.io/bond/manual/compiler.html#idl-syntax)
