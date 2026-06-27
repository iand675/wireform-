# wireform-capnproto

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

[Cap'n Proto](https://capnproto.org/) for Haskell. Encode and decode
the dynamic [`CapnProto.Value`](src/CapnProto/Value.hs), derive
typeclass instances generically or via Template Haskell, parse `.capnp`
schema files, generate Haskell types from them, and use a
`[capnp| ... |]` quasiquoter for inline schemas.

Cap'n Proto is Kenton Varda's serialization framework, designed after
he left Google having built protobuf v2. The goals are similar to
FlatBuffers (zero-copy reads, mmap-friendly buffers) but the wire
format is different: structs are split into a fixed data section
(scalars, packed by size) and a pointer section (lists, sub-structs,
text). The framework also includes a capability-based RPC system,
schema evolution rules with stricter guarantees than protobuf's, and
a "packed" encoding that compresses runs of zero bytes for the times
when zero-copy isn't a hard requirement.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-capnproto,
  wireform-derive,    -- only if you want the cross-format annotation deriver
```

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-capnproto` to
compile locally. Compiling with the LLVM backend (`-fllvm`) adds
compile time but measurably improves runtime performance.

## Hello world

Working directly with `CapnProto.Value`:

```haskell
import qualified Data.ByteString as BS
import qualified Data.Vector     as V
import qualified CapnProto.Value  as CP
import qualified CapnProto.Encode as CPE
import qualified CapnProto.Decode as CPD

main :: IO ()
main = do
  let val = CP.Struct
        (V.fromList [CP.Int64 42, CP.Float64 3.14])  -- data section
        (V.fromList [CP.Text "hello capnp"])          -- pointer section
      bytes = CPE.encode val
  case CPD.decode bytes of
    Right decoded -> print decoded
    Left  err     -> putStrLn err
```

The runnable version (which also demonstrates list encoding) lives
in [`examples/CapnProtoExample.hs`](../examples/CapnProtoExample.hs).

## What's in here

| Module                | Role                                                      |
|-----------------------|-----------------------------------------------------------|
| `CapnProto.Value`     | Dynamic untyped `Value` ADT (scalars, `Struct` with separate data + pointer sections, `List`, `Text`, `Data`, `Union`) |
| `CapnProto.Encode`    | Low-level encoder                                         |
| `CapnProto.Decode`    | Low-level decoder                                         |
| `CapnProto.Derive`    | `deriveCapnProto` Template Haskell entry point            |
| `CapnProto.Schema`    | `.capnp` schema AST                                       |
| `CapnProto.Parser`    | `parseCapnProto :: Text -> Either String CapnProtoSchema` for `.capnp` files |
| `CapnProto.CodeGen`   | Generate Haskell types and `Encode` / `Decode` instances from a schema |
| `CapnProto.QQ`        | `[capnp| ... |]` quasiquoter                              |
| `CapnProto.Registry`  | Runtime struct-schema registry                            |

## Encode and decode

The low-level entry points work with `CapnProto.Value`:

```haskell
CapnProto.Encode.encode :: Value      -> ByteString
CapnProto.Decode.decode :: ByteString -> Either String Value
```

Cap'n Proto structs are split into a data section (where scalars
live, packed in size order) and a pointer section (where lists,
sub-structs, and text live). The `Struct` constructor in
`CapnProto.Value` reflects that split directly, so you don't lose
the structural information by routing through the dynamic
representation.

## Annotation-driven deriving

`CapnProto.Derive` consumes the cross-format
`Wireform.Derive.Modifier` vocabulary from
[`wireform-derive`](../wireform-derive/README.md). Cap'n Proto field
ordinals come from the same `tag N` annotation other tag-based
formats use:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified CapnProto.Derive as DCP
import Wireform.Derive (tag)

data Person = Person
  { personName :: !Text
  , personAge  :: !Word32
  } deriving stock (Show, Eq, Generic)

{-# ANN type Person ("Person" :: String) #-}
{-# ANN personName (tag 0) #-}
{-# ANN personAge  (tag 1) #-}

DCP.deriveCapnProto ''Person
```

## Cap'n Proto schema and code generation

`.capnp` schema files go through `CapnProto.Parser.parseCapnProto` to
produce a `CapnProtoSchema`, and through `CapnProto.CodeGen` to emit
Haskell types + the corresponding encoder / decoder instances:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import CapnProto.QQ (capnp)

[capnp|
  struct Person {
    name @0 :Text;
    age  @1 :UInt32;
  }
|]
-- Generates: data Person = Person { name :: Text, age :: Word32 }
--            and the corresponding encode / decode instances
```

For external `.capnp` files, the `wireform-gen` CLI in the umbrella
package wraps the same codegen:

```bash
wireform-gen capnp -i schema.capnp -o src/Gen/
```

## Testing

The per-format Hedgehog suite lives in `test/`:

```bash
cabal test wireform-capnproto:wireform-capnproto-derive-test
```

It covers the typeclass instances, the deriver, the dynamic `Value`
ADT, the schema parser, and the code generator output.

## Benchmarks

A criterion harness in [`bench/Bench.hs`](bench/Bench.hs):

```bash
cabal bench wireform-capnproto:wireform-capnproto-bench
```

<!-- BEGIN_AUTOGEN bench:capnproto-encode-decode -->
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="bench-results/charts/capnproto-encode-decode-dark.svg">
  <img src="bench-results/charts/capnproto-encode-decode-light.svg" alt="wireform-capnproto encode + decode (zero-copy decode)">
</picture>

| Operation     |  encode |  decode | ratio |
| :------------ | ------: | ------: | ----: |
| Person struct |  114 ns | 28.4 ns | 0.25x |
| Person[100]   | 8610 ns | 28.0 ns | 0.00x |

<sub>Last run 2026-06-27 11:35:54 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5. Decode is a zero-copy cursor by design: only the outer envelope is resolved at decode time. Per-field reads happen lazily..</sub>
<!-- END_AUTOGEN bench:capnproto-encode-decode -->

For cross-language comparisons:

- Haskell: [`capnp`](https://hackage.haskell.org/package/capnp) (the
  existing Hackage Cap'n Proto library).
- C++: [Cap'n Proto's reference C++ library](https://github.com/capnproto/capnproto),
  the canonical implementation.
- Rust: [`capnp`](https://crates.io/crates/capnp) crate.
- Python: [`pycapnp`](https://pypi.org/project/pycapnp/) on PyPI.

## License

BSD-3-Clause.

## References

- [Cap'n Proto project](https://capnproto.org/)
- [Cap'n Proto encoding spec](https://capnproto.org/encoding.html)
- [Cap'n Proto schema language](https://capnproto.org/language.html)
