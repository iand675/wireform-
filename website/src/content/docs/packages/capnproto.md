---
title: wireform-capnproto
description: "Cap'n Proto zero-copy serialization with IDL codegen and segment-based wire layout."
sidebar:
  order: 33
---

`wireform-capnproto` implements [Cap'n Proto](https://capnproto.org/), Kenton
Varda's zero-copy serialization framework. Cap'n Proto splits structs into a fixed
data section (scalars packed by size) and a pointer section (text, lists, nested
structs), making buffers directly mappable for read-heavy workloads. Use this
package when you need mmap-friendly serialization with strict schema evolution
rules, or when integrating with Cap'n Proto services and `.capnp` schema files.

## Key features

- **Typeclass API** via `ToCapnProto` and `FromCapnProto` with Template Haskell deriving
- **Cap'n Proto IDL parser and codegen** from `.capnp` schema files
- **Segment-based wire layout** with separate data and pointer sections
- **Zero-copy-oriented decode** that reconstructs values from mapped buffers
- **QuasiQuoter** for inline `[capnp| ... |]` schemas
- **Runtime registry** for struct schema lookup

## Basic usage

For typed records, derive instances and round-trip through the segment encoder:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

import CapnProto.Decode qualified as CPD
import CapnProto.Derive (ToCapnProto (..), FromCapnProto (..), deriveCapnProto)
import CapnProto.Encode qualified as CPE
import Data.Text (Text)
import Data.Word (Word32)
import GHC.Generics (Generic)

data Person = Person
  { personName :: !Text
  , personAge  :: !Word32
  }
  deriving stock (Show, Eq, Generic)

$(deriveCapnProto ''Person)

encodePerson :: Person -> ByteString
encodePerson = CPE.encode . toCapnProto

decodePerson :: ByteString -> Either String Person
decodePerson bs = do
  val <- CPD.decode bs
  fromCapnProto val

bob :: Person
bob = Person "Bob" 42

roundTrip :: Either String Person
roundTrip = decodePerson (encodePerson bob)
```

You can also work directly with the dynamic `Value` ADT when exploring wire
layout or bridging between schemas:

```haskell
import qualified Data.Vector as V
import qualified CapnProto.Value as CP

manualStruct :: CP.Value
manualStruct = CP.Struct
  (V.fromList [CP.UInt32 42])           -- data section
  (V.fromList [CP.Text "hello capnp"]) -- pointer section

manualBytes :: ByteString
manualBytes = CPE.encode manualStruct
```

Generate types from `.capnp` files with the quasiquoter or CLI:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import CapnProto.QQ (capnp)

[capnp|
  struct Person {
    name @0 :Text;
    age @1 :UInt32;
  }
|]
```

```bash
wireform-gen capnp -i schema.capnp -o src/Gen/
```

## Performance

### Encode/decode (zero-copy decode)

| Shape | encode | decode |
|-------|--------|--------|
| Person struct | 108 ns | 27 ns |
| Person[100] | 8.5 µs | 27 ns |

Decode is effectively O(1) regardless of payload size because Cap'n Proto uses zero-copy cursors: only the outer envelope is resolved at decode time, and per-field reads happen lazily on access. Encode is proportional to message size.

Criterion, GHC 9.8.4, Apple Silicon. See `wireform-capnproto/bench-results/` for raw data.

## Notable modules

| Module | Purpose |
|--------|---------|
| `CapnProto.Derive` | `ToCapnProto` / `FromCapnProto` and `deriveCapnProto` |
| `CapnProto.Encode` / `CapnProto.Decode` | Segment encoder and decoder |
| `CapnProto.Value` | Dynamic untyped `Value` ADT (data + pointer sections) |
| `CapnProto.Schema` / `CapnProto.Parser` | Schema AST and `.capnp` parser |
| `CapnProto.CodeGen` / `CapnProto.QQ` | Haskell codegen and quasiquoter |
| `CapnProto.Registry` | Runtime struct schema registry |
