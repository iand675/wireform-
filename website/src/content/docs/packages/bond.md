---
title: wireform-bond
description: "Microsoft Bond Compact Binary v1 with IDL codegen, packed field headers, and ZigZag varint encoding."
sidebar:
  order: 32
---

`wireform-bond` implements [Microsoft Bond](https://github.com/microsoft/bond)
Compact Binary serialization. Bond uses explicit numeric field IDs for schema
evolution, supports nullable types and struct inheritance in the IDL, and encodes
with packed delta/type headers and ZigZag varints. Use this package when you need
wire compatibility with Bond-based services (Bing, Cosmos DB, and other Microsoft
systems) or a compact, ID-driven binary format with a rich schema language.

## Key features

- **Typeclass API** via `ToBond` and `FromBond` with Template Haskell deriving
- **Compact Binary v1 wire format** with packed delta/type field headers
- **ZigZag varints** for signed integer encoding
- **Bond IDL parser and codegen** from `.bond` schema files
- **Schema AST** with type parameters, field modifiers, and custom attributes
- **QuasiQuoter** for inline `[bond| ... |]` schemas

## Basic usage

Derive instances for your record types, then encode through the Compact Binary
codec:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

import Bond.Decode qualified as BD
import Bond.Derive (ToBond (..), FromBond (..), deriveBond)
import Bond.Encode qualified as BE
import Bond.Value qualified as BV
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)

data Person = Person
  { personName  :: !Text
  , personAge   :: !Int32
  , personEmail :: !Text
  }
  deriving stock (Show, Eq, Generic)

$(deriveBond ''Person)

encodePerson :: Person -> ByteString
encodePerson = BE.encode . toBond

decodePerson :: ByteString -> Either String Person
decodePerson bs = do
  val <- BD.decode BV.BT_STRUCT bs
  fromBond val

alice :: Person
alice = Person "Alice" 30 "alice@example.com"

roundTrip :: Either String Person
roundTrip = decodePerson (encodePerson alice)
```

For schema-first workflows, parse Bond IDL and generate Haskell types:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Bond.QQ (bond)

[bond|
  struct Person {
    1: string name;
    2: int32 age;
    3: string email;
  }
|]
```

```bash
wireform-gen bond -i schema.bond -o src/Gen/
```

## Performance

### Compact Binary encode/decode

| Payload | encode | decode |
|---------|--------|--------|
| Person | 162 ns | 185 ns |
| [Person] x 100 | 15.7 µs | 2.1 µs |

Decode is structurally lazy at the Value layer: the 100-element list decode resolves only the outer container, deferring per-element materialization until access. This makes random-access patterns fast for large payloads.

Criterion, GHC 9.8.4, Apple Silicon. See `wireform-bond/bench-results/` for raw data.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Bond.Derive` | `ToBond` / `FromBond` classes and `deriveBond` |
| `Bond.Encode` / `Bond.Decode` | Compact Binary v1 encoder and decoder |
| `Bond.Value` | Dynamic untyped `Value` ADT with Bond type tags |
| `Bond.Schema` / `Bond.Parser` | Schema AST and `.bond` IDL parser |
| `Bond.CodeGen` / `Bond.QQ` | Haskell codegen and quasiquoter |
| `Bond.Registry` | Runtime schema registry |
