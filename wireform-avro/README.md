# wireform-avro

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

[Apache Avro](https://avro.apache.org/) for Haskell. The wire format,
the dynamic [`Avro.Value`](src/Avro/Value.hs), the
annotation-driven Template Haskell deriver, the `.avsc` JSON schema
parser, the `.avdl` IDL parser and converter, code generation,
schema fingerprinting, schema resolution between writer and reader
schemas, the Avro Object Container File (OCF) reader and writer, the
Avro IPC / RPC protocol, a JSON bridge, and an `[avsc| ... |]`
quasiquoter.

Avro is the schema-driven serialization format that came out of
Hadoop and now anchors the Confluent Kafka ecosystem (via Schema
Registry), Apache Iceberg's manifest files, and most pipelines that
need a writer-reader schema split. The wire format is small and
schema-dependent: there are no type tags on the wire, just the values
in the order the schema dictates. That makes Avro fast and tight, but
it also means the schema has to be present at decode time, which is
why container files and Schema Registry exist.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-avro,
  wireform-derive,    -- only if you want the cross-format annotation deriver
```

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-avro` to compile
locally. Compiling with the LLVM backend (`-fllvm`) adds compile time
but measurably improves runtime performance.

## Hello world

Avro is schema-driven, so the lowest-level entry points take an
`AvroType` alongside the value:

```haskell
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Vector     as V
import qualified Avro.Value  as A
import           Avro.Encode (encodeAvro)
import           Avro.Decode (decodeAvro)
import           Avro.Schema

main :: IO ()
main = do
  let schema = AvroRecord
        { avroRecordName = "Person"
        , avroRecordNamespace = Nothing
        , avroRecordDoc       = Nothing
        , avroRecordAliases   = V.empty
        , avroRecordFields    = V.fromList
            [ AvroField "name" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
            , AvroField "age"  (AvroPrimitive AvroInt)    Nothing Nothing V.empty Nothing Map.empty
            ]
        , avroRecordProps     = Map.empty
        }
      val   = A.Record (V.fromList [A.String "Charlie", A.Int 42])
      bytes = encodeAvro schema val
  case decodeAvro schema bytes of
    Right decoded -> print decoded
    Left  err     -> putStrLn err
```

The runnable version lives in [`examples/AvroExample.hs`](../examples/AvroExample.hs).

For typed records, derive the `ToAvro` / `FromAvro` instances
instead of constructing the schema by hand:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Avro.Derive as DAvro

data Person = Person
  { name :: !Text
  , age  :: !Int32
  } deriving stock (Show, Eq, Generic)

DAvro.deriveAvro          ''Person
DAvro.deriveHasAvroSchema ''Person
```

## What's in here

| Module               | Role                                                      |
|----------------------|-----------------------------------------------------------|
| `Avro.Value`         | Dynamic untyped `Value` ADT (`Null`, `Boolean`, `Int`, `Long`, `Float`, `Double`, `Bytes`, `String`, `Record`, `Enum`, `Array`, `Map`, `Union`, `Fixed`) |
| `Avro.Wire`          | Avro wire-level primitives (zigzag varints, etc.)         |
| `Avro.Encoding`      | The `Encoding` builder type used by `ToAvro` instances    |
| `Avro.Encode`        | Schema-driven encoder: `encodeAvro :: AvroType -> Value -> ByteString` |
| `Avro.Decode`        | Schema-driven decoder: `decodeAvro :: AvroType -> ByteString -> Either String Value` |
| `Avro.Class`         | Public `ToAvro` / `FromAvro` typeclasses                  |
| `Avro.Derive`        | `deriveAvro` / `deriveToAvro` / `deriveFromAvro` / `deriveHasAvroSchema` Template Haskell entry points + `encodeAvro` / `decodeAvro` typeclass wrappers |
| `Avro.Schema`        | Avro schema AST (`AvroType`, `AvroSchema`, `AvroField`, ...) |
| `Avro.Schema.Parse`  | `.avsc` JSON schema parser                                |
| `Avro.IDL`           | `.avdl` (Avro IDL) parser                                 |
| `Avro.IDLConvert`    | Translate parsed `.avdl` into `AvroSchema`                |
| `Avro.CodeGen`       | Generate Haskell types and `ToAvro` / `FromAvro` instances from a schema |
| `Avro.QQ`            | `[avsc| ... |]` and `[avdl| ... |]` quasiquoters          |
| `Avro.Container`     | Avro Object Container File (OCF) reader and writer        |
| `Avro.Resolution`    | Schema resolution between writer and reader schemas       |
| `Avro.Fingerprint`   | CRC-64-AVRO and SHA-256 schema fingerprinting             |
| `Avro.Protocol`      | Avro RPC / IPC protocol envelope                          |
| `Avro.Registry`      | Runtime schema registry (used by JSON + dynamic decoders) |
| `Avro.JSON`          | Bridge to and from `aeson`'s `Value`                      |

## Encode and decode

Two layers, both exposed:

```haskell
-- Schema-driven, dynamic Value <-> bytes
Avro.Encode.encodeAvro :: AvroType -> Value      -> ByteString
Avro.Decode.decodeAvro :: AvroType -> ByteString -> Either String Value

-- Typeclass: derived encoder / decoder for your records
Avro.Derive.encodeAvro :: ToAvro   a => a          -> ByteString
Avro.Derive.decodeAvro :: FromAvro a => ByteString -> Either String a
```

The schema-driven path is the right entry when you're talking to a
schema you don't own (Schema Registry, third-party `.avsc` file).
The typeclass path is the right entry when you control the Haskell
type and want the schema generated from it.

## Annotation-driven deriving

`Avro.Derive` consumes the cross-format `Wireform.Derive.Modifier`
vocabulary from [`wireform-derive`](../wireform-derive/README.md).
Avro field names are conventionally snake_case, which the
`renameStyle SnakeCase` annotation produces:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified Avro.Derive          as DAvro
import qualified Wireform.Derive.Aeson as DAeson
import Wireform.Derive (renameStyle, SnakeCase)

data Person = Person
  { personFullName :: !Text
  , personAge      :: !Word32
  } deriving stock (Show, Eq, Generic)

{-# ANN type Person ("Person" :: String) #-}
{-# ANN personFullName (renameStyle SnakeCase) #-}
{-# ANN personAge      (renameStyle SnakeCase) #-}

DAvro.deriveAvro          ''Person
DAvro.deriveHasAvroSchema ''Person
DAeson.deriveJSON         ''Person
```

## Schema, IDL, and code generation

Two schema languages are supported. The JSON-shaped `.avsc` is the
canonical schema dialect (parsed by `Avro.Schema.Parse`), and the
DSL-shaped `.avdl` is the IDL dialect (parsed by `Avro.IDL`,
converted to `AvroSchema` by `Avro.IDLConvert`). Both feed
`Avro.CodeGen`, which emits Haskell types + `ToAvro` / `FromAvro` /
`HasAvroSchema` instances.

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Avro.QQ (avsc)

[avsc|
  { "type": "record"
  , "name": "Person"
  , "fields":
      [ { "name": "name", "type": "string" }
      , { "name": "age",  "type": "int" }
      ]
  }
|]
-- Generates: data Person = Person { name :: Text, age :: Int32 }
--            instance ToAvro Person ; instance FromAvro Person
```

For external `.avsc` and `.avdl` files, the `wireform-gen` CLI in
the umbrella package wraps the same codegen:

```bash
wireform-gen avro -i person.avsc -o src/Gen/
```

## Container files (OCF)

Avro's Object Container File format wraps a writer schema and a
sequence of records into a single self-describing file. Used
extensively as the on-disk format for Iceberg manifests, Kafka
Connect's filesystem sink, and historical Hadoop pipelines.

```haskell
import qualified Avro.Container as OCF

let bytes = OCF.writeContainer schema records
case OCF.readContainer bytes of
  Right (writerSchema, records') -> ...
  Left  err                      -> ...
```

`writeContainerWith` lets you pick a codec name (`null`, `deflate`,
`snappy`, `bzip2`, ...). `readContainerResolved` accepts a reader
schema and resolves the writer schema against it on the fly via
`Avro.Resolution`.

## Schema resolution and fingerprinting

`Avro.Resolution` implements the standard Avro schema-resolution
rules: a reader schema may differ from the writer schema in
well-defined ways (added fields with defaults, removed fields,
reordered fields, type promotions, alias renames), and the resolver
materialises the reader's view of the writer's payload.

`Avro.Fingerprint` produces the canonical CRC-64-AVRO 8-byte
fingerprint and the SHA-256 32-byte fingerprint described in the
spec, suitable for use as Schema Registry IDs or for the
`Single Object Encoding` envelope.

## Avro IPC / Protocol

`Avro.Protocol` covers the protocol envelope used by Avro RPC: the
handshake, the per-message envelope, and the request / response
framing. The transport layer (HTTP / Netty / etc.) is left to the
caller.

## JSON bridge

`Avro.JSON` round-trips between `Avro.Value` and `Data.Aeson.Value`
following the canonical JSON encoding rules in the Avro spec
(unions render as `{ "<type>": <value> }`, bytes as base64 strings,
fixed as base64 strings, etc.).

## Testing

The per-format Hedgehog suite lives in `test/`:

```bash
cabal test wireform-avro:wireform-avro-derive-test
```

It covers the schema-driven encoder / decoder, the typeclass
instances, the deriver, both schema parsers, schema resolution, the
container file reader / writer, the fingerprint algorithms, and the
JSON bridge.

## Benchmarks

Encode + decode of a small record and a 100-element batch, against the
schema-driven wire codec (`cabal bench wireform-avro:wireform-avro-bench`):

<!-- BEGIN_AUTOGEN bench:avro-encode-decode -->
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="bench-results/charts/avro-encode-decode-dark.svg">
  <img src="bench-results/charts/avro-encode-decode-light.svg" alt="wireform-avro encode + decode (Person record, schema-driven)">
</picture>

| Operation      |   encode |   decode | ratio |
| :------------- | -------: | -------: | ----: |
| Person         |   176 ns |   101 ns | 0.57x |
| [Person] x 100 | 15094 ns | 10524 ns | 0.70x |

<sub>Last run 2026-06-27 12:10:15 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:avro-encode-decode -->

Planned cross-language / cross-library comparisons:

- Haskell: [`avro`](https://hackage.haskell.org/package/avro) (the
  established Haskell Avro library).
- Java: [Apache Avro reference implementation](https://github.com/apache/avro/tree/main/lang/java),
  the canonical implementation.
- Python: [`avro` / `fastavro`](https://github.com/fastavro/fastavro)
  on PyPI.
- Rust: [`apache-avro`](https://crates.io/crates/apache-avro).
- C: [`avro-c`](https://github.com/apache/avro/tree/main/lang/c).

> Numbers TBD: harness pending.

## License

BSD-3-Clause.

## References

- [Apache Avro specification](https://avro.apache.org/docs/1.11.1/specification/)
- [Avro IDL](https://avro.apache.org/docs/1.11.1/idl-language/)
- [Apache Avro project](https://avro.apache.org/)
