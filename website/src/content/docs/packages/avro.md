---
title: wireform-avro
description: "Apache Avro encoding and decoding with schema resolution, Object Container Files, IDL codegen, and logical types."
sidebar:
  order: 30
---

`wireform-avro` implements [Apache Avro](https://avro.apache.org/), a schema-driven
binary format used in Kafka (via Schema Registry), Apache Iceberg manifests, and
Hadoop-era data pipelines. Avro omits type tags on the wire: field order and types
come entirely from the schema. That keeps payloads compact, but it means the schema
must be available at decode time. Use this package when you need writer/reader schema
evolution, self-describing container files, or tight integration with the Confluent
ecosystem.

## Key features

- **Typeclass API** via `ToAvro` and `FromAvro`, with Template Haskell deriving and
  a companion `HasAvroSchema` class that reflects the Avro schema for each type
- **Schema resolution** between writer and reader schemas (added fields with
  defaults, removed fields, reordered fields, type promotions, alias renames)
- **Object Container Files (OCF)** with `null`, `deflate`, and `snappy` codecs
- **Avro IDL parser and codegen** from `.avdl` and `.avsc` schemas
- **Schema fingerprinting** (CRC-64-AVRO and SHA-256) for Schema Registry IDs
- **JSON bridge** for canonical Avro JSON encoding
- **Protocol support** for Avro RPC message envelopes
- **Runtime registry** for dynamic schema lookup
- **Logical types** for decimal, date, time, timestamp, duration, and uuid

## Basic usage

Avro is schema-driven, so the lowest-level encode and decode functions take an
`AvroType` alongside the value. For typed records, derive instances and use the
schema reflection class:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

import Avro.Class (toAvro, fromAvro)
import Avro.Decode (decodeAvro)
import Avro.Derive (deriveAvro, HasAvroSchema, avroSchema)
import Avro.Encode (encodeAvro)
import Data.Int (Int32)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data Person = Person
  { personName :: !Text
  , personAge  :: !Int32
  }
  deriving stock (Show, Eq, Generic)

$(deriveAvro ''Person)

encodePerson :: Person -> ByteString
encodePerson p =
  encodeAvro (avroSchema (Proxy :: Proxy Person)) (toAvro p)

decodePerson :: ByteString -> Either String Person
decodePerson bs = do
  val <- decodeAvro (avroSchema (Proxy :: Proxy Person)) bs
  fromAvro val
```

### Schema resolution

When the reader's schema differs from the writer's (for example, a new field was
added with a default), decode with the writer schema and resolve to the reader's
view:

```haskell
import Avro.Decode (decodeAvroResolved)
import Avro.Schema
import qualified Avro.Value as AV
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

writerSchema :: AvroType
writerSchema = AvroRecord
  { avroRecordName      = "Person"
  , avroRecordNamespace = Nothing
  , avroRecordDoc       = Nothing
  , avroRecordAliases   = V.empty
  , avroRecordFields    = V.fromList
      [ AvroField "name" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
      , AvroField "age"  (AvroPrimitive AvroInt)    Nothing Nothing V.empty Nothing Map.empty
      ]
  , avroRecordProps     = Map.empty
  }

readerSchema :: AvroType
readerSchema = AvroRecord
  { avroRecordName      = "Person"
  , avroRecordNamespace = Nothing
  , avroRecordDoc       = Nothing
  , avroRecordAliases   = V.empty
  , avroRecordFields    = V.fromList
      [ AvroField "name"  (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
      , AvroField "age"   (AvroPrimitive AvroInt)    Nothing Nothing V.empty Nothing Map.empty
      , AvroField "email" (AvroPrimitive AvroString) (Just "\"unknown@example.com\"")
          Nothing V.empty Nothing Map.empty
      ]
  , avroRecordProps     = Map.empty
  }

decodeWithEvolution :: ByteString -> Either String AV.Value
decodeWithEvolution bytes =
  decodeAvroResolved writerSchema readerSchema bytes
```

For self-describing files, write and read Object Container Files:

```haskell
import qualified Avro.Container as OCF

writeRecords :: AvroType -> [AV.Value] -> ByteString
writeRecords schema vals = OCF.writeContainer schema (V.fromList vals)

readRecords :: ByteString -> Either String (AvroType, V.Vector AV.Value)
readRecords bytes = OCF.readContainer bytes
```

## Performance

Encode and decode of a small record and a 100-element batch through the
schema-driven wire codec (the schema is resolved once via the derived
`HasAvroSchema` instance and reused across iterations, as a real
producer/consumer would):

<!-- BEGIN_AUTOGEN bench:avro-encode-decode -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-avro encode + decode (Person record, schema-driven)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-avro encode + decode (Person record, schema-driven)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">5000</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">10000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">15000</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">20000</text>
    </g>
    <g>
      <rect x="171" y="317.7" width="62" height="2.3" rx="2" fill="#0969da"/>
      <rect x="235" y="318.7" width="62" height="1.3" rx="2" fill="#cf222e"/>
      <rect x="481" y="123.8" width="62" height="196.2" rx="2" fill="#0969da"/>
      <rect x="545" y="183.2" width="62" height="136.8" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="202" y="313.7" text-anchor="middle" font-size="10" fill="#1f2328">176</text>
      <text x="266" y="314.7" text-anchor="middle" font-size="10" fill="#1f2328">101</text>
      <text x="512" y="119.8" text-anchor="middle" font-size="10" fill="#1f2328">15094</text>
      <text x="576" y="179.2" text-anchor="middle" font-size="10" fill="#1f2328">10524</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#1f2328">Person</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#1f2328">[Person] x 100</text>
    </g>
    <g>
      <g transform="translate(292, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">encode</text>
      </g>
      <g transform="translate(368, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">decode</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-avro encode + decode (Person record, schema-driven)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">5000</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">10000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">15000</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">20000</text>
    </g>
    <g>
      <rect x="171" y="317.7" width="62" height="2.3" rx="2" fill="#58a6ff"/>
      <rect x="235" y="318.7" width="62" height="1.3" rx="2" fill="#ff7b72"/>
      <rect x="481" y="123.8" width="62" height="196.2" rx="2" fill="#58a6ff"/>
      <rect x="545" y="183.2" width="62" height="136.8" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="202" y="313.7" text-anchor="middle" font-size="10" fill="#e6edf3">176</text>
      <text x="266" y="314.7" text-anchor="middle" font-size="10" fill="#e6edf3">101</text>
      <text x="512" y="119.8" text-anchor="middle" font-size="10" fill="#e6edf3">15094</text>
      <text x="576" y="179.2" text-anchor="middle" font-size="10" fill="#e6edf3">10524</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">Person</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">[Person] x 100</text>
    </g>
    <g>
      <g transform="translate(292, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">encode</text>
      </g>
      <g transform="translate(368, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">decode</text>
      </g>
    </g>
  </g>
</svg>


| Operation      |   encode |   decode | ratio |
| :------------- | -------: | -------: | ----: |
| Person         |   176 ns |   101 ns | 0.57x |
| [Person] x 100 | 15094 ns | 10524 ns | 0.70x |

<sub>Last run 2026-06-27 12:10:15 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:avro-encode-decode -->

The chart and table above are regenerated by [`wireform-stats`](../stats/) from `wireform-avro/bench-results/summary/avro-encode-decode.json` — the same source the README chart is built from. Cross-library comparisons (against the Hackage `avro` package and the Apache reference implementations) are planned.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Avro.Class` | `ToAvro` / `FromAvro` typeclasses |
| `Avro.Encode` / `Avro.Decode` | Schema-driven `encodeAvro` / `decodeAvro` / `decodeAvroResolved` |
| `Avro.Value` | Dynamic untyped `Value` ADT |
| `Avro.Schema` / `Avro.Schema.Parse` | Schema AST and `.avsc` JSON parser |
| `Avro.IDL` / `Avro.IDLConvert` | `.avdl` IDL parser and converter |
| `Avro.Derive` | `deriveAvro`, `deriveHasAvroSchema`, `HasAvroSchema` |
| `Avro.CodeGen` / `Avro.QQ` | Haskell codegen and `[avsc\| ... \|]` quasiquoter |
| `Avro.Container` | Object Container File reader and writer |
| `Avro.Resolution` | Writer/reader schema resolution rules |
| `Avro.Fingerprint` | CRC-64-AVRO and SHA-256 schema fingerprints |
| `Avro.Protocol` | Avro RPC protocol envelope |
| `Avro.Registry` | Runtime schema registry |
| `Avro.JSON` | Avro JSON encoding bridge |
