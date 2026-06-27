---
title: Kafka protocol
description: "Generated Kafka request/response records and the wire codec layer that the native client builds on."
sidebar:
  order: 2
---

`wireform-kafka-protocol` holds the Kafka *wire protocol* layer: the generated
request/response/data-message records (`Kafka.Protocol.Generated.*`) and the
codec primitives (`Kafka.Protocol.Wire.*`) that serialize them. It is the
foundation that the high-level [`wireform-kafka`](../kafka/) client builds on.

The package owns no networking, no producer/consumer logic, and no client
configuration; it has just the types that go on the wire and the code that reads and
writes them.

## Why it is a separate package

The protocol types are *code-generated* from the upstream Apache Kafka message
schemas, and there are a lot of them. Keeping them in their own package means:

- **Haddock and linking stay unambiguous.** The generated modules live under
  `wireform-kafka/src` (shared via `hs-source-dirs`), but exposing them through
  a dedicated package keeps documentation and module resolution clean.
- **Explicit imports.** `wireform-kafka` depends on this package but does *not*
  re-export it. Client code that needs the raw protocol records imports
  `wireform-kafka-protocol` directly.

## Key features

| Layer | Modules | What it does |
|-------|---------|--------------|
| Primitive types | `Kafka.Protocol.Primitives` | The fundamental newtype + smart-constructor vocabulary of the protocol |
| Message class | `Kafka.Protocol.Message` | The typeclass every protocol message implements |
| Wire codec | `Kafka.Protocol.Wire`, `Kafka.Protocol.Wire.Codec`, `Kafka.Protocol.Wire.Primitives` | Direct-poke encode/decode: byte/int helpers, length-prefixed strings/bytes/arrays, UUIDs, tagged fields, varint newtypes, and `WireCodec` runners over the codegen-emitted native pokes |
| Slice vectors | `Kafka.Protocol.Wire.SliceVector` | A compact vector of `ByteString` slices that share a single backing `ForeignPtr Word8` buffer |
| Generated messages | `Kafka.Protocol.Generated.*` | One module per Kafka request, response, and data message |

The generated layer covers the full breadth of the Kafka protocol: `Produce`,
`Fetch`, `Metadata`, `OffsetCommit`/`OffsetFetch`, the consumer-group and
share-group requests, transaction coordination (`InitProducerId`, `AddPartitionsToTxn`,
`EndTxn`, `WriteTxnMarkers`), admin operations (`CreateTopics`, `AlterConfigs`,
ACLs, quotas), SASL handshakes, and the KRaft controller/quorum messages, with
the request and response halves each in their own module
(`Kafka.Protocol.Generated.<Api>Request` / `...Response`). The cabal file
exposes 188 generated `Kafka.Protocol.Generated.*` modules.

## Using the generated types

The generated records are plain Haskell data types. Encode and decode them
through the `Kafka.Protocol.Wire` codec layer:

```haskell
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.MetadataRequest as M
import qualified "wireform-kafka-protocol" Kafka.Protocol.Wire as Wire
```

Client code in `wireform-kafka` imports these protocol modules with
`PackageImports` so GHC does not compile duplicate copies of the
`Kafka.Protocol.Generated.*` tree.

## Code generation and the regen workflow

Every `Kafka.Protocol.Generated.*` module is **output**, not source. The schemas
come from the upstream Apache Kafka tree
(`clients/src/main/resources/common/message/*.json`), and the `kafka-codegen`
executable walks that directory and emits one Haskell module per
request/response/data message.

To regenerate against a Kafka checkout:

```bash
# build the generator
cabal build wireform-kafka:exe:kafka-codegen

# regenerate from a local Kafka source tree
KAFKA_SRC=~/src/kafka ./scripts/regen-kafka-protocol.sh
# or point directly at the message directory:
./scripts/regen-kafka-protocol.sh /path/to/kafka/clients/src/main/resources/common/message
```

The script writes the generated modules under
`wireform-kafka/src/Kafka/Protocol/Generated/`, syncs the message inventory
sidecar into `wireform-kafka/data/`, and runs
`scripts/sync-kafka-protocol-cabal-modules.sh` to update the module list.

:::caution
**Never hand-edit a generated file.** The `kafka-codegen` executable deletes
every existing `.hs` file in the output directory before writing fresh output,
so any manual tweak is silently clobbered on the next regen. Fold the intent
into the codegen (`wireform-kafka/codegen/Kafka/Protocol/Codegen/`) and
regenerate instead.

When importing a newer Kafka schema set, reconcile the
`wireform-kafka-protocol.cabal` `exposed-modules` list in the same change:
every regen-produced module should appear there, and every entry should map to a
regen-produced module. Run `scripts/split-kafka-protocol-package.py` if module
membership changed.
:::

## Notable modules

| Module | Role |
|--------|------|
| `Kafka.Protocol.Primitives` | Core newtype + smart-constructor protocol vocabulary |
| `Kafka.Protocol.Message` | Typeclass implemented by every protocol message |
| `Kafka.Protocol.Wire` | Direct-poke wire codec (byte/int helpers, the base layer) |
| `Kafka.Protocol.Wire.Codec` | `WireCodec` runners over codegen-emitted native pokes |
| `Kafka.Protocol.Wire.Primitives` | `Wire` instances for Kafka length-prefixed types (strings, bytes, arrays, UUIDs, tagged fields, varints) |
| `Kafka.Protocol.Wire.SliceVector` | Vector of byte slices over a single shared `ForeignPtr` |
| `Kafka.Protocol.Generated.*` | One generated module per Kafka request / response / data message |

## Next steps

- **The client:** [`wireform-kafka`](../kafka/) is the high-level producer,
  consumer, admin, and transaction API built on top of these protocol types.
