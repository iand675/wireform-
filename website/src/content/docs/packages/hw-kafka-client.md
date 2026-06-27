---
title: hw-kafka-client compat
description: "A source-compatible facade over the popular hw-kafka-client API, backed by the native wireform Kafka client."
sidebar:
  order: 6
---

`wireform-hw-kafka-client` is a source-compatible facade for the public
[`hw-kafka-client`](https://hackage.haskell.org/package/hw-kafka-client) modules,
backed by the native [`wireform-kafka`](../kafka/) client. It preserves the
legacy module shape, constructor names, and record selectors so existing
`hw-kafka-client` source can compile with minimal changes, while routing the
actual work through the pure-Haskell wireform stack instead of `librdkafka`.

## Who this is for

This package is a **transitional bridge** for applications migrating from the
`librdkafka`-backed `hw-kafka-client` API to the native `wireform-kafka`
modules. It lets you swap the dependency first and migrate call sites
incrementally, rather than rewriting everything at once.

New code should prefer the native modules directly:

- `Kafka.Client.Producer`
- `Kafka.Client.Consumer`
- `Kafka.Client.AdminClient`
- `Kafka.Client.Transaction`

## Key features

- **Familiar surface.** The facade keeps the public `hw-kafka-client` names:
  `KafkaConsumer`, `KafkaProducer`, `ConsumerProperties`, `ProducerProperties`,
  `Subscription`, `ProducerRecord`, `TopicName`, `ConsumerGroupId`, and the
  builder combinators (`brokersList`, `groupId`, `topics`, `offsetReset`,
  `noAutoCommit`, `logLevel`, …).
- **Native backend.** Subscription, polling, assignment, seek, pause/resume, and
  commit operations are routed to `Kafka.Client.Consumer`; sends go through
  `Kafka.Client.Producer`. No JVM and no `librdkafka` FFI in the data path.
- **Compatibility types retained.** Metadata, transaction, callback, and dump
  modules port the legacy public data types so existing imports keep compiling
  during the migration.

## Producer facade

`Kafka.Producer` mirrors the legacy producer surface:

```haskell
import Kafka.Producer

producerProps :: ProducerProperties
producerProps = brokersList ["localhost:9092"]
             <> logLevel KafkaLogInfo

mkMessage :: Maybe ByteString -> Maybe ByteString -> ProducerRecord
mkMessage k v = ProducerRecord
  { prTopic     = TopicName "events"
  , prPartition = UnassignedPartition
  , prKey       = k
  , prValue     = v
  , prHeaders   = mempty
  }
```

It exposes `runProducer`, `newProducer`, `produceMessage`, `produceMessage'`,
`flushProducer`, and `closeProducer`, preserving the old constructor and
function names while routing sends through the native wireform producer.

## Consumer facade

`Kafka.Consumer` mirrors the legacy consumer surface:

```haskell
import Kafka.Consumer

consumerProps :: ConsumerProperties
consumerProps = brokersList ["localhost:9092"]
             <> groupId (ConsumerGroupId "consumer-example")
             <> noAutoCommit

consumerSub :: Subscription
consumerSub = topics [TopicName "events"] <> offsetReset Earliest
```

It exposes the familiar lifecycle and offset operations: `runConsumer`,
`newConsumer`, `assign`, `assignment`, `subscription`, `pausePartitions`,
`resumePartitions`, `committed`, `position`, `seek`, `seekPartitions`,
`pollMessage`, `pollMessageBatch`, `commitOffsetMessage`, `commitAllOffsets`,
`commitPartitionsOffsets`, `storeOffsets`, `rewindConsumer`, and
`closeConsumer`, all backed by the native consumer.

## Migration notes

| Legacy concern | What the facade does |
|----------------|----------------------|
| Metadata queries | `Kafka.Metadata` ports the public metadata data types so imports compile. The old functions were `librdkafka` metadata queries; for real metadata, group, and topic queries use `Kafka.Client.AdminClient`. |
| Transactions | `Kafka.Transaction` is present so legacy imports compile. The native stack models transactions through `Kafka.Client.Transaction` and explicit transaction handles. |
| Callbacks | `Kafka.Callbacks` preserves the legacy error / log / statistics callback builders. Error callbacks fire on facade-level create/send failures; log and statistics callbacks are retained as configuration values. |
| Config dump | `Kafka.Dump` can return the compatibility property maps it stores, but native wireform handles do not have `librdkafka`'s complete runtime dump. |

The facade depends on `wireform-kafka` and reuses its compression types
(`Kafka.Compression.Types`), so compression and other native capabilities are
available underneath the legacy API.

## Notable modules

| Module | Role |
|--------|------|
| `Kafka.Producer` | Transitional producer facade over `Kafka.Client.Producer` |
| `Kafka.Producer.ProducerProperties` | Legacy producer config builders |
| `Kafka.Producer.Types` | Legacy producer types (`ProducerRecord`, etc.) |
| `Kafka.Consumer` | Transitional consumer facade over `Kafka.Client.Consumer` |
| `Kafka.Consumer.ConsumerProperties` | Legacy consumer config builders |
| `Kafka.Consumer.Subscription` | Legacy `Subscription` builders (`topics`, `offsetReset`) |
| `Kafka.Consumer.Types` | Legacy consumer types (`KafkaConsumer`, offsets) |
| `Kafka.Types` | Shared compatibility types (public names, constructors, selectors) |
| `Kafka.Metadata` | Ported metadata data types for migration |
| `Kafka.Transaction` | Legacy transaction API surface (migration shim) |
| `Kafka.Callbacks` | Legacy error / log / statistics callback builders |
| `Kafka.Dump` | Legacy configuration dump helpers |

## Next steps

- **The native client:** [`wireform-kafka`](../kafka/) is the producer,
  consumer, admin, and transaction API this facade delegates to, and where new
  code should go.
