---
title: Kafka client
description: "Pure-Haskell native Apache Kafka client: producer, consumer, admin, transactions, and authentication."
sidebar:
  order: 1
---

`wireform-kafka` is a native Haskell client for the Apache Kafka wire protocol.
It talks directly to Kafka brokers over TCP or TLS, with no JVM, no
`librdkafka`, and no FFI shim in the data path. Everything from connection
management to SASL authentication to record-batch compression is implemented in
Haskell.

## What the package provides

| Layer | Modules | What it does |
|-------|---------|--------------|
| Wire protocol | `Kafka.Protocol.*` | Varints, compact strings, tagged fields, CRC32C (hardware-accelerated), version negotiation |
| Generated messages | `Kafka.Protocol.Generated.*` | One module per Kafka API key, emitted from upstream JSON schemas by `kafka-codegen` |
| Networking | `Kafka.Network.*` | TCP / TLS connections, SASL handshake (PLAIN, SCRAM-SHA-256/512, OAUTHBEARER, AWS MSK IAM) |
| Magic-ring transport | `Kafka.Network.RingTransport`, `Kafka.Network.FrameParser` | Bridges a broker `Network.Connection` onto the [`wireform-network`](../network/) magic-ring transport; streaming frame parser reads zero-copy slices off the ring (60-65 % faster end-to-end than the classic per-frame `connectionGetExact` + `runGet` shape; see the [benchmarks](../network/#kafka)) |
| Compression | `Kafka.Compression.*`, `Kafka.Compression.Ring` | gzip, snappy, lz4, zstd record-batch codecs; the `Ring` variant takes a raw `Ptr Word8` source (e.g. a ring slice) and writes plaintext straight into a caller-supplied destination magic ring via direct `libz` / `liblz4` / `libzstd` / `libsnappy` FFI |
| High-level client | `Kafka.Client.*` | Producer, Consumer, AdminClient, Transaction |
| Mock broker | `Kafka.Client.Mock.*` | Deterministic in-process broker for tests |
| Telemetry | `Kafka.Telemetry.OpenTelemetry` | Semantic-convention spans for produce/consume/admin |

The umbrella module `Kafka` re-exports the high-level client surface so you can
get started with a single import.

## Producing records

A `Producer` maintains a connection pool and a background sender thread that
batches records for efficiency. The typical lifecycle uses a bracket:

```haskell
import Kafka

main :: IO ()
main = do
  let cfg = defaultProducerConfig
        { producerBootstrap = "localhost:9092"
        }
  withProducer cfg $ \producer -> do
    result <- sendMessage producer ProducerRecord
      { prTopic     = "events"
      , prKey       = Just "user-42"
      , prValue     = "{\"action\":\"login\"}"
      , prHeaders   = mempty
      , prPartition = Nothing
      , prTimestamp = Nothing
      }
    case result of
      Right meta -> putStrLn $ "Wrote to partition " <> show (rmPartition meta)
      Left  err  -> putStrLn $ "Send failed: " <> err
```

### Typed produces with `Serde`

If you define a `Topic` with key and value types, `publish` handles
serialization automatically via `HasSerde`:

```haskell
publish producer myTopic myKey myValue
```

### Delivery guarantees

Set `producerDelivery` on the config:

| Value | Meaning |
|-------|---------|
| `AtMostOnce` | Fire and forget. Fastest, may lose records. |
| `AtLeastOnce` | Retries until ack. Default. Records may be duplicated on retry. |
| `ExactlyOnce` | Idempotent producer + transactions. No duplicates, no loss. |

### Flushing

`flushProducer` blocks until every buffered record has been sent or has failed.
Call it before shutdown if you need delivery confirmation.

## Consuming records

The consumer manages group membership, partition assignment, and offset commits.
Two APIs are available:

### Handler-based (recommended)

`runConsumer` from `Kafka.Client.Group` takes a per-record handler and manages
the poll loop, rebalancing, and commit cycle for you:

```haskell
import Kafka

main :: IO ()
main = do
  let cfg = defaultGroupConfig
        { groupConsumerConfig = defaultConsumerConfig
            { consumerBootstrap = "localhost:9092"
            }
        , groupId = "my-service"
        , groupTopics = ["events"]
        }
  runConsumer cfg $ \record -> do
    putStrLn $ "Got: " <> show (crValue record)
```

`runBatchedConsumer` gives you the full `ConsumerRecords` batch per poll cycle
when you need to process records in bulk.

### Manual poll loop

`withConsumer` gives you a `Consumer` handle for fine-grained control:

```haskell
withConsumer cfg $ \consumer -> do
  subscribe consumer ["events"]
  forever $ do
    records <- poll consumer 1000
    mapM_ process (consumerRecordsAll records)
    commitSync consumer
```

### Offset management

| Function | Behavior |
|----------|----------|
| `commitSync` | Block until offsets are committed |
| `commitAsync` | Fire-and-forget commit |
| `commitSyncOffsets` | Commit specific partition/offset pairs |
| `seek` / `seekToBeginning` / `seekToEnd` | Rewind or fast-forward |
| `offsetsForTimes` | Find offsets by timestamp |

### Auto-commit

Enabled by default (`consumerAutoCommit = True`). Disable it when you need
explicit control over when offsets advance.

## Transactions

Transactions give you atomic multi-partition produces combined with consumer
offset commits. This is how you build exactly-once pipelines.

```haskell
import Kafka

main :: IO ()
main = do
  let cfg = defaultProducerConfig
        { producerBootstrap    = "localhost:9092"
        , producerTransactional = Just "my-txn-id"
        , producerIdempotent   = True
        }
  withProducer cfg $ \producer -> do
    txn <- bindTransaction producer
    initTransactions txn
    withTransaction txn $ do
      sendInTransaction txn (ProducerRecord { .. })
      commitOffsetsInTransaction txn consumerGroupMeta offsets
```

`withTransaction` calls `beginTransaction`, runs your action, and either commits
or aborts on exception. The transaction coordinator on the broker ensures that
either all partitions see the records and offset commits, or none do.

## Admin operations

`AdminClient` provides control-plane operations:

```haskell
withAdminClient defaultAdminClientConfig { adminBootstrap = "localhost:9092" } $ \admin -> do
  createTopics admin [NewTopic "events" 6 3]
  topics <- listTopics admin
  groups <- listConsumerGroups admin
  describeConfigs admin [ConfigResource BrokerResource "0"]
```

Supported operations include topic CRUD, consumer group management, config
inspection and mutation, ACL management, log dir inspection, partition
reassignment, transaction control, and cluster metadata.

## Authentication

TLS and SASL are configured on the `ConnectionConfig`, which is shared across
producer, consumer, and admin:

```haskell
let conn = defaultConnectionConfig
      { connBootstrap = "broker.example.com:9094"
      , connUseTls = True
      , connSasl = Just (SaslScram ScramSha256 "user" "pass")
      }
```

| Mechanism | Constructor | Notes |
|-----------|-------------|-------|
| PLAIN | `SaslPlain user pass` | Username/password in the clear (use with TLS) |
| SCRAM-SHA-256/512 | `SaslScram alg user pass` | Challenge-response; password never sent in the clear |
| OAUTHBEARER | `SaslOAuthBearer tokenProvider` | Callback that returns a JWT |
| AWS MSK IAM | `SaslAwsMskIam region creds` | AWS Signature V4 for Amazon MSK |

The SASL handshake runs automatically when a connection is established; you
don't need to call any auth functions manually.

## Environment variable configuration

`Kafka.Client.Env` parses standard `KAFKA_*` environment variables (the same
names used by librdkafka and the JVM client) and overlays them onto your config.
This happens automatically when you call `createProducer` or `createConsumer`.

Variables include `KAFKA_BOOTSTRAP_SERVERS`, `KAFKA_SECURITY_PROTOCOL`,
`KAFKA_SASL_MECHANISM`, `KAFKA_SASL_USERNAME`, `KAFKA_SASL_PASSWORD`,
`KAFKA_GROUP_ID`, and others.

Programmatic config always takes precedence; env vars only fill in fields you
haven't set.

## Testing with the mock broker

`Kafka.Client.Mock.Cluster` provides a deterministic, in-process Kafka broker
simulation. It uses STM internally and advances time via `tickClock`, so tests
are fast and reproducible:

```haskell
import Kafka.Client.Mock.Cluster

test :: IO ()
test = do
  cluster <- newMockCluster
  createTopic cluster "events" 3
  appendToPartition cluster "events" 0 record
  slice <- fetchSlice cluster "events" 0 0 100
  -- verify slice contents
```

The mock supports consumer groups (join, leave, rebalance), transactions (begin,
commit, abort, fence), leader epochs, and offset management. It does not
simulate network latency or partial failures (use `Kafka.Client.Mock.Fault` for
fault injection).

## Compression

Record batches are compressed transparently based on `producerCompression`:

| Codec | Flag | Notes |
|-------|------|-------|
| None | `NoCompression` | Default |
| Gzip | `GzipCompression` | Broad compatibility, higher CPU |
| Snappy | `SnappyCompression` | Fast, moderate ratio |
| LZ4 | `Lz4Compression` | Fast, good ratio (recommended for throughput) |
| Zstd | `ZstdCompression` | Best ratio, moderate CPU |

The consumer decompresses automatically based on the batch header; no config
needed on the read side.

## Serdes

The `Serde` type pairs a serializer and deserializer:

```haskell
data Serde a = Serde
  { serialize   :: a -> ByteString
  , deserialize :: ByteString -> Either Text a
  }
```

The `HasSerde` typeclass provides a default serde for a type. Built-in instances
cover `ByteString`, `Text`, `Int16`/`Int32`/`Int64`, `Word16`/`Word32`/`Word64`,
`Float`, `Double`, `UUID`, and any `ToJSON`/`FromJSON` type (via `jsonSerde`).

The Kafka Streams DSL (documented separately under **Kafka Streams**) uses
`HasSerde` to resolve serdes automatically for stream and table types.

## OpenTelemetry

`Kafka.Telemetry.OpenTelemetry` adds semantic-convention spans for produce,
consume, and admin operations. Pass your `TracerProvider` to the config and
spans appear automatically.

## Next steps

- **Kafka Streams:** If you're building stream processing pipelines, see the
  [Kafka Streams documentation](../../kafka-streams/).
- **Getting started:** The [quickstart](../../guides/getting-started/) shows how to
  wire wireform into a Cabal project.
