# wireform-kafka

> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

A pure-Haskell Kafka client. Send messages, process streams, manage transactions, all from Haskell. No JVM, no native bindings, just the Kafka protocol implemented directly.

New to Kafka? Read [`CONCEPTS.md`](./CONCEPTS.md) for a quick primer on topics, partitions, and consumer groups. Then try [`TUTORIAL.md`](./TUTORIAL.md) for hands-on examples.

## What you can do

| Task | Module | Notes |
|---|---|---|
| Send messages | `Kafka.Client.Producer` | `withProducer` handles setup and cleanup |
| Receive messages (managed) | `Kafka.Client.Group.runConsumer` | Automatic commits, error handling, rebalancing |
| Receive messages (manual) | `Kafka.Client.Consumer` | You control polling and commits |
| Stream processing | `Kafka.Streams` | Aggregations, joins, windowing, exactly-once |
| Admin operations | `Kafka.Client.AdminClient` | Topics, groups, ACLs |
| Raw protocol | `Kafka.Protocol.Generated.*` | One module per Kafka API |

Import `qualified Kafka` for common functions.

## Quick start

### Send a message

```haskell
import qualified Kafka

main :: IO ()
main =
  Kafka.withProducer ["localhost:9092"] Kafka.defaultProducerConfig $ \p -> do
    md <- Kafka.sendMessage p "events" Nothing "hello"
    print md
```

`withProducer` opens a connection, runs your code, then flushes and closes cleanly, even on exceptions.

### Receive messages

```haskell
{-# LANGUAGE OverloadedRecordDot #-}

import qualified Kafka
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main =
  Kafka.runConsumer
    Kafka.defaultGroupConfig
      { Kafka.bootstrapBrokers = ["localhost:9092"]
      , Kafka.groupId          = "my-service"
      , Kafka.topics           = ["events"]
      }
    $ \rec -> BS.putStrLn rec.value
```

Access fields with `rec.key`, `rec.value`, `rec.topic`, `rec.partition`, `rec.offset`, `rec.timestamp`, `rec.headers`.

For batch processing, use `runBatchedConsumer`. Your handler receives a batch and one commit covers all.

### Manual control

```haskell
import qualified Kafka.Client.Consumer as Consumer

main :: IO ()
main =
  Consumer.withConsumer
    ["localhost:9092"] "my-service"
    Consumer.defaultConsumerConfig
    ["events"]
    $ \c -> do
        result <- Consumer.poll c 1000
        case result of
          Right recs -> mapM_ print recs
          Left err   -> putStrLn ("poll failed: " <> err)
        _ <- Consumer.commitSync c
        pure ()
```

### Type-safe topics

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Kafka
import qualified Kafka.Topic as Topic

events :: Topic.Topic Text Text
events = Topic.textTopic "events"

main =
  Kafka.withProducer ["localhost:9092"] Kafka.defaultProducerConfig $ \p -> do
    Kafka.publish p events (Just "k1") "hello"
```

## Error handling

Operations throw `KafkaException` on failure with structured `KafkaErrorKind`:

- `ConnectError` - broker unreachable
- `AuthenticationError` - SASL or TLS failure
- `TimeoutError` - deadline exceeded
- `ProducerFencedError` - transactional ID conflict
- `ConfigurationError` - invalid settings

Each error has `isRetriable` and `isFatal` for retry decisions.

## Configuration

Start from defaults, override as needed:

```haskell
producerConfig = Kafka.defaultProducerConfig
  { Kafka.producerCompression = Kafka.Zstd
  , Kafka.producerBatchSize   = 32768
  , Kafka.producerLingerMs    = 10
  , Kafka.producerIdempotent  = True
  }

groupConfig = Kafka.defaultGroupConfig
  { Kafka.bootstrapBrokers = ["broker-1:9092"]
  , Kafka.groupId          = "my-service"
  , Kafka.topics           = ["events"]
  , Kafka.commitMode         = Kafka.CommitSync
  }
```

See [`CONFIG_PARITY.md`](./CONFIG_PARITY.md) for the librdkafka mapping. Use `KAFKA_*` environment variables for overrides.

### TLS offload

For sidecars, load balancers, or kernel TLS:

```haskell
viaSidecar :: ConnectionConfig
viaSidecar = defaultConnectionConfig
  { connTlsOffload = Just $ staticTlsOffload (TlsOffloadUnix "/var/run/kafka-proxy.sock") }
```

## Security

- **SASL**: PLAIN, SCRAM-SHA-256, SCRAM-SHA-512, OAUTHBEARER (with re-authentication)
- **TLS**: 1.2 and 1.3

## Observability

- **OpenTelemetry**: spans for producer/consumer/transaction operations
- **Stats JSON**: librdkafka-compatible format
- **Interceptors**: per-record hooks

## Testing

```bash
# In-process, no external dependencies
cabal test wireform-kafka:wireform-kafka-test

# Against a live broker
docker compose -f test-integration/docker-compose.yml up -d
WIREFORM_KAFKA_BROKER=localhost:9092 cabal test wireform-kafka:wireform-kafka-integration
```

The `cbits/` kernels (`crc32c.c`, the LZ4/snappy/zstd shims,
`wireform_decompress.c`, `wireform_time.c`) are additionally tested
directly by [`c-harness/`](../c-harness/README.md): unit, boundary and
differential tests plus libFuzzer targets under ASan+UBSan (and MSan,
valgrind and the clang static analyzer in `ci`). `make -C c-harness
coverage` puts `crc32c.c` and `wireform_time.c` at 100% line coverage
and the third-party codec shims at 86-96%.

## Stream processing

```haskell
import qualified Kafka.Streams as S

main :: IO ()
main = do
  builder <- S.newStreamsBuilder
  -- HasSerde typeclass provides automatic serde resolution for common types
  src <- S.streamFromTopic builder (S.topicName "in") (S.consumed S.textSerde S.textSerde)
  _ <- S.mapValues (\v -> v <> "!") src
       >>= S.toTopic (S.topicName "out") (S.produced S.textSerde S.textSerde)
  topo <- S.buildTopology builder
  print topo
```

The `HasSerde` typeclass automatically resolves serdes for types like `Text`, `Int64`, `Double`, `UUID`. Use `*With` variants or explicit `Serde` values for custom encodings. See [`streams/README.md`](./streams/README.md) for the full DSL.

## Documentation

| Doc | Purpose |
|---|---|
| [`CONCEPTS.md`](./CONCEPTS.md) | Kafka fundamentals |
| [`TUTORIAL.md`](./TUTORIAL.md) | Walkthrough from basics to streams |
| [`streams/README.md`](./streams/README.md) | Streams DSL reference |
| [`CONFIG_PARITY.md`](./CONFIG_PARITY.md) | Configuration mapping |
| [`INTEGRATION_TESTING.md`](./INTEGRATION_TESTING.md) | Testing guide |
| [`PERFORMANCE.md`](./PERFORMANCE.md) | Tuning and benchmarks |

## Install

```cabal
build-depends:
  base,
  wireform-kafka,
  wireform-kafka-streams,
```

From the [wireform monorepo](https://github.com/iand675/wireform). `cabal build wireform-kafka` to compile. Use `-fllvm` for better performance.

Wire-compatible with Apache Kafka 4.0. Tested with GHC 9.6, 9.8, 9.10, 9.12.

## License

BSD-3-Clause
