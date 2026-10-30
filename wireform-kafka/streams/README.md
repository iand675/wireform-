# Kafka Streams

A Haskell port of Apache Kafka Streams. Stateful stream processing with joins, windowing, and exactly-once guarantees.

> **Status:** Feature-complete for Apache Kafka 4.0. Runs against real brokers or an in-process test driver.

## What you can build

| Task | DSL provides |
|---|---|
| Filter/transform records | `filter`, `map`, `flatMap`, `selectKey` |
| Count, sum, aggregate | `count`, `reduce`, `aggregate` with state stores |
| Time-windowed analysis | Tumbling, hopping, sliding, session windows |
| Join streams/tables | Stream-stream, stream-table, table-table, foreign-key |
| Queryable state | Interactive Queries (IQ) from outside the topology |
| Exactly-once | One transaction per commit: output, changelogs and offsets together |

## Core abstractions

**KStream** - A stream of events. Every record, in order.

**KTable** - A stream of updates. Latest value per key; a materialized view.

Converting: `toTable` (stream to table), `toKStreamFromKTable` (table to stream of current values) and `toChangelogStream` (every change, deletes as tombstones). Table operators forward each row's new and old value, so deletes and retractions reach every derived table.

## Building a topology

```haskell
import qualified Kafka.Streams as S
import qualified Kafka.Streams.StreamsBuilder as SB

main :: IO ()
main = do
  builder <- SB.newStreamsBuilder
  stream <- SB.streamFromTopic builder "input" (S.consumed S.textSerde S.textSerde)
  transformed <- S.mapValues (\v -> T.toUpper v) stream
  _ <- S.toTopic (S.topicName "output") (S.produced S.textSerde S.textSerde) transformed
  topo <- SB.buildTopology builder
  print topo
```

## Stateless transforms

```haskell
-- Filter
filtered <- S.filterStream (\k v -> v /= "") stream

-- Transform value
mapped <- S.mapValues (\v -> v <> "!") stream

-- Transform key and value
remapped <- S.mapKeyValue (\k v -> (k <> "-new", v <> "!")) stream

-- One input to many outputs
expanded <- S.concatMapValues (\v -> [v, v <> "-copy"]) stream

-- Change partitioning key
rekeyed <- S.selectKey (\k v -> T.take 3 v) stream

-- Side effect (logging, metrics)
_ <- S.peekStream (\k v -> putStrLn ("saw: " <> show v)) stream
```

**Naming:** Most operators have `*Named` variants for explicit processor names.

**IO variants:** Use `mapValuesM` and `mapKeyValueM` when you need IO.

**Automatic serde resolution:** The DSL uses the `HasSerde` typeclass to automatically resolve serdes when types change. For types with a `HasSerde` instance (like `Text`, `Int64`, `Double`), you don't need to manually thread `Serde` values through every operator. Use `*With` variants for explicit serdes or wrap with a newtype for alternative encodings.

## Stateful aggregations

```haskell
-- Count occurrences
grouped <- S.groupByKey stream
counted <- S.countStream grouped

-- Keep maximum
maxTable <- S.reduceStream grouped (\v1 v2 -> if v1 > v2 then v1 else v2)

-- Full aggregation
aggTable <- S.aggregateStream grouped
  (\key -> 0)              -- initializer
  (\key v agg -> agg + 1)  -- adder
```

## Windowing

```haskell
-- Tumbling: fixed-size, non-overlapping
tumbled <- S.windowedByTime grouped
             (S.tumblingWindows (S.seconds 60))

-- Hopping: fixed-size with overlap
hopped <- S.windowedByTime grouped
            (S.hoppingWindows (S.minutes 5))
            (S.advanceBy (S.minutes 1))

-- Session: dynamic by activity
dynamic <- S.windowedBySession grouped
             (S.sessionWindows (S.minutes 30))
```

## Joins

```haskell
-- Stream-stream (windowed)
joined <- S.joinKStreamKStream stream1 stream2
            (\v1 v2 -> v1 <> "-" <> v2)
            (S.streamJoined serde1 serde2)

-- Stream-table (enrichment)
enriched <- S.joinKStreamKTable stream table
              (\streamVal tableVal -> ...)

-- Table-table
combined <- S.joinKTableKTable table1 table2
              (\v1 v2 -> v1 + v2)
```

## State stores

| Store type | Purpose | Backends |
|---|---|---|
| Key-Value | Latest value per key | In-memory, RocksDB |
| Window | Time-windowed aggregations | In-memory |
| Session | Session-windowed aggregations | In-memory |
| Timestamped | KV with record timestamps | In-memory, RocksDB |
| Versioned | Point-in-time lookups | In-memory, RocksDB |

## Interactive Queries (IQ)

Query materialized state from outside:

```haskell
import qualified Kafka.Streams.State as State

value <- State.roKvGet store "user-123"
range <- State.roKvRange store "user-100" "user-200"
all <- State.roKvAll store
```

## Side effects and IO

| Operator | Use for | Behavior |
|---|---|---|
| `peekStream` | Observation | Non-destructive |
| `foreachStream` | Terminal sink | Blocking |
| `foreachStreamAsync` | Fire-and-forget | Non-blocking, forks async |
| `mapValuesM` | IO transforms | Blocking with explicit IO |

Side effects are not part of the exactly-once transaction. Use a state-store-backed idempotency token for exactly-once side effects.

## Exactly-once (EOS)

Enable:

```haskell
config = defaultStreamsConfig { processingGuarantee = ExactlyOnceV2 }
```

Provides:
- One transactional producer per instance; each commit interval (100 ms by default) is one transaction
- Output records, state-store changelog writes, and the consumed offsets (`sendOffsetsToTransaction`) commit together
- On an aborted transaction every task is restored from its changelogs and rewound to the last commit
- `read_committed` consumers and restore readers

External systems can join the commit as two-phase commit participants (`Kafka.Streams.Sinks.TwoPhase`, `Kafka.Streams.Runtime.addCommitParticipant`).

## Running a topology

### Against a real broker

```haskell
import qualified Kafka.Streams.Runtime as Runtime

main :: IO ()
main = do
  let config = S.defaultStreamsConfig
        { S.applicationId = "my-app"
        , S.bootstrapServers = ["localhost:9092"]
        }
  valid <- either (fail . show) pure (S.validateTopology topo)
  streams <- Runtime.newKafkaStreams config valid
  Runtime.startKafkaStreams streams
  Runtime.awaitState streams Runtime.StreamsClosed
```

Each source partition number of each subtopology is a task with its own
stores; the instance's tasks run in parallel over `numStreamThreads`
workers, and instances with the same `applicationId` share the
partitions through the consumer group. Stores are change-logged to
compacted topics and restored from them before a task processes
anything.

### In-memory broker (no cluster)

`Kafka.Streams.Runtime.MockBroker` runs the same runtime against an
in-memory broker (topics, groups and rebalances, transactions, fault
injection):

```haskell
broker <- newMockBroker (Just 1)
mockCreateTopic broker "input" 4
streams <- Runtime.newKafkaStreams config valid
Runtime.startKafkaStreamsWith streams =<< mockDriverFor broker config
```

### Test driver (no broker)

```haskell
import qualified Kafka.Streams.Driver as TTD

main :: IO ()
main = do
  driver <- TTD.newDriver topo "test-app"
  TTD.pipeInput driver (S.topicName "input") (Just "key") "value" (Timestamp 0) 0
  outputs <- TTD.readOutput driver (S.topicName "output")
  print outputs
```

The test driver runs the whole topology in one in-process engine (no
tasks, rebalances or transactions); use the in-memory broker for those.

## Examples

```bash
cabal run wireform-kafka-streams-examples -- pipe
cabal run wireform-kafka-streams-examples -- word-count
cabal run wireform-kafka-streams-examples -- all
```

Source in `wireform-kafka/streams/examples/`.

## Related

- [`../TUTORIAL.md`](../TUTORIAL.md) - Walkthrough from basics
- [`../CONFIG_PARITY.md`](../CONFIG_PARITY.md) - Configuration
- [`examples/README.md`](examples/README.md) - Example index
