# wireform-kafka-streams

A Haskell port of [Apache Kafka Streams](https://kafka.apache.org/documentation/streams/).
Mirrors the Java DSL operator-for-operator and the runtime
contract semantics-for-semantics; differences from the JVM are
spelled out below rather than being implicit.

This document is the canonical scope-of-the-library reference.
It tells you what works the same as the JVM, what works
differently, and what isn't there yet — so you can decide
whether the library fits your use case before writing code.

> **Status**: the DSL and runtime track Apache Kafka 4.0 Streams.
> Most parity surfaces are implemented; the remaining gap is not an
> operator, but a fixture: live-broker CI still exercises one process
> at a time. The multi-instance rebalance path is covered by the
> in-process harness.

---

## TL;DR

| Area | Status |
| ---- | ------ |
| DSL surface | Parity for stateless transforms, aggregations, windows, joins, Processor API, state stores, side effects, and Interactive Queries. |
| Runtime | Real-broker runtime, in-process `TopologyTestDriver`, multi-thread worker pool, standby tasks, probing rebalance, lifecycle/listener hooks. |
| Exactly-once | Transactional producer, `EOSCoordinator`, and transactional-store drain are wired. |
| Schema Registry | Avro / JSON-Schema / Protobuf envelopes, compatibility probing, in-memory and HTTP-backed clients. |
| Integration tests | KRaft live-broker PR fixture for Kafka 3.7 + 4.0; multi-instance rebalance currently covered by the mock harness. |
| GHC | 9.6.4 / 9.8.4 / 9.10.1 / 9.12.1 in CI. |

What follows is the user-facing scope: a section per operator
family + a worked example, the runtime model, and an honest
list of what isn't there yet.

---

## What works (DSL surface)

### Sources / sinks

| JVM                                         | Haskell                                                              |
| ------------------------------------------- | -------------------------------------------------------------------- |
| `StreamsBuilder.stream(topic)`              | `streamFromTopic`                                                    |
| `StreamsBuilder.table(topic, Materialized)` | `tableFromTopic`                                                     |
| `StreamsBuilder.globalTable(topic, Materialized)` | `globalTable`                                                  |
| `KStream.to(topic, Produced)`               | `toTopic`                                                            |
| `KStream.to(TopicNameExtractor, Produced)`  | `toExtracted` + `TopicNameExtractor`                       |
| `KStream.through(topic)`                    | `throughTopic`                                                       |
| `KStream.repartition(...)`                  | `repartition`                                                        |
| `KStream.toTable(...)`                      | `toTable`                                                  |
| `KTable.toStream(...)`                      | `toKStreamFromKTable`                                                |
| `KTable.suppress(...)`            | `suppressKStream` / `suppressWindowed` / `suppressUntilTimeLimit`    |

### Stateless transforms

| JVM                          | Haskell                                            |
| ---------------------------- | -------------------------------------------------- |
| `KStream.filter` / `filterNot` | `filterStream` / `filterNotStream` (+ `*Named`)  |
| `KStream.map` / `mapValues`  | `mapKeyValue` / `mapValues` (+ `*M` IO variants, `*Named`) |
| `KStream.flatMap` / `concatMapValues` | `concatMapKeyValue` / `concatMapValues`         |
| `KStream.selectKey`          | `selectKey` (+ `selectKeyNamed`)                   |
| `KStream.peek`               | `peekStream` (+ `peekStreamNamed`)                 |
| `KStream.foreach`            | `foreachStream`                                    |
| `KStream.print(Printed)`     | `printStream` / `printToHandle`                    |
| `KStream.merge`              | `mergeStreams` / `mergeStreamsN`                   |
| `KStream.split`    | `splitStream` + `Branched` / `branchedFrom`         |
| `KStream.transformValues`    | `transformValuesStream` / `processValuesStream`    |
| `KStream.process`            | `processStream` (returns `IO ()`; for typed downstream use `processValuesStream`) |
| `KStream.values`             | `valuesStream`                                     |

### KTable surface

| JVM                                     | Haskell                                  |
| --------------------------------------- | ---------------------------------------- |
| `KTable.filter` / `filterNot`           | `filterTable`                            |
| `KTable.mapValues`                      | `mapValuesTable`                         |
| `KTable.suppress(Suppressed)` | `suppressKStream` etc.                   |
| `KTable.join` / `leftJoin` / `outerJoin` | `joinKTableKTable` / `leftJoinKTableKTable` / `outerJoinKTableKTable` |
| `KTable.join(KTable, fkExtractor, joiner, Materialized)` | `foreignKeyJoinKTable` / `leftForeignKeyJoinKTable`. Subscription-token verification baked in. |
| `KTable.groupBy(KeyValueMapper)`        | `groupByKTable`                          |
| `KTable.toStream`                       | `toKStreamFromKTable`                    |

### Aggregations

| JVM                                         | Haskell                                |
| ------------------------------------------- | -------------------------------------- |
| `KGroupedStream.count` / `reduce` / `aggregate` | `countStream` / `reduceStream` / `aggregateStream` |
| `KGroupedStream.windowedBy(TimeWindows)`    | `windowedByTime`                       |
| `KGroupedStream.windowedBy(SessionWindows)` | `windowedBySession`                    |
| `TimeWindowedKStream.count` / `reduce` / `aggregate` | `countWindowed` / `reduceWindowed` / `aggregateWindowed` |
| `SessionWindowedKStream.count` / `aggregate` | `countSessionWindowed` / `aggregateSessionWindowed` |
| `KGroupedStream.cogroup(Aggregator)` | `cogroup` / `addCogrouped` / `aggregateCogrouped` |

### Joins

| JVM                                                       | Haskell                                                                  |
| --------------------------------------------------------- | ------------------------------------------------------------------------ |
| `KStream.join(KStream, ValueJoiner, JoinWindows, StreamJoined)` | `joinKStreamKStream` (+ `leftJoinKStreamKStream`, `outerJoinKStreamKStream`) |
| `KStream.join(KTable, ValueJoiner, Joined)`               | `joinKStreamKTable` (+ `leftJoinKStreamKTable`)                          |
| `KTable.join(KTable, ValueJoiner, Materialized)`          | `joinKTableKTable` etc.                                                  |
| `KStream.join(GlobalKTable, KeyValueMapper, ValueJoiner)` | `joinKStreamGlobalKTable` (+ `leftJoinKStreamGlobalKTable`)              |
| `KTable.join(KTable, fkExtractor, joiner, Materialized)`  | `foreignKeyJoinKTable` (with token verification)                |

### Windowing

| JVM                                | Haskell                            |
| ---------------------------------- | ---------------------------------- |
| `TimeWindows.of(Duration)`         | `tumblingWindows`                  |
| `TimeWindows.of(...).advanceBy(...)` | `hoppingWindows`                 |
| `SlidingWindows.ofTimeDifferenceWithNoGrace(...)` | `slidingWindows` |
| `SessionWindows.with(...)`         | `sessionWindows`                   |
| `Windows.grace(...)`               | `withGracePeriod` / `withSessionGracePeriod` |
| `Suppressed.untilWindowCloses(BufferConfig)` | `suppressWindowed` / `suppressWindowedHandle` |
| `Suppressed.untilTimeLimit(...)`   | `suppressUntilTimeLimit`           |

### State stores

All four shapes the JVM ships, plus the modern timestamped /
versioned / transactional variants:

| JVM                                        | Haskell                                |
| ------------------------------------------ | -------------------------------------- |
| `KeyValueStore` (in-memory + RocksDB)      | `Kafka.Streams.State.KeyValue.{InMemory, RocksDB, Persistent}` |
| `WindowStore`                              | `Kafka.Streams.State.Window.InMemory`  |
| `SessionStore`                             | `Kafka.Streams.State.Session.InMemory` |
| `TimestampedKeyValueStore`       | `Kafka.Streams.State.KeyValue.Timestamped` |
| `VersionedKeyValueStore`   | `Kafka.Streams.State.KeyValue.Versioned` |
| `CachingKeyValueStore`                     | `Kafka.Streams.State.KeyValue.Caching` |
| Transactional store (EOS-V3)      | `Kafka.Streams.State.Transactional`    |
| `DslStoreSuppliers`             | `Kafka.Streams.DslStoreSuppliers`  |
| `addReadOnlyStateStore`          | `addReadOnlyStateStore`                |

The RocksDB backend is gated behind the `rocksdb` cabal flag
and uses `rocksdb-haskell-kadena`.

### Processor API

Everything that's reachable via `org.apache.kafka.streams.processor.api`:

- `Processor`, `ProcessorContext`, `processorName`, `noopProcessor`,
  `statelessProcessor`.
- `forwardRecord` / `forwardTo`, `currentRecordMetadata`,
  `getStateStore`, `taskId`, `applicationIdC`, `streamTimeC`,
  `wallClockTimeC`.
- `schedule` with `WallClockTimePunctuation` /
  `StreamTimePunctuation` and `Cancellable`.
- Custom store builders via `StoreBuilder` / `StoreBuilderKV` /
  `StoreBuilderW` / `StoreBuilderS`.
- `addStateStore` / `connectProcessorAndStateStores`.
- `RestoreListener`-style hooks for changelog replay.

### Topology / configuration

- `StreamsBuilder` mutable builder mirroring Java's fluent API.
- `Topology` data type with full description (`TopologyDescription`,
  pretty-printer matches the JVM output shape).
- optimisation toggles (`OptimizationConfig`,
  `optimizeTopology`).
- stable processor names.
- `StreamsConfig` covering `application.id`, `bootstrap.servers`,
  `num.stream.threads`, `commit.interval.ms`, `cache.max.bytes.buffering`,
  `processing.guarantee`, `default.deserialization.exception.handler`,
  `task.timeout.ms`, `acceptable.recovery.lag`,
  `max.warmup.replicas`, `probing.rebalance.interval.ms`,
  `task.assignor.class`, `poll.ms`.
- `StreamsConfigKey` + `streamsConfigFromMap` for `Properties`-style
  configuration.

### Runtime

- `Kafka.Streams.Runtime.KafkaStreams` mirrors the JVM
  `KafkaStreams` lifecycle: `newKafkaStreams`, `startKafkaStreams`,
  `closeKafkaStreams`, `streamsStatus`, `setStateListener`,
  `awaitState`.
- `pauseKafkaStreams` / `resumeKafkaStreams` / `isPausedKafkaStreams`.
- `LagListener` / `LagInfo` / `publishLag`.
- `applyEOSCoordinator` to plug in a transactional commit
  driver (`newRealEOSCoordinator` wraps a real
  `Kafka.Client.Transaction`).
- `startKafkaStreamsWith :: KafkaStreams -> StreamDriver -> IO ()`
  injection seam — you can hand the runtime any
  `Kafka.Streams.Runtime.NativeDriver.StreamDriver`. Two
  constructors ship: `newNativeDriver` (production,
  `Producer` + `Consumer` + optional bound `Transaction`) and
  `newMockDriver` (deterministic, in-process, used by the
  test suite).

### Interactive Queries

- `queryEngineStore`, `ReadOnlyKeyValueStore`,
  `roKvGet`, `roKvAll`, `roKvRange` — direct read access to
  materialised state stores from outside the stream thread.
- Typed `Query` API for key-range, count, all.
- Works against every store backend (in-memory + RocksDB).

### Schema Registry serdes

- `Kafka.Streams.Serde.SchemaRegistry`: client interface,
  Confluent envelope (`magicByte` + `int32 BE id` + payload),
  in-memory + mock-HTTP clients for tests.
- `Kafka.Streams.Serde.SchemaRegistry.Http`: transport-agnostic
  HTTP-backed registry (`HttpRequester` record-of-IO so users
  pin their own `http-client` / `wreq` / `req`).
- Payload-format serdes:
  - `Kafka.Streams.Serde.Avro` — pluggable Avro encoder /
    decoder; works with any Avro library.
  - `Kafka.Streams.Serde.JsonSchema` — JSON-Schema-typed
    payloads, same Confluent envelope.
  - `Kafka.Streams.Serde.Protobuf` — Protobuf with the
    Confluent message-index varint (zigzag-encoded; index 0 is
    `0x00`).

### Time

`Kafka.Streams.Time` ships `Timestamp`, `Duration`, the
`millis` / `seconds` / `minutes` / `hours` / `days` builders,
and the `TimestampExtractor` types `recordTimestampExtractor`,
`wallClockTimestampExtractor`, `extractFromValue`.

### Metrics

`Kafka.Streams.Metrics` is the in-process registry — counters,
gauges, duration stats. Plus the librdkafka-shaped
`Kafka.Telemetry.StatsJson` and the OTel push state machine.

---

## Side effects

The DSL has four seams for `IO` inside a topology (covered
end-to-end by [`Kafka.Streams.Examples.SideEffects`](examples/Kafka/Streams/Examples/SideEffects.hs)):

| Seam                         | JVM equivalent             | Notes                                                            |
| ---------------------------- | -------------------------- | ---------------------------------------------------------------- |
| `peekStream`                 | `KStream.peek`             | Observe records non-destructively.                               |
| `foreachStream`              | `KStream.foreach`          | Terminal IO sink.                                                |
| `mapValuesM` / `mapKeyValueM` | `KStream.mapValues` / `map` | Explicit IO variants. JVM `mapValues` is nominally pure but in practice people sneak side effects in; the typed variant makes the IO contract visible. |
| Processor API + `Punctuator` | `ProcessorContext.schedule` | Wall-clock + stream-time scheduled effects, full `IO`.          |

**Two caveats apply to all of them:**

- **EOS-V2 doesn't extend to side effects.** Under
  `ExactlyOnceV2` the producer side is transactional and offsets
  commit through `sendOffsetsToTransaction`, but external
  effects in `peek` / `foreach` / `mapValuesM` / a `Punctuator`
  are not part of that transaction. A topology rewind on
  rebalance will replay them. If you need exactly-once for
  the side effect, gate it on a state-store-backed idempotency
  token. Same as the JVM.
- **Order is per-task, not global.** On a single task, effects
  fire in topology order. With `numStreamThreads > 1` different
  keys may be processed concurrently across tasks. To force a
  global order, route through `repartition` to one partition
  or sink to a topic and consume separately. Same as the JVM.

---

## Semantic differences from the JVM

These are the things that look the same on paper but behave
slightly differently. None are deal-breakers; they just exist.

1. **`mapValues` / `map` are pure by default; `*M` variants are
   explicit IO.** The JVM types `mapValues` as `(V) -> V'` and
   leaves IO implicit. We type the pure variant as `(v -> v')`
   and ship `mapValuesM :: (v -> IO v')` for the IO case. If
   you have IO inside a transform, use the `M` variant; the
   pure one runs at type-check time.

2. **List comprehensions / `do`-notation in topology code is
   normal Haskell.** Building topologies imperatively with
   `IO` inside `StreamsBuilder` is the idiomatic style — it
   mirrors the JVM fluent API. Don't try to make it pure;
   the JVM doesn't either.

3. **No reflection.** The JVM uses `Class<?>` for store-type
   discovery (`QueryableStoreTypes.keyValueStore()`); we use a
   typed `Query` API plus `unsafeCoerce` inside the DSL when
   the user-supplied serde guarantees the type. If you bypass
   the DSL and pin store types incorrectly, you'll get a
   crash at the use site; don't bypass the DSL.

4. **`KTable.suppress` returns a `KStream`, not a `KTable`.**
   The JVM allows `KTable.suppress(Suppressed.untilWindowCloses(...)).toStream()`
   to chain. Here `suppressWindowed` already returns
   `KStream (WindowedKey k) v` because we don't carry the
   compound `KTable<Windowed<K>, V>` type — the windowed key is
   a separate type. To strip the window envelope use
   `selectKey` after `suppress`.

5. **The runtime is "one consumer, N workers", not "N
   stream-threads, each its own consumer".** Java's
   `StreamThread` model is one consumer + one producer per
   thread joining the same group; the broker's coordinator
   reassigns partitions across threads. We use a
   simpler-but-effective design:
   - One `StreamDriver` (one consumer, one producer) per
     `KafkaStreams` instance.
   - With `numStreamThreads = N > 1`, the runtime spins up a
     'WorkerPool' of N workers, each with its own engine and
     state stores. Records dispatch by `hash (topic,
     partition) mod N` (sticky within a process), so the
     same partition consistently lands on the same worker.
   - At commit time the runtime drains every worker's
     in-memory collector through the shared producer.
   - Interactive Queries federate across worker engines the
     way Java's `CompositeReadOnlyKeyValueStore` federates
     across local tasks.

   The tradeoffs vs. Java's per-thread model:
   - **Less network overhead** — one consumer connection per process.
   - **Stickier local state** — a partition's state stays on the worker
     it first hashes to inside that process. Cross-process movement is
     handled by group rebalances; live-broker CI for that path is still
     pending.

6. **`processStream` returns `IO ()`, not `KStream`.** The
   JVM `KStream.process(ProcessorSupplier, ...)` returns a
   typed `KStream<K, V'>`. Encoding the type change at the
   record-of-callbacks boundary requires a typed value-serde
   coming back from the processor; we instead expose
   `processValuesStream` which takes the output value serde
   explicitly and returns the typed `KStream`. Use it when
   your processor changes the value type.

7. **In-process driver vs. real broker.** The JVM
   `TopologyTestDriver` requires the same `StreamsConfig`
   you would use against a real broker, just without the
   network. Our `Kafka.Streams.Driver.TopologyTestDriver`
   doesn't need any config: `newDriver topology applicationId`
   is enough. The runtime path uses the same `StreamsConfig`
   the JVM does.

---

## What's not yet there

The remaining parity gap is verification shape, not API surface:
multi-instance rebalance works through the in-process mock harness,
but CI does not yet run two OS processes against a live broker. The
single-process Docker fixture in `.github/workflows/wireform-kafka-integration.yml`
covers the real protocol path.

Otherwise the streams runtime and DSL are feature-complete relative
to Apache Kafka 4.0 Streams.

### Haskell-native DSL façade

For users who prefer the Haskell-native shape over the JVM-style
fluent builder, [`Kafka.Streams.DSL`](src/Kafka/Streams/DSL.hs)
re-skins the operator surface as a builder-implicit reader monad
with short names and a `|>` pipe:

```haskell
import qualified Kafka.Streams.DSL as S

topology :: IO Topology
topology = S.build $ do
  src <- S.source "in" textSerde textSerde
  src S.|> S.map T.toUpper
      S.|> S.filter (\r -> recordValue r /= "")
      S.|> S.sink "out" textSerde textSerde
```

The original imperative API (`streamFromTopic` / `filterStream`
/ `mapValues` / …) is unchanged.

---

## When to use this library

**Good fit:**

- You're building a streaming app in Haskell and want to keep it in
  the same runtime as the rest of the service.
- You want exactly the JVM Kafka Streams operator surface but
  in Haskell (DSL parity is one-to-one).
- Your topology is unit-testable through the in-process
  driver, and you don't strictly need a multi-thread live
  runtime today.
- You want typed state stores (in-memory or RocksDB) with the
  full timestamped + versioned-store surface (including
  point-in-time and range-by-time queries).
- You want Confluent Schema Registry interop without forcing
  an `http-client` dep on the world.

**Consider alternatives:**

- You require live-broker CI proof for multi-process rebalances
  before adoption. The code path exists and is covered by the mock
  harness, but the live fixture is still single-process.
- You need bug-for-bug compatibility with Streams-the-product
  including its quirks. We aim for spec compliance, not
  bug-for-bug.
- Your app is fundamentally pull-based microservices with
  occasional async work — a plain
  `Kafka.Client.{Producer, Consumer}` setup is simpler.

---

## Reusable transformation fragments

The core DSL is plain Haskell IO with a `StreamsBuilder` you
thread through source / sink operations — that's the shape
most user docs and examples are written for, and it mirrors
the JVM fluent builder one-to-one.

For transformations you want to *name* and *reuse* across
topologies, `Kafka.Streams.Pipeline` provides
`Pipeline a b`: a thin newtype over `a -> IO b` with a
`Control.Category` instance. Fragments compose with `(>>>)`
exactly like ordinary functions:

```haskell
import Control.Category ((>>>))
import Kafka.Streams.Pipeline

normalise :: Pipeline (KStream Text Text) (KStream Text Text)
normalise = pmapValues T.toUpper
        >>> pfilter    (\r -> recordValue r /= "")

-- ...later, inside any topology that has a `KStream Text Text`:
out <- applyPipeline normalise src
```

The equivalent JVM idiom is extracting a helper method that
takes a `KStream` and returns a transformed `KStream`.
`Pipeline` is purely additive — you can reach for it when you
have a transformation worth naming, and ignore it otherwise.
See [`IdiomaticPipeline`](examples/Kafka/Streams/Examples/IdiomaticPipeline.hs)
for a worked example.

---

## Examples

`wireform-kafka/streams/examples/` ships fourteen demos
mirroring the canonical Apache Kafka Streams examples plus a
dedicated [SideEffects](examples/Kafka/Streams/Examples/SideEffects.hs)
walk-through. Run any of them from the in-process driver
without a broker:

```
cabal run wireform-kafka-streams-examples -- pipe
cabal run wireform-kafka-streams-examples -- word-count
cabal run wireform-kafka-streams-examples -- side-effects
cabal run wireform-kafka-streams-examples -- all
```

See [`examples/README.md`](examples/README.md) for the full
index with one-liner descriptions.

---

## Related documents

- [`../TUTORIAL.md`](../TUTORIAL.md) — runnable walkthrough that
  goes from producer/consumer hello-world to a transactional
  Streams pipeline including the multi-instance + IQ + standby
  story.
- [`../CONFIG_PARITY.md`](../CONFIG_PARITY.md) — `librdkafka` knob
  parity for the client layer; streams-specific config knobs
  are listed in `Kafka.Streams.Config.StreamsConfig` and
  spelled out in the §StreamsConfig section above.
- [`examples/README.md`](examples/README.md) — operator-by-operator
  example index with the JVM equivalent of each demo.
- [`bench/results/README.md`](bench/results/README.md) — runtime
  benchmark numbers + reproduction recipe.
