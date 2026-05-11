# Feature Parity Spec

This document is the running plan for bringing `wireform-kafka` (core
client) and `wireform-kafka-streams` (Streams DSL + runtime) to
production-grade parity with the upstream references:

| Library                     | Reference target            | Tracked version |
|-----------------------------|-----------------------------|-----------------|
| `wireform-kafka`            | Apache Kafka client wire    | Kafka 4.0       |
| `wireform-kafka` (UX side)  | `librdkafka`                | 2.x             |
| `wireform-kafka-streams`    | Apache Kafka Streams (Java) | Kafka 4.0       |

The spec is intentionally bias-toward-test: every "Done" item below has at
least one targeted test (unit, property, or against the in-process mock
broker); every "Outstanding" item lists the test surface that has to land
with the implementation.

---

## 1. Goals & non-goals

### Goals

1. **Wire compatibility** — a real Kafka 4.0 broker can speak to our
   client, and a real Kafka 4.0 client can speak to our streams
   implementation, end-to-end without the other side noticing it isn't
   talking to a Java peer.
2. **Semantic compatibility** — the public API behaves the way the JVM
   client / Streams library does for the same input. Especially:
   exactly-once-V2, sticky / cooperative-sticky rebalancing, transactional
   offset commit, suppress (KIP-328), versioned stores (KIP-889/KIP-960).
3. **Operational compatibility** — every `librdkafka` knob that maps to
   client behaviour has a corresponding `wireform-kafka` config field
   with the same name, default, and semantics. See
   [`CONFIG_PARITY.md`](./CONFIG_PARITY.md).
4. **Testability** — an in-process `MockCluster` covers every failure
   mode that `librdkafka`'s mock-cluster suite exercises, so we can land
   regressions with deterministic tests.

### Non-goals

- Bug-for-bug compatibility with quirks the JVM client itself documents
  as deprecated.
- The pre-KIP-848 ZooKeeper coordinator path. KRaft only.
- Compatibility with brokers older than Kafka 2.5 (we negotiate down via
  `ApiVersions` to the lowest version we still implement, not arbitrarily
  far back).
- Schema-Registry-the-product (Confluent's HTTP service). We will ship
  the *client-side* Avro / JSON-Schema / Protobuf serdes interfaces, but
  spinning up a registry is out of scope.

---

## 2. Done — current state inventory

### 2.1 `wireform-kafka` (core client)

#### Wire protocol

- API key coverage: Produce, Fetch, ListOffsets, Metadata, OffsetCommit,
  OffsetFetch, FindCoordinator, JoinGroup, SyncGroup, Heartbeat,
  LeaveGroup, DescribeGroups, ListGroups, DeleteGroups, CreateTopics,
  DeleteTopics, ApiVersions, InitProducerId, AddPartitionsToTxn,
  AddOffsetsToTxn, EndTxn, TxnOffsetCommit, DescribeConfigs, AlterConfigs.
- Versioned (incl. flexible / tagged-fields) request/response
  encoders + decoders, generated from the Kafka schema. Comprehensive
  round-trip property tests (`test/Protocol/Generated/ComprehensiveSpec.hs`).
- Record batch v2 (KIP-98), variable-length records, headers,
  CRC32-C, idempotent producer sequencing, transactional records.
- Compression: gzip, snappy, lz4, zstd. Per-batch + per-record codecs,
  property-tested.

#### Client behaviour

- `Producer`:
  - Idempotent producer (KIP-98) with per-partition sequence numbers.
  - `BatchAccumulator` with linger.ms, batch.size, sticky-partitioning
    (KIP-480 inspired), per-batch retry counter + exponential backoff
    with jitter, structured `Logger` callback for sender errors.
  - Transaction *coordination protocol* (`Kafka.Client.Transaction`):
    `initTransactions`, `beginTransaction`, `sendOffsetsToTransaction`,
    `commitTransaction`, `abortTransaction`. State machine, coordinator
    discovery, `InitProducerId`, `AddPartitionsToTxn`,
    `AddOffsetsToTxn`, `TxnOffsetCommit`, `EndTxn` are wired against
    the `TransactionCoordinator`. Per-stamp `(TxnId, Epoch)`
    visibility tracking in the mock means aborted records *stay*
    aborted across epoch reuse.
  - `bindTransaction :: Producer -> Transaction -> IO ()` attaches
    a `Transaction` to a `Producer`. Once bound:
      - `sendMessage` rejects with a typed-string error unless the
        transaction is in `InTransaction` (gate logic exposed
        publicly as `producerTxnGate` for testing);
      - the first send on a (topic, partition) inside the open
        transaction issues `AddPartitionsToTxn` to the coordinator
        (memoised per-txn so subsequent sends are STM-only);
      - outgoing record batches are stamped with the transactional
        producer-id / epoch / sequence (allocated via
        `appendRecordStamped` + `BatchStamp` on the
        accumulator) and the `attrIsTransactional` bit is flipped
        on the wire-level `Attributes` word;
      - `closeProducer` aborts an open transaction before shutdown
        so the broker doesn't keep the txn id locked for
        `transaction.timeout.ms`.
    What's still pending: the end-to-end live-broker test, plus the
    Streams runtime hooking the engine's commit ticks into the
    bound producer's `beginTransaction` /
    `sendOffsetsToTransaction` / `commitTransaction` cycle (S0
    "Full `KafkaStreams` runtime against the real client" in §3.2).
- `Consumer`:
  - `subscribeFlow` with FindCoordinator → JoinGroup → SyncGroup →
    OffsetFetch.
  - Range / round-robin / sticky / cooperative-sticky assignors. Sticky
    consumes previous-generation `ownedPartitions` (consumer-protocol
    v1, KIP-341/KIP-429); fixed in this branch.
  - Per-partition pause/resume, manual offset store, isolation level
    (read-committed / read-uncommitted), commit-with-metadata.
  - LeaveGroup RPC on close (no more silent-timeout-out members).
  - Heartbeat thread.
- `AdminClient`:
  - createTopics / deleteTopics / listTopics / describeTopics /
    listConsumerGroups / describeConsumerGroups / deleteConsumerGroups /
    describeConfigs.
  - DescribeConfigs decodes KIP-226 `ConfigSource` correctly to
    distinguish DEFAULT_CONFIG from any override; the unwrap helpers are
    public + unit-tested.
- `TransactionCoordinator`:
  - findCoordinator, initProducerId, addPartitionsToTxn, endTransaction,
    addOffsetsToTxn, txnOffsetCommitWith. Pure request builders are
    public + tested.
- `Pipeline`:
  - Send / receive / timeout threads, correlation-id routing into
    caller-held `TMVar` slots, in-flight + queue-size backpressure,
    statistics counters, clean shutdown. Tested against a localhost echo
    broker.

#### Networking

- `Network.Connection`:
  - Per-broker connection pool (`ConnectionManager`).
  - DNS resolution modes (host / `use_all_dns_ips` /
    `resolve_canonical_bootstrap_servers_only` per KIP-235), broker
    address family selection (any / v4 / v6).
  - Socket buffer / Nagle / keepalive / max-idle / max-fails knobs.
  - Best-effort `isConnected` liveness probe (alive-idle case fixed in
    this branch).
  - **io_uring transport** (`Kafka.Network.IoUring`, opt-in
    via the `io-uring` cabal flag): replaces the `recv` /
    `send` syscall pair with a single `io_uring_enter` per
    round-trip on Linux 5.1+. Self-contained — no `liburing`
    build dep; raw syscalls + mmap'd SQ/CQ rings. Runtime
    probe (`isIoUringSupported`) detects kernel availability;
    a stub C file is compiled on non-Linux hosts so the FFI
    symbols stay linkable. Currently surfaces as a
    `Kafka.Network.Transport.Transport`; pipeline integration
    is staged.
- API version negotiation:
  - `ApiVersionsRequest` on connect, `ApiVersionCache` per broker,
    `selectVersion` picks the highest version both sides understand.
- SASL:
  - PLAIN, SCRAM-SHA-256, SCRAM-SHA-512, OAUTHBEARER, AWS_MSK_IAM,
    GSSAPI handshakes. `wireName` round-trips.

#### Mock broker

- `Kafka.Client.Mock.*`:
  - `MockCluster`: topics, partitions, logs, HWM/LSO, brokers (up/down),
    groups (offsets, members, generation id), transactions (state, epoch,
    committed/aborted stamps, pending offsets), KRaft roles, re-auth
    deadlines, manual clock.
  - `FaultPolicy`: per-op / per-topic / per-group / per-txn injectable
    errors (retriable, fatal, timeout, leader-change, txn-fenced).
  - `MockProducer`, `MockConsumer`, `MockAdmin` with the same surface as
    the real clients, used by both the streams `MockStreamsDriver` and
    the core-client failure-mode tests.
  - `IdempotentState`, `BackoffPolicy`, `TelemetryCounters`,
    `ProducerStamp`.
  - Failure-mode coverage parity with selected `librdkafka` mock-cluster
    tests (0121, 0125, 0127, 0130, 0137, 0142, 0143, 0146, 0147, 0148,
    0150).

### 2.2 `wireform-kafka-streams`

- KStream DSL: filter, map, mapValues, flatMap, flatMapValues,
  selectKey, peek, branch / split, merge, repartition, through, toTable
  (KIP-523), `processValuesStream` (low-level), named variants for every
  operator (KIP-307), `printToHandle`, dynamic-topic sink
  (`TopicNameExtractor`, KIP-303).
- KTable DSL: filter, mapValues, suppress (KIP-328), join, leftJoin,
  outerJoin, transformValues, toStream, groupBy.
- GlobalKTable + global join.
- Aggregations: `count`, `reduce`, `aggregate` over KGroupedStream and
  KGroupedTable.
- Cogroup (KIP-150): multiple grouped streams of distinct value types
  feeding one aggregate.
- Windowed processing: tumbling, hopping, sliding (KIP-450), session
  windows. `TimeWindowedKStream`, `SessionWindowedKStream`.
- Joins: stream-stream, stream-table, table-table, foreign-key
  (rudimentary), windowed stream-stream.
- State stores:
  - `KeyValueStore` (in-memory + RocksDB-backed via
    `rocksdb-haskell-kadena`).
  - `WindowStore`, `SessionStore`, `TimestampedKeyValueStore`
    (KIP-258), `VersionedKeyValueStore` (KIP-889/KIP-960).
- Processor API: `ProcessorContext`, `Punctuator`, schedule
  wall-clock + stream-time punctuations.
- Topology:
  - `Topology`, `StreamsBuilder`, sub-topology computation by
    repartition boundary.
  - `TopologyDescription` with pretty-printer mirroring the JVM
    representation.
- `KafkaStreams` runtime (engine-only):
  - Worker pool (`numStreamThreads`), per-task processing, deterministic
    quiescence via STM.
  - `Engine` integrates with mock or real client; `MockStreamsDriver`
    hooks the engine to the in-process broker.
- Exactly-once V2 *scaffolding*: idempotent producer wired to the
  engine's commit boundaries, plus a pluggable `EOSCoordinator`
  recording-stub the runtime tests assert against. The actual
  transactional producer integration (records bracketed by
  `BeginTxn` / `EndTxn` markers; `sendOffsetsToTransaction` for
  consumer offsets) is still pending — tracked in §3.1 below.
- Standby task scaffolding (the `MockCluster` already understands
  changelog topics and warmup reads).
- `StreamsConfig` covering processing.guarantee, num.stream.threads,
  commit.interval.ms, cache.max.bytes.buffering,
  default.deserialization.exception.handler, etc.
- `StreamsMetrics`: in-process registry, counters / gauges / duration
  stats.
- Interactive Queries v1 (KIP-67) + KIP-796 typed Query API (key range,
  count, all).
- `DslStoreSuppliers` (KIP-1247) so users can swap default backends.
- `RestoreListener`-style hooks for changelog replay.

### 2.3 Tests

| Suite                              | Count |
|------------------------------------|-------|
| `wireform-kafka:wireform-kafka-test`         | 588 |
| `wireform-kafka:wireform-kafka-streams-test` | 316 |

Both green on every commit on this branch.

---

## 3. Outstanding work — gap analysis

For each item: severity (S0 = parity-blocker, S1 = important for real
production use, S2 = nice-to-have), rough invasiveness, and the test
surface that must land.

### 3.1 Core client gaps

> The S0 / S1 / S2 client-side items previously listed here have
> all landed on this branch. What remains is the integration-side
> follow-up that requires running infrastructure rather than new
> code:

#### S1 — KIP-368 SASL re-auth: mid-session handshake driver

- **What's done.** `Kafka.Network.Auth.SASL.effectiveReauthDeadlineMs`
  + `reauthRequiredAtMs` are the pure decision layer (computes the
  effective deadline + applies the safety margin).
- **What's left.** The pipeline-side machinery that actually runs
  a fresh `SaslHandshake` + `SaslAuthenticate` exchange mid-session
  without dropping in-flight requests. Touches
  `Kafka.Client.Pipeline` (it has to pause new sends, drain
  in-flight, run the handshake, resume).

#### S1 — librdkafka-shaped stats: per-counter wiring

- **What's done.** `Kafka.Telemetry.StatsJson` renders a snapshot
  matching the librdkafka shape. Mirror dashboards / collectd /
  Datadog port over without changes.
- **What's left.** A scheduled emitter inside the producer /
  consumer that polls the existing `MetricsRegistry` /
  `TelemetryCounters` on the configured `statistics.interval.ms`
  and hands the snapshot to a user callback. Additionally, the
  OTel exporter side (`Kafka.Telemetry.OpenTelemetry` is still a
  stub) is independent of the JSON path and remains pending.

### 3.2 Streams gaps

#### S0 — KafkaStreams runtime: engine ↔ NativeDriver wiring

- **What's done.** `Kafka.Streams.Runtime.NativeDriver` is the
  driver record that wires `Producer` + `Consumer` + bound
  `Transaction` against the streams engine. The pure decision
  layers for KIP-441 probing (`ProbingRebalance`) and KIP-869
  revocation grace (`RevocationGrace`) are in place. The KIP-892
  store transactional buffer (`State.Transactional`) is in place.
- **What's left.** Refactor `Kafka.Streams.Runtime` so the engine
  consumes a `StreamDriver` instead of driving the in-process
  mock directly. Cascades into `Streams/MockDriverModesSpec` and
  `Streams/EngineSpec`. The driver / store-transactional / probe
  / revocation building blocks the refactor needs are all
  present; the work is "thread the driver through".

#### S0 — Schema Registry: Avro / JSON-Schema / Protobuf payload serdes

- **What's done.** `Kafka.Streams.Serde.SchemaRegistry` ships the
  `SchemaRegistryClient` interface, an `inMemoryRegistry` for
  tests, a `mockHttpRegistry` for asserting the HTTP exchange
  shape, the Confluent magic-byte envelope, and `registrySerde`.
- **What's left.** Concrete payload serdes for Avro / JSON-Schema
  / Protobuf, plus a real HTTP-backed `SchemaRegistryClient`
  (separate from the core lib so the http-client dep stays
  optional). Compatibility-mode tests against a real registry.

#### S1 — KIP-213 foreign-key join: DSL combinator wiring

- **What's done.** `Kafka.Streams.DSL.ForeignKeyJoinV2` ships the
  pure data-layer state machine (`SubscriptionMessage`,
  `Responder`, `stepLeft`, `stepRight`, `runEvents`).
- **What's left.** Replace the existing `foreignKeyJoin`
  combinator's naive re-key implementation with a topology that
  pipes left updates through the V2 subscription store + responder
  topic + token verification.

### 3.3 Cross-cutting / infrastructure

| Item                                    | Severity | Notes                                                                 |
|-----------------------------------------|----------|-----------------------------------------------------------------------|
| Live-broker integration test harness    | S1       | `WIREFORM_KAFKA_BROKER`-gated suite includes the new transactional spec; promoting that to CI / Docker is still pending. |
| GHC 9.10 / 9.12 build matrix            | S2       | Currently 9.6.4 only.                                                 |
| Benchmark suite (vs `librdkafka` numbers) | S2     | New `Benchmarks.StatsAndStamping` covers stats JSON + record-batch building; comparative `librdkafka` numbers still anecdotal. |
| Documentation: tutorial-grade walkthrough | --     | `TUTORIAL.md` covers mock cluster → producer → transactions → Streams DSL → KIP-892 → Schema Registry → stats JSON. |

---

## 4. Prioritisation

In approximate order:

1. **`KafkaStreams` runtime ↔ NativeDriver wiring** (S0). The
   driver record + the pure decision layers (probing rebalance,
   revocation grace) and the EOS-V3 store transactional buffer
   are all in place. The remaining work is a deep refactor of
   `Kafka.Streams.Runtime` so the engine consumes a `StreamDriver`
   rather than driving the mock directly. Cascades into the
   existing streams-test specs.
2. **KIP-368 SASL re-auth pipeline integration** (S1). Pure
   helpers are in place (`effectiveReauthDeadlineMs` /
   `reauthRequiredAtMs`); the pipeline-side machinery that runs
   the fresh handshake mid-session without dropping in-flight
   requests still has to land.
3. **Schema Registry payload serdes** (S0). Wire concrete Avro /
   JSON-Schema / Protobuf serdes on top of the existing
   `SchemaRegistryClient` + envelope helpers. HTTP-backed client
   in a separate sub-library so http-client stays optional.
4. **KIP-213 FK-join DSL wiring** (S1). Pure data layer is in
   place; the DSL combinator that uses it is the missing
   integration.
5. **librdkafka stats interval emitter + OTLP exporter** (S1).
   `StatsJson` ships the snapshot shape; the producer / consumer
   need a scheduled emitter that polls existing counters every
   `statistics.interval.ms`. OTLP exporter side independent.
6. **Live-broker Docker fixture + GHC 9.10 / 9.12 matrix**
   (cross-cutting). The new transactional integration spec
   already runs against `WIREFORM_KAFKA_BROKER`.

---

## 5. Test strategy

Every implementation step lands with the following tiers:

1. **Unit / property tests** in `test/Client/*Spec.hs` or
   `test/Streams/*Spec.hs`. Pure-function tests for codec, builders,
   assignors, store invariants.
2. **Mock-broker integration tests** in `test/Client/MockBroker*Spec.hs`
   or `test/Streams/Mock*Spec.hs`. Drive end-to-end flows through
   `MockCluster` with injected faults.
3. **Live-broker smoke tests** (planned), gated behind
   `-fwith-live-broker` so CI without Docker still runs.
4. **Benchmark targets** (planned) using `tasty-bench`, guarded by
   `-fwith-benchmarks`.

For each item in §3, the listed "Tests" subsection becomes the spec for
the test work; a feature isn't considered done until those tests are
landed and green.

---

## 6. Risks & open questions

- **Engine driver abstraction shape.** Class-based vs record-of-IO. The
  former is more idiomatic but harder to mock without exposing instance
  internals; the latter is simpler but uglier. Decision deferred to the
  start of the runtime-rewire work.
- **State store transactional buffer.** RocksDB's `WriteBatch` is the
  natural fit, but we'd need to thread a batch identity through every
  store operation. The in-memory implementations need a parallel
  layer.
- **Schema Registry HTTP client.** We don't currently depend on `http-client`
  or `wreq`. Picking the dep is a one-way door; pin it to `http-client` +
  `tls` since we already pull `crypton-connection`.
- **Multi-broker test ergonomics.** Spinning N `MockCluster` instances
  and joining them is currently per-test boilerplate. A shared
  `withMockEnsemble` helper would reduce friction.

---

## 7. Tracking

Outstanding work is tracked in this document, not in GitHub issues, until
the project graduates beyond personal-fork status. When an item moves
from "Outstanding" to "Done" the corresponding section in §2 grows and
its mention in §3 is deleted, with a one-line PR reference left in the
commit message.

The current snapshot of in-progress / pending work is:

- **Top of the queue:** thread the new `Kafka.Streams.Runtime.NativeDriver`
  through `Kafka.Streams.Runtime` so the engine drives the real
  `Producer` + `Consumer` (with the bound `Transaction` providing
  EOS-V2 commit boundaries) and the KIP-892 store-transactional
  buffer drains on each commit (§3.2). All the building blocks
  this refactor needs landed in this branch.
- After that, the open items are the SASL re-auth pipeline driver
  (KIP-368), the concrete Schema Registry payload serdes (Avro /
  JSON-Schema / Protobuf), the FK-join DSL wiring (KIP-213), and
  the live-broker Docker fixture for CI. None block the others.
