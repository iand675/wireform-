---
title: Glossary
description: Definitions of Kafka, Kafka Streams, Riffle, and distributed-systems terms used throughout the Kafka Streams docs.
sidebar:
  order: 99
---

This glossary defines every term used across the Kafka Streams documentation. Use it when you encounter unfamiliar terminology.

Terms are organized alphabetically. Each definition includes the module where the concept lives and links to deeper explanations. Other pages in this documentation link directly to specific glossary entries. For example, the [Visibility page](./operating/visibility/) links to [event time](#event-time) when first introducing the concept.

---

## A

### ACID

**A**tomicity, **C**onsistency, **I**solation, **D**urability -
the four classical guarantees of a SQL database transaction.
Streaming systems give you a different bundle of guarantees, and
the [Visibility versus ACID databases](./operating/visibility/)
page works through the mismatch.

### Alignment group

A Riffle concept: a set of [watermark](#watermark) sources whose
watermarks should not diverge by more than a configured bound. A
fast source whose watermark out-paces the group's slowest member
by more than `agBound` is **backpressured**: the runtime pauses
fetching from it until the slowest member catches up. Lives in
`Kafka.Streams.Watermark.AlignmentGroup`.

### `applicationId`

The consumer-group identity and the prefix every internal topic
(changelog, repartition) inherits. Changing it is a fresh-start
deploy, not a rollout. See
[Topology evolution](./operating/topology-evolution/).

### Assignor

The component that decides which member of a [consumer
group](#consumer-group) owns which [partition](#partition) (and,
in Streams, which [task](#task)). The runtime's main consumer uses
the classic group protocol with the **range** assignor: partition
`p` of equally partitioned topics lands on the same member, which
[co-partitioned](#co-partitioning) tasks need. See
[Scaling](./operating/scaling/#processes-across-the-group).

### Async I/O operator

A Riffle [Prim](#prim) that runs the user's `IO` action on a
bounded worker pool instead of on the [stream thread](#stream-thread).
Provides backpressure, EOS-correct offsets, ordered or unordered
output, per-request timeout / retry, and explicit failure policy.
Full walkthrough in [Enrichment via external systems](./guides/enrichment/).

### At-least-once

The default [processing guarantee](#processing-guarantee). Every
record is processed *at least once*: and therefore *possibly more
than once* on a [rebalance](#rebalance) or fault. External side
effects should be idempotent.

---

## B

### Backpressure

The mechanism by which a slow downstream stage signals an upstream
stage to slow down. In Kafka Streams the consumer poll loop is
naturally backpressured by the processor it feeds: a partition
with `bufferedRecordsPerPartition` (default 1000) records waiting
is paused until its task catches up. In Riffle async I/O, a full in-flight queue
blocks the [stream thread](#stream-thread) on enqueue.

### Broker

A node in a Kafka cluster: the server-side process that stores
topic logs and serves produce / fetch requests. Multiple brokers
form a cluster; one is the controller (under KRaft mode) that
coordinates metadata.

---

## C

### CDC (Change Data Capture)

A pattern (and protocol family) where each row-level change in an
upstream database (insert, update, delete with before / after
images) is captured and published downstream. Debezium and AWS DMS
are the canonical implementations. Riffle's `Kafka.Streams.Sources.CDC`
ships a primitive for materialising CDC feeds into KTables with
snapshot/streaming-phase awareness and key-aware compaction.

### `Change`

What a [KTable](#ktable) operator forwards per update: the row's new
value and its old one (`Nothing` for an insert or a delete). It
lets a downstream table, join or re-grouped aggregation retract the
old value. Defined in `Kafka.Streams.KTable`.

### Changelog topic

The compacted internal Kafka topic the runtime writes a [state
store's](#state-store) updates to (every update: there is no record
cache), one partition per [task](#task). A task that moves or
restarts restores the store from it before processing: from its
checkpoint for persistent stores, else from the log start.
Convention: `<applicationId>-<storeName>-changelog`.

### Co-partitioning

The requirement that two topics being joined share the same
partition count and the same key partitioner, so each `(key, A)`
record and its matching `(key, B)` record land on the same task.
The runtime checks it at start: a stateful
[subtopology](#subtopology) reading topics with different partition
counts puts the instance in `StreamsError`.

### Cogroup

A Kafka Streams DSL operator that lets you aggregate multiple
input streams into one output table with a single aggregator
function per stream and a single shared store. The Haskell port is
`cogroup` / `addCogrouped` / `aggregateCogrouped` in
`Kafka.Streams.Cogroup`.

### Commit cycle

What the runtime runs every `commitIntervalMs` (`Nothing`: 100 ms
under exactly-once, 30 s otherwise) to make a batch of work durable
(`Kafka.Streams.Runtime.Tasks.tmCommit`): every task's
[pre-commit drains](#pre-commit-drain), a flush of every store
(writing its changelog), a producer flush, the
[commit participants'](#commit-participant) prepare, then the
offsets: under exactly-once `sendOffsetsToTransaction` +
`commitTransaction`, otherwise an offset commit; then the
participants' commit. A failed exactly-once commit aborts and
recovers every task. See the [Exactly-once page](./operating/exactly-once/).

### Commit participant

An external system that commits and aborts with the runtime's
[commits](#commit-cycle): a `CommitParticipant`
(`Kafka.Streams.Runtime.Commit`) registered with
`addCommitParticipant`. Each cycle it prepares after the flush and
commits once Kafka has; the committed cycles are recorded in
`<applicationId>-twophase-commits`, so prepared cycles left by a
crash are committed or aborted at the next start.
`twoPhaseParticipant` builds one from a
[two-phase commit sink](#two-phase-commit-sink).

### Consumer group

A set of Kafka consumers sharing the same `group.id` (in Streams,
the [`applicationId`](#applicationid)) that the broker
coordinator collectively assigns partitions to. Each partition is
owned by at most one member at a time.

### Cooperative-sticky

The JVM consumer's default assignor. This runtime's consumer uses
the range assignor instead; see [Assignor](#assignor).

### CQRS (Command Query Responsibility Segregation)

An architectural pattern in which writes go to one model
(commands) and reads come from another (queries), connected by an
asynchronous projection. Kafka Streams + downstream query store is
a natural CQRS implementation; the trade-off discussion lives in
[Visibility versus ACID databases](./operating/visibility/).

---

## D

### DLQ (Dead-letter queue)

A topic (or other sink) that receives records the runtime couldn't
process. Riffle's bounded `suppress` supports a dead-letter
overflow policy; `processing.exception.handler` (KIP-1033) supports
a DLQ disposition for records that throw.

### DSL

**D**omain-**s**pecific **l**anguage. In Kafka Streams, the
typed combinator API (`stream`, `mapValues`, `filter`, `groupBy`,
`count`, …) as opposed to the lower-level [Processor
API](#processor-api). The library exposes two DSL surfaces: the
Free-Arrow Free DSL (`Kafka.Streams.Topology.Free`) and the
imperative builder DSL (`Kafka.Streams.KStream`, etc.).

---

## E

### EOS / EOS-V2 / EOS-V3

**E**xactly-**o**nce **s**emantics. EOS-V2 (KIP-447, KIP-129) is
the transactional-producer-plus-`TxnOffsetCommit` story that
ensures records written by Streams and consumer offsets advance
atomically. EOS-V3 / KIP-892 (transactional state stores) is not
implemented: after an abort, stores are restored from their
changelogs instead. The `processingGuarantee` config selects between
`AtLeastOnceP` and `ExactlyOnceV2`.

### Emit policy

Riffle generalisation of the JVM Streams `EmitStrategy`
(KIP-825). A first-class `EmitPolicy` value any windowed /
stateful operator can consume: `EmitOnUpdate`,
`EmitOnWindowClose`, `EmitOnCount n`, or `EmitCustom` with a
user predicate. Lives in `Kafka.Streams.EmitPolicy`.

### Event time

The timestamp associated with a record by its producer: when the
underlying business event happened. Contrast with [processing
time](#processing-time): the timestamp the runtime saw the record.
The pair drives all of [windowing](#window), [watermarks](#watermark),
and [grace periods](#grace-period).

### Event-time TTL

Riffle KV-store wrapper: state expires based on the [coordinated
watermark](#watermark-coordinator), not on wall-clock. Lives in
`Kafka.Streams.State.KeyValue.TTL`. Pair with
`ttlClockFromCoordinator` for the coordinator-driven clock.

---

## F

### Fenced producer

A producer the broker has rejected because a newer producer with
the same `transactional.id` was observed. Surfaces as
`ProducerFencedException` / `InvalidProducerEpochException`. Each
instance has its own transactional id
(`<applicationId>-<random uuid>`), so a zombie instance is fenced by
the group generation instead (its `sendOffsetsToTransaction` is
rejected). See the [zombie-producer
runbook](./operating/runbooks/#producer-fenced--invalid_producer_epoch).

### Foreign-key join

A KTable-KTable join keyed not on the record key but on a
caller-supplied extractor of the left value. As in KIP-213, the left
side sends subscriptions through a `<base>-subscription-registration`
topic co-partitioned with the right table, and the right side
answers through a `<base>-subscription-response` topic
co-partitioned with the left; a hash of the left value drops stale
responses. The two tables can be partitioned differently. Haskell
port: `foreignKeyJoinKTable` / `leftForeignKeyJoinKTable` in
`Kafka.Streams.ForeignKeyJoin`.

### Free arrow / `FreeArrow`

A free construction over the `Arrow` typeclass: the typed DSL is
represented as a value, then interpreted / optimised / compiled at
the boundary. `Kafka.Streams.Topology.Free` builds a
`FreeArrow Prim` AST that the compiler walks. The benefit:
topologies are first-class values you can inspect, snapshot, and
optimise before they run.

---

## G

### GADT (Generalized Algebraic Data Type)

A Haskell ADT whose constructors can refine the type parameters of
the resulting value. Used by `Prim` in
`Kafka.Streams.Topology.Free` to encode the input / output types
of each operator at the type level.

### GlobalKTable

A KTable replicated in full on every instance, not partitioned
across the group. Used for small reference data (currency rates,
country lookups) where you want zero-network-cost lookups. Loaded
from a Kafka topic via `globalTable`.

### Grace period

The amount of time a [window](#window) stays open accepting late
records past its end. A 1-hour tumbling window with a 10-minute
grace closes 70 minutes after the window's start in event time.
Configured via `withGracePeriod` / `withSessionGracePeriod`.

---

## H

### Heartbeat

The periodic message a [consumer group](#consumer-group) member
sends the coordinator to confirm liveness. Under [KIP-848](#kip),
heartbeats also carry [subscription metadata](#subscription-metadata)
and receive [reconciliation](#reconciliation) deltas.

### Hopping window

A fixed-size [window](#window) that advances by a step smaller
than its size: adjacent windows overlap. A 5-minute hopping
window with a 1-minute advance produces a new window every minute,
each containing the last 5 minutes' worth of records.

---

## I

### Idempotent / idempotency

An operation that has the same effect when invoked multiple times
as when invoked once. `PUT /resource/42` with the same body is
idempotent; `POST /create-order` typically isn't. Critical for
external side effects under [at-least-once](#at-least-once) or for
the recovery path of a [two-phase commit sink](#two-phase-commit-sink).

### Idempotency token

A stable per-record identifier (usually the upstream `(topic,
partition, offset)` tuple) written to a [state store](#state-store)
to deduplicate side effects on [replay](#replay). The pattern: on
each record, check the store; if absent, fire the side effect,
then write the token. See [Enrichment](./guides/enrichment/#pattern-6-idempotency-tokens-in-a-state-store).

### Idle source

A [watermark](#watermark) source that hasn't produced records for
longer than its `IdleAfter` threshold. The
[coordinator](#watermark-coordinator) excludes idle sources from
the min-watermark computation so a quiet partition doesn't stall
downstream windows.

### Interactive query (IQ)

Read-only access to a live [state store](#state-store) from
outside the [stream thread](#stream-thread). Lives in
`Kafka.Streams.InteractiveQueries`. The query layer is your code;
the library exposes the typed handles + cross-instance discovery
metadata (KIP-535).

### Internal topic

A Kafka topic the framework auto-creates and owns:
[changelog](#changelog-topic) topics for state stores,
[repartition](#repartition-topic) topics for keyed shuffles. Their
names derive from the `applicationId` and the [stable
name](#stable-name) of the owning operator.

---

## J

### Join

An operator that combines records from two streams (or a stream
and a table). Five shapes: stream-stream (windowed), stream-table,
table-table, stream-[GlobalKTable](#globalktable), and [foreign-key](#foreign-key-join).

---

## K

### `KafkaStreams`

The runtime handle. `newKafkaStreams` constructs it from a
topology + config; `startKafkaStreams` runs it. `closeKafkaStreams`
drains and shuts down. Most live operations
(`setStateListener`, `addStreamThread`, `pauseKafkaStreams`, etc.)
take a `KafkaStreams` value.

### Key-group

A Riffle abstraction: a fixed routing space (default 128) into
which record keys hash, decoupling worker count from
[partition](#partition) count. Lives in
`Kafka.Streams.Runtime.KeyGroup`, a separate library: the runtime
doesn't use it. See [Scaling](./operating/scaling/#key-groups).

### KGroupedStream / KGroupedTable

Intermediate types produced by `groupBy` / `groupByKey` on a
stream / table. They're the input to aggregation operators
(`count`, `reduce`, `aggregate`).

### KIP

**K**afka **I**mprovement **P**roposal: the Apache Kafka design-
review process. Specific KIPs that come up by name across these
docs:

| KIP | What |
| --- | ---- |
| **KIP-295** | `topology.optimization` config knob (reuse-source-topics, merge-repartitions, single-store-self-join) |
| **KIP-307** | Stable, deterministic processor names |
| **KIP-418** | Named branches (`Branched.*`) |
| **KIP-441** | Probing rebalances (`probingRebalanceIntervalMs`) |
| **KIP-447** | EOS-V2 producer-per-instance with `TxnOffsetCommit` |
| **KIP-535** | Cross-instance IQ discovery (`StreamsMetadata`, `KeyQueryMetadata`) |
| **KIP-591** | DSL store backend selection (`DslStoreSuppliers` on the builder) |
| **KIP-825** | First-class `EmitStrategy` |
| **KIP-848** | Next-gen consumer-group protocol: broker-side incremental reconciliation |
| **KIP-892** | EOS-V3 transactional state stores |
| **KIP-924** | In-process `TaskAssignor` plug-in |
| **KIP-925** | Rack-aware assignment strategy |
| **KIP-1033** | First-class processing-exception handler with DLQ disposition |

### KStream

A Kafka Streams record stream: an append-only sequence of `(key,
value, timestamp, headers)` tuples. Compare with [KTable](#ktable).

### KTable

A Kafka Streams changelog stream interpreted as a key-value table:
later records overwrite earlier ones for the same key, and a
tombstone deletes it. Materialised into a [state store](#state-store)
(in-memory or RocksDB). Operators forward a `Change` (new and old
value) per update, so deletes and filter retractions reach every
derived table; `toChangelogStream` keeps them as tombstones,
`toKStreamFromKTable` keeps current values only.
[GlobalKTable](#globalktable) is the fully-replicated variant.

### KRaft

Kafka's Raft-based metadata protocol that replaced ZooKeeper for
cluster coordination. The library's integration tests spin up a
KRaft-mode broker; the streams runtime is KRaft-agnostic at the
client layer.

---

## L

### Linearisable

A consistency model in which every operation appears to take
effect atomically at some point between its invocation and its
response. The Kafka Streams in-memory stores' single-key reads
are linearisable; iterators are eager snapshots, not
linearisable across mutations.

### Little's law

The relation `L = λW`: the average number of items in a system
equals the arrival rate times the average time in the system.
Used in [Enrichment](./guides/enrichment/#picking-aiobuffercapacity-and-aioworkers)
to size async-I/O worker pools: `workers ≈ throughput × latency`.

### Log compaction

A Kafka topic-level retention policy that keeps the latest value
per key indefinitely (rather than truncating by time or size).
Changelog topics use compaction so state recovery from offset 0
remains bounded by unique-key count, not by total record count.

---

## M

### `Materialized`

The DSL knob that controls which [state store](#state-store)
backend an aggregation, join, or table-source uses. Picks the
store builder and the serdes. Mirrors the JVM
`Materialized` builder.

### Member epoch / rebalance epoch

KIP-848 versioning. Member epoch bumps every time a member's
owned assignment changes; rebalance epoch bumps every time the
group-wide target changes. Stale-epoch heartbeats must reconcile
before continuing.

### `MockBroker`

The in-memory Kafka broker in `Kafka.Streams.Runtime.MockBroker`
for testing the real runtime without a cluster: topics and
partitions, consumer groups with rebalances, transactions with
`read_committed` visibility, and fault injection (`mockFailSends`,
`mockFailTransactionCommits`, `mockFailOffsetCommits`).
`mockDriverFor` gives `startKafkaStreamsWith` a driver over it.

---

## O

### Object store

Riffle's abstraction over S3 / GCS / Azure Blob / a filesystem.
Snapshot stores write their snapshot blobs through an
`ObjectStoreClient` and read them back on recovery. Lives in
`Kafka.Streams.Runtime.ObjectStore`.

### Offset

The position of a record within a [partition](#partition). The
consumer commits offsets to the broker so a restart resumes from
the right place. Under EOS, offsets advance only when the
[commit cycle](#commit-cycle) commits.

### Orphan internal topic

A [changelog](#changelog-topic) or [repartition](#repartition-topic)
topic on the broker that doesn't correspond to any store /
operator in the current topology. Usually a sign that a previous
deploy renamed something. Detected by
`Kafka.Streams.Observability.OrphanTopics.detectOrphans`. See the
[runbook](./operating/runbooks/#orphan-internal-topics-detected).

---

## P

### Parallelism

The number of concurrent units of work the runtime can run:
`numStreamThreads × instances`, capped by the number of
[tasks](#task), i.e. by source-topic [partition](#partition)
counts.

### Partition

A subset of a Kafka topic's data, distributed across brokers and
consumed in parallel. Records with the same key always land on the
same partition (under the default partitioner). The partition is
the unit of consumer-group assignment.

### Pipeline

A `newtype` in `Kafka.Streams.Pipeline` wrapping `a -> IO b` with
`Category`, `Arrow`, `ArrowChoice`, `Functor`, and `Applicative`
instances. Used to build reusable, named topology fragments that
compose with `(>>>)`. The `ArrowChoice` instance gives you the
[ROP](#railway-oriented-programming) `(+++)` / `(|||)` combinators
over `Either`.

### Pre-commit drain

Riffle hook (`ProcessorContext.ctxRegisterPreCommitDrain`) that
lets an async operator block the [commit cycle](#commit-cycle)
until its in-flight queue is empty. Used by `asyncMapValues` etc.
to make offset commits EOS-correct.

### `Prim`

The GADT constructor type in `Kafka.Streams.Topology.Free` that
represents one operator in the typed AST. Every DSL combinator
(`mapValues`, `filter`, `groupBy`, …) corresponds to a `Prim`.

### Probing rebalance

The cadence at which the runtime asks for a rebalance when a warmup
reported through `reportWarmupLag` is within
`acceptableRecoveryLag` (nothing reports one on its own, since
there are no [standby](#standby-task) tasks). Interval `0` disables it. Lives in
`Kafka.Streams.Runtime.ProbingRebalance`. Default cadence: 10
minutes (`probingRebalanceIntervalMs`).

### Processing guarantee

`AtLeastOnceP` vs `ExactlyOnceV2` in `StreamsConfig.processingGuarantee`.
Determines whether the producer is transactional, whether offsets
commit through `TxnOffsetCommit`, whether consumers read
`read_committed`, and the default commit interval. See
[EOS](#eos--eos-v2--eos-v3).

### Processing time

The wall-clock time the runtime saw a record. Contrast with
[event time](#event-time).

### Processor API

The lower-level Kafka Streams API: write a `Processor` /
`FixedKeyProcessor` directly with `process` + `init` callbacks,
access state stores by name, schedule [punctuators](#punctuator).
The DSL ultimately compiles to processor-API calls.

### Punctuator

A scheduled callback inside a processor. Two clocks:
`WallClockTimePunctuation` (every N ms of real time) and
`StreamTimePunctuation` (every N ms of [event time](#event-time)
advance). Used for time-driven emits, idle-window detection,
TTL expiry.

---

## R

### Rebalance

The process by which a [consumer group](#consumer-group)
reassigns partition / [task](#task) ownership across its members
in response to a membership change. The runtime's consumer uses
the classic protocol: before rejoining it commits, then closes the
tasks of revoked partitions and restores the tasks of assigned
ones. See [Scaling](./operating/scaling/#processes-across-the-group).

### Reconciliation

KIP-848 term for the diff between a member's currently-owned
assignment and its target assignment. `Reconciliation` carries
`rAdd` and `rRemove` sets; a losing member acknowledges its
`rRemove` before the gaining member sees the task in `rAdd`.
Defined in `Kafka.Streams.Runtime.RebalanceProtocol`, a model the
runtime doesn't use (its consumer runs the classic protocol).

### Remote KV

A Riffle KV-store backend with no local state: every get / put
is a network call against a remote store (FoundationDB / TiKV /
DynamoDB shape). Node restart is a metadata operation. Lives in
`Kafka.Streams.State.KeyValue.Remote`.

### Repartition

The operation of re-keying a stream and re-publishing it to an
internal topic so downstream stateful operators see records
co-partitioned. Performed by `repartition` / `through` /
implicit auto-insert (`optAutoInsertRepartition`).

### Repartition topic

The [internal topic](#internal-topic) a `repartition` operator
writes to. Convention: `<applicationId>-<nodeName>-repartition`.

### Replay

Re-processing records the runtime already saw, after a fault or
[rebalance](#rebalance) rewinds the consumer to a prior committed
[offset](#offset). Under [at-least-once](#at-least-once) this is
the normal recovery path; under EOS it still happens but the
duplicate output is aborted.

### Railway-oriented programming

A pattern, popularised by Scott Wlaschin's F# write-up, for
modelling pipelines of fallible operations as two parallel
tracks: a success track and a failure track. Each stage either
advances the value on the success track or routes it to the
failure track; failure short-circuits all downstream success
stages cleanly. Kafka Streams' `DeserializationHandler` /
`ProcessingExceptionHandler` / `ProductionHandler` /
`AsyncFailurePolicy` / `SinkOutcome` are all track-switch
surfaces; the [`Pipeline`](#pipeline) newtype's `ArrowChoice`
instance is the explicit ROP combinator set. Full mapping in
[Railway-oriented programming with streams](../concepts/railway-oriented-programming/).

---

## S

### Schema Registry

Confluent's per-subject schema-versioning service. The library's
serdes (`Kafka.Streams.Serde.Avro`, `Kafka.Streams.Serde.JsonSchema`,
`Kafka.Streams.Serde.Protobuf`) speak the Confluent envelope and
the `registrySerdeChecked` wrapper enforces compatibility-mode
checks at construction time.

### `SchemaVersioned` store

Riffle KV-store wrapper that tags every write with a
`SchemaVersion` and migrates reads forward through a chain of
`SchemaMigration` callbacks. `burnInMigrate` rewrites older
entries in-place with resumable progress. Lives in
`Kafka.Streams.State.KeyValue.SchemaVersioned`.

### `SinkTxnId`

The Riffle identifier for one in-flight transaction on a
[two-phase commit sink](#two-phase-commit-sink): the runtime's
commit cycle id, `<instance>/<n>`. Recovery compares it with the
last committed cycle of that instance to commit or abort it.

### Snapshot store

A Riffle KV-store backend that incrementally writes snapshots of
its state to an [object store](#object-store). Recovery time
becomes `O(time-since-last-snapshot)` rather than `O(state-size)`.
Lives in `Kafka.Streams.State.KeyValue.Snapshot`.

### Stable name

A deterministic, build-stable name for an operator node. Mirrors
the JVM's KIP-307 generator: `KSTREAM-MAPVALUES-0000000007`. The
name is part of the deployment contract because [internal
topics](#internal-topic) inherit from it. Lives in
`Kafka.Streams.Topology.StableNames`.

### Standby task

In the JVM, a warm replica of an active [task's](#task) state,
maintained by tailing its [changelog](#changelog-topic). The runtime
doesn't run standbys (`numStandbyReplicas > 0` only logs a warning):
a task that moves restores from the changelog. The
`Kafka.Streams.Runtime.Standby*` modules are standalone building
blocks.

### State store

The persistent (or in-memory) key-value, window, or session store
attached to a stateful operator. Backends: in-memory + RocksDB
(via `+rocksdb`); Riffle adds snapshot, tiered, remote, and
versioned variants. Read-only IQ access via
`Kafka.Streams.InteractiveQueries`.

### `StreamsConfig`

The top-level runtime configuration record. Mirrors the Java
`StreamsConfig` properties. Defined in `Kafka.Streams.Config`.

### `StreamsMetadata`

KIP-535 record: what each instance in the group looks like to its
peers (host:port, owned partitions per store, standby partitions
per store), in `Kafka.Streams.Discovery`. The runtime doesn't
publish it (there is no `application.server`); it's there for an
external IQ proxy that collects it itself.

### `StreamTime`

The per-task event-time clock: running max of extracted
timestamps on records this task has seen. Replaced by the
[coordinated watermark](#watermark-coordinator) where Riffle
opts in.

### Stream thread

One of the `numStreamThreads` workers an instance spreads its
[tasks](#task) over between polls. Unlike the JVM, there is one
consumer per instance, not per stream thread, and tasks aren't tied
to a worker: `addStreamThread` / `removeStreamThread` change the
count without a rebalance. Workers run user-supplied processor code;
nothing in user code should block one for long.

### Subscription metadata

Bytes a [consumer group](#consumer-group) member attaches to its
JoinGroup request. The JVM's Streams assignor uses it to advertise
`application.server`, owned standby tasks, and per-member state;
this runtime's consumer runs the plain range assignor and sends
none of that.

### Subtopology

A connected piece of the [topology](#topology): nodes linked by an
edge, or sharing a state store, belong together; a
[repartition topic](#repartition-topic) is a cut. Each subtopology
runs as one [task](#task) per partition number. Computed by
`Kafka.Streams.Topology.Subtopology`.

### Suppress

The DSL operator that holds emissions back until a condition is
met: typically "until window closes" or "until time limit
expires". Riffle adds a bounded variant with explicit
`BufferOverflowPolicy` (`DropOldestSilently`, `ShutdownWhenFull`,
`suppressWindowedShed` to DLQ).

---

## T

### Task

The unit of [parallelism](#parallelism). One task owns partition
`p` of every source topic of one [subtopology](#subtopology), its
own [state stores](#state-store), and partition `p` of their
[changelogs](#changelog-topic). `TaskId` identifies a
`(subtopology, partition)` pair.

### `TaskAssignor`

KIP-924 in-process plug-in (`StreamsConfig.taskAssignor`).
`streamsRunUserAssignor` builds an `ApplicationState` from the
instance's local view and invokes `taAssign` on it. The result
doesn't drive the consumer, whose assignment comes from the range
assignor.

### Tiered KV

A Riffle KV-store backend with a hot tier (local in-memory or
RocksDB) + a cold tier (S3 / GCS). Reads probe hot, fall through
to cold, and promote. Eviction decides which entries demote when
the hot tier exceeds its budget. Lives in
`Kafka.Streams.State.KeyValue.Tiered`.

### `Topology`

The compiled, validated graph the runtime executes. `Topology i o` is a type with two parameters, like a function `i -> o`.

- **Input type `i`**: What stream type enters the topology. `Void` means the topology pulls from sources (Kafka topics), not from other code.
- **Output type `o`**: What stream type exits the topology. `()` means the topology pushes to sinks (Kafka topics), not to other code.

Built from the DSL value of type `Topology Void ()` (or other combinations), or directly via the imperative `Kafka.Streams.Topology` builder. Validated by `validateTopology` before the runtime starts.

### `TopicNameExtractor`

KIP-303 sink-side dynamic-topic-name extractor. Lets one sink
route records to different topics based on the record. The library
exposes it via `toExtracted` + `TopicNameExtractor`. Useful when
you want "dynamic topology" without actually mutating the
compiled topology.

### Transactional producer

A Kafka producer with a `transactional.id` that supports
`beginTransaction` / `commitTransaction` / `abortTransaction` /
`sendOffsetsToTransaction`. The foundation of [EOS-V2](#eos--eos-v2--eos-v3).

### Tumbling window

A fixed-size, non-overlapping [window](#window). A 5-minute
tumbling window produces a new window every 5 minutes; each record
belongs to exactly one window.

### Two-phase commit sink

A Riffle sink interface that commits external-system writes
atomically with the runtime's [commit](#commit-cycle), as a
[commit participant](#commit-participant) (`twoPhaseParticipant`).
Records reach it through the topology's `sinkTwoPhase` node.
Six operations: `tpsStage` (per-record buffer), `tpsPrepare`
(promote batch to "prepared"), `tpsCommit` (atomically make
visible), `tpsAbort` (discard a prepared batch), `tpsDiscardStaged`
(drop unprepared rows when the runtime rewinds), `tpsRecover`
(resolve half-committed txns on restart). Lives in
`Kafka.Streams.Sinks.TwoPhase`.

---

## W

### WAL (Write-ahead log)

A durable log of intended changes written before the changes are
applied to the underlying store. Under Riffle [snapshot
stores](#snapshot-store) the [changelog topic](#changelog-topic) is
the WAL between snapshots: not the sole source of truth.

### Watermark

A [timestamp](#event-time) that the runtime guarantees no later
record will arrive before. Lets downstream operators decide when
a window can close, when state can be expired, when a stream-
stream join's window is finalised. Derived from a
`WatermarkGenerator` (`MonotonicTimestamps`, `BoundedOutOfOrderness`,
`CustomGenerator`).

### Watermark coordinator

The Riffle per-`StreamsApp` component that aggregates per-source
watermarks into the effective watermark (= min of live, non-idle
sources), handles [idle-source](#idle-source) detection, and
enforces [alignment-group](#alignment-group) backpressure. Lives
in `Kafka.Streams.Watermark`.

### Window

A bounded slice of [event time](#event-time) over which a
stateful operator aggregates. Four shapes: [tumbling](#tumbling-window),
[hopping](#hopping-window), sliding, and session. Each has its own
builder in `Kafka.Streams.Window`.

---

## Glossary maintenance

If a term appears in the docs and isn't defined here, add it.
Prefer one-paragraph definitions; link out to deeper coverage
rather than restating it.
