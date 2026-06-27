---
title: "Riffle: Extended features"
description: "Optional extensions that add Flink-class capabilities: async I/O, snapshot recovery, two-phase commit sinks, and more."
sidebar:
  order: 1
  label: Extended features
---

Kafka Streams is deliberately minimal. It gives you stateful stream processing as a library: no cluster to deploy, no job submission system, no resource manager. You compile your topology into your application binary, start it, and the framework handles partition assignment, state management, and offset tracking. That simplicity is its main advantage over heavier systems like Flink.

But the simplicity has a cost. As your application grows, you start hitting walls that the base Kafka Streams model wasn't designed to solve. Your enrichment calls block the stream thread. Your state stores take 45 minutes to recover after a restart. Your exactly-once guarantees evaporate the moment you write to Postgres. Your windows stall because one partition stopped producing. You need 64-way parallelism but your topic only has 12 partitions.

These aren't edge cases. They're the normal trajectory of a stream processing application that started simple and grew. In the JVM Kafka Streams ecosystem, each of these problems has a different workaround: custom thread pools, manual state checkpointing, outbox patterns, polling hacks. Flink solves most of them natively, but Flink is a distributed cluster you have to operate.

Riffle is a set of opt-in extensions that solve these problems while keeping the library model. Every feature is independent, every feature is additive, and a topology that uses none of them compiles to the same imperative graph as base Kafka Streams.

## Async I/O

### The problem

A very common stream processing pattern is enrichment: for each incoming record, call an external service to add information. Look up a user profile from a REST API. Fetch pricing data from a database. Call a fraud-scoring model over gRPC.

In base Kafka Streams, this happens synchronously on the stream thread:

```haskell
mapValuesM (\record -> do
  profile <- callUserAPI (userId record)
  pure (record { userProfile = profile }))
```

This works, but the stream thread is single-threaded per task. While it waits 50ms for the HTTP response, it processes zero records. If your enrichment service has P99 latency of 200ms and you're processing 10,000 records per second, you need 2,000 concurrent requests just to keep up. A single stream thread can do one at a time.

The usual workaround is to spin up your own thread pool, manage a bounded queue, drain it before commits, and handle failures yourself. That's a lot of concurrency plumbing for what should be a simple "call this API for each record" operation.

### The solution

Riffle's async I/O operators move external calls off the stream thread entirely. You describe the call, the concurrency bounds, and what to do when things fail. The runtime handles the rest.

```haskell
asyncMapValues
  (asyncIOConfig
    { aioMaxConcurrency = 64
    , aioTimeout = 5_000_000       -- 5 seconds
    , aioOutputMode = Ordered      -- or Unordered for higher throughput
    , aioFailurePolicy = SkipAndLog
    })
  (\record -> callEnrichmentAPI (rvValue record))
```

`aioMaxConcurrency` caps how many requests are in flight at once; this is your backpressure knob. `aioOutputMode` controls whether results are emitted in input order (safer, slightly slower) or as they complete (higher throughput when order doesn't matter). `aioFailurePolicy` determines whether a failed request skips the record, retries, or shuts down the task.

Exactly-once semantics are preserved through a drain mechanism: before any offset commit, the runtime blocks the stream thread until every in-flight async request completes. This ensures that committed offsets always reflect fully-processed records, even though the processing happened concurrently on other threads.

The topology optimizer can also fuse adjacent synchronous operators into an async operator's callback, so you don't pay the async overhead for the pure transformation steps that follow the external call.

Full walkthrough with capacity sizing: [Enrichment via external systems](../guides/enrichment/).

## Snapshot stores

### The problem

Kafka Streams maintains state by writing every state change to a changelog topic. When a task starts (a fresh deploy, a rebalance, or a crash recovery), it replays the entire changelog from offset zero to rebuild the state store.

This design is sound and correct, but recovery time scales with state size. A state store holding 50 GB of data means replaying 50 GB of changelog records, which can take 30-60 minutes depending on your broker throughput and record complexity. On a rolling deploy with 20 instances, each one waits its turn. What should be a 5-minute deploy becomes a multi-hour operation.

Standby replicas help (they pre-replicate state so that a rebalanced task can start from a warm copy), but they double your state storage requirements and still need to catch up on any records written since the last replication cycle.

### The solution

Riffle adds snapshot-based recovery. The runtime periodically checkpoints the entire state store to a durable object store: local filesystem for development, S3 or GCS or Azure Blob for production. The changelog still exists and still captures every write, but it now acts as a write-ahead log between snapshots rather than the sole recovery mechanism.

On restart, the runtime:

1. Finds the latest snapshot for each state store
2. Loads it (a bulk read, not a record-by-record replay)
3. Replays only the changelog records written after the snapshot's offset

Recovery time becomes proportional to time-since-last-snapshot instead of total state size. If you snapshot every 10 minutes and your changelog write rate is 1,000 records/second, recovery replays at most 600,000 records instead of the full 50 GB history.

Riffle provides several store backends depending on how much state you have and where you want it to live:

**Snapshot-aware KV** is the most common choice. It wraps a standard key-value store with periodic checkpointing. Your existing store code doesn't change; you just swap the store builder. Import from `Kafka.Streams.State.KeyValue.Snapshot`.

**Hot + cold tiered KV** is for state that exceeds local disk. Recent entries live in a fast local tier; older entries live in cold storage (typically S3). Reads probe the hot tier first and fall through to cold, promoting accessed entries back to hot. Import from `Kafka.Streams.State.KeyValue.Tiered`.

**Remote KV** eliminates local state entirely. The store lives in a remote system (matching the shape of FoundationDB, TiKV, or DynamoDB). Node restart becomes a metadata operation: there's no state to recover, just a connection to re-establish. Import from `Kafka.Streams.State.KeyValue.Remote`.

**Pointer-mode standby** is for environments where standby replicas are too expensive to maintain as full copies. Instead of replicating the entire state, the standby tracks a `(snapshotId, offset)` pair. When promoted to active, it fetches the snapshot and replays from the offset. Import from `Kafka.Streams.Runtime.StandbyTask`.

See [Topology evolution](../operating/topology-evolution/) for how snapshot stores change rolling deploy strategies.

## Two-phase commit sinks

### The problem

Kafka's exactly-once semantics (EOS) work through transactional producers. The stream thread reads records, processes them, writes output records and offset commits in a single atomic transaction. If the transaction commits, both the output and the offsets are visible. If it aborts, neither is.

This guarantee holds as long as the output goes to Kafka. The moment your sink is an external system (a Postgres INSERT, an Iceberg table append, an S3 file upload, an HTTP POST), the atomicity breaks. There are two separate systems now, and no single transaction spans both.

Consider what happens during a crash:

1. Your processor writes a row to Postgres. The write succeeds.
2. Your processor commits the Kafka transaction (output records + offset).
3. The process crashes before the commit completes.
4. On restart, Kafka replays from the last committed offset, which is before step 1.
5. Your processor writes the same row to Postgres again. Duplicate.

The reverse is also possible: the Kafka transaction commits but the Postgres write was lost, leaving a gap. The fundamental issue is that you have two systems that need to agree on whether a piece of work happened, and there's no coordinator.

### The solution

Riffle provides a two-phase commit (2PC) sink interface that coordinates the external system's transaction lifecycle with Kafka's. The protocol works like this:

1. **Prepare**: The sink accumulates writes during the processing window.
2. **Pre-commit**: The sink prepares its transaction (e.g., `PREPARE TRANSACTION` in Postgres). If this fails, the Kafka transaction aborts too.
3. **Kafka commit**: The producer transaction commits offsets and output records.
4. **Sink commit**: The sink commits its prepared transaction (e.g., `COMMIT PREPARED` in Postgres).
5. **Recovery**: If the process crashes between steps 3 and 4, the sink transaction is left in prepared state. On next boot, `tpsRecover` inspects the prepared transaction and decides whether to commit or abort it based on whether the Kafka transaction landed.

```haskell
sinkTwoPhase "postgres-sink" postgresSink produced
```

You implement the `TwoPhaseSink` interface for your external system. The interface has four methods: `tpsPreCommit` (prepare the transaction), `tpsCommit` (commit it), `tpsAbort` (roll it back), and `tpsRecover` (resolve any in-doubt transactions from a previous run).

The contract and reference sinks (in-memory, filesystem, HTTP echo) ship in core so you can develop and test locally. Production adapters for JDBC, Iceberg, S3, and HTTP live in separate packages to keep the core dependency footprint small.

Operator walkthrough: [Exactly-once across Kafka and other systems](../operating/exactly-once/).

## Watermark coordinator

### The problem

Kafka Streams tracks "stream time" per task: the maximum timestamp seen so far across all records processed by that task. Windowed operators use stream time to decide when a window is complete and can be emitted.

This works well when you have a single source with a steady record rate. It breaks in two important ways:

**Mixed-rate joins.** You're joining a high-volume clickstream (thousands of records per second) with a low-volume user-update stream (a few records per minute). Stream time advances rapidly on the clickstream side. The user-update side barely moves. Any window that spans both sources can't close until the slow side catches up, which might take minutes. Meanwhile, the fast side accumulates unbounded state.

**Idle partitions.** A partition stops producing records entirely. Maybe the upstream producer crashed; maybe traffic naturally dried up on that shard. Stream time for that partition freezes. Any window that includes that partition will never close, because the watermark never advances past the last record.

Both of these are well-known problems in stream processing. Flink solves them with a global watermark mechanism. Base Kafka Streams doesn't have one.

### The solution

Riffle adds a per-application watermark coordinator. Each source registers a watermark strategy that describes how to extract timestamps and what to do about idleness:

```haskell
let clickStrategy = boundedOutOfOrderness (seconds 5)
let userStrategy  = boundedOutOfOrderness (seconds 30)
                      & withIdleness (idleAfter (seconds 60))

addSourceWith "clicks" (consumed & withWatermarkStrategy clickStrategy)
addSourceWith "users"  (consumed & withWatermarkStrategy userStrategy)
```

`boundedOutOfOrderness` says "timestamps might arrive up to N seconds late; hold the watermark back by that much." `withIdleness` says "if this source produces no records for 60 seconds, stop waiting for it and exclude it from the global watermark calculation."

The coordinator tracks the minimum watermark across all active (non-idle) sources. When a window's end time falls below the global watermark, the window can safely close and emit results. Fast sources can optionally be backpressured via alignment groups to prevent them from racing too far ahead of slow sources.

Sources that don't register a watermark strategy keep the legacy per-task stream-time model. There's no runtime cost for sources that don't use this feature.

The `suppress` operator already uses coordinated watermarks when available. You can also pair the coordinator with event-time TTL (via `ttlClockFromCoordinator`) so that state expiry is driven by data timestamps rather than wall-clock time.

See [Visibility versus ACID databases](../operating/visibility/).

## Key-group routing

### The problem

In Kafka Streams, parallelism equals partition count. Each partition maps to exactly one stream task, and each task runs on exactly one thread. If your topic has 12 partitions, you can run at most 12 concurrent tasks.

This is fine when you create the topic with enough partitions up front. But requirements change. Maybe your stateful aggregation was handling 1,000 records/second when you launched, and now it handles 50,000. You need more parallelism, but the topic already has 12 partitions with years of data. Repartitioning means creating a new topic, migrating data, updating every producer and consumer, and coordinating the cutover. It's one of the most disruptive operations in the Kafka ecosystem.

### The solution

Key-group routing decouples parallelism from the topic's partition count. Instead of assigning one task per partition, the runtime hashes each record's key onto one of N key-groups (default 128, configurable). The group assignor distributes key-groups across workers, and workers can handle multiple key-groups concurrently.

```haskell
let config = defaultStreamsConfig
      { dispatchMode = DispatchKeyGroup (keyGroupConfig 128) }
```

With 128 key-groups, you can scale from 1 worker to 128 workers without touching the topic. Key-group assignment is sticky: the assignor tries to keep key-groups on the same worker across rebalances to minimize state migration.

The standard dispatch modes (`DispatchPartition` for one-task-per-partition, `DispatchHashed` for hash-based routing within a partition) remain available and stay the default. Key-group routing is purely opt-in.

See [Scaling and rebalancing](../operating/scaling/).

## Additional features

Beyond the major features above, Riffle includes a set of smaller improvements that address common operational pain points:

**Typed store references.** Base Kafka Streams uses string keys to look up state stores, returning an untyped handle that you cast at runtime. A typo in the store name or a wrong type cast is a runtime exception. Riffle's `StoreRef k v` carries the key and value types as phantom parameters, so the compiler catches mismatches. Import from `Kafka.Streams.State.Ref`.

**Bounded suppress.** The standard `suppress(untilWindowCloses)` operator buffers suppressed records in memory with no upper bound. Under sustained load, this can exhaust heap space. Riffle's bounded variant lets you set an explicit `BufferOverflowPolicy`: drop the oldest records, shut down the task cleanly, or shed excess records to a dead-letter queue. Import from `Kafka.Streams.Suppress`.

**Schema-versioned stores.** When the serialization format of your state store changes, base Kafka Streams requires a full repartition to migrate existing data. Riffle's `SchemaVersioned` wrapper associates each store with a version and a chain of migration functions. `burnInMigrate` rewrites entries in the background with resumable progress, so you can evolve your state schema without downtime. Import from `Kafka.Streams.State.KeyValue.SchemaVersioned`.

**Configurable emit policies.** Windowed operators in base Kafka Streams emit on every update. Sometimes you want to emit only when the window closes, or every N records, or on a custom predicate. `EmitPolicy` makes this configurable per operator. Import from `Kafka.Streams.EmitPolicy`.

**CDC sources.** Materializing a Change Data Capture feed (Debezium, AWS DMS) into a KTable requires manually handling the snapshot-vs-streaming phase transition, schema changes, and key-aware compaction. `cdcSource` handles all of this. Import from `Kafka.Streams.Sources.CDC`.

**Orphan topic detection.** When you rename or remove a topology, the internal topics it created (changelogs, repartition topics) stay on the broker. Over time, these orphans accumulate. `detectOrphans` compares the expected internal topics (derived from the current topology) against what actually exists on the broker, and surfaces any drift as a startup diagnostic. Import from `Kafka.Streams.Observability.OrphanTopics`.

**Structured observability.** Per-operator lag, queue depths, and time-spent metrics, plus `topologyDescription` which emits a versioned JSON representation of the running topology suitable for UI overlays and monitoring dashboards. Import from `Kafka.Streams.Observability.Topology`.

## Adoption path

Every Riffle feature is independent. You can adopt one without adopting any of the others, and the order doesn't matter. That said, if you're looking for a sequence that maximizes value with minimal risk, here's what we'd suggest:

1. **Start with observability.** Add topology JSON export and orphan-topic detection. This changes zero runtime behavior but gives you visibility into your topology's shape and health. You'll want this instrumentation in place before changing anything else.

2. **Add async I/O where you have blocking calls.** If any of your processors call external services synchronously, this is likely your biggest throughput bottleneck. Swap one `mapValuesM` at a time, verify the throughput improvement and EOS behavior, and expand from there.

3. **Add bounded suppress** if you're running windowed aggregations under sustained load and seeing memory pressure from the unbounded suppress buffer.

4. **Switch to typed StoreRef** during any refactor that touches state store access. The old stringly-typed calls keep working alongside the new typed ones, so you can migrate incrementally.

5. **Add snapshot stores** when changelog replay time becomes the gate on rolling deploys. This is the biggest infrastructure change (you need an object store), but the payoff is dramatic for large state.

6. **Add the watermark coordinator** when you start joining mixed-rate sources or hit idle-partition stalls in your windowed operations.

7. **Add 2PC sinks** when you have external writes that need atomic-with-Kafka semantics.

8. **Switch to key-group dispatch** when you hit the partition-count parallelism ceiling and can't or don't want to repartition topics.

You can stop at any step. Every step is an additive deploy; you're never committed to the full set.

## Related reading

- [Enrichment via external systems](../guides/enrichment/): async I/O walkthrough with capacity sizing
- [Topology evolution](../operating/topology-evolution/): how snapshot stores change rolling deploys
- [Scaling and rebalancing](../operating/scaling/): key-groups and rebalance
- [Exactly-once across Kafka and other systems](../operating/exactly-once/): the 2PC sink contract
- [Observability](../operating/observability/): topology JSON, orphan detection, live overlays
- [Visibility versus ACID databases](../operating/visibility/): watermarks and event-time TTL
- [Topology optimization](../concepts/topology-optimization/): including the `optFuseSyncIntoAsync` fusion rule
- [`RIFFLE_SPEC.md`](https://github.com/iand675/wireform-/blob/main/wireform-kafka/streams/RIFFLE_SPEC.md): design contract with per-section rationale
