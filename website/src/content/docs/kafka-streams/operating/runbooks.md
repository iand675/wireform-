---
title: Runbooks
description: Common incidents in a wireform-kafka-streams deployment and the procedures for resolving them.
sidebar:
  order: 8
---

This page contains procedures for common incidents. Each runbook is designed to be followed during an active incident. Start with the alert you are seeing, work through diagnosis, then follow the resolution steps.

Keep this page bookmarked. When an alert fires, open it and follow the relevant procedure.

## What a runbook is (and isn't)

A runbook is a **checklist for your brain during an outage**. When production
is down, you don't want to be figuring out which metrics matter or what the
recovery command is. You want a procedure you can follow that leads to a known
good state.

Each runbook in this page follows the same structure:
- **Alert:** What you see in your monitoring (the trigger)
- **Diagnosis:** How to confirm the root cause (the investigation)
- **Resolve:** The steps to fix it (the action)
- **Prevent:** How to stop it happening again (the learning)

A runbook is not a troubleshooting guide for development. If you're debugging
why your topology behaves unexpectedly in the test driver, that's a different
process. Runbooks are for when the service is already deployed and something
has gone wrong in production.

:::tip[Unfamiliar terms?]
Kafka, Streams, and Riffle terminology is defined in the [Glossary](../glossary/).
:::

:::note[TL;DR]
- Most-common incidents covered: rebalance storm, transactions that keep aborting, slow restores, orphan internal topics, producer fenced, async-I/O stall, deserialisation flood, unbounded state-dir growth, slow commit cycle, IQ during rebalance.
- Each runbook is structured the same way: alert → diagnosis → resolve → prevent.
- Use the quick metric-reference at the bottom when you don't know what to look at first.
:::

## Rebalance storm

**Alert:** `stream-thread-metrics:rebalance-total` rate spike across multiple
instances, throughput drops, `state-transition: RUNNING ->
REBALANCING -> RUNNING` flapping in logs.

### Diagnosis

A "storm" is [rebalances](../glossary/#rebalance) triggered faster than the group can
settle, usually within minutes of each other. Common causes:

1. **Liveness flaps.** One instance is hitting GC pauses, network
   blips, or saturated CPU. Its heartbeats time out, the group
   kicks it, it rejoins, repeat.
2. **Probing-rebalance loop.** `probingRebalanceIntervalMs` is
   short while warmups keep being assigned (an assignment with
   warmups makes every member rejoin after the interval); a
   `maxWarmupReplicas` far below the number of tasks that want to
   move stretches that over many probes.
3. **Memory pressure.** RocksDB compaction stalling the instance
   long enough to miss heartbeats.
4. **Misconfigured `session.timeout.ms` on the consumer side**
   relative to `heartbeat.interval.ms`.

### Resolve

1. **Identify the flapping instance(s).** `setRebalanceListener`
   logs should show which instances repeatedly join and leave.
2. **For liveness flaps:** look at OS-level metrics for that
   instance (CPU, GC, network). Fix the underlying cause; the
   rebalance loop stops itself.
3. **For probing-rebalance loops:** temporarily raise
   `probingRebalanceIntervalMs` to 600_000 (10 min default).
   Restart the affected instances with the new config.
4. **For RocksDB compaction pressure:** check
   `storePutTotal` versus underlying disk write throughput; the
   ratio tells you whether you're write-throttled. Mitigations
   include moving the state directory to faster storage, or shedding load
   via `pauseKafkaStreams` on the worst-affected instance until
   compaction catches up.
5. **For consumer-config skew:** confirm
   `session.timeout.ms > 3 × heartbeat.interval.ms` and that
   neither has changed recently.

### Prevent

- Standardise `probingRebalanceIntervalMs` and
  `acceptableRecoveryLag` so all instances agree.
- Monitor GC pauses; alert at p99 > 200 ms.
- Provision local disk IOPS for RocksDB's compaction worst case,
  not its steady state. See [Running in containers](./containers/#3-memory-accounting)
  for how compaction interacts with container memory limits and the
  OOM-killer.

---

## Transactions keep aborting (exactly-once)

**Alert:** repeated `[streams] commit aborted, recovering every task: …`
log lines; output on `read_committed` consumers stalls.

### Diagnosis

Under exactly-once a commit that can't complete (a failed send, a
failed flush, `sendOffsetsToTransaction` rejected, `commitTransaction`
failing) aborts the transaction and recovers every task: each is wiped,
restored from its changelogs and rewound to the last committed
offsets. One abort is harmless; a loop means the same commit keeps
failing. The reason is in the log line:

1. **`send to <topic>: …`**: a sink or changelog write is rejected,
   e.g. a record over `max.message.bytes`, a missing topic with
   auto-creation off, or an authorisation error. It fails again after
   every rewind.
2. **`send offsets to transaction: ILLEGAL_GENERATION …`**: the group
   rebalanced under the instance (it was too slow to rejoin, or a
   zombie). The rebalance resolves it.
3. **`commit transaction: …`** with broker errors: transaction
   coordinator or broker trouble.
4. **`prepare <sink>: …`**: a [commit participant](../glossary/#commit-participant)
   (a two-phase sink) can't prepare; its external system is down or
   rejecting the batch.

### Resolve

1. **For (1):** fix the record or the topic (raise the size limit,
   create the topic, grant the ACL). The poison record is reprocessed
   after each rewind, so the loop only stops once it can be written.
2. **For (2):** see "Rebalance storm".
3. **For (3):** check the brokers' transaction coordinator and
   `__transaction_state` health.
4. **For (4):** fix the external system; the input is reprocessed
   and staged again into the next cycle.

### Prevent

- Alert on the abort rate, not on single aborts.
- Keep every store's logging on under exactly-once: an aborted task
  restores only what its changelogs hold.

---

## Restores take too long after a rebalance or restart

**Alert:** instances stay in `StreamsRebalancing` long after a
rebalance; `onRestoreStart` / `onRestoreEnd` (from
`setStateRestoreListener`) far apart.

### Diagnosis

A task that moves (or starts after a wipe) restores every logged store
before it processes anything, with its source partitions paused.
Restore reads from where the task's standby got to (if it lands on
one), else its checkpoint (persistent stores only), else the log start,
up to the end offset (the last stable offset under exactly-once).
Common causes of slow restores:

1. **Large changelogs.** The changelog isn't compacted (a store whose
   changelog was created by hand with `cleanup.policy=delete`) or the
   key space is huge.
2. **In-memory stores** always restore from the log start; nothing is
   checkpointed for them.
3. **Unclean exactly-once stops** wipe `<state.dir>/<application.id>`
   on the next start, so everything restores.
4. **Network or disk throughput** of the restoring instance.
5. **No standbys** (`numStandbyReplicas = 0`), or the task landed on an
   instance without one: `localTaskLags` on each instance shows which
   standbys exist and how far behind they are.

### Resolve

1. **Identify the store and partition.** `onBatchRestored` reports
   progress per store and changelog partition.
2. **For (1):** compact the changelog (`cleanup.policy=compact`, which
   the runtime sets on the changelogs it creates).
3. **For (2):** use a persistent store
   (`persistentKeyValueStoreBuilder`) so a task coming back to the
   same machine only restores the tail after its checkpoint.
4. **For (3):** stop instances with `closeKafkaStreams` (which
   commits and leaves the clean-shutdown marker) instead of killing
   them.
5. **For (5):** set `numStandbyReplicas = 1`; a failover then restores
   only what the standby hadn't applied. Keep `acceptableRecoveryLag`
   tight so tasks only move to instances that are caught up (the others
   warm up first).

### Prevent

- With standbys, failover time is the standby's lag (watch
  `localTaskLags` and `stream-state-metrics:standby-update-total`);
  without them, budget it as changelog size divided by restore
  throughput.
- Give instances stable state directories (persistent volumes) so
  checkpoints survive restarts.

---

## Orphan internal topics detected

**Alert:** Startup log line `orphan internal topic: <name>` or
the orphan-detector metric > 0.

### Diagnosis

A previous deploy renamed an operator or removed a store. The
broker is keeping the old internal topic (changelog or
repartition) and its disk usage continues.

### Resolve

1. **Confirm it is genuinely orphaned.** Run
   `Kafka.Streams.Observability.OrphanTopics.detectOrphans`
   against the current production topology and the current
   broker topic list. The output should match the alert.
2. **Check for in-flight rolling deploys.** If a v1 instance is
   still running and v2 introduced the rename, the "orphan" is
   actually still in use by v1. Don't delete until v1 is fully
   drained.
3. **Settle.** Wait at least one full commit-cycle multiple
   beyond the slowest instance's drain time.
4. **Delete via the AdminClient.** Use a real broker-side
   `deleteTopics` call. The runtime will not do this for you.
5. **Confirm clean.** Re-run the detector; expect zero output.

### Prevent

- Pin every stateful operator's name with `Named`.
- Run the orphan detector in CI against the deployment-shape
  golden file.
- Treat any topology change that affects internal-topic names as
  a stateful migration (see
  [Topology evolution](./topology-evolution/)).

---

## Producer fenced / `INVALID_PRODUCER_EPOCH`

**Alert:** Runtime log line containing
`InvalidProducerEpochException` or `ProducerFencedException`.

### Diagnosis

Under [EOS-V2](../glossary/#eos--eos-v2--eos-v3) each instance's
producer has its own `transactional.id`
(`<application.id>-<random uuid>`), so two live instances never fence
each other by id; a zombie is fenced by the group generation instead
(its `sendOffsetsToTransaction` is rejected and its transaction
aborted). A [`ProducerFencedException`](../glossary/#fenced-producer)
therefore usually means:

1. **`transactional.id.expiration.ms` on the broker has elapsed**
   for an idle producer; the broker forgets it; the instance
   tries to commit and gets fenced.
2. **Something else is using the same transactional id**, e.g. a
   `producerOverrides` that pins `producerTransactional` to a fixed
   value shared by several instances.

### Resolve

1. The runtime aborts and recovers its tasks; if it can't abort, it
   re-initialises the transactional producer. If that fails too, the
   stream thread stops and `setUncaughtExceptionHandler` decides
   (the default, `replaceThreadOnException`, recovers and retries).
2. **If a pinned transactional id is shared:** remove the override;
   the runtime picks a unique one per process.
3. **If the cause was broker-side TXN expiration:** raise
   `transactional.id.expiration.ms` on the broker for this
   workload, or shorten the EOS commit interval so the producer
   isn't ever idle long enough to expire.

### Prevent

- Use process supervisors that kill old generations before
  starting new ones (no overlapping lifecycle).
- Set sensible `instance.id` for `static membership` so
  rebalances don't churn during normal restarts.

---

## Async I/O backpressure causes stream-thread stall

**Alert:** Async-operator `aio-deposit-rate` near zero while
`aio-enqueue-rate` also near zero (the queue is full and the
stream thread is blocked).

### Diagnosis

The external system the async operator is calling has slowed
down or is failing. The in-flight queue
(`aioBufferCapacity`) fills, the stream thread blocks on
enqueue, and the entire downstream pipeline stalls.

This is **working as intended**: it's the backpressure signal -
but it should not last long.

### Resolve

1. **Check the external system.** If it's down, the right
   action is at the external system, not at the streams app.
2. **If the external system is slow but up:** check the async
   operator's `aioRetry` and `aioTimeout`. A long timeout with
   retries means each in-flight slot is occupied for
   `(attempts + 1) × timeout` worst case. The queue can never
   drain faster than that.
3. **If the queue is structurally too small:** raise
   `aioBufferCapacity` (requires restart). The trade-off is
   more in-flight memory and a longer pre-commit drain.
4. **If the failures are partial:** consider switching
   `aioOnFailure` from `FailTask` to `LogAndContinue` so a
   minority of failures don't shed the whole pipeline.
5. **If the external system is overwhelmed:** the async
   operator is doing what you asked: flooding it. Drop
   `aioWorkers` (restart required) to reduce concurrent calls.

### Prevent

- Size `aioBufferCapacity ≈ 4 × aioWorkers` so brief stalls
  don't immediately propagate.
- Set `aioTimeout` to the external system's p99 + a buffer,
  not its average.
- Monitor `aio-failure-rate` separately; sustained high
  failure rate is its own incident.

---

## Schema deserialisation flood

**Alert:** `droppedRecordsTotal` rate spike, paired with a
matching rate on the `DeserializationException` log channel.

### Diagnosis

An upstream producer started writing records that the current
deserialiser can't parse. (The [railway-oriented programming](../concepts/railway-oriented-programming/) page explains where this routing decision lives and how a DLQ wired through the `DeserializationHandler` gives you reprocessability.) Three usual causes:

1. **Schema Registry compat policy was bypassed**: a new schema
   was published without compatibility checks, and your
   `registrySerdeChecked` wrapper rejects every record. (This
   means the wrapper is doing its job; the producer is the
   problem.)
2. **A new producer service rolled out** with a different wire
   format and skipped Schema Registry entirely.
3. **A new field with a default value was added correctly**,
   but your generated decoder doesn't have it.

### Resolve

1. **Identify the offending source.** Look at the
   `DeserializationException` payload to see the bad record;
   trace it to its producer.
2. **Decide between stopping and skipping.** The default
   deserialization handler (`logAndFail`) stops the stream thread at
   the first bad record, so nothing is skipped and the group stops
   making progress until the records are dealt with. If the records
   are expendable, switch `defaultDeserHandler` to `logAndContinue`
   (they are dropped and counted in `droppedRecordsTotal`).
3. **Fix the producer.** Either roll the producer back, or
   re-publish through Schema Registry with the correct
   compatibility check.
4. **Re-process the dropped records.** Use the
   `processingException.handler` `DEAD_LETTER` policy
   (KIP-1033) if you have one configured; otherwise re-consume
   from the offending offset range with a one-shot consumer.

### Prevent

- Use `registrySerdeChecked` for every Schema Registry-backed
  serde. It probes the per-subject compatibility mode and fails
  fast at construction.
- Alert on `droppedRecordsTotal` rate, not just absolute count.
- Enforce Schema Registry compatibility at the producer side
  too: don't rely on the consumer being the only check.

---

## Local state directory grows without bound

**Alert:** `stateDir` disk usage exceeds expected ceiling.

### Diagnosis

State stores grow with the number of unique keys. Common causes
of unbounded growth:

1. **The source topic is not compacted** (or has very long
   retention) and you're materialising every key ever seen.
2. **A windowed store has `withGracePeriod`** longer than your
   retention budget assumes.
3. **A KTable is built off a topic with very high unique-key
   cardinality** and you didn't realise.
4. **Old task directories** (`<state.dir>/<application.id>/<task>`)
   from a prior deploy or a topology change were never cleaned up.

### Resolve

1. **Identify which store** is growing. RocksDB has per-store
   subdirectories under `stateDir`; sizing them is direct.
2. **For (1) and (3):** add a TTL via
   `Kafka.Streams.State.KeyValue.TTL` (wall-clock) or
   `EventTimeTTL` (driven off the coordinated watermark). This
   actively expires entries on every read; pair with a punctuator
   that calls `expireBefore` for active sweeping.
3. **For (2):** confirm the grace period matches the window
   retention. A 1-hour window with a 7-day grace materialises 7
   days of state.
4. **For (4):** `cleanUp` (while the instance is stopped) wipes
   `<state.dir>/<application.id>`; every task restores from its
   changelogs on the next start. Nothing is lost, but the restart is
   as slow as a full restore.

### Prevent

- Always pick a `KeyValueStore` backend that fits the cardinality
  budget. In-memory is fine for ≤10⁶ keys; for more, use
  RocksDB (`+rocksdb` flag), the Riffle tiered (hot + cold S3)
  backend, or the remote-KV backend.
- Apply TTLs proactively for any topology where the key
  cardinality is unbounded.
- Monitor `stateDir` size as a first-class metric, not just disk
  usage. [Running in containers → disk sizing](./containers/#4-disk-sizing)
  has the budgeting formula.

---

## EOS commit taking longer than `commitIntervalMs`

**Alert:** commits fall behind their interval; output on
`read_committed` consumers arrives in bursts.

### Diagnosis

A commit (`Kafka.Streams.Runtime.Tasks.tmCommit`) runs every task's
pre-commit drains, flushes every task's stores (writing their
changelogs), flushes the producer, prepares every commit participant,
sends the offsets to the transaction, commits it, and commits the
participants. Likely costs:

1. **Pre-commit drains** waiting on slow async-I/O calls.
2. **The producer flush** when a lot of output (or changelog traffic:
   every update of an uncached store is logged, and the record caches
   flush the latest value of every key they hold) piled up in the
   interval.
3. **The transaction round trips** to a loaded transaction
   coordinator.
4. **Commit participants** (two-phase sinks) with slow prepare or
   commit calls.

### Resolve

1. **For (1):** see "Async I/O backpressure" above; drain time is
   bounded by the slowest in-flight call.
2. **For (2):** cut changelog volume (fewer intermediate stores,
   `withLoggingDisabled` on stores that are recomputable), or raise
   `commitIntervalMs` so each transaction amortises its round trips
   over more records. Raising it delays output visibility and widens
   the replay window of an abort.
3. **For (3):** check broker health.
4. **For (4):** time the sink's `tpsPrepare` / `tpsCommit`; they run
   on the stream thread, once per cycle.

### Prevent

- Benchmark the commit under your expected throughput at
  load-test time; size `commitIntervalMs` accordingly (the EOS
  default is 100 ms).

---

## Interactive query returns `StoreNotFound` during a rebalance

**Alert:** Spikes of 404s on an IQ-fronted endpoint during a
rolling deploy.

### Diagnosis

During a rebalance, a partition (and its state store) is
transiently unowned. Queries routed to the previous owner get
`StoreNotFound`; queries routed to the new owner get the same
until its task has restored the store from the changelog. Unless
`staleStoresEnabled`, `queryKVStoreWithParameters` answers nothing
until the instance is `StreamsRunning` again.

### Resolve

1. **In the query layer, retry with backoff and refresh
   metadata.** `streamsMetadata` / `queryMetadataForKey` reflect the
   new assignment as soon as the rebalance completes; a retry after
   `~1s` usually succeeds. With standbys, `queryMetadataForKey` also
   names the standby hosts, which can answer stale reads meanwhile.
2. **If a stale read beats a 404**, query with `staleStoresEnabled`
   (it answers from whatever the local tasks hold, restoring or not).
3. **If the rebalance is taking minutes**, see "Restores take too
   long" above: that's actually the underlying issue.

### Prevent

- Build the query layer with the rebalance-window assumption
  baked in. A naïve "one shot, fail on 404" client will be
  brittle.
- Keep restores short (compacted changelogs, persistent stores with
  checkpoints on stable volumes): the unavailability window is the
  restore time.

---

## Reading the metrics during an incident

**When you don't know where to start:** Every incident produces symptoms, but
the same symptom ("things are slow") can have different root causes (CPU
saturation, disk I/O, network latency, or a poison-pill record). This table
gives you a diagnostic path. Start with the symptom you're seeing, check the
"First metric" column, and use that reading to decide what to check next.

Quick reference for which metric to look at first:

| Symptom | First metric | Then |
| ------- | ------------ | ---- |
| "Things are slow" | `process-total` rate per task (`streamsMetrics`) | A task far behind the others is the hot partition; all of them slow points at a slow processor or store |
| "Throughput dropped" | `process-total` rate | If one task, check its partition's lag; if every task, check `commit-aborted-total` and `task-recoveries-total` |
| "Records seem to disappear" | `dropped-records-total` | Check `DeserializationException` log channel |
| "Rebalance loop" | `rebalance-total` | Check `setRebalanceListener` log |
| "EOS issues" | `commit-aborted-total`, then the `commit aborted` log lines | The reason says which step failed |
| "Stuck rebalancing" | `restore-total`, `StateRestoreListener` events | Check restore throughput and changelog size |

---

## Related reading

- [Observability](./observability/): the metric surface this
  page leans on.
- [Topology evolution](./topology-evolution/): the deployment
  procedures these runbooks reference.
- [Exactly-once across Kafka and other systems](./exactly-once/) -
  the EOS internals behind the commit-cycle runbooks.
