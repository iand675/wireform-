---
title: Visibility versus ACID databases
description: "How read-after-write, isolation, atomicity, and durability look in Kafka Streams compared with a SQL database, and how to reason about queries against materialised state."
sidebar:
  order: 7
---

A Postgres `SELECT` after `INSERT ... COMMIT` always returns the inserted row. A streams app's view of its state is more nuanced. These differences are not bugs. They follow from using an append-only log as the source of truth and rebuilding derived state asynchronously.

This page maps ACID properties onto wireform-kafka-streams so you know which guarantees you get for free and which require explicit work.

:::tip[Unfamiliar terms?]
Kafka, Streams, and Riffle terminology is defined in the [Glossary](../glossary/).
:::

:::note[TL;DR]

- Commit boundary is `commitIntervalMs` (default 30 s at least once, 100 ms under exactly-once), not a `COMMIT` statement. Downstream `read_committed` consumers see up to that much staleness.
- IQ reads see the live store, including writes an EOS abort will roll back.
- State partitions across instances. `StreamsMetadata` + `KeyQueryMetadata` model query routing, but the runtime doesn't publish them: the routing is yours.
- Event time and processing time differ. Windowed aggregations use event-time; rate metrics use wall-clock.
- Side effects in `peek` / `foreach` / `mapValuesM` replay on rewind. Use two-phase commit sinks or idempotency tokens for exactly-once external effects.

:::

## A vs. ACID

| ACID property | Kafka Streams analogue |
| ------------- | ---------------------- |
| Atomicity | Per-record processing is atomic on a single task; per-commit-cycle atomicity is EOS |
| Consistency | **Eventual**: derived state catches up to the log |
| Isolation | Downstream consumers: read-uncommitted by default, read-committed with `isolation.level = read_committed`. IQ: always the live (uncommitted) store |
| Durability | Kafka log is durable; derived state is durable after changelog flush |

Five operationally-interesting differences: **commit boundary**, **read path freshness**, **multi-instance distribution**, **event-time vs processing-time**, and **replay-on-failure**.

## 1. The commit boundary

In SQL, the commit boundary is `COMMIT`. Reads after see the write.

In Kafka Streams, the commit boundary is **`commitIntervalMs`** (or sooner when a processor calls `ctxRequestCommit`, and before every rebalance). Between commits:

- Producer sends output and changelog records inside the open transaction (under EOS)
- State stores apply writes immediately; there is no write buffer or record cache
- Consumer position moves forward but offset commit hasn't fired

A downstream consumer with `isolation.level = read_committed` sees nothing until commit. A consumer with `read_uncommitted` sees records as written, including potential aborts: `READ UNCOMMITTED` semantics.

Within a stream thread, a processor that writes then reads sees its own write immediately (read-your-writes-within-task). This does **not** extend to cross-task reads.

### Why this matters operationally

- Under EOS the default 100 ms `commitIntervalMs` bounds downstream `read_committed` staleness; a longer interval raises it to match.

```mermaid
sequenceDiagram
  participant Up as Upstream producer
  participant Eng as Streams engine
  participant Store as State store
  participant IQ as Interactive query
  participant Down as read_committed consumer
  Up->>Eng: record at t=0
  Eng->>Store: put (applied immediately)
  IQ->>Store: get
  Store-->>IQ: new, uncommitted value
  Note over Eng: ...more records...
  Eng->>Eng: commit at t=100ms
  Eng->>Down: records become visible
  Note over Store: an abort instead would restore the store from its changelog
```

If downstream SLAs are tight, shorten the interval or accept the staleness budget.

- Interactive queries read the **uncommitted** store. See "Read path freshness" below.

## 2. Read path freshness

Interactive queries hit `IORef`-backed maps for in-memory stores and the persistent shadow map for RocksDB. Writes use `atomicModifyIORef'`; reads use `readIORef`.

Semantics:

- **Linearisable** reads at `readIORef` time. A query thread sees a consistent snapshot.
- **Eager** iterators (`queryRange` / `queryAll`): materialized at creation time; concurrent writes are not visible.

What you don't get:

- **Read-after-write across threads.** A processor's `put` is immediate, but query thread visibility depends on IORef ordering. In practice this is microseconds, but not a contract.
- **Read-committed semantics under EOS.** IQ sees every write as the processor makes it. If the transaction aborts, the store is restored from its changelog and the value rolls back. (`Kafka.Streams.State.Transactional` is a standalone write-buffering overlay; the runtime's stores don't use it.)

For **committed-only** reads, consume the store's output topic (or its changelog) with `read_committed` instead of querying. Most IQ callers tolerate the abort risk.

## 3. Multi-instance distribution

A SQL database is a logical singleton. You connect to it, and you read whatever the latest committed value is, regardless of which backend was the writer.

A Kafka Streams state store is **partitioned**, with its changelog
as the replica. A key lives on exactly one instance's task (there
are no standby replicas). A query for that key must route to that
instance.

The routing vocabulary lives in `Kafka.Streams.Discovery`
(`StreamsMetadata`, `KeyQueryMetadata`, `HostInfo`), but the runtime
doesn't advertise a host or exchange metadata with its peers. Each
instance knows its own partitions (`ownedPartitions`); collecting
them across instances, and mapping a key to a partition with the
producer's partitioner, is your query layer's job.

A query against the wrong instance returns `StoreNotFound`. There
is no automatic redirect within the library: the query proxy is
your code.

### Rebalance-window unavailability

During a rebalance, the partition (and therefore its state) is in
flux: the previous owner closes its task, the new owner restores the
store from the changelog before it serves anything, and until the
instance is `StreamsRunning` again `queryKVStoreWithParameters`
returns nothing unless `staleStoresEnabled`. IQ during that window
returns `StoreNotFound` or nothing.

A query layer should handle this:

```
on IQ:
  route to the instance that owns the key's partition
  on StoreNotFound or not running:
    backoff + refresh ownership
    retry up to N times
    (or query with staleStoresEnabled if a stale answer beats none)
```

## 4. Event-time vs processing-time

A SQL `INSERT` has one timestamp: when the row was inserted. A
Kafka record has two: the **event time** (when the underlying
business event happened, carried in the record's timestamp or
embedded in its value) and the **processing time** (when the
runtime saw the record).

This is the single biggest mental shift coming from databases. A
windowed aggregation is defined over event time. A late record
(event time before the current `StreamTime`) lands in a window
that may already have closed, depending on `withGracePeriod`. The
result is **correct** in event-time terms but **surprising** to a
caller who expected processing-time semantics.

The pieces:

| Concept | Where it lives |
| ------- | -------------- |
| Record timestamp extraction | `TimestampExtractor` family in `Kafka.Streams.Time` |
| Per-task event-time clock | `engineStreamTime`: running max of extracted timestamps on this task |
| Cross-source coordination | `Kafka.Streams.Watermark.WatermarkCoordinator` |
| Window grace period | `withGracePeriod` / `withSessionGracePeriod` on window builders |
| Out-of-order handling | `BoundedOutOfOrderness` watermark generator |
| Idle-source handling | `IdleAfter` in `IdlenessConfig` |

### Why per-task StreamTime is not enough

The Phase-1 `engineStreamTime` is the running max of extracted
timestamps on **this task**. A topology that joins two streams
whose source partitions are consumed by different tasks must wait
for the laggard, but per-task `StreamTime` doesn't know about the
laggard. Three failure modes:

1. **Idle source stall.** One partition stops receiving records;
   its `StreamTime` stops advancing; a downstream window stalls
   waiting for it.
2. **Cross-source skew.** Source A is at event time `t`; source B
   is at `t + 1h`. A join's window expiry uses one side's
   `StreamTime` and misses or admits records depending on which
   side fires first.
3. **Replay race.** During a rebalance the new owner's
   `StreamTime` starts at zero until the first record arrives.

The Riffle `WatermarkCoordinator` fixes all three by computing the
**effective watermark = min of live, non-idle sources**, and by
optionally backpressuring fast sources via alignment groups. See
`Kafka.Streams.Watermark` for the API.

### Operational reading: don't conflate the two clocks

- A dashboard showing "records per second" is processing-time.
- A windowed aggregate emitting `windowEnd` is event-time.
- The two diverge during catch-up after an outage: processing rate
  is high (catching up), event time is also moving fast (replaying
  old records), but neither is wall-clock.

`punctuateTotal` on the wall-clock punctuator and on the
stream-time punctuator are different metrics. The wall-clock one
tells you "scheduled effects fired"; the stream-time one tells you
"event time advanced past this trigger". A wall-clock punctuator
that fires at the expected rate while the stream-time one is
stalled is a sign that an idle source is holding back event time.

## 5. Replay-on-failure

A SQL transaction either commits or doesn't. There is no scenario
in which a row is committed twice.

Kafka Streams **may [replay](../glossary/#replay) records on failure**:

- At-least-once: any unrecoverable error rewinds the consumer to
  the last committed offset and replays from there. Side effects
  in `peek` / `foreach` / `mapValuesM` happen again.
- Exactly-once-V2: the producer transaction either commits or
  aborts atomically. State-store changelog writes are part of the
  transaction, and an abort restores the stores from them. **External side effects that aren't inside a
  two-phase-commit sink replay on abort.**

The alternative is dropping records on partial failure, but replay has consequences
for code that thinks in database terms:

- **No "commit-then-act" pattern.** You cannot "commit, then call
  the external API knowing the commit is durable" because the
  commit boundary is implicit and the runtime can rewind.
- **Idempotency is non-negotiable for external side effects.**
  Either the side effect is naturally idempotent (a PUT to a
  versioned URL, an UPSERT keyed on a stable id) or you make it
  idempotent (token in a state store, 2PC sink).
- **`PROCESSING_GUARANTEE = AT_LEAST_ONCE` is the default**, and
  it does what it says: every record is processed *at least
  once*, which means *possibly more than once*. Plan for it.

## A worked comparison

The user-facing question "did this transfer go through?" mapped to
both worlds:

### Postgres

```sql
BEGIN;
INSERT INTO transfers (id, amount, status) VALUES (?, ?, 'pending');
UPDATE accounts SET balance = balance - ? WHERE id = ?;
UPDATE accounts SET balance = balance + ? WHERE id = ?;
UPDATE transfers SET status = 'complete' WHERE id = ?;
COMMIT;

-- Then:
SELECT status FROM transfers WHERE id = ?;
-- → 'complete'. Guaranteed.
```

### Kafka Streams (EOS)

```
[transfers topic] → process → updates [accounts changelog] + [transfers-out topic]
                                                                  ↓
                              [external read service] ← IQ ← [transfers-out KTable]
```

A read on `[transfers-out KTable]` for the transfer id may return:

- The new value, as soon as the record was processed, even
  before the commit (an abort would roll it back).
- The previous value (or "not found"), if the record hasn't been
  processed yet.
- Nothing, if a rebalance is in flight and the new owner is still
  restoring the store.
- Inconsistent values across instances during a brief rebalance
  window.

For "did the transfer go through?" semantics you typically:

1. Process the transfer atomically in Streams.
2. Sink the final state to a downstream system the user reads
   from: Postgres, a key-value store, or a separate IQ-fronted
   service.
3. The downstream system is the source of truth for the user's
   read; it just lags Kafka by a bounded amount.

This is the same architectural pattern as CQRS: and the same
trade-off. You give up read-your-writes in exchange for unlimited
horizontal write throughput, replayability, and the ability to
materialise the same source data into many downstream views.

## Mental-model tips

- **Treat the Kafka log as the source of truth.** Everything else
  is a materialised view.
- **Treat the commit cycle as the consistency boundary.** Within
  a cycle, things are in flux; between cycles, things are
  durable.
- **Don't expect a query to see what a write-then-read pair would
  return.** Use IQ as a *debugger* or as a *bounded-staleness
  cache*, not as a replacement for a query layer.
- **Event time and processing time are different clocks.** Pick
  one per concern and stick with it.
- **For external-system effects, design idempotently or use 2PC.**

## Related reading

- [Exactly-once across Kafka and other systems](./exactly-once/) -
  the commit cycle in detail.
- [Enrichment via external systems](../guides/enrichment/) -
  the idempotency-token pattern for external writes.
- [Observability](./observability/): IQ as a debugging surface
  and what its iterators actually show you.
