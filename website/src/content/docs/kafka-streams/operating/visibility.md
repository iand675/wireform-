---
title: Visibility versus ACID databases
description: How read-after-write, isolation, atomicity, and durability look in Kafka Streams compared with a SQL database — and how to reason about queries against materialised state.
sidebar:
  order: 7
---

A Postgres `SELECT` after an `INSERT ... COMMIT` always returns the inserted row. A streams app's view of its state is more nuanced — and the differences are not bugs. They follow from using an append-only log as the source of truth and rebuilding derived state asynchronously.

This page maps each ACID property onto wireform-kafka-streams so you know which guarantees you get for free and which require explicit work.

:::tip[Unfamiliar terms?]
Kafka, Streams, and Riffle terminology is defined in the [Glossary](../glossary/).
:::

:::note[TL;DR]
- The consistency boundary is the Streams commit cycle, usually `commitIntervalMs`, not an explicit `COMMIT` statement.
- Queries read partitioned materialised state. Route by key, expect bounded staleness, and handle rebalances.
- External side effects replay unless you make them idempotent or put them behind a two-phase commit sink.
:::

## A vs. ACID

| ACID property | Kafka Streams analogue |
| ------------- | ---------------------- |
| Atomicity | Per-record processing is atomic on a single task; per-commit-cycle atomicity is the unit of EOS |
| Consistency | "Consistency" in the streaming sense is **eventual** — derived state catches up to the log |
| Isolation | Read-uncommitted by default; read-committed with `isolation.level = read_committed` on consumers; transactional state stores buffer writes until commit |
| Durability | The Kafka log is durable; derived state is durable only after the changelog flush |

The rest of this page works through the differences that usually
surprise people coming from a database: commits, read freshness,
partitioned ownership, time, and replay.

## 1. The commit boundary

In a SQL database, the commit boundary is the `COMMIT` statement.
Reads after that point see the write.

In Kafka Streams the commit boundary is **`commitIntervalMs`** (or
sooner if the cache fills). Between commits:

- The producer has accumulated records in its transactional batch.
- The state store has buffered writes in its in-memory cache and
  (under EOS) in its transactional buffer.
- The consumer's position has moved forward but the offset commit
  hasn't fired.

A downstream consumer with `isolation.level = read_committed` sees
nothing from this batch until the commit. A downstream consumer
with `read_uncommitted` sees records as the producer writes them,
including records that may be aborted later — exactly the semantics
of a database `READ UNCOMMITTED`.

Within the stream thread itself, a processor that writes to a
state store then reads from it does see its own write
immediately. This is read-your-writes-within-task. It does **not**
extend to cross-task reads even within the same process.

### Why this matters operationally

A 30 s `commitIntervalMs` means downstream `read_committed`
consumers can be 30 s behind the stream thread. If that is too
stale, lower the interval and pay the extra commit overhead.
Interactive queries have their own read path, covered next.

## 2. Read path freshness

Interactive queries hit `Data.IORef.IORef`-backed maps for the
in-memory stores and the persistent shadow map for RocksDB.
Writes go through `atomicModifyIORef'`; reads through `readIORef`.
The semantics:

- **Linearisable** reads at the moment the `readIORef` runs. A
  query thread sees a consistent snapshot of the map.
- **Eager** iterators (`queryRange` / `queryAll`): the iterator is
  materialised at iterator-creation time. Writes that arrive
  during iteration are not visible.

What you don't get:

- **Read-after-write across stream thread and query thread.**
  A processor's `put` is in the store immediately, but a query
  thread observing it depends on the IORef's atomic-update
  ordering. In practice the visibility is microseconds, but it's
  not a contract.
- **Read-committed semantics under EOS.** The transactional store
  buffers writes until `storeCommit` fires (after the producer
  txn commits). An IQ that reads through the underlying store
  during the cycle sees the pre-commit value; an IQ that reads
  through the transactional overlay sees the buffered value. The
  default `queryKVStore` exposes the underlying store — i.e.
  committed-only reads.

If you need **strict** committed-only semantics for IQ, query
through the transactional overlay's `getCommitted` only after
confirming `commitTotal` has advanced since your last commit
observation.

If you need **fresh** reads including in-flight writes, accept the
EOS abort risk: the value you read might be rolled back on the
next cycle abort. Most external query consumers can tolerate this
because their freshness budget is wider than the commit cadence.

## 3. Multi-instance distribution

A SQL database is a logical singleton — you connect to it, you
read whatever the latest committed value is, regardless of which
backend was the writer.

A Kafka Streams state store is **partitioned and replicated**. A
key lives on exactly one instance's active task; standbys hold a
warm replica. A query for that key must route to the active
instance.

The routing primitives:

- `Kafka.Streams.Discovery.StreamsMetadata` advertises per-instance
  ownership.
- `Kafka.Streams.Discovery.KeyQueryMetadata` answers "for this
  (store, key), which instance is the active and which are the
  standbys".
- `Kafka.Streams.Discovery.HostInfo` is the `host:port` an instance
  advertises via `application.server`.

A query against the wrong instance returns `StoreNotFound`. There
is no automatic redirect within the library — the query proxy is
your code.

### Standby reads

A standby's state lags the active by up to `acceptableRecoveryLag`
records. Reading from a standby gives you:

- **Lower contention** with the stream thread on the active.
- **Stale data** bounded by lag.

Whether that trade-off is worth it depends on the use case. Read
queries that are part of a user-visible request flow usually go to
the active; bulk analytical scans usually go to standbys.

The library doesn't enforce a routing policy — you decide.
`KeyQueryMetadata.standbyHosts` returns every standby for a key;
pick by your own policy (round-robin, latency-aware, freshness-
preferring).

### Rebalance-window unavailability

During a rebalance, the partition (and therefore its state) is in
flux. KIP-848's incremental reconciliation guarantees no
double-ownership but does mean there's a window where the task is
owned by no member. IQ during that window returns either
`StoreNotFound` or a stale read from a standby.

A robust query layer handles this:

```
on IQ:
  fetch StreamsMetadata
  route to KeyQueryMetadata.activeHost
  on StoreNotFound or rebalance-error:
    fall back to KeyQueryMetadata.standbyHosts in order
    if all fail:
      backoff + refresh metadata
      retry up to N times
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
| Per-task event-time clock | `engineStreamTime` — running max of extracted timestamps on this task |
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
the [Watermark concept page](../concepts/watermarks/) (TODO) and
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
  aborts atomically. State-store writes drain in step with the
  producer commit. **External side effects that aren't inside a
  two-phase-commit sink replay on abort.**

This is the right model for a streaming pipeline — the alternative
is dropping records on partial failure — but it has consequences
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

- The current value, if the commit cycle has fired since the
  record was processed.
- The previous value (or "not found"), if processing happened but
  the commit cycle hasn't fired yet.
- The previous value, if a rebalance is in flight and the active
  is mid-transfer.
- Inconsistent values across instances during a brief rebalance
  window.

For "did the transfer go through?" semantics you typically:

1. Process the transfer atomically in Streams.
2. Sink the final state to a downstream system the user reads
   from — Postgres, a key-value store, or a separate IQ-fronted
   service.
3. The downstream system is the source of truth for the user's
   read; it just lags Kafka by a bounded amount.

This is the same architectural pattern as CQRS — and the same
trade-off. You give up read-your-writes in exchange for unlimited
horizontal write throughput, replayability, and the ability to
materialise the same source data into many downstream views.


## Related reading

- [Exactly-once across Kafka and other systems](./exactly-once/) —
  the commit cycle in detail.
- [Enrichment via external systems](../guides/enrichment/) —
  the idempotency-token pattern for external writes.
- [Observability](./observability/) — IQ as a debugging surface
  and what its iterators actually show you.
