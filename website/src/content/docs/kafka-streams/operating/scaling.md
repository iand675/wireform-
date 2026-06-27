---
title: Scaling and rebalancing
description: Threads, partitions, key-groups, dispatch modes, standby tasks, and what each one buys you when you want more or less parallelism.
sidebar:
  order: 3
---

As your data volume grows, you need more processing power. Kafka Streams gives you three ways to scale, each with different trade-offs.

**Vertical scaling** (more threads) is easiest but hits CPU/memory limits. **Horizontal scaling** (more processes) works well but is capped by your topic's partition count. **Key-group scaling** (an optional extension) removes that partition limit entirely.

This page covers all three approaches, when to use each, and how the rebalance protocol keeps assignments consistent as you scale.

:::tip[Unfamiliar terms?]
Kafka, Streams, and Riffle terminology is defined in the [Glossary](../glossary/).
:::

:::note[TL;DR]

- Standard parallelism is capped at `numStreamThreads × instances × partition_count`. Key-group routing (optional) decouples it from partition count entirely.
- Three dispatch modes: `DispatchPartition` (default), `DispatchHashed`, `DispatchKeyGroup` (for scaling past partitions).
- `numStandbyReplicas >= 1` is the difference between metadata-only failover and a full changelog replay.
- KIP-848 incremental rebalance: tasks are never double-owned during a transfer.
- `addStreamThread` / `removeStreamThread` reshape in-process workers without triggering a broker-side rebalance.

:::

## The three axes

| Axis | Knob | Bound |
| ---- | ---- | ----- |
| Threads per process | `numStreamThreads` in `StreamsConfig` | None (in practice, limited by CPU cores) |
| Processes per group | More OS processes joining the same `applicationId` | Number of source-topic partitions × `numStreamThreads` per process |
| Logical shards per task | `dispatchMode = DispatchKeyGroup` + `KeyGroupConfig` | The configured `kgcTotal` (typically 128) |

The first two work with standard Kafka Streams. The third requires the key-group routing extension.

## Threads inside a process

The runtime uses a "one consumer, N workers" model, not the JVM's
"N stream-threads, each a separate consumer joining the same group".
The trade-offs:

- **Less broker-side fan-out.** One consumer connection per
  process, regardless of `numStreamThreads`.
- **Sticky per-partition routing within the process.** A given
  `(topic, partition)` consistently lands on the same worker, so the
  worker's state stores stay coherent. The dispatcher is selected by
  `DispatchMode`:

  | `DispatchMode` | Routing |
  | -------------- | ------- |
  | `DispatchPartition` (default) | Explicit `(topic, partition) -> Int` ownership; matches the JVM 1:1 |
  | `DispatchHashed` | `hash (topic, partition) mod workerCount`; sticky for stable membership |
  | `DispatchKeyGroup` | Key-group routing (see below) |

- **No store rebalance across workers within a process.** A
  partition's state lives on whichever worker first hashed to it.
  If you want to redistribute work across workers, you scale across
  *processes* (multi-instance), not by churning `numStreamThreads`.

`addStreamThread` / `removeStreamThread` mirror the JVM lifecycle
calls. Both are safe at runtime and incur a brief in-process
worker-pool reshuffle. Neither triggers a broker-side rebalance.

## Processes across the group

This is the primary horizontal scale axis. Each new process is a new
group member; [KIP-848](../glossary/#kip) handles the assignment incrementally via [reconciliation](../glossary/#reconciliation). The full
loop:

1. New process starts; calls `subscribe`; joins the group with its
   current `Subscription` (`subscribedTopics`, `currentlyOwned`,
   `memberEpoch`).
2. Group coordinator computes the new `TargetAssignment` (either via
   the built-in cooperative-sticky assignor or the user-installed
   `taskAssignor`).
3. Coordinator emits per-member `Reconciliation` records:
   `rAdd :: Set TaskId` and `rRemove :: Set TaskId`.
4. Losing members release their `rRemove` tasks first
   (acknowledged on the next heartbeat by dropping them from
   `currentlyOwned`). Only then do gaining members see those tasks
   in their `rAdd`. This is the no-double-ownership guarantee in
   `Kafka.Streams.Runtime.RebalanceProtocol.reconcile`.
5. Each transfer is observable via `setRebalanceListener` on
   `KafkaStreams`. The handler fires on every revoke / assign so
   you can drain external resources keyed by partition.

A single task moving from member A to member B over three
heartbeats:

```mermaid
sequenceDiagram
  participant Coord as Group coordinator (broker)
  participant A as Member A (losing task T)
  participant B as Member B (gaining task T)
  Note over A,B: Steady state: A owns task T
  B->>Coord: Heartbeat (join)
  Coord->>Coord: Recompute TargetAssignment
  Coord-->>A: Reconciliation { rRemove = {T} }
  Coord-->>B: Reconciliation { rAdd = {T} }\n(blocked: A still owns T)
  A->>A: Drain T; commit cycle on T closes
  A->>Coord: Heartbeat (currentlyOwned no longer includes T)
  Coord->>Coord: Mark T released
  Coord-->>B: Reconciliation { rAdd = {T} }
  B->>B: Fetch standby state / replay tail
  B->>Coord: Heartbeat (currentlyOwned += T)
  Note over A,B: New steady state: B owns task T
```

At no point during the transfer is T owned by both members.

### The ceiling is the partition count

A group has at most `numStreamThreads × processes` workers, but no
worker can process work that doesn't come from a partition it owns.
Adding a 6th instance to a 5-partition topic just creates an idle
member. The pre-Riffle answer was "repartition the topic up-front";
the Riffle answer is key-groups.

### `taskAssignor` is the leader-side plug-in

`StreamsConfig.taskAssignor` (KIP-924) lets you replace the built-in
cooperative-sticky assignor with an in-process plug-in. The runtime
constructs an `ApplicationState` from the live view and invokes
`taAssign`. Use cases:

- Rack-awareness beyond what the built-in
  `rackAwareAssignmentStrategy` already covers.
- Sticky-by-tenancy when you want some partitions pinned to specific
  instances for cache locality.
- Co-locating two related stores on the same task so an
  in-process join works without going through a repartition topic.

If you do not need any of those, leave it at `Nothing` and let the
built-in cooperative-sticky assignor handle it.

## Key-groups: parallelism decoupled from partitions

`Kafka.Streams.Runtime.KeyGroup` introduces a fixed routing space
(`kgcTotal`, typically 128) that sits **between** the record's key
and the assignor. The default at construction is
`defaultKeyGroupConfig`:

```haskell
defaultKeyGroupConfig :: KeyGroupConfig
defaultKeyGroupConfig = KeyGroupConfig
  { kgcTotal = KeyGroupCount 128
  , kgcHash  = hash . BS.unpack
  }
```

Hot path per record:

1. The runtime serialises the key.
2. `keyGroupOfBytes cfg keyBytes` returns a `KeyGroupId`
   (`abs (kgcHash keyBytes) mod kgcTotal`).
3. The live `KeyGroupAssignment` decides which worker owns that
   key-group. `assignedToKeyGroupRange` projects the assignment
   into an `IntSet`-backed `KeyGroupRange` for O(log n) membership
   checks.
4. The record is dispatched to that worker.

### Why this lets you scale past the partition count

State is sharded by key-group, not by partition. A topology that was
provisioned for 16 partitions but needs 32 workers can run on 32
workers, each owning 4 key-groups (128 / 32). State for each
key-group lives in its own bucket; the snapshot key in the object
store is `(store, keyGroupId, snapshotId)`. A rebalance moves
key-groups, not partitions.

### When key-groups are not what you want

- You have plenty of partitions and don't need to scale further.
  `DispatchPartition` is simpler and matches JVM Streams 1:1.
- You depend on byte-identical partitioning with a non-streams
  consumer of the same topic. Key-group routing happens *inside*
  the streams worker pool; the underlying partition assignment is
  still whatever Kafka does. But if you mix-and-match with a
  consumer that assumes JVM-style partition-stickiness for state,
  you'll diverge.
- Your topology is stateless. Key-groups solve a state-sharding
  problem; they don't help a pure `map`/`filter` pipeline.

### Switching dispatch modes

`StreamsConfig.dispatchMode` is picked once at startup and the
worker pool is built around it. Switching modes between deploys is
safe **only** if both versions agree on the routing function for
state. This means it's not safe to change without planning, because
`DispatchHashed` and `DispatchPartition` route partitions
deterministically while `DispatchKeyGroup` routes by key, so an
existing local store assembled under one mode is not necessarily
usable under another.

Procedure for the rare switch:

1. `pauseKafkaStreams` to stop processing.
2. `cleanUp` to wipe local state on every instance.
3. Restart with the new `dispatchMode`. The new pool warms state
   from the changelog (or the snapshot) into the new sharding.

## Standby tasks

`numStandbyReplicas` in `StreamsConfig` is the per-[task](../glossary/#task) replication
factor for warm state. The mechanism:

- `Kafka.Streams.Runtime.StandbyTask` runs a second consumer that
  tails each task's changelog and replays into a local replica
  store.
- `Kafka.Streams.Runtime.StandbyDriver` orchestrates the replay,
  tracks per-task lag, and publishes it to the
  `Kafka.Streams.Runtime.WarmupReadiness` map.
- The assignor consults that map. A standby that is within
  `acceptableRecoveryLag` of the active is a candidate for
  promotion the next time a rebalance fires.

### Standby modes

Riffle introduces `StandbyMode = ReplayBytes | SnapshotPointer`:

- `ReplayBytes`: the classic mode. Maintain a full local replica.
  Costs 2× storage and 2× write amplification per replica.
- `SnapshotPointer`: pointer-mode. The standby holds only
  `(snapshotId, advancedTo)`, not a full local copy. Promotion
  fetches the snapshot blob from the object store + replays the
  changelog tail. Costs near-zero storage; promotion time is
  bounded by snapshot size plus the tail-replay window. Use when
  state is large and you have an object store handy.

The constructor is `newSnapshotPointerStandby`;
`bumpSnapshotPointer` is the runtime hook the active calls after
each fresh snapshot.

### How many standbys do you want?

| `numStandbyReplicas` | Behaviour |
| -------------------- | --------- |
| 0 | Single point of failure for state. Any task whose owner dies replays the entire changelog before serving. **Only choose this for stateless or recreatable workloads.** |
| 1 | Default safe value for production stateful topologies. Survives one instance loss with metadata-only promotion. |
| 2+ | Survives concurrent loss of N instances. Pay 1 + N times the state-storage and changelog-write cost. |

## Probing rebalances

`probingRebalanceIntervalMs` (default 10 minutes) is the cadence at
which `Kafka.Streams.Runtime.ProbingRebalance` re-issues a rebalance
*when warmups are within `acceptableRecoveryLag`*. The point is to
hand a task over to its now-warm standby without waiting for the
active to misbehave.

The check is gated: if every warmup is still further behind than
`acceptableRecoveryLag`, the probe is skipped. So setting the
interval shorter does not stampede the group; it just means a
freshly-promoted standby can take over sooner once it's caught up.

For aggressive zero-downtime rollouts, drop `probingRebalanceIntervalMs`
to 60_000 (1 minute) and set `acceptableRecoveryLag` tight (say
1_000). The trade-off is more rebalances, each one cheap, against
fewer rebalances, each one with a longer tail of waiting for warmup.

## `addStreamThread` / `removeStreamThread`

Available at runtime; both are idempotent on the worker-pool side
and trigger a brief in-process reshuffle. Neither triggers a
broker-side rebalance.

Use them for:

- Autoscaling on a per-process basis (e.g. a controller that
  monitors latency and bumps worker count).
- Reacting to in-process pressure (e.g. extra workers during a
  catch-up after a backlog).

Do **not** use them as a substitute for adding instances; once the
process's owned partition set is saturating CPU on every worker,
more in-process workers won't help.

## Pause / resume

`pauseKafkaStreams` / `resumeKafkaStreams` (`Kafka.Streams.Runtime`)
stop record processing while keeping the consumer alive (it still
heartbeats and holds its assignment). Use for:

- Quiescing the runtime before an in-place state migration.
- Synchronising with an out-of-band batch job that needs exclusive
  state access.
- Maintenance windows that shouldn't trigger a rebalance.

Pause / resume is per-instance. To pause the whole consumer group
you must pause every instance; there is no broker-side equivalent.

## What you can't change at runtime

| Knob | Why not |
| ---- | ------- |
| `applicationId` | Determines consumer-group identity and internal-topic names |
| `processingGuarantee` | Producer / consumer wiring is built around it at startup |
| `numStandbyReplicas` (effectively) | Honoured at next assignment; the standby state machine doesn't tear down replicas on a live downgrade |
| Topology shape | Compiled at startup; restart to apply changes |
| `dispatchMode` | Worker pool is constructed around it |

Everything else in `StreamsConfig` is read at startup. Restart to apply.

## A rough capacity formula

For a stateful topology, plan capacity roughly as:

```
total_workers     ≈ ceil(peak_records_per_second / per_worker_throughput)
per_worker_state  ≈ total_state / total_workers
per_worker_ram    ≈ per_worker_state * cache_fraction
                  + per_worker_in_flight * record_size
```

Then:

- `numStreamThreads = ceil(total_workers / num_instances)`
- `num_instances = ceil(total_workers / cores_per_box)` if you're
  CPU-bound, or driven by per-worker memory if you're RAM-bound
- `numStandbyReplicas` adds linearly to both storage and changelog
  write throughput; budget accordingly

Per-worker throughput is the slowest of: record deserialisation,
the user-supplied processor function, state-store write, and (for
EOS) the transactional-producer cycle. Measure rather than guess -
`Kafka.Streams.Metrics` tracks each one.

## Related reading

- [Topology evolution](./topology-evolution/): the deployment side
  of a rolling capacity change.
- [Running in containers](./containers/): stable identity,
  persistent volumes, and off-heap memory when each instance is a
  pod.
- [Exactly-once across Kafka and other systems](./exactly-once/) -
  how the transactional producer interacts with rebalance.
- [Runbooks](./runbooks/): rebalance storms and how to break the
  loop.
