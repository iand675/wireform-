---
title: Scaling and rebalancing
description: Tasks, threads, instances, co-partitioning, and what happens to state when partitions move.
sidebar:
  order: 3
---

As your data volume grows, you need more processing power. The runtime
gives you two ways to scale: more worker threads in a process, and more
processes in the group. Both are bounded by the number of tasks, which
is bounded by your topics' partition counts.

:::tip[Unfamiliar terms?]
Kafka, Streams, and Riffle terminology is defined in the [Glossary](../glossary/).
:::

:::note[TL;DR]

- A task is one subtopology on one partition number. Parallelism is
  capped by the number of tasks, i.e. by partition counts.
- One consumer per instance (range assignor); its tasks are processed
  in parallel over `numStreamThreads` workers between polls.
- Stateful subtopologies must read co-partitioned topics (same
  partition count); the runtime refuses to start otherwise.
- Failover restores state from changelogs. Standby replicas are not
  supported (`numStandbyReplicas > 0` only logs a warning).
- `addStreamThread` / `removeStreamThread` change the worker count in
  place; they never trigger a rebalance.

:::

## Tasks

At start the runtime splits the topology into subtopologies
(`Kafka.Streams.Topology.Subtopology`): nodes linked by an edge, or
sharing a state store, belong together; a repartition topic is a cut.
Each subtopology runs as one task per partition number: task
`(s, p)` gets partition `p` of every source topic of subtopology `s`,
owns its own stores, and writes partition `p` of their changelogs.

The number of tasks of a subtopology is the most partitions of any of
its source topics. Repartition topics get their partition count from
the plan (`Kafka.Streams.Runtime.Plan`):

1. an explicit count (`Repartitioned` with a partition count), else
2. if a stateful subtopology reads the topic alongside other topics,
   their partition count (it must be co-partitioned with them), else
3. the most partitions of the topics feeding the subtopology that
   writes it.

The runtime refuses to start (and the instance goes to
`StreamsError`) when a stateful subtopology reads source topics with
different partition counts: a join or aggregation over them would see
the same key in different tasks. Repartition the smaller side first.

## Threads inside a process

Each instance has **one** main consumer, a member of the
`applicationId` group, and a set of tasks for the partitions it was
assigned. Between polls, the tasks process their buffered records in
parallel, spread over `numStreamThreads` workers; then the loop polls
again (and commits when due). This differs from the JVM, where each
stream thread is its own consumer:

- One consumer connection and one transactional producer per process,
  whatever `numStreamThreads` is.
- Tasks aren't tied to a worker: `addStreamThread` and
  `removeStreamThread` just change the worker count, and the tasks are
  spread over the new count on the next loop. Neither triggers a
  rebalance, and no state moves.

Worker threads only help while there are more tasks than workers on
the instance. A process with 4 tasks gains nothing from an 8th thread.

## Processes across the group

More instances with the same `applicationId` join the group and share
the partitions. The consumer uses the range assignor, which hands out
contiguous partition ranges per topic, so partition `p` of
equally-partitioned topics lands on the same member: what a task that
reads several co-partitioned topics needs.

A rebalance (an instance joins or leaves, or a probing rebalance) runs
like this on each member, inside `poll`:

1. **Before rejoining**, the member commits everything its tasks have
   processed (the pre-rejoin hook; the old generation can still commit
   at this point).
2. **Revoked** partitions: their tasks are closed cleanly and
   checkpointed. **Lost** partitions (the member was fenced): their
   tasks are closed dirty, with nothing committed.
3. **Assigned** partitions: new tasks are built; their source
   partitions are paused while every logged store restores from its
   changelog (from a checkpoint for persistent stores, else from the
   log start; under exactly-once only committed data). Then they
   resume. Partitions with no committed offset start at their source's
   offset-reset policy (earliest by default).

`setRebalanceListener` fires after each of these steps, with the
partitions involved. `ownedPartitions` shows the current assignment.

### The ceiling is the partition count

An instance can only work on tasks for partitions it owns. Adding a
6th instance to a topology whose widest subtopology has 5 tasks just
creates an idle member. Repartition up front (more partitions on the
input, or `repartitionWith` an explicit partition count) to scale
further.

### Restore time is the failover time

There are no standby replicas: a task that moves restores its stores
from their changelogs before processing. Keep that bounded:

- Changelog topics are compacted (`cleanup.policy=compact`), so a
  restore replays roughly one record per live key.
- Persistent stores (`persistentKeyValueStoreBuilder`) are
  checkpointed under at least once and at every clean close; a task
  that comes back to the same machine restores only from its
  checkpoint.
- `setStateRestoreListener` reports start, batches and end per store
  and changelog partition: alert on long restores.

## Key-groups

`Kafka.Streams.Runtime.KeyGroup` and `Kafka.Streams.Runtime.WorkerPool`
(the "Riffle" key-group routing: a fixed routing space between keys and
workers) are a separate library. The runtime does not use them; its
parallelism is tasks.

## Probing rebalances

`probingRebalanceIntervalMs` (default 10 minutes) is the cadence at
which the runtime asks for a rebalance when a warmup reported through
`reportWarmupLag` is within `acceptableRecoveryLag`. With interval `0`
it never does. Without standby replicas nothing reports warmup lag on
its own; the hook is there for callers that track it themselves.

## `addStreamThread` / `removeStreamThread`

Available at runtime; they change the number of workers the tasks are
spread over and return the new count (`removeStreamThread` never goes
below one). Use them for autoscaling a process, e.g. extra workers
during a backlog catch-up. They don't substitute for instances once
every task already has a worker.

## Pause / resume

`pauseKafkaStreams` / `resumeKafkaStreams` (`Kafka.Streams.Runtime`)
pause fetching and processing on every assigned partition while the
consumer stays in the group. Use for:

- Quiescing the runtime before an in-place state migration.
- Synchronising with an out-of-band batch job that needs exclusive
  state access.
- Maintenance windows that shouldn't trigger a rebalance.

Pause / resume is per-instance. To pause the whole application you
must pause every instance.

## What you can't change at runtime

| Knob | Why not |
| ---- | ------- |
| `applicationId` | Determines consumer-group identity and internal-topic names |
| `processingGuarantee` | Producer / consumer wiring is built around it at startup |
| Topology shape | Compiled at startup; restart to apply changes |

The worker count changes with `addStreamThread` / `removeStreamThread`.
Everything else in `StreamsConfig` is read at startup. Restart to apply.

## A rough capacity formula

For a stateful topology, plan capacity roughly as:

```
total_workers     ≈ ceil(peak_records_per_second / per_worker_throughput)
tasks             ≥ total_workers            (raise partition counts if not)
per_worker_state  ≈ total_state / total_workers
per_worker_ram    ≈ per_worker_state + per_worker_buffered * record_size
```

Then:

- `numStreamThreads = ceil(total_workers / num_instances)`
- `num_instances = ceil(total_workers / cores_per_box)` if you're
  CPU-bound, or driven by per-worker memory if you're RAM-bound
- `bufferedRecordsPerPartition` (default 1000) bounds
  `per_worker_buffered`: a partition is paused once that many of its
  records wait unprocessed

Per-worker throughput is the slowest of: record deserialisation,
the user-supplied processor function, state-store write, and (for
EOS) the transactional-producer cycle. Measure rather than guess.

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
