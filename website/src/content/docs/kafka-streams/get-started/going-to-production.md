---
title: "Tutorial 5: Going to production"
description: The bridge from "it runs on my laptop" to "it runs in production". Eight things to set up before deploying.
sidebar:
  order: 6
  label: 5. Going to production
---

You've written a topology, given it state, and joined two streams.
Going from that to a production deploy is mostly about
**operational concerns** — things that don't show up in the test
driver but bite you in production if you ignore them.

This part is a checklist. Each item has a short "what" and a
pointer to the page where it's covered in depth.

## The eight things you need

If you do nothing else, do these eight before deploying:

1. **Pick a [processing guarantee](#1-processing-guarantee)** —
   at-least-once or exactly-once.
2. **Name every stateful operator** explicitly.
3. **Set `numStandbyReplicas` to at least 1**.
4. **Wire metrics to your observability stack**.
5. **Add the orphan-topic detector** at startup.
6. **Capture a golden file of the topology shape** in CI.
7. **Test rebalance and restart** before they happen for real.
8. **Decide whether you need [Riffle](../riffle/) extensions**.

The rest of this page gives you the short version and links out
where the details matter.

## 1. Processing guarantee

`StreamsConfig.processingGuarantee` picks between:

| | At-least-once (default) | Exactly-once-V2 |
| --- | ----------------------- | --------------- |
| Output records | Possibly duplicated on rewind | Atomic with offset commit |
| Side effects (peek, foreach, mapValuesM) | Replay on rewind | Replay on rewind (still!) |
| Throughput | Higher | Slightly lower (transactional producer overhead) |
| When to pick it | Your downstream is idempotent | You need exactly-once into Kafka, and you handle side effects separately |

**At-least-once is fine for most things.** Pick it if downstream
consumers are already idempotent or if you can tolerate occasional
duplicates.

**Pick exactly-once when** you write to Kafka and a duplicate
would matter (financial postings, invoice generation, downstream
analytics that can't dedupe).

For external-system writes that need to be atomic with the Kafka
side, see
[Exactly-once across Kafka and other systems](../../operating/exactly-once/).
That's where the Riffle two-phase-commit sink interface comes in.

## 2. Name every stateful operator

The library generates names for unnamed operators automatically.
Those names become part of the **internal topic** layout:
`<applicationId>-<storeName>-changelog`,
`<applicationId>-<nodeName>-repartition`.

If you reshuffle your topology (insert a new `map` upstream of an
existing aggregation, for example), the auto-generated names can
shift — which means the broker creates *new* internal topics and
abandons the old ones. The old ones don't go away; they sit on the
broker holding your previous state.

**Rule:** every operator that owns a state store should pass an
explicit `Named` or a custom `materializedAs (storeName "...")`.
Anything that creates a repartition topic (`repartition`,
`through`) should also have a stable name.

```haskell
-- Good: pinned names
F.count (Mat.materializedAs (storeName "view-counts"))

-- Risky: auto-named, will shift if topology changes
F.count Mat.defaultMaterialized
```

Full discussion in
[Topology evolution and rolling deploys](../../operating/topology-evolution/#stable-names-are-the-deployment-contract).

## 3. Standby replicas

`numStandbyReplicas = 0` (the default) means each task's state
lives on exactly one instance. When that instance drains, the next
owner has to rebuild from the changelog before serving.

**Set `numStandbyReplicas = 1`** for any non-trivial state. Use `2`
for workloads where a failed standby during a rollout would still be
a real incident.

```haskell
import qualified Kafka.Streams.Config as C

cfg :: C.StreamsConfig
cfg = C.defaultStreamsConfig
  { C.applicationId      = "my-app"
  , C.bootstrapServers   = ["broker:9092"]
  , C.numStreamThreads   = 4
  , C.numStandbyReplicas = 1     -- <-- this
  , C.processingGuarantee = C.ExactlyOnceP
  }
```

The trade-off is extra disk and changelog traffic for every
replica. For stateful services, that is usually cheaper than cold
replay during a deploy.

Details: [Scaling — standby tasks](../../operating/scaling/#standby-tasks).

## 4. Metrics

`Kafka.Streams.Metrics.dumpMetrics` returns a snapshot of every
counter, gauge, and duration stat the runtime records. Wire it to
your observability stack via a periodic poll.

The minimum you should alert on:

| Metric | Alert when |
| ------ | ---------- |
| `droppedRecordsTotal` | Sustained non-zero rate (silent data loss) |
| `commit-cycle-aborted` | High rate (something keeps failing) |
| `commit-cycle-fatal` | Any value > 0 (operator intervention required) |
| Per-task `warmup-lag` from `LagListener` | Above `acceptableRecoveryLag` for longer than `probingRebalanceIntervalMs` |

Plus per-operator throughput (`processTotal`, `forwardTotal`) so
you can see where the bottleneck is when latency rises.

Full list and patterns:
[Observability](../../operating/observability/).

## 5. Orphan-topic detector

When you rename a stateful operator (or accidentally renumber its
auto-generated name), the old internal topic on the broker becomes
an **orphan**. It silently consumes disk forever.

`detectOrphans :: Topology -> Text -> [TopicName] ->
[OrphanInternalTopic]` is a pure function. Call it on startup with
your topology and an `AdminClient.listTopics` result:

```haskell
topics  <- AdminClient.listTopics admin
orphans <- pure (detectOrphans topo appId topics)
forM_ orphans $ \o ->
  warn ("orphan internal topic: " <> unTopicName (orphanTopic o))
```

The detector only warns; it never deletes. Treat deletion as an
audited operator action after the rollout has settled.

Details:
[Observability — orphan internal topics](../../operating/observability/#orphan-internal-topics).

## 6. Topology golden file

Snapshot the topology JSON into your repo and fail CI when it
changes. That makes topology drift a code-review event instead of a
surprise at deploy time.

A diff touching `sources`, `stores`, or `edges` is a deploy you
have to think about. A diff touching only `processors` (renumbered
unnamed nodes) is harmless **only if** none of the renumbered
nodes own state. See
[Topology evolution](../../operating/topology-evolution/) for the
full classification.

## 7. Test rebalance and restart

The test driver runs one task in one thread. Production runs
multiple tasks across multiple instances. The gap between them
is where the operationally-surprising bugs live.

Two extra tests every production topology should have:

- **Rebalance test.** Spin up two `KafkaStreams` instances in the
  same process pointed at the same `applicationId`. Use the
  multi-instance mock harness
  (`Kafka.Streams.Runtime.MultiInstanceMockHarness`) to simulate
  membership changes. Verify the assignment is what you expect
  and that no task is double-owned during the transfer.
- **Restart test.** Start a topology, feed some records, close it,
  start it again with the same `applicationId` and state
  directory. Verify the second run picks up from where the first
  left off and produces the right output. (If it doesn't, your
  changelog or state-store wiring is wrong.)

Both tests catch problems that only manifest when the runtime is
exercised in its full mode — partition handoff and replay.

## 8. Do you need Riffle?

Riffle is the extensions tier. You can ignore it until a parity
limitation bites. Common signs you'll want it:

| If you... | Reach for |
| --------- | --------- |
| Enrich from a slow external API (HTTP, gRPC) | [Async I/O](../../guides/enrichment/#pattern-5-async-io-for-high-latency-external-calls) |
| Write to Postgres / S3 / Iceberg / HTTP and need EOS | [Two-phase commit sinks](../../operating/exactly-once/) |
| Have multi-TB state and 1 TB changelog replay is killing your rollouts | [Snapshot-aware stores](../riffle/#state-durability-decoupled-from-the-changelog) |
| Want more workers than your topic has partitions | [Key-group dispatch](../../operating/scaling/#key-groups-parallelism-decoupled-from-partitions) |
| Join two streams that advance at very different rates | [Watermark coordinator](../riffle/#cross-source-watermark-coordinator) |
| Need to migrate a state store's schema | [`SchemaVersioned` store](../riffle/#operator-level-upgrades) |

Adopting Riffle is incremental. Each feature is a new module or a
new smart constructor; selecting one doesn't change anything else
about your topology. See [Riffle](../riffle/) for the tour.

## A minimal production config

Putting it together:

```haskell
import qualified Kafka.Streams.Config as C

prodCfg :: C.StreamsConfig
prodCfg = C.defaultStreamsConfig
  { C.applicationId             = "my-app"
  , C.bootstrapServers          = ["broker-1:9092", "broker-2:9092"]
  , C.clientId                  = "my-app-instance-1"
  , C.numStreamThreads          = 4
  , C.numStandbyReplicas        = 1
  , C.processingGuarantee       = C.ExactlyOnceP
  , C.commitIntervalMs          = 30_000
  , C.acceptableRecoveryLag     = 10_000
  , C.probingRebalanceIntervalMs = 600_000
  , C.replicationFactor         = 3                  -- match your broker
  , C.stateDir                  = "/var/lib/my-app/state"
  , C.applicationServer         = Just "my-app-instance-1.svc.cluster.local:8080"
  }
```

Then the startup wiring:

```haskell
main :: IO ()
main = do
  topo <- F.buildTopologyFrom myTopology

  -- Orphan-topic detector
  admin   <- AdminClient.newAdminClient (C.bootstrapServers prodCfg)
  topics  <- AdminClient.listTopics admin
  let orphans = detectOrphans topo (C.applicationId prodCfg) topics
  forM_ orphans $ \o ->
    warn ("orphan: " <> unTopicName (orphanTopic o))

  -- Construct + start
  ks <- newKafkaStreams topo prodCfg
  setStateListener ks $ \old new ->
    info ("state " <> show old <> " -> " <> show new)
  setLagListener  ks $ \lags ->
    forM_ lags publishMetric
  startKafkaStreams ks

  -- Wait for shutdown signal
  awaitShutdown
  closeKafkaStreams ks
```

## Where to go from here

You've finished the tutorial. The [overview page](../../) is the map
for the rest of the docs: operations, concepts, guides, runbooks, and
the glossary.
