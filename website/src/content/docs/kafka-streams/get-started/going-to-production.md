---
title: "Tutorial 5: Going to production"
description: The bridge from "it runs on my laptop" to "it runs in production". Eight things to set up before deploying.
sidebar:
  order: 6
  label: 5. Going to production
---

You've written a topology, given it state, and joined two streams. Going from that to a production deploy is mostly about **operational concerns**: things that don't show up in the test driver but bite you in production if you ignore them.

## Why production is different from the test driver

The test driver gives you a controlled environment where everything runs in a single process, state lives in memory and disappears when the process exits, there are no network partitions or broker unavailability or slow consumers, and exactly one thread processes everything so there are no race conditions.

Production is the opposite. Your topology will run across multiple instances, each with multiple threads. Network issues happen. Brokers restart. Consumers fall behind. State must survive restarts. The checklist below addresses the gaps between "it works in the test driver" and "it runs reliably in production."

## The eight things you need

Each item below prevents a specific class of production incident:

1. **Pick a [processing guarantee](#1-processing-guarantee)**: at-least-once or exactly-once.
2. **Name every stateful operator** explicitly.
3. **Set `numStandbyReplicas` to at least 1**.
4. **Wire metrics to your observability stack**.
5. **Add the orphan-topic detector** at startup.
6. **Capture a golden file of the topology shape** in CI.
7. **Test rebalance and restart** before they happen for real.
8. **Decide whether you need [Riffle](../riffle/) extensions**.

The rest of this page expands each one.

## 1. Processing guarantee

In the test driver, every record is processed exactly once because there's only one thread and no failures. In production, instances restart, network partitions happen, and the consumer group rebalances. Without an explicit choice, you might get duplicate records (at-least-once) when a task fails over, or pay overhead you don't need (exactly-once) for idempotent workloads.

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

Auto-generated names change when you add or remove operators in your topology. When a name changes, the runtime creates a new internal topic (changelog or repartition) and abandons the old one. Your previous state is orphaned, queries return empty results, and you have zombie topics consuming disk forever.

The library generates names for unnamed operators automatically. Those names become part of the **internal topic** layout: `<applicationId>-<storeName>-changelog`, `<applicationId>-<nodeName>-repartition`.

If you reshuffle your topology (insert a new `map` upstream of an existing aggregation, for example), the auto-generated names can shift. The broker creates *new* internal topics and abandons the old ones. The old ones don't go away; they sit on the broker holding your previous state.

Every operator that owns a state store should pass an explicit `Named` or a custom `materializedAs (storeName "...")`. Anything that creates a repartition topic (`repartition`, `through`) should also have a stable name.

```haskell
-- Good: pinned names
F.count (Mat.materializedAs (storeName "view-counts"))

-- Risky: auto-named, will shift if topology changes
F.count Mat.defaultMaterialized
```

Full discussion in
[Topology evolution and rolling deploys](../../operating/topology-evolution/#stable-names-are-the-deployment-contract).

## 3. Standby replicas

Without standbys, when an instance restarts (deploy, failure, or scale-up), the new owner of each task must replay the entire changelog from the beginning. For large state stores this takes hours, during which that partition is unavailable for interactive queries and processes records slowly. Standbys keep warm replicas so failover is near-instant.

`numStandbyReplicas = 0` (the default) means each task's state
lives on exactly one instance. When that instance drains, the
next owner replays the *entire* changelog before serving: for a
1 TB store on 100 MB/s replay, that's three hours of unavailability
for any query against that task.

**Set `numStandbyReplicas = 1`** for any non-trivial state. The
runtime keeps warm replicas; failover becomes metadata-only and
takes seconds, not hours.

For zero-downtime rollouts of critical workloads, set it to 2 so
each standby has its own standby.

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

Trade-off is 2× state-storage and 2× changelog write
amplification per replica. Worth it.

Details: [Scaling: standby tasks](../../operating/scaling/#standby-tasks).

## 4. Metrics

In the test driver, you see every record. In production, you have hundreds of thousands of records per second across multiple instances. Without metrics, you have no visibility into whether processing is keeping up, whether commits are succeeding, or whether records are being silently dropped. The four metrics in the table below catch the most common production incidents.

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

Deployments that rename operators (even accidentally) leave behind internal topics that nobody is using but everyone is paying for. A 100-partition changelog topic with 7-day retention consumes significant disk. Over months, these orphans accumulate. Detecting them at startup lets you clean up before they become a budget line item.

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

The detector only warns; it never deletes. Auto-deletion is a
foot-gun (a misconfigured rollout would happily nuke live state).
Make manual deletion an audited operator action.

Details:
[Observability: orphan internal topics](../../operating/observability/#orphan-internal-topics).

## 6. Topology golden file

Code review can't catch topology changes that accidentally shift auto-generated names. A seemingly innocent refactor (adding a `map` upstream of an aggregation) can renumber every subsequent operator, causing all internal topics to be recreated and state to be lost. A golden file test fails the build when the topology shape changes, forcing explicit review of the diff.

Snapshot the topology JSON into your repo:

```haskell
import qualified Data.Aeson.Encode.Pretty as P
import qualified Kafka.Streams.Observability.Topology as Obs

writeGolden :: IO ()
writeGolden = do
  topo <- F.buildTopologyFrom myTopology
  BL.writeFile "test/golden/topology.json"
    (P.encodePretty (Obs.topologyDescription topo))
```

Add a test that fails if `myTopology` produces a different JSON. Even innocent-looking PRs surface the diff explicitly. Reviewing the diff is your last line of defence against the "I added a `map` and now my changelog topic has a different name" class of bug.

A diff touching `sources`, `stores`, or `edges` is a deploy you
have to think about. A diff touching only `processors` (renumbered
unnamed nodes) is harmless **only if** none of the renumbered
nodes own state. See
[Topology evolution](../../operating/topology-evolution/) for the
full classification.

## 7. Test rebalance and restart

The test driver runs everything in one thread with no failure injection. Production has network partitions, broker restarts, rolling deploys, and consumer group rebalances. These events trigger code paths your unit tests never exercise. Testing rebalance and restart scenarios before they happen in production reveals whether your state stores recover correctly, whether your external resources clean up properly, and whether your exactly-once guarantees hold under failure.

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
exercised in its full mode. This includes partition handoff and replay.

## 8. Do you need Riffle?

The base Kafka Streams library handles most workloads well, but it has limitations. External API calls block the stream thread. Exactly-once semantics only work for Kafka-to-Kafka pipelines. Large state stores take hours to replay. You can't scale past your partition count. Riffle extensions address these specific gaps.

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

## Recap

The eight things to set up before a production deploy. The trade-offs of each `processingGuarantee`. Why naming every stateful operator matters across deploys. The minimum metrics and listeners to wire up. The decision tree for whether to reach for Riffle.

## Where to go from here

You've finished the tutorial. The rest of these docs are organised
by what you'll need them for:

| If you're... | Read |
| ------------ | ---- |
| Designing a rolling deploy | [Topology evolution](../../operating/topology-evolution/) |
| Sizing your cluster | [Scaling and rebalancing](../../operating/scaling/) |
| Deploying to Kubernetes or another container runtime | [Running in containers](../../operating/containers/) |
| Writing to a non-Kafka sink with EOS | [Exactly-once across systems](../../operating/exactly-once/) |
| Building your observability | [Observability](../../operating/observability/) |
| Trying to understand IQ semantics | [Visibility versus ACID](../../operating/visibility/) |
| On-call for a streams app | [Runbooks](../../operating/runbooks/) |
| Enriching from an external system | [Enrichment](../../guides/enrichment/) |
| Curious what auto-optimisation does | [Topology optimization](../../concepts/topology-optimization/) |
| Wondering what you can change at runtime | [Dynamic topology changes](../../concepts/dynamic-topology/) |
| Looking at Riffle for the first time | [Riffle: Flink-class extensions](../../riffle/) |
| Hitting an unfamiliar term | [Glossary](../../glossary/) |

The [Overview page](../../) has the operator-facing map of everything.

Welcome to streams.
