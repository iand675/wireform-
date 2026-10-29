---
title: Observability
description: How to monitor your Kafka Streams application. What to measure, what to alert on, and how to debug when things go wrong.
sidebar:
  order: 6
---

A Kafka Streams application can fail in ways that traditional HTTP services do not. The failures are often invisible to standard metrics like request latency or error rates.

## Why streaming observability is different

Stateless HTTP services are straightforward to monitor: if requests succeed quickly and return 200s, the service is healthy. Streaming applications have more failure modes:

- **Silent data loss**: Bad records get dropped without anyone noticing
- **Stalled processing**: The app is running but not making progress
- **State corruption**: Local state diverges from the Kafka log
- **Rebalance storms**: Constant reassignments prevent any work from happening
- **Slow commits**: Transaction timeouts cause repeated retries

These problems do not show up in CPU or memory graphs. You need streaming-specific metrics.

## What to monitor: the four key surfaces

The library exposes four main observability surfaces:

| Surface | What it tells you | Why it matters |
| ------- | ----------------- | -------------- |
| **Metrics** (`streamsMetrics`) | Throughput, drops, commits, recoveries, rebalances | Detects stalls, data loss, and performance degradation |
| **Topology JSON** | The shape of your processing graph | Catches accidental topology changes before deploy |
| **Orphan-topic detection** | Internal topics that should not exist | Prevents storage leaks and state confusion |
| **Runtime listeners** | State transitions, rebalances, restore progress | Shows stuck rebalances and how long failover takes |

Plus **interactive queries** for debugging: looking directly at your state stores when something seems wrong.

## Metrics

Unlike HTTP services where you can see failures in request
logs, streaming applications process records continuously in the background.
Without metrics, you have no visibility into whether records are being processed,
dropped, or stalled.

A running instance counts in its tasks (unboxed counters, nothing
allocated per record) and `streamsMetrics` snapshots them, under the
JVM metric names, as a `Map Text MetricValue`. Nothing is pushed
anywhere: you poll it, or hand it to the OpenTelemetry bridge
(`Kafka.Streams.Observability.OpenTelemetry.registerStreamsMetrics`),
which takes the same snapshot action.

### What it reports

Per task (tagged `{task=<subtopology>_<partition>}`) and summed over
the instance's tasks (untagged):

| Metric | What it counts | Alert when |
| -------- | ---------------- | ---------- |
| `stream-processor-node-metrics:process-total` | Records handed to the topology | The rate drops while input lag grows |
| `stream-task-metrics:dropped-records-total` | Records skipped (a deserialization failure the handler continued past, a missing value where one is required) | Any sustained non-zero rate |
| `stream-task-metrics:punctuate-total` | Punctuator firings | It stops while a punctuator should be firing |
| `stream-task-metrics:commit-total` | Task commits | |

For the instance:

| Metric | What it counts | Alert when |
| -------- | ---------------- | ---------- |
| `stream-thread-metrics:commit-total` | Commits | The rate drops |
| `stream-thread-metrics:commit-failed-total` | At least once: offset commits that failed (input is replayed) | It rises outside rebalances |
| `stream-thread-metrics:commit-aborted-total` | Commits aborted (every task recovered) | Any sustained rate |
| `stream-thread-metrics:task-recoveries-total` | Rewinds of every task to the last commit: aborts, replaced threads, lost partitions under EOS | A spike, or a steady rate |
| `stream-thread-metrics:rebalance-total` | Assignments received | Rising quickly: a rebalance storm |
| `stream-state-metrics:restore-total` | Changelog records applied while restoring | Rising for long after a rebalance |

Counters restart from zero when the instance starts (and a task's
when it is assigned), so alert on rates.

### Reading metrics in code

```haskell
import qualified Data.Map.Strict as Map
import Kafka.Streams.Metrics (MetricValue (..))
import qualified Kafka.Streams.Metrics as Met

m <- streamsMetrics streams
case Map.lookup Met.threadCommitAbortedTotal m of
  Just (MVCounter n) -> report "commit-aborted-total" n
  _ -> pure ()
```

A typical setup polls it on a timer and forwards the snapshot:

```haskell
metricsLoop :: KafkaStreams -> IO ()
metricsLoop streams = forever $ do
  streamsMetrics streams >>= pushToGateway
  threadDelay (10 * 1000000)  -- 10 seconds
```

`Kafka.Streams.Metrics` also has a standalone registry
(`newMetricsRegistry`, `incCounter`, `dumpMetrics`) for counters of
your own; `registrySource` turns it into the same kind of snapshot
action.

### Essential metric labels

When forwarding metrics, include these dimensions:

- **`applicationId`**: Distinguishes multiple apps in the same cluster
- **`instance`**: Identifies which process emitted the metric
- **`taskId`**: Groups by partition and subtopology
- **`nodeName`**: Identifies specific operators in the topology

Without these, you cannot tell which part of your pipeline is having problems.

## Topology JSON

The `topologyDescription` function exports your topology as JSON. This serves two purposes: CI validation and runtime debugging.

### CI golden-file testing

Snapshot your topology JSON in version control:

```haskell
import qualified Data.Aeson.Encode.Pretty as P
import qualified Kafka.Streams.Observability.Topology as Obs

writeGolden :: IO ()
writeGolden = do
  topo <- buildTopologyFrom myTopology
  BL.writeFile "test/golden/topology.json"
    (P.encodePretty (Obs.topologyDescription topo))
```

Add a test that fails if the topology changes:

```haskell
testTopologyShape :: IO ()
testTopologyShape = do
  topo <- buildTopologyFrom myTopology
  let actual = P.encodePretty (Obs.topologyDescription topo)
  expected <- BL.readFile "test/golden/topology.json"
  unless (actual == expected) $
    error "Topology changed! Review before deploying."
```

This catches accidental changes that would create new internal topics or rename state stores.

### Runtime topology inspection

During incidents, dump the live topology:

```haskell
live <- Obs.liveTopologyDescription topo (streamsMetrics streams) cfg
-- live includes current metric values overlaid on the graph
```

This shows which operators are processing records and which are stalled.

## Orphan-topic detection

When you rename a stateful operator or change window configuration, the framework creates new internal topics. The old topics remain on the broker, silently consuming storage.

### Detecting orphans

Run this on startup:

```haskell
import qualified Kafka.Streams.Observability.OrphanTopics as Orphan

topics <- AdminClient.listTopics admin
forM_ (Orphan.detectOrphans topo appId topics) $ \o ->
  warn ("Orphan internal topic: " <> unTopicName (Orphan.orphanTopic o))
```

The detector compares your current topology against actual topics on the broker. It flags anything that looks like an internal topic but is not referenced by your current topology.

### Why this matters

Orphan topics cause two problems:

1. **Storage cost**: Unbounded growth of unused data
2. **Confusion**: During incidents, operators cannot tell which topics are live

The detector only warns. It never deletes. Make deletion a manual, audited operation.

## Runtime listeners

Failover time is restore time: a task that moves restores its stores
before it processes anything (from its standby's position when it
lands on one, else from a checkpoint or the changelog start), and the
instance reports `StreamsRebalancing` meanwhile. Three listeners show
what's happening:

```haskell
import Kafka.Client.RebalanceListener (RebalanceListener (..), noopRebalanceListener)

R.setStateListener streams $ \old new ->
  logInfo ("state " <> show old <> " -> " <> show new)

R.setRebalanceListener streams noopRebalanceListener
  { rlOnAssigned = \tps -> logInfo ("assigned " <> show tps)
  , rlOnRevoked  = \tps -> logInfo ("revoked " <> show tps)
  , rlOnLost     = \tps -> logWarn ("lost " <> show tps)
  }

R.setStateRestoreListener streams R.defaultStateRestoreListener
  { R.onRestoreStart  = \tp store from to -> restoreStarted tp store (to - from)
  , R.onBatchRestored = \tp store _offset n -> restoreProgress tp store n
  , R.onRestoreEnd    = \tp store total -> restoreFinished tp store total
  }
```

### Alert thresholds

- **Warning**: an instance in `StreamsRebalancing` for longer than
  your expected restore time.
- **Critical**: `StreamsError` (the instance stopped; the state
  listener gets the reason).
- Track the restore duration per store over time: it's the failover
  time, and it grows with the changelog.

`localTaskLags` reads, on demand, every task the instance holds state
for (running, restoring, standby) against its changelogs' end: a
standby that falls behind there makes its failover slower.
`Kafka.Streams.Observability.Lag` summarises the rows and
`Kafka.Streams.Observability.Health.kafkaStreamsHealthWithLag` turns
the active ones into readiness.

## Interactive queries for debugging

When metrics show a problem but the cause is unclear, query the state stores directly:

```haskell
-- with OverloadedRecordDot
Just ro <- IQ.queryKVStore @Text @Int64 streams (storeName "user-store")
count <- ro.roKvGet "user-123"
putStrLn ("User 123 has " <> show count <> " events")
```

### When to use IQ

| Situation | How to use IQ |
|-----------|---------------|
| Records seem missing | Query the relevant store to see if state exists |
| Counts look wrong | Read current totals and compare to expectations |
| Test failures | Inspect store contents to understand test behavior |
| Incident response | Verify state is what you expect before taking action |

### Important caveats

IQ reads local state, which may be slightly ahead of committed state:

```
Timeline:
  t=0: Record processed, state updated
  t=1: IQ query returns new value
  t=2: Commit runs
  t=3: State is durable in Kafka
```

Between t=1 and t=2, a crash would roll that update back (it's replayed from the last committed offsets). IQ is for debugging and approximate reads, not financial transactions.

## The minimum viable observability setup

Before going to production, set up:

1. **Metrics polling**: `streamsMetrics` every 10 seconds to your time-series database
2. **Three alerts**:
   - the instance in `StreamsError`
   - `StreamsRebalancing` for longer than your restore budget
   - repeated `commit aborted` log lines
3. **Golden-file test**: CI fails if topology shape changes unexpectedly
4. **Orphan detection**: Startup warning for drift detection
5. **State listener**: Log all state transitions for incident debugging

```haskell
main = do
  streams <- newKafkaStreams cfg topo

  -- Alert-critical listeners
  setStateListener streams $ \_old new -> case new of
    StreamsError why -> sendPage ("Streams entered StreamsError: " <> why)
    _ -> pure ()

  -- Observability setup
  forkIO (metricsLoop streams)    -- streamsMetrics in a loop
  checkForOrphans streams

  startKafkaStreams streams
```

## Related reading

- [Visibility versus ACID databases](./visibility/): Understanding what IQ actually shows you
- [Runbooks](./runbooks/): Specific procedures for the alerts described here
- [Topology evolution](./topology-evolution/): How the orphan detector and golden files interact with deploys
