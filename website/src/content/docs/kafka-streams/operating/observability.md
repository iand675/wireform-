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
| **Metrics registry** | Throughput, latency, errors per operator | Detects stalls, data loss, and performance degradation |
| **Topology JSON** | The shape of your processing graph | Catches accidental topology changes before deploy |
| **Orphan-topic detection** | Internal topics that should not exist | Prevents storage leaks and state confusion |
| **Lag tracking** | How far standbys are behind the active | Ensures fast failover is actually possible |

Plus **interactive queries** for debugging: looking directly at your state stores when something seems wrong.

## The metrics registry

Unlike HTTP services where you can see failures in request
logs, streaming applications process records continuously in the background.
Without metrics, you have no visibility into whether records are being processed,
dropped, or stalled. The registry is the foundation of all observability.

`Kafka.Streams.Metrics` maintains counters, gauges, and duration stats in memory. Unlike some clients, it does not automatically push to Prometheus or Datadog. You poll it and forward to your own system.

### Key metrics to watch

The most important metrics for operational health:

| Metric | What it measures | Alert when |
| -------- | ---------------- | ---------- |
| `processTotal` | Records processed per operator | Sudden drop indicates stall |
| `forwardTotal` | Records emitted per operator | Gap from `processTotal` shows filtering rate |
| `droppedRecordsTotal` | Records dropped (deserialization failures, etc.) | Any sustained non-zero value |
| `commitTotal` | Commit cycles completed | Drop indicates transaction problems |
| `commit-cycle-fatal` | Unrecoverable commit failures | Any value above zero |
| `warmup-lag` | How far standbys trail the active | Above threshold for extended period |

### Reading metrics in code

```haskell
import qualified Kafka.Streams.Metrics as Met

-- Poll all metrics
m <- Met.dumpMetrics registry

-- Read specific counters
counter <- Met.readCounter registry "stream-task-metrics:commit-total"
gauge   <- Met.readGauge   registry "stream-state-metrics:cache-hit-ratio"
```

A typical setup runs a background thread:

```haskell
metricsLoop :: Met.MetricsRegistry -> IO ()
metricsLoop registry = forever $ do
  metrics <- Met.dumpMetrics registry
  -- Forward to Prometheus, Datadog, etc.
  pushToGateway metrics
  threadDelay (10 * 1000000)  -- 10 seconds
```

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
live <- Obs.liveTopologyDescription topo metricsRegistry cfg
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

## Lag tracking

Standby replicas maintain copies of state for fast failover. But they only help if they are reasonably current.

### What lag tells you

`LagListener` receives snapshots of how far each standby trails its active:

```haskell
R.setLagListener streams $ \lags ->
  forM_ lags $ \lag -> do
    -- lag contains taskId, store name, current offset, end offset
    publishMetric (makeLagGauge lag)
```

If a standby's lag exceeds `acceptableRecoveryLag`, it will not be promoted during failover. The new active will replay from the changelog instead, which takes time.

### Alert thresholds

- **Warning**: Lag above 50% of `acceptableRecoveryLag` for 5 minutes
- **Critical**: Lag above `acceptableRecoveryLag` for any duration
- **Page**: Standby promoted but lag above zero (means degraded failover)

## Interactive queries for debugging

When metrics show a problem but the cause is unclear, query the state stores directly:

```haskell
ro <- IQ.queryKVStore streams "user-store"
count <- IQ.roKvGet ro "user-123"
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
  t=2: Commit cycle runs
  t=3: State is durable in Kafka
```

Between t=1 and t=2, a crash would lose that state update. IQ is for debugging and approximate reads, not financial transactions.

## The minimum viable observability setup

Before going to production, set up:

1. **Metrics polling**: Every 10 seconds to your time-series database
2. **Three alerts**:
   - `droppedRecordsTotal` > 0 for 1 minute
   - `commit-cycle-fatal` > 0
   - `warmup-lag` > `acceptableRecoveryLag` for 5 minutes
3. **Golden-file test**: CI fails if topology shape changes unexpectedly
4. **Orphan detection**: Startup warning for drift detection
5. **State listener**: Log all state transitions for incident debugging

```haskell
main = do
  streams <- newKafkaStreams topo cfg

  -- Alert-critical listeners
  setStateListener streams $ \old new ->
    when (new == ERROR) $ sendPage "Streams entered ERROR state"

  -- Observability setup
  forkIO metricsLoop
  checkForOrphans streams

  startKafkaStreams streams
```

## Related reading

- [Visibility versus ACID databases](./visibility/): Understanding what IQ actually shows you
- [Runbooks](./runbooks/): Specific procedures for the alerts described here
- [Topology evolution](./topology-evolution/): How the orphan detector and golden files interact with deploys
