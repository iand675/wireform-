---
title: Observability
description: Metrics, topology JSON, orphan-topic detection, lag tracking, and interactive queries — the surfaces you need wired up before the first production incident.
sidebar:
  order: 6
---

A Kafka Streams app can be healthy from an HTTP-latency point of view
while its topology is stalled, replaying, dropping records, or leaking
old internal topics. This page covers the surfaces that make those
failures visible.

:::tip[Unfamiliar terms?]
Kafka, Streams, and Riffle terminology is defined in the [Glossary](../glossary/).
:::

:::note[TL;DR]
- Four surfaces: metrics registry, topology JSON, orphan-topic detector, lag tracking.
- The metrics registry is plain in-memory; you wire it to your push gateway via a periodic `dumpMetrics` poll.
- Topology JSON (`topologyDescription` / `liveTopologyDescription`) is a versioned document suitable for CI golden-file diffing and live UI overlays.
- Always alert on `droppedRecordsTotal` (silent data loss), `commit-cycle-fatal` (operator-required), and per-task warmup lag above `acceptableRecoveryLag`.
- Interactive queries (IQ) are a debugging surface, not a replacement for a query layer.
:::

## The four surfaces

| Surface | Module | What it answers |
| ------- | ------ | --------------- |
| Metrics registry | `Kafka.Streams.Metrics` | Throughput, latency, error counts per operator |
| Topology JSON | `Kafka.Streams.Observability.Topology` | The shape of the running graph; live overlay with metrics |
| Orphan-topic detection | `Kafka.Streams.Observability.OrphanTopics` | Internal topics on the broker that don't belong to the current topology |
| Lag tracking | `Kafka.Streams.Runtime.LagInfo` + `LagListener` | Per-task warmup lag for standby promotion decisions |

Plus [interactive queries (IQ)](../glossary/#interactive-query-iq) (`Kafka.Streams.InteractiveQueries`) as a
debugging tool — not strictly observability, but the closest thing
to a "look at the current state" view.

```mermaid
flowchart LR
  subgraph rt[Running streams runtime]
    Eng[Engine]
    Stores[(State stores)]
    Standby[(Standby tasks)]
    Mr["MetricsRegistry\n(counters / gauges / timers)"]
    Topo[Compiled Topology]
  end
  Eng -->|recordCounter| Mr
  Standby -->|warmup lag| LagL[LagListener callback]
  Eng -->|state transitions| StateL[StateListener callback]
  Mr -->|dumpMetrics poll| Push[Push gateway / OTel / Prometheus]
  Topo -->|topologyDescription| GoldenFile[CI golden file]
  Topo -->|liveTopologyDescription| UIOverlay[Web UI overlay]
  Topo -->|detectOrphans + AdminClient.listTopics| Orphan[Orphan-topic log]
  Stores -->|queryEngineStore / queryKVStore| IQ[Interactive Queries\n(your HTTP handler)]
```


## Metrics

`Kafka.Streams.Metrics` is an in-process registry of counters,
gauges, and duration stats. It does not push to Prometheus or
OpenTelemetry; you wire it to whatever observability stack you use
via a periodic `dumpMetrics` poll. The naming scheme mirrors the
Java client:

```
stream-processor-node-metrics:process-total
stream-task-metrics:commit-total
stream-state-metrics:put-rate
```

The runtime pokes counters and gauges at well-known points:

| Metric | Where the runtime records it | What it means |
| ------ | ---------------------------- | ------------- |
| `processTotal` | Per-record entry into a processor node | Throughput per operator |
| `forwardTotal` | Per-record exit from a processor node | Discrepancies vs `processTotal` indicate filtering / branching |
| `commitTotal` | Per commit-cycle completion | Cadence and rate of EOS commits |
| `punctuateTotal` | Per `Punctuator` firing | Confirms scheduled effects run |
| `storePutTotal` / `storeGetTotal` / `storeDeleteTotal` | Per state-store operation | State-access patterns |
| `droppedRecordsTotal` | Per dropped record (filter, async failure with `DropAndContinue`, etc.) | Silent-data-loss signal |

Reading:

```haskell
import qualified Kafka.Streams.Metrics as Met

m <- Met.dumpMetrics registry            -- :: IO (Map Text MetricValue)
counter <- Met.readCounter registry "stream-task-metrics:commit-total"
gauge   <- Met.readGauge   registry "stream-state-metrics:cache-hit-ratio"
hist    <- Met.readDurationStats registry "stream-processor-node-metrics:process-latency"
```

A typical wiring writes a thread that polls `dumpMetrics` every 10 s
and republishes everything to your push gateway with the right tags
(`applicationId`, instance id, host).

### Metric labels you usually want

- `applicationId` — multi-tenant clusters share metric backends.
- `instance` — per-process metrics need a per-instance dimension.
- `taskId` — partition × subtopology, the natural grouping for
  state-store metrics.
- `nodeName` — operator-level metrics group by it.

The registry stores names as bare `Text`. Add your dimensions at
the push-time wrapping layer.

### Metrics that always need an alert

| Metric | Alert when |
| ------ | ---------- |
| `droppedRecordsTotal` | Non-zero rate sustained over the window |
| `commit-failures` | Any value > 0 |
| `commit2PC-fatal` (custom; see [Exactly-once](./exactly-once/)) | Any value > 0 |
| `task-restart-total` | Sustained rate, especially per-task |
| `warmup-lag` for a task that's been promoted candidate | Above `acceptableRecoveryLag` for longer than `probingRebalanceIntervalMs` |

Sustained `droppedRecordsTotal` is the silent killer. The
`logAndContinue` deserialisation handler increments it on every
unparseable record; an upstream schema change can quietly turn
half your input into the bin without affecting any latency
indicator.

## Topology JSON

`Kafka.Streams.Observability.Topology.topologyDescription` emits the
validated `Topology` graph as a versioned JSON document. Use it for
three things:

1. **CI golden-file diff.** Snapshot the JSON; any PR that changes
   the topology shape surfaces the diff.
2. **Web UI overlay.** Pair the JSON with a live metrics snapshot
   via `liveTopologyDescription` to render a Flink-style topology
   view with per-node throughput, lag, and error overlays.
3. **Operator interrogation.** During an incident, dump the JSON
   from a running instance to confirm the topology shape matches
   what you think you deployed.

The schema:

```json
{
  "version": 1,
  "applicationId": "...",       // optional
  "insertionOrder": ["..."],
  "sources":    [{ "id": "...", "topics": [...], "outputs": [...] }],
  "processors": [{ "id": "...", "inputs": [...], "outputs": [...], "stores": [...] }],
  "sinks":      [{ "id": "...", "inputs": [...], "topic": "..." }],
  "stores":     [{ "name": "...", "kind": "keyValue|window|session|raw",
                   "loggingEnabled": true, "changelogTopic": "...",
                   "owners": [...], "global": false }],
  "edges":      [{ "from": "...", "to": "..." }]
}
```

The `version` field exists so callers can gate parsing on a known
shape. The current value is `1`; any backwards-incompatible change
bumps it.

### Live overlay

```haskell
import qualified Kafka.Streams.Observability.Topology as Obs

snapshot <- Obs.liveTopologyDescription topo metricsRegistry cfg
-- snapshot :: Value with a "metrics" object overlaid on the DAG
```

The metrics object is the full `MetricsRegistry` dump; the UI is
responsible for cross-referencing each node's `id` against the
metric keys. The library doesn't impose a scoping convention
because different teams want different per-node aggregation.

### Golden-file test pattern

Capture `topologyDescription` in a golden file and fail CI when it
changes. Diffs touching `sources`, `stores`, or `edges` deserve a
deploy review; processor-only renumbering is harmless only when no
renumbered processor owns state. See
[Topology evolution](./topology-evolution/) for the full pattern.

## Orphan internal topics

`Kafka.Streams.Observability.OrphanTopics.detectOrphans` is a pure
function:

```haskell
detectOrphans
  :: Topology
  -> Text                -- applicationId
  -> [TopicName]         -- broker's full topic list
  -> [OrphanInternalTopic]
```

Wire it at startup:

```haskell
import qualified Kafka.Streams.Observability.OrphanTopics as Orphan

topics <- AdminClient.listTopics admin
forM_ (Orphan.detectOrphans topo appId topics) $ \o ->
  warn ( "orphan internal topic: "
       <> unTopicName (Orphan.orphanTopic o)
       <> " ("
       <> T.pack (show (Orphan.orphanReason o))
       <> ")"
       )
```

The detector excludes:

- Stores with `loggingEnabled = False`.
- Stores with an explicit `loggingSourceTopic` (KIP-295 reuse).
- Stores covered by `topoChangelogPlan` (optimiser-derived
  external-topic reuse).

Anything left over with the framework's `-changelog` or
`-repartition` suffix that isn't in the expected set is reported
as an orphan. The detector does not delete. Treat deletion as a documented
operator action after you have confirmed no live instance still
expects the topic.

### What to do with orphans

Three options, in order of preference:

1. **Leave them.** Disk usage is the only cost, and `log.retention`
   on the changelog topic will cap it at the configured retention
   bytes. The downside is they show up in tooling forever and
   confuse operators.
2. **Delete via the AdminClient after a settlement period.** Wait
   a deploy cycle or two to be confident no instance still expects
   the topic. Then `AdminClient.deleteTopics`.
3. **Archive the messages first** (e.g. consume into S3 with an
   archival consumer), then delete. Belt-and-braces approach for
   teams that consider any audit-trail loss unacceptable.

## Lag tracking

`Kafka.Streams.Runtime.LagInfo` is one row of per-task warmup-lag
information; `publishLag` is the runtime's internal hook for
emitting fresh snapshots; `LagListener` is the user-installed
callback that consumes them.

```haskell
data LagInfo = LagInfo
  { liTaskId       :: !TaskId
  , liStore        :: !StoreName
  , liCurrentOffset :: !Int64
  , liEndOffset     :: !Int64
  } -- (paraphrased; see source)
```

The lag is the difference between the current restored offset and
the end-of-changelog. The runtime publishes it on every standby
catch-up tick, so the listener fires at the consumer poll
cadence.

Two operational uses:

1. **Promotion readiness.** A standby with lag within
   `acceptableRecoveryLag` is a probing-rebalance candidate. If
   lag is consistently high, the standby will never get promoted
   — investigate why replay is slow (network, disk, EOS commit
   overhead).
2. **Rolling-deploy gating.** Don't drain an instance until every
   one of its tasks' standbys is caught up. Otherwise the
   replacement will replay from changelog rather than promote
   metadata-only.

The simplest dashboard is "per-task warmup lag over time" with an
alert when any task is above `acceptableRecoveryLag` for longer
than `probingRebalanceIntervalMs`.

## Interactive queries

`Kafka.Streams.InteractiveQueries` exposes typed read-only handles
to live state stores from outside the stream thread. The use case
is exposing a key-value lookup on top of the materialised state,
e.g. "GET /users/42 hits the local state store directly".

```haskell
ro <- queryKVStore streams "user-store" :: IO (ReadOnlyKeyValueStore Text User)
user <- roKvGet ro "42"
```

For multi-instance deployments, use
`StreamsMetadata` / `KeyQueryMetadata` from `Kafka.Streams.Discovery`
to figure out **which** instance owns the key:

```haskell
peers <- pollGroupMetadata streams
case makeKeyQueryMetadata peers "user-store" partitionForKey of
  Just kq | activeHost kq == myHost -> queryLocal
          | otherwise                -> proxyToHost (activeHost kq)
  Nothing                            -> retryAfterRebalance
```

The proxy step is on you — the library doesn't ship a discovery
HTTP server, only the metadata. Common shape: each instance
exposes an HTTP endpoint that takes `(store, key)`, looks up
locally if it owns the partition, and 307-redirects otherwise.

### IQ as a debugging tool

During an incident, IQ is the closest thing to `SELECT * FROM
state_store WHERE key = ?`. The store iterators are
**eager snapshots** taken at iterator-creation time: they don't
see writes that arrive during iteration. That's the right
behaviour for a debugger; it's also why IQ doesn't substitute for
a proper query layer.

See [Visibility versus ACID databases](./visibility/) for the
semantics of IQ reads under EOS.

## Logs that matter

The runtime emits structured log events at the following points;
forward them to your log aggregator with `taskId` and `nodeName`
tags:

| Event | When |
| ----- | ---- |
| `state-transition: REBALANCING -> RUNNING` | Per state change |
| `task-assigned` / `task-revoked` | Per rebalance step |
| `commit-cycle-aborted` | Per commit-cycle abort with reason |
| `commit-cycle-fatal` | Per commit-cycle fatal — needs an alert |
| `standby-promoted` | Per standby promotion |
| `orphan-topic` | Per orphan detected on startup |
| `processing-exception` | Per record that fell to the exception handler |

`StreamsStatus` (`Kafka.Streams.Runtime.streamsStatus`) reports the
current state. The `setStateListener` hook fires on every
transition so you can mirror state changes into your logs without
polling.

## Tracing

The library does not bundle an OpenTelemetry exporter; the
metrics registry is plain in-memory data. For trace propagation
across topology boundaries:

1. Carry the trace context in record headers (`Record.headers`).
2. Read it in your processor; start a span before doing user
   work; close it when forwarding downstream.
3. For async I/O, the span lives across worker threads — handle
   the context handoff inside the worker function.

Trace context is also useful for cross-topic correlation:
upstream topic → enrichment → downstream topic, all under a single
trace id. The header survives `repartition`, `through`, and `sink
.. to`.

## The minimum viable observability bundle

For a new service, wire metrics polling, a topology golden file, the
orphan-topic detector, `LagListener`, and `setStateListener` before
the first production deploy. Add IQ, live topology overlays, and
tracing once the basics are flowing.

## Related reading

- [Visibility versus ACID databases](./visibility/) — what a
  metric "snapshot" of state actually means.
- [Runbooks](./runbooks/) — the alerts above paired with
  response procedures.
- [Topology evolution](./topology-evolution/) — how the topology
  JSON and orphan detector together give you safe deploys.
