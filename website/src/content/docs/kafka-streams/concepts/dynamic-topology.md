---
title: Dynamic topology changes
description: What you can change about a running KafkaStreams instance, what requires a restart, and what requires a full migration.
sidebar:
  order: 3
---

Some changes to a Kafka Streams application are instant. Others require a restart. Some require a full migration procedure. Kafka Streams ties your topology to persistent state, and different kinds of changes carry different costs.

## Why changes have different requirements

Kafka Streams ties your topology to persistent state:

- **Internal topics** (changelog, repartition) are created based on your topology shape
- **State stores** are written to local disk with specific formats
- **Consumer group membership** determines who processes which partitions
- **Task assignments** pin specific work to specific instances

Changes that affect these require care. The framework enforces safety by making expensive changes explicit.

## The four tiers of change

| Tier | What you can change | How to change it | Examples |
| ---- | ------------------- | ---------------- | ---------- |
| **Hot** | Runtime configuration | Function call on running instance | Worker threads, pause/resume, listeners |
| **Warm** | Group membership | Add/remove instances | Scaling horizontally, rolling deploys |
| **Restart** | Startup configuration | Restart the process | Processing guarantee, dispatch mode, most config |
| **Migration** | Topology fundamentals | Follow a procedure | Change topology shape, application ID, serde |

## Hot tier: change without stopping

These changes take effect immediately without disturbing processing.

### Adjust worker thread count

Add or remove processing threads within your application:

```haskell
import qualified Kafka.Streams.Runtime as R

R.addStreamThread streams     -- Add a worker
R.removeStreamThread streams  -- Remove a worker
```

Workers share the same consumer connection. Adding one just reshuffles partition assignments within the process. No broker coordination needed.

Use this when your CPU usage is low and you want more parallelism, or when you want to reduce threads during quiet periods.

### Pause and resume processing

Temporarily stop processing without leaving the consumer group:

```haskell
R.pauseKafkaStreams streams   -- Stop processing
R.resumeKafkaStreams streams  -- Resume processing
```

The consumer keeps heartbeating, so the broker knows the instance is alive, but no records are polled or processed.

Use when you need exclusive access to state stores for maintenance, when you're coordinating with an external batch job, or when you want to pause during a dependency outage without triggering rebalances.

### Register event listeners

Add or replace listeners for observability:

```haskell
-- State transitions (RUNNING, REBALANCING, ERROR)
R.setStateListener streams $ \old new ->
  publishLog ("State: " <> show old <> " -> " <> show new)

-- Rebalance events (assignments, revocations)
R.setRebalanceListener streams $ \event ->
  case event of
    Assigned partitions -> warmCaches partitions
    Revoked partitions  -> flushBuffers partitions
```

Listeners observe; they don't change processing. Replacing a listener just changes who gets notified.

Use when you need to add monitoring, change alert destinations, or adjust debug logging.

## Warm tier: change with one rebalance

These changes require coordination with the consumer group but don't interrupt processing for long.

### Scale by adding instances

Start a new process with the same `applicationId`:

```haskell
-- In a new terminal or pod
my-streams-app  -- same config, same applicationId
```

The new instance joins the consumer group, the group coordinator computes new partition assignments, existing instances release some partitions (incremental rebalance), the new instance picks up its share, and processing resumes. Typically takes seconds; the new instance must replay changelog tails for assigned state stores.

### Scale by removing instances

Stop a process cleanly:

```haskell
R.closeKafkaStreams streams  -- Clean shutdown
```

The instance leaves the consumer group, remaining instances get reassigned its partitions, and if standbys exist they become active (fast); otherwise replay from changelog (slower). The group must rebalance to redistribute work, which is why this is warm, not hot.

## Restart tier: change configuration

These settings are read once at startup and baked into the runtime:

| Setting | Why it requires restart |
| ------- | ----------------------- |
| `processingGuarantee` | Determines whether producer is transactional |
| `dispatchMode` | Worker pool routing function baked in |
| `numStandbyReplicas` | Standby state machine initialized at startup |
| `commitIntervalMs` | Commit cycle scheduler configured at start |
| `stateDir` | Local paths opened at initialization |

To change: update configuration, drain the instance (stop processing cleanly), restart with new config, rejoin the consumer group.

For rolling restarts, update one instance at a time and wait for it to reach RUNNING state before proceeding. With standbys configured, each restart is a metadata-only promotion.

## Migration tier: change fundamentals

These changes affect data layout or identity and require explicit procedures.

### Change topology shape

Adding, removing, or renaming operators affects internal topics and state stores. Your old changelog topic contains state in the old format. The new topology expects a different format or different stores entirely.

Procedure:

1. Review the change in [topology evolution](../operating/topology-evolution/)
2. Run golden-file diff in CI to see what changes
3. For stateful operator changes: drain, deploy, accept warmup cost
4. For renames: old topics become orphans, clean up after verification

### Change application ID

The `applicationId` is your consumer group identity and internal topic prefix. Changing it means a new consumer group (starts from scratch, no offsets), new internal topics created (old ones become orphans), and state rebuilt from upstream (queries return empty until catch-up).

Treat it as a fresh-start deployment. Run old and new in parallel during transition if possible. Clean up old internal topics after confirming the new app is stable.

### Change state store serde

Changing how state is serialized affects changelog compatibility.

- **Schema-compatible:** Both old and new can read each other's data. Deploy normally.
- **Schema-incompatible:** Use `SchemaVersioned` store to migrate reads forward. Or double-write and cut over.

See [topology evolution](../operating/topology-evolution/#changing-serde) for the full procedure.

## What you cannot change (and why)

Some changes aren't supported because they would undermine correctness.

### Truly dynamic topology

You cannot add a new processing branch to a running topology without restarting. New nodes need state stores created atomically across the group, the rebalance protocol would need to version topology shapes, and optimizations assume static topology for correctness.

Alternatives: use `TopicNameExtractor` for dynamic sink topics (one topology, many destinations), use `Branched.withConsumer` for runtime-registered handlers (within fixed topology), or deploy a new topology version as a separate application.

### Change key-group count live

You cannot scale `kgcTotal` without restart. Key-group routing is baked into state store sharding, and changing it requires re-partitioning state.

Alternative: drain, wipe local state, restart with new count, warm from changelog.

## Summary table

| Change | Tier | Downtime | Procedure |
| ------ | ---- | ---------- | --------- |
| Add worker threads | Hot | None | Function call |
| Pause/resume | Hot | None | Function call |
| Add listener | Hot | None | Function call |
| Add instance | Warm | Seconds | Start new process |
| Remove instance | Warm | Seconds | `closeKafkaStreams` |
| Change processing guarantee | Restart | Minutes | Rolling restart |
| Change commit interval | Restart | Minutes | Rolling restart |
| Rename operator | Migration | Minutes | Drain + deploy |
| Change serde | Migration | Minutes | Schema migration or double-write |
| Change application ID | Migration | Hours | Fresh deploy, parallel run |

## Related reading

- [Topology evolution](../operating/topology-evolution/): Detailed procedures for migration-tier changes
- [Scaling](../operating/scaling/): Details on warm-tier scaling
- [Running in containers](../operating/containers/): Configuring for Kubernetes and similar environments
