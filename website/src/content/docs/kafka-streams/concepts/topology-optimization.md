---
title: Topology optimization
description: How the compiler optimizes your topology, which rewrites happen automatically, and how to inspect what changed.
sidebar:
  order: 2
---

Your source code describes intent. The compiler rewrites the topology into an efficient execution plan while preserving semantics. This page covers what happens automatically, what you control, and how to verify the result.

## Why topologies get optimized

Write three separate `map` operations:

```haskell
source "input" serde serde
  >>> mapValues fn1
  >>> mapValues fn2
  >>> mapValues fn3
  >>> sink "output" serde serde
```

Running this literally would allocate intermediate records after each map, traverse the stream three times, and add synchronization overhead between steps. The compiler fuses them into a single pass that applies all three functions to each record once. Same result, better performance.

## Two optimization layers

| Layer | What it does | Enabled by default? |
| ----- | ------------- | ------------------- |
| **AST fusion** | Merges adjacent operators, eliminates redundancy | Yes |
| **Graph rewrites** | Changes internal topic layout | No (opt-in) |

AST fusion is always safe. Graph rewrites affect internal topics and require careful rollout.

## AST-level optimizations (always on)

These rewrites reduce node count and eliminate overhead. They run automatically unless you disable them.

### Operator fusion

Combine adjacent operators of the same type.

| Pattern | Becomes | Benefit |
|---------|---------|---------|
| Two `mapValues` | One `mapValues` with composed functions | One pass, not two |
| Two `filter` | One `filter` with AND of predicates | Check once, not twice |
| `mapValues` then `filter` | Fused predicate | No intermediate allocations |

```haskell
-- You write
source >>> mapValues upper >>> filter (not . null) >>> sink

-- Runtime sees
source >>> mapAndFilter (\v -> let u = upper v in (not (null u), u)) >>> sink
```

### Repartition optimization

Remove or reorder shuffles. Repartitioning is expensive: it requires network I/O to Kafka. The compiler eliminates unnecessary repartitions:

| Pattern | Action | Why |
|---------|--------|-----|
| Two repartitions with no key change between | Remove second | Redundant shuffle |
| Repartition before a key-changing op | Remove first | Would be invalidated anyway |
| Pure ops between repartitions | Hoist before first | Run on smaller pre-shuffle data |

### Identity elimination

Remove no-op operations:

```haskell
-- These disappear entirely
mapValues id           -- Does nothing
filter (const True)    -- Keeps everything
through "same-topic"   -- Reads what it just wrote
```

### Auto-insert missing repartitions

Add required shuffles you forgot. Stateful operations (`count`, `aggregate`, `reduce`) need records partitioned by key. If your topology would process mis-partitioned records, the compiler inserts a repartition automatically.

```haskell
-- You write (potentially buggy)
source >>> mapValues extractKey >>> count

-- Compiler inserts
source >>> mapValues extractKey >>> repartition >>> count
```

This prevents silent wrong answers from distributed counting bugs.

## Graph-level optimizations (opt-in)

These changes affect the internal topic layout on your Kafka brokers. They are disabled by default because they require coordination with deployment.

| Rewrite | Effect | When to enable |
|---------|--------|----------------|
| `REUSE_KTABLE_SOURCE_TOPICS` | Skip repartition when source topic already has right key | New topologies with KTable sources |
| `MERGE_REPARTITION_TOPICS` | Combine adjacent repartitions into one | Complex topologies with many shuffles |
| `SINGLE_STORE_SELF_JOIN` | Use one store instead of two for self-joins | Stream-stream self-joins |

They change which internal topics exist. Rolling out a topology with different optimization settings than before can orphan topics or require state migration.

To enable:

```haskell
import qualified Kafka.Streams.Topology.Optimization as Opt

let cfg = Opt.defaultStreamsConfig
      { Opt.topologyOptimization = Opt.OptimizeAll
      }
```

## Inspecting optimizations

The compiler can report what it changed.

### View optimization statistics

```haskell
import qualified Kafka.Streams.Topology.Free.Optimize as Opt

stats <- Opt.optimizationStats myTopology
-- OptimizationStats
--   { osBefore = 15        -- Nodes in original topology
--   , osAfter = 9          -- Nodes after optimization
--   , osRulesFired = [("fuseMaps", 3), ("collapseRepartition", 1)]
--   }
```

Check this when performance surprises you.

### Disable optimizations (for debugging)

```haskell
topo <- Opt.compileNoOptimize myTopology
-- Compile exactly what you wrote, no rewrites
```

Use when you suspect an optimization is buggy or want to compare optimized vs unoptimized behavior.

### Golden-file testing

Pin your topology shape in CI:

```haskell
testTopologyShape :: IO ()
testTopologyShape = do
  optimized <- Opt.optimizeWith Opt.noOptimization myTopology
               >>= F.compileTopology
  golden <- readFile "test/golden/topology.json"
  assertEqual optimized golden
```

This fails CI if your source changes produce a different compiled shape.

## When optimizations don't happen

The compiler deliberately avoids some fusions to preserve semantics.

### Async boundaries

Two async I/O operators stay separate. Each has independent timeout/retry config, its own backpressure buffer, and merging would hide which call is failing. If you want one async worker, write one `asyncMapValues` with composed logic.

### Side-effect ordering

Peek operations stay in place. Side effects (logging, metrics) have observable order, and moving them would change when they fire. That breaks debugging and monitoring.

### Post-async sync work

A sync `mapValues` after an `asyncMapValues` stays separate. The sync work runs on the stream thread anyway, the overhead is just a function call, and keeping it distinct clarifies the async boundary.

## Common issues and fixes

| Symptom | Likely cause | Fix |
|---------|------------- | --- |
| More repartitions than expected | Missing key extractor, forcing auto-insert | Add explicit `groupBy` with correct key |
| Optimization didn't fuse | Operators have different types or side effects | Restructure to group same-type ops |
| Topology shape changed unexpectedly | Auto-generated operator names shifted | Add explicit `Named` annotations |

## Summary

- **AST fusion** runs automatically and is always safe
- **Graph rewrites** are opt-in and change internal topics
- **Inspect** with `optimizationStats` to see what changed
- **Disable** with `compileNoOptimize` for debugging
- **Golden-file** in CI to catch unexpected shape changes

## Related reading

- [Dynamic topology changes](./dynamic-topology/): Understanding what you can change at runtime
- [Observability](../operating/observability/): Monitoring the optimized topology in production
