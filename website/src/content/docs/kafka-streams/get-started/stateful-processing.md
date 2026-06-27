---
title: "Tutorial 3: Stateful processing"
description: Count words across a stream and query the running totals. Learn how state stores work and why they matter.
sidebar:
  order: 4
  label: 3. Stateful processing
---

Counting how many times each word appears in a stream of text requires *state*: the running totals must persist between records. Kafka Streams handles this with **state stores**.

## The problem

A stream of log lines where you want to count error occurrences:

```
Input:  [ERROR] Connection timeout
        [WARN] Retrying...
        [ERROR] Connection timeout
        [ERROR] Database unavailable
```

You need to track "connection timeout" → 2, "database unavailable" → 1.

The naive approach in a plain consumer:

```haskell
-- DON'T DO THIS
errorCounts :: IORef (Map Text Int)  -- shared mutable state

process record = do
  counts <- readIORef errorCounts
  let word = extractError record
  let newCounts = adjust (+1) word counts
  writeIORef errorCounts newCounts
```

This fails in production because if your process restarts the map is empty, if you scale to multiple instances each has its own map, and if an instance dies mid-batch some increments are lost. Kafka Streams solves all three.

## The solution

Here's the complete word-count topology:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Kafka.Streams.Examples.WordCount (runDemo) where

import Control.Category ((>>>))
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void (Void)

import Kafka.Streams
import qualified Kafka.Streams.Materialized as Mat
import qualified Kafka.Streams.Topology.Free as F

-- Topology Void (): reads from sources (not upstream code), writes to sinks (not downstream code).
-- Void = pulls from Kafka. () = pushes to Kafka. See tutorial 2 for why two type parameters.
wordCountTopology :: F.Topology Void ()
wordCountTopology =
  F.source "lines" textSerde textSerde           -- 1. Read text lines
    >>> F.concatMapValues (T.words . T.toLower)  -- 2. Split into words
    >>> F.groupBy (\r -> recordValue r)          -- 3. Group by word
          (grouped textSerde textSerde)
    >>> F.count countMat                         -- 4. Count per word
    >>> F.toStream                               -- 5. Convert back to stream
    >>> F.sink "counts" textSerde int64Serde    -- 6. Write counts
  where
    countMat :: Materialized Text Int64
    countMat =
      Mat.withValueSerde int64Serde
        $ Mat.withKeySerde textSerde
        $ Mat.materializedAs (storeName "counts-store")

runDemo :: IO ()
runDemo = do
  topo   <- F.buildTopologyFrom wordCountTopology
  driver <- newDriver topo "word-count-app"

  -- Send three lines
  mapM_ (\line ->
    pipeInput driver (topicName "lines")
      Nothing
      (BSC.pack (T.unpack line))
      (Timestamp 0) 0)
    [ "hello world"
    , "hello kafka streams"
    , "kafka summit kafka"
    ]

  -- Read the changelog output
  out <- readOutput driver (topicName "counts")
  mapM_ (\cr ->
    let word = maybe "?" BSC.unpack (crKey cr)
        n    = either (const (-1)) id
                 (deserialize int64Serde (crValue cr) :: Either Text Int64)
    in putStrLn (word <> " = " <> show n)
    ) out

  closeDriver driver
```

Run it:

```
ghci> runDemo
hello = 1
world = 1
hello = 2
kafka = 1
streams = 1
kafka = 2
summit = 1
kafka = 3
```

## What just happened

Tracing through the topology:

1. Three lines arrive as input.
2. Each line splits into multiple words.
3. Same words are routed to the same processing unit.
4. Running totals are maintained.
5. Every count change is emitted.

The output is a **changelog**. "hello" appears as `1` then `2` because the count updated. "kafka" ends at `3` because it appeared three times total.

## The operators explained

### `concatMapValues`: One input, many outputs

```haskell
F.concatMapValues (T.words . T.toLower)
```

Most operators produce one output per input. `concatMapValues` produces *multiple*.

Input: `"hello world"`
Output: `["hello", "world"]` (two separate records)

Same semantics as `concatMap` on lists: `concatMap words ["a b", "c d"] = ["a", "b", "c", "d"]`.

### `groupBy`: Routing by key

```haskell
F.groupBy (\r -> recordValue r) (grouped textSerde textSerde)
```

Before counting, all records with the same word must go to the same processing unit. If two workers each see "hello" once, both think the count is 1. The real count is 2.

`groupBy` solves this by re-keying the stream. Behind the scenes records are written to an internal **repartition topic**, re-consumed partitioned by the new key, and now all "hello" records land on the same worker. The cost is a network round-trip to Kafka. The benefit is correct counts.

### `count`: Stateful aggregation

```haskell
F.count countMat
```

This maintains a running count per key. It needs a **state store** to remember counts between records.

The `Materialized` configuration specifies store keys as `Text` (the words), store values as `Int64` (the counts), and names the store "counts-store" (for querying later).

### `toStream`: KTable to KStream

`count` produces a **KTable**: a changelog stream where later values replace earlier ones for the same key. `toStream` converts it back to a regular **KStream** for output.

## KStream vs KTable

|  | KStream | KTable |
|--|---------|--------|
| **Analogy** | Event log | Database table |
| **Same key twice** | Both kept (two events) | Second replaces first |
| **Deletion** | Tombstone record | Value set to null |
| **Use for** | Time-series, raw events | Current state, aggregates |

Example with the same input:

```
Input: (alice, 100), (bob, 50), (alice, 150)

KStream view: Three independent events
  → (alice, 100), (bob, 50), (alice, 150)

KTable view: Latest value per key
  → alice = 150, bob = 50
```

Our word count uses both: a **KStream** of words going in (each word is an event), a **KTable** of counts (latest count per word), and a **KStream** of count updates going out.

## How state stores work

The state store is a local key-value structure (in-memory or RocksDB). It's **per-task**: each processing unit has its own store for the keys it owns.

Durability comes from three mechanisms. Every state change is written to a hidden Kafka **changelog topic**. On restart, the changelog is replayed to rebuild state. Other instances keep **standby replicas** for fast failover.

Your process can restart without losing counts, you can scale out and counts stay correct, and if an instance dies another takes over quickly.

## Querying state from outside

The state store isn't just for the topology. You can read it directly:

```haskell
import qualified Kafka.Streams.InteractiveQueries as IQ

-- After feeding records, before closeDriver:
ro <- IQ.queryEngineStore @Text @Int64
        (driverEngine driver)
        (storeName "counts-store")

case ro of
  Nothing  -> putStrLn "Store not found"
  Just kvs -> do
    hello <- IQ.roKvGet kvs "hello"
    kafka <- IQ.roKvGet kvs "kafka"
    putStrLn ("hello count: " <> show hello)  -- Just 2
    putStrLn ("kafka count: " <> show kafka)  -- Just 3
```

This is **Interactive Queries** (IQ). Use it to build HTTP endpoints that return current counts, debug your topology in production, or monitor processing health.

IQ reads the local store, which may be slightly ahead of what's committed to Kafka. For strongly consistent reads, query after a commit cycle completes.

## Recap

Stateful processing requires remembering data between records. State stores provide this, backed by changelog topics for durability. `groupBy` ensures related records go to the same processing unit. KStream is an event log (append-only); KTable is a table (latest wins). Interactive Queries let you read state stores from outside the topology. The library handles recovery, replication, and failover automatically.

## Next up

Stateful processing on one stream is half the job. The other half is **combining** streams: joining events with reference data.

[Continue to Tutorial 4: Joins and tables →](../joins-and-tables/)
