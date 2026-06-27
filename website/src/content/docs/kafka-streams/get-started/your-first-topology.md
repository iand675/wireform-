---
title: "Tutorial 2: Your first topology"
description: Write a topology that copies records from one Kafka topic to another. Run it without setting up a real Kafka cluster.
sidebar:
  order: 3
  label: 2. Your first topology
---

The simplest useful topology: read from one topic, write to another. It shows the data flow without any complex processing.

## The goal

We want to build this:

```mermaid
flowchart LR
  Input[("Topic: input")] --> Process[Process] --> Output[("Topic: output")]
```

Every record that arrives on "input" gets copied to "output".

In the test driver, there's no real Kafka cluster. The "topics" are in-memory queues that behave like Kafka topics.

## The complete code

Create a file at `wireform-kafka/streams/examples/Kafka/Streams/Examples/MyPipe.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Kafka.Streams.Examples.MyPipe (runDemo) where

import Control.Category ((>>>))
import qualified Data.ByteString.Char8 as BSC
import Data.Void (Void)

import Kafka.Streams
import qualified Kafka.Streams.Topology.Free as F

-- Our topology: read from "input", write to "output"
pipeTopology :: F.Topology Void ()
pipeTopology =
  F.source "input"  textSerde textSerde
    >>> F.sink   "output" textSerde textSerde

runDemo :: IO ()
runDemo = do
  -- Build and start the topology
  topo   <- F.buildTopologyFrom pipeTopology
  driver <- newDriver topo "my-pipe-app"

  -- Send a test record
  pipeInput driver (topicName "input")
    (Just (BSC.pack "user-123"))  -- key
    (BSC.pack "Hello, world!")    -- value
    (Timestamp 0)                  -- timestamp
    0                               -- partition

  -- See what came out
  out <- readOutput driver (topicName "output")
  mapM_ (\cr ->
    putStrLn ("Output: " <> show (crKey cr) <> " -> " <> BSC.unpack (crValue cr))
    ) out

  closeDriver driver
```

Run it:

```
cabal repl wireform-kafka-streams-examples
ghci> :load Kafka.Streams.Examples.MyPipe
ghci> runDemo
Output: Just "user-123" -> Hello, world!
```

The record went in one side and came out the other. Here's what each piece does.

## Breaking down the topology

### 1. The type: `Topology Void ()`

```haskell
pipeTopology :: F.Topology Void ()
```

`Topology input output` is the core type. It works like a function `input -> output` that transforms a stream of `input` values into a stream of `output` values. You compose topologies with `>>>` where the output type of one matches the input type of the next:

```haskell
firstTopo  :: Topology Void Text      -- reads from Kafka, produces Text
secondTopo :: Topology Text Int64    -- takes Text, produces Int64
combined   :: Topology Void Int64     -- composed: reads from Kafka, produces Int64
combined = firstTopo >>> secondTopo
```

`Void` is the type with no values. It marks the input because this topology pulls data from a Kafka **source** rather than receiving it from upstream Haskell code. `()` (unit) marks the output because this topology pushes data to a **sink** rather than returning it upstream.

Most topologies read from Kafka and write to Kafka, so `Topology Void ()` is the common shape. Other forms exist:

```haskell
-- A topology that ends in a queryable state store
Topology Void (KTable Key Value)  -- reads from Kafka, produces a table you can query

-- A topology that takes a stream from another topology
Topology (KStream k v) ()          -- takes a stream, writes to Kafka
```

The two type parameters let you compose topologies like functions, with the compiler checking that types match at each connection point.

### 2. Source and sink

```haskell
F.source "input" textSerde textSerde   -- read from topic "input"
  >>> F.sink "output" textSerde textSerde  -- write to topic "output"
```

**Source** pulls records from a Kafka topic. It takes the topic name, a key serde, and a value serde. **Serde** (serializer/deserializer) handles converting between bytes and Haskell values. `textSerde` handles UTF-8 text.

**Sink** writes records to a topic. Same three arguments.

### 3. Composition with `>>>`

The `>>>` operator (from `Control.Category`) chains operators left to right:

```haskell
source >>> sink
-- "read from source, then write to sink"
```

A longer pipeline:

```haskell
F.source "input" textSerde textSerde
  >>> F.filter (\r -> recordValue r /= "")
  >>> F.mapValues T.toUpper
  >>> F.sink "output" textSerde textSerde
```

This reads, filters out empty values, uppercases them, and writes.

## The test driver

You don't need a real Kafka cluster to develop or test. The `TopologyTestDriver` runs your topology in-process, using in-memory "topics" that behave like Kafka topics. Tests run in milliseconds with no external dependencies, and each test starts fresh.

### Key functions

| Function | Purpose |
| ---------- | ------- |
| `buildTopologyFrom` | Compile your topology to a runnable graph |
| `newDriver` | Create a test driver instance |
| `pipeInput` | Inject a record into a source topic |
| `readOutput` | Read records from a sink topic |
| `advanceWallClockTime` | Advance timestamps (for windowed processing) |
| `closeDriver` | Clean up |

### The record we sent

```haskell
pipeInput driver (topicName "input")
  (Just (BSC.pack "user-123"))  -- key: who sent this
  (BSC.pack "Hello, world!")    -- value: the payload
  (Timestamp 0)                  -- when this happened
  0                               -- which partition
```

Every Kafka record has these fields:

- **Key**: Optional identifier (used for partitioning)
- **Value**: The actual data
- **Timestamp**: When the event occurred (or when Kafka received it)
- **Partition**: Which shard of the topic (0 to N-1)

Keys matter because records with the same key go to the same partition, preserving order for related events. Stateful operators use keys to group related records.

## Writing tests

Here's a full test with Hspec:

```haskell
import Test.Hspec

spec :: Spec
spec = describe "Pipe topology" $ do
  it "copies records unchanged" $ do
    topo   <- F.buildTopologyFrom pipeTopology
    driver <- newDriver topo "test"

    -- Send input
    pipeInput driver (topicName "input")
      (Just "key1") "value1" (Timestamp 0) 0

    -- Read output
    out <- readOutput driver (topicName "output")

    closeDriver driver

    -- Assert
    length out `shouldBe` 1
    crKey (head out) `shouldBe` Just "key1"
    crValue (head out) `shouldBe` "value1"
```

A unit test for a streaming topology. Runs in milliseconds, needs no external services.

## Recap

- A **topology** is a typed value describing a processing pipeline
- `Topology Void ()` means "reads from sources, writes to sinks"
- **Source** reads from a topic; **sink** writes to a topic
- **Serde** converts between bytes and Haskell values
- `>>>` composes operators: "do this, then that"
- **Test driver** runs topologies in-process for fast testing
- Every record has a key, value, timestamp, and partition

## Next up

A pipe is useful, but Kafka Streams shines with **stateful** processing: counting, aggregating, joining. The next part introduces state stores.

[Continue to Tutorial 3: Stateful processing →](../stateful-processing/)
