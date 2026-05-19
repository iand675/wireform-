---
title: "Tutorial 2: Your first topology"
description: Write a topology that copies records from one Kafka topic to another. Run it without a broker.
sidebar:
  order: 3
  label: 2. Your first topology
---

You'll write a topology that reads from one topic and writes to
another. Five lines of meaningful code. We'll run it against the
in-process test driver, which means no broker, no Docker.

## What you'll learn

- How to declare sources and sinks.
- What `Topology Void ()` actually means.
- How to run a topology against the test driver.
- How to feed it records and read what comes out.

## Set up the file

Create a new Haskell file. Anywhere works; the examples directory
in the repo is the easiest starting point:

```bash
cd wireform-/wireform-kafka/streams/examples/Kafka/Streams/Examples
touch MyPipe.hs
```

Paste this in:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Kafka.Streams.Examples.MyPipe (runDemo) where

import Control.Category ((>>>))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void (Void)

import Kafka.Streams
import qualified Kafka.Streams.Topology as Topo
import qualified Kafka.Streams.Topology.Free as F

-- The whole topology: read from one topic, write to another.
pipeTopology :: F.Topology Void ()
pipeTopology =
  F.source "input"  textSerde textSerde
    >>> F.sink   "output" textSerde textSerde

runDemo :: IO ()
runDemo = do
  topo   <- F.buildTopologyFrom pipeTopology
  driver <- newDriver topo "my-pipe-app"

  -- feed a record
  pipeInput driver (topicName "input")
    (Just (BSC.pack "k1"))    -- key
    (BSC.pack "hello world")  -- value
    (Timestamp 0)             -- record timestamp
    0                         -- partition

  -- see what came out
  out <- readOutput driver (topicName "output")
  mapM_ (\cr ->
    putStrLn ("got: " <> show (crKey cr) <> " -> " <> BSC.unpack (crValue cr))
    ) out
  closeDriver driver
```

That's the whole thing. Run it from `ghci`:

```
cabal repl wireform-kafka-streams-examples
ghci> :load Kafka.Streams.Examples.MyPipe
ghci> runDemo
got: Just "k1" -> hello world
```

If you got that output, your topology ran end-to-end. Now let's
look at what each piece is doing.

## Read the topology

The relevant five lines:

```haskell
pipeTopology :: F.Topology Void ()
pipeTopology =
  F.source "input"  textSerde textSerde
    >>> F.sink   "output" textSerde textSerde
```

### `Topology Void ()`

A `Topology i o` is a typed value representing a pipeline that
takes input `i` and produces output `o`.

- `Void` as the input means **there is no upstream operator** —
  the topology is self-contained; sources are inside it.
- `()` as the output means **there is no downstream value** to
  hand to the rest of your program. (A topology that ends in a
  state store would have output `KTable k v`, for example.)

Most topologies are `Topology Void ()` — they read from Kafka and
write to Kafka.

### `source` and `sink`

Sources and sinks are bound to topic names and serde pairs (one
for the key, one for the value):

```haskell
F.source "input"  textSerde textSerde   --     KStream Text Text
F.sink   "output" textSerde textSerde   --     consumes a KStream Text Text
```

A serde (`textSerde`, `int64Serde`, etc.) is a serialiser /
deserialiser pair. The library ships the basics; for richer
formats see [Avro / JSON-Schema / Protobuf serdes](../../../guides/formats/).

### `>>>`

The composition operator. It's plain `Control.Category.(>>>)` —
the same one you'd use to compose two functions in `Control.Arrow`.
Read it as "and then".

```haskell
source "input" ... >>> sink "output" ...
-- "read from input, then write to output"
```

You can chain as many stages as you want. A longer pipeline:

```haskell
F.source "input" textSerde textSerde
  >>> F.mapValues T.toUpper
  >>> F.filter   (\r -> recordValue r /= "")
  >>> F.sink   "output" textSerde textSerde
```

This is the entire DSL idiom: compose stages with `>>>`.

## Drive it without a broker

The interesting part for newcomers: you don't need a real Kafka
broker to develop, test, or learn this library. The
`TopologyTestDriver` runs your topology in-process.

```haskell
topo   <- F.buildTopologyFrom pipeTopology
driver <- newDriver topo "my-pipe-app"
```

`buildTopologyFrom` compiles the typed AST into the runtime
graph. `newDriver` wraps it in a test harness that exposes:

| Function | What it does |
| -------- | ------------ |
| `pipeInput` | Feed one record into a source topic |
| `readOutput` | Drain everything currently in a sink topic |
| `advanceWallClockTime` | Move the test driver's clock forward |
| `closeDriver` | Tear down |

You'll use these in every tutorial part. They're also how the
library's own test suite is structured.

### Feed a record

```haskell
pipeInput driver (topicName "input")
  (Just (BSC.pack "k1"))     -- key (Maybe ByteString)
  (BSC.pack "hello world")   -- value (ByteString)
  (Timestamp 0)              -- event-time timestamp
  0                          -- partition
```

The key, value, timestamp, and partition are exactly what a real
Kafka record carries. The driver behaves as if a producer wrote
this to the broker and the runtime polled it.

### Read what came out

```haskell
out <- readOutput driver (topicName "output")
```

`readOutput` drains every record the topology has written to that
topic since the last call. The result is a list of
`CollectedRecord`, with `crKey` / `crValue` / `crTimestamp` /
`crPartition` accessors.

## Why this matters

The "library + in-process driver" combination is one of the
biggest practical advantages of Kafka Streams over cluster-based
streaming. You can:

- **Unit-test** a topology like any other Haskell function.
- **Iterate locally** without standing up infrastructure.
- **Reproduce production bugs** by writing a small driver script
  that feeds the offending records.

Every example in the repo runs through this driver. You can mix
it freely with `Hspec` or `tasty`:

```haskell
spec :: Spec
spec = describe "myPipe" $ do
  it "uppercases values" $ do
    topo   <- F.buildTopologyFrom myUppercasePipe
    driver <- newDriver topo "test"
    pipeInput driver (topicName "input") Nothing "hello" (Timestamp 0) 0
    out    <- readOutput driver (topicName "output")
    closeDriver driver
    map crValue out `shouldBe` ["HELLO"]
```

## What's happening under the hood

Even without a broker, the test driver runs the **same engine**
the production runtime uses. Specifically:

- Records go through the same `WorkerPool` dispatcher.
- State stores are the same in-memory or RocksDB backends.
- The commit cycle fires on the same cadence (you can step it
  manually with `advanceWallClockTime` for tests that care).

That means a topology that passes its test-driver suite is much
more likely to work against a real broker than one tested only
through mock objects. The driver is a thin wrapper around the
real runtime, not a separate implementation.

## What to keep in mind

A topology is a typed Haskell value, usually `Topology Void ()` for a
self-contained app. Compose stages with `>>>`, then use the
in-process test driver to push input records and read outputs without
starting a broker.

## Next up

You've built a pipe. The next part adds the thing that makes
Kafka Streams actually interesting: **state**.

[Continue to Tutorial 3: Stateful processing →](../stateful-processing/)
