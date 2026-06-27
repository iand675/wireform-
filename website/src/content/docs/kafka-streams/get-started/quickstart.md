---
title: Quickstart
description: Run your first wireform-kafka-streams topology in five minutes. No Kafka broker required.
sidebar:
  order: 1
---

Run your first streaming topology in about five minutes. No Kafka install, no Docker, no network setup. The library includes an in-process test driver that simulates a full Kafka cluster inside your Haskell process.

## What you need

- **GHC 9.6 or newer**: The Glasgow Haskell Compiler. If you have Haskell installed, you likely have this.
- **cabal-install 3.x**: The Haskell build tool, similar to npm or cargo.

If you do not have these, install them via [ghcup](https://www.haskell.org/ghcup/) or your package manager.

## Why a test driver?

Normally, testing a streaming application requires a running Kafka cluster, topic management, and state cleanup between tests. The `TopologyTestDriver` included with this library runs topologies in milliseconds instead of seconds, with no network dependencies, so you can write unit tests that run in CI without infrastructure. The driver behaves like a real Kafka cluster but lives entirely in your process.

## Step 1: Run the word-count demo

The word-count example is the "hello world" of stream processing. It reads lines of text, splits them into words, and counts how many times each word appears. The application remembers counts between records.

Clone the repository and run the example:

```bash
git clone https://github.com/iand675/wireform-.git
cd wireform-
cabal run wireform-kafka-streams-examples -- word-count
```

You will see output like this:

```text
=== WordCountDemo ===
Word-count updates emitted (16):
  all = 1
  streams = 1
  lead = 1
  to = 1
  kafka = 1
  hello = 1
  kafka = 2
  streams = 2
  join = 1
  kafka = 3
  summit = 1
  kafka = 4
  streams = 3
  kafka = 5
  summit = 2
```

Notice how "kafka" appears multiple times with increasing counts. Each line the topology reads updates the running count. The output format is a changelog: every time a word's count changes, the new total is emitted.

## Step 2: Understand the topology

Here is the complete topology that produced that output:

```haskell
import Control.Category ((>>>))
import Kafka.Streams
import qualified Kafka.Streams.Topology.Free as F

-- Topology Void () means: reads from sources (not from other code),
-- writes to sinks (not to other code). See tutorial 2 for full explanation.
wordCountTopology :: F.Topology Void ()
wordCountTopology =
  F.source "streams-plaintext-input" textSerde textSerde
    >>> F.concatMapValues (T.words . T.toLower)
    >>> F.groupBy (\r -> recordValue r) (grouped textSerde textSerde)
    >>> F.count countMat
    >>> F.toStream
    >>> F.sink "streams-wordcount-output" textSerde int64Serde
```

Read this as a data pipeline, left to right:

1. **`source`**: Reads text lines from a topic named "streams-plaintext-input"
2. **`concatMapValues`**: Splits each line into individual words, creating multiple output records per input
3. **`groupBy`**: Reorganizes the stream so all records with the same word go to the same processing unit
4. **`count`**: Maintains a running total for each word (this is where state happens)
5. **`toStream`**: Converts the internal table format back to a stream of updates
6. **`sink`**: Writes the count updates to a topic named "streams-wordcount-output"

`count` maintains state. It remembers previous counts and updates them as new words arrive. This state survives restarts because Kafka Streams automatically persists it to a hidden topic.

## Step 3: Explore other examples

The same executable contains fifteen different demonstrations. Each shows a specific capability:

| Command | What it demonstrates |
|---------|---------------------|
| `pipe` | Simple pass-through between topics |
| `line-split` | Breaking records into multiple outputs |
| `page-views` | Windowed aggregations (counts per time window) |
| `temperature` | Filtering and alerting on thresholds |
| `top-articles` | Finding most popular items in a stream |
| `orders` | Stream-table joins for enrichment |
| `fraud` | Pattern detection across multiple events |
| `fk-join` | Foreign-key joins (join by a field in the value) |
| `iq` | Interactive queries (reading state from outside the topology) |
| `processor` | Low-level processor API access |
| `branching` | Routing records to different outputs |
| `global` | Global tables (replicated to all instances) |
| `cogroup` | Aggregating multiple input streams together |
| `all` | Runs every example in sequence |

Try a few:

```bash
cabal run wireform-kafka-streams-examples -- pipe
cabal run wireform-kafka-streams-examples -- page-views
cabal run wireform-kafka-streams-examples -- iq
```

Each example includes source code in `wireform-kafka/streams/examples/Kafka/Streams/Examples/`.

## Where to go next

Two paths from here:

**Explore more examples**: Run through the demos above, read their source code, and modify them to see what happens.

**Follow the tutorial**: For a structured introduction to streaming concepts, work through the five-part tutorial:

1. **[What is Kafka Streams?](./what-is-kafka-streams/)**: Understand the mental model, what problems streaming solves, and how Kafka Streams fits together
2. **[Your first topology](./your-first-topology/)**: Write a simple pipe topology and understand the test driver
3. **[Stateful processing](./stateful-processing/)**: Learn how state stores work and why they matter
4. **[Joins and tables](./joins-and-tables/)**: Combine multiple streams and tables
5. **[Going to production](./going-to-production/)**: Learn what changes when you deploy to a real Kafka cluster

The tutorial takes about 30 minutes and explains concepts that will help you understand all the examples.

When you are comfortable with the basics, the [Overview](../) page maps all available documentation.
