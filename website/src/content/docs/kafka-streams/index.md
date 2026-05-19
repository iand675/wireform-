---
title: Kafka Streams
description: A library for building stateful, fault-tolerant streaming pipelines in Haskell — Apache Kafka Streams parity plus the Riffle extensions tier.
sidebar:
  order: 1
  label: Overview
---

`wireform-kafka-streams` is a Haskell library for Kafka-backed
streaming apps. You write a topology as ordinary Haskell, run it
inside your service, and let Kafka provide the durable log.

The library mirrors the Apache Kafka Streams DSL and adds optional
Riffle extensions for the cases where classic Streams starts to
feel tight: async I/O, faster state recovery, external commits,
watermarks, and key-group rescaling.

## Where to start

| If you... | Go to |
| --------- | ----- |
| Want it running in 5 minutes | [Quickstart](./get-started/quickstart/) |
| Are new to Kafka Streams | [Tutorial part 1: What is Kafka Streams?](./get-started/what-is-kafka-streams/) |
| Have used JVM Kafka Streams before | Start with [Operations](#operations); skim [Riffle](./riffle/) only if you need the extensions |
| Need to ship to production | [Tutorial part 5: Going to production](./get-started/going-to-production/) |
| Are reading an alert | [Runbooks](./operating/runbooks/) |
| Hit a term you don't know | [Glossary](./glossary/) |

## The tutorial

Five parts, about 30 minutes end-to-end. Each part is
self-contained code you can run against an in-process test
driver — no Kafka broker required.

1. **[What is Kafka Streams?](./get-started/what-is-kafka-streams/)** —
   the mental model, vocabulary, and where this library fits next
   to Flink and a plain consumer.
2. **[Your first topology](./get-started/your-first-topology/)** —
   read from one topic, write to another. The minimum viable
   pipeline.
3. **[Stateful processing](./get-started/stateful-processing/)** —
   count words across a stream and look up the counts via
   interactive queries.
4. **[Joins and tables](./get-started/joins-and-tables/)** —
   enrich a stream of page views against a table of user
   profiles.
5. **[Going to production](./get-started/going-to-production/)** —
   the eight things to set up before deploying for real.

## Riffle: optional extensions

The base layer is the Kafka Streams 4.0 port. Riffle is the set of
extras you reach for when an operational problem needs more than
classic Streams gives you.

| Layer | What |
| ----- | ---- |
| **Parity** | Operator-for-operator port of Apache Kafka Streams 4.0 |
| **Riffle** | Async I/O, snapshot-backed recovery, two-phase commit sinks, coordinated watermarks, and key-group rescaling |

If you never import a Riffle module, nothing changes about the
compiled graph. If one of those problems is yours, read the
[Riffle overview](./riffle/).

## Operations

The operations section is the bulk of the docs. It's reference
material organised by the question you have in front of you:

| You're asking… | Read |
| -------------- | ---- |
| "How do I roll out a new version without breaking state?" | [Topology evolution and rolling deploys](./operating/topology-evolution/) |
| "How do I scale this past my partition count?" | [Scaling and rebalancing](./operating/scaling/) |
| "How do I deploy this in containers without losing state on restart?" | [Running in containers](./operating/containers/) |
| "How do I commit Kafka and Postgres atomically?" | [Exactly-once across Kafka and other systems](./operating/exactly-once/) |
| "What should I be alerting on?" | [Observability](./operating/observability/) |
| "Why doesn't my IQ read return what I just wrote?" | [Visibility versus ACID databases](./operating/visibility/) |
| "It's on fire; what now?" | [Runbooks](./operating/runbooks/) |

## Concepts and guides

| Page | When |
| ---- | ---- |
| [Topology optimization](./concepts/topology-optimization/) | You want to know which rewrites the compiler does automatically and which it doesn't |
| [Dynamic topology changes](./concepts/dynamic-topology/) | You want to know what can change at runtime versus what requires a redeploy |
| [Enrichment via external systems](./guides/enrichment/) | Your topology needs to call out to an HTTP API, a database, or another service |
| [Glossary](./glossary/) | Anything unfamiliar |

## Quick context

A topology is a typed Haskell value that compiles to a runtime graph.
The runtime is a library, not a cluster: scaling out means running
more service processes in the same consumer group. State stores live
next to those processes and recover from Kafka changelog topics.

That's enough to start the [tutorial](./get-started/what-is-kafka-streams/).
