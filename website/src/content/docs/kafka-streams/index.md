---
title: Kafka Streams
description: Build stateful, fault-tolerant streaming pipelines in Haskell. Run them inside your service. No separate cluster required.
sidebar:
  order: 1
  label: Overview
---

`wireform-kafka-streams` is a Haskell library for building streaming applications on Apache Kafka. You write topologies as ordinary Haskell values and run them inside your service. Kafka handles durability and ordering; the library handles state stores, joins, windowing, exactly-once semantics, and rebalancing.

## Start here

**New to Kafka Streams?** [Quickstart](./get-started/quickstart/) → [What is Kafka Streams?](./get-started/what-is-kafka-streams/) → [Your first topology](./get-started/your-first-topology/)

**Have a specific problem?**

- Calling external APIs from my topology → [Enrichment guide](./guides/enrichment/)
- Deploying to production → [Going to production](./get-started/going-to-production/)
- Exactly-once to Postgres/S3/etc → [Exactly-once guide](./operating/exactly-once/)
- Something's on fire → [Runbooks](./operating/runbooks/)

**Coming from Java Kafka Streams?** The API mirrors the Java client. Check the [Riffle extensions](./riffle/) for features the Java client doesn't have.

## The tutorial (30 minutes)

Five self-contained parts. Run against an in-process test driver. No external Kafka broker needed.

1. **[What is Kafka Streams?](./get-started/what-is-kafka-streams/)**: The mental model and vocabulary
2. **[Your first topology](./get-started/your-first-topology/)**: Read from one topic, write to another
3. **[Stateful processing](./get-started/stateful-processing/)**: Count words and query the results
4. **[Joins and tables](./get-started/joins-and-tables/)**: Enrich a stream with reference data
5. **[Going to production](./get-started/going-to-production/)**: Eight things to set up before deploying

## Common tasks

| When you need to… | Read this |
| ----------------- | --------- |
| Call external HTTP/SQL/GRPC APIs | [Enrichment guide](./guides/enrichment/) |
| Scale past your partition count | [Scaling and rebalancing](./operating/scaling/) |
| Deploy in Kubernetes without losing state | [Running in containers](./operating/containers/) |
| Write to Postgres/S3 with exactly-once semantics | [Exactly-once across systems](./operating/exactly-once/) |
| Roll out a new topology version | [Topology evolution](./operating/topology-evolution/) |
| Understand why IQ reads don't match writes | [Visibility versus ACID databases](./operating/visibility/) |
| Set up monitoring and alerts | [Observability](./operating/observability/) |
| Handle an incident | [Runbooks](./operating/runbooks/) |

## Extended features (Riffle)

Optional extensions for advanced use cases. These solve problems that standard Kafka Streams doesn't address.

- **Async I/O**: Call slow external APIs without blocking processing. The operator handles concurrency, timeouts, retries, and exactly-once semantics automatically.

- **Snapshot stores**: Recover large state stores quickly. Instead of replaying hours of changelog to rebuild state, restore from a recent checkpoint.

- **2PC sinks**: Write to Postgres, S3, or HTTP endpoints with exactly-once semantics. Uses two-phase commit to keep Kafka and external systems in sync.

- **Watermark coordinator**: Handle streams with very different data rates. Prevents windows from stalling when one source goes idle.

- **Key-group routing**: Scale your application past your topic's partition count. Useful when you need more parallelism than your input topics provide.

See [Extended features](./riffle/) for details.

## Reference

- [Glossary](./glossary/): Definitions for all terminology
- [Dynamic topology changes](./concepts/dynamic-topology/): What you can change at runtime versus what requires a restart

## How it fits together

- A **topology** is a typed Haskell value (`Topology input output`) composed with `Control.Category.(>>>)`. Like a function `input -> output`, it transforms streams. Common pattern: `Topology Void ()` for topologies that read from and write to Kafka topics.
- The runtime is a **library**, not a cluster. Your service contains the topology; scaling means running more processes in the same consumer group.
- State stores live **next to** your service (local disk or memory), backed by Kafka **changelog topics** for durability and **standby tasks** for fast failover.
