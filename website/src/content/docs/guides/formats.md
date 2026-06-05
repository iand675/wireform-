---
title: Format catalogue
description: Every format, protocol, and capability in the wireform ecosystem.
sidebar:
  order: 2
---

:::caution[Not yet on Hackage]
wireform is in heavy development and has not been published to Hackage yet.
APIs may change. To use it today, clone the repo and add it as a path
dependency.
:::

wireform currently ships 34 packages covering serialization, codegen,
streaming, DOM processing, messaging, and analytics. This page lists
every format with its capabilities and the modules to reach for.

## Schema & IDL formats

These formats define types in a schema language and generate Haskell code.

| Format | Package | Codegen | RPC / Framing |
|--------|---------|---------|---------------|
| Protocol Buffers | `wireform-proto` | `loadProto` TH / `wireform-gen proto` / `protoc` plugin | `Proto.GRPC` framing; full gRPC via `wireform-grpc` |
| Avro | `wireform-avro` | `wireform-gen avro` | Avro IPC (`Avro.Protocol`), container files (`Avro.Container`) |
| Thrift | `wireform-thrift` | `wireform-gen thrift` | Binary + Compact protocol (`Thrift.Message`) |
| Bond | `wireform-bond` | `wireform-gen bond` | — |
| Cap'n Proto | `wireform-capnproto` | `wireform-gen capnp` | — |
| FlatBuffers | `wireform-flatbuffers` | `wireform-gen fbs` | — |
| ASN.1 | `wireform-asn1` | `wireform-gen asn1` | — |

## Binary value formats

Schema-less formats with `Generic` deriving. Each package exposes a `Value`
ADT for dynamic messages.

| Format | Package | Class module | Extras |
|--------|---------|-------------|--------|
| MessagePack | `wireform-msgpack` | `MsgPack.Class` | Streaming (`MsgPack.Stream`), RPC (`MsgPack.RPC`), JSON bridge |
| CBOR | `wireform-cbor` | `CBOR.Class` | Streaming (`CBOR.Stream`), CDDL schema + codegen, diagnostics, JSON bridge |
| BSON | `wireform-bson` | `BSON.Class` | MongoDB wire format |
| Ion | `wireform-ion` | `Ion.Class` | ISL schema + codegen |
| EDN | `wireform-edn` | `EDN.Class` | Clojure/Datomic format |
| Bencode | `wireform-bencode` | `Bencode.Class` | BitTorrent serialization |
| Fory | `wireform-fory` | — | Apache Fory xlang format (interop-tested against pyfory) |

## Text & markup formats

| Format | Package | Class module | Extras |
|--------|---------|-------------|--------|
| XML | `wireform-xml` | `XML.Class` | SAX (`XML.SAX`), zero-copy DOM (`XML.FastDOM`), XPath (`XML.Path`), XSLT 1.0 (`XML.XSLT`), XSD codegen, concurrent parsing (`XML.Incremental`) |
| HTML5 | `wireform-html` | `HTML.Class` | Spec-compliant tree builder, CSS selectors (`HTML.Selector`), streaming rewriter (`HTML.Rewriter`) |
| YAML | `wireform-yaml` | `YAML.Class` | YAML 1.2, 100% yaml-test-suite conformance, annotated AST, JSON bridge |
| TOML | `wireform-toml` | `TOML.Class` | TOML 1.0/1.1, toml-test conformant |
| EDI | `wireform-edi` | `EDI.Class` | UN/EDIFACT + X12 segment streams, GS1/TRADACOMS/ODETTE/VDA/HL7/HIPAA/IATA/NCPDP/EDIGAS profiles, queries, validation, 997 acknowledgements, TH-derived segment records |
| CSV | `wireform-csv` | `CSV.Class` | CSV / TSV / pipe-separated |
| NDJSON | `wireform-ndjson` | `NDJSON.Class` | Newline-delimited JSON, incremental decoder |

## Analytics & lake formats

| Format | Package | Reader | Writer | Extras |
|--------|---------|:------:|:------:|--------|
| Parquet | `wireform-parquet` | Full | Full | All encodings, snappy/zstd/lz4/brotli compression, bloom filters, page index, encryption, predicate pushdown, Arrow bridge |
| Arrow IPC | `wireform-arrow` | Full | Full | Schema framing, record batch materialization, zstd/lz4 compression |
| ORC | `wireform-orc` | Full | Partial | Integer RLE v1/v2, snappy/zstd/lz4 compression, predicate evaluator |
| Iceberg | `wireform-iceberg` | Full | — | Schema evolution, partition transforms, deletion vectors, Puffin statistics, catalog clients (REST, Glue, Hadoop, SQL) |
| Delta Lake | `wireform-delta` | Partial | — | Transaction log, checkpoints, time travel |
| Hudi | `wireform-hudi` | Partial | — | Timeline reader, copy-on-write |
| Lance | `wireform-lance` | Partial | — | Data files, manifests, dataset versions |

## Messaging & RPC

| Protocol | Package | Capabilities |
|----------|---------|--------------|
| gRPC | `wireform-grpc` | Client/server, unary + streaming (server, client, bidi), HTTP/2, TLS |
| Kafka | `wireform-kafka` | Producer, consumer, exactly-once transactions, Kafka Streams DSL (KStream, KTable, windowed aggregations, joins), SASL auth (PLAIN, SCRAM, OAUTHBEARER, MSK IAM), snappy/zstd/lz4/gzip compression, OpenTelemetry instrumentation |

## Shared infrastructure

These packages are used internally by the format packages. You generally
don't import them directly, but they're good to know about.

| Package | What it does |
|---------|--------------|
| `wireform-core` | FFI/SIMD primitives shared across formats — packed varint decode, UTF-8 validation, byte/whitespace/JSON scanning |
| `wireform-derive` | Annotation vocabulary for Generic deriving. One set of annotations (`rename`, `tag`, `skip`, `defaults`, `flatten`, etc.) works across all 25+ backends. |
| `wireform-columnar` | Shared columnar primitives — pull-based iterators, predicate pushdown, mmap-aware file loading, SIMD bit-unpacking |

## Conformance

Several packages are tested against upstream conformance suites:

| Package | Suite | Status |
|---------|-------|--------|
| `wireform-proto` | Official protobuf conformance | 2,675 / 2,675 passing |
| `wireform-html` | html5lib tree builder tests | 1,779 / 1,779 passing |
| `wireform-yaml` | yaml-test-suite | 100% passing |
| `wireform-toml` | toml-test | Passing |
| `wireform-fory` | pyfory interop | 45 / 45 passing |
| `wireform-parquet` | pyarrow / DuckDB interop | Passing |
| `wireform-iceberg` | pyiceberg / fastavro interop | Passing |
