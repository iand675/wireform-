# Changelog

## Unreleased

* **`wireform-lattice`** — an implementation of Lattice, a cache-native
  graph query protocol (GraphQL/JSON:API problem space; spec Draft 27 +
  GraphQL comparison corpus in the docs site under `/lattice/`): schema
  IDL parser + canonical content-addressed IDL, the normative query
  grammar + canonicalization + BLAKE3 query hashes, the authorization
  path-join planner (`pub`/`ctx`/`priv` slices, plan ids over pertinent
  declarations, budgets, `explain`), the NDJSON entity-stream wire
  format with scoped errors, a full HTTP origin on `wireform-http`
  (transport ladder incl. the RFC 10008 `QUERY` introduction, point
  fetches with masks + version pinning, mutations with at-most-once
  idempotency keys and write-set-derived invalidation, per-slice cache
  headers + `Surrogate-Key` tags), an HTTP client with a normalized
  entity store, an STM in-memory backend, and the Star Wars corpus demo
  origin (`example-lattice`). Proven behind real CDN tiers from the dev
  shell: a Varnish (vmod_xkey) VCL harness and a plug-and-play
  Cloudflare Worker (KV surrogate-key index), both driven by one
  conformance checker (request coalescing, tag purging, slice
  isolation, idempotent replay through the cache). Companion
  TypeScript client `lattice-ts/` (zero runtime deps, React bindings,
  merged multi-root query batching). Later amendments (all Draft 27):
  asynchronous/out-of-band invalidation pinned in spec §11.5 with a
  TLA+ model of the pipeline (`wireform-lattice/tla/`, `tlc` in the
  dev shell; the intent-time-purge stale-refill race is a checked
  counterexample); co-keyed entities (§3.8) — `entity P joins B`
  (1:1 identifying join, independent invalidation) vs `entity R
  refines B` (subclass view: shared ver, family-wide surrogate-key
  fan-out and tombstone cascade), with identity edges (`has one?
  profile: UserProfile by id`) planning as ordinary to-ones; and
  cardinality declarations (§3.4–3.6) — `has one` promises resolution
  (Edge-scoped `lattice:cardinality` on a dangle) with `has one?` as
  the declared maybe, bounded collections take a `min N` floor
  (`lattice:collection-underflow`), and the type language gains the
  nonempty list `[t]+` shared by fields, mutation inputs, and
  variables.

* **`wireform-lattice` deferred features complete** (spec Draft 27
  amendments pinned as implemented): derived fields (§3.7: `on read`
  hidden-traversal planning, `maintained` outbox-relay materialization,
  witness validators, information-flow checking with `@declassify`); cache
  digests (§10.4: `X-Have` + a bit-level-pinned Golomb-coded set,
  `unchanged` markers, client gap repair); NFC canonicalization (§5.1) in
  both language canonicalizers; verb bindings (§11.7-11.8: `as
  PUT/PATCH/DELETE/POST`, merge-patch, `If-Match`/`428` conditional
  machinery, bound batches); origin coalescing (§6.9: per-type accumulation
  windows, single-flight, `coalesceWindowMs` discovery); signed admission
  (§14.3: Ed25519 over canonical text at memo-miss compile); live queries
  (§12: SSE subscriptions with single-flight deltas and reauth, plus a
  client `subscribeQuery`); observability (§19: the span topology, the ten
  named instruments, explain's span skeleton diffable against live traces);
  the compatibility registry (§17: four-axis change taxonomy, transitive
  check windows, `@break`/`@deprecated`, corpus-weighted `POST
  /schema/check`); the `nodes` root (§14.4); schema modules (§18.1: `extend
  entity`, order-insensitive fusion, fused in-process backends); the
  federation gateway (§18.3-18.8: per-upstream `nodes` subplans over
  ordinary cacheable GETs, `src`-tagged streams, namespaced snapshots and
  surrogate keys, claim re-minting, feed-driven purges) and the
  `/invalidations` feed (§18.6). Also: the repo-wide `blake3` build is
  pinned portable (`constraints: blake3 source` + `BLAKE3_USE_NEON=0`),
  fixing a null NEON dispatch that segfaulted any >1 KiB hash on aarch64
  whenever the nix global unit was selected.

## 0.1.0.0 -- 2026

Initial release of the `wireform` umbrella package and its per-format
siblings.

### Formats

`Wireform.*` facade modules ship for every format the workspace covers:

* **Schema / IDL binary**: Protocol Buffers, Apache Thrift, Apache Avro,
  Apache Bond, FlatBuffers, Cap'n Proto, ASN.1.
* **Schema-less binary**: CBOR (RFC 8949), MessagePack, BSON, Amazon Ion,
  Apache Fory (Fury), Bencode.
* **Text**: JSON (via NDJSON), EDN, TOML, YAML, CSV, XML, HTML5.
* **Columnar / table**: Apache Arrow IPC, Apache Parquet, Apache ORC,
  Apache Iceberg, Delta Lake, Apache Hudi, Apache Lance.
* **Streaming / RPC**: gRPC framing (`wireform-grpc`), Apache Kafka
  protocol + native client (`wireform-kafka`).

### Highlights

* **Annotation-driven deriver** (`wireform-derive`) -- one `{-# ANN ... #-}`
  vocabulary drives instance generation for every backend.  Per-format
  derivers live in their respective `wireform-*` packages.
* **High-performance hot paths**:
    * Unboxed-sum decoder result types (no boxed `Either` / `Maybe` on
      the decode loop).
    * Two-pass sized encoders that allocate exactly the right buffer
      and write tag + length + payload in a single pass.
    * SWAR / SIMD C kernels in `wireform-core` (`Wireform.FFI`) for
      UTF-8 validation, packed-varint pre-scan, byte / NUL / JSON
      escape scanning, and Iceberg partition-bound comparison.
    * Direct-write `Wireform.Encode.Direct` buffer for the columnar
      packages.
* **IDL parsers and code generators** for `.proto`, `.avsc` / `.avdl`,
  `.thrift`, `.bond`, `.capnp`, `.fbs`, ASN.1, ISL, CDDL, XSD, Iceberg
  table metadata.
* **Streaming / incremental** decoders for protobuf, MsgPack, CBOR,
  XML, NDJSON.
* **Container file I/O** for Avro OCF, Parquet (footer + page index +
  bloom filter + column chunks), Arrow IPC (file + stream), ORC
  (stripes + statistics), Iceberg (manifests + table metadata), Delta
  Lake transaction log, Hudi timeline, Lance.
* **Schema resolution / evolution** for Avro and proto2 / proto3
  compatibility.
* **Dynamic / untyped** protobuf messages, `.pbtxt` text format, CBOR
  diagnostic notation (RFC 8949), and a runtime `MessageRegistry`.
* **Multi-format codegen CLI** (`wireform-gen`) that targets every
  IDL-backed format from a single binary.
* **Protobuf conformance** harness driving the upstream
  `conformance_test_runner` end-to-end: 2675 successes, 0 unexpected
  failures against `protocolbuffers/protobuf@v28.2`.

### Toolchain

* CI: GHC 9.6.4 and GHC 9.8.4.
* `cabal-version: 3.0` across the workspace.
* LLVM is OFF workspace-wide (`-fasm` set in `cabal.project`); a
  vanilla GHC toolchain is sufficient.
