# Changelog for `wireform-kafka`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

- **Optional io_uring backend** (`Kafka.Network.IoUring`),
  gated behind the new `io-uring` cabal flag. Replaces the
  `recv`/`send` syscall pair the GHC RTS otherwise issues
  (via its epoll-based IO manager) with a single
  `io_uring_enter` per round-trip, removing one syscall per
  small request. Self-contained: the C shim talks to the
  kernel via raw syscalls (`SYS_io_uring_setup`,
  `SYS_io_uring_enter`) and mmaps the SQ/CQ rings directly, so
  there is no `liburing` build-time dependency. On non-Linux
  hosts a stub C file is compiled instead so the FFI symbols
  remain linkable. `isIoUringSupported` runtime-detects the
  kernel's verdict (Linux \< 5.1, `kernel.io_uring_disabled=2`,
  seccomp denials, etc., all return `False`). Wraps a
  connected `Socket` as a
  `Kafka.Network.Transport.Transport` so the rest of the test
  / future-pipeline surface plugs in unchanged.

### Changed

- Vendored Apache Kafka **4.0.0** protocol JSON schemas
  (`data/kafka-protocol-schemas/`) and regenerated every
  `Kafka.Protocol.Generated.*` module. Removes 4.1+-only message
  types (`AlterShareGroupOffsets*`, `DeleteShareGroupOffsets*`,
  `DescribeShareGroupOffsets*`, `StreamsGroupHeartbeat*`,
  `StreamsGroupDescribe*`) and renames `ListConfigResources*` back
  to `ListClientMetricsResources*` (its 4.0.0 spelling — same
  apiKey 74).
- `Kafka.Protocol.Codegen.Types.FieldSpec` parses `tag` from either
  a JSON number (post-4.0 trunk style) or a numeric string (the
  4.0.0 release ships `"tag": "0"`), so the codegen is tolerant of
  either upstream encoding.
- High-level client call sites that hand-built request records lost
  the post-4.0 fields they no longer need to populate
  (`apiVersionsRequestClusterId`/`NodeId` (KIP-1242),
  `topicProduceDataTopicId`/`offsetCommitRequestTopicTopicId`/
  `offsetFetchRequestTopicsTopicId`/`txnOffsetCommitRequestTopicTopicId`
  (KIP-516 / KIP-848 v6/v10 topic-id additions),
  `fetchPartitionHighWatermark` (KIP-405),
  `initProducerIdRequestEnable2Pc`/`KeepPreparedTxn` (KIP-939),
  `txnOffsetCommitRequestGenerationIdOrMemberEpoch` rename).
- Integration `docker-compose.yml` and the bench / tutorial docs
  bumped from `apache/kafka:3.7.0` / `kafka_2.13-3.9.2` references
  to `apache/kafka:4.0.0` / `kafka_2.13-4.0.0`.
