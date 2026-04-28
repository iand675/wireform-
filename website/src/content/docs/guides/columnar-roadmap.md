---
title: Columnar & lake-format roadmap
description: Progress toward complete, spec-faithful support for Parquet, Arrow, ORC, and Iceberg.
sidebar:
  order: 3
---

This document tracks progress toward **complete, spec-faithful** support for the
columnar and table-metadata formats exposed in wireform. "Complete" is staged:
each format has **reader** and (where applicable) **writer** milestones, with
optional spec extensions (encryption, advanced statistics, etc.) in late tiers.

## Shared principles

- **Tests**: golden vectors vs reference implementations (arrow-rs / pyarrow /
  parquet-cpp) where feasible; property tests for round-trips on writers.
- **Performance**: keep hot paths allocation-tight (see the wireform columnar
  guidelines in `agents.md`).
- **Dependencies**: optional compression codecs via Cabal flags (`snappy`,
  `zstd`, future `lz4`).

## Phase A — Parquet

### Page index + bloom filter (A.7) details

- `Parquet.PageIndex` round-trips `OffsetIndex` (page locations + optional
  unencoded byte-array sizes) and `ColumnIndex` (null pages, min/max,
  `BoundaryOrder`, optional null-counts and rep/def histograms) through
  the same Thrift Compact codec used by `Parquet.Footer`.
- `Parquet.BloomFilter` implements parquet-format 2.10 split-block bloom
  filters (BLOCK + XXHASH + UNCOMPRESSED). The bitset is an unboxed
  `Vector Word64` and the inner kernels do not allocate per insert/check.
- `Parquet.XXH64` is a from-scratch xxHash 0.1.1 implementation, byte-exact
  against `xxhsum -H1`.
- `Parquet.Types.ColumnChunk` and `ColumnMetadata` now carry the page-index
  and bloom-filter offset/length pointers from the parquet.thrift spec.


| Milestone | Reader | Writer | Status |
|-----------|--------|--------|--------|
| A.1 | Footer + Thrift metadata (done) | Footer round-trip (done) | Done |
| A.2 | DATA_PAGE v1: **def/rep levels** + PLAIN **optional** primitives | — | **Done** |
| A.3 | PLAIN_DICTIONARY + levels; dictionary optional columns | — | **Done** |
| A.4 | DATA_PAGE v2 + DELTA_BINARY_PACKED encoding | — | **Done** |
| A.5 | All compression codecs used in the wild (incl. LZ4 / LZ4_RAW) | — | Partial (LZ4 pending) |
| A.6 | Column writer + file assembly + reference-file tests | Writer | **Done** |
| A.7 | Statistics, Bloom filters, page indexes, encryption | Optional tier | **Partial** (page index + bloom filter Done; encryption Planned) |
| A.8 | Remaining encodings (DELTA_LENGTH_BYTE_ARRAY, DELTA_BYTE_ARRAY, BYTE_STREAM_SPLIT, RLE_DICTIONARY) | — | **Done** |
| A.9 | Repetition level semantics for repeated/nested columns | — | **Done** (`materializeRepeatedInt32`, `materializeRepeatedInt64`, `materializeRepeatedFloat`, `materializeRepeatedDouble`, `materializeRepeatedBool`, `materializeRepeatedByteArray`) |

## Phase B — Apache Arrow IPC

| Milestone | Reader | Writer | Status |
|-----------|--------|--------|--------|
| B.1 | Flat record batch materialization | — | **Done** |
| B.2 | Nested types (struct, list), dictionaries | Symmetric | **Done** |
| B.3 | Stream + file IPC + writer | Writer | **Done** |
| B.4 | Golden IPC interop with pyarrow | Planned | Planned |
| B.5 | f16, unsigned ints, unions, map, large binary/utf8 | — | Planned |

## Phase C — Apache ORC

| Milestone | Reader | Writer | Status |
|-----------|--------|--------|--------|
| C.1 | Footer + stripe slice + stream bytes | — | **Done** |
| C.2 | Integer RLE v1/v2 + present stream + boolean RLE | — | **Done** |
| C.3 | Column decoders (int, bool, string, float, double) + compression | — | **Done** |
| C.4 | End-to-end `readColumn` | — | **Done** |
| C.5 | Remaining types (timestamp, date, decimal) + RLE v2 Patched Base | — | **Done** (`decodeTimestampColumn`, `decodeDateColumn`, `decodeDecimalColumn`, `decodeShortColumn`, `decodeTinyIntColumn`, `decodeBinaryColumn`, RLE v2 Patched Base) |
| C.6 | Writer + ORC file assembly | Writer | **Partial** (`buildORCFile`, integer/string/float/double/bool encoders; timestamp / decimal / date writers planned) |

## Phase D — Apache Iceberg

| Milestone | Scope | Status |
|-----------|--------|--------|
| D.1 | Manifest / manifest-list Avro; path helpers | **Done** |
| D.2 | Snapshot selection, schema evolution, partition specs, scan planning | **Done** |
| D.3 | Delete files (position / equality), sequence numbers | **Done** (`PositionDelete`, `applyPositionDeletes`, `planScanWithDeletes`, sequence/file-sequence numbers) |
| D.4 | REST catalog client (optional) | Planned |

Iceberg builds on **Parquet** (and optional other file formats); Phases A–C feed D.

## Execution order

1. **Parquet A.2–A.4** (correct nested + dictionary + encodings) — unblocks real
   files.
2. **Arrow B.2–B.3** — shared test vectors and column materialization.
3. **ORC C.2+** — column decoders on top of existing stripe layout.
4. **Iceberg D.2+** — table semantics on top of Parquet.
