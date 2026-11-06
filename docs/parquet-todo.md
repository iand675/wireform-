# Parquet follow-ups

## Bloom-filter performance

Deferred by Ian on 2026-11-06. Bloom throughput is not a publication blocker;
the remaining writer capabilities must be finished before publication.

- Replace the flat writer's immutable insertion fold, which copies the entire
  bitset for each value, with efficient whole-column construction.
- Bring construction and serialization throughput up to the compiled
  references, including numeric columns and variable-length byte values.
- The isolated bulk-builder prototype allocates about 68 KiB per 32 KiB
  filter, versus 66 KiB for parquet-rs. For 8192 pre-built values it still
  takes 3.8–4.9 times as long as parquet-rs 60.1.0 across the four tested
  widths (1, 4, 8 and 32 bytes). These are prototype measurements, not
  performance claims for the current package.
- Preserve the 48 byte-for-byte reference fixtures, sliced-bitset checks,
  malformed-constructor checks and sizing-boundary regressions. Both
  parquet-rs 58.2.0 and 60.1.0 agree on the reference bytes.
- The speculative Haskell short-input hash path was removed from the
  prototype; all 519 shared-core source files match the existing candidate.
- Complete the supported-compiler and external conformance checks before
  integrating the prototype. Run timing benchmarks sequentially, without
  concurrent builds or tests.

Local prototype and evidence:
`/private/tmp/wireform-parquet-bloom-builder-audit/`, particularly
`fifth-current-reference-comparison.json`, `oracle-proof.json`,
`sizing-decimal-reference.json` and `unit-c-hash-sizing.log`.

Nested writing now uses the bulk builder. The flat writer and the matched
throughput work remain follow-ups.

## Writer capabilities required before publication

- [x] Generate bloom filters by name for nested writes (all three supported
  compilers and independent membership checks pass).
- [x] Generate min/max statistics and column indexes for non-null nested leaves
  (including FLOAT16, IEEE NaN/zero ordering and explicit INT96 ordering).
- [x] Support nullable and nested encrypted writes.
- [x] Support separate column encryption keys.
- [x] Support signed plaintext footers.

Each capability needs regression coverage and independent interoperability
checks before it is considered complete.
