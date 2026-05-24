# wireform-core

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

Shared SWAR / SIMD-accelerated primitives used by every
[`wireform-*`][wireform] format package.  Format-agnostic; the C
sources live in `cbits/` and the vendored [simde][simde] headers in
`include/simde/`.

[wireform]: https://github.com/iand675/wireform-
[simde]: https://github.com/simd-everywhere/simde

## What's in here

| Module                      | Role                                                              |
|-----------------------------|-------------------------------------------------------------------|
| `Wireform.Builder`          | High-performance byte builder (vendored fast-builder engine).     |
| `Wireform.Builder.FastBuilder` | Builder internals: `DataSink`, `StreamSink`, `BuildM`, etc.    |
| `Wireform.FFI`              | C FFI surface (varints, UTF-8 / ASCII / NUL / JSON scanners, …). |
| `Wireform.Encode.Direct`    | Shared direct-write encode buffer.                                |
| `Wireform.Hash`             | SIMD-accelerated hashing helpers.                                 |

### Builder

`Wireform.Builder` is the shared byte builder used by all wireform
format packages. It supports O(1) concatenation, direct `Handle`
output without intermediate `ByteString` allocation, streaming
transforms (compression, encryption) that process chunks as the
builder produces them, and direct ring write via `RingSink` (the
builder-side counterpart of `sendBuilderDirect` in
`Wireform.Transport.Send`).

Based on [fast-builder](https://github.com/takano-akio/fast-builder)
by Takano Akio (public domain).

This package is intentionally small; it exists so the per-format
packages can share the builder engine and the hottest C kernels
(e.g. `validateUtf8SWAR`, `countPackedVarints`, `findByte`) without
duplicating the `__attribute__((target(...)))` / `simde-features.h`
plumbing.

## License

BSD-3-Clause.  Vendored `simde` headers carry their own MIT license
under `include/simde/simde/COPYING` and friends.
