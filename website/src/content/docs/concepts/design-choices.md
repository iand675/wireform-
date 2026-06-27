---
title: Design choices
description: "The performance, correctness, dependency, and ergonomics principles that every wireform package is held to."
sidebar:
  order: 2
---

Why wireform looks the way it does. The per-package pages explain what each format does; this page covers the constraints that apply across all of them.

## Why this exists

Pick a serialization format (protobuf, Avro, CBOR, whatever). Then go find a Haskell library for it. The questions you end up asking are always the same: is it fast? (Benchmarks were last updated two GHC releases ago.) Does it pass the conformance suite? (Never mentioned.) Is it maintained? Recent commit history suggests maybe. Will it pull in half of Hackage? Check the `.cabal`. Does its API look anything like the last one you used? It does not. `aeson` says `eitherDecodeStrict`, the CBOR library says `deserialiseFromBytes`, MessagePack says `unpack`.

wireform is the answer to that audit: one workspace of format packages sharing the same core, the same deriver, the same testing bar, and (where an upstream conformance suite exists) an opt-in runner wired to it.

## Four properties

Every package under the wireform name has to clear the same bar:

| Property | Meaning |
|----------|---------|
| Ergonomic | One annotation vocabulary drives instances for every format. Same names, same concepts, same workflow. |
| Fast | Generated code matches or beats hand-written codecs, and sits within striking distance of C/Rust/Zig implementations. |
| Correct | Property tests plus, where one exists, the format's official conformance suite or a cross-language interop check. |
| Dependency-light | Each package depends on `wireform-core`, `wireform-derive`, and whatever the format genuinely requires. Nothing else. |

## One deriver, every format

You annotate a Haskell record once and every format reads the same annotations. The vocabulary lives in [`wireform-derive`](../../packages/derive/) (`Wireform.Derive.Modifier`): `rename` / `renameStyle`, `tag N`, `skip`, `required`, `optional`, `flatten`, `wireOverride`, and `forBackend backendJSON (...)` for per-format overrides. Formats that need extra knobs opt in through `BackendModifier` extensions (`XmlFieldOpt`, `HtmlFieldOpt`, `Asn1Tag`) instead of polluting the shared ADT.

The result: `personFullName` can become `full_name` on every binary wire but `fullName` in JSON, and `personSecret` can be omitted from JSON entirely, all driven by the same `ANN` pragmas. `deriveProto`, `deriveCBOR`, `deriveMsgPack`, and `deriveJSON` splice from one description. See [Deriving instances](../deriving/) for the worked example.

This is also why per-format `Derive` modules are structural twins: each imports `Wireform.Derive`, reifies the type, walks the resolved `ModifierInfo`, and splices instances. Adding a new format is mostly "clone the nearest existing `<Format>.Derive` and adapt the value-mapping calls."

## Allocation discipline

Performance across every package comes from a set of rules applied uniformly, not from hot-spot tuning:

- **Unboxed sums for finite branching.** The `Decoder` newtype wraps `ByteString -> Int# -> (# (# a, Int# #) | DecodeError #)`. Success, failure, and end-of-input are unboxed alternatives. Boxed `Either` and `Maybe` are banned on internal hot paths.
- **CPS tag dispatch.** Decode loops dispatch field tags through a `withTag` continuation-passing helper whose continuations are statically known lambdas that GHC will inline.
- **Unboxed `Int#` offsets** threaded through the decoder instead of boxed counters.
- **No round-trips through `String`.** No `T.pack (show n)`, no `reads (T.unpack t)`. Integer formatting goes straight to a `Builder` or a purpose-built `intToText`; integer parsing uses `Data.Text.Read`.
- **No plain tuples in domain return types.** Small strict records with `{-# UNPACK #-}` on numeric fields, so GHC can unbox nested fields that a tuple would hide.
- **Cons-per-element lists are a last resort.** Builders prefer `VecBuilder` (IO doubling array) or `Data.Vector.create` + `MV.grow` in ST over accumulating linked nodes.

The hottest paths go further: zero-copy encoding, SIMD-accelerated scanning, and hand-written C kernels, shared through [`wireform-core`](../../packages/core/) (`fast_decode.c`, `fast_scan.c`, the SIMD hashing surface) and [`wireform-columnar`](../../packages/columnar/) (bit-unpacking / RLE kernels with vendored `simde`). The rule for generated code: match or beat an equivalent hand-written codec.

LLVM is off by default (`-fasm` in `cabal.project`) so day-to-day builds stay fast; production builds can opt into `-fllvm` for up to ~27% on tight loops.

## Correctness against upstream suites

A README claiming "spec-compliant" is not proof. Where a format has an official conformance suite, wireform ships an opt-in runner that wires it up and silently skips when the suite isn't installed:

- **Protocol Buffers** runs the official `protocolbuffers/protobuf` harness (2,675 / 2,675).
- **TOML** runs `toml-test`; **YAML** runs `yaml-test-suite`.
- **Iceberg, Delta Lake, Hudi, Lance** round-trip through their respective Python/Rust readers (pyiceberg + fastavro, delta-rs, hudi-rs, pylance).
- **Fory** tests against `pyfory`; **Kafka** clients test against a live broker via `WIREFORM_KAFKA_BROKER=host:port`.
- **CEL** runs the upstream cel-spec suite (pass=1124, skip=128, fail=0 for the non-message core).

Where no upstream suite exists, the bar is an explicit interop test against another language's implementation, plus Hedgehog property tests. Property tests do not check things inherent to the language (e.g. that setting a record field and reading it back works).

## Generated code is output; the generator is source

Several formats generate Haskell (protobuf well-known types, Kafka protocol messages, Lance protobufs, benchmark comparison types). Rule: **never hand-edit a generated file.** A tweak applied directly to a `Generated/Foo.hs` survives only until the next regen clobbers it and reintroduces the original bug. Changes go into the codegen (`<Format>.CodeGen.*` / `<Format>/codegen/`), and the regenerated output is what gets committed. CI audits this by regenerating and diffing. A non-trivial diff means a hand-edit has crept in.

The same principle governs the per-package `README.md` AUTOGEN regions. The `tests`, `coverage`, and `bench:<id>` blocks between `<!-- BEGIN_AUTOGEN -->` / `<!-- END_AUTOGEN -->` markers are owned by [`wireform-stats`](../../packages/stats/)' `regen-stats` tool and rebuilt from in-tree data, so the numbers in the docs never drift from reality.

## Dependency discipline

The workspace is a set of self-contained packages, not one mega-library: if you only need CBOR, you only build CBOR. Heavyweight or flaky-to-install dependency trees are gated behind Cabal flags that default to `False` (`+python-interop`, `+dataframe-bridge`, `+snappy`, `+zstd`, `+lz4`, `+brotli`, `+rest-client`, …). When a new optional dependency has a heavy transitive closure, it gets the same treatment.

Cross-cutting conventions live under the `Wireform.*` namespace (`wireform-core`, `wireform-derive`); each format owns its `<Format>.*` namespace. Vendored libraries that predate or sit outside this shape (`hermes` for HTTP header grammar, the grapesy-derived gRPC stack, `http-semantics`) are kept recognizably themselves rather than retrofitted, with notes in `AGENTS.md` about where wire-grammar changes belong.

## Module layout

Because the deriver and testing bar are shared, the layout is shared too. Every per-format package converges on the same shape:

```
<Format>.Encode / .Decode      -- wire codec primitives
<Format>.Class                 -- typeclass (ToCBOR, FromThrift, …)
<Format>.Derive                -- TH deriver consuming Modifier annotations
<Format>.Value                 -- dynamic value ADT (where applicable)
<Format>.JSON                  -- JSON bridge (where applicable)
```

Formats with an IDL add `<Format>.Parser` and `<Format>.CodeGen`. Once you know one package, you can navigate any of them.

## The bar for a new format

A new format package is welcome when it:

1. is fast enough to rival C/Rust/Zig with minimal GC overhead;
2. is tested hard enough to prove conformance with the format's official suite, or (absent one) an explicit cross-language interop test;
3. is wired into the shared annotation deriver so users don't learn a new API per format; and
4. is dependency-light enough not to raise eyebrows.

The format coverage is intentionally broad. The per-format bar is intentionally high.
