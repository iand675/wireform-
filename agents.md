# wireform Development Guidelines

## Code Generation Principles

**All message types must come from the code generator.** This includes well-known
types (`Timestamp`, `Duration`, `Struct`, etc.), descriptor types, and benchmark
types. Hand-written wire encode/decode instances are not permitted because they
drift from what the code generator produces and mask codegen bugs.

- Well-known types live in `src/Proto/Google/Protobuf/*.hs` and are generated
  from the `.proto` files in `proto/google/protobuf/`.
- Supplementary logic (e.g. `packAny`, RFC 3339 formatting, `TypeRegistry`)
  belongs in companion modules like `Proto.Google.Protobuf.Any.Util` or
  `Proto.JSON.WellKnown`. These import the generated types but never define
  wire-level instances.
- Benchmark comparison types must also be code-generated so that benchmarks
  measure the *actual* codegen output, not idealised hand-written decoders.

## Performance

### Allocation discipline

- **Unboxed sums** for finite branching (success / failure / end-of-input).
  Never use boxed `Either` or `Maybe` on an internal hot path.
- **`withTag` CPS** for the decode loop tag dispatch, where continuations are
  statically known lambdas that GHC will inline.
- **Unboxed `Int#`** for offsets threaded through the decoder.
- Avoid `IORef` in benchmarks where an unboxed accumulator loop suffices.

### String / Text handling

- Never round-trip through `String`. No `T.pack (show n)`, no
  `reads (T.unpack t)`, no `T.pack . show`. Use `Data.Text.Builder` or
  direct numeric-to-Text conversion instead.
- For integer formatting, write directly to a `Builder` or use a purpose-built
  `intToText` helper.
- For parsing integers from `Text`, use `Data.Text.Read.decimal` /
  `Data.Text.Read.signed` rather than `reads . T.unpack`.

### Numeric patterns

- When you need both quotient and remainder, use `divMod` or `quotRem` in a
  single call rather than separate `div` and `mod` on the same operands.
- Prefer `quot`/`rem` over `div`/`mod` for non-negative values (avoids the
  sign-correction branch).

### Data structures

- **No plain tuples** in domain-specific return types. Define a small strict
  record with `{-# UNPACK #-}` on numeric fields. Tuples hide meaning and
  prevent GHC from unboxing nested fields.
- **GrowList is a last resort.** Each `snoc` allocates a cons cell + a
  `GrowList` node (≈48 bytes on 64-bit). Prefer:
  1. `VecBuilder` (IO-based doubling array) when inside IO/ST.
  2. `Data.Vector.create` + `MV.grow` in an ST block when the final size
     is unknown but the builder can be scoped.
  3. If stuck in a pure context (the Decoder monad), a chunked representation
     with amortised allocation (e.g. small arrays of 64 elements, chained)
     is better than a cons-per-element list.

### Decoder monad style

- The `Decoder` newtype wraps `ByteString -> Int# -> (# (# a, Int# #) | DecodeError #)`.
  All primitives (`getVarint`, `getText`, etc.) return unboxed sums.
- In hand-optimised decoders, use `withTag` + direct `runDecoder#` calls for
  each field. In generated code, the monadic `do` notation with `getTagOrU`
  is acceptable (slightly less optimal but far simpler to generate).
- Always `{-# INLINE messageDecoder #-}` on instances.

## Code style

- Do not use list comprehensions. Prefer `do` block syntax or
  higher-order functions.
- Prefer datatype-specific functions with better complexity over
  `toList` / `fromList` conversions.
- No `threadDelay` in tests.
- Keep lens usage to where the alternative would be unwieldy; comment
  complex lens expressions.
- Property-based tests via Hedgehog. Do not test things inherent to
  the language (e.g. setting a record field and reading it back).

## Toolchain

CI builds against GHC `9.6.4` and `9.8.4` (see
`.github/workflows/ci.yml`). When working from a fresh VM that
doesn't already have the Haskell toolchain installed:

- **Apt packages** (Ubuntu 24.04, root):
  ```
  apt-get install -y build-essential libgmp-dev libffi-dev libffi8 \
    libncurses-dev libtinfo6 zlib1g-dev libnuma-dev xz-utils \
    protobuf-compiler
  ```
- **Haskell toolchain via ghcup**:
  ```
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh
  source /root/.ghcup/env
  ghcup install ghc 9.6.4 --set
  ghcup install cabal 3.10.3.0 --set
  cabal update
  ```
- **Cross-language interop tests** (optional): `pip3 install pyarrow`
  for the pyarrow round-trip suites in `wireform-parquet/test/Main.hs`,
  and `pip3 install protobuf` for `python-interop`.
- **First build is slow.** `cabal build all` rebuilds the whole
  workspace (~12 min on a 2-core VM) plus the Hackage dep
  closure; subsequent builds reuse `~/.cabal/store`. Use
  `cabal build all -j2 --ghc-options="-j2"` on small VMs.

If you find yourself running this often, propose an env-setup
agent at <https://cursor.com/onboard> so the cloud-agent base
image bakes the toolchain in.

### Cabal flags worth knowing

The repo opts into a few heavyweight optional dep trees behind
flags so the default `cabal build all` stays lean:

| Flag                  | Pulls in                              | Used by                                |
| --------------------- | ------------------------------------- | -------------------------------------- |
| `+python-interop`     | `process` + a python3 runtime         | `python-interop` test-suite            |
| `+dataframe-bridge`   | `dataframe` (and its cassava/regex/zstd/zlib/granite/vector-algorithms tree) | `example-dataframe-bridge` exe         |
| `+snappy`             | `snappy-c`                            | Avro container files                   |
| `+zstd`               | `libzstd`                             | Parquet ZSTD column chunks, Arrow      |
| `+lz4`                | `liblz4`                              | Parquet LZ4_RAW column chunks, Arrow   |
| `+rest-client`        | `http-client` etc.                    | `Iceberg.Catalog.REST.Client`          |
| `+brotli`             | `libbrotli`                           | Parquet Brotli codec                   |
| `+profile`            | (none)                                | `profile-rewriter` cost-centre build   |

When adding a new optional dependency that has a heavy or
flaky-to-install transitive closure, add it behind a Cabal flag
the same way (default `False`, `manual: True`).

## Module layout

The repo is a monorepo: one umbrella package `wireform` plus 27
per-format / shared-infrastructure packages listed in `cabal.project`.
Each format owns a top-level Haskell namespace (`<Format>.*`); the
umbrella `wireform` package only exposes thin `Wireform.<Format>`
facades and the `wireform-gen` CLI. Cross-package conventions live
under the `Wireform.*` namespace (in `wireform-core` and
`wireform-derive`).

If you are adding or moving modules, update this section *and* the
relevant `*.cabal` `exposed-modules` list in the same change.

### Umbrella

| Package          | Top-level namespace              | Notes |
| ---------------- | -------------------------------- | ----- |
| `wireform`       | `Wireform.<Format>` (facades)    | Re-exports each format under `Wireform.*` (`Wireform.Proto`, `Wireform.Avro`, …, `Wireform.Kafka`) and ships the `wireform-gen` multi-format codegen CLI plus the conformance/profiling/example executables. `Wireform.Columnar` is the cross-format columnar entry point — `decodeIter` / `decodeProjectedIter` / `decodeFilteredIter` / `decodeProjectedFilteredIter` (Parquet + ORC pushdown), `decodeRecordsIter` (Arrow.Record.Table-driven typed records with auto-projection), `decodeDatasetIter` / `decodeDatasetRowSlicedIter` / `decodeHeterogeneousDatasetIter` / `decodePartitionedDataset` (multi-file). |

### Shared infrastructure (`Wireform.*` namespace)

| Package             | Exposed modules                                                                                                                                                                                                          | Purpose |
| ------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ------- |
| `wireform-core`     | `Wireform.FFI`, `Wireform.Encode.Direct`, `Wireform.Hash`                                                                                                                                                                | Shared C-FFI primitives (`fast_decode.c`, `fast_scan.c`, `wireform_hash_simd.c`), the direct-write encode buffer, and the SIMD hashing surface. No format code. |
| `wireform-derive`   | `Wireform.Derive`, `Wireform.Derive.Backend`, `Wireform.Derive.NameStyle`, `Wireform.Derive.Modifier`, `Wireform.Derive.TypeInfo`, `Wireform.Derive.ModifierInfo`, `Wireform.Derive.Extension`, `Wireform.Derive.Aeson`  | Annotation-driven TH deriver core. `Modifier` / `ModifierInfo` are the cross-backend annotation vocabulary; `Backend` / `BackendModifier` (in `Extension`) are how a format opts in. `NameStyle` and `TypeInfo` are the rename + reification helpers used by every per-format `<Format>.Derive`. `Wireform.Derive.Aeson` is the canonical worked example deriver — it lives here (rather than a separate `wireform-derive-aeson` package) so the deriver core has a self-contained reference user. |
| `wireform-columnar` | `Columnar.IO`, `Columnar.Predicate`, `Columnar.SIMD`, `Columnar.Stream`                                                                                                                                                  | Shared by every columnar package: `Columnar.IO` is the mmap-aware file loader (`loadFile` defaults to mmap above 64 KiB, eager below); `Columnar.Predicate` is the `PValue`/`PColPredicate`/`Predicate` vocabulary all per-format pushdown evaluators feed into; `Columnar.Stream` is the pull-based `Iter` / `IterIO` plus combinators (`iterChunk`, `iterScan`, `iterMergeBy`, `iterIOPrefetch`, `iterParallelMap`); `Columnar.SIMD` is the SIMD-accelerated bit-unpacking / RLE kernel shared with the C side via `cbits/columnar_simd.c` + vendored `simde`. |

### Per-format packages — Haskell `<Format>.*` namespace

Each per-format package conventionally exposes the same module
shape:

```
<Format>                         -- (sometimes) top-level umbrella module
<Format>.Class                   -- typeclass(es) for value-level codecs
<Format>.Encode / <Format>.Decode -- low-level encode / decode primitives
<Format>.Value                   -- dynamic / untyped value ADT
<Format>.Derive                  -- annotation-driven TH deriver (consumes Wireform.Derive)
<Format>.Schema | .Parser | .CodeGen | .QQ | .Registry  -- IDL surface where the format has one
<Format>.JSON                    -- self-describing-format ↔ JSON bridge
```

Derivers (`<Format>.Derive`) are structural twins: they import
`Wireform.Derive`, reify the type, walk the `ModifierInfo`, and
splice instance declarations for `<Format>.Class`. To add a new
format, the path of least resistance is "clone the nearest
existing `<Format>.Derive` and adapt the value-mapping calls".

| Package               | Exposed modules                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | Notes |
| --------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ----- |
| `wireform-cbor`       | `CBOR.Class`, `CBOR.Encode`, `CBOR.Decode`, `CBOR.Derive`, `CBOR.Value`, `CBOR.Diagnostic`, `CBOR.JSON`, `CBOR.QQ`, `CBOR.Stream`, `CBOR.TagRegistry`, `CBOR.CDDL`, `CBOR.CDDLSchema`, `CBOR.CDDLCodeGen`                                                                                                                                                                                                                                                                                                                                       | RFC 8949 CBOR + CDDL schema & codegen. |
| `wireform-msgpack`    | `MsgPack.Class`, `MsgPack.Encode`, `MsgPack.Decode`, `MsgPack.Derive`, `MsgPack.Value`, `MsgPack.JSON`, `MsgPack.RPC`, `MsgPack.Stream`                                                                                                                                                                                                                                                                                                                                                                                                          | MessagePack + msgpack-RPC. |
| `wireform-thrift`     | `Thrift.Class`, `Thrift.Encode`, `Thrift.Decode`, `Thrift.Derive`, `Thrift.Value`, `Thrift.Wire`, `Thrift.Schema`, `Thrift.Parser`, `Thrift.CodeGen`, `Thrift.Message`, `Thrift.Transport`, `Thrift.Registry`, `Thrift.JSON`, `Thrift.QQ`                                                                                                                                                                                                                                                                                                       | Apache Thrift binary / compact + IDL. |
| `wireform-avro`       | `Avro.Class`, `Avro.Encode`, `Avro.Decode`, `Avro.Derive`, `Avro.Value`, `Avro.Wire`, `Avro.Schema`, `Avro.Schema.Parse`, `Avro.IDL`, `Avro.IDLConvert`, `Avro.Container`, `Avro.Resolution`, `Avro.Fingerprint`, `Avro.Protocol`, `Avro.JSON`, `Avro.QQ`, `Avro.Registry`, `Avro.CodeGen`                                                                                                                                                                                                                                                       | Apache Avro + IDL + container files. |
| `wireform-bson`       | `BSON.Class`, `BSON.Encode`, `BSON.Decode`, `BSON.Derive`, `BSON.Value`                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | BSON (MongoDB). |
| `wireform-ion`        | `Ion.Class`, `Ion.Encode`, `Ion.Decode`, `Ion.Derive`, `Ion.Value`, `Ion.SchemaLang`, `Ion.ISLSchema`, `Ion.ISLCodeGen`, `Ion.QQ`                                                                                                                                                                                                                                                                                                                                                                                                                | Amazon Ion + Ion Schema Language (ISL). |
| `wireform-edn`        | `EDN.Class`, `EDN.Encode`, `EDN.Decode`, `EDN.Derive`, `EDN.Value`, `EDN.JSON`                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | Extensible Data Notation. |
| `wireform-toml`       | `TOML.Class`, `TOML.Encode`, `TOML.Decode`, `TOML.Derive`, `TOML.Value`                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | TOML 1.0 / 1.1. Conformance against the upstream [toml-test](https://github.com/toml-lang/toml-test) suite is opt-in via `TOML_TEST_SUITE=/path/to/clone` at test time (the test binary picks up either the repo root or its `tests/` subdirectory). |
| `wireform-yaml`       | `YAML.Class`, `YAML.Encode`, `YAML.Decode`, `YAML.Derive`, `YAML.Encoding`, `YAML.JSON`, `YAML.Value`                                                                                                                                                                                                                                                                                                                                                                                                                                            | YAML 1.2 (block + flow styles, anchors / aliases, tags, block literal / folded scalars, multi-document streams). Conformance against the upstream [yaml-test-suite](https://github.com/yaml/yaml-test-suite) is opt-in via `YAML_TEST_SUITE=/path/to/clone` at test time. |
| `wireform-bencode`    | `Bencode.Class`, `Bencode.Encode`, `Bencode.Decode`, `Bencode.Derive`, `Bencode.Value`                                                                                                                                                                                                                                                                                                                                                                                                                                                          | BitTorrent bencode. |
| `wireform-fory`       | `Fory.Class`, `Fory.Encode`, `Fory.Decode`, `Fory.Derive`, `Fory.Encoding`, `Fory.IO`, `Fory.MetaString`, `Fory.MetaString.Encoder`, `Fory.MetaString.Hash`, `Fory.Options`, `Fory.Struct`, `Fory.TypeId`, `Fory.Value`                                                                                                                                                                                                                                                                                                                                       | Apache Fory (formerly Fury) xlang serialization. Wire-compatible with `pyfory` 0.17 for: `null`, `bool`, `int*`/`varint*`, `uint*`/`varuint*`, `float32`/`float64`, `string` (with LATIN-1 / UTF-8 selection), `binary`, `LIST` / `SET` (chunked `collect_flag` format incl. `TRACKING_REF`), `MAP` (chunked key-type/value-type), `NAMED_STRUCT` (with `Fory.Struct.StructSchema` registered on both sides; produces byte-identical bytes incl. the 4-byte fingerprint hash and pyfory's canonical field reordering), one-dimensional primitive arrays (`BoolArray` … `Float64Array`, byte-length payloads matching pyfory's NumPy serializer), reference tracking (`Fory.Options.eoRefTracking`; structural sharing detected via `Hashable Value`), and meta-string compression (the five LowerSpecial / LowerUpperDigitSpecial / FirstToLowerSpecial / AllToLowerSpecial / UTF-8 encodings + MurmurHash3-x64-128 hashcodes for >16-byte strings). Verified by `wireform-fory-interop` (45 / 45 cases passing). The remaining ❌ is pyfory-compatible `NAMED_COMPATIBLE_STRUCT` (schema evolution): its bit-packed `TypeDef` field-info layout is deferred. The in-package self-describing `CompatibleStructVal` round-trips fine; only cross-language interop for it is unfinished. See `Fory.Encode`'s haddock for the exact wire shape. |
| `wireform-csv`        | `CSV.Class`, `CSV.Encode`, `CSV.Decode`, `CSV.Derive`, `CSV.Value`                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | CSV / tab / pipe-separated. |
| `wireform-ndjson`     | `NDJSON.Encode`, `NDJSON.Decode`, `NDJSON.Derive`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | Newline-delimited JSON (line framing on top of aeson). |
| `wireform-asn1`       | `ASN1.Encode`, `ASN1.Decode`, `ASN1.Derive`, `ASN1.Value`, `ASN1.Schema`, `ASN1.Parser`, `ASN1.CodeGen`, `ASN1.QQ`                                                                                                                                                                                                                                                                                                                                                                                                                              | ASN.1 BER / DER. |
| `wireform-bond`       | `Bond.Encode`, `Bond.Decode`, `Bond.Derive`, `Bond.Value`, `Bond.Schema`, `Bond.Parser`, `Bond.CodeGen`, `Bond.Registry`, `Bond.QQ`                                                                                                                                                                                                                                                                                                                                                                                                              | Microsoft Bond. |
| `wireform-flatbuffers`| `FlatBuffers.Encode`, `FlatBuffers.Decode`, `FlatBuffers.Derive`, `FlatBuffers.Value`, `FlatBuffers.Schema`, `FlatBuffers.Parser`, `FlatBuffers.CodeGen`, `FlatBuffers.Registry`, `FlatBuffers.QQ`                                                                                                                                                                                                                                                                                                                                              | Google FlatBuffers + IDL. |
| `wireform-capnproto`  | `CapnProto.Encode`, `CapnProto.Decode`, `CapnProto.Derive`, `CapnProto.Value`, `CapnProto.Schema`, `CapnProto.Parser`, `CapnProto.CodeGen`, `CapnProto.Registry`, `CapnProto.QQ`                                                                                                                                                                                                                                                                                                                                                                | Cap'n Proto + IDL. |
| `wireform-columnar`   | `Columnar.IO`, `Columnar.Predicate`, `Columnar.SIMD`, `Columnar.Stream`                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | Format-agnostic columnar primitives shared by Arrow / Parquet / ORC. `Columnar.Stream` exposes pull-based `Iter` / `IterIO` + combinators (`iterChunk`, `iterScan`, `iterMergeBy`, `iterIOPrefetch`, `iterParallelMap`); `Columnar.Predicate` is the shared pushdown vocabulary; `Columnar.IO` is the mmap-aware file loader (`loadFile` / `loadFileMmap` / `loadFileEager`). |
| `wireform-arrow`      | `Arrow.Types` (incl. `schemaFingerprint` / `schemaEquivalent`), `Arrow.Column` (incl. `validateMapKeysSorted`), `Arrow.Record` (incl. `structE`/`structEMaybe` + `structD`/`structDMaybe`, `columnDWithDefault`, `subsetTable`/`projectTable`, `NameStrategy`/`applyNameStrategy`), `Arrow.Record.Generic`, `Arrow.Record.TH`, `Arrow.Derive`, `Arrow.IPC`, `Arrow.FlatBufferIPC`, `Arrow.Stream`, `Arrow.File`, `Arrow.Write`                                                                                                                     | Apache Arrow IPC / records. Test suite lives in `test-derive/` (separate from the main `test/` so the deriver fixtures isolate easily). Uses the `+zstd` and `+lz4` flags. |
| `wireform-parquet`    | `Parquet.Types`, `Parquet.Footer`, `Parquet.Read` (path + handle helpers: `loadParquetFilePath` / `openParquetReader`), `Parquet.Write`, `Parquet.Aggregate` (count(*) / count(col) / min / max from stats), `Parquet.Page`, `Parquet.PageIndex`, `Parquet.Levels`, `Parquet.LevelsEncode`, `Parquet.Nested`, `Parquet.Compress`, `Parquet.Delta`, `Parquet.DeltaEncode`, `Parquet.ByteStreamSplit`, `Parquet.BloomFilter`, `Parquet.NullPagesBitmap`, `Parquet.Encryption`, `Parquet.Thrift.Schema`, `Parquet.Arrow`, `Parquet.HighLevel`, `Parquet.Derive` | Apache Parquet (reader / writer / Thrift schema bridge). Test suite in `test-derive/`. |
| `wireform-orc`        | `ORC.Types`, `ORC.Footer`, `ORC.Stripe`, `ORC.RowIndex`, `ORC.BloomFilter` (incl. `decodeBloomFilter` + `bfCheckBytes` / `bfCheckLong`), `ORC.Encryption`, `ORC.Read` (path + handle helpers, `decompressORCStreamSized`), `ORC.Write`, `ORC.Aggregate` (count + min/max + sum from stats), `ORC.Statistics` (predicate evaluator), `ORC`, `ORC.Arrow` (incl. `streamStripesFilteredIter` / `streamStripesProjectedFilteredIter`), `ORC.Proto.Schema`, `ORC.Derive`                                                                              | Apache ORC. Test suite in `test-derive/`. |
| `wireform-iceberg`    | `Iceberg.Types`, `Iceberg.Snapshot`, `Iceberg.Manifest`, `Iceberg.ManifestMerge`, `Iceberg.Partition`, `Iceberg.Sort`, `Iceberg.Transform`, `Iceberg.Expression`, `Iceberg.Update`, `Iceberg.Validate`, `Iceberg.Read`, `Iceberg.Write`, `Iceberg.Maintenance`, `Iceberg.MetricsConfig`, `Iceberg.SchemaCompat`, `Iceberg.SchemaEvolution`, `Iceberg.SingleValue`, `Iceberg.BoundTrunc`, `Iceberg.Murmur3`, `Iceberg.Geometry`, `Iceberg.Variant{,.Parquet,.Shredding}`, `Iceberg.Puffin`, `Iceberg.Delete`, `Iceberg.DeletionVector`, `Iceberg.View`, `Iceberg.Parquet`, `Iceberg.JSON`, `Iceberg.Catalog.{Glue,Hadoop,REST,REST.Client,Sql}`, `Iceberg.Derive` | Apache Iceberg table format + catalog clients. Behind `+rest-client` flag for the HTTP client. Test suite in `test-derive/`. |
| `wireform-xml`        | `XML.Class`, `XML.Encode`, `XML.Decode`, `XML.Derive`, `XML.Value`, `XML.Schema`, `XML.SAX`, `XML.DSL`, `XML.QQ`, `XML.FastDOM`, `XML.Generic`, `XML.Incremental`, `XML.Path`, `XML.XSLT`, `XML.CodeGen`                                                                                                                                                                                                                                                                                                                                        | XML 1.0 + SAX / DOM / XSLT / XPath. |
| `wireform-html`       | `HTML.Value`, `HTML.Parse`, `HTML.Encode`, `HTML.Class`, `HTML.Derive`, `HTML.TagId`, `HTML.DOM`, `HTML.Selector`, `HTML.Rewriter`                                                                                                                                                                                                                                                                                                                                                                                                              | HTML5 parser + DOM + CSS selectors + streaming rewriter. Has its own benchmarks (`bench/HTMLBench.hs`, `bench/ProfileRewriter.hs`). |
| `wireform-grpc`       | `Network.GRPC.{Client,Server,Common,*}` — see cabal for the full list                                                                                                                                                                                                                                                                                                                                                                                                                                                                            | **Vendored from [`grapesy`](https://github.com/well-typed/grapesy) by Edsko de Vries.** Modules under `Network.GRPC.Util.*` are intentionally `other-modules` (private). Do not match wireform's `<Format>.*` shape; do not retrofit module headers or codegen-generated style here without coordinating an upstream sync. |
| `wireform-kafka`      | `Kafka` (umbrella), `Kafka.Protocol.{Primitives,Encoding,RecordBatch,CRC32C,ApiVersions,Message}`, `Kafka.Protocol.Generated.*` (one module per Kafka API key — `MetadataRequest`, `FetchResponse`, `ProduceRequest`, `JoinGroupRequest`, `Sync*`, `OffsetCommit*`, `Txn*`, `Sasl*`, `Share*`, …), `Kafka.Network.{Connection, Auth.{Plain,SCRAM,SASL}}`, `Kafka.Compression.{Gzip,Lz4,Snappy,Zstd,Types}`, `Kafka.Client.{Producer,Consumer,AdminClient,Transaction,Metadata,Pipeline,Simple,Internal.*}`, `Kafka.Telemetry.OpenTelemetry`. | Pure-Haskell native client for the Apache Kafka wire protocol (TCP / TLS / SASL / compression / version negotiation / transactions / consumer groups / pipelining / OTel). The `Kafka.Protocol.Generated.*` namespace is emitted by the `kafka-codegen` executable from upstream Kafka JSON message definitions; regenerate via `scripts/regen-kafka-protocol.sh`. The codegen lives in `wireform-kafka/codegen/` (sublibrary `wireform-kafka-codegen`). Has its own C FFI (`cbits/{crc32c,snappy_ffi,lz4_ffi}.c`); the SSE4.2 / AVX-512 / ARM CRC32 helpers use `__attribute__((target(...)))` so we do not need a global `-msse4.2` / `-mcrc`. Tests that need a live broker are gated by `WIREFORM_KAFKA_BROKER=host:port`; `Protocol.Generated.{Comprehensive,KnownGood}` are gated on a `test-vectors.json` fixture (silent skip when absent). |
| `wireform-jsonschema` | `JsonPointer`, `JsonSchema`, `JsonSchema.{Types,Parser,Parser.Extended,Parser.Internal,Renderer,Validator,Validator.{Result,Annotations,Extended},Metadata,Dialect,Vocabulary{,.Types,.Registry,.Validation},ReferenceLoader,EmbeddedMetaschemas{,.Raw},MetaschemaValidation,Regex,Codegen.Types}`, `JsonSchema.Keywords.*` (one module per JSON Schema keyword), `JsonSchema.Keyword.{Types,Compile,Validate,Monadic}`, `JsonSchema.{Class,CodeGen,Derive}`. | JSON Schema draft-04 / 06 / 07 / 2019-09 / 2020-12 (parser, validator, renderer, dialect / vocabulary registry, embedded metaschemas, code generation). Ported from [`iand675/fractal`](https://github.com/iand675/fractal)'s `fractal-openapi`; namespace lifted out of `Fractal.*` into the wireform `<Format>.*` shape. The IDN format checks (`hostname`, `idn-hostname`) come from [`idn`](https://hackage.haskell.org/package/idn) on Hackage; the `pattern` keyword's ECMAScript-262 regex engine still vendors as a repo-root `ecma262-regex/` package (QuickJS `libregexp` FFI; not on Hackage as of 2026-05). The annotation-driven deriver is `JsonSchema.Derive`; `JsonSchema.CodeGen` is the `Schema -> Text` Haskell-source generator. Conformance against the upstream [json-schema-test-suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite) is opt-in via `JSON_SCHEMA_TEST_SUITE=/path/to/clone` at test time (silent skip when unset). |

### `wireform-proto` — bigger surface, historical layout

The protobuf package predates the per-format split and is the
largest in the repo. It owns the `Proto.*` namespace and contains
both the IDL toolchain and the generated well-known types.

```
Proto.AST                              -- .proto IDL AST
Proto.Parser, Proto.Parser.{Lexer, Resolver, Error}
                                       -- IDL parser pipeline
Proto.Wire, Proto.Wire.{Encode, Decode, Result}
                                       -- wire-format primitives (tags, varints,
                                          unboxed-sum decode results)
Proto.Encode, Proto.Encode.{Direct, Lazy, Archetype}
Proto.Decode, Proto.Decode.{Fast, Stream, Streaming}
                                       -- high-level encode / decode typeclasses
                                          and the hand-tuned hot paths
Proto.SizedBuilder, Proto.VectorBuilder
                                       -- builder utilities used by encoders
Proto.CodeGen, Proto.CodeGen.{Combinators, Decode, Encode, Service, Hooks, Types}
                                       -- pure-text Haskell code generator
Proto.TH                               -- IDL → TH bridge (`loadProto`,
                                          `loadProtoWith`, `loRepConfig`)
Proto.Derive                           -- annotation-driven TH deriver: hand-written
                                          records + `ANN tag`/`wireOverride`/
                                          `customModifier` produce instances
Proto.Derive.Internal                  -- body builders shared by `Proto.Derive`
                                          and `Proto.TH` (so the IDL bridge and the
                                          annotation deriver emit identical code)
Proto.Repr                             -- per-field representation choices
                                          (`StringRep`, `BytesRep`, `RepeatedRep`,
                                          `MapRep`, `OptionalRep`, `FieldRep`,
                                          `RepConfig`)
Proto.Schema                           -- runtime type metadata
Proto.Message                          -- `IsMessage` typeclass
Proto.FieldPresence                    -- proto2/proto3 presence helpers
Proto.Merge, Proto.Compat              -- merge semantics + version-compat helpers
Proto.Lens, Proto.Inspect, Proto.Print -- lens accessors, debug printers
Proto.Annotations                      -- `Annotation`s reified by the deriver
Proto.Options, Proto.Options.Custom    -- proto file/message/field options
Proto.Extension                        -- proto2 `extend`s
Proto.Registry, Proto.Registry.TH      -- runtime type registry
Proto.Setup                            -- Cabal `Setup.hs` integration
Proto.QQ                               -- proto-source QuasiQuoter
Proto.Dynamic                          -- dynamic (untyped) messages
Proto.TextFormat                       -- pbtxt serialisation
Proto.TDP                              -- transparent dynamic proto support
Proto.Conformance                      -- protobuf conformance test driver
Proto.Church                           -- Church-encoded message walks
Proto.Descriptor.Convert               -- AST ↔ descriptor.proto bridge
Proto.GRPC                             -- gRPC service-method codegen
Proto.JSON, Proto.JSON.WellKnown       -- proto3 JSON mapping (canonical encoding)
Proto.Internal.{Either, Maybe}         -- strict unboxed sums for hot loops
                                          (intra-package `.Internal`; do not import
                                          from outside `wireform-proto`)
Proto.Google.Protobuf.*                -- code-generated well-known types from
                                          `proto/google/protobuf/*.proto`
Proto.Google.Protobuf.*.Util           -- supplementary logic for well-known
                                          types (`packAny`, RFC 3339 formatting,
                                          `TypeRegistry`, `FieldMask` ops, etc.)
```

Everything under `Proto.Google.Protobuf.*` is regenerated from the
.proto files in `proto/google/protobuf/` by the `gen-wkt`
executable; see "Code Generation Principles" above.

## Annotation-driven deriver vocabulary

Every per-format `Derive` module accepts the same `Modifier`
vocabulary from `wireform-derive`. Rule of thumb when adding a new
constructor to `Wireform.Derive.Modifier.Modifier`:

1. Bump `wireform-derive` to a new version (any constructor add is
   technically API-breaking under PVP).
2. Extend `Wireform.Derive.ModifierInfo.ModifierInfo` with the
   resolved field, plus a `ConflictX` constructor in `ModifierError`.
3. Wire the new field into `mergeOne` and `shadowOne`.
4. Per-format derivers consult `mi<Whatever>` and silently ignore
   the field if the modifier does not apply to their backend (e.g.
   `miMapKey` is proto-only).

Backend-specific payloads that should not pollute the core ADT use
the `Wireform.Derive.Extension.BackendModifier` typeclass — see
`XmlFieldOpt`, `HtmlFieldOpt`, and `Asn1Tag` for examples.
