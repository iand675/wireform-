# wireform


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

You need to serialize a thing. Maybe it's protobuf, because the team
next door decided proto is what events look like. Maybe it's Avro,
because Iceberg is involved. Maybe it's CBOR, because you need to
do something with COSE. The exact format doesn't matter. The next ten minutes are
roughly the same regardless.

You open Hackage. There are usually a couple of packages. Now you must audit the code:

**Is it fast?** The README doesn't say. The benchmarks live in a
`bench/` directory last touched in 2019, when GHC was on a different
number. Inconclusive.

**Does it pass the upstream conformance suite?** The README doesn't
mention a conformance suite. Every serious
format has one. You find yourself wondering whether the library you're looking at actually behaves as expected.

**Has anyone touched it lately?** Define "lately." There's a commit
from six months ago that says `bump bounds`, and before that one from
two years ago that says `wip`. The author has, in the intervening
period, started writing Rust full-time. Understandable, but
inconvenient.

**Will it pull half of Hackage in?** Let's check the cabal file.
`bytestring`, fine. `text`, fine. `lens`, because the author wanted
`view _Just` somewhere. `lens-aeson`, naturally. Before too long,
the transitive footprint means you'll spend 20 minutes just compiling the dependencies. You start to wonder if maybe you should just write this yourself.

**Will the API match the rest of your stack?** It will not. The same concepts end up
with slightly different names in each library:

`aeson` says `eitherDecodeStrict`.

The CBOR library calls it `deserialiseFromBytes`.

The MsgPack library calls it `unpack`.

And so on.

You end up doing this song and dance semi-regularly. After enough times you notice that
determining whether to trust libraries from random authors takes as
long as writing the library yourself.

You repeatedly end up writing the same custom code over and over:

Benchmarks to see if the library is actually fast enough, property tests to see if it actually behaves as expected, and so on. Template Haskell deriver boilerplate or Generics,
an orphan instance to bridge to/from JSON for the format
that didn't think it would need one.

Surely someone has already solved this, you think. Rust has `serde`, for example.

Enter `wireform`.

Wireform provides an ecosystem of roughly thirty format packages where
every one shares the extremely performant core utilities (`wireform-core`),
the same annotation-driven Template Haskell deriver (`wireform-derive`),
aggressively complete test suites, and, where an upstream conformance
suite exists, an opt-in test runner that wires it up.

For example:

- Protobuf runs against the official `protocolbuffers/protobuf` harness.
- TOML runs against `toml-test`.
- YAML runs against `yaml-test-suite`.
- Iceberg, Delta Lake, Hudi, and Lance round-trip through their respective Python or Rust readers.
- Fory tests against `pyfory`.
- Kafka clients test against a live broker.

Our goal, with this project, is to have any package published on Hackage under the
wireform monicker to be a trustworthy promise that the library is _ergonomic_, _performant_, _correct_, and avoids incurring more dependencies than necessary.

Every format draws on the same handful of
heavily optimized primitives. SIMD-accelerated parsing, zero-copy encoding/decoding where possible, and specialized C kernels for the hottest paths. All generated code in the repo
is held to the standard that it is has to be as fast as, or faster than, hand-written codecs, and within spitting distance of Rust/C/Zig, if not faster.

`wireform` as a project is unapologetically maximalist. If you need to
parse, render, encode, decode, frame, or otherwise shuffles bytes
between two systems, we want to support it.

The current thirty packages are a starting
point for the project, but new format packages are welcome and actively
wanted, provided they clear the same bar the existing ones aspire to:

- fast enough to rival C/Rust/Zig, minimal garbage collection overhead,
tested hard enough to prove it fully conforms with the format's official conformance suite (or, where no such
suite exists, with an explicit interop test against another language's
implementation)
- wired into the shared annotation deriver so users don't learn a new API per format
- and dependency-light enough to not raise eyebrows. Acceptance criteria in full are
under [Adding a new format](#adding-a-new-format).

---

## Packages

The repo is a workspace of `wireform-*` packages. Each is
self-contained and depends only on `wireform-core`, `wireform-derive`,
and whatever third-party libraries the format genuinely needs. If you
only need CBOR, you only build CBOR.

### Infrastructure

| Package | What it does |
|---------|------|
| [`wireform-core`](wireform-core/README.md) | Shared builder engine, FFI primitives, SIMD helpers |
| [`wireform-derive`](wireform-derive/README.md) | Annotation-driven TH deriver (`Modifier`, `BackendModifier`) |
| [`wireform-columnar`](wireform-columnar/README.md) | Columnar primitives shared by Arrow, Parquet, and ORC |

### Serialization formats

| Package | Format |
|---------|--------|
| [`wireform-proto`](wireform-proto/README.md) | Protocol Buffers (proto2/proto3) with IDL parser, codegen, JSON, well-known types |
| [`wireform-avro`](wireform-avro/README.md) | Apache Avro with schema resolution, container files, IPC |
| [`wireform-thrift`](wireform-thrift/README.md) | Apache Thrift (binary + compact protocol) |
| [`wireform-cbor`](wireform-cbor/README.md) | CBOR (RFC 8949) with CDDL schema |
| [`wireform-msgpack`](wireform-msgpack/README.md) | MessagePack with msgpack-rpc |
| [`wireform-bson`](wireform-bson/README.md) | BSON |
| [`wireform-ion`](wireform-ion/README.md) | Amazon Ion with ISL schema |
| [`wireform-bond`](wireform-bond/README.md) | Microsoft Bond (compact binary) |
| [`wireform-flatbuffers`](wireform-flatbuffers/README.md) | FlatBuffers (zero-copy) |
| [`wireform-capnproto`](wireform-capnproto/README.md) | Cap'n Proto (zero-copy) |
| [`wireform-fory`](wireform-fory/README.md) | Apache Fory (cross-language, wire-compatible with pyfory) |
| [`wireform-asn1`](wireform-asn1/README.md) | ASN.1 BER/DER |

### Text and config formats

| Package | Format |
|---------|--------|
| [`wireform-xml`](wireform-xml/README.md) | XML with SIMD SAX, zero-copy DOM, XPath, XSLT 1.0 subset |
| [`wireform-html`](wireform-html/README.md) | HTML5 tokenizer, tree builder, DOM, CSS selectors, streaming rewriter |
| [`wireform-toml`](wireform-toml/README.md) | TOML 1.0/1.1 |
| [`wireform-yaml`](wireform-yaml/README.md) | YAML 1.2 (100% conformance, no C deps, billion-laughs hardened) |
| [`wireform-edn`](wireform-edn/README.md) | Extensible Data Notation |
| [`wireform-bencode`](wireform-bencode/README.md) | BitTorrent bencode |
| [`wireform-csv`](wireform-csv/README.md) | CSV / TSV |
| [`wireform-ndjson`](wireform-ndjson/README.md) | Newline-delimited JSON |

### Analytics and table formats

| Package | Format |
|---------|--------|
| [`wireform-arrow`](wireform-arrow/README.md) | Apache Arrow IPC |
| [`wireform-parquet`](wireform-parquet/README.md) | Apache Parquet (read/write, page index, bloom filters) |
| [`wireform-orc`](wireform-orc/README.md) | Apache ORC (read/write, predicate pushdown) |
| [`wireform-iceberg`](wireform-iceberg/README.md) | Apache Iceberg (metadata, manifests, schema evolution) |
| [`wireform-delta`](wireform-delta/README.md) | Delta Lake (transaction log, time travel) |
| [`wireform-hudi`](wireform-hudi/README.md) | Apache Hudi (timeline reader) |
| [`wireform-lance`](wireform-lance/README.md) | Lance (data files, manifests) |

### Networking

| Package | What it does |
|---------|------|
| [`wireform-grpc`](wireform-grpc/README.md) | gRPC client and server |
| [`wireform-kafka`](wireform-kafka/README.md) | Native Kafka client with Streams, transactions, and OpenTelemetry |

### Internal tooling

| Package | What it does |
|---------|------|
| [`wireform-stats`](wireform-stats/README.md) | `regen-stats` keeps the per-package READMEs' tests / coverage / benchmark sections in sync with in-tree data, with light + dark SVG bar charts emitted via `wireform-xml` |

### Module conventions

Every per-format package follows the same layout:

```
<Format>.Encode / .Decode      -- wire codec primitives
<Format>.Class                 -- typeclass (ToCBOR, FromThrift, etc.)
<Format>.Derive                -- TH deriver consuming Modifier annotations
<Format>.Value                 -- dynamic value ADT (where applicable)
<Format>.JSON                  -- JSON bridge (where applicable)
```

Formats with an IDL also have `<Format>.Parser` and
`<Format>.CodeGen`. See each package's README for specifics.

---

## The deriver

One annotated Haskell record drives instance generation for every
format. Annotations live in `Wireform.Derive.Modifier`:

```haskell
data Person = Person
  { personFullName :: !Text
  , personAge      :: !Word32
  , personBalance  :: !Int64
  , personSecret   :: !Text
  } deriving stock (Show, Eq, Generic)

{-# ANN type Person ("Person" :: String) #-}

{-# ANN personFullName (tag 1) #-}
{-# ANN personAge      (tag 2) #-}
{-# ANN personBalance  (tag 3) #-}
{-# ANN personSecret   (tag 4) #-}

{-# ANN personFullName (renameStyle SnakeCase) #-}
{-# ANN personAge      (renameStyle SnakeCase) #-}
{-# ANN personBalance  (renameStyle SnakeCase) #-}
{-# ANN personSecret   (renameStyle SnakeCase) #-}

{-# ANN personFullName (forBackend backendJSON (rename "fullName")) #-}
{-# ANN personSecret   (forBackend backendJSON skip) #-}

DProto.deriveProto    ''Person
DCBOR.deriveCBOR      ''Person
DMP.deriveMsgPack     ''Person
DAeson.deriveJSON     ''Person
```

`personFullName` becomes `full_name` on every binary wire but
`fullName` in JSON. `personSecret` is omitted from JSON entirely.

The annotation vocabulary:

- `rename`, `renameStyle SnakeCase`, `renameIdiomatic` for wire keys
- `tag N` for field numbers (proto, Bond, Thrift, Iceberg)
- `skip`, `defaults`, `required`, `optional`, `flatten`,
  `wireOverride WireZigZag`
- `forBackend backendJSON (rename "x")` for per-format overrides
- `extension XmlFieldOpt` for typed per-backend configuration

Formats that need knobs beyond the core vocabulary use
`BackendModifier` extensions (`XmlFieldOpt`, `HtmlFieldOpt`,
`Asn1Tag`). See `wireform-derive` for the full API.

---

## Building

```bash
cabal update
cabal build all
```

A Nix flake is provided:

```bash
nix develop          # default (GHC 9.8)
nix develop .#ghc96  # GHC 9.6
nix develop .#ghc910 # GHC 9.10
```

LLVM is off by default (`-fasm` in `cabal.project`). Production
builds benefit from `-fllvm` (up to 27% faster on tight loops).

---

## Testing

```bash
cabal test all
```

Format packages have opt-in interop tests against upstream
conformance suites (silent skip when the runner isn't installed):

- `wireform-proto`: [protobuf conformance suite](https://github.com/protocolbuffers/protobuf/tree/main/conformance) (2675/2675)
- `wireform-toml`: [toml-test](https://github.com/toml-lang/toml-test)
- `wireform-yaml`: [yaml-test-suite](https://github.com/yaml/yaml-test-suite)
- `wireform-iceberg`: pyiceberg + fastavro
- `wireform-delta`: delta-rs
- `wireform-hudi`: hudi-rs
- `wireform-lance`: pylance
- `wireform-fory`: pyfory
- `wireform-kafka`: live broker via `WIREFORM_KAFKA_BROKER=host:port`

---

## Examples

Run from the workspace root with `cabal run <name>`:

| Example | What it shows |
|---------|---------------|
| `example-derive` | One record, four formats (proto + CBOR + MsgPack + JSON) |
| `example-th` | `loadProto` from a `.proto` file |
| `example-custom-repr` | Lazy/short bytes, list-backed repeated fields |
| `example-msgpack` / `cbor` / `bson` / `edn` / `ion` | Schema-less binary formats |
| `example-thrift` / `avro` / `capnproto` / `flatbuffers` / `bond` / `asn1` | Schema-driven IDL formats |
| `example-xml` | XML encode/decode |
| `example-parquet` / `arrow` / `iceberg` | Analytics formats |

See `examples/` for the full list.

---

## License

BSD-3-Clause. See [LICENSE](LICENSE).
