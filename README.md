# wireform

**One Haskell library for serialization, schema parsing, code generation,
streaming, RPC framing, container I/O, and analytics metadata** — across
Protocol Buffers, Avro, Thrift, MessagePack, CBOR, XML, HTML, and 15+ more
wire formats.

> **New here?** Start with **[docs/getting-started.md](docs/getting-started.md)**
> — run an example in two minutes, then wire your own Cabal package.

---

## Table of contents

- [Try it now](#try-it-now)
- [Choose your path](#choose-your-path)
- [What you can do](#what-you-can-do)
- [Supported formats at a glance](#supported-formats-at-a-glance)
- [Quick start snippets](#quick-start-snippets)
- [Beyond encode / decode](#beyond-encode--decode)
- [Code generation (`wireform-gen`)](#code-generation-wireform-gen)
- [Runnable examples](#runnable-examples)
- [Installation](#installation)
- [Performance](#performance)
- [Why one library?](#why-one-library)
- [Packages in this repo](#packages-in-this-repo)
- [Development](#development)
- [GHC compatibility](#ghc-compatibility)
- [License](#license)

---

## Try it now

```bash
git clone https://github.com/iand675/wireform-.git && cd wireform-
cabal update
cabal run example-msgpack     # Generics-derived MessagePack — quickest demo
cabal run example-basic       # hand-written protobuf message
cabal run example-xml         # Generic XML encode/decode
```

If `cabal run` fails, `cabal build wireform` and check for a missing C
compiler (the bundled `cbits/` needs one).

---

## Choose your path

| I want to… | Start with |
|------------|------------|
| Serialize Haskell types, no schema files | `MsgPack.Class` / `CBOR.Class` / `BSON.Class` — [Generic deriving](#generic-deriving) |
| Use `.proto` files, interop with other languages | `Proto.TH` / `Proto.QQ` / `wireform-gen proto` — [Protobuf](#protocol-buffers) |
| Generate code from Avro / Thrift / Bond / XSD / … | `wireform-gen` — [codegen CLI](#code-generation-wireform-gen) |
| Stream protobuf or MsgPack over sockets | `Proto.Decode.Stream` / `MsgPack.Stream` — [streaming](#streaming-decoders) |
| Read / write Avro container files | `Avro.Container` — [container I/O](#avro-containers-and-schema-resolution) |
| Frame gRPC or Thrift RPC messages | `Proto.GRPC` / `Thrift.Message` — [RPC framing](#rpc-framing) |
| Parse & query XML/HTML documents | `XML.SAX` / `XML.Path` / `HTML.Query` — [XML pipeline](#xml-pipeline) / [HTML](#html) |
| Inspect Parquet / ORC / Arrow / Iceberg metadata | `Parquet.Footer` / `ORC.Footer` / `Arrow.IPC` / `Iceberg.*` — [analytics formats](#analytics-table-formats) |
| Convert between formats at the value level | Decode one format's `Value`, map to another's, re-encode |

---

## What you can do

This is not just an encode/decode library. Here is the full surface by
category.

### Encode / decode

Every format has at least `Encode` and `Decode` modules. Schema-backed formats
(`Proto`, `Avro`, `Thrift`, `CBOR` via CDDL, `Ion` via ISL, `CapnProto`,
`FlatBuffers`, `Bond`, `ASN1`, `XML` via XSD) additionally parse the schema
and drive codegen.

### Generic deriving (no schema files)

Derive encode/decode from `GHC.Generics` for schema-less formats:

| Class module | Classes |
|-------------|---------|
| `MsgPack.Class` | `ToMsgPack` / `FromMsgPack` |
| `CBOR.Class` | `ToCBOR` / `FromCBOR` |
| `BSON.Class` | `ToBSON` / `FromBSON` |
| `EDN.Class` | `ToEDN` / `FromEDN` |
| `Ion.Class` | `ToIon` / `FromIon` |
| `Avro.Class` | `ToAvro` / `FromAvro` |
| `Thrift.Class` | `ToThrift` / `FromThrift` |
| `Bencode.Class` | `ToBencode` / `FromBencode` |
| `TOML.Class` | `ToTOML` / `FromTOML` |
| `CSV.Class` | `ToCSV` / `FromCSV` |
| `XML.Class` | `ToXML` / `FromXML` |
| `HTML.Class` | `ToHTML` / `FromHTML` |

### Streaming decoders

| Module | What it does |
|--------|-------------|
| `Proto.Decode.Stream` | Lazy stream of varint-length-delimited messages; incremental `IDecode` with `feedChunk` |
| `MsgPack.Stream` | Incremental one-value-at-a-time MessagePack (handles Timestamp extension) |
| `CBOR.Stream` | Incremental CBOR value decoding |

### RPC framing

| Module | Protocol |
|--------|----------|
| `Proto.GRPC` | gRPC length-prefixed framing (`grpcFrame` / `grpcUnframe` / `grpcFrameMany`) |
| `Thrift.Message` | Binary + Compact protocol RPC headers (method, call/reply/exception/oneway, seq id) |
| `Thrift.Transport` | 4-byte BE framed transport; `unframeMessages` for streamed connections |
| `MsgPack.RPC` | msgpack-rpc request/response/notification arrays |
| `Avro.Protocol` | Avro IPC protocol AST, handshake request/response, MD5 fingerprinting |

A full **gRPC client/server** is in the companion `wireform-grpc` package.

### Container file I/O

| Module | What it reads/writes |
|--------|---------------------|
| `Avro.Container` | Avro Object Container Files (`null` / `deflate` / optional `snappy` codecs); `readContainerResolved` for reader-schema resolution |
| `Parquet.Footer` + `Parquet.Page` + `Parquet.Read` | Footer metadata; Thrift page headers; PLAIN column reads (INT32/INT64/FLOAT/DOUBLE/BOOL/BYTE_ARRAY), dictionary-encoded INT32, GZip/Snappy/uncompressed |
| `Arrow.IPC` + `Arrow.Column` | IPC framing + schema encode/decode; flat record-batch materialization (primitives + nullable validity bitmaps) |
| `ORC.Footer` + `ORC.Read` + `ORC.Stripe` | Postscript + protobuf footer; stripe bytes; stripe footer streams; `stripeColumnStreams` splits stream payloads |
| `Iceberg.JSON` + `Iceberg.Read` | Table metadata JSON; Avro manifest / manifest-list readers; helpers for data-file paths (`manifestFilePaths`, `manifestEntryParquetPaths`, …) |

### Schema resolution and evolution

| Module | What it does |
|--------|-------------|
| `Avro.Resolution` | Full Avro schema resolution (promotions, records, enums, unions, fixed, logical types) |
| `Proto.Compat` | Proto2 ↔ proto3 field presence / default-value semantics |
| `Proto.Merge` | Protobuf message merging (last-write-wins scalars, concatenated repeated fields) |

### Dynamic / untyped messages

| Module | What it does |
|--------|-------------|
| `Proto.Dynamic` | Wire encode/decode to `Map FieldNumber DynamicValue` without generated types; `dynamicToJson` |
| `Proto.TextFormat` | Protobuf text format (`.pbtxt`) encode/decode |

### Code generation and TH

| Tool | IDLs |
|------|------|
| `wireform-gen` CLI | `.proto`, `.avsc` / `.avdl`, `.thrift`, `.bond`, `.capnp`, `.fbs`, ASN.1, XSD |
| `Proto.TH` / `Proto.QQ` | Compile-time `.proto` → Haskell types |
| `Proto.Setup` | Cabal setup hook for pre-build codegen |
| `protoc-gen-wireform` | `protoc` plugin (`--wireform_out`) |

Per-format quasiquoters: `Proto.QQ`, `Avro.QQ`, `Thrift.QQ`, `CBOR.QQ`,
`Ion.QQ`, `CapnProto.QQ`, `FlatBuffers.QQ`, `Bond.QQ`, `ASN1.QQ`, `XML.QQ`.

### XML pipeline

XML support goes well beyond encode/decode:

| Module | Capability |
|--------|-----------|
| `XML.SAX` | SIMD-accelerated SAX event stream; `parseSAX`, `parseSAXStream`, `foldSAX` |
| `XML.FastDOM` | Zero-copy DOM (spans into original `ByteString`); `toDocument` to materialize |
| `XML.Incremental` | Chunk-fed parsing; **concurrent parse** with `TBQueue` (`withConcurrentParse`) |
| `XML.Path` | XPath-lite queries (axes, predicates, `parsePath` / `query`) |
| `XML.DSL` | Composable query operators (`/>`, `//>`, `\|>`) |
| `XML.XSLT` | Subset XSLT 1.0 transforms (templates, for-each, if/choose, value-of, copy-of, …) |
| `XML.Generic` | `GHC.Generics`-based XML mapping |

### HTML

| Module | Capability |
|--------|-----------|
| `HTML.Parse` | Full HTML5 tree construction (spec-compliant tokenizer + parser) |
| `HTML.Encode` | SIMD-accelerated serialization |
| `HTML.Query` | CSS selectors: tag, `.class`, `#id`, descendant chains; `querySelector`, `querySelectorAll` |
| `HTML.Class` | `ToHTML` / `FromHTML` with Generic deriving |

### CBOR extras

| Module | Capability |
|--------|-----------|
| `CBOR.Diagnostic` | RFC 8949 diagnostic notation (human-readable CBOR dumps) |
| `CBOR.TagRegistry` | Extensible tag handlers with validation; default tags 0–3 (datetime, epoch, bignums) |

### NDJSON extras

| Module | Capability |
|--------|-----------|
| `NDJSON.Decode` | `decodeStream` (per-line callback), `decodeConcurrent` (producer/consumer via `TBQueue`) |

---

## Supported formats at a glance

| Format | Modules | Encode | Decode | JSON | IDL / Schema | Codegen | Class |
|--------|---------|--------|--------|------|--------------|---------|-------|
| Protocol Buffers | `Proto.*` | yes | yes | yes | `.proto` | yes | — |
| Apache Avro | `Avro.*` | yes | yes | yes | `.avsc`/`.avdl` | yes | yes |
| Apache Thrift | `Thrift.*` | yes | yes | yes | `.thrift` | yes | yes |
| CBOR | `CBOR.*` | yes | yes | yes | CDDL | yes | yes |
| MessagePack | `MsgPack.*` | yes | yes | yes | — | — | yes |
| BSON | `BSON.*` | yes | yes | — | — | — | yes |
| Amazon Ion | `Ion.*` | yes | yes | — | ISL | yes | yes |
| Cap'n Proto | `CapnProto.*` | yes | yes | — | `.capnp` | yes | — |
| FlatBuffers | `FlatBuffers.*` | yes | yes | — | `.fbs` | yes | — |
| Microsoft Bond | `Bond.*` | yes | yes | — | `.bond` | yes | — |
| ASN.1 BER/DER | `ASN1.*` | yes | yes | — | ASN.1 | yes | — |
| EDN | `EDN.*` | yes | yes | yes | — | — | yes |
| XML | `XML.*` | yes | yes | — | XSD | yes | yes |
| Bencode | `Bencode.*` | yes | yes | — | — | — | yes |
| TOML | `TOML.*` | yes | yes | — | — | — | yes |
| HTML | `HTML.*` | parse | yes | — | — | — | yes |
| CSV / TSV | `CSV.*` | yes | yes | — | — | — | yes |
| NDJSON | `NDJSON.*` | yes | yes | — | — | — | — |
| Apache Parquet | `Parquet.*` | footer | footer + column pages | — | — | — | — |
| Apache Arrow IPC | `Arrow.*` | framing | framing + schema + `Arrow.Column` | — | — | — | — |
| Apache Iceberg | `Iceberg.*` | — | metadata + manifests | yes | — | — | — |
| Apache ORC | `ORC.*` | footer | footer + stripes + stream split | — | — | — | — |

**Re-export hubs** (`Wireform.*`): `Proto`, `MsgPack`, `CBOR`, `Avro`,
`Thrift`, `XML`, `HTML`, `CSV`, `NDJSON`, `Bencode`, `TOML`, `EDN`, `BSON`,
`Ion`, `Bond`, `CapnProto`, `FlatBuffers`, `ASN1`, `Parquet`, `Arrow`, `Iceberg`, `ORC`.

---

## Quick start snippets

### Protocol Buffers

```haskell
import qualified Wireform.Proto as P

let bytes = P.encodeMessage myMessage
let decoded = P.decodeMessage bytes :: Either P.DecodeError MyMsg
```

**Template Haskell** — load `.proto` at compile time:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Proto.TH (loadProto)

$(loadProto "proto/person.proto")
```

**Quasiquoter** — inline proto:

```haskell
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
import Proto.QQ (proto)

[proto|
  syntax = "proto3";
  message SearchRequest {
    string query = 1;
    int32 page_number = 2;
  }
|]
```

### MessagePack (Generics — no schema)

```haskell
{-# LANGUAGE DeriveGeneric, DerivingStrategies, DeriveAnyClass #-}
import Data.Text (Text)
import GHC.Generics (Generic)
import MsgPack.Class (ToMsgPack, FromMsgPack, encodeMsgPack, decodeMsgPack)

data Person = Person { name :: !Text, age :: !Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToMsgPack, FromMsgPack)

main :: IO ()
main = do
  let bytes = encodeMsgPack (Person "Ada" 36)
  print (decodeMsgPack bytes :: Either String Person)
```

### Avro (schema-driven)

```haskell
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Avro.Value as AV
import Avro.Encode (encodeAvro)
import Avro.Decode (decodeAvro)
import Avro.Schema

let schema = AvroRecord
      { avroRecordName = "Person", avroRecordNamespace = Nothing
      , avroRecordDoc = Nothing, avroRecordAliases = V.empty
      , avroRecordFields = V.fromList
          [ AvroField "name" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
          , AvroField "age"  (AvroPrimitive AvroInt)    Nothing Nothing V.empty Nothing Map.empty
          ]
      , avroRecordProps = Map.empty
      }
    val   = AV.Record (V.fromList [AV.String "Alice", AV.Int 30])
    bytes = encodeAvro schema val

case decodeAvro schema bytes of
  Right v  -> print v
  Left err -> putStrLn err
```

### Generic deriving (multi-format)

```haskell
data Person = Person { name :: Text, age :: Int }
  deriving stock Generic
  deriving anyclass (ToMsgPack, FromMsgPack, ToCBOR, FromCBOR, ToBSON, FromBSON)
```

The same type can derive codecs for as many formats as you need.

---

## Beyond encode / decode

### Streaming protobuf

```haskell
import Proto.Decode.Stream (decodeMessageStream)

let messages = decodeMessageStream lazyBytes :: [Either DecodeError MyMsg]
```

Or incremental with `feedChunk` for chunk-at-a-time I/O.

### Avro containers and schema resolution

```haskell
import Avro.Container (readContainer, readContainerResolved)

records <- readContainer bytes             -- writer schema only
records <- readContainerResolved readerSchema bytes  -- with resolution
```

Supports `null`, `deflate`, and (with the `snappy` flag) `snappy` codecs.

### gRPC framing

```haskell
import Proto.GRPC (grpcFrame, grpcUnframe)

let framed = grpcFrame (P.encodeMessage req)
let Right payload = grpcUnframe framed
```

Full client/server: `wireform-grpc` package.

### XML: SAX → query → transform

```haskell
import XML.SAX (parseSAX)
import XML.Path (query, parsePath)
import XML.XSLT (applyStylesheet)

events <- parseSAX xmlBytes               -- SAX event stream
nodes  <- query (parsePath "//item/@id")  -- XPath-lite
result <- applyStylesheet sheet doc       -- XSLT 1.0 subset
```

Also: `XML.FastDOM` (zero-copy DOM), `XML.Incremental` (chunk-fed +
concurrent), `XML.DSL` (composable query operators).

### HTML: parse → query

```haskell
import HTML.Parse (parseHTML)
import HTML.Query (querySelectorAll)

let doc = parseHTML htmlBytes
    links = querySelectorAll "a.external" doc
```

### Parquet metadata inspection

```haskell
import Parquet.Footer (readFooter)
import Parquet.Types (fmSchema, fmRowGroups)

let Right meta = readFooter parquetBytes
    schema = fmSchema meta
    rowGroups = fmRowGroups meta
```

### Parquet predicate pushdown — page index + bloom filter

```haskell
import Parquet.Read (loadParquetFile)
import Parquet.PageIndex (readOffsetIndex, readColumnIndex)
import Parquet.BloomFilter
  (decodeBloomFilter, sbbfCheck, newSbbf, sbbfInsert, encodeBloomFilter)

let Right pf  = loadParquetFile parquetBytes
Right mOI <- pure (readOffsetIndex pf 0 0)         -- per-page byte offsets
Right mCI <- pure (readColumnIndex pf 0 0)         -- per-page min/max + nulls

-- Build a bloom filter and check membership
let sbbf = foldr (sbbfInsert . encodeUtf8) (newSbbf 1024) keys
    bs   = encodeBloomFilter sbbf
Right (_, sbbf') <- pure (decodeBloomFilter bs)
print (sbbfCheck (encodeUtf8 "needle") sbbf')      -- True / False
```

### Iceberg table metadata

```haskell
import Iceberg.JSON (metadataFromJSON)
import Iceberg.Read (readManifestEntries, readManifestList)

let Right tableMeta = metadataFromJSON jsonBytes
entries <- readManifestEntries manifestAvroBytes
```

### Dynamic protobuf (no generated types)

```haskell
import Proto.Dynamic (decodeDynamic, dynamicToJson)

let Right dyn = decodeDynamic wireBytes
    json = dynamicToJson dyn
```

### CBOR diagnostic notation

```haskell
import CBOR.Diagnostic (toDiagnostic)

putStrLn (toDiagnostic cborValue)   -- RFC 8949 §8 human-readable output
```

---

## Code generation (`wireform-gen`)

```bash
cabal exec wireform-gen -- --help
```

| Command | IDL |
|---------|-----|
| `wireform-gen proto -i f.proto -o gen/` | Protocol Buffers |
| `wireform-gen avro -i f.avsc -o gen/` | Avro (`.avsc` or `.avdl` via `--format`) |
| `wireform-gen thrift -i f.thrift -o gen/` | Thrift |
| `wireform-gen bond -i f.bond -o gen/` | Bond |
| `wireform-gen capnp -i f.capnp -o gen/` | Cap'n Proto |
| `wireform-gen fbs -i f.fbs -o gen/` | FlatBuffers |
| `wireform-gen asn1 -i f.asn1 -o gen/` | ASN.1 |
| `wireform-gen xsd -i f.xsd -o gen/` | XML Schema |

Common flags: `-m MODULE_PREFIX`, `-I INCLUDE_DIR` (proto).

Proto also has `wireform-gen proto print` (exact-print) and `wireform-gen proto
summary` (structural summary).

**`protoc` plugin:** `protoc-gen-wireform` — use with `protoc --wireform_out=DIR`.

---

## Runnable examples

| Command | What it shows |
|---------|---------------|
| `example-msgpack` | `Generics` MsgPack — best first demo |
| `example-cbor` | `Generics` CBOR |
| `example-bson` | `Generics` BSON |
| `example-edn` | `Generics` EDN |
| `example-ion` | `Generics` Ion |
| `example-xml` | `Generics` XML |
| `example-basic` | Hand-written protobuf `MessageEncode` / `MessageDecode` |
| `example-protobuf` | Low-level `Proto.Wire` |
| `example-th` | `loadProto` Template Haskell |
| `example-qq` | `Proto.QQ` quasiquoter |
| `example-codegen` | `wireform-gen` in-process |
| `example-custom-repr` | Custom field backing types |
| `example-wellknown` | Well-known protobuf types |
| `example-any` | `google.protobuf.Any` pack/unpack |
| `example-avro` | Avro schema + value API |
| `example-thrift` | Thrift binary + compact |
| `example-capnproto` | Cap'n Proto struct + list |
| `example-flatbuffers` | FlatBuffers table + vector |
| `example-bond` | Bond compact binary |
| `example-asn1` | ASN.1 DER encode / BER decode |
| `example-parquet` | Parquet footer metadata roundtrip |
| `example-arrow` | Arrow IPC schema message |
| `example-iceberg` | Iceberg table metadata JSON |

Run any of them with `cabal run <name>`.

---

## Installation

```cabal
build-depends: wireform ^>=0.1
```

Or use a path dependency — see **[docs/getting-started.md — Step
3](docs/getting-started.md#step-3--use-wireform-from-your-cabal-package)** for
a copy-paste `cabal.project` + `.cabal` + `Main.hs`.

**Flags:** `snappy` (Avro Snappy codec, off by default), `python-interop`
(conformance tests, dev only). The Nix shell enables `snappy`.

---

## Performance

Benchmarks vs common Haskell libraries (small struct, ~60–80 bytes). Treat as
order-of-magnitude guidance.

| Benchmark | wireform | competitor | speedup |
|-----------|----------|------------|---------|
| Proto encode | ~120 ns | ~450 ns (proto-lens) | ~3.8× |
| Proto decode | ~80 ns | ~350 ns (proto-lens) | ~4.4× |
| MsgPack encode | ~150 ns | ~900 ns (msgpack) | ~6× |
| MsgPack decode | ~200 ns | ~1.2 µs (msgpack) | ~6× |
| CBOR encode | ~180 ns | ~350 ns (cborg) | ~1.9× |
| CBOR decode | ~220 ns | ~500 ns (cborg) | ~2.3× |

```bash
cabal bench compare-bench    # protobuf vs proto-lens
cabal bench format-bench     # MsgPack vs msgpack, CBOR vs cborg
cabal bench xml-bench        # XML vs xml-conduit, xeno, hexml
```

---

## Why one library?

Separate packages per format (`proto-lens`, `cborg`, `msgpack`, …) each bring
their own API idioms, performance floor, build pipeline, and streaming story.
wireform shares:

- **Encoding** — sized two-pass encoding, `SizedBuilder`, direct buffer writes
- **Decoding** — `Addr#` cursor decoders, unboxed sums, C FFI + SIMD acceleration
- **Codegen** — one CLI (`wireform-gen`) for 8 IDLs
- **Generic classes** — 12 `Class` modules with the same `GHC.Generics` pattern
- **Module layout** — `Format.Value` / `Encode` / `Decode` / `JSON` everywhere

---

## Packages in this repo

| Package | Role |
|---------|------|
| `wireform` | Core library: all codecs, parsers, codegen, TH/QQ, streaming, containers, benchmarks |
| `wireform-grpc` | Native gRPC client/server using wireform for protobuf serialization |

---

## Development

Nix flake dev shells (GHC 9.6 / 9.8 / 9.10):

```bash
nix develop            # default: GHC 9.8
nix develop .#ghc96
nix develop .#ghc910
```

Includes Cabal, HLS, ghciwatch, fourmolu, hlint, prek. Haskell deps for
`wireform` are pre-built via Nix.

```bash
cabal build all
cabal test wireform-test    # ~1400 tests
```

Contributor notes: [AGENTS.md](AGENTS.md). Troubleshooting:
[docs/getting-started.md#troubleshooting](docs/getting-started.md#troubleshooting).

---

## GHC compatibility

`GHC2021`. Tested with GHC 9.6.4 and 9.8.4; Nix flake also provides 9.10.

---

## License

BSD-3-Clause. See [NOTICE](NOTICE) for third-party attributions.
