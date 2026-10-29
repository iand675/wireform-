# Get started with wireform

Working code before theory. The wireform repo is the fastest playground; your
own Cabal package is only a few lines more.

---

## What wireform is

wireform is a single Haskell library covering **22 serialization formats** with:

- **Encode / decode** for every format, with a `Value` type where appropriate
- **Generic deriving** (`GHC.Generics`) for 12 schema-less formats
- **IDL parsers + code generation** for 8 schema languages (`.proto`, `.avsc`,
  `.thrift`, `.bond`, `.capnp`, `.fbs`, ASN.1, XSD)
- **Streaming decoders** (protobuf, MessagePack, CBOR, XML)
- **RPC framing** (gRPC, Thrift binary/compact, MsgPack-RPC, Avro IPC)
- **Container file I/O** (Avro OCF with codec support, Parquet/ORC/Arrow metadata)
- **XML pipeline** (SIMD SAX, XPath queries, XSLT transforms, concurrent parse)
- **HTML5** parser + CSS selectors
- **Analytics format readers** (Parquet pages, Iceberg manifests, ORC stripes)

The APIs rhyme across formats: `Format.Value`, `Format.Encode`,
`Format.Decode`, `Format.Class`, `Format.JSON`. Pick one format (or several).

---

## Prerequisites

| You need | Notes |
|----------|--------|
| **GHC** | 9.10 or newer (CI: 9.10, 9.12, 9.14); `GHC2021`. |
| **cabal-install** | 3.x |
| **Optional: Nix** | `nix develop` gives pinned GHC + pre-built deps — see [README](../README.md#development). |

No Docker, no `protoc`, and no language-specific runtimes needed below.

---

## Step 1 — Run something (two minutes)

```bash
cd wireform-
cabal update
cabal run example-msgpack
```

Output: a `Person` record round-tripped through MessagePack with `Generics` —
no `.proto` file, no schema, no codegen. This is the lowest-ceremony path.

Now try a few more to see the range:

```bash
cabal run example-basic       # hand-written protobuf message instances
cabal run example-xml         # Generic XML encode/decode
cabal run example-avro        # Avro schema + value API
cabal run example-parquet     # Parquet footer metadata roundtrip
```

**If builds fail:** `cabal build wireform` and read the first error. A missing
C compiler for `cbits/` is the usual issue on minimal systems.

The [README](../README.md#runnable-examples) has the full list of 23 example
executables.

---

## Step 2 — Pick a workflow

### A. Schema-less Haskell types (MessagePack, CBOR, BSON, EDN, Ion, Bencode, TOML, CSV, XML, HTML)

1. Define records with `Generic`.
2. Derive the format's classes.
3. Call the typed encode/decode.

```haskell
import MsgPack.Class (ToMsgPack, FromMsgPack, encodeMsgPack, decodeMsgPack)

data Person = Person { name :: !Text, age :: !Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToMsgPack, FromMsgPack)

let bytes = encodeMsgPack (Person "Ada" 36)
```

Same pattern for `ToCBOR`/`FromCBOR`, `ToBSON`/`FromBSON`, `ToXML`/`FromXML`,
`ToHTML`/`FromHTML`, `ToTOML`/`FromTOML`, etc. You can derive multiple at once:

```haskell
  deriving anyclass (ToMsgPack, FromMsgPack, ToCBOR, FromCBOR, ToBSON, FromBSON)
```

**Best when:** you control both ends of the wire and want little ceremony.

### B. Protocol Buffers (`.proto` is the source of truth)

Three codegen options:

| Approach | When to use |
|----------|-------------|
| `$(loadProto "file.proto")` | Small projects, TH is fine, want types in the same module |
| `wireform-gen proto -i file.proto -o gen/` | Larger projects, CI-friendly, committed generated code |
| `protoc --wireform_out=DIR` | Your org standardizes on `protoc` |

Then:

```haskell
import qualified Wireform.Proto as P
let bytes = P.encodeMessage myMessage
```

**Best when:** you interoperate with other languages, use gRPC, or need field
evolution.

### C. Schema-first documents (Avro, CDDL, Ion ISL, XSD, …)

1. Load or build a schema.
2. Encode/decode `Value` trees against it.
3. Or generate typed Haskell with `wireform-gen`.

**Best when:** data pipelines, registries, Avro container files, analytics.

### D. Analytics metadata (Parquet, Arrow, Iceberg, ORC)

These formats have read-only or metadata-only support:

```haskell
import Parquet.Footer (readFooter)
import Iceberg.JSON (metadataFromJSON)
import ORC.Footer (readORCFooter)
```

**Best when:** you need to inspect file layouts, extract schemas, or route
data without pulling in heavyweight JVM tooling.

### E. XML / HTML document processing

wireform includes a full pipeline, not just encode/decode:

| Step | Module |
|------|--------|
| SAX event stream | `XML.SAX` (SIMD-accelerated) |
| Zero-copy DOM | `XML.FastDOM` |
| Tree DOM | `XML.Decode` |
| XPath queries | `XML.Path` |
| CSS selectors (HTML) | `HTML.Query` |
| XSLT transforms | `XML.XSLT` |
| Concurrent chunk parsing | `XML.Incremental` |

**Best when:** scraping, config parsing, XML pipeline tooling.

---

## Step 3 — Use wireform from your Cabal package

### 3a. Path dependency (typical while hacking or before Hackage)

```text
~/Code/
  wireform-/          # this repo
  my-app/
    cabal.project
    my-app.cabal
    app/Main.hs
```

**`cabal.project`**:

```cabal
packages:
  .
  ../wireform-
```

**`my-app.cabal`**:

```cabal
cabal-version:   3.0
name:            my-app
version:         0.1.0.0
build-type:      Simple

executable my-app
  main-is:          Main.hs
  hs-source-dirs:   app
  default-language:  GHC2021
  build-depends:
      base       ^>=4.18
    , text       ^>=2.0
    , bytestring ^>=0.11
    , wireform
  default-extensions:
    OverloadedStrings
    DeriveGeneric
    DerivingStrategies
    DeriveAnyClass
```

**`app/Main.hs`**:

```haskell
module Main where

import Data.Text (Text)
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import MsgPack.Class (ToMsgPack, FromMsgPack, encodeMsgPack, decodeMsgPack)

data Person = Person
  { name :: !Text
  , age  :: !Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToMsgPack, FromMsgPack)

main :: IO ()
main = do
  let bytes = encodeMsgPack (Person "Ada" 36)
  putStrLn $ "Encoded to " <> show (BS.length bytes) <> " bytes"
  case decodeMsgPack bytes of
    Right p -> print (p :: Person)
    Left e  -> putStrLn $ "decode failed: " <> e
```

```bash
cd ~/Code/my-app && cabal run my-app
```

### 3b. Hackage dependency

```cabal
build-depends: wireform ^>=0.1
```

---

## Step 4 — Template Haskell for protobufs

1. Put `.proto` files under `proto/` (or project root).
2. Enable `TemplateHaskell`.
3. Import and splice:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Proto.TH (loadProto)
$(loadProto "proto/person.proto")
```

For imports across files, pass include dirs:

```haskell
import Proto.TH (loadProtoWith, defaultLoadOpts, LoadOpts(..))
$(loadProtoWith defaultLoadOpts { loIncludeDirs = ["proto", "."] } "proto/api.proto")
```

Reference: `cabal run example-th` in the wireform repo.

---

## Step 5 — Code generation with `wireform-gen`

No global install needed:

```bash
cabal exec wireform-gen -- proto -i proto/person.proto -o gen/
cabal exec wireform-gen -- avro -i schemas/user.avsc -o gen/
cabal exec wireform-gen -- thrift -i service.thrift -o gen/
```

Then add `gen` to `hs-source-dirs` and list modules in your `.cabal`.

Full subcommand list: `proto`, `avro`, `thrift`, `bond`, `capnp`, `fbs`,
`asn1`, `xsd`. See [README](../README.md#code-generation-wireform-gen).

---

## Step 6 — Go deeper

After basic encode/decode works, these are the modules you reach for:

| Goal | Module | What it does |
|------|--------|-------------|
| Stream protobuf messages over a socket | `Proto.Decode.Stream` | Lazy list of varint-length messages, or incremental `feedChunk` |
| Stream MessagePack values | `MsgPack.Stream` | One-value-at-a-time incremental decode |
| Read Avro container files | `Avro.Container` | OCF with `null`/`deflate`/`snappy` codecs |
| Avro schema evolution | `Avro.Resolution` | Full reader/writer schema resolution |
| gRPC framing (without full server) | `Proto.GRPC` | `grpcFrame` / `grpcUnframe` |
| Thrift RPC headers | `Thrift.Message` | Binary + Compact protocol method/seqid framing |
| MsgPack RPC | `MsgPack.RPC` | Request/response/notification arrays |
| Dynamic protobuf (no generated types) | `Proto.Dynamic` | Decode to `Map FieldNumber DynamicValue` |
| Protobuf text format | `Proto.TextFormat` | `.pbtxt` encode/decode |
| CBOR diagnostic dumps | `CBOR.Diagnostic` | RFC 8949 human-readable notation |
| SAX events from XML | `XML.SAX` | SIMD-accelerated event stream |
| XPath queries | `XML.Path` | Axes, predicates, `query` |
| XSLT transforms | `XML.XSLT` | Subset XSLT 1.0 |
| CSS selectors on HTML | `HTML.Query` | `querySelector`, `querySelectorAll` |
| Concurrent XML parsing | `XML.Incremental` | Chunk-fed, `TBQueue`-based |
| Parquet metadata | `Parquet.Footer` / `Parquet.Read` | Schema, row groups, column statistics, page reads |
| Iceberg table metadata | `Iceberg.JSON` / `Iceberg.Read` | Table JSON + Avro manifest reading |
| ORC footer + stripes | `ORC.Footer` / `ORC.Read` | Postscript, type tree, stripe slicing |
| Arrow IPC | `Arrow.IPC` | Schema + record batch framing |

---

## Troubleshooting

| Symptom | Likely cause | Fix |
|---------|-------------|-----|
| `loadProto` cannot find file | Wrong cwd or path | Paths are relative to package root; `cabal run` from there. |
| Proto import not found | Missing include dir | `loIncludeDirs` in `loadProtoWith`, or `-I` for `wireform-gen proto`. |
| `Could not find module 'Proto.…'` | Generated code not wired | Add `gen` to `hs-source-dirs`; list modules in `.cabal`. |
| Link errors / missing C symbols | No C compiler | Install gcc/clang; on macOS, Xcode CLI tools. |
| Bounds / solver failures | Version mismatch | Match `base`/`text` to your GHC; or use the path dependency. |

---

## What to read next

| Goal | Where |
|------|-------|
| Full format matrix, benchmarks, capability tables | [README](../README.md) |
| All 23 example executables | [README — Runnable examples](../README.md#runnable-examples) |
| Contributor guidelines, decoder style, perf discipline | [AGENTS.md](../AGENTS.md) |
