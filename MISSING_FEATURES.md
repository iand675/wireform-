# Feature Gap Analysis: wireform vs Other Protobuf Libraries

Analysis comparing wireform against proto-lens (Haskell), prost (Rust), and the
official Google protobuf runtimes (Java, Python, Go, C++).

## Implementation Status

| # | Feature | Status | Module(s) |
|---|---------|--------|-----------|
| 1 | gRPC Service Stub Generation | **Implemented** | `Proto.CodeGen.Service` |
| 2 | protoc Plugin Mode | **Implemented** | `Proto.Google.Protobuf.Compiler.Plugin`, `protoc-gen-wireform` exe |
| 3 | Protobuf Editions Support | **Implemented** | `Proto.AST` (Editions, FeatureSet), `Proto.Parser` |
| 4 | Full descriptor.proto Types | **Implemented** | `Proto.Google.Protobuf.Descriptor` |
| 5 | Text Format (pbtxt) | **Implemented** | `Proto.TextFormat` |
| 6 | Dynamic Messages | **Implemented** | `Proto.Dynamic` |
| 7 | Conformance Test Harness | **Implemented** | `Proto.Conformance`, `wireform-conformance` exe |
| 8 | Streaming/Incremental Decode | **Implemented** | `Proto.Decode.Stream`, `Proto.Encode.Lazy` |
| 9 | Well-Known Type JSON | **Implemented** | `Proto.JSON.WellKnown` |
| 10 | aeson Integration | **Implemented** | `Proto.JSON.Aeson` |
| 11 | Group Field Handling | **Already implemented** | `Proto.Wire.Decode.skipGroup` |
| 12 | Custom Option Extensions | **Implemented** | `Proto.Options.Custom` |
| 13 | Map/Oneof in TH Codegen | **Implemented** | `Proto.TH` |
| 14 | Enum Alias Codegen | **Implemented** | `Proto.CodeGen` |

### Details

**1. gRPC Service Stub Generation** — The code generator now emits service
declarations instead of discarding them. `Proto.CodeGen.Service` generates:
- Server handler record types (one method handler field per RPC)
- Client stub record types
- Method metadata types with names and streaming modes
- Service metadata (fully-qualified names, method paths)

**2. protoc Plugin Mode** — `Proto.Google.Protobuf.Compiler.Plugin` provides
`CodeGeneratorRequest`/`CodeGeneratorResponse` types with full encode/decode,
plus a `pluginMain` entry point. The `protoc-gen-wireform` executable
implements the protoc plugin protocol.

**3. Protobuf Editions** — The AST now has `Editions Edition` as a `Syntax`
variant. The parser handles `edition = "2023"` syntax. `FeatureSet` and all
feature enum types are defined with `featuresForEdition` defaults.

**4. descriptor.proto Types** — Hand-written Haskell types for the core
descriptor hierarchy: `FileDescriptorSet`, `FileDescriptorProto`,
`DescriptorProto`, `FieldDescriptorProto`, `EnumDescriptorProto`,
`ServiceDescriptorProto`, `MethodDescriptorProto`, `OneofDescriptorProto`.

**5. Text Format** — `Proto.TextFormat` provides rendering and parsing of
the protobuf text format (pbtxt) for dynamic messages, with both compact
and pretty-printed output modes.

**6. Dynamic Messages** — `Proto.Dynamic` provides `DynamicMessage` backed
by `Map Int DynamicValue`, with wire format encode/decode and JSON conversion.

**7. Conformance Test Harness** — `Proto.Conformance` implements the
length-prefixed stdin/stdout protocol for the official conformance runner.

**9. Well-Known Type JSON** — Canonical JSON conversions:
- Timestamp ↔ RFC 3339 strings
- Duration ↔ "3.5s" format
- FieldMask ↔ comma-separated paths
- Struct/Value/ListValue ↔ native JSON

**10. aeson Integration** — Full aeson integration. Generated types get
`ToJSON`/`FromJSON` instances directly. The library depends on aeson.

**12. Custom Option Extensions** — `CustomOptionRegistry` for tracking and
resolving custom option extensions, with extraction from parsed proto files.

**13. Map/Oneof in TH** — The TH code generator now handles `MEMapField`
(generating `Map` fields) and `MEOneof` (generating `Maybe`-wrapped sum types).

**14. Enum Alias** — The code generator detects `allow_alias` enums, generates
only primary constructors with pattern synonyms for aliases, and skips
deriving `Enum` for non-sequential numbering.

### All features implemented

All originally-identified feature gaps have been closed, including:

**8. Streaming/Incremental Decode** — Implemented in `Proto.Decode.Stream`
(incremental/resumable decoder) and `Proto.Encode.Lazy` (push-based encoder).

## Columnar / lake-format gap closures

These items were tracked in [`docs/columnar-roadmap.md`](docs/columnar-roadmap.md):

| # | Feature | Module(s) |
|---|---------|-----------|
| P-A.7a | Parquet page index (`OffsetIndex` / `ColumnIndex`) | `Parquet.PageIndex` |
| P-A.7b | Parquet bloom filter (split-block, XXH64) | `Parquet.BloomFilter`, `Parquet.XXH64` |
| P-A.8  | DELTA_LENGTH_BYTE_ARRAY / DELTA_BYTE_ARRAY / BYTE_STREAM_SPLIT / RLE_DICTIONARY decoders | `Parquet.Delta`, `Parquet.Read` |

Page-index pointers (offset, length) and bloom-filter pointers
(`bloom_filter_offset`, `bloom_filter_length`) are round-tripped through
`ColumnChunk` / `ColumnMetadata` so existing Parquet files written by
arrow-cpp, parquet-mr, and pyarrow are recognised on read.

### Still planned

- Parquet encryption (modular encryption, footer key wrapping).
  Out of scope for the current sweep — needs a vetted AES-GCM
  implementation as a new dependency.

### Closed in this iteration

- Parquet `buildParquetFileWithIndex` writer that splits each column
  into pages, builds `OffsetIndex` + `ColumnIndex` + split-block bloom
  filter footers, and populates the metadata pointers
  (`cm_bloom_filter_offset`, `cc_offset_index_offset`,
  `cc_column_index_offset`) so existing readers see them.
- ORC `DICTIONARY_V2` string column decoding wired up via a new
  `decodeRLEv2IntAll` that walks RLE-v2 run headers to count values
  exactly (dictionary length streams have implicit counts).
- ORC writer paths for `timestamp` / `date` / `decimal64` columns,
  including the SECONDARY-stream nanosecond packing.
- Iceberg REST catalog protocol layer (`Iceberg.RestCatalog`) with
  request/response types, JSON codecs, atomic-commit
  `TableUpdate` / `TableRequirement` algebras, and an abstract
  `CatalogClient` that plugs in any HTTP transport.
