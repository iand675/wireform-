# Changelog for wireform-proto


## 0.2.0.0 -- 2026-07-16

Breaking release from 0.1.0.0.

* Rename generated well-known-type modules to match their canonical
  `google.protobuf` package paths (for example,
  `Proto.Google.Protobuf.Timestamp`).
* Replace the legacy public wire codec surface with `Proto.Encode` and
  `Proto.Decode`, and add collecting decode diagnostics in
  `Proto.Decode.Collect`.
* Add `Proto.JSONSchema`, a draft-2020-12 / OpenAPI 3.1 schema generator
  aligned with the proto3 JSON mapping.
* Update `loadProto`, code generation, well-known types, dynamic messages,
  extensions, text format, compatibility, and tests to the current
  wireform-derive and wireform-core APIs.

## 0.1.0.0 -- 2026

Initial release.

### Highlights

* `.proto` IDL parser (proto2 + proto3) with full reference resolution.
* Pure-text Haskell code generator (`Proto.CodeGen`) that emits records,
  enums, oneofs, maps, services, well-known type imports, and
  extension bindings -- plus a serialized `FileDescriptorProto` blob
  for each module.
* Annotation-driven Template Haskell deriver (`Proto.Derive`) that
  emits wire codecs (`MessageEncode` / `MessageDecode` / `MessageSize`),
  JSON instances, `Hashable`, `IsMessage`, and `ProtoMessage` schema
  metadata for hand-written Haskell records.
* `Proto.TH.loadProto` Template Haskell splice that runs the parser
  and the deriver together for an in-place `data` + instances bundle.
* Inline `[proto| ... |]` quasi-quoter for one-off types
  (`Proto.QQ`).
* `Proto.Setup` Cabal `Setup.hs` integration hook for pre-build
  protobuf code generation.
* `protoc-gen-wireform` `protoc` plugin (`--wireform_out=DIR`).
* Allocation-disciplined wire-format primitives: unboxed-sum decoder
  result (`Proto.Wire.Decode`), two-pass sized encoder
  (`Proto.SizedBuilder`, `Proto.Encode.Archetype`), pre-computed
  field tags, packed repeated field encode/decode, branchless varint
  sizing, lazy submessage decoding.
* Proto3 canonical JSON mapping (`Proto.JSON`) with `json_name`
  override, base64 bytes, string-encoded 64-bit integers, and
  NaN/Infinity sentinels.
* Proto → JSON Schema (draft 2020-12 / OpenAPI 3.1) walk
  (`Proto.JSONSchema`): the transport-agnostic half of schema-derived
  API doc generation. Emits `#/components/schemas` objects whose field
  names, 64-bit-int-as-string, base64 bytes, inlined well-known types,
  enum-as-string, map, and oneof shapes match the proto3-canonical-JSON
  codec byte-for-byte. Consumed by `wireform-connect`'s OpenAPI emitter.
* Well-known types (`Timestamp`, `Duration`, `Any`, `FieldMask`,
  `Struct`, `Value`, `ListValue`, `NullValue`, `Empty`, `Wrappers`,
  `SourceContext`) generated from the bundled `.proto` files by the
  `gen-wkt` executable.  Supplementary logic
  (`Proto.Google.Protobuf.*.Util`) ships `packAny`, RFC 3339
  formatting, `TypeRegistry`, `FieldMask` ops.
* Proto2 typed extensions (`Proto.Extension.HasExtensions`),
  unknown-field round-trip preservation, dynamic / untyped messages
  (`Proto.Dynamic`), `.pbtxt` text format I/O
  (`Proto.TextFormat`), and a runtime `MessageRegistry`
  (`Proto.Registry`).
* Streaming and incremental decoders
  (`Proto.Decode.Stream`, `Proto.Decode.Streaming`).
* gRPC service-method codegen (`Proto.GRPC`).  The wire framing
  lives in the `wireform-grpc` package.
* Conformance test driver (`Proto.Conformance`) that exposes the
  protocol expected by the upstream
  [`conformance_test_runner`](https://github.com/protocolbuffers/protobuf/tree/main/conformance).
  Today's baseline against `protocolbuffers/protobuf@v28.2`:
  2675 successes, 0 unexpected failures across proto3 + proto2,
  binary + JSON suites.
