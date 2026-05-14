# Changelog for wireform-proto

## 0.1.0.0 -- 2026-05-16

Initial release.

### Highlights

* `.proto` IDL parser (proto2 + proto3) with full reference resolution.
* Pure-text Haskell code generator (`Proto.CodeGen`) that emits records,
  enums, oneofs, maps, services, well-known type imports, and
  extension bindings -- plus a serialized `FileDescriptorProto` blob
  for each module.
* Annotation-driven Template Haskell deriver (`Proto.TH.Derive`) that
  emits wire codecs (`MessageEncode` / `MessageDecode`),
  `IsMessage`, and `ProtoMessage` schema metadata for hand-written
  Haskell records. JSON instances (`ToJSON` / `FromJSON`) and `Hashable`
  are derived separately via `deriving anyclass` on the generated types.
* `Proto.TH.loadProto` Template Haskell splice that runs the parser
  and the deriver together for an in-place `data` + instances bundle.
* Inline `[proto| ... |]` quasi-quoter for one-off types
  (`Proto.TH.QQ`).
* `Proto.Setup` Cabal `Setup.hs` integration hook for pre-build
  protobuf code generation.
* `protoc-gen-wireform` `protoc` plugin (`--wireform_out=DIR`).
* Allocation-disciplined wire-format primitives: unboxed-sum decoder
  result (`Proto.Wire.Decode`), two-pass sized encoder
  (`Proto.SizedBuilder`, `Proto.Encode.Archetype`), pre-computed
  field tags, packed repeated field encode/decode, branchless varint
  sizing, lazy submessage decoding.
* Proto3 canonical JSON mapping with `json_name` override, base64
  bytes, string-encoded 64-bit integers, and NaN/Infinity sentinels.
  Generated types get `ToJSON` / `FromJSON` instances automatically;
  the implementation lives in `Proto.Internal.JSON` and
  `Proto.Internal.JSON.WellKnown`.
* Well-known types (`Timestamp`, `Duration`, `Any`, `FieldMask`,
  `Struct`, `Value`, `ListValue`, `NullValue`, `Empty`, `Wrappers`,
  `SourceContext`) generated from the bundled `.proto` files by the
  `regen-wkt` executable (`cabal run regen-wkt`).  Supplementary logic
  (  `Proto.Google.Protobuf.*.Util`) ships `packAny`, RFC 3339
  formatting, `TypeRegistry`, `FieldMask` ops. Well-known types are
  regenerated from the bundled `.proto` files by `cabal run regen-wkt`.
* Proto2 typed extensions (`Proto.Extension.HasExtensions`),
  unknown-field round-trip preservation, dynamic / untyped messages
  (`Proto.Dynamic`), `.pbtxt` text format I/O
  (`Proto.TextFormat`), and a runtime `MessageRegistry`
  (`Proto.Registry`).
* Streaming and incremental decoders
  (`Proto.Decode.Stream`, `Proto.Decode.Streaming`).
* gRPC service-method codegen. `loadProto` and the other codegen entry
  points generate typed service/method descriptor values. Wire framing
  and transport live in the `wireform-grpc` package.
* Conformance test driver (`Proto.Conformance`) that exposes the
  protocol expected by the upstream
  [`conformance_test_runner`](https://github.com/protocolbuffers/protobuf/tree/main/conformance).
  Today's baseline against `protocolbuffers/protobuf@v28.2`:
  2675 successes, 0 unexpected failures across proto3 + proto2,
  binary + JSON suites.
