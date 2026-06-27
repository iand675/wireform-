---
title: Protocol Buffers
description: "Full proto2/proto3 support: IDL parser, code generation, TH splicing, JSON mapping, well-known types, and gRPC."
sidebar:
  order: 4
---

`wireform-proto` implements Protocol Buffers from the `.proto` IDL down to the
wire. It includes a parser, a code generator, Template Haskell splicing,
proto3 JSON mapping, text format, well-known types, extensions, dynamic
messages, and a type registry. It is the largest and oldest package in the
wireform ecosystem.

## Three ways to get Haskell types from `.proto` files

| Approach | When to use |
|----------|-------------|
| `$(loadProto "file.proto")` | Small projects where TH is acceptable; types land in the same module |
| `wireform-gen proto -i file.proto -o gen/` | Larger projects; CI-friendly; commit generated code |
| `protoc --wireform_out=DIR` | Organizations that standardize on `protoc` plugins |

All three produce the same output: record types with `MessageEncode`,
`MessageDecode`, and `MessageSize` instances, plus Aeson JSON instances and
`ProtoMessage` metadata.

## Template Haskell splicing

The fastest way to get started:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module MyModule where

import Proto.TH (loadProto)

$(loadProto "proto/person.proto")
```

This parses the `.proto` file at compile time and splices Haskell types into
your module. For files that import other `.proto` files, pass include
directories:

```haskell
import Proto.TH (loadProtoWith, defaultLoadOpts, LoadOpts(..))

$(loadProtoWith defaultLoadOpts { loIncludeDirs = ["proto", "."] } "proto/api.proto")
```

### What gets generated

For each message, the splice produces:

- A Haskell record type with strict fields
- `MessageEncode` / `MessageSize` / `MessageDecode` instances
- `ToJSON` / `FromJSON` instances (proto3 canonical JSON)
- `Hashable`, `NFData`, `ProtoMessage` instances
- For enums: a sum type with an `Unknown` constructor for forward compatibility

### LoadOpts

| Field | Default | Effect |
|-------|---------|--------|
| `loIncludeDirs` | `["proto/", "."]` | Search paths for imports |
| `loFieldNaming` | `PrefixedFields` | `PrefixedFields` or `UnprefixedFields` |
| `loRepConfig` | `defaultRepConfig` | How proto types map to Haskell types |
| `loTHHooks` | none | Inject extra declarations per message |

## Standalone code generation

For projects where TH is not desirable:

```bash
cabal exec wireform-gen -- proto -i proto/person.proto -o gen/
```

This writes `.hs` files to `gen/`. Add `gen` to `hs-source-dirs` in your
`.cabal` file and list the generated modules in `exposed-modules` or
`other-modules`.

The `GenerateOpts` type controls output:

| Option | Default | Effect |
|--------|---------|--------|
| `genModulePrefix` | `"Proto.Gen"` | Haskell module namespace |
| `genFieldNaming` | `PrefixedFields` | Field naming convention |
| `genStrictFields` | `True` | Strict fields (bang patterns) |
| `genUnpackPrims` | `True` | `UNPACK` on numeric fields |
| `genDeriveGeneric` | `True` | Derive `Generic` |
| `genPackedRepeated` | `True` | Use packed encoding for repeated fields |
| `genLazySubmessages` | `False` | Lazy decode of nested messages |

## Encoding and decoding

The `Proto` umbrella module re-exports the primary API:

```haskell
import Proto

let bytes = encodeMessage myMessage
case decodeMessage bytes of
  Right msg -> use msg
  Left err  -> handleError err
```

### Encode

`encodeMessage` does a two-pass encode: first `messageSize` computes the exact
byte count, then `buildMessage` writes into a pre-allocated buffer. This avoids
the intermediate chunk copies that a streaming `Builder` would produce.

For streaming or framed output:

| Function | Use case |
|----------|----------|
| `encodeMessageLazy` | Lazy `ByteString` |
| `hPutMessage` | Write directly to a handle |
| `hPutMessageLen` | Length-prefixed framing |
| `buildMessageFramed` | gRPC-style length-delimited framing |

### Decode

`decodeMessage` returns `Either DecodeError a`. The decoder uses unboxed sums
internally, so the success path allocates only the final Haskell value. Unknown
fields are captured and round-tripped if the type has `HasExtensions`.

## Annotation-driven deriving

If you have hand-written Haskell types and want proto instances without a
`.proto` file, use `Proto.TH.Derive`:

```haskell
import Proto.TH.Derive (deriveProto)
import Wireform.Derive (tag, wireOverride, WireOverride(..))

data Event = Event
  { eventId   :: !Int64
  , eventName :: !Text
  , eventTime :: !Word64
  }

{-# ANN eventId   (tag 1) #-}
{-# ANN eventName (tag 2) #-}
{-# ANN eventTime (tag 3) #-}

deriveProto ''Event
```

Every field needs an explicit `tag`. The deriver supports `Maybe` fields,
repeated fields (`Vector`, `[]`), `Map`, oneofs, enums, and wire overrides
like `wireOverride WireZigZag`.

## Representation adapters

Proto fields map to Haskell types through configurable adapters in
`Proto.Repr`. The defaults are:

| Proto type | Default Haskell type |
|------------|---------------------|
| `string` | strict `Text` |
| `bytes` | strict `ByteString` |
| `repeated T` | `Vector T` |
| `map<K,V>` | `Map K V` (ordered) |

Override these via `RepConfig`:

```haskell
import Proto.Repr

myRepConfig = defaultRepConfig
  { configDefault = defaultFieldRep
      { fieldRepeated = listAdapter       -- use [] instead of Vector
      , fieldMap      = hashMapAdapter    -- use HashMap instead of Map
      }
  }

$(loadProtoWith defaultLoadOpts { loRepConfig = myRepConfig } "proto/api.proto")
```

Available adapters include `lazyTextAdapter`, `shortTextAdapter`,
`lazyBytesAdapter`, `shortBytesAdapter`, `unboxedVectorAdapter`, `seqAdapter`,
and `hashMapAdapter`.

## Dynamic messages

When you don't have generated types (e.g. processing arbitrary proto messages
at runtime), `Proto.Dynamic` gives you an untyped API:

```haskell
import Proto.Dynamic

let bytes = encodeDynamic myDynamicMessage
case decodeDynamic schema bytes of
  Right msg -> print (dynamicField "name" msg)
  Left err  -> handleError err
```

For better decode performance, compile a `ParseTable` from the schema once and
reuse it across many decodes with `decodeDynamicWithSchema`.

## Text format

`Proto.TextFormat` reads and writes the protobuf text format (`.pbtxt`):

```haskell
import Proto.TextFormat

let text = typedToTextPretty (Proxy @MyMessage) myMsg
case textToDynamic schema text of
  Right dynMsg -> use dynMsg
  Left err     -> handleError err
```

## Type registry

`Proto.Registry` provides an explicit registry of message types for use with
`Any` packing/unpacking and dynamic message dispatch:

```haskell
import Proto.Registry

let registry = emptyRegistry
      & registerMessage @MyMessage
      & registerMessage @OtherMessage

case lookupDecoder registry "type.googleapis.com/my.Message" of
  Just decoder -> decoder bytes
  Nothing      -> unknownType
```

`discoverRegistry` is a TH splice that scans all imported modules for
`IsMessage` instances and builds the registry automatically:

```haskell
myRegistry :: TypeRegistry
myRegistry = $(discoverRegistry)
```

## Well-known types

`Proto.Google.Protobuf.*` modules are code-generated from the upstream
`.proto` files in `proto/google/protobuf/`. Each well-known type has a
companion `*.Util` module with helper functions:

| Type | Util module | Key helpers |
|------|-------------|-------------|
| `Timestamp` | `Timestamp.Util` | RFC 3339 formatting, `getCurrentTimestamp` |
| `Duration` | `Duration.Util` | Arithmetic, conversion to/from seconds |
| `Any` | `Any.Util` | `packAny`, `unpackAny` with `TypeRegistry` |
| `FieldMask` | `FieldMask.Util` | Path operations, merging |
| `Struct` | `Struct.Util` | Conversion to/from Aeson `Value` |
| `Wrappers` | `Wrappers.Util` | `Int32Value`, `StringValue`, etc. |

## Proto3 JSON

The `Proto.Internal.JSON` modules implement the proto3 canonical JSON mapping.
The `ToJSON`/`FromJSON` instances generated by `loadProto` and `wireform-gen`
use this mapping automatically. It handles field name conversion (proto
`snake_case` to JSON `camelCase`), default value omission, `Any` type URLs,
well-known type special encodings, and `NullValue`.

## Extensions (proto2)

Proto2 extensions are supported via `Proto.Extension`:

```haskell
import Proto.Extension

let val = getExtension myExtField msg
let msg' = setExtension myExtField val msg
```

Extensions are carried as unknown fields in the wire format and decoded on
access.

## Conformance

`wireform-proto` passes 2,675 / 2,675 tests in the official protobuf
conformance suite, covering proto2 and proto3 binary encoding, proto3 JSON,
and text format.

## Performance

wireform-proto is 3-7x faster than proto-lens on both encode and decode. The
speedup comes from unboxed sums in the decoder, direct-write encoding, and
inlined field codecs.

### Decode: wireform-proto vs proto-lens

<!-- BEGIN_AUTOGEN bench:proto-vs-proto-lens-decode -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-proto vs proto-lens (decode)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-proto vs proto-lens (decode)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">625</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">1250</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">1875</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">2500</text>
    </g>
    <g>
      <rect x="95.5" y="315.7" width="60" height="4.3" rx="2" fill="#0969da"/>
      <rect x="157.5" y="311.5" width="60" height="8.5" rx="2" fill="#cf222e"/>
      <rect x="250.5" y="308.4" width="60" height="11.6" rx="2" fill="#0969da"/>
      <rect x="312.5" y="298.0" width="60" height="22.0" rx="2" fill="#cf222e"/>
      <rect x="405.5" y="311.6" width="60" height="8.4" rx="2" fill="#0969da"/>
      <rect x="467.5" y="304.3" width="60" height="15.7" rx="2" fill="#cf222e"/>
      <rect x="560.5" y="172.1" width="60" height="147.9" rx="2" fill="#0969da"/>
      <rect x="622.5" y="81.5" width="60" height="238.5" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="125.5" y="311.7" text-anchor="middle" font-size="10" fill="#1f2328">41.6</text>
      <text x="187.5" y="307.5" text-anchor="middle" font-size="10" fill="#1f2328">81.6</text>
      <text x="280.5" y="304.4" text-anchor="middle" font-size="10" fill="#1f2328">111</text>
      <text x="342.5" y="294.0" text-anchor="middle" font-size="10" fill="#1f2328">212</text>
      <text x="435.5" y="307.6" text-anchor="middle" font-size="10" fill="#1f2328">81.0</text>
      <text x="497.5" y="300.3" text-anchor="middle" font-size="10" fill="#1f2328">151</text>
      <text x="590.5" y="168.1" text-anchor="middle" font-size="10" fill="#1f2328">1422</text>
      <text x="652.5" y="77.5" text-anchor="middle" font-size="10" fill="#1f2328">2293</text>
    </g>
    <g>
      <text x="157.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">Small</text>
      <text x="312.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">Medium</text>
      <text x="467.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">Nested</text>
      <text x="622.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">Repeated</text>
    </g>
    <g>
      <g transform="translate(250, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">wireform-proto</text>
      </g>
      <g transform="translate(382, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">proto-lens</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-proto vs proto-lens (decode)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">625</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">1250</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">1875</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">2500</text>
    </g>
    <g>
      <rect x="95.5" y="315.7" width="60" height="4.3" rx="2" fill="#58a6ff"/>
      <rect x="157.5" y="311.5" width="60" height="8.5" rx="2" fill="#ff7b72"/>
      <rect x="250.5" y="308.4" width="60" height="11.6" rx="2" fill="#58a6ff"/>
      <rect x="312.5" y="298.0" width="60" height="22.0" rx="2" fill="#ff7b72"/>
      <rect x="405.5" y="311.6" width="60" height="8.4" rx="2" fill="#58a6ff"/>
      <rect x="467.5" y="304.3" width="60" height="15.7" rx="2" fill="#ff7b72"/>
      <rect x="560.5" y="172.1" width="60" height="147.9" rx="2" fill="#58a6ff"/>
      <rect x="622.5" y="81.5" width="60" height="238.5" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="125.5" y="311.7" text-anchor="middle" font-size="10" fill="#e6edf3">41.6</text>
      <text x="187.5" y="307.5" text-anchor="middle" font-size="10" fill="#e6edf3">81.6</text>
      <text x="280.5" y="304.4" text-anchor="middle" font-size="10" fill="#e6edf3">111</text>
      <text x="342.5" y="294.0" text-anchor="middle" font-size="10" fill="#e6edf3">212</text>
      <text x="435.5" y="307.6" text-anchor="middle" font-size="10" fill="#e6edf3">81.0</text>
      <text x="497.5" y="300.3" text-anchor="middle" font-size="10" fill="#e6edf3">151</text>
      <text x="590.5" y="168.1" text-anchor="middle" font-size="10" fill="#e6edf3">1422</text>
      <text x="652.5" y="77.5" text-anchor="middle" font-size="10" fill="#e6edf3">2293</text>
    </g>
    <g>
      <text x="157.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">Small</text>
      <text x="312.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">Medium</text>
      <text x="467.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">Nested</text>
      <text x="622.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">Repeated</text>
    </g>
    <g>
      <g transform="translate(250, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">wireform-proto</text>
      </g>
      <g transform="translate(382, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">proto-lens</text>
      </g>
    </g>
  </g>
</svg>


| Operation | wireform-proto | proto-lens | ratio |
| :-------- | -------------: | ---------: | ----: |
| Small     |        41.6 ns |    81.6 ns | 1.96x |
| Medium    |         111 ns |     212 ns | 1.90x |
| Nested    |       80.10 ns |     151 ns | 1.87x |
| Repeated  |        1422 ns |    2293 ns | 1.61x |

<sub>Last run 2026-06-27 11:56:42 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:proto-vs-proto-lens-decode -->

### Encode: wireform-proto vs proto-lens

<!-- BEGIN_AUTOGEN bench:proto-vs-proto-lens-encode -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-proto vs proto-lens (encode, builder path)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-proto vs proto-lens (encode, builder path)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">1250</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">2500</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">3750</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">5000</text>
    </g>
    <g>
      <rect x="95.5" y="318.6" width="60" height="1.4" rx="2" fill="#0969da"/>
      <rect x="157.5" y="312.2" width="60" height="7.8" rx="2" fill="#cf222e"/>
      <rect x="250.5" y="316.2" width="60" height="3.8" rx="2" fill="#0969da"/>
      <rect x="312.5" y="305.1" width="60" height="14.9" rx="2" fill="#cf222e"/>
      <rect x="405.5" y="317.1" width="60" height="2.9" rx="2" fill="#0969da"/>
      <rect x="467.5" y="302.7" width="60" height="17.3" rx="2" fill="#cf222e"/>
      <rect x="560.5" y="266.0" width="60" height="54.0" rx="2" fill="#0969da"/>
      <rect x="622.5" y="176.6" width="60" height="143.4" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="125.5" y="314.6" text-anchor="middle" font-size="10" fill="#1f2328">26.6</text>
      <text x="187.5" y="308.2" text-anchor="middle" font-size="10" fill="#1f2328">151</text>
      <text x="280.5" y="312.2" text-anchor="middle" font-size="10" fill="#1f2328">72.4</text>
      <text x="342.5" y="301.1" text-anchor="middle" font-size="10" fill="#1f2328">286</text>
      <text x="435.5" y="313.1" text-anchor="middle" font-size="10" fill="#1f2328">55.5</text>
      <text x="497.5" y="298.7" text-anchor="middle" font-size="10" fill="#1f2328">333</text>
      <text x="590.5" y="262.0" text-anchor="middle" font-size="10" fill="#1f2328">1038</text>
      <text x="652.5" y="172.6" text-anchor="middle" font-size="10" fill="#1f2328">2758</text>
    </g>
    <g>
      <text x="157.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">Small</text>
      <text x="312.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">Medium</text>
      <text x="467.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">Nested</text>
      <text x="622.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">Repeated</text>
    </g>
    <g>
      <g transform="translate(250, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">wireform-proto</text>
      </g>
      <g transform="translate(382, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">proto-lens</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-proto vs proto-lens (encode, builder path)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">1250</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">2500</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">3750</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">5000</text>
    </g>
    <g>
      <rect x="95.5" y="318.6" width="60" height="1.4" rx="2" fill="#58a6ff"/>
      <rect x="157.5" y="312.2" width="60" height="7.8" rx="2" fill="#ff7b72"/>
      <rect x="250.5" y="316.2" width="60" height="3.8" rx="2" fill="#58a6ff"/>
      <rect x="312.5" y="305.1" width="60" height="14.9" rx="2" fill="#ff7b72"/>
      <rect x="405.5" y="317.1" width="60" height="2.9" rx="2" fill="#58a6ff"/>
      <rect x="467.5" y="302.7" width="60" height="17.3" rx="2" fill="#ff7b72"/>
      <rect x="560.5" y="266.0" width="60" height="54.0" rx="2" fill="#58a6ff"/>
      <rect x="622.5" y="176.6" width="60" height="143.4" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="125.5" y="314.6" text-anchor="middle" font-size="10" fill="#e6edf3">26.6</text>
      <text x="187.5" y="308.2" text-anchor="middle" font-size="10" fill="#e6edf3">151</text>
      <text x="280.5" y="312.2" text-anchor="middle" font-size="10" fill="#e6edf3">72.4</text>
      <text x="342.5" y="301.1" text-anchor="middle" font-size="10" fill="#e6edf3">286</text>
      <text x="435.5" y="313.1" text-anchor="middle" font-size="10" fill="#e6edf3">55.5</text>
      <text x="497.5" y="298.7" text-anchor="middle" font-size="10" fill="#e6edf3">333</text>
      <text x="590.5" y="262.0" text-anchor="middle" font-size="10" fill="#e6edf3">1038</text>
      <text x="652.5" y="172.6" text-anchor="middle" font-size="10" fill="#e6edf3">2758</text>
    </g>
    <g>
      <text x="157.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">Small</text>
      <text x="312.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">Medium</text>
      <text x="467.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">Nested</text>
      <text x="622.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">Repeated</text>
    </g>
    <g>
      <g transform="translate(250, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">wireform-proto</text>
      </g>
      <g transform="translate(382, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">proto-lens</text>
      </g>
    </g>
  </g>
</svg>


| Operation | wireform-proto | proto-lens | ratio |
| :-------- | -------------: | ---------: | ----: |
| Small     |        26.6 ns |     151 ns | 5.67x |
| Medium    |        72.4 ns |     286 ns | 3.95x |
| Nested    |        55.5 ns |     333 ns | 6.00x |
| Repeated  |        1038 ns |    2758 ns | 2.66x |

<sub>Last run 2026-06-27 11:56:42 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:proto-vs-proto-lens-encode -->

The charts and tables above are regenerated by [`wireform-stats`](../stats/) from `wireform-proto/bench-results/summary/proto-vs-proto-lens-{decode,encode}.json` — the same source the README charts are built from.
