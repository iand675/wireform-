---
title: wireform-thrift
description: "Apache Thrift binary and compact wire protocols with IDL codegen, RPC message framing, and Template Haskell deriving."
sidebar:
  order: 31
---

`wireform-thrift` implements [Apache Thrift](https://thrift.apache.org/), the
IDL-driven serialization framework used by Cassandra, Parquet footers, and many
high-throughput services. Thrift structs carry numbered field IDs for forward and
backward compatibility, and the package supports both the legacy binary protocol
and the smaller compact protocol. Use this package when you need Thrift wire
compatibility, RPC message framing, or schema codegen from `.thrift` IDL files.

## Key features

- **Template Haskell deriving** via `deriveThrift` for Haskell record types,
  with `wireform-derive` annotations; Generic defaults (empty instances) work
  for simple cases
- **Binary and Compact wire protocols** with matching encode/decode entry points
- **Thrift IDL parser and codegen** from `.thrift` schema files
- **Service definitions** for RPC method signatures
- **Message framing** for request/response envelopes (`Thrift.Message`)
- **JSON bridge** for self-describing text rendering
- **QuasiQuoter** for inline `[thrift| ... |]` schemas
- **Runtime registry** for dynamic struct lookup

## Basic usage

Derive instances with the Template Haskell deriver, then pick a wire protocol.
Compact is the recommended choice for new code because it produces smaller
payloads:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}

import Data.Text (Text)
import GHC.Generics (Generic)
import Thrift.Class
  ( ToThrift, FromThrift
  , encodeThriftBinary, decodeThriftBinary
  , encodeThriftCompact, decodeThriftCompact
  )
import Thrift.Derive (deriveThrift)

data LogEntry = LogEntry
  { level   :: !Text
  , message :: !Text
  , code    :: !Int
  }
  deriving stock (Show, Eq, Generic)

$(deriveThrift ''LogEntry)

entry :: LogEntry
entry = LogEntry "ERROR" "disk full" 507

-- Binary protocol (tag-prefixed fields, larger on the wire)
binaryBytes :: ByteString
binaryBytes = encodeThriftBinary entry

decodeBinary :: Either String LogEntry
decodeBinary = decodeThriftBinary binaryBytes

-- Compact protocol (variable-length encoding, recommended)
compactBytes :: ByteString
compactBytes = encodeThriftCompact entry

decodeCompact :: Either String LogEntry
decodeCompact = decodeThriftCompact compactBytes
```

For simple cases with no wire-format customization, Generic defaults also
work: add `deriving Generic` and declare empty `instance ToThrift LogEntry`
and `instance FromThrift LogEntry` declarations.

For RPC-style communication, wrap payloads in a message envelope:

```haskell
import Thrift.Class (toThrift)
import Thrift.Message
  ( ThriftMessage (..), ThriftMessageType (..)
  , encodeMessageCompact, decodeMessageCompact
  )

sendRequest :: Text -> LogEntry -> ByteString
sendRequest methodName entry =
  encodeMessageCompact $
    ThriftMessage methodName TMsgCall 1 (toThrift entry)

receiveResponse :: ByteString -> Either String LogEntry
receiveResponse framed = do
  ThriftMessage _ TMsgReply _ payload <- decodeMessageCompact framed
  fromThrift payload
```

Generate types from IDL with the quasiquoter or CLI:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Thrift.QQ (thrift)

[thrift|
  struct Person {
    1: string name,
    2: i32 age,
  }
|]
```

```bash
wireform-gen thrift -i service.thrift -o src/Gen/
```

## Performance

### Binary vs Compact wire protocol

<!-- BEGIN_AUTOGEN bench:thrift-binary-vs-compact -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-thrift binary vs compact wire protocols</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-thrift binary vs compact wire protocols</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">12500</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">25000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">37500</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">50000</text>
    </g>
    <g>
      <rect x="171" y="318.7" width="62" height="1.3" rx="2" fill="#0969da"/>
      <rect x="235" y="318.7" width="62" height="1.3" rx="2" fill="#cf222e"/>
      <rect x="481" y="195.5" width="62" height="124.5" rx="2" fill="#0969da"/>
      <rect x="545" y="185.0" width="62" height="135.0" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="202" y="314.7" text-anchor="middle" font-size="10" fill="#1f2328">248</text>
      <text x="266" y="314.7" text-anchor="middle" font-size="10" fill="#1f2328">259</text>
      <text x="512" y="191.5" text-anchor="middle" font-size="10" fill="#1f2328">23948</text>
      <text x="576" y="181.0" text-anchor="middle" font-size="10" fill="#1f2328">25963</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode Person</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode [Person] x 100</text>
    </g>
    <g>
      <g transform="translate(288.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">binary</text>
      </g>
      <g transform="translate(364.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">compact</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-thrift binary vs compact wire protocols</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">12500</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">25000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">37500</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">50000</text>
    </g>
    <g>
      <rect x="171" y="318.7" width="62" height="1.3" rx="2" fill="#58a6ff"/>
      <rect x="235" y="318.7" width="62" height="1.3" rx="2" fill="#ff7b72"/>
      <rect x="481" y="195.5" width="62" height="124.5" rx="2" fill="#58a6ff"/>
      <rect x="545" y="185.0" width="62" height="135.0" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="202" y="314.7" text-anchor="middle" font-size="10" fill="#e6edf3">248</text>
      <text x="266" y="314.7" text-anchor="middle" font-size="10" fill="#e6edf3">259</text>
      <text x="512" y="191.5" text-anchor="middle" font-size="10" fill="#e6edf3">23948</text>
      <text x="576" y="181.0" text-anchor="middle" font-size="10" fill="#e6edf3">25963</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode Person</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode [Person] x 100</text>
    </g>
    <g>
      <g transform="translate(288.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">binary</text>
      </g>
      <g transform="translate(364.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">compact</text>
      </g>
    </g>
  </g>
</svg>


| Operation             |   binary |  compact | ratio |
| :-------------------- | -------: | -------: | ----: |
| encode Person         |   248 ns |   259 ns | 1.04x |
| encode [Person] x 100 | 23948 ns | 25963 ns | 1.08x |

<sub>Last run 2026-06-27 11:35:55 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:thrift-binary-vs-compact -->

Binary protocol is slightly faster (~8-12%) than Compact across payload sizes. Both are fast enough that the encode cost is negligible relative to network I/O for typical RPC payloads.

The chart and table above are regenerated by [`wireform-stats`](../stats/) from `wireform-thrift/bench-results/summary/thrift-binary-vs-compact.json` — the same source the README chart is built from.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Thrift.Class` | `ToThrift` / `FromThrift` plus `encodeThriftBinary` / `encodeThriftCompact` |
| `Thrift.Encode` / `Thrift.Decode` | Low-level binary and compact wire primitives |
| `Thrift.Wire` | Type tags, field IDs, and TType constants |
| `Thrift.Value` | Dynamic untyped `Value` ADT |
| `Thrift.Schema` / `Thrift.Parser` | IDL AST and `.thrift` parser |
| `Thrift.CodeGen` / `Thrift.QQ` | Haskell codegen and quasiquoter |
| `Thrift.Message` | RPC message envelope and framing |
| `Thrift.Transport` | Length-prefixed transport helpers |
| `Thrift.Registry` | Runtime struct schema registry |
| `Thrift.JSON` | Thrift to JSON bridge |
| `Thrift.Derive` | Template Haskell deriver with annotation modifiers |
