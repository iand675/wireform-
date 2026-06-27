---
title: wireform-flatbuffers
description: "Google FlatBuffers zero-copy serialization with vtable layout, schema codegen, and shared builder with Arrow IPC."
sidebar:
  order: 34
---

`wireform-flatbuffers` implements [Google FlatBuffers](https://flatbuffers.dev/),
a zero-copy serialization format designed for game engines, mobile apps, and
real-time inference serving. FlatBuffers lay out tables with vtables that index
field offsets, keeping scalars inline and strings or sub-tables behind indirection.
Use this package when you need to read large buffers without deserializing the
entire message, or when sharing the builder infrastructure with Arrow IPC.

## Key features

- **Typeclass API** via `ToFlatBuffers` and `FromFlatBuffers` with Template Haskell deriving
- **FlatBuffers IDL parser and codegen** from `.fbs` schema files
- **Vtable-based wire layout** with inline scalars and offset-indirected collections
- **Zero-copy view/reader** via `FlatBuffers.View` for schema-known access patterns
- **Shared builder** with Arrow IPC for cross-format buffer construction
- **QuasiQuoter** for inline `[flatbuffers| ... |]` schemas

## Basic usage

Derive instances for your table types, then encode and decode through the
value-level codec:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

import FlatBuffers.Decode qualified as FBD
import FlatBuffers.Derive (ToFlatBuffers (..), FromFlatBuffers (..), deriveFlatBuffers, deriveView)
import FlatBuffers.Encode qualified as FBE
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)

data Widget = Widget
  { widgetName  :: !Text
  , widgetCount :: !Int32
  , widgetPrice :: !Double
  }
  deriving stock (Show, Eq, Generic)

$(deriveFlatBuffers ''Widget)
$(deriveView ''Widget)

encodeWidget :: Widget -> ByteString
encodeWidget = FBE.encode . toFlatBuffers

decodeWidget :: ByteString -> Either String Widget
decodeWidget bs = do
  val <- FBD.decode bs
  fromFlatBuffers val

sample :: Widget
sample = Widget "wireform" 42 2.718

roundTrip :: Either String Widget
roundTrip = decodeWidget (encodeWidget sample)
```

For zero-copy reads on known schemas, use the view layer after encoding:

```haskell
import FlatBuffers.View (decodeRoot)

readWidget :: ByteString -> Either String Widget
readWidget = decodeRoot
```

Generate types from `.fbs` files:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import FlatBuffers.QQ (fbs)

[fbs|
  table Widget {
    name:string;
    count:int;
    price:double;
  }
  root_type Widget;
|]
```

```bash
wireform-gen flatbuffers -i schema.fbs -o src/Gen/
```

## Performance

### Encode/decode (zero-copy decode)

<!-- BEGIN_AUTOGEN bench:flatbuffers-encode-decode -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-flatbuffers encode + decode (zero-copy decode)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-flatbuffers encode + decode (zero-copy decode)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5. Decode is a zero-copy cursor by design: only the outer envelope is resolved at decode time. Per-field reads happen lazily.</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">25000</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">50000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">75000</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">100000</text>
    </g>
    <g>
      <rect x="171" y="317.8" width="62" height="2.2" rx="2" fill="#0969da"/>
      <rect x="235" y="319.6" width="62" height="0.4" rx="2" fill="#cf222e"/>
      <rect x="481" y="72.1" width="62" height="247.9" rx="2" fill="#0969da"/>
      <rect x="545" y="319.8" width="62" height="0.2" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="202" y="313.8" text-anchor="middle" font-size="10" fill="#1f2328">827</text>
      <text x="266" y="315.6" text-anchor="middle" font-size="10" fill="#1f2328">142</text>
      <text x="512" y="68.1" text-anchor="middle" font-size="10" fill="#1f2328">95352</text>
      <text x="576" y="315.8" text-anchor="middle" font-size="10" fill="#1f2328">75.7</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#1f2328">Person table</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#1f2328">Person[100] vector</text>
    </g>
    <g>
      <g transform="translate(292, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">encode</text>
      </g>
      <g transform="translate(368, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">decode</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-flatbuffers encode + decode (zero-copy decode)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5. Decode is a zero-copy cursor by design: only the outer envelope is resolved at decode time. Per-field reads happen lazily.</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">25000</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">50000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">75000</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">100000</text>
    </g>
    <g>
      <rect x="171" y="317.8" width="62" height="2.2" rx="2" fill="#58a6ff"/>
      <rect x="235" y="319.6" width="62" height="0.4" rx="2" fill="#ff7b72"/>
      <rect x="481" y="72.1" width="62" height="247.9" rx="2" fill="#58a6ff"/>
      <rect x="545" y="319.8" width="62" height="0.2" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="202" y="313.8" text-anchor="middle" font-size="10" fill="#e6edf3">827</text>
      <text x="266" y="315.6" text-anchor="middle" font-size="10" fill="#e6edf3">142</text>
      <text x="512" y="68.1" text-anchor="middle" font-size="10" fill="#e6edf3">95352</text>
      <text x="576" y="315.8" text-anchor="middle" font-size="10" fill="#e6edf3">75.7</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">Person table</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">Person[100] vector</text>
    </g>
    <g>
      <g transform="translate(292, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">encode</text>
      </g>
      <g transform="translate(368, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">decode</text>
      </g>
    </g>
  </g>
</svg>


| Operation          |   encode |  decode | ratio |
| :----------------- | -------: | ------: | ----: |
| Person table       |   827 ns |  142 ns | 0.17x |
| Person[100] vector | 95352 ns | 75.7 ns | 0.00x |

<sub>Last run 2026-06-27 11:35:54 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5. Decode is a zero-copy cursor by design: only the outer envelope is resolved at decode time. Per-field reads happen lazily..</sub>
<!-- END_AUTOGEN bench:flatbuffers-encode-decode -->

Like Cap'n Proto, FlatBuffers decode is a zero-copy cursor. The decode cost is near-constant because only the root table offset is resolved; field access is lazy pointer arithmetic into the original buffer. Encode is proportional to the number of fields and vector elements.

The chart and table above are regenerated by [`wireform-stats`](../stats/) from `wireform-flatbuffers/bench-results/summary/flatbuffers-encode-decode.json` — the same source the README chart is built from.

## Notable modules

| Module | Purpose |
|--------|---------|
| `FlatBuffers.Derive` | `ToFlatBuffers` / `FromFlatBuffers` and `deriveFlatBuffers` |
| `FlatBuffers.Encode` / `FlatBuffers.Decode` | High-level encoder and value-tree decoder |
| `FlatBuffers.View` | Zero-copy cursor access for schema-known tables |
| `FlatBuffers.Reader` | Low-level pointer-walking decoder |
| `FlatBuffers.Builder` | Vtable and offset builder |
| `FlatBuffers.Value` | Dynamic untyped `Value` ADT |
| `FlatBuffers.Schema` / `FlatBuffers.Parser` | Schema AST and `.fbs` parser |
| `FlatBuffers.CodeGen` / `FlatBuffers.QQ` | Haskell codegen and quasiquoter |
| `FlatBuffers.Registry` | Runtime schema registry |
