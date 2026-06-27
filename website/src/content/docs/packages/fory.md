---
title: wireform-fory
description: "Apache Fory cross-language serialization with reference tracking, meta-string compression, and pyfory 0.17 wire compatibility."
sidebar:
  order: 16
---

`wireform-fory` implements Apache Fory (formerly Apache Fury), a
cross-language serialization format optimized for RPC and data exchange
between JVM, Python, and other runtimes. Fory supports reference tracking,
meta-string compression, schema-hashed named structs, and chunked collections.
Use this package when you need wire-compatible payloads with Python services
using `pyfory` 0.17, or when shared subgraphs and large string tables make
reference tracking worthwhile.

Fory is more configuration-heavy than CBOR or MessagePack. Encoder options,
struct registries, and schema registration affect the on-wire layout.

## Key features

- **Template Haskell deriving** via `deriveFory` for records and algebraic
  types, with `wireform-derive` annotations; Generic defaults (empty instances)
  work for simple cases
- **Reference tracking** to deduplicate shared objects and cyclic graphs on
  the wire
- **Meta-string compression** for repeated field and type names
- **Named structs with schema hash** for pyfory-compatible `NAMED_STRUCT`
  layout
- **Chunked collections** for lists, sets, and maps with homogeneous element
  types
- **One-dimensional primitive arrays** (`BoolArray`, `Int32Array`, ...) with
  byte-identical layouts to pyfory's NumPy serializer
- **Wire compatibility** with `pyfory` 0.17 for the supported type set

## Basic usage

Derive Fory codecs with the Template Haskell deriver and round-trip through
the default encoder:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Event where

import Fory.Class (ToFory, FromFory, encodeFory, decodeFory)
import Fory.Derive (deriveFory)
import GHC.Generics (Generic)
import Data.Text (Text)

data Event = Event
  { eventId   :: !Int64
  , eventName :: !Text
  }
  deriving stock (Show, Eq, Generic)

$(deriveFory ''Event)

send :: Event -> ByteString
send ev = encodeFory ev

receive :: ByteString -> Either String Event
receive bs = decodeFory bs
```

For simple cases with no wire-format customization, Generic defaults also
work: add `deriving Generic` and declare empty `instance ToFory Event` and
`instance FromFory Event` declarations.

When the same object appears more than once in a graph, enable reference
tracking so subsequent occurrences encode as back-references:

```haskell
import Fory.Class (toFory)
import Fory.Encode (encodeWith)
import Fory.Options qualified as O

encodeWithRefs :: Event -> ByteString
encodeWithRefs ev =
  encodeWith (O.defaultEncodeOptions { O.eoRefTracking = True }) (toFory ev)
```

For pyfory-compatible named structs, register schemas in the encoder options
so the wire layout includes the 4-byte fingerprint hash:

```haskell
import Fory.Options qualified as O
import Fory.Struct (StructSchema, mkSchema)
import Fory.TypeId (INT32, STRING)

personSchema :: StructSchema
personSchema =
  mkSchema "myapp" "Person"
    [ ("name", STRING)
    , ("age", INT32)
    ]

encodePersonOpts :: O.EncodeOptions
encodePersonOpts =
  O.defaultEncodeOptions
    { O.eoStructRegistry = O.registerStruct personSchema O.emptyStructRegistry
    }
```

Use the primitive array newtypes when exchanging numeric buffers with Python
NumPy code:

```haskell
import Fory.Class (Int32Array(..), ToFory, FromFory, encodeFory, decodeFory)
import qualified Data.Vector.Storable as VS

timeseries :: VS.Vector Int32 -> ByteString
timeseries vec = encodeFory (Int32Array vec)

readTimeseries :: ByteString -> Either String (VS.Vector Int32)
readTimeseries bs = do
  Int32Array vec <- decodeFory bs
  pure vec
```

## Performance

### Encode/decode across representative shapes

<!-- BEGIN_AUTOGEN bench:fory-encode-decode -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-fory encode + decode across representative shapes</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-fory encode + decode across representative shapes</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">5000</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">10000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">15000</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">20000</text>
    </g>
    <g>
      <rect x="92.4" y="319.0" width="47.6" height="1.0" rx="2" fill="#0969da"/>
      <rect x="142" y="319.2" width="47.6" height="0.8" rx="2" fill="#cf222e"/>
      <rect x="216.4" y="318.9" width="47.6" height="1.1" rx="2" fill="#0969da"/>
      <rect x="266" y="319.1" width="47.6" height="0.9" rx="2" fill="#cf222e"/>
      <rect x="340.4" y="318.3" width="47.6" height="1.7" rx="2" fill="#0969da"/>
      <rect x="390" y="319.2" width="47.6" height="0.8" rx="2" fill="#cf222e"/>
      <rect x="464.4" y="316.5" width="47.6" height="3.5" rx="2" fill="#0969da"/>
      <rect x="514" y="314.3" width="47.6" height="5.7" rx="2" fill="#cf222e"/>
      <rect x="588.4" y="233.8" width="47.6" height="86.2" rx="2" fill="#0969da"/>
      <rect x="638" y="179.8" width="47.6" height="140.2" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="116.2" y="315.0" text-anchor="middle" font-size="10" fill="#1f2328">75.6</text>
      <text x="165.8" y="315.2" text-anchor="middle" font-size="10" fill="#1f2328">62.5</text>
      <text x="240.2" y="314.9" text-anchor="middle" font-size="10" fill="#1f2328">85.3</text>
      <text x="289.8" y="315.1" text-anchor="middle" font-size="10" fill="#1f2328">71.8</text>
      <text x="364.2" y="314.3" text-anchor="middle" font-size="10" fill="#1f2328">127</text>
      <text x="413.8" y="315.2" text-anchor="middle" font-size="10" fill="#1f2328">61.0</text>
      <text x="488.2" y="312.5" text-anchor="middle" font-size="10" fill="#1f2328">266</text>
      <text x="537.8" y="310.3" text-anchor="middle" font-size="10" fill="#1f2328">440</text>
      <text x="612.2" y="229.8" text-anchor="middle" font-size="10" fill="#1f2328">6630</text>
      <text x="661.8" y="175.8" text-anchor="middle" font-size="10" fill="#1f2328">10783</text>
    </g>
    <g>
      <text x="142" y="338" text-anchor="middle" font-size="11" fill="#1f2328">int</text>
      <text x="266" y="338" text-anchor="middle" font-size="11" fill="#1f2328">string</text>
      <text x="390" y="338" text-anchor="middle" font-size="11" fill="#1f2328">bytes 1KB</text>
      <text x="514" y="338" text-anchor="middle" font-size="11" fill="#1f2328">Person struct</text>
      <text x="638" y="338" text-anchor="middle" font-size="11" fill="#1f2328">list[Person]*100</text>
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
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-fory encode + decode across representative shapes</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">5000</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">10000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">15000</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">20000</text>
    </g>
    <g>
      <rect x="92.4" y="319.0" width="47.6" height="1.0" rx="2" fill="#58a6ff"/>
      <rect x="142" y="319.2" width="47.6" height="0.8" rx="2" fill="#ff7b72"/>
      <rect x="216.4" y="318.9" width="47.6" height="1.1" rx="2" fill="#58a6ff"/>
      <rect x="266" y="319.1" width="47.6" height="0.9" rx="2" fill="#ff7b72"/>
      <rect x="340.4" y="318.3" width="47.6" height="1.7" rx="2" fill="#58a6ff"/>
      <rect x="390" y="319.2" width="47.6" height="0.8" rx="2" fill="#ff7b72"/>
      <rect x="464.4" y="316.5" width="47.6" height="3.5" rx="2" fill="#58a6ff"/>
      <rect x="514" y="314.3" width="47.6" height="5.7" rx="2" fill="#ff7b72"/>
      <rect x="588.4" y="233.8" width="47.6" height="86.2" rx="2" fill="#58a6ff"/>
      <rect x="638" y="179.8" width="47.6" height="140.2" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="116.2" y="315.0" text-anchor="middle" font-size="10" fill="#e6edf3">75.6</text>
      <text x="165.8" y="315.2" text-anchor="middle" font-size="10" fill="#e6edf3">62.5</text>
      <text x="240.2" y="314.9" text-anchor="middle" font-size="10" fill="#e6edf3">85.3</text>
      <text x="289.8" y="315.1" text-anchor="middle" font-size="10" fill="#e6edf3">71.8</text>
      <text x="364.2" y="314.3" text-anchor="middle" font-size="10" fill="#e6edf3">127</text>
      <text x="413.8" y="315.2" text-anchor="middle" font-size="10" fill="#e6edf3">61.0</text>
      <text x="488.2" y="312.5" text-anchor="middle" font-size="10" fill="#e6edf3">266</text>
      <text x="537.8" y="310.3" text-anchor="middle" font-size="10" fill="#e6edf3">440</text>
      <text x="612.2" y="229.8" text-anchor="middle" font-size="10" fill="#e6edf3">6630</text>
      <text x="661.8" y="175.8" text-anchor="middle" font-size="10" fill="#e6edf3">10783</text>
    </g>
    <g>
      <text x="142" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">int</text>
      <text x="266" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">string</text>
      <text x="390" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">bytes 1KB</text>
      <text x="514" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">Person struct</text>
      <text x="638" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">list[Person]*100</text>
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


| Operation        |  encode |   decode | ratio |
| :--------------- | ------: | -------: | ----: |
| int              | 75.7 ns |  62.5 ns | 0.83x |
| string           | 85.3 ns |  71.8 ns | 0.84x |
| bytes 1KB        |  127 ns | 60.10 ns | 0.48x |
| Person struct    |  266 ns |   440 ns | 1.65x |
| list[Person]*100 | 6630 ns | 10783 ns | 1.63x |

<sub>Last run 2026-06-27 11:35:55 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:fory-encode-decode -->

Scalar encode/decode runs under 130 ns. Struct payloads are sub-microsecond. The 100-element list benchmark shows ~67 ns per element on encode and ~107 ns per element on decode, competitive with Fory implementations in other languages.

The chart and table above are regenerated by [`wireform-stats`](../stats/) from `wireform-fory/bench-results/summary/fory-encode-decode.json` — the same source the README chart is built from.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Fory.Class` | `ToFory` / `FromFory`, primitive array newtypes, `Shared` wrapper |
| `Fory.Encode` / `Fory.Decode` | Pure encode and decode entry points |
| `Fory.IO` | In-place buffer encoder with ref and meta-string pools |
| `Fory.Options` | `EncodeOptions` / `DecodeOptions`, struct registry |
| `Fory.Struct` | `StructSchema` definitions for named struct wire layout |
| `Fory.Value` | Dynamic untyped value ADT |
| `Fory.MetaString` | Meta-string compression tables and encodings |
| `Fory.TypeId` | Wire type identifiers |
| `Fory.Derive` | Annotation-driven deriver with field renaming support |

## Interoperability

The package is verified against `pyfory` 0.17 for null, booleans, integers,
floats, strings, binary, chunked lists/sets/maps, named structs with
registered schemas, primitive arrays, reference tracking, and meta-string
compression. Cross-language interop for `NAMED_COMPATIBLE_STRUCT` (schema
evolution) is still in progress.
