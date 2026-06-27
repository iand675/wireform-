---
title: wireform-capnproto
description: "Cap'n Proto zero-copy serialization with IDL codegen and segment-based wire layout."
sidebar:
  order: 33
---

`wireform-capnproto` implements [Cap'n Proto](https://capnproto.org/), Kenton
Varda's zero-copy serialization framework. Cap'n Proto splits structs into a fixed
data section (scalars packed by size) and a pointer section (text, lists, nested
structs), making buffers directly mappable for read-heavy workloads. Use this
package when you need mmap-friendly serialization with strict schema evolution
rules, or when integrating with Cap'n Proto services and `.capnp` schema files.

## Key features

- **Typeclass API** via `ToCapnProto` and `FromCapnProto` with Template Haskell deriving
- **Cap'n Proto IDL parser and codegen** from `.capnp` schema files
- **Segment-based wire layout** with separate data and pointer sections
- **Zero-copy-oriented decode** that reconstructs values from mapped buffers
- **QuasiQuoter** for inline `[capnp| ... |]` schemas
- **Runtime registry** for struct schema lookup

## Basic usage

For typed records, derive instances and round-trip through the segment encoder:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

import CapnProto.Decode qualified as CPD
import CapnProto.Derive (ToCapnProto (..), FromCapnProto (..), deriveCapnProto)
import CapnProto.Encode qualified as CPE
import Data.Text (Text)
import Data.Word (Word32)
import GHC.Generics (Generic)

data Person = Person
  { personName :: !Text
  , personAge  :: !Word32
  }
  deriving stock (Show, Eq, Generic)

$(deriveCapnProto ''Person)

encodePerson :: Person -> ByteString
encodePerson = CPE.encode . toCapnProto

decodePerson :: ByteString -> Either String Person
decodePerson bs = do
  val <- CPD.decode bs
  fromCapnProto val

bob :: Person
bob = Person "Bob" 42

roundTrip :: Either String Person
roundTrip = decodePerson (encodePerson bob)
```

You can also work directly with the dynamic `Value` ADT when exploring wire
layout or bridging between schemas:

```haskell
import qualified Data.Vector as V
import qualified CapnProto.Value as CP

manualStruct :: CP.Value
manualStruct = CP.Struct
  (V.fromList [CP.UInt32 42])           -- data section
  (V.fromList [CP.Text "hello capnp"]) -- pointer section

manualBytes :: ByteString
manualBytes = CPE.encode manualStruct
```

Generate types from `.capnp` files with the quasiquoter or CLI:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import CapnProto.QQ (capnp)

[capnp|
  struct Person {
    name @0 :Text;
    age @1 :UInt32;
  }
|]
```

```bash
wireform-gen capnp -i schema.capnp -o src/Gen/
```

## Performance

### Encode/decode (zero-copy decode)

<!-- BEGIN_AUTOGEN bench:capnproto-encode-decode -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-capnproto encode + decode (zero-copy decode)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-capnproto encode + decode (zero-copy decode)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5. Decode is a zero-copy cursor by design: only the outer envelope is resolved at decode time. Per-field reads happen lazily.</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">2500</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">5000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">7500</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">10000</text>
    </g>
    <g>
      <rect x="171" y="317.0" width="62" height="3.0" rx="2" fill="#0969da"/>
      <rect x="235" y="319.3" width="62" height="0.7" rx="2" fill="#cf222e"/>
      <rect x="481" y="96.2" width="62" height="223.8" rx="2" fill="#0969da"/>
      <rect x="545" y="319.3" width="62" height="0.7" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="202" y="313.0" text-anchor="middle" font-size="10" fill="#1f2328">114</text>
      <text x="266" y="315.3" text-anchor="middle" font-size="10" fill="#1f2328">28.4</text>
      <text x="512" y="92.2" text-anchor="middle" font-size="10" fill="#1f2328">8610</text>
      <text x="576" y="315.3" text-anchor="middle" font-size="10" fill="#1f2328">28.0</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#1f2328">Person struct</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#1f2328">Person[100]</text>
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
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-capnproto encode + decode (zero-copy decode)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5. Decode is a zero-copy cursor by design: only the outer envelope is resolved at decode time. Per-field reads happen lazily.</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">2500</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">5000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">7500</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">10000</text>
    </g>
    <g>
      <rect x="171" y="317.0" width="62" height="3.0" rx="2" fill="#58a6ff"/>
      <rect x="235" y="319.3" width="62" height="0.7" rx="2" fill="#ff7b72"/>
      <rect x="481" y="96.2" width="62" height="223.8" rx="2" fill="#58a6ff"/>
      <rect x="545" y="319.3" width="62" height="0.7" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="202" y="313.0" text-anchor="middle" font-size="10" fill="#e6edf3">114</text>
      <text x="266" y="315.3" text-anchor="middle" font-size="10" fill="#e6edf3">28.4</text>
      <text x="512" y="92.2" text-anchor="middle" font-size="10" fill="#e6edf3">8610</text>
      <text x="576" y="315.3" text-anchor="middle" font-size="10" fill="#e6edf3">28.0</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">Person struct</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">Person[100]</text>
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


| Operation     |  encode |  decode | ratio |
| :------------ | ------: | ------: | ----: |
| Person struct |  114 ns | 28.4 ns | 0.25x |
| Person[100]   | 8610 ns | 28.0 ns | 0.00x |

<sub>Last run 2026-06-27 11:35:54 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5. Decode is a zero-copy cursor by design: only the outer envelope is resolved at decode time. Per-field reads happen lazily..</sub>
<!-- END_AUTOGEN bench:capnproto-encode-decode -->

Decode is effectively O(1) regardless of payload size because Cap'n Proto uses zero-copy cursors: only the outer envelope is resolved at decode time, and per-field reads happen lazily on access. Encode is proportional to message size.

The chart and table above are regenerated by [`wireform-stats`](../stats/) from `wireform-capnproto/bench-results/summary/capnproto-encode-decode.json` — the same source the README chart is built from.

## Notable modules

| Module | Purpose |
|--------|---------|
| `CapnProto.Derive` | `ToCapnProto` / `FromCapnProto` and `deriveCapnProto` |
| `CapnProto.Encode` / `CapnProto.Decode` | Segment encoder and decoder |
| `CapnProto.Value` | Dynamic untyped `Value` ADT (data + pointer sections) |
| `CapnProto.Schema` / `CapnProto.Parser` | Schema AST and `.capnp` parser |
| `CapnProto.CodeGen` / `CapnProto.QQ` | Haskell codegen and quasiquoter |
| `CapnProto.Registry` | Runtime struct schema registry |
