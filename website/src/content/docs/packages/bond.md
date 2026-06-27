---
title: wireform-bond
description: "Microsoft Bond Compact Binary v1 with IDL codegen, packed field headers, and ZigZag varint encoding."
sidebar:
  order: 32
---

`wireform-bond` implements [Microsoft Bond](https://github.com/microsoft/bond)
Compact Binary serialization. Bond uses explicit numeric field IDs for schema
evolution, supports nullable types and struct inheritance in the IDL, and encodes
with packed delta/type headers and ZigZag varints. Use this package when you need
wire compatibility with Bond-based services (Bing, Cosmos DB, and other Microsoft
systems) or a compact, ID-driven binary format with a rich schema language.

## Key features

- **Typeclass API** via `ToBond` and `FromBond` with Template Haskell deriving
- **Compact Binary v1 wire format** with packed delta/type field headers
- **ZigZag varints** for signed integer encoding
- **Bond IDL parser and codegen** from `.bond` schema files
- **Schema AST** with type parameters, field modifiers, and custom attributes
- **QuasiQuoter** for inline `[bond| ... |]` schemas

## Basic usage

Derive instances for your record types, then encode through the Compact Binary
codec:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

import Bond.Decode qualified as BD
import Bond.Derive (ToBond (..), FromBond (..), deriveBond)
import Bond.Encode qualified as BE
import Bond.Value qualified as BV
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)

data Person = Person
  { personName  :: !Text
  , personAge   :: !Int32
  , personEmail :: !Text
  }
  deriving stock (Show, Eq, Generic)

$(deriveBond ''Person)

encodePerson :: Person -> ByteString
encodePerson = BE.encode . toBond

decodePerson :: ByteString -> Either String Person
decodePerson bs = do
  val <- BD.decode BV.BT_STRUCT bs
  fromBond val

alice :: Person
alice = Person "Alice" 30 "alice@example.com"

roundTrip :: Either String Person
roundTrip = decodePerson (encodePerson alice)
```

For schema-first workflows, parse Bond IDL and generate Haskell types:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Bond.QQ (bond)

[bond|
  struct Person {
    1: string name;
    2: int32 age;
    3: string email;
  }
|]
```

```bash
wireform-gen bond -i schema.bond -o src/Gen/
```

## Performance

### Compact Binary encode/decode

<!-- BEGIN_AUTOGEN bench:bond-encode-decode -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-bond encode + decode (Compact Binary, Person record)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-bond encode + decode (Compact Binary, Person record)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5. Decode is structurally lazy at the Value layer; the [Person] x 100 number reflects only the outer container resolution.</text>
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
      <rect x="171" y="317.8" width="62" height="2.2" rx="2" fill="#0969da"/>
      <rect x="235" y="317.5" width="62" height="2.5" rx="2" fill="#cf222e"/>
      <rect x="481" y="115.3" width="62" height="204.7" rx="2" fill="#0969da"/>
      <rect x="545" y="292.2" width="62" height="27.8" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="202" y="313.8" text-anchor="middle" font-size="10" fill="#1f2328">168</text>
      <text x="266" y="313.5" text-anchor="middle" font-size="10" fill="#1f2328">192</text>
      <text x="512" y="111.3" text-anchor="middle" font-size="10" fill="#1f2328">15742</text>
      <text x="576" y="288.2" text-anchor="middle" font-size="10" fill="#1f2328">2140</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#1f2328">single Person</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#1f2328">[Person] x 100</text>
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
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-bond encode + decode (Compact Binary, Person record)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5. Decode is structurally lazy at the Value layer; the [Person] x 100 number reflects only the outer container resolution.</text>
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
      <rect x="171" y="317.8" width="62" height="2.2" rx="2" fill="#58a6ff"/>
      <rect x="235" y="317.5" width="62" height="2.5" rx="2" fill="#ff7b72"/>
      <rect x="481" y="115.3" width="62" height="204.7" rx="2" fill="#58a6ff"/>
      <rect x="545" y="292.2" width="62" height="27.8" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="202" y="313.8" text-anchor="middle" font-size="10" fill="#e6edf3">168</text>
      <text x="266" y="313.5" text-anchor="middle" font-size="10" fill="#e6edf3">192</text>
      <text x="512" y="111.3" text-anchor="middle" font-size="10" fill="#e6edf3">15742</text>
      <text x="576" y="288.2" text-anchor="middle" font-size="10" fill="#e6edf3">2140</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">single Person</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">[Person] x 100</text>
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


| Operation      |   encode |  decode | ratio |
| :------------- | -------: | ------: | ----: |
| single Person  |   168 ns |  192 ns | 1.14x |
| [Person] x 100 | 15742 ns | 2140 ns | 0.14x |

<sub>Last run 2026-06-27 11:35:54 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5. Decode is structurally lazy at the Value layer; the [Person] x 100 number reflects only the outer container resolution..</sub>
<!-- END_AUTOGEN bench:bond-encode-decode -->

Decode is structurally lazy at the Value layer: the 100-element list decode resolves only the outer container, deferring per-element materialization until access. This makes random-access patterns fast for large payloads.

The chart and table above are regenerated by [`wireform-stats`](../stats/) from `wireform-bond/bench-results/summary/bond-encode-decode.json` — the same source the README chart is built from.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Bond.Derive` | `ToBond` / `FromBond` classes and `deriveBond` |
| `Bond.Encode` / `Bond.Decode` | Compact Binary v1 encoder and decoder |
| `Bond.Value` | Dynamic untyped `Value` ADT with Bond type tags |
| `Bond.Schema` / `Bond.Parser` | Schema AST and `.bond` IDL parser |
| `Bond.CodeGen` / `Bond.QQ` | Haskell codegen and quasiquoter |
| `Bond.Registry` | Runtime schema registry |
