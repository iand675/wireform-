---
title: wireform-bson
description: "MongoDB BSON encoding and decoding with TH deriving, wireform-derive annotations, and full MongoDB element types."
sidebar:
  order: 12
---

`wireform-bson` implements BSON, the binary document format used by MongoDB
on the wire and in storage. BSON extends JSON-like documents with typed
fields, binary subtypes, and MongoDB-specific types such as `ObjectId` and
`Decimal128`. Use this package when you talk to MongoDB drivers, parse
change streams, or exchange documents with services that speak BSON rather
than JSON.

## Key features

- **Template Haskell deriving** via `deriveBSON` for Haskell record types,
  with `wireform-derive` annotations; Generic defaults (empty instances) work
  for simple cases
- **Full MongoDB element set** including `ObjectId`, `Decimal128`,
  JavaScript code, regex, timestamps, and MinKey/MaxKey
- **Binary subtypes** for UUID, user-defined payloads, and other BSON binary
  conventions
- **Dynamic values** via the untyped `Value` ADT for schema-less documents
- **Direct encoding** for pre-sized buffer writes on hot paths

## Basic usage

Map a Haskell record to a BSON document with the Template Haskell deriver:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module UserDoc where

import BSON.Class (ToBSON, FromBSON, encodeBSON, decodeBSON)
import BSON.Derive (deriveBSON)
import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Data.Text (Text)

data User = User
  { userName :: !Text
  , userAge  :: !Int
  , userId   :: !ByteString
  }
  deriving stock (Show, Eq, Generic)

$(deriveBSON ''User)

insertBytes :: User -> ByteString
insertBytes user = encodeBSON user

readUser :: ByteString -> Either String User
readUser bs = decodeBSON bs
```

For simple cases with no wire-format customization, Generic defaults also
work: add `deriving Generic` and declare empty `instance ToBSON User` and
`instance FromBSON User` declarations.

When you need MongoDB-specific field types, model them with the `Value`
constructors and use the dynamic ADT, or wrap the wire shapes in newtypes
with custom instances:

```haskell
import BSON.Value qualified as B
import Data.Vector qualified as V

paymentDoc :: B.Value
paymentDoc =
  B.Document $
    V.fromList
      [ ("amount", B.Decimal128 amountBytes)
      , ("note", B.JavaScript "function() { return true; }")
      , ("tags", B.Regex "paid" "i")
      ]
```

For documents whose shape is only known at runtime, work with the dynamic ADT:

```haskell
import BSON.Value qualified as B
import BSON.Encode (encode)
import BSON.Decode (decode)
import Data.Vector qualified as V

lookupName :: B.Value -> Maybe Text
lookupName doc =
  case doc of
    B.Document fields ->
      case V.find ((== "name") . fst) (V.toList fields) of
        Just (_, B.String t) -> Just t
        _                    -> Nothing
    _ -> Nothing
```

## Performance

### Encode/decode

<!-- BEGIN_AUTOGEN bench:bson-encode-decode -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-bson encode + decode (Person record)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-bson encode + decode (Person record)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
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
      <rect x="171" y="319.2" width="62" height="0.8" rx="2" fill="#0969da"/>
      <rect x="235" y="319.0" width="62" height="1.0" rx="2" fill="#cf222e"/>
      <rect x="481" y="176.1" width="62" height="143.9" rx="2" fill="#0969da"/>
      <rect x="545" y="258.7" width="62" height="61.3" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="202" y="315.2" text-anchor="middle" font-size="10" fill="#1f2328">309</text>
      <text x="266" y="315.0" text-anchor="middle" font-size="10" fill="#1f2328">399</text>
      <text x="512" y="172.1" text-anchor="middle" font-size="10" fill="#1f2328">55343</text>
      <text x="576" y="254.7" text-anchor="middle" font-size="10" fill="#1f2328">23567</text>
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
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-bson encode + decode (Person record)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
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
      <rect x="171" y="319.2" width="62" height="0.8" rx="2" fill="#58a6ff"/>
      <rect x="235" y="319.0" width="62" height="1.0" rx="2" fill="#ff7b72"/>
      <rect x="481" y="176.1" width="62" height="143.9" rx="2" fill="#58a6ff"/>
      <rect x="545" y="258.7" width="62" height="61.3" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="202" y="315.2" text-anchor="middle" font-size="10" fill="#e6edf3">309</text>
      <text x="266" y="315.0" text-anchor="middle" font-size="10" fill="#e6edf3">399</text>
      <text x="512" y="172.1" text-anchor="middle" font-size="10" fill="#e6edf3">55343</text>
      <text x="576" y="254.7" text-anchor="middle" font-size="10" fill="#e6edf3">23567</text>
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


| Operation      |   encode |   decode | ratio |
| :------------- | -------: | -------: | ----: |
| single Person  |   309 ns |   399 ns | 1.29x |
| [Person] x 100 | 55343 ns | 23567 ns | 0.43x |

<sub>Last run 2026-06-27 11:35:54 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:bson-encode-decode -->

Single-record round-trips under a microsecond. Batch decode is faster than encode because the BSON wire format allows scanning without full materialization of nested documents.

The chart and table above are regenerated by [`wireform-stats`](../stats/) from `wireform-bson/bench-results/summary/bson-encode-decode.json` — the same source the README chart is built from.

## Notable modules

| Module | Purpose |
|--------|---------|
| `BSON.Class` | `ToBSON` / `FromBSON`, `encodeBSON`, `decodeBSON` |
| `BSON.Encode` / `BSON.Decode` | Low-level wire encode and decode |
| `BSON.Value` | Dynamic `Value` ADT and MongoDB-specific types (`ObjectId`, `Decimal128`, `Regex`, ...) |
| `BSON.Derive` | Template Haskell deriver with `wireform-derive` annotations |
