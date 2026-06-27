---
title: wireform-msgpack
description: "MessagePack encoding and decoding with TH deriving, streaming decode, msgpack-RPC, and a JSON bridge."
sidebar:
  order: 11
---

`wireform-msgpack` implements the MessagePack binary serialization format.
MessagePack is widely used for RPC, caching, and inter-service communication
because it is compact, fast to parse, and supported by libraries in most
languages. Use this package when you want a lightweight alternative to JSON
with similar flexibility but smaller payloads.

## Key features

- **Template Haskell deriving** via `deriveMsgPack` for records, enums, and sum
  types, with `wireform-derive` annotations; Generic defaults (empty instances)
  work for simple cases
- **Streaming decode** for concatenated or length-prefixed MessagePack frames
- **msgpack-RPC** message encoding for request/response/notification patterns
- **JSON bridge** for converting between MessagePack and Aeson `Value`
- **Dynamic values** via the untyped `Value` ADT when schemas are unknown at
  compile time

## Basic usage

Define a type and derive codec instances with Template Haskell:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Person where

import MsgPack.Class (ToMsgPack, FromMsgPack, encodeMsgPack, decodeMsgPack)
import MsgPack.Derive (deriveMsgPack)
import GHC.Generics (Generic)
import Data.Text (Text)

data Person = Person
  { personName :: !Text
  , personAge  :: !Int
  }
  deriving stock (Show, Eq, Generic)

$(deriveMsgPack ''Person)

roundTrip :: Person -> Either String Person
roundTrip p =
  case decodeMsgPack (encodeMsgPack p) of
    Left err  -> Left err
    Right val -> Right val
```

For simple cases with no wire-format customization, Generic defaults also
work: add `deriving Generic` and declare empty `instance ToMsgPack Person` and
`instance FromMsgPack Person` declarations.

For RPC-style messaging, use the msgpack-RPC envelope helpers:

```haskell
import MsgPack.RPC (RPCMessage(..), encodeRPC, decodeRPC)
import Data.Vector (Vector)
import qualified Data.Vector as V
import MsgPack.Value qualified as MV

call :: Text -> Vector MV.Value -> ByteString
call method params =
  encodeRPC (RPCRequest 1 method params)

handle :: ByteString -> Either String RPCMessage
handle = decodeRPC
```

When processing a buffer that may contain multiple MessagePack values, decode
one at a time and advance the cursor:

```haskell
import MsgPack.Stream (decodeOneWithLeftover)

takeNext :: ByteString -> Either String (MV.Value, ByteString)
takeNext = decodeOneWithLeftover
```

To inspect or transform values without generated types, round-trip through
the dynamic ADT:

```haskell
import MsgPack.Value qualified as MV
import MsgPack.Encode (encode)
import MsgPack.Decode (decode)

dynamicRoundTrip :: MV.Value -> Either String MV.Value
dynamicRoundTrip val =
  case decode (encode val) of
    Left err  -> Left err
    Right out -> Right out
```

## Performance

wireform-msgpack is ~4x faster than the Hackage `msgpack` package on both
encode and decode for a typical record payload.

### wireform-msgpack vs Hackage `msgpack`

<!-- BEGIN_AUTOGEN bench:msgpack-vs-msgpack-haskell -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-msgpack vs Hackage msgpack</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-msgpack vs Hackage msgpack</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">500</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">1000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">1500</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">2000</text>
    </g>
    <g>
      <rect x="171" y="281.3" width="62" height="38.7" rx="2" fill="#0969da"/>
      <rect x="235" y="159.9" width="62" height="160.1" rx="2" fill="#cf222e"/>
      <rect x="481" y="269.6" width="62" height="50.4" rx="2" fill="#0969da"/>
      <rect x="545" y="89.2" width="62" height="230.8" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="202" y="277.3" text-anchor="middle" font-size="10" fill="#1f2328">297</text>
      <text x="266" y="155.9" text-anchor="middle" font-size="10" fill="#1f2328">1232</text>
      <text x="512" y="265.6" text-anchor="middle" font-size="10" fill="#1f2328">388</text>
      <text x="576" y="85.2" text-anchor="middle" font-size="10" fill="#1f2328">1775</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode</text>
    </g>
    <g>
      <g transform="translate(253.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">wireform-msgpack</text>
      </g>
      <g transform="translate(399.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">msgpack</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-msgpack vs Hackage msgpack</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">500</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">1000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">1500</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">2000</text>
    </g>
    <g>
      <rect x="171" y="281.3" width="62" height="38.7" rx="2" fill="#58a6ff"/>
      <rect x="235" y="159.9" width="62" height="160.1" rx="2" fill="#ff7b72"/>
      <rect x="481" y="269.6" width="62" height="50.4" rx="2" fill="#58a6ff"/>
      <rect x="545" y="89.2" width="62" height="230.8" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="202" y="277.3" text-anchor="middle" font-size="10" fill="#e6edf3">297</text>
      <text x="266" y="155.9" text-anchor="middle" font-size="10" fill="#e6edf3">1232</text>
      <text x="512" y="265.6" text-anchor="middle" font-size="10" fill="#e6edf3">388</text>
      <text x="576" y="85.2" text-anchor="middle" font-size="10" fill="#e6edf3">1775</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode</text>
    </g>
    <g>
      <g transform="translate(253.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">wireform-msgpack</text>
      </g>
      <g transform="translate(399.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">msgpack</text>
      </g>
    </g>
  </g>
</svg>


| Operation | wireform-msgpack | msgpack | ratio |
| :-------- | ---------------: | ------: | ----: |
| encode    |           297 ns | 1232 ns | 4.14x |
| decode    |           388 ns | 1775 ns | 4.58x |

<sub>Last run 2026-06-27 11:56:42 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:msgpack-vs-msgpack-haskell -->

The chart and table above are regenerated by [`wireform-stats`](../stats/) from `wireform-msgpack/bench-results/summary/msgpack-vs-msgpack-haskell.json` — the same source the README chart is built from.

## Notable modules

| Module | Purpose |
|--------|---------|
| `MsgPack.Class` | `ToMsgPack` / `FromMsgPack`, `encodeMsgPack`, `decodeMsgPack` |
| `MsgPack.Encode` / `MsgPack.Decode` | Low-level wire encode and decode |
| `MsgPack.Value` | Dynamic untyped `Value` ADT |
| `MsgPack.Stream` | Incremental decode for framed input |
| `MsgPack.RPC` | msgpack-RPC request, response, and notification messages |
| `MsgPack.JSON` | MessagePack ↔ JSON conversion |
| `MsgPack.Derive` | Template Haskell deriver with `wireform-derive` annotations |
