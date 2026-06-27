---
title: wireform-edn
description: "Extensible Data Notation encoding and decoding with TH deriving, Clojure literals, and a JSON bridge."
sidebar:
  order: 14
---

`wireform-edn` implements Extensible Data Notation (EDN), the text-based data
format used by Clojure and many ClojureScript tools. EDN is human-readable
like JSON but adds keywords, symbols, sets, tagged literals, and richer
numeric types. Use this package when you exchange data with Clojure services,
read EDN configuration files, or need a text format that maps naturally to
Clojure's data model.

EDN is a text format, not a binary codec. Payloads are UTF-8 encoded
documents rather than compact byte streams.

## Key features

- **Template Haskell deriving** via `deriveEDN` from `EDN.Derive`, with
  `wireform-derive` annotations; Generic defaults (empty instances) work for
  simple uncustomized records
- **Clojure literals** including keywords, symbols, sets, and tagged values
- **JSON bridge** for converting between EDN and Aeson `Value`
- **Dynamic values** via the untyped `Value` ADT for schema-less parsing
- **Direct encoding** for writing into pre-allocated buffers

## Basic usage

Define a record and derive EDN codecs. Records encode as EDN maps with
keyword keys:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Point where

import EDN.Class (ToEDN, FromEDN, encodeEDN, decodeEDN)
import EDN.Derive (deriveEDN)
import GHC.Generics (Generic)

data Point = Point
  { pointX :: !Double
  , pointY :: !Double
  }
  deriving stock (Show, Eq, Generic)

$(deriveEDN ''Point)

toText :: Point -> ByteString
toText pt = encodeEDN pt

fromText :: ByteString -> Either String Point
fromText bs = decodeEDN bs
```

For simple records with no custom wire naming, Generic defaults also work:
declare empty `instance ToEDN Point` and `instance FromEDN Point` after
`deriving stock (Show, Eq, Generic)`. Field names go to the wire verbatim and
annotations are not supported.

Tagged literals use EDN's `#tag` reader syntax. Build them with the dynamic
ADT when you need custom tags:

```haskell
import EDN.Value qualified as E

uuidTag :: Text -> E.Value
uuidTag s = E.Tagged "" "uuid" (E.String s)
```

Convert between EDN and JSON when bridging to HTTP APIs or Aeson-based tools:

```haskell
import EDN.JSON (toJSON, fromJSON)
import EDN.Value qualified as E
import Data.Aeson (Value)

bridgeToJson :: E.Value -> Value
bridgeToJson edn = toJSON edn

bridgeFromJson :: Value -> E.Value
bridgeFromJson json = fromJSON json
```

## Performance

### Encode/decode (text format)

<!-- BEGIN_AUTOGEN bench:edn-encode-decode -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-edn encode + decode (Person record, text format)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-edn encode + decode (Person record, text format)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">62500</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">125000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">187500</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">250000</text>
    </g>
    <g>
      <rect x="171" y="319.2" width="62" height="0.8" rx="2" fill="#0969da"/>
      <rect x="235" y="317.9" width="62" height="2.1" rx="2" fill="#cf222e"/>
      <rect x="481" y="231.7" width="62" height="88.3" rx="2" fill="#0969da"/>
      <rect x="545" y="65.9" width="62" height="254.1" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="202" y="315.2" text-anchor="middle" font-size="10" fill="#1f2328">787</text>
      <text x="266" y="313.9" text-anchor="middle" font-size="10" fill="#1f2328">2010</text>
      <text x="512" y="227.7" text-anchor="middle" font-size="10" fill="#1f2328">84916</text>
      <text x="576" y="61.9" text-anchor="middle" font-size="10" fill="#1f2328">244287</text>
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
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-edn encode + decode (Person record, text format)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">62500</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">125000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">187500</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">250000</text>
    </g>
    <g>
      <rect x="171" y="319.2" width="62" height="0.8" rx="2" fill="#58a6ff"/>
      <rect x="235" y="317.9" width="62" height="2.1" rx="2" fill="#ff7b72"/>
      <rect x="481" y="231.7" width="62" height="88.3" rx="2" fill="#58a6ff"/>
      <rect x="545" y="65.9" width="62" height="254.1" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="202" y="315.2" text-anchor="middle" font-size="10" fill="#e6edf3">787</text>
      <text x="266" y="313.9" text-anchor="middle" font-size="10" fill="#e6edf3">2010</text>
      <text x="512" y="227.7" text-anchor="middle" font-size="10" fill="#e6edf3">84916</text>
      <text x="576" y="61.9" text-anchor="middle" font-size="10" fill="#e6edf3">244287</text>
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


| Operation      |   encode |    decode | ratio |
| :------------- | -------: | --------: | ----: |
| single Person  |   787 ns |   2010 ns | 2.55x |
| [Person] x 100 | 84916 ns | 244287 ns | 2.88x |

<sub>Last run 2026-06-27 11:35:54 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:edn-encode-decode -->

EDN is a text format, so encode/decode is naturally slower than binary formats. Single-record encode is still sub-microsecond; decode is under 2 µs.

The chart and table above are regenerated by [`wireform-stats`](../stats/) from `wireform-edn/bench-results/summary/edn-encode-decode.json` — the same source the README chart is built from.

## Notable modules

| Module | Purpose |
|--------|---------|
| `EDN.Class` | `ToEDN` / `FromEDN`, `encodeEDN`, `decodeEDN` |
| `EDN.Encode` / `EDN.Decode` | Low-level text encode and decode |
| `EDN.Value` | Dynamic `Value` ADT (keywords, symbols, sets, tags, ...) |
| `EDN.JSON` | EDN ↔ JSON conversion |
| `EDN.Derive` | Template Haskell deriver with `wireform-derive` annotations |
