---
title: wireform-ion
description: "Amazon Ion binary encoding and decoding with TH deriving, Ion Schema Language support, and a QuasiQuoter."
sidebar:
  order: 13
---

`wireform-ion` implements Amazon Ion, a superset of JSON designed for
high-volume structured data in AWS services such as QLDB and Ion-based data
lakes. Ion supports symbols, timestamps, decimals, and a rich type system in
both text and binary forms. Use this package when you exchange Ion payloads
with AWS tooling or need schema-checked Ion documents in Haskell.

## Key features

- **Template Haskell deriving** via `deriveIon` from `Ion.Derive`, with
  `wireform-derive` annotations; Generic defaults (empty instances) work for
  simple uncustomized records
- **Ion Schema Language (ISL)** parser for declarative schema definitions
- **Schema-driven codegen** that emits Haskell types and codec stubs from ISL
- **QuasiQuoter** for embedding Ion text literals at compile time
- **Dynamic values** via the untyped `Value` ADT for exploratory processing

## Basic usage

Derive Ion codecs for a record and round-trip through binary Ion:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Metrics where

import Ion.Class (ToIon, FromIon, encodeIon, decodeIon)
import Ion.Derive (deriveIon)
import GHC.Generics (Generic)
import Data.Text (Text)

data Metric = Metric
  { metricName  :: !Text
  , metricValue :: !Double
  }
  deriving stock (Show, Eq, Generic)

$(deriveIon ''Metric)

publish :: Metric -> ByteString
publish m = encodeIon m

consume :: ByteString -> Either String Metric
consume bs = decodeIon bs
```

For simple records with no custom wire naming, Generic defaults also work:
declare empty `instance ToIon Metric` and `instance FromIon Metric` after
`deriving stock (Show, Eq, Generic)`. Field names go to the wire verbatim and
annotations are not supported.

For schema-first workflows, define types in ISL and splice them at compile
time with the QuasiQuoter:

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module MetricsSchema where

import Ion.QQ (isl)

[isl|
  type::{ name: Metric, fields: { name: string, value: float } }
|]
```

This parses the ISL definition and generates a Haskell record with `ToIon`
and `FromIon` instances that match the schema field names and types.

## Performance

### Encode/decode (binary Ion)

<!-- BEGIN_AUTOGEN bench:ion-encode-decode -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-ion encode + decode (Person record)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-ion encode + decode (Person record)</text>
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
      <rect x="171" y="318.3" width="62" height="1.7" rx="2" fill="#0969da"/>
      <rect x="235" y="317.9" width="62" height="2.1" rx="2" fill="#cf222e"/>
      <rect x="481" y="120.1" width="62" height="199.9" rx="2" fill="#0969da"/>
      <rect x="545" y="102.5" width="62" height="217.5" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="202" y="314.3" text-anchor="middle" font-size="10" fill="#1f2328">330</text>
      <text x="266" y="313.9" text-anchor="middle" font-size="10" fill="#1f2328">403</text>
      <text x="512" y="116.1" text-anchor="middle" font-size="10" fill="#1f2328">38449</text>
      <text x="576" y="98.5" text-anchor="middle" font-size="10" fill="#1f2328">41835</text>
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
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-ion encode + decode (Person record)</text>
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
      <rect x="171" y="318.3" width="62" height="1.7" rx="2" fill="#58a6ff"/>
      <rect x="235" y="317.9" width="62" height="2.1" rx="2" fill="#ff7b72"/>
      <rect x="481" y="120.1" width="62" height="199.9" rx="2" fill="#58a6ff"/>
      <rect x="545" y="102.5" width="62" height="217.5" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="202" y="314.3" text-anchor="middle" font-size="10" fill="#e6edf3">330</text>
      <text x="266" y="313.9" text-anchor="middle" font-size="10" fill="#e6edf3">403</text>
      <text x="512" y="116.1" text-anchor="middle" font-size="10" fill="#e6edf3">38449</text>
      <text x="576" y="98.5" text-anchor="middle" font-size="10" fill="#e6edf3">41835</text>
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
| single Person  |   330 ns |   403 ns | 1.22x |
| [Person] x 100 | 38449 ns | 41835 ns | 1.09x |

<sub>Last run 2026-06-27 11:35:54 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:ion-encode-decode -->

Sub-microsecond single-record performance. Batch operations scale linearly.

The chart and table above are regenerated by [`wireform-stats`](../stats/) from `wireform-ion/bench-results/summary/ion-encode-decode.json` — the same source the README chart is built from.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Ion.Class` | `ToIon` / `FromIon`, `encodeIon`, `decodeIon` |
| `Ion.Encode` / `Ion.Decode` | Low-level binary Ion encode and decode |
| `Ion.Value` | Dynamic untyped `Value` ADT |
| `Ion.SchemaLang` | Ion Schema Language parser |
| `Ion.ISLSchema` / `Ion.ISLCodeGen` | ISL AST and Haskell code generator |
| `Ion.QQ` | QuasiQuoter for Ion text literals |
| `Ion.Derive` | Template Haskell deriver with `wireform-derive` annotations |
