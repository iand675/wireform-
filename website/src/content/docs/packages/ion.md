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
- **QuasiQuoter** for embedding Ion Schema Language declarations at compile time
- **Dynamic values** via the untyped `Value` ADT for exploratory processing

## Binary compatibility

The binary codec uses system symbol IDs and local symbol tables for field
names, symbols and ordered annotations. It handles local table replacement,
append imports, version-marker resets, duplicate/gap slots and padding.
The dynamic model supports null, bool, Int64, Double, string, symbol, blob,
clob, list, struct and annotations. Decimal, timestamp, s-expression,
typed-null and unknown-text symbol values return `Left`.

There is no shared-symbol catalog API. Imports with `max_id` reserve their
unknown SID range so later local symbols retain their correct IDs; an
unknown imported symbol or an import without `max_id` returns `Left`.
Earlier nonstandard symbol-bearing bytes, which wrote text instead of
symbol IDs, are not a supported interchange format.

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
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · ns · GHC 9.10.3 -O2 -fasm on darwin-aarch64, Criterion 1.6.5.0; TH-generated Person codecs</text>
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
      <rect x="171" y="312.8" width="62" height="7.2" rx="2" fill="#0969da"/>
      <rect x="235" y="314.1" width="62" height="5.9" rx="2" fill="#cf222e"/>
      <rect x="481" y="182.1" width="62" height="137.9" rx="2" fill="#0969da"/>
      <rect x="545" y="95.6" width="62" height="224.4" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="202" y="308.8" text-anchor="middle" font-size="10" fill="#1f2328">554</text>
      <text x="266" y="310.1" text-anchor="middle" font-size="10" fill="#1f2328">450</text>
      <text x="512" y="178.1" text-anchor="middle" font-size="10" fill="#1f2328">10608</text>
      <text x="576" y="91.6" text-anchor="middle" font-size="10" fill="#1f2328">17260</text>
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
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · ns · GHC 9.10.3 -O2 -fasm on darwin-aarch64, Criterion 1.6.5.0; TH-generated Person codecs</text>
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
      <rect x="171" y="312.8" width="62" height="7.2" rx="2" fill="#58a6ff"/>
      <rect x="235" y="314.1" width="62" height="5.9" rx="2" fill="#ff7b72"/>
      <rect x="481" y="182.1" width="62" height="137.9" rx="2" fill="#58a6ff"/>
      <rect x="545" y="95.6" width="62" height="224.4" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="202" y="308.8" text-anchor="middle" font-size="10" fill="#e6edf3">554</text>
      <text x="266" y="310.1" text-anchor="middle" font-size="10" fill="#e6edf3">450</text>
      <text x="512" y="178.1" text-anchor="middle" font-size="10" fill="#e6edf3">10608</text>
      <text x="576" y="91.6" text-anchor="middle" font-size="10" fill="#e6edf3">17260</text>
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
| single Person  |   554 ns |   450 ns | 0.81x |
| [Person] x 100 | 10608 ns | 17260 ns | 1.63x |

<sub>Last run 2026-11-04 13:05:38 UTC. GHC 9.10.3 -O2 -fasm on darwin-aarch64, Criterion 1.6.5.0; TH-generated Person codecs.</sub>
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


<!-- BEGIN_AUTOGEN bench:ion-rust-codec -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>Ion 1.0 binary codecs — owned dynamic values</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">Ion 1.0 binary codecs — owned dynamic values</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · ns · GHC 9.10.3 -O2 -fasm, Criterion 1.6.5.0; ion-rs 1.1.0, criterion.rs 0.7; darwin-aarch64</text>
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
      <rect x="90.3" y="318.7" width="39.3" height="1.3" rx="2" fill="#0969da"/>
      <rect x="131.7" y="317.5" width="39.3" height="2.5" rx="2" fill="#cf222e"/>
      <rect x="193.7" y="319.0" width="39.3" height="1.0" rx="2" fill="#0969da"/>
      <rect x="235.0" y="315.5" width="39.3" height="4.5" rx="2" fill="#cf222e"/>
      <rect x="297.0" y="305.1" width="39.3" height="14.9" rx="2" fill="#0969da"/>
      <rect x="338.3" y="303.3" width="39.3" height="16.7" rx="2" fill="#cf222e"/>
      <rect x="400.3" y="279.5" width="39.3" height="40.5" rx="2" fill="#0969da"/>
      <rect x="441.7" y="221.2" width="39.3" height="98.8" rx="2" fill="#cf222e"/>
      <rect x="503.7" y="278.2" width="39.3" height="41.8" rx="2" fill="#0969da"/>
      <rect x="545" y="274.4" width="39.3" height="45.6" rx="2" fill="#cf222e"/>
      <rect x="607" y="239.7" width="39.3" height="80.3" rx="2" fill="#0969da"/>
      <rect x="648.3" y="87.8" width="39.3" height="232.2" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="110" y="314.7" text-anchor="middle" font-size="10" fill="#1f2328">515</text>
      <text x="151.3" y="313.5" text-anchor="middle" font-size="10" fill="#1f2328">957</text>
      <text x="213.3" y="315.0" text-anchor="middle" font-size="10" fill="#1f2328">387</text>
      <text x="254.7" y="311.5" text-anchor="middle" font-size="10" fill="#1f2328">1729</text>
      <text x="316.7" y="301.1" text-anchor="middle" font-size="10" fill="#1f2328">5719</text>
      <text x="358.0" y="299.3" text-anchor="middle" font-size="10" fill="#1f2328">6413</text>
      <text x="420" y="275.5" text-anchor="middle" font-size="10" fill="#1f2328">15559</text>
      <text x="461.3" y="217.2" text-anchor="middle" font-size="10" fill="#1f2328">38016</text>
      <text x="523.3" y="274.2" text-anchor="middle" font-size="10" fill="#1f2328">16061</text>
      <text x="564.7" y="270.4" text-anchor="middle" font-size="10" fill="#1f2328">17538</text>
      <text x="626.7" y="235.7" text-anchor="middle" font-size="10" fill="#1f2328">30877</text>
      <text x="668" y="83.8" text-anchor="middle" font-size="10" fill="#1f2328">89302</text>
    </g>
    <g>
      <text x="131.7" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode Person</text>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode Person</text>
      <text x="338.3" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode [Person] x 100</text>
      <text x="441.7" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode [Person] x 100</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode symbols x 100</text>
      <text x="648.3" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode symbols x 100</text>
    </g>
    <g>
      <g transform="translate(271, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">wireform-ion</text>
      </g>
      <g transform="translate(389, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">ion-rs</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">Ion 1.0 binary codecs — owned dynamic values</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · ns · GHC 9.10.3 -O2 -fasm, Criterion 1.6.5.0; ion-rs 1.1.0, criterion.rs 0.7; darwin-aarch64</text>
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
      <rect x="90.3" y="318.7" width="39.3" height="1.3" rx="2" fill="#58a6ff"/>
      <rect x="131.7" y="317.5" width="39.3" height="2.5" rx="2" fill="#ff7b72"/>
      <rect x="193.7" y="319.0" width="39.3" height="1.0" rx="2" fill="#58a6ff"/>
      <rect x="235.0" y="315.5" width="39.3" height="4.5" rx="2" fill="#ff7b72"/>
      <rect x="297.0" y="305.1" width="39.3" height="14.9" rx="2" fill="#58a6ff"/>
      <rect x="338.3" y="303.3" width="39.3" height="16.7" rx="2" fill="#ff7b72"/>
      <rect x="400.3" y="279.5" width="39.3" height="40.5" rx="2" fill="#58a6ff"/>
      <rect x="441.7" y="221.2" width="39.3" height="98.8" rx="2" fill="#ff7b72"/>
      <rect x="503.7" y="278.2" width="39.3" height="41.8" rx="2" fill="#58a6ff"/>
      <rect x="545" y="274.4" width="39.3" height="45.6" rx="2" fill="#ff7b72"/>
      <rect x="607" y="239.7" width="39.3" height="80.3" rx="2" fill="#58a6ff"/>
      <rect x="648.3" y="87.8" width="39.3" height="232.2" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="110" y="314.7" text-anchor="middle" font-size="10" fill="#e6edf3">515</text>
      <text x="151.3" y="313.5" text-anchor="middle" font-size="10" fill="#e6edf3">957</text>
      <text x="213.3" y="315.0" text-anchor="middle" font-size="10" fill="#e6edf3">387</text>
      <text x="254.7" y="311.5" text-anchor="middle" font-size="10" fill="#e6edf3">1729</text>
      <text x="316.7" y="301.1" text-anchor="middle" font-size="10" fill="#e6edf3">5719</text>
      <text x="358.0" y="299.3" text-anchor="middle" font-size="10" fill="#e6edf3">6413</text>
      <text x="420" y="275.5" text-anchor="middle" font-size="10" fill="#e6edf3">15559</text>
      <text x="461.3" y="217.2" text-anchor="middle" font-size="10" fill="#e6edf3">38016</text>
      <text x="523.3" y="274.2" text-anchor="middle" font-size="10" fill="#e6edf3">16061</text>
      <text x="564.7" y="270.4" text-anchor="middle" font-size="10" fill="#e6edf3">17538</text>
      <text x="626.7" y="235.7" text-anchor="middle" font-size="10" fill="#e6edf3">30877</text>
      <text x="668" y="83.8" text-anchor="middle" font-size="10" fill="#e6edf3">89302</text>
    </g>
    <g>
      <text x="131.7" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode Person</text>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode Person</text>
      <text x="338.3" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode [Person] x 100</text>
      <text x="441.7" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode [Person] x 100</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode symbols x 100</text>
      <text x="648.3" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode symbols x 100</text>
    </g>
    <g>
      <g transform="translate(271, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">wireform-ion</text>
      </g>
      <g transform="translate(389, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">ion-rs</text>
      </g>
    </g>
  </g>
</svg>


| Operation             | wireform-ion |   ion-rs | ratio |
| :-------------------- | -----------: | -------: | ----: |
| encode Person         |       515 ns |   957 ns | 0.54x |
| decode Person         |       387 ns |  1729 ns | 0.22x |
| encode [Person] x 100 |      5719 ns |  6413 ns | 0.89x |
| decode [Person] x 100 |     15559 ns | 38016 ns | 0.41x |
| encode symbols x 100  |     16061 ns | 17538 ns | 0.92x |
| decode symbols x 100  |     30877 ns | 89302 ns | 0.35x |

<sub>Last run 2026-11-04 13:06:38 UTC. GHC 9.10.3 -O2 -fasm, Criterion 1.6.5.0; ion-rs 1.1.0, criterion.rs 0.7; darwin-aarch64.</sub>
<!-- END_AUTOGEN bench:ion-rust-codec -->

### Generated typed records

The typed Rust reference uses `ion-rs` 1.1.0's experimental serde bridge;
this is separate from the owned dynamic-value codec comparison. Fixture
construction is outside the clock on both sides.

<!-- BEGIN_AUTOGEN bench:ion-rust-typed -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>Ion 1.0 generated typed record codecs</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">Ion 1.0 generated typed record codecs</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · ns · GHC 9.10.3 -O2 -fasm, TH-derived records, Criterion 1.6.5.0; ion-rs 1.1.0 experimental-serde, serde derive, criterion.rs 0.7; darwin-aarch64</text>
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
      <rect x="95.5" y="317.1" width="60" height="2.9" rx="2" fill="#0969da"/>
      <rect x="157.5" y="314.0" width="60" height="6.0" rx="2" fill="#cf222e"/>
      <rect x="250.5" y="317.7" width="60" height="2.3" rx="2" fill="#0969da"/>
      <rect x="312.5" y="311.0" width="60" height="9.0" rx="2" fill="#cf222e"/>
      <rect x="405.5" y="264.8" width="60" height="55.2" rx="2" fill="#0969da"/>
      <rect x="467.5" y="229.0" width="60" height="91.0" rx="2" fill="#cf222e"/>
      <rect x="560.5" y="230.3" width="60" height="89.7" rx="2" fill="#0969da"/>
      <rect x="622.5" y="149.1" width="60" height="170.9" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="125.5" y="313.1" text-anchor="middle" font-size="10" fill="#1f2328">554</text>
      <text x="187.5" y="310.0" text-anchor="middle" font-size="10" fill="#1f2328">1153</text>
      <text x="280.5" y="313.7" text-anchor="middle" font-size="10" fill="#1f2328">450</text>
      <text x="342.5" y="307.0" text-anchor="middle" font-size="10" fill="#1f2328">1731</text>
      <text x="435.5" y="260.8" text-anchor="middle" font-size="10" fill="#1f2328">10608</text>
      <text x="497.5" y="225.0" text-anchor="middle" font-size="10" fill="#1f2328">17498</text>
      <text x="590.5" y="226.3" text-anchor="middle" font-size="10" fill="#1f2328">17260</text>
      <text x="652.5" y="145.1" text-anchor="middle" font-size="10" fill="#1f2328">32871</text>
    </g>
    <g>
      <text x="157.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode Person</text>
      <text x="312.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode Person</text>
      <text x="467.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode [Person] x 100</text>
      <text x="622.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode [Person] x 100</text>
    </g>
    <g>
      <g transform="translate(250, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">wireform-ion</text>
      </g>
      <g transform="translate(368, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">ion-rs serde</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">Ion 1.0 generated typed record codecs</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · ns · GHC 9.10.3 -O2 -fasm, TH-derived records, Criterion 1.6.5.0; ion-rs 1.1.0 experimental-serde, serde derive, criterion.rs 0.7; darwin-aarch64</text>
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
      <rect x="95.5" y="317.1" width="60" height="2.9" rx="2" fill="#58a6ff"/>
      <rect x="157.5" y="314.0" width="60" height="6.0" rx="2" fill="#ff7b72"/>
      <rect x="250.5" y="317.7" width="60" height="2.3" rx="2" fill="#58a6ff"/>
      <rect x="312.5" y="311.0" width="60" height="9.0" rx="2" fill="#ff7b72"/>
      <rect x="405.5" y="264.8" width="60" height="55.2" rx="2" fill="#58a6ff"/>
      <rect x="467.5" y="229.0" width="60" height="91.0" rx="2" fill="#ff7b72"/>
      <rect x="560.5" y="230.3" width="60" height="89.7" rx="2" fill="#58a6ff"/>
      <rect x="622.5" y="149.1" width="60" height="170.9" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="125.5" y="313.1" text-anchor="middle" font-size="10" fill="#e6edf3">554</text>
      <text x="187.5" y="310.0" text-anchor="middle" font-size="10" fill="#e6edf3">1153</text>
      <text x="280.5" y="313.7" text-anchor="middle" font-size="10" fill="#e6edf3">450</text>
      <text x="342.5" y="307.0" text-anchor="middle" font-size="10" fill="#e6edf3">1731</text>
      <text x="435.5" y="260.8" text-anchor="middle" font-size="10" fill="#e6edf3">10608</text>
      <text x="497.5" y="225.0" text-anchor="middle" font-size="10" fill="#e6edf3">17498</text>
      <text x="590.5" y="226.3" text-anchor="middle" font-size="10" fill="#e6edf3">17260</text>
      <text x="652.5" y="145.1" text-anchor="middle" font-size="10" fill="#e6edf3">32871</text>
    </g>
    <g>
      <text x="157.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode Person</text>
      <text x="312.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode Person</text>
      <text x="467.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode [Person] x 100</text>
      <text x="622.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode [Person] x 100</text>
    </g>
    <g>
      <g transform="translate(250, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">wireform-ion</text>
      </g>
      <g transform="translate(368, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">ion-rs serde</text>
      </g>
    </g>
  </g>
</svg>


| Operation             | wireform-ion | ion-rs serde | ratio |
| :-------------------- | -----------: | -----------: | ----: |
| encode Person         |       554 ns |      1153 ns | 0.48x |
| decode Person         |       450 ns |      1731 ns | 0.26x |
| encode [Person] x 100 |     10608 ns |     17498 ns | 0.61x |
| decode [Person] x 100 |     17260 ns |     32871 ns | 0.53x |

<sub>Last run 2026-11-04 13:07:17 UTC. GHC 9.10.3 -O2 -fasm, TH-derived records, Criterion 1.6.5.0; ion-rs 1.1.0 experimental-serde, serde derive, criterion.rs 0.7; darwin-aarch64.</sub>
<!-- END_AUTOGEN bench:ion-rust-typed -->
