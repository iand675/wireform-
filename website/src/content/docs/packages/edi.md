---
title: wireform-edi
description: "Positional EDI segments, X12 delimiter inference, release-aware EDIFACT codecs, envelope checks and TH deriving."
sidebar:
  order: 15
---

`wireform-edi` models ordered segments and positional simple/composite
fields. It provides generic delimiter codecs, X12 ISA inference and
UN/EDIFACT UNA advice, release-aware encoding/decoding, and envelope count
and control-reference checks. `EDI.Derive` generates typed positional
segment codecs using the shared `wireform-derive` annotations.

## EDIFACT example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Vector qualified as V
import EDI.EDIFACT
import EDI.Value

example :: Either String Interchange
example = do
  let advice = defaultServiceStringAdvice
      doc = Interchange (serviceStringSyntax advice)
        (V.singleton (Segment "FTX" (V.fromList [Simple "?", Simple "X"])))
  bytes <- encodeEDIFACTBS advice doc -- FTX+??+X'
  decodeEDIFACTBS bytes
```

Use the EDIFACT encoder when payloads contain literal service characters.
Its explicit advice must match the document syntax, and custom advice is
written as UNA. The generic `EDI.Encode` renderer writes literal fields
with the supplied delimiters; it does not guess a release character.
The byte decoders reject invalid UTF-8 with `Left`.

## Scope

X12 and EDIFACT envelope validation is separate from lexical decoding.
The package does not include business-message directory validation. Its
standard-family inventory also detects GS1, TRADACOMS, ODETTE, VDA, HL7,
HIPAA, IATA Cargo-IMP, NCPDP and EDIGAS profiles; this is not a claim of
complete payload codecs for all those families. XML and fixed-width
payloads require dedicated representations. The segment ADT currently
represents simple and composite elements, without a repeated-element AST.
A one-component composite has the same wire representation as a simple
field, so use at least two components when that distinction matters.

## Verification and performance

The package test suite includes deriving tests, both reported #82 decoder
regressions, release-rich Unicode round trips, custom service advice,
control-count overflow rejection and long escaped fields. Optional tests
cross-read and rewrite with the pinned Rust `edifact-rs` reference.

```sh
cabal test wireform-edi:wireform-edi-derive-test
python3 scripts/run-benchmarks.py --only edi
```

The benchmark compares UTF-8 input/output, plain and released fields,
1 and 1,000 segments, and generated typed records. It performs lexical
codec work on both sides; envelope and directory validation are outside
these measurements.

<!-- BEGIN_AUTOGEN bench:edi-rust-codec -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>EDIFACT UTF-8 codecs — plain, released and generated typed records</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">EDIFACT UTF-8 codecs — plain, released and generated typed records</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · ns · GHC 9.10.3 -O2 -fasm, Criterion 1.6.5.0; edifact-rs 0.18.0, criterion.rs 0.7; darwin-aarch64</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">125000</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">250000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">375000</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">500000</text>
    </g>
    <g>
      <rect x="86.2" y="320.0" width="22.8" height="0.0" rx="2" fill="#0969da"/>
      <rect x="111" y="319.9" width="22.8" height="0.1" rx="2" fill="#cf222e"/>
      <rect x="148.2" y="319.9" width="22.8" height="0.1" rx="2" fill="#0969da"/>
      <rect x="173" y="319.8" width="22.8" height="0.2" rx="2" fill="#cf222e"/>
      <rect x="210.2" y="296.6" width="22.8" height="23.4" rx="2" fill="#0969da"/>
      <rect x="235" y="292.3" width="22.8" height="27.7" rx="2" fill="#cf222e"/>
      <rect x="272.2" y="192.7" width="22.8" height="127.3" rx="2" fill="#0969da"/>
      <rect x="297" y="181.6" width="22.8" height="138.4" rx="2" fill="#cf222e"/>
      <rect x="334.2" y="320.0" width="22.8" height="0.0" rx="2" fill="#0969da"/>
      <rect x="359" y="319.9" width="22.8" height="0.1" rx="2" fill="#cf222e"/>
      <rect x="396.2" y="319.8" width="22.8" height="0.2" rx="2" fill="#0969da"/>
      <rect x="421" y="319.8" width="22.8" height="0.2" rx="2" fill="#cf222e"/>
      <rect x="458.2" y="291.5" width="22.8" height="28.5" rx="2" fill="#0969da"/>
      <rect x="483" y="278.9" width="22.8" height="41.1" rx="2" fill="#cf222e"/>
      <rect x="520.2" y="186.5" width="22.8" height="133.5" rx="2" fill="#0969da"/>
      <rect x="545" y="156.8" width="22.8" height="163.2" rx="2" fill="#cf222e"/>
      <rect x="582.2" y="320.0" width="22.8" height="0.0" rx="2" fill="#0969da"/>
      <rect x="607" y="319.9" width="22.8" height="0.1" rx="2" fill="#cf222e"/>
      <rect x="644.2" y="319.9" width="22.8" height="0.1" rx="2" fill="#0969da"/>
      <rect x="669" y="319.8" width="22.8" height="0.2" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="97.6" y="316.0" text-anchor="middle" font-size="10" fill="#1f2328">74.0</text>
      <text x="122.4" y="315.9" text-anchor="middle" font-size="10" fill="#1f2328">144</text>
      <text x="159.6" y="315.9" text-anchor="middle" font-size="10" fill="#1f2328">281</text>
      <text x="184.4" y="315.8" text-anchor="middle" font-size="10" fill="#1f2328">342</text>
      <text x="221.6" y="292.6" text-anchor="middle" font-size="10" fill="#1f2328">44968</text>
      <text x="246.4" y="288.3" text-anchor="middle" font-size="10" fill="#1f2328">53233</text>
      <text x="283.6" y="188.7" text-anchor="middle" font-size="10" fill="#1f2328">244714</text>
      <text x="308.4" y="177.6" text-anchor="middle" font-size="10" fill="#1f2328">266206</text>
      <text x="345.6" y="316.0" text-anchor="middle" font-size="10" fill="#1f2328">95.5</text>
      <text x="370.4" y="315.9" text-anchor="middle" font-size="10" fill="#1f2328">164</text>
      <text x="407.6" y="315.8" text-anchor="middle" font-size="10" fill="#1f2328">307</text>
      <text x="432.4" y="315.8" text-anchor="middle" font-size="10" fill="#1f2328">401</text>
      <text x="469.6" y="287.5" text-anchor="middle" font-size="10" fill="#1f2328">54744</text>
      <text x="494.4" y="274.9" text-anchor="middle" font-size="10" fill="#1f2328">79029</text>
      <text x="531.6" y="182.5" text-anchor="middle" font-size="10" fill="#1f2328">256651</text>
      <text x="556.4" y="152.8" text-anchor="middle" font-size="10" fill="#1f2328">313899</text>
      <text x="593.6" y="316.0" text-anchor="middle" font-size="10" fill="#1f2328">92.2</text>
      <text x="618.4" y="315.9" text-anchor="middle" font-size="10" fill="#1f2328">144</text>
      <text x="655.6" y="315.9" text-anchor="middle" font-size="10" fill="#1f2328">244</text>
      <text x="680.4" y="315.8" text-anchor="middle" font-size="10" fill="#1f2328">343</text>
    </g>
    <g>
      <text x="111" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode plain 1</text>
      <text x="173" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode plain 1</text>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode plain 1000</text>
      <text x="297" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode plain 1000</text>
      <text x="359" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode released 1</text>
      <text x="421" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode released 1</text>
      <text x="483" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode released 1000</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode released 1000</text>
      <text x="607" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode typed FTX</text>
      <text x="669" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode typed FTX</text>
    </g>
    <g>
      <g transform="translate(257, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">wireform-edi</text>
      </g>
      <g transform="translate(375, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">edifact-rs</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">EDIFACT UTF-8 codecs — plain, released and generated typed records</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · ns · GHC 9.10.3 -O2 -fasm, Criterion 1.6.5.0; edifact-rs 0.18.0, criterion.rs 0.7; darwin-aarch64</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">125000</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">250000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">375000</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">500000</text>
    </g>
    <g>
      <rect x="86.2" y="320.0" width="22.8" height="0.0" rx="2" fill="#58a6ff"/>
      <rect x="111" y="319.9" width="22.8" height="0.1" rx="2" fill="#ff7b72"/>
      <rect x="148.2" y="319.9" width="22.8" height="0.1" rx="2" fill="#58a6ff"/>
      <rect x="173" y="319.8" width="22.8" height="0.2" rx="2" fill="#ff7b72"/>
      <rect x="210.2" y="296.6" width="22.8" height="23.4" rx="2" fill="#58a6ff"/>
      <rect x="235" y="292.3" width="22.8" height="27.7" rx="2" fill="#ff7b72"/>
      <rect x="272.2" y="192.7" width="22.8" height="127.3" rx="2" fill="#58a6ff"/>
      <rect x="297" y="181.6" width="22.8" height="138.4" rx="2" fill="#ff7b72"/>
      <rect x="334.2" y="320.0" width="22.8" height="0.0" rx="2" fill="#58a6ff"/>
      <rect x="359" y="319.9" width="22.8" height="0.1" rx="2" fill="#ff7b72"/>
      <rect x="396.2" y="319.8" width="22.8" height="0.2" rx="2" fill="#58a6ff"/>
      <rect x="421" y="319.8" width="22.8" height="0.2" rx="2" fill="#ff7b72"/>
      <rect x="458.2" y="291.5" width="22.8" height="28.5" rx="2" fill="#58a6ff"/>
      <rect x="483" y="278.9" width="22.8" height="41.1" rx="2" fill="#ff7b72"/>
      <rect x="520.2" y="186.5" width="22.8" height="133.5" rx="2" fill="#58a6ff"/>
      <rect x="545" y="156.8" width="22.8" height="163.2" rx="2" fill="#ff7b72"/>
      <rect x="582.2" y="320.0" width="22.8" height="0.0" rx="2" fill="#58a6ff"/>
      <rect x="607" y="319.9" width="22.8" height="0.1" rx="2" fill="#ff7b72"/>
      <rect x="644.2" y="319.9" width="22.8" height="0.1" rx="2" fill="#58a6ff"/>
      <rect x="669" y="319.8" width="22.8" height="0.2" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="97.6" y="316.0" text-anchor="middle" font-size="10" fill="#e6edf3">74.0</text>
      <text x="122.4" y="315.9" text-anchor="middle" font-size="10" fill="#e6edf3">144</text>
      <text x="159.6" y="315.9" text-anchor="middle" font-size="10" fill="#e6edf3">281</text>
      <text x="184.4" y="315.8" text-anchor="middle" font-size="10" fill="#e6edf3">342</text>
      <text x="221.6" y="292.6" text-anchor="middle" font-size="10" fill="#e6edf3">44968</text>
      <text x="246.4" y="288.3" text-anchor="middle" font-size="10" fill="#e6edf3">53233</text>
      <text x="283.6" y="188.7" text-anchor="middle" font-size="10" fill="#e6edf3">244714</text>
      <text x="308.4" y="177.6" text-anchor="middle" font-size="10" fill="#e6edf3">266206</text>
      <text x="345.6" y="316.0" text-anchor="middle" font-size="10" fill="#e6edf3">95.5</text>
      <text x="370.4" y="315.9" text-anchor="middle" font-size="10" fill="#e6edf3">164</text>
      <text x="407.6" y="315.8" text-anchor="middle" font-size="10" fill="#e6edf3">307</text>
      <text x="432.4" y="315.8" text-anchor="middle" font-size="10" fill="#e6edf3">401</text>
      <text x="469.6" y="287.5" text-anchor="middle" font-size="10" fill="#e6edf3">54744</text>
      <text x="494.4" y="274.9" text-anchor="middle" font-size="10" fill="#e6edf3">79029</text>
      <text x="531.6" y="182.5" text-anchor="middle" font-size="10" fill="#e6edf3">256651</text>
      <text x="556.4" y="152.8" text-anchor="middle" font-size="10" fill="#e6edf3">313899</text>
      <text x="593.6" y="316.0" text-anchor="middle" font-size="10" fill="#e6edf3">92.2</text>
      <text x="618.4" y="315.9" text-anchor="middle" font-size="10" fill="#e6edf3">144</text>
      <text x="655.6" y="315.9" text-anchor="middle" font-size="10" fill="#e6edf3">244</text>
      <text x="680.4" y="315.8" text-anchor="middle" font-size="10" fill="#e6edf3">343</text>
    </g>
    <g>
      <text x="111" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode plain 1</text>
      <text x="173" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode plain 1</text>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode plain 1000</text>
      <text x="297" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode plain 1000</text>
      <text x="359" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode released 1</text>
      <text x="421" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode released 1</text>
      <text x="483" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode released 1000</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode released 1000</text>
      <text x="607" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode typed FTX</text>
      <text x="669" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode typed FTX</text>
    </g>
    <g>
      <g transform="translate(257, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">wireform-edi</text>
      </g>
      <g transform="translate(375, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">edifact-rs</text>
      </g>
    </g>
  </g>
</svg>


| Operation            | wireform-edi | edifact-rs | ratio |
| :------------------- | -----------: | ---------: | ----: |
| encode plain 1       |      74.0 ns |     144 ns | 0.51x |
| decode plain 1       |       281 ns |     342 ns | 0.82x |
| encode plain 1000    |     44968 ns |   53233 ns | 0.84x |
| decode plain 1000    |    244714 ns |  266206 ns | 0.92x |
| encode released 1    |      95.5 ns |     164 ns | 0.58x |
| decode released 1    |       307 ns |     401 ns | 0.77x |
| encode released 1000 |     54744 ns |   79029 ns | 0.69x |
| decode released 1000 |    256651 ns |  313899 ns | 0.82x |
| encode typed FTX     |      92.2 ns |     144 ns | 0.64x |
| decode typed FTX     |       244 ns |     343 ns | 0.71x |

<sub>Last run 2026-11-04 13:29:28 UTC. GHC 9.10.3 -O2 -fasm, Criterion 1.6.5.0; edifact-rs 0.18.0, criterion.rs 0.7; darwin-aarch64.</sub>
<!-- END_AUTOGEN bench:edi-rust-codec -->
