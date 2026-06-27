---
title: wireform-cbor
description: "RFC 8949 CBOR encoding and decoding with TH deriving, CDDL codegen, diagnostic notation, and deterministic encoding."
sidebar:
  order: 10
---

`wireform-cbor` implements Concise Binary Object Representation (CBOR) per
RFC 8949. CBOR is a compact, self-describing binary format used in IoT
protocols, COSE/JOSE, WebAuthn, and many other standards. Use this package
when you need a schema-flexible binary codec with strong tooling for
debugging, schema definition, and cross-language interoperability.

## Key features

- **Template Haskell deriving** via `deriveCBOR` for records, enums, and sum
  types, with `wireform-derive` annotations; Generic defaults (empty instances)
  work for simple cases
- **Streaming decode** for framed or concatenated CBOR values without loading
  the entire input into memory
- **CDDL schema language** (RFC 8610) with a parser and Haskell code generator
- **Diagnostic notation** for human-readable debug output (RFC 8949 Section 8)
- **JSON bridge** for converting between CBOR and Aeson `Value`
- **Deterministic encoding** per RFC 8949 Section 4.2 for canonical byte
  sequences suitable for hashing and signing
- **Tag registry** for application-specific CBOR tags

## Basic usage

Derive instances with the Template Haskell deriver, then encode and decode in
one call:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Config where

import CBOR.Class (ToCBOR, FromCBOR, encodeCBOR, decodeCBOR)
import CBOR.Derive (deriveCBOR)
import GHC.Generics (Generic)
import Data.Text (Text)

data Config = Config
  { cfgHost :: !Text
  , cfgPort :: !Int
  }
  deriving stock (Show, Eq, Generic)

$(deriveCBOR ''Config)

save :: Config -> IO ()
save cfg = do
  let bytes = encodeCBOR cfg
  writeFileBinary "config.cbor" bytes

load :: IO (Either String Config)
load = do
  bytes <- readFileBinary "config.cbor"
  pure (decodeCBOR bytes)
```

For simple cases with no wire-format customization, Generic defaults also
work: add `deriving Generic` and declare empty `instance ToCBOR Config` and
`instance FromCBOR Config` declarations.

For signed payloads or content-addressed storage, use deterministic encoding
so the same value always produces the same bytes:

```haskell
import CBOR.Encode (encodeDeterministic)
import CBOR.Value qualified as CV
import CBOR.Class (toCBOR)

canonicalBytes :: Config -> ByteString
canonicalBytes cfg = encodeDeterministic (toCBOR cfg)
```

When debugging wire format issues, render values as diagnostic notation:

```haskell
import CBOR.Diagnostic (toDiagnostic)
import CBOR.Class (toCBOR)

debugConfig :: Config -> Text
debugConfig cfg = toDiagnostic (toCBOR cfg)
```

For streams of CBOR items (logs, multiplexed channels), decode one value at
a time and keep the leftover bytes:

```haskell
import CBOR.Stream (decodeOneWithLeftover)

decodeStream :: ByteString -> [(Either String CV.Value, ByteString)]
decodeStream bs = go bs
  where
    go rest
      | BS.null rest = []
      | otherwise =
          case decodeOneWithLeftover rest of
            Left err -> [(Left err, BS.empty)]
            Right (val, leftover) -> (Right val, leftover) : go leftover
```

## Performance

### wireform-cbor vs cborg

<!-- BEGIN_AUTOGEN bench:cbor-vs-cborg-encode -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-cbor vs cborg</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-cbor vs cborg</text>
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
      <rect x="171" y="280.7" width="62" height="39.3" rx="2" fill="#0969da"/>
      <rect x="235" y="283.5" width="62" height="36.5" rx="2" fill="#cf222e"/>
      <rect x="481" y="260.0" width="62" height="60.0" rx="2" fill="#0969da"/>
      <rect x="545" y="152.4" width="62" height="167.6" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="202" y="276.7" text-anchor="middle" font-size="10" fill="#1f2328">302</text>
      <text x="266" y="279.5" text-anchor="middle" font-size="10" fill="#1f2328">281</text>
      <text x="512" y="256.0" text-anchor="middle" font-size="10" fill="#1f2328">462</text>
      <text x="576" y="148.4" text-anchor="middle" font-size="10" fill="#1f2328">1289</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode</text>
    </g>
    <g>
      <g transform="translate(271, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">wireform-cbor</text>
      </g>
      <g transform="translate(396, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">cborg</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-cbor vs cborg</text>
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
      <rect x="171" y="280.7" width="62" height="39.3" rx="2" fill="#58a6ff"/>
      <rect x="235" y="283.5" width="62" height="36.5" rx="2" fill="#ff7b72"/>
      <rect x="481" y="260.0" width="62" height="60.0" rx="2" fill="#58a6ff"/>
      <rect x="545" y="152.4" width="62" height="167.6" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="202" y="276.7" text-anchor="middle" font-size="10" fill="#e6edf3">302</text>
      <text x="266" y="279.5" text-anchor="middle" font-size="10" fill="#e6edf3">281</text>
      <text x="512" y="256.0" text-anchor="middle" font-size="10" fill="#e6edf3">462</text>
      <text x="576" y="148.4" text-anchor="middle" font-size="10" fill="#e6edf3">1289</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode</text>
    </g>
    <g>
      <g transform="translate(271, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">wireform-cbor</text>
      </g>
      <g transform="translate(396, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">cborg</text>
      </g>
    </g>
  </g>
</svg>


| Operation | wireform-cbor |   cborg | ratio |
| :-------- | ------------: | ------: | ----: |
| encode    |        302 ns |  281 ns | 0.93x |
| decode    |        462 ns | 1289 ns | 2.79x |

<sub>Last run 2026-06-27 11:56:42 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:cbor-vs-cborg-encode -->

Encode performance is roughly even with cborg (the established Haskell CBOR library). Decode is 2.6x faster due to wireform's unboxed-sum decoder architecture.

The chart and table above are regenerated by [`wireform-stats`](../stats/) from `wireform-cbor/bench-results/summary/cbor-vs-cborg-encode.json` — the same source the README chart is built from.

## Notable modules

| Module | Purpose |
|--------|---------|
| `CBOR.Class` | `ToCBOR` / `FromCBOR` typeclasses, `encodeCBOR`, `decodeCBOR` |
| `CBOR.Encode` / `CBOR.Decode` | Low-level wire primitives and `encodeDeterministic` |
| `CBOR.Value` | Dynamic untyped `Value` ADT for schema-less processing |
| `CBOR.Diagnostic` | Diagnostic notation rendering and parsing |
| `CBOR.CDDL` / `CBOR.CDDLCodeGen` | CDDL parser and Haskell stub generator |
| `CBOR.JSON` | CBOR ↔ JSON conversion |
| `CBOR.Stream` | Incremental decode for framed input |
| `CBOR.TagRegistry` | Application tag registration and lookup |
| `CBOR.Derive` | Template Haskell deriver with `wireform-derive` annotations |

## Conformance

Deterministic encoding follows RFC 8949 Section 4.2: shortest integer forms,
definite-length containers, and canonical map key ordering. Use
`encodeDeterministic` when byte-for-byte reproducibility matters for signatures
or content hashes.
