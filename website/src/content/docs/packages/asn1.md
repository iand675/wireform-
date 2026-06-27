---
title: wireform-asn1
description: "ASN.1 BER/DER encoding and decoding with module definition parser, tagging modes, constraints, and codegen."
sidebar:
  order: 35
---

`wireform-asn1` implements ASN.1 Basic Encoding Rules (BER) and Distinguished
Encoding Rules (DER) per [ITU-T X.690](https://www.itu.int/rec/T-REC-X.690).
ASN.1 underpins X.509 certificates, LDAP, SNMP, Kerberos, and smart-card
protocols. DER is the canonical subset required for cryptographic uses. Use this
package when you need standards-compliant DER output, ASN.1 module parsing, or
typed encoding of certificate and telecom structures.

## Key features

- **Typeclass API** via `ToASN1` and `FromASN1` with `encodeASN1` / `decodeASN1`
- **ITU-T X.690 DER encoder** producing canonical byte sequences
- **ASN.1 module definition parser** for `.asn1` schema files
- **Schema AST** with tagging modes (Automatic, Implicit, Explicit) and constraints
- **Codegen and QuasiQuoter** for inline `[asn1| ... |]` modules
- **Template Haskell deriver** with `asn1ImplicitTag` and `asn1ExplicitTag` modifiers

## Basic usage

Derive instances for your record types, then encode to DER and decode back:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

import ASN1.Derive
  ( ToASN1, FromASN1
  , encodeASN1, decodeASN1
  , deriveASN1
  , asn1ImplicitTag
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data Person = Person
  { personId    :: !Int
  , personName  :: !Text
  , personAdmin :: !Bool
  }
  deriving stock (Show, Eq, Generic)

{-# ANN personAdmin (asn1ImplicitTag 0) #-}

$(deriveASN1 ''Person)

carol :: Person
carol = Person 1 "Carol" True

encodePerson :: Person -> ByteString
encodePerson = encodeASN1

decodePerson :: ByteString -> Either String Person
decodePerson = decodeASN1

roundTrip :: Either String Person
roundTrip = decodePerson (encodePerson carol)
```

For certificate-shaped structures, work directly with the dynamic `Value` ADT
when you need fine-grained control over tagging:

```haskell
import qualified Data.Vector as V
import qualified ASN1.Value as AV
import qualified ASN1.Encode as AE
import qualified ASN1.Decode as AD

tbsPrefix :: AV.Value
tbsPrefix = AV.Sequence $ V.fromList
  [ AV.Tagged AV.ContextSpecific 0 (AV.Integer 2)  -- X.509 version v3
  , AV.Integer 12345                              -- serial number
  ]

derBytes :: ByteString
derBytes = AE.encode tbsPrefix

parseDer :: Either String AV.Value
parseDer = AD.decode derBytes
```

Generate types from ASN.1 modules:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import ASN1.QQ (asn1)

[asn1|
  Person DEFINITIONS ::= BEGIN
    Person ::= SEQUENCE {
      id    INTEGER,
      name  UTF8String,
      admin BOOLEAN
    }
  END
|]
```

```bash
wireform-gen asn1 -i module.asn1 -o src/Gen/
```

## Performance

### DER encode/decode

<!-- BEGIN_AUTOGEN bench:asn1-encode-decode -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-asn1 encode + decode (DER, Subject record)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-asn1 encode + decode (DER, Subject record)</text>
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
      <rect x="171" y="318.2" width="62" height="1.8" rx="2" fill="#0969da"/>
      <rect x="235" y="318.5" width="62" height="1.5" rx="2" fill="#cf222e"/>
      <rect x="481" y="95.6" width="62" height="224.4" rx="2" fill="#0969da"/>
      <rect x="545" y="154.9" width="62" height="165.1" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="202" y="314.2" text-anchor="middle" font-size="10" fill="#1f2328">140</text>
      <text x="266" y="314.5" text-anchor="middle" font-size="10" fill="#1f2328">115</text>
      <text x="512" y="91.6" text-anchor="middle" font-size="10" fill="#1f2328">17260</text>
      <text x="576" y="150.9" text-anchor="middle" font-size="10" fill="#1f2328">12699</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#1f2328">single Subject</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#1f2328">[Subject] x 100</text>
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
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-asn1 encode + decode (DER, Subject record)</text>
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
      <rect x="171" y="318.2" width="62" height="1.8" rx="2" fill="#58a6ff"/>
      <rect x="235" y="318.5" width="62" height="1.5" rx="2" fill="#ff7b72"/>
      <rect x="481" y="95.6" width="62" height="224.4" rx="2" fill="#58a6ff"/>
      <rect x="545" y="154.9" width="62" height="165.1" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="202" y="314.2" text-anchor="middle" font-size="10" fill="#e6edf3">140</text>
      <text x="266" y="314.5" text-anchor="middle" font-size="10" fill="#e6edf3">115</text>
      <text x="512" y="91.6" text-anchor="middle" font-size="10" fill="#e6edf3">17260</text>
      <text x="576" y="150.9" text-anchor="middle" font-size="10" fill="#e6edf3">12699</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">single Subject</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">[Subject] x 100</text>
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


| Operation       |   encode |   decode | ratio |
| :-------------- | -------: | -------: | ----: |
| single Subject  |   140 ns |   115 ns | 0.82x |
| [Subject] x 100 | 17260 ns | 12699 ns | 0.74x |

<sub>Last run 2026-06-27 11:35:54 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:asn1-encode-decode -->

Sub-microsecond per-record encode and decode. The DER codec is allocation-lean with unboxed field codecs.

The chart and table above are regenerated by [`wireform-stats`](../stats/) from `wireform-asn1/bench-results/summary/asn1-encode-decode.json` — the same source the README chart is built from.

## Notable modules

| Module | Purpose |
|--------|---------|
| `ASN1.Derive` | `ToASN1` / `FromASN1`, `encodeASN1` / `decodeASN1`, `deriveASN1` |
| `ASN1.Encode` / `ASN1.Decode` | BER/DER wire encoder and decoder |
| `ASN1.Value` | Dynamic untyped `Value` ADT (Sequence, Tagged, Integer, etc.) |
| `ASN1.Schema` / `ASN1.Parser` | Schema AST and module definition parser |
| `ASN1.CodeGen` / `ASN1.QQ` | Haskell codegen and quasiquoter |
