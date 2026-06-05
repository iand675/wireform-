---
title: wireform-edi
description: "Electronic Data Interchange segment parsing and rendering with X12 delimiter inference and TH deriving."
sidebar:
  order: 15
---

`wireform-edi` implements delimiter-sensitive EDI segment streams. It models
the concrete interchange syntax, decodes ordered segments into a dynamic ADT,
infers X12 delimiters from `ISA` headers, parses UN/EDIFACT `UNA` service
strings, validates EDIFACT and X12 envelopes, generates accepted 997
acknowledgements, and derives typed codecs for segment records through the
shared `wireform-derive` annotation vocabulary.

## Key features

- **Dynamic EDI ADT** via `Interchange`, `Segment`, and `Element`
- **Explicit syntax** for element, component, repetition, and segment
  delimiters
- **X12 `ISA` delimiter inference** when decoding full interchanges
- **UN/EDIFACT helpers** for `UNA`, release-character decoding, and
  UNB/UNH/UNT/UNZ control validation
- **Standards registry** for UN/EDIFACT, X12, GS1 EDI, TRADACOMS, ODETTE,
  VDA, HL7, HIPAA, IATA Cargo-IMP, NCPDP SCRIPT, NCPDP Telecom, and EDIGAS
- **Query helpers** for tag lookup, element access, and segment counts
- **Validation** for delimiter safety plus X12 control numbers and segment counts
- **997 acknowledgement generation** for accepted functional groups
- **Template Haskell deriving** via `deriveEDI`
- **Field-level scalar classes** for positional segment elements

## Basic usage

```haskell
import qualified Data.Vector as V
import qualified EDI.Decode as Decode
import qualified EDI.Encode as Encode
import EDI.Value

doc :: Interchange
doc = Interchange defaultSyntax
  (V.singleton (Segment "ST" (V.fromList [Simple "850", Simple "0001"])))

encoded :: Text
encoded = Encode.encode doc

decoded :: Either String Interchange
decoded = Decode.decode encoded
```

## Notable modules

| Module | Purpose |
|--------|---------|
| `EDI.Value` | `Syntax`, `Interchange`, `Segment`, and `Element` |
| `EDI.Encode` / `EDI.Decode` | Low-level text encode and decode |
| `EDI.EDIFACT` | UN/EDIFACT service strings, decoding, and envelope validation |
| `EDI.Class` | `ToEDI` / `FromEDI` plus scalar element classes |
| `EDI.Derive` | Template Haskell deriver with `wireform-derive` annotations |
| `EDI.Query` | Segment and element lookup helpers |
| `EDI.Standard` | Standard profiles and detection |
| `EDI.Validation` | Generic structural validation |
| `EDI.X12` | X12 envelope parsing, validation, and 997 generation |
