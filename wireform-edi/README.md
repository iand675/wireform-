# wireform-edi

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

Electronic Data Interchange (EDI) segment tooling for Haskell. The package
models delimiter syntax explicitly, parses ordered segments into a dynamic ADT,
infers X12 delimiters from `ISA` headers, validates envelopes and control
numbers, generates accepted 997 acknowledgements, and exposes annotation-driven
Template Haskell deriving for typed segment records.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its annotation deriver and testing discipline with every
other format package.

## Install

```cabal
build-depends:
  base,
  wireform-edi,
  wireform-derive,    -- only if you want the annotation deriver
```

Clone the repo and run `cabal build wireform-edi` to compile locally.

## Hello world

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Vector as V
import qualified EDI.Decode as D
import qualified EDI.Encode as E
import EDI.Value

doc :: Interchange
doc = Interchange defaultSyntax
  (V.fromList
    [ Segment "ST"  (V.fromList [Simple "850", Simple "0001"])
    , Segment "BEG" (V.fromList [Simple "00", Simple "SA", Simple "12345"])
    ])

main :: IO ()
main = do
  print (E.encode doc)
  print (D.decode "ST*850*0001~BEG*00*SA*12345~")
```

`EDI.Derive` maps Haskell records to single positional segments:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Data.Text (Text)
import EDI.Class
import EDI.Derive
import Wireform.Derive

data NameSegment = NameSegment
  { entityCode :: !Text
  , nameValue  :: !Text
  }

{-# ANN NameSegment (forBackend backendEDI (rename "N1")) #-}
deriveEDI ''NameSegment
```

## What's in here

| Module | Role |
|--------|------|
| `EDI.Value` | `Syntax`, `Interchange`, `Segment`, and `Element` ADTs |
| `EDI.Encode` | Text and UTF-8 rendering |
| `EDI.Decode` | Parser with X12 `ISA` delimiter inference |
| `EDI.Class` | `ToEDI` / `FromEDI` and field-level scalar classes |
| `EDI.Derive` | Template Haskell deriver for typed segment records |
| `EDI.Encoding` | Small builder wrapper used by encoders |
| `EDI.Query` | Segment lookup, element access, and tag counting helpers |
| `EDI.Validation` | Generic delimiter-safety and structural validation |
| `EDI.X12` | X12 envelope grouping, control validation, and 997 acknowledgements |

## Testing

```bash
cabal test wireform-edi:wireform-edi-derive-test
cabal test wireform-test --test-show-details=streaming
```

The tests cover dynamic segment parsing, X12 delimiter inference, envelope
validation, 997 generation, encode/decode round-trips, and TH-derived segment
codecs.

## License

BSD-3-Clause.
