# idn

A pure Haskell implementation of Internationalized Domain Names (IDN) following the IDNA2008 specification (RFC 5890-5893), including Punycode encoding (RFC 3492).

## What is IDN?

Internationalized Domain Names allow domain names to contain non-ASCII characters, enabling domain names in languages like Chinese, Arabic, Japanese, and many others. For example:

- `münchen.de` (German)
- `北京.cn` (Chinese)
- `مصر.eg` (Arabic)

Since DNS only supports ASCII characters, these Unicode domain names are encoded using Punycode to create ASCII-compatible representations:

- `münchen.de` → `xn--mnchen-3ya.de`
- `北京.cn` → `xn--1ci.cn`
- `مصر.eg` → `xn--wgbh1c.eg`

## Features

- **Pure Haskell**: No C dependencies, fully portable
- **IDNA2008 compliant**: Implements RFC 5890-5893 specification
- **Punycode support**: Low-level Punycode encoding/decoding (RFC 3492)
- **Comprehensive validation**: Code point validation, bidirectional text rules, contextual rules
- **Well documented**: Full Haddock documentation with examples
- **Well tested**: Extensive test suite including RFC test vectors

## Installation

### Using Stack

Add to your `stack.yaml`:

```yaml
packages:
  - idn
```

Or add to your `.cabal` file:

```cabal
build-depends:
  idn >= 0.1.0.0
```

### Using Cabal

```bash
cabal install idn
```

## Quick Start

### Converting Domain Names

```haskell
import Data.Text.IDN
import qualified Data.Text as T

-- Convert Unicode domain to ASCII (Punycode)
toASCII "münchen.de"
-- Right "xn--mnchen-3ya.de"

-- Convert ASCII domain back to Unicode
toUnicode "xn--mnchen-3ya.de"
-- Right "münchen.de"

-- Validate a domain label
validateLabel "example"
-- Right ()

validateLabel "invalid-"
-- Left (InvalidHyphenPosition EndsWithHyphen)
```

### Using Punycode Directly

```haskell
import Data.Text.Punycode

-- Encode Unicode to Punycode
encode "münchen"
-- Right "mnchen-3ya"

-- Decode Punycode to Unicode
decode "mnchen-3ya"
-- Right "münchen"
```

## API Overview

### High-Level Domain Name Processing

The `Data.Text.IDN` module provides functions for working with complete domain names:

- `toASCII :: Text -> Either IDNError Text` - Convert Unicode domain to ASCII
- `toUnicode :: Text -> Either IDNError Text` - Convert ASCII domain to Unicode
- `validateLabel :: Text -> Either IDNError ()` - Validate a single domain label

### Low-Level Punycode Encoding

The `Data.Text.Punycode` module provides direct access to Punycode encoding:

- `encode :: Text -> Either PunycodeError Text` - Encode Unicode to Punycode
- `decode :: Text -> Either PunycodeError Text` - Decode Punycode to Unicode

### Types

The `Data.Text.IDN.Types` module exports:

- `IDNError` - Domain name processing errors
- `PunycodeError` - Punycode encoding/decoding errors
- `Label`, `DomainName` - Domain structure types
- Various Unicode property types

## Examples

### Converting a Domain Name

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text.IDN
import Data.Text (Text)

convertDomain :: Text -> IO ()
convertDomain domain = do
  case toASCII domain of
    Right ascii -> putStrLn $ "ASCII: " ++ show ascii
    Left err -> putStrLn $ "Error: " ++ show err
```

### Validating User Input

```haskell
import Data.Text.IDN

isValidDomainLabel :: Text -> Bool
isValidDomainLabel label = case validateLabel label of
  Right () -> True
  Left _ -> False
```

### Working with Punycode

```haskell
import Data.Text.Punycode

-- Encode a Unicode string
encodeUnicode :: Text -> Either PunycodeError Text
encodeUnicode = encode

-- Decode a Punycode string
decodePunycode :: Text -> Either PunycodeError Text
decodePunycode = decode
```

## Error Handling

The library uses `Either` types for error handling:

```haskell
-- IDN errors include detailed context
data IDNError
  = EmptyLabel
  | LabelTooLong { actualLength :: Int, maxLength :: Int }
  | InvalidHyphenPosition HyphenError
  | DisallowedCodePoint { character :: Char, codepoint :: Int }
  | -- ... and more

-- Punycode errors
data PunycodeError
  = InvalidPunycode Text
  | Overflow
```

## Specification Compliance

This library implements:

- **RFC 5890**: Internationalized Domain Names for Applications (IDNA): Definitions and Document Framework
- **RFC 5891**: Internationalized Domain Names in Applications (IDNA): Protocol
- **RFC 5892**: The Unicode Code Points and Internationalized Domain Names for Applications (IDNA)
- **RFC 5893**: Right-to-Left Scripts for Internationalized Domain Names for Applications (IDNA)
- **RFC 3492**: Punycode: A Bootstring encoding of Unicode for Internationalized Domain Names in Applications (IDNA)

## Performance

The implementation is optimized for performance:

- Uses unboxed vectors for efficient code point processing
- Mutable arrays for string building
- Efficient Unicode property lookups using IntMap/IntSet
- Benchmarks included in the package

## Testing

Run the test suite:

```bash
stack test idn
```

The test suite includes:
- RFC 3492 official test vectors
- RFC 5893 bidirectional text test cases
- Property-based tests
- Round-trip encoding/decoding tests

## License

BSD-3-Clause

## See Also

- [RFC 5890-5893](https://tools.ietf.org/html/rfc5890) - IDNA2008 specification
- [RFC 3492](https://tools.ietf.org/html/rfc3492) - Punycode specification
- [libidn2](https://www.gnu.org/software/libidn/) - C library for IDN (similar functionality)

