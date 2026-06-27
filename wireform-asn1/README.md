# wireform-asn1

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

[ASN.1](https://www.itu.int/rec/T-REC-X.680) BER and DER ([ITU-T
X.690](https://www.itu.int/rec/T-REC-X.690)) for Haskell. Encode and
decode the dynamic [`ASN1.Value`](src/ASN1/Value.hs), derive typeclass
instances generically or via Template Haskell, parse ASN.1 modules
and generate Haskell types from them, plus a `[asn1| ... |]`
quasiquoter for inline modules.

ASN.1 is the wire format underneath every X.509 certificate, every
LDAP message, every SNMP trap, every Kerberos ticket, and large
swathes of the telecom and smart-card stack. The "1" stands for
"Notation One"; the type system is older than C, far more expressive
than its reputation suggests, and unfortunately accompanied by a
schema language that takes a couple of read-throughs to internalize.
The DER (Distinguished Encoding Rules) subset is the canonical
encoding required by most cryptographic uses; BER (Basic Encoding
Rules) is the more permissive superset.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-asn1,
  wireform-derive,    -- only if you want the cross-format annotation deriver
```

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-asn1` to compile
locally. Compiling with the LLVM backend (`-fllvm`) adds compile time
but measurably improves runtime performance.

## Hello world

Working directly with `ASN1.Value`, building a tiny chunk of an X.509
certificate (the version + serial-number prefix) and round-tripping
it through DER:

```haskell
import qualified Data.ByteString as BS
import qualified Data.Vector     as V
import qualified ASN1.Value  as A
import qualified ASN1.Encode as AE
import qualified ASN1.Decode as AD

main :: IO ()
main = do
  let v = A.Sequence $ V.fromList
        [ A.Tagged A.ContextSpecific 0 (A.Integer 2)  -- version v3
        , A.Integer 12345                              -- serial number
        ]
      bytes = AE.encode v
  putStrLn $ "DER encoded: " ++ show (BS.length bytes) ++ " bytes"
  case AD.decode bytes of
    Right val -> print val
    Left  err -> putStrLn err
```

The runnable version (which extends to a more realistic X.509-shaped
SEQUENCE) lives in [`examples/ASN1Example.hs`](../examples/ASN1Example.hs).

For typed records, use the `ASN1.Derive` typeclass surface:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

import GHC.Generics (Generic)
import Data.Text (Text)
import ASN1.Derive (ToASN1, FromASN1, encodeASN1, decodeASN1, deriveASN1)

data Subject = Subject
  { subjectCN :: !Text
  , subjectO  :: !Text
  } deriving stock (Show, Eq, Generic)

deriveASN1 ''Subject

main :: IO ()
main = do
  let s     = Subject "example.com" "Example Corp"
      bytes = encodeASN1 s
  case decodeASN1 bytes of
    Right (decoded :: Subject) -> print decoded
    Left  err                  -> putStrLn err
```

## What's in here

| Module          | Role                                                      |
|-----------------|-----------------------------------------------------------|
| `ASN1.Value`    | Dynamic untyped `Value` ADT covering every ASN.1 type (`Integer`, `OctetString`, `BitString`, `OID`, `Sequence`, `Set`, `Tagged` with class + tag number, `UTF8String`, `UTCTime`, ...) |
| `ASN1.Encode`   | Low-level DER encoder: `encode :: Value -> ByteString`    |
| `ASN1.Decode`   | Low-level BER / DER decoder                               |
| `ASN1.Derive`   | Public `ToASN1` / `FromASN1` typeclasses + `encodeASN1` / `decodeASN1` + `deriveASN1` / `deriveToASN1` / `deriveFromASN1` Template Haskell entry points |
| `ASN1.Schema`   | ASN.1 module AST (`ASN1Module`, `ASN1TypeDef`, `ASN1Type`, ...) |
| `ASN1.Parser`   | `parseASN1 :: Text -> Either String ASN1Module` for `.asn1` schema files |
| `ASN1.CodeGen`  | Generate Haskell types and `ToASN1` / `FromASN1` instances from an ASN.1 module |
| `ASN1.QQ`       | `[asn1| ... |]` quasiquoter for inline ASN.1 modules      |

## Encode and decode

Two layers, both exposed:

```haskell
-- Low level: dynamic Value <-> bytes
ASN1.Encode.encode :: Value      -> ByteString
ASN1.Decode.decode :: ByteString -> Either String Value

-- Typeclass: derived encoder / decoder for your records
encodeASN1 :: ToASN1   a => a          -> ByteString
decodeASN1 :: FromASN1 a => ByteString -> Either String a
```

The encoder produces DER (canonical, deterministic). The decoder
accepts both DER and the more permissive BER, which means it'll read
input produced by encoders that don't bother to enforce DER's
"definite-length only" / "minimal length encoding" / "sorted SET"
constraints.

The `Tagged` constructor in `ASN1.Value` carries the tag class
(`Universal`, `Application`, `ContextSpecific`, `Private`) and number
explicitly, which is how X.509 and the rest of the ASN.1 world
disambiguate fields inside a `SEQUENCE`.

## Annotation-driven deriving

`ASN1.Derive` consumes the cross-format `Wireform.Derive.Modifier`
vocabulary from [`wireform-derive`](../wireform-derive/README.md). The
ASN.1-specific knobs (implicit / explicit tagging, tag number) live
under the `Asn1Tag` `BackendModifier` extension:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified ASN1.Derive as DASN1
import Wireform.Derive (extension)
import ASN1.Derive (Asn1Tag (..))

data Cert = Cert
  { certVersion      :: !Int
  , certSerialNumber :: !Integer
  , certSubject      :: !Text
  } deriving stock (Show, Eq, Generic)

{-# ANN type Cert ("Cert" :: String) #-}
{-# ANN certVersion (extension (Explicit 0)) #-}

DASN1.deriveASN1 ''Cert
```

`Explicit n` and `Implicit n` correspond directly to the ASN.1 tagging
modes; `Universal` is the default for primitive types.

## ASN.1 schema and code generation

`.asn1` schema files go through `ASN1.Parser.parseASN1` to produce an
`ASN1Module`, and through `ASN1.CodeGen` to emit Haskell types +
`ToASN1` / `FromASN1` instances:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import ASN1.QQ (asn1)

[asn1|
  MyModule DEFINITIONS ::= BEGIN
    Person ::= SEQUENCE {
      name UTF8String,
      age  INTEGER
    }
  END
|]
-- Generates: data Person = Person { name :: Text, age :: Integer }
--            instance ToASN1 Person ; instance FromASN1 Person
```

For external `.asn1` files, the `wireform-gen` CLI in the umbrella
package wraps the same codegen:

```bash
wireform-gen asn1 -i schema.asn1 -o src/Gen/
```

## Testing

The per-format Hedgehog suite lives in `test/`:

```bash
cabal test wireform-asn1:wireform-asn1-derive-test
```

It covers the typeclass instances, the deriver, BER and DER round
trips, the dynamic `Value` ADT (every tag class), and the schema
parser + codegen.

## Benchmarks

A criterion harness in [`bench/Bench.hs`](bench/Bench.hs):

```bash
cabal bench wireform-asn1:wireform-asn1-bench
```

<!-- BEGIN_AUTOGEN bench:asn1-encode-decode -->
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="bench-results/charts/asn1-encode-decode-dark.svg">
  <img src="bench-results/charts/asn1-encode-decode-light.svg" alt="wireform-asn1 encode + decode (DER, Subject record)">
</picture>

| Operation       |   encode |   decode | ratio |
| :-------------- | -------: | -------: | ----: |
| single Subject  |   140 ns |   115 ns | 0.82x |
| [Subject] x 100 | 17260 ns | 12699 ns | 0.74x |

<sub>Last run 2026-06-27 11:35:54 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:asn1-encode-decode -->

For cross-language comparisons:

- Haskell:
  [`asn1-encoding`](https://hackage.haskell.org/package/asn1-encoding)
  and
  [`asn1-types`](https://hackage.haskell.org/package/asn1-types) (the
  long-standing Haskell ASN.1 stack used by `tls`, `x509`, etc.).
- C: OpenSSL's [`d2i_*` / `i2d_*`](https://www.openssl.org/docs/man3.0/man3/d2i_X509.html)
  family and [GnuTLS's libtasn1](https://www.gnu.org/software/libtasn1/).
- Rust: [`rasn`](https://crates.io/crates/rasn) and
  [`asn1`](https://crates.io/crates/asn1).

## License

BSD-3-Clause.

## References

- [ITU-T X.680: ASN.1 specification of basic notation](https://www.itu.int/rec/T-REC-X.680)
- [ITU-T X.690: BER, CER, and DER encoding rules](https://www.itu.int/rec/T-REC-X.690)
- [RFC 5280: Internet X.509 PKI certificate profile](https://www.rfc-editor.org/rfc/rfc5280) (the most-encountered ASN.1 use case)
