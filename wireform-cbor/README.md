# wireform-cbor

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

CBOR ([RFC 8949](https://www.rfc-editor.org/rfc/rfc8949)) for Haskell.
Encode and decode the dynamic [`CBOR.Value`](src/CBOR/Value.hs), derive
typeclass instances generically or via Template Haskell, parse and code
generate [CDDL](https://www.rfc-editor.org/rfc/rfc8610) schemas, stream
incrementally over partial input, and bridge to JSON when interop
calls for it.

CBOR shares JSON's data model (scalars, arrays, maps) but encodes it
in a tagged binary format with a small registry of type tags for
dates, big integers, byte strings, and other things JSON can't
represent natively. The format is an IETF standard and is used as the
wire format for COSE, CWT, W3C WebAuthn assertions, and a number of
IoT and constrained-device protocols.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-cbor,
  wireform-derive,    -- only if you want the cross-format annotation deriver
```

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-cbor` to compile
locally. Compiling with the LLVM backend (`-fllvm`) adds compile time
but measurably improves runtime performance.

## Hello world

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

import GHC.Generics (Generic)
import Data.Text (Text)
import CBOR.Class (ToCBOR, FromCBOR, encodeCBOR, decodeCBOR)

data Measurement = Measurement
  { sensor :: !Text
  , value  :: !Double
  , unit   :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToCBOR, FromCBOR)

main :: IO ()
main = do
  let m     = Measurement "temperature" 23.5 "celsius"
      bytes = encodeCBOR m
  case decodeCBOR bytes of
    Right (decoded :: Measurement) -> print decoded
    Left  err                      -> putStrLn err
```

The runnable version lives in [`examples/CBORExample.hs`](../examples/CBORExample.hs).

## What's in here

| Module                 | Role                                                      |
|------------------------|-----------------------------------------------------------|
| `CBOR.Value`           | Dynamic untyped `Value` ADT (`VInt` / `VText` / `VBytes` / `VArray` / `VMap` / `VTagged` / ...) |
| `CBOR.Encode`          | Low-level encoding primitives building straight onto `wireform-core`'s `Builder` |
| `CBOR.Encoding`        | The `Encoding` builder type used by `ToCBOR` instances    |
| `CBOR.Decode`          | Low-level decoding primitives over the strict `ByteString` input |
| `CBOR.Class`           | Public `ToCBOR` / `FromCBOR` typeclasses + `encodeCBOR` / `decodeCBOR` / `encodeCBORDirect` |
| `CBOR.Derive`          | `deriveCBOR` / `deriveToCBOR` / `deriveFromCBOR` Template Haskell entry points |
| `CBOR.Stream`          | Incremental decoder for chunked / streaming input         |
| `CBOR.JSON`            | Bridge to and from `aeson`'s `Value`                      |
| `CBOR.Diagnostic`      | RFC 8949 diagnostic notation pretty-printer (debug only)  |
| `CBOR.TagRegistry`     | Decoders keyed by IANA-registered tag number              |
| `CBOR.CDDL`            | CDDL parser (`parseCDDL :: Text -> Either String CDDLSchema`) |
| `CBOR.CDDLSchema`      | CDDL AST types                                            |
| `CBOR.CDDLCodeGen`     | Generate Haskell types and `ToCBOR` / `FromCBOR` instances from a CDDL schema |
| `CBOR.QQ`              | `[cddl| ... |]` quasiquoter                               |

## Encode and decode

The typeclass entry points are the usual shape:

```haskell
encodeCBOR       :: ToCBOR a   => a          -> ByteString
encodeCBORDirect :: ToCBOR a   => a          -> ByteString  -- direct-write path
decodeCBOR       :: FromCBOR a => ByteString -> Either String a
```

All three live in `CBOR.Class` and dispatch through the `Encoding`
builder from `CBOR.Encoding`. The direct-write variant skips the
intermediate builder representation when the size is statically
predictable, which is the hot path that the per-format Hedgehog suite
keeps competitive with hand-written encoders.

For dynamic values without a Haskell type to mirror them, work with
[`CBOR.Value`](src/CBOR/Value.hs) directly:

```haskell
import qualified CBOR.Encode as CE
import qualified CBOR.Decode as CD
import qualified CBOR.Value  as CV

let bytes = CE.encode (CV.VMap [(CV.VText "ok", CV.VBool True)])
case CD.decode bytes of
  Right (v :: CV.Value) -> ...
```

## Annotation-driven deriving

`CBOR.Derive` consumes the cross-format `Wireform.Derive.Modifier`
vocabulary from [`wireform-derive`](../wireform-derive/README.md), so
the same annotated record can produce CBOR, MsgPack, JSON, proto, and
any other backend's instances without redefining the field shapes:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified CBOR.Derive          as DCBOR
import qualified Wireform.Derive.Aeson as DAeson
import Wireform.Derive (rename, renameStyle, SnakeCase, forBackend, backendJSON)

data Person = Person
  { personFullName :: !Text
  , personAge      :: !Word32
  } deriving stock (Show, Eq, Generic)

{-# ANN type Person ("Person" :: String) #-}
{-# ANN personFullName (renameStyle SnakeCase) #-}
{-# ANN personAge      (renameStyle SnakeCase) #-}
{-# ANN personFullName (forBackend backendJSON (rename "fullName")) #-}

DCBOR.deriveCBOR ''Person
DAeson.deriveJSON ''Person
```

`personFullName` lands as `full_name` on the CBOR wire and `fullName`
in JSON. Same record, two backends, one annotation set.

## CDDL: schema and code generation

[CDDL](https://www.rfc-editor.org/rfc/rfc8610) is the IETF schema
language for CBOR. `wireform-cbor` parses it with `CBOR.CDDL.parseCDDL`
and generates Haskell types + `ToCBOR` / `FromCBOR` instances with
`CBOR.CDDLCodeGen`:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import CBOR.QQ (cddl)

[cddl|
  person = { name: tstr, age: uint }
|]
-- Generates: data Person = Person { name :: Text, age :: Word }
--            instance ToCBOR Person ; instance FromCBOR Person
```

For external `.cddl` files there's the `CBOR.CDDLCodeGen.generateCDDL`
entry point + the `wireform-gen` CLI in the umbrella package
(`wireform-gen cddl -i schema.cddl -o src/Gen/`).

## JSON bridge

`CBOR.JSON` round-trips between `CBOR.Value` and `Data.Aeson.Value`,
following the JSON-mapping recommendations in
[RFC 8949 §6.1](https://www.rfc-editor.org/rfc/rfc8949#section-6.1)
where there is an obvious one. CBOR types JSON has no shape for
(byte strings, tagged values, half-precision floats) follow the
recommended fallbacks: byte strings as base64url, tags as
`{"tag": N, "value": ...}` envelopes.

## Streaming

`CBOR.Stream.streamDecode` consumes input in `ByteString` chunks and
returns a `DecodeStep` that either yields a value, asks for more bytes,
or fails. Suitable for parsing CBOR off a socket or a file handle
without loading the whole input first:

```haskell
import qualified CBOR.Stream as CS

go :: Handle -> CS.DecodeStep CV.Value -> IO (Either String CV.Value)
go h step = case step of
  CS.NeedMore k -> BS.hGet h 4096 >>= go h . k
  CS.Done v _   -> pure (Right v)
  CS.Fail err   -> pure (Left err)
```

## Testing

The per-format Hedgehog suite lives in `test/`:

```bash
cabal test wireform-cbor:wireform-cbor-derive-test
```

<!-- BEGIN_AUTOGEN tests -->
_No data yet. Run `cabal test wireform-cbor:all --test-show-details=streaming --xml=dist-stats/test-results/wireform-cbor.junit.xml` to populate._
<!-- END_AUTOGEN tests -->

It covers the typeclass instances, the deriver, generic and
TH-derived round-trips, and the dynamic `Value` ADT.

## Benchmarks

A criterion harness in [`bench/FormatBench.hs`](../bench/FormatBench.hs)
(in the umbrella package) compares wireform-cbor's encode and decode
against [`cborg`](https://hackage.haskell.org/package/cborg), the
established Haskell CBOR library:

```bash
cabal bench format-bench
```

<!-- BEGIN_AUTOGEN bench:cbor-vs-cborg-encode -->
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="bench-results/charts/cbor-vs-cborg-encode-dark.svg">
  <img src="bench-results/charts/cbor-vs-cborg-encode-light.svg" alt="wireform-cbor vs cborg">
</picture>

| Operation | wireform-cbor |   cborg | ratio |
| :-------- | ------------: | ------: | ----: |
| encode    |        302 ns |  281 ns | 0.93x |
| decode    |        462 ns | 1289 ns | 2.79x |

<sub>Last run 2026-06-27 11:56:42 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:cbor-vs-cborg-encode -->

For cross-language comparisons, the canonical reference implementations
are [libcbor](https://github.com/PJK/libcbor) (C) and
[`ciborium`](https://crates.io/crates/ciborium) (Rust). A cross-language
harness against those is on the roadmap; numbers will land here once it
ships.

> Numbers TBD: run the harness above and drop a results table in.

## License

BSD-3-Clause.

## References

- [RFC 8949: Concise Binary Object Representation (CBOR)](https://www.rfc-editor.org/rfc/rfc8949)
- [RFC 8610: Concise Data Definition Language (CDDL)](https://www.rfc-editor.org/rfc/rfc8610)
- [IANA CBOR tag registry](https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml)
