# wireform-msgpack

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

[MessagePack](https://msgpack.org/) for Haskell. Encode and decode the
dynamic [`MsgPack.Value`](src/MsgPack/Value.hs), derive typeclass
instances generically or via Template Haskell, stream over chunked
input, run [msgpack-rpc](https://github.com/msgpack-rpc/msgpack-rpc) on
top of the codec, and bridge to JSON when interop calls for it.

MessagePack shares JSON's data model (scalars, arrays, maps) and adds
a binary type and an extension type for application-specific tagging.
It encodes the same shape JSON does in a compact tagged-byte format
instead of escaped UTF-8. Mature client libraries exist in essentially
every mainstream language, and it's a common pick wherever wire size
or parse speed matter more than human-readability.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-msgpack,
  wireform-derive,    -- only if you want the cross-format annotation deriver
```

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-msgpack` to compile
locally. Compiling with the LLVM backend (`-fllvm`) adds compile time
but measurably improves runtime performance.

## Hello world

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

import GHC.Generics (Generic)
import Data.Text (Text)
import MsgPack.Class (ToMsgPack, FromMsgPack, encodeMsgPack, decodeMsgPack)

data Person = Person
  { name  :: !Text
  , age   :: !Int
  , email :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToMsgPack, FromMsgPack)

main :: IO ()
main = do
  let alice = Person "Alice" 30 "alice@example.com"
      bytes = encodeMsgPack alice
  case decodeMsgPack bytes of
    Right (decoded :: Person) -> print decoded
    Left  err                 -> putStrLn err
```

The runnable version lives in [`examples/MsgPackExample.hs`](../examples/MsgPackExample.hs).

## What's in here

| Module             | Role                                                      |
|--------------------|-----------------------------------------------------------|
| `MsgPack.Value`    | Dynamic untyped `Value` ADT (`VInt` / `VStr` / `VBin` / `VArray` / `VMap` / `VExt` / ...) |
| `MsgPack.Encode`   | Low-level encoding primitives building straight onto `wireform-core`'s `Builder` |
| `MsgPack.Encoding` | The `Encoding` builder type used by `ToMsgPack` instances |
| `MsgPack.Decode`   | Low-level decoding primitives over the strict `ByteString` input |
| `MsgPack.Class`    | Public `ToMsgPack` / `FromMsgPack` typeclasses + `encodeMsgPack` / `decodeMsgPack` / `encodeMsgPackDirect` |
| `MsgPack.Derive`   | `deriveMsgPack` / `deriveToMsgPack` / `deriveFromMsgPack` Template Haskell entry points |
| `MsgPack.Stream`   | Incremental decoder for chunked / streaming input         |
| `MsgPack.JSON`     | Bridge to and from `aeson`'s `Value`                      |
| `MsgPack.RPC`      | msgpack-rpc framing (request / response / notification)   |

## Encode and decode

The typeclass entry points are the usual shape:

```haskell
encodeMsgPack       :: ToMsgPack a   => a          -> ByteString
encodeMsgPackDirect :: ToMsgPack a   => a          -> ByteString  -- direct-write path
decodeMsgPack       :: FromMsgPack a => ByteString -> Either String a
```

All three live in `MsgPack.Class` and dispatch through the `Encoding`
builder from `MsgPack.Encoding`. The direct-write variant skips the
intermediate builder representation when the size is statically
predictable.

For dynamic values without a Haskell type to mirror them, work with
[`MsgPack.Value`](src/MsgPack/Value.hs) directly:

```haskell
import qualified MsgPack.Encode as ME
import qualified MsgPack.Decode as MD
import qualified MsgPack.Value  as MV

let bytes = ME.encode (MV.VMap [(MV.VStr "ok", MV.VBool True)])
case MD.decode bytes of
  Right (v :: MV.Value) -> ...
```

## Annotation-driven deriving

`MsgPack.Derive` consumes the cross-format `Wireform.Derive.Modifier`
vocabulary from [`wireform-derive`](../wireform-derive/README.md), so
the same annotated record can produce MsgPack, CBOR, JSON, proto, and
any other backend's instances without redefining the field shapes:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified MsgPack.Derive       as DMP
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

DMP.deriveMsgPack  ''Person
DAeson.deriveJSON  ''Person
```

`personFullName` lands as `full_name` on the MsgPack wire and
`fullName` in JSON.

## JSON bridge

`MsgPack.JSON` round-trips between `MsgPack.Value` and
`Data.Aeson.Value`. The mapping is mostly obvious; the awkward
corners (binary blobs, ext types) follow the convention every other
language picks: binary as base64, ext as `{"type": N, "data": "..."}`
envelopes.

## Streaming

`MsgPack.Stream` is the incremental decoder for chunked input. It
consumes `ByteString` chunks and yields decoded values as soon as
they're complete, holding partial state between feeds. Suitable for
parsing MsgPack off a socket or a file handle without loading the
whole input first.

## msgpack-rpc

`MsgPack.RPC` ships the
[msgpack-rpc](https://github.com/msgpack-rpc/msgpack-rpc) framing on
top of the codec. The wire format is just MsgPack arrays with a
known leading shape (request, response, notification), which means
once you have the codec you essentially have the RPC layer for free:

```haskell
import qualified MsgPack.RPC as RPC

let req     = RPC.Request 42 "method.name" [RPC.VInt 1, RPC.VInt 2]
    bytes   = RPC.encodeRPC req
case RPC.decodeRPC bytes of
  Right (RPC.Response 42 _err result) -> ...
  Right (RPC.Notification _ _)        -> ...
  Left  err                           -> ...
```

`encodeRPC` / `decodeRPC` are pure. Wiring them into a transport
(TCP, Unix socket, websocket) is whatever your server or client
prefers; `MsgPack.Stream` handles the framing on the read side.

## Testing

The per-format Hedgehog suite lives in `test/`:

```bash
cabal test wireform-msgpack:wireform-msgpack-derive-test
```

<!-- BEGIN_AUTOGEN tests -->
_No data yet. Run `cabal test wireform-msgpack:all --test-show-details=streaming --xml=dist-stats/test-results/wireform-msgpack.junit.xml` to populate._
<!-- END_AUTOGEN tests -->

It covers the typeclass instances, the deriver, generic and
TH-derived round-trips, and the dynamic `Value` ADT.

## Benchmarks

A criterion harness in [`bench/FormatBench.hs`](../bench/FormatBench.hs)
(in the umbrella package) compares wireform-msgpack's encode and
decode against the Hackage
[`msgpack`](https://hackage.haskell.org/package/msgpack) library:

```bash
cabal bench format-bench
```

<!-- BEGIN_AUTOGEN bench:msgpack-vs-msgpack-haskell -->
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="bench-results/charts/msgpack-vs-msgpack-haskell-dark.svg">
  <img src="bench-results/charts/msgpack-vs-msgpack-haskell-light.svg" alt="wireform-msgpack vs Hackage msgpack">
</picture>

| Operation | wireform-msgpack | msgpack | ratio |
| :-------- | ---------------: | ------: | ----: |
| encode    |           297 ns | 1232 ns | 4.14x |
| decode    |           388 ns | 1775 ns | 4.58x |

<sub>Last run 2026-06-27 11:56:42 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:msgpack-vs-msgpack-haskell -->

For cross-language comparisons, the canonical reference implementations
are [msgpack-c](https://github.com/msgpack/msgpack-c) and
[`rmp-serde`](https://crates.io/crates/rmp-serde) (Rust). A
cross-language harness against those is on the roadmap; numbers will
land here once it ships.

> Numbers TBD: run the harness above and drop a results table in.

## License

BSD-3-Clause.

## References

- [MessagePack specification](https://github.com/msgpack/msgpack/blob/master/spec.md)
- [msgpack-rpc protocol](https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md)
