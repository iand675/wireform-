# wireform-connect

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

A native Haskell implementation of the [Connect RPC
protocol](https://connectrpc.com/docs/protocol): a client **and** server for
unary and all three streaming RPC kinds, over **both HTTP/1.1 and HTTP/2**,
with **binary Protobuf and JSON** codecs, **GET** support for side-effect-free
unary calls, **identity/gzip/br/zstd** compression, the **gRPC-derived
error-code model**, leading/trailing **metadata**, and the streaming
**EndStreamResponse** envelope.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Like [`wireform-grpc`](../wireform-grpc/), it is an RPC framework, so
it owns the `Network.Connect.*` namespace rather than wireform's per-format
`<Format>.*` convention, and it is not re-exported through the umbrella
`wireform` package.

## Connect is a transport over the existing service tags

The Connect runtime reuses the **same protocol-agnostic service description**
that gRPC uses. A generated Protobuf service — the `Protobuf serv "meth"` tags
that [`loadProtoServices`](../wireform-grpc/src/Network/GRPC/Protobuf/TH.hs)
emits, together with the message types from
[`loadProto`](../wireform-proto/src/Proto/TH.hs) — drives Connect **unchanged**.
There is no separate Connect codegen: Connect is purely a new transport over the
existing `IsRPC` / `SupportsClientRpc` / `SupportsServerRpc` / `HasStreamingType`
abstraction in [`grpc-spec`](../grpc-spec/). The `application/grpc+proto`
content-type baked into each tag is ignored; Connect computes its own content
types (`application/proto`, `application/json`, `application/connect+proto`,
`application/connect+json`) from the codec and streaming kind.

The proto3-JSON path works because the `Proto` newtype carries `ToJSON` /
`FromJSON` instances (defined in `grpc-spec`, delegating to the message's
generated aeson instances), so a method's `Input` / `Output` serialize in
either codec without unwrapping.

## Hello world

Generate the service tags + message types once (in a module with
`{-# LANGUAGE DataKinds #-}`):

```haskell
{-# LANGUAGE DataKinds, TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
module Eliza where

import Network.GRPC.Protobuf.TH (loadProtoServices)
import Proto.TH (loadProto)

$(loadProto "proto/eliza.proto")
$(loadProtoServices "proto/eliza.proto")
```

A server (one handler per method; the streaming kind is fixed by the tag, so
the builder is chosen with a type application):

```haskell
import Network.Connect.Server
import Network.Connect.Compression (ContentCoding (..))
import Network.HTTP.Server (defaultServerConfig, ServerConfig (..))
import Network.HTTP.VersionRange (preferHttp2)
import Network.GRPC.Spec (Proto (..))
import Eliza

main :: IO ()
main =
  runConnectServer defaultConnectServerConfig serverCfg handlers
  where
    serverCfg = defaultServerConfig { serverPort = "8080", serverVersionRange = preferHttp2 }
    handlers =
      [ mkNonStreaming    @Say       say
      , mkServerStreaming @Introduce introduce
      , mkBiDiStreaming   @Converse  converse
      ]

    say (Proto req) =
      pure (Proto defaultSayResponse { sayResponseSentence = "Hello, " <> sayRequestSentence req })
    -- introduce / converse : ConnectServerM handlers using the recv / send callbacks
```

A client:

```haskell
import Network.Connect.Client
import Network.Connect.Protocol (Codec (..))
import Network.GRPC.Spec (Proto (..))
import Data.Proxy (Proxy (..))
import Eliza

main :: IO ()
main = do
  let ccfg    = defaultConnectClientConfig { cccCodec = CodecProto }
      connCfg = defaultConnectionConfig { connectionHost = "demo.connectrpc.com", connectionPort = "443" }
  withConnectClient ccfg connCfg { connectionTls = Just (defaultTlsConnectionConfig "demo.connectrpc.com") } $ \cl -> do
    Proto resp <- nonStreaming cl (Proxy @Say) (Proto defaultSayRequest { sayRequestSentence = "Hi" })
    print (sayResponseSentence resp)
```

## What's in here

| Module | Role |
|---|---|
| `Network.Connect.Server` | `runConnectServer`, the `mkNonStreaming`/`mkClientStreaming`/`mkServerStreaming`/`mkBiDiStreaming` handler builders, `ConnectServerM` + metadata accessors |
| `Network.Connect.Client` | `withConnectClient` + `nonStreaming`/`nonStreamingGet`/`serverStreaming`/`clientStreaming`/`biDiStreaming` |
| `Network.Connect.Protocol` | Codecs, the content-type matrix, reserved header names, GET query parameters |
| `Network.Connect.Error` | `ConnectError`/`ConnectException`, the code↔name + code↔HTTP-status tables, the JSON error envelope |
| `Network.Connect.Envelope` | The streaming frame (1 flag byte + 4-byte length) + `EndStreamResponse` |
| `Network.Connect.Metadata` | `CustomMetadata` ↔ HTTP headers (ASCII / `-bin` base64 / `trailer-` prefix) |
| `Network.Connect.Codec` | proto / JSON message-body (de)serialization |
| `Network.Connect.Compression` | `identity` / `gzip` / `br` / `zstd` + accept-encoding negotiation |

## Testing

```bash
cabal test wireform-connect:wireform-connect-test
```

The suite is an in-process loopback (server + client over an ephemeral port)
covering every RPC kind for both codecs, plus unary GET, the error path, gzip
compression, and metadata propagation; it also unit-tests the wire vocabulary
(content-type matrix, error tables, envelope framing, metadata round-trips,
compression).

Opt-in interop against the public demo server is gated by an environment
variable:

```bash
CONNECT_DEMO=1 cabal test wireform-connect:wireform-connect-test
```

runs `Test.Interop` against `demo.connectrpc.com`'s `connectrpc.eliza.v1.ElizaService`
over TLS HTTP/2 for both codecs (skipped silently otherwise).

## License

BSD-3-Clause.

## References

- [Connect protocol reference](https://connectrpc.com/docs/protocol)
- [`grpc-spec`](../grpc-spec/) — the shared RPC abstraction Connect reuses
- [`wireform-http`](../wireform-http/) — the HTTP/1.1 + HTTP/2 transport
