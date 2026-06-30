---
title: Connect RPC
description: "Serve and call Connect RPCs in Haskell — unary and streaming over HTTP/1.1 and HTTP/2, protobuf and JSON, with the gRPC error model. wireform-connect is a new transport over wireform-grpc's existing service tags."
sidebar:
  order: 1
  label: Overview
---

`wireform-connect` is a native Haskell implementation of the
[Connect RPC protocol](https://connectrpc.com/docs/protocol): a client **and**
server for unary and all three streaming RPC kinds, over **both HTTP/1.1 and
HTTP/2**, with **binary Protobuf and JSON** codecs, **GET** for side-effect-free
unary calls, **identity/gzip/br/zstd** compression, the **gRPC-derived
error-code model**, leading/trailing **metadata**, and the streaming
**EndStreamResponse** envelope.

Connect speaks the same Protobuf services as gRPC but over plain HTTP: no
HTTP/2 requirement, no custom framing on unary calls, and responses any HTTP
client (curl, a browser, `fetch`) can read directly. Reach for `wireform-connect`
when you want a gRPC-style service that is also callable from plain HTTP; reach
for [`wireform-grpc`](../packages/grpc/) when you need gRPC's wire compatibility
specifically.

## A transport, not a codegen

The headline design point: **Connect is purely a new transport over the existing
service description.** The `Protobuf serv "meth"` service-method tags that
`loadProtoServices` emits for gRPC, together with the message types from
`loadProto`, drive Connect **unchanged**. There is no Connect-specific code
generator — you write one `.proto`, run the same two TH splices, and the
generated service works under gRPC *and* Connect. Connect ignores the
`application/grpc+proto` content-type baked into each tag and computes its own
(`application/proto`, `application/json`, `application/connect+proto`,
`application/connect+json`) from the codec and streaming kind.

The proto3-JSON path works because the `Proto` newtype carries `ToJSON` /
`FromJSON` instances (defined in [`grpc-spec`](../packages/grpc-spec/)),
delegating to each message's generated aeson instances — so a method's `Input`
and `Output` serialize in either codec with no unwrapping.

## Hello world

Generate the service tags and message types once (the module needs
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

A server — one handler per method, the builder chosen with a type application
because the streaming kind is fixed by the tag:

```haskell
import Network.Connect.Server
import Network.HTTP.Server (defaultServerConfig, ServerConfig (..))
import Network.HTTP.VersionRange (preferHttp20)
import Network.GRPC.Spec (Proto (..))
import Eliza

main :: IO ()
main = runConnectServer defaultConnectServerConfig serverCfg handlers
  where
    serverCfg = defaultServerConfig
      { serverPort = "8080", serverVersionRange = preferHttp20 }
    handlers =
      [ mkNonStreaming @Say say
      , mkServerStreaming @Introduce introduce
      , mkBiDiStreaming @Converse converse
      ]
    say (Proto req) = pure (Proto defaultSayResponse
      { sayResponseSentence = "Hello, " <> sayRequestSentence req })
```

A client:

```haskell
import Network.Connect.Client
import Network.HTTP.Client
  (defaultConnectionConfig, ConnectionConfig (..))
import Network.GRPC.Spec (Proto (..))
import Data.Proxy (Proxy (..))
import Eliza

main :: IO ()
main = do
  let connCfg = defaultConnectionConfig
        { connectionHost = "localhost", connectionPort = "8080" }
  withConnectClient defaultConnectClientConfig connCfg $ \cl -> do
    Proto resp <- nonStreaming cl (Proxy @Say)
                    (Proto defaultSayRequest { sayRequestSentence = "Hi" })
    print (sayResponseSentence resp)
```

For the full, runnable walkthrough — dependencies, codegen, running the server,
calling it, and the `demo.connectrpc.com` interop check — see
[Getting started](./getting-started/). For the per-method API, see
[Serving Connect RPCs](./server/) and [Calling Connect RPCs](./client/); for the
on-the-wire shapes (content types, the streaming envelope, unary GET,
compression), see [Wire protocol](./protocol/).

## What's in the package

| Module | Role |
|---|---|
| `Network.Connect` | Umbrella re-export of the public surface |
| `Network.Connect.Server` | `runConnectServer`, the `mkNonStreaming` / `mkClientStreaming` / `mkServerStreaming` / `mkBiDiStreaming` handler builders, `ConnectServerM` + metadata accessors |
| `Network.Connect.Client` | `withConnectClient` + `nonStreaming` / `nonStreamingGet` / `serverStreaming` / `clientStreaming` / `biDiStreaming` |
| `Network.Connect.Protocol` | Codecs, the content-type matrix, reserved header names, GET query parameters |
| `Network.Connect.Error` | `ConnectError` / `ConnectException`, the code↔name and code↔HTTP-status tables, the JSON error envelope |
| `Network.Connect.Envelope` | The streaming frame (1 flag byte + 4-byte length) + `EndStreamResponse` |
| `Network.Connect.Metadata` | `CustomMetadata` ↔ HTTP headers (ASCII / `-bin` base64 / `trailer-` prefix) |
| `Network.Connect.Codec` | proto / JSON message-body (de)serialization |
| `Network.Connect.Compression` | `identity` / `gzip` / `br` / `zstd` + accept-encoding negotiation |

Like `wireform-grpc`, this is an RPC framework: it owns the `Network.Connect.*`
namespace (not wireform's per-format `<Format>.*` convention) and is not
re-exported through the umbrella `wireform` package.
