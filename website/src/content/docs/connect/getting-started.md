---
title: Getting started with Connect RPC
description: "Add wireform-connect to a project, generate the service tags, run a Connect server, call it from a client, and run the loopback + demo.connectrpc.com interop tests."
sidebar:
  order: 2
  label: Getting started
---

This page walks from an empty project to a working Connect server and client.
The running example is the
[`connectrpc.eliza.v1.ElizaService`](https://buf.build/connectrpc/eliza) used by
the test suite — a unary `Say`, a server-streaming `Introduce`, a
bidirectional `Converse`, and a client-streaming `Aggregate`.

## Prerequisites

`wireform-connect` sits on top of three in-tree packages, all of which must be
on the build path (they ship together in this monorepo):

- [`wireform-proto`](../packages/proto/) — message types + the `loadProto` splice
- [`wireform-grpc`](../packages/grpc/) / [`grpc-spec`](../packages/grpc-spec/) —
  the protocol-agnostic service tags (`loadProtoServices`) and the `Proto`
  newtype with its JSON instances
- [`wireform-http`](../packages/http/) — the HTTP/1.1 + HTTP/2 transport

Add `wireform-connect` (plus `wireform-grpc` and `wireform-proto`) to your
cabal `build-depends`. It targets GHC 9.6 / 9.8.

## Generate the service

The service description is shared with gRPC. One `.proto`, two splices — in a
module with `{-# LANGUAGE DataKinds #-}` (the IDL bridge emits `HasField`
instances with type-level string literals):

```haskell
{-# LANGUAGE DataKinds, TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
module Eliza where

import Network.GRPC.Protobuf.TH (loadProtoServices)
import Proto.TH (loadProto)

$(loadProto "proto/eliza.proto")
$(loadProtoServices "proto/eliza.proto")
```

`loadProto` emits the message records (with proto3-JSON aeson instances and the
wire codec); `loadProtoServices` emits the protocol-agnostic
`Protobuf ElizaService "<meth>"` tags that both gRPC and Connect consume. Every
method tag — `Say`, `Introduce`, `Converse`, `Aggregate` — is now usable from
both transports.

## Serve it

A Connect server is one HTTP handler built from a list of method handlers. Each
handler is constructed with a builder whose name matches the streaming kind; the
method is fixed by a type application:

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
      [ mkNonStreaming    @Say       say
      , mkServerStreaming @Introduce introduce
      , mkBiDiStreaming   @Converse  converse
      ]

    -- Unary: Input -> ConnectServerM Output
    say (Proto req) = pure (Proto defaultSayResponse
      { sayResponseSentence = "Hello, " <> sayRequestSentence req })

    -- Server streaming: take the request, call `send` for each output.
    introduce (Proto req) send = do
      send (Proto defaultIntroduceResponse
        { introduceResponseSentence = "Hi " <> introduceRequestName req })
      send (Proto defaultIntroduceResponse { introduceResponseSentence = "..." })

    -- Bidirectional: `recv` for inputs, `send` for outputs, in any order.
    converse recv send = ...
```

Run it. `serverVersionRange = preferHttp20` negotiates HTTP/2 over TLS and falls
back to HTTP/1.1 on plaintext, so the same server speaks both. See
[Serving Connect RPCs](../server/) for the handler shapes, metadata accessors,
and error handling.

## Call it

A client is a connection plus codec settings, bracketed by `withConnectClient`:

```haskell
import Network.Connect.Client
import Network.Connect.Protocol (Codec (..))
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
    -- Unary POST; throws ConnectException on an error response.
    Proto resp <- nonStreaming cl (Proxy @Say)
                    (Proto defaultSayRequest { sayRequestSentence = "Hi" })
    print (sayResponseSentence resp)
```

Want JSON instead of binary Protobuf so you can read it with curl? Set the
codec:

```haskell
defaultConnectClientConfig { cccCodec = CodecJSON }
```

See [Calling Connect RPCs](../client/) for the streaming calls, TLS, and
per-call config.

## Verify against the reference server

The package's test suite is an in-process loopback (server + client over an
ephemeral port) covering every RPC kind for both codecs, plus unary GET, the
error path, gzip, and metadata propagation:

```bash
cabal test wireform-connect:wireform-connect-test
```

There is also opt-in interop against the public Connect reference server.
`demo.connectrpc.com` runs the same `ElizaService`; set one environment variable
and the `Test.Interop` suite calls it over TLS HTTP/2 for both codecs (skipped
silently otherwise):

```bash
CONNECT_DEMO=1 cabal test wireform-connect:wireform-connect-test
```

A green `CONNECT_DEMO` run is the strongest end-to-end signal that the
implementation matches the protocol — it talks to a server written by the
protocol's authors.

## Where to next

- [Serving Connect RPCs](../server/) — handlers, metadata, errors
- [Calling Connect RPCs](../client/) — calls, config, TLS
- [Wire protocol](../protocol/) — content types, framing, GET, compression
