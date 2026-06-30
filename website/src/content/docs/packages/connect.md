---
title: wireform-connect
description: "Native Haskell Connect RPC client and server (connectrpc.com): unary and all streaming kinds over HTTP/1.1 and HTTP/2, protobuf and JSON codecs, GET, compression, and the gRPC error model — a new transport over wireform-grpc's existing service tags."
sidebar:
  order: 57
---

`wireform-connect` is a native Haskell implementation of the
[Connect RPC protocol](https://connectrpc.com/docs/protocol): a client **and**
server for unary and all three streaming RPC kinds, over **both HTTP/1.1 and
HTTP/2**, with **binary Protobuf and JSON** codecs, **GET** for side-effect-free
unary calls, **identity/gzip/br/zstd** compression, the **gRPC-derived
error-code model**, leading/trailing **metadata**, and the streaming
**EndStreamResponse** envelope.

Connect speaks the same Protobuf services as gRPC but over plain HTTP — no
HTTP/2 requirement, no custom framing on unary calls, and responses any HTTP
client (curl, a browser, `fetch`) can read directly. Reach for it when you want
a gRPC-style service that is also callable from plain HTTP; reach for
[`wireform-grpc`](../grpc/) when you need gRPC's wire compatibility.

## In-depth guides

This page is the package catalogue entry. For the full documentation — setup,
serving, calling, and the wire protocol — see the **[Connect RPC
section](../../connect/)**:

- [Overview](../../connect/) — what Connect is, when to use it vs gRPC, hello world
- [Getting started](../../connect/getting-started/) — codegen, run a server, call it, demo interop
- [Serving Connect RPCs](../../connect/server/) — handlers, metadata, errors
- [Calling Connect RPCs](../../connect/client/) — calls, config, TLS
- [Wire protocol](../../connect/protocol/) — content types, framing, GET, compression

## A transport, not a codegen

The headline design point: Connect is purely a new transport over the existing
service description. The `Protobuf serv "meth"` tags that `loadProtoServices`
emits for gRPC, plus the message types from `loadProto`, drive Connect
**unchanged** — there is no Connect-specific code generator. Connect ignores the
`application/grpc+proto` content-type baked into each tag and computes its own
from the codec and streaming kind.

## Notable modules

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

Like `wireform-grpc` this is an RPC framework: it owns the `Network.Connect.*`
namespace (not wireform's per-format `<Format>.*` convention) and is not
re-exported through the umbrella `wireform` package.

## Testing

```bash
cabal test wireform-connect:wireform-connect-test   # in-process loopback, all kinds × both codecs
CONNECT_DEMO=1 cabal test wireform-connect:wireform-connect-test  # + real interop vs demo.connectrpc.com
```

## References

- [Connect protocol reference](https://connectrpc.com/docs/protocol)
- [`grpc-spec`](../grpc-spec/) — the shared RPC abstraction Connect reuses
- [`wireform-grpc`](../grpc/) — the gRPC implementation whose service tags Connect shares
- [`wireform-http`](../http/) — the HTTP/1.1 + HTTP/2 transport
