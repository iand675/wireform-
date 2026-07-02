---
title: wireform-grpc
description: "Native Haskell gRPC client and server with unary and streaming RPCs, HTTP/2 transport, TLS, compression, protobuf and JSON codecs, and OpenTelemetry."
sidebar:
  order: 50
---

`wireform-grpc` is a native Haskell implementation of gRPC. gRPC is the
standard RPC layer for microservices that already speak Protocol Buffers.
Use this package when you need a pure-Haskell client or server with unary
and streaming call patterns, without binding to the C++ gRPC core library.

The library is **vendored from [`grapesy`](https://github.com/well-typed/grapesy)**
by Edsko de Vries (Well-Typed) and integrated into the wireform monorepo so
service codegen targets a known in-tree binding.

## Key features

- **Client and server** under `Network.GRPC.Client` and `Network.GRPC.Server`
- **Unary and streaming RPCs**: server streaming, client streaming, and
  bidirectional streaming
- **HTTP/2 transport** via the in-tree `wireform-http2` engine
- **TLS** with certificate store helpers
- **Message compression** negotiation
- **Protobuf and JSON codecs** through `Network.GRPC.Common.Protobuf` and
  `Network.GRPC.Common.JSON`
- **OpenTelemetry** instrumentation via `Network.GRPC.Client.Otel` and
  `Network.GRPC.Server.Otel`

## Basic usage

A minimal unary client call. Protobuf service types come from
`wireform-proto` codegen or a `Proto.TH.loadProto` splice:

```haskell
import           Network.GRPC.Client
import qualified Network.GRPC.Client.StreamType.IO as IO
import           Network.GRPC.Common
import           Network.GRPC.Common.Protobuf

import qualified Routeguide.Service.RouteGuide as RG

main :: IO ()
main = do
  let server = ServerInsecure (Address "localhost" 50051 Nothing)
  withConnection def server $ \conn -> do
    feature <-
      IO.nonStreaming
        conn
        (rpc @(Protobuf RG.RouteGuide "GetFeature"))
        (defMessage & RG.point .~ pt 408000000 (-743000000))
    print feature
  where
    pt lat lon =
      defMessage & RG.latitude .~ lat & RG.longitude .~ lon
```

For server-side handlers, implement the service with the transport-agnostic
`Service` vocabulary (shared with `wireform-connect`) and adapt it with
`Network.GRPC.Server.Service.fromService`:

```haskell
import           Network.GRPC.Server.Run
import           Network.GRPC.Server.Service

runGuideServer :: IO ()
runGuideServer =
  runServerWithHandlers def config (fromService routeGuide)
  where
    config =
      ServerConfig
        { serverInsecure = Just (InsecureConfig (Just "0.0.0.0") 50051)
        , serverSecure   = Nothing
        }

routeGuide :: Service RouteGuide IO
routeGuide =
  service
    (  method @GetFeature handleGetFeature
    :& ...
    :& Done
    )
```

Wrap calls with `Network.GRPC.Client.Otel.withTracedRPC` when you need
distributed tracing spans around RPC latency.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Network.GRPC.Client` | Connection management, `withConnection`, `withRPC` |
| `Network.GRPC.Client.StreamType.IO` | Unary and streaming client handlers |
| `Network.GRPC.Server` | Server entry point and call dispatch |
| `Network.GRPC.Server.Service` | `Service` / `method` registration + `fromService` (shared vocabulary with `wireform-connect`) |
| `Network.GRPC.Server.Run` | Server lifecycle and listener setup |
| `Network.GRPC.Common.Protobuf` | Protobuf codec integration with `wireform-proto` |
| `Network.GRPC.Common.Compression` | Compression negotiation |
| `Network.GRPC.Client.Otel` / `Network.GRPC.Server.Otel` | OpenTelemetry hooks |
| `Network.GRPC.Util.TLS` | Certificate store and TLS helpers |

## Module naming

This package follows grapesy's `Network.GRPC.*` module layout rather than
wireform's `<Format>.*` convention, so existing grapesy imports continue to
work after the vendored migration.
