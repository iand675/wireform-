# wireform-grpc

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

Native Haskell [gRPC](https://grpc.io/) client and server. **Vendored
from [`grapesy`](https://github.com/well-typed/grapesy)** by Edsko de
Vries (Well-Typed) and re-published as `wireform-grpc` so that
wireform's gRPC service-method codegen targets a known-good in-tree
binding instead of forking against an upstream version.

The library is a fully compliant native Haskell implementation of
gRPC: client (`Network.GRPC.Client`), server
(`Network.GRPC.Server`), the four streaming flavors (unary, server
streaming, client streaming, bidirectional), HTTP/2 + TLS via the
in-tree [`wireform-http2`](../wireform-http2/) package's
`Network.HTTP2.Engine.*` modules (we no longer depend on the
upstream `http2`, `http2-tls`, or `http-semantics` packages),
compression negotiation, deadline handling, status code semantics,
metadata, and OpenTelemetry instrumentation. The wire format is
length-prefixed gRPC over HTTP/2; the message-level protobuf encoding
goes through [`wireform-proto`](../wireform-proto/) (the original
upstream `grapesy` uses `proto-lens`; the migration to wireform-proto
is what this package's vendored fork carries).

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Module names follow `grapesy`'s `Network.GRPC.*` shape, not
wireform's `<Format>.*` per-format convention, because the original
naming is what every grapesy user already imports against.

## Install

```cabal
build-depends:
  base,
  wireform-grpc,
  wireform-proto,
```

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-grpc` to compile
locally. The dep closure pulls in `wireform-http2`, `tls`,
`network`, `network-run`, `crypton-x509`, and friends; this is one of
the largest dep trees in the monorepo and the reason `wireform-grpc`
is not part of the default `nix develop` shell.

## Hello world

A minimal client call. The protobuf service definitions and message
types come from `wireform-proto`'s codegen
([`wireform-gen proto`](../wireform-proto/) or
[`Proto.TH.loadProto`](../wireform-proto/src/Proto/TH.hs) in a TH
splice):

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
    feature <- IO.nonStreaming conn (rpc @(Protobuf RG.RouteGuide "GetFeature"))
                  (defMessage & RG.point .~ pt 408000000 (-743000000))
    print feature
```

A minimal server. Handlers are registered with the transport-agnostic
`Service` vocabulary (shared with `wireform-connect`): one `method` per RPC,
order-insensitive, completeness-checked at compile time:

```haskell
import           Network.GRPC.Server.Run
import           Network.GRPC.Server.Service

import qualified Routeguide.Service.RouteGuide as RG

main :: IO ()
main = runServerWithHandlers def config (fromService routeGuide)
  where
    config = ServerConfig
      { serverInsecure = Just (InsecureConfig (Just "0.0.0.0") 50051)
      , serverSecure   = Nothing
      }

routeGuide :: Service RG.RouteGuide IO
routeGuide =
  service
    (  method @RG.GetFeature   handleGetFeature
    :& method @RG.ListFeatures handleListFeatures
    :& method @RG.RecordRoute  handleRecordRoute
    :& method @RG.RouteChat    handleRouteChat
    :& Done
    )

handleGetFeature :: RG.Point -> IO RG.Feature
handleGetFeature pt = ...
```

For raw RPCs (or full control over a single method via the `Call` API), drop
down to `Network.GRPC.Server.StreamType.fromMethod` /
`Network.GRPC.Server.someRpcHandler` and concatenate handler lists.

## What's in here

The full module list is in
[`wireform-grpc.cabal`](wireform-grpc.cabal). The most-imported
modules:

| Module                                       | Role                                                      |
|----------------------------------------------|-----------------------------------------------------------|
| `Network.GRPC.Client`                        | Client connection + per-call API                          |
| `Network.GRPC.Client.StreamType.IO`          | Direct-style helpers for the four streaming flavors |
| `Network.GRPC.Client.StreamType.Conduit`     | Conduit-based streaming helpers                           |
| `Network.GRPC.Server`                        | Server entry point + handler registration                 |
| `Network.GRPC.Server.Run`                    | `runServerWithHandlers` + the insecure / TLS config |
| `Network.GRPC.Server.Service`                | `Service`/`method` registration + `fromService` |
| `Network.GRPC.Server.StreamType`             | Per-RPC streaming-flavor handler builders (escape hatch) |
| `Network.GRPC.Common`                        | Shared types: `Address`, `Timeout`, `Status`, metadata, deadlines |
| `Network.GRPC.Common.Compression`            | gzip / deflate / identity compression negotiation         |
| `Network.GRPC.Common.Protobuf`               | Protobuf message helpers (the bridge to `wireform-proto`) |
| `Network.GRPC.Common.Protobuf.Any`           | `google.protobuf.Any` envelope handling                   |
| `Network.GRPC.Server.Otel` / `.Client.Otel`  | OpenTelemetry instrumentation following the messaging semantic conventions |

Everything under `Network.GRPC.Util.*` is intentionally
`other-modules` (private) and not re-exported.

## Differences from upstream `grapesy`

This package began as a vendor of
[`grapesy`](https://github.com/well-typed/grapesy) version 1.1.x and is now
a maintained fork with two substantive changes:

- **Protobuf binding migration**: upstream uses `proto-lens` for
  protobuf message types; this fork uses `wireform-proto`. The
  `Network.GRPC.Common.Protobuf` and
  `Network.GRPC.Client.StreamType.IO.Binary` modules wire through
  the wireform-proto `Proto.Encode` / `Proto.Decode` typeclasses
  instead of `proto-lens`'s `Message`.

- **Handler-registration replacement**: upstream's order-sensitive
  `Methods`/`Services` GADTs (`fromMethods`, `fromServices`,
  `simpleMethods`, `Network.GRPC.Server.Protobuf`) are replaced by the
  transport-agnostic `Service` vocabulary in `grpc-spec`
  (`Network.GRPC.Spec.Service`) plus the `fromService` adapter in
  `Network.GRPC.Server.Service`. Registration is order-insensitive,
  completeness-checked, and the same `Service` value is servable over
  Connect (`wireform-connect`). `ServiceMethods` moved from
  `Network.GRPC.Server.Protobuf` to `grpc-spec`.

The minimal test suite (`wireform-grpc-test-minimal`) verifies the
binding compiles and round-trips. The fuller upstream test suites and
benchmarks are temporarily disabled while the proto-lens-generated
fixtures are regenerated through wireform-proto's codegen.

## Authorship and license

Original library: **`grapesy` by Edsko de Vries / Well-Typed**.
Vendored and republished as `wireform-grpc` with the proto-lens →
wireform-proto migration applied. License preserved: BSD-3-Clause.

If you're using gRPC outside the wireform monorepo, prefer the
upstream [`grapesy`](https://hackage.haskell.org/package/grapesy)
package directly. This vendored copy exists so that wireform's
gRPC service-method codegen can target a known-good in-tree binding.

## Testing

```bash
cabal test wireform-grpc:wireform-grpc-test-minimal
```

The minimal test confirms the proto-lens → wireform-proto migration
hasn't broken the wire-level encoding. The fuller upstream suites
(client / server / interop / stress) are temporarily disabled
pending regeneration of their fixtures through wireform-proto's
codegen.

## Benchmarks

No per-package criterion harness in tree yet. Planned comparisons:

- Haskell: the upstream
  [`grapesy`](https://hackage.haskell.org/package/grapesy) (the same
  code, different protobuf binding) for a wireform-proto-vs-proto-
  lens overhead measurement.
- Go: [`grpc-go`](https://github.com/grpc/grpc-go), the
  highest-traffic gRPC implementation in production.
- Rust: [`tonic`](https://crates.io/crates/tonic).
- C++: [`grpc-cpp`](https://github.com/grpc/grpc), the reference
  implementation.

> Numbers TBD: harness pending.

## License

BSD-3-Clause. Original copyright: Edsko de Vries / Well-Typed.

## References

- [`grapesy` upstream](https://github.com/well-typed/grapesy)
- [gRPC over HTTP/2 protocol](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md)
- [gRPC core concepts](https://grpc.io/docs/what-is-grpc/core-concepts/)
- [OpenTelemetry RPC semantic conventions](https://opentelemetry.io/docs/specs/semconv/rpc/)
