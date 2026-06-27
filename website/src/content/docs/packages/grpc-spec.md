---
title: grpc-spec
description: "Pure, transport-independent implementation of the core gRPC-over-HTTP/2 spec, vendored from the grapesy stack."
sidebar:
  order: 56
---

`grpc-spec` is a foundation library that implements the **pure part of the
core gRPC spec**, the parts of
[`PROTOCOL-HTTP2.md`](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md)
that do not depend on any particular transport or I/O strategy. It models gRPC
request/response headers, trailers, status codes, timeouts, compression
negotiation, custom metadata, and the length-prefixed message framing, but it
does not open sockets or drive HTTP/2 itself. As the package description puts
it, "this is by no means a full gRPC implementation, but can be used as the
basis for one."

In this monorepo it is exactly that basis: [`wireform-grpc`](../grpc/) builds
its client and server on top of `grpc-spec` plus the in-tree
[`wireform-http2`](../http/) engine.

## Provenance / vendored

This package is **vendored from
[`grapesy`](https://github.com/well-typed/grapesy)** by Edsko de Vries
(Well-Typed), the same upstream stack that [`wireform-grpc`](../grpc/) is
vendored from. The cabal `source-repository` still points at
`github.com/well-typed/grapesy`. The upstream test suite is currently disabled
during the wireform migration because it depends on `proto-lens` test helpers
that need rewriting (see the comment in `grpc-spec.cabal`). Because it is
foundation code, follow the same coordination rule as `wireform-grpc`: prefer
upstream-aligned changes over local divergence.

## What it owns

- **Request and response headers**: the gRPC pseudo-header and metadata model
  (`Network.GRPC.Spec.Headers.*`, surfaced through `Network.GRPC.Spec`).
- **Custom metadata**: typed and raw metadata maps and the `NoMetadata` case.
- **Status and trailers**: gRPC status codes and trailer handling
  (`Network.GRPC.Spec.Status`, `Network.GRPC.Spec.Serialization.Status`).
- **Timeouts**: the gRPC timeout grammar
  (`Network.GRPC.Spec.Timeout` / `Network.GRPC.Spec.Serialization.Timeout`).
- **Compression**: per-message compression negotiation
  (`Network.GRPC.Spec.Compression`).
- **Length-prefixed framing**: the 5-byte message prefix codec
  (`Network.GRPC.Spec.Serialization.LengthPrefixed`).
- **RPC codecs**: Protobuf, JSON, and raw RPC descriptions plus the
  `StreamType` vocabulary (`Network.GRPC.Spec.RPC.*`). Protobuf integration is
  built on [`wireform-proto`](../proto/).
- **Tracing / load reporting**: W3C trace context and ORCA load reports
  (`Network.GRPC.Spec.TraceContext`, `Network.GRPC.Spec.OrcaLoadReport`).

## Notable modules

These are the exposed modules from `grpc-spec.cabal`:

| Module | Purpose |
|--------|---------|
| `Network.GRPC.Spec` | Top-level surface: headers, metadata, status, timeouts, RPC types |
| `Network.GRPC.Spec.Serialization` | Wire serialization of headers, status, timeouts, framing |
| `Network.GRPC.Spec.Util.HKD` | Higher-kinded-data helpers used across the spec types |
| `Network.GRPC.Spec.Util.Parser` | Shared parser combinators |
| `Network.GRPC.Spec.Util.Protobuf` | Protobuf helper glue over `wireform-proto` |

The bulk of the implementation (`Network.GRPC.Spec.Headers.*`,
`Network.GRPC.Spec.RPC.*`, `Network.GRPC.Spec.CustomMetadata.*`,
`Network.GRPC.Spec.Serialization.*`, the bundled `Proto.OrcaLoadReport` and
`Proto.Status`) lives as `other-modules` and is re-exported through the five
modules above.

## Relationship to wireform-grpc

| Layer | Package |
|-------|---------|
| Pure gRPC spec (headers, status, framing, codecs) | `grpc-spec` |
| HTTP/2 transport | [`wireform-http2`](../http/) |
| Client / server runtime, connection management | [`wireform-grpc`](../grpc/) |

If you are writing application code, use [`wireform-grpc`](../grpc/). Reach for
`grpc-spec` directly only when you need the transport-independent spec types,
for example to build an alternative transport or to inspect/construct gRPC
headers and trailers without a live connection.
