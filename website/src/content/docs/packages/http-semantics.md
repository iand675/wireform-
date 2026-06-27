---
title: http-semantics
description: "Version-independent common parts of HTTP, vendored from kazu-yamamoto's http-semantics and used by the HTTP/2 stack."
sidebar:
  order: 58
---

`http-semantics` is a foundation library holding the **version-independent
common parts of HTTP**: the request/response, header, status, trailer, and I/O
abstractions that are shared regardless of whether the bytes on the wire are
HTTP/1 or HTTP/2. It is consumed by the HTTP/2 engine that
[`wireform-http`](../http/) and [`wireform-grpc`](../grpc/) build on, providing
the common semantic types those transports specialise.

## Provenance / vendored

This package is **vendored from
[`kazu-yamamoto/http-semantics`](https://github.com/kazu-yamamoto/http-semantics)**
(author/maintainer Kazu Yamamoto, BSD-3-Clause). Its cabal `synopsis` is "HTTP
semantics library" and its `description` is simply "Version-independent common
parts of HTTP." As foundation code shared with the upstream `http2` stack,
prefer upstream-aligned changes over local divergence.

## What it owns

- **Semantic types**: the common request/response and header model used by
  both client and server roles.
- **Token table**: the well-known header token set (`Network.HTTP.Semantics.Token`).
- **Client and server surfaces**: role-specific entry points plus their
  `Internal` variants.
- **I/O abstractions**: the buffer-fill / read primitives (`FillBuf`, `ReadN`)
  surfaced through `Network.HTTP.Semantics.IO`.
- **Status and trailers**: status-code and trailer handling shared across
  versions.

## Notable modules

These are the exposed modules from `http-semantics.cabal`:

| Module | Purpose |
|--------|---------|
| `Network.HTTP.Semantics` | Top-level common HTTP semantic types |
| `Network.HTTP.Semantics.Client` | Client-role semantics |
| `Network.HTTP.Semantics.Client.Internal` | Internal client types |
| `Network.HTTP.Semantics.Server` | Server-role semantics |
| `Network.HTTP.Semantics.Server.Internal` | Internal server types |
| `Network.HTTP.Semantics.IO` | Buffer / read I/O abstractions |
| `Network.HTTP.Semantics.Token` | Well-known header token table |

Supporting types (`Header`, `Status`, `Trailer`, `File`, `FillBuf`, `ReadN`,
`Types`) are `other-modules` re-exported through the surface above. The library
builds against `http-types`, `network`, `network-byte-order`, and
`time-manager`, and is compiled `Strict`/`StrictData`.

## Relationship to the HTTP stack

| Layer | Package |
|-------|---------|
| Version-independent HTTP semantics | `http-semantics` |
| HTTP/2 transport | [`wireform-http2`](../http/) |
| Unified client / server, version negotiation | [`wireform-http`](../http/) |

Application code should use [`wireform-http`](../http/). Reach for
`http-semantics` directly only when you are working below the transport (for
example, building or extending an HTTP/2 implementation that needs the shared
semantic types). For HTTP *header wire grammar* specifically, see
[`hermes`](../hermes/), which is the canonical home for per-header parsing and
rendering in this monorepo.
