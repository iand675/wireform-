---
title: wireform-http-wai
description: "Bidirectional adapter between WAI and the wireform-http unified request/response types."
sidebar:
  order: 54
---

`wireform-http-wai` is a bidirectional adapter between WAI (the Web
Application Interface) and the [`wireform-http`](../http/) unified
request/response types. Use it to exercise existing WAI applications with
wireform-http client tooling, or to serve wireform-http handlers through
Warp or any WAI-compatible server.

The package exposes a single module, `Network.HTTP.WAI`, which carries the
conversion functions in both directions plus the higher-level adapters.

## Key features

- **Run WAI apps as a wireform-http `Handler`** with `waiToHandler`
- **Drive WAI apps through a wireform-http `Transport`** for in-process
  client testing with the full middleware stack (`waiTransport`)
- **Serve wireform-http handlers through Warp** (or any WAI server) with
  `handlerToWai`
- **Low-level conversions** for method, status, version, headers, request,
  and response (`toWai*` / `fromWai*`)

## Basic usage

Wrap an existing WAI `Application` so wireform-http can call it:

```haskell
import qualified Network.Wai as Wai
import           Network.HTTP.WAI (waiToHandler, waiTransport)

myWaiApp :: Wai.Application
myWaiApp = ...

-- Use as a wireform-http Handler (server-level types)
handler :: Network.HTTP.Server.Handler
handler = waiToHandler myWaiApp

-- Or as a wireform-http client Transport for in-process testing
transport :: Network.HTTP.Client.Transport.Transport IO
transport = waiTransport myWaiApp
```

Serve a wireform-http handler through Warp:

```haskell
import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.WAI (handlerToWai)

myHandler :: Network.HTTP.Server.Handler
myHandler req = ...

main :: IO ()
main = Warp.run 8080 (handlerToWai myHandler)
```

## Notable functions

All exported from the single module `Network.HTTP.WAI`:

| Function | Purpose |
|----------|---------|
| `waiToHandler` | Run a WAI `Application` as a wireform-http `Handler` |
| `handlerToWai` | Expose a wireform-http handler as a WAI `Application` |
| `waiTransport` | Drive a WAI `Application` through a client-level `Transport` |
| `toWaiRequest` / `fromWaiRequest` | Request conversion in both directions |
| `toWaiResponse` / `fromWaiResponse` | Response conversion in both directions |
| `toWaiMethod` / `fromWaiMethod` | Method conversion |
| `toWaiStatus` / `fromWaiStatus` | Status conversion |
| `toWaiHeaders` / `fromWaiHeaders` | Header conversion |

## Limitations

- WAI `responseRaw` (WebSocket upgrade) is not supported; the adapter
  converts the backup response. Use a real TCP server for upgrade testing.
- `fromWaiResponse` materialises the entire response body into memory. For
  large streaming responses, test at the WAI level directly.
