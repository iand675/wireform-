---
title: wireform-http1
description: "From-scratch HTTP/1.x client and server (RFC 9112) with zero-copy ring-buffer parsing, SIMD header scanning, and connection pooling."
sidebar:
  order: 52
---

`wireform-http1` is a from-scratch HTTP/1.0 / HTTP/1.1 implementation built
on wireform's performance philosophy: zero-copy parsing on a pinned ring
buffer, SIMD-accelerated header / CRLF / token scanning, exact-size sends
through `Wireform.Builder`, and allocation-free hot loops. It is one of the
two parallel implementations behind the unified [`wireform-http`](../http/)
stack (the other being [`wireform-http2`](../http2/)).

The package targets full conformance with the relevant HTTP/1.x RFCs:
RFC 9110 (HTTP semantics), RFC 9112 (HTTP/1.1 message syntax & routing,
including chunked transfer-coding and trailers), and the RFC 7230 §3.3
framing rules, with request-smuggling guards that reject conflicting
`Content-Length` / `Transfer-Encoding`, duplicate or non-numeric
`Content-Length`, non-final `chunked` codings, and CR / LF / NUL inside
header values.

## Key features

- **Zero-copy parsing** on a pinned magic-ring buffer (see
  [`wireform-network`](../network/))
- **SIMD header / CRLF / token scanning** via a C fast path
  (`cbits/http1_scan.c`)
- **Chunked transfer-coding** encode and decode, including request and
  response trailers
- **Request-smuggling guards** following RFC 7230 §3.3 framing precedence
- **Connection pooling** through `Network.HTTP1.Client.Pool`
- **`sendfile(2)` static-file fast path** via `Network.HTTP1.SendFile`
- **TLS bridge** (`Network.HTTP1.TLS`); the ring-direct OpenSSL path lives
  in [`Wireform.Network.TLS.OpenSSL`](../network/)
- **Optional ring pool** (`serverRingPool`) so accepted connections reuse
  pre-allocated rings instead of allocating fresh ones per connection

## Basic usage

Run a server with a `Handler` (`Request -> IO Response`):

```haskell
import qualified Network.HTTP1.Server as H1
import           Network.HTTP1.Types
import           Network.HTTP1.Status (pattern OK)

main :: IO ()
main = H1.runServer cfg
  where
    cfg =
      H1.defaultServerConfig
        { H1.serverHost    = "0.0.0.0"
        , H1.serverPort    = "8080"
        , H1.serverHandler = \_req ->
            pure
              Response
                { responseStatus   = OK
                , responseVersion  = HTTP_1_1
                , responseHeaders  = [("content-type", "text/plain")]
                , responseBody     = BodyBytes "ok\n"
                , responseTrailers = pure []
                }
        }
```

Send a single client request (the connection is opened, used, and closed):

```haskell
import           Network.HTTP1.Client
import           Network.HTTP1.Types
import           Network.HTTP1.Method (Method (GET))

fetch :: IO (Either ParseError Response)
fetch =
  sendRequest
    defaultClientConfig { clientHost = "127.0.0.1", clientPort = "8080" }
    Request
      { requestMethod   = GET
      , requestTarget   = "/"
      , requestVersion  = HTTP_1_1
      , requestHeaders  = [("host", "127.0.0.1")]
      , requestBody     = BodyEmpty
      , requestTrailers = pure []
      }
```

To reuse connections across requests, check them out of a `Pool`:

```haskell
import Network.HTTP1.Client.Pool

withPool :: Pool -> ClientConfig -> Request -> IO (Either ParseError Response)
withPool pool cfg req =
  withPooledRequest pool cfg req sendRequestOn
```

## Notable modules

| Module | Purpose |
|--------|---------|
| `Network.HTTP1.Types` | `Request` / `Response` / `Body` records and shared types |
| `Network.HTTP1.Method` | `Method` ADT (`GET`, `POST`, … `MethodOther`) |
| `Network.HTTP1.Status` | `Status` newtype and named patterns (`OK`, `NotFound`, …) |
| `Network.HTTP1.Headers` | Header field representation |
| `Network.HTTP1.Parser` | SIMD-accelerated request / response head parsing |
| `Network.HTTP1.StreamingReader` | Magic-ring head + body readers |
| `Network.HTTP1.Encode` | Exact-size response / request encoding |
| `Network.HTTP1.Chunked` | Chunked transfer-coding encode / decode |
| `Network.HTTP1.Connection` | Per-connection state over the ring transport |
| `Network.HTTP1.SendFile` | `sendfile(2)` static-file fast path |
| `Network.HTTP1.Server` | `runServer`, `Handler`, `ServerConfig` |
| `Network.HTTP1.Client` | `openClientConnection`, `sendRequest`, `sendRequestOn` |
| `Network.HTTP1.Client.Pool` | Connection pool (`newPool`, `withPooledRequest`) |
| `Network.HTTP1.TLS` | `tls`-package bridge for HTTP/1 |

## Notes

`serverMaxHeaderBytes` caps the request head size (request line + headers +
the terminating `CRLFCRLF`); oversized heads receive a `431` response. The
server defaults to 32 KiB. `CONNECT` requests are rejected with `405` unless
`serverAllowConnect` is set, since terminating `CONNECT` tunnels on an
origin server is a known SSRF / port-scan vector.
