---
title: wireform-http2
description: "From-scratch HTTP/2 (RFC 9113) server and client with redesigned concurrency, zero-copy frames, and a hand-tuned C HPACK Huffman codec."
sidebar:
  order: 53
---

`wireform-http2` is a from-scratch HTTP/2 (RFC 9113) server and client built
to wireform's performance philosophy: zero-copy frame encode/decode,
SWAR/SIMD-accelerated HPACK Huffman, exact-size allocation, a pinned recv
ring buffer, and allocation-free hot loops. It is the HTTP/2 half of the
unified [`wireform-http`](../http/) stack and is designed to replace the
upstream `http2` + `http-semantics` + `hpack` libraries for wireform
clients and servers.

The concurrency primitives were redesigned from lessons learned optimising
[`wireform-grpc`](../grpc/): `OUnary` output sends HEADERS + DATA + trailers
in one pass, `TxWindow` does connection-level flow control without STM, the
body channel is an SPSC `IORef`/`TBQueue`, and HPACK uses a per-connection
scratch array to avoid per-decode allocation.

## Key features

- **RFC 9113 frame layer**: DATA, HEADERS, PRIORITY, SETTINGS, PING,
  GOAWAY, WINDOW_UPDATE, RST_STREAM, CONTINUATION, PUSH_PROMISE, with a
  pattern-synonym ADT and zero-copy decode/encode
- **RFC 7541 HPACK** with a hand-tuned C Huffman codec
  (`cbits/hpack_huffman.c`) plus static and dynamic tables
- **Magic-ring recv path** (shared with the rest of the stack via
  [`wireform-network`](../network/)) read through a pluggable `Transport`
- **Cleartext (h2c)** client and server, plus **TLS / `h2` over ALPN**
- **Rate limiting** (`Network.HTTP2.RateLimit`) for defensive frame caps
- **Vendored `http-semantics`-shaped engine** (`Network.HTTP2.Engine.*`)
  that `wireform-grpc` drives directly

## Basic usage

Run a cleartext HTTP/2 server:

```haskell
import qualified Data.ByteString      as BS
import qualified Network.HTTP2.Server as H2

main :: IO ()
main = H2.runServer cfg
  where
    cfg =
      H2.defaultServerConfig
        { H2.serverHost    = "0.0.0.0"
        , H2.serverPort    = "8080"
        , H2.serverHandler = \_req respond ->
            respond
              H2.Response
                { H2.responseStatus  = 200
                , H2.responseHeaders = [("content-type", "text/plain")]
                , H2.responseBody    = H2.ResponseBodyBS (BS.pack [111, 107, 10])
                }
        }
```

Serve HTTP/2 over TLS, advertising `h2` via ALPN:

```haskell
import qualified Data.X509      as X509
import qualified Data.X509.File as X509
import qualified Network.HTTP2.TLS.Server as TLS

main :: IO ()
main = do
  certs  <- X509.readSignedObject "server.crt"
  key:_  <- X509.readKeyFile "server.key"
  let cfg = (TLS.defaultTLSServerConfig (X509.CertificateChain certs) key)
        { TLS.tlsServerConfig = TLS.defaultServerConfig
            { TLS.serverHost = "0.0.0.0"
            , TLS.serverPort = "8443"
            }
        }
  TLS.runTLSServer cfg
```

The client connects through `Network.HTTP2.TLS.Client.withTLSConnection`
and sends a `ClientRequest`:

```haskell
import qualified Network.HTTP2.TLS.Client as TLS

main :: IO ()
main = do
  let httpCfg = TLS.defaultClientConfig { TLS.clientHost = "127.0.0.1", TLS.clientPort = "8443" }
      cfg     = (TLS.defaultTLSClientConfig "localhost") { TLS.tlsClientConfig = httpCfg }
  TLS.withTLSConnection cfg $ \conn -> do
    _ <-
      TLS.sendRequest conn
        TLS.ClientRequest
          { TLS.crMethod    = "GET"
          , TLS.crPath      = "/"
          , TLS.crScheme    = "https"
          , TLS.crAuthority = "localhost"
          , TLS.crHeaders   = []
          , TLS.crBody      = Nothing
          }
    pure ()
```

`ALPNFailed` is thrown if the peer refuses to negotiate `h2`; ALPN is
non-optional in the TLS modules, with no HTTP/1.1 fallback.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Network.HTTP2.Frame` | RFC 9113 frame layer; pattern-synonym ADT, zero-copy codec |
| `Network.HTTP2.Frame.StreamingReader` | Magic-ring frame reader (9-byte header + payload slice) |
| `Network.HTTP2.HPACK` | RFC 7541 encode/decode with a C Huffman codec |
| `Network.HTTP2.Connection` | Settings, flow-control windows, stream table, HPACK state |
| `Network.HTTP2.Transport` | I/O abstraction (send-all / send-many / recv-into-Ptr / close) |
| `Network.HTTP2.Client` | Cleartext (h2c) client over TCP |
| `Network.HTTP2.Server` | Cleartext (h2c) server over TCP |
| `Network.HTTP2.TLS` | Shared TLS plumbing: `h2` ALPN id, `ALPNFailed`, `tlsTransport` |
| `Network.HTTP2.TLS.Client` / `.Server` | HTTP/2-over-TLS client / server |
| `Network.HTTP2.RateLimit` | Defensive frame-rate limiting |
| `Network.HTTP2.Types` | Shared request/response and protocol types |
| `Network.HTTP2.Engine.*` | Vendored `http-semantics`-shaped engine used by `wireform-grpc` |

## Server push

`PUSH_PROMISE` is parsed and the frame type is exposed, but the high-level
server API does not initiate pushes: server push was de facto removed from
the web platform (Chrome 106 / Firefox 96 disabled it) and the gRPC profile
never used it.

## Relationship with `wireform-grpc`

[`wireform-grpc`](../grpc/) drives its HTTP/2 transport through the
`Network.HTTP2.Engine.*` modules in this package; it no longer depends on
the upstream `http2`, `http2-tls`, or `http-semantics` packages. The engine
exposes an `http-semantics`-shaped API (Request, Response, OutBodyIface,
TrailersMaker, InpObj, OutObj, Aux) so the gRPC consumer code is essentially
unchanged from the upstream `grapesy` shape; only the import paths move.
