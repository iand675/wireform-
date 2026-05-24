---
title: wireform-http
description: "Unified HTTP client and server with version negotiation across HTTP/1.x and HTTP/2, plus dedicated wireform-http1 and wireform-http2 packages."
sidebar:
  order: 51
---

`wireform-http` is a unified HTTP client and server stack for Haskell. Modern
services speak both HTTP/1.1 and HTTP/2 depending on the peer and whether
TLS with ALPN is in play. Use this package when you want one request API
that negotiates the wire version, backed by purpose-built HTTP/1 and HTTP/2
implementations tuned for wireform's performance goals.

The stack spans three packages:

- **`wireform-http`**: unified client and server with version negotiation
- **`wireform-http1`**: RFC 9112 HTTP/1.x with SIMD header scanning and
  connection pooling
- **`wireform-http2`**: RFC 9113 HTTP/2 with redesigned concurrency, flow
  control, and HPACK

## Key features

- **Version negotiation** across HTTP/1.x and HTTP/2 from a single client API
- **TLS with ALPN** to select the on-wire protocol on secure connections
- **HTTP/1 connection pooling** and SIMD-accelerated header parsing
- **HTTP/2 multiplexing** with exact-size allocation and zero-copy frame
  reads off a [magic ring transport](/packages/network/)
- **Magic-ring receive path** (shared with `wireform-kafka` via
  [`wireform-network`](/packages/network/)) — recv bytes land directly
  in a double-mapped pinned buffer; the parser reads zero-copy slices.
  15-51% faster end-to-end than the classic `RecvBuffer` shape; see the
  [benchmarks](/packages/network/#benchmarks-faster-than-the-classic-recv-path)
- **HPACK** with a hand-tuned C Huffman codec
- **Direct OpenSSL TLS** (`Wireform.Network.TLS.OpenSSL`) — decrypts
  plaintext straight into the magic ring with no `ByteString`
  intermediate; the pure-Haskell `tls` bridge stays available for
  deployments that can't add a `libssl` system dep
- **Shared message types** (`Network.HTTP.Message`) across both versions

## Basic usage

Send a request through the unified client. The negotiated version is chosen
from the `VersionRange` in the client config:

```haskell
import           Network.HTTP
import           Network.HTTP.Client
import           Network.HTTP.Message
import qualified Network.HTTP.Types.Body as Body

fetchIndex :: IO ()
fetchIndex =
  withClient defaultClientConfig $ \client -> do
    resp <-
      sendRequest client $
        Request
          { requestMethod    = GET
          , requestTarget    = "/"
          , requestAuthority = Just "example.com"
          , requestScheme    = SchemeHttps
          , requestHeaders   = [("accept", "text/html")]
          , requestBody      = Body.BodyEmpty
          , requestVersion   = HTTP2
          , requestTrailers  = pure []
          }
    putStrLn $
      "status="
        ++ show (responseStatus resp)
        ++ " version="
        ++ show (clientNegotiatedVersion client)
```

Prefer HTTP/2 on TLS by setting the version range and TLS config:

```haskell
import           Network.HTTP.Client
import           Network.HTTP.VersionRange

tlsClientConfig :: ClientConfig
tlsClientConfig =
  defaultClientConfig
    { clientHost         = "example.com"
    , clientPort         = "443"
    , clientVersionRange = preferHttp2
    , clientTls          = Just (defaultTlsClientConfig "example.com")
    }
```

Run a simple HTTP/2 server directly through `wireform-http2` when you do
not need version negotiation:

```haskell
import qualified Data.ByteString       as BS
import qualified Network.HTTP2.Server  as H2

main :: IO ()
main =
  H2.runServer cfg
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
                , H2.responseBody  = H2.ResponseBodyBS (BS.pack "ok\n")
                }
```

## Notable modules

| Package | Module | Purpose |
|---------|--------|---------|
| `wireform-http` | `Network.HTTP.Client` | `withClient`, `sendRequest`, version negotiation |
| `wireform-http` | `Network.HTTP.Server` | Unified server entry point |
| `wireform-http` | `Network.HTTP.VersionRange` | `preferHttp1`, `preferHttp2`, `http2Only` |
| `wireform-http1` | `Network.HTTP1.Client` | HTTP/1.x client with connection pooling |
| `wireform-http1` | `Network.HTTP1.Parser` | SIMD-accelerated request/response parsing |
| `wireform-http1` | `Network.HTTP1.StreamingReader` | Magic-ring head + body readers (SIMD CRLFCRLF scanner on the ring + classic `parseRequest`) |
| `wireform-http1` | `Network.HTTP1.TLS` | `tls`-package bridge for HTTP/1 (legacy path); use [`Wireform.Network.TLS.OpenSSL`](/packages/network/#tls-on-ring-via-openssl) for the ring-direct path |
| `wireform-http2` | `Network.HTTP2.Connection` | Flow control, stream table, HPACK state, magic-ring recv + send |
| `wireform-http2` | `Network.HTTP2.Frame` | Zero-copy frame encode/decode |
| `wireform-http2` | `Network.HTTP2.Frame.StreamingReader` | Magic-ring frame reader (9-byte header + payload slice, single `transportLoadHead` per frame) |
| `wireform-http2` | `Network.HTTP2.HPACK` | Header compression with C Huffman codec |
| `wireform-http2` | `Network.HTTP2.TLS` | `tls`-package bridge for HTTP/2 (legacy path) |

## Transport matrix

On plaintext connections, mixed version ranges fall back to the preferred
protocol. For mixed HTTP/1 and HTTP/2 over a single port, use TLS with
ALPN rather than the deprecated h2c Upgrade dance.
