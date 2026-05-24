# wireform-http2

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage
> yet. APIs may change.

From-scratch HTTP/2 (RFC 9113) implementation following wireform's
performance philosophy: zero-copy frame encode/decode, SWAR/SIMD-
accelerated HPACK Huffman, exact-size allocation, pinned recv ring
buffer, allocation-free hot loops.

## What's in here

| Module                                  | Role |
| --------------------------------------- | ---- |
| `Network.HTTP2.Frame`                   | RFC 9113 frame layer — DATA, HEADERS, PRIORITY, SETTINGS, PING, GOAWAY, WINDOW_UPDATE, RST_STREAM, CONTINUATION, PUSH_PROMISE. Pattern-synonym ADT + zero-copy decode/encode. |
| `Network.HTTP2.HPACK`                   | RFC 7541 HPACK encode/decode with a hand-tuned C Huffman codec (`cbits/hpack_huffman.c`) and a static + dynamic table. |
| `Network.HTTP2.Connection`              | Per-connection state: settings, flow-control windows, stream table, HPACK encoder/decoder, send lock, magic-ring recv + send transports. Talks to the outside world through a `Transport`. |
| `Network.HTTP2.Transport`               | I/O abstraction the connection layer runs on top of: pointer-based `tSendFn` + `tRecvBuf` + close. `socketTransport` is the default for plain TCP; TLS transports supply matching pointer-based callbacks via OpenSSL. `mkConnection` builds a `SendTransport` (magic ring) on top of `tSendFn`. |
| `Network.HTTP2.Client`                  | Cleartext HTTP/2 client (h2c) over a TCP socket. |
| `Network.HTTP2.Server`                  | Cleartext HTTP/2 server (h2c) over a TCP socket. |
| `Network.HTTP2.TLS`                     | Shared TLS plumbing: the `h2` ALPN identifier, the `ALPNFailed` exception, and `tlsTransport` (wraps a `Network.TLS.Context` as a wireform-http2 `Transport`). |
| `Network.HTTP2.TLS.Client`              | HTTP/2-over-TLS client. Does the TLS handshake against the peer, advertises `h2` via ALPN, and runs the existing `Network.HTTP2.Client` loop over the resulting TLS context. |
| `Network.HTTP2.TLS.Server`              | HTTP/2-over-TLS server. Accepts TLS connections, picks `h2` from the ALPN list (drops connections that ask for anything else), and runs the existing `Network.HTTP2.Server` loop. |

## Cabal flags

| Flag    | Default | Pulls in                                  | Disables when off                                  |
| ------- | ------- | ----------------------------------------- | -------------------------------------------------- |
| `+tls`  | True    | `tls`, `crypton-x509`, `crypton-x509-store` | `Network.HTTP2.TLS.*` modules, TLS test suite |

## Hello world — cleartext

```haskell
import qualified Data.ByteString as BS
import qualified Network.HTTP2.Server as H2

main :: IO ()
main = H2.runServer cfg
  where
    cfg = H2.defaultServerConfig
      { H2.serverHost = "0.0.0.0"
      , H2.serverPort = "8080"
      , H2.serverHandler = \_req respond ->
          respond H2.Response
            { H2.responseStatus = 200
            , H2.responseHeaders = [("content-type", "text/plain")]
            , H2.responseBody = H2.ResponseBodyBS (BS.pack [104,105,10])
            }
      }
```

## Hello world — TLS / h2

```haskell
import qualified Data.X509      as X509
import qualified Data.X509.File as X509
import qualified Network.TLS    as TLS
import qualified Network.HTTP2.TLS.Server as TLS

main :: IO ()
main = do
  certs <- X509.readSignedObject "server.crt"
  key:_ <- X509.readKeyFile "server.key"
  let cfg = (TLS.defaultTLSServerConfig (X509.CertificateChain certs) key)
        { TLS.tlsServerConfig = TLS.defaultServerConfig
            { TLS.serverHost = "0.0.0.0"
            , TLS.serverPort = "8443"
            }
        }
  TLS.runTLSServer cfg
```

The client side is `Network.HTTP2.TLS.Client.withTLSConnection`:

```haskell
import qualified Network.HTTP2.TLS.Client as TLS

main :: IO ()
main = do
  let httpCfg = TLS.defaultClientConfig { TLS.clientHost = "127.0.0.1", TLS.clientPort = "8443" }
      cfg     = (TLS.defaultTLSClientConfig "localhost") { TLS.tlsClientConfig = httpCfg }
  TLS.withTLSConnection cfg $ \conn -> do
    _ <- TLS.sendRequest conn $ TLS.ClientRequest
            { TLS.crMethod    = "GET"
            , TLS.crPath      = "/"
            , TLS.crScheme    = "https"
            , TLS.crAuthority = "localhost"
            , TLS.crHeaders   = []
            , TLS.crBody      = Nothing
            }
    pure ()
```

`ALPNFailed` is thrown if the peer refuses to negotiate `h2`. ALPN is
non-optional: there is intentionally no HTTP/1.1 fallback in this
package. Plug a different transport adapter on top of
`Network.HTTP2.Transport.Transport` if you want a Connect-style
upgrade or h2c-over-TLS.

## Server push

Server push (`PUSH_PROMISE`) is parsed and the frame type is exposed,
but the high-level server API does not initiate pushes. Server push
was de facto removed from the web platform (Chrome 106 / Firefox 96
disabled it; the gRPC profile never used it). We do not plan to add a
push API to `Network.HTTP2.Server` unless a concrete user need turns up.

## Relationship with `wireform-grpc`

[`wireform-grpc`](../wireform-grpc/) drives its HTTP/2 transport through
the `Network.HTTP2.Engine.*` modules in this package; it no longer
depends on the upstream `http2`, `http2-tls`, or `http-semantics`
packages.

The engine modules expose an `http-semantics`-shaped API (Request,
Response, OutBodyIface, TrailersMaker, NextTrailersMaker, InpObj,
OutObj, Aux) so the wireform-grpc consumer code is essentially
unchanged from the upstream `grapesy` shape — only the import paths
move:

| Old import (`http2` / `http-semantics` / `http2-tls`) | New import (this package)                |
|------------------------------------------------------|------------------------------------------|
| `Network.HTTP.Semantics`                              | `Network.HTTP2.Engine.Types`             |
| `Network.HTTP.Semantics.Server`                       | `Network.HTTP2.Engine.Server`            |
| `Network.HTTP.Semantics.Client`                       | `Network.HTTP2.Engine.Client`            |
| `Network.HTTP2.Server`                                | `Network.HTTP2.Engine.Server`            |
| `Network.HTTP2.Client`                                | `Network.HTTP2.Engine.Client`            |
| `Network.HTTP2.TLS.Server`                            | `Network.HTTP2.Engine.TLS.Server`        |
| `Network.HTTP2.TLS.Client`                            | `Network.HTTP2.Engine.TLS.Client`        |
| `Network.HPACK` (for `BufferSize`)                    | `Network.HTTP2.Engine.Types`             |

The runtime under those modules
(`Network.HTTP2.Engine.Run.{Server,Client}`) handles the gRPC happy
path: HEADERS + DATA + trailing HEADERS, half-close in both
directions, OutBodyIface push/pushFinal/cancel/flush, automatic
WINDOW_UPDATE bookkeeping, RST_STREAM cancellation, PING / SETTINGS
acks. Things gRPC doesn't use (PUSH_PROMISE, responseFile, the
deprecated `numberOfWorkers` thread pool) are intentionally absent.

## Tests

```
cabal test wireform-http2:wireform-http2-test
```

The `Test.TLS` group spins up `runTLSServerOnSocket` on a freshly
bound random port, runs `withTLSConnection` against it, and asserts
that ALPN agreed on `h2`. It uses a precomputed self-signed cert from
`test/data/` so the test is hermetic and never touches the system
trust store.

`cabal test wireform-http2:wireform-http2-conformance` runs the
`h2spec` conformance suite against the cleartext server (requires the
`h2spec` binary on `PATH`).

## License

BSD-3-Clause. Copyright Ian Duncan, 2026.
