---
title: wireform-websocket
description: "RFC 6455 WebSocket implementation on the wireform parser/builder stack, with TLS, permessage-deflate, and wireform-http upgrade integration."
sidebar:
  order: 55
---

`wireform-websocket` is a native RFC 6455 WebSocket implementation built on
wireform-core's streaming `Wireform.Parser` and `Wireform.Builder`
primitives. The magic-ring transport comes from
[`wireform-network`](../network/) and TLS from
`Wireform.Network.TLS.OpenSSL`, the same backend
[`wireform-http`](../http/) uses, so `wss://` is a one-line opt-in.

The package integrates directly with the `wireform-http` stack: the
server-side handshake parser consumes a unified `Request` and replies with a
`Response`, while the standalone listener (`runWebSocketServer`) accepts both
plain TCP and TLS. Per-connection `acceptWebSocketOnSocket` /
`acceptWebSocketOnTls` hooks let you upgrade a connection from your own
dispatch loop.

## Key features

- **RFC 6455 framing** with a streaming parser and builder, mode-polymorphic
  over `Wireform.Parser`
- **Handshake** (RFC 6455 §4) with SHA-1 + base64 `Sec-WebSocket-Accept` via
  `Wireform.Base64`
- **Reassembled text / binary messages** across continuation frames
- **Standalone TCP / TLS listener** plus per-connection hand-off for
  wireform-http upgrade handlers
- **Client connect** over `ws://` and `wss://`, including connecting
  straight from a URL string
- **Sub-protocol negotiation** on both client and server sides
- **permessage-deflate** (RFC 7692) negotiation and compression via
  `Network.WebSocket.PerMessageDeflate` (backed by `zlib` through
  `cbits/wf_pmd.c`)

## Basic usage

### Server

```haskell
import Network.WebSocket

main :: IO ()
main =
  runWebSocketServer
    defaultWebSocketServerConfig
      { wscPort    = "8443"
      , wscHandler = echo
      , wscTls     = Just WebSocketTlsConfig
          { wstCertPath = "cert.pem"
          , wstKeyPath  = "key.pem"
          , wstAlpn     = []  -- ALPN optional for wss://
          }
      }
  where
    echo _ conn = forEachMessage conn defaultMessageLimit $ \m ->
      case m of
        TextMessage   t  -> sendTextMessage   conn t
        BinaryMessage bs -> sendBinaryMessage conn bs
```

### Client

```haskell
import Network.WebSocket

main :: IO ()
main = do
  let cfg = (defaultWebSocketClientConfig "echo.example" "443" "/echo")
              { wcTls = Just wsTlsDefault }
  withWebSocketClient cfg $ \conn -> do
    sendTextMessage conn "hi"
    TextMessage reply <- receiveMessage conn defaultMessageLimit
    print reply
```

You can also connect straight from a URL string:

```haskell
withWebSocketClientURI "wss://echo.example/echo" $ \conn -> do
  sendTextMessage conn "hi"
  TextMessage reply <- receiveMessage conn defaultMessageLimit
  print reply
```

For sub-protocol negotiation, `withWebSocketClient'` exposes the server's
`ServerHandshakeResult` (selected sub-protocol, extensions, response
headers); the server side mirrors this through `wscSelectSubProtocol`.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Network.WebSocket` | Umbrella re-export |
| `Network.WebSocket.Frame` | Frame ADT, streaming parser, builder |
| `Network.WebSocket.Handshake` | RFC 6455 §4 handshake (SHA-1 + base64) |
| `Network.WebSocket.Connection` | `Connection` over send/receive transports; ping/pong/close |
| `Network.WebSocket.Message` | Reassembled text / binary messages across continuation frames |
| `Network.WebSocket.PerMessageDeflate` | RFC 7692 permessage-deflate negotiation and codec |
| `Network.WebSocket.Server` | Standalone TCP / TLS listener + per-connection hand-off |
| `Network.WebSocket.Client` | `ws://` / `wss://` connect (`withWebSocketClient`, `withWebSocketClient'`) |
| `Network.WebSocket.URI` | `parseWebSocketURI` / `renderWebSocketURI` / `clientConfigFromURI` |

## Conformance

The package ships an Autobahn conformance harness that runs the upstream
`crossbario/autobahn-testsuite` Docker image against the included echo
server (`wireform-websocket/scripts/run-autobahn.sh`). The README's last
full run reports **247 / 247 passed** across sections 1 (framing),
2 (ping/pong), 3 (reserved bits), 4 (opcodes), 5 (fragmentation),
6 (UTF-8 handling), 7 (close handling), and 10 (misc); the `9.*`
performance section is excluded as it is not a conformance check.

## Benchmarks

The README reports end-to-end loopback echo round-trips (single persistent
connection, post-handshake measurement window) against the canonical
Haskell `websockets` package and Rust's `tungstenite-rs`. wireform-websocket
beats `websockets` on every benched shape (8 to 32% faster) and beats
`tungstenite-rs` at large payloads (33% faster on 128 KiB binary), while
`tungstenite-rs` keeps roughly a 2× edge under 16 KiB. Reproduce with:

```
cabal bench wireform-websocket:wireform-websocket-bench \
  --benchmark-options='--time-limit 5 +RTS -N1 -RTS'
```
