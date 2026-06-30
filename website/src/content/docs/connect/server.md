---
title: Serving Connect RPCs
description: "Build Connect handlers for unary and all three streaming kinds, read and stage metadata via ConnectServerM, configure compression, surface errors, and wire the application into a wireform-http server."
sidebar:
  order: 3
  label: Serving
---

A Connect server is a single HTTP handler — `connectApplication` — built from a
list of type-erased `MethodHandler`s. You don't construct `MethodHandler`s
directly; you use one of four builder functions, each named for the streaming
kind it accepts. The streaming kind is fixed by the method's service tag, so you
choose the builder and pin the method with a type application.

Handlers run in `ConnectServerM` — `ReaderT ServerContext IO` — which gives them
the request's leading metadata and mutable cells to stage response metadata.

## The four handler builders

```haskell
import Network.Connect.Server
import Network.GRPC.Spec (Proto (..))
import Eliza

handlers =
  [ mkNonStreaming    @Say       say        -- Input -> ConnectServerM Output
  , mkClientStreaming @Aggregate aggregate  -- recv -> ConnectServerM Output
  , mkServerStreaming @Introduce introduce  -- Input -> send -> ConnectServerM ()
  , mkBiDiStreaming   @Converse  converse   -- recv -> send -> ConnectServerM ()
  ]
```

| Builder | Argument shape | Connect method |
|---|---|---|
| `mkNonStreaming` | `Input rpc -> ConnectServerM (Output rpc)` | unary |
| `mkClientStreaming` | `ConnectServerM (Maybe (Input rpc)) -> ConnectServerM (Output rpc)` | client-streaming |
| `mkServerStreaming` | `Input rpc -> (Output rpc -> ConnectServerM ()) -> ConnectServerM ()` | server-streaming |
| `mkBiDiStreaming` | `ConnectServerM (Maybe (Input rpc)) -> (Output rpc -> ConnectServerM ()) -> ConnectServerM ()` | bidirectional |

For the streaming builders, `recv :: ConnectServerM (Maybe (Input rpc))` yields
the next request message, or `Nothing` at end-of-stream; `send :: Output rpc ->
ConnectServerM ()` emits a response message. The builders wrap your function
into a uniform `recv -> send -> ConnectServerM ()` shape so one runner drives
them all.

A unary handler serves **both** the unary `POST` and (for side-effect-free
methods) the unary `GET` request shapes from the same builder — no separate
handler for GET.

### Unary

```haskell
say (Proto req) = pure (Proto defaultSayResponse
  { sayResponseSentence = "Hello, " <> sayRequestSentence req })
```

### Server streaming

Read the single request, then call `send` once per response message:

```haskell
introduce (Proto req) send = do
  mapM_ send
    [ Proto defaultIntroduceResponse { introduceResponseSentence = line }
    | line <- greetingLines (introduceRequestName req)
    ]
```

### Client streaming

Receive until `Nothing`, then return the single response:

```haskell
aggregate recv = do
  sentences <- foldRecv recv [] (:)        -- collect until end-of-stream
  pure (Proto defaultSayResponse
    { sayResponseSentence = unwords (reverse sentences) })
```

### Bidirectional streaming

Call `recv` and `send` any number of times, in any order:

```haskell
converse recv send = loop
  where
    loop = do
      mIn <- recv
      case mIn of
        Nothing         -> pure ()
        Just (Proto rq) -> do
          send (Proto defaultConverseResponse
            { converseResponseSentence = replyFor (converseRequestSentence rq) })
          loop
```

## Metadata

`ServerContext` carries the request's leading metadata and mutable cells for the
response's leading and trailing metadata. The accessors run in `ConnectServerM`:

```haskell
getRequestMetadata    :: ConnectServerM [CustomMetadata]
setResponseMetadata   :: [CustomMetadata] -> ConnectServerM ()
addResponseTrailers   :: [CustomMetadata] -> ConnectServerM ()
```

- `getRequestMetadata` is the client-sent leading metadata (custom headers).
- `setResponseMetadata` **replaces** the response leading metadata (sent as
  response headers).
- `addResponseTrailers` **appends** trailing metadata. On a unary call these
  become `trailer-`-prefixed headers; on a streaming call they ride the final
  `EndStreamResponse` frame. The distinction is handled for you — just call
  `addResponseTrailers` from either handler kind.

```haskell
say (Proto req) = do
  leading <- getRequestMetadata
  addResponseTrailers [CustomMetadata (AsciiHeader "x-elapsed") "12ms"]
  pure (Proto defaultSayResponse { ... })
```

Connect's metadata rules (ASCII values verbatim, `-bin` keys carry base64) are
applied at the header boundary by `Network.Connect.Metadata`; reserved header
names are filtered out automatically.

## Errors

Throw a `ConnectException` from any handler and the runtime renders it to the
wire — the right HTTP status for the code, a JSON `Error` body on unary, or an
`EndStreamResponse` carrying the error on the final streaming frame:

```haskell
import Network.Connect.Error (throwConnect)

chargeCard amt
  | amt <= 0   = throwConnect GrpcInvalidArgument "amount must be positive"
  | otherwise  = ...
```

`throwConnect :: ConnectCode -> Text -> IO a` takes one of gRPC's sixteen status
codes (re-exported as `ConnectCode`) and a message. The server also catches
arbitrary `SomeException`s; by default their messages are kept opaque to the
client, and the failure is reported as `internal`. To surface a client-safe
message for a known exception type, set
`cscExceptionToClient :: SomeException -> Maybe Text` on the server config.

## Configuration

`ConnectServerConfig` is server-wide:

```haskell
data ConnectServerConfig = ConnectServerConfig
  { cscSupportedCompression :: [ContentCoding]   -- accepted codings
  , cscExceptionToClient    :: SomeException -> Maybe Text
  }
```

`defaultConnectServerConfig` accepts `identity` and `gzip` and keeps
uncaught-exception messages opaque. The server negotiates the first
client-preferred coding (from `connect-accept-encoding`) that it supports; an
unsupported requested coding yields an `unimplemented` error listing what is
supported.

## Wiring it in

`runConnectServer` is the convenience entry point: it builds the application and
hands it to a wireform-http `ServerConfig`:

```haskell
import Network.Connect.Server
import Network.HTTP.Server (defaultServerConfig, ServerConfig (..))
import Network.HTTP.VersionRange (preferHttp20)

main :: IO ()
main = runConnectServer defaultConnectServerConfig serverCfg handlers
  where
    serverCfg = defaultServerConfig
      { serverPort = "8080", serverVersionRange = preferHttp20 }
```

If you already run a wireform-http server and want Connect to be one route among
many, use `connectApplication` directly — it returns the `Request -> IO
Response` handler you can compose into your own dispatch:

```haskell
connectApplication :: ConnectServerConfig -> [MethodHandler] -> Handler
```

`serverVersionRange` controls the wire version: `preferHttp20` negotiates HTTP/2
over TLS (with ALPN) and HTTP/1.1 on plaintext, so one server speaks both —
which matters for Connect, whose whole point is plain-HTTP reachability.

## Where to next

- [Calling Connect RPCs](../client/) — the client side of every shape above
- [Wire protocol](../protocol/) — how the frames, headers, and errors look on the wire
