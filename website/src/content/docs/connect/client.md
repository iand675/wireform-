---
title: Calling Connect RPCs
description: "Open a Connect client, configure codec/compression/metadata/timeout, issue unary (POST and GET) and all three streaming RPCs, handle ConnectException, and use TLS."
sidebar:
  order: 4
  label: Calling
---

A Connect client is a `ConnectClient` — a live wireform-http connection plus
codec/compression settings — built inside `withConnectClient`. Pass it to one of
five call functions; the streaming kind is fixed by the method's service tag, so
each call's type guarantees you're using the right shape.

## Open a client

`withConnectClient` brackets an HTTP connection (plain or TLS; HTTP/1.1 or
HTTP/2 per the connection config) and hands you a `ConnectClient`:

```haskell
import Network.Connect.Client
import Network.HTTP.Client
  (defaultConnectionConfig, ConnectionConfig (..))

withConnectClient defaultConnectClientConfig connCfg $ \cl -> do
  ...
  where connCfg = defaultConnectionConfig
          { connectionHost = "demo.connectrpc.com", connectionPort = "443"
          , connectionTls  = Just (defaultTlsConnectionConfig "demo.connectrpc.com") }
```

`ConnectClientConfig` controls what goes on every call:

```haskell
data ConnectClientConfig = ConnectClientConfig
  { cccCodec              :: Codec          -- CodecProto or CodecJSON
  , cccRequestCompression :: ContentCoding  -- outgoing compression (Identity = none)
  , cccAcceptCompression  :: [ContentCoding]-- advertised via connect-accept-encoding
  , cccTimeoutMs          :: Maybe Int      -- deadline, sent as connect-timeout-ms
  , cccMetadata           :: [CustomMetadata] -- leading metadata on every call
  , cccSendProtocolVersion:: Bool           -- send connect-protocol-version: 1 (default True)
  }
```

`defaultConnectClientConfig` is binary Protobuf, no request compression,
advertising `identity` + `gzip`, no timeout, empty metadata, protocol-version
header on. Switch to JSON (so responses are curl-readable) with
`{ cccCodec = CodecJSON }`.

`ConnectionConfig` and `TlsConnectionConfig` are re-exported from
`Network.HTTP.Client` for convenience — set `connectionTls = Just (...)` for
`https://`, leave it `Nothing` for `http://`. The scheme and `Host`/`:authority`
are derived from the TLS config automatically.

## Unary (POST)

```haskell
nonStreaming :: ConnectClient -> Proxy rpc -> Input rpc -> IO (Output rpc)
```

Encode the input, send the request, return the decoded response. On an error
response it throws `ConnectException`:

```haskell
withConnectClient cfg connCfg $ \cl -> do
  Proto resp <- nonStreaming cl (Proxy @Say)
                  (Proto defaultSayRequest { sayRequestSentence = "Hi" })
  print (sayResponseSentence resp)
```

## Unary (GET)

```haskell
nonStreamingGet :: ConnectClient -> Proxy rpc -> Input rpc -> IO (Output rpc)
```

The same unary call over HTTP `GET`, with the request encoded as query
parameters (`?connect=v1&encoding=…&message=…`). Use it only for
side-effect-free, cacheable methods — the response handling is identical to
`nonStreaming`. Binary (proto) and compressed messages are base64url-unpadded in
the `message` parameter.

## Streaming

The streaming calls hand you **continuations** rather than returning a list, so
you can interleave the stream with whatever your handler needs to do.

**Server streaming** — send the single request, then drain responses through a
`recv` action the call gives you. `recv` yields `Nothing` at end-of-stream; the
stream ends when your continuation returns:

```haskell
serverStreaming :: ConnectClient -> Proxy rpc -> Input rpc
                -> (IO (Maybe (Output rpc)) -> IO r) -> IO r

serverStreaming cl (Proxy @Introduce) (Proto req) $ \recv -> do
  let loop = recv >>= \case
        Nothing      -> pure ()
        Just (Proto o) -> print (introduceResponseSentence o) >> loop
  loop
```

**Client streaming** — your continuation gets a `send` action for request
messages; when it returns, the client half-closes and reads the single response:

```haskell
clientStreaming :: ConnectClient -> Proxy rpc
                -> ((Input rpc -> IO ()) -> IO ()) -> IO (Output rpc)

clientStreaming cl (Proxy @Aggregate) $ \send -> do
  mapM_ (send . Proto . defaultSayRequest) ["one", "two", "three"]
```

Request frames are buffered before the request body is sent, so this works over
HTTP/1.1 (which requires the full body up front) as well as HTTP/2.

**Bidirectional streaming** — your continuation gets both a `send` (requests)
and a `recv` (responses, `Nothing` at end-of-stream); call either any number of
times. The stream ends when the continuation returns:

```haskell
biDiStreaming :: ConnectClient -> Proxy rpc
              -> ((Input rpc -> IO ()) -> IO (Maybe (Output rpc)) -> IO r) -> IO r
```

This requires HTTP/2's full-duplex capability; the client sends the streaming
request body on a forked thread so it can read responses concurrently.

## Errors

A failed call — a non-2xx unary response, an error in the streaming
`EndStreamResponse`, or a decode failure — surfaces as a `ConnectException`
wrapping a `ConnectError`:

```haskell
data ConnectError = ConnectError
  { ceCode    :: ConnectCode      -- one of gRPC's sixteen codes
  , ceMessage :: Maybe Text       -- human-readable, if the server sent one
  , ceDetails :: [ErrorDetail]    -- typed details (message name + base64 payload)
  }
```

Catch it where you want to recover:

```haskell
import Control.Exception (try)

result <- try (nonStreaming cl (Proxy @Say) req) :: IO (Either ConnectException SayResponse)
case result of
  Left e | ceCode (connectError e) == GrpcUnavailable -> retry
         | otherwise                                   -> throwIO e
  Right resp -> pure resp
```

When the response body is missing or unparseable, the client infers a code from
the HTTP status (the spec's inference table) so you still get a meaningful
`ConnectCode`.

## TLS

Set `connectionTls` for `https://`. ALPN negotiates HTTP/2 when the server
supports it and falls back to HTTP/1.1 otherwise — no separate configuration:

```haskell
import Network.HTTP.Client (defaultTlsConnectionConfig)

connCfg = defaultConnectionConfig
  { connectionHost = "demo.connectrpc.com", connectionPort = "443"
  , connectionTls  = Just (defaultTlsConnectionConfig "demo.connectrpc.com") }
```

`defaultTlsConnectionConfig` takes the server hostname (for SNI / certificate
validation); certificate validation is on by default. For mTLS, set
`tlsClientCertificate` on the `TlsConnectionConfig`.

## Where to next

- [Serving Connect RPCs](../server/) — the server side
- [Wire protocol](../protocol/) — the bytes these calls produce
