{-# LANGUAGE DuplicateRecordFields #-}

{- | The high-level wireform HTTP client.

This is an umbrella module: it re-exports the user-facing pieces of
the @Network.HTTP.Client.*@ tree so most callers only need one
import. The split modules are still available for callers who want
to be precise about their import surface (for example, test code
that needs the mock helpers).

A request is a 'Request' value with a phantom body type. The body
type is converted via 'Body' at the wire boundary. Responses come
back through 'Transport' as 'RawResponse's and are decoded via
'ResponseDecoder's. Middleware is just @'Transport' m -> 'Transport' m@.

@
import Network.HTTP.Client

main :: IO ()
main = withClient defaultClientConfig \\transport -\> do
  user <- send transport
    (get [uri|https:\/\/api.example.com\/users\/{id}|]
       \`bindVar` ("id" :: Text) `42`)
    (as @JSON @User)
  print (responseBody user)
@
-}
module Network.HTTP.Client (
  -- * Body and streaming
  module Network.HTTP.Client.BodyStream,
  module Network.HTTP.Client.Body,

  -- * Requests and responses
  Request (Request, method, requestURI, body, protocolHints, spanAttributes),
  request,
  get,
  post,
  put,
  delete,
  patch,
  options,
  head_,
  setMethod,
  setHeader,
  addHeader,
  removeHeader,
  addRequestHeader,
  withRequestTrailers,
  withoutRequestTrailers,
  RequestTrailerError (..),
  withBody,
  withRawBody,
  addSpanAttribute,
  addProtocolHint,
  mapBody,
  SpanAttribute (..),
  RawResponse (RawResponse, statusCode, bodyPopper, protocolInfo),
  rawResponseBytes,
  rawResponseTrailers,
  Response (Response, responseStatus, responseHeaders, responseBody, responseProtocolInfo),
  responseTrailers,
  mapResponse,

  -- * URIs and base URLs
  module Network.HTTP.Client.URI,

  -- * Content type system
  module Network.HTTP.Client.Media,
  module Network.HTTP.Client.Media.JSON,
  module Network.HTTP.Client.Media.OctetStream,
  module Network.HTTP.Client.Media.PlainText,
  module Network.HTTP.Client.Media.FormUrlEncoded,
  module Network.HTTP.Client.Decoder,

  -- * Transports and middleware
  Transport (..),
  unsafeMkTransport,
  noMiddleware,
  module Network.HTTP.Client.Middleware,

  -- * Compression
  module Network.HTTP.Client.Compression,

  -- * Send
  module Network.HTTP.Client.Send,

  -- * Server-Sent Events
  module Network.HTTP.Client.SSE,

  -- * Cookies
  module Network.HTTP.Client.Cookies,

  -- * Client wiring
  module Network.HTTP.Client.Config,
  module Network.HTTP.Client.Base,
  module Network.HTTP.Client.Pool,
  module Network.HTTP.Client.Streaming,

  -- * Protocol metadata
  module Network.HTTP.Client.Protocol,

  -- * Tracing (OpenTelemetry)
  module Network.HTTP.Client.Tracing,

  -- * Test helpers (mock transports, stubs, request log, assertions)
  module Network.HTTP.Client.Test,

  -- * VCR (record/replay)
  module Network.HTTP.Client.VCR,
) where

import Network.HTTP.Client.Base
import Network.HTTP.Client.Body
import Network.HTTP.Client.BodyStream
import Network.HTTP.Client.Compression
import Network.HTTP.Client.Config
import Network.HTTP.Client.Cookies
import Network.HTTP.Client.Decoder
import Network.HTTP.Client.Media
import Network.HTTP.Client.Media.FormUrlEncoded
import Network.HTTP.Client.Media.JSON
import Network.HTTP.Client.Media.OctetStream
import Network.HTTP.Client.Media.PlainText
import Network.HTTP.Client.Middleware
import Network.HTTP.Client.Pool
import Network.HTTP.Client.Protocol
import Network.HTTP.Client.Request
import Network.HTTP.Client.Response
import Network.HTTP.Client.SSE
import Network.HTTP.Client.Send
import Network.HTTP.Client.Streaming
import Network.HTTP.Client.Test
import Network.HTTP.Client.Tracing
import Network.HTTP.Client.Transport
import Network.HTTP.Client.URI
import Network.HTTP.Client.VCR hiding (
  RecordedRequest (..),
  RecordedResponse (..),
 )

