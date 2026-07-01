{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | OpenTelemetry instrumentation for gRPC servers
--
-- Wraps every RPC in an OTel @Server@ span following the
-- <https://opentelemetry.io/docs/specs/semconv/rpc/grpc/ OTel gRPC semantic conventions>.
-- Built directly on
-- [@hs-opentelemetry-api@](https://hackage.haskell.org/package/hs-opentelemetry-api):
-- the tracer is a real 'OTel.Tracer', spans are real 'OTel.Span's, and the
-- propagator configured on the 'OTel.TracerProvider' (W3C Trace Context by
-- default in the SDK) is used to recover the caller's parent context from the
-- request headers.
--
-- Build a tracer once from your provider and thread it into 'otelServerParams':
--
-- > tp <- OTel.getGlobalTracerProvider
-- > let params' = otelServerParams (grpcTracer tp) def
-- > server <- mkGrpcServer params' handlers
--
-- If the provider has no registered span processors (i.e. no SDK has been
-- installed) every span is a no-op, so this is safe to wire in
-- unconditionally.
module Network.GRPC.Server.Otel (
    -- * Tracer
    grpcTracer
  , grpcInstrumentationLibrary
    -- * Server middleware
  , otelServerMiddleware
  , otelServerParams
  ) where

import Control.Exception (SomeException, catch, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS.Char8
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Network.HTTP2.Engine.Types qualified as HTTP.Semantics
import Network.HTTP2.Engine.Server qualified as Server

import OpenTelemetry.Attributes qualified as OAttr
import OpenTelemetry.Context qualified as OCtx
import OpenTelemetry.Context.ThreadLocal qualified as OCtxTL
import OpenTelemetry.Propagator qualified as OProp
import OpenTelemetry.Trace.Core (
    InstrumentationLibrary (..)
  , SpanArguments (..)
  , SpanKind (..)
  , SpanStatus (..)
  , Tracer
  , TracerProvider
  , defaultSpanArguments
  , tracerOptions
  )
import OpenTelemetry.Trace.Core qualified as OTel

#if !MIN_VERSION_hs_opentelemetry_api(1,0,0)
import Data.CaseInsensitive qualified as CI
#endif

import Network.GRPC.Server.Context (ServerParams(..))
import Network.GRPC.Server.RequestHandler.API (RequestHandler)

{-------------------------------------------------------------------------------
  Tracer
-------------------------------------------------------------------------------}

-- | The 'InstrumentationLibrary' record used for the @wireform-grpc@ tracer.
-- Exposed so callers can match against it in custom processor filters /
-- samplers.
grpcInstrumentationLibrary :: InstrumentationLibrary
grpcInstrumentationLibrary =
  InstrumentationLibrary
    { libraryName       = "wireform-grpc"
    , libraryVersion    = "0.1.0.0"
    , librarySchemaUrl  = ""
    , libraryAttributes = OAttr.emptyAttributes
    }

-- | Make a 'Tracer' for the @wireform-grpc@ instrumentation library from the
-- supplied 'TracerProvider'. Pass the result to 'otelServerParams' /
-- 'otelServerMiddleware'.
--
-- If the provider has no registered span processors every span this tracer
-- creates is a no-op, so this call is safe to make unconditionally at
-- service-start time even when no SDK is initialised.
grpcTracer :: TracerProvider -> Tracer
grpcTracer tp = OTel.makeTracer tp grpcInstrumentationLibrary tracerOptions

{-------------------------------------------------------------------------------
  Server middleware
-------------------------------------------------------------------------------}

-- | OTel middleware that wraps every RPC in a @Server@ span following the
-- <https://opentelemetry.io/docs/specs/semconv/rpc/grpc/ gRPC semantic conventions>.
--
-- Span name: @\{service\}\/\{method\}@ (from the request path)
--
-- Attributes set at span start:
--
-- * @rpc.system@   = @\"grpc\"@
-- * @rpc.service@  = the gRPC service name
-- * @rpc.method@   = the gRPC method name
--
-- After the handler completes:
--
-- * @rpc.grpc.status_code@ = @0@ (OK) or @2@ (UNKNOWN) for unhandled errors
-- * on an unhandled exception the span status is set to 'Error' and the
--   exception recorded via 'OTel.recordException'
--
-- The span is always ended (via 'OTel.endSpan'), even on exceptions. A parent
-- context is recovered from the request headers (W3C @traceparent@) through the
-- provider's propagator, so the server span nests under the remote caller's
-- span.
otelServerMiddleware :: Tracer -> RequestHandler a -> RequestHandler a
otelServerMiddleware tracer handler unmask req respond = do
    let (service, method) = parsePathFromRequest req
        spanName = service <> "/" <> method
        args = defaultSpanArguments
          { kind = Server
          , attributes = HashMap.fromList
              [ ("rpc.system" , OTel.toAttribute @Text "grpc")
              , ("rpc.service", OTel.toAttribute service)
              , ("rpc.method" , OTel.toAttribute method)
              ]
          }

    parentCtx <- extractParentContext tracer (extractHeaders req)
    sp        <- OTel.createSpan tracer parentCtx spanName args

    result <- handler unmask req respond `catch` \(exc :: SomeException) -> do
      OTel.recordException sp mempty Nothing exc
      OTel.addAttribute sp "rpc.grpc.status_code" (2 :: Int64)
      OTel.setStatus sp (Error (textShow exc))
      OTel.endSpan sp Nothing
      throwIO exc

    OTel.addAttribute sp "rpc.grpc.status_code" (0 :: Int64)
    OTel.endSpan sp Nothing
    return result

-- | Convenience function to install OTel middleware into 'ServerParams'.
--
-- Composes the OTel middleware with any existing 'serverTopLevel' wrapper,
-- so that the OTel span is the outermost layer.
otelServerParams :: Tracer -> ServerParams -> ServerParams
otelServerParams tracer params = params
  { serverTopLevel = \h -> otelServerMiddleware tracer (serverTopLevel params h)
  }

{-------------------------------------------------------------------------------
  Internal helpers
-------------------------------------------------------------------------------}

-- | Recover a parent 'OCtx.Context' from the incoming request headers via the
-- 'TracerProvider''s configured propagator (W3C Trace Context by default in
-- the SDK). Starts from the current thread-local context so any ambient
-- baggage is preserved when the request carries no @traceparent@.
extractParentContext :: Tracer -> [(ByteString, ByteString)] -> IO OCtx.Context
extractParentContext tracer hdrs = do
  let tp   = OTel.getTracerTracerProvider tracer
      prop = OTel.getTracerProviderPropagators tp
  ctx0 <- OCtxTL.getContext
#if MIN_VERSION_hs_opentelemetry_api(1,0,0)
  -- OTel >= 1.0: the propagator extracts from a 'TextMap' carrier.
  OProp.extract prop (toTextMap hdrs) ctx0
#else
  -- OTel 0.2: the propagator's carrier is the header list itself
  -- ('Propagator Context RequestHeaders ResponseHeaders').
  OProp.extract prop (toRequestHeaders hdrs) ctx0
#endif

#if MIN_VERSION_hs_opentelemetry_api(1,0,0)
toTextMap :: [(ByteString, ByteString)] -> OProp.TextMap
toTextMap =
  OProp.textMapFromList
    . map (\(k, v) -> (Text.Encoding.decodeUtf8Lenient k, Text.Encoding.decodeUtf8Lenient v))
#else
-- OTel 0.2 carrier: 'Network.HTTP.Types.RequestHeaders'
-- (@[(CI ByteString, ByteString)]@). gRPC header names are already ASCII.
toRequestHeaders :: [(ByteString, ByteString)] -> [(CI.CI ByteString, ByteString)]
toRequestHeaders = map (\(k, v) -> (CI.mk k, v))
#endif

-- | Extract the gRPC service and method names from the request path.
--
-- The gRPC path format is @\/{service}\/{method}@. If parsing fails we
-- return placeholder values so the middleware never crashes.
parsePathFromRequest :: Server.Request -> (Text, Text)
parsePathFromRequest req =
    case Server.requestPath req of
      Nothing   -> ("<unknown>", "<unknown>")
      Just path ->
        case BS.Char8.split '/' path of
          ["", service, method] ->
            ( Text.Encoding.decodeUtf8Lenient service
            , Text.Encoding.decodeUtf8Lenient method
            )
          _ -> (Text.Encoding.decodeUtf8Lenient path, "<unknown>")

-- | Pull raw headers from an http-semantics 'Server.Request' as
-- key-value pairs suitable for trace context extraction.
extractHeaders :: Server.Request -> [(ByteString, ByteString)]
extractHeaders req =
    map (\(tok, val) -> (HTTP.Semantics.tokenCIKey tok, val))
      . fst
      $ Server.requestHeaders req

textShow :: Show a => a -> Text
textShow = Text.Encoding.decodeUtf8Lenient . BS.Char8.pack . show
