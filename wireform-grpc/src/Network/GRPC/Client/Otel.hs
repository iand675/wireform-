{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | OpenTelemetry instrumentation for gRPC clients
--
-- Wraps RPC calls in an OTel @Client@ span following the
-- <https://opentelemetry.io/docs/specs/semconv/rpc/grpc/ OTel gRPC semantic conventions>.
-- Built directly on
-- [@hs-opentelemetry-api@](https://hackage.haskell.org/package/hs-opentelemetry-api).
--
-- Typical usage:
--
-- > tp <- OTel.getGlobalTracerProvider
-- > withTracedRPC (grpcTracer tp) conn callParams (Proxy @MyRpc) $ \call -> do
-- >   sendFinalInput call myInput
-- >   fst <$> recvFinalOutput call
--
-- __Trace context propagation:__ the W3C @traceparent@ header cannot be
-- injected into gRPC request headers from outside the core library (the trace
-- context field is set internally by @startRPC@). For full distributed tracing
-- propagation, use an SDK that hooks into the HTTP\/2 layer directly. This
-- helper still opens a client span that parents any nested spans created in
-- the callback.
module Network.GRPC.Client.Otel (
    -- * Traced RPC calls
    withTracedRPC
    -- * Re-exports from "Network.GRPC.Server.Otel"
  , grpcTracer
  , grpcInstrumentationLibrary
  ) where

import Control.Exception (SomeException, throwIO)
import Data.ByteString.Char8 qualified as BS.Char8
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.Proxy (Proxy)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Control.Monad.Catch (MonadMask, catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Stack (HasCallStack)

import OpenTelemetry.Context.ThreadLocal qualified as OCtxTL
import OpenTelemetry.Trace.Core (
    SpanArguments (..)
  , SpanKind (..)
  , SpanStatus (..)
  , Tracer
  , defaultSpanArguments
  )
import OpenTelemetry.Trace.Core qualified as OTel

import Network.GRPC.Client.Call (Call, withRPC)
import Network.GRPC.Client.Connection (Connection)
import Network.GRPC.Server.Otel (grpcTracer, grpcInstrumentationLibrary)
import Network.GRPC.Spec (CallParams, IsRPC(..), SupportsClientRpc)

{-------------------------------------------------------------------------------
  Traced RPC calls
-------------------------------------------------------------------------------}

-- | Like 'withRPC', but wraps the call in an OTel @Client@ span.
--
-- Creates a span named @\{service\}\/\{method\}@ with the standard gRPC
-- semantic convention attributes (@rpc.system@, @rpc.service@, @rpc.method@).
-- On success, sets @rpc.grpc.status_code@ to @0@ (OK). On exception, sets it
-- to @2@ (UNKNOWN), records the exception, sets the span status to 'Error',
-- and re-raises.
--
-- The span is always ended via 'OTel.endSpan', even on exceptions.
withTracedRPC :: forall rpc m a.
     (MonadMask m, MonadIO m, SupportsClientRpc rpc, HasCallStack)
  => Tracer
  -> Connection
  -> CallParams rpc
  -> Proxy rpc
  -> (Call rpc -> m a)
  -> m a
withTracedRPC tracer conn callParams proxy k = do
    let service  = Text.Encoding.decodeUtf8Lenient $ rpcServiceName proxy
        method   = Text.Encoding.decodeUtf8Lenient $ rpcMethodName proxy
        spanName = service <> "/" <> method
        args = defaultSpanArguments
          { kind = Client
          , attributes = HashMap.fromList
              [ ("rpc.system" , OTel.toAttribute @Text "grpc")
              , ("rpc.service", OTel.toAttribute service)
              , ("rpc.method" , OTel.toAttribute method)
              ]
          }

    ctx0 <- liftIO OCtxTL.getContext
    sp   <- liftIO $ OTel.createSpan tracer ctx0 spanName args

    withRPC conn callParams proxy $ \call -> do
      result <- k call `catchM` \(exc :: SomeException) -> liftIO $ do
        OTel.recordException sp mempty Nothing exc
        OTel.addAttribute sp "rpc.grpc.status_code" (2 :: Int64)
        OTel.setStatus sp (Error (textShow exc))
        OTel.endSpan sp Nothing
        throwIO exc

      liftIO $ do
        OTel.addAttribute sp "rpc.grpc.status_code" (0 :: Int64)
        OTel.endSpan sp Nothing

      return result

{-------------------------------------------------------------------------------
  Internal helpers
-------------------------------------------------------------------------------}

-- | 'catch' lifted to 'MonadIO' + 'MonadMask'
catchM ::
     MonadMask m
  => m a -> (SomeException -> m a) -> m a
catchM = catch

textShow :: Show a => a -> Text
textShow = Text.Encoding.decodeUtf8Lenient . BS.Char8.pack . show
