module Network.GRPC.Server (
    -- * Server proper
    mkGrpcServer

    -- ** Configuration
  , ServerParams(..)
  , RequestHandler
  , ContentType(..)

    -- * Handlers
  , Call       -- opaque
  , RpcHandler -- opaque
  , mkRpcHandler
  , mkRpcHandlerNoDefMetadata
  , hoistRpcHandler

    -- ** Hide @rpc@ type variable
  , SomeRpcHandler -- opaque
  , someRpcHandler
  , hoistSomeRpcHandler

    -- * Open (ongoing) call
  , recvInput
  , sendOutput
  , sendGrpcException
  , getRequestMetadata
  , setResponseInitialMetadata

    -- ** Protocol specific wrappers
  , sendNextOutput
  , sendFinalOutput
  , sendTrailers
  , recvNextInputElem
  , recvNextInput
  , recvFinalInput
  , recvEndOfInput

    -- ** Low-level\/specialized API
  , initiateResponse
  , sendTrailersOnly
  , recvInputWithMeta
  , sendOutputWithMeta
  , getRequestHeaders
  , sendProperTrailers

    -- * Exceptions
  , CallSetupFailure(..)
  , ClientDisconnected(..)
  , HandlerTerminated(..)
  , ResponseAlreadyInitiated(..)

    -- * OpenTelemetry
    -- | For OpenTelemetry instrumentation, see "Network.GRPC.Server.Otel".
    -- Use 'Network.GRPC.Server.Otel.otelServerParams' to wrap 'ServerParams'
    -- with tracing middleware.
  ) where

import Network.HTTP2.Engine.Server qualified as Server

import Network.GRPC.Server.Call
import Network.GRPC.Server.Context
import Network.GRPC.Server.Handler
import Network.GRPC.Server.HandlerMap (HandlerMap)
import Network.GRPC.Server.HandlerMap qualified as HandlerMap
import Network.GRPC.Server.RequestHandler
import Network.GRPC.Server.Session (CallSetupFailure(..))
import Network.GRPC.Spec
import Network.GRPC.Util.Stream (ClientDisconnected(..))

{-------------------------------------------------------------------------------
  Server proper
-------------------------------------------------------------------------------}

-- | Construct server
--
-- The server can be run using the standard infrastructure offered by the
-- @http2@ package, but "Network.GRPC.Server.Run" provides some convenience
-- functions.
--
-- If you are using Protobuf, you may wish to use the infrastructure from
-- "Network.GRPC.Server.Service" (in particular,
-- 'Network.GRPC.Server.Service.fromService') to construct the set of
-- handlers.
mkGrpcServer :: ServerParams -> [SomeRpcHandler IO] -> IO Server.Server
mkGrpcServer params@ServerParams{serverTopLevel} handlers = do
    ctxt <- newServerContext params
    return $
        requestHandlerToServer
      $ serverTopLevel
      $ requestHandler handlerMap ctxt
  where
    handlerMap :: HandlerMap IO
    handlerMap = HandlerMap.fromList handlers
