-- | Per-RPC server handlers, shaped by streaming kind
--
-- For registering complete Protobuf services, see
-- "Network.GRPC.Server.Service" ('Network.GRPC.Server.Service.fromService');
-- the functions here are the per-RPC escape hatch (raw RPCs, custom
-- formats, or fine-grained control).
module Network.GRPC.Server.StreamType (
    -- * Handler type
    ServerHandler'(..)
  , ServerHandler
    -- * Construct server handler
  , mkNonStreaming
  , mkClientStreaming
  , mkServerStreaming
  , mkBiDiStreaming
    -- * Construct 'SomeRpcHandler'
  , fromMethod
  ) where

import Network.GRPC.Util.Imports

import Network.GRPC.Common.StreamElem (StreamElem(..))
import Network.GRPC.Common.NextElem qualified as NextElem
import Network.GRPC.Server

{-------------------------------------------------------------------------------
  Construct server handler

  It may sometimes be useful to use explicit type applications with these
  functions, which is why the @rpc@ type variable is always first.
-------------------------------------------------------------------------------}

mkNonStreaming :: forall rpc m.
     SupportsStreamingType rpc NonStreaming
  => (    Input rpc
       -> m (Output rpc)
     )
  -> ServerHandler' NonStreaming m rpc
mkNonStreaming = ServerHandler

mkClientStreaming :: forall rpc m.
     SupportsStreamingType rpc ClientStreaming
  => (    IO (NextElem (Input rpc))
       -> m (Output rpc)
     )
  -> ServerHandler' ClientStreaming m rpc
mkClientStreaming = ServerHandler

mkServerStreaming :: forall rpc m.
     SupportsStreamingType rpc ServerStreaming
  => (    Input rpc
       -> (NextElem (Output rpc) -> IO ())
       -> m ()
     )
  -> ServerHandler' ServerStreaming m rpc
mkServerStreaming = ServerHandler

mkBiDiStreaming :: forall rpc m.
     SupportsStreamingType rpc BiDiStreaming
  => (    IO (NextElem (Input rpc))
       -> (NextElem (Output rpc) -> IO ())
       -> m ()
     )
  -> ServerHandler' BiDiStreaming m rpc
mkBiDiStreaming = ServerHandler . uncurry

{-------------------------------------------------------------------------------
  Run server handler (used internally only)
-------------------------------------------------------------------------------}

nonStreaming ::
     ServerHandler' NonStreaming m rpc
  -> Input rpc
  -> m (Output rpc)
nonStreaming (ServerHandler h) = h

clientStreaming ::
     ServerHandler' ClientStreaming m rpc
  -> IO (NextElem (Input rpc))
  -> m (Output rpc)
clientStreaming (ServerHandler h) = h

serverStreaming ::
     ServerHandler' ServerStreaming m rpc
  -> Input rpc
  -> (NextElem (Output rpc) -> IO ())
  -> m ()
serverStreaming (ServerHandler h) = h

biDiStreaming ::
    ServerHandler' BiDiStreaming m rpc
 -> IO (NextElem (Input rpc))
 -> (NextElem (Output rpc) -> IO ())
 -> m ()
biDiStreaming (ServerHandler h) = curry h

{-------------------------------------------------------------------------------
  Construct 'RpcHandler'
-------------------------------------------------------------------------------}

class FromStreamingHandler (styp :: StreamingType) where
  -- | Construct 'RpcHandler' from streaming type specific handler
  --
  -- Most applications will probably not need to call this function directly,
  -- instead relying on 'fromMethods'\/'fromServices'. If however you want to
  -- construct a list of 'RpcHandler's manually, without a type-level
  -- specification of the server's API, you can use 'fromStreamingHandler'.
  fromStreamingHandler :: forall k (rpc :: k) m.
        ( SupportsServerRpc rpc
        , Default (ResponseInitialMetadata rpc)
        , Default (ResponseTrailingMetadata rpc)
        , MonadIO m
        )
     => ServerHandler' styp m rpc -> RpcHandler m rpc

instance FromStreamingHandler NonStreaming where
  fromStreamingHandler h = mkRpcHandler $ \call -> do
      inp <- liftIO $ recvFinalInput call
      out <- nonStreaming h inp
      liftIO $ sendFinalOutput call (out, def)

instance FromStreamingHandler ClientStreaming where
  fromStreamingHandler h = mkRpcHandler $ \call -> do
      out <- clientStreaming h (liftIO $ recvNextInputElem call)
      liftIO $ sendFinalOutput call (out, def)

instance FromStreamingHandler ServerStreaming where
  fromStreamingHandler h = mkRpcHandler $ \call -> do
      inp <- liftIO $ recvFinalInput call
      serverStreaming h inp (liftIO . sendOutput call . fromNextElem call)

instance FromStreamingHandler BiDiStreaming where
  fromStreamingHandler h = mkRpcHandler $ \call -> do
      biDiStreaming h
        (liftIO $ recvNextInputElem call)
        (liftIO . sendOutput call . fromNextElem call)

{-------------------------------------------------------------------------------
  Internal: dealing with metadata
-------------------------------------------------------------------------------}

fromNextElem ::
     Default (ResponseTrailingMetadata rpc)
  => proxy rpc
  -> NextElem out
  -> StreamElem (ResponseTrailingMetadata rpc) out
fromNextElem _ = NextElem.toStreamElem def

{-------------------------------------------------------------------------------
  Construct 'SomeRpcHandler'
-------------------------------------------------------------------------------}
-- | Construct 'SomeRpcHandler' from a streaming handler
--
-- Most users will not need to call this function, but it can occassionally be
-- useful when using the lower-level API. Depending on usage you may need to
-- provide a type argument to fix the @rpc@, for example
--
-- > Server.fromMethod @EmptyCall $ ServerHandler $ \(_ ::Empty) ->
-- >   return (defMessage :: Empty)
--
-- If the streaming type cannot be deduced, you might need to specify that also:
--
-- > Server.fromMethod @Ping @NonStreaming $ ServerHandler $ ..
--
-- Alternatively, use one of the handler construction functions, such as
--
-- > Server.fromMethod @Ping $ Server.mkNonStreaming $ ..
fromMethod :: forall rpc styp m.
     ( SupportsServerRpc rpc
     , ValidStreamingType styp
     , Default (ResponseInitialMetadata rpc)
     , Default (ResponseTrailingMetadata rpc)
     , MonadIO m
     )
  => ServerHandler' styp m rpc -> SomeRpcHandler m
fromMethod =
    case validStreamingType (Proxy @styp) of
      SNonStreaming    -> someRpcHandler . fromStreamingHandler
      SClientStreaming -> someRpcHandler . fromStreamingHandler
      SServerStreaming -> someRpcHandler . fromStreamingHandler
      SBiDiStreaming   -> someRpcHandler . fromStreamingHandler

