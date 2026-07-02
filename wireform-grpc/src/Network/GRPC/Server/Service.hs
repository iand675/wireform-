-- | Serve a transport-agnostic 'Service' implementation over gRPC.
--
-- This is the recommended way to register handlers for Protobuf services
-- whose tags come from 'Network.GRPC.Protobuf.TH.loadProtoServices':
--
-- > import Network.GRPC.Server.Service
-- >
-- > eliza :: MonadIO m => Service ElizaService m
-- > eliza =
-- >   service
-- >     (  method @Say       sayH
-- >     :& method @Introduce introduceH
-- >     :& method @Aggregate aggregateH
-- >     :& method @Converse  converseH
-- >     :& Done
-- >     )
-- >
-- > main :: IO ()
-- > main = runServerWithHandlers def config (fromService eliza)
--
-- The same 'Service' value can be served over Connect
-- (@connectHandlers@ in @wireform-connect@).
--
-- Registration is order-insensitive and completeness-checked: a missing,
-- duplicated, or foreign method is a compile-time error naming the method.
-- Methods the server deliberately does not support are declared with
-- 'methodUnimplemented' (the server then responds with @UNIMPLEMENTED@).
--
-- For RPCs outside the @Protobuf serv meth@ scheme (raw or custom-format
-- RPCs), or when you need the full power of the 'Network.GRPC.Server.Call'
-- API for a single method, drop down to 'fromMethod' /
-- 'Network.GRPC.Server.someRpcHandler' and concatenate the results.
module Network.GRPC.Server.Service (
    -- * Service implementation vocabulary (re-exported from grpc-spec)
    ServiceMethods
  , HandlerOf
  , MethodOf(..)
  , method
  , methodUnimplemented
  , Handlers(..)
  , Service(..)
  , service
  , CompleteService(..)
  , PluckMethod(..)
    -- * Serving over gRPC
  , fromService
  , FromServiceMethods -- opaque
  ) where

import Network.GRPC.Util.Imports

import Network.GRPC.Server.Handler (SomeRpcHandler)
import Network.GRPC.Server.StreamType
import Network.GRPC.Spec

{-------------------------------------------------------------------------------
  Serving over gRPC
-------------------------------------------------------------------------------}

-- | Handler list for a complete 'Service', for 'Network.GRPC.Server.mkGrpcServer'
-- or 'Network.GRPC.Server.Run.runServerWithHandlers'.
--
-- Multiple services concatenate:
--
-- > mkGrpcServer params (fromService greeter <> fromService routeGuide)
fromService :: forall serv m.
     (MonadIO m, FromServiceMethods serv (ServiceMethods serv))
  => Service serv m -> [SomeRpcHandler m]
fromService (Service hs) = fromServiceMethods hs

-- | Internal: fold a canonical 'Handlers' list into 'SomeRpcHandler's.
class FromServiceMethods (serv :: Type) (meths :: [Symbol]) where
  fromServiceMethods :: MonadIO m => Handlers serv meths m -> [SomeRpcHandler m]

instance FromServiceMethods serv '[] where
  fromServiceMethods Done = []

instance
     ( SupportsServerRpc (Protobuf serv meth)
     , Default (ResponseInitialMetadata (Protobuf serv meth))
     , Default (ResponseTrailingMetadata (Protobuf serv meth))
     , FromServiceMethods serv meths
     )
  => FromServiceMethods serv (meth ': meths) where
  fromServiceMethods (m :& rest) =
      toGrapesy @serv @meth m : fromServiceMethods rest

toGrapesy :: forall serv meth m.
     ( SupportsServerRpc (Protobuf serv meth)
     , Default (ResponseInitialMetadata (Protobuf serv meth))
     , Default (ResponseTrailingMetadata (Protobuf serv meth))
     , MonadIO m
     )
  => MethodOf serv meth m -> SomeRpcHandler m
toGrapesy (MethodImpl styp h) = case styp of
    SNonStreaming ->
      fromMethod @(Protobuf serv meth) $ mkNonStreaming h
    SClientStreaming ->
      fromMethod @(Protobuf serv meth) $ mkClientStreaming $ \recv ->
        h (nextElemToMaybe <$> liftIO recv)
    SServerStreaming ->
      fromMethod @(Protobuf serv meth) $ mkServerStreaming $ \inp send -> do
        h inp (liftIO . send . NextElem)
        liftIO $ send NoNextElem
    SBiDiStreaming ->
      fromMethod @(Protobuf serv meth) $ mkBiDiStreaming $ \recv send -> do
        h (nextElemToMaybe <$> liftIO recv) (liftIO . send . NextElem)
        liftIO $ send NoNextElem
toGrapesy MethodUnimplemented =
    fromMethod @(Protobuf serv meth) $
      mkRawUnimplemented @(Protobuf serv meth)

-- | An 'UNIMPLEMENTED'-throwing handler, shaped to the method's streaming
-- kind. The exception is raised as soon as the handler runs; grapesy
-- forwards it to the client as proper trailers.
mkRawUnimplemented :: forall rpc m.
     ( IsRPC rpc
     , HasStreamingType rpc
     , MonadIO m
     )
  => ServerHandler' (RpcStreamingType rpc) m rpc
mkRawUnimplemented = case validStreamingType (Proxy @(RpcStreamingType rpc)) of
    SNonStreaming    -> mkNonStreaming    $ \_      -> throwUnimpl
    SClientStreaming -> mkClientStreaming $ \_      -> throwUnimpl
    SServerStreaming -> mkServerStreaming $ \_ _    -> throwUnimpl
    SBiDiStreaming   -> mkBiDiStreaming   $ \_ _    -> throwUnimpl
  where
    throwUnimpl :: forall a. m a
    throwUnimpl =
        liftIO $ throwIO GrpcException {
            grpcError         = GrpcUnimplemented
          , grpcErrorMessage  = Just $ unimplementedMessage (Proxy @rpc)
          , grpcErrorDetails  = Nothing
          , grpcErrorMetadata = []
          }

nextElemToMaybe :: NextElem a -> Maybe a
nextElemToMaybe NoNextElem   = Nothing
nextElemToMaybe (NextElem a) = Just a
