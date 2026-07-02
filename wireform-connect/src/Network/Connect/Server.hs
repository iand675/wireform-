{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
-- The streaming-kind equality constraints on the @mk*Streaming@ builders
-- (e.g. @RpcStreamingType rpc ~ 'NonStreaming@) are load-bearing API shaping —
-- they reject misuse at call sites — even though their dictionaries are unused
-- in the bodies.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Connect server: an HTTP 'Handler' that dispatches Connect RPCs.
--
-- The server is one @'Request' -> IO 'Response'@ handler that looks up the
-- request path (@\/Service\/Method@) in a dispatch map of type-erased
-- 'MethodHandler's and runs the matching method. Unary (POST + GET) and all
-- three streaming kinds are supported, over HTTP\/1.1 and HTTP\/2.
--
-- = Registering handlers
--
-- Implement the service with the transport-agnostic vocabulary from
-- "Network.GRPC.Spec.Service" (re-exported here), then adapt it with
-- 'connectHandlers':
--
-- > eliza :: Service ElizaService ConnectServerM
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
-- > main = runConnectServer defaultConnectServerConfig serverCfg (connectHandlers eliza)
--
-- Registration is order-insensitive and completeness-checked at compile
-- time; declare deliberately-unsupported methods with 'methodUnimplemented'.
-- The same 'Service' value can be served over gRPC with @wireform-grpc@'s
-- @Network.GRPC.Server.Service.fromService@ (when its handlers are
-- polymorphic in the monad, e.g. @MonadIO m => Service Eliza m@).
module Network.Connect.Server (
  -- * Server monad
  ConnectServerM,
  ServerContext (..),
  getRequestMetadata,
  requestIsGet,
  requestQueryParams,
  requestTimeoutMs,
  setResponseMetadata,
  addResponseTrailers,

  -- * Service implementation vocabulary (re-exported from grpc-spec)
  ServiceMethods,
  HandlerOf,
  MethodOf (..),
  method,
  methodUnimplemented,
  Handlers (..),
  Service (..),
  service,
  CompleteService (..),
  PluckMethod (..),

  -- * Serving over Connect
  MethodHandler, -- opaque
  connectHandlers,
  ConnectMethods, -- opaque class

  -- * Configuration
  ConnectServerConfig (..),
  defaultConnectServerConfig,

  -- * Running
  connectApplication,
  runConnectServer,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.STM
  ( TQueue
  , atomically
  , newTQueueIO
  , readTQueue
  , writeTQueue
  )
import Control.Exception (SomeException, fromException, throwIO, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Data.Aeson qualified as Aeson
import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Network.Connect.Codec (decodeInputBody, encodeOutputBody)
import Network.Connect.Compression qualified as CC
import Network.Connect.Envelope
  ( EndStreamResponse (..)
  , EnvelopeFlags (..)
  , buildFrameLazy
  , encodeEndStream
  , newFrameReader
  , readFrame
  )
import Network.Connect.Error
  ( ConnectError (..)
  , ConnectException (..)
  , connectCodeToHttpStatus
  , encodeConnectError
  , toConnectError
  )
import Network.Connect.Metadata
  ( headersToLeading
  , leadingToHeaders
  , trailingToPrefixedHeaders
  )
import Data.Text.Encoding (decodeUtf8)
import Network.Connect.Protocol
  ( Codec (..)
  , IsStreaming (..)
  , hConnectAcceptEncoding
  , hConnectContentEncoding
  , hConnectTimeoutMs
  , parseContentType
  , qpBase64
  , qpCompression
  , qpEncoding
  , qpMessage
  , streamContentType
  , unaryContentType
  )
import Network.GRPC.Spec
  ( CompleteService (..)
  , CustomMetadata
  , GrpcError (..)
  , HandlerOf
  , Handlers (..)
  , HasStreamingType (..)
  , Input
  , IsRPC (..)
  , MethodOf (..)
  , Output
  , PluckMethod (..)
  , Protobuf
  , SStreamingType (..)
  , Service (..)
  , ServiceMethods
  , StreamingType (..)
  , SupportsServerRpc
  , ValidStreamingType (..)
  , method
  , methodUnimplemented
  , service
  , unimplementedMessage
  )
import Network.HTTP.Message (Request (..), Response (..))
import Network.HTTP.PercentEncoding (decodeQueryString)
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Server (Handler, ServerConfig (..), runServer)
import Network.HTTP.Types.Header
  ( Headers
  , hAcceptEncoding
  , hContentEncoding
  , hContentType
  , lookupHeader
  )
import Network.HTTP.Types.Method qualified as Method
import Network.HTTP.Types.Status (Status, status200, status404, status405, status415)
import Network.HTTP.Types.Version (pattern HTTP1_1)
import System.Timeout (timeout)

------------------------------------------------------------------------
-- Server monad
------------------------------------------------------------------------

-- | The monad Connect server handlers run in: a 'ReaderT' over a per-call
-- 'ServerContext', so a handler can read the request's leading metadata and
-- stage response metadata. Lift 'IO' for effects.
type ConnectServerM = ReaderT ServerContext IO

-- | Per-call server context: the parsed request leading metadata plus
-- mutable cells for response leading and trailing metadata.
data ServerContext = ServerContext
  { scLeadingMetadata :: ![CustomMetadata]
    -- ^ The request's leading metadata (parsed from the HTTP request headers).
  , scRespLeading :: !(IORef [CustomMetadata])
    -- ^ Response leading metadata to send (set by 'setResponseMetadata').
  , scRespTrailing :: !(IORef [CustomMetadata])
    -- ^ Response trailing metadata to send (accumulated by 'addResponseTrailers').
  , scIsGet :: !Bool
    -- ^ Whether the request used HTTP @GET@ (Connect GET). 'False' for POST/streaming.
  , scQueryParams :: ![(ByteString, ByteString)]
    -- ^ Decoded query parameters (Connect GET only; empty otherwise).
  , scTimeoutMs :: !(Maybe Int)
    -- ^ The observed @connect-timeout-ms@ deadline, if the client set one.
  }

-- | The request's leading metadata (the custom headers the client sent).
getRequestMetadata :: ConnectServerM [CustomMetadata]
getRequestMetadata = asks scLeadingMetadata

-- | Whether the request used HTTP @GET@ (Connect GET semantics).
requestIsGet :: ConnectServerM Bool
requestIsGet = asks scIsGet

-- | The request's decoded query parameters (Connect GET only; empty otherwise).
requestQueryParams :: ConnectServerM [(ByteString, ByteString)]
requestQueryParams = asks scQueryParams

-- | The observed @connect-timeout-ms@ deadline, if the client set one.
requestTimeoutMs :: ConnectServerM (Maybe Int)
requestTimeoutMs = asks scTimeoutMs

-- | Replace the response leading metadata (sent as the response headers).
setResponseMetadata :: [CustomMetadata] -> ConnectServerM ()
setResponseMetadata ms = do
  ctx <- ask
  liftIO (writeIORef (scRespLeading ctx) ms)

-- | Append response trailing metadata. On a unary call these become
-- @trailer-@-prefixed headers; on a streaming call they ride the final
-- 'EndStreamResponse' frame.
addResponseTrailers :: [CustomMetadata] -> ConnectServerM ()
addResponseTrailers ms = do
  ctx <- ask
  liftIO (modifyIORef' (scRespTrailing ctx) (<> ms))

------------------------------------------------------------------------
-- Type-erased method handler
------------------------------------------------------------------------

-- | A type-erased Connect method handler: the fully-qualified service\/method
-- names, the streaming kind, and the runner. Produced by 'connectHandlers';
-- opaque outside this module.
data MethodHandler = MethodHandler
  { mhService :: !ByteString
  , mhMethod :: !ByteString
  , mhStream :: !StreamingType
  , mhRun :: ConnectServerConfig -> Request -> IO Response
  }

-- The three streaming builders all wrap their user function into a uniform
-- @recv -> send -> ConnectServerM ()@ shape; a single runner drives them.
type StreamingFn rpc =
  ConnectServerM (Maybe (Input rpc)) -> (Output rpc -> ConnectServerM ()) -> ConnectServerM ()

------------------------------------------------------------------------
-- Serving a transport-agnostic Service over Connect
------------------------------------------------------------------------

-- | Adapt a transport-agnostic 'Service' implementation to Connect: the
-- dispatch list for 'connectApplication' \/ 'runConnectServer'.
--
-- Multiple services concatenate:
--
-- > connectApplication cfg (connectHandlers eliza <> connectHandlers health)
connectHandlers
  :: forall serv
   . ConnectMethods serv (ServiceMethods serv)
  => Service serv ConnectServerM
  -> [MethodHandler]
connectHandlers (Service hs) = connectMethods hs

-- | Internal: fold a canonical 'Handlers' list into 'MethodHandler's.
class ConnectMethods (serv :: Type) (meths :: [Symbol]) where
  connectMethods :: Handlers serv meths ConnectServerM -> [MethodHandler]

instance ConnectMethods serv '[] where
  connectMethods Done = []

instance
  ( SupportsServerRpc (Protobuf serv meth)
  , Aeson.FromJSON (Input (Protobuf serv meth))
  , Aeson.ToJSON (Output (Protobuf serv meth))
  , ConnectMethods serv meths
  )
  => ConnectMethods serv (meth ': meths)
  where
  connectMethods (m :& rest) = toConnect @serv @meth m : connectMethods rest

toConnect
  :: forall serv meth
   . ( SupportsServerRpc (Protobuf serv meth)
     , Aeson.FromJSON (Input (Protobuf serv meth))
     , Aeson.ToJSON (Output (Protobuf serv meth))
     )
  => MethodOf serv meth ConnectServerM
  -> MethodHandler
toConnect (MethodImpl styp h) = case styp of
  SNonStreaming -> mkNonStreaming @(Protobuf serv meth) h
  SClientStreaming -> mkClientStreaming @(Protobuf serv meth) h
  SServerStreaming -> mkServerStreaming @(Protobuf serv meth) h
  SBiDiStreaming -> mkBiDiStreaming @(Protobuf serv meth) h
toConnect MethodUnimplemented =
  case validStreamingType (Proxy @(RpcStreamingType (Protobuf serv meth))) of
    SNonStreaming -> mkNonStreaming @(Protobuf serv meth) (\_ -> throwUnimpl)
    SClientStreaming -> mkClientStreaming @(Protobuf serv meth) (\_ -> throwUnimpl)
    SServerStreaming -> mkServerStreaming @(Protobuf serv meth) (\_ _ -> throwUnimpl)
    SBiDiStreaming -> mkBiDiStreaming @(Protobuf serv meth) (\_ _ -> throwUnimpl)
  where
    throwUnimpl :: forall a. ConnectServerM a
    throwUnimpl =
      liftIO
        ( throwConnectIO
            ( ConnectError
                { ceCode = GrpcUnimplemented
                , ceMessage = Just (unimplementedMessage (Proxy @(Protobuf serv meth)))
                , ceDetails = []
                }
            )
        )

------------------------------------------------------------------------
-- Handler builders (internal)
------------------------------------------------------------------------
-- | Register a unary handler of type @Input -> ConnectServerM Output@.
-- Serves both the unary @POST@ and (for side-effect-free methods) the unary
-- @GET@ request shapes. Apply the method's service tag with a type
-- application, e.g. @mkNonStreaming \@Say say@.
mkNonStreaming
  :: forall (rpc :: Type)
   . ( SupportsServerRpc rpc
     , Aeson.FromJSON (Input rpc)
     , Aeson.ToJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'NonStreaming
     )
  => (Input rpc -> ConnectServerM (Output rpc))
  -> MethodHandler
mkNonStreaming h =
  MethodHandler
    { mhService = rpcServiceName p
    , mhMethod = rpcMethodName p
    , mhStream = NonStreaming
    , mhRun = runUnary @rpc p h
    }
  where
    p = Proxy @rpc

-- | Register a client-streaming handler. The argument receives a @recv@
-- action — 'Nothing' marks end-of-stream — and must produce the single
-- response output.
mkClientStreaming
  :: forall (rpc :: Type)
   . ( SupportsServerRpc rpc
     , Aeson.FromJSON (Input rpc)
     , Aeson.ToJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'ClientStreaming
     )
  => (ConnectServerM (Maybe (Input rpc)) -> ConnectServerM (Output rpc))
  -> MethodHandler
mkClientStreaming h =
  streamingHandler @rpc (Proxy @rpc) ClientStreaming wrap
  where
    wrap recv send = do
      out <- h recv
      send out

-- | Register a server-streaming handler. The argument receives the one
-- request 'Input' and a @send@ continuation, which it calls once per
-- response message.
mkServerStreaming
  :: forall (rpc :: Type)
   . ( SupportsServerRpc rpc
     , Aeson.FromJSON (Input rpc)
     , Aeson.ToJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'ServerStreaming
     )
  => (Input rpc -> (Output rpc -> ConnectServerM ()) -> ConnectServerM ())
  -> MethodHandler
mkServerStreaming h =
  streamingHandler @rpc (Proxy @rpc) ServerStreaming wrap
  where
    wrap recv send = do
      mFirst <- recv
      case mFirst of
        Nothing ->
          liftIO
            ( throwConnectIO
                ( toConnectError
                    GrpcUnimplemented
                    (Just "connect: server-streaming RPC requires exactly one request message; received none")
                )
            )
        Just input -> do
          mNext <- recv
          case mNext of
            Just _ ->
              liftIO
                ( throwConnectIO
                    ( toConnectError
                        GrpcUnimplemented
                        (Just "connect: server-streaming RPC requires exactly one request message; received more than one")
                    )
                )
            Nothing -> h input send

-- | Register a bidirectional-streaming handler. The argument receives a
-- @recv@ action ('Nothing' at end-of-stream) and a @send@ continuation;
-- either may be called any number of times in any order.
mkBiDiStreaming
  :: forall (rpc :: Type)
   . ( SupportsServerRpc rpc
     , Aeson.FromJSON (Input rpc)
     , Aeson.ToJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'BiDiStreaming
     )
  => (ConnectServerM (Maybe (Input rpc)) -> (Output rpc -> ConnectServerM ()) -> ConnectServerM ())
  -> MethodHandler
mkBiDiStreaming = streamingHandler @rpc (Proxy @rpc) BiDiStreaming

streamingHandler
  :: forall (rpc :: Type)
   . ( SupportsServerRpc rpc
     , Aeson.FromJSON (Input rpc)
     , Aeson.ToJSON (Output rpc)
     )
  => Proxy rpc
  -> StreamingType
  -> StreamingFn rpc
  -> MethodHandler
streamingHandler p kind fn =
  MethodHandler
    { mhService = rpcServiceName p
    , mhMethod = rpcMethodName p
    , mhStream = kind
    , mhRun = runStreaming p fn
    }

------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------

-- | Server-wide Connect configuration.
data ConnectServerConfig = ConnectServerConfig
  { cscSupportedCompression :: ![CC.ContentCoding]
  -- ^ Compression codings the server will accept (default [Identity, Gzip]).
  , cscExceptionToClient :: !(SomeException -> Maybe Text)
  -- ^ Map an uncaught non-Connect exception to a client-visible message.
  -- 'Nothing' (default) keeps the message opaque.
  }

-- | Accept @identity@ and @gzip@ compression; keep uncaught-exception
-- messages opaque to the client.
defaultConnectServerConfig :: ConnectServerConfig
defaultConnectServerConfig =
  ConnectServerConfig
    { cscSupportedCompression = [CC.Identity, CC.Gzip]
    , cscExceptionToClient = const Nothing
    }

------------------------------------------------------------------------
-- Dispatch
------------------------------------------------------------------------

-- | Build an HTTP 'Handler' from a Connect config and handler list.
connectApplication :: ConnectServerConfig -> [MethodHandler] -> Handler
connectApplication cfg hs0 req =
  case Map.lookup path hmap of
    Nothing -> bareStatusResponse status404
    Just mh -> mhRun mh cfg req
  where
    hmap = methodMap hs0
    (path, _query) = splitTarget (requestTarget req)

-- | Run a Connect server: wire 'connectApplication' into a wireform-http
-- 'ServerConfig' and call 'runServer'.
runConnectServer :: ConnectServerConfig -> ServerConfig -> [MethodHandler] -> IO ()
runConnectServer cfg serverCfg hs =
  runServer serverCfg{serverHandler = connectApplication cfg hs}

methodMap :: [MethodHandler] -> Map ByteString MethodHandler
methodMap hs = Map.fromList [(key h, h) | h <- hs]
  where
    key h = "/" <> mhService h <> "/" <> mhMethod h

------------------------------------------------------------------------
-- Unary runner
------------------------------------------------------------------------

runUnary
  :: ( SupportsServerRpc rpc
     , Aeson.FromJSON (Input rpc)
     , Aeson.ToJSON (Output rpc)
     )
  => Proxy rpc
  -> (Input rpc -> ConnectServerM (Output rpc))
  -> ConnectServerConfig
  -> Request
  -> IO Response
runUnary p userHandler cfg req =
  case Method.methodToBytes (requestMethod req) of
    "GET" -> runUnaryGet p userHandler cfg req
    "POST" -> case lookupHeader hContentType (requestHeaders req) >>= parseContentType of
      Nothing -> bareStatusResponse status415
      Just (Unary, codec) -> runUnaryPost p userHandler cfg req codec
      Just (Streaming, _) -> bareStatusResponse status415
    _ -> bareStatusResponse status405

runUnaryPost
  :: ( SupportsServerRpc rpc
     , Aeson.FromJSON (Input rpc)
     , Aeson.ToJSON (Output rpc)
     )
  => Proxy rpc
  -> (Input rpc -> ConnectServerM (Output rpc))
  -> ConnectServerConfig
  -> Request
  -> Codec
  -> IO Response
runUnaryPost p userHandler cfg req codec = do
  bodyBytes <- drainBody (requestBody req)
  case resolveReqCoding cfg (lookupHeader hContentEncoding (requestHeaders req)) of
    Left ce -> connectErrorResponse ce
    Right mReqCoding -> do
      edecompressed <- decompressUnary cfg bodyBytes mReqCoding
      case edecompressed of
        Left ce -> connectErrorResponse ce
        Right decompressed -> case decodeInputBody codec p decompressed of
          Left e ->
            connectErrorResponse
              (toConnectError GrpcInvalidArgument (Just (T.pack ("connect: could not decode request: " <> e))))
          Right input -> runUnaryHandler p userHandler cfg req codec False [] input

runUnaryGet
  :: ( SupportsServerRpc rpc
     , Aeson.FromJSON (Input rpc)
     , Aeson.ToJSON (Output rpc)
     )
  => Proxy rpc
  -> (Input rpc -> ConnectServerM (Output rpc))
  -> ConnectServerConfig
  -> Request
  -> IO Response
runUnaryGet p userHandler cfg req = do
  let (_, query) = splitTarget (requestTarget req)
      qs = decodeQueryString query
      codec = fromMaybe CodecJSON (lookupParam qs qpEncoding >>= goCodec)
      isBase64 = lookupParam qs qpBase64 == Just "1"
      mCompression = lookupParam qs qpCompression >>= CC.codingFromName
      msgBytesRaw = fromMaybe BS.empty (lookupParam qs qpMessage)
      eDecoded =
        if isBase64
          then maybe (Left (toConnectError GrpcInvalidArgument (Just "connect: invalid base64 message"))) Right (decodeUrlSafeUnpadded msgBytesRaw)
          else Right msgBytesRaw
  case eDecoded of
    Left ce -> connectErrorResponse ce
    Right decodedMsg -> do
      edecompressed <- decompressUnary cfg decodedMsg mCompression
      case edecompressed of
        Left ce -> connectErrorResponse ce
        Right decompressed -> case decodeInputBody codec p decompressed of
          Left e ->
            connectErrorResponse
              (toConnectError GrpcInvalidArgument (Just (T.pack ("connect: could not decode message: " <> e))))
          Right input -> runUnaryHandler p userHandler cfg req codec True qs input

runUnaryHandler
  :: (SupportsServerRpc rpc, Aeson.ToJSON (Output rpc))
  => Proxy rpc
  -> (Input rpc -> ConnectServerM (Output rpc))
  -> ConnectServerConfig
  -> Request
  -> Codec
  -> Bool
  -> [(ByteString, ByteString)]
  -> Input rpc
  -> IO Response
runUnaryHandler p userHandler cfg req codec isGet qparams input = do
  let headers = requestHeaders req
      leadingMeta = headersToLeading headers
      mTimeoutMs = lookupHeader hConnectTimeoutMs headers >>= readInt
  leadRef <- newIORef []
  trailRef <- newIORef []
  let ctx =
        ServerContext
          { scLeadingMetadata = leadingMeta
          , scRespLeading = leadRef
          , scRespTrailing = trailRef
          , scIsGet = isGet
          , scQueryParams = qparams
          , scTimeoutMs = mTimeoutMs
          }
  outcome <-
    try
      ( case mTimeoutMs of
          Just ms -> timeout (ms * 1000) (runReaderT (userHandler input) ctx)
          Nothing -> Just <$> runReaderT (userHandler input) ctx
      )
  case outcome of
    Right (Just output) -> do
      outLead <- readIORef leadRef
      outTrail <- readIORef trailRef
      let bodyBytes = encodeOutputBody codec p output
          clientAccept = maybe [] CC.parseAcceptEncoding (lookupHeader hAcceptEncoding headers)
          respCoding = CC.negotiate (cscSupportedCompression cfg) clientAccept
          (finalBody, codingHdrs) =
            case respCoding of
              CC.Identity -> (bodyBytes, [])
              c -> (CC.compress c bodyBytes, [(hContentEncoding, CC.codingName c)])
          respHeaders =
            codingHdrs
              <> [(hContentType, unaryContentType codec)]
              <> leadingToHeaders outLead
              <> trailingToPrefixedHeaders outTrail
      mkResponse status200 respHeaders (BodyBytes finalBody)
    Right Nothing -> do
      outLead <- readIORef leadRef
      outTrail <- readIORef trailRef
      connectErrorResponseWith
        (leadingToHeaders outLead <> trailingToPrefixedHeaders outTrail)
        (toConnectError GrpcDeadlineExceeded (Just "connect: deadline exceeded"))
    Left (e :: SomeException) -> do
      outLead <- readIORef leadRef
      outTrail <- readIORef trailRef
      let extraHdrs = leadingToHeaders outLead <> trailingToPrefixedHeaders outTrail
          ce
            | Just (ConnectException c) <- fromException e = c
            | otherwise = toConnectError GrpcInternal (cscExceptionToClient cfg e)
      connectErrorResponseWith extraHdrs ce

-- | Resolve a request's @content-encoding@ \/ @connect-content-encoding@
-- header to a content coding. A missing header or an explicit @identity@
-- means no compression ('Nothing'). Any other value — a coding the server
-- doesn't support, or an unknown token — is rejected as @unimplemented@
-- (the Connect protocol requires servers to reject unsupported request
-- compression rather than silently treat it as identity).
resolveReqCoding
  :: ConnectServerConfig -> Maybe ByteString -> Either ConnectError (Maybe CC.ContentCoding)
resolveReqCoding _ Nothing = Right Nothing
resolveReqCoding cfg (Just raw)
  | raw == "identity" = Right Nothing
  | otherwise = case CC.codingFromName raw of
      Just c | c `elem` cscSupportedCompression cfg -> Right (Just c)
      _ -> Left (unsupportedCodingError cfg)

-- | Decompress a unary request body per its (optional) content-coding,
-- returning @Left@ on an unsupported or undecodable coding.
decompressUnary :: ConnectServerConfig -> ByteString -> Maybe CC.ContentCoding -> IO (Either ConnectError ByteString)
decompressUnary cfg bodyBytes mReqCoding =
  case mReqCoding of
    Nothing -> pure (Right bodyBytes)
    Just CC.Identity -> pure (Right bodyBytes)
    Just c
      | c `notElem` cscSupportedCompression cfg -> pure (Left (unsupportedCodingError cfg))
      | BS.null bodyBytes -> pure (Right bodyBytes)
      | otherwise -> do
          r <- CC.decompress c bodyBytes
          pure $ case r of
            Left _ -> Left (toConnectError GrpcInvalidArgument (Just "connect: could not decompress request body"))
            Right bs -> Right bs

------------------------------------------------------------------------
-- Streaming runner
------------------------------------------------------------------------

runStreaming
  :: forall rpc
   . ( SupportsServerRpc rpc
     , Aeson.FromJSON (Input rpc)
     , Aeson.ToJSON (Output rpc)
     )
  => Proxy rpc
  -> StreamingFn rpc
  -> ConnectServerConfig
  -> Request
  -> IO Response
runStreaming p userHandler cfg req =
  case Method.methodToBytes (requestMethod req) of
    "POST" -> case lookupHeader hContentType (requestHeaders req) >>= parseContentType of
      Just (Streaming, codec) -> go codec
      _ -> bareStatusResponse status415
    _ -> bareStatusResponse status405
  where
    go codec = do
      let headers = requestHeaders req
      case resolveReqCoding cfg (lookupHeader hConnectContentEncoding headers) of
        Left ce -> connectErrorResponse ce
        Right mReqCoding -> do
          let reqCoding = fromMaybe CC.Identity mReqCoding
          leadRef <- newIORef []
          trailRef <- newIORef []
          metaReady <- newEmptyMVar
          let leadingMeta = headersToLeading headers
              mStreamTimeoutMs = lookupHeader hConnectTimeoutMs headers >>= readInt
              ctx =
                ServerContext
                  { scLeadingMetadata = leadingMeta
                  , scRespLeading = leadRef
                  , scRespTrailing = trailRef
                  , scIsGet = False
                  , scQueryParams = []
                  , scTimeoutMs = mStreamTimeoutMs
                  }
              clientAccept = maybe [] CC.parseAcceptEncoding (lookupHeader hConnectAcceptEncoding headers)
              rCoding = CC.negotiate (cscSupportedCompression cfg) clientAccept
          outQ <- newTQueueIO :: IO (TQueue (Maybe ByteString))
          fr <- newFrameReader =<< bodyProducer (requestBody req)
          let recv :: ConnectServerM (Maybe (Input rpc))
              recv = liftIO $ do
                mframe <- readFrame fr
                case mframe of
                  Nothing -> pure Nothing
                  Just (flags, payload)
                    | efCompressed flags && reqCoding == CC.Identity ->
                        throwConnectIO
                          ( toConnectError
                              GrpcInternal
                              (Just "connect: received compressed message but no compression was negotiated")
                          )
                    | otherwise -> do
                        pl <- decodePayload p codec reqCoding (efCompressed flags) payload
                        case pl of
                          Left e -> throwConnectIO (toConnectError GrpcInvalidArgument e)
                          Right x -> pure (Just x)
              send :: Output rpc -> ConnectServerM ()
              send out = liftIO $ do
                _ <- tryPutMVar metaReady ()
                let outPayload = encodeOutputBody codec p out
                    (framePayload, compressed) =
                      case rCoding of
                        CC.Identity -> (outPayload, False)
                        c -> (CC.compress c outPayload, True)
                    sflags = EnvelopeFlags{efCompressed = compressed, efEndStream = False}
                atomically (writeTQueue outQ (Just (BL.toStrict (buildFrameLazy sflags framePayload))))
          _ <- forkIO $ do
            result <- try (runReaderT (userHandler recv send) ctx) :: IO (Either SomeException ())
            _ <- tryPutMVar metaReady ()
            trail <- readIORef trailRef
            let esr = mkEndStream result trail
                endFrame = encodeEndStream esr
                eflags = EnvelopeFlags{efCompressed = False, efEndStream = True}
            atomically (writeTQueue outQ (Just (BL.toStrict (buildFrameLazy eflags endFrame))))
            atomically (writeTQueue outQ Nothing)
          takeMVar metaReady
          outLead <- readIORef leadRef
          let producer = atomically (readTQueue outQ)
              baseHeaders = (hContentType, streamContentType codec) : streamingEncodingHdr rCoding
              finalHeaders = baseHeaders <> leadingToHeaders outLead
          mkResponse status200 finalHeaders (BodyStream producer)
      where
        mkEndStream :: Either SomeException () -> [CustomMetadata] -> EndStreamResponse
        mkEndStream result trail = case result of
          Right () -> EndStreamResponse{esError = Nothing, esMetadata = trail}
          Left e
            | Just (ConnectException ce) <- fromException e ->
                EndStreamResponse{esError = Just ce, esMetadata = trail}
            | otherwise ->
                EndStreamResponse
                  { esError = Just (toConnectError GrpcInternal (cscExceptionToClient cfg e))
                  , esMetadata = trail
                  }

-- | Decode one streaming input frame: optionally decompress, then decode.
decodePayload
  :: (SupportsServerRpc rpc, Aeson.FromJSON (Input rpc))
  => Proxy rpc
  -> Codec
  -> CC.ContentCoding
  -> Bool
  -> ByteString
  -> IO (Either (Maybe Text) (Input rpc))
decodePayload p codec reqCoding compressed payload = do
  pl <-
    if compressed && reqCoding /= CC.Identity
      then CC.decompress reqCoding payload
      else pure (Right payload)
  pure $ case pl of
    Left _ -> Left (Just "connect: could not decompress frame")
    Right bs -> case decodeInputBody codec p bs of
      Left e -> Left (Just (T.pack ("connect: could not decode frame: " <> e)))
      Right x -> Right x

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

splitTarget :: ByteString -> (ByteString, ByteString)
splitTarget t =
  case BS.break (== 0x3F) t of
    (path, q)
      | BS.null q -> (path, BS.empty)
      | otherwise -> (path, BS.drop 1 q)

drainBody :: Body -> IO ByteString
drainBody BodyEmpty = pure BS.empty
drainBody (BodyBytes bs) = pure bs
drainBody (BodyStream p) = go []
  where
    go acc =
      p >>= \case
        Nothing -> pure (BS.concat (reverse acc))
        Just chunk -> go (chunk : acc)

-- | Turn a 'Body' into a stateful chunk producer for the frame reader.
bodyProducer :: Body -> IO (IO (Maybe ByteString))
bodyProducer BodyEmpty = pure (pure Nothing)
bodyProducer (BodyBytes bs) = do
  r <- newIORef False
  pure
      ( do
          done <- readIORef r
          if done
            then pure Nothing
            else do
              writeIORef r True
              pure (Just bs)
      )
bodyProducer (BodyStream p) = pure p

connectErrorResponse :: ConnectError -> IO Response
connectErrorResponse = connectErrorResponseWith []

-- | Like 'connectErrorResponse' but with extra response headers (e.g. the
-- handler's staged leading metadata and @trailer-@-prefixed trailing metadata).
connectErrorResponseWith :: Headers -> ConnectError -> IO Response
connectErrorResponseWith extra ce = do
  let body = BL.toStrict (Aeson.encode (encodeConnectError ce))
      status = connectCodeToHttpStatus (ceCode ce)
      hdrs = (hContentType, "application/json") : extra
  mkResponse status hdrs (BodyBytes body)

-- | A bare HTTP status response with no Connect error envelope, for
-- protocol-level rejections detected before/around the RPC: unsupported
-- media type (415), unknown route (404), unsupported HTTP method (405).
-- The peer infers the RPC error code from the HTTP status (per the Connect
-- protocol's HTTP-to-code mapping), so we deliberately omit a JSON body
-- whose @code@ would otherwise override that inference.
bareStatusResponse :: Status -> IO Response
bareStatusResponse st = mkResponse st [] BodyEmpty

unsupportedCodingError :: ConnectServerConfig -> ConnectError
unsupportedCodingError cfg =
  ConnectError
    { ceCode = GrpcUnimplemented
    , ceMessage =
        Just (T.pack ("connect: unsupported compression; supported: " <> CC.renderCodingList (cscSupportedCompression cfg)))
    , ceDetails = []
    }

streamingEncodingHdr :: CC.ContentCoding -> Headers
streamingEncodingHdr CC.Identity = []
streamingEncodingHdr c = [(hConnectContentEncoding, CC.codingName c)]


lookupParam :: [(ByteString, ByteString)] -> ByteString -> Maybe ByteString
lookupParam qs name = lookup name qs

goCodec :: ByteString -> Maybe Codec
goCodec "proto" = Just CodecProto
goCodec "json" = Just CodecJSON
goCodec _ = Nothing

readInt :: ByteString -> Maybe Int
readInt bs = case reads (T.unpack (decodeUtf8 bs)) of
  [(n, "")] -> Just n
  _ -> Nothing

decodeUrlSafeUnpadded :: ByteString -> Maybe ByteString
decodeUrlSafeUnpadded bs =
  case B64U.decodeUnpadded bs of
    Right x -> Just x
    Left _ -> case B64U.decode (padBs bs) of
      Right x -> Just x
      Left _ -> Nothing
  where
    padBs s =
      let n = BS.length s `mod` 4
       in if n == 0 then s else s <> BS.replicate (4 - n) 0x3D

throwConnectIO :: ConnectError -> IO a
throwConnectIO ce = throwIO (ConnectException ce)

mkResponse :: Status -> Headers -> Body -> IO Response
mkResponse status hdrs body =
  pure
    Response
      { responseStatus = status
      , responseVersion = HTTP1_1
      , responseHeaders = hdrs
      , responseBody = body
      , responseTrailers = pure []
      , responseH2StreamId = 0
      , responseCancel = pure ()
      , responsePushPromises = pure []
      }
