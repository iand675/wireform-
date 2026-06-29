{-# LANGUAGE OverloadedStrings #-}
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
module Network.Connect.Server (
  -- * Server monad
  ConnectServerM,
  ServerContext (..),
  getRequestMetadata,
  setResponseMetadata,
  addResponseTrailers,

  -- * Handlers
  MethodHandler (..),
  mkNonStreaming,
  mkClientStreaming,
  mkServerStreaming,
  mkBiDiStreaming,

  -- * Configuration
  ConnectServerConfig (..),
  defaultConnectServerConfig,

  -- * Running
  connectApplication,
  runConnectServer,
) where

import Control.Concurrent (forkIO)
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
  ( CustomMetadata
  , GrpcError (..)
  , HasStreamingType (..)
  , Input
  , IsRPC (..)
  , Output
  , StreamingType (..)
  , SupportsServerRpc
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
import Network.HTTP.Types.Status (Status, status200)
import Network.HTTP.Types.Version (pattern HTTP1_1)
import System.Timeout (timeout)

------------------------------------------------------------------------
-- Server monad
------------------------------------------------------------------------

type ConnectServerM = ReaderT ServerContext IO

-- | Per-call server context: the parsed request leading metadata plus
-- mutable cells for response leading and trailing metadata.
data ServerContext = ServerContext
  { scLeadingMetadata :: ![CustomMetadata]
  , scRespLeading :: !(IORef [CustomMetadata])
  , scRespTrailing :: !(IORef [CustomMetadata])
  }

getRequestMetadata :: ConnectServerM [CustomMetadata]
getRequestMetadata = asks scLeadingMetadata

setResponseMetadata :: [CustomMetadata] -> ConnectServerM ()
setResponseMetadata ms = do
  ctx <- ask
  liftIO (writeIORef (scRespLeading ctx) ms)

addResponseTrailers :: [CustomMetadata] -> ConnectServerM ()
addResponseTrailers ms = do
  ctx <- ask
  liftIO (modifyIORef' (scRespTrailing ctx) (<> ms))

------------------------------------------------------------------------
-- Type-erased method handler
------------------------------------------------------------------------

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
-- Handler builders
------------------------------------------------------------------------
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
        Nothing -> pure ()
        Just input -> h input send

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

data ConnectServerConfig = ConnectServerConfig
  { cscSupportedCompression :: ![CC.ContentCoding]
  -- ^ Compression codings the server will accept (default [Identity, Gzip]).
  , cscExceptionToClient :: !(SomeException -> Maybe Text)
  -- ^ Map an uncaught non-Connect exception to a client-visible message.
  -- 'Nothing' (default) keeps the message opaque.
  }

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
    Nothing -> connectErrorResponse (toConnectError GrpcUnimplemented (Just "connect: no such method"))
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
    _ -> case lookupHeader hContentType (requestHeaders req) >>= parseContentType of
      Nothing -> connectErrorResponse unsupportedMediaType
      Just (Unary, codec) -> runUnaryPost p userHandler cfg req codec
      Just (Streaming, _) -> connectErrorResponse unsupportedMediaType

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
  let mReqCoding = lookupHeader hContentEncoding (requestHeaders req) >>= CC.codingFromName
  edecompressed <- decompressUnary cfg bodyBytes mReqCoding
  case edecompressed of
    Left ce -> connectErrorResponse ce
    Right decompressed -> case decodeInputBody codec p decompressed of
      Left e ->
        connectErrorResponse
          (toConnectError GrpcInvalidArgument (Just (T.pack ("connect: could not decode request: " <> e))))
      Right input -> runUnaryHandler p userHandler cfg req codec input

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
          Right input -> runUnaryHandler p userHandler cfg req codec input

runUnaryHandler
  :: (SupportsServerRpc rpc, Aeson.ToJSON (Output rpc))
  => Proxy rpc
  -> (Input rpc -> ConnectServerM (Output rpc))
  -> ConnectServerConfig
  -> Request
  -> Codec
  -> Input rpc
  -> IO Response
runUnaryHandler p userHandler cfg req codec input = do
  let headers = requestHeaders req
      leadingMeta = headersToLeading headers
  leadRef <- newIORef []
  trailRef <- newIORef []
  let ctx =
        ServerContext
          { scLeadingMetadata = leadingMeta
          , scRespLeading = leadRef
          , scRespTrailing = trailRef
          }
      mTimeoutMs = lookupHeader hConnectTimeoutMs headers >>= readInt
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
    Right Nothing ->
      connectErrorResponse (toConnectError GrpcDeadlineExceeded (Just "connect: deadline exceeded"))
    Left (e :: SomeException)
      | Just (ConnectException ce) <- fromException e -> connectErrorResponse ce
      | otherwise -> connectErrorResponse (toConnectError GrpcInternal (cscExceptionToClient cfg e))

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
  case lookupHeader hContentType (requestHeaders req) >>= parseContentType of
    Just (Streaming, codec) -> go codec
    _ ->
      connectErrorResponse
        ( ConnectError
            { ceCode = GrpcUnimplemented
            , ceMessage = Just "connect: streaming requires application/connect+<codec>"
            , ceDetails = []
            }
        )
  where
    go codec = do
      let headers = requestHeaders req
          reqCoding = fromMaybe CC.Identity (lookupHeader hConnectContentEncoding headers >>= CC.codingFromName)
      if reqCoding /= CC.Identity && reqCoding `notElem` cscSupportedCompression cfg
        then connectErrorResponse (unsupportedCodingError cfg)
        else do
          leadRef <- newIORef []
          trailRef <- newIORef []
          let leadingMeta = headersToLeading headers
              ctx =
                ServerContext
                  { scLeadingMetadata = leadingMeta
                  , scRespLeading = leadRef
                  , scRespTrailing = trailRef
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
                  Just (flags, payload) -> do
                    pl <- decodePayload p codec reqCoding (efCompressed flags) payload
                    case pl of
                      Left e -> throwConnectIO (toConnectError GrpcInvalidArgument e)
                      Right x -> pure (Just x)
              send :: Output rpc -> ConnectServerM ()
              send out = liftIO $ do
                let outPayload = encodeOutputBody codec p out
                    (framePayload, compressed) =
                      case rCoding of
                        CC.Identity -> (outPayload, False)
                        c -> (CC.compress c outPayload, True)
                    sflags = EnvelopeFlags{efCompressed = compressed, efEndStream = False}
                atomically (writeTQueue outQ (Just (BL.toStrict (buildFrameLazy sflags framePayload))))
          _ <- forkIO $ do
            result <- try (runReaderT (userHandler recv send) ctx) :: IO (Either SomeException ())
            trail <- readIORef trailRef
            let esr = mkEndStream result trail
                endFrame = encodeEndStream esr
                eflags = EnvelopeFlags{efCompressed = False, efEndStream = True}
            atomically (writeTQueue outQ (Just (BL.toStrict (buildFrameLazy eflags endFrame))))
            atomically (writeTQueue outQ Nothing)
          let producer = atomically (readTQueue outQ)
              baseHeaders = (hContentType, streamContentType codec) : streamingEncodingHdr rCoding
              finalHeaders = baseHeaders <> leadingToHeaders leadingMeta
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
connectErrorResponse ce = do
  let body = BL.toStrict (Aeson.encode (encodeConnectError ce))
      status = connectCodeToHttpStatus (ceCode ce)
      hdrs = [(hContentType, "application/json")]
  mkResponse status hdrs (BodyBytes body)

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

unsupportedMediaType :: ConnectError
unsupportedMediaType =
  ConnectError
    { ceCode = GrpcUnimplemented
    , ceMessage = Just "connect: unsupported media type"
    , ceDetails = []
    }

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
