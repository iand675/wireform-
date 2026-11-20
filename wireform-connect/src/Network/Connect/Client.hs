{-# LANGUAGE OverloadedStrings #-}

-- | Connect client: issue Connect RPCs over a wireform-http 'Connection'.
module Network.Connect.Client
  ( -- * Configuration
    ConnectClientConfig (..),
    defaultConnectClientConfig,

    -- * Client
    ConnectClient (..),
    withConnectClient,
    withConnectClientOnTransport,

    -- * Calls
    nonStreaming,
    nonStreamingGet,
    serverStreaming,
    clientStreaming,
    biDiStreaming,

    -- * Resolved (full-outcome) calls
    RpcOutcome (..),
    nonStreamingResolved,
    nonStreamingGetResolved,
    serverStreamingResolved,
    serverStreamingResolvedUpTo,
    clientStreamingResolved,
    biDiStreamingResolved,
    biDiStreamingResolvedUpTo,

    -- * Re-exports
    -- | The HTTP connection a Connect client runs over (re-exported from
    -- "Network.HTTP.Connection").
    Connection.Connection,
    -- | Configuration for opening an HTTP connection
    -- (@connectionHost@, @connectionPort@, @connectionTls@,
    -- @connectionVersionRange@).
    Connection.ConnectionConfig (..),
    -- | The default 'ConnectionConfig' (plain HTTP; set @connectionHost@
    -- and @connectionPort@).
    Connection.defaultConnectionConfig,
    -- | TLS settings for an HTTP connection.
    Connection.TlsConnectionConfig (..),
    -- | A 'TlsConnectionConfig' seeded with the server hostname (passed as
    -- its argument, for SNI).
    Connection.defaultTlsConnectionConfig,
  )
where

import System.Timeout (timeout)
import Control.Concurrent.STM
  ( TQueue
  , atomically
  , newTQueueIO
  , readTQueue
  , writeTQueue
  )
import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Exception (throwIO)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Lazy qualified as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Network.Connect.Codec (decodeOutputBody, encodeInputBody)
import Network.Connect.Error
  ( ConnectError (..)
  , ConnectException (..)
  , decodeConnectErrorLenient
  , httpStatusToConnectCode
  )
import Network.Connect.Compression qualified as CC
import Network.Connect.Envelope
  ( EndStreamResponse (esError, esMetadata)
  , EnvelopeFlags (..)
  , FrameReader
  , buildFrameLazy
  , decodeEndStreamLenient
  , newFrameReader
  , readFrame
  )
import Network.Connect.Metadata (headersToLeading, leadingToHeaders, prefixedHeadersToTrailing)
import Network.Connect.Protocol
  ( Codec (..)
  , ConnectContentType (..)
  , ConnectContentTypeError (..)
  , IsStreaming (..)
  , boundTimeoutMillis
  , connectGetVersion
  , connectProtocolVersion
  , hConnectAcceptEncoding
  , hConnectContentEncoding
  , hConnectProtocolVersion
  , hConnectTimeoutMs
  , lookupConnectContentType
  , qpBase64
  , qpCompression
  , qpConnect
  , qpEncoding
  , qpMessage
  , renderTimeoutMillis
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
  , SupportsClientRpc
  )
import Network.HTTP.Connection qualified as Connection
import Network.HTTP2.Transport (Transport)
import Network.HTTP.Message (Request (..), Response (..), Scheme (..))
import Network.HTTP.PercentEncoding (renderQueryString)
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Types.Header
  ( Headers
  , hAcceptEncoding
  , hContentEncoding
  , hContentType
  , insertHeader
  , lookupHeaders
  )
import Network.HTTP.Types.Method qualified as Method
import Network.HTTP.Types.Status (Status, status200)
import Network.HTTP.Types.Version (pattern HTTP1_1)

------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------

-- | Per-client Connect configuration: the codec, compression policy,
-- deadline, and metadata applied to every call.
data ConnectClientConfig = ConnectClientConfig
  { cccCodec :: !Codec
    -- ^ Wire codec: 'CodecProto' (binary Protobuf) or 'CodecJSON' (proto3 JSON).
  , cccRequestCompression :: !CC.ContentCoding
    -- ^ Compression applied to outgoing messages ('Identity' = none).
  , cccAcceptCompression :: ![CC.ContentCoding]
    -- ^ Codings advertised via @accept-encoding@ for unary calls and
    -- @connect-accept-encoding@ for streaming calls. An empty list omits
    -- the field, so the request's coding is implicitly accepted.
  , cccTimeoutMs :: !(Maybe Int)
    -- ^ Optional per-call deadline, sent as @connect-timeout-ms@.
    -- Negative durations are already expired. Long durations are capped
    -- at the ten-digit wire limit and local timer range.
  , cccMetadata :: ![CustomMetadata]
    -- ^ Leading metadata sent on every call.
  , cccSendProtocolVersion :: !Bool
    -- ^ Send the optional @connect-protocol-version: 1@ marker on POST
    -- calls. Unary GET uses the @connect=v1@ query parameter.
  }

-- | Binary Protobuf, no request compression, advertising @identity@ + @gzip@,
-- no timeout, empty metadata, protocol-version header on.
defaultConnectClientConfig :: ConnectClientConfig
defaultConnectClientConfig =
  ConnectClientConfig
    { cccCodec = CodecProto
    , cccRequestCompression = CC.Identity
    , cccAcceptCompression = [CC.Identity, CC.Gzip]
    , cccTimeoutMs = Nothing
    , cccMetadata = []
    , cccSendProtocolVersion = True
    }

------------------------------------------------------------------------
-- Client
------------------------------------------------------------------------

-- | A live Connect client over a wireform-http connection. The connection,
-- codec settings, and authority\/scheme are fixed at construction; pass the
-- record to 'nonStreaming' \/ 'serverStreaming' \/ etc. to issue calls.
data ConnectClient = ConnectClient
  { clConn :: !Connection.Connection
    -- ^ The underlying HTTP connection.
  , clConfig :: !ConnectClientConfig
    -- ^ The client configuration (codec, compression, metadata, …).
  , clAuthority :: !ByteString
    -- ^ The @Host@\/@:authority@ sent on requests.
  , clScheme :: !Scheme
    -- ^ @http@ or @https@, derived from the connection's TLS config.
  }

-- | Bracket a Connect client over a 'Connection.Connection'. Opens the HTTP
-- connection (plain or TLS; HTTP\/1.1 or HTTP\/2 per the
-- 'Connection.ConnectionConfig') and passes the client to the action; the
-- connection is closed on exit.
withConnectClient
  :: ConnectClientConfig
  -> Connection.ConnectionConfig
  -> (ConnectClient -> IO a)
  -> IO a
withConnectClient cfg connCfg action =
  Connection.withConnection connCfg $ \conn -> do
    let host = bsPack (Connection.connectionHost connCfg)
        scheme = case Connection.connectionTls connCfg of
          Just _ -> SchemeHttps
          Nothing -> SchemeHttp
    action (ConnectClient conn cfg host scheme)

-- | Bracket a Connect client over a caller-supplied 'Transport' (e.g. one end
-- of an in-memory @wireform-dst-net@ fault link) instead of dialing a socket,
-- speaking HTTP\/2 prior-knowledge (h2c). The transport counterpart of
-- 'withConnectClient'; @connectionHost@ supplies the @:authority@. The link is
-- plaintext, so the scheme is always @http@.
withConnectClientOnTransport
  :: ConnectClientConfig
  -> Connection.ConnectionConfig
  -> Transport
  -> (ConnectClient -> IO a)
  -> IO a
withConnectClientOnTransport cfg connCfg transport action =
  Connection.withConnectionOnTransport connCfg transport $ \conn -> do
    let host = bsPack (Connection.connectionHost connCfg)
    action (ConnectClient conn cfg host SchemeHttp)

------------------------------------------------------------------------
-- Unary
------------------------------------------------------------------------

-- | Issue a unary RPC over HTTP @POST@: encode the input, send the request,
-- and return the decoded response, or throw 'ConnectException' on an
-- error response.
nonStreaming
  :: ( SupportsClientRpc rpc
     , Aeson.ToJSON (Input rpc)
     , Aeson.FromJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'NonStreaming
     )
  => ConnectClient
  -> Proxy rpc
  -> Input rpc
  -> IO (Output rpc)
nonStreaming cl p input = do
  let cfg = clConfig cl
      codec = cccCodec cfg
      raw = encodeInputBody codec p input
      (bodyBytes, _coding) = compressUnary (cccRequestCompression cfg) raw
      path = "/" <> rpcServiceName p <> "/" <> rpcMethodName p
      req =
        (baseRequest cl)
          { requestMethod = Method.mPost
          , requestTarget = path
          , requestHeaders = unaryHeaders cl cfg codec
          , requestBody = BodyBytes bodyBytes
          }
  resp <- Connection.sendOn (clConn cl) req
  readUnaryResponse cfg p resp

-- | Issue a unary RPC over HTTP @GET@, encoding the request in query
-- parameters (@?connect=v1&encoding=…&message=…@). Use only for
-- side-effect-free, cacheable methods; response handling matches
-- 'nonStreaming'.
nonStreamingGet
  :: ( SupportsClientRpc rpc
     , Aeson.ToJSON (Input rpc)
     , Aeson.FromJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'NonStreaming
     )
  => ConnectClient
  -> Proxy rpc
  -> Input rpc
  -> IO (Output rpc)
nonStreamingGet cl p input = do
  let cfg = clConfig cl
      codec = cccCodec cfg
      raw = encodeInputBody codec p input
      isBinary = codec == CodecProto || cccRequestCompression cfg /= CC.Identity
      (payload, _coding) = compressUnary (cccRequestCompression cfg) raw
      msgParam
        | isBinary = B64U.encodeUnpadded payload
        | otherwise = payload
      query =
        renderQueryString
          [ (qpConnect, connectGetVersion)
          , (qpEncoding, codecToken codec)
          , (qpBase64, if isBinary then "1" else "0")
          , (qpCompression, CC.codingName (cccRequestCompression cfg))
          , (qpMessage, msgParam)
          ]
      path = "/" <> rpcServiceName p <> "/" <> rpcMethodName p <> "?" <> query
      req =
        (baseRequest cl)
          { requestMethod = Method.mGet
          , requestTarget = path
          , requestHeaders = unaryGetHeaders cfg
          , requestBody = BodyEmpty
          }
  resp <- Connection.sendOn (clConn cl) req
  readUnaryResponse cfg p resp

readUnaryResponse
  :: (SupportsClientRpc rpc, Aeson.FromJSON (Output rpc))
  => ConnectClientConfig
  -> Proxy rpc
  -> Response
  -> IO (Output rpc)
readUnaryResponse cfg p resp = decodeUnaryResponse cfg p resp >>= either throwConnectIO pure

decodeUnaryResponse
  :: (SupportsClientRpc rpc, Aeson.FromJSON (Output rpc))
  => ConnectClientConfig
  -> Proxy rpc
  -> Response
  -> IO (Either ConnectError (Output rpc))
decodeUnaryResponse cfg p resp = case unaryResponseCoding cfg resp of
  Left err -> discardBody body >> pure (Left err)
  Right coding -> do
    plain <- decodeUnaryBody coding body
    pure $
      if status == status200
        then first decodeFailure (plain >>= decodeOutputBody (cccCodec cfg) p)
        else Left $ case plain >>= Aeson.eitherDecodeStrict of
          Right v -> fromMaybe (inferError status) (decodeConnectErrorLenient (httpStatusToConnectCode status) v)
          Left _ -> inferError status
  where
    status = responseStatus resp
    body = responseBody resp

unaryResponseCoding :: ConnectClientConfig -> Response -> Either ConnectError CC.ContentCoding
unaryResponseCoding cfg resp = case mediaError of
  Just err -> Left err
  Nothing -> case validateResponseCoding cfg "Content-Encoding" parsedCoding of
    Left _ | status /= status200 -> Left (inferError status)
    result -> result
  where
    status = responseStatus resp
    headers = responseHeaders resp
    parsedCoding = CC.parseUnaryContentCoding (lookupHeaders hContentEncoding headers)
    mediaError
      | status == status200 = responseContentTypeError Unary (cccCodec cfg) headers
      | Right (ConnectContentType Unary CodecJSON) <- lookupConnectContentType headers = Nothing
      | otherwise = Just (inferError status)

------------------------------------------------------------------------
-- Server streaming
------------------------------------------------------------------------

-- | Issue a server-streaming RPC. Sends the single request, then passes a
-- @recv@ action to the continuation that yields each response message
-- ('Nothing' at end-of-stream). The stream ends when the continuation returns.
serverStreaming
  :: ( SupportsClientRpc rpc
     , Aeson.ToJSON (Input rpc)
     , Aeson.FromJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'ServerStreaming
     )
  => ConnectClient
  -> Proxy rpc
  -> Input rpc
  -> (IO (Maybe (Output rpc)) -> IO r)
  -> IO r
serverStreaming cl p input recvAction = do
  let cfg = clConfig cl
      codec = cccCodec cfg
      body = singleInputFrame codec p (cccRequestCompression cfg) input
      req =
        (baseRequest cl)
          { requestMethod = Method.mPost
          , requestTarget = "/" <> rpcServiceName p <> "/" <> rpcMethodName p
          , requestHeaders = streamingHeaders cl cfg (cccRequestCompression cfg)
          , requestBody = BodyBytes body
          }
  Connection.withResponseOn (clConn cl) req $ \resp -> do
    reader <- openStreamingResponse cl resp >>= either throwConnectIO pure
    recvAction (readDataFrame codec p reader)

------------------------------------------------------------------------
-- Client streaming
------------------------------------------------------------------------

-- | Issue a client-streaming RPC. The continuation is given a @send@ action
-- for request messages; when it returns the client half-closes and reads the
-- single response. (Frames are buffered before the request body is sent, so
-- this works over HTTP\/1.1, which requires the full body up front.)
clientStreaming
  :: ( SupportsClientRpc rpc
     , Aeson.ToJSON (Input rpc)
     , Aeson.FromJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'ClientStreaming
     )
  => ConnectClient
  -> Proxy rpc
  -> ((Input rpc -> IO ()) -> IO ())
  -> IO (Output rpc)
clientStreaming cl p sendAction = do
  outQ <- newTQueueIO :: IO (TQueue (Maybe ByteString))
  let cfg = clConfig cl
      codec = cccCodec cfg
      reqCoding = cccRequestCompression cfg
      send msg = atomically (writeTQueue outQ (Just (inputFrame codec p reqCoding msg)))
  -- Pre-fill the request frames so the body producer never blocks (works on
  -- HTTP\/1.1, which sends the full request body before reading the response).
  sendAction send
  atomically (writeTQueue outQ Nothing)
  let producer =
        atomically $
          readTQueue outQ >>= \case
            Just fr -> pure (Just fr)
            Nothing -> pure Nothing
      req =
        (baseRequest cl)
          { requestMethod = Method.mPost
          , requestTarget = "/" <> rpcServiceName p <> "/" <> rpcMethodName p
          , requestHeaders = streamingHeaders cl cfg reqCoding
          , requestBody = BodyStream producer
          }
  Connection.withResponseOn (clConn cl) req $ \resp -> do
    reader <- openStreamingResponse cl resp >>= either throwConnectIO pure
    readClientStreamOutput codec p reader

------------------------------------------------------------------------
-- Bidirectional streaming
------------------------------------------------------------------------

-- | Issue a bidirectional-streaming RPC. The continuation is given a @send@
-- action for requests and a @recv@ action for responses ('Nothing' at
-- end-of-stream); either may be called any number of times, in any order —
-- full duplex over HTTP\/2. The stream ends when the continuation returns.
--
-- The request is dispatched without waiting for the response head: a
-- Connect server may stage its response headers until its handler sends the
-- first message, which for a ping-pong conversation only happens after the
-- client's first request frame — blocking on the response before running
-- the continuation would deadlock. @recv@ awaits the response head on its
-- first call instead.
biDiStreaming
  :: ( SupportsClientRpc rpc
     , Aeson.ToJSON (Input rpc)
     , Aeson.FromJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'BiDiStreaming
     )
  => ConnectClient
  -> Proxy rpc
  -> ((Input rpc -> IO ()) -> IO (Maybe (Output rpc)) -> IO r)
  -> IO r
biDiStreaming cl p action = do
  outQ <- newTQueueIO :: IO (TQueue (Maybe ByteString))
  let cfg = clConfig cl
      codec = cccCodec cfg
      reqCoding = cccRequestCompression cfg
      send msg = atomically (writeTQueue outQ (Just (inputFrame codec p reqCoding msg)))
      producer = atomically (readTQueue outQ)
      req =
        (baseRequest cl)
          { requestMethod = Method.mPost
          , requestTarget = "/" <> rpcServiceName p <> "/" <> rpcMethodName p
          , requestHeaders = streamingHeaders cl cfg reqCoding
          , requestBody = BodyStream producer
          }
  Connection.withResponseDeferredOn (clConn cl) req $ \awaitResp -> do
    -- The frame reader is created lazily on the first recv (the response
    -- head may not have arrived yet). Cache both admission and rejection.
    frCell <- newMVar Nothing
    let getFrameReader = modifyMVar frCell $ \case
          st@(Just fr) -> pure (st, fr)
          Nothing -> do
            result <- awaitResp >>= openStreamingResponse cl
            pure (Just result, result)
        recv = getFrameReader >>= either throwConnectIO (readDataFrame codec p)
    r <- action send recv
    atomically (writeTQueue outQ Nothing)
    pure r

------------------------------------------------------------------------
-- Resolved (full-outcome) calls
------------------------------------------------------------------------

-- | The complete observed outcome of an RPC: response leading metadata, all
-- response payloads (0-or-1 for unary\/client-stream, 0-or-more for
-- server\/bidi-stream), response trailing metadata, and the RPC error if one
-- occurred. Unlike 'nonStreaming' et al. these do /not/ throw on an RPC error
-- — it is returned in 'roError'. Intended for tooling that must report the
-- full result (e.g. conformance testing).
data RpcOutcome a = RpcOutcome
  { roHeaders :: ![CustomMetadata]
  , roPayloads :: ![a]
  , roTrailers :: ![CustomMetadata]
  , roError :: !(Maybe ConnectError)
  }

-- | Enforce the client's configured deadline ('cccTimeoutMs') on a resolved
-- RPC. If the call does not complete within the deadline, it is aborted (the
-- async exception from 'timeout' tears down the underlying connection via the
-- @withConnection@ \/ @withResponseOn@ bracket) and a 'GrpcDeadlineExceeded'
-- error is reported. A Connect client must honor its own deadline locally
-- rather than relying solely on the server to do so.
withRpcDeadline :: ConnectClient -> IO (RpcOutcome a) -> IO (RpcOutcome a)
withRpcDeadline cl act =
  case boundTimeoutMillis <$> cccTimeoutMs (clConfig cl) of
    Just ms | ms > 0 -> do
      m <- timeout (ms * 1000) act
      case m of
        Just r -> pure r
        Nothing ->
          pure
            ( RpcOutcome
                []
                []
                []
                (Just (ConnectError GrpcDeadlineExceeded (Just "connect: deadline exceeded") []))
            )
    _ -> act

-- | Unary @POST@, returning the full outcome instead of throwing.
nonStreamingResolved
  :: ( SupportsClientRpc rpc
     , Aeson.ToJSON (Input rpc)
     , Aeson.FromJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'NonStreaming
     )
  => ConnectClient
  -> Proxy rpc
  -> Input rpc
  -> IO (RpcOutcome (Output rpc))
nonStreamingResolved cl p input = withRpcDeadline cl $ do
  let cfg = clConfig cl
      codec = cccCodec cfg
      raw = encodeInputBody codec p input
      (bodyBytes, _coding) = compressUnary (cccRequestCompression cfg) raw
      req =
        (baseRequest cl)
          { requestMethod = Method.mPost
          , requestTarget = "/" <> rpcServiceName p <> "/" <> rpcMethodName p
          , requestHeaders = unaryHeaders cl cfg codec
          , requestBody = BodyBytes bodyBytes
          }
  resp <- Connection.sendOn (clConn cl) req
  resolveUnary cfg p resp

-- | Unary @GET@, returning the full outcome instead of throwing.
nonStreamingGetResolved
  :: ( SupportsClientRpc rpc
     , Aeson.ToJSON (Input rpc)
     , Aeson.FromJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'NonStreaming
     )
  => ConnectClient
  -> Proxy rpc
  -> Input rpc
  -> IO (RpcOutcome (Output rpc))
nonStreamingGetResolved cl p input = withRpcDeadline cl $ do
  let cfg = clConfig cl
      codec = cccCodec cfg
      raw = encodeInputBody codec p input
      isBinary = codec == CodecProto || cccRequestCompression cfg /= CC.Identity
      (payload, _coding) = compressUnary (cccRequestCompression cfg) raw
      msgParam
        | isBinary = B64U.encodeUnpadded payload
        | otherwise = payload
      query =
        renderQueryString
          [ (qpConnect, connectGetVersion)
          , (qpEncoding, codecToken codec)
          , (qpBase64, if isBinary then "1" else "0")
          , (qpCompression, CC.codingName (cccRequestCompression cfg))
          , (qpMessage, msgParam)
          ]
      req =
        (baseRequest cl)
          { requestMethod = Method.mGet
          , requestTarget = "/" <> rpcServiceName p <> "/" <> rpcMethodName p <> "?" <> query
          , requestHeaders = unaryGetHeaders cfg
          , requestBody = BodyEmpty
          }
  resp <- Connection.sendOn (clConn cl) req
  resolveUnary cfg p resp

-- | Server-streaming, collecting all response payloads + trailing metadata.
serverStreamingResolved
  :: ( SupportsClientRpc rpc
     , Aeson.ToJSON (Input rpc)
     , Aeson.FromJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'ServerStreaming
     )
  => ConnectClient
  -> Proxy rpc
  -> Input rpc
  -> IO (RpcOutcome (Output rpc))
serverStreamingResolved cl p = serverStreamingResolvedUpTo cl p Nothing

-- | Like 'serverStreamingResolved' but, when given @Just n@, cancels the RPC
-- after reading @n@ response messages (reporting a 'GrpcCancelled' error). For
-- the @after_num_responses@ cancellation mode.
serverStreamingResolvedUpTo
  :: ( SupportsClientRpc rpc
     , Aeson.ToJSON (Input rpc)
     , Aeson.FromJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'ServerStreaming
     )
  => ConnectClient
  -> Proxy rpc
  -> Maybe Int
  -> Input rpc
  -> IO (RpcOutcome (Output rpc))
serverStreamingResolvedUpTo cl p mcap input = withRpcDeadline cl $ do
  let cfg = clConfig cl
      codec = cccCodec cfg
      body = singleInputFrame codec p (cccRequestCompression cfg) input
      req =
        (baseRequest cl)
          { requestMethod = Method.mPost
          , requestTarget = "/" <> rpcServiceName p <> "/" <> rpcMethodName p
          , requestHeaders = streamingHeaders cl cfg (cccRequestCompression cfg)
          , requestBody = BodyBytes body
          }
  Connection.withResponseOn (clConn cl) req (collectStreamResp cl mcap p)

-- | Client-streaming: send all inputs, then collect the single response.
clientStreamingResolved
  :: ( SupportsClientRpc rpc
     , Aeson.ToJSON (Input rpc)
     , Aeson.FromJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'ClientStreaming
     )
  => ConnectClient
  -> Proxy rpc
  -> [Input rpc]
  -> IO (RpcOutcome (Output rpc))
clientStreamingResolved cl p inputs = do
  o <- streamSendAllResolved cl p inputs
  -- Connect client-streaming responses must carry exactly one message. If the
  -- server sent a different count and reported no error of its own, that is a
  -- protocol violation (matches connect-go: CODE_UNIMPLEMENTED).
  pure $ case roError o of
    Nothing
      | length (roPayloads o) /= 1 ->
          o
            { roPayloads = []
            , roError =
                Just
                  ( ConnectError
                      GrpcUnimplemented
                      (Just "connect: client-streaming response did not contain exactly one message")
                      []
                  )
            }
    _ -> o

-- | Bidi-streaming, half-duplex: send all inputs, then collect responses.
biDiStreamingResolved
  :: ( SupportsClientRpc rpc
     , Aeson.ToJSON (Input rpc)
     , Aeson.FromJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'BiDiStreaming
     )
  => ConnectClient
  -> Proxy rpc
  -> [Input rpc]
  -> IO (RpcOutcome (Output rpc))
biDiStreamingResolved = streamSendAllResolved

-- | Like 'biDiStreamingResolved' but cancels after reading @Just n@ responses.
biDiStreamingResolvedUpTo
  :: ( SupportsClientRpc rpc
     , Aeson.ToJSON (Input rpc)
     , Aeson.FromJSON (Output rpc)
     , HasStreamingType rpc
     , RpcStreamingType rpc ~ 'BiDiStreaming
     )
  => ConnectClient
  -> Proxy rpc
  -> Maybe Int
  -> [Input rpc]
  -> IO (RpcOutcome (Output rpc))
biDiStreamingResolvedUpTo = streamSendAllResolvedUpTo

-- Shared by client- and bidi-streaming resolved: prefill all input frames,
-- send, then collect the response stream (canceling after @mcap@ responses).
streamSendAllResolved
  :: (SupportsClientRpc rpc, Aeson.ToJSON (Input rpc), Aeson.FromJSON (Output rpc))
  => ConnectClient
  -> Proxy rpc
  -> [Input rpc]
  -> IO (RpcOutcome (Output rpc))
streamSendAllResolved cl p = streamSendAllResolvedUpTo cl p Nothing

streamSendAllResolvedUpTo
  :: (SupportsClientRpc rpc, Aeson.ToJSON (Input rpc), Aeson.FromJSON (Output rpc))
  => ConnectClient
  -> Proxy rpc
  -> Maybe Int
  -> [Input rpc]
  -> IO (RpcOutcome (Output rpc))
streamSendAllResolvedUpTo cl p mcap inputs = withRpcDeadline cl $ do
  outQ <- newTQueueIO :: IO (TQueue (Maybe ByteString))
  let cfg = clConfig cl
      codec = cccCodec cfg
      reqCoding = cccRequestCompression cfg
  mapM_ (\m -> atomically (writeTQueue outQ (Just (inputFrame codec p reqCoding m)))) inputs
  atomically (writeTQueue outQ Nothing)
  let producer = atomically (readTQueue outQ)
      req =
        (baseRequest cl)
          { requestMethod = Method.mPost
          , requestTarget = "/" <> rpcServiceName p <> "/" <> rpcMethodName p
          , requestHeaders = streamingHeaders cl cfg reqCoding
          , requestBody = BodyStream producer
          }
  Connection.withResponseOn (clConn cl) req (collectStreamResp cl mcap p)

-- | Parse a unary response into a full outcome (does not throw).
resolveUnary
  :: (SupportsClientRpc rpc, Aeson.FromJSON (Output rpc))
  => ConnectClientConfig
  -> Proxy rpc
  -> Response
  -> IO (RpcOutcome (Output rpc))
resolveUnary cfg p resp = do
  let headers = responseHeaders resp
      leading = headersToLeading (filter (not . isTrailerHeader) headers)
      trailers = prefixedHeadersToTrailing headers
  result <- decodeUnaryResponse cfg p resp
  pure $ case result of
    Left err -> RpcOutcome leading [] trailers (Just err)
    Right out -> RpcOutcome leading [out] trailers Nothing

-- | Collect a streaming response into a full outcome (does not throw): all
-- data frames as payloads, plus the EndStreamResponse's metadata and error.
--
-- @mcap@ caps the number of payloads to read: once @Just n@ payloads have
-- arrived the collection stops and reports a 'GrpcCancelled' error, and the
-- caller's @withResponseOn@ bracket tears down the connection — i.e. the RPC
-- is canceled after reading @n@ responses (the conformance @after_num_responses@
-- cancellation mode).
collectStreamResp
  :: (SupportsClientRpc rpc, Aeson.FromJSON (Output rpc))
  => ConnectClient
  -> Maybe Int
  -> Proxy rpc
  -> Response
  -> IO (RpcOutcome (Output rpc))
collectStreamResp cl mcap p resp = do
  result <- openStreamingResponse cl resp
  case result of
    Left err -> failWith [] err
    Right (StreamingReader fr coding) -> go fr coding []
  where
    codec = cccCodec (clConfig cl)
    leading = headersToLeading (filter (not . isTrailerHeader) (responseHeaders resp))
    failWith acc err = pure (RpcOutcome leading (reverse acc) [] (Just err))
    go fr coding acc = do
      mframe <- readFrame fr
      case mframe of
        Nothing -> pure (RpcOutcome leading (reverse acc) [] Nothing)
        Just (flags, payload) ->
          withFramePayload coding flags payload (failWith acc) $ \plain ->
            if efEndStream flags
              then
                let esr = decodeEndStreamLenient plain
                in pure (RpcOutcome leading (reverse acc) (esMetadata esr) (esError esr))
              else case decodeOutputBody codec p plain of
                Left e -> failWith acc (decodeFailure e)
                Right out ->
                  let acc' = out : acc
                  in case mcap of
                       Just n | length acc' >= n ->
                         failWith acc' (ConnectError GrpcCancelled (Just "connect: canceled") [])
                       _ -> go fr coding acc'

isTrailerHeader :: (CI.CI ByteString, ByteString) -> Bool
isTrailerHeader (n, _) = "trailer-" `BS.isPrefixOf` CI.foldedCase n

decodeFailure :: String -> ConnectError
decodeFailure e =
  ConnectError
    { ceCode = GrpcInternal
    , ceMessage = Just (T.pack ("connect: could not decode response: " <> e))
    , ceDetails = []
    }

------------------------------------------------------------------------
-- Request building
------------------------------------------------------------------------

baseRequest :: ConnectClient -> Request
baseRequest cl =
  Request
    { requestMethod = Method.mPost
    , requestTarget = "/"
    , requestAuthority = Just (clAuthority cl)
    , requestScheme = clScheme cl
    , requestHeaders = []
    , requestBody = BodyEmpty
    , requestVersion = HTTP1_1
    , requestTrailers = pure []
    }

unaryHeaders :: ConnectClient -> ConnectClientConfig -> Codec -> Headers
unaryHeaders _cl cfg codec =
  let h0 = leadingToHeaders (cccMetadata cfg) <> [(hContentType, unaryContentType codec)]
      h1 = case cccRequestCompression cfg of
        CC.Identity -> h0
        c -> insertHeader hContentEncoding (CC.codingName c) h0
   in postControlHeaders cfg (unaryAcceptHeaders cfg h1)

unaryGetHeaders :: ConnectClientConfig -> Headers
unaryGetHeaders cfg =
  timeoutHeaders cfg (unaryAcceptHeaders cfg (leadingToHeaders (cccMetadata cfg)))

unaryAcceptHeaders :: ConnectClientConfig -> Headers -> Headers
unaryAcceptHeaders cfg headers = case cccAcceptCompression cfg of
  [] -> headers
  codings -> insertHeader hAcceptEncoding (CC.renderAcceptEncoding codings) headers

streamingHeaders :: ConnectClient -> ConnectClientConfig -> CC.ContentCoding -> Headers
streamingHeaders _cl cfg reqCoding =
  let codec = cccCodec cfg
      baseHeaders =
        leadingToHeaders (cccMetadata cfg)
          <> [(hContentType, streamContentType codec)]
      h0 = case reqCoding of
        CC.Identity -> baseHeaders
        c -> insertHeader hConnectContentEncoding (CC.codingName c) baseHeaders
      h1 = case cccAcceptCompression cfg of
        [] -> h0
        cs -> insertHeader hConnectAcceptEncoding (CC.renderAcceptEncoding cs) h0
   in postControlHeaders cfg h1

timeoutHeaders :: ConnectClientConfig -> Headers -> Headers
timeoutHeaders cfg headers = case cccTimeoutMs cfg of
  Nothing -> headers
  Just ms -> insertHeader hConnectTimeoutMs (renderTimeoutMillis ms) headers

postControlHeaders :: ConnectClientConfig -> Headers -> Headers
postControlHeaders cfg headers
  | cccSendProtocolVersion cfg = insertHeader hConnectProtocolVersion connectProtocolVersion withTimeout
  | otherwise = withTimeout
  where
    withTimeout = timeoutHeaders cfg headers

-- | A single enveloped input frame (for server streaming's one message).
singleInputFrame
  :: (SupportsClientRpc rpc, Aeson.ToJSON (Input rpc))
  => Codec
  -> Proxy rpc
  -> CC.ContentCoding
  -> Input rpc
  -> ByteString
singleInputFrame codec p reqCoding input =
  let raw = encodeInputBody codec p input
      (payload, compressed) = compressUnary' reqCoding raw
      flags = EnvelopeFlags{efCompressed = compressed, efEndStream = False}
   in BL.toStrict (buildFrameLazy flags payload)

-- | A single enveloped input frame as an action (for streaming send).
inputFrame
  :: (SupportsClientRpc rpc, Aeson.ToJSON (Input rpc))
  => Codec
  -> Proxy rpc
  -> CC.ContentCoding
  -> Input rpc
  -> ByteString
inputFrame = singleInputFrame

------------------------------------------------------------------------
-- Frame reading
------------------------------------------------------------------------

data StreamingReader = StreamingReader !FrameReader !CC.ContentCoding

-- Validate the response head before creating a reader or consuming the body.
openStreamingResponse :: ConnectClient -> Response -> IO (Either ConnectError StreamingReader)
openStreamingResponse cl resp = case streamingResponseCoding (clConfig cl) resp of
  Right coding -> do
    fr <- newFrameReader =<< responseBodyProducer (responseBody resp)
    pure (Right (StreamingReader fr coding))
  Left err -> do
    case responseBody resp of
      -- HTTP/1 cannot reuse a connection with an unread body. Closing it
      -- also avoids waiting for a body whose headers we already rejected.
      BodyStream _ | responseH2StreamId resp == 0 ->
        Connection.closeConnection (clConn cl)
      _ -> responseCancel resp
    pure (Left err)

streamingResponseCoding :: ConnectClientConfig -> Response -> Either ConnectError CC.ContentCoding
streamingResponseCoding cfg resp
  | responseStatus resp /= status200 = Left (inferError (responseStatus resp))
  | Just err <- responseContentTypeError Streaming (cccCodec cfg) headers = Left err
  | otherwise = validateResponseCoding cfg "Connect-Content-Encoding" (CC.parseStreamingContentCoding headers)
  where
    headers = responseHeaders resp

-- An omitted preference field implicitly advertises the request's coding.
validateResponseCoding
  :: ConnectClientConfig
  -> String
  -> Either String CC.ContentCoding
  -> Either ConnectError CC.ContentCoding
validateResponseCoding cfg field result = case result of
  Left err -> Left (decodeFailure err)
  Right coding
    | coding == CC.Identity || advertised coding -> Right coding
    | otherwise -> Left (decodeFailure ("unadvertised response " <> field))
  where
    advertised coding = case cccAcceptCompression cfg of
      [] -> coding == cccRequestCompression cfg
      codings -> coding `elem` codings
{-# INLINE validateResponseCoding #-}

-- | Read the next output frame; @Nothing@ at the end-stream frame (throwing
-- if the end-stream carries an error).
readDataFrame
  :: (SupportsClientRpc rpc, Aeson.FromJSON (Output rpc))
  => Codec
  -> Proxy rpc
  -> StreamingReader
  -> IO (Maybe (Output rpc))
readDataFrame codec p (StreamingReader fr coding) = do
  mframe <- readFrame fr
  case mframe of
    Nothing -> pure Nothing
    Just (flags, payload) ->
      withFramePayload coding flags payload throwConnectIO $ \plain ->
        if efEndStream flags
          then checkEndStream plain >> pure Nothing
          else either (throwConnectIO . decodeFailure) (pure . Just) (decodeOutputBody codec p plain)

-- | Read the single output message of a client-streaming response, then the
-- end-stream frame.
readClientStreamOutput
  :: (SupportsClientRpc rpc, Aeson.FromJSON (Output rpc))
  => Codec
  -> Proxy rpc
  -> StreamingReader
  -> IO (Output rpc)
readClientStreamOutput codec p (StreamingReader fr coding) = do
  mframe <- readFrame fr
  case mframe of
    Nothing -> throwConnectIO missingOutput
    Just (flags, payload) ->
      withFramePayload coding flags payload throwConnectIO $ \plain ->
        if efEndStream flags
          then throwConnectIO (fromMaybe missingOutput (esError (decodeEndStreamLenient plain)))
          else do
            out <- either (throwConnectIO . decodeFailure) pure (decodeOutputBody codec p plain)
            mEnd <- readFrame fr
            case mEnd of
              Just (endFlags, endPayload) | efEndStream endFlags ->
                withFramePayload coding endFlags endPayload throwConnectIO checkEndStream
              _ -> pure ()
            pure out
  where
    missingOutput = ConnectError GrpcInternal (Just "connect: client stream ended without output") []

-- | Throw if a decoded end-stream frame carries an RPC error.
checkEndStream :: ByteString -> IO ()
checkEndStream plain =
  case esError (decodeEndStreamLenient plain) of
    Just ce -> throwConnectIO ce
    Nothing -> pure ()

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

compressUnary :: CC.ContentCoding -> ByteString -> (ByteString, CC.ContentCoding)
compressUnary CC.Identity bs = (bs, CC.Identity)
compressUnary c bs = (CC.compress c bs, c)

compressUnary' :: CC.ContentCoding -> ByteString -> (ByteString, Bool)
compressUnary' CC.Identity bs = (bs, False)
compressUnary' c bs = (CC.compress c bs, True)

decodeUnaryBody :: CC.ContentCoding -> Body -> IO (Either String ByteString)
decodeUnaryBody coding body = do
  bytes <- drainBody body
  if BS.null bytes
    then pure (Right bytes)
    else CC.decompress coding bytes

-- Uncompressed frames use the original payload without wrapping a result.
withFramePayload :: CC.ContentCoding -> EnvelopeFlags -> ByteString -> (ConnectError -> IO a) -> (ByteString -> IO a) -> IO a
withFramePayload coding flags payload onError onPayload
  | not (efCompressed flags) = onPayload payload
  | coding == CC.Identity =
      onError (ConnectError GrpcInternal (Just "connect: received compressed message but no compression was negotiated") [])
  | otherwise = CC.decompress coding payload >>= either (onError . decodeFailure) onPayload
{-# INLINE withFramePayload #-}

drainBody :: Body -> IO ByteString
drainBody BodyEmpty = pure BS.empty
drainBody (BodyBytes bs) = pure bs
drainBody (BodyStream p) = go []
  where
    go acc =
      p >>= \case
        Nothing -> pure (BS.concat (reverse acc))
        Just chunk -> go (chunk : acc)

-- Consume the wire framing for connection reuse without buffering the body.
discardBody :: Body -> IO ()
discardBody (BodyStream pull) = go
  where
    go = pull >>= maybe (pure ()) (const go)
discardBody _ = pure ()

responseBodyProducer :: Body -> IO (IO (Maybe ByteString))
responseBodyProducer BodyEmpty = pure (pure Nothing)
responseBodyProducer (BodyBytes bs) = do
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
responseBodyProducer (BodyStream p) = pure p

inferError :: Status -> ConnectError
inferError status = ConnectError{ceCode = httpStatusToConnectCode status, ceMessage = Just "connect: server error", ceDetails = []}

-- | Validate the response media type against the request's envelope and codec.
responseContentTypeError :: IsStreaming -> Codec -> Headers -> Maybe ConnectError
responseContentTypeError expected codec headers =
  case lookupConnectContentType headers of
    Right (ConnectContentType envelope actual) | envelope == expected && actual == codec -> Nothing
    Right (ConnectContentType Unary _) | expected == Streaming -> unexpectedType
    Left (UnsupportedConnectContentType Unary) | expected == Streaming -> unexpectedType
    Left MissingContentType -> ctErr GrpcUnknown "connect: response missing content-type"
    Left NonApplicationContentType -> unexpectedType
    Left MalformedContentType -> ctErr GrpcInternal "connect: malformed response content-type"
    Left RepeatedContentType -> ctErr GrpcInternal "connect: repeated response content-type"
    _ -> ctErr GrpcInternal "connect: unsupported or unexpected response codec"
  where
    unexpectedType = ctErr GrpcUnknown "connect: unexpected response content-type"
    ctErr c m = Just ConnectError{ceCode = c, ceMessage = Just m, ceDetails = []}
{-# INLINE responseContentTypeError #-}

throwConnectIO :: ConnectError -> IO a
throwConnectIO ce = throwIO (ConnectException ce)

codecToken :: Codec -> ByteString
codecToken CodecProto = "proto"
codecToken CodecJSON = "json"

bsPack :: String -> ByteString
bsPack = BS.pack . map (fromIntegral . fromEnum)
