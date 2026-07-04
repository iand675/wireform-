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
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Lazy qualified as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
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
  , connectGetVersion
  , connectProtocolVersion
  , hConnectAcceptEncoding
  , hConnectContentEncoding
  , hConnectTimeoutMs
  , qpBase64
  , qpCompression
  , qpConnect
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
  , lookupHeader
  )
import Network.HTTP.Types.Method qualified as Method
import Network.HTTP.Types.Status (Status, pattern Status)
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
    -- ^ Codings advertised to the server via @connect-accept-encoding@.
  , cccTimeoutMs :: !(Maybe Int)
    -- ^ Optional per-call deadline, sent as @connect-timeout-ms@.
  , cccMetadata :: ![CustomMetadata]
    -- ^ Leading metadata sent on every call.
  , cccSendProtocolVersion :: !Bool
    -- ^ Send @connect-protocol-version: 1@ on unary calls (spec-required;
    -- disable only for non-conformant peers).
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
  readUnaryResponse codec p resp

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
          , requestHeaders = leadingToHeaders (cccMetadata cfg)
          , requestBody = BodyEmpty
          }
  resp <- Connection.sendOn (clConn cl) req
  readUnaryResponse codec p resp

readUnaryResponse
  :: (SupportsClientRpc rpc, Aeson.FromJSON (Output rpc))
  => Codec
  -> Proxy rpc
  -> Response
  -> IO (Output rpc)
readUnaryResponse codec p resp = do
  let status = responseStatus resp
      headers = responseHeaders resp
  bodyBytes <- drainBody (responseBody resp)
  if is2xx status
    then case unaryContentTypeError codec headers of
      Just err -> throwIO (ConnectException err)
      Nothing -> do
        let mRespCoding = lookupHeader hContentEncoding headers >>= CC.codingFromName
        plain <- decompressResp bodyBytes mRespCoding
        case decodeOutputBody codec p plain of
          Left e ->
            throwConnectIO $
              ConnectError
                { ceCode = GrpcInternal
                , ceMessage = Just (T.pack ("connect: could not decode response: " <> e))
                , ceDetails = []
                }
          Right out -> pure out
    else do
      let mRespCoding = lookupHeader hContentEncoding headers >>= CC.codingFromName
      plainErr <- decompressResp bodyBytes mRespCoding
      let err =
            case Aeson.decode (BL.fromStrict plainErr) of
              Just v -> fromMaybe (inferError status) (decodeConnectErrorLenient (httpStatusToConnectCode status) v)
              Nothing -> inferError status
      throwIO (ConnectException err)

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
    fr <- newFrameReader =<< responseBodyProducer (responseBody resp)
    recvAction (readDataFrame codec p cfg fr)

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
    fr <- newFrameReader =<< responseBodyProducer (responseBody resp)
    readClientStreamOutput codec p cfg fr

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
    -- head may not have arrived yet); memoized for subsequent calls.
    frCell <- newMVar Nothing
    let getFrameReader = modifyMVar frCell $ \case
          st@(Just fr) -> pure (st, fr)
          Nothing -> do
            resp <- awaitResp
            fr <- newFrameReader =<< responseBodyProducer (responseBody resp)
            pure (Just fr, fr)
        recv = getFrameReader >>= readDataFrame codec p cfg
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
  case cccTimeoutMs (clConfig cl) of
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
  resolveUnary codec p resp

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
          , requestHeaders = leadingToHeaders (cccMetadata cfg)
          , requestBody = BodyEmpty
          }
  resp <- Connection.sendOn (clConn cl) req
  resolveUnary codec p resp

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
  Connection.withResponseOn (clConn cl) req (collectStreamResp mcap codec p cfg)

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
  Connection.withResponseOn (clConn cl) req (collectStreamResp mcap codec p cfg)

-- | Parse a unary response into a full outcome (does not throw).
resolveUnary
  :: (SupportsClientRpc rpc, Aeson.FromJSON (Output rpc))
  => Codec
  -> Proxy rpc
  -> Response
  -> IO (RpcOutcome (Output rpc))
resolveUnary codec p resp = do
  let status = responseStatus resp
      headers = responseHeaders resp
      leading = headersToLeading (filter (not . isTrailerHeader) headers)
      trailers = prefixedHeadersToTrailing headers
  bodyBytes <- drainBody (responseBody resp)
  if is2xx status
    then case unaryContentTypeError codec headers of
      Just err -> pure (RpcOutcome leading [] trailers (Just err))
      Nothing -> do
        let mRespCoding = lookupHeader hContentEncoding headers >>= CC.codingFromName
        plain <- decompressResp bodyBytes mRespCoding
        pure $ case decodeOutputBody codec p plain of
          Left e -> RpcOutcome leading [] trailers (Just (decodeFailure e))
          Right out -> RpcOutcome leading [out] trailers Nothing
    else do
      let mRespCoding = lookupHeader hContentEncoding headers >>= CC.codingFromName
      plainErr <- decompressResp bodyBytes mRespCoding
      let err = case Aeson.decode (BL.fromStrict plainErr) of
            Just v -> fromMaybe (inferError status) (decodeConnectErrorLenient (httpStatusToConnectCode status) v)
            Nothing -> inferError status
      pure (RpcOutcome leading [] trailers (Just err))

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
  => Maybe Int
  -> Codec
  -> Proxy rpc
  -> ConnectClientConfig
  -> Response
  -> IO (RpcOutcome (Output rpc))
collectStreamResp mcap codec p cfg resp = do
  let headers = responseHeaders resp
      leading = headersToLeading (filter (not . isTrailerHeader) headers)
      compressedAllowed =
        maybe False (\c -> CC.codingName c /= "identity")
          (lookupHeader hConnectContentEncoding headers >>= CC.codingFromName)
  fr <- newFrameReader =<< responseBodyProducer (responseBody resp)
  let go acc = do
        mframe <- readFrame fr
        case mframe of
          Nothing -> pure (RpcOutcome leading (reverse acc) [] Nothing)
          Just (flags, payload)
            | efCompressed flags && not compressedAllowed ->
                pure
                  ( RpcOutcome
                      leading
                      (reverse acc)
                      []
                      (Just (ConnectError GrpcInternal (Just "connect: received compressed message but no compression was negotiated") []))
                  )
            | efEndStream flags -> do
                plain <- decompressFrame flags payload cfg
                let esr = decodeEndStreamLenient plain
                pure (RpcOutcome leading (reverse acc) (esMetadata esr) (esError esr))
            | otherwise -> do
                plain <- decompressFrame flags payload cfg
                case decodeOutputBody codec p plain of
                  Left e -> pure (RpcOutcome leading (reverse acc) [] (Just (decodeFailure e)))
                  Right out ->
                    let acc' = out : acc
                    in case mcap of
                         Just n | length acc' >= n ->
                           pure
                             ( RpcOutcome
                                 leading
                                 (reverse acc')
                                 []
                                 (Just (ConnectError GrpcCancelled (Just "connect: canceled") []))
                             )
                         _ -> go acc'
  go []

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
      h2 = case cccAcceptCompression cfg of
        [] -> h1
        cs -> insertHeader hAcceptEncoding (renderAccept cs) h1
      h3 = case cccTimeoutMs cfg of
        Nothing -> h2
        Just ms -> insertHeader hConnectTimeoutMs (bsPack (show ms)) h2
      h4 =
        if cccSendProtocolVersion cfg
          then insertHeader "connect-protocol-version" connectProtocolVersion h3
          else h3
   in h4

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
        cs -> insertHeader hConnectAcceptEncoding (renderAccept cs) h0
      h2 = case cccTimeoutMs cfg of
        Nothing -> h1
        Just ms -> insertHeader hConnectTimeoutMs (bsPack (show ms)) h1
   in h2

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

-- | Read the next output frame; @Nothing@ at the end-stream frame (throwing
-- if the end-stream carries an error).
readDataFrame
  :: (SupportsClientRpc rpc, Aeson.FromJSON (Output rpc))
  => Codec
  -> Proxy rpc
  -> ConnectClientConfig
  -> FrameReader
  -> IO (Maybe (Output rpc))
readDataFrame codec p cfg fr = do
  mframe <- readFrame fr
  case mframe of
    Nothing -> pure Nothing
    Just (flags, payload)
      | efEndStream flags -> do
          checkEndStream flags payload cfg
          pure Nothing
      | otherwise -> do
          plain <- decompressFrame flags payload cfg
          case decodeOutputBody codec p plain of
            Left e ->
              throwConnectIO $
                ConnectError
                  { ceCode = GrpcInternal
                  , ceMessage = Just (T.pack ("connect: could not decode response frame: " <> e))
                  , ceDetails = []
                  }
            Right out -> pure (Just out)

-- | Read the single output message of a client-streaming response, then the
-- end-stream frame.
readClientStreamOutput
  :: (SupportsClientRpc rpc, Aeson.FromJSON (Output rpc))
  => Codec
  -> Proxy rpc
  -> ConnectClientConfig
  -> FrameReader
  -> IO (Output rpc)
readClientStreamOutput codec p cfg fr = do
  mframe <- readFrame fr
  case mframe of
    Nothing ->
      throwConnectIO $
        ConnectError
          { ceCode = GrpcInternal
          , ceMessage = Just "connect: client stream ended without output"
          , ceDetails = []
          }
    Just (flags, payload)
      | efEndStream flags -> do
          plain <- decompressFrame flags payload cfg
          let esErr = esError (decodeEndStreamLenient plain)
          throwConnectIO $
            fromMaybe
              (ConnectError{ceCode = GrpcInternal, ceMessage = Just "connect: client stream ended without output", ceDetails = []})
              esErr
      | otherwise -> do
          plain <- decompressFrame flags payload cfg
          out <-
            case decodeOutputBody codec p plain of
              Left e ->
                throwConnectIO $
                  ConnectError
                    { ceCode = GrpcInternal
                    , ceMessage = Just (T.pack ("connect: could not decode response: " <> e))
                    , ceDetails = []
                    }
              Right x -> pure x
          -- Read and validate the end-stream frame.
          mEnd <- readFrame fr
          case mEnd of
            Just (ef, ep) | efEndStream ef -> checkEndStream ef ep cfg
            _ -> pure ()
          pure out

-- | Throw if an end-stream frame carries an error (decompressing it first
-- when the frame's compressed bit is set).
checkEndStream :: EnvelopeFlags -> ByteString -> ConnectClientConfig -> IO ()
checkEndStream flags payload cfg = do
  plain <- decompressFrame flags payload cfg
  case esError (decodeEndStreamLenient plain) of
    Just ce -> throwIO (ConnectException ce)
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

decompressResp :: ByteString -> Maybe CC.ContentCoding -> IO ByteString
decompressResp bodyBytes mCoding = case mCoding of
  Nothing -> pure bodyBytes
  Just CC.Identity -> pure bodyBytes
  Just c
    | BS.null bodyBytes -> pure bodyBytes
    | otherwise -> do
        r <- CC.decompress c bodyBytes
        pure $ case r of
          Left _ -> bodyBytes
          Right bs -> bs

-- | Decompress a streaming response frame. The coding is taken from the
-- client's first accepted non-identity coding (sent in
-- @connect-accept-encoding@); the server's @connect-content-encoding@ header
-- isn't exposed here, so we rely on the frame's compressed bit.
decompressFrame :: EnvelopeFlags -> ByteString -> ConnectClientConfig -> IO ByteString
decompressFrame flags payload cfg =
  if efCompressed flags
    then case filter (/= CC.Identity) (cccAcceptCompression cfg) of
      (c : _) -> do
        r <- CC.decompress c payload
        pure $ case r of
          Left _ -> payload
          Right bs -> bs
      [] -> pure payload
    else pure payload

drainBody :: Body -> IO ByteString
drainBody BodyEmpty = pure BS.empty
drainBody (BodyBytes bs) = pure bs
drainBody (BodyStream p) = go []
  where
    go acc =
      p >>= \case
        Nothing -> pure (BS.concat (reverse acc))
        Just chunk -> go (chunk : acc)

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

renderAccept :: [CC.ContentCoding] -> ByteString
renderAccept = BS.intercalate ", " . map CC.codingName

is2xx :: Status -> Bool
is2xx (Status w) = w >= 200 && w < 300

inferError :: Status -> ConnectError
inferError status = ConnectError{ceCode = httpStatusToConnectCode status, ceMessage = Just "connect: server error", ceDetails = []}

-- | Validate a 2xx unary response's @Content-Type@. Connect unary responses
-- must use @application/<codec>@; a different @application/<x>@ is a
-- wrong-codec protocol error ('GrpcInternal'), and anything else (or a
-- missing content-type) is 'GrpcUnknown'. Matches the connectrpc conformance
-- "unexpected responses" expectations.
unaryContentTypeError :: Codec -> Headers -> Maybe ConnectError
unaryContentTypeError codec headers =
  case lookupHeader hContentType headers of
    Nothing -> Just (ctErr GrpcUnknown "connect: response missing content-type")
    Just ct ->
      let base = lowerBS (BS.takeWhile (\c -> c /= 0x3B && c /= 0x20) ct)
          expected = "application/" <> codecToken codec
       in if base == expected
            then Nothing
            else
              if "application/" `BS.isPrefixOf` base
                then Just (ctErr GrpcInternal "connect: unexpected response codec")
                else Just (ctErr GrpcUnknown "connect: unexpected response content-type")
  where
    ctErr c m = ConnectError{ceCode = c, ceMessage = Just m, ceDetails = []}
    lowerBS = BS.map (\w -> if w >= 65 && w <= 90 then w + 32 else w)

throwConnectIO :: ConnectError -> IO a
throwConnectIO ce = throwIO (ConnectException ce)

codecToken :: Codec -> ByteString
codecToken CodecProto = "proto"
codecToken CodecJSON = "json"

bsPack :: String -> ByteString
bsPack = BS.pack . map (fromIntegral . fromEnum)
