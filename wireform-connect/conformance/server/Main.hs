{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Connect conformance @--mode server@ program: a server-under-test that
-- implements @connectrpc.conformance.v1.ConformanceService@ on top of
-- @wireform-connect@. Reads one 'P.ServerCompatRequest' from stdin, starts a
-- Connect server on an ephemeral port, writes a 'P.ServerCompatResponse' to
-- stdout, then serves (over the Connect protocol, plaintext HTTP\/1.1 or
-- HTTP\/2) until stdin reaches EOF.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Vector qualified as V
import Data.Word (Word32)
import Network.Socket qualified as NS
import System.Exit (exitFailure)
import System.IO (BufferMode (NoBuffering), hSetBinaryMode, hSetBuffering, stderr, stdin, stdout)
import System.IO qualified as IO

import Connect.Conformance.Proto qualified as P
import Connect.Conformance.Service (BidiStream, ClientStream, ConformanceService, IdempotentUnary, ServerStream, Unary, Unimplemented)
import Connect.Conformance.Support (headersToMetadata, metadataToHeaders, packMsgAny, recvMsg, registerConformanceTypes, sendMsg)
import Network.Connect ()
import Network.Connect.Compression (ContentCoding (..))
import Network.Connect.Error (ConnectError (..), ConnectException (..), ErrorDetail (..))
import Network.Connect.Server
  ( ConnectServerConfig (..)
  , ConnectServerM
  , Handlers (..)
  , MethodHandler
  , Service
  , addResponseTrailers
  , connectApplication
  , connectHandlers
  , defaultConnectServerConfig
  , getRequestMetadata
  , method
  , methodUnimplemented
  , requestIsGet
  , requestQueryParams
  , requestTimeoutMs
  , service
  , setResponseMetadata
  )
import Network.GRPC.Spec (GrpcError (..), Proto (..))
import Network.HTTP.Server (ServerConfig (..), defaultServerConfig, runServerOnListener)
import Network.HTTP.VersionRange (VersionRange, http1Only, http2Only, preferHttp2)
import Proto.Decode (decodeMessage)
import Proto.Encode (encodeMessage)
import Proto.Google.Protobuf.Any (Any (..))
import Proto.Google.Protobuf.Any.Util (typeNameFromUrl)

main :: IO ()
main = do
  registerConformanceTypes
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdout NoBuffering
  mreq <- recvMsg stdin
  case mreq of
    Nothing -> exitFailure
    Just bs -> case decodeMessage bs of
      Left e -> IO.hPutStrLn stderr ("conformance-server: bad ServerCompatRequest: " <> show e) >> exitFailure
      Right (scr :: P.ServerCompatRequest) -> runWith scr

runWith :: P.ServerCompatRequest -> IO ()
runWith scr =
  withServerSocket $ \sock port -> do
    let range = versionRangeFor (P.serverCompatRequestHttpVersion scr)
        scfg =
          defaultServerConfig
            { serverHost = "127.0.0.1"
            , serverPort = show port
            , serverVersionRange = range
            , serverHandler = connectApplication conformanceCfg handlers
            }
    let resp =
          P.defaultServerCompatResponse
            { P.serverCompatResponseHost = "127.0.0.1"
            , P.serverCompatResponsePort = fromIntegral port
            , P.serverCompatResponsePemCert = BS.empty
            }
    sendMsg stdout (encodeMessage resp)
    -- The socket is already bound + listening, so the server is reachable now.
    -- Serve the accept loop in the main thread; the conformance runner stops the
    -- server by terminating the process. It closes our stdin right after sending
    -- the request, so we must NOT treat stdin EOF as a shutdown signal.
    runServerOnListener scfg sock

-- | The server advertises every coding it supports so the reference client's
-- requested request\/response compression is honored.
conformanceCfg :: ConnectServerConfig
conformanceCfg = defaultConnectServerConfig {cscSupportedCompression = [Identity, Gzip, Br, Zstd]}

versionRangeFor :: P.HTTPVersion -> VersionRange
versionRangeFor P.HTTPVersion'HttpVersion1 = http1Only
versionRangeFor P.HTTPVersion'HttpVersion2 = http2Only
versionRangeFor _ = preferHttp2


------------------------------------------------------------------------
-- ConformanceService handlers
------------------------------------------------------------------------

conformanceService :: Service ConformanceService ConnectServerM
conformanceService =
  service
    ( method @Unary unaryH
        :& method @IdempotentUnary idempotentH
        :& methodUnimplemented @Unimplemented
        :& method @ServerStream serverStreamH
        :& method @ClientStream clientStreamH
        :& method @BidiStream bidiStreamH
        :& Done
    )

handlers :: [MethodHandler]
handlers = connectHandlers conformanceService

-- Unary / IdempotentUnary share their logic.
unaryH :: Proto P.UnaryRequest -> ConnectServerM (Proto P.UnaryResponse)
unaryH (Proto req) = do
  ri <- buildRequestInfo [packMsgAny req]
  payload <- runUnaryDef (P.unaryRequestResponseDefinition req) ri
  pure (Proto (P.defaultUnaryResponse {P.unaryResponsePayload = Just payload}))

idempotentH :: Proto P.IdempotentUnaryRequest -> ConnectServerM (Proto P.IdempotentUnaryResponse)
idempotentH (Proto req) = do
  ri <- buildRequestInfo [packMsgAny req]
  payload <- runUnaryDef (P.idempotentUnaryRequestResponseDefinition req) ri
  pure (Proto (P.defaultIdempotentUnaryResponse {P.idempotentUnaryResponsePayload = Just payload}))

-- | Apply a 'UnaryResponseDefinition' (shared by Unary, IdempotentUnary, and
-- ClientStream): stage response headers\/trailers, sleep, then either raise the
-- requested error (with the request info appended to its details) or return a
-- payload echoing the request info plus any response data.
runUnaryDef
  :: Maybe P.UnaryResponseDefinition
  -> P.ConformancePayload'RequestInfo
  -> ConnectServerM P.ConformancePayload
runUnaryDef Nothing ri =
  pure (P.defaultConformancePayload {P.conformancePayloadRequestInfo = Just ri})
runUnaryDef (Just def) ri = do
  stageMeta (P.unaryResponseDefinitionResponseHeaders def) (P.unaryResponseDefinitionResponseTrailers def)
  delayMs (P.unaryResponseDefinitionResponseDelayMs def)
  case P.unaryResponseDefinitionResponse def of
    Just (P.UnaryResponseDefinition'Response'Error perr) ->
      liftIO (throwIO (ConnectException (errorWith perr [packMsgAny ri])))
    Just (P.UnaryResponseDefinition'Response'ResponseData d) ->
      pure (P.defaultConformancePayload {P.conformancePayloadData = d, P.conformancePayloadRequestInfo = Just ri})
    Nothing ->
      pure (P.defaultConformancePayload {P.conformancePayloadRequestInfo = Just ri})

-- ServerStream: one request, a stream of responses.
serverStreamH
  :: Proto P.ServerStreamRequest
  -> (Proto P.ServerStreamResponse -> ConnectServerM ())
  -> ConnectServerM ()
serverStreamH (Proto req) send = do
  ri <- buildRequestInfo [packMsgAny req]
  runStreamDef (P.serverStreamRequestResponseDefinition req) ri $ \pay ->
    send (Proto (P.defaultServerStreamResponse {P.serverStreamResponsePayload = Just pay}))

-- ClientStream: many requests, one response.
clientStreamH
  :: ConnectServerM (Maybe (Proto P.ClientStreamRequest))
  -> ConnectServerM (Proto P.ClientStreamResponse)
clientStreamH recv = do
  reqs <- drainRecv recv
  let mdef = case reqs of
        (Proto r0 : _) -> P.clientStreamRequestResponseDefinition r0
        [] -> Nothing
  ri <- buildRequestInfo (map (\(Proto r) -> packMsgAny r) reqs)
  payload <- runUnaryDef mdef ri
  pure (Proto (P.defaultClientStreamResponse {P.clientStreamResponsePayload = Just payload}))

-- BidiStream: faithfully implements the conformance contract for both
-- half- and full-duplex (selected by the first request's @full_duplex@ flag).
--
-- * Full-duplex (@full_duplex = true@): send one response per request as it
--   arrives (interleaved request->response pairs), up to the number of
--   @response_data@ entries. The first response carries the full request
--   info; later ones carry only the requests received since the previous
--   response (the @requests@ list is reset after each send). This is what
--   lets a full-duplex client make progress — the first 'send' flushes the
--   response HEADERS so the client can continue.
-- * Half-duplex (@full_duplex = false@): drain every request, then send all
--   responses (the first carrying the full request info).
--
-- After the receive loop, any remaining responses are flushed, then the
-- requested error (if any) is raised — with the request info appended to its
-- details only when no response messages were sent.
bidiStreamH
  :: ConnectServerM (Maybe (Proto P.BidiStreamRequest))
  -> (Proto P.BidiStreamResponse -> ConnectServerM ())
  -> ConnectServerM ()
bidiStreamH recv send = loopRecv True Nothing False 0 [] 0
  where
    -- loopRecv firstRecv mdef fullDuplex respNum reqsSinceSend responseDelay
    loopRecv firstRecv mdef fullDuplex respNum reqsAcc rdelay = do
      mreq <- recv
      case mreq of
        Nothing -> finish mdef respNum reqsAcc rdelay
        Just (Proto r) -> do
          let reqs' = reqsAcc <> [packMsgAny r]
          (mdef', fullDuplex', rdelay') <-
            if firstRecv
              then do
                let d = P.bidiStreamRequestResponseDefinition r
                    fd = P.bidiStreamRequestFullDuplex r
                case d of
                  Just def -> do
                    stageMeta
                      (P.streamResponseDefinitionResponseHeaders def)
                      (P.streamResponseDefinitionResponseTrailers def)
                    pure (d, fd, P.streamResponseDefinitionResponseDelayMs def)
                  Nothing -> pure (d, fd, 0)
              else pure (mdef, fullDuplex, rdelay)
          if fullDuplex'
            then case mdef' of
              Just def
                | respNum < V.length (P.streamResponseDefinitionResponseData def) -> do
                    ri <-
                      if respNum == 0
                        then buildRequestInfo reqs'
                        else
                          pure
                            P.defaultConformancePayload'RequestInfo
                              { P.conformancePayload'RequestInfoRequests = V.fromList reqs'
                              }
                    delayMs rdelay'
                    sendPayload (P.streamResponseDefinitionResponseData def V.! respNum) (Just ri)
                    loopRecv False mdef' True (respNum + 1) [] rdelay'
              -- full-duplex but no more responses to send: stop receiving.
              _ -> finish mdef' respNum reqs' rdelay'
            else loopRecv False mdef' False respNum reqs' rdelay'

    -- After the client half-closes: flush any remaining responses, then the
    -- error (if any).
    finish Nothing _ _ _ = pure ()
    finish (Just def) respNum reqsAcc rdelay = do
      respNum' <- flushRemaining def respNum reqsAcc rdelay
      case P.streamResponseDefinitionError def of
        Just perr -> do
          extra <-
            if respNum' == 0
              then (\ri -> [packMsgAny ri]) <$> buildRequestInfo reqsAcc
              else pure []
          liftIO (throwIO (ConnectException (errorWith perr extra)))
        Nothing -> pure ()

    flushRemaining def n reqsAcc rdelay
      | n >= V.length (P.streamResponseDefinitionResponseData def) = pure n
      | otherwise = do
          mri <- if n == 0 then Just <$> buildRequestInfo reqsAcc else pure Nothing
          delayMs rdelay
          sendPayload (P.streamResponseDefinitionResponseData def V.! n) mri
          flushRemaining def (n + 1) reqsAcc rdelay

    sendPayload d mri =
      send
        ( Proto
            ( P.defaultBidiStreamResponse
                { P.bidiStreamResponsePayload =
                    Just
                      P.defaultConformancePayload
                        { P.conformancePayloadData = d
                        , P.conformancePayloadRequestInfo = mri
                        }
                }
            )
        )

-- | Apply a 'StreamResponseDefinition': stage headers\/trailers, send each
-- response-data payload (the first carrying the request info), then raise the
-- requested error if present.
runStreamDef
  :: Maybe P.StreamResponseDefinition
  -> P.ConformancePayload'RequestInfo
  -> (P.ConformancePayload -> ConnectServerM ())
  -> ConnectServerM ()
runStreamDef Nothing _ _ = pure ()
runStreamDef (Just def) ri sendPayload = do
  stageMeta (P.streamResponseDefinitionResponseHeaders def) (P.streamResponseDefinitionResponseTrailers def)
  let datas = V.toList (P.streamResponseDefinitionResponseData def)
      delay = P.streamResponseDefinitionResponseDelayMs def
  sendEach delay True datas
  case P.streamResponseDefinitionError def of
    Just perr ->
      let extra = if null datas then [packMsgAny ri] else []
       in liftIO (throwIO (ConnectException (errorWith perr extra)))
    Nothing -> pure ()
  where
    sendEach _ _ [] = pure ()
    sendEach delay first (d : ds) = do
      delayMs delay
      let pay =
            P.defaultConformancePayload
              { P.conformancePayloadData = d
              , P.conformancePayloadRequestInfo = if first then Just ri else Nothing
              }
      sendPayload pay
      sendEach delay False ds

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Build the echoed request info: observed request headers, timeout, the
-- received requests (as @Any@), and — for Connect GET — the query params.
buildRequestInfo :: [Any] -> ConnectServerM P.ConformancePayload'RequestInfo
buildRequestInfo reqs = do
  hdrs <- metadataToHeaders <$> getRequestMetadata
  mto <- requestTimeoutMs
  isGet <- requestIsGet
  qps <- requestQueryParams
  let getInfo
        | isGet = Just (P.defaultConformancePayload'ConnectGetInfo {P.conformancePayload'ConnectGetInfoQueryParams = V.fromList (map qpToHeader qps)})
        | otherwise = Nothing
  pure
    P.defaultConformancePayload'RequestInfo
      { P.conformancePayload'RequestInfoRequestHeaders = V.fromList hdrs
      , P.conformancePayload'RequestInfoTimeoutMs = fmap fromIntegral mto
      , P.conformancePayload'RequestInfoRequests = V.fromList reqs
      , P.conformancePayload'RequestInfoConnectGetInfo = getInfo
      }

qpToHeader :: (ByteString, ByteString) -> P.Header
qpToHeader (k, v) =
  P.defaultHeader {P.headerName = decodeUtf8Lenient k, P.headerValue = V.singleton (decodeUtf8Lenient v)}

-- | Build a Connect error from a conformance 'P.Error', mapping the code and
-- appending the given extra detail @Any@s after the error's own details.
errorWith :: P.Error -> [Any] -> ConnectError
errorWith perr extra =
  ConnectError
    { ceCode = fromConformanceCode (P.errorCode perr)
    , ceMessage = P.errorMessage perr
    , ceDetails = map anyToDetail (V.toList (P.errorDetails perr) <> extra)
    }

anyToDetail :: Any -> ErrorDetail
anyToDetail a = ErrorDetail {edType = typeNameFromUrl (anyTypeUrl a), edValue = anyValue a, edDebug = Nothing}

stageMeta :: V.Vector P.Header -> V.Vector P.Header -> ConnectServerM ()
stageMeta hs ts = do
  setResponseMetadata (headersToMetadata (V.toList hs))
  addResponseTrailers (headersToMetadata (V.toList ts))

delayMs :: Word32 -> ConnectServerM ()
delayMs 0 = pure ()
delayMs ms = liftIO (threadDelay (fromIntegral ms * 1000))

drainRecv :: Monad m => m (Maybe a) -> m [a]
drainRecv recv = go []
  where
    go acc = do
      m <- recv
      case m of
        Nothing -> pure (reverse acc)
        Just x -> go (x : acc)

fromConformanceCode :: P.Code -> GrpcError
fromConformanceCode c = case c of
  P.Code'CodeCanceled -> GrpcCancelled
  P.Code'CodeUnknown -> GrpcUnknown
  P.Code'CodeInvalidArgument -> GrpcInvalidArgument
  P.Code'CodeDeadlineExceeded -> GrpcDeadlineExceeded
  P.Code'CodeNotFound -> GrpcNotFound
  P.Code'CodeAlreadyExists -> GrpcAlreadyExists
  P.Code'CodePermissionDenied -> GrpcPermissionDenied
  P.Code'CodeResourceExhausted -> GrpcResourceExhausted
  P.Code'CodeFailedPrecondition -> GrpcFailedPrecondition
  P.Code'CodeAborted -> GrpcAborted
  P.Code'CodeOutOfRange -> GrpcOutOfRange
  P.Code'CodeUnimplemented -> GrpcUnimplemented
  P.Code'CodeInternal -> GrpcInternal
  P.Code'CodeUnavailable -> GrpcUnavailable
  P.Code'CodeDataLoss -> GrpcDataLoss
  P.Code'CodeUnauthenticated -> GrpcUnauthenticated
  _ -> GrpcUnknown

------------------------------------------------------------------------
-- Ephemeral listener
------------------------------------------------------------------------

withServerSocket :: (NS.Socket -> Int -> IO a) -> IO a
withServerSocket k = do
  let hints = NS.defaultHints {NS.addrFlags = [NS.AI_PASSIVE], NS.addrSocketType = NS.Stream}
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> error "conformance-server: no bind address"
    (addr : _) ->
      bracket (NS.openSocket addr) NS.close $ \sock -> do
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.bind sock (NS.addrAddress addr)
        NS.listen sock 1024
        bound <- NS.getSocketName sock
        let port = case bound of
              NS.SockAddrInet p _ -> fromIntegral p
              _ -> 0 :: Int
        k sock port
