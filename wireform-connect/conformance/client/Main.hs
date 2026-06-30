{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Connect conformance @--mode client@ program: a client-under-test that
-- issues RPCs to the reference server using @wireform-connect@'s client. Reads
-- size-delimited 'P.ClientCompatRequest' messages from stdin, performs each RPC
-- against @connectrpc.conformance.v1.ConformanceService@, and writes a
-- 'P.ClientCompatResponse' to stdout. Exits on stdin EOF.
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Concurrent.STM (atomically, check, modifyTVar', newTVarIO, readTVar)
import Control.Exception (SomeException, bracket_, finally, throwIO, try)
import System.Timeout (timeout)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import System.IO (hSetBinaryMode, stdin, stdout)

import Connect.Conformance.Proto qualified as P
import Connect.Conformance.Service (BidiStream, ClientStream, IdempotentUnary, ServerStream, Unary, Unimplemented)
import Connect.Conformance.Support (headersToMetadata, metadataToHeaders, recvMsg, registerConformanceTypes, sendMsg, toConnectCodec, toContentCoding, unpackMsgAny)
import Network.Connect ()
import Network.Connect.Client
  ( ConnectClient
  , ConnectClientConfig (..)
  , ConnectionConfig (..)
  , RpcOutcome (..)
  , biDiStreamingResolved
  , biDiStreamingResolvedUpTo
  , clientStreamingResolved
  , defaultConnectClientConfig
  , defaultConnectionConfig
  , nonStreamingGetResolved
  , nonStreamingResolved
  , serverStreamingResolved
  , serverStreamingResolvedUpTo
  , withConnectClient
  )
import Network.Connect.Compression qualified as CC
import Network.Connect.Error (ConnectError (..), ErrorDetail (..))
import Network.GRPC.Spec (GrpcError (..), Proto (..), getProto)
import Network.HTTP.VersionRange (VersionRange, http1Only, http2Only, preferHttp2)
import Proto.Decode (MessageDecode)
import Proto.Decode qualified as PD
import Proto.Encode (encodeMessage)
import Proto.Google.Protobuf.Any (Any (..), defaultAny)
import Proto.Google.Protobuf.Any.Util (typeUrlPrefix)
import Proto.Schema (ProtoMessage)

main :: IO ()
main = do
  registerConformanceTypes
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  lock <- newMVar ()
  sem <- newQSem maxConcurrency
  outstanding <- newTVarIO (0 :: Int)
  let writeResp r = withMVar lock (\_ -> sendMsg stdout (encodeMessage r))
      spawn req = do
        atomically (modifyTVar' outstanding (+ 1))
        _ <-
          forkIO $
            bracket_ (waitQSem sem) (signalQSem sem) (handleTimed req >>= writeResp)
              `finally` atomically (modifyTVar' outstanding (subtract 1))
        pure ()
      loop = do
        mreq <- recvMsg stdin
        case mreq of
          Nothing -> pure ()
          Just bs -> do
            either (const (pure ())) spawn (PD.decodeMessage bs)
            loop
  loop
  -- Wait for all in-flight RPCs to report before exiting.
  atomically (readTVar outstanding >>= \n -> check (n == 0))

-- | Bound on concurrent in-flight RPCs (the runner sends ~1500).
maxConcurrency :: Int
maxConcurrency = 64

-- | 'handle' with a per-RPC safety timeout so one stuck RPC cannot leak a
-- thread forever (the runner has its own per-test timeout too).
handleTimed :: P.ClientCompatRequest -> IO P.ClientCompatResponse
handleTimed req = do
  m <- timeout (20 * 1000000) (handle req)
  case m of
    Just r -> pure r
    Nothing ->
      pure
        P.defaultClientCompatResponse
          { P.clientCompatResponseTestName = P.clientCompatRequestTestName req
          , P.clientCompatResponseResult =
              Just (P.ClientCompatResponse'Result'Error (P.defaultClientErrorResult {P.clientErrorResultMessage = "client: RPC timed out"}))
          }

handle :: P.ClientCompatRequest -> IO P.ClientCompatResponse
handle req = do
  let tn = P.clientCompatRequestTestName req
  outcome <- try (runReq req) :: IO (Either SomeException P.ClientResponseResult)
  pure $ case outcome of
    Right res ->
      P.defaultClientCompatResponse
        { P.clientCompatResponseTestName = tn
        , P.clientCompatResponseResult = Just (P.ClientCompatResponse'Result'Response res)
        }
    Left e ->
      P.defaultClientCompatResponse
        { P.clientCompatResponseTestName = tn
        , P.clientCompatResponseResult =
            Just (P.ClientCompatResponse'Result'Error (P.defaultClientErrorResult {P.clientErrorResultMessage = T.pack (show e)}))
        }

runReq :: P.ClientCompatRequest -> IO P.ClientResponseResult
runReq req = do
  codec <- maybe (throwIO (userError "unsupported codec")) pure (toConnectCodec (P.clientCompatRequestCodec req))
  coding <- maybe (throwIO (userError "unsupported compression")) pure (toContentCoding (P.clientCompatRequestCompression req))
  let ccfg =
        defaultConnectClientConfig
          { cccCodec = codec
          , cccRequestCompression = coding
          , cccAcceptCompression = [CC.Identity, coding]
          , cccTimeoutMs = fromIntegral <$> P.clientCompatRequestTimeoutMs req
          , cccMetadata = headersToMetadata (V.toList (P.clientCompatRequestRequestHeaders req))
          }
      connCfg =
        defaultConnectionConfig
          { connectionHost = T.unpack (P.clientCompatRequestHost req)
          , connectionPort = show (P.clientCompatRequestPort req)
          , connectionVersionRange = versionRangeFor (P.clientCompatRequestHttpVersion req)
          }
  withConnectClient ccfg connCfg (applyCancel req . dispatch req)

-- | Apply the requested client cancellation to an in-flight RPC.
--
-- Time-based cancellations abort the RPC after a delay and report
-- @CODE_CANCELED@ — the async exception from 'timeout' tears down the
-- connection, which /is/ the cancellation:
--
--   * @after_close_send_ms = n@ — cancel @n@ ms after the request is sent.
--   * @before_close_send@ / no timing — cancel immediately (@timeout 0@ never
--     starts the call, so nothing is left half-sent).
--
-- @after_num_responses@ is response-count-based and is handled by 'dispatch'
-- (it reads that many responses, then cancels).
withCancellation
  :: Maybe P.ClientCompatRequest'Cancel
  -> IO P.ClientResponseResult
  -> IO P.ClientResponseResult
withCancellation Nothing act = act
withCancellation (Just c) act =
  case P.clientCompatRequest'CancelCancelTiming c of
    Just (P.ClientCompatRequest'Cancel'CancelTiming'AfterNumResponses _) -> act
    Just (P.ClientCompatRequest'Cancel'CancelTiming'AfterCloseSendMs n) ->
      cancelAfter (fromIntegral n * 1000)
    _ -> cancelAfter 0
  where
    cancelAfter us = fromMaybe canceledResult <$> timeout us act
    canceledResult =
      P.defaultClientResponseResult
        { P.clientResponseResultError =
            Just
              ( P.defaultError
                  { P.errorCode = P.Code'CodeCanceled
                  , P.errorMessage = Just "client: RPC canceled"
                  }
              )
        }

versionRangeFor :: P.HTTPVersion -> VersionRange
versionRangeFor P.HTTPVersion'HttpVersion1 = http1Only
versionRangeFor P.HTTPVersion'HttpVersion2 = http2Only
versionRangeFor _ = preferHttp2

-- | The @after_num_responses@ cancellation cap — cancel the RPC after reading
-- this many response messages — if that cancellation mode was requested.
numResponsesCap :: P.ClientCompatRequest -> Maybe Int
numResponsesCap req =
  case P.clientCompatRequestCancel req >>= P.clientCompatRequest'CancelCancelTiming of
    Just (P.ClientCompatRequest'Cancel'CancelTiming'AfterNumResponses n) -> Just (fromIntegral n)
    _ -> Nothing

-- | Whether a cancellation is realized by capping the number of received
-- responses (handled in 'dispatch') rather than by a wall-clock abort. True
-- for @after_num_responses@ and for full-duplex bidi, which receives one
-- response per request before cancelling.
capHandledCancel :: P.ClientCompatRequest -> Bool
capHandledCancel req =
  case P.clientCompatRequestCancel req >>= P.clientCompatRequest'CancelCancelTiming of
    Nothing -> False
    Just (P.ClientCompatRequest'Cancel'CancelTiming'AfterNumResponses _) -> True
    Just _ -> isFullDuplexBidi req

-- | Run an RPC under the requested client cancellation. Cap-handled
-- cancellations are realized inside 'dispatch' by limiting how many responses
-- are read (then reporting CANCELED), so they just run; the rest use the
-- wall-clock 'withCancellation' abort.
applyCancel :: P.ClientCompatRequest -> IO P.ClientResponseResult -> IO P.ClientResponseResult
applyCancel req act
  | capHandledCancel req = act
  | otherwise = withCancellation (P.clientCompatRequestCancel req) act

isFullDuplexBidi :: P.ClientCompatRequest -> Bool
isFullDuplexBidi req =
  P.clientCompatRequestStreamType req == P.StreamType'StreamTypeFullDuplexBidiStream

-- | Response cap for a bidi RPC: @after_num_responses@ as given, or — for a
-- full-duplex bidi cancelled before/after close-send — the request count
-- (full-duplex receives one response per request, then cancels).
bidiResponseCap :: P.ClientCompatRequest -> Maybe Int
bidiResponseCap req =
  case P.clientCompatRequestCancel req >>= P.clientCompatRequest'CancelCancelTiming of
    Just (P.ClientCompatRequest'Cancel'CancelTiming'AfterNumResponses n) -> Just (fromIntegral n)
    Just _ | isFullDuplexBidi req -> Just (V.length (P.clientCompatRequestRequestMessages req))
    _ -> Nothing

dispatch :: P.ClientCompatRequest -> ConnectClient -> IO P.ClientResponseResult
dispatch req cl = do
  let msgs = V.toList (P.clientCompatRequestRequestMessages req)
      useGet = P.clientCompatRequestUseGetHttpMethod req
      method = fromMaybe (methodForStreamType (P.clientCompatRequestStreamType req)) (P.clientCompatRequestMethod req)
  case method of
    "Unary" -> do
      input <- unpack1 @P.UnaryRequest msgs
      o <-
        if useGet
          then nonStreamingGetResolved cl (Proxy @Unary) (Proto input)
          else nonStreamingResolved cl (Proxy @Unary) (Proto input)
      pure (mkResult (P.unaryResponsePayload . getProto) o)
    "IdempotentUnary" -> do
      input <- unpack1 @P.IdempotentUnaryRequest msgs
      o <-
        if useGet
          then nonStreamingGetResolved cl (Proxy @IdempotentUnary) (Proto input)
          else nonStreamingResolved cl (Proxy @IdempotentUnary) (Proto input)
      pure (mkResult (P.idempotentUnaryResponsePayload . getProto) o)
    "Unimplemented" -> do
      input <- unpack1 @P.UnimplementedRequest msgs
      o <- nonStreamingResolved cl (Proxy @Unimplemented) (Proto input)
      pure (mkResult (const Nothing) o)
    "ServerStream" -> do
      input <- unpack1 @P.ServerStreamRequest msgs
      o <- serverStreamingResolvedUpTo cl (Proxy @ServerStream) (numResponsesCap req) (Proto input)
      pure (mkResult (P.serverStreamResponsePayload . getProto) o)
    "ClientStream" -> do
      inputs <- mapM (unpackAnyTo @P.ClientStreamRequest) msgs
      o <- clientStreamingResolved cl (Proxy @ClientStream) (map Proto inputs)
      pure (mkResult (P.clientStreamResponsePayload . getProto) o)
    "BidiStream" -> do
      inputs <- mapM (unpackAnyTo @P.BidiStreamRequest) msgs
      o <- biDiStreamingResolvedUpTo cl (Proxy @BidiStream) (bidiResponseCap req) (map Proto inputs)
      pure (mkResult (P.bidiStreamResponsePayload . getProto) o)
    other -> throwIO (userError ("unknown method: " <> T.unpack other))

methodForStreamType :: P.StreamType -> Text
methodForStreamType st = case st of
  P.StreamType'StreamTypeUnary -> "Unary"
  P.StreamType'StreamTypeClientStream -> "ClientStream"
  P.StreamType'StreamTypeServerStream -> "ServerStream"
  P.StreamType'StreamTypeHalfDuplexBidiStream -> "BidiStream"
  P.StreamType'StreamTypeFullDuplexBidiStream -> "BidiStream"
  _ -> "Unary"

-- | Turn an 'RpcOutcome' into a conformance 'P.ClientResponseResult': echo the
-- response headers/trailers, the (extracted) payloads, and any RPC error.
mkResult :: (a -> Maybe P.ConformancePayload) -> RpcOutcome a -> P.ClientResponseResult
mkResult extract o =
  P.defaultClientResponseResult
    { P.clientResponseResultResponseHeaders = V.fromList (metadataToHeaders (roHeaders o))
    , P.clientResponseResultPayloads = V.fromList (map (fromMaybe P.defaultConformancePayload . extract) (roPayloads o))
    , P.clientResponseResultError = fmap toConformanceError (roError o)
    , P.clientResponseResultResponseTrailers = V.fromList (metadataToHeaders (roTrailers o))
    , P.clientResponseResultNumUnsentRequests = 0
    }

toConformanceError :: ConnectError -> P.Error
toConformanceError ce =
  P.defaultError
    { P.errorCode = toConformanceCode (ceCode ce)
    , P.errorMessage = ceMessage ce
    , P.errorDetails = V.fromList (map detailToAny (ceDetails ce))
    }

detailToAny :: ErrorDetail -> Any
detailToAny d = defaultAny {anyTypeUrl = typeUrlPrefix <> edType d, anyValue = edValue d}

toConformanceCode :: GrpcError -> P.Code
toConformanceCode c = case c of
  GrpcCancelled -> P.Code'CodeCanceled
  GrpcUnknown -> P.Code'CodeUnknown
  GrpcInvalidArgument -> P.Code'CodeInvalidArgument
  GrpcDeadlineExceeded -> P.Code'CodeDeadlineExceeded
  GrpcNotFound -> P.Code'CodeNotFound
  GrpcAlreadyExists -> P.Code'CodeAlreadyExists
  GrpcPermissionDenied -> P.Code'CodePermissionDenied
  GrpcResourceExhausted -> P.Code'CodeResourceExhausted
  GrpcFailedPrecondition -> P.Code'CodeFailedPrecondition
  GrpcAborted -> P.Code'CodeAborted
  GrpcOutOfRange -> P.Code'CodeOutOfRange
  GrpcUnimplemented -> P.Code'CodeUnimplemented
  GrpcInternal -> P.Code'CodeInternal
  GrpcUnavailable -> P.Code'CodeUnavailable
  GrpcDataLoss -> P.Code'CodeDataLoss
  GrpcUnauthenticated -> P.Code'CodeUnauthenticated

unpack1 :: forall a. (ProtoMessage a, MessageDecode a) => [Any] -> IO a
unpack1 (a : _) = unpackAnyTo a
unpack1 [] = throwIO (userError "expected at least one request message")

unpackAnyTo :: forall a. (ProtoMessage a, MessageDecode a) => Any -> IO a
unpackAnyTo a = case unpackMsgAny a of
  Just (Right x) -> pure x
  Just (Left e) -> throwIO (userError ("could not decode request Any: " <> show e))
  Nothing -> throwIO (userError "request Any type mismatch")
