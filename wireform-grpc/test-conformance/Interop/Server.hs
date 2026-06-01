{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Interop test server implementing grpc.testing.TestService.
module Interop.Server (
    interopHandlers
  ) where

import Control.Exception (throwIO)
import Data.ByteString qualified as BS
import Data.Default (def)
import Data.Vector qualified as V
import Data.Word (Word)

import Network.GRPC.Common
import Network.GRPC.Server
import Network.GRPC.Server.StreamType
import Network.GRPC.Spec (toGrpcError)

import Interop.API
import Proto.Interop


interopHandlers :: [SomeRpcHandler IO]
interopHandlers =
  [ handleEmptyCall
  , handleUnaryCall
  , handleStreamingOutputCall
  , handleStreamingInputCall
  , handleFullDuplexCall
  , handleHalfDuplexCall
  ]

handleEmptyCall :: SomeRpcHandler IO
handleEmptyCall = fromMethod @EmptyCall $ mkNonStreaming $ \_req ->
  return defaultEmpty

handleUnaryCall :: SomeRpcHandler IO
handleUnaryCall = fromMethod @UnaryCall $ mkNonStreaming $ \req -> do
  echoStatusIfSet (simpleRequestResponseStatus req)
  let respSize = fromIntegral (simpleRequestResponseSize req)
  return defaultSimpleResponse
    { simpleResponsePayload = Just defaultPayload
        { payloadBody = BS.replicate respSize 0
        }
    }

handleStreamingOutputCall :: SomeRpcHandler IO
handleStreamingOutputCall = fromMethod @StreamingOutputCall $
    mkServerStreaming $ \req send -> do
      echoStatusIfSet (streamingOutputCallRequestResponseStatus req)
      V.mapM_ (sendResponse send) (streamingOutputCallRequestResponseParameters req)

handleStreamingInputCall :: SomeRpcHandler IO
handleStreamingInputCall = fromMethod @StreamingInputCall $
    mkClientStreaming $ \recv -> do
      totalSize <- accumulateInput recv 0
      return defaultStreamingInputCallResponse
        { streamingInputCallResponseAggregatedPayloadSize = fromIntegral totalSize
        }

handleFullDuplexCall :: SomeRpcHandler IO
handleFullDuplexCall = fromMethod @FullDuplexCall $
    mkBiDiStreaming $ \recv send -> do
      bidiLoop recv send

handleHalfDuplexCall :: SomeRpcHandler IO
handleHalfDuplexCall = fromMethod @HalfDuplexCall $
    mkBiDiStreaming $ \recv send -> do
      reqs <- bufferAll recv []
      mapM_ (\req -> do
        echoStatusIfSet (streamingOutputCallRequestResponseStatus req)
        V.mapM_ (sendResponse send) (streamingOutputCallRequestResponseParameters req)
        ) reqs

-- Helpers

echoStatusIfSet :: Maybe EchoStatus -> IO ()
echoStatusIfSet Nothing = return ()
echoStatusIfSet (Just es) = do
  let code = echoStatusCode es
  case toGrpcError (fromIntegral code :: Word) of
    Nothing -> return ()
    Just err -> throwIO $ GrpcException
      { grpcError        = err
      , grpcErrorMessage = Just (echoStatusMessage es)
      , grpcErrorDetails = Nothing
      , grpcErrorMetadata = []
      }

sendResponse :: (NextElem StreamingOutputCallResponse -> IO ()) -> ResponseParameters -> IO ()
sendResponse send rp = do
  let sz = fromIntegral (responseParametersSize rp)
  send $ NextElem defaultStreamingOutputCallResponse
    { streamingOutputCallResponsePayload = Just defaultPayload
        { payloadBody = BS.replicate sz 0
        }
    }

accumulateInput :: IO (NextElem StreamingInputCallRequest) -> Int -> IO Int
accumulateInput recv !acc = do
  next <- recv
  case next of
    NoNextElem -> return acc
    NextElem req -> do
      let sz = case streamingInputCallRequestPayload req of
                 Nothing -> 0
                 Just p  -> BS.length (payloadBody p)
      accumulateInput recv (acc + sz)

bidiLoop :: IO (NextElem StreamingOutputCallRequest) -> (NextElem StreamingOutputCallResponse -> IO ()) -> IO ()
bidiLoop recv send = do
  next <- recv
  case next of
    NoNextElem -> return ()
    NextElem req -> do
      echoStatusIfSet (streamingOutputCallRequestResponseStatus req)
      V.mapM_ (sendResponse send) (streamingOutputCallRequestResponseParameters req)
      bidiLoop recv send

bufferAll :: IO (NextElem a) -> [a] -> IO [a]
bufferAll recv acc = do
  next <- recv
  case next of
    NoNextElem -> return (reverse acc)
    NextElem x -> bufferAll recv (x : acc)
