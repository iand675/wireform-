{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Interop test client implementing the official gRPC interop test cases.
module Interop.Client (
    testEmptyUnary
  , testLargeUnary
  , testClientStreaming
  , testServerStreaming
  , testPingPong
  , testEmptyStream
  , testStatusCodeAndMessage
  , testUnimplementedMethod
  , testUnimplementedService
  , testCancelAfterBegin
  , testCancelAfterFirstResponse
  , testTimeoutOnSleepingServer
  ) where

import Control.Exception (catch, throwIO)
import Data.ByteString qualified as BS
import Data.Default (def)
import Data.Proxy (Proxy(..))
import Data.Vector qualified as V

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.IO qualified as IO
import Network.GRPC.Common

import Interop.API
import Proto.Interop


testEmptyUnary :: Connection -> IO ()
testEmptyUnary conn = do
  resp <- IO.nonStreaming conn (rpc @EmptyCall) defaultEmpty
  assertEq "empty_unary response" resp defaultEmpty

testLargeUnary :: Connection -> IO ()
testLargeUnary conn = do
  let req = defaultSimpleRequest
        { simpleRequestResponseSize = 314159
        , simpleRequestPayload = Just defaultPayload
            { payloadBody = BS.replicate 271828 0
            }
        }
  resp <- IO.nonStreaming conn (rpc @UnaryCall) req
  case simpleResponsePayload resp of
    Nothing -> assertFail "large_unary: no payload in response"
    Just p  -> assertEq "large_unary payload size"
                 (BS.length (payloadBody p)) 314159

testClientStreaming :: Connection -> IO ()
testClientStreaming conn = do
  let sizes = [27182, 8, 1828, 45904 :: Int]
  resp <- IO.clientStreaming_ conn (rpc @StreamingInputCall) $ \send -> do
    mapM_ (\sz -> send $ NextElem defaultStreamingInputCallRequest
      { streamingInputCallRequestPayload = Just defaultPayload
          { payloadBody = BS.replicate sz 0
          }
      }) sizes
    send NoNextElem
  assertEq "client_streaming aggregated_payload_size"
    (fromIntegral $ streamingInputCallResponseAggregatedPayloadSize resp)
    (74922 :: Int)

testServerStreaming :: Connection -> IO ()
testServerStreaming conn = do
  let expectedSizes = [31415, 9, 2653, 58979 :: Int]
      req = defaultStreamingOutputCallRequest
        { streamingOutputCallRequestResponseParameters =
            V.fromList (fmap (\sz -> defaultResponseParameters
              { responseParametersSize = fromIntegral sz
              }) expectedSizes)
        }
  responses <- IO.serverStreaming conn (rpc @StreamingOutputCall) req $ \recv -> do
    collectAll recv []
  let actualSizes = fmap (maybe 0 (BS.length . payloadBody) .
                          streamingOutputCallResponsePayload) responses
  assertEq "server_streaming response count" (length responses) 4
  assertEq "server_streaming response sizes" actualSizes expectedSizes

testPingPong :: Connection -> IO ()
testPingPong conn = do
  let pairs = [ (31415, 27182)
              , (9, 8)
              , (2653, 1828)
              , (58979, 45904)
              ] :: [(Int, Int)]
  IO.biDiStreaming conn (rpc @FullDuplexCall) $ \send recv -> do
    mapM_ (\(respSize, reqSize) -> do
      send $ NextElem defaultStreamingOutputCallRequest
        { streamingOutputCallRequestResponseParameters =
            V.singleton (defaultResponseParameters { responseParametersSize = fromIntegral respSize })
        , streamingOutputCallRequestPayload = Just defaultPayload
            { payloadBody = BS.replicate reqSize 0
            }
        }
      resp <- recv
      case resp of
        NoNextElem -> assertFail "ping_pong: expected response"
        NextElem r -> case streamingOutputCallResponsePayload r of
          Nothing -> assertFail "ping_pong: no payload"
          Just p  -> assertEq "ping_pong payload size"
                       (BS.length (payloadBody p)) respSize
      ) pairs
    send NoNextElem
    final <- recv
    case final of
      NoNextElem -> return ()
      NextElem _ -> assertFail "ping_pong: unexpected extra response"

testEmptyStream :: Connection -> IO ()
testEmptyStream conn = do
  IO.biDiStreaming conn (rpc @FullDuplexCall) $ \send recv -> do
    send NoNextElem
    resp <- recv
    case resp of
      NoNextElem -> return ()
      NextElem _ -> assertFail "empty_stream: got unexpected response"

testStatusCodeAndMessage :: Connection -> IO ()
testStatusCodeAndMessage conn = do
  let echoSt = defaultEchoStatus
        { echoStatusCode = 2
        , echoStatusMessage = "test status message"
        }
  -- Test via UnaryCall
  let req = defaultSimpleRequest
        { simpleRequestResponseStatus = Just echoSt
        }
  catch (do
    _ <- IO.nonStreaming conn (rpc @UnaryCall) req
    assertFail "status_code_and_message: expected exception (unary)"
    ) $ \(e :: GrpcException) -> do
      assertEq "status_code unary" (grpcError e) GrpcUnknown
      assertEq "status_message unary" (grpcErrorMessage e) (Just "test status message")

  -- Test via FullDuplexCall
  let streamReq = defaultStreamingOutputCallRequest
        { streamingOutputCallRequestResponseStatus = Just echoSt
        }
  catch (do
    IO.biDiStreaming conn (rpc @FullDuplexCall) $ \send recv -> do
      send $ NextElem streamReq
      send NoNextElem
      _ <- recv
      return ()
    assertFail "status_code_and_message: expected exception (bidi)"
    ) $ \(e :: GrpcException) -> do
      assertEq "status_code bidi" (grpcError e) GrpcUnknown
      assertEq "status_message bidi" (grpcErrorMessage e) (Just "test status message")

testUnimplementedMethod :: Connection -> IO ()
testUnimplementedMethod conn = do
  catch (do
    _ <- IO.nonStreaming conn (rpc @UnimplementedCall) defaultEmpty
    assertFail "unimplemented_method: expected exception"
    ) $ \(e :: GrpcException) -> do
      assertEq "unimplemented_method status" (grpcError e) GrpcUnimplemented

testUnimplementedService :: Connection -> IO ()
testUnimplementedService conn = do
  catch (do
    _ <- IO.nonStreaming conn (rpc @UnimplementedServiceCall) defaultEmpty
    assertFail "unimplemented_service: expected exception"
    ) $ \(e :: GrpcException) -> do
      assertEq "unimplemented_service status" (grpcError e) GrpcUnimplemented

testCancelAfterBegin :: Connection -> IO ()
testCancelAfterBegin conn = do
  catch (do
    withRPC conn def (Proxy @StreamingInputCall) $ \call -> do
      throwIO $ GrpcException
        { grpcError        = GrpcCancelled
        , grpcErrorMessage = Just "cancelled"
        , grpcErrorDetails = Nothing
        , grpcErrorMetadata = []
        }
    ) $ \(e :: GrpcException) -> do
      assertEq "cancel_after_begin status" (grpcError e) GrpcCancelled

testCancelAfterFirstResponse :: Connection -> IO ()
testCancelAfterFirstResponse conn = do
  catch (do
    withRPC conn def (Proxy @FullDuplexCall) $ \call -> do
      sendInput call $ StreamElem defaultStreamingOutputCallRequest
        { streamingOutputCallRequestResponseParameters =
            V.singleton (defaultResponseParameters { responseParametersSize = 31415 })
        , streamingOutputCallRequestPayload = Just defaultPayload
            { payloadBody = BS.replicate 27182 0
            }
        }
      _resp <- recvOutput call
      throwIO $ GrpcException
        { grpcError        = GrpcCancelled
        , grpcErrorMessage = Just "cancelled"
        , grpcErrorDetails = Nothing
        , grpcErrorMetadata = []
        }
    ) $ \(e :: GrpcException) -> do
      assertEq "cancel_after_first_response status" (grpcError e) GrpcCancelled

testTimeoutOnSleepingServer :: Connection -> IO ()
testTimeoutOnSleepingServer conn = do
  catch (do
    let params = def { callTimeout = Just (Timeout Millisecond (TimeoutValue 1)) }
    withRPC conn params (Proxy @FullDuplexCall) $ \call -> do
      sendInput call $ StreamElem defaultStreamingOutputCallRequest
        { streamingOutputCallRequestPayload = Just defaultPayload
            { payloadBody = BS.replicate 27182 0
            }
        }
      _ <- recvOutput call
      return ()
    assertFail "timeout: expected exception"
    ) $ \(e :: GrpcException) -> do
      assertEq "timeout status" (grpcError e) GrpcDeadlineExceeded

-- Utility functions

collectAll :: IO (NextElem a) -> [a] -> IO [a]
collectAll recv acc = do
  next <- recv
  case next of
    NoNextElem -> return (reverse acc)
    NextElem x -> collectAll recv (x : acc)

assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq label actual expected
  | actual == expected = return ()
  | otherwise = assertFail $
      label ++ ": expected " ++ show expected ++ " but got " ++ show actual

assertFail :: String -> IO a
assertFail msg = throwIO $ userError $ "ASSERTION FAILED: " ++ msg
