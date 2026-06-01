{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Self-test: starts a server and runs the client test cases against it.
module Interop.SelfTest (
    runSelfTest
  ) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.MVar (MVar)
import Control.Exception (SomeException, bracket, catch, displayException)
import Data.Default (def)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import System.Exit (exitFailure, exitSuccess)

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Server (mkGrpcServer)
import Network.GRPC.Server.Run

import Interop.Client
import Interop.Server


runSelfTest :: IO ()
runSelfTest = do
  -- Use a high port to avoid conflicts
  let port = 50888
      serverConfig = ServerConfig
        { serverInsecure = Just $ InsecureConfig Nothing port
        , serverSecure   = Nothing
        }
  -- MVar to signal when server is ready
  ready <- newEmptyMVar
  results <- newIORef ([] :: [(String, Either String ())])

  -- Start server in background
  bracket
    (do server <- mkGrpcServer def interopHandlers
        tid <- forkIO $ do
          putMVar ready ()
          runServer def serverConfig server
        return (tid, server))
    (\_ -> return ())
    $ \_ -> do
      -- Wait for server to be ready
      takeMVar ready

      -- Connect and run tests
      let addr = Address "127.0.0.1" port Nothing
          server = ServerInsecure addr
      withConnection def server $ \conn -> do
        runTest results "empty_unary"                 (testEmptyUnary conn)
        runTest results "large_unary"                 (testLargeUnary conn)
        runTest results "client_streaming"            (testClientStreaming conn)
        runTest results "server_streaming"            (testServerStreaming conn)
        runTest results "ping_pong"                   (testPingPong conn)
        runTest results "empty_stream"                (testEmptyStream conn)
        runTest results "status_code_and_message"     (testStatusCodeAndMessage conn)
        runTest results "unimplemented_method"        (testUnimplementedMethod conn)
        runTest results "unimplemented_service"       (testUnimplementedService conn)
        runTest results "cancel_after_begin"          (testCancelAfterBegin conn)
        runTest results "cancel_after_first_response" (testCancelAfterFirstResponse conn)
        runTest results "timeout_on_sleeping_server"  (testTimeoutOnSleepingServer conn)

  -- Report results
  allResults <- readIORef results
  let passed = length [ () | (_, Right _) <- allResults ]
      failed = length [ () | (_, Left _) <- allResults ]
      total  = length allResults
  putStrLn ""
  putStrLn $ "=== gRPC Interop Test Results ==="
  putStrLn $ show passed ++ "/" ++ show total ++ " passed"
  mapM_ (\(name, result) -> case result of
    Right () -> putStrLn $ "  PASS: " ++ name
    Left err -> putStrLn $ "  FAIL: " ++ name ++ " -- " ++ err
    ) (reverse allResults)
  if failed > 0
    then exitFailure
    else exitSuccess


runTest :: IORef [(String, Either String ())] -> String -> IO () -> IO ()
runTest ref name action = do
  result <- catch (action >> return (Right ()))
    $ \(e :: SomeException) -> return (Left (displayException e))
  modifyIORef' ref ((name, result) :)
  case result of
    Right () -> putStrLn $ "  PASS: " ++ name
    Left err -> putStrLn $ "  FAIL: " ++ name ++ " -- " ++ err
