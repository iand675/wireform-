{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Self-test: starts a server and runs the client test cases against it.
module Interop.SelfTest (
    runSelfTest
  ) where

import Control.Exception (SomeException, displayException)
import Control.Exception qualified as E
import Data.Default (def)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import System.Exit (exitFailure, exitSuccess)
import System.IO (hSetBuffering, BufferMode(..), stdout, hFlush)

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Server (mkGrpcServer)
import Network.GRPC.Server.Run

import Interop.Client
import Interop.Server


runSelfTest :: IO ()
runSelfTest = do
  hSetBuffering stdout LineBuffering
  results <- newIORef ([] :: [(String, Either String ())])

  let serverConfig = ServerConfig
        { serverInsecure = Just $ InsecureConfig (Just "127.0.0.1") 50051
        , serverSecure   = Nothing
        }

  server <- mkGrpcServer def interopHandlers
  forkServer def serverConfig server $ \running -> do
    port <- getServerPort running
    let addr = Address "127.0.0.1" port Nothing
        serverAddr = ServerInsecure addr
    withConnection def serverAddr $ \conn -> do
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
  result <- E.catch (action >> return (Right ()))
    $ \(e :: SomeException) -> return (Left (displayException e))
  modifyIORef' ref ((name, result) :)
  case result of
    Right () -> putStrLn $ "  PASS: " ++ name
    Left err -> putStrLn $ "  FAIL: " ++ name ++ " -- " ++ err
