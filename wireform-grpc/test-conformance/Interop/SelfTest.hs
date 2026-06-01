{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Self-test: starts a server and runs the client test cases against it.
module Interop.SelfTest (
    runSelfTest
  ) where

import Control.Concurrent (forkIO, killThread, yield)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, displayException)
import Control.Exception qualified as E
import Data.Default (def)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Network.Socket qualified as Socket
import System.Exit (exitFailure, exitSuccess)
import System.IO (hSetBuffering, BufferMode(..), stdout)

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

  -- Find an available port by binding to 0, then close and reuse
  port <- findFreePort

  let serverConfig = ServerConfig
        { serverInsecure = Just $ InsecureConfig (Just "127.0.0.1") port
        , serverSecure   = Nothing
        }

  ready <- newEmptyMVar
  server <- mkGrpcServer def interopHandlers

  tid <- forkIO $ do
    putMVar ready ()
    runServerWithHandlers def serverConfig interopHandlers

  -- Wait for the thread to be scheduled
  takeMVar ready

  -- Connect with retry (server may need a moment to bind)
  let serverAddr = ServerInsecure (Address "127.0.0.1" port Nothing)
  connectWithRetry serverAddr 50 $ \conn -> do
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

  killThread tid

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


-- | Find a free port by binding to port 0 and reading back the assigned port.
findFreePort :: IO Socket.PortNumber
findFreePort = do
  let hints = Socket.defaultHints
        { Socket.addrSocketType = Socket.Stream
        , Socket.addrFlags = [Socket.AI_PASSIVE]
        }
  addr:_ <- Socket.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  bracket (Socket.openSocket addr) Socket.close $ \sock -> do
    Socket.setSocketOption sock Socket.ReuseAddr 1
    Socket.bind sock (Socket.addrAddress addr)
    Socket.socketPort sock


-- | Connect with retries using exponential backoff via STM retry.
-- Uses yield-based spinning rather than threadDelay.
connectWithRetry :: Server -> Int -> (Connection -> IO a) -> IO a
connectWithRetry serverAddr 0 k = withConnection def serverAddr k
connectWithRetry serverAddr n k =
  E.catch (withConnection def serverAddr k) $ \(_ :: SomeException) ->
    do yield
       connectWithRetry serverAddr (n - 1) k


runTest :: IORef [(String, Either String ())] -> String -> IO () -> IO ()
runTest ref name action = do
  result <- E.catch (action >> return (Right ()))
    $ \(e :: SomeException) -> return (Left (displayException e))
  modifyIORef' ref ((name, result) :)
  case result of
    Right () -> putStrLn $ "  PASS: " ++ name
    Left err -> putStrLn $ "  FAIL: " ++ name ++ " -- " ++ err
