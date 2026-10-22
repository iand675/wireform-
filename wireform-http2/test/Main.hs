{-# LANGUAGE OverloadedStrings #-}
-- | h2spec conformance test runner.
--
-- Starts the wireform-http2 server on an ephemeral port and runs the
-- @h2spec@ binary against it.  Exits with the @h2spec@ exit code so CI
-- can detect failures.  When the @h2spec@ binary isn't installed (the
-- common dev path) we skip with success — the rest of the wireform
-- HTTP test surface lives in @wireform-http@.
module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BC
import Network.Socket
  ( AddrInfo (..), AddrInfoFlag (..), SocketType (..)
  , defaultHints, getAddrInfo, bind, close, listen
  , setSocketOption, SocketOption (..), socketPort, socket
  )
import System.Directory (findExecutable)
import System.Exit (exitWith, ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.Process (rawSystem)

import Network.HTTP2.Server

----------------------------------------------------------------
-- Test server handler

-- | A minimal HTTP/2 server handler: responds 200 with "hello" body.
-- h2spec only cares about *protocol* conformance, not response content.
echoHandler :: Request -> (Response -> IO ()) -> IO ()
echoHandler _ respond = respond defaultResponse
  { responseStatus  = 200
  , responseHeaders = [("content-type", "text/plain")]
  , responseBody    = ResponseBodyBS (BC.pack "hello")
  }

----------------------------------------------------------------
-- Entry point

main :: IO ()
main = do
  h2specBin <- findExecutable "h2spec"
  case h2specBin of
    Nothing -> do
      hPutStrLn stderr "h2spec not on PATH; skipping conformance test."
      exitWith ExitSuccess
    Just bin -> do
      portVar <- newEmptyMVar
      _ <- forkIO $ withSocketServer portVar echoHandler
      port <- takeMVar portVar
      hPutStrLn stderr $ "Server listening on port " ++ show port
      let args =
            [ "--host", "127.0.0.1"
            , "--port", show port
            , "--timeout", "2"
            ]
      hPutStrLn stderr $ "Running h2spec on port " ++ show port
      ec <- rawSystem bin args
      exitWith ec

----------------------------------------------------------------
-- Server socket setup

withSocketServer :: MVar Int -> (Request -> (Response -> IO ()) -> IO ()) -> IO ()
withSocketServer portVar handler = do
  let hints = defaultHints
        { addrFlags      = [AI_PASSIVE]
        , addrSocketType = Stream
        }
  addrs <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  let addr = head addrs
  bracket
    (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
    close
    $ \sock -> do
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        listen sock 10
        port <- socketPort sock
        putMVar portVar (fromIntegral port)
        runServerOnSocket
          defaultServerConfig { serverHandler = handler }
          sock
