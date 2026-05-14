{-# LANGUAGE OverloadedStrings #-}
-- | h2spec conformance test runner.
--
-- Starts our HTTP/2 server on an ephemeral port, then runs the h2spec
-- binary against it.  Exits with the h2spec exit code so CI can detect
-- failures.
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (bracket, catch, IOException)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.List (isPrefixOf)
import Network.Socket (AddrInfo(..), AddrInfoFlag(..), SocketType(..), defaultHints, getAddrInfo, accept, bind, close, listen, setSocketOption, SocketOption(..), socketPort, socket, SockAddr)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.Process

import Network.HTTP2.New

----------------------------------------------------------------
-- Test server handler

-- | A minimal HTTP/2 server handler: responds 200 with "hello" body.
-- h2spec only cares about *protocol* conformance, not response content.
echoHandler :: Request -> IO Response
echoHandler _ = return ResponseUnary
    { rspStatus   = 200
    , rspHeaders  = [("content-type", "text/plain")]
    , rspBody     = BC.pack "hello"
    , rspTrailers = []
    }

----------------------------------------------------------------
-- Entry point

main :: IO ()
main = do
    -- Find h2spec binary.
    let h2specBin = "/tmp/h2spec"

    -- Start server on an ephemeral port.
    portVar <- newEmptyMVar
    _ <- forkIO $ withSocketServer portVar echoHandler

    -- Wait for server to be ready.
    port <- takeMVar portVar
    hPutStrLn stderr $ "Server listening on port " ++ show port

    -- Give the server a moment to settle.
    threadDelay 100000  -- 100 ms

    -- Run h2spec.
    let args =
            [ "--host", "127.0.0.1"
            , "--port", show port
            , "--timeout", "2"   -- 2s per test for faster iteration
            ]
    hPutStrLn stderr $ "Running h2spec on port " ++ show port
    -- Redirect our server's stderr away so h2spec output is clean.
    ec <- rawSystem "arch" (["-x86_64", h2specBin] ++ args)
    exitWith ec

----------------------------------------------------------------
-- Server socket setup

withSocketServer :: MVar Int -> (Request -> IO Response) -> IO ()
withSocketServer portVar handler = do
    let hints = defaultHints
            { addrFlags      = [AI_PASSIVE]
            , addrSocketType = Network.Socket.Stream
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
            run defaultServerConfig sock handler
