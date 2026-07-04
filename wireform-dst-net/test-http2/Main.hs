{-# LANGUAGE OverloadedStrings #-}

-- | Drives the __real vendored HTTP/2 engine__ (client + server, unmodified)
-- over the @wireform-dst@ fault-injecting in-memory link.
--
-- The engine consumes a 'Network.HTTP2.Transport.Transport' — a raw
-- @SendFn@ + pointer-based recv + shutdown. A 'Sim.Net.Link.LinkEnd' already
-- exposes exactly those (@leSendFn@ / @leReceiveFn@ / @leShutdown@), so the
-- production request/response path runs byte-for-byte over the fault link with
-- zero engine changes. We prove: (1) a clean link round-trips a real
-- request/response, (2) injected latency is tolerated (still correct), and
-- (3) a partition actually stalls the live engine.
--
-- No socket-readiness handshake is needed: the link buffers bytes in memory, so
-- the client may send its preface before the server thread starts reading.
module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import System.Timeout (timeout)
import Test.Syd

import Network.HTTP2.Client
import Network.HTTP2.Server
import Network.HTTP2.Transport (Transport (..))
import Sim.Net.Link

-- Bridge one end of the fault link into the HTTP/2 engine's transport seam.
linkTransport :: LinkEnd -> Transport
linkTransport end =
  Transport
    { tSendFn = leSendFn end
    , tRecvBuf = leReceiveFn end
    , tShutdownWrite = leShutdown end
    , tClose = pure ()
    }

-- A trivial server: answer every request 200 "pong".
serverCfg :: ServerConfig
serverCfg =
  defaultServerConfig
    { serverHandler = \_req respond ->
        respond
          defaultResponse
            { responseStatus = 200
            , responseHeaders = [("content-type", "text/plain")]
            , responseBody = ResponseBodyBS "pong"
            }
    }

pingReq :: ClientRequest
pingReq =
  ClientRequest
    { crMethod = "GET"
    , crPath = "/ping"
    , crScheme = "http"
    , crAuthority = "sim"
    , crHeaders = []
    , crBody = ReqBodyNone
    }

-- Fork the real server on the server end, connect the real client on the client
-- end, and hand the caller the live client handle. Server thread is killed on
-- exit (it may be parked in a blocking recv — killThread interrupts the STM).
withLink :: SimLink -> (ClientHandle -> IO a) -> IO a
withLink l action =
  bracket
    (forkIO $ runServerOnTransport serverCfg (linkTransport (slServer l)))
    killThread
    ( \_ ->
        withConnectionOnTransport
          defaultClientConfig
          (linkTransport (slClient l))
          Nothing
          action
    )

-- One real HTTP/2 request; returns (status, full body).
oneRequest :: ClientHandle -> IO (Int, ByteString)
oneRequest h = do
  resp <- sendRequest h pingReq
  body <- drainResponseBody resp
  pure (crStatus resp, body)

main :: IO ()
main =
  sydTest $
    describe "HTTP/2 engine over the wireform-dst fault link" $ do
      it "clean link: a real request/response round-trips" $ do
        l <- newSimLink (Seed 100)
        r <- withLink l $ \h -> timeout 3000000 (oneRequest h)
        r `shouldBe` Just (200, "pong")

      it "latency injection: request still completes correctly" $ do
        l <- newSimLink (Seed 101)
        setLatency (slControl l) ClientToServer (UniformMs 1 3)
        setLatency (slControl l) ServerToClient (UniformMs 1 3)
        r <- withLink l $ \h -> timeout 5000000 (oneRequest h)
        r `shouldBe` Just (200, "pong")

      it "partitioned response direction stalls the live engine" $ do
        l <- newSimLink (Seed 102)
        -- Request flows to the server and it responds, but the response
        -- bytes are held by the partition — the client blocks forever.
        partition (slControl l) ServerToClient
        r <- withLink l $ \h -> timeout 500000 (oneRequest h)
        r `shouldBe` Nothing
