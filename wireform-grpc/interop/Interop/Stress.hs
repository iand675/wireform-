{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Massive-load concurrency stress driver for the gRPC stack.
--
-- Unlike 'Interop.Client' (which runs one RPC at a time on a fresh
-- connection per test), this fires many concurrent streams across many
-- concurrent connections, all multiplexed and all over TLS. That is the
-- shape that exercises the connection-shared mutable state — the HPACK
-- encoder/decoder dynamic tables, the send lock, and the single OpenSSL
-- @SSL*@ object read by the recv loop while it is written by every
-- stream handler. A single-stream-per-connection driver never hits those
-- races.
module Interop.Stress (runStress) where

import Control.Concurrent.Async (forConcurrently_, replicateConcurrently_)
import Control.Exception
import Control.Monad (replicateM_, when)
import Data.IORef
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Proxy
import System.IO

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Common.StreamElem qualified as StreamElem

import Interop.Client.Connect (testServer)
import Interop.Cmdline

import Proto.API.Interop

-- | @runStress cmdline conns streamsPerConn itersPerStream@ runs
-- @conns@ connections concurrently; each connection multiplexes
-- @streamsPerConn@ concurrent streams; each stream issues
-- @itersPerStream@ sequential unary @EmptyCall@ RPCs. Every failure is
-- logged; a non-zero failure count aborts with 'error' so the test
-- suite exits non-zero.
runStress :: Cmdline -> Int -> Int -> Int -> IO ()
runStress cmdline conns streamsPerConn iters = do
    cats  <- newIORef (Map.empty :: Map.Map String Int)
    total <- newIORef (0 :: Int)
    forConcurrently_ [1 .. conns] $ \_connIx ->
      withConnection def (testServer cmdline) $ \conn ->
        replicateConcurrently_ streamsPerConn $
          replicateM_ iters $ do
            atomicModifyIORef' total (\n -> (n + 1, ()))
            r <- try (oneEmptyCall conn)
            case r of
              Right () -> pure ()
              Left (e :: SomeException) ->
                atomicModifyIORef' cats $ \m ->
                  (Map.insertWith (+) (classify e) 1 m, ())
    t <- readIORef total
    m <- readIORef cats
    let f = sum (Map.elems m)
    hPutStrLn stderr $
      "STRESS_RESULT total=" ++ show t ++ " failures=" ++ show f
    mapM_ (\(k, v) -> hPutStrLn stderr ("  STRESS_CAT " ++ show v ++ "  " ++ k))
          (Map.toList m)
    hFlush stderr
    when (f > 0) $
      error ("stress: " ++ show f ++ "/" ++ show t ++ " RPCs failed")
  where
    classify :: SomeException -> String
    classify e =
      let s = displayException e
          tags = [ "PeerMissingPseudoHeaderStatus"
                 , "ClientStreamConnectionClosed"
                 , "ServerDisconnected"
                 , "tlsSendFn"
                 , "tlsReceiveFn"
                 , "OpenSslError"
                 , "DeadlineExceeded"
                 , "timeout"
                 , "response mismatch"
                 , "no response"
                 , "CompressionError"
                 , "ProtocolError"
                 ]
      in case filter (`isInfixOf` s) tags of
           (t : _) -> t
           []      -> "OTHER: " ++ takeWhile (/= '\n') s

    oneEmptyCall :: Connection -> IO ()
    oneEmptyCall conn =
      withRPC conn def (Proxy @EmptyCall) $ \call -> do
        sendFinalInput call empty
        resp <- StreamElem.value <$> recvOutputWithMeta call
        case resp of
          Just (_meta, r) ->
            when (r /= empty) $
              throwIO (userError "EmptyCall response mismatch")
          Nothing ->
            throwIO (userError "EmptyCall: no response")

    empty :: Proto Empty
    empty = mempty
