-- | Run the interop tests against itself
module Interop.SelfTest (selfTest) where

import Text.Read (readMaybe)
import System.Environment (lookupEnv)

import Network.GRPC.Server.Run

import Interop.Client (runInteropClient)
import Interop.Client.Ping
import Interop.Cmdline
import Interop.Server (withInteropServer)
import Interop.Stress (runStress)

selfTest :: Cmdline -> IO ()
selfTest cmdline = do
    -- Start the server
    withInteropServer cmdline $ \server -> do

      -- Ask the server for its port
      port <- getServerPort server
      let cmdline' = cmdline{cmdMode = Client, cmdPortOverride = Just port}

      -- Give the server a chance to get ready
      --
      -- We could configure the client to automatically reconnect, but this
      -- changes the semantics of doing RPCs (and in a way that the spec says
      -- should /not/ be done by default), so we prefer to do things this way.
      waitReachable cmdline'

      -- @WIREFORM_STRESS=conns,streamsPerConn,itersPerStream@ swaps the
      -- sequential interop client for the concurrent-load driver.
      mspec <- lookupEnv "WIREFORM_STRESS"
      case mspec >>= parseStressSpec of
        Just (c, s, i) -> runStress cmdline' c s i
        Nothing        -> runInteropClient cmdline'

parseStressSpec :: String -> Maybe (Int, Int, Int)
parseStressSpec spec =
    case break (== ',') spec of
      (a, ',' : rest) -> case break (== ',') rest of
        (b, ',' : c) -> (,,) <$> readMaybe a <*> readMaybe b <*> readMaybe c
        _            -> Nothing
      _ -> Nothing
