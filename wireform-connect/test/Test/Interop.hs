{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Opt-in interop against the public @demo.connectrpc.com@ Eliza service.
-- Skipped (passes trivially) unless @CONNECT_DEMO@ is set in the environment.
module Test.Interop (tests) where

import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Hedgehog
import Network.Connect.Client
import Network.Connect.Protocol (Codec (..))
import Network.GRPC.Spec (Proto (..), getProto)
import Network.HTTP.VersionRange (http2Only)
import System.Environment (lookupEnv)

import Connect.TestProto

tests :: Group
tests =
  Group
    "Interop"
    [ ("demo.connectrpc.com Say (proto)", withTests 1 (property (demoSay CodecProto)))
    , ("demo.connectrpc.com Say (json)", withTests 1 (property (demoSay CodecJSON)))
    ]

demoSay :: Codec -> PropertyT IO ()
demoSay codec = do
  enabled <- evalIO (lookupEnv "CONNECT_DEMO")
  case enabled of
    Nothing -> success -- skipped without the env gate
    Just _ -> do
      out <- evalIO (callDemo codec)
      assert (not (T.null (sayResponseSentence (getProto out))))

callDemo :: Codec -> IO (Proto SayResponse)
callDemo codec =
  withConnectClient ccfg conncfg $ \cl ->
    nonStreaming cl (Proxy @Say) (Proto defaultSayRequest{sayRequestSentence = "Hello from wireform-connect"})
  where
    ccfg = defaultConnectClientConfig{cccCodec = codec}
    conncfg =
      defaultConnectionConfig
        { connectionHost = "demo.connectrpc.com"
        , connectionPort = "443"
        , connectionVersionRange = http2Only
        , connectionTls = Just (defaultTlsConnectionConfig "demo.connectrpc.com")
        }
