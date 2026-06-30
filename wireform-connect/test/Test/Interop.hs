{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Opt-in interop against the public @demo.connectrpc.com@ Eliza service.
-- The whole group is skipped unless @CONNECT_DEMO@ is set in the environment.
module Test.Interop (tests) where

import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Network.Connect.Client
import Network.Connect.Protocol (Codec (..))
import Network.GRPC.Spec (Proto (..), getProto)
import Network.HTTP.VersionRange (http2Only)
import System.Environment (lookupEnv)
import Test.Syd

import Connect.TestProto

tests :: Spec
tests = describe "Interop (demo.connectrpc.com)" $ do
  enabled <- runIO (isJust <$> lookupEnv "CONNECT_DEMO")
  if not enabled
    then it "skipped (set CONNECT_DEMO=1 to run)" (pure () :: IO ())
    else do
      it "unary Say (proto)" (demoSay CodecProto)
      it "unary Say (json)" (demoSay CodecJSON)
      it "server-streaming Introduce (proto)" (demoIntroduce CodecProto)
      it "server-streaming Introduce (json)" (demoIntroduce CodecJSON)

demoSay :: Codec -> IO ()
demoSay codec = do
  out <- withDemo codec $ \cl ->
    nonStreaming cl (Proxy @Say) (Proto defaultSayRequest{sayRequestSentence = "Hello from wireform-connect"})
  T.null (sayResponseSentence (getProto out)) `shouldBe` False

demoIntroduce :: Codec -> IO ()
demoIntroduce codec = do
  rs <- withDemo codec $ \cl ->
    serverStreaming cl (Proxy @Introduce) (Proto defaultIntroduceRequest{introduceRequestName = "wireform"}) drain
  -- The reference Eliza streams one or more introduction sentences.
  null rs `shouldBe` False

withDemo :: Codec -> (ConnectClient -> IO a) -> IO a
withDemo codec = withConnectClient ccfg conncfg
  where
    ccfg = defaultConnectClientConfig{cccCodec = codec}
    conncfg =
      defaultConnectionConfig
        { connectionHost = "demo.connectrpc.com"
        , connectionPort = "443"
        , connectionVersionRange = http2Only
        , connectionTls = Just (defaultTlsConnectionConfig "demo.connectrpc.com")
        }

drain :: IO (Maybe a) -> IO [a]
drain recv = go []
  where
    go acc = recv >>= maybe (pure (reverse acc)) (\x -> go (x : acc))
