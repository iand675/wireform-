{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Loopback (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, finally, try)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Hedgehog
import Network.Connect.Client
import Network.Connect.Compression (ContentCoding (..))
import Control.Monad.IO.Class (liftIO)
import Network.Connect.Error (ConnectError (..), ConnectException (..), throwConnect)
import Network.Connect.Protocol (Codec (..))
import Network.Connect.Server
import Network.GRPC.Spec
  ( CustomMetadata (..)
  , GrpcError (..)
  , HeaderName (..)
  , Proto (..)
  )
import Network.HTTP.Server (ServerConfig (..), defaultServerConfig, runServerOnListener)
import Network.HTTP.VersionRange (VersionRange, http1Only, http2Only)
import Network.Socket qualified as NS

import Connect.TestProto

------------------------------------------------------------------------
-- Service implementation
------------------------------------------------------------------------

elizaHandlers :: [MethodHandler]
elizaHandlers =
  [ mkNonStreaming @Say sayH
  , mkServerStreaming @Introduce introduceH
  , mkClientStreaming @Aggregate aggregateH
  , mkBiDiStreaming @Converse converseH
  ]

sayH :: Proto SayRequest -> ConnectServerM (Proto SayResponse)
sayH (Proto req) =
  let s = sayRequestSentence req
   in if s == "boom"
        then liftIO (throwConnect GrpcUnimplemented "intentional boom")
        else do
          md <- getRequestMetadata
          pure (Proto defaultSayResponse{sayResponseSentence = "echo:" <> s <> foldMap echoVal md})

echoVal :: CustomMetadata -> Text
echoVal (CustomMetadata (AsciiHeader "x-echo") v) = TE.decodeUtf8 v
echoVal _ = ""

introduceH :: Proto IntroduceRequest -> (Proto IntroduceResponse -> ConnectServerM ()) -> ConnectServerM ()
introduceH (Proto req) send = mapM_ one ([1, 2, 3] :: [Int])
  where
    nm = introduceRequestName req
    one i = send (Proto defaultIntroduceResponse{introduceResponseSentence = nm <> T.pack (show i)})

aggregateH :: ConnectServerM (Maybe (Proto SayRequest)) -> ConnectServerM (Proto SayResponse)
aggregateH recv = go []
  where
    go acc = do
      m <- recv
      case m of
        Nothing -> pure (Proto defaultSayResponse{sayResponseSentence = T.intercalate "," (reverse acc)})
        Just (Proto r) -> go (sayRequestSentence r : acc)

converseH :: ConnectServerM (Maybe (Proto ConverseRequest)) -> (Proto ConverseResponse -> ConnectServerM ()) -> ConnectServerM ()
converseH recv send = loop
  where
    loop = do
      m <- recv
      case m of
        Nothing -> pure ()
        Just (Proto r) -> do
          send (Proto defaultConverseResponse{converseResponseSentence = "re:" <> converseRequestSentence r})
          loop

------------------------------------------------------------------------
-- Message helpers
------------------------------------------------------------------------

mkSay :: Text -> Proto SayRequest
mkSay s = Proto defaultSayRequest{sayRequestSentence = s}

saidSentence :: Proto SayResponse -> Text
saidSentence (Proto r) = sayResponseSentence r

mkIntro :: Text -> Proto IntroduceRequest
mkIntro n = Proto defaultIntroduceRequest{introduceRequestName = n}

introSentence :: Proto IntroduceResponse -> Text
introSentence (Proto r) = introduceResponseSentence r

mkConv :: Text -> Proto ConverseRequest
mkConv s = Proto defaultConverseRequest{converseRequestSentence = s}

convSentence :: Proto ConverseResponse -> Text
convSentence (Proto r) = converseResponseSentence r

------------------------------------------------------------------------
-- Server / client harness
------------------------------------------------------------------------

withLoopback :: VersionRange -> ConnectClientConfig -> (ConnectClient -> IO a) -> IO a
withLoopback range ccfg action =
  withServerSocket $ \sock port -> do
    let scfg =
          defaultServerConfig
            { serverHost = "127.0.0.1"
            , serverPort = show port
            , serverVersionRange = range
            , serverHandler = connectApplication defaultConnectServerConfig elizaHandlers
            }
    readyVar <- newEmptyMVar
    tid <- forkIO (putMVar readyVar () >> runServerOnListener scfg sock)
    takeMVar readyVar
    let conncfg =
          defaultConnectionConfig
            { connectionHost = "127.0.0.1"
            , connectionPort = show port
            , connectionVersionRange = range
            }
    withConnectClient ccfg conncfg action `finally` killThread tid

withServerSocket :: (NS.Socket -> Int -> IO a) -> IO a
withServerSocket k = do
  let hints = NS.defaultHints{NS.addrFlags = [NS.AI_PASSIVE], NS.addrSocketType = NS.Stream}
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> error "no addr available for test bind"
    (addr : _) ->
      bracket (NS.openSocket addr) NS.close $ \sock -> do
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.bind sock (NS.addrAddress addr)
        NS.listen sock 128
        bound <- NS.getSocketName sock
        let port = case bound of
              NS.SockAddrInet p _ -> fromIntegral p
              _ -> 0 :: Int
        k sock port

clientCfg :: Codec -> ConnectClientConfig
clientCfg codec = defaultConnectClientConfig{cccCodec = codec}

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

tests :: Group
tests =
  Group
    "Loopback"
    [ ("unary Say echo (proto)", withTests 1 (property (sayEcho CodecProto)))
    , ("unary Say echo (json)", withTests 1 (property (sayEcho CodecJSON)))
    , ("unary GET Say (proto)", withTests 1 (property (sayGet CodecProto)))
    , ("unary GET Say (json)", withTests 1 (property (sayGet CodecJSON)))
    , ("server streaming Introduce", withTests 1 (property (introduceTest CodecProto)))
    , ("client streaming Aggregate", withTests 1 (property (aggregateTest CodecJSON)))
    , ("bidi Converse over http2", withTests 1 (property converseTest))
    , ("error path surfaces ConnectException", withTests 1 (property errorTest))
    , ("gzip-compressed request round-trips", withTests 1 (property gzipTest))
    , ("leading metadata reaches the handler", withTests 1 (property metadataTest))
    ]

sayEcho :: Codec -> PropertyT IO ()
sayEcho codec = do
  out <- evalIO $ withLoopback http1Only (clientCfg codec) $ \cl ->
    nonStreaming cl (Proxy @Say) (mkSay "hi")
  saidSentence out === "echo:hi"

sayGet :: Codec -> PropertyT IO ()
sayGet codec = do
  out <- evalIO $ withLoopback http1Only (clientCfg codec) $ \cl ->
    nonStreamingGet cl (Proxy @Say) (mkSay "hi")
  saidSentence out === "echo:hi"

introduceTest :: Codec -> PropertyT IO ()
introduceTest codec = do
  rs <- evalIO $ withLoopback http1Only (clientCfg codec) $ \cl ->
    serverStreaming cl (Proxy @Introduce) (mkIntro "X") (drainAll introSentence)
  rs === ["X1", "X2", "X3"]

aggregateTest :: Codec -> PropertyT IO ()
aggregateTest codec = do
  out <- evalIO $ withLoopback http1Only (clientCfg codec) $ \cl ->
    clientStreaming cl (Proxy @Aggregate) (\send -> mapM_ (send . mkSay) ["a", "b", "c"])
  saidSentence out === "a,b,c"

converseTest :: PropertyT IO ()
converseTest = do
  rs <- evalIO $ withLoopback http2Only (clientCfg CodecProto) $ \cl ->
    biDiStreaming cl (Proxy @Converse) $ \send recv -> do
      send (mkConv "a")
      r1 <- recv
      send (mkConv "b")
      r2 <- recv
      pure (mapMaybe (fmap convSentence) [r1, r2])
  rs === ["re:a", "re:b"]

errorTest :: PropertyT IO ()
errorTest = do
  res <- evalIO $ withLoopback http1Only (clientCfg CodecProto) $ \cl ->
    try (nonStreaming cl (Proxy @Say) (mkSay "boom"))
  case (res :: Either ConnectException (Proto SayResponse)) of
    Left (ConnectException ce) -> ceCode ce === GrpcUnimplemented
    Right _ -> failure

gzipTest :: PropertyT IO ()
gzipTest = do
  out <- evalIO $ withLoopback http1Only (clientCfg CodecProto){cccRequestCompression = Gzip} $ \cl ->
    nonStreaming cl (Proxy @Say) (mkSay "zzz")
  saidSentence out === "echo:zzz"

metadataTest :: PropertyT IO ()
metadataTest = do
  let cfg = (clientCfg CodecProto){cccMetadata = [CustomMetadata (AsciiHeader "x-echo") "!"]}
  out <- evalIO $ withLoopback http1Only cfg $ \cl ->
    nonStreaming cl (Proxy @Say) (mkSay "hi")
  saidSentence out === "echo:hi!"

-- Drain a server-stream recv action into a list, projecting each element.
drainAll :: (a -> b) -> IO (Maybe a) -> IO [b]
drainAll proj recv = go []
  where
    go acc = do
      m <- recv
      case m of
        Nothing -> pure (reverse acc)
        Just x -> go (proj x : acc)
