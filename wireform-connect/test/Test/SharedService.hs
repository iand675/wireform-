{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | The headline property of the service vocabulary: ONE 'Service' value —
-- polymorphic in its monad — served over BOTH gRPC (wireform-grpc) and
-- Connect (wireform-connect), exercised with each protocol's native client.
module Test.SharedService (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, finally)
import Control.Monad.IO.Class (MonadIO)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Network.Socket qualified as NS
import Test.Syd

import Data.Default (def)
import Network.GRPC.Client qualified as GClient
import Network.GRPC.Client.StreamType.IO qualified as GClient
import Network.GRPC.Common.NextElem qualified as NextElem
import Network.GRPC.Server qualified as GServer
import Network.GRPC.Server.Run qualified as GServer
import Network.GRPC.Server.Service (fromService)
import Network.GRPC.Spec (NextElem (..), Proto (..))

import Network.Connect.Client
  ( ConnectClient
  , clientStreaming
  , connectionHost
  , connectionPort
  , connectionVersionRange
  , defaultConnectClientConfig
  , defaultConnectionConfig
  , nonStreaming
  , serverStreaming
  , withConnectClient
  )
import Network.Connect.Server
import Network.HTTP.Server (ServerConfig (..), defaultServerConfig, runServerOnListener)
import Network.HTTP.VersionRange (http1Only)

import Connect.TestProto

------------------------------------------------------------------------
-- ONE implementation, ANY transport
------------------------------------------------------------------------

-- | Polymorphic in @m@: instantiates to @IO@ for gRPC and to
-- 'ConnectServerM' for Connect.
elizaShared :: MonadIO m => Service ElizaService m
elizaShared =
  service
    ( method @Say sayH
        :& method @Introduce introduceH
        :& method @Aggregate aggregateH
        :& method @Converse converseH
        :& Done
    )
  where
    sayH (Proto req) =
      pure (Proto defaultSayResponse{sayResponseSentence = "shared:" <> sayRequestSentence req})

    introduceH (Proto req) send =
      mapM_
        (\i -> send (Proto defaultIntroduceResponse{introduceResponseSentence = introduceRequestName req <> T.pack (show i)}))
        ([1, 2, 3] :: [Int])

    aggregateH recv = go []
      where
        go acc = do
          m <- recv
          case m of
            Nothing -> pure (Proto defaultSayResponse{sayResponseSentence = T.intercalate "," (reverse acc)})
            Just (Proto r) -> go (sayRequestSentence r : acc)

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
-- Tests
------------------------------------------------------------------------

tests :: Spec
tests =
  describe "SharedService" $ do
    describe "over gRPC (grapesy client)" $ do
      it "unary Say" $
        withGrpc $ \conn -> do
          Proto resp <- GClient.nonStreaming conn (GClient.rpc @Say) (mkSay "one")
          sayResponseSentence resp `shouldBe` "shared:one"
      it "server streaming Introduce" $
        withGrpc $ \conn -> do
          names <- GClient.serverStreaming conn (GClient.rpc @Introduce) (mkIntro "n") $ \recv ->
            NextElem.collect recv
          map (introduceResponseSentence . getP) names `shouldBe` ["n1", "n2", "n3"]
      it "client streaming Aggregate" $
        withGrpc $ \conn -> do
          Proto resp <- GClient.clientStreaming_ conn (GClient.rpc @Aggregate) $ \send -> do
            send (NextElem (mkSay "a"))
            send (NextElem (mkSay "b"))
            send NoNextElem
          sayResponseSentence resp `shouldBe` "a,b"
      it "bidi Converse" $
        withGrpc $ \conn -> do
          resps <- GClient.biDiStreaming conn (GClient.rpc @Converse) $ \send recv -> do
            send (NextElem (mkConv "x"))
            Proto r1 <- expectElem =<< recv
            send NoNextElem
            pure [converseResponseSentence r1]
          resps `shouldBe` ["re:x"]

    describe "over Connect (connect client)" $ do
      it "unary Say" $ do
        Proto resp <- withConnect $ \cl ->
          nonStreaming cl (Proxy @Say) (mkSay "one")
        sayResponseSentence resp `shouldBe` "shared:one"
      it "server streaming Introduce" $ do
        names <- withConnect $ \cl ->
          serverStreaming cl (Proxy @Introduce) (mkIntro "n") collectAll
        map (introduceResponseSentence . getP) names `shouldBe` ["n1", "n2", "n3"]
      it "client streaming Aggregate" $ do
        Proto resp <- withConnect $ \cl ->
          clientStreaming cl (Proxy @Aggregate) $ \send -> do
            send (mkSay "a")
            send (mkSay "b")
        sayResponseSentence resp `shouldBe` "a,b"

------------------------------------------------------------------------
-- Harnesses
------------------------------------------------------------------------

getP :: Proto a -> a
getP (Proto a) = a

mkSay :: Text -> Proto SayRequest
mkSay s = Proto defaultSayRequest{sayRequestSentence = s}

mkIntro :: Text -> Proto IntroduceRequest
mkIntro n = Proto defaultIntroduceRequest{introduceRequestName = n}

mkConv :: Text -> Proto ConverseRequest
mkConv s = Proto defaultConverseRequest{converseRequestSentence = s}

expectElem :: NextElem a -> IO a
expectElem NoNextElem = expectationFailure "unexpected end of stream" >> error "unreachable"
expectElem (NextElem a) = pure a

collectAll :: IO (Maybe a) -> IO [a]
collectAll recv = go []
  where
    go acc = do
      m <- recv
      case m of
        Nothing -> pure (reverse acc)
        Just x -> go (x : acc)

-- | Serve 'elizaShared' with grapesy (@m ~ IO@); run the client against it.
withGrpc :: (GClient.Connection -> IO a) -> IO a
withGrpc k = do
  server <- GServer.mkGrpcServer def (fromService elizaShared)
  GServer.forkServer def serverConfig server $ \running -> do
    port <- GServer.getServerPort running
    let addr =
          GClient.ServerInsecure
            GClient.Address
              { GClient.addressHost = "127.0.0.1"
              , GClient.addressPort = port
              , GClient.addressAuthority = Nothing
              }
    GClient.withConnection def addr k
  where
    serverConfig =
      GServer.ServerConfig
        { GServer.serverInsecure = Just (GServer.InsecureConfig (Just "127.0.0.1") 0)
        , GServer.serverSecure = Nothing
        }

-- | Serve 'elizaShared' with wireform-connect (@m ~ ConnectServerM@); run
-- the Connect client against it.
withConnect :: (ConnectClient -> IO a) -> IO a
withConnect action =
  withServerSocket $ \sock port -> do
    let scfg =
          defaultServerConfig
            { serverHost = "127.0.0.1"
            , serverPort = show port
            , serverVersionRange = http1Only
            , serverHandler = connectApplication defaultConnectServerConfig (connectHandlers elizaShared)
            }
    readyVar <- newEmptyMVar
    tid <- forkIO (putMVar readyVar () >> runServerOnListener scfg sock)
    takeMVar readyVar
    let conncfg =
          defaultConnectionConfig
            { connectionHost = "127.0.0.1"
            , connectionPort = show port
            , connectionVersionRange = http1Only
            }
    withConnectClient defaultConnectClientConfig conncfg action `finally` killThread tid

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
