{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Loopback (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, finally, try)
import Control.Monad (forM, forM_)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.Connect.Client
import Network.Connect.Compression (ContentCoding (..))
import Network.Connect.Error (ConnectError (..), ConnectException (..), decodeConnectError, throwConnect)
import Network.Connect.Protocol (Codec (..))
import Network.Connect.Server
import Network.GRPC.Spec
  ( CustomMetadata (..)
  , GrpcError (..)
  , HeaderName (..)
  , Proto (..)
  )
import Network.HTTP.Connection (sendOn)
import Network.HTTP.Message (Request (..), Response (..))
import Network.HTTP.Server (ServerConfig (..), defaultServerConfig, runServerOnListener)
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (mPost)
import Network.HTTP.Types.Status (status501)
import Network.HTTP.Types.Version (pattern HTTP1_1)
import Network.HTTP.VersionRange (VersionRange, http1Only, http2Only)
import Network.Socket qualified as NS
import Test.Syd

import Connect.TestProto

------------------------------------------------------------------------
-- Service implementation
------------------------------------------------------------------------

elizaService :: Service ElizaService ConnectServerM
elizaService =
  service
    ( method @Say sayH
        :& method @Aggregate aggregateH
        :& method @Converse converseH
        :& method @Introduce introduceH
        :& Done
    )

elizaHandlers :: [MethodHandler]
elizaHandlers = connectHandlers elizaService

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

-- The loopback server advertises every coding so request br/zstd is accepted.
serverCfg :: ConnectServerConfig
serverCfg = defaultConnectServerConfig{cscSupportedCompression = [Identity, Gzip, Br, Zstd]}

withLoopback :: VersionRange -> ConnectClientConfig -> (ConnectClient -> IO a) -> IO a
withLoopback range ccfg action =
  withServerSocket $ \sock port -> do
    let scfg =
          defaultServerConfig
            { serverHost = "127.0.0.1"
            , serverPort = show port
            , serverVersionRange = range
            , serverHandler = connectApplication serverCfg elizaHandlers
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

-- Drain a server-stream recv action into a list, projecting each element.
drainAll :: (a -> b) -> IO (Maybe a) -> IO [b]
drainAll proj recv = go []
  where
    go acc = do
      m <- recv
      case m of
        Nothing -> pure (reverse acc)
        Just x -> go (proj x : acc)

-- Read a full response body into one strict ByteString.
drainBody :: Body -> IO ByteString
drainBody BodyEmpty = pure ""
drainBody (BodyBytes bs) = pure bs
drainBody (BodyStream p) = go []
  where
    go acc = p >>= maybe (pure (BS.concat (reverse acc))) (\c -> go (c : acc))

------------------------------------------------------------------------
-- Test matrix
------------------------------------------------------------------------

transports :: [(String, VersionRange)]
transports = [("http1", http1Only), ("http2", http2Only)]

codecs :: [(String, Codec)]
codecs = [("proto", CodecProto), ("json", CodecJSON)]

tests :: Spec
tests =
  describe "Loopback" $ do
    describe "unary Say echo" $
      forM_ transports $ \(tn, range) ->
        forM_ codecs $ \(cn, codec) ->
          it (tn <> "/" <> cn) (unaryEchoT range codec)
    describe "concurrent unary over one http2 connection" $
      forM_ codecs $ \(cn, codec) ->
        it cn (concurrentUnaryT codec)
    describe "unary GET Say" $
      forM_ transports $ \(tn, range) ->
        forM_ codecs $ \(cn, codec) ->
          it (tn <> "/" <> cn) (unaryGetT range codec)
    describe "server streaming Introduce" $
      forM_ transports $ \(tn, range) ->
        forM_ codecs $ \(cn, codec) ->
          it (tn <> "/" <> cn) (serverStreamT range codec)
    describe "client streaming Aggregate" $
      forM_ transports $ \(tn, range) ->
        forM_ codecs $ \(cn, codec) ->
          it (tn <> "/" <> cn) (clientStreamT range codec)
    describe "bidi Converse (http2)" $
      forM_ codecs $ \(cn, codec) ->
        it cn (bidiT codec)
    describe "request compression round-trips (http1)" $
      forM_ [("gzip", Gzip), ("brotli", Br), ("zstd", Zstd)] $ \(cn, coding) ->
        it cn (reqCompressionT coding)
    it "error path surfaces ConnectException" errorT
    it "error path returns HTTP 501 on the wire" rawErrorStatusT
    it "leading metadata reaches the handler" metadataT

unaryEchoT :: VersionRange -> Codec -> IO ()
unaryEchoT range codec = do
  out <- withLoopback range (clientCfg codec) $ \cl ->
    nonStreaming cl (Proxy @Say) (mkSay "hi")
  saidSentence out `shouldBe` "echo:hi"

unaryGetT :: VersionRange -> Codec -> IO ()
unaryGetT range codec = do
  out <- withLoopback range (clientCfg codec) $ \cl ->
    nonStreamingGet cl (Proxy @Say) (mkSay "hi")
  saidSentence out `shouldBe` "echo:hi"

-- | Many unary RPCs multiplexed concurrently over ONE HTTP/2 connection.
-- This is the scenario the HPACK send-ordering fix targets: concurrent
-- streams must keep the encoder's dynamic-table mutation order in lockstep
-- with the wire order (and allocate monotonically-increasing stream ids),
-- or the peer's decoder desyncs / the server rejects an out-of-order id.
-- Each call carries a unique payload; every reply must be that call's own
-- echo.
concurrentUnaryT :: Codec -> IO ()
concurrentUnaryT codec = do
  let inputs = map (T.pack . show) [1 .. 50 :: Int]
  results <- withLoopback http2Only (clientCfg codec) $ \cl -> do
    vars <- forM inputs $ \s -> do
      v <- newEmptyMVar
      _ <- forkIO $ do
        r <-
          try (nonStreaming cl (Proxy @Say) (mkSay s))
            :: IO (Either SomeException (Proto SayResponse))
        putMVar v (s, r)
      pure v
    mapM takeMVar vars
  forM_ results $ \(s, r) -> case r of
    Right out -> saidSentence out `shouldBe` ("echo:" <> s)
    Left e ->
      expectationFailure ("concurrent call " <> T.unpack s <> " failed: " <> show e)

serverStreamT :: VersionRange -> Codec -> IO ()
serverStreamT range codec = do
  rs <- withLoopback range (clientCfg codec) $ \cl ->
    serverStreaming cl (Proxy @Introduce) (mkIntro "X") (drainAll introSentence)
  rs `shouldBe` ["X1", "X2", "X3"]

clientStreamT :: VersionRange -> Codec -> IO ()
clientStreamT range codec = do
  out <- withLoopback range (clientCfg codec) $ \cl ->
    clientStreaming cl (Proxy @Aggregate) (\send -> mapM_ (send . mkSay) ["a", "b", "c"])
  saidSentence out `shouldBe` "a,b,c"

bidiT :: Codec -> IO ()
bidiT codec = do
  rs <- withLoopback http2Only (clientCfg codec) $ \cl ->
    biDiStreaming cl (Proxy @Converse) $ \send recv -> do
      send (mkConv "a")
      r1 <- recv
      send (mkConv "b")
      r2 <- recv
      pure (mapMaybe (fmap convSentence) [r1, r2])
  rs `shouldBe` ["re:a", "re:b"]

reqCompressionT :: ContentCoding -> IO ()
reqCompressionT coding = do
  out <- withLoopback http1Only (clientCfg CodecProto){cccRequestCompression = coding} $ \cl ->
    nonStreaming cl (Proxy @Say) (mkSay "zzz")
  saidSentence out `shouldBe` "echo:zzz"

errorT :: IO ()
errorT = do
  res <- withLoopback http1Only (clientCfg CodecProto) $ \cl ->
    try (nonStreaming cl (Proxy @Say) (mkSay "boom"))
  case (res :: Either ConnectException (Proto SayResponse)) of
    Left (ConnectException ce) -> ceCode ce `shouldBe` GrpcUnimplemented
    Right _ -> expectationFailure "expected ConnectException, got a response"

-- The high-level client derives the code from the JSON error body; this
-- additionally pins the on-the-wire HTTP status the server emits.
rawErrorStatusT :: IO ()
rawErrorStatusT = withLoopback http1Only (clientCfg CodecJSON) $ \cl -> do
  resp <- rawSayJSON cl "boom"
  responseStatus resp `shouldBe` status501
  bs <- drainBody (responseBody resp)
  case Aeson.eitherDecodeStrict bs >>= decodeConnectError of
    Right ce -> ceCode ce `shouldBe` GrpcUnimplemented
    Left e -> expectationFailure ("bad error envelope on the wire: " <> e)

rawSayJSON :: ConnectClient -> Text -> IO Response
rawSayJSON cl sentence = sendOn (clConn cl) req
  where
    body = BL.toStrict (Aeson.encode (Proto defaultSayRequest{sayRequestSentence = sentence}))
    req =
      Request
        { requestMethod = mPost
        , requestTarget = "/connectrpc.eliza.v1.ElizaService/Say"
        , requestAuthority = Just (clAuthority cl)
        , requestScheme = clScheme cl
        , requestHeaders = [(hContentType, "application/json")]
        , requestBody = BodyBytes body
        , requestVersion = HTTP1_1
        , requestTrailers = pure []
        }

metadataT :: IO ()
metadataT = do
  let cfg = (clientCfg CodecProto){cccMetadata = [CustomMetadata (AsciiHeader "x-echo") "!"]}
  out <- withLoopback http1Only cfg $ \cl ->
    nonStreaming cl (Proxy @Say) (mkSay "hi")
  saidSentence out `shouldBe` "echo:hi!"
