{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | End-to-end echo test.

Binds an ephemeral TCP port, runs the standalone WebSocket
server, connects a client, exchanges a few messages, and tears
both sides down.  Exercises the full pipeline:

* server-side accept loop + handshake parser,
* receive transport (magic ring) driving the streaming frame
  parser,
* send transport with the builder-direct path,
* client-side handshake roundtrip,
* server- vs client-direction masking.
-}
module Test.Echo (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (SomeException, bracket, try)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Network.Socket qualified as NS
import Network.WebSocket.Client
import Network.WebSocket.Handshake
import Network.WebSocket.Message
import Network.WebSocket.PerMessageDeflate qualified as PMD
import Network.WebSocket.Server
import System.Timeout (timeout)
import Test.Syd


tests :: Spec
tests =
  describe "Echo" $
    sequence_
      [ echoText
      , echoBinary
      , echoTextTls
      , echoViaURI
      , subProtocolNegotiation
      , unofferedSubProtocolRejected
      , echoDeflate
      , echoDeflateLargePayload
      , echoDeflateBackToBack
      , echoDeflateClientOffersServerDeclines
      , echoDeflateInflateBombRejected
      ]


------------------------------------------------------------------------
-- Echo: text
------------------------------------------------------------------------

echoText :: Spec
echoText = it "client <-> echo server: text message" $ do
  r <- timeout 10_000_000 $ withEchoServer $ \port -> do
    let cfg = defaultWebSocketClientConfig "127.0.0.1" (show port) "/echo"
    withWebSocketClient cfg $ \conn -> do
      sendTextMessage conn "hello, websocket"
      msg <- receiveMessage conn defaultMessageLimit
      case msg of
        TextMessage t -> t `shouldBe` "hello, websocket"
        other -> expectationFailure ("expected TextMessage, got " <> show other)
  case r of
    Just () -> pure ()
    Nothing -> expectationFailure "echo text timed out after 10s"


echoBinary :: Spec
echoBinary = it "client <-> echo server: binary message" $ do
  r <- timeout 10_000_000 $ withEchoServer $ \port -> do
    let cfg = defaultWebSocketClientConfig "127.0.0.1" (show port) "/echo"
        payload = BS.pack [0 .. 99]
    withWebSocketClient cfg $ \conn -> do
      sendBinaryMessage conn payload
      msg <- receiveMessage conn defaultMessageLimit
      case msg of
        BinaryMessage bs -> bs `shouldBe` payload
        other -> expectationFailure ("expected BinaryMessage, got " <> show other)
  case r of
    Just () -> pure ()
    Nothing -> expectationFailure "echo binary timed out after 10s"


------------------------------------------------------------------------
-- Echo over TLS (wss://)
------------------------------------------------------------------------

echoTextTls :: Spec
echoTextTls = it "wss:// client <-> echo server: text message" $ do
  r <- timeout 20_000_000 $ withEchoServerTls $ \port -> do
    let cfg =
          (defaultWebSocketClientConfig "127.0.0.1" (show port) "/echo")
            { wcTls =
                Just
                  ( wsTlsDefault
                      { wctVerifyPeer = False -- self-signed in fixture
                      }
                  )
            }
    withWebSocketClient cfg $ \conn -> do
      sendTextMessage conn "tls hello"
      msg <- receiveMessage conn defaultMessageLimit
      case msg of
        TextMessage t -> t `shouldBe` "tls hello"
        other -> expectationFailure ("expected TextMessage, got " <> show other)
  case r of
    Just () -> pure ()
    Nothing -> expectationFailure "TLS echo test timed out after 20s"


------------------------------------------------------------------------
-- URI-based connect
------------------------------------------------------------------------

echoViaURI :: Spec
echoViaURI = it "withWebSocketClientURI: ws:// round-trip" $ do
  r <- timeout 10_000_000 $ withEchoServer $ \port -> do
    let uri = "ws://127.0.0.1:" <> BS8.pack (show port) <> "/echo"
    withWebSocketClientURI uri $ \conn -> do
      sendTextMessage conn "uri-routed"
      msg <- receiveMessage conn defaultMessageLimit
      case msg of
        TextMessage t -> t `shouldBe` "uri-routed"
        other -> expectationFailure ("expected TextMessage, got " <> show other)
  case r of
    Just () -> pure ()
    Nothing -> expectationFailure "URI echo timed out"


------------------------------------------------------------------------
-- Sub-protocol negotiation
------------------------------------------------------------------------

subProtocolNegotiation :: Spec
subProtocolNegotiation =
  it "client sees the sub-protocol the server selected" $ do
    r <- timeout 10_000_000 $
      withEchoServerProto $ \port -> do
        let cfg =
              (defaultWebSocketClientConfig "127.0.0.1" (show port) "/echo")
                { wcSubProtocols = ["chat.v2", "chat.v1"]
                }
        withWebSocketClient' cfg $ \shr _conn ->
          shrSelectedProtocol shr `shouldBe` Just "chat.v2"
    case r of
      Just () -> pure ()
      Nothing -> expectationFailure "sub-protocol test timed out"


unofferedSubProtocolRejected :: Spec
unofferedSubProtocolRejected =
  it "client rejects a server-selected protocol it did not offer" $ do
    -- Server config forces the selection regardless of what the
    -- client offered — emulates a misbehaving server.
    r <- timeout 10_000_000 $
      withForcedProtoServer "imposed-protocol" $ \port -> do
        let cfg =
              (defaultWebSocketClientConfig "127.0.0.1" (show port) "/echo")
                { wcSubProtocols = ["something-else"]
                }
        result <- try $ withWebSocketClient cfg $ \_ -> pure ()
        case result :: Either SomeException () of
          Left _ -> pure () -- handshake validation throws
          Right _ ->
            expectationFailure
              "expected client to reject server's unoffered sub-protocol"
    case r of
      Just () -> pure ()
      Nothing -> expectationFailure "rejection test timed out"


{- | Echo server that picks the first offered sub-protocol via the
'wscSelectSubProtocol' callback.
-}
withEchoServerProto :: (Int -> IO a) -> IO a
withEchoServerProto = withServerCfgVariant cfg
  where
    cfg =
      defaultWebSocketServerConfig
        { wscHandler = echoHandler
        , wscSelectSubProtocol = \req ->
            case wsReqProtocols req of
              (p : _) -> Just p
              [] -> Nothing
        }


{- | Echo server that always selects @forced@ as the sub-protocol,
regardless of what the client offered.  Lets us drive the
client's negative-path validation.
-}
withForcedProtoServer :: BS.ByteString -> (Int -> IO a) -> IO a
withForcedProtoServer forced = withServerCfgVariant cfg
  where
    cfg =
      defaultWebSocketServerConfig
        { wscHandler = echoHandler
        , wscSelectSubProtocol = \_ -> Just forced
        }


-- | Plumbing shared by the proto-server fixtures.
withServerCfgVariant
  :: WebSocketServerConfig
  -> (Int -> IO a)
  -> IO a
withServerCfgVariant cfg action = do
  let hints =
        NS.defaultHints
          { NS.addrFlags = [NS.AI_PASSIVE]
          , NS.addrSocketType = NS.Stream
          }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  let addr = case addrs of
        [] -> error "no addr"
        (a : _) -> a
  bracket (NS.openSocket addr) NS.close $ \sock -> do
    NS.setSocketOption sock NS.ReuseAddr 1
    NS.bind sock (NS.addrAddress addr)
    NS.listen sock 16
    boundAddr <- NS.getSocketName sock
    port <- case boundAddr of
      NS.SockAddrInet p _ -> pure (fromIntegral p :: Int)
      NS.SockAddrInet6 p _ _ _ -> pure (fromIntegral p :: Int)
      _ -> error "unexpected sockaddr"
    tid <- forkIO $ runWebSocketServerOnListener cfg sock
    a <- action port
    killThread tid
    pure a


withEchoServerTls :: (Int -> IO a) -> IO a
withEchoServerTls action = do
  let hints =
        NS.defaultHints
          { NS.addrFlags = [NS.AI_PASSIVE]
          , NS.addrSocketType = NS.Stream
          }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  let addr = case addrs of
        [] -> error "no addr"
        (a : _) -> a
  bracket (NS.openSocket addr) NS.close $ \sock -> do
    NS.setSocketOption sock NS.ReuseAddr 1
    NS.bind sock (NS.addrAddress addr)
    NS.listen sock 16
    boundAddr <- NS.getSocketName sock
    port <- case boundAddr of
      NS.SockAddrInet p _ -> pure (fromIntegral p :: Int)
      NS.SockAddrInet6 p _ _ _ -> pure (fromIntegral p :: Int)
      _ -> error "unexpected sockaddr"
    let cfg =
          defaultWebSocketServerConfig
            { wscHandler = echoHandler
            , wscTls =
                Just
                  WebSocketTlsConfig
                    { wstCertPath = "test-tls/cert.pem"
                    , wstKeyPath = "test-tls/key.pem"
                    , wstAlpn = []
                    }
            }
    tid <- forkIO $ runWebSocketServerOnListener cfg sock
    a <- action port
    killThread tid
    pure a


------------------------------------------------------------------------
-- Server fixture
------------------------------------------------------------------------

withEchoServer :: (Int -> IO a) -> IO a
withEchoServer action = do
  -- Bind to an ephemeral port so the test never collides with
  -- another listener on the host.
  let hints =
        NS.defaultHints
          { NS.addrFlags = [NS.AI_PASSIVE]
          , NS.addrSocketType = NS.Stream
          }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  let addr = case addrs of
        [] -> error "no addr"
        (a : _) -> a
  bracket (NS.openSocket addr) NS.close $ \sock -> do
    NS.setSocketOption sock NS.ReuseAddr 1
    NS.bind sock (NS.addrAddress addr)
    NS.listen sock 16
    boundAddr <- NS.getSocketName sock
    port <- case boundAddr of
      NS.SockAddrInet p _ -> pure (fromIntegral p :: Int)
      NS.SockAddrInet6 p _ _ _ -> pure (fromIntegral p :: Int)
      _ -> error "unexpected sockaddr"
    -- Spawn the server thread and an MVar to wait on its
    -- shutdown so test errors surface promptly.
    done <- newEmptyMVar
    let cfg =
          (defaultWebSocketServerConfig)
            { wscHandler = echoHandler
            }
    tid <- forkIO $ do
      runWebSocketServerOnListener cfg sock
        `finallyMV` putMVar done ()
    a <- action port
    killThread tid
    pure a
  where
    finallyMV act fin = act >>= \r -> fin >> pure r


echoHandler :: WebSocketHandler
echoHandler _req conn = loop
  where
    loop = do
      r <- try @SomeException $ receiveMessage conn defaultMessageLimit
      case r of
        Right (TextMessage t) -> sendTextMessage conn t >> loop
        Right (BinaryMessage bs) -> sendBinaryMessage conn bs >> loop
        Left _ -> pure () -- peer closed


------------------------------------------------------------------------
-- permessage-deflate round-trips
------------------------------------------------------------------------

{- | Echo server that advertises @permessage-deflate@ with the
default ceiling.  When the client offers compression, the server
accepts and both sides exchange RSV1-marked frames transparently
to the user-level message API.
-}
withEchoServerPmd :: (Int -> IO a) -> IO a
withEchoServerPmd = withServerCfgVariant cfg
  where
    cfg =
      defaultWebSocketServerConfig
        { wscHandler = echoHandler
        , wscPermessageDeflate = Just PMD.defaultPmdParams
        }


echoDeflate :: Spec
echoDeflate = it "echoes a short text with permessage-deflate" $ do
  r <- timeout 10_000_000 $ withEchoServerPmd $ \port -> do
    let cfg =
          (defaultWebSocketClientConfig "127.0.0.1" (show port) "/echo")
            { wcPermessageDeflate = Just PMD.defaultPmdOffer
            }
    withWebSocketClient' cfg $ \shr conn -> do
      shrExtensions shr `shouldBe` ["permessage-deflate"]
      sendTextMessage conn "hello, compressed websocket"
      msg <- receiveMessage conn defaultMessageLimit
      case msg of
        TextMessage t -> t `shouldBe` "hello, compressed websocket"
        other -> expectationFailure ("expected TextMessage, got " <> show other)
  case r of
    Just () -> pure ()
    Nothing -> expectationFailure "deflate echo timed out after 10s"


echoDeflateLargePayload :: Spec
echoDeflateLargePayload =
  it "echoes a highly compressible 64 KiB payload with permessage-deflate" $ do
    r <- timeout 20_000_000 $ withEchoServerPmd $ \port -> do
      let cfg =
            (defaultWebSocketClientConfig "127.0.0.1" (show port) "/echo")
              { wcPermessageDeflate = Just PMD.defaultPmdOffer
              }
          big = BS.replicate (64 * 1024) (fromIntegral (fromEnum 'X'))
      withWebSocketClient cfg $ \conn -> do
        sendBinaryMessage conn big
        msg <- receiveMessage conn defaultMessageLimit
        case msg of
          BinaryMessage bs -> bs `shouldBe` big
          other -> expectationFailure ("expected BinaryMessage, got " <> show other)
    case r of
      Just () -> pure ()
      Nothing -> expectationFailure "deflate large-payload echo timed out"


{- | Send a sequence of messages with context-takeover enabled.  Each
subsequent message benefits from the deflate dictionary built up
by previous ones; the round-trip must still recover the input
exactly.
-}
echoDeflateBackToBack :: Spec
echoDeflateBackToBack =
  it "context takeover round-trips a sequence of messages" $ do
    r <- timeout 20_000_000 $ withEchoServerPmd $ \port -> do
      let cfg =
            (defaultWebSocketClientConfig "127.0.0.1" (show port) "/echo")
              { wcPermessageDeflate = Just PMD.defaultPmdOffer
              }
          msgs =
            [ "first message"
            , "second message reusing some 'message' tokens"
            , "third message reusing tokens from the previous messages"
            , "fourth message even more dictionary hits"
            ]
      withWebSocketClient cfg $ \conn -> do
        mapM_ (oneRound conn) msgs
    case r of
      Just () -> pure ()
      Nothing -> expectationFailure "back-to-back deflate echo timed out"
  where
    oneRound conn m = do
      sendTextMessage conn m
      received <- receiveMessage conn defaultMessageLimit
      case received of
        TextMessage t -> t `shouldBe` m
        other -> expectationFailure ("unexpected " <> show other)


{- | Client offers PMD, server is configured not to accept it.  The
handshake must complete normally and the connection must work
without compression.
-}
echoDeflateClientOffersServerDeclines :: Spec
echoDeflateClientOffersServerDeclines =
  it "server declines PMD; uncompressed echo still works" $ do
    r <- timeout 10_000_000 $ withEchoServer $ \port -> do
      let cfg =
            (defaultWebSocketClientConfig "127.0.0.1" (show port) "/echo")
              { wcPermessageDeflate = Just PMD.defaultPmdOffer
              }
      withWebSocketClient' cfg $ \shr conn -> do
        shrExtensions shr `shouldBe` []
        sendTextMessage conn "no deflate this time"
        msg <- receiveMessage conn defaultMessageLimit
        case msg of
          TextMessage t -> t `shouldBe` "no deflate this time"
          other -> expectationFailure ("expected TextMessage, got " <> show other)
    case r of
      Just () -> pure ()
      Nothing -> expectationFailure "fallback echo timed out"


{- | A compressed message may inflate to far more than its wire size.
The receiver must bound the inflation by the message limit while
inflating (not after): a 1 MiB payload sent as ~1 KiB of deflate is
rejected with a 1009 close, and the server-side error names the limit.
-}
echoDeflateInflateBombRejected :: Spec
echoDeflateInflateBombRejected =
  it "rejects a compressed message that inflates past the message limit" $ do
    serverSaw <- newEmptyMVar
    let cfg =
          defaultWebSocketServerConfig
            { wscHandler = \_req conn -> do
                e <- try @SomeException $
                  receiveMessage conn (MessageLimit (64 * 1024))
                putMVar serverSaw (either show (const "accepted") e)
            , wscPermessageDeflate = Just PMD.defaultPmdParams
            }
    r <- timeout 20_000_000 $ withServerCfgVariant cfg $ \port -> do
      let ccfg =
            (defaultWebSocketClientConfig "127.0.0.1" (show port) "/echo")
              { wcPermessageDeflate = Just PMD.defaultPmdOffer
              }
          bomb = BS.replicate (1024 * 1024) 0x61
      withWebSocketClient ccfg $ \conn -> do
        sendBinaryMessage conn bomb
        -- The server answers with close 1009; the next receive must fail
        -- rather than deliver a message.
        rr <- try @SomeException (receiveMessage conn defaultMessageLimit)
        case rr of
          Left _ -> pure ()
          Right _ ->
            expectationFailure "client received a message the server should have rejected"
    case r of
      Just () -> do
        saw <- takeMVar serverSaw
        saw `shouldContain` "message exceeds configured limit"
      Nothing -> expectationFailure "inflate-bomb test timed out"
