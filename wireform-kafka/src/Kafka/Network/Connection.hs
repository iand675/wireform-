{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Network.Connection
Description : TCP/TLS connection management for Kafka brokers

A Kafka broker connection wraps a 'Wireform.Network.DuplexTransport'
— one magic-ring receive transport + one magic-ring send transport
on the same underlying byte stream — and, when TLS is in use, the
owning 'Wireform.Network.TLS.OpenSSL.SslConn'.  The legacy
'connectionGet' \/ 'connectionPut' \/ 'connectionClose' surface
that the rest of @wireform-kafka@ speaks to is implemented in
terms of those.
-}
module Kafka.Network.Connection (
  -- * Connection
  I.Connection (..),
  ConnectionConfig (..),
  BrokerAddress (..),
  BrokerAddressFamily (..),
  DnsLookupMode (..),

  -- * crypton-connection-shaped surface (re-exported from

  -- "Kafka.Network.Connection.Internal" to break a cycle with
  -- "Kafka.Network.Auth.SASL")
  I.connectionGet,
  I.connectionPut,
  I.connectionPutBuilder,
  I.connectionClose,

  -- * Connection lifecycle
  connect,
  connectTls,
  disconnect,
  withConnection,

  -- * Connection manager
  ConnectionManager,
  createConnectionManager,
  getOrCreateConnection,
  closeAllConnections,
  withBrokerLock,

  -- * Liveness
  isConnected,

  -- * Defaults
  defaultConnectionConfig,
  defaultTlsSettings,

  -- * TLS offload (sidecar / kTLS / NLB)
  TlsOffloadConfig (..),
  TlsOffloadEndpoint (..),
  OffloadBrokerKey (..),
  transparentTlsOffload,
  staticTlsOffload,
  perBrokerTlsOffload,
  customTlsOffload,
  brokerAddressToOffloadKey,
  connectOffload,

  -- * Backoff
  calculateBackoffDelay,

  -- * Pluggable host resolution
  HostResolver (..),

  -- * Connection-quota throttling
  ConnectionQuotaState,
  newConnectionQuotaState,
  recordThrottle,
  shouldDelayConnect,

  -- * Idle-expiry tracking
  IdleConnTracker,
  newIdleConnTracker,
  recordActivity,
  isIdle,

  -- * SASL timeouts
  SaslTimeouts (..),
  defaultSaslTimeouts,

  -- * Request quality-of-service
  QosClass (..),
  QosWeight (..),
  prioritise,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracketOnError, try)
import Control.Monad (when)
import Data.ByteString.Internal qualified as BSI
import Data.Hashable (Hashable (hashWithSalt))
import Data.IORef
import Data.Int (Int64)
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Kafka.Network.Auth.SASL qualified as SASL
import Kafka.Network.Connection.Internal (
  Connection (..),
 )
import Kafka.Network.Connection.Internal qualified as I
import Kafka.Telemetry.Log qualified as KLog
import Kafka.Network.TlsOffload (
  OffloadBrokerKey (..),
  TlsOffloadConfig (..),
  TlsOffloadEndpoint (..),
  customTlsOffload,
  describeOffloadEndpoint,
  perBrokerTlsOffload,
  staticTlsOffload,
  transparentTlsOffload,
 )
import Kafka.Network.TlsOffload qualified as TlsOffload
import ListT qualified
import Network.Socket (HostName, PortNumber)
import Network.Socket qualified as NS
import StmContainers.Map qualified as StmMap
import System.Random (randomRIO)
import Wireform.Network (
  defaultTransportConfig,
  newDuplexTransport,
 )
import Wireform.Network.TLS.Config (
  TlsClientConfig (..),
  buildClientCtx,
  defaultTlsClientConfig,
 )
import Wireform.Network.TLS.OpenSSL (
  SslConn,
  SslCtx,
  newClient,
  newTlsDuplexTransport,
  setClientHostnameVerify,
 )
import Wireform.Transport.Config qualified as WC


------------------------------------------------------------------------
-- Broker addressing + config
------------------------------------------------------------------------

data BrokerAddress = BrokerAddress
  { brokerHost :: !HostName
  , brokerPort :: !PortNumber
  }
  deriving (Eq, Show, Ord, Generic)


instance Hashable BrokerAddress where
  hashWithSalt salt (BrokerAddress host port) =
    salt `hashWithSalt` host `hashWithSalt` (fromIntegral port :: Int)


data BrokerAddressFamily
  = BrokerAddressAny
  | BrokerAddressIPv4
  | BrokerAddressIPv6
  deriving (Eq, Show, Generic)


data DnsLookupMode
  = DnsResolveCanonicalBootstrapServersOnly
  | DnsUseAllDnsIps
  deriving (Eq, Show, Generic)


data ConnectionConfig = ConnectionConfig
  { connTimeout :: !Int
  , connReadTimeout :: !Int
  , connWriteTimeout :: !Int
  , connRequestTimeoutMs :: !Int
  , connRetryDelay :: !Int
  , connMaxRetries :: !Int
  , connBackoffMaxMs :: !Int
  , connBackoffMultiplier :: !Double
  , connSocketKeepalive :: !Bool
  , connSocketNagleDisable :: !Bool
  , connSocketSendBuffer :: !Int
  , connSocketReceiveBuffer :: !Int
  , connSocketMaxFails :: !Int
  , connMaxIdleMs :: !Int
  , connMaxReauthMs :: !Int
  , connMessageMaxBytes :: !Int
  , connReceiveMessageMaxBytes :: !Int
  , connMetadataMaxAgeMs :: !Int
  , connTopicMetadataRefreshFastIntervalMs :: !Int
  , connTopicMetadataRefreshSparse :: !Bool
  , connBrokerAddressTtl :: !Int
  , connBrokerAddressFamily :: !BrokerAddressFamily
  , connDnsLookup :: !DnsLookupMode
  , connUseTls :: !Bool
  , connTlsSettings :: !(Maybe TlsClientConfig)
  , connSasl :: !(Maybe SASL.SaslConfig)
  , connClientId :: !Text
  , connTlsOffload :: !(Maybe TlsOffloadConfig)
  }
  deriving (Generic)


defaultConnectionConfig :: ConnectionConfig
defaultConnectionConfig =
  ConnectionConfig
    { connTimeout = 10
    , connReadTimeout = 30
    , connWriteTimeout = 30
    , connRequestTimeoutMs = 30_000
    , connRetryDelay = 100
    , connMaxRetries = 3
    , connBackoffMaxMs = 10_000
    , connBackoffMultiplier = 2.0
    , connSocketKeepalive = True
    , connSocketNagleDisable = True
    , connSocketSendBuffer = 0
    , connSocketReceiveBuffer = 0
    , connSocketMaxFails = 1
    , connMaxIdleMs = 540_000
    , connMaxReauthMs = 0
    , connMessageMaxBytes = 1_000_000
    , connReceiveMessageMaxBytes = 100_000_000
    , connMetadataMaxAgeMs = 900_000
    , connTopicMetadataRefreshFastIntervalMs = 250
    , connTopicMetadataRefreshSparse = True
    , connBrokerAddressTtl = 1000
    , connBrokerAddressFamily = BrokerAddressAny
    , connDnsLookup = DnsResolveCanonicalBootstrapServersOnly
    , connUseTls = False
    , connTlsSettings = Nothing
    , connSasl = Nothing
    , connClientId = T.pack "wireform-kafka"
    , connTlsOffload = Nothing
    }


{- | OpenSSL-backed default TLS settings.  @hostname@ becomes the
SNI server name + cert-pinned hostname.
-}
defaultTlsSettings :: HostName -> TlsClientConfig
defaultTlsSettings hostname =
  defaultTlsClientConfig
    { tlsClientVerifyPeer = True
    , tlsClientVerifyHostname = Just (BSI.packChars hostname)
    }


------------------------------------------------------------------------
-- Connection manager
------------------------------------------------------------------------

data ConnectionManager = ConnectionManager
  { connectionMap :: !(StmMap.Map BrokerAddress Connection)
  , connectionLocks :: !(StmMap.Map BrokerAddress (MVar ()))
  }


createConnectionManager :: IO ConnectionManager
createConnectionManager = ConnectionManager <$> StmMap.newIO <*> StmMap.newIO


withBrokerLock :: ConnectionManager -> BrokerAddress -> IO a -> IO a
withBrokerLock (ConnectionManager _ lockMap) addr action = do
  lock <-
    atomically (StmMap.lookup addr lockMap) >>= \case
      Just l -> pure l
      Nothing -> do
        fresh <- newMVar ()
        atomically $ do
          m <- StmMap.lookup addr lockMap
          case m of
            Just existing -> pure existing
            Nothing -> do
              StmMap.insert fresh addr lockMap
              pure fresh
  withMVar lock (\_ -> action)


getOrCreateConnection
  :: ConnectionManager
  -> BrokerAddress
  -> ConnectionConfig
  -> IO (Either String Connection)
getOrCreateConnection cm@(ConnectionManager connMap _) addr config = do
  existing <- atomically $ StmMap.lookup addr connMap
  case existing of
    Just c -> do
      alive <- isConnected c
      if alive
        then pure (Right c)
        else do
          atomically (StmMap.delete addr connMap)
          getOrCreateConnection cm addr config
    Nothing -> do
      connResult <- case connTlsOffload config of
        Just offload -> connectOffload addr config offload
        Nothing
          | connUseTls config -> connectTls addr config
          | otherwise -> connect addr config
      case connResult of
        Left err -> pure (Left err)
        Right newConn -> do
          authResult <- case connSasl config of
            Nothing -> pure (Right ())
            Just sc -> do
              let host = T.pack (brokerHost addr)
              r <- SASL.authenticateDetailed newConn (connClientId config) host sc
              case r of
                Right _ -> pure (Right ())
                Left e -> pure (Left ("SASL authentication failed: " ++ show e))
          case authResult of
            Left err -> do
              I.connectionClose newConn
              pure (Left err)
            Right () -> do
              atomically $ StmMap.insert newConn addr connMap
              pure (Right newConn)


closeAllConnections :: ConnectionManager -> IO ()
closeAllConnections (ConnectionManager connMap lockMap) = do
  connections <- atomically $ do
    pairs <- ListT.toList $ StmMap.listT connMap
    pure $ fmap snd pairs
  mapM_ disconnect connections
  atomically $ do
    StmMap.reset connMap
    StmMap.reset lockMap


calculateBackoffDelay :: Int -> ConnectionConfig -> IO Int
calculateBackoffDelay attemptNum config = do
  let baseDelayMs = fromIntegral (connRetryDelay config)
      multiplier = connBackoffMultiplier config
      maxDelayMs = fromIntegral (connBackoffMaxMs config)
      exponentialDelayMs = min maxDelayMs (baseDelayMs * (multiplier ** fromIntegral attemptNum))
  jitterFactor <- randomRIO (0.8, 1.2)
  let finalDelayMs = exponentialDelayMs * jitterFactor
      delayMicros = round (finalDelayMs * 1000)
  pure delayMicros


------------------------------------------------------------------------
-- Socket helpers
------------------------------------------------------------------------

{-# INLINE openTunedSocket #-}
openTunedSocket
  :: ConnectionConfig
  -> HostName
  -> PortNumber
  -> IO NS.Socket
openTunedSocket cfg host port = do
  let !hints =
        NS.defaultHints
          { NS.addrFlags = [NS.AI_ADDRCONFIG]
          , NS.addrSocketType = NS.Stream
          , NS.addrFamily = case connBrokerAddressFamily cfg of
              BrokerAddressIPv4 -> NS.AF_INET
              BrokerAddressIPv6 -> NS.AF_INET6
              BrokerAddressAny -> NS.AF_UNSPEC
          }
  addrs <- NS.getAddrInfo (Just hints) (Just host) (Just (show port))
  let attempt addr = bracketOnError
        ( NS.socket
            (NS.addrFamily addr)
            (NS.addrSocketType addr)
            (NS.addrProtocol addr)
        )
        NS.close
        $ \sock -> do
          applyTcpTuning cfg sock
          NS.connect sock (NS.addrAddress addr)
          pure sock
      tryAll [] = ioError (userError ("openTunedSocket: no addrinfo for " ++ host ++ ":" ++ show port))
      tryAll [a] = attempt a
      tryAll (a : as) = do
        r <- try (attempt a)
        case r of
          Right sock -> pure sock
          Left (_ :: SomeException) -> tryAll as
  tryAll addrs


{-# INLINE applyTcpTuning #-}
applyTcpTuning :: ConnectionConfig -> NS.Socket -> IO ()
applyTcpTuning cfg sock = do
  when (connSocketNagleDisable cfg) $
    silently $
      NS.setSocketOption sock NS.NoDelay 1
  when (connSocketKeepalive cfg) $
    silently $
      NS.setSocketOption sock NS.KeepAlive 1
  when (connSocketSendBuffer cfg > 0) $
    silently $
      NS.setSocketOption sock NS.SendBuffer (connSocketSendBuffer cfg)
  when (connSocketReceiveBuffer cfg > 0) $
    silently $
      NS.setSocketOption sock NS.RecvBuffer (connSocketReceiveBuffer cfg)
  where
    silently :: IO () -> IO ()
    silently act = do
      _ <- try act :: IO (Either SomeException ())
      pure ()


{-# INLINE openUnixSocket #-}
openUnixSocket :: FilePath -> IO NS.Socket
openUnixSocket path =
  bracketOnError
    (NS.socket NS.AF_UNIX NS.Stream 0)
    NS.close
    ( \sock -> do
        NS.connect sock (NS.SockAddrUnix path)
        pure sock
    )


------------------------------------------------------------------------
-- Connection construction
------------------------------------------------------------------------

{- | Build a magic-ring 'Connection' from an already-connected socket.
Plain TCP path; no TLS.
-}
buildPlainConnection :: NS.Socket -> IO Connection
buildPlainConnection sock = do
  let cfg =
        defaultTransportConfig
          { WC.ringSizeHint = 1024 * 1024
          }
  duplex <- newDuplexTransport cfg sock
  cursor <- newIORef 0
  closed <- newIORef False
  reqLock <- newMVar ()
  pure
    I.Connection
      { I.connDuplex = duplex
      , I.connSocket = sock
      , I.connSslConn = Nothing
      , I.connCtx = Nothing
      , I.connCursor = cursor
      , I.connClosed = closed
      , I.connRequestLock = reqLock
      }


-- | Build a 'Connection' from an already-handshaked TLS connection.
buildTlsConnection :: NS.Socket -> SslCtx -> SslConn -> IO Connection
buildTlsConnection sock ctx sslConn = do
  let cfg =
        defaultTransportConfig
          { WC.ringSizeHint = 1024 * 1024
          }
  duplex <- newTlsDuplexTransport cfg sslConn
  cursor <- newIORef 0
  closed <- newIORef False
  reqLock <- newMVar ()
  pure
    I.Connection
      { I.connDuplex = duplex
      , I.connSocket = sock
      , I.connSslConn = Just sslConn
      , I.connCtx = Just ctx
      , I.connCursor = cursor
      , I.connClosed = closed
      , I.connRequestLock = reqLock
      }


connect :: BrokerAddress -> ConnectionConfig -> IO (Either String Connection)
connect addr config = go 0
  where
    go attemptNum = do
      result <- try $ do
        sock <- openTunedSocket config (brokerHost addr) (brokerPort addr)
        buildPlainConnection sock
      case result of
        Right c -> pure (Right c)
        Left (e :: SomeException)
          | attemptNum < connMaxRetries config -> do
              delayMicros <- calculateBackoffDelay attemptNum config
              KLog.logWarn KLog.kafkaInstrumentationLibrary "broker connection attempt failed, retrying" $
                KLog.serverAttrs (brokerHost addr) (fromIntegral (brokerPort addr))
                  <> [ KLog.attrInt "kafka.connection.attempt" (attemptNum + 1)
                     , KLog.attrInt "kafka.connection.retry_backoff_ms" (delayMicros `div` 1000)
                     , KLog.attrShow "exception.message" e
                     ]
              threadDelay delayMicros
              go (attemptNum + 1)
          | otherwise ->
              pure $
                Left $
                  "Failed to connect to "
                    ++ brokerHost addr
                    ++ ":"
                    ++ show (brokerPort addr)
                    ++ " after "
                    ++ show (attemptNum + 1)
                    ++ " attempts: "
                    ++ show e


connectTls :: BrokerAddress -> ConnectionConfig -> IO (Either String Connection)
connectTls addr config = case connTlsSettings config of
  Nothing -> pure $ Left "TLS enabled but no TLS settings provided"
  Just tlsCfg -> go tlsCfg 0
  where
    go tlsCfg attemptNum = do
      result <- try $ do
        sock <- openTunedSocket config (brokerHost addr) (brokerPort addr)
        ctx <- buildClientCtx tlsCfg
        conn <-
          newClient
            ctx
            sock
            ( case tlsClientVerifyHostname tlsCfg of
                Just h -> Just h
                Nothing -> Just (BSI.packChars (brokerHost addr))
            )
        case tlsClientVerifyHostname tlsCfg of
          Just h | tlsClientVerifyPeer tlsCfg -> setClientHostnameVerify conn h
          _ -> pure ()
        buildTlsConnection sock ctx conn
      case result of
        Right c -> pure (Right c)
        Left (e :: SomeException)
          | attemptNum < connMaxRetries config -> do
              delayMicros <- calculateBackoffDelay attemptNum config
              KLog.logWarn KLog.kafkaInstrumentationLibrary "broker TLS connection attempt failed, retrying" $
                KLog.serverAttrs (brokerHost addr) (fromIntegral (brokerPort addr))
                  <> [ KLog.attrInt "kafka.connection.attempt" (attemptNum + 1)
                     , KLog.attrInt "kafka.connection.retry_backoff_ms" (delayMicros `div` 1000)
                     , KLog.attrShow "exception.message" e
                     ]
              threadDelay delayMicros
              go tlsCfg (attemptNum + 1)
          | otherwise ->
              pure $
                Left $
                  "Failed to connect (TLS) to "
                    ++ brokerHost addr
                    ++ ":"
                    ++ show (brokerPort addr)
                    ++ " after "
                    ++ show (attemptNum + 1)
                    ++ " attempts: "
                    ++ show e


disconnect :: Connection -> IO ()
disconnect = I.connectionClose


withConnection
  :: BrokerAddress
  -> ConnectionConfig
  -> (Connection -> IO a)
  -> IO (Either String a)
withConnection addr config action = do
  connResult <- case connTlsOffload config of
    Just offload -> connectOffload addr config offload
    Nothing
      | connUseTls config -> connectTls addr config
      | otherwise -> connect addr config
  case connResult of
    Left err -> pure (Left err)
    Right conn -> do
      result <- try $ action conn
      disconnect conn
      pure $ case result of
        Left (e :: SomeException) -> Left $ "Connection action failed: " ++ show e
        Right val -> Right val


{- | Best-effort liveness check.  Reads zero bytes (a 0-byte recv on
the socket via the GHC IO manager would block; we use a tail-vs-
head delta as a cheap availability hint, treating any sticky
error state as dead).
-}
isConnected :: Connection -> IO Bool
isConnected conn = do
  closedFlag <- readIORef (I.connClosed conn)
  pure (not closedFlag)


------------------------------------------------------------------------
-- TLS-offload variant
------------------------------------------------------------------------

brokerAddressToOffloadKey :: BrokerAddress -> OffloadBrokerKey
brokerAddressToOffloadKey BrokerAddress {..} =
  OffloadBrokerKey
    { offloadBrokerHost = brokerHost
    , offloadBrokerPort = brokerPort
    }


{-# INLINE openOffloadOnce #-}
openOffloadOnce
  :: BrokerAddress
  -> ConnectionConfig
  -> TlsOffloadConfig
  -> IO (Either String (Connection, String))
openOffloadOnce addr cfg offload = do
  let key = brokerAddressToOffloadKey addr
  mEndpoint <- TlsOffload.resolveOffloadEndpoint offload key
  result <- try $ case mEndpoint of
    Just (TlsOffload.TlsOffloadUnix path) -> do
      sock <- openUnixSocket path
      c <- buildPlainConnection sock
      pure (c, describeOffloadEndpoint (TlsOffload.TlsOffloadUnix path))
    Just (TlsOffload.TlsOffloadTcp h p) -> do
      sock <- openTunedSocket cfg h p
      c <- buildPlainConnection sock
      pure (c, describeOffloadEndpoint (TlsOffload.TlsOffloadTcp h p))
    Nothing -> do
      sock <- openTunedSocket cfg (brokerHost addr) (brokerPort addr)
      c <- buildPlainConnection sock
      pure (c, "transparent:" <> brokerHost addr <> ":" <> show (brokerPort addr))
  case result of
    Right ok -> pure (Right ok)
    Left (e :: SomeException) -> pure (Left (show e))


connectOffload
  :: BrokerAddress
  -> ConnectionConfig
  -> TlsOffloadConfig
  -> IO (Either String Connection)
connectOffload addr config offload = go 0
  where
    go attemptNum = do
      r <- openOffloadOnce addr config offload
      case r of
        Right (conn, _label) -> pure (Right conn)
        Left err ->
          if attemptNum < connMaxRetries config
            then do
              delayMicros <- calculateBackoffDelay attemptNum config
              KLog.logWarn KLog.kafkaInstrumentationLibrary "broker TLS-offload connection attempt failed, retrying" $
                KLog.serverAttrs (brokerHost addr) (fromIntegral (brokerPort addr))
                  <> [ KLog.attrInt "kafka.connection.attempt" (attemptNum + 1)
                     , KLog.attrInt "kafka.connection.retry_backoff_ms" (delayMicros `div` 1000)
                     , KLog.attrText "exception.message" (T.pack err)
                     , KLog.attrText "tls.offload" (TlsOffload.tlsOffloadLabel offload)
                     ]
              threadDelay delayMicros
              go (attemptNum + 1)
            else
              pure $
                Left $
                  "Failed to connect (TLS-offload "
                    ++ T.unpack (TlsOffload.tlsOffloadLabel offload)
                    ++ ") to "
                    ++ brokerHost addr
                    ++ ":"
                    ++ show (brokerPort addr)
                    ++ " after "
                    ++ show (attemptNum + 1)
                    ++ " attempts: "
                    ++ err


------------------------------------------------------------------------
-- Additional connection-layer ergonomics
------------------------------------------------------------------------

newtype HostResolver = HostResolver
  { resolveHost :: Text -> IO [Text]
  }


newtype ConnectionQuotaState = ConnectionQuotaState
  {cqsThrottleUntilMs :: TVar Int64}


newConnectionQuotaState :: IO ConnectionQuotaState
newConnectionQuotaState = ConnectionQuotaState <$> newTVarIO 0


recordThrottle :: ConnectionQuotaState -> Int64 -> Int -> IO ()
recordThrottle st now throttleMs =
  atomically $
    writeTVar (cqsThrottleUntilMs st) (now + fromIntegral throttleMs)


shouldDelayConnect :: ConnectionQuotaState -> Int64 -> IO (Maybe Int)
shouldDelayConnect st now = do
  until_ <- readTVarIO (cqsThrottleUntilMs st)
  let !wait = until_ - now
  pure $ if wait > 0 then Just (fromIntegral wait) else Nothing


newtype IdleConnTracker key = IdleConnTracker
  {ictLastActivity :: TVar (Map key Int64)}


newIdleConnTracker :: Ord key => IO (IdleConnTracker key)
newIdleConnTracker = IdleConnTracker <$> newTVarIO Map.empty


recordActivity :: Ord key => IdleConnTracker key -> key -> Int64 -> IO ()
recordActivity (IdleConnTracker v) k now =
  atomically $
    modifyTVar' v (Map.insert k now)


isIdle
  :: Ord key
  => IdleConnTracker key
  -> key
  -> Int64
  -> Int
  -> IO Bool
isIdle (IdleConnTracker v) k now thresholdMs = do
  m <- readTVarIO v
  pure $ case Map.lookup k m of
    Nothing -> True
    Just ts -> now - ts >= fromIntegral thresholdMs


data SaslTimeouts = SaslTimeouts
  { saslConnectTimeoutMs :: !Int
  , saslMaxIdleMs :: !Int
  }
  deriving stock (Eq, Show, Generic)


defaultSaslTimeouts :: SaslTimeouts
defaultSaslTimeouts =
  SaslTimeouts
    { saslConnectTimeoutMs = 30_000
    , saslMaxIdleMs = 0
    }


data QosClass
  = QosCritical
  | QosHigh
  | QosNormal
  | QosLow
  deriving stock (Eq, Ord, Show, Generic)


newtype QosWeight = QosWeight {unQosWeight :: Int}
  deriving stock (Eq, Show, Generic)


prioritise :: [(QosClass, a)] -> [(QosClass, a)]
prioritise = L.sortOn (\(c, _) -> classWeight c)
  where
    classWeight QosCritical = 0 :: Int
    classWeight QosHigh = 1
    classWeight QosNormal = 2
    classWeight QosLow = 3
