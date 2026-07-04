-- Note: atm this module is used only by Server.Run
module Network.GRPC.Util.HTTP2 (
    -- * Configuration
  withConfigForInsecure,
  withConfigForSecure,
  withConfigForTransport,
    -- * Settings
  mkServerConfig,
  mkTlsSettings,
  ) where

import Network.GRPC.Util.Imports

import Data.ByteString qualified as Strict (ByteString)
import qualified Data.ByteString.Internal as BSI
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Data.Word (Word8)
import Network.HTTP2.Engine.Types (BufferSize)
import Network.HTTP2.Engine.Server qualified as Server
import Network.HTTP2.Engine.TLS.Server qualified as Server.TLS
import Network.HTTP2.Transport (SendFn)
import Network.Socket (Socket, SockAddr)
import Network.Socket qualified as Socket
import Network.Socket.BufferPool qualified as Recv

import Network.GRPC.Common.HTTP2Settings
import Network.GRPC.Util.TimeManager (TimeManager, disableTimeout)

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Create config to be used with @http2@ (without TLS)
--
-- We do not use @allocSimpleConfig@ from @http2:Network.HTTP2.Server@, but
-- instead create a config that is very similar to the config created by
-- 'allocConfigForSecure'.
withConfigForInsecure ::
     TimeManager
  -> Socket
  -> (Server.Config -> IO a)
  -> IO a
withConfigForInsecure mgr sock k = do
    pool   <- Recv.newBufferPool readBufferLowerLimit readBufferSize
    mysa   <- Socket.getSocketName sock
    peersa <- Socket.getPeerName sock
    withConfig
      mgr
      (Socket.sendBuf sock)
      (Recv.receive sock pool)
      mysa
      peersa
      k
  where
    readBufferLowerLimit, readBufferSize :: Int
    readBufferLowerLimit = Server.TLS.settingsReadBufferLowerLimit Server.TLS.defaultSettings
    readBufferSize       = Server.TLS.settingsReadBufferSize       Server.TLS.defaultSettings

-- | Create config to be used with @http2-tls@ (with TLS)
--
-- This is adapted from @allocConfigForServer@ in
-- @http2-tls:Network.HTTP2.TLS.Config@.
withConfigForSecure ::
     TimeManager
  -> Server.TLS.IOBackend
  -> (Server.Config -> IO a)
  -> IO a
withConfigForSecure mgr backend =
    withConfig
      mgr
      (bsToSendFn (Server.TLS.send backend))
      (Server.TLS.recv         backend)
      (Server.TLS.mySockAddr   backend)
      (Server.TLS.peerSockAddr backend)

-- | Convert a 'ByteString'-based send to a pointer-based 'SendFn'.
-- Adds one memcpy per drain call (ring → fresh BS → underlying send),
-- but the ring still eliminates the builder→ByteString copy on the
-- encode side.
bsToSendFn :: (Strict.ByteString -> IO ()) -> SendFn
bsToSendFn sendAll ptr len = do
  bs <- BSI.create len (\dst -> copyBytes dst ptr len)
  sendAll bs
  pure len

-- | Internal generalization
withConfig ::
     TimeManager
  -> SendFn
  -> Recv.Recv
  -> SockAddr
  -> SockAddr
  -> (Server.Config -> IO a)
  -> IO a
withConfig mgr sendFn recv mysa peersa k = do
    recvN <- Recv.makeRecvN mempty recv
    k Server.Config {
        confSendFn            = sendFn
      , confReadN             = recvN
      , confPositionReadMaker = Server.defaultPositionReadMaker
      , confTimeoutManager    = mgr
      , confMySockAddr        = mysa
      , confPeerSockAddr      = peersa
      }

-- | Create a server config over a caller-supplied byte transport (a
-- pointer-based send and an exactly-N read), rather than a 'Socket'. Used to
-- run the gRPC server over any transport — e.g. one end of a
-- @wireform-dst-net@ fault link. The socket addresses are placeholders (there
-- is no real socket); nothing in the request path depends on them for gRPC.
-- The @readN@ callback MUST return exactly the requested number of bytes,
-- fewer only at end-of-input (the engine's frame reader relies on this).
withConfigForTransport ::
     TimeManager
  -> SendFn
  -> (Int -> IO Strict.ByteString)
  -> (Server.Config -> IO a)
  -> IO a
withConfigForTransport mgr sendFn readN k =
    k Server.Config {
        confSendFn            = sendFn
      , confReadN             = readN
      , confPositionReadMaker = Server.defaultPositionReadMaker
      , confTimeoutManager    = mgr
      , confMySockAddr        = placeholder
      , confPeerSockAddr      = placeholder
      }
  where
    placeholder :: SockAddr
    placeholder = Socket.SockAddrUnix "wireform-dst-net"

{-------------------------------------------------------------------------------
  Settings

  NOTE: If we want to override 'HTTP2.TLS.settingsReadBufferLowerLimit' or
  'HTTP2.TLS.settingsReadBufferSize', we should also modify
  'allocConfigForInsecure'.
-------------------------------------------------------------------------------}

mkServerConfig :: HTTP2Settings -> Server.ServerConfig
mkServerConfig http2Settings =
    Server.defaultServerConfig {
        Server.connectionWindowSize = fromIntegral $
          http2ConnectionWindowSize http2Settings
      , Server.settings =
          Server.defaultSettings {
              Server.initialWindowSize = fromIntegral $
                http2StreamWindowSize http2Settings
            , Server.maxConcurrentStreams = Just . fromIntegral $
                http2MaxConcurrentStreams http2Settings
            , Server.pingRateLimit =
                case http2OverridePingRateLimit http2Settings of
                  Nothing    -> Server.pingRateLimit Server.defaultSettings
                  Just limit -> limit
            , Server.emptyFrameRateLimit =
                case http2OverrideEmptyFrameRateLimit http2Settings of
                  Nothing    -> Server.emptyFrameRateLimit Server.defaultSettings
                  Just limit -> limit
            , Server.settingsRateLimit =
                case http2OverrideSettingsRateLimit http2Settings of
                  Nothing    -> Server.settingsRateLimit Server.defaultSettings
                  Just limit -> limit
            , Server.rstRateLimit =
                case http2OverrideRstRateLimit http2Settings of
                  Nothing    -> Server.rstRateLimit Server.defaultSettings
                  Just limit -> limit
            }
      }

-- | Settings for secure server (with TLS)
--
-- NOTE: This overlaps with the values in 'mkServerConfig', and I /think/ we
-- don't actually need this, because we don't use @runWithSocket@ from
-- @http2-tls@ (but rather @runTLSWithSocket@. However, we set them here anyway
-- for completeness and in case @http2-tls@ decides to use them elsewhere.
mkTlsSettings ::
     HTTP2Settings
  -> (String -> IO ())  -- ^ Key logger
  -> Server.TLS.Settings
mkTlsSettings http2Settings keyLogger =
    Server.TLS.defaultSettings {
        Server.TLS.settingsKeyLogger =
          keyLogger
      , Server.TLS.settingsTimeout =
          disableTimeout
      , Server.TLS.settingsConnectionWindowSize = fromIntegral $
          http2ConnectionWindowSize http2Settings
      , Server.TLS.settingsStreamWindowSize = fromIntegral $
          http2StreamWindowSize http2Settings
      , Server.TLS.settingsConcurrentStreams = fromIntegral $
          http2MaxConcurrentStreams http2Settings
      , Server.TLS.settingsPingRateLimit =
          case http2OverridePingRateLimit http2Settings of
            Nothing    -> Server.pingRateLimit Server.defaultSettings
            Just limit -> limit
      , Server.TLS.settingsEmptyFrameRateLimit =
          case http2OverrideEmptyFrameRateLimit http2Settings of
            Nothing    -> Server.emptyFrameRateLimit Server.defaultSettings
            Just limit -> limit
      , Server.TLS.settingsSettingsRateLimit =
          case http2OverrideSettingsRateLimit http2Settings of
            Nothing    -> Server.settingsRateLimit Server.defaultSettings
            Just limit -> limit
      , Server.TLS.settingsRstRateLimit =
          case http2OverrideRstRateLimit http2Settings of
            Nothing    -> Server.rstRateLimit Server.defaultSettings
            Just limit -> limit
      }
