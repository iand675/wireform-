{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The base transport: a 'Transport' built on top of the
low-level "Network.HTTP.Connection" single-connection API.

This is the production wire shim. It does enough to be useful:

* Parses the request URI to extract scheme \/ host \/ port.
* Opens a fresh connection per request (no pool yet; pooling is
  intended to follow).
* Honours HTTPS via the existing TLS module.
* Bridges the low-level @'Network.HTTP.Types.Body.Body' =
  'IO (Maybe ByteString)'@ shape to the high-level
  @'Popper' = 'IO ByteString'@.

It /eagerly drains the response body/ before returning the
'RawResponse'. That's a deliberate limitation — the low-level
body popper's lifetime is tied to the connection scope, which
means streaming through 'Transport's @sendRaw :: IO RawResponse@
shape would require connection-lifetime threading we don't have
yet.
-}
module Network.HTTP.Client.Base (
  baseTransport,
  baseTransportVia,
  BaseTransportError (..),
) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Maybe (fromMaybe)
import Network.HTTP.Client.BodyStream
import Network.HTTP.Client.Protocol
import Network.HTTP.Client.Proxy (ProxyConfig)
import Network.HTTP.Client.Proxy qualified as Pxy
import Network.HTTP.Client.Request qualified as WReq
import Network.HTTP.Client.Response
import Network.HTTP.Client.Transport
import Network.HTTP.Client.URI qualified as WURI
import Network.HTTP.Connection qualified as Conn
import Network.HTTP.Internal.Convert qualified as Conv
import Network.HTTP.Message qualified as Msg
import Network.HTTP.Types.Body qualified as LB
import Network.HTTP.Types.Version qualified as LV
import Network.HTTP.VersionRange qualified as VR


data BaseTransportError
  = -- | Could not parse the resolved URI.
    BaseTransportInvalidURI !String
  | BaseTransportLowLevel !Conn.ConnectionError
  deriving stock (Show)


instance Exception BaseTransportError


{- | The default base transport. Each call opens a connection, ships
the request, drains the response, and closes the connection. Use
TLS if the URI scheme is @https@. No proxy is consulted; for
proxy-aware routing use 'baseTransportVia'.
-}
baseTransport :: VR.VersionRange -> Transport IO
baseTransport versionRange = baseTransportVia versionRange Pxy.noProxyConfig


{- | Proxy-aware variant of 'baseTransport'. The supplied
'ProxyConfig' picks a per-request proxy:

* HTTPS targets through an HTTPS proxy go via @CONNECT@ +
  in-tunnel TLS handshake.
* HTTP targets through an HTTP proxy dial the proxy directly;
  the absolute-form request line is the responsibility of the
  'Network.HTTP.Client.Proxy.withProxy' middleware.
-}
baseTransportVia :: VR.VersionRange -> ProxyConfig -> Transport IO
baseTransportVia versionRange pcfg = Transport $ \req -> do
  uri_ <- case WURI.renderRequestURI (WReq.requestURI req) of
    Right u -> pure u
    Left err -> throwIO (BaseTransportInvalidURI err)
  let scheme = WURI.uriScheme uri_
      lowScheme = case scheme of
        WURI.SchemeHttps -> Msg.SchemeHttps
        WURI.SchemeHttp -> Msg.SchemeHttp
      host = BS8.unpack (WURI.uriHost uri_)
      port = show (WURI.uriPort uri_)
      tls = case scheme of
        WURI.SchemeHttps -> Just (Conn.defaultTlsConnectionConfig host)
        WURI.SchemeHttp -> Nothing
      connCfg =
        Conn.ConnectionConfig
          { Conn.connectionHost = host
          , Conn.connectionPort = port
          , Conn.connectionVersionRange = versionRange
          , Conn.connectionTls = tls
          }
      mProxy
        | Pxy.shouldBypass pcfg (WURI.uriHost uri_) = Nothing
        | otherwise = case scheme of
            WURI.SchemeHttp -> Pxy.proxyForHttp pcfg
            WURI.SchemeHttps -> Pxy.proxyForHttps pcfg
      target = WURI.uriPathAndQuery uri_
      authority =
        Just
          ( WURI.uriHost uri_ <> case BS8.readInt (BS8.pack port) of
              Just (p, _)
                | scheme == WURI.SchemeHttp && p == 80 -> ""
                | scheme == WURI.SchemeHttps && p == 443 -> ""
              _ -> ":" <> BS8.pack port
          )

  prepared <- Conv.prepareRequestBody toLowLevelBody req
  let lowReq =
        Msg.Request
          { Msg.requestMethod = WReq.method req
          , Msg.requestTarget = target
          , Msg.requestAuthority = authority
          , Msg.requestScheme = lowScheme
          , Msg.requestHeaders = WReq.headers prepared
          , Msg.requestBody = WReq.body prepared
          , Msg.requestVersion = VR.preferredVersion versionRange
          , Msg.requestTrailers = pure []
          }

  Conn.withConnectionVia connCfg mProxy Nothing $ \conn -> do
    let hints = WReq.protocolHints req
    framed <- Conv.prepareRequestTrailers (Conn.negotiatedVersion conn) hints lowReq
    resp <- Conn.sendOnWithContinueTimeout (continueTimeoutMicros hints) conn framed
    materialised <- lowLevelBodyBytes (Msg.responseBody resp)
    trailers <- Msg.responseTrailers resp
    newPopper <- popperFromStrict materialised
    pure
      RawResponse
        { statusCode = Msg.responseStatus resp
        , headers = Msg.responseHeaders resp
        , bodyPopper = newPopper
        , protocolInfo = withBufferedTrailers trailers (lowToProtocol resp)
        }


lowToProtocol :: Msg.Response -> ProtocolInfo WReq.Request RawResponse
lowToProtocol resp = case Msg.responseVersion resp of
  LV.HTTP2 ->
    HTTP2
      Http2Info
        { h2StreamId = Msg.responseH2StreamId resp
        , h2PushPromises = pure []
        , h2CancelStream = Msg.responseCancel resp
        }
  _ -> HTTP1_1


{- | Bridge from the high-level 'BodyStream' (which signals EOF with
an empty 'ByteString') to the low-level 'LB.Body' (which signals
EOF with 'Nothing'). Bodies of unknown size become 'LB.BodyStream';
known-size ones get folded down to 'LB.BodyBytes' so the low-level
encoder can set @Content-Length@ without sniffing.
-}
toLowLevelBody :: BodyStream -> IO LB.Body
toLowLevelBody bs = case knownSize bs of
  Just 0 -> pure LB.BodyEmpty
  Just _ -> LB.BodyBytes <$> bodyStreamBytes bs
  Nothing -> pure (Conv.streamedRequestBody bs)


{- | Materialise a low-level 'LB.Body' into a strict 'ByteString'.

Streaming bodies are adapted to the wireform 'Popper' shape
(empty 'ByteString' signalling EOF instead of 'Nothing') and then
folded with 'popperBytes', which uses a builder for O(1)
per-chunk appends.
-}
lowLevelBodyBytes :: LB.Body -> IO ByteString
lowLevelBodyBytes = \case
  LB.BodyEmpty -> pure BS.empty
  LB.BodyBytes bs -> pure bs
  LB.BodyStream p -> popperBytes (fromMaybe BS.empty <$> p)
