{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Low-level single-connection HTTP client with version negotiation.

A 'ConnectionConfig' carries a 'VersionRange' that declares which
on-wire HTTP versions the connection is willing to speak.
'withConnection' opens a connection that honours the range and
exposes a single 'sendOn' API regardless of whether the peer ends
up speaking HTTP\/1.x or HTTP\/2.  If the peer forces us out of
range a 'Network.HTTP.VersionRange.VersionOutOfRange' is thrown.

This is the byte-level transport that the higher-level
"Network.HTTP.Client" sits on top of via
'Network.HTTP.Client.Base.baseTransport'. Most callers should use
the high-level API; reach for this module only when you want to
manage a single connection's lifetime by hand.

Transport matrix:

* __Plaintext, HTTP\/1.x only__ ('http1Only' or 'preferHttp1' on
  plaintext) — request and response both end-to-end.
* __Plaintext, HTTP\/2 only__ ('http2Only' on plaintext; h2c prior
  knowledge) — request, response headers, and a buffered response
  body all work, with the connection multiplexing concurrent
  requests on separate streams.
* __Plaintext, mixed range__ — falls back to the preferred version.
  The h2c @Upgrade:@ dance (RFC 7540 § 3.2) is intentionally not
  implemented: RFC 9113 deprecated it.  For mixed-protocol clients
  use TLS-ALPN.
* __TLS__ — handshake done with ALPN advertising the protocols in
  'connectionVersionRange'; the negotiated protocol drives the
  per-version runtime.  Both HTTP\/2 and HTTP\/1.x over TLS work
  end-to-end.  If ALPN ends up picking a version that isn't in the
  range, 'VersionOutOfRange' is raised.

== TLS configuration (§4.4 audit)

'TlsConnectionConfig' exposes:

* 'tlsServerName' — SNI / X.509 hostname (defaults to
  'connectionHost').
* 'tlsValidateCert' — certificate chain + hostname verification
  (default 'True').
* 'tlsClientCertificate' — @(certChainPEM, privateKeyPEM)@ file
  paths for mutual TLS (mTLS). Corresponds to
  'Wireform.Network.TLS.Config.tlsClientCertificate'.
* 'tlsMinVersion' — minimum acceptable TLS protocol version.
  Defaults to 'Wireform.Network.TLS.OpenSSL.Tls12'. Set to 'Tls13'
  to require TLS 1.3.
-}
module Network.HTTP.Connection (
  -- * Configuration
  ConnectionConfig (..),
  defaultConnectionConfig,
  TlsConnectionConfig (..),
  defaultTlsConnectionConfig,

  -- * Connecting
  Connection,
  negotiatedVersion,
  withConnection,
  withConnectionVia,
  openConnection,
  openConnectionVia,
  closeConnection,
  isPlaintextHttp1,

  -- * Sending requests
  sendOn,
  withResponseOn,
  withResponseDeferredOn,

  -- * Errors
  ConnectionError (..),
) where

import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Exception (Exception, bracketOnError, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.CaseInsensitive qualified as CI
import Data.Maybe qualified
import Network.HTTP.Client.BodyStream (popperFromStrict)
import Network.HTTP.Client.Protocol (
  Http2Info (..),
  ProtocolInfo (..),
  PushPromise (..),
 )
import Network.HTTP.Client.Proxy (Proxy (..))
import Network.HTTP.Client.Proxy.Connect (connectThroughProxy)
import Network.HTTP.Client.Request hiding (Request, headers)
import Network.HTTP.Client.Response (RawResponse (..))
import Network.HTTP.Internal.Convert qualified as Conv
import Network.HTTP.Message
import Network.HTTP.TLS qualified as TLS
import Network.HTTP.Types.Body qualified as U
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as U
import Network.HTTP.Types.Status qualified as U
import Network.HTTP.Types.Version qualified as U
import Network.HTTP.VersionRange
import Network.HTTP1.Client qualified as H1
import Network.HTTP1.Parser qualified as H1
import Network.HTTP1.Types qualified as H1
import Network.HTTP2.Client qualified as H2
import Network.Socket qualified as NS
import Wireform.Network.TLS.OpenSSL (TlsProtoVersion (..))


-- | Configuration for opening an HTTP connection: the host, port,
-- acceptable on-wire versions, and optional TLS settings.
data ConnectionConfig = ConnectionConfig
  { connectionHost :: !String
  , connectionPort :: !String
  , connectionVersionRange :: !VersionRange
  -- ^ Acceptable on-wire versions, in preference order.
  , connectionTls :: !(Maybe TlsConnectionConfig)
  -- ^ 'Just' to do TLS + ALPN; 'Nothing' for plaintext.
  }


-- | TLS settings for an HTTP connection: the SNI hostname, a
-- certificate-validation toggle, an optional mTLS client certificate,
-- and a minimum protocol version.
data TlsConnectionConfig = TlsConnectionConfig
  { tlsServerName :: !String
  -- ^ SNI \/ X.509 hostname (defaults to 'connectionHost').
  , tlsValidateCert :: !Bool
  {- ^ When 'True' (default), the server certificate chain and
  hostname are verified against the system trust store.
  -}
  , tlsClientCertificate :: !(Maybe (FilePath, FilePath))
  {- ^ @(certChainPEM, privateKeyPEM)@ for mutual TLS (mTLS).
  'Nothing' means no client cert is presented (the common
  case). Corresponds to
  'Wireform.Network.TLS.Config.tlsClientCertificate'.
  -}
  , tlsMinVersion :: !TlsProtoVersion
  {- ^ Minimum acceptable TLS protocol version.  Defaults to
  'Tls12'.  Set to 'Tls13' to reject TLS 1.2 handshakes
  (recommended for new deployments where the peer is known
  to support TLS 1.3).
  -}
  }


-- | A plaintext HTTP\/1.1 default: @127.0.0.1:80@, no TLS. Override
-- 'connectionHost', 'connectionPort', and 'connectionTls' for real use.
defaultConnectionConfig :: ConnectionConfig
defaultConnectionConfig =
  ConnectionConfig
    { connectionHost = "127.0.0.1"
    , connectionPort = "80"
    , connectionVersionRange = http1Only
    , connectionTls = Nothing
    }


-- | A 'TlsConnectionConfig' seeded with the server hostname (for SNI),
-- certificate validation on, no mTLS client cert, TLS 1.2 minimum.
defaultTlsConnectionConfig :: String -> TlsConnectionConfig
defaultTlsConnectionConfig serverName =
  TlsConnectionConfig
    { tlsServerName = serverName
    , tlsValidateCert = True
    , tlsClientCertificate = Nothing
    , tlsMinVersion = Tls12
    }


data ConnectionError
  = ConnectionUnsupportedRange !VersionRange
  | ConnectionParseError !H1.ParseError
  deriving stock (Show)


instance Exception ConnectionError


-- | An opaque connection handle.  Inspect with 'negotiatedVersion'.
data Connection
  = Http1Connection !H1.ClientConnection !U.Version
  | Http2Connection !H2.ClientHandle !U.Version


negotiatedVersion :: Connection -> U.Version
negotiatedVersion = \case
  Http1Connection _ v -> v
  Http2Connection _ v -> v


{- | Open a connection, run the action, close the connection.

The protocol used is determined by 'connectionVersionRange' and
'connectionTls': TLS uses ALPN with the range's protocols (or
fails with 'VersionOutOfRange'); plaintext picks the preferred
version in the range.
-}
withConnection :: ConnectionConfig -> (Connection -> IO a) -> IO a
withConnection cfg = withConnectionVia cfg Nothing Nothing


{- | Variant of 'withConnection' that routes through an explicit
'Proxy' (with optional @Proxy-Authorization@ header value on the
'CONNECT' exchange).

* HTTPS target + proxy: dial the proxy, issue
  @CONNECT host:port@, then layer TLS on the resulting tunnel.
* HTTP target + proxy: dial the proxy directly. The HTTP request
  line is expected to be in absolute form
  (the 'Network.HTTP.Client.Proxy.withProxy' middleware rewrites
  it).
* 'Nothing' for the proxy argument: identical to 'withConnection'.
-}
withConnectionVia
  :: ConnectionConfig
  -> Maybe Proxy
  -> Maybe ByteString
  -- ^ Proxy-Authorization header value (CONNECT only)
  -> (Connection -> IO a)
  -> IO a
withConnectionVia cfg mProxy mAuth action = case connectionTls cfg of
  Just tlsCfg -> case mProxy of
    Just prx -> withTlsConnectionThroughProxy cfg tlsCfg prx mAuth action
    Nothing -> withTlsConnection cfg tlsCfg action
  Nothing -> case mProxy of
    Just prx -> withPlaintextHttp1Via prx cfg action
    Nothing ->
      let preferred = preferredVersion (connectionVersionRange cfg)
      in if preferred == U.HTTP2
           then withPlaintextHttp2 cfg action
           else withPlaintextHttp1 cfg action


withTlsConnection :: ConnectionConfig -> TlsConnectionConfig -> (Connection -> IO a) -> IO a
withTlsConnection cfg tlsCfg action =
  TLS.withTlsClient
    (connectionHost cfg)
    (connectionPort cfg)
    (tlsServerName tlsCfg)
    (tlsValidateCert tlsCfg)
    (tlsClientCertificate tlsCfg)
    (tlsMinVersion tlsCfg)
    (connectionVersionRange cfg)
    $ \case
      TLS.TlsClientHttp2 handle -> action (Http2Connection handle U.HTTP2)
      TLS.TlsClientHttp1 conn -> action (Http1Connection conn U.HTTP1_1)


{- | Dial @proxy@, run @CONNECT target:port@, and then drive the
caller-supplied TLS handshake over the established tunnel via
'TLS.withTlsClientOnSocket'.  Socket lifetime is bracketed by
this helper.
-}
withTlsConnectionThroughProxy
  :: ConnectionConfig
  -> TlsConnectionConfig
  -> Proxy
  -> Maybe ByteString
  -> (Connection -> IO a)
  -> IO a
withTlsConnectionThroughProxy cfg tlsCfg prx mAuth action =
  bracketOnError
    (connectThroughProxy prx targetHostBs targetPort mAuth)
    NS.close
    $ \sock -> do
      let host = connectionHost cfg
          port = connectionPort cfg
      result <-
        TLS.withTlsClientOnSocket
          sock
          host
          port
          (tlsServerName tlsCfg)
          (tlsValidateCert tlsCfg)
          (tlsClientCertificate tlsCfg)
          (tlsMinVersion tlsCfg)
          (connectionVersionRange cfg)
          $ \case
            TLS.TlsClientHttp2 handle ->
              action (Http2Connection handle U.HTTP2)
            TLS.TlsClientHttp1 conn ->
              action (Http1Connection conn U.HTTP1_1)
      NS.close sock
      pure result
  where
    targetHostBs = BS8.pack (connectionHost cfg)
    targetPort = case reads (connectionPort cfg) :: [(Int, String)] of
      [(n, "")] -> n
      _ -> 443


withPlaintextHttp1 :: ConnectionConfig -> (Connection -> IO a) -> IO a
withPlaintextHttp1 cfg = withPlaintextHttp1Dial (connectionHost cfg) (connectionPort cfg) cfg


{- | Same as 'withPlaintextHttp1' but dials the supplied proxy host
\/ port instead of the URI's target. Used for HTTP-via-HTTP-proxy
(the request line is already in absolute form via the
'withProxy' middleware).
-}
withPlaintextHttp1Via
  :: Proxy
  -> ConnectionConfig
  -> (Connection -> IO a)
  -> IO a
withPlaintextHttp1Via prx =
  withPlaintextHttp1Dial (BS8.unpack (proxyHost prx)) (show (proxyPort prx))


withPlaintextHttp1Dial
  :: String
  -> String
  -> ConnectionConfig
  -> (Connection -> IO a)
  -> IO a
withPlaintextHttp1Dial host port cfg action = do
  let h1cfg =
        H1.defaultClientConfig
          { H1.clientHost = host
          , H1.clientPort = port
          }
      ver = case preferredVersion (connectionVersionRange cfg) of
        U.HTTP1_0 -> U.HTTP1_0
        _ -> U.HTTP1_1
  H1.withClientConnection h1cfg $ \conn ->
    action (Http1Connection conn ver)


{- | True iff this 'Connection' is a plaintext HTTP\/1.x connection
— i.e. one that can be opened \/ closed without 'withConnection'.
The connection pool keys reuse on this: TLS and HTTP\/2
connections fall back to per-request bracketing because the
existing low-level transport stack doesn't expose
open\/close out of bracket scope for them yet.
-}
isPlaintextHttp1 :: Connection -> Bool
isPlaintextHttp1 = \case
  Http1Connection _ _ -> True
  Http2Connection _ _ -> False


{- | Open a plaintext HTTP\/1.x connection without bracketing it.
Returns 'Left' for configurations that need the bracketed
'withConnection' path (TLS or HTTP\/2-preferred). Pair with
'closeConnection' for proper teardown.
-}
openConnection :: ConnectionConfig -> IO (Either String Connection)
openConnection cfg = openConnectionVia cfg Nothing


{- | Variant of 'openConnection' that dials through an explicit
proxy when supplied. Only valid for plaintext HTTP\/1.x
targets — TLS \/ HTTP\/2 paths still need the bracketed
'withConnectionVia' API.
-}
openConnectionVia
  :: ConnectionConfig
  -> Maybe Proxy
  -> IO (Either String Connection)
openConnectionVia cfg mProxy = case connectionTls cfg of
  Just _ -> pure (Left "openConnectionVia: TLS connections require the bracketed withConnectionVia API")
  Nothing -> case preferredVersion (connectionVersionRange cfg) of
    U.HTTP2 -> pure (Left "openConnectionVia: HTTP/2 connections require the bracketed withConnectionVia API")
    ver -> do
      let (dialHost, dialPort) = case mProxy of
            Just prx -> (BS8.unpack (proxyHost prx), show (proxyPort prx))
            Nothing -> (connectionHost cfg, connectionPort cfg)
          h1cfg =
            H1.defaultClientConfig
              { H1.clientHost = dialHost
              , H1.clientPort = dialPort
              }
          usedVer = case ver of
            U.HTTP1_0 -> U.HTTP1_0
            _ -> U.HTTP1_1
      conn <- H1.openClientConnection h1cfg
      pure (Right (Http1Connection conn usedVer))


{- | Close a previously-opened 'Connection'. Only meaningful for
connections produced by 'openConnection'; bracketed connections
close themselves when 'withConnection' returns.
-}
closeConnection :: Connection -> IO ()
closeConnection = \case
  Http1Connection conn _ -> H1.closeClientConnection conn
  Http2Connection _ _ ->
    -- The HTTP/2 client API only exposes 'withConnection'. The
    -- pool ensures we never reach this path for HTTP/2; if we
    -- somehow do, leaking the connection is the safest
    -- alternative to throwing.
    pure ()


withPlaintextHttp2 :: ConnectionConfig -> (Connection -> IO a) -> IO a
withPlaintextHttp2 cfg action = do
  let h2cfg =
        H2.defaultClientConfig
          { H2.clientHost = connectionHost cfg
          , H2.clientPort = connectionPort cfg
          }
  H2.withConnection h2cfg $ \handle ->
    action (Http2Connection handle U.HTTP2)


{- | Send a request on the connection. The 'requestVersion' field on
the input is ignored; the on-wire version is whatever
'withConnection' negotiated.
-}
sendOn :: Connection -> Request -> IO Response
sendOn (Http1Connection conn ver) req = do
  let req1 = (Conv.toHttp1Request req) {H1.requestVersion = Conv.toHttp1Version ver}
  -- When the request carries @Expect: 100-continue@, use the
  -- two-stage send: headers first, wait for 100, then body.
  -- 1-second default timeout per RFC 9110 §10.1.1.
  result <-
    if hasExpect100Continue (requestHeaders req)
      then H1.sendRequestOnWithExpect conn req1 1_000_000
      else H1.sendRequestOn conn req1
  case result of
    Left err -> throwIO (ConnectionParseError err)
    Right resp -> pure (Conv.fromHttp1Response resp)
  where
    hasExpect100Continue hdrs = case H.lookupHeader H.hExpect hdrs of
      Nothing -> False
      Just v -> CI.mk v == CI.mk "100-continue"
sendOn (Http2Connection handle _) req = do
  h2resp <- H2.sendRequest handle (requestToH2 req)
  pure (h2ResponseToUnified h2resp)


{- | Bracket-style request that cancels the underlying stream if the
action throws (HTTP\/2) or closes the connection if the action
throws on HTTP\/1.x.  Use this whenever the caller might bail
mid-request (timeouts, racing requests, cooperative cancellation).

For HTTP\/2 we emit @RST_STREAM(CANCEL)@ to the peer on abnormal
exit, so a cancelled download stops costing bandwidth.  For
HTTP\/1.x there's no per-stream cancellation; if the action
throws we let the exception propagate and the surrounding
'withConnection' bracket closes the connection.
-}
withResponseOn :: Connection -> Request -> (Response -> IO a) -> IO a
withResponseOn (Http1Connection conn ver) req action = do
  let req1 = (Conv.toHttp1Request req) {H1.requestVersion = Conv.toHttp1Version ver}
  result <- H1.sendRequestOn conn req1
  case result of
    Left err -> throwIO (ConnectionParseError err)
    Right resp -> action (Conv.fromHttp1Response resp)
withResponseOn (Http2Connection handle _) req action = do
  let h2req = requestToH2 req
  H2.withResponse handle h2req $ \h2resp ->
    action (h2ResponseToUnified h2resp)


{- | Like 'withResponseOn', but the action runs immediately after the
request headers are sent; it receives an @awaitResponse@ action that
blocks until the response head arrives (idempotent — every call
returns the same response).

This is the shape full-duplex exchanges need on HTTP\/2: with a
streaming request body the peer may withhold its response headers
until it has seen the first request data (e.g. a Connect\/gRPC bidi
server staging response metadata until the handler's first send), so
the client must be able to produce request data before blocking on
the response.

On HTTP\/1.x there is no full duplex: @awaitResponse@ performs the
entire exchange on first call — it drains the request-body producer
to completion before the response appears, so a caller that waits on
the response before finishing its request body will deadlock. Only
half-duplex usage (send everything, then await) works there.
-}
withResponseDeferredOn :: Connection -> Request -> (IO Response -> IO a) -> IO a
withResponseDeferredOn conn@(Http1Connection _ _) req action = do
  cell <- newMVar Nothing
  let awaitResponse = modifyMVar cell $ \case
        st@(Just resp) -> pure (st, resp)
        Nothing -> do
          resp <- sendOn conn req
          pure (Just resp, resp)
  action awaitResponse
withResponseDeferredOn (Http2Connection handle _) req action =
  H2.withResponseDeferred handle (requestToH2 req) $ \awaitH2 ->
    action (h2ResponseToUnified <$> awaitH2)

requestToH2 :: Request -> H2.ClientRequest
requestToH2 req =
  H2.ClientRequest
    { H2.crMethod = U.fromMethod (requestMethod req)
    , H2.crPath = requestTarget req
    , H2.crScheme = case requestScheme req of
        SchemeHttp -> "http"
        SchemeHttps -> "https"
    , H2.crAuthority = Data.Maybe.fromMaybe "" (requestAuthority req)
    , H2.crHeaders = Conv.toHttp2Headers (requestHeaders req)
    , H2.crBody = case requestBody req of
        U.BodyEmpty -> H2.ReqBodyNone
        U.BodyBytes bs -> H2.ReqBodyBytes bs
        U.BodyStream p -> H2.ReqBodyStream p
    }


-- | Drain a 'U.Body' to a strict 'ByteString'.
drainUnifiedBody :: U.Body -> IO BS.ByteString
drainUnifiedBody body = case body of
  U.BodyEmpty -> pure BS.empty
  U.BodyBytes b -> pure b
  U.BodyStream p -> go []
    where
      go acc =
        p >>= \case
          Nothing -> pure $! BS.concat (reverse acc)
          Just b -> go (b : acc)


{- | Materialise a server-pushed 'H2.ClientResponse' into a 'RawResponse'
with a fully-drained body.  The push body is consumed before returning
so the caller does not need to manage the H/2 stream lifetime.
-}
materializePushResponse :: H2.ClientResponse -> IO RawResponse
materializePushResponse cr = do
  let resp = h2ResponseToUnified cr
  bodyBs <- drainUnifiedBody (responseBody resp)
  popper <- popperFromStrict bodyBs
  pure
    RawResponse
      { statusCode = responseStatus resp
      , headers = responseHeaders resp
      , bodyPopper = popper
      , protocolInfo = case responseVersion resp of
          U.HTTP2 ->
            HTTP2
              Http2Info
                { h2StreamId = responseH2StreamId resp
                , -- Nested push promises on a pushed response are not
                  -- followed recursively (extremely rare in practice and
                  -- structurally unsound with a finite value).
                  h2PushPromises = pure []
                , h2CancelStream = responseCancel resp
                }
          _ -> HTTP1_1
      }


h2ResponseToUnified :: H2.ClientResponse -> Response
h2ResponseToUnified h2resp =
  Response
    { responseStatus = U.Status (fromIntegral (H2.crStatus h2resp))
    , responseVersion = U.HTTP2
    , responseHeaders = Conv.fromHttp2Headers (H2.crResponseHeaders h2resp)
    , responseBody = U.BodyStream (H2.crResponseBody h2resp)
    , responseTrailers = Conv.fromHttp2Headers <$> H2.crResponseTrailers h2resp
    , -- Body + trailers are both pull-shaped; both are only valid
      -- for the lifetime of the surrounding 'withConnection' bracket --
      -- consume them before exiting.
      responseH2StreamId = fromIntegral (H2.crStreamId h2resp)
    , responseCancel = H2.crCancel h2resp
    , responsePushPromises = fmap toPushPromise <$> H2.crPushPromises h2resp
    }
  where
    toPushPromise pp =
      ResponsePushPromise
        { rppPromisedStreamId = H2.pprPromisedStreamId pp
        , rppHeaders = Conv.fromHttp2Headers (H2.pprHeaders pp)
        , rppFulfil = materializePushResponse =<< H2.pprFulfil pp
        }
