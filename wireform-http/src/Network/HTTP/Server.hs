{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Unified HTTP server with version negotiation.

A 'ServerConfig' carries a 'VersionRange' that declares which on-wire
HTTP versions the server is willing to speak.  'runServer' binds a
TCP listener and dispatches each accepted connection to the
appropriate per-version runtime; connections that don't match the
range are dropped during negotiation.

Negotiation:

* __Plaintext__ — dispatched on the preferred version of the range:
  'http1Only' \/ 'preferHttp1' runs the HTTP\/1.x server; 'http2Only'
  \/ 'preferHttp2' runs the HTTP\/2 server (and requires the client
  to send the @PRI * HTTP\/2.0@ preface up front, i.e.
  prior-knowledge h2c).  An h2c @Upgrade:@ handshake (RFC 7540 § 3.2)
  is still TODO.
* __TLS__ — handshake done with ALPN advertising the 'VersionRange'
  protocols; the server picks the highest-preference overlap.  Both
  HTTP\/2 and HTTP\/1.x ride on the same listener.  Connections
  whose negotiated protocol isn't in range are dropped after the
  handshake.

The handler is a plain @'Request' -> IO 'Response'@: the HTTP\/2
server's continuation shape is adapted internally.
-}
module Network.HTTP.Server (
  -- * Configuration
  ServerConfig (..),
  defaultServerConfig,
  TlsServerConfig (..),
  ServerLimits (..),
  defaultServerLimits,

  -- * Running
  runServer,
  runServerOnListener,
  runServerOnTransport,
  handleAcceptedSocket,

  -- * Handler
  Handler,

  -- * Response defaults and OPTIONS \/ Allow
  addServerDefaults,
  optionsAllowResponse,
  methodNotAllowed,
) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Exception (SomeException, catch)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.CaseInsensitive qualified as CI
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List qualified as List
import Data.Time.Clock (getCurrentTime)
import Data.Word (Word8)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr)
import Network.HTTP.HttpDate (formatHttpDate)
import Network.HTTP.Internal.Convert qualified as Conv
import Network.HTTP.Message
import Network.HTTP.TLS qualified as TLS
import Network.HTTP.Types.Body qualified as U
import Network.HTTP.Types.Header qualified as U
import Network.HTTP.Types.Method qualified as U
import Network.HTTP.Types.Status qualified as U
import Network.HTTP.Types.Version qualified as U
import Network.HTTP.VersionRange
import Network.HTTP1.Connection qualified as H1C
import Network.HTTP1.Server qualified as H1
import Network.HTTP1.Types qualified as H1
import Network.HTTP2.Server qualified as H2
import Network.HTTP2.Transport qualified as H2T
import Network.Socket qualified as NS
import Network.Socket.ByteString qualified as NBS
import Wireform.Network (
  defaultTransportConfig,
  newDuplexBufTransport,
  newDuplexBufTransportPooled,
 )
import Wireform.Ring.Pool (RingPool)
import Wireform.Transport.Config qualified as WC


{- | The user's request handler.  Operates on unified
'Network.HTTP.Message.Request' \/ 'Response' so it's portable
between HTTP\/1.x and HTTP\/2.
-}
type Handler = Request -> IO Response


data ServerConfig = ServerConfig
  { serverHost :: !String
  , serverPort :: !String
  , serverVersionRange :: !VersionRange
  {- ^ Versions the server is willing to speak.  The preferred
  version drives the plaintext dispatch.
  -}
  , serverHandler :: !Handler
  , serverTls :: !(Maybe TlsServerConfig)
  , serverForkConnection :: IO () -> IO ThreadId
  {- ^ How to fork the per-connection thread.  Defaults to 'forkIO';
  use 'Control.Concurrent.forkOn' for pinned-core scheduling.
  -}
  , serverNameToken :: !(Maybe ByteString)
  {- ^ Default value for the @Server@ response header
  (RFC 9110 \u00a710.2.4). 'Nothing' suppresses the header
  entirely.
  -}
  , serverEmitDate :: !Bool
  {- ^ Auto-inject a @Date@ header on responses that don't carry
  one (RFC 9110 \u00a710.2.2). Defaults to 'True'.
  -}
  , serverLimits :: !ServerLimits
  , serverRingPool :: !(Maybe RingPool)
  {- ^ Optional pool of pre-allocated magic ring buffers for
  connection recycling. See 'Wireform.Ring.Pool'.
  -}
  }


{- | Per-connection limits.  The unified server passes these through
to the underlying HTTP\/1.x and HTTP\/2 server runtimes; honouring
them is up to those layers (and currently only enforced at the
HTTP\/2 frame layer, where their absence is the most damaging).
-}
data ServerLimits = ServerLimits
  { limitMaxHeaderBytes :: !Int
  {- ^ Cap on the cumulative header block size, including request
  line. Default: 64 KiB. RFC 9112 has no fixed value but the
  common practice cap is 8\u201364 KiB. The unified server
  enforces this against the sum of @name + \": \" + value + 2@
  bytes of every parsed header (a close proxy for the
  raw block size; the parser layer has already discarded the
  exact byte count by the time we get here). Violations
  short-circuit the handler and emit a @431 Request Header
  Fields Too Large@ response per RFC 6585 §5.
  -}
  , limitMaxRequestTargetBytes :: !Int
  {- ^ Cap on the @request-target@ size (path + query). Default:
  8 KiB. Violations short-circuit the handler and emit a
  @414 URI Too Long@ response per RFC 9110 §15.5.15.
  -}
  , limitMaxRequestBody :: !(Maybe Int)
  {- ^ Cap on inbound request body length. 'Nothing' = unlimited.
  Enforcement currently delegates to user middleware that
  wraps 'requestBody'; ServerConfig surfaces the value so the
  middleware can read one consistent number.
  -}
  , limitReadTimeoutSecs :: !(Maybe Double)
  -- ^ Idle-read timeout. 'Nothing' disables.
  , limitWriteTimeoutSecs :: !(Maybe Double)
  -- ^ Per-write timeout. 'Nothing' disables.
  }


defaultServerLimits :: ServerLimits
defaultServerLimits =
  ServerLimits
    { limitMaxHeaderBytes = 64 * 1024
    , limitMaxRequestTargetBytes = 8 * 1024
    , limitMaxRequestBody = Just (32 * 1024 * 1024) -- 32 MiB
    , limitReadTimeoutSecs = Just 30
    , limitWriteTimeoutSecs = Just 30
    }


data TlsServerConfig = TlsServerConfig
  { tlsServerCertPath :: !FilePath
  , tlsServerKeyPath :: !FilePath
  }


defaultServerConfig :: ServerConfig
defaultServerConfig =
  ServerConfig
    { serverHost = "0.0.0.0"
    , serverPort = "8080"
    , serverVersionRange = http1Only
    , serverHandler = \_ -> pure stubResponse
    , serverTls = Nothing
    , serverForkConnection = forkIO
    , serverNameToken = Just "wireform-http"
    , serverEmitDate = True
    , serverLimits = defaultServerLimits
    , serverRingPool = Nothing
    }
  where
    stubResponse =
      Response
        { responseStatus = U.status200
        , responseVersion = U.HTTP1_1
        , responseHeaders = []
        , responseBody = U.BodyEmpty
        , responseTrailers = pure []
        , responseH2StreamId = 0
        , responseCancel = pure ()
        , responsePushPromises = pure []
        }


-- ---------------------------------------------------------------------------
-- Response defaults / OPTIONS / Allow
-- ---------------------------------------------------------------------------

{- | Inject @Server@ and @Date@ headers when the handler hasn't set
them already.  Single pass over the header list to check both.
-}
addServerDefaults :: ServerConfig -> Response -> IO Response
addServerDefaults cfg r0 = do
  let hdrs0 = responseHeaders r0
      (!hasDate, !hasServer) = scanForDateServer hdrs0
  date <-
    if serverEmitDate cfg && not hasDate
      then do
        t <- getCurrentTime
        pure [(U.hDate, formatHttpDate t)]
      else pure []
  let server = case serverNameToken cfg of
        Just tok | not hasServer -> [(U.hServer, tok)]
        _ -> []
  pure r0 {responseHeaders = hdrs0 <> server <> date}


-- | Single pass: check for Date and Server headers simultaneously.
scanForDateServer :: U.Headers -> (Bool, Bool)
scanForDateServer = go False False
  where
    go !d !s [] = (d, s)
    go !d !s ((n, _) : rest)
      | d && s = (d, s)
      | not d && n == U.hDate = go True s rest
      | not s && n == U.hServer = go d True rest
      | otherwise = go d s rest


{- | Build a response to @OPTIONS *@ that advertises the methods the
server understands. Sets @Allow@ and a no-body 200.
-}
optionsAllowResponse :: [U.Method] -> Response
optionsAllowResponse methods =
  Response
    { responseStatus = U.status200
    , responseVersion = U.HTTP1_1
    , responseHeaders = [(U.hAllow, allowValue methods)]
    , responseBody = U.BodyEmpty
    , responseTrailers = pure []
    , responseH2StreamId = 0
    , responseCancel = pure ()
    , responsePushPromises = pure []
    }


{- | Build a 405 response with @Allow@ enumerating the supported
methods on this resource.
-}
methodNotAllowed :: [U.Method] -> Response
methodNotAllowed methods =
  Response
    { responseStatus = U.status405
    , responseVersion = U.HTTP1_1
    , responseHeaders = [(U.hAllow, allowValue methods)]
    , responseBody = U.BodyEmpty
    , responseTrailers = pure []
    , responseH2StreamId = 0
    , responseCancel = pure ()
    , responsePushPromises = pure []
    }


allowValue :: [U.Method] -> ByteString
allowValue = BS.intercalate ", " . map U.fromMethod . List.nub


-- | Bind a TCP listener and serve until killed.
runServer :: ServerConfig -> IO ()
runServer cfg = case serverTls cfg of
  Just tlsCfg -> runTlsServer cfg tlsCfg
  Nothing ->
    let preferred = preferredVersion (serverVersionRange cfg)
    in if preferred == U.HTTP2
         then runHttp2 cfg
         else runHttp1 cfg

{- | Serve a __single connection__ over a caller-supplied 'H2T.Transport'
(e.g. one end of an in-memory @wireform-dst-net@ fault link) instead of
binding a socket, speaking HTTP\/2 prior-knowledge (h2c). Returns when the
connection closes. The counterpart of the HTTP\/2 client's
'Network.HTTP.Connection.withConnectionOnTransport'; the two let the whole
@wireform-http@ / @wireform-connect@ stack run over a transport with no
socket. (Plaintext h2c only — there is no version sniff on a raw transport,
so both ends must agree to speak HTTP\/2.)
-}
runServerOnTransport :: ServerConfig -> H2T.Transport -> IO ()
runServerOnTransport cfg transport =
  H2.runServerOnTransport
    (mkH2Config cfg) {H2.serverHandler = wrapHttp2Handler cfg (serverHandler cfg)}
    transport


runTlsServer :: ServerConfig -> TlsServerConfig -> IO ()
runTlsServer cfg tlsCfg =
  TLS.runTlsServer
    (serverHost cfg)
    (serverPort cfg)
    (tlsServerCertPath tlsCfg)
    (tlsServerKeyPath tlsCfg)
    (serverVersionRange cfg)
    -- HTTP/1.x dispatch over the TLS connection. The TLS layer
    -- hands us an 'SslConn' for each accepted-and-handshaked
    -- connection; the http1 server's 'runServerOnTls' wraps it
    -- into the usual per-connection request loop.
    (\_v sslConn -> H1.runServerOnTls (mkH1Config cfg) sslConn)
    -- HTTP/2 ServerConfig factory.
    (\_v -> (mkH2Config cfg) {H2.serverHandler = wrapHttp2Handler cfg (serverHandler cfg)})


mkH1Config :: ServerConfig -> H1.ServerConfig
mkH1Config cfg =
  H1.defaultServerConfig
    { H1.serverHost = serverHost cfg
    , H1.serverPort = serverPort cfg
    , H1.serverForkConnection = serverForkConnection cfg
    , H1.serverHandler = wrapHttp1Handler cfg (serverHandler cfg)
    , H1.serverRingPool = serverRingPool cfg
    }


mkH2Config :: ServerConfig -> H2.ServerConfig
mkH2Config cfg =
  H2.defaultServerConfig
    { H2.serverHost = serverHost cfg
    , H2.serverPort = serverPort cfg
    , H2.serverForkConnection = serverForkConnection cfg
    }


runHttp1 :: ServerConfig -> IO ()
runHttp1 cfg = H1.runServer (mkH1Config cfg)


{- | Drive the server over an already-bound listening 'NS.Socket'.
Useful for tests that want to pick an ephemeral port via
@bind 0@ and inspect what port the kernel handed out before any
client connects.

The version-dispatch mirrors 'runServer': we look at the
'serverVersionRange'\'s preferred version and run the matching
per-connection runtime.  TLS isn't wired through this entry point
— use the plain 'runServer' with a 'TlsServerConfig' for TLS, or
bind the listener and call 'Network.HTTP.TLS.runTlsServer'-style
helpers directly.
-}
runServerOnListener :: ServerConfig -> NS.Socket -> IO ()
runServerOnListener cfg listenSock = acceptLoop
  where
    acceptLoop = do
      (clientSock, _) <- NS.accept listenSock
      NS.setSocketOption clientSock NS.NoDelay 1
      _ <-
        serverForkConnection cfg $
          handleAcceptedSocket cfg clientSock
            `catch` (\(_ :: SomeException) -> NS.close clientSock)
      acceptLoop


{- | Dispatch a single accepted socket to either the HTTP\/1.x or
HTTP\/2 per-connection runtime based on the configured
'VersionRange'.  Exposed mainly so test fixtures can wire up a
single connection without standing up a full accept loop.

For mixed plaintext ranges (a 'VersionRange' that allows /both/
HTTP\/1.x and HTTP\/2 prior-knowledge) we sniff the first bytes
of the connection: if they match the HTTP\/2 connection preface
(@\"PRI * HTTP\/2.0\\r\\n\\r\\nSM\\r\\n\\r\\n\"@) we dispatch the
HTTP\/2 runtime, otherwise the HTTP\/1.x runtime. The peeked
bytes are then prepended to the per-version runtime via
'bufferedRecvTransport' so neither runtime observes the sniff.
-}
handleAcceptedSocket :: ServerConfig -> NS.Socket -> IO ()
handleAcceptedSocket cfg sock =
  let allowed = versionRangeList (serverVersionRange cfg)
      mixed =
        U.HTTP2 `elem` allowed
          && (U.HTTP1_1 `elem` allowed || U.HTTP1_0 `elem` allowed)
      preferred = preferredVersion (serverVersionRange cfg)
  in case (mixed, preferred) of
       (False, U.HTTP2) -> dispatchH2 cfg sock BS.empty
       (False, _) -> dispatchH1 cfg sock BS.empty
       (True, _) -> sniffAndDispatch cfg sock


{- | Read up to the length of the HTTP\/2 preface from the socket
and decide which runtime to dispatch.
-}
sniffAndDispatch :: ServerConfig -> NS.Socket -> IO ()
sniffAndDispatch cfg sock = do
  peeked <- recvAtLeast sock (BS.length http2Preface)
  if http2Preface `BS.isPrefixOf` peeked
    then dispatchH2 cfg sock peeked
    else dispatchH1 cfg sock peeked


http2Preface :: ByteString
http2Preface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"


{- | Loop until at least @n@ bytes have been received or the peer
closes. May return fewer if EOF arrives early.
-}
recvAtLeast :: NS.Socket -> Int -> IO ByteString
recvAtLeast s n = go BS.empty
  where
    go acc
      | BS.length acc >= n = pure acc
      | otherwise = do
          chunk <- NBS.recv s (n - BS.length acc)
          if BS.null chunk
            then pure acc
            else go (acc <> chunk)


{- | Dispatch HTTP\/1.x with @prebuf@ bytes already pulled off the
socket (will be the first bytes the runtime sees).
-}
dispatchH1 :: ServerConfig -> NS.Socket -> ByteString -> IO ()
dispatchH1 cfg sock prebuf
  | BS.null prebuf = H1.runServerOnSocket (mkH1Config cfg) sock
  | otherwise = do
      conn <- prebufferedH1Connection (serverRingPool cfg) sock prebuf
      H1.runServerOnConnection (mkH1Config cfg) conn


dispatchH2 :: ServerConfig -> NS.Socket -> ByteString -> IO ()
dispatchH2 cfg sock prebuf =
  let h2cfg =
        (mkH2Config cfg)
          { H2.serverHandler = wrapHttp2Handler cfg (serverHandler cfg)
          }
  in if BS.null prebuf
       then H2.runServerOnSocket h2cfg sock
       else do
         transport <- prebuffered2 sock prebuf
         H2.runServerOnTransport h2cfg transport


{- | Build an HTTP\/1 'H1C.Connection' from a 'NS.Socket' with @prebuf@
bytes already pulled off the wire (delivered as the first bytes
the receive ring sees) on the receive side.
-}
prebufferedH1Connection :: Maybe RingPool -> NS.Socket -> ByteString -> IO H1C.Connection
prebufferedH1Connection mPool sock prebuf = do
  ref <- newIORef prebuf
  let !cfg = defaultTransportConfig {WC.ringSizeHint = 256 * 1024}
      mkDuplex = case mPool of
        Just pool -> newDuplexBufTransportPooled pool cfg
        Nothing -> newDuplexBufTransport cfg
  duplex <-
    mkDuplex
      (prefixedRecv ref sock)
      (NS.sendBuf sock)
      (NS.shutdown sock NS.ShutdownSend)
  H1C.newConnectionFromDuplex duplex


{- | Build a 'H2T.Transport' from a 'NS.Socket' whose first
@prebuf@ bytes have already been pulled off the wire (from the
HTTP\/2 sniff).  The 'H2T.Transport' record is pointer-based
now, so we hand-roll a recv that drains @prebuf@ into the
caller's buffer before falling through to 'NS.recvBuf'.
-}
prebuffered2 :: NS.Socket -> ByteString -> IO H2T.Transport
prebuffered2 sock prebuf = do
  ref <- newIORef prebuf
  pure
    H2T.Transport
      { H2T.tSendFn = NS.sendBuf sock
      , H2T.tRecvBuf = prefixedRecv ref sock
      , H2T.tShutdownWrite = NS.shutdown sock NS.ShutdownSend
      , H2T.tClose = NS.close sock
      }


prefixedRecv :: IORef ByteString -> NS.Socket -> Ptr Word8 -> Int -> IO Int
prefixedRecv ref sock dst want = do
  buf <- readIORef ref
  if BS.null buf
    then NS.recvBuf sock dst want
    else do
      let !take_ = min (BS.length buf) want
          !taken = BS.take take_ buf
          !rest = BS.drop take_ buf
      writeIORef ref rest
      BSU.unsafeUseAsCStringLen taken $ \(src, len) ->
        copyBytes dst (castPtr src) len
      pure take_
{-# INLINE prefixedRecv #-}


wrapHttp1Handler :: ServerConfig -> Handler -> H1.Request -> IO H1.Response
wrapHttp1Handler cfg handler h1req = do
  let req = Conv.fromHttp1Request SchemeHttp h1req
  resp0 <- case enforceLimits cfg req of
    Just bad -> pure bad
    Nothing -> handler req
  resp <- addServerDefaults cfg resp0
  let h1resp = Conv.toHttp1Response resp
  pure h1resp {H1.responseVersion = H1.requestVersion h1req}


runHttp2 :: ServerConfig -> IO ()
runHttp2 cfg = H2.runServer h2cfg
  where
    h2cfg =
      (mkH2Config cfg)
        { H2.serverHandler = wrapHttp2Handler cfg (serverHandler cfg)
        }


wrapHttp2Handler :: ServerConfig -> Handler -> H2.Request -> (H2.Response -> IO ()) -> IO ()
wrapHttp2Handler cfg handler h2req respond = do
  let req = Conv.fromHttp2Request h2req
  resp0 <- case enforceLimits cfg req of
    Just bad -> pure bad
    Nothing -> handler req
  resp <- addServerDefaults cfg resp0
  h2resp <- Conv.toHttp2Response resp
  respond h2resp


{- | Pre-handler check for 'limitMaxHeaderBytes' and
'limitMaxRequestTargetBytes'. Returns 'Nothing' when the
request fits both limits; otherwise returns the canonical
414 \/ 431 response so the handler is skipped entirely.
-}
enforceLimits :: ServerConfig -> Request -> Maybe Response
enforceLimits cfg req
  | targetSize > limitMaxRequestTargetBytes lims = Just uriTooLong
  | headerSize > limitMaxHeaderBytes lims = Just headerFieldsTooLarge
  | otherwise = Nothing
  where
    lims = serverLimits cfg
    targetSize = BS.length (requestTarget req)
    -- "name: value\r\n" → +4 bytes of overhead per entry.
    headerSize = sum (map onePair (requestHeaders req))
    onePair (n, v) = BS.length (CI.original n) + BS.length v + 4


uriTooLong :: Response
uriTooLong =
  Response
    { responseStatus = U.status414
    , responseVersion = U.HTTP1_1
    , responseHeaders = [(U.hContentLength, "0"), (U.hConnection, "close")]
    , responseBody = U.BodyEmpty
    , responseTrailers = pure []
    , responseH2StreamId = 0
    , responseCancel = pure ()
    , responsePushPromises = pure []
    }


headerFieldsTooLarge :: Response
headerFieldsTooLarge =
  Response
    { responseStatus = U.status431
    , responseVersion = U.HTTP1_1
    , responseHeaders = [(U.hContentLength, "0"), (U.hConnection, "close")]
    , responseBody = U.BodyEmpty
    , responseTrailers = pure []
    , responseH2StreamId = 0
    , responseCancel = pure ()
    , responsePushPromises = pure []
    }
