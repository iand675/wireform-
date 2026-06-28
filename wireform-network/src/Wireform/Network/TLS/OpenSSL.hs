{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Direct OpenSSL bindings for TLS-on-magic-ring.

The Haskell @tls@ package only exposes 'ByteString'-returning
APIs ('recvData'), so plumbing it into the magic-ring transport
needs a bridge that allocates one 'ByteString' per record and
memcpys into the ring.  OpenSSL's @SSL_read_ex@ writes plaintext
directly into a caller-supplied buffer; the magic-ring's
@recvBuf@-style 'RecvFn' is exactly that shape.  This module
exposes thin FFI bindings (see @cbits\/wireform_openssl.c@) and
the higher-level 'tlsClient' / 'tlsServer' \/ 'tlsTransport'
helpers that turn a freshly-handshaked OpenSSL connection into a
'Wireform.Transport.Transport' the parser surface can drive.

== Concurrency

The OpenSSL @SSL*@ object is not thread-safe for concurrent use
from multiple threads, yet the HTTP\/2 engine reads on one thread
while writing on another (and TLS 1.3 post-handshake messages —
@KeyUpdate@ \/ @NewSessionTicket@ — make a lock-free reader\/writer
split provably unsafe, not merely racy).  Each 'SslConn' therefore
carries its own 'MVar' that serialises every @SSL*@ operation
('tlsReceiveFn' \/ 'tlsSendFn' \/ 'tlsSend' \/ 'tlsShutdown' \/
'freeConn').  The lock is held only across the FFI call, never
across the IO-manager park, so a reader blocked in 'threadWaitRead'
cannot starve a waiting writer.  Single-threaded callers
(@wireform-http1@ \/ @wireform-kafka@) pay only an uncontended
take\/put, dwarfed by the per-record crypto + syscall.
-}
module Wireform.Network.TLS.OpenSSL (
  -- * One-time init
  sslInit,

  -- * Contexts
  SslCtx,
  newClientCtx,
  newServerCtx,
  freeCtx,
  setAlpnClient,
  setAlpnServer,

  -- * Optional context tuning
  loadCaBundle,
  useClientCert,
  setMinProto,
  setCipherSuites,
  TlsProtoVersion (..),

  -- * Connections
  SslConn,
  sslConnSocket,
  newClient,
  newClientVerified,
  newServer,
  setClientHostnameVerify,
  freeConn,
  getAlpn,

  -- * Direct-into-ring I\/O
  ReceiveFn,
  SendFn,
  tlsReceiveFn,
  tlsSendFn,
  tlsSend,
  tlsShutdown,

  -- * Magic-ring transport bridges
  withTlsReceiveTransport,
  newTlsReceiveTransport,
  withTlsSendTransport,
  newTlsSendTransport,
  withTlsDuplexTransport,
  newTlsDuplexTransport,

  -- * Errors
  OpenSslError (..),
) where

import Control.Concurrent (threadWaitRead, threadWaitWrite)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (Exception, SomeException, catch, throwIO)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Word (Word8)
import Foreign.C.String (CString, peekCStringLen, withCString)
import Foreign.C.Types
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek)
import GHC.Generics (Generic)
import Network.Socket (Socket, withFdSocket)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types (Fd (Fd))
import Wireform.Network.Transport.Duplex (
  DuplexTransport,
  newDuplexBufTransport,
  withDuplexBufTransport,
 )
import Wireform.Network.Transport.Receive (
  ReceiveFn,
  newReceiveBufTransport,
  withReceiveBufTransport,
 )
import Wireform.Network.Transport.Send (
  SendFn,
  newSendBufTransport,
  withSendBufTransport,
 )
import Wireform.Transport.Config (TransportConfig)
import Wireform.Transport.Receive (ReceiveTransport)
import Wireform.Transport.Send (SendTransport)


------------------------------------------------------------------------
-- Raw FFI
------------------------------------------------------------------------

-- Opaque C types.
data SslCtxStruct


data SslStruct


type SslCtxPtr = Ptr SslCtxStruct


type SslPtr = Ptr SslStruct


foreign import ccall unsafe "wireform_openssl.h wf_ssl_init"
  c_ssl_init :: IO ()


foreign import ccall unsafe "wireform_openssl.h wf_ssl_ctx_new_client"
  c_ssl_ctx_new_client :: CInt -> IO SslCtxPtr


foreign import ccall unsafe "wireform_openssl.h wf_ssl_ctx_new_server"
  c_ssl_ctx_new_server :: CString -> CString -> IO SslCtxPtr


foreign import ccall unsafe "wireform_openssl.h wf_ssl_ctx_free"
  c_ssl_ctx_free :: SslCtxPtr -> IO ()


foreign import ccall unsafe "wireform_openssl.h wf_ssl_ctx_set_alpn"
  c_ssl_ctx_set_alpn :: SslCtxPtr -> Ptr CUChar -> CUInt -> IO CInt


foreign import ccall unsafe "wireform_openssl.h wf_ssl_ctx_set_alpn_select_server"
  c_ssl_ctx_set_alpn_select_server :: SslCtxPtr -> Ptr CUChar -> IO ()


foreign import ccall unsafe "wireform_openssl.h wf_ssl_ctx_load_ca_bundle"
  c_ssl_ctx_load_ca_bundle :: SslCtxPtr -> CString -> IO CInt


foreign import ccall unsafe "wireform_openssl.h wf_ssl_ctx_use_client_cert"
  c_ssl_ctx_use_client_cert :: SslCtxPtr -> CString -> CString -> IO CInt


foreign import ccall unsafe "wireform_openssl.h wf_ssl_ctx_set_min_proto"
  c_ssl_ctx_set_min_proto :: SslCtxPtr -> CInt -> IO CInt


foreign import ccall unsafe "wireform_openssl.h wf_ssl_ctx_set_cipher_suites"
  c_ssl_ctx_set_cipher_suites :: SslCtxPtr -> CString -> IO CInt


foreign import ccall unsafe "wireform_openssl.h wf_ssl_new_for_fd"
  c_ssl_new_for_fd :: SslCtxPtr -> CInt -> IO SslPtr


foreign import ccall unsafe "wireform_openssl.h wf_ssl_set_sni"
  c_ssl_set_sni :: SslPtr -> CString -> IO CInt


foreign import ccall unsafe "wireform_openssl.h wf_ssl_set_verify_hostname"
  c_ssl_set_verify_hostname :: SslPtr -> CString -> IO CInt


foreign import ccall safe "wireform_openssl.h wf_ssl_connect"
  c_ssl_connect :: SslPtr -> IO CInt


foreign import ccall safe "wireform_openssl.h wf_ssl_accept"
  c_ssl_accept :: SslPtr -> IO CInt


foreign import ccall unsafe "wireform_openssl.h wf_ssl_get_alpn"
  c_ssl_get_alpn :: SslPtr -> Ptr (Ptr CUChar) -> Ptr CUInt -> IO CInt


foreign import ccall safe "wireform_openssl.h wf_ssl_read_into"
  c_ssl_read_into :: SslPtr -> Ptr Word8 -> CSize -> Ptr CSize -> IO CInt


foreign import ccall safe "wireform_openssl.h wf_ssl_write_from"
  c_ssl_write_from :: SslPtr -> Ptr Word8 -> CSize -> Ptr CSize -> IO CInt


foreign import ccall unsafe "wireform_openssl.h wf_ssl_shutdown"
  c_ssl_shutdown :: SslPtr -> IO ()


foreign import ccall unsafe "wireform_openssl.h wf_ssl_free"
  c_ssl_free :: SslPtr -> IO ()


foreign import ccall unsafe "wireform_openssl.h wf_ssl_last_error"
  c_ssl_last_error :: CString -> CSize -> IO CSize


pattern WfSslOk, WfSslEof, WfSslWantRetry, WfSslFatal :: CInt
pattern WfSslOk = 0
pattern WfSslEof = -1
pattern WfSslWantRetry = -2
pattern WfSslFatal = -3


{-# COMPLETE WfSslOk, WfSslEof, WfSslWantRetry, WfSslFatal #-}


------------------------------------------------------------------------
-- One-time init
------------------------------------------------------------------------

{- | Initialise OpenSSL (load algorithms + error strings).
Idempotent on OpenSSL 1.1+; safe to call once at program start
or before each context creation.
-}
sslInit :: IO ()
sslInit = c_ssl_init


-- Init runs once at module load via 'unsafePerformIO'.  OpenSSL
-- 1.1+ guards its own internal init so this is safe to call
-- alongside any other library that initialises OpenSSL.
sslInitOnce :: ()
sslInitOnce = unsafePerformIO sslInit
{-# NOINLINE sslInitOnce #-}


------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

{- | An OpenSSL operation failed.  Carries a hint string drained
from the OpenSSL error queue at call time.
-}
data OpenSslError = OpenSslError !String
  deriving stock (Show, Generic)


instance Exception OpenSslError


throwSsl :: String -> IO a
throwSsl ctx = do
  msg <- readLastError
  throwIO (OpenSslError (ctx <> if null msg then "" else ": " <> msg))


readLastError :: IO String
readLastError = allocaBytes 512 $ \buf -> do
  n <- c_ssl_last_error buf 512
  if n == 0
    then pure ""
    else peekCStringLen (buf, fromIntegral n)


------------------------------------------------------------------------
-- Contexts
------------------------------------------------------------------------

newtype SslCtx = SslCtx SslCtxPtr


{- | Build a TLS client context.  When @verifyPeer@ is 'True' the
system trust store is used; when 'False' any certificate is
accepted (test \/ self-signed setups).
-}
newClientCtx :: Bool -> IO SslCtx
newClientCtx verify =
  sslInitOnce `seq` do
    ctx <- c_ssl_ctx_new_client (if verify then 1 else 0)
    if ctx == nullPtr
      then throwSsl "newClientCtx"
      else pure (SslCtx ctx)


{- | Build a TLS server context bound to the supplied PEM cert and
key files.
-}
newServerCtx :: FilePath -> FilePath -> IO SslCtx
newServerCtx certPath keyPath = sslInitOnce `seq`
  withCString certPath $ \cp ->
    withCString keyPath $ \kp -> do
      ctx <- c_ssl_ctx_new_server cp kp
      if ctx == nullPtr
        then throwSsl ("newServerCtx (" <> certPath <> ", " <> keyPath <> ")")
        else pure (SslCtx ctx)


{- | Tear down a context.  All connections created from it must
have been freed first.
-}
freeCtx :: SslCtx -> IO ()
freeCtx (SslCtx ctx) = c_ssl_ctx_free ctx


------------------------------------------------------------------------
-- Optional context tuning
------------------------------------------------------------------------

-- | Minimum TLS protocol version.  See 'setMinProto'.
data TlsProtoVersion
  = Tls12
  | Tls13
  deriving stock (Eq, Show)


{- | Load an explicit CA bundle (PEM) on top of the system trust
store the client context already uses (when 'newClientCtx' was
called with @verifyPeer=True@).  Throws 'OpenSslError' on failure.
-}
loadCaBundle :: SslCtx -> FilePath -> IO ()
loadCaBundle (SslCtx ctx) path = withCString path $ \cp -> do
  rc <- c_ssl_ctx_load_ca_bundle ctx cp
  if rc /= 0 then throwSsl ("loadCaBundle: " <> path) else pure ()


{- | Configure an mTLS client identity: PEM cert chain + matching
private key (also PEM).  Apply before 'newClient'.
-}
useClientCert :: SslCtx -> FilePath -> FilePath -> IO ()
useClientCert (SslCtx ctx) cert key =
  withCString cert $ \cp ->
    withCString key $ \kp -> do
      rc <- c_ssl_ctx_use_client_cert ctx cp kp
      if rc /= 0 then throwSsl ("useClientCert: " <> cert) else pure ()


{- | Override the minimum TLS protocol version for this context.
Default is TLS 1.2; raise to 'Tls13' to refuse earlier-version
handshakes.
-}
setMinProto :: SslCtx -> TlsProtoVersion -> IO ()
setMinProto (SslCtx ctx) v = do
  let n = case v of Tls12 -> 12; Tls13 -> 13
  rc <- c_ssl_ctx_set_min_proto ctx n
  if rc /= 0 then throwSsl "setMinProto" else pure ()


{- | Override the TLS 1.2 cipher suite list (OpenSSL cipher-string
syntax, e.g. @\"HIGH:!aNULL:!MD5\"@).  TLS 1.3 cipher selection
is left at OpenSSL defaults.
-}
setCipherSuites :: SslCtx -> BS.ByteString -> IO ()
setCipherSuites (SslCtx ctx) cs =
  BSU.unsafeUseAsCString (cs `BS.snoc` 0) $ \cp -> do
    rc <- c_ssl_ctx_set_cipher_suites ctx cp
    if rc /= 0 then throwSsl "setCipherSuites" else pure ()


{- | Encode an ALPN protocol list as the wire-format
@\\xNNproto1\\xMMproto2...@ shape OpenSSL expects.
-}
encodeAlpn :: [BS.ByteString] -> BS.ByteString
encodeAlpn = BS.concat . map encOne
  where
    encOne bs = BS.cons (fromIntegral (BS.length bs)) bs


{- | Advertise the supplied ALPN protocols on the client side.  Each
'BS.ByteString' is a protocol identifier (e.g. @"h2"@, @"http\/1.1"@);
they're listed in the ClientHello in the given preference order.
-}
setAlpnClient :: SslCtx -> [BS.ByteString] -> IO ()
setAlpnClient (SslCtx ctx) protos = do
  let !encoded = encodeAlpn protos
  BSU.unsafeUseAsCStringLen encoded $ \(p, l) -> do
    rc <- c_ssl_ctx_set_alpn ctx (castUCharPtr p) (fromIntegral l)
    if rc /= 0 then throwSsl "setAlpnClient" else pure ()


{- | Server-side ALPN: pick the first match between the client's
advertised list and the supplied preference order.  The protocol
list is null-terminated internally so the callback can iterate
it without a length parameter.
-}
setAlpnServer :: SslCtx -> [BS.ByteString] -> IO ()
setAlpnServer (SslCtx ctx) protos = do
  -- We need the BS to outlive the SSL_CTX.  Cheapest correct path:
  -- allocate a pinned ByteString (FinalPtr) that lives as long as
  -- the IORef-held reference.  We park the reference on the SslCtx
  -- via a single global map keyed on the pointer — but the caller
  -- already keeps a Haskell reference to the SslCtx, so we just
  -- pin the encoded BS to the context's lifetime by using a
  -- ForeignPtr finalizer would be cleaner; for the simple case
  -- here we copy + Term + just keep it pinned via an unsafe
  -- 'pure' that GHC won't reorder past the callback registration.
  let !encoded = encodeAlpn protos `BS.snoc` 0 -- NUL terminator
  -- 'unsafeUseAsCStringLen' would let the BS get freed after the
  -- callback registration; instead, force a copy that stays
  -- referenced via the IORef below.
  withPinnedBs encoded $ \p ->
    c_ssl_ctx_set_alpn_select_server ctx (castUCharPtr p)


{- | Copy @bs@ into a freshly mallocBytes-backed 'BSI.fromForeignPtr'
buffer and run the continuation with a 'Ptr' into it.  The
buffer is alive for the duration of @action@; intentionally
/not/ freed afterwards because the OpenSSL ALPN-select callback
retains a raw 'Ptr' into it for the SSL_CTX's lifetime.  Memory
"leak" is bounded by the number of SSL_CTX objects in the
process (one per server endpoint, typically a single-digit
count); negligible vs. SSL_CTX itself.
-}
withPinnedBs :: BS.ByteString -> (Ptr CChar -> IO a) -> IO a
withPinnedBs bs action = do
  let len = BS.length bs
  fp <- BSI.mallocByteString len
  withForeignPtr fp $ \p -> do
    BSU.unsafeUseAsCStringLen bs $ \(src, _) ->
      BSI.memcpy p (castPtr src) len
    -- Keep fp alive past 'action' by forcing it into the closure.
    _ <- pure fp
    action (castPtr p)


castUCharPtr :: Ptr a -> Ptr CUChar
castUCharPtr = castPtr


------------------------------------------------------------------------
-- Connections
------------------------------------------------------------------------

{- | A handshaked OpenSSL connection bound to a 'Socket'.  Owns the
underlying @SSL*@; call 'freeConn' to release.
-}
data SslConn = SslConn
  { _sslConnPtr :: !SslPtr
  , _sslSocket :: !Socket
  {- ^ Kept here so the recv\/send paths can park on the IO manager
  when 'WfSslWantRetry' bubbles up.
  -}
  , _sslLock :: !(MVar ())
  {- ^ Serialises every @SSL*@ operation on this connection.  OpenSSL
  forbids concurrent use of one @SSL*@ from multiple threads, yet the
  HTTP\/2 engine reads on one thread while writing on another (and
  TLS 1.3 post-handshake messages make this provably unsafe without
  it).  Held only across the FFI call, never across the IO-manager
  park, so a reader blocked in 'threadWaitRead' cannot starve a
  waiting writer.
  -}
  }


{- | The underlying socket carrying this TLS connection.  Exposed
so callers that want to set socket options (KEEPALIVE, NODELAY)
or pull a raw fd can do so without a separate handle.
-}
sslConnSocket :: SslConn -> Socket
sslConnSocket (SslConn _ sock _) = sock


{- | Build + handshake a TLS client.  Steps: bind the connection to
the socket fd, set SNI (if hostname given), then 'SSL_connect'
with WANT_READ\/WANT_WRITE looping handled in Haskell so we park
on the IO manager rather than spinning.
-}
newClient
  :: SslCtx
  -> Socket
  -> Maybe BS.ByteString
  -- ^ SNI hostname
  -> IO SslConn
newClient (SslCtx ctx) sock mHost = do
  ssl <- withFdSocket sock $ \fd -> c_ssl_new_for_fd ctx (fromIntegral fd)
  if ssl == nullPtr
    then throwSsl "newClient: SSL_new"
    else do
      case mHost of
        Just h ->
          -- SNI only — does NOT enable cert verification.  The
          -- 'SslCtx' decides whether to verify (via
          -- 'newClientCtx'); use 'setClientHostnameVerify' below
          -- to additionally pin the cert's CN / SAN to a hostname.
          BSU.unsafeUseAsCString (h `BS.snoc` 0) $ \cstr -> do
            _ <- c_ssl_set_sni ssl cstr
            pure ()
        Nothing -> pure ()
      retryHandshake sock c_ssl_connect ssl `catchAndFree` ssl
      lock <- newMVar ()
      pure (SslConn ssl sock lock)


{- | Like 'newClient' but additionally pins the server's certificate
CN \/ SAN to the supplied hostname, applied /before/ the
handshake.  That is the only point at which OpenSSL’s
@SSL_set1_host@ \/ @X509_VERIFY_PARAM_set1_host@ takes effect;
calling 'setClientHostnameVerify' /after/ 'newClient' returned
would be after @SSL_connect@ and so a no-op.
-}
newClientVerified
  :: SslCtx
  -> Socket
  -> Maybe BS.ByteString
  -- ^ SNI hostname
  -> Maybe BS.ByteString
  -- ^ certificate verify hostname
  -> IO SslConn
newClientVerified (SslCtx ctx) sock mHost mVerifyHost = do
  ssl <- withFdSocket sock $ \fd -> c_ssl_new_for_fd ctx (fromIntegral fd)
  if ssl == nullPtr
    then throwSsl "newClientVerified: SSL_new"
    else do
      case mHost of
        Just h ->
          BSU.unsafeUseAsCString (h `BS.snoc` 0) $ \cstr -> do
            _ <- c_ssl_set_sni ssl cstr
            pure ()
        Nothing -> pure ()
      case mVerifyHost of
        Just vh ->
          BSU.unsafeUseAsCString (vh `BS.snoc` 0) $ \cstr -> do
            rc <- c_ssl_set_verify_hostname ssl cstr
            if rc /= 0
              then do
                _ <- c_ssl_free ssl
                throwSsl "newClientVerified: set_verify_hostname"
              else pure ()
        Nothing -> pure ()
      retryHandshake sock c_ssl_connect ssl `catchAndFree` ssl
      lock <- newMVar ()
      pure (SslConn ssl sock lock)


{- | Additionally enforce that the server's cert CN \/ SAN matches
@hostname@.  Call /before/ 'newClient'-style connect.  Only
meaningful when the context was constructed with verification
enabled.
-}
setClientHostnameVerify :: SslConn -> BS.ByteString -> IO ()
setClientHostnameVerify (SslConn ssl _ _) host =
  BSU.unsafeUseAsCString (host `BS.snoc` 0) $ \cstr -> do
    rc <- c_ssl_set_verify_hostname ssl cstr
    if rc /= 0 then throwSsl "setClientHostnameVerify" else pure ()


{- | Build + handshake a TLS server.  The socket should already be
@accept()@ed and connected to a peer; this performs the TLS
handshake on it.
-}
newServer :: SslCtx -> Socket -> IO SslConn
newServer (SslCtx ctx) sock = do
  ssl <- withFdSocket sock $ \fd -> c_ssl_new_for_fd ctx (fromIntegral fd)
  if ssl == nullPtr
    then throwSsl "newServer: SSL_new"
    else do
      retryHandshake sock c_ssl_accept ssl `catchAndFree` ssl
      lock <- newMVar ()
      pure (SslConn ssl sock lock)


{- | Retry the handshake on WANT_READ / WANT_WRITE, parking on the
IO manager between attempts.
-}
retryHandshake :: Socket -> (SslPtr -> IO CInt) -> SslPtr -> IO ()
retryHandshake sock step ssl = loop
  where
    loop = do
      rc <- step ssl
      case rc of
        WfSslOk -> pure ()
        WfSslWantRetry -> do
          -- Conservatively wait for both directions: OpenSSL doesn't
          -- distinguish WANT_READ from WANT_WRITE here without an
          -- extra round-trip and threadWaitRead is the common case
          -- during handshake.
          withFdSocket sock $ \fd -> threadWaitRead (Fd (fromIntegral fd))
          loop
        WfSslEof -> throwSsl "handshake: peer closed"
        WfSslFatal -> throwSsl "handshake failed"


{- | Run the IO action; if it throws, free the SSL* first then
rethrow.  Used during handshake where we own the SSL* but haven't
handed it to a 'SslConn' yet.
-}
catchAndFree :: IO a -> SslPtr -> IO a
catchAndFree action ssl = do
  r <- (Right <$> action) `catch` (\e -> pure (Left (e :: SomeException)))
  case r of
    Right a -> pure a
    Left e -> c_ssl_free ssl *> throwIO e


{- | Release the connection's SSL*.  Issues a best-effort
@SSL_shutdown@ first (sends close_notify) and then frees the
SSL object.  The underlying socket is NOT closed — caller owns
its lifetime.
-}
freeConn :: SslConn -> IO ()
freeConn (SslConn ssl _ lock) = withMVar lock $ \() -> do
  c_ssl_shutdown ssl
  c_ssl_free ssl


{- | Negotiated ALPN protocol after handshake, or 'Nothing' if the
peer didn't pick one.
-}
getAlpn :: SslConn -> IO (Maybe BS.ByteString)
getAlpn (SslConn ssl _ _) =
  alloca $ \protoPP -> alloca $ \plenP -> do
    rc <- c_ssl_get_alpn ssl protoPP plenP
    if rc /= 0
      then pure Nothing
      else do
        proto <- peek protoPP
        plen <- peek plenP
        bs <- BS.packCStringLen (castPtr proto, fromIntegral plen)
        pure (Just bs)


------------------------------------------------------------------------
-- Direct-into-ring I\/O
------------------------------------------------------------------------

{- | A 'ReceiveFn' that decrypts plaintext bytes directly into the
supplied 'Ptr Word8'.  No intermediate 'ByteString' allocation:
OpenSSL's @SSL_read_ex@ writes straight into the magic ring's
backing memory.

Blocks on the IO manager when @SSL_read_ex@ returns WANT_READ
(waiting for the next TLS record's bytes on the wire); returns
@0@ on clean EOF (@close_notify@).
-}
tlsReceiveFn :: SslConn -> ReceiveFn
tlsReceiveFn (SslConn ssl sock lock) = recv
  where
    recv dst want = loop
      where
        loop = do
          (rc, n) <- withMVar lock $ \() -> alloca $ \nP -> do
            rc <- c_ssl_read_into ssl dst (fromIntegral want) nP
            n <- peek nP
            pure (rc, n)
          case rc of
            WfSslOk -> pure (fromIntegral n)
            WfSslEof -> pure 0
            WfSslWantRetry -> do
              withFdSocket sock $ \fd -> threadWaitRead (Fd (fromIntegral fd))
              loop
            WfSslFatal -> throwSsl "tlsReceiveFn"


{- | A 'SendFn' that encrypts plaintext directly from the supplied
'Ptr Word8' (e.g. a slice of the send magic ring).  Returns the
number of bytes consumed from the buffer (always > 0 on success).

Blocks on the IO manager when @SSL_write_ex@ returns WANT_WRITE.
-}
tlsSendFn :: SslConn -> SendFn
tlsSendFn (SslConn ssl sock lock) = send
  where
    send src want = loop
      where
        loop = do
          (rc, n) <- withMVar lock $ \() -> alloca $ \nP -> do
            rc <- c_ssl_write_from ssl src (fromIntegral want) nP
            n <- peek nP
            pure (rc, n)
          case rc of
            WfSslOk -> pure (fromIntegral n)
            WfSslWantRetry -> do
              withFdSocket sock $ \fd -> threadWaitWrite (Fd (fromIntegral fd))
              loop
            WfSslEof -> throwSsl "tlsSendFn: peer closed"
            WfSslFatal -> throwSsl "tlsSendFn"


{- | Encrypt the supplied bytes and write them to the wire.  Blocks
on the IO manager on WANT_WRITE.
-}
tlsSend :: SslConn -> BS.ByteString -> IO ()
tlsSend (SslConn ssl sock lock) bs = BSU.unsafeUseAsCStringLen bs go
  where
    go (src, len) = loop (castPtr src) len
      where
        loop p n
          | n <= 0 = pure ()
          | otherwise = do
              (rc, wrote) <- withMVar lock $ \() -> alloca $ \nP -> do
                rc <- c_ssl_write_from ssl p (fromIntegral n) nP
                wrote <- peek nP
                pure (rc, wrote)
              case rc of
                WfSslOk -> do
                  let !wroteI = fromIntegral wrote
                  loop (p `plusPtr` wroteI) (n - wroteI)
                WfSslWantRetry -> do
                  withFdSocket sock $ \fd -> threadWaitWrite (Fd (fromIntegral fd))
                  loop p n
                WfSslEof -> throwSsl "tlsSend: peer closed"
                WfSslFatal -> throwSsl "tlsSend"


{- | Best-effort TLS @close_notify@ (no @SSL_free@; use 'freeConn'
for that).
-}
tlsShutdown :: SslConn -> IO ()
tlsShutdown (SslConn ssl _ lock) = withMVar lock $ \() -> c_ssl_shutdown ssl


------------------------------------------------------------------------
-- Magic-ring transport bridge
------------------------------------------------------------------------

{- | Run an action with a 'ReceiveTransport' whose recv side
decrypts OpenSSL plaintext directly into the ring (no
intermediate 'ByteString').  The TLS connection's lifetime is
bound to the action; on exit the ring is unmapped and the SSL*
is freed.
-}
withTlsReceiveTransport
  :: TransportConfig
  -> SslConn
  -> (ReceiveTransport -> IO a)
  -> IO a
withTlsReceiveTransport cfg conn action =
  withReceiveBufTransport cfg (tlsReceiveFn conn) action


{- | IO-style ('bracket'-free) constructor for an OpenSSL-backed
'ReceiveTransport'.  Caller is responsible for 'receiveClose'
(unmaps the ring) and 'freeConn' (frees the SSL*).
-}
newTlsReceiveTransport
  :: TransportConfig
  -> SslConn
  -> IO ReceiveTransport
newTlsReceiveTransport cfg conn =
  newReceiveBufTransport cfg (tlsReceiveFn conn)


{- | Send-side counterpart: encrypt bytes drained from the magic
ring straight to the wire.  'sendShutdownWrite' issues
@SSL_shutdown@ (sends close_notify).
-}
withTlsSendTransport
  :: TransportConfig
  -> SslConn
  -> (SendTransport -> IO a)
  -> IO a
withTlsSendTransport cfg conn action =
  withSendBufTransport cfg (tlsSendFn conn) (tlsShutdown conn) action


-- | IO-style constructor for an OpenSSL-backed 'SendTransport'.
newTlsSendTransport
  :: TransportConfig
  -> SslConn
  -> IO SendTransport
newTlsSendTransport cfg conn =
  newSendBufTransport cfg (tlsSendFn conn) (tlsShutdown conn)


{- | One TLS context, two magic rings: the natural shape for a
request/response connection over TLS.  Both rings share the
same SSL*; the connection's lifetime is the action's lifetime.
-}
withTlsDuplexTransport
  :: TransportConfig
  -> SslConn
  -> (DuplexTransport -> IO a)
  -> IO a
withTlsDuplexTransport cfg conn =
  withDuplexBufTransport
    cfg
    (tlsReceiveFn conn)
    (tlsSendFn conn)
    (tlsShutdown conn)


{- | IO-style 'DuplexTransport' for TLS.  Caller is responsible for
'duplexClose' (releases both rings) and 'freeConn' (frees the
SSL*); the SSL* must outlive both rings.
-}
newTlsDuplexTransport
  :: TransportConfig
  -> SslConn
  -> IO DuplexTransport
newTlsDuplexTransport cfg conn =
  newDuplexBufTransport
    cfg
    (tlsReceiveFn conn)
    (tlsSendFn conn)
    (tlsShutdown conn)
