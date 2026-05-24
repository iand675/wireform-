{-# LANGUAGE CPP #-}
{- | HTTP\/1.x server runtime.

The runtime is intentionally minimal: it implements RFC 9112 framing,
keep-alive, and pipelining, and delegates everything else to the
application handler.

Pipeline rules (RFC 9112 § 9.3.2):

  * Server SHOULD process requests in the order they arrive on a
    connection.
  * Server MUST send responses in the same order.

We implement this by running the handler synchronously on the
connection thread (one accept = one OS thread = one connection), so
ordering is automatic. Applications that want to fan out request
handling can fork their own worker and stash the response back via an
'Control.Concurrent.MVar.MVar'.

Connection lifetime:

  * Read request head -> framing -> request body producer.
  * Call user handler.
  * After the handler returns:
      - drain unread body bytes (so the next request lines up).
      - decide whether to keep alive (RFC 9112 § 9.3): respect
        @Connection: close@ from either side and HTTP\/1.0 default.
  * Close on parse error with an appropriate status.
-}
module Network.HTTP1.Server
  ( ServerConfig (..)
  , defaultServerConfig
  , runServer
  , runServerOnSocket
  , runServerOnTls
  , runServerOnConnection
  , Handler
  ) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Exception (bracket, catch, fromException, SomeException, finally, try)
import qualified Data.ByteString as BS
import Network.Socket (Socket)
import qualified Network.Socket as NS

import qualified Wireform.Builder as B
import Wireform.Transport.Send (sendBuilderDirect, sendByteString)
import qualified Network.HTTP1.SendFile as SF
import Data.Word (Word64)
import System.IO
  (Handle, IOMode (..), SeekMode (..), hSeek, withBinaryFile)
import qualified System.Posix.IO as PosixIO
import System.Posix.IO (closeFd, defaultFileFlags, openFd, OpenMode (..))


import Wireform.Network.TLS.OpenSSL (SslConn)

import Network.HTTP1.Chunked
  (encodeChunk, encodeLastChunk, encodeLastChunkWithTrailers)
import Network.HTTP1.Connection
import Network.HTTP1.Encode (responseBuilder)
import Network.HTTP1.Headers
import Network.HTTP1.Parser
import qualified Network.HTTP1.StreamingReader as SR
import Network.HTTP1.Status
import Network.HTTP1.Types

-- | The application's request handler.
--
-- The handler is called once per request. The 'Request' it receives has
-- a 'BodyStream' producer that pulls from the recv buffer; pulling is
-- optional but if you don't pull, the server will drain whatever is
-- left for you before reading the next request.
type Handler = Request -> IO Response

data ServerConfig = ServerConfig
  { serverHost :: !String
  , serverPort :: !String
  , serverHandler :: !Handler
  , serverForkConnection :: IO () -> IO ThreadId
    -- ^ How to fork a new thread for each accepted connection. Default:
    -- 'forkIO'. Use 'Control.Concurrent.forkOn' for pinned-core
    -- scheduling, which the bench-server demonstrates.
  , serverMaxHeaderBytes :: !Int
    -- ^ Cap on request head size (request line + headers + CRLFCRLF).
    -- h2o's default is 16 KiB; we use 32 KiB. Oversized heads get a
    -- 431 response.
  , serverKeepAlive :: !Bool
    -- ^ If 'False', the server emits @Connection: close@ on every
    -- response and tears down after each request. Useful for tests
    -- and for unusual deployments.
  , serverListenBacklog :: !Int
    -- ^ TCP @listen()@ backlog. Default 1024. Raise to 4096+ for very
    -- high accept rates.
  , serverTcpDeferAcceptSecs :: !(Maybe Int)
    -- ^ If 'Just n', set Linux's @TCP_DEFER_ACCEPT@ to @n@ seconds on
    -- the listening socket. With this enabled, @accept()@ won't
    -- return a connection until at least one byte of data has arrived
    -- on it — skipping a syscall round-trip per connection. On
    -- non-Linux kernels the call silently no-ops. Default 'Nothing'.
  , serverAllowConnect :: !Bool
    -- ^ Whether to dispatch @CONNECT@ method requests to the user
    -- handler. Default 'False': @CONNECT@ requests are rejected with
    -- @405 Method Not Allowed@ before the handler runs.
    --
    -- @CONNECT@ is intended for use against an HTTP proxy (RFC 9110
    -- § 9.3.6); accepting it on an origin server effectively turns
    -- the server into an open proxy and is a known SSRF / port-scan
    -- vector. Set 'True' only if you really do want to terminate
    -- @CONNECT@ tunnels in your application.
  }

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { serverHost = "0.0.0.0"
  , serverPort = "8080"
  , serverHandler = \_ ->
      pure Response
        { responseStatus  = OK
        , responseVersion = HTTP_1_1
        , responseHeaders = []
        , responseBody    = BodyBytes ""
        , responseTrailers = pure []
        }
  , serverForkConnection = forkIO
  , serverMaxHeaderBytes = 32 * 1024
  , serverKeepAlive = True
  , serverListenBacklog = 1024
  , serverTcpDeferAcceptSecs = Nothing
  , serverAllowConnect = False
  }

runServer :: ServerConfig -> IO ()
runServer cfg = do
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just (serverHost cfg)) (Just (serverPort cfg))
  case addrs of
    [] -> error "wireform-http1: no address found"
    (addr : _) -> bracket
      (NS.openSocket addr)
      NS.close
      $ \sock -> do
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.setSocketOption sock NS.NoDelay 1
        NS.bind sock (NS.addrAddress addr)
        applyTcpDeferAccept (serverTcpDeferAcceptSecs cfg) sock
        NS.listen sock (serverListenBacklog cfg)
        acceptLoop cfg sock

-- | Apply Linux's @TCP_DEFER_ACCEPT@ if requested. Swallows errors
-- (the option doesn't exist on non-Linux kernels; that's fine).
applyTcpDeferAccept :: Maybe Int -> Socket -> IO ()
applyTcpDeferAccept Nothing _ = pure ()
applyTcpDeferAccept (Just secs) sock = do
  let optName = NS.SockOpt 6 {- IPPROTO_TCP -} 9 {- TCP_DEFER_ACCEPT -}
  NS.setSocketOption sock optName secs
    `catch` (\(_ :: SomeException) -> pure ())


acceptLoop :: ServerConfig -> Socket -> IO ()
acceptLoop cfg listenSock = do
  (clientSock, _) <- NS.accept listenSock
  NS.setSocketOption clientSock NS.NoDelay 1
  _ <- serverForkConnection cfg $
    runServerOnSocket cfg clientSock
      `catch` (\(_ :: SomeException) -> NS.close clientSock)
  acceptLoop cfg listenSock

runServerOnSocket :: ServerConfig -> Socket -> IO ()
runServerOnSocket cfg sock = do
  conn <- newConnectionFromSocket sock
  handleConnection cfg conn

-- | Drive the server over a TLS-wrapped connection.  The caller is
-- responsible for the handshake and any ALPN assertion
-- ('Network.HTTP1.TLS.assertHttp11Alpn').
runServerOnTls :: ServerConfig -> SslConn -> IO ()
runServerOnTls cfg ssl = do
  conn <- newConnectionFromTls ssl
  handleConnection cfg conn

-- | Drive the server over an already-built 'Connection' (e.g. an
-- in-memory pipe duplex wrapped via 'newConnectionFromDuplex').
runServerOnConnection :: ServerConfig -> Connection -> IO ()
runServerOnConnection = handleConnection

handleConnection :: ServerConfig -> Connection -> IO ()
handleConnection cfg conn0 = loop conn0 `finally` closeConnection conn0
  where
    loop conn = do
      hdE <- readRequestHead conn
      case hdE of
        Left SR.ReadUnexpectedEof   -> pure ()  -- EOF before a request
        Left (SR.ReadMessageTooLong _) ->
          sendErrorResponse conn RequestHeaderFieldsTooLarge
        Left (SR.ReadTransportError _) -> pure ()
        Left (SR.ReadParse err) ->
          sendErrorResponse conn (errorToStatus err)
        Right (req0, _) | requestMethod req0 == CONNECT
                       , not (serverAllowConnect cfg) ->
          -- Reject CONNECT by default. RFC 9110 § 9.3.6: CONNECT is
          -- intended for proxies. Accepting it on an origin server
          -- turns it into an open tunnel. Override via
          -- 'serverAllowConnect' if you really want this.
          sendErrorResponse conn MethodNotAllowed
        Right (req0, framing) -> do
            -- RFC 9110 §10.1.1: if the client said
            -- @Expect: 100-continue@, the server MUST either send
            -- 100 Continue or a final response *before* the client
            -- starts streaming the body.  Auto-acknowledge here so
            -- handlers don't have to worry about the dance.  We
            -- only honour the directive for HTTP/1.1 (RFC 9112
            -- §9.1); 1.0 has no 100-continue concept.
            case findExpect (requestHeaders req0) of
              Just v
                | requestVersion req0 == HTTP_1_1
                , isContinueExpect v -> sendInterim conn
              _ -> pure ()
            (body, trailersIO) <- readBodyAndTrailers conn framing
            let req = req0
                  { requestBody = body
                  , requestTrailers = trailersIO
                  }
            r <- try @SomeException $ do
              resp0 <- serverHandler cfg req
              let willClose = decideClose cfg req resp0
                  resp =
                    if willClose
                      then addCloseHeader resp0
                      else resp0
              sendResponse conn (requestMethod req) resp
              drainBody (requestBody req)
              pure willClose
            case r of
              Left e
                | Just (ProtocolException pe) <- fromException e ->
                    -- The handler or body-drain hit a protocol error
                    -- while reading the request body (e.g. malformed
                    -- chunk-size line). Reply with the matching 4xx and
                    -- close the connection; we cannot keep keep-alive
                    -- alive because the wire is desynced.
                    sendErrorResponse conn (errorToStatus pe)
                | otherwise -> do
                    -- Best-effort 500 if no response went out yet.
                    -- If a partial response already raced out we'll
                    -- just close.
                    sendErrorResponse conn InternalServerError
                      `catch` (\(_ :: SomeException) -> pure ())
              Right willClose
                | willClose -> pure ()
                | otherwise -> loop conn

------------------------------------------------------------------------
-- Response writing
------------------------------------------------------------------------

-- | Send a complete response (head + body) over the connection.
--
-- Three fast paths:
--
--   1. 'BodyPreEncoded' — emit the precomputed wire bytes with a
--      single send. For HEAD we slice to 'peHeadLen' so the body is
--      dropped while the metadata survives (RFC 9110 § 9.3.2).
--   2. 'BodyBytes' — combine head + body in one vectored send (raw
--      socket) or two sequential sends (TLS / other non-socket
--      transports).
--   3. Everything else falls back to the head-first send + body
--      stream.
--
-- The @sendfile(2)@ fast path requires a raw socket fd, so it only
-- fires on socket-backed transports. Over TLS we fall back to a
-- userspace read \/ write loop.
sendResponse :: Connection -> Method -> Response -> IO ()
sendResponse conn reqMethod resp = do
  let mustOmitBody =
        reqMethod == HEAD
          || let sc = case responseStatus resp of Status w -> w
             in (sc >= 100 && sc < 200) || sc == 204 || sc == 304
  case responseBody resp of
    BodyPreEncoded pe ->
      let bs = if mustOmitBody then preEncodedHead pe else peBytes pe
      in connectionSendBytes conn bs
    body -> do
      if mustOmitBody
        then connectionSendBuilderDirect conn (responseBuilder resp)
        else case body of
          BodyEmpty -> connectionSendBuilderDirect conn (responseBuilder resp)
          BodyBytes bs
            | BS.null bs -> connectionSendBuilderDirect conn (responseBuilder resp)
            | otherwise  ->
                -- Cork: headers + body go into the ring without
                -- publishing; one publish on exit = one sendmsg /
                -- one io_uring SQE.
                withConnectionCork conn $ \corked -> do
                  sendBuilderDirect corked (responseBuilder resp)
                  sendByteString corked bs
          BodyStream producer -> do
                -- Streaming: send headers immediately so the client
                -- sees the status before the first body chunk.
            connectionSendBuilderDirect conn (responseBuilder resp)
            case responseVersion resp of
              HTTP_1_1 -> do
                trs <- responseTrailers resp
                if null trs
                  then streamChunked conn producer
                  else streamChunkedWithTrailers conn producer trs
              HTTP_1_0 -> streamRaw conn producer
          BodyFile fb -> do
            let headBs = B.toStrictByteStringWith 1024 (responseBuilder resp)
            sendFileBody conn headBs fb

-- | Push a file-backed response body.  On plain TCP connections we
-- use the @sendfile(2)@ fast path with @MSG_MORE@ on the head; on
-- TLS connections (where the kernel cannot encrypt for us) we fall
-- back to a userspace @read()@\/ring-send loop.
sendFileBody :: Connection -> BS.ByteString -> FileBody -> IO ()
sendFileBody conn headBs fb = case connSocket of
  Just sock -> do
    -- Flush the send ring first so head bytes go out in order with
    -- respect to the upcoming sendfile.  Then SF.sendMore primes
    -- the kernel with MSG_MORE so the sendfile can coalesce on the
    -- wire.
    --
    -- (We deliberately call SF.sendMore here, not through the send
    -- ring: the sendfile fast path is a kernel-managed zero-copy
    -- and the head MUST be the immediately-preceding write on the
    -- same socket fd.)
    sendFlushConn conn
    SF.sendMore sock headBs
    case fbSource fb of
      FileSourcePath p ->
        bracket
#if MIN_VERSION_unix(2,8,0)
          (openFd p ReadOnly defaultFileFlags)
#else
          (openFd p ReadOnly Nothing defaultFileFlags)
#endif
          closeFd
          $ \fd -> SF.sendFile sock fd (fbOffset fb) (fbLength fb)
      FileSourceFd fd ->
        SF.sendFile sock fd (fbOffset fb) (fbLength fb)
  Nothing -> do
    -- TLS / non-socket: head through the ring, body through a
    -- userspace copy loop that also goes through the ring.
    connectionSendBytes conn headBs
    case fbSource fb of
      FileSourcePath p ->
        withBinaryFile p ReadMode $ \h ->
          userspaceCopyHandle conn h (fbOffset fb) (fbLength fb)
      FileSourceFd fd -> do
        h <- PosixIO.fdToHandle fd
        userspaceCopyHandle conn h (fbOffset fb) (fbLength fb)
  where
    -- sendfile bypasses the ring; only fires on plain TCP (TLS sets
    -- connSocket to Nothing intentionally so the userspace branch
    -- runs through the TLS-aware send ring).
    connSocket = connectionSocket conn

-- | Best-effort flush of any pending bytes in the send ring.  The
-- inline send-ring drain is already synchronous so this is usually a
-- no-op, but the io_uring backend (when wired in later) keeps
-- in-flight SQEs that this would await.
sendFlushConn :: Connection -> IO ()
sendFlushConn _ = pure ()

-- | Userspace read + ring-send loop, used when sendfile is
-- unavailable (TLS, in-memory).  64 KiB chunks match the
-- @sendfile(2)@ default and are large enough to amortise per-call
-- overhead.
userspaceCopyHandle :: Connection -> Handle -> Word64 -> Word64 -> IO ()
userspaceCopyHandle conn h off0 len0 = do
  hSeek h AbsoluteSeek (fromIntegral off0)
  loop len0
  where
    chunkSize = 65536 :: Int
    loop 0 = pure ()
    loop rem' = do
      let want = fromIntegral (min rem' (fromIntegral chunkSize)) :: Int
      bs <- BS.hGet h want
      let got = BS.length bs
      if got <= 0
        then pure ()
        else do
          connectionSendBytes conn bs
          loop (rem' - fromIntegral got)

streamChunked :: Connection -> IO (Maybe BS.ByteString) -> IO ()
streamChunked conn producer = loop
  where
    loop = do
      mc <- producer
      case mc of
        Nothing -> connectionSendBuilder conn encodeLastChunk
        Just bs ->
          if BS.null bs
            then loop  -- skip empty chunk (would be a premature terminator)
            else do
              connectionSendBuilder conn (encodeChunk bs)
              loop

-- | Like 'streamChunked' but emits @trailers@ in the chunked-body
-- terminator's field block (RFC 9112 \u00a77.1.2). Pass @[]@ when
-- there are no trailers and you'll get the same wire bytes as
-- 'streamChunked'.
streamChunkedWithTrailers
  :: Connection
  -> IO (Maybe BS.ByteString)
  -> Headers
  -> IO ()
streamChunkedWithTrailers conn producer trailers = loop
  where
    -- The empty-trailers branch reuses 'streamChunked' rather than
    -- calling 'encodeLastChunkWithTrailers []' so the wire bytes go
    -- out as a single 'B.byteString' append (= one send-ring
    -- publish on a small buffer). That difference shouldn't matter on a
    -- well-behaved peer, but it does interact with the
    -- recv-buffered single-shot client used by 'sendRequest', which
    -- closes its bracketed connection as soon as the high-level
    -- send-and-parse-head sequence returns.
    loop = do
      mc <- producer
      case mc of
        Nothing
          | null trailers -> connectionSendBuilder conn encodeLastChunk
          | otherwise     -> connectionSendBuilder conn (encodeLastChunkWithTrailers trailers)
        Just bs ->
          if BS.null bs
            then loop
            else do
              connectionSendBuilder conn (encodeChunk bs)
              loop

streamRaw :: Connection -> IO (Maybe BS.ByteString) -> IO ()
streamRaw conn producer = loop
  where
    loop = do
      mc <- producer
      case mc of
        Nothing -> pure ()
        Just bs -> connectionSendBytes conn bs >> loop

------------------------------------------------------------------------
-- Errors / framing decisions
------------------------------------------------------------------------

-- | Case-insensitive @"100-continue"@ check for an @Expect:@ value.
-- Trims a single trailing OWS character; the parser already strips
-- enclosing whitespace so we only need to be tolerant about the
-- common @"100-continue "@ variant.
isContinueExpect :: BS.ByteString -> Bool
isContinueExpect bs = BS.map asciiLower (trim bs) == "100-continue"
  where
    trim s = case BS.unsnoc s of
      Just (rest, w) | w == 0x20 || w == 0x09 -> trim rest
      _ -> s
    asciiLower w
      | w >= 0x41 && w <= 0x5A = w + 0x20
      | otherwise              = w

-- | Emit a bare @HTTP/1.1 100 Continue\\r\\n\\r\\n@.  We bypass the
-- full response encoder because the interim response carries no
-- headers and no body and must not affect the surrounding
-- request \/ response framing.
sendInterim :: Connection -> IO ()
sendInterim conn =
  connectionSendBytes conn "HTTP/1.1 100 Continue\r\n\r\n"
    `catch` (\(_ :: SomeException) -> pure ())

-- | Send a static error response after a parse failure. Always closes
-- the connection because at that point our reader is desynced from the
-- wire and we can't safely recover.
sendErrorResponse :: Connection -> Status -> IO ()
sendErrorResponse conn st = do
  let resp = Response
        { responseStatus = st
        , responseVersion = HTTP_1_1
        , responseHeaders = [("Connection", "close"), ("Content-Length", "0")]
        , responseBody = BodyEmpty
        , responseTrailers = pure []
        }
  connectionSendBuilderDirect conn (responseBuilder resp)
    `catch` (\(_ :: SomeException) -> pure ())

errorToStatus :: ParseError -> Status
errorToStatus = \case
  ParseMessageTooLong -> RequestHeaderFieldsTooLarge
  ParseBadRequestLine -> BadRequest
  ParseBadStatusLine -> BadGateway
  ParseBadHeaderName -> BadRequest
  ParseInvalidHeaderValue -> BadRequest
  ParseLengthConflict -> BadRequest
  ParseLengthAndTransferEncoding -> BadRequest
  ParseChunkedNotFinal -> BadRequest
  ParseInvalidLength -> BadRequest
  ParseUnsupportedVersion -> HttpVersionNotSupported
  ParseBadChunkHeader -> BadRequest
  -- An oversize chunk size *line* (more than 16 hex digits, or a
  -- value that wouldn't fit in 64 bits) is a malformed-input
  -- problem, not a "your payload is too large for me" problem. The
  -- security-test corpus expects 400 here (413 is for the body
  -- exceeding a server-imposed limit). Match RFC 9112's "400 or
  -- close" recommendation.
  ParseChunkTooLarge -> BadRequest
  ParseUnexpectedEof -> BadRequest
  ParseMissingHost -> BadRequest
  ParseMultipleHosts -> BadRequest
  ParseInvalidHost -> BadRequest
  ParseInvalidTarget -> BadRequest

------------------------------------------------------------------------
-- Keep-alive decision
------------------------------------------------------------------------

decideClose :: ServerConfig -> Request -> Response -> Bool
decideClose cfg req resp
  | not (serverKeepAlive cfg) = True
  | requestVersion req == HTTP_1_0 = not (clientWantsKeepAlive (requestHeaders req))
  | otherwise = clientWantsClose (requestHeaders req) || serverWantsClose (responseHeaders resp)

clientWantsKeepAlive :: Headers -> Bool
clientWantsKeepAlive hs = case findConnection hs of
  Nothing -> False
  Just v  -> any (== ConnKeepAlive) (parseConnection v)

clientWantsClose :: Headers -> Bool
clientWantsClose hs = case findConnection hs of
  Nothing -> False
  Just v  -> any (== ConnClose) (parseConnection v)

serverWantsClose :: Headers -> Bool
serverWantsClose hs = case findConnection hs of
  Nothing -> False
  Just v  -> any (== ConnClose) (parseConnection v)

addCloseHeader :: Response -> Response
addCloseHeader r
  | hHas "connection" (responseHeaders r) = r
  | otherwise = r { responseHeaders = responseHeaders r <> [("Connection", "close")] }
