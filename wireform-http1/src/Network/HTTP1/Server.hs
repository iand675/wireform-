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
import Control.Exception (Exception, bracket, catch, fromException, SomeException, finally, throwIO, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word8)
import Foreign.C.Error (throwErrnoIfMinus1Retry)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (Ptr)
import Network.Socket (Socket)
import qualified Network.Socket as NS

import qualified Wireform.Builder as B
import Wireform.Transport.Send (SendTransport (..), sendBuilderDirect, sendByteString)
import Wireform.Transport.Receive (ReceiveTransport (..), ReceiveWait (..))
import System.Timeout (timeout)
import Control.Monad (when)
import qualified Network.HTTP1.SendFile as SF
import Data.Word (Word64)
import System.Posix.IO (closeFd, defaultFileFlags, openFd, OpenMode (..))
import System.Posix.Types (COff (..), CSsize (..), Fd (..), FileOffset)


import Wireform.Network.TLS.OpenSSL (SslConn)
import Wireform.Ring.Pool (RingPool)

import qualified Network.HTTP.Headers as Typed
import qualified Network.HTTP.Headers.TransferEncoding as TransferEncoding
import Network.HTTP1.Chunked (encodeChunk, encodeChunkPrefix)
import Network.HTTP1.Connection
import Network.HTTP1.Encode
  ( ResponseFramingHint (..)
  , checkKnownResponseLength
  , headersBuilder
  , prepareChunkedResponse
  , responseAllowsContent
  , responseBuilder
  , responseBuilderWithoutFraming
  , responseFramingHint
  )
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
  , serverRingPool :: !(Maybe RingPool)
    -- ^ Optional pool of pre-allocated magic ring buffers. When
    -- 'Just', accepted connections acquire rings from the pool
    -- instead of allocating fresh ones (saving ~6 syscalls per
    -- ring). Connections return rings to the pool on close.
    -- Default 'Nothing' (allocate per-connection).
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
  , serverRingPool = Nothing
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
  conn <- case serverRingPool cfg of
    Just pool -> newConnectionFromSocketPooled pool sock
    Nothing   -> newConnectionFromSocket sock
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
          sendErrorResponse conn RequestHeaderFieldsTooLarge >> lingeringClose conn
        Left (SR.ReadTransportError _) -> pure ()
        Left (SR.ReadParse err) ->
          sendErrorResponse conn (errorToStatus err) >> lingeringClose conn
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
            when
              ( requestVersion req0 == HTTP_1_1
                  && framing /= NoBody
                  && framing /= ContentLength 0
                  && expectsContinue (requestHeaders req0)
              )
              (sendInterim conn)
            (body, trailersIO) <- readBodyAndTrailers conn framing
            let req = req0
                  { requestBody = body
                  , requestTrailers = trailersIO
                  }
            r <- try @SomeException $
              serverHandler cfg req >>= sendResponseForRequest cfg conn req
            case r of
              Left e
                | Just (ResponseStarted _) <- fromException e ->
                    pure ()  -- Close without writing another status into the body.
                | Just (ProtocolException pe) <- fromException e ->
                    -- The handler hit a protocol error
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
              Right CloseDelimitedResponse ->
                when (framing /= NoBody && framing /= ContentLength 0) (lingeringClose conn)
              Right disposition -> do
                -- A final response has already gone out. If the unread
                -- body is incomplete, close the connection.
                drained <- try @ProtocolException (drainBody (requestBody req))
                case drained of
                  Right () | KeepConnectionOpen <- disposition -> loop conn
                  _ -> pure ()

------------------------------------------------------------------------
-- Response writing
------------------------------------------------------------------------

newtype ResponseStarted = ResponseStarted SomeException
  deriving stock (Show)

instance Exception ResponseStarted

writeCommitted :: IO a -> IO a
writeCommitted action = action `catch` (throwIO . ResponseStarted)

data ResponseDisposition = KeepConnectionOpen | CloseAfterDrain | CloseDelimitedResponse

sendResponseForRequest :: ServerConfig -> Connection -> Request -> Response -> IO ResponseDisposition
sendResponseForRequest cfg conn req original = do
  let legacy = requestVersion req == HTTP_1_0 || responseVersion original == HTTP_1_0
      version = if legacy then HTTP_1_0 else HTTP_1_1
      method = requestMethod req
  resp <- if legacy then either (ioError . userError) pure (prepareLegacyResponse method original) else pure original
  legacyFraming <- case responseBody resp of
    BodyStream _ | legacy, responseHasContent method resp -> do
      framing <- either (ioError . userError . show) pure
        (responseFraming method HTTP_1_0 (responseStatus resp) (responseHeaders resp))
      pure $ case framing of
        NoBody -> CloseDelimited  -- A successful CONNECT writes raw tunnel bytes.
        _ -> framing
    _ -> pure NoBody
  let responseOptions = connectionOptions (responseHeaders resp)
      closeDelimited = legacyFraming == CloseDelimited
      willClose = closeDelimited || decideClose cfg req version responseOptions
      extraOption
        | closeDelimited = Nothing
        | otherwise = responseConnectionOption willClose version responseOptions
      outgoing
        | closeDelimited = closeDelimitedResponse responseOptions resp
        | otherwise = resp
  sendResponse conn method version legacyFraming extraOption outgoing
  pure $
    if closeDelimited then CloseDelimitedResponse
    else if willClose then CloseAfterDrain
    else KeepConnectionOpen

prepareLegacyResponse :: Method -> Response -> Either String Response
prepareLegacyResponse method resp = case responseBody resp of
  BodyPreEncoded _ -> Right resp  -- The encoded writer handles its own bytes.
  _ -> do
    let fields = responseHeaders resp
        codings = hLookupAll "Transfer-Encoding" fields
        hasContent = responseHasContent method resp
        discardLength = hasContent && not (null codings)
        omit name =
          headerNameIeq "Transfer-Encoding" name
            || headerNameIeq "Trailer" name
            || (discardLength && headerNameIeq "Content-Length" name)
    when hasContent (checkLegacyTransferCodings codings)
    pure resp {responseHeaders = filter (not . omit . fst) fields}

closeDelimitedResponse :: [ConnectionOption] -> Response -> Response
closeDelimitedResponse options resp =
  resp
    { responseHeaders = filter (not . omit . fst) (responseHeaders resp)
        <> [("Connection", BS.intercalate ", " ("close" : foldr addExtension [] options))]
    }
  where
    omit name = headerNameIeq "Connection" name || headerNameIeq "Keep-Alive" name
    addExtension (ConnOption option) rest = option : rest
    addExtension _ rest = rest

responseHasContent :: Method -> Response -> Bool
responseHasContent method resp = method /= HEAD && responseAllowsContent (responseStatus resp)

-- | Send a complete response (head + body) over the connection.
--
-- Three fast paths:
--
--   1. 'BodyPreEncoded' — emit the precomputed wire bytes with a
--      single send. For HEAD we slice to 'peHeadLen' so the body is
--      dropped while the metadata survives (RFC 9110 § 9.3.2).
--      A per-connection response option is inserted into the head.
--   2. 'BodyBytes' without a trailer declaration — combine head and body
--      in one vectored send or two sequential sends on other transports.
--   3. Everything else falls back to the head-first send + body
--      stream.
--
-- The @sendfile(2)@ fast path requires a raw socket fd, so it only
-- fires on socket-backed transports. Over TLS we fall back to a
-- userspace read \/ write loop.
sendResponse :: Connection -> Method -> Version -> Framing -> Maybe BS.ByteString -> Response -> IO ()
sendResponse conn reqMethod framingVersion legacyFraming extraOption original =
  case responseBody original of
    BodyPreEncoded pe@PreEncodedWithTrailers {}
      | framingVersion == HTTP_1_0 ->
          let legacy = resp {responseHeaders = filter (not . isTrailerFraming . fst) (responseHeaders resp)}
          in case preEncodedPayloadForLegacy (responseHeaders resp) pe of
            Right bytes -> sendBufferedPlain (legacy {responseBody = BodyBytes bytes}) bytes
            Left _ | mustOmitBody -> connectionSendBuilderDirect conn (responseBuilder legacy)
            Left err -> ioError (userError err)
      | mustOmitBody -> sendPreEncoded pe
      | otherwise -> writeCommitted $ do
          sendPreEncoded pe
          writeChunkedBody conn (pure Nothing) (responseTrailers resp)
    BodyPreEncoded pe -> sendPreEncoded pe
    body -> do
      if mustOmitBody
        then sendHead
        else case body of
          BodyEmpty -> sendBuffered BS.empty
          BodyBytes bs -> sendBuffered bs
          BodyStream producer -> sendStream producer
          BodyFile fb -> do
            checkFileExtent fb
            if usesChunked
              then do
                headBuilder <- chunkedHead
                withFileBodyFd fb $ \fd ->
                  writeCommitted (sendChunkedFileBody conn headBuilder fd (fbOffset fb) (fbLength fb) (responseTrailers resp))
              else do
                checkLength (fbLength fb)
                withFileBodyFd fb $ \fd ->
                  writeCommitted (sendFixedFileBody conn (responseBuilder resp) fd (fbOffset fb) (fbLength fb))
  where
    mustOmitBody = not (responseHasContent reqMethod original)

    resp = case extraOption of
      Nothing -> original
      Just option -> original {responseHeaders = responseHeaders original <> [("Connection", option)]}

    sendHead = connectionSendBuilderDirect conn $ case responseBody resp of
      BodyStream _ | framingVersion == HTTP_1_0 -> responseBuilderWithoutFraming resp
      _ -> responseBuilder resp
    hint = responseFramingHint (responseHeaders resp)
    usesChunked
      | framingVersion == HTTP_1_1, ChunkedFraming <- hint = True
      | otherwise = False

    chunkedHead = responseBuilder <$> either (ioError . userError) pure (prepareChunkedResponse resp)
    sendChunked write = chunkedHead >>= writeCommitted . write

    sendPreEncoded pe =
      let bs = if mustOmitBody then preEncodedHead pe else peBytes pe
          end = peHeadLen pe
      in case extraOption of
           Just option
             | end >= 4 && end <= BS.length (peBytes pe)
             , "\r\n\r\n" `BS.isSuffixOf` preEncodedHead pe ->
                 -- Insert a field before the blank line, keeping the encoded body intact.
                 let fieldStart = end - 2
                 in withConnectionCork conn $ \corked -> do
                      sendResponseBytes corked (BS.take fieldStart bs)
                      sendBuilderDirect corked (headersBuilder [("Connection", option)])
                      sendResponseBytes corked (BS.drop fieldStart bs)
           _ -> sendResponseBytes (connectionSend conn) bs

    sendBuffered bs
      | usesChunked = sendChunked $ \headBuilder -> do
          let firstChunk = if BS.null bs then mempty else encodeChunk bs
          connectionSendBuilderDirect conn (headBuilder <> firstChunk)
          writeChunkedBody conn (pure Nothing) (responseTrailers resp)
      | otherwise = do
          checkLength (fromIntegral (BS.length bs))
          sendBufferedPlain resp bs

    sendStream producer
      | usesChunked = sendChunked $ \headBuilder -> do
          connectionSendBuilderDirect conn headBuilder
          writeChunkedBody conn producer (responseTrailers resp)
      | ContentLength remaining <- legacyFraming =
          writeCommitted (sendHead >> streamFixed conn remaining producer)
      | CloseDelimited <- legacyFraming =
          writeCommitted (sendHead >> streamRaw conn producer)
      | FixedFraming <- hint = do
          declared <- either (ioError . userError . show) pure (declaredContentLength (responseHeaders resp))
          case declared of
            Just remaining -> writeCommitted (sendHead >> streamFixed conn remaining producer)
            Nothing -> ioError (userError "missing Content-Length for fixed response")
      | otherwise = writeCommitted (sendHead >> writeChunkedBody conn producer (responseTrailers resp))

    checkLength size = case hint of
      FixedFraming -> either (ioError . userError) pure (checkKnownResponseLength size (responseHeaders resp))
      _ -> pure ()

    sendBufferedPlain response bs
      | mustOmitBody || BS.null bs = connectionSendBuilderDirect conn (responseBuilder response)
      | otherwise =
          -- Publish the head and the buffered body together.
          withConnectionCork conn $ \corked -> do
            sendBuilderDirect corked (responseBuilder response)
            sendResponseBytes corked bs

    isTrailerFraming name =
      headerNameIeq "Trailer" name
        || headerNameIeq "Transfer-Encoding" name
        || headerNameIeq "Content-Length" name

-- HTTP/1.0 cannot decode transfer codings. For a compatible payload,
-- reuse the data from the cached chunk as a slice of the same buffer.
preEncodedPayloadForLegacy :: Headers -> PreEncoded -> Either String BS.ByteString
preEncodedPayloadForLegacy fields pe = do
  checkLegacyTransferCodings (hLookupAll "Transfer-Encoding" fields)
  if BS.null chunk
    then Right BS.empty
    else case parseChunkHeader chunk 0 of
      Right (Just (ChunkHeader size start))
        | Just bytes <- BS.stripSuffix "\r\n" (BS.drop start chunk)
        , fromIntegral (BS.length bytes) == size -> Right bytes
      _ -> Left "invalid pre-encoded data chunk"
  where
    chunk = BS.drop (peHeadLen pe) (peBytes pe)

checkLegacyTransferCodings :: [BS.ByteString] -> Either String ()
checkLegacyTransferCodings [] = Right ()
checkLegacyTransferCodings (first : rest) = do
  coding <- Typed.parseFromHeaders Typed.defaultHeaderSettings (first :| rest)
  case TransferEncoding.transferCodings coding of
    [] -> Right ()
    [_] | TransferEncoding.transferEncodingEndsInChunked coding -> Right ()
    _ -> Left "HTTP/1.0 client cannot receive transfer-coded content"

-- Keep the single reservation for small sends. A builder can publish
-- in segments when the cached bytes exceed the transport's ring.
sendResponseBytes :: SendTransport -> BS.ByteString -> IO ()
sendResponseBytes transport bytes
  | BS.length bytes <= sendRingSize transport = sendByteString transport bytes
  | otherwise = sendBuilderDirect transport (B.byteString bytes)
{-# INLINE sendResponseBytes #-}

-- | Send exactly the promised extent. Plain TCP uses @sendfile(2)@
-- with @MSG_MORE@ on the head so the kernel can coalesce the writes.
-- Other transports use the positional copy loop.
sendFixedFileBody :: Connection -> B.Builder -> Fd -> Word64 -> Word64 -> IO ()
sendFixedFileBody conn headBuilder fd off len
  | len == 0 = connectionSendBuilderDirect conn headBuilder
  | Just sock <- connectionSocket conn = do
      sendFlushConn conn
      SF.sendMore sock (B.toStrictByteStringWith 1024 headBuilder)
      SF.sendFileExact sock fd off len
  | otherwise = do
      connectionSendBuilderDirect conn headBuilder
      copyFileExtent fd (sendResponseBytes (connectionSend conn)) off len

withFileBodyFd :: FileBody -> (Fd -> IO a) -> IO a
withFileBodyFd fb use = case fbSource fb of
  FileSourcePath path ->
    bracket
#if MIN_VERSION_unix(2,8,0)
      (openFd path ReadOnly defaultFileFlags)
#else
      (openFd path ReadOnly Nothing defaultFileFlags)
#endif
      (writeCommitted . closeFd)
      use
  FileSourceFd fd -> use fd

checkFileExtent :: FileBody -> IO ()
checkFileExtent fb =
  when (len /= 0 && (off > limit || len > limit - off)) $
    ioError (userError "file response extent exceeds the file offset range")
  where
    off = fbOffset fb
    len = fbLength fb
    limit = fromIntegral (maxBound :: FileOffset)

-- A socket can send the entire extent as one chunk without copying.
-- Other transports read positionally so shared descriptors keep their
-- current offset and remain owned by the application.
sendChunkedFileBody :: Connection -> B.Builder -> Fd -> Word64 -> Word64 -> IO Headers -> IO ()
sendChunkedFileBody conn headBuilder fd off len produceTrailers
  | len == 0 = connectionSendBuilderDirect conn headBuilder >> finish mempty
  | Just sock <- connectionSocket conn = do
      sendFlushConn conn
      SF.sendMore sock (B.toStrictByteStringWith 1024 (headBuilder <> encodeChunkPrefix len))
      SF.sendFileExact sock fd off len
      finish (B.byteString "\r\n")
  | otherwise = do
      connectionSendBuilderDirect conn headBuilder
      copyFileExtent fd (connectionSendBuilder conn . encodeChunk) off len
      finish mempty
  where
    finish suffix = writeChunkedTerminator conn suffix produceTrailers

-- | A bounded positional copy preserves the caller's descriptor offset.
copyFileExtent :: Fd -> (BS.ByteString -> IO ()) -> Word64 -> Word64 -> IO ()
copyFileExtent fd write = loop
  where
    loop !_ 0 = pure ()
    loop !offset !remaining = do
      let want = fromIntegral (min remaining 65536)
      bytes <- readFileChunkAt fd offset want
      when (BS.null bytes) (ioError (userError "file response: unexpected end of file"))
      write bytes
      let got = fromIntegral (BS.length bytes)
      loop (offset + got) (remaining - got)
{-# INLINE copyFileExtent #-}

readFileChunkAt :: Fd -> Word64 -> Int -> IO BS.ByteString
readFileChunkAt (Fd fd) offset want =
  BSI.createUptoN want $ \buf ->
    fromIntegral <$> throwErrnoIfMinus1Retry "pread" (c_pread fd buf (fromIntegral want) (fromIntegral offset))

foreign import ccall safe "pread"
  c_pread :: CInt -> Ptr Word8 -> CSize -> COff -> IO CSsize

-- | Best-effort flush of any pending bytes in the send ring.  The
-- inline send-ring drain is already synchronous so this is usually a
-- no-op, but the io_uring backend (when wired in later) keeps
-- in-flight SQEs that this would await.
sendFlushConn :: Connection -> IO ()
sendFlushConn _ = pure ()

streamRaw :: Connection -> IO (Maybe BS.ByteString) -> IO ()
streamRaw conn producer = loop
  where
    loop = do
      mc <- producer
      case mc of
        Nothing -> pure ()
        Just bs -> sendResponseBytes (connectionSend conn) bs >> loop

streamFixed :: Connection -> Word64 -> IO (Maybe BS.ByteString) -> IO ()
streamFixed conn = loop
  where
    loop !remaining producer = do
      next <- producer
      case next of
        Nothing | remaining == 0 -> pure ()
        Just bytes | let size = fromIntegral (BS.length bytes), size <= remaining -> do
          sendResponseBytes (connectionSend conn) bytes
          loop (remaining - size) producer
        _ -> ioError (userError "response body does not match Content-Length")

------------------------------------------------------------------------
-- Errors / framing decisions
------------------------------------------------------------------------

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

-- | Close after an error response sent before the request was read in
-- full (RFC 9112 §9.6). Closing a socket with unread input makes the
-- kernel send RST, which can discard our response from the client's
-- receive buffer before it reads it (seen as EOF instead of a 431 when
-- a client is still sending an oversized header block). So: flush and
-- half-close, then discard what the peer still sends until it closes,
-- bounded to 2 s and 1 MiB so a client cannot pin the connection.
lingeringClose :: Connection -> IO ()
lingeringClose conn = do
  let tx = connectionSend conn
      rx = connectionReceive conn
      discard !budget = do
        h <- receiveLoadHead rx
        receiveAdvanceTail rx h
        when (budget > 0) $ do
          r <- receiveWaitData rx h
          case r of
            ReceiveMoreData h' -> discard (budget - fromIntegral (h' - h))
            _ -> pure ()
  (sendFlush tx >> sendShutdownWrite tx) `catch` (\(_ :: SomeException) -> pure ())
  _ <- timeout 2000000 (discard (1024 * 1024 :: Int)) `catch` (\(_ :: SomeException) -> pure Nothing)
  pure ()

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

decideClose :: ServerConfig -> Request -> Version -> [ConnectionOption] -> Bool
decideClose cfg req version responseOptions =
  not (serverKeepAlive cfg)
    || ConnClose `elem` requestOptions
    || ConnClose `elem` responseOptions
    || (version == HTTP_1_0 && ConnKeepAlive `notElem` requestOptions)
  where
    requestOptions = connectionOptions (requestHeaders req)

-- | Set the appropriate Connection header based on the keep-alive
-- decision and the client's protocol version.
--
-- - Closing: add @Connection: close@ unless that option is already present.
-- - Keeping alive on HTTP/1.0: echo @Connection: keep-alive@ so the
--   1.0 client knows the connection persists (RFC 9112 §9.3)
-- - Keeping alive on HTTP/1.1: no header needed (persistence is default)
responseConnectionOption :: Bool -> Version -> [ConnectionOption] -> Maybe BS.ByteString
responseConnectionOption willClose version options
  | willClose = if ConnClose `elem` options then Nothing else Just "close"
  | version == HTTP_1_0 && ConnKeepAlive `notElem` options = Just "keep-alive"
  | otherwise = Nothing
