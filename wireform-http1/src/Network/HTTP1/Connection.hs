{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Connection-level state shared by client and server.

A 'Connection' owns:

  * a magic-ring 'Wireform.Network.DuplexTransport' (one receive ring
    + one send ring, both pinned to the same underlying byte stream),
  * an optional 'Socket' (set on plain TCP; @Nothing@ on TLS) for
    the @sendfile(2)@ fast path,
  * an optional 'SslConn' (set on TLS; @Nothing@ on plain TCP) for
    explicit shutdown,
  * a cursor 'IORef' that tracks how far the recv side has consumed
    so successive @read*@ helpers can chain without round-tripping
    through 'Wireform.Transport.Receive.receiveLoadHead' between
    iterations,
  * lifecycle state for connection retirement and idempotent cleanup.

The body-reading helpers ('readBody', 'drainBody') run the framing
state machine inferred by 'Network.HTTP1.Parser.requestFraming' \/
'responseFraming' and feed the next request \/ response on the wire
as soon as the previous body is consumed (HTTP\/1.1 keep-alive +
pipelining).
-}
module Network.HTTP1.Connection (
  Connection,
  newConnectionFromSocket,
  newConnectionFromOwnedSocket,
  newConnectionFromSocketPooled,
  newConnectionFromTls,
  newConnectionFromDuplex,
  newConnectionFromDuplexWithRingSize,
  defaultRingSize,
  connectionReceive,
  connectionSend,
  connectionSocket,
  connectionSslConn,
  connectionCursor,
  connectionReadCursor,
  connectionAdvanceCursor,
  connectionCanReuse,
  retireConnection,
  closeConnection,

  -- * Send helpers
  connectionSendBytes,
  connectionSendMany,
  connectionSendBuilder,
  connectionSendBuilderDirect,
  withConnectionCork,

  -- * Head readers (zero-copy, SIMD on the ring)
  readRequestHead,
  readResponseHead,

  -- * Framing-aware body
  writeChunkedBody,
  writeChunkedTerminator,
  readBody,
  readBodyAndTrailers,
  drainBody,
  ProtocolException (..),
) where

import Control.Concurrent.MVar (
  MVar,
  newEmptyMVar,
  newMVar,
  readMVar,
  tryPutMVar,
 )
import Control.Exception (Exception, SomeException, finally, throwIO, try)
import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Word (Word64)
import GHC.Generics (Generic)
import Network.HTTP.Headers.Trailer (isForbiddenTrailerName)
import Network.HTTP1.Chunked (encodeChunk, encodeLastChunkWithTrailers)
import Network.HTTP1.Headers (ConnectionOption (..), Header, connectionOptions, hHas)
import Network.HTTP1.Method qualified as Method
import Network.HTTP1.Parser (Framing (..), ParseError (..), findNonFieldValue, findNonToken, parseHeaderLine)
import Network.HTTP1.StreamingReader qualified as SR
import Network.HTTP1.Types (
  Body (..),
  Request,
  Response (..),
  statusCode,
  pattern HTTP_1_0,
 )
import Network.Socket (Socket)
import Network.Socket qualified as S
import Wireform.Builder qualified as B
import Wireform.Network (
  DuplexTransport (..),
  closeDuplexTransport,
  newDuplexBufTransportPooled,
  newDuplexTransport,
 )
import Wireform.Network.TLS.OpenSSL (SslConn, freeConn, newTlsDuplexTransport, sslConnSocket)
import Wireform.Ring.Pool (RingPool)
import Wireform.Transport.Config (defaultTransportConfig)
import Wireform.Transport.Config qualified as WC
import Wireform.Transport.Receive (ReceiveTransport, receiveAdvanceTail)
import Wireform.Transport.Send (
  SendTransport,
  sendBuilder,
  sendBuilderDirect,
  sendByteString,
  sendByteStringMany,
  withSendCork,
 )


{- | Thrown by a streaming-body producer when the wire bytes violate
the framing the parser inferred.
-}
newtype ProtocolException = ProtocolException ParseError
  deriving stock (Eq, Show, Generic)


instance Exception ProtocolException


data Connection = Connection
  { connDuplex :: !DuplexTransport
  , connSocket :: !(Maybe Socket)
  {- ^ The raw socket fd if this is a plain TCP connection.  TLS
  connections set this to 'Nothing' so the sendfile fast path
  correctly falls back to a userspace copy.
  -}
  , connSslConn :: !(Maybe SslConn)
  {- ^ The OpenSSL connection if this is a TLS connection.  Used
  so 'closeConnection' can issue @SSL_shutdown@ + free the
  @SSL*@.
  -}
  , connCursor :: !(IORef Word64)
  -- ^ Position past the last byte the recv path has consumed.
  , connState :: !(IORef ConnectionState)
  }


data ConnectionState = ConnectionOpen | ConnectionRetiring | ConnectionClosed
  deriving stock (Eq)


{- | Default magic-ring size: 256 KiB.  Easily fits the largest
header block (h2o caps requests at 32 KiB by default) plus
several chunked-TE body chunks (16 KiB cap each) plus some
breathing room, while keeping per-connection virtual memory
modest.
-}
defaultRingSize :: Int
defaultRingSize = 256 * 1024


{- | Build a 'Connection' from a plain TCP 'Socket'.  Allocates one
receive ring + one send ring of 'defaultRingSize' bytes each.
Caller is responsible for closing the socket; 'closeConnection'
only tears down the rings.
-}
newConnectionFromSocket :: Socket -> IO Connection
newConnectionFromSocket sock = do
  let !cfg = defaultTransportConfig {WC.ringSizeHint = defaultRingSize}
  duplex <- newDuplexTransport cfg sock
  buildConnection duplex (Just sock) Nothing


-- | Transfer a connected TCP socket to this connection for cleanup.
newConnectionFromOwnedSocket :: Socket -> IO Connection
newConnectionFromOwnedSocket sock = do
  let !cfg = defaultTransportConfig {WC.ringSizeHint = defaultRingSize}
  duplex <- newDuplexTransport cfg sock
  buildConnection (duplex {duplexClose = duplexClose duplex `finally` S.close sock}) (Just sock) Nothing


{- | Like 'newConnectionFromSocket' but acquires ring buffers from a
'RingPool' instead of allocating fresh ones. On 'closeConnection',
rings are returned to the pool for reuse. This eliminates the
@memfd_create@ + @mmap@ + @memset@ cost of ring allocation on the
connection hot path.
-}
newConnectionFromSocketPooled :: RingPool -> Socket -> IO Connection
newConnectionFromSocketPooled pool sock = do
  let !cfg = defaultTransportConfig {WC.ringSizeHint = defaultRingSize}
  duplex <-
    newDuplexBufTransportPooled
      pool
      cfg
      (\p n -> S.recvBuf sock p n)
      (\p n -> S.sendBuf sock p n)
      (S.shutdown sock S.ShutdownSend)
  buildConnection duplex (Just sock) Nothing


{- | Build a 'Connection' from a handshaked OpenSSL TLS connection.
The @SSL*@ is owned by the 'Connection': 'closeConnection' issues
@SSL_shutdown@ then frees the SSL.  The underlying socket is NOT
closed (caller owns the socket).
-}
newConnectionFromTls :: SslConn -> IO Connection
newConnectionFromTls conn = do
  let !cfg = defaultTransportConfig {WC.ringSizeHint = defaultRingSize}
  duplex <- newTlsDuplexTransport cfg conn
  -- For TLS we deliberately set connSocket to Nothing so the
  -- sendfile(2) fast path falls back to a userspace copy.
  buildConnection duplex Nothing (Just conn)


{- | Build a 'Connection' from an externally-managed 'DuplexTransport'.
Used by in-memory pipes / tests that already own the duplex.
'closeConnection' will still call 'duplexClose'.
-}
newConnectionFromDuplex :: DuplexTransport -> IO Connection
newConnectionFromDuplex = newConnectionFromDuplexWithRingSize defaultRingSize


{- | Like 'newConnectionFromDuplex' but the ring-size argument is
noted purely for forwards-compatibility (the duplex is taken
as-is).  Kept for API symmetry with the pre-rewrite shape.
-}
newConnectionFromDuplexWithRingSize :: Int -> DuplexTransport -> IO Connection
newConnectionFromDuplexWithRingSize _ duplex =
  buildConnection duplex Nothing Nothing


buildConnection :: DuplexTransport -> Maybe Socket -> Maybe SslConn -> IO Connection
buildConnection duplex mSock mSsl = do
  cursor <- newIORef 0
  state <- newIORef ConnectionOpen
  pure
    Connection
      { connDuplex = duplex
      , connSocket = mSock
      , connSslConn = mSsl
      , connCursor = cursor
      , connState = state
      }


-- | The receive-side transport.
connectionReceive :: Connection -> ReceiveTransport
connectionReceive = duplexReceive . connDuplex


-- | The send-side transport.
connectionSend :: Connection -> SendTransport
connectionSend = duplexSend . connDuplex


{- | The raw socket if this connection is plain TCP, else 'Nothing'.
The sendfile fast path branches on this.
-}
connectionSocket :: Connection -> Maybe Socket
connectionSocket conn = case connSocket conn of
  Just s -> Just s
  Nothing -> sslConnSocket <$> connSslConn conn


-- TLS connections expose the underlying socket too (for socket
-- options, raw fd inspection); only the sendfile fast path
-- intentionally avoids the TLS socket because writing raw bytes
-- past the TLS layer would corrupt the stream.

-- | The OpenSSL connection if this connection is TLS, else 'Nothing'.
connectionSslConn :: Connection -> Maybe SslConn
connectionSslConn = connSslConn


-- | The 'IORef' tracking how far the recv path has consumed.
connectionCursor :: Connection -> IORef Word64
connectionCursor = connCursor


-- | Read the cursor.
connectionReadCursor :: Connection -> IO Word64
connectionReadCursor = readIORef . connCursor


{- | Bump the cursor to the supplied position and tell the recv ring
transport it can recycle bytes up to that point.
-}
connectionAdvanceCursor :: Connection -> Word64 -> IO ()
connectionAdvanceCursor conn pos = do
  writeIORef (connCursor conn) pos
  receiveAdvanceTail (connectionReceive conn) pos


{- | Send the given bytes through the connection's send ring.  Goes
through one ring reservation + one drain into the wire.
-}
connectionSendBytes :: Connection -> ByteString -> IO ()
connectionSendBytes conn = sendByteString (connectionSend conn)


{- | Send multiple byte strings as one merged reservation.  Lets the
kernel coalesce them into one @sendmsg@.
-}
connectionSendMany :: Connection -> [ByteString] -> IO ()
connectionSendMany conn = sendByteStringMany (connectionSend conn)


-- | Materialise + stage a 'B.Builder' into the send ring.
connectionSendBuilder :: Connection -> B.Builder -> IO ()
connectionSendBuilder conn = sendBuilder (connectionSend conn)


{- | Like 'connectionSendBuilder' but uses 'sendBuilderDirect'
explicitly (writes the builder directly into ring memory).
-}
connectionSendBuilderDirect :: Connection -> B.Builder -> IO ()
connectionSendBuilderDirect conn = sendBuilderDirect (connectionSend conn)


{- | Cork the connection's send transport.  Bytes written inside
the callback accumulate in the ring without triggering a drain
(no @sendmsg@ \/ io_uring SQE).  On exit, a single publish
covers everything — one syscall for headers + body.

If the ring fills mid-cork, the cork falls back to a real
publish so the consumer can drain.
-}
withConnectionCork :: Connection -> (SendTransport -> IO a) -> IO a
withConnectionCork conn = withSendCork (connectionSend conn)


{- | Tear down the connection.  Order: drain + close the send ring,
close the receive ring, then (for TLS) issue @SSL_shutdown@ and
free the @SSL*@. Idempotent. An owned TCP socket is closed with its
transport.
-}
closeConnection :: Connection -> IO ()
closeConnection conn = do
  previous <- atomicModifyIORef' (connState conn) (\state -> (ConnectionClosed, state))
  if previous == ConnectionClosed
    then pure ()
    else do
      _ <- try @SomeException (closeDuplexTransport (connDuplex conn))
      case connSslConn conn of
        Just s -> do
          _ <- try @SomeException (freeConn s)
          pure ()
        Nothing -> pure ()


-- | Whether the protocol still permits reuse once the current body is read.
connectionCanReuse :: Connection -> IO Bool
connectionCanReuse conn = (== ConnectionOpen) <$> readIORef (connState conn)


-- | Let the current exchange finish, but exclude this connection from reuse.
retireConnection :: Connection -> IO ()
retireConnection conn = atomicModifyIORef' (connState conn) $ \case
  ConnectionOpen -> (ConnectionRetiring, ())
  state -> (state, ())


------------------------------------------------------------------------
-- Head readers
------------------------------------------------------------------------

readRequestHead
  :: Connection
  -> IO (Either SR.ReadError (Request, Framing))
readRequestHead conn = do
  pos <- readIORef (connCursor conn)
  r <- SR.readRequestHeadFrom (connectionReceive conn) pos
  case r of
    Right (ok, newPos) -> do
      writeIORef (connCursor conn) newPos
      pure (Right ok)
    Left e -> pure (Left e)


readResponseHead
  :: Connection
  -> Method.Method
  -> IO (Either SR.ReadError (Response, Framing))
readResponseHead conn reqMethod = do
  pos <- readIORef (connCursor conn)
  r <- SR.readResponseHeadFrom (connectionReceive conn) pos reqMethod
  case r of
    Right (ok@(response, framing), newPos) -> do
      writeIORef (connCursor conn) newPos
      when (responseEndsConnection reqMethod response framing) (retireConnection conn)
      pure (Right ok)
    Left e -> do
      retireConnection conn
      pure (Left e)


-- RFC 9112 sections 6.1 and 9.3. Upgrades and successful CONNECTs hand
-- the connection to another protocol. A close-delimited body consumes it.
responseEndsConnection :: Method.Method -> Response -> Framing -> Bool
responseEndsConnection method response framing
  | framing == CloseDelimited || status == 101 = True
  | ConnClose `elem` options = True
  | status < 200 = False
  | method == Method.CONNECT && status < 300 = True
  | responseVersion response == HTTP_1_0 = ConnKeepAlive `notElem` options || hHas "transfer-encoding" headers
  | otherwise = False
  where
    status = statusCode (responseStatus response)
    headers = responseHeaders response
    options = connectionOptions headers


------------------------------------------------------------------------
-- Body
------------------------------------------------------------------------

-- | Write chunked content, evaluating the trailer producer after EOF.
writeChunkedBody :: Connection -> IO (Maybe ByteString) -> IO [Header] -> IO ()
writeChunkedBody conn producer produceTrailers = loop
  where
    loop = do
      chunk <- producer
      case chunk of
        Nothing -> writeChunkedTerminator conn mempty produceTrailers
        Just bytes
          | BS.null bytes -> loop
          | otherwise -> connectionSendBuilder conn (encodeChunk bytes) >> loop


-- | Finish a chunked message. The optional prefix finishes a data
-- chunk whose payload was sent separately. Validate the complete
-- field list before writing any part of the terminator.
writeChunkedTerminator :: Connection -> B.Builder -> IO [Header] -> IO ()
writeChunkedTerminator conn prefix produceTrailers = do
  trailers <- produceTrailers
  forM_ trailers validateField
  connectionSendBuilder conn (prefix <> encodeLastChunkWithTrailers trailers)
  where
    validateField (name, value)
      | BS.null name || findNonToken name 0 /= BS.length name =
          ioError (userError "invalid trailer field name")
      | findNonFieldValue value 0 /= BS.length value =
          ioError (userError "invalid trailer field value")
      | isForbiddenTrailerName name =
          ioError (userError "forbidden trailer field")
      | otherwise = pure ()


readBody :: Connection -> Framing -> IO Body
readBody conn framing = fst <$> readBodyAndTrailers conn framing


readBodyAndTrailers
  :: Connection -> Framing -> IO (Body, IO [Header])
readBodyAndTrailers _ NoBody = do
  mv <- newMVar []
  pure (BodyEmpty, readMVar mv)
readBodyAndTrailers _ (ContentLength 0) = do
  mv <- newMVar []
  pure (BodyEmpty, readMVar mv)
readBodyAndTrailers conn (ContentLength n) = do
  remRef <- newIORef n
  trailersMV <- newEmptyMVar
  let producer = do
        rem' <- readIORef remRef
        if rem' == 0
          then do
            _ <- tryPutMVar trailersMV []
            pure Nothing
          else do
            let want = fromIntegral (min rem' 16384) :: Int
            pos <- readIORef (connCursor conn)
            r <- SR.readUpTo (connectionReceive conn) pos want
            case r of
              Left e -> failBodyRead conn (readErrorToParseError e)
              Right (chunk, newPos) -> do
                let !chunkCopy = BS.copy chunk
                connectionAdvanceCursor conn newPos
                let newRem = rem' - fromIntegral (BS.length chunkCopy)
                writeIORef remRef newRem
                when' (newRem == 0) $ do
                  _ <- tryPutMVar trailersMV []
                  pure ()
                pure (Just chunkCopy)
  pure (BodyStream producer, readMVar trailersMV)
readBodyAndTrailers conn Chunked = do
  stateRef <- newIORef (ChunkPending 0)
  trailersMV <- newEmptyMVar
  let producer = readChunkedStep conn stateRef trailersMV
  pure (BodyStream producer, readMVar trailersMV)
readBodyAndTrailers conn CloseDelimited = do
  trailersMV <- newEmptyMVar
  let producer = do
        pos <- readIORef (connCursor conn)
        r <- SR.readUpTo (connectionReceive conn) pos 16384
        case r of
          Left SR.ReadUnexpectedEof -> do
            _ <- tryPutMVar trailersMV []
            pure Nothing
          Left e -> failBodyRead conn (readErrorToParseError e)
          Right (chunk, newPos) -> do
            let !chunkCopy = BS.copy chunk
            connectionAdvanceCursor conn newPos
            pure (Just chunkCopy)
  pure (BodyStream producer, readMVar trailersMV)


when' :: Bool -> IO () -> IO ()
when' True m = m
when' False _ = pure ()


data ChunkState
  = ChunkPending !Word64
  | ChunkDone


readChunkedStep
  :: Connection
  -> IORef ChunkState
  -> MVar [Header]
  -> IO (Maybe ByteString)
readChunkedStep conn ref trailersMV = do
  st <- readIORef ref
  case st of
    ChunkDone -> pure Nothing
    ChunkPending 0 -> do
      pos <- readIORef (connCursor conn)
      lineE <- SR.readChunkSizeLineFrom (connectionReceive conn) pos
      case lineE of
        Left e -> failBodyRead conn (readErrorToParseError e)
        Right (sz, newPos) -> do
          writeIORef (connCursor conn) newPos
          if sz == 0
            then do
              trs <- readTrailers conn
              _ <- tryPutMVar trailersMV trs
              writeIORef ref ChunkDone
              pure Nothing
            else do
              writeIORef ref (ChunkPending sz)
              readChunkedStep conn ref trailersMV
    ChunkPending n -> do
      let want = fromIntegral (min n 16384) :: Int
      pos <- readIORef (connCursor conn)
      r <- SR.readUpTo (connectionReceive conn) pos want
      case r of
        Left _ -> failBodyRead conn ParseUnexpectedEof
        Right (slice, newPos) -> do
          let !sliceCopy = BS.copy slice
              consumed = BS.length sliceCopy
              n' = n - fromIntegral consumed
          if n' == 0
            then do
              termE <- SR.readExact (connectionReceive conn) newPos 2
              case termE of
                Left _ -> failBodyRead conn ParseBadChunkHeader
                Right (term, afterTerm)
                  | BS.length term < 2
                      || BS.index term 0 /= 0x0d
                      || BS.index term 1 /= 0x0a ->
                      failBodyRead conn ParseBadChunkHeader
                  | otherwise -> do
                      connectionAdvanceCursor conn afterTerm
                      writeIORef ref (ChunkPending 0)
            else do
              connectionAdvanceCursor conn newPos
              writeIORef ref (ChunkPending n')
          pure (Just sliceCopy)


readTrailers :: Connection -> IO [Header]
readTrailers conn = go SR.defaultHeaderBlockCap []
  where
    go !remaining acc = do
      pos <- readIORef (connCursor conn)
      r <- SR.readUntilCRLFStrict (connectionReceive conn) pos 8192
      case r of
        Left e -> failBodyRead conn (readErrorToParseError e)
        Right (Nothing, _) ->
          failBodyRead conn ParseInvalidHeaderValue
        Right (Just SR.StrictBareLf, _) ->
          failBodyRead conn ParseInvalidHeaderValue
        Right (Just (SR.StrictLine bs), newPos)
          | BS.null bs -> do
              writeIORef (connCursor conn) newPos
              pure (reverse acc)
          | let lineBytes = BS.length bs + 2
          , lineBytes <= remaining -> case parseHeaderLine bs of
              Left e -> failBodyRead conn e
              Right header -> do
                writeIORef (connCursor conn) newPos
                go (remaining - lineBytes) (header : acc)
          | otherwise -> failBodyRead conn ParseMessageTooLong


drainBody :: Body -> IO ()
drainBody = \case
  BodyEmpty -> pure ()
  BodyBytes _ -> pure ()
  BodyPreEncoded _ -> pure ()
  BodyFile _ -> pure ()
  BodyStream producer -> loop producer
  where
    loop producer = do
      mc <- producer
      case mc of
        Nothing -> pure ()
        Just _ -> loop producer


failBodyRead :: Connection -> ParseError -> IO a
failBodyRead conn e = retireConnection conn *> throwIO (ProtocolException e)


readErrorToParseError :: SR.ReadError -> ParseError
readErrorToParseError = \case
  SR.ReadParse e -> e
  SR.ReadMessageTooLong _ -> ParseMessageTooLong
  SR.ReadUnexpectedEof -> ParseUnexpectedEof
  SR.ReadTransportError _ -> ParseUnexpectedEof
