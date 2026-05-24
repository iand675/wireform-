{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
  * a closed-flag so finalizers don't double-close.

The body-reading helpers ('readBody', 'drainBody') run the framing
state machine inferred by 'Network.HTTP1.Parser.requestFraming' \/
'responseFraming' and feed the next request \/ response on the wire
as soon as the previous body is consumed (HTTP\/1.1 keep-alive +
pipelining).
-}
module Network.HTTP1.Connection
  ( Connection
  , newConnectionFromSocket
  , newConnectionFromTls
  , newConnectionFromDuplex
  , newConnectionFromDuplexWithRingSize
  , defaultRingSize
  , connectionReceive
  , connectionSend
  , connectionSocket
  , connectionSslConn
  , connectionCursor
  , connectionReadCursor
  , connectionAdvanceCursor
  , closeConnection
    -- * Send helpers
  , connectionSendBytes
  , connectionSendMany
  , connectionSendBuilder
  , connectionSendBuilderDirect
  , withConnectionCork
    -- * Head readers (zero-copy, SIMD on the ring)
  , readRequestHead
  , readResponseHead
    -- * Framing-aware body
  , readBody
  , readBodyAndTrailers
  , drainBody
  , ProtocolException (..)
  ) where

import Control.Concurrent.MVar
  (MVar, newEmptyMVar, newMVar, readMVar, tryPutMVar)
import Control.Exception (Exception, SomeException, throwIO, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word (Word64)
import GHC.Generics (Generic)
import Network.Socket (Socket)
import Network.HTTP1.Headers (Header)

import qualified Wireform.Builder as B
import Wireform.Network
  ( DuplexTransport (..)
  , closeDuplexTransport
  , newDuplexTransport
  )
import qualified Wireform.Transport.Config as WC
import Wireform.Transport.Config (defaultTransportConfig)
import Wireform.Transport.Receive (ReceiveTransport, receiveAdvanceTail)
import Wireform.Transport.Send
  ( SendTransport
  , sendBuilder
  , sendBuilderDirect
  , sendByteString
  , sendByteStringMany
  , withSendCork
  )

import Wireform.Network.TLS.OpenSSL (SslConn, freeConn, newTlsDuplexTransport, sslConnSocket)

import qualified Network.HTTP1.Method as Method
import Network.HTTP1.Parser (Framing (..), ParseError (..))
import qualified Network.HTTP1.StreamingReader as SR
import Network.HTTP1.Types
  ( Body (..)
  , Request
  , Response
  )

-- | Thrown by a streaming-body producer when the wire bytes violate
-- the framing the parser inferred.
newtype ProtocolException = ProtocolException ParseError
  deriving stock (Eq, Show, Generic)

instance Exception ProtocolException

data Connection = Connection
  { connDuplex  :: !DuplexTransport
  , connSocket  :: !(Maybe Socket)
    -- ^ The raw socket fd if this is a plain TCP connection.  TLS
    -- connections set this to 'Nothing' so the sendfile fast path
    -- correctly falls back to a userspace copy.
  , connSslConn :: !(Maybe SslConn)
    -- ^ The OpenSSL connection if this is a TLS connection.  Used
    -- so 'closeConnection' can issue @SSL_shutdown@ + free the
    -- @SSL*@.
  , connCursor  :: !(IORef Word64)
    -- ^ Position past the last byte the recv path has consumed.
  , connClosed  :: !(IORef Bool)
  }

-- | Default magic-ring size: 256 KiB.  Easily fits the largest
-- header block (h2o caps requests at 32 KiB by default) plus
-- several chunked-TE body chunks (16 KiB cap each) plus some
-- breathing room, while keeping per-connection virtual memory
-- modest.
defaultRingSize :: Int
defaultRingSize = 256 * 1024

-- | Build a 'Connection' from a plain TCP 'Socket'.  Allocates one
-- receive ring + one send ring of 'defaultRingSize' bytes each.
-- Caller is responsible for closing the socket; 'closeConnection'
-- only tears down the rings.
newConnectionFromSocket :: Socket -> IO Connection
newConnectionFromSocket sock = do
  let !cfg = defaultTransportConfig { WC.ringSizeHint = defaultRingSize }
  duplex <- newDuplexTransport cfg sock
  buildConnection duplex (Just sock) Nothing

-- | Build a 'Connection' from a handshaked OpenSSL TLS connection.
-- The @SSL*@ is owned by the 'Connection': 'closeConnection' issues
-- @SSL_shutdown@ then frees the SSL.  The underlying socket is NOT
-- closed (caller owns the socket).
newConnectionFromTls :: SslConn -> IO Connection
newConnectionFromTls conn = do
  let !cfg = defaultTransportConfig { WC.ringSizeHint = defaultRingSize }
  duplex <- newTlsDuplexTransport cfg conn
  -- For TLS we deliberately set connSocket to Nothing so the
  -- sendfile(2) fast path falls back to a userspace copy.
  buildConnection duplex Nothing (Just conn)

-- | Build a 'Connection' from an externally-managed 'DuplexTransport'.
-- Used by in-memory pipes / tests that already own the duplex.
-- 'closeConnection' will still call 'duplexClose'.
newConnectionFromDuplex :: DuplexTransport -> IO Connection
newConnectionFromDuplex = newConnectionFromDuplexWithRingSize defaultRingSize

-- | Like 'newConnectionFromDuplex' but the ring-size argument is
-- noted purely for forwards-compatibility (the duplex is taken
-- as-is).  Kept for API symmetry with the pre-rewrite shape.
newConnectionFromDuplexWithRingSize :: Int -> DuplexTransport -> IO Connection
newConnectionFromDuplexWithRingSize _ duplex =
  buildConnection duplex Nothing Nothing

buildConnection :: DuplexTransport -> Maybe Socket -> Maybe SslConn -> IO Connection
buildConnection duplex mSock mSsl = do
  cursor <- newIORef 0
  closed <- newIORef False
  pure Connection
    { connDuplex  = duplex
    , connSocket  = mSock
    , connSslConn = mSsl
    , connCursor  = cursor
    , connClosed  = closed
    }

-- | The receive-side transport.
connectionReceive :: Connection -> ReceiveTransport
connectionReceive = duplexReceive . connDuplex

-- | The send-side transport.
connectionSend :: Connection -> SendTransport
connectionSend = duplexSend . connDuplex

-- | The raw socket if this connection is plain TCP, else 'Nothing'.
-- The sendfile fast path branches on this.
connectionSocket :: Connection -> Maybe Socket
connectionSocket conn = case connSocket conn of
  Just s  -> Just s
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

-- | Bump the cursor to the supplied position and tell the recv ring
-- transport it can recycle bytes up to that point.
connectionAdvanceCursor :: Connection -> Word64 -> IO ()
connectionAdvanceCursor conn pos = do
  writeIORef (connCursor conn) pos
  receiveAdvanceTail (connectionReceive conn) pos

-- | Send the given bytes through the connection's send ring.  Goes
-- through one ring reservation + one drain into the wire.
connectionSendBytes :: Connection -> ByteString -> IO ()
connectionSendBytes conn = sendByteString (connectionSend conn)

-- | Send multiple byte strings as one merged reservation.  Lets the
-- kernel coalesce them into one @sendmsg@.
connectionSendMany :: Connection -> [ByteString] -> IO ()
connectionSendMany conn = sendByteStringMany (connectionSend conn)

-- | Direct ring write of a 'B.Builder' into the send ring
-- (no intermediate 'ByteString').  Delegates to 'sendBuilder'
-- which uses 'sendBuilderDirect' under the hood.
connectionSendBuilder :: Connection -> B.Builder -> IO ()
connectionSendBuilder conn = sendBuilder (connectionSend conn)

-- | Like 'connectionSendBuilder' but uses 'sendBuilderDirect'
-- explicitly (writes the builder directly into ring memory).
connectionSendBuilderDirect :: Connection -> B.Builder -> IO ()
connectionSendBuilderDirect conn = sendBuilderDirect (connectionSend conn)

-- | Cork the connection's send transport.  Bytes written inside
-- the callback accumulate in the ring without triggering a drain
-- (no @sendmsg@ \/ io_uring SQE).  On exit, a single publish
-- covers everything — one syscall for headers + body.
--
-- If the ring fills mid-cork, the cork falls back to a real
-- publish so the consumer can drain.
withConnectionCork :: Connection -> (SendTransport -> IO a) -> IO a
withConnectionCork conn = withSendCork (connectionSend conn)

-- | Tear down the connection.  Order: drain + close the send ring,
-- close the receive ring, then (for TLS) issue @SSL_shutdown@ and
-- free the @SSL*@.  Idempotent.  The underlying 'Socket' is NOT
-- closed; the caller owns its lifetime.
closeConnection :: Connection -> IO ()
closeConnection conn = do
  wasClosed <- atomicModifyIORef' (connClosed conn) (\c -> (True, c))
  if wasClosed
    then pure ()
    else do
      _ <- try @SomeException (closeDuplexTransport (connDuplex conn))
      case connSslConn conn of
        Just s  -> do
          _ <- try @SomeException (freeConn s)
          pure ()
        Nothing -> pure ()

------------------------------------------------------------------------
-- Head readers
------------------------------------------------------------------------

readRequestHead
  :: Connection
  -> IO (Either SR.ReadError (Request, Framing))
readRequestHead conn = do
  pos <- readIORef (connCursor conn)
  r   <- SR.readRequestHeadFrom (connectionReceive conn) pos
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
  r   <- SR.readResponseHeadFrom (connectionReceive conn) pos reqMethod
  case r of
    Right (ok, newPos) -> do
      writeIORef (connCursor conn) newPos
      pure (Right ok)
    Left e -> pure (Left e)

------------------------------------------------------------------------
-- Body
------------------------------------------------------------------------

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
              Left _ -> do
                _ <- tryPutMVar trailersMV []
                pure Nothing
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
          Left _ -> do
            _ <- tryPutMVar trailersMV []
            pure Nothing
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
        Left e -> throwIO (ProtocolException (readErrorToParseError e))
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
        Left _ -> throwIO (ProtocolException ParseUnexpectedEof)
        Right (slice, newPos) -> do
          let !sliceCopy = BS.copy slice
              consumed = BS.length sliceCopy
              n' = n - fromIntegral consumed
          if n' == 0
            then do
              termE <- SR.readExact (connectionReceive conn) newPos 2
              case termE of
                Left _ -> throwIO (ProtocolException ParseBadChunkHeader)
                Right (term, afterTerm)
                  | BS.length term < 2
                      || BS.index term 0 /= 0x0d
                      || BS.index term 1 /= 0x0a ->
                      throwIO (ProtocolException ParseBadChunkHeader)
                  | otherwise -> do
                      connectionAdvanceCursor conn afterTerm
                      writeIORef ref (ChunkPending 0)
            else do
              connectionAdvanceCursor conn newPos
              writeIORef ref (ChunkPending n')
          pure (Just sliceCopy)

readTrailers :: Connection -> IO [Header]
readTrailers conn = go []
  where
    go acc = do
      pos <- readIORef (connCursor conn)
      r   <- SR.readUntilCRLFStrict (connectionReceive conn) pos 8192
      case r of
        Left e -> throwIO (ProtocolException (readErrorToParseError e))
        Right (Nothing, _) ->
          throwIO (ProtocolException ParseInvalidHeaderValue)
        Right (Just SR.StrictBareLf, _) ->
          throwIO (ProtocolException ParseInvalidHeaderValue)
        Right (Just (SR.StrictLine bs), newPos) -> do
          writeIORef (connCursor conn) newPos
          if BS.null bs
            then pure (reverse acc)
            else if BS.any badByte bs
                   then throwIO (ProtocolException ParseInvalidHeaderValue)
                   else case parseTrailerLine bs of
                     Nothing  -> throwIO (ProtocolException ParseInvalidHeaderValue)
                     Just hdr -> go (hdr : acc)
    badByte b = b == 0x0a || b == 0x00 || b == 0x0d

parseTrailerLine :: ByteString -> Maybe Header
parseTrailerLine bs = do
  let (rawName, rest0) = BS.break (== 0x3A {- ':' -}) bs
  case BS.uncons rest0 of
    Just (0x3A, rest1) ->
      let value = stripOWS rest1
      in if BS.null rawName then Nothing else Just (rawName, value)
    _ -> Nothing
  where
    stripOWS = BS.dropWhile isOWS . BS.reverse . BS.dropWhile isOWS . BS.reverse
    isOWS b = b == 0x20 || b == 0x09

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

readErrorToParseError :: SR.ReadError -> ParseError
readErrorToParseError = \case
  SR.ReadParse e            -> e
  SR.ReadMessageTooLong _   -> ParseMessageTooLong
  SR.ReadUnexpectedEof      -> ParseUnexpectedEof
  SR.ReadTransportError _   -> ParseUnexpectedEof
