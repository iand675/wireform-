module Network.HTTP2.Connection
  ( Connection (..)
  , ConnectionConfig (..)
  , ConnectionRole (..)
  , ConnectionError (..)
  , newConnection
  , newConnectionFromTransport
  , sendFrame
  , sendFrameUnlocked
  , sendFrames
  , sendFramesUnlocked
  , sendHeaderBlock
  , sendEncodedHeaders
  , sendNewStreamHeaders
  , encodeAndSendHeaderBlock
  , headerBlockFrames
  , recvFrame
  , recvFrameRaw
  , closeConnection
  , connectionSettings
    -- * Re-exports
  , module Network.HTTP2.Connection.Settings
  , module Network.HTTP2.Connection.FlowControl
  , module Network.HTTP2.Connection.StreamTable
  , module Network.HTTP2.Transport
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (Exception, catch, SomeException)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word
import Network.Socket (Socket)

import qualified Wireform.Transport as WT
import Wireform.Transport.Send (SendTransport, sendByteString, sendByteStringMany, sendClose)
import Wireform.Network (newReceiveBufTransport)
import Wireform.Network.Transport.Send (newSendBufTransport)
import qualified Wireform.Transport.Config as WC
import Wireform.Transport.Config (defaultTransportConfig)

import Network.HTTP2.Connection.FlowControl
import Network.HTTP2.Connection.Settings
import Network.HTTP2.Connection.StreamTable
import Network.HTTP2.Frame
import qualified Network.HTTP2.Frame.StreamingReader as SR
import Network.HTTP2.HPACK
import Network.HTTP2.Transport
import Network.HTTP2.Types

data ConnectionRole = RoleClient | RoleServer
  deriving stock (Eq, Show)

-- | Static configuration for opening a connection.
--
-- A connection can be opened either over a raw socket (the common case;
-- pass 'ccSocket') or over an arbitrary 'Transport' (e.g. a TLS-wrapped
-- stream; pass 'ccTransport'). Exactly one of those two fields must be
-- 'Just'.
data ConnectionConfig = ConnectionConfig
  { ccRole :: !ConnectionRole
  , ccSettings :: !Settings
  , ccSocket :: !(Maybe Socket)
  , ccTransport :: !(Maybe Transport)
  , ccOnGoAway :: StreamId -> ErrorCode -> ByteString -> IO ()
  }

data ConnectionError = ConnectionError
  { ceErrorCode :: !ErrorCode
  , ceMessage :: !ByteString
  , ceStreamId :: !StreamId
  }
  deriving stock (Eq, Show)

instance Exception ConnectionError

data Connection = Connection
  { connRole :: !ConnectionRole
  , connTransport :: !Transport
  , connSocket :: !(Maybe Socket)
    -- ^ The raw socket, when the transport was built from one. Higher
    -- layers (e.g. server accept loops that want to know the peer addr)
    -- can use this; TLS connections may leave it 'Nothing'.
  , connLocalSettings :: !(IORef Settings)
  , connRemoteSettings :: !(IORef Settings)
  , connStreamTable :: !StreamTable
  , connSendFlowControl :: !FlowControl
  , connRecvFlowControl :: !FlowControl
  , connHpackEncoder :: !(MVar DynamicTable)
  , connHpackDecoder :: !(MVar DynamicTable)
  , connSendLock :: !(MVar ())
  , connRingTransport :: !WT.ReceiveTransport
    -- ^ Magic-ring transport plumbed onto @tRecvBuf connTransport@.
    -- Owns its own 'Wireform.Ring.Internal.MagicRing' (destroyed
    -- on 'closeConnection') and is the sole receive side for the
    -- recv loop.  Replaces the previous pinned 'RecvBuffer'.
  , connRingCursor :: !(IORef Word64)
    -- ^ Position past the last byte consumed by 'recvFrame' /
    -- 'recvFrameRaw'.  Chained through the StreamingReader so we
    -- don't pay a 'receiveLoadHead' round-trip per frame.
  , connSendTransport :: !SendTransport
    -- ^ Magic-ring send transport built from @tSendFn connTransport@.
    -- All frame data flows through the ring and is drained to the
    -- wire via the inline send loop in the ring.
  , connLastStreamId :: !(IORef StreamId)
  , connClosed :: !(IORef Bool)
  , connOnGoAway :: StreamId -> ErrorCode -> ByteString -> IO ()
  }

-- | Build a 'Connection' from either a 'Socket' (the common case) or an
-- arbitrary 'Transport'. See 'newConnectionFromTransport' for the
-- transport-only variant.
newConnection :: ConnectionConfig -> IO Connection
newConnection cfg = case (ccTransport cfg, ccSocket cfg) of
  (Just t, mSock) -> mkConnection (ccRole cfg) (ccSettings cfg) (ccOnGoAway cfg) t mSock
  (Nothing, Just sock) ->
    mkConnection (ccRole cfg) (ccSettings cfg) (ccOnGoAway cfg) (socketTransport sock) (Just sock)
  (Nothing, Nothing) ->
    error "Network.HTTP2.Connection.newConnection: ConnectionConfig has neither ccTransport nor ccSocket"

-- | Build a 'Connection' over a generic 'Transport'. Use this when the
-- connection lives on top of something other than a bare TCP socket
-- (notably TLS).
newConnectionFromTransport
  :: ConnectionRole
  -> Settings
  -> (StreamId -> ErrorCode -> ByteString -> IO ())
  -> Transport
  -> IO Connection
newConnectionFromTransport role settings onGoAway t =
  mkConnection role settings onGoAway t Nothing

mkConnection
  :: ConnectionRole
  -> Settings
  -> (StreamId -> ErrorCode -> ByteString -> IO ())
  -> Transport
  -> Maybe Socket
  -> IO Connection
mkConnection role settings onGoAway transport mSock = do
  localSettings <- newIORef settings
  remoteSettings <- newIORef defaultSettings
  streamTable <- newStreamTable (role == RoleServer)
  sendFC <- atomically $ newFlowControl 65535
  recvFC <- atomically $ newFlowControl 65535
  encoder <- newDynamicTable 4096 >>= newMVar
  decoder <- newDynamicTable 4096 >>= newMVar
  sendLock <- newMVar ()
  let !ringCfg = defaultTransportConfig { WC.ringSizeHint = 1024 * 1024 }
  ringT <- newReceiveBufTransport ringCfg (tRecvBuf transport)
  ringCursor <- newIORef 0
  sendT <- newSendBufTransport ringCfg (tSendFn transport) (tShutdownWrite transport)
  lastStreamId <- newIORef 0
  closed <- newIORef False
  pure Connection
    { connRole = role
    , connTransport = transport
    , connSocket = mSock
    , connLocalSettings = localSettings
    , connRemoteSettings = remoteSettings
    , connStreamTable = streamTable
    , connSendFlowControl = sendFC
    , connRecvFlowControl = recvFC
    , connHpackEncoder = encoder
    , connHpackDecoder = decoder
    , connSendLock = sendLock
    , connRingTransport = ringT
    , connRingCursor = ringCursor
    , connSendTransport = sendT
    , connLastStreamId = lastStreamId
    , connClosed = closed
    , connOnGoAway = onGoAway
    }

-- | Send a frame. Encodes and sends in one operation.
-- Uses a send lock to ensure frames aren't interleaved between connections.
sendFrame :: Connection -> Frame -> IO ()
sendFrame conn frame = do
  let bs = encodeFrame frame
  withMVar (connSendLock conn) $ \_ ->
    sendByteString (connSendTransport conn) bs

-- | Send a frame without acquiring the send lock.
--
-- __Unsafe__: concurrent callers will interleave frame bytes on the wire.
-- Only exposed for benchmarks that provably run single-threaded per connection.
-- Production code should use 'sendFrame'.
{-# INLINE sendFrameUnlocked #-}
sendFrameUnlocked :: Connection -> Frame -> IO ()
sendFrameUnlocked conn frame =
  sendByteString (connSendTransport conn) (encodeFrame frame)

-- | Send multiple frames in a single write (reduces syscall overhead).
sendFrames :: Connection -> [Frame] -> IO ()
sendFrames conn frames = do
  let bss = map encodeFrame frames
  withMVar (connSendLock conn) $ \_ ->
    sendByteStringMany (connSendTransport conn) bss

-- | Encode @headers@ with the connection's HPACK encoder and emit the
-- resulting block as a single HEADERS frame, holding the send lock
-- across /both/ the encode and the send.
--
-- This atomicity is mandatory. HPACK is stateful: the encoder mutates a
-- dynamic table as it encodes, and the peer's decoder replays those
-- mutations in the order the header blocks arrive on the wire. Encoding
-- under the HPACK lock and then sending under the send lock as two
-- separate critical sections lets two concurrent streams encode in one
-- order and reach the wire in the other — desyncing the peer's dynamic
-- table, after which it mis-decodes every subsequent header block (the
-- classic symptom is a dropped @:status@ / @PeerMissingPseudoHeaderStatus@).
-- Holding the send lock for the whole encode+send makes the table
-- mutation order identical to the wire order.
sendEncodedHeaders
  :: Connection
  -> StreamId
  -> Bool                          -- ^ set END_STREAM on the HEADERS frame
  -> [(ByteString, ByteString)]    -- ^ raw header list (incl. pseudo-headers)
  -> IO ()
sendEncodedHeaders conn sid endStream headers =
  withMVar (connSendLock conn) $ \_ -> do
    block <- withMVar (connHpackEncoder conn) $ \enc ->
      encodeHeaderBlock defaultEncodeStrategy enc headers
    let !len   = BS.length block
        flags  = flagEndHeaders .|. (if endStream then flagEndStream else 0)
        frame  = Frame
                   (FrameHeader (fromIntegral len) FrameHeaders flags sid)
                   (HeadersFrame Nothing block)
    sendByteString (connSendTransport conn) (encodeFrame frame)

-- | Open a new (client-initiated) stream: atomically run @openStream@ to
-- allocate the next stream id /and/ register its engine state, then encode
-- and send the request HEADERS — all under the send lock, returning the id.
--
-- Two atomicity requirements collapse into this one critical section:
--
--   * HTTP\/2 requires a new stream's id to be numerically greater than
--     every stream the endpoint has already opened (RFC 9113 §5.1.1). A
--     peer that receives a HEADERS frame whose id is lower than one it has
--     already seen MUST treat it as a connection error. Allocating the id
--     outside the send lock lets two concurrent openers grab ids @n@ and
--     @n+2@ and then reach the wire in the opposite order — corrupting the
--     connection for every stream on it.
--
--   * the HPACK encode must stay atomic with the send (see
--     'sendEncodedHeaders').
--
-- @openStream@ runs while the lock is held and before the HEADERS reach the
-- wire, so the recv loop can never see a response for a stream that has not
-- yet been registered.
sendNewStreamHeaders
  :: Connection
  -> IO StreamId                   -- ^ allocate next id + register state (under lock)
  -> Bool                          -- ^ set END_STREAM on the HEADERS frame
  -> [(ByteString, ByteString)]    -- ^ raw header list (incl. pseudo-headers)
  -> IO StreamId
sendNewStreamHeaders conn openStream endStream headers =
  withMVar (connSendLock conn) $ \_ -> do
    sid   <- openStream
    block <- withMVar (connHpackEncoder conn) $ \enc ->
      encodeHeaderBlock defaultEncodeStrategy enc headers
    let !len  = BS.length block
        flags = flagEndHeaders .|. (if endStream then flagEndStream else 0)
        frame = Frame
                  (FrameHeader (fromIntegral len) FrameHeaders flags sid)
                  (HeadersFrame Nothing block)
    sendByteString (connSendTransport conn) (encodeFrame frame)
    pure sid

-- | Emit an encoded HPACK header block as a HEADERS frame followed
-- by zero or more CONTINUATION frames, splitting at the peer's
-- @SETTINGS_MAX_FRAME_SIZE@.  END_HEADERS is set on the final frame;
-- the @endStream@ flag is set on the initial HEADERS frame only.
--
-- A header block that fits within one frame is sent as a single
-- HEADERS with @END_HEADERS@ set, matching the pre-CONTINUATION
-- code path bit-for-bit.
--
-- The frames are sent atomically (with the connection send lock held)
-- so concurrent senders on other streams can't interleave a frame
-- between our HEADERS and its CONTINUATION block, which the wire
-- protocol forbids (RFC 9113 §6.10).
sendHeaderBlock
  :: Connection
  -> StreamId
  -> Bool         -- ^ set END_STREAM on the initial HEADERS frame
  -> FrameFlags   -- ^ extra flags to OR into the initial HEADERS frame
  -> ByteString   -- ^ encoded HPACK header block
  -> Int          -- ^ peer SETTINGS_MAX_FRAME_SIZE
  -> IO ()
sendHeaderBlock conn sid endStream extraFlags block maxFrame =
  sendFrames conn (headerBlockFrames sid endStream extraFlags block maxFrame)

-- | Split an already-encoded HPACK header block into a HEADERS frame
-- followed by zero or more CONTINUATION frames at @maxFrame@ boundaries.
-- END_HEADERS is set on the final frame; @extraFlags@ and END_STREAM (when
-- requested) ride the initial HEADERS frame only. Pure, so the same
-- splitting feeds both the locked 'sendHeaderBlock' and the
-- encode-and-send-atomic 'encodeAndSendHeaderBlock'.
headerBlockFrames :: StreamId -> Bool -> FrameFlags -> ByteString -> Int -> [Frame]
headerBlockFrames sid endStream extraFlags block maxFrame
  | n <= maxFrame =
      [ Frame
          (FrameHeader (fromIntegral n) FrameHeaders soleFlags sid)
          (HeadersFrame Nothing block) ]
  | otherwise = headFrame head1 initialFlags : contFrames rest
  where
    n = BS.length block
    soleFlags = flagEndHeaders .|. extraFlags
              .|. (if endStream then flagEndStream else 0)
    initialFlags = extraFlags .|. (if endStream then flagEndStream else 0)
    (head1, rest) = BS.splitAt maxFrame block
    headFrame bs flags = Frame
      (FrameHeader (fromIntegral (BS.length bs)) FrameHeaders flags sid)
      (HeadersFrame Nothing bs)
    contFrames bs
      | BS.length bs <= maxFrame =
          [Frame
            (FrameHeader (fromIntegral (BS.length bs)) FrameContinuation flagEndHeaders sid)
            (ContinuationFrame bs)]
      | otherwise =
          let (chunk, rest') = BS.splitAt maxFrame bs
              f = Frame
                (FrameHeader (fromIntegral maxFrame) FrameContinuation 0 sid)
                (ContinuationFrame chunk)
          in f : contFrames rest'

-- | Encode @headers@ with the connection's HPACK encoder and emit the
-- resulting block as a HEADERS (+ CONTINUATION) frame sequence, holding
-- the send lock across /both/ the encode and the send. This atomicity is
-- mandatory for the same reason as 'sendEncodedHeaders': HPACK is stateful,
-- so the encoder's dynamic-table mutation order MUST match the order header
-- blocks reach the wire, or a concurrent sender desyncs the peer's decoder.
-- Unlike 'sendEncodedHeaders' this splits oversized blocks into
-- CONTINUATION frames at @maxFrame@.
encodeAndSendHeaderBlock
  :: Connection
  -> StreamId
  -> Bool                          -- ^ set END_STREAM on the initial HEADERS frame
  -> FrameFlags                    -- ^ extra flags to OR into the initial HEADERS frame
  -> [(ByteString, ByteString)]    -- ^ raw header list (incl. pseudo-headers)
  -> Int                           -- ^ peer SETTINGS_MAX_FRAME_SIZE
  -> IO ()
encodeAndSendHeaderBlock conn sid endStream extraFlags headers maxFrame =
  withMVar (connSendLock conn) $ \_ -> do
    block <- withMVar (connHpackEncoder conn) $ \enc ->
      encodeHeaderBlock defaultEncodeStrategy enc headers
    sendFramesUnlocked conn (headerBlockFrames sid endStream extraFlags block maxFrame)

-- | Send multiple frames without the send lock. Combines into one write.
--
-- __Unsafe__: see 'sendFrameUnlocked'.
{-# INLINE sendFramesUnlocked #-}
sendFramesUnlocked :: Connection -> [Frame] -> IO ()
sendFramesUnlocked conn frames =
  sendByteStringMany (connSendTransport conn) (map encodeFrame frames)

-- | Receive a typed frame off the wire.  Walks the magic ring via
-- 'Network.HTTP2.Frame.StreamingReader.readFrameFrom' (single
-- 'receiveLoadHead' per frame, zero-copy payload slice into the
-- ring) and runs 'decodeFramePayload' for per-type validation.
--
-- Bytes of the payload slice are valid only until the connection's
-- ring tail next advances past them, which 'readFrameFrom' does on
-- success — copy via 'BS.copy' if you need the bytes past the
-- next recv loop iteration.
recvFrame :: Connection -> IO (Either FrameDecodeError Frame)
recvFrame conn = do
  pos <- readIORef (connRingCursor conn)
  r   <- SR.readFrameFrom (connRingTransport conn) pos
  case r of
    Right (fr, newPos) -> do
      writeIORef (connRingCursor conn) newPos
      pure (Right fr)
    Left (SR.ReadDecode e)        -> pure (Left e)
    Left SR.ReadUnexpectedEof     -> pure (Left FrameTooShort)
    Left (SR.ReadTransportError _) -> pure (Left FrameTooShort)

-- | Receive a frame header + raw payload without constructing the
-- typed 'FramePayload'.  Used by the engine layer for DATA / HEADERS
-- where the payload bytes IS what the caller wants.  Returns
-- 'Nothing' on connection close (clean EOF or transport error).
{-# INLINE recvFrameRaw #-}
recvFrameRaw :: Connection -> IO (Maybe (FrameHeader, ByteString))
recvFrameRaw conn = do
  pos <- readIORef (connRingCursor conn)
  r   <- SR.readFrameFrom (connRingTransport conn) pos
  case r of
    Right (Frame hdr (FramePayloadRaw bs), newPos) -> do
      writeIORef (connRingCursor conn) newPos
      pure (Just (hdr, bs))
    Left _ -> pure Nothing

closeConnection :: Connection -> ErrorCode -> ByteString -> IO ()
closeConnection conn code msg = do
  alreadyClosed <- atomicModifyIORef' (connClosed conn) (\c -> (True, c))
  if alreadyClosed
    then pure ()
    else do
      lastId <- readIORef (connLastStreamId conn)
      let goaway = Frame
            (FrameHeader 0 FrameGoAway 0 0)
            (GoAwayFrame lastId code msg)
      sendFrame conn goaway
        `catch` (\(_ :: SomeException) -> pure ())
      -- Tear down the send ring (flushes remaining bytes + unmaps).
      sendClose (connSendTransport conn)
        `catch` (\(_ :: SomeException) -> pure ())
      -- Tear down the receive ring (frees its mmap).  Any frame
      -- payload slices the caller still holds become dangling
      -- pointers; they should have been 'BS.copy'd inside the
      -- per-frame handler.
      WT.receiveClose (connRingTransport conn)
        `catch` (\(_ :: SomeException) -> pure ())

connectionSettings :: Connection -> IO (Settings, Settings)
connectionSettings conn = do
  local <- readIORef (connLocalSettings conn)
  remote <- readIORef (connRemoteSettings conn)
  pure (local, remote)
