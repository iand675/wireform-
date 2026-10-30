{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

{- | Send-side magic-ring transport.

The dual of 'Wireform.Transport.Receive.ReceiveTransport':

* Producer = the encoder (a 'Wireform.Builder.Builder', a hand-
  written pointer-bumping codec, a 'ByteString' source, …).
  Advances @head@ by writing bytes into the ring at
  @[head, head + n)@ and publishing.
* Consumer = the wire (a network @sendmsg@ loop, a TLS
  @SSL_write@ loop, an io_uring @prep_send@ submission, an in-
  memory test sink, …).  Advances @tail@ as it drains.

The encoder never sees a wrap point: a single 'reserveSend' may
span the ring's wrap boundary, but the double mapping makes the
reserved pointer contiguous in virtual memory.
-}
module Wireform.Transport.Send (
  -- * The transport
  SendTransport (..),
  SendWait (..),
  sendRing,

  -- * Encoder-facing API
  reserveSend,
  withSendReservation,
  sendByteString,
  sendByteStringMany,
  sendBuilder,
  sendBuilderDirect,
  sendBuilderViaByteString,

  -- * Corking (batch multiple sends into one publish)
  withSendCork,

  -- * Exceptions
  SendRingFull (..),
  SendReservationTooLarge (..),
) where

import Control.Exception (Exception, SomeException, throwIO)
import Control.Monad (when)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.IORef
import Data.Typeable (Typeable)
import Data.Word (Word64, Word8)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Wireform.Builder qualified as B
import Wireform.Builder.FastBuilder (
  DataSink (RingSink),
  RingSinkState (..),
  RingSpill (NoSpill),
  finalizeRingSink,
  runBuilder,
 )
import Wireform.Ring.Internal (MagicRing (..))


------------------------------------------------------------------------
-- SendTransport
------------------------------------------------------------------------

{- | A consumer-side cursor + a slot to wait on more room.

Encoders interact with the wire exclusively through this record.
Implementations bind one to a socket, a TLS context, an io_uring
instance, an in-memory fixture, etc.
-}
data SendTransport = SendTransport
  { sendRingBase :: {-# UNPACK #-} !(Ptr Word8)
  -- ^ Base address of the send ring's first mapping.
  , sendRingSize :: {-# UNPACK #-} !Int
  -- ^ Physical ring size (N, not 2N).  Always a power of two.
  , sendRingMask :: {-# UNPACK #-} !Int
  -- ^ @sendRingSize - 1@, cached for cheap @pos .&. mask@.
  , sendLoadTail :: !(IO Word64)
  {- ^ Read the consumer's current tail position
  (monotonically increasing).
  -}
  , sendLoadHead :: !(IO Word64)
  {- ^ Read the current head position.  Encoders maintain this
  locally between commits; exposed here so debugging tools
  and background workers can observe progress.
  -}
  , sendPublishHead :: !(Word64 -> IO ())
  {- ^ Producer publishes a new head after writing bytes into the
  ring.  Implementations may use this as the flush trigger
  (e.g. an io_uring SQ submit) or rely on an explicit
  'sendFlush'.
  -}
  , sendWaitSpace :: !(Word64 -> IO SendWait)
  {- ^ Block until 'sendLoadTail >= pos - sendRingSize' — i.e.
  until there is room to advance head to @pos@.  The dual of
  'Wireform.Transport.Receive.receiveWaitData'.
  -}
  , sendFlush :: !(IO ())
  {- ^ Request the consumer side drain everything published so
  far.  For an inline (synchronous @sendmsg@ / @SSL_write@)
  consumer this is @pure ()@ because 'sendPublishHead' already
  drained.  For a background-worker / io_uring consumer this
  kicks the SQ / signals the worker.
  -}
  , sendShutdownWrite :: !(IO ())
  {- ^ Half-close (@shutdown(SHUT_WR)@ on TCP, @close_notify@
  on TLS), keeping the recv side alive so we can still read
  the peer's final reply.
  -}
  , sendClose :: !(IO ())
  {- ^ Full release: tear down the ring + any background worker.
  Idempotent.
  -}
  }


-- | Outcome of waiting for more room.
data SendWait
  = -- | New tail position.  Room is @sendRingSize - (head - tail)@.
    SendSpaceAvailable {-# UNPACK #-} !Word64
  | {- | Consumer closed (peer sent RST, FIN-after-shutdown, etc).
    Sticky.
    -}
    SendPeerClosed
  | -- | Wire-side failure.  Sticky.
    SendFailed !SomeException
  deriving stock (Show)


{- | Reconstruct the underlying send ring as a 'MagicRing'.
Polymorphic in @s@ for the same reason 'receiveRing' is — the
resulting handle does not inherit any scope.
-}
sendRing :: SendTransport -> MagicRing s
sendRing t = MagicRing (sendRingBase t) (sendRingSize t)
{-# INLINE sendRing #-}


------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------

{- | The send ring is full and the consumer has not yet drained any
room within the wait policy's budget.  Surfaced as a sticky
'SendFailed' (and re-thrown by 'reserveSend' / 'sendByteString')
instead of letting the producer/consumer pair spin forever.
-}
data SendRingFull = SendRingFull
  { sendRingFullSize :: !Int
  -- ^ Physical ring size (N).
  , sendRingFullHead :: !Word64
  , sendRingFullTail :: !Word64
  }
  deriving stock (Show, Typeable)


instance Exception SendRingFull


{- | A 'reserveSend' / 'sendByteString' asked for more bytes in a
single contiguous reservation than the ring physically holds.
The ring is sized at construction time; raise the
'Wireform.Transport.Config.ringSizeHint' for the connection if
you need to stage a larger payload in one shot.
-}
data SendReservationTooLarge = SendReservationTooLarge
  { sendReservationRequested :: !Int
  , sendReservationRingSize :: !Int
  }
  deriving stock (Show, Typeable)


instance Exception SendReservationTooLarge


------------------------------------------------------------------------
-- Encoder-facing API
------------------------------------------------------------------------

{- | Reserve a contiguous span of @n@ bytes at the current head.
Blocks (via 'sendWaitSpace') until there is room.  Returns a
pointer into the ring's double mapping (so the encoder can write
@n@ bytes contiguously regardless of wrap) and the new head that
'commitSend' / 'sendPublishHead' should publish once the write
is complete.

Throws 'SendReservationTooLarge' if @n > sendRingSize@.
Throws the sticky 'SendRingFull' / underlying 'SendFailed'
exception if the transport has been closed (concretely: the
ring's backing mmap may already be gone, so we must not hand
out a pointer into it).
-}
reserveSend
  :: SendTransport
  -> Int
  -> IO (Ptr Word8, Word64)
reserveSend t n
  | n < 0 = error "Wireform.Transport.Send.reserveSend: negative length"
  | n > sendRingSize t =
      throwIO
        ( SendReservationTooLarge
            { sendReservationRequested = n
            , sendReservationRingSize = sendRingSize t
            }
        )
  | otherwise = do
      h <- sendLoadHead t
      let !newHead = h + fromIntegral n
      ensureRoom t newHead
      let !off = fromIntegral h .&. sendRingMask t
          !writeP = sendRingBase t `plusPtr` off
      pure (writeP, newHead)
{-# INLINE reserveSend #-}


{- | Reserve up to @maxLen@ bytes, run the fill callback, publish
head by the number of bytes the callback actually wrote.
Convenient bracket-style wrapper around 'reserveSend' +
'sendPublishHead'.
-}
withSendReservation
  :: SendTransport
  -> Int
  -- ^ max bytes to reserve
  -> (Ptr Word8 -> Int -> IO Int)
  -- ^ fill callback, returns bytes written
  -> IO Int
withSendReservation t maxLen fill
  | maxLen <= 0 = pure 0
  | otherwise = do
      (p, _) <- reserveSend t maxLen
      written <- fill p maxLen
      if written < 0 || written > maxLen
        then error "Wireform.Transport.Send.withSendReservation: callback returned out-of-range length"
        else do
          when_ (written > 0) $ do
            h <- sendLoadHead t
            sendPublishHead t (h + fromIntegral written)
          pure written
  where
    when_ True m = m
    when_ False _ = pure ()
{-# INLINE withSendReservation #-}


{- | Copy a 'ByteString' into the send ring as one contiguous
reservation, then publish head.  The bytes are committed by the
time this returns; whether they have hit the wire by then
depends on the transport's consumer (an inline socket consumer
has already drained; an io_uring consumer has at least submitted
the SQE).
-}
sendByteString :: SendTransport -> ByteString -> IO ()
sendByteString t bs = do
  let !len = BS.length bs
  if len == 0
    then pure ()
    else do
      (p, newHead) <- reserveSend t len
      BSU.unsafeUseAsCStringLen bs $ \(src, _) ->
        copyBytes p (castPtr src) len
      sendPublishHead t newHead
{-# INLINE sendByteString #-}


{- | Stage many byte strings as one merged reservation, then
publish head once.  Lets the consumer coalesce them into a
single @sendmsg@ / io_uring SQE.
-}
sendByteStringMany :: SendTransport -> [ByteString] -> IO ()
sendByteStringMany _ [] = pure ()
sendByteStringMany t bss = do
  let !total = sum (fmap BS.length bss)
  if total == 0
    then pure ()
    else do
      (p, newHead) <- reserveSend t total
      copyAll p bss
      sendPublishHead t newHead
  where
    copyAll _ [] = pure ()
    copyAll dst (b : bs) = do
      let !n = BS.length b
      BSU.unsafeUseAsCStringLen b $ \(src, _) ->
        copyBytes dst (castPtr src) n
      copyAll (dst `plusPtr` n) bs
{-# INLINEABLE sendByteStringMany #-}


{- | Stage a 'B.Builder' into the send ring.  Equivalent to
'sendBuilderDirect' -- writes directly into ring memory with
no intermediate 'ByteString'.
-}
sendBuilder :: SendTransport -> B.Builder -> IO ()
sendBuilder = sendBuilderDirect
{-# INLINE sendBuilder #-}


{- | The pre-optimisation path: materialise to a strict
'ByteString', then memcpy into the ring.  Exported so
benchmarks can compare the two strategies side-by-side.
-}
sendBuilderViaByteString :: SendTransport -> B.Builder -> IO ()
sendBuilderViaByteString t b =
  sendByteString t (B.toStrictByteStringWith 4096 b)
{-# INLINE sendBuilderViaByteString #-}


{- | Run a 'B.Builder' directly into the send ring with no
intermediate 'ByteString' allocation or copy.

The builder writes into the ring's double-mapped memory.
When the current region fills, the already-written bytes are
published and the builder continues into freshly-available ring
space.  The final bytes are published before this function
returns.

For a typical small frame (< 32 KiB) this is one @runBuilder@
call with zero allocations beyond a single 'IORef'.  For larger
payloads the builder publishes in chunks, keeping memory
pressure proportional to the ring size rather than the payload.
-}
sendBuilderDirect :: SendTransport -> B.Builder -> IO ()
sendBuilderDirect t b = do
  h <- sendLoadHead t
  let !chunkSz = min (sendRingSize t) defaultDirectChunk
      !wantHead = h + fromIntegral chunkSz
  ensureRoom t wantHead
  let !off = fromIntegral h .&. sendRingMask t
      !ptr = sendRingBase t `plusPtr` off
  rsRef <-
    newIORef
      RingSinkState
        { rsHead = h
        , rsPublished = h
        , rsBase = sendRingBase t
        , rsMask = sendRingMask t
        , rsRingSize = sendRingSize t
        , rsChunkSize = chunkSz
        , rsPublishHead = sendPublishHead t
        , rsEnsureRoom = ensureRoom t
        , rsLoadTail = sendLoadTail t
        , rsSpill = NoSpill
        }
  finalCur <- runBuilder b (RingSink rsRef) ptr (ptr `plusPtr` chunkSz)
  _ <- finalizeRingSink rsRef finalCur
  pure ()
{-# INLINE sendBuilderDirect #-}


-- 32 KiB: large enough that most protocol frames never overflow,
-- small enough that the initial ensureRoom resolves immediately on
-- a typical 64-256 KiB ring.
defaultDirectChunk :: Int
defaultDirectChunk = 32768


------------------------------------------------------------------------
-- Corking
------------------------------------------------------------------------

{- | Run an action with deferred publishing.  Bytes written via
'sendByteString' \/ 'sendBuilderDirect' \/ etc. accumulate in the
ring without triggering the consumer (no @sendmsg@, no io_uring
SQE submission).  On scope exit, a single 'sendPublishHead' covers
everything written.

The cork is demand-driven: 'sendPublishHead' is a no-op inside the
scope, and the cork only breaks when 'sendWaitSpace' detects the
ring is genuinely full and needs a drain to continue.  This means
the cork batches as much as the ring can hold — for typical HTTP
responses (headers + body < ringSize) that is one publish total.
For payloads larger than ringSize, publishes happen only when
strictly necessary to free ring space.

@
withSendCork transport $ \corked -> do
  sendBuilderDirect corked (responseBuilder resp)
  sendByteString    corked bodyBytes
-- single publish \/ sendmsg here
@
-}
withSendCork :: SendTransport -> (SendTransport -> IO a) -> IO a
withSendCork t action = do
  h0 <- sendLoadHead t
  headRef <- newIORef h0
  publishedRef <- newIORef h0
  let corkedPublish h = writeIORef headRef h
      corkedWaitSpace pos = do
        r <- sendWaitSpace t pos
        case r of
          SendSpaceAvailable tl
            | pos - tl <= fromIntegral (sendRingSize t) -> pure r
            | otherwise -> do
                -- Ring full — publish accumulated bytes to trigger
                -- drain, then retry.
                cur <- readIORef headRef
                pub <- readIORef publishedRef
                when (cur > pub) $ do
                  sendPublishHead t cur
                  writeIORef publishedRef cur
                sendWaitSpace t pos
          _ -> pure r
  let corked =
        t
          { sendPublishHead = corkedPublish
          , sendLoadHead = readIORef headRef
          , sendWaitSpace = corkedWaitSpace
          }
  result <- action corked
  finalHead <- readIORef headRef
  pub <- readIORef publishedRef
  when (finalHead > pub) $ sendPublishHead t finalHead
  pure result


------------------------------------------------------------------------
-- Internal: wait for room
------------------------------------------------------------------------

{- | Block until the ring has room for head to advance to @newHead@.

Always consults 'sendWaitSpace' once, even when the local cursor
arithmetic says room is already available.  This is what surfaces
a sticky closed state (e.g. after 'sendClose') BEFORE the caller
reaches for the ring's base pointer — without this probe,
'reserveSend' on a closed inline transport would happily hand out
a pointer into the ring's already-unmapped backing memory.
-}
ensureRoom :: SendTransport -> Word64 -> IO ()
ensureRoom t newHead = loop
  where
    sz = fromIntegral (sendRingSize t) :: Word64
    loop = do
      r <- sendWaitSpace t newHead
      case r of
        SendSpaceAvailable tl
          | newHead - tl <= sz -> pure ()
          | otherwise -> loop
        SendPeerClosed ->
          throwIO
            ( SendRingFull
                { sendRingFullSize = sendRingSize t
                , sendRingFullHead = newHead
                , sendRingFullTail = newHead -- unknown; report newHead
                }
            )
        SendFailed e -> throwIO e
{-# INLINE ensureRoom #-}
