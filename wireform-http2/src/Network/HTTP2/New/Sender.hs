{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Sender loop.
--
-- Design goals vs the existing http2 library:
--
-- * 'OUnary' is processed in a *single pass*: HEADERS frame → DATA frame →
--   trailing HEADERS frame, all encoded into the write buffer in one
--   'sendAll' call.  No re-enqueue of the DATA output and no second
--   dequeue round-trip.
--
-- * Connection flow control uses 'TxWindow' (IORef + MVar) instead of
--   STM, so the sender only touches STM to dequeue from the output queue.
--
-- * HPACK encoding is serialised by 'ctxHpackLock'; the lock is held only
--   for the microseconds it takes to encode the header block, not for the
--   entire frame send.
module Network.HTTP2.New.Sender
    ( frameSender
    ) where

import Control.Concurrent.STM
import Control.Exception (handle, SomeException)
import Control.Monad (unless, when, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (plusPtr, castPtr)
import Network.ByteOrder (bufferIO)

import Network.HTTP2.Frame hiding (EncodeInfo)
import qualified Network.HTTP2.Frame as F

import Network.HTTP2.New.Frame
import Network.HTTP2.New.HPACK
import Network.HTTP2.New.Types

----------------------------------------------------------------
-- Sender entry point

frameSender :: Config -> Context -> IO ()
frameSender cfg ctx =
    handle (\(_ :: SomeException) -> return ()) $ senderLoop cfg ctx

senderLoop :: Config -> Context -> IO ()
senderLoop cfg ctx@Context{..} = loop
  where
    loop = do
        item <- atomically $ do
            mc <- tryReadTQueue ctxControlQ
            case mc of
                Just x  -> return x
                Nothing -> readTQueue ctxOutputQ
        processItem cfg ctx item
        loop

processItem :: Config -> Context -> Output -> IO ()
processItem cfg ctx = \case
    OUnary strm rspHdrs body trailHdrs onDone ->
        sendUnary cfg ctx strm rspHdrs body trailHdrs onDone

    OStreaming strm rspHdrs producer trailHdrs onDone ->
        sendStreaming cfg ctx strm rspHdrs producer trailHdrs onDone

    OControl ctl ->
        sendControl cfg ctx ctl

----------------------------------------------------------------
-- Unary response: one-pass encoding.

sendUnary
    :: Config
    -> Context
    -> Stream
    -> [(ByteString, ByteString)]
    -> ByteString
    -> [(ByteString, ByteString)]
    -> IO ()
    -> IO ()
sendUnary cfg@Config{..} ctx@Context{..} strm@Stream{..} rspHdrs body trailHdrs onDone = do
    let sid = streamId

    -- Step 1: HPACK-encode both header blocks under the lock.
    -- We do this BEFORE any window accounting so the lock is held briefly.
    (hdrsBlock, trailBlock) <- withHpackLock ctxHpackLock $ do
        h <- encodeHeaders ctxHpackEnc cfgBufferSize
                 (statusPseudo 200 : rspHdrs)
        t <- encodeHeaders ctxHpackEnc cfgBufferSize trailHdrs
        return (h, t)

    -- Step 2: Check/consume connection + stream TX window for the body.
    let bodyLen = BS.length body
    when (bodyLen > 0) $ do
        waitTxWindow ctxConnTxWin bodyLen
        waitTxWindow streamTxWin  bodyLen

    -- Step 3: Write all frames into the write buffer in one contiguous pass.
    -- Layout: [HEADERS][DATA][HEADERS(trailers)]
    -- All fit comfortably in a 32 KiB buffer for typical gRPC responses.
    off0 <- writeHeadersFrameBS cfgWriteBuffer 0 sid
                (if bodyLen == 0 && null trailHdrs then setEndStream defaultFlags
                                                    else defaultFlags)
                hdrsBlock
    off1 <- if bodyLen > 0
                then writeDataFrameBS cfgWriteBuffer off0 sid
                         (if null trailHdrs then setEndStream defaultFlags
                                            else defaultFlags)
                         body
                else return off0
    off2 <- if not (null trailHdrs) || bodyLen == 0
                then writeHeadersFrameBS cfgWriteBuffer off1 sid
                         (setEndHeader (setEndStream defaultFlags))
                         trailBlock
                else return off1

    -- Step 4: Single sendAll — all frames in one syscall.
    bufferIO cfgWriteBuffer off2 cfgSendAll

    -- Step 5: Mark TX complete, run callback.
    writeIORef streamTxDone True
    onDone

----------------------------------------------------------------
-- Streaming response (first-pass headers, then body chunks).

sendStreaming
    :: Config
    -> Context
    -> Stream
    -> [(ByteString, ByteString)]
    -> (((ByteString, Bool) -> IO ()) -> IO ())
    -> [(ByteString, ByteString)]
    -> IO ()
    -> IO ()
sendStreaming cfg@Config{..} ctx@Context{..} strm@Stream{..} rspHdrs producer trailHdrs onDone = do
    let sid = streamId

    -- Encode and send HEADERS.
    hdrsBlock <- withHpackLock ctxHpackLock $
        encodeHeaders ctxHpackEnc cfgBufferSize
            (statusPseudo 200 : rspHdrs)
    off0 <- writeHeadersFrameBS cfgWriteBuffer 0 sid defaultFlags hdrsBlock
    bufferIO cfgWriteBuffer off0 cfgSendAll

    -- Stream body chunks from the producer.
    let sendChunk (chunk, isLast) = do
            let clen = BS.length chunk
            when (clen > 0) $ do
                waitTxWindow ctxConnTxWin clen
                waitTxWindow streamTxWin  clen
            let flags = if isLast && null trailHdrs
                            then setEndStream defaultFlags
                            else defaultFlags
            off <- writeDataFrameBS cfgWriteBuffer 0 sid flags chunk
            bufferIO cfgWriteBuffer off cfgSendAll
    producer sendChunk

    -- Trailing HEADERS.
    trailBlock <- withHpackLock ctxHpackLock $
        encodeHeaders ctxHpackEnc cfgBufferSize trailHdrs
    off1 <- writeHeadersFrameBS cfgWriteBuffer 0 sid
                (setEndHeader (setEndStream defaultFlags)) trailBlock
    bufferIO cfgWriteBuffer off1 cfgSendAll

    writeIORef streamTxDone True
    onDone

----------------------------------------------------------------
-- Control frame dispatch

sendControl :: Config -> Context -> ControlFrame -> IO ()
sendControl Config{..} ctx@Context{..} = \case
    CSettings sl -> do
        n <- writeSettingsFrame cfgWriteBuffer sl
        bufferIO cfgWriteBuffer n cfgSendAll

    CSettingsAck -> do
        n <- writeSettingsAckFrame cfgWriteBuffer
        bufferIO cfgWriteBuffer n cfgSendAll

    CPing payload -> do
        n <- writePingAckFrame cfgWriteBuffer payload
        bufferIO cfgWriteBuffer n cfgSendAll

    CGoAway lastSid ec msg -> do
        n <- writeGoAwayFrame cfgWriteBuffer lastSid ec msg
        bufferIO cfgWriteBuffer n cfgSendAll

    CRstStream sid ec -> do
        n <- writeRstStreamFrame cfgWriteBuffer sid ec
        bufferIO cfgWriteBuffer n cfgSendAll

    CWindowUpdate sid increment -> do
        n <- writeWindowUpdateFrame cfgWriteBuffer sid increment
        bufferIO cfgWriteBuffer n cfgSendAll

----------------------------------------------------------------
-- Buffer write helpers (offset-based, no separate allocation)

-- | Write a HEADERS frame at @off@ in the write buffer, return new offset.
writeHeadersFrameBS :: Buffer -> Int -> StreamId -> FrameFlags -> ByteString -> IO Int
writeHeadersFrameBS buf off sid flags hpackBlock = do
    let plen = BS.length hpackBlock
    putFrameHeaderAt buf off FrameHeaders plen sid flags
    copyBSAt buf (off + frameHeaderLength) hpackBlock
    return (off + frameHeaderLength + plen)

-- | Write a DATA frame at @off@, return new offset.
writeDataFrameBS :: Buffer -> Int -> StreamId -> FrameFlags -> ByteString -> IO Int
writeDataFrameBS buf off sid flags payload = do
    let plen = BS.length payload
    putFrameHeaderAt buf off FrameData plen sid flags
    copyBSAt buf (off + frameHeaderLength) payload
    return (off + frameHeaderLength + plen)

putFrameHeaderAt :: Buffer -> Int -> FrameType -> Int -> StreamId -> FrameFlags -> IO ()
putFrameHeaderAt buf off ftype plen sid flags =
    encodeFrameHeaderBuf ftype (FrameHeader plen flags sid) (buf `plusPtr` off)

copyBSAt :: Buffer -> Int -> ByteString -> IO ()
copyBSAt buf off bs = BS.useAsCStringLen bs $ \(src, len) ->
    copyBytes (buf `plusPtr` off) (castPtr src) len

----------------------------------------------------------------
-- Header helpers

statusPseudo :: Int -> (ByteString, ByteString)
statusPseudo n = (":status", BS.pack (map (fromIntegral . fromEnum) (show n)))
