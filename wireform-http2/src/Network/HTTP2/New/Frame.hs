{-# LANGUAGE LambdaCase #-}
-- | Frame-level I/O: parse frames from the socket, encode frames to the
-- write buffer.  Re-uses Network.HTTP2.Frame for the wire format.
module Network.HTTP2.New.Frame
    ( -- * Reading
      readFrame
    , readFrameHeader
      -- * Writing (into a Buffer)
    , writeDataFrame
    , writeHeadersFrame
    , writePingFrame
    , writePingAckFrame
    , writeSettingsFrame
    , writeSettingsAckFrame
    , writeGoAwayFrame
    , writeRstStreamFrame
    , writeWindowUpdateFrame
      -- * Re-exports
    , module Network.HTTP2.Frame
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (plusPtr, castPtr)
import Network.ByteOrder (Buffer)

import Network.HTTP2.Frame hiding (EncodeInfo)
import Network.HTTP2.New.Types (Config(..))

----------------------------------------------------------------
-- Reading

-- | Read one HTTP/2 frame header from the socket, validating it.
-- Returns the 'FrameType' alongside the 'FrameHeader'.
readFrameHeader :: Config -> IO (FrameType, FrameHeader)
readFrameHeader Config{cfgReadN} = do
    bs <- cfgReadN frameHeaderLength
    case checkFrameHeader (decodeFrameHeader bs) of
        Left  e -> ioError (userError (show e))
        Right r -> return r

-- | Read exactly one HTTP/2 frame from the socket.
readFrame :: Config -> IO Frame
readFrame cfg@Config{cfgReadN} = do
    (ftype, hdr) <- readFrameHeader cfg
    payload <- cfgReadN (payloadLength hdr)
    case decodeFramePayload ftype hdr payload of
        Left  e -> ioError (userError (show e))
        Right p -> return (Frame hdr p)

----------------------------------------------------------------
-- Writing helpers
--
-- Data and HEADERS frames write directly into the connection write buffer
-- using 'encodeFrameHeaderBuf' for the 9-byte header, then copy the payload
-- inline.  Control frames (SETTINGS, PING, GOAWAY, RST_STREAM,
-- WINDOW_UPDATE) use 'encodeFrame' for simplicity since they are rare.

-- | Write a DATA frame to the buffer.  Returns bytes written.
writeDataFrame :: Buffer -> StreamId -> FrameFlags -> ByteString -> IO Int
writeDataFrame buf sid flags body = do
    let plen = BS.length body
    encodeFrameHeaderBuf FrameData (FrameHeader plen flags sid) buf
    copyBSInto (buf `plusPtr` frameHeaderLength) body
    return (frameHeaderLength + plen)

-- | Write a HEADERS frame (already HPACK-compressed) to the buffer.
writeHeadersFrame :: Buffer -> StreamId -> FrameFlags -> ByteString -> IO Int
writeHeadersFrame buf sid flags hpackBlock = do
    let plen = BS.length hpackBlock
    encodeFrameHeaderBuf FrameHeaders (FrameHeader plen flags sid) buf
    copyBSInto (buf `plusPtr` frameHeaderLength) hpackBlock
    return (frameHeaderLength + plen)

writePingFrame :: Buffer -> ByteString -> IO Int
writePingFrame buf payload = do
    encodeFrameHeaderBuf FramePing (FrameHeader 8 defaultFlags 0) buf
    copyBSInto (buf `plusPtr` frameHeaderLength) payload
    return (frameHeaderLength + 8)

writePingAckFrame :: Buffer -> ByteString -> IO Int
writePingAckFrame buf payload = do
    encodeFrameHeaderBuf FramePing (FrameHeader 8 (setAck defaultFlags) 0) buf
    copyBSInto (buf `plusPtr` frameHeaderLength) payload
    return (frameHeaderLength + 8)

writeSettingsFrame :: Buffer -> SettingsList -> IO Int
writeSettingsFrame buf settings = do
    let encoded = encodeFrame (encodeInfo id 0) (SettingsFrame settings)
        n       = BS.length encoded
    copyBSInto buf encoded
    return n

writeSettingsAckFrame :: Buffer -> IO Int
writeSettingsAckFrame buf = do
    let encoded = encodeFrame (encodeInfo setAck 0) (SettingsFrame [])
        n       = BS.length encoded
    copyBSInto buf encoded
    return n

writeGoAwayFrame :: Buffer -> StreamId -> ErrorCode -> ByteString -> IO Int
writeGoAwayFrame buf lastSid errCode debugData = do
    let encoded = encodeFrame (encodeInfo id 0) (GoAwayFrame lastSid errCode debugData)
        n       = BS.length encoded
    copyBSInto buf encoded
    return n

writeRstStreamFrame :: Buffer -> StreamId -> ErrorCode -> IO Int
writeRstStreamFrame buf sid errCode = do
    let encoded = encodeFrame (encodeInfo id sid) (RSTStreamFrame errCode)
        n       = BS.length encoded
    copyBSInto buf encoded
    return n

writeWindowUpdateFrame :: Buffer -> StreamId -> WindowSize -> IO Int
writeWindowUpdateFrame buf sid increment = do
    let encoded = encodeFrame (encodeInfo id sid) (WindowUpdateFrame increment)
        n       = BS.length encoded
    copyBSInto buf encoded
    return n

----------------------------------------------------------------
-- Internal helpers

copyBSInto :: Buffer -> ByteString -> IO ()
copyBSInto dst bs = BS.useAsCStringLen bs $ \(src, len) ->
    copyBytes dst (castPtr src) len
