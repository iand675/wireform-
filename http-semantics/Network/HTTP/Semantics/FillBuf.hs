{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.HTTP.Semantics.FillBuf (
    -- * Filling a buffer
    Next (..),
    DynaNext,
    BytesFilled,
    StreamingChunk (..),
    IsEndOfStream (..),
    CleanupStream,
    fillBuilderBodyGetNext,
    fillFileBodyGetNext,
    fillStreamBodyGetNext,
) where

import Control.Exception (SomeException)
import Control.Monad
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder.Extra as B
import Data.ByteString.Internal
import Data.Int (Int64)
import Data.Maybe
import Foreign.Ptr (plusPtr)
import Network.ByteOrder
import Network.HTTP.Semantics.Client

----------------------------------------------------------------

-- | Write part of a streaming response to the write buffer
--
-- In @http2@ this will be used to construct a single HTTP2 @DATA@ frame
-- (see discussion of the maximum number of bytes, below).
type DynaNext =
    Buffer
    -- ^ Write buffer
    -> Int
    -- ^ Maximum number of bytes we are allowed to write
    --
    -- In @http2@, this maximum will be set to the space left in the write
    -- buffer. Implicitly this also means that this maximum cannot exceed the
    -- maximum size of a HTTP2 frame, since in @http2@ the size of the write
    -- buffer is also used to set @SETTINGS_MAX_FRAME_SIZE@ (see
    -- @confBufferSize@).
    -> IO Next
    -- ^ Information on the data written, and on how to continue if not all data
    -- was written

type BytesFilled = Int

data Next
    = Next
        BytesFilled -- payload length
        Bool -- require flushing
        (Maybe DynaNext)
    | CancelNext (Maybe SomeException)

----------------------------------------------------------------

data StreamingChunk
    = -- | Indicate that the stream is finished
      StreamingFinished (Maybe CleanupStream)
    | -- | Indicate that the stream is cancelled
      StreamingCancelled (Maybe SomeException)
    | -- | Flush the stream
      --
      -- This will cause the write buffer to be written to the network socket,
      -- without waiting for more data.
      StreamingFlush
    | -- | Construct a DATA frame, optionally terminating the stream
      StreamingBuilder Builder IsEndOfStream

-- | Action to run prior to terminating the stream
type CleanupStream = IO ()

data IsEndOfStream
    = -- | The stream is not yet terminated
      NotEndOfStream
    | -- | The stream is terminated
      --
      -- In addition to indicating that the stream is terminated, we can also
      -- specify an optional `Cleanup` handler to be run.
      EndOfStream (Maybe CleanupStream)

----------------------------------------------------------------

fillBuilderBodyGetNext :: Builder -> DynaNext
fillBuilderBodyGetNext bb buf room = do
    (len, signal) <- B.runBuilder bb buf room
    return $ nextForBuilder len signal

fillFileBodyGetNext
    :: PositionRead -> FileOffset -> ByteCount -> Sentinel -> DynaNext
fillFileBodyGetNext pread start bytecount sentinel buf room = do
    len <- pread start (mini room bytecount) buf
    let len' = fromIntegral len
    nextForFile len' pread (start + len) (bytecount - len) sentinel

fillStreamBodyGetNext :: IO (Maybe StreamingChunk) -> DynaNext
fillStreamBodyGetNext takeQ = loop 0
  where
    loop :: NextWithTotal
    loop total buf room = do
        mChunk <- takeQ
        case mChunk of
            Just chunk -> runStreamingChunk chunk loop total buf room
            Nothing -> return $ Next total False (Just $ loop 0)

----------------------------------------------------------------

fillBufBuilderOne :: Int -> B.BufferWriter -> DynaNext
fillBufBuilderOne minReq writer buf0 room = do
    if room >= minReq
        then do
            (len, signal) <- writer buf0 room
            return $ nextForBuilder len signal
        else do
            return $ Next 0 True (Just $ fillBufBuilderOne minReq writer)

fillBufBuilderTwo :: ByteString -> B.BufferWriter -> DynaNext
fillBufBuilderTwo bs writer buf0 room
    | BS.length bs <= room = do
        buf1 <- copy buf0 bs
        let len1 = BS.length bs
        (len2, signal) <- writer buf1 (room - len1)
        return $ nextForBuilder (len1 + len2) signal
    | otherwise = do
        let (bs1, bs2) = BS.splitAt room bs
        void $ copy buf0 bs1
        return $ nextForBuilder room (B.Chunk bs2 writer)

nextForBuilder :: BytesFilled -> B.Next -> Next
nextForBuilder len B.Done =
    Next len True Nothing -- let's flush
nextForBuilder len (B.More minReq writer) =
    Next len False $ Just (fillBufBuilderOne minReq writer)
nextForBuilder len (B.Chunk bs writer) =
    Next len False $ Just (fillBufBuilderTwo bs writer)

----------------------------------------------------------------

-- | Like 'DynaNext', but with additional argument indicating total bytes written
--
-- Since @http2@ uses @DynaNext@ to construct a /single/ @DATA@ frame, the
-- \"total number of bytes written\" refers to the current size of the payload
-- of that @DATA@ frame.
type NextWithTotal = Int -> DynaNext

-- | Run the chunk, then continue as specified, unless streaming is finished
runStreamingChunk :: StreamingChunk -> NextWithTotal -> NextWithTotal
runStreamingChunk chunk next =
    case chunk of
        StreamingFinished mdec -> finished mdec
        StreamingCancelled mErr -> cancel mErr
        StreamingFlush -> flush
        StreamingBuilder builder NotEndOfStream -> runStreamingBuilder builder next
        StreamingBuilder builder (EndOfStream mdec) -> runStreamingBuilder builder (finished mdec)
  where
    finished :: Maybe CleanupStream -> NextWithTotal
    finished mdec = \total _buf _room -> do
        fromMaybe (return ()) mdec
        return $ Next total True Nothing

    flush :: NextWithTotal
    flush = \total _buf _room -> do
        return $ Next total True (Just $ next 0)

    -- Cancel streaming
    --
    -- The @_total@ number of bytes written refers to the @DATA@ frame currently
    -- under construction, but not yet sent (see discussion at 'DynaNext' and
    -- 'NextWithTotal'). Moreover, the documentation of 'outBodyCancel'
    -- explicitly states that such a partially constructed frame, if one exists,
    -- will be discarded on cancellation. We can therefore simply ignore
    -- @_total@ here.
    cancel :: Maybe SomeException -> NextWithTotal
    cancel mErr = \_total _buf _room -> pure $ CancelNext mErr

-- | Run 'Builder' until completion, then continue as specified
runStreamingBuilder :: Builder -> NextWithTotal -> NextWithTotal
runStreamingBuilder builder next = \total buf room -> do
    writeResult <- B.runBuilder builder buf room
    ranWriter writeResult total buf room
  where
    ranWriter :: (Int, B.Next) -> NextWithTotal
    ranWriter (len, signal) = \total buf room -> do
        let total' = total + len
        case signal of
            B.Done ->
                next total' (buf `plusPtr` len) (room - len)
            B.More minReq writer ->
                return $ Next total' False (Just $ goMore (Just minReq) writer 0)
            B.Chunk bs writer ->
                return $ Next total' False (Just $ goChunk bs writer 0)

    goMore :: Maybe Int -> B.BufferWriter -> NextWithTotal
    goMore mMinReq writer = \total buf room -> do
        let enoughRoom = maybe True (room >=) mMinReq
        if enoughRoom
            then do
                writeResult <- writer buf room
                ranWriter writeResult total buf room
            else do
                return $ Next total True (Just $ goMore mMinReq writer 0)

    goChunk :: ByteString -> B.BufferWriter -> NextWithTotal
    goChunk bs writer = \total buf room ->
        if BS.length bs <= room
            then do
                buf' <- copy buf bs
                let len = BS.length bs
                goMore Nothing writer (total + len) buf' (room - len)
            else do
                let (bs1, bs2) = BS.splitAt room bs
                void $ copy buf bs1
                return $ Next (total + room) False (Just $ goChunk bs2 writer 0)

----------------------------------------------------------------

fillBufFile :: PositionRead -> FileOffset -> ByteCount -> Sentinel -> DynaNext
fillBufFile pread start bytes sentinel buf room = do
    len <- pread start (mini room bytes) buf
    case sentinel of
        Refresher refresh -> refresh
        _ -> return ()
    let len' = fromIntegral len
    nextForFile len' pread (start + len) (bytes - len) sentinel

nextForFile
    :: BytesFilled -> PositionRead -> FileOffset -> ByteCount -> Sentinel -> IO Next
nextForFile 0 _ _ _ _ = return $ Next 0 True Nothing -- let's flush
nextForFile len _ _ 0 sentinel = do
    case sentinel of
        Closer close -> close
        _ -> return ()
    return $ Next len False Nothing
nextForFile len pread start bytes refresh =
    return $ Next len False $ Just $ fillBufFile pread start bytes refresh

{-# INLINE mini #-}
mini :: Int -> Int64 -> Int64
mini i n
    | fromIntegral i < n = fromIntegral i
    | otherwise = n
