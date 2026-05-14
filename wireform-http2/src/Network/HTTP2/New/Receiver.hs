{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Receiver loop: reads frames from the socket and dispatches them.
-- Correctly handles all RFC 9113 (HTTP/2) frame types and error cases.
module Network.HTTP2.New.Receiver
    ( frameReceiver
    ) where

import Control.Concurrent (forkIO)
import Data.Bits ((.|.))
import Data.List (foldl')
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (throwIO, handle, SomeException, fromException)
import Control.Monad (when, unless, forM_, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.IntMap.Strict as IntMap

import Network.HTTP.Semantics.Token
    ( tokenMethod, tokenStatus, tokenScheme, tokenPath
    , tokenConnection, tokenTE
    )
import Network.HTTP2.Frame

import Network.HTTP2.New.HPACK
import Network.HTTP2.New.Frame
import Network.HTTP2.New.Types

----------------------------------------------------------------

frameReceiver :: Config -> Context -> (Request -> IO Response) -> IO ()
frameReceiver cfg@Config{..} ctx@Context{..} handler =
    handle onError $
        receiverLoop cfg ctx handler
  where
    -- Send error frames as self-contained ByteStrings (no write buffer needed).
    onError e = case fromException e :: Maybe HTTP2Error of
        -- NoError means the peer sent GOAWAY and we already sent our response
        -- in the dispatch handler; skip the double-send.
        Just (ConnectionError NoError _) -> return ()
        Just (ConnectionError errCode _) -> do
            streams <- readIORef ctxStreams
            let lastSid = if IntMap.null streams then 0
                              else fst (IntMap.findMax streams)
            cfgSendAll $ encodeFrame (encodeInfo id 0)
                             (GoAwayFrame lastSid errCode "")
        Just (StreamError sid errCode _) ->
            cfgSendAll $ encodeFrame (encodeInfo id sid)
                             (RSTStreamFrame errCode)
        _ -> return ()

-- | @mPendingHdr@: when the previous HEADERS had no END_HEADERS flag, holds
-- @(streamId, flags, accumulated_hpack_bytes)@ while we wait for CONTINUATION.
receiverLoop :: Config -> Context -> (Request -> IO Response) -> IO ()
receiverLoop cfg@Config{..} ctx@Context{..} handler = go 0 Nothing
  where
    go !maxSid !mPendingHdr = do
        (ftype, hdr@FrameHeader{..}) <- readFrameHeader cfg
        -- Always read the full payload FIRST, then validate.
        -- If we throw before reading, unread bytes in the TCP buffer cause RST
        -- which arrives before our GOAWAY, failing conformance tests.
        -- Cap at 2×maxFrameSize to avoid huge allocations on malformed frames.
        let readLen = min payloadLength (maxFrameSize ctxMySettings * 2 + 1)
        payload <- cfgReadN readLen

        -- Frame size check: throw after reading to drain socket buffer.
        when (payloadLength > maxFrameSize ctxMySettings) $
            throwIO $ ConnectionError FrameSizeError "frame exceeds SETTINGS_MAX_FRAME_SIZE"

        -- RFC 9113 §6.10: while assembling a header block, ONLY CONTINUATION
        -- frames for the same stream are allowed.
        case mPendingHdr of
            Just (pendSid, _, _) ->
                when (ftype /= FrameContinuation || streamId /= pendSid) $
                    throwIO $ ConnectionError ProtocolError
                        "expected CONTINUATION for same stream"
            Nothing ->
                when (ftype == FrameContinuation) $
                    throwIO $ ConnectionError ProtocolError "unexpected CONTINUATION"

        validateFrameHeader ftype hdr

        -- Monotonically increasing stream IDs for HEADERS frames.
        when (ftype == FrameHeaders && streamId /= 0) $
            when (even streamId || streamId <= maxSid) $
                throwIO $ ConnectionError ProtocolError
                    "stream identifier must not decrease"

        let newMaxSid = if ftype == FrameHeaders && streamId /= 0
                            then streamId
                            else maxSid

        case decodeFramePayload ftype hdr payload of
            Left e -> throwIO $ ConnectionError ProtocolError
                          (BS.pack (map (fromIntegral . fromEnum) (show e)))
            Right framePayload ->
                case mPendingHdr of
                    -- Assembling a header block: append CONTINUATION bytes.
                    Just (pendSid, initFlags, acc) -> do
                        ContinuationFrame chunk <- pure framePayload
                        let acc' = acc <> chunk
                        if testEndHeader flags
                            then do
                                -- Header block complete.
                                -- Combine: END_STREAM from the original HEADERS
                                -- (initFlags) + END_HEADERS from this CONTINUATION.
                                let combinedFlags = initFlags .|. flags
                                dispatch cfg ctx handler
                                    (hdr { streamId = pendSid, flags = combinedFlags })
                                    (HeadersFrame Nothing acc')
                                go newMaxSid Nothing
                            else go newMaxSid (Just (pendSid, initFlags, acc'))

                    -- Normal frame: dispatch as-is.
                    Nothing ->
                        case framePayload of
                            HeadersFrame _ chunk | not (testEndHeader flags) -> do
                                -- HEADERS without END_HEADERS: start accumulating.
                                go newMaxSid (Just (streamId, flags, chunk))
                            _ -> do
                                dispatch cfg ctx handler hdr framePayload
                                go newMaxSid Nothing

dispatch :: Config -> Context -> (Request -> IO Response) -> FrameHeader -> FramePayload -> IO ()
dispatch cfg@Config{..} ctx@Context{..} handler FrameHeader{..} = \case

    -- HEADERS: new request stream or trailing headers on an existing stream.
    HeadersFrame mPrio hpackBlock -> do
        when (streamId == 0) $
            throwIO $ ConnectionError ProtocolError "HEADERS on stream 0"
        streams <- readIORef ctxStreams
        case IntMap.lookup streamId streams of
            -- Trailing headers on an existing open stream (RFC 9113 §8.1).
            -- Don't re-create the stream; just update its close state.
            Just strm -> do
                when (testEndStream flags) $ do
                    writeIORef (streamRxDone strm) True
                    atomicModifyIORef' ctxClosedStreams $ \m ->
                        (IntMap.insert streamId () m, ())
                    -- Signal body EOF so the handler can finish.
                    case streamBody strm of
                        UnaryBody mvar -> void $ tryPutMVar mvar (Right "")
                        _              -> return ()

            -- New stream: validate, create, and dispatch handler.
            Nothing -> do
                -- Check concurrent stream limit.
                case maxConcurrentStreams ctxMySettings of
                    Just limit ->
                        when (IntMap.size streams >= limit) $
                            throwIO $ StreamError streamId RefusedStream "exceeds max concurrent streams"
                    Nothing -> return ()
                -- Check HEADERS frame self-dependency (RFC 9113 §5.3.1).
                case mPrio of
                    Just Priority{streamDependency = dep} ->
                        when (dep == streamId) $
                            throwIO $ StreamError streamId ProtocolError "HEADERS depends on itself"
                    Nothing -> return ()
                tbl <- decodeHeaders ctxHpackDec hpackBlock
                -- Validate request pseudo-headers (RFC 9113 §8.3.1).
                validateRequestHeaders tbl streamId
                -- TX window = our buffer capacity (not peer's IWS to avoid deadlock
                -- when the peer announces SETTINGS_INITIAL_WINDOW_SIZE = 0).
                strm <- newStream streamId (initialWindowSize ctxMySettings)
                atomicModifyIORef' ctxStreams $ \m -> (IntMap.insert streamId strm m, ())
                when (testEndStream flags) $ do
                    writeIORef (streamRxDone strm) True
                    atomicModifyIORef' ctxClosedStreams $ \m -> (IntMap.insert streamId () m, ())
                when (testEndHeader flags) $ do
                    let req = Request streamId tbl (mkBodyReader strm)
                    void $ forkHandlerThread ctx handler strm req

    -- DATA: request body chunk
    DataFrame body -> do
        when (streamId == 0) $
            throwIO $ ConnectionError ProtocolError "DATA on stream 0"
        when (payloadLength > 0) $
            atomicModifyIORef' ctxConnRxWin (\n -> (n + payloadLength, ()))
        streams <- readIORef ctxStreams
        case IntMap.lookup streamId streams of
            Just strm -> deliverData ctx strm body (testEndStream flags)
            Nothing -> do
                closed <- readIORef ctxClosedStreams
                if streamId `IntMap.member` closed
                    then throwIO $ StreamError streamId StreamClosed "DATA on closed stream"
                    else throwIO $ ConnectionError ProtocolError "DATA on idle stream"

    -- SETTINGS
    SettingsFrame pairs -> do
        when (streamId /= 0) $
            throwIO $ ConnectionError ProtocolError "SETTINGS on non-zero stream"
        if testAck flags
            then do
                -- ACK for our own SETTINGS. Per RFC 9113 §6.5: if ACK has
                -- non-zero length, it's a FRAME_SIZE_ERROR.
                when (payloadLength /= 0) $
                    throwIO $ ConnectionError FrameSizeError "SETTINGS ACK with payload"
            else do
                -- Validate settings values before applying.
                validateSettings pairs
                -- Capture old initial window before applying.
                oldPeer <- readIORef ctxPeerSettings
                let oldWin = initialWindowSize oldPeer
                -- Apply peer's settings (last value wins per RFC 9113 §6.5).
                atomicModifyIORef' ctxPeerSettings $ \old ->
                    (applySettings old pairs, ())
                newPeer <- readIORef ctxPeerSettings
                let newWin = initialWindowSize newPeer
                -- RFC 9113 §6.9.2: propagate SETTINGS_INITIAL_WINDOW_SIZE
                -- change to all existing streams by the delta.
                when (oldWin /= newWin) $ do
                    let delta = newWin - oldWin
                    streams <- readIORef ctxStreams
                    forM_ (IntMap.elems streams) $ \strm ->
                        adjustStreamTxWindow (streamTxWin strm) delta
                -- Ack.
                atomically $ writeTQueue ctxControlQ (OControl CSettingsAck)

    -- PING (stream/length already validated in validateFrameHeader)
    PingFrame payload ->
        unless (testAck flags) $
            atomically $ writeTQueue ctxControlQ (OControl (CPing payload))

    -- WINDOW_UPDATE (connection or stream level)
    WindowUpdateFrame increment -> do
        when (increment == 0) $
            if streamId == 0
                then throwIO $ ConnectionError FlowControlError "zero WINDOW_UPDATE on connection"
                else throwIO $ StreamError streamId FlowControlError "zero WINDOW_UPDATE on stream"
        -- WINDOW_UPDATE on an idle stream is a connection error.
        when (streamId /= 0) $ do
            streams <- readIORef ctxStreams
            when (streamId `IntMap.notMember` streams) $
                throwIO $ ConnectionError ProtocolError "WINDOW_UPDATE on idle stream"
        -- Check window overflow (connection or stream).
        if streamId == 0
            then do
                avail <- readTVarIO (txAvailable ctxConnTxWin)
                when (fromIntegral avail + fromIntegral increment > (2147483647 :: Integer)) $
                    throwIO $ ConnectionError FlowControlError "connection window overflow"
            else do
                streams <- readIORef ctxStreams
                case IntMap.lookup streamId streams of
                    Just strm -> do
                        avail <- readTVarIO (txAvailable (streamTxWin strm))
                        when (fromIntegral avail + fromIntegral increment > (2147483647 :: Integer)) $
                            throwIO $ StreamError streamId FlowControlError "stream window overflow"
                    Nothing -> return ()
        if streamId == 0
            then addTxWindow ctxConnTxWin increment
            else do
                streams <- readIORef ctxStreams
                case IntMap.lookup streamId streams of
                    Just strm -> addTxWindow (streamTxWin strm) increment
                    Nothing   -> return ()

    -- RST_STREAM
    RSTStreamFrame _ -> do
        when (streamId == 0) $
            throwIO $ ConnectionError ProtocolError "RST_STREAM on stream 0"
        streams <- readIORef ctxStreams
        when (streamId `IntMap.notMember` streams) $ do
            closed <- readIORef ctxClosedStreams
            unless (streamId `IntMap.member` closed) $
                throwIO $ ConnectionError ProtocolError "RST_STREAM on idle stream"
        atomicModifyIORef' ctxStreams $ \m -> (IntMap.delete streamId m, ())
        atomicModifyIORef' ctxClosedStreams $ \m -> (IntMap.insert streamId () m, ())

    -- PRIORITY (deprecated in HTTP/2.1; validate but otherwise ignore)
    PriorityFrame Priority{streamDependency} -> do
        -- Self-dependency is a stream error (RFC 9113 §5.3.1).
        when (streamDependency == streamId) $
            throwIO $ StreamError streamId ProtocolError "PRIORITY depends on itself"

    -- GOAWAY: peer wants to close the connection.
    -- RFC 9113 §6.8: respond with our own GOAWAY then close.
    -- We use 'NoError' as the sentinel so 'onError' skips sending a second GOAWAY.
    GoAwayFrame _peerLastSid _errCode _ -> do
        streams <- readIORef ctxStreams
        let ourLast = if IntMap.null streams then 0
                          else fst (IntMap.findMax streams)
        -- Send GOAWAY without touching ctxControlQ — the sender might be idle
        -- or already finished, so enqueuing would leave GOAWAY unsent.
        cfgSendAll $ encodeFrame (encodeInfo id 0) (GoAwayFrame ourLast NoError "")
        -- Throwing terminates the receiver loop, causing frameReceiver to return,
        -- which causes runWith to return, which closes the TCP connection.
        throwIO $ ConnectionError NoError "peer sent GOAWAY; closing"

    -- CONTINUATION: we handle this by not allowing it here
    -- (it must immediately follow a HEADERS frame with no END_HEADERS)
    ContinuationFrame _ ->
        throwIO $ ConnectionError ProtocolError "unexpected CONTINUATION"

    -- PushPromise: client should never receive this in request paths
    PushPromiseFrame _ _ ->
        throwIO $ ConnectionError ProtocolError "unexpected PUSH_PROMISE"

    UnknownFrame _ _ ->
        return ()  -- RFC 9113 §4.1: ignore unknown frame types

----------------------------------------------------------------
-- Body delivery

deliverData :: Context -> Stream -> ByteString -> Bool -> IO ()
deliverData ctx@Context{ctxClosedStreams} strm@Stream{streamId} body isLast = do
    -- Check if already half-closed-remote (DATA after END_STREAM is an error).
    rxDone <- readIORef (streamRxDone strm)
    when rxDone $
        throwIO $ StreamError streamId StreamClosed "DATA after END_STREAM"
    let chan = streamBody strm
    case chan of
        UnaryBody mvar ->
            when isLast $ putMVar mvar (Right body)
        StreamBody mvar ->
            putMVar mvar (Right (body, isLast))
    when isLast $ do
        writeIORef (streamRxDone strm) True
        atomicModifyIORef' ctxClosedStreams $ \m ->
            (IntMap.insert streamId () m, ())

mkBodyReader :: Stream -> IO (ByteString, Bool)
mkBodyReader Stream{streamBody} = case streamBody of
    UnaryBody mvar -> do
        r <- takeMVar mvar
        case r of
            Right bs -> return (bs, True)
            Left e   -> throwIO e
    StreamBody mvar -> do
        r <- takeMVar mvar
        case r of
            Right chunk -> return chunk
            Left e      -> throwIO e

----------------------------------------------------------------
-- Handler dispatch thread

forkHandlerThread :: Context -> (Request -> IO Response) -> Stream -> Request -> IO ()
forkHandlerThread ctx@Context{..} handler strm req = do
    _ <- forkIO $ handle (\(e :: SomeException) -> handleError ctx strm e) $ do
        rsp <- handler req
        enqueueResponse ctx strm rsp
    return ()

enqueueResponse :: Context -> Stream -> Response -> IO ()
enqueueResponse Context{ctxOutputQ} strm rsp = case rsp of
    ResponseUnary{..} ->
        atomically $ writeTQueue ctxOutputQ $
            OUnary strm
                (map (\(k,v) -> (k, v)) rspHeaders)
                rspBody
                rspTrailers
                (return ())
    ResponseStreaming{..} ->
        atomically $ writeTQueue ctxOutputQ $
            OStreaming strm
                (map (\(k,v) -> (k, v)) rspHeaders)
                rspProducer
                rspTrailers
                (return ())

handleError :: Context -> Stream -> SomeException -> IO ()
handleError Context{ctxControlQ} Stream{streamId = sid} _ =
    atomically $ writeTQueue ctxControlQ $
        OControl (CRstStream sid InternalError)

----------------------------------------------------------------
-- Settings helpers

-- | Apply settings in order; last value for a given key wins (RFC 9113 §6.5).
-- Use foldl' so the last entry in the list takes effect (not foldr which
-- gives the first entry priority).
applySettings :: Settings -> SettingsList -> Settings
applySettings old = foldl' apply old
  where
    apply s (SettingsTokenHeaderTableSize, n) = s { headerTableSize       = n }
    apply s (SettingsEnablePush,           n) = s { enablePush            = n /= 0 }
    apply s (SettingsMaxConcurrentStreams, n)  = s { maxConcurrentStreams  = Just n }
    apply s (SettingsInitialWindowSize,   n)  = s { initialWindowSize     = n }
    apply s (SettingsMaxFrameSize,        n)  = s { maxFrameSize          = n }
    apply s (SettingsMaxHeaderListSize,   n)  = s { maxHeaderListSize     = Just n }
    apply s _                                 = s

-- | Frame-header level validation — size and stream-ID rules that apply
-- before the payload is decoded.
validateFrameHeader :: FrameType -> FrameHeader -> IO ()
validateFrameHeader ftype FrameHeader{..} = case ftype of
    FrameSettings -> do
        when (streamId /= 0) $
            throwIO $ ConnectionError ProtocolError "SETTINGS on non-zero stream"
        when (payloadLength `rem` 6 /= 0) $
            throwIO $ ConnectionError FrameSizeError "SETTINGS length not multiple of 6"
        when (testAck flags && payloadLength /= 0) $
            throwIO $ ConnectionError FrameSizeError "SETTINGS ACK with payload"
    FramePriority -> do
        when (streamId == 0) $
            throwIO $ ConnectionError ProtocolError "PRIORITY on stream 0"
        when (payloadLength /= 5) $
            throwIO $ StreamError streamId FrameSizeError "PRIORITY length not 5"
    FrameRSTStream -> do
        when (streamId == 0) $
            throwIO $ ConnectionError ProtocolError "RST_STREAM on stream 0"
        when (payloadLength /= 4) $
            throwIO $ ConnectionError FrameSizeError "RST_STREAM length not 4"
    FrameGoAway ->
        when (streamId /= 0) $
            throwIO $ ConnectionError ProtocolError "GOAWAY on non-zero stream"
    FrameWindowUpdate ->
        when (payloadLength /= 4) $
            if streamId == 0
                then throwIO $ ConnectionError FrameSizeError "WINDOW_UPDATE length not 4"
                else throwIO $ StreamError streamId FrameSizeError "WINDOW_UPDATE length not 4"
    FramePing -> do
        when (streamId /= 0) $
            throwIO $ ConnectionError ProtocolError "PING on non-zero stream"
        when (payloadLength /= 8) $
            throwIO $ ConnectionError FrameSizeError "PING length not 8"
    FrameContinuation ->
        when (streamId == 0) $
            throwIO $ ConnectionError ProtocolError "CONTINUATION on stream 0"
    _ -> return ()

-- | Validate request pseudo-headers per RFC 9113 §8.3.1.
validateRequestHeaders :: TokenHeaderTable -> StreamId -> IO ()
validateRequestHeaders (hdrs, vt) sid = do
    -- Response pseudo-headers must not appear in requests.
    case getFieldValue tokenStatus vt of
        Just _ -> throwIO $ StreamError sid ProtocolError "response :status in request"
        Nothing -> return ()
    -- Must have :method.
    case getFieldValue tokenMethod vt of
        Nothing -> throwIO $ StreamError sid ProtocolError "missing :method"
        Just _  -> return ()
    -- CONNECT method: must not have :scheme/:path; others must have both.
    let isConnect = getFieldValue tokenMethod vt == Just "CONNECT"
    if isConnect
        then return ()  -- relaxed requirements for CONNECT
        else do
            case getFieldValue tokenScheme vt of
                Nothing -> throwIO $ StreamError sid ProtocolError "missing :scheme"
                Just _  -> return ()
            case getFieldValue tokenPath vt of
                Nothing -> throwIO $ StreamError sid ProtocolError "missing :path"
                Just "" -> throwIO $ StreamError sid ProtocolError "empty :path"
                Just _  -> return ()
    -- Connection-specific headers (e.g. Connection:) are forbidden.
    case getFieldValue tokenConnection vt of
        Just _ -> throwIO $ StreamError sid ProtocolError "connection-specific header"
        Nothing -> return ()
    -- TE header must be exactly "trailers" if present.
    case getFieldValue tokenTE vt of
        Just v | v /= "trailers" ->
            throwIO $ StreamError sid ProtocolError "invalid TE header value"
        _ -> return ()

-- | Validate SETTINGS values per RFC 9113 §6.5.2.
validateSettings :: SettingsList -> IO ()
validateSettings = mapM_ check
  where
    -- SETTINGS_ENABLE_PUSH must be 0 or 1.
    check (SettingsEnablePush, n) =
        when (n /= 0 && n /= 1) $
            throwIO $ ConnectionError ProtocolError "SETTINGS_ENABLE_PUSH invalid"
    -- SETTINGS_INITIAL_WINDOW_SIZE must be ≤ 2^31-1 (sign bit means "negative").
    check (SettingsInitialWindowSize, n) =
        when (n > 2147483647 || n < 0) $
            throwIO $ ConnectionError FlowControlError "SETTINGS_INITIAL_WINDOW_SIZE overflow"
    -- SETTINGS_MAX_FRAME_SIZE must be between 16384 and 2^24-1.
    check (SettingsMaxFrameSize, n) = do
        when (n < 16384)    $ throwIO $ ConnectionError ProtocolError "SETTINGS_MAX_FRAME_SIZE below 16384"
        when (n > 16777215) $ throwIO $ ConnectionError ProtocolError "SETTINGS_MAX_FRAME_SIZE above maximum"
    check _ = return ()
