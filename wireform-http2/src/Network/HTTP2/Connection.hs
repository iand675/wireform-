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
  , acknowledgePeerSettings
  , sendHeaderBlock
  , sendEncodedHeaders
  , sendContinueIfExpected
  , ContinueGate (..)
  , newContinueGate
  , awaitContinue
  , noteContinueResponse
  , stopContinue
  , finishContinueResponse
  , sendNewStreamHeaders
  , sendPeerData
  , encodeAndSendHeaderBlock
  , headerBlockFrames
  , validateTrailerBlock
  , RequestHead (..)
  , parseRequestHead
  , validateRequestHead
  , validateResponseFields
  , parseResponseStatus
  , readContentLength
  , ContentLengthRule (..)
  , readResponseContentLength
  , contentLengthAsInt
  , remainingContentLength
  , stripFramePadding
  , returnFrameCredit
  , recvFrame
  , recvExactFrame
  , recvHeaderBlock
  , recvHeaderBlockWith
  , recvFieldBlockWith
  , copyReceivedBytes
  , abortConnection
  , recvFrameRaw
  , closeConnection
  , releaseConnection
  , connectionSettings
    -- * Re-exports
  , module Network.HTTP2.Connection.Settings
  , module Network.HTTP2.Connection.FlowControl
  , module Network.HTTP2.Connection.StreamTable
  , module Network.HTTP2.Transport
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (Exception, catch, SomeException, throwIO)
import Control.Monad (unless, when)
import Data.Bits (testBit, (.|.), (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.IORef
import Data.Word
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (plusPtr)
import Network.Socket (Socket)
import Network.HTTP.Grammar.Authority (hostAndPortBytes, uriSchemeBytes)
import Network.IPAddress (IPAddress (..), ipLiteralParser)
import Network.HTTP.Grammar.Parser (boundedDecimalWord64, withShortDecimal)
import Network.HTTP.Headers.Expect (hasContinueExpectation)
import System.Timeout (timeout)

import qualified Wireform.Transport as WT
import Wireform.Transport.Send (SendTransport, sendByteString, sendByteStringMany, sendClose)
import Wireform.Network.Transport.Receive (buildReceiveTransport)
import Wireform.Ring.Internal (destroyMagicRing, newMagicRing)
import Wireform.Network.Transport.Send (newSendBufTransport)
import qualified Wireform.Transport.Config as WC
import Wireform.Transport.Config (defaultTransportConfig)
import Wireform.Parser (empty, eof)
import Wireform.Parser.Driver (parseByteString)

import Network.HTTP2.Connection.FlowControl
import Network.HTTP2.Connection.Settings
import Network.HTTP2.Connection.StreamTable
import Network.HTTP2.Frame
import qualified Network.HTTP2.Frame.StreamingReader as SR
import Network.HTTP2.HPACK
import Network.HTTP2.Transport
import Network.HTTP2.Types

-- | Check a decoded trailer block against RFC 9113 §8.1 and §8.2.
-- Decode HPACK first even when the frame flags already indicate an error.
validateTrailerBlock :: FrameFlags -> [Header] -> Maybe ByteString
validateTrailerBlock flags
  | not (testFlag flags flagEndStream) = const (Just "trailers without END_STREAM")
  | otherwise = validateFieldLines TrailerFields

-- | Validate the fields after a response's leading :status.
validateResponseFields :: [Header] -> Maybe ByteString
validateResponseFields = validateFieldLines ResponseFields

-- | Decode the required three-digit HTTP/2 response status.
parseResponseStatus :: ByteString -> Maybe Int
parseResponseStatus value
  | BS.length value == 3
  , Just (status, suffix) <- BS8.readInt value
  , BS.null suffix
  , status >= 100 && status <= 599 && status /= 101 = Just status
  | otherwise = Nothing

data FieldSection = RequestFields | ResponseFields | TrailerFields

validateFieldLines :: FieldSection -> [Header] -> Maybe ByteString
validateFieldLines section = checkFields
  where
    checkFields [] = Nothing
    checkFields ((name, value) : rest) = case validateFieldLine section name value of
      Nothing -> checkFields rest
      err -> err
{-# INLINE validateFieldLines #-}

validateFieldLine :: FieldSection -> ByteString -> ByteString -> Maybe ByteString
validateFieldLine section name value
  | BS.isPrefixOf ":" name = Just $ case section of
          TrailerFields -> "pseudo-header in trailers"
          _ -> "pseudo-header after the initial pseudo-headers"
  | BS.null name || BS.any (not . lowerTokenByte) name = Just $ case section of
          TrailerFields -> "invalid trailer field name"
          _ -> "invalid field name"
  | not (validFieldValue value) = Just $ case section of
          TrailerFields -> "invalid trailer field value"
          _ -> "invalid field value"
  | connectionSpecific name = Just $ case section of
          TrailerFields -> "connection-specific trailer field"
          _ -> "connection-specific header field"
  | name == "te" = case section of
          RequestFields
            | asciiCaseEqual value "trailers" -> Nothing
            | otherwise -> Just "TE request field must be trailers"
          ResponseFields -> Just "TE in a response"
          TrailerFields -> Just "connection-specific trailer field"
  | otherwise = Nothing
{-# INLINE validateFieldLine #-}

connectionSpecific :: ByteString -> Bool
connectionSpecific name =
  name == "connection" || name == "keep-alive" || name == "proxy-connection"
    || name == "transfer-encoding" || name == "upgrade"
{-# INLINE connectionSpecific #-}

validFieldValue :: ByteString -> Bool
validFieldValue value =
  BS.null value
    || (not (isWhitespace (BS.head value) || isWhitespace (BS.last value))
        && not (BS.any invalidValueByte value))
  where
    isWhitespace w = w == 0x20 || w == 0x09
    invalidValueByte w = (w < 0x20 && w /= 0x09) || w == 0x7F
{-# INLINE validFieldValue #-}

-- One bit per ASCII code for digits, lowercase letters and token punctuation.
lowerTokenByte :: Word8 -> Bool
lowerTokenByte w
  | w < 64 = testBit (0x03ff6cfa00000000 :: Word64) (fromIntegral w)
  | w < 128 = testBit (0x57ffffffc0000000 :: Word64) (fromIntegral w - 64)
  | otherwise = False
{-# INLINE lowerTokenByte #-}

asciiLower :: Word8 -> Word8
asciiLower w
  | w >= 0x41 && w <= 0x5A = w + 0x20
  | otherwise = w
{-# INLINE asciiLower #-}

-- | Validated request control data. The authority retains the chosen field's
-- wire spelling, with the empty value representing absent authority.
data RequestHead = RequestHead
  { requestHeadMethod :: !ByteString
  , requestHeadScheme :: !ByteString
  , requestHeadPath :: !ByteString
  , requestHeadAuthority :: !ByteString
  , requestHeadFields :: ![Header]
  }

validateRequestHead :: [Header] -> Maybe ByteString
validateRequestHead = either Just (const Nothing) . parseRequestHead

-- | Validate and resolve an HTTP/2 request head (RFC 9113 §8.3.1).
parseRequestHead :: [Header] -> Either ByteString RequestHead
parseRequestHead = pseudo 0 "" "" "" ""
  where
    methodBit, schemeBit, pathBit, authorityBit :: Word8
    methodBit = 1
    schemeBit = 2
    pathBit = 4
    authorityBit = 8
    pseudo seen method scheme path authority ((name, value) : rest)
      | BS.isPrefixOf ":" name = case name of
          ":method"
            | BS.null value || BS.any (not . lowerTokenByte . asciiLower) value ->
                Left "invalid :method"
            | seen .&. methodBit /= 0 -> duplicate
            | otherwise -> pseudo (seen .|. methodBit) value scheme path authority rest
          ":scheme"
            | seen .&. schemeBit /= 0 -> duplicate
            | Just normalized <- parseScheme value ->
                pseudo (seen .|. schemeBit) method normalized path authority rest
            | otherwise -> Left "invalid :scheme"
          ":path"
            | BS.null value -> Left "empty :path"
            | not (validFieldValue value) -> Left "invalid :path"
            | seen .&. pathBit /= 0 -> duplicate
            | otherwise -> pseudo (seen .|. pathBit) method scheme value authority rest
          ":authority"
            | BS.null value -> Left "empty :authority"
            | seen .&. authorityBit /= 0 -> duplicate
            | otherwise -> pseudo (seen .|. authorityBit) method scheme path value rest
          _ -> Left "unknown request pseudo-header"
    pseudo seen method scheme path authority fields
      | seen .&. methodBit == 0 = Left "missing :method"
      | method == "CONNECT", seen .&. (schemeBit .|. pathBit) /= 0 =
          Left "CONNECT must omit :scheme and :path"
      | method /= "CONNECT", seen .&. schemeBit == 0 = Left "missing :scheme"
      | method /= "CONNECT", seen .&. pathBit == 0 = Left "missing :path"
      | otherwise = do
          hostField <- requestHost Nothing fields
          selected <- resolveAuthority method scheme authority hostField
          pure (RequestHead method scheme path selected fields)

    duplicate = Left "duplicate request pseudo-header"

    parseScheme value
      | asciiCaseEqual value "http" = Just "http"
      | asciiCaseEqual value "https" = Just "https"
      | otherwise = case parseByteString (uriSchemeBytes <* eof) value of
          Right _ -> Just value
          Left _ -> Nothing

    requestHost hostField [] = Right hostField
    requestHost hostField ((name, value) : rest)
      | name == "host" = case hostField of
          Nothing -> requestHost (Just value) rest
          Just _ -> Left "duplicate Host"
      | otherwise = case validateFieldLine RequestFields name value of
          Just reason -> Left reason
          Nothing -> requestHost hostField rest

data AuthorityParts = AuthorityParts !ByteString !(Maybe Word16)

resolveAuthority :: ByteString -> ByteString -> ByteString -> Maybe ByteString -> Either ByteString ByteString
resolveAuthority method scheme authority hostField
  | BS.null authority =
      if method == "CONNECT"
        then Left "CONNECT requires a host and port in :authority"
        else case hostField of
          Nothing -> Right ""
          Just host
            | BS.null host -> Right ""
            | otherwise -> host <$ parseAuthority "invalid Host" host
  | otherwise = do
      parts@(AuthorityParts _ port) <- parseAuthority "invalid :authority" authority
      if method == "CONNECT" && port == Nothing
        then Left "CONNECT requires a host and port in :authority"
        else case hostField of
          Nothing -> Right authority
          Just host
            | host == authority -> Right authority
            | BS.null host -> Left "Host differs from :authority"
            | otherwise -> do
                other <- parseAuthority "invalid Host" host
                if authorityEqual scheme parts other
                  then Right authority
                  else Left "Host differs from :authority"

parseAuthority :: ByteString -> ByteString -> Either ByteString AuthorityParts
parseAuthority reason wire
  | BS.null wire = Left reason
  | otherwise = scan 0
  where
    size = BS.length wire
    scan i
      | i == size = Right (AuthorityParts wire Nothing)
      | plainHostByte byte = scan (i + 1)
      | i > 0 && byte == 0x3A =
          let host = BS.take i wire
              port = BS.drop (i + 1) wire
          in if BS.null port
              then Right (AuthorityParts host Nothing)
              else withShortDecimal port (smallPort host) full
      | otherwise = full
      where
        byte = BSU.unsafeIndex wire i
    plainHostByte w = asciiAlnum w || w == 0x2D || w == 0x2E
    smallPort host n
      | n <= 65_535 = Right (AuthorityParts host (Just (fromIntegral n)))
      | otherwise = Left reason
    full = case parseByteString (hostAndPortBytes AuthorityParts <* eof) wire of
      Left _ -> Left reason
      Right parts -> Right parts

authorityEqual :: ByteString -> AuthorityParts -> AuthorityParts -> Bool
authorityEqual scheme (AuthorityParts first firstPort) (AuthorityParts second secondPort) =
  effectivePort firstPort == effectivePort secondPort && hostEqual first second
  where
    effectivePort (Just port) = Just port
    effectivePort Nothing
      | asciiCaseEqual scheme "https" = Just 443
      | asciiCaseEqual scheme "http" = Just 80
      | otherwise = Nothing

hostEqual :: ByteString -> ByteString -> Bool
hostEqual first second
  | asciiCaseEqual first second = True
  | BS.isPrefixOf "[" first && BS.isPrefixOf "[" second =
      case (parseByteString literal first, parseByteString literal second) of
        (Right (IPv6Address a), Right (IPv6Address b)) -> a == b
        _ -> False
  | BS.elem 0x25 first || BS.elem 0x25 second =
      asciiCaseEqual (unescapeHost first) (unescapeHost second)
  | otherwise = False
  where
    literal = ipLiteralParser <* eof

-- The authority parser has already validated all percent escapes.
-- Decode unreserved octets and leave reserved escapes for case-folded comparison.
unescapeHost :: ByteString -> ByteString
unescapeHost wire = fst (BS.unfoldrN size next 0)
  where
    size = BS.length wire
    next i
      | i >= size = Nothing
      | byte == 0x25 && i + 2 < size
      , let decoded = hex (BSU.unsafeIndex wire (i + 1)) * 16 + hex (BSU.unsafeIndex wire (i + 2))
      , asciiAlnum decoded || decoded == 0x2D || decoded == 0x2E || decoded == 0x5F || decoded == 0x7E =
          Just (decoded, i + 3)
      | otherwise = Just (byte, i + 1)
      where
        byte = BSU.unsafeIndex wire i
    hex w
      | w <= 0x39 = w - 0x30
      | otherwise = asciiLower w - 0x61 + 10

asciiAlnum :: Word8 -> Bool
asciiAlnum w =
  (w >= 0x30 && w <= 0x39) || (lower >= 0x61 && lower <= 0x7A)
  where
    lower = asciiLower w
{-# INLINE asciiAlnum #-}

asciiCaseEqual :: ByteString -> ByteString -> Bool
asciiCaseEqual first second =
  first == second || (size == BS.length second && go 0)
  where
    size = BS.length first
    go i =
      i == size || (asciiLower (BSU.unsafeIndex first i) == asciiLower (BSU.unsafeIndex second i) && go (i + 1))

-- | Read an optional Content-Length, accepting repeated equal values.
-- Invalid syntax and values beyond the byte counter's range are errors.
readContentLength :: [Header] -> Either ByteString (Maybe Word64)
readContentLength = fields Nothing
  where
    fields known [] = Right known
    fields known ((name, value) : rest)
      | name /= "content-length" = fields known rest
      | otherwise = case decimal value of
          Just n -> agree known n >>= \next -> fields next rest
          Nothing
            | BS.elem 0x2C value
            , not (ows (BS8.head value) || ows (BS8.last value)) -> do
                next <- members known (BS8.split ',' value)
                fields next rest
            | otherwise -> Left "invalid content-length"

    members known [] = Right known
    members known (value : rest) = case decimal (BS8.dropWhile ows (BS8.dropWhileEnd ows value)) of
      Just n -> agree known n >>= \next -> members next rest
      Nothing -> Left "invalid content-length"

    agree Nothing n = Right (Just n)
    agree known@(Just expected) n
      | expected == n = Right known
      | otherwise = Left "conflicting content-length values"

    ows c = c == ' ' || c == '\t'
    decimal value = withShortDecimal value (Just . fromIntegral) $
      case parseByteString (boundedDecimalWord64 empty <* eof) value of
        Right n -> Just n
        Left _ -> Nothing

data ContentLengthRule
  = ContentUnbounded
  | ContentForbidden
  | ContentExpected {-# UNPACK #-} !Word64

-- | Interpret Content-Length with the request method and response status.
-- HEAD and 304 can describe a representation while carrying no content.
readResponseContentLength :: ByteString -> Int -> [Header] -> Either ByteString ContentLengthRule
readResponseContentLength method status fields
  | method == "CONNECT", status >= 200, status < 300 = Right ContentUnbounded
  | otherwise = readContentLength fields >>= classify
  where
    classify declared
      | status < 200 || status == 204 = case declared of
          Nothing -> Right ContentForbidden
          Just _ -> Left "content-length prohibited for this status"
      | status == 205 = case declared of
          Just n | n /= 0 -> Left "nonzero content-length on a 205 response"
          _ -> Right (ContentExpected 0)
      | method == "HEAD" || status == 304 = Right ContentForbidden
      | otherwise = Right (maybe ContentUnbounded ContentExpected declared)

-- | Project a validated wire length into the engine's Int-sized API.
contentLengthAsInt :: Word64 -> Maybe Int
contentLengthAsInt n
  | n <= fromIntegral (maxBound :: Int) = Just (fromIntegral n)
  | otherwise = Nothing
{-# INLINE contentLengthAsInt #-}

-- | Subtract content bytes, checking for overrun and premature END_STREAM.
remainingContentLength :: Word64 -> Int -> Bool -> Maybe Word64
remainingContentLength remaining bytes endStream
  | bytes < 0 || count > remaining || (endStream && count /= remaining) = Nothing
  | otherwise = Just (remaining - count)
  where
    count = fromIntegral bytes
{-# INLINE remainingContentLength #-}

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
    -- ^ Magic-ring transport plumbed onto @tRecvBuf connTransport@,
    -- and the sole receive side for the recv loop. Frames decoded
    -- from it are zero-copy slices of the ring, so the ring is
    -- unmapped only by 'releaseConnection', never by
    -- 'closeConnection' (which other threads may call while the
    -- recv loop is mid-frame).
  , connReleaseRecvRing :: !(IO ())
    -- ^ Unmaps the receive ring; runs at most once.
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
  recvRing <- newMagicRing (WC.ringSizeHint ringCfg)
  ringT <- buildReceiveTransport recvRing (tRecvBuf transport)
  recvRingLive <- newIORef True
  let releaseRecvRing = do
        live <- atomicModifyIORef' recvRingLive (\l -> (False, l))
        when live (destroyMagicRing recvRing)
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
    , connReleaseRecvRing = releaseRecvRing
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
-- resulting block as HEADERS and any required CONTINUATION frames, holding the send lock
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
--
-- The encode and the send also run masked: 'withMVar' runs its body
-- unmasked, so an async exception (a handler cancelled because its peer
-- reset the stream) could otherwise land after the encode has mutated
-- the table but before the frame is in the send ring, desyncing the
-- peer's decoder for every later block on the connection. Neither step
-- can be interrupted part-way: encoding does not block, and once the
-- frame is published to the ring it is committed (an interrupted drain
-- resumes from the ring's tail on the next send).
sendEncodedHeaders
  :: Connection
  -> StreamId
  -> Bool                          -- ^ set END_STREAM on the HEADERS frame
  -> [(ByteString, ByteString)]    -- ^ raw header list (incl. pseudo-headers)
  -> IO ()
sendEncodedHeaders conn sid endStream headers =
  withMVarMasked (connSendLock conn) $ \_ -> do
    sendPeerHeaderBlockUnlocked conn sid endStream headers

-- | Acknowledge a 100-continue expectation on an open request with content.
-- Send an informational head before the application can wait for DATA.
sendContinueIfExpected :: Connection -> StreamId -> Maybe Word64 -> RequestHead -> IO ()
sendContinueIfExpected conn sid declared requestHead
  | declared == Just 0 = pure ()
  | requestHeadMethod requestHead == "CONNECT" = pure ()
  | expectsContinue (requestHeadFields requestHead) =
      sendEncodedHeaders conn sid False [(":status", "100")]
  | otherwise = pure ()

expectsContinue :: [Header] -> Bool
expectsContinue = hasContinueExpectation . foldr expectField []
  where
    expectField (name, value) rest
      | name == "expect" = value : rest
      | otherwise = rest

data ContinueGate = SendImmediately | WaitForContinue !(TVar ContinueState)

data ContinueState
  = AwaitingContinue
  | ContinueReceived
  | RequestStarted
  | RequestSuppressed
  | ContinueClosed
  deriving stock (Eq)

-- | Gate uploads with a bare expectation and a body.
newContinueGate :: Bool -> ByteString -> [Header] -> IO ContinueGate
newContinueGate carriesContent method headers
  | carriesContent && method /= "CONNECT" && expectsContinue headers
  , readContentLength headers /= Right (Just 0) =
      WaitForContinue <$> newTVarIO AwaitingContinue
  | otherwise = pure SendImmediately

-- | The sender can start on 100 or after the timeout in microseconds. A final response or
-- reset that arrived before the sender claimed the body takes precedence.
awaitContinue :: Int -> ContinueGate -> IO Bool
awaitContinue _ SendImmediately = pure True
awaitContinue timeoutUs (WaitForContinue gate) = do
  _ <- timeout (max 0 timeoutUs) $ atomically $ do
    state <- readTVar gate
    check (state /= AwaitingContinue)
  atomically $ do
    state <- readTVar gate
    case state of
      AwaitingContinue -> start
      ContinueReceived -> start
      _ -> pure False
  where
    start = writeTVar gate RequestStarted >> pure True

-- | Only a validated 100 or final response changes the upload decision.
noteContinueResponse :: ContinueGate -> Int -> IO ()
noteContinueResponse SendImmediately _ = pure ()
noteContinueResponse (WaitForContinue gate) status
  | status == 100 = atomically $ do
      state <- readTVar gate
      when (state == AwaitingContinue) $ writeTVar gate ContinueReceived
  | status >= 200 = atomically $ do
      state <- readTVar gate
      when (state == AwaitingContinue || state == ContinueReceived) $
        writeTVar gate RequestSuppressed
  | otherwise = pure ()

stopContinue :: ContinueGate -> IO ()
stopContinue SendImmediately = pure ()
stopContinue (WaitForContinue gate) = atomically $ writeTVar gate ContinueClosed

-- | Claim the reset for a suppressed upload after the entire response has
-- arrived. An empty END_STREAM would violate a nonzero Content-Length.
finishContinueResponse :: ContinueGate -> IO Bool
finishContinueResponse SendImmediately = pure False
finishContinueResponse (WaitForContinue gate) = atomically $ do
  state <- readTVar gate
  if state == RequestSuppressed
    then writeTVar gate ContinueClosed >> pure True
    else pure False

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
  withMVarMasked (connSendLock conn) $ \_ -> do
    sid <- openStream
    sendPeerHeaderBlockUnlocked conn sid endStream headers
    pure sid

sendPeerHeaderBlockUnlocked :: Connection -> StreamId -> Bool -> [Header] -> IO ()
sendPeerHeaderBlockUnlocked conn sid endStream headers = do
  settings <- readIORef (connRemoteSettings conn)
  block <- withMVar (connHpackEncoder conn) $ \enc ->
    encodeHeaderBlock defaultEncodeStrategy enc headers
  let maxFrame = fromIntegral (settingsMaxFrameSize settings)
      len = BS.length block
  if len <= maxFrame
    then sendFrameUnlocked conn $
      Frame (FrameHeader (fromIntegral len) FrameHeaders flags sid) (HeadersFrame Nothing block)
    else sendFramesUnlocked conn (headerBlockFrames sid endStream 0 block maxFrame)
  where
    flags = flagEndHeaders .|. (if endStream then flagEndStream else 0)

-- | Send DATA within the frame size the engine's peer advertised. Each
-- frame takes the send lock separately so other streams can make progress.
sendPeerData :: Connection -> StreamId -> ByteString -> Bool -> IO ()
sendPeerData conn sid body endStream = sendChunks body
  where
    sendChunks bytes = do
      remaining <- withMVar (connSendLock conn) $ \_ -> do
        settings <- readIORef (connRemoteSettings conn)
        let maxFrame = fromIntegral (settingsMaxFrameSize settings)
        if BS.length bytes <= maxFrame
          then sendChunk bytes endStream >> pure BS.empty
          else do
            let (chunk, rest) = BS.splitAt maxFrame bytes
            sendChunk chunk False
            pure rest
      unless (BS.null remaining) (sendChunks remaining)

    sendChunk bytes isLast =
      sendFrameUnlocked conn $
        Frame
          (FrameHeader (fromIntegral (BS.length bytes)) FrameData flags sid)
          (DataFrame bytes)
      where
        flags = if isLast then flagEndStream else 0

-- | Update the peer settings and send the acknowledgement in the same
-- send-lock critical section used by the engine's header writer.
acknowledgePeerSettings :: Connection -> [(Word16, Word32)] -> IO ()
acknowledgePeerSettings conn settings = do
  valid <- withMVarMasked (connSendLock conn) $ \_ -> do
    previous <- readIORef (connRemoteSettings conn)
    case applySettingsParams previous settings of
      Left _ -> pure False
      Right current -> do
        writeIORef (connRemoteSettings conn) current
        sendFrameUnlocked conn $
          Frame (FrameHeader 0 FrameSettings flagAck 0) (SettingsFrame [])
        pure True
  if valid then pure () else abortConnection conn ProtocolError "invalid peer settings"

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
-- blocks reach the wire, or a concurrent sender desyncs the peer's decoder;
-- and it runs masked for the same reason too.
-- The caller supplies the frame size used by this send.
encodeAndSendHeaderBlock
  :: Connection
  -> StreamId
  -> Bool                          -- ^ set END_STREAM on the initial HEADERS frame
  -> FrameFlags                    -- ^ extra flags to OR into the initial HEADERS frame
  -> [(ByteString, ByteString)]    -- ^ raw header list (incl. pseudo-headers)
  -> Int                           -- ^ peer SETTINGS_MAX_FRAME_SIZE
  -> IO ()
encodeAndSendHeaderBlock conn sid endStream extraFlags headers maxFrame =
  withMVarMasked (connSendLock conn) $ \_ -> do
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

-- | Read one frame from a callback that returns owned bytes and supplies
-- exactly the requested length unless it reaches EOF.
recvExactFrame :: Connection -> (Int -> IO ByteString) -> Word32 -> IO (Maybe Frame)
recvExactFrame conn readN maxFrame = do
  hdrBytes <- readN frameHeaderLength
  if BS.null hdrBytes
    then pure Nothing
    else case decodeFrameHeader hdrBytes of
      Left _ -> abortConnection conn FrameSizeError "incomplete frame header"
      Right hdr
        | fhLength hdr > maxFrame -> abortConnection conn FrameSizeError "frame exceeds receive limit"
        | otherwise -> do
            body <- if fhLength hdr == 0 then pure BS.empty else readN (fromIntegral (fhLength hdr))
            case decodeFramePayload hdr body of
              Left InvalidWindowUpdateIncrement -> abortConnection conn ProtocolError "invalid WINDOW_UPDATE increment"
              Left _ -> abortConnection conn FrameSizeError "invalid frame payload"
              Right payload -> pure (Just (Frame hdr payload))

-- | Collect a HEADERS block from an owned-frame reader. Only the
-- initial fragment carries padding or priority data. Its END_STREAM
-- flag remains available to the caller in the original frame header.
recvHeaderBlock :: Connection -> IO (Maybe Frame) -> FrameHeader -> FramePayload -> IO ByteString
recvHeaderBlock conn readNext hdr = recvHeaderBlockWith pure conn readNext hdr . rawPayload

-- | As 'recvHeaderBlock', retaining each fragment before reading the
-- next frame. Ring-backed readers supply an eager copy.
{-# INLINE recvHeaderBlockWith #-}
recvHeaderBlockWith
  :: (ByteString -> IO ByteString) -> Connection -> IO (Maybe Frame)
  -> FrameHeader -> ByteString -> IO ByteString
recvHeaderBlockWith retain conn readNext hdr payload =
  case headerFragment (fhFlags hdr) payload of
    Nothing -> abortConnection conn ProtocolError "invalid HEADERS payload"
    Just fragment -> recvFieldBlockWith retain conn readNext hdr fragment
  where
    headerFragment flags bytes = do
      unpadded <- stripFramePadding flags bytes
      if testFlag flags flagPriority
        then if BS.length unpadded >= 5 then Just (BS.drop 5 unpadded) else Nothing
        else Just unpadded

-- | Strip the optional pad length and padding from a frame payload.
stripFramePadding :: FrameFlags -> ByteString -> Maybe ByteString
stripFramePadding flags bytes
  | not (testFlag flags flagPadded) = Just bytes
  | otherwise = case BS.uncons bytes of
      Just (padding, body) | fromIntegral padding <= BS.length body ->
        Just (BS.take (BS.length body - fromIntegral padding) body)
      _ -> Nothing
{-# INLINE stripFramePadding #-}

-- | Return DATA frame credit in the callback engine. Discarded DATA
-- still counts toward connection flow control.
returnFrameCredit :: Connection -> StreamId -> Word32 -> Bool -> IO ()
returnFrameCredit conn sid len updateStream = when (len > 0) $ do
  sendFrame conn (update 0)
  when updateStream (sendFrame conn (update sid))
  where
    update stream = Frame (FrameHeader 4 FrameWindowUpdate 0 stream) (WindowUpdateFrame len)

-- | Assemble a field block whose initial HEADERS or PUSH_PROMISE
-- fragment has already been stripped of frame-specific fields.
{-# INLINE recvFieldBlockWith #-}
recvFieldBlockWith
  :: (ByteString -> IO ByteString) -> Connection -> IO (Maybe Frame)
  -> FrameHeader -> ByteString -> IO ByteString
recvFieldBlockWith retain conn readNext hdr fragment
  | testFlag (fhFlags hdr) flagEndHeaders = retain fragment
  | otherwise = do
      first <- retain fragment
      collectFieldBlock retain conn readNext (fhStreamId hdr) (headerBlockBudget - frameWireSize hdr) [first]
  where
    -- Limit buffered wire bytes, including framing overhead.
    headerBlockBudget = 16 * 1024 * 1024

frameWireSize :: FrameHeader -> Int
frameWireSize hdr = frameHeaderLength + fromIntegral (fhLength hdr)
{-# INLINE frameWireSize #-}

-- Keep the fragmented read loop out of the single-frame receive path.
{-# NOINLINE collectFieldBlock #-}
collectFieldBlock
  :: (ByteString -> IO ByteString) -> Connection -> IO (Maybe Frame)
  -> StreamId -> Int -> [ByteString] -> IO ByteString
collectFieldBlock retain conn readNext sid = collect
  where
    collect !remaining fragments = do
      next <- readNext
      case next of
        Just (Frame hdr (FramePayloadRaw chunk))
          | fhType hdr == FrameContinuation, fhStreamId hdr == sid ->
              let bytes = frameWireSize hdr
              in if bytes > remaining
                  then abortConnection conn EnhanceYourCalm "encoded header block exceeds receive budget"
                  else do
                    retained <- retain chunk
                    let parts = retained : fragments
                    if testFlag (fhFlags hdr) flagEndHeaders
                      then pure (BS.concat (reverse parts))
                      else collect (remaining - bytes) parts
        _ -> abortConnection conn ProtocolError "expected same-stream CONTINUATION"

-- | Detach bytes from the receive ring before its next read. The
-- copy runs in IO and finishes before the owned slice is returned.
copyReceivedBytes :: ByteString -> IO ByteString
copyReceivedBytes (BSI.PS src offset len)
  | len == 0 = pure BS.empty
  | otherwise = do
      dst <- BSI.mallocByteString len
      withForeignPtr src $ \from ->
        withForeignPtr dst $ \to ->
          copyBytes to (from `plusPtr` offset) len
      pure (BSI.PS dst 0 len)

abortConnection :: Connection -> ErrorCode -> ByteString -> IO a
abortConnection conn code reason = do
  closeConnection conn code reason
  throwIO (ConnectionError code reason 0)

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

-- | Send GOAWAY and shut the connection: the send ring is drained and
-- unmapped, and the receive side is marked closed so the recv loop sees
-- end of input on its next wait. Idempotent, and safe to call from any
-- thread, including while another thread is decoding a frame: the
-- receive ring stays mapped until 'releaseConnection'.
--
-- The send side is torn down under 'connSendLock', so a sender either
-- finishes before the unmap or finds the transport closed and throws;
-- it can never write into the unmapped ring.
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
      withMVar (connSendLock conn) $ \_ -> do
        sendFrameUnlocked conn goaway
          `catch` (\(_ :: SomeException) -> pure ())
        sendClose (connSendTransport conn)
          `catch` (\(_ :: SomeException) -> pure ())
      WT.receiveClose (connRingTransport conn)
        `catch` (\(_ :: SomeException) -> pure ())

-- | 'closeConnection' (with 'NoError' if it was still open), then unmap
-- the receive ring. Frames from 'recvFrame' / 'recvFrameRaw' are
-- zero-copy slices of that ring, so call this only once no thread is
-- reading from the connection or still holds an uncopied frame: the
-- owner of the recv loop calls it after the loop has exited.
releaseConnection :: Connection -> IO ()
releaseConnection conn = do
  closeConnection conn NoError ""
  connReleaseRecvRing conn

connectionSettings :: Connection -> IO (Settings, Settings)
connectionSettings conn = do
  local <- readIORef (connLocalSettings conn)
  remote <- readIORef (connRemoteSettings conn)
  pure (local, remote)
