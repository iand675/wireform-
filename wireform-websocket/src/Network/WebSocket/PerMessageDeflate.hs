{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | RFC 7692 @permessage-deflate@ extension.

This is the standard WebSocket compression extension: each
message's payload is compressed with raw DEFLATE
('Codec.Compression.Zlib' in @-MAX_WBITS@ mode), framed with the
RSV1 bit set on the first frame of the message, with a trailing
@00 00 FF FF@ marker stripped before transmission (RFC 7692
§7.2.1) and re-appended before inflate (§7.2.2).

This module is purely about /the extension/: negotiation
(client offers and server selects with parameter values for
context-takeover and max-window-bits), per-message
'compressMessage' / 'decompressMessage' helpers, and the
'PmdContext' opaque pair of inflate \/ deflate streams.

Wiring into the high-level send \/ receive path lives in
"Network.WebSocket.Connection".

The implementation goes through a small FFI shim
('cbits\/wf_pmd.c') against the system @libz@ — no
@Codec.Compression.Zlib@ lazy-ByteString round-trips on the hot
path.  Persistent 'z_stream' contexts survive across messages so
the deflate dictionary is reused; that's the whole point of
context-takeover.
-}
module Network.WebSocket.PerMessageDeflate (
  -- * Negotiation
  PmdParams (..),
  defaultPmdParams,
  PmdOffer (..),
  defaultPmdOffer,
  ClientMaxWindowBitsHint (..),
  offerHeader,
  parseOffers,
  selectOffer,
  responseHeader,
  parseResponseParams,

  -- * Context (per-connection state)
  PmdContext,
  newPmdContext,
  freePmdContext,
  pmdParams,

  -- * Compression
  compressMessage,
  decompressMessage,
  decompressMessageBounded,
  pmdMaybeReset,
  Direction (..),

  -- * Errors
  PmdError (..),
) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Char (toLower)
import Data.IORef
import Data.Word (Word8)
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (alloca, free, mallocBytes, reallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek)
import Network.WebSocket.Connection.Role (Role (..))


------------------------------------------------------------------------
-- Negotiation
------------------------------------------------------------------------

{- | Negotiated parameters that drive both endpoints once
@permessage-deflate@ is active.

@serverMaxWindowBits@ and @clientMaxWindowBits@ are the RFC 7692
§7.1.2 window-size parameters: they bound the LZ77 sliding window
the /respective side's compressor/ may use.  zlib's actual support
starts at 9; values of 8 on the wire are silently bumped (matches
nginx \/ Chromium).

@serverNoContextTakeover@ and @clientNoContextTakeover@ (RFC 7692
§7.1.1) instruct the respective compressor to reset its state
after every message.  Both extension peers must comply when the
parameter is in the negotiated response — failure to do so
corrupts the next message because zlib refers back to the
previous message's window.
-}
data PmdParams = PmdParams
  { pmdServerNoContextTakeover :: !Bool
  , pmdClientNoContextTakeover :: !Bool
  , pmdServerMaxWindowBits :: !Int -- 8..15; clamped to 9..15 internally
  , pmdClientMaxWindowBits :: !Int -- 8..15; clamped to 9..15 internally
  }
  deriving stock (Eq, Show)


defaultPmdParams :: PmdParams
defaultPmdParams =
  PmdParams
    { pmdServerNoContextTakeover = False
    , pmdClientNoContextTakeover = False
    , pmdServerMaxWindowBits = 15
    , pmdClientMaxWindowBits = 15
    }


{- | A single client offer parsed from a @Sec-WebSocket-Extensions@
header value.  Each header line may carry multiple offers
separated by commas (RFC 6455 §9.1) — clients typically send one.
-}
data PmdOffer = PmdOffer
  { offServerNoContextTakeover :: !Bool
  , offClientNoContextTakeover :: !Bool
  , offServerMaxWindowBits :: !(Maybe Int)
  , offClientMaxWindowBitsHint :: !ClientMaxWindowBitsHint
  }
  deriving stock (Eq, Show)


{- | Per RFC 7692 §7.1.2.2, @client_max_window_bits@ can appear
bare (as a hint asking the server to pick) /or/ with a value.
-}
data ClientMaxWindowBitsHint
  = ClientWindowBitsAbsent
  | ClientWindowBitsHinted
  | ClientWindowBitsSet !Int
  deriving stock (Eq, Show)


defaultPmdOffer :: PmdOffer
defaultPmdOffer =
  PmdOffer
    { offServerNoContextTakeover = False
    , offClientNoContextTakeover = False
    , offServerMaxWindowBits = Nothing
    , offClientMaxWindowBitsHint = ClientWindowBitsHinted
    }


{- | Render the client's offer as the value of a single
@Sec-WebSocket-Extensions@ header.
-}
offerHeader :: PmdOffer -> ByteString
offerHeader o =
  BS.intercalate "; " $
    "permessage-deflate"
      : concat
        [ ["server_no_context_takeover" | offServerNoContextTakeover o]
        , ["client_no_context_takeover" | offClientNoContextTakeover o]
        , case offServerMaxWindowBits o of
            Just n -> ["server_max_window_bits=" <> BS8.pack (show n)]
            Nothing -> []
        , case offClientMaxWindowBitsHint o of
            ClientWindowBitsAbsent -> []
            ClientWindowBitsHinted -> ["client_max_window_bits"]
            ClientWindowBitsSet n -> ["client_max_window_bits=" <> BS8.pack (show n)]
        ]


{- | Parse all @permessage-deflate@ offers out of the comma-separated
token list passed in @Sec-WebSocket-Extensions@.  Non-PMD
extensions are dropped silently.
-}
parseOffers :: [ByteString] -> [PmdOffer]
parseOffers raw =
  [ off
  | tok <- raw
  , let parts = map stripOws (BS.split 0x3B tok) -- ';'
  , case parts of
      (name : _) -> BS8.map toLower name == "permessage-deflate"
      _ -> False
  , let mOff = parseExtParams (drop 1 (map stripOws (BS.split 0x3B tok)))
  , Just off <- [mOff]
  ]
  where
    parseExtParams :: [ByteString] -> Maybe PmdOffer
    parseExtParams = go defaultPmdOffer {offClientMaxWindowBitsHint = ClientWindowBitsAbsent}
      where
        go !acc [] = Just acc
        go !acc (p : ps) =
          let (k, mv) = splitKV p
          in case BS8.map toLower k of
               "server_no_context_takeover"
                 | BS.null mv -> go acc {offServerNoContextTakeover = True} ps
                 | otherwise -> Nothing
               "client_no_context_takeover"
                 | BS.null mv -> go acc {offClientNoContextTakeover = True} ps
                 | otherwise -> Nothing
               "server_max_window_bits" ->
                 case readWindowBits mv of
                   Just n -> go acc {offServerMaxWindowBits = Just n} ps
                   Nothing -> Nothing
               "client_max_window_bits"
                 | BS.null mv -> go acc {offClientMaxWindowBitsHint = ClientWindowBitsHinted} ps
                 | otherwise -> case readWindowBits mv of
                     Just n -> go acc {offClientMaxWindowBitsHint = ClientWindowBitsSet n} ps
                     Nothing -> Nothing
               _ -> Nothing

    splitKV b = case BS.elemIndex 0x3D b of -- '='
      Just i -> (stripOws (BS.take i b), stripQuotes (stripOws (BS.drop (i + 1) b)))
      Nothing -> (b, BS.empty)

    stripQuotes b
      | BS.length b >= 2
      , BS.head b == 0x22
      , BS.last b == 0x22 =
          BS.init (BS.tail b)
      | otherwise = b

    readWindowBits b = case BS8.readInt b of
      Just (n, rest) | BS.null rest && n >= 8 && n <= 15 -> Just (fromIntegral n)
      _ -> Nothing


{- | Server-side decision: pick one of the client's offers and
produce the negotiated 'PmdParams'.  Returns 'Nothing' to
decline (no @Sec-WebSocket-Extensions@ response header should be
emitted in that case).

@serverPolicy@ is the server's hard limit on its own deflate
window size (typically 15) plus a flag saying whether the server
/requires/ @server_no_context_takeover@ for memory reasons.  We
pick the most permissive offer compatible with the policy.
-}
selectOffer
  :: PmdParams
  -- ^ server's preferred ceiling
  -> [PmdOffer]
  -> Maybe PmdParams
selectOffer policy = foldr step Nothing
  where
    step off acc =
      let serverWb =
            min
              (pmdServerMaxWindowBits policy)
              (maybe 15 id (offServerMaxWindowBits off))
          clientWb = case offClientMaxWindowBitsHint off of
            ClientWindowBitsAbsent -> 15
            ClientWindowBitsHinted -> pmdClientMaxWindowBits policy
            ClientWindowBitsSet n -> min n (pmdClientMaxWindowBits policy)
          serverNc =
            offServerNoContextTakeover off
              || pmdServerNoContextTakeover policy
          clientNc =
            offClientNoContextTakeover off
              || pmdClientNoContextTakeover policy
          ok =
            serverWb >= 8
              && serverWb <= 15
              && clientWb >= 8
              && clientWb <= 15
      in if ok
           then
             Just
               PmdParams
                 { pmdServerNoContextTakeover = serverNc
                 , pmdClientNoContextTakeover = clientNc
                 , pmdServerMaxWindowBits = serverWb
                 , pmdClientMaxWindowBits = clientWb
                 }
           else acc


-- | Render the server's @Sec-WebSocket-Extensions@ response.
responseHeader :: PmdParams -> ByteString
responseHeader p =
  BS.intercalate "; " $
    "permessage-deflate"
      : concat
        [ ["server_no_context_takeover" | pmdServerNoContextTakeover p]
        , ["client_no_context_takeover" | pmdClientNoContextTakeover p]
        , if pmdServerMaxWindowBits p < 15
            then ["server_max_window_bits=" <> BS8.pack (show (pmdServerMaxWindowBits p))]
            else []
        , if pmdClientMaxWindowBits p < 15
            then ["client_max_window_bits=" <> BS8.pack (show (pmdClientMaxWindowBits p))]
            else []
        ]


{- | Client-side: parse the server's reply (the value of one
@Sec-WebSocket-Extensions@ header line) into the negotiated
'PmdParams'.  Returns 'Nothing' if the line is not for
@permessage-deflate@.
-}
parseResponseParams :: ByteString -> Maybe PmdParams
parseResponseParams hdr =
  let parts = map stripOws (BS.split 0x3B hdr)
  in case parts of
       (name : rest)
         | BS8.map toLower name == "permessage-deflate" ->
             go defaultPmdParams rest
       _ -> Nothing
  where
    go acc [] = Just acc
    go acc (p : ps) =
      let (k, mv) = splitKV p
      in case BS8.map toLower k of
           "server_no_context_takeover"
             | BS.null mv -> go acc {pmdServerNoContextTakeover = True} ps
             | otherwise -> Nothing
           "client_no_context_takeover"
             | BS.null mv -> go acc {pmdClientNoContextTakeover = True} ps
             | otherwise -> Nothing
           "server_max_window_bits" -> case readBits mv of
             Just n -> go acc {pmdServerMaxWindowBits = n} ps
             Nothing -> Nothing
           "client_max_window_bits" -> case readBits mv of
             Just n -> go acc {pmdClientMaxWindowBits = n} ps
             Nothing -> Nothing
           _ -> Nothing
    splitKV b = case BS.elemIndex 0x3D b of
      Just i -> (stripOws (BS.take i b), stripOws (BS.drop (i + 1) b))
      Nothing -> (b, BS.empty)
    readBits b = case BS8.readInt b of
      Just (n, rest) | BS.null rest && n >= 8 && n <= 15 -> Just (fromIntegral n)
      _ -> Nothing


stripOws :: ByteString -> ByteString
stripOws = trimEnd . BS.dropWhile isOws
  where
    isOws b = b == 0x20 || b == 0x09
    trimEnd s =
      let !n = BS.length s
          go i
            | i <= 0 = BS.empty
            | isOws (BS.index s (i - 1)) = go (i - 1)
            | otherwise = BS.take i s
      in go n


------------------------------------------------------------------------
-- FFI to cbits/wf_pmd.c
------------------------------------------------------------------------

data WfPmdStream -- abstract C type


foreign import ccall unsafe "wf_pmd_inflate_new"
  c_wf_pmd_inflate_new :: CInt -> IO (Ptr WfPmdStream)


foreign import ccall unsafe "wf_pmd_deflate_new"
  c_wf_pmd_deflate_new :: CInt -> CInt -> CInt -> IO (Ptr WfPmdStream)


foreign import ccall unsafe "wf_pmd_free"
  c_wf_pmd_free :: Ptr WfPmdStream -> IO ()


foreign import ccall unsafe "wf_pmd_inflate"
  c_wf_pmd_inflate
    :: Ptr WfPmdStream
    -> Ptr Word8
    -> CSize
    -> Ptr Word8
    -> CSize
    -> Ptr CSize
    -> IO CInt


foreign import ccall unsafe "wf_pmd_deflate"
  c_wf_pmd_deflate
    :: Ptr WfPmdStream
    -> Ptr Word8
    -> CSize
    -> Ptr Word8
    -> CSize
    -> Ptr CSize
    -> IO CInt


foreign import ccall unsafe "wf_pmd_reset_inflate"
  c_wf_pmd_reset_inflate :: Ptr WfPmdStream -> IO CInt


foreign import ccall unsafe "wf_pmd_reset_deflate"
  c_wf_pmd_reset_deflate :: Ptr WfPmdStream -> IO CInt


-- | Return codes from the C shim mirror 'PmdError'.
codeOk, codeNeedsMoreOut, codeBadInput, codeOom, codeInitFail :: CInt
codeOk = 0
codeNeedsMoreOut = -1
codeBadInput = -2
codeOom = -3
codeInitFail = -4


data PmdError
  = PmdBadInput
  | PmdInitFailed
  | PmdOutOfMemory
  | PmdInternal !Int
  deriving stock (Eq, Show)


instance Exception PmdError


------------------------------------------------------------------------
-- Context
------------------------------------------------------------------------

{- | Per-connection PMD state.  Holds a deflate context for outbound
messages and an inflate context for inbound; both are persistent
across messages unless the matching @*_no_context_takeover@ flag
is negotiated, in which case 'pmdMaybeReset' is called between
messages.
-}
data PmdContext = PmdContext
  { pmdInflate :: {-# UNPACK #-} !(IORef (Ptr WfPmdStream))
  , pmdDeflate :: {-# UNPACK #-} !(IORef (Ptr WfPmdStream))
  , pmdRole :: !Role
  , pmdParams_ :: !PmdParams
  }


pmdParams :: PmdContext -> PmdParams
pmdParams = pmdParams_


{- | Build a new context for the given role.  The two underlying
C streams are allocated immediately.  Throws 'PmdInitFailed' if
zlib refuses to initialise (out of memory, invalid window bits
somehow slipping through).

The role determines the window-bits assignment: on the server
the deflate window is 'pmdServerMaxWindowBits' and the inflate
window is 'pmdClientMaxWindowBits' (and vice versa on the
client) — RFC 7692 §7.1.2 talks about /the deflater/, not the
direction on the wire.
-}
newPmdContext :: Role -> PmdParams -> IO PmdContext
newPmdContext role params = do
  let (defWb, infWb) = case role of
        Server -> (pmdServerMaxWindowBits params, pmdClientMaxWindowBits params)
        Client -> (pmdClientMaxWindowBits params, pmdServerMaxWindowBits params)
  defPtr <-
    c_wf_pmd_deflate_new
      (fromIntegral defWb)
      (fromIntegral defaultCompressionLevel)
      (fromIntegral defaultMemLevel)
  if defPtr == nullPtr
    then throwIO PmdInitFailed
    else do
      infPtr <- c_wf_pmd_inflate_new (fromIntegral infWb)
      if infPtr == nullPtr
        then do
          c_wf_pmd_free defPtr
          throwIO PmdInitFailed
        else do
          dRef <- newIORef defPtr
          iRef <- newIORef infPtr
          pure
            PmdContext
              { pmdInflate = iRef
              , pmdDeflate = dRef
              , pmdRole = role
              , pmdParams_ = params
              }


{- | Default deflate level (1 — Z_BEST_SPEED).  Browsers
typically negotiate level 1 by convention because PMD on the
happy path is small messages and deflate's level-1 already
catches the common case while keeping the deflate ms per message
well under a millisecond.
-}
defaultCompressionLevel :: Int
defaultCompressionLevel = 1


defaultMemLevel :: Int
defaultMemLevel = 8


-- | Tear down the underlying C streams.  Idempotent.
freePmdContext :: PmdContext -> IO ()
freePmdContext ctx = do
  d <- atomicModifyIORef' (pmdDeflate ctx) (\p -> (nullPtr, p))
  i <- atomicModifyIORef' (pmdInflate ctx) (\p -> (nullPtr, p))
  c_wf_pmd_free d
  c_wf_pmd_free i


------------------------------------------------------------------------
-- Compression
------------------------------------------------------------------------

{- | Compress one message.  Strips the trailing @00 00 FF FF@
marker per RFC 7692 §7.2.1.  Returns the bytes to ship inside
the WebSocket message frames (the caller still has to set RSV1
and fragment as it likes).
-}
compressMessage :: PmdContext -> ByteString -> IO ByteString
compressMessage ctx plaintext = do
  defPtr <- readIORef (pmdDeflate ctx)
  if defPtr == nullPtr
    then throwIO PmdInitFailed
    else do
      compressed <- runDeflate defPtr plaintext
      -- Strip trailing 00 00 FF FF (or just the FF FF if the
      -- whole buffer is shorter than 4 bytes, which only happens
      -- for empty messages — RFC 7692 §7.2.3.6 says an empty
      -- compressed payload is encoded as a single 0x00 byte; we
      -- still strip when the trailer is present and otherwise
      -- leave it alone).
      let !n = BS.length compressed
      pure $!
        if n >= 4
          && BS.index compressed (n - 4) == 0x00
          && BS.index compressed (n - 3) == 0x00
          && BS.index compressed (n - 2) == 0xFF
          && BS.index compressed (n - 1) == 0xFF
          then BS.take (n - 4) compressed
          else compressed


{- | Decompress one message.  Appends @00 00 FF FF@ per RFC 7692
§7.2.2 before driving zlib.
-}
decompressMessage :: PmdContext -> ByteString -> IO ByteString
decompressMessage ctx payload = do
  infPtr <- readIORef (pmdInflate ctx)
  if infPtr == nullPtr
    then throwIO PmdInitFailed
    else runInflate infPtr (payload <> "\x00\x00\xFF\xFF")


{- | 'decompressMessage' with a cap on the inflated size: 'Nothing' as
soon as the message would exceed @limit@ bytes.  The output buffer never
grows past @limit + 64@ bytes, so a small compressed message cannot make
the receiver allocate without bound (an inflate bomb) before the
message-size check.  After 'Nothing' the inflate stream is mid-message;
the caller fails the connection.
-}
decompressMessageBounded :: PmdContext -> Int -> ByteString -> IO (Maybe ByteString)
decompressMessageBounded ctx limit payload = do
  infPtr <- readIORef (pmdInflate ctx)
  if infPtr == nullPtr
    then throwIO PmdInitFailed
    else driveZ infPtr (payload <> "\x00\x00\xFF\xFF") c_wf_pmd_inflate limit


{- | Reset whichever streams need resetting between messages
according to the negotiated @*_no_context_takeover@ flags.  Call
after every successful 'compressMessage' \/ 'decompressMessage'
pair on the same direction.
-}
pmdMaybeReset :: PmdContext -> Direction -> IO ()
pmdMaybeReset ctx dir = case dir of
  Outbound ->
    let needs = case pmdRole ctx of
          Server -> pmdServerNoContextTakeover (pmdParams_ ctx)
          Client -> pmdClientNoContextTakeover (pmdParams_ ctx)
    in if needs
         then do
           d <- readIORef (pmdDeflate ctx)
           _ <- c_wf_pmd_reset_deflate d
           pure ()
         else pure ()
  Inbound ->
    let needs = case pmdRole ctx of
          Server -> pmdClientNoContextTakeover (pmdParams_ ctx)
          Client -> pmdServerNoContextTakeover (pmdParams_ ctx)
    in if needs
         then do
           i <- readIORef (pmdInflate ctx)
           _ <- c_wf_pmd_reset_inflate i
           pure ()
         else pure ()


data Direction = Inbound | Outbound


------------------------------------------------------------------------
-- Inflate / deflate drivers
--
-- Both drivers grow a malloc'd output buffer until the C shim
-- reports WF_PMD_OK.  Final buffer is reallocated to exact size
-- and handed to BSI.mkBS as a pinned ForeignPtr.
------------------------------------------------------------------------

runInflate :: Ptr WfPmdStream -> ByteString -> IO ByteString
runInflate s input = driveZ s input c_wf_pmd_inflate maxBound >>= unbounded


runDeflate :: Ptr WfPmdStream -> ByteString -> IO ByteString
runDeflate s input = driveZ s input c_wf_pmd_deflate maxBound >>= unbounded


-- | With a limit of 'maxBound' 'driveZ' never gives up.
unbounded :: Maybe ByteString -> IO ByteString
unbounded = maybe (throwIO (PmdInternal 0)) pure


driveZ
  :: Ptr WfPmdStream
  -> ByteString
  -> ( Ptr WfPmdStream
       -> Ptr Word8
       -> CSize
       -> Ptr Word8
       -> CSize
       -> Ptr CSize
       -> IO CInt
     )
  -> Int
  -- ^ output limit: 'Nothing' once the output exceeds this many bytes
  -> IO (Maybe ByteString)
driveZ s input op limit =
  BSU.unsafeUseAsCStringLen input $ \(srcPtr0, srcLen0) -> do
    let initialCap = max 64 (srcLen0 * 2)
    buf0 <- mallocBytes initialCap
    drive (castPtr srcPtr0) srcLen0 buf0 0 initialCap
  where
    -- @produced@ tracks how many bytes are already in the output
    -- buffer from previous c-shim iterations; the next call
    -- writes starting at @buf + produced@ into @cap - produced@
    -- bytes of remaining capacity.
    --
    -- For inflate the input is fully buffered up front; for
    -- deflate, the input is buffered up front too.  Either way,
    -- the C shim retains src/avail_in on its z_stream across
    -- calls when WF_PMD_NEEDS_MORE_OUT comes back, so we pass
    -- NULL/0 on the retry to mean "keep going".
    drive srcPtr srcLen buf produced cap = alloca $ \outProducedRef -> do
      let !remaining = cap - produced
          !writePtr = buf `plusPtr` produced
      rc <-
        op
          s
          srcPtr
          (fromIntegral srcLen)
          writePtr
          (fromIntegral remaining)
          outProducedRef
      addProduced <- fromIntegral <$> peek outProducedRef
      let !nowProduced = produced + addProduced
      case () of
        _
          | rc == codeOk -> do
              fp <- BSI.mallocByteString nowProduced
              withForeignPtr fp $ \p ->
                copyBytes p buf nowProduced
              free buf
              pure $! Just $! BSI.BS fp nowProduced
          | rc == codeNeedsMoreOut ->
              if nowProduced > limit
                then do
                  free buf
                  pure Nothing
                else do
                  -- Never grow past limit + 64: the output only has to
                  -- show that it exceeds the limit.  (64 > 6 keeps
                  -- zlib's minimum room for a sync flush.)
                  let !newCap = min (cap * 2) (limit + min 64 (maxBound - limit))
                  buf' <- reallocBytes buf newCap
                  -- Subsequent shim call: input was fully delivered;
                  -- avail_in on the z_stream is whatever didn't fit.
                  drive (castPtr nullPtr) 0 buf' nowProduced newCap
          | rc == codeBadInput -> do
              free buf
              throwIO PmdBadInput
          | rc == codeOom -> do
              free buf
              throwIO PmdOutOfMemory
          | rc == codeInitFail -> do
              free buf
              throwIO PmdInitFailed
          | otherwise -> do
              free buf
              throwIO (PmdInternal (fromIntegral rc))
