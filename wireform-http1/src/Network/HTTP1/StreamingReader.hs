{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

{- | Zero-copy HTTP\/1.x head reader on top of the wireform magic-ring
'Wireform.Transport'.

This module is the /fast/ streaming entry point: it walks the ring
directly with the SIMD CR / CRLFCRLF scanners that live in
@cbits/http1_scan.c@, slices the header block as a single zero-copy
'BS.ByteString' over the ring's foreign pointer, and hands it to the
existing 'Network.HTTP1.Parser.parseRequest' / 'parseResponse' which
walk the slice with the same SIMD tchar / field-vchar primitives the
classic 'Network.HTTP1.Connection' read path uses.

Compared to 'Network.HTTP1.StreamingParser' (which composes
byte-by-byte under the 'Wireform.Parser' @Stream@ monad), this
implementation pays no monad / unboxed-sum overhead — the inner loop
is a single @hs_http1_find_cr@ call per 16-byte stride, identical to
what 'Network.HTTP1.Connection.readBody' / 'Network.HTTP1.Server' do
today on top of 'Network.HTTP1.Internal.RecvBuffer'.  The only
difference is the receive buffer: a double-mapped magic ring instead
of a heap-allocated pinned buffer.  The slice handed to 'parseRequest'
crosses the ring wrap transparently because the magic ring's second
mapping makes any read of up to @ringSize@ bytes contiguous.

Use this in preference to 'Network.HTTP1.StreamingParser' on the hot
connection-handler path; reserve 'StreamingParser' for situations
where you want incremental parsing primitives composable with the
rest of the 'Wireform.Parser' surface.
-}
module Network.HTTP1.StreamingReader
  ( -- * Errors
    ReadError (..)

    -- * Whole-request \/ whole-response readers
  , readRequestHead
  , readResponseHead
  , readRequestHeadFrom
  , readResponseHeadFrom

    -- * Body / line readers (used by Connection)
  , readHeaderBlock
  , readHeaderBlockFrom
  , readChunkSizeLine
  , readChunkSizeLineFrom
  , readUpTo
  , readExact
  , readUntilCRLFStrict
  , StrictLine (..)
  , advancePast
  ) where

import Control.Exception (SomeException)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Word (Word8, Word64)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import GHC.Ptr (Ptr (Ptr))
import Foreign.Storable (peekByteOff)
import GHC.Exts (Int (..))
import GHC.ForeignPtr (ForeignPtr (..), ForeignPtrContents (..))
import GHC.Generics (Generic)

import Wireform.Ring.Internal
  ( MagicRing
  , ringBase
  , ringMask
  )
import Wireform.Transport
  ( ReceiveTransport (..)
  , ReceiveWait (..)
  , receiveRing
  )

import Network.HTTP1.Method (Method)
import qualified Network.HTTP1.Parser as Classic
import Network.HTTP1.Parser (Framing, ParseError)
import Network.HTTP1.Types (Request, Response)

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

-- | Why a streaming read failed.
data ReadError
  = ReadParse !ParseError
    -- ^ The header / chunk-size block was fully delivered but the
    --   structural parser rejected it.  Contains the existing
    --   'Network.HTTP1.Parser.ParseError' value so callers can map
    --   onto the corresponding HTTP response code.
  | ReadMessageTooLong !Int
    -- ^ Header / chunk-size block exceeded the supplied cap before
    --   a terminating delimiter was seen.  Maps to 431 (request) /
    --   502 (response upstream) / protocol error (chunked body).
  | ReadUnexpectedEof
    -- ^ Transport reported EOF before the terminator was seen.
  | ReadTransportError !SomeException
  deriving stock (Generic)

instance Show ReadError where
  show (ReadParse e)         = "ReadParse " <> show e
  show (ReadMessageTooLong n) = "ReadMessageTooLong " <> show n
  show ReadUnexpectedEof     = "ReadUnexpectedEof"
  show (ReadTransportError exc) = "ReadTransportError " <> show exc

------------------------------------------------------------------------
-- C imports — share the SIMD scanners with the classic recv-buffer.
------------------------------------------------------------------------

foreign import ccall unsafe "hs_http1_find_cr"
  c_find_cr :: Ptr Word8 -> CInt -> CInt -> CInt

------------------------------------------------------------------------
-- Default caps (match @Network.HTTP1.Internal.RecvBuffer@)
------------------------------------------------------------------------

-- | h2o's default request-header block cap.
defaultHeaderBlockCap :: Int
defaultHeaderBlockCap = 32 * 1024

-- | Conservative bound on a single chunk-size line.
defaultChunkLineCap :: Int
defaultChunkLineCap = 4 * 1024

------------------------------------------------------------------------
-- Header block reader
------------------------------------------------------------------------

-- | Read the full HTTP\/1.x request \/ response head — everything up
-- to and including the first @\\r\\n\\r\\n@ — as a single contiguous
-- zero-copy 'BS.ByteString' slice over the magic-ring's backing
-- memory.
--
-- Advances the transport tail past the terminator on success, so a
-- subsequent call sees the bytes starting at the body's first octet.
--
-- The returned slice is valid until the next call that advances the
-- ring tail past it (i.e. for the duration of the current
-- request-handling scope).  If the caller needs to retain the bytes
-- beyond that scope it MUST @BS.copy@.
readHeaderBlock
  :: ReceiveTransport
  -> Int          -- ^ hard cap; @ReadMessageTooLong@ if exceeded.
  -> IO (Either ReadError ByteString)
readHeaderBlock t cap = do
  startPos <- receiveLoadHead t
  fmap (fmap fst) (readHeaderBlockFrom t startPos cap)

-- | Like 'readHeaderBlock' but starting from an explicit ring
-- position (matches the @startPos@ argument shape of
-- 'Wireform.Parser.Driver.runParserInternal') and returns the
-- @(block, newStartPos)@ pair so callers can loop without
-- re-reading the transport head between iterations.  Used by
-- benchmark harnesses and by connection-layer pull loops that
-- track position themselves.
readHeaderBlockFrom
  :: ReceiveTransport
  -> Word64
  -> Int
  -> IO (Either ReadError (ByteString, Word64))
readHeaderBlockFrom t startPos cap = do
  let !ring = receiveRing t
      !base = ringBase ring
      !msk  = ringMask ring
  -- @scanFrom@ tracks how far we've already scanned for the
  -- terminator on previous round-trips; on each waitData refill we
  -- step back 3 bytes (so a CRLFCR that straddled the previous
  -- 'avail' end is re-examined) but otherwise we don't rescan the
  -- bytes we've already classified.  Reduces SIMD work on
  -- drip-fed transports.
  loop ring base msk 0
  where
    loop !ring !base !msk !scanFrom = do
      h <- receiveLoadHead t
      let !avail = fromIntegral (h - startPos) :: Int
      mIdx <- findCRLFCRLF base msk startPos scanFrom avail
      case mIdx of
        Just idx
          | idx > cap -> pure (Left (ReadMessageTooLong cap))
          | otherwise -> do
              let !blockLen   = idx
                  !block      = ringSlice ring startPos blockLen
                  !nextPos    = startPos + fromIntegral (blockLen + 4)
              receiveAdvanceTail t nextPos
              pure (Right (block, nextPos))
        Nothing
          | avail > cap -> pure (Left (ReadMessageTooLong cap))
          | otherwise -> do
              -- Need more bytes; suspend on the IO manager.  On
              -- the next iteration resume scanning from a small
              -- backstep so a CRLFCR straddling the previous
              -- buffer end is re-examined.
              let !nextScan = max 0 (avail - 3)
              r <- receiveWaitData t h
              case r of
                ReceiveMoreData _         -> loop ring base msk nextScan
                ReceiveEndOfInput         -> pure (Left ReadUnexpectedEof)
                ReceiveFailed exc -> pure (Left (ReadTransportError exc))

-- | Locate the byte offset (relative to @startPos@) of the next
-- @\\r\\n\\r\\n@ in the ring.  Returns 'Nothing' when no terminator
-- is in the @[startPos+scanFrom, startPos+avail)@ window.
--
-- The double-mapped ring guarantees that any read of up to
-- @ringSize@ bytes starting anywhere in @[base, base + N)@ is
-- contiguous.  We rely on that — the scanner reads up to
-- @avail@ bytes from @base + (startPos .&. mask)@ without any wrap
-- handling.
findCRLFCRLF
  :: Ptr Word8
  -> Int            -- ^ mask = ringSize - 1
  -> Word64         -- ^ start position
  -> Int            -- ^ scan-from offset (resume after a refill)
  -> Int            -- ^ bytes available from start
  -> IO (Maybe Int)
findCRLFCRLF base msk startPos scanFrom avail
  | avail < 4 = pure Nothing
  | otherwise = do
      let !startOff = fromIntegral startPos .&. msk
          !startPtr = base `plusPtr` startOff
      go startPtr scanFrom
  where
    -- We linear-scan for CR using the SIMD finder, then check the
    -- three follow-up bytes (LF / CR / LF).  Most CRs in a header
    -- block belong to inter-header CRLFs, so the follow-up check
    -- fails fast and we resume scanning from CR+1.
    go !ptr !off
      | off + 4 > avail = pure Nothing
      | otherwise = do
          let !crPos = fromIntegral
                (c_find_cr (castPtr ptr) (fromIntegral off) (fromIntegral avail))
          if crPos + 4 > avail
            then pure Nothing
            else do
              b1 <- peekByteOff ptr (crPos + 1) :: IO Word8
              b2 <- peekByteOff ptr (crPos + 2) :: IO Word8
              b3 <- peekByteOff ptr (crPos + 3) :: IO Word8
              if b1 == 0x0a && b2 == 0x0d && b3 == 0x0a
                then pure (Just crPos)
                else go ptr (crPos + 1)
{-# INLINE findCRLFCRLF #-}

-- | Locate the byte offset of the next @\\r\\n@ in the ring (returns
-- the offset of the CR).
findCRLF
  :: Ptr Word8
  -> Int
  -> Word64
  -> Int            -- ^ scan-from offset (resume after a refill)
  -> Int
  -> IO (Maybe Int)
findCRLF base msk startPos scanFrom avail
  | avail < 2 = pure Nothing
  | otherwise = do
      let !startOff = fromIntegral startPos .&. msk
          !startPtr = base `plusPtr` startOff
      go startPtr scanFrom
  where
    go !ptr !off
      | off + 2 > avail = pure Nothing
      | otherwise = do
          let !crPos = fromIntegral
                (c_find_cr (castPtr ptr) (fromIntegral off) (fromIntegral avail))
          if crPos + 2 > avail
            then pure Nothing
            else do
              b1 <- peekByteOff ptr (crPos + 1) :: IO Word8
              if b1 == 0x0a
                then pure (Just crPos)
                else go ptr (crPos + 1)
{-# INLINE findCRLF #-}

------------------------------------------------------------------------
-- Slice helper
------------------------------------------------------------------------

-- | Build a zero-copy 'BS.ByteString' slice over @[pos, pos + len)@
-- of the ring's backing memory.  Constructs the 'ForeignPtr' with
-- 'FinalPtr' (no finalizer) directly via the 'GHC.ForeignPtr'
-- constructor — this is the same convention 'Wireform.Parser.takeBs'
-- follows, and it avoids the 'newForeignPtr_' allocation overhead
-- that a more conservative @unsafePerformIO (newForeignPtr_ ptr)@
-- would otherwise pay on every per-frame slice.
--
-- The slice becomes a dangling pointer if it outlives the magic
-- ring's 'withMagicRing' bracket; callers that retain past that
-- scope MUST @BS.copy@.
ringSlice :: MagicRing s -> Word64 -> Int -> ByteString
ringSlice ring pos (I# len#) =
  let !base = ringBase ring
      !msk  = ringMask ring
      !off  = fromIntegral pos .&. msk
      !(Ptr addr#) = base `plusPtr` off
  in BSI.BS (ForeignPtr addr# FinalPtr) (I# len#)
{-# INLINE ringSlice #-}

------------------------------------------------------------------------
-- High-level: full request / response head
------------------------------------------------------------------------

-- | Read a full HTTP\/1.x request head from the transport.
--
-- Composes:
--
--   1. 'readHeaderBlock' — magic-ring + SIMD CRLFCRLF.
--   2. 'Network.HTTP1.Parser.parseRequest' — SIMD tchar /
--      field-vchar scan over the resulting slice.
--   3. Smuggling-guard pipeline (validateHost, validateTarget,
--      requestFraming) shared with the classic parser.
--
-- Returns the parsed 'Request' (body slot stays @BodyEmpty@; the
-- caller wires it up using the returned 'Framing') or a 'ReadError'.
readRequestHead :: ReceiveTransport -> IO (Either ReadError (Request, Framing))
readRequestHead t = do
  startPos <- receiveLoadHead t
  fmap (fmap fst) (readRequestHeadFrom t startPos)
{-# INLINE readRequestHead #-}

-- | Like 'readRequestHead' but tracks position explicitly.  Returns
-- @(result, newPos)@ so callers can loop reading successive
-- requests on a keep-alive connection without re-reading
-- 'receiveLoadHead' (which gives the head, not the consumed
-- position) between iterations.
--
-- The header block is copied off the ring before 'parseRequest'
-- runs so the resulting 'Request's header slices stay valid past
-- the next 'receiveAdvanceTail' that the body reader (or the
-- next request's head reader) will issue.  Single allocation per
-- request — typically <2 KB on the wire — and it's the price we
-- pay for not racing the recv path on a long-lived keep-alive
-- connection where the magic ring will eventually wrap.
readRequestHeadFrom
  :: ReceiveTransport
  -> Word64
  -> IO (Either ReadError ((Request, Framing), Word64))
readRequestHeadFrom t startPos = do
  blockE <- readHeaderBlockFrom t startPos defaultHeaderBlockCap
  pure $ case blockE of
    Left e               -> Left e
    Right (block, nextPos) ->
      let !blockCopy = BS.copy block
      in case Classic.parseRequestLower blockCopy of
        Right ok -> Right (ok, nextPos)
        Left  e  -> Left (ReadParse e)
{-# INLINE readRequestHeadFrom #-}

-- | Read a full HTTP\/1.x response head from the transport.
--
-- Takes the request method so 'responseFraming' can apply the
-- HEAD \/ CONNECT special cases.
readResponseHead
  :: ReceiveTransport
  -> Method
  -> IO (Either ReadError (Response, Framing))
readResponseHead t reqMethod = do
  startPos <- receiveLoadHead t
  fmap (fmap fst) (readResponseHeadFrom t startPos reqMethod)
{-# INLINE readResponseHead #-}

readResponseHeadFrom
  :: ReceiveTransport
  -> Word64
  -> Method
  -> IO (Either ReadError ((Response, Framing), Word64))
readResponseHeadFrom t startPos reqMethod = do
  blockE <- readHeaderBlockFrom t startPos defaultHeaderBlockCap
  pure $ case blockE of
    Left e               -> Left e
    Right (block, nextPos) ->
      let !blockCopy = BS.copy block
      in case Classic.parseResponseLower reqMethod blockCopy of
        Right ok -> Right (ok, nextPos)
        Left  e  -> Left (ReadParse e)
{-# INLINE readResponseHeadFrom #-}

------------------------------------------------------------------------
-- Chunk-size line
------------------------------------------------------------------------

-- | Read one chunked TE @chunk-size [ chunk-ext ] CRLF@ line and
-- return the parsed size in bytes.  Walks the ring + SIMD CR scanner
-- to find the CRLF, then defers to 'Classic.parseChunkSize' for the
-- hex / extension grammar.
readChunkSizeLine
  :: ReceiveTransport
  -> IO (Either ReadError Word64)
readChunkSizeLine t = do
  startPos <- receiveLoadHead t
  fmap (fmap fst) (readChunkSizeLineFrom t startPos)

readChunkSizeLineFrom
  :: ReceiveTransport
  -> Word64
  -> IO (Either ReadError (Word64, Word64))
readChunkSizeLineFrom t startPos = do
  let !ring = receiveRing t
      !base = ringBase ring
      !msk  = ringMask ring
  loop ring base msk 0
  where
    loop !ring !base !msk !scanFrom = do
      h <- receiveLoadHead t
      let !avail = fromIntegral (h - startPos) :: Int
      mIdx <- findCRLF base msk startPos scanFrom avail
      case mIdx of
        Just idx
          -- Found the CRLF. The chunk-size line itself is @idx@
          -- bytes long; that's what the cap is meant to bound,
          -- not how much was happening to be buffered in the
          -- ring when we noticed it. (On a fast streaming
          -- response the ring routinely contains many full
          -- chunked-transfer chunks beyond the size line we're
          -- about to read.)
          | idx > defaultChunkLineCap ->
              pure (Left (ReadMessageTooLong defaultChunkLineCap))
          | otherwise -> do
              let !lineLen = idx
                  !line    = ringSlice ring startPos lineLen
                  !nextPos = startPos + fromIntegral (lineLen + 2)
              receiveAdvanceTail t nextPos
              pure $ case Classic.parseChunkSize line of
                Right n -> Right (n, nextPos)
                Left e  -> Left (ReadParse e)
        Nothing
          -- No CRLF yet. If we've already buffered more than the
          -- cap without finding one, the size line is either
          -- genuinely too long or malformed.
          | avail > defaultChunkLineCap ->
              pure (Left (ReadMessageTooLong defaultChunkLineCap))
          | otherwise -> do
              let !nextScan = max 0 (avail - 1)
              r <- receiveWaitData t h
              case r of
                ReceiveMoreData _         -> loop ring base msk nextScan
                ReceiveEndOfInput         -> pure (Left ReadUnexpectedEof)
                ReceiveFailed exc -> pure (Left (ReadTransportError exc))

------------------------------------------------------------------------
-- Body-shaped readers
------------------------------------------------------------------------

-- | Pull at most @want@ bytes starting at @startPos@.  Blocks until
-- at least one byte is available; returns @(slice, newPos)@ on
-- success, @Left ReadUnexpectedEof@ on EOF before any bytes arrived.
--
-- Used by the @ContentLength@ and @CloseDelimited@ body-stream
-- producers in 'Network.HTTP1.Connection'.  The returned slice is
-- zero-copy into the ring memory and is invalidated by the next
-- 'advancePast' / 'receiveAdvanceTail' call — i.e. the caller
-- must hand it to the application synchronously before pulling
-- another chunk.
readUpTo
  :: ReceiveTransport
  -> Word64       -- ^ start position
  -> Int          -- ^ maximum bytes to return (clamped to ring size)
  -> IO (Either ReadError (ByteString, Word64))
readUpTo t startPos want = do
  let !ring = receiveRing t
  h0 <- receiveLoadHead t
  let !haveNow = h0 - startPos
  if haveNow > 0
    then deliver ring startPos h0
    else do
      r <- receiveWaitData t h0
      case r of
        ReceiveMoreData h1 -> deliver ring startPos h1
        ReceiveEndOfInput  -> pure (Left ReadUnexpectedEof)
        ReceiveFailed exc -> pure (Left (ReadTransportError exc))
  where
    deliver ring s h =
      let !want64 = fromIntegral want :: Word64
          !taken  = fromIntegral (min (h - s) want64) :: Int
          !slice  = ringSlice ring s taken
          !next   = s + fromIntegral taken
      in pure (Right (slice, next))

-- | Pull exactly @n@ bytes starting at @startPos@.  Blocks until
-- they arrive; returns @(slice, newPos)@ on success, @Left
-- ReadUnexpectedEof@ if EOF arrives first.
readExact
  :: ReceiveTransport
  -> Word64
  -> Int
  -> IO (Either ReadError (ByteString, Word64))
readExact t startPos n = do
  let !ring = receiveRing t
      !need64 = fromIntegral n :: Word64
  e <- ensureBytes t startPos need64
  case e of
    Left err -> pure (Left err)
    Right _  -> do
      let !slice = ringSlice ring startPos n
          !next  = startPos + need64
      pure (Right (slice, next))

-- | Outcome of a strict CRLF line read.
data StrictLine
  = StrictBareLf
    -- ^ A bare LF (no preceding CR) appeared before the next CRLF.
    --   Forbidden inside the trailer section (smuggling vector).
  | StrictLine !ByteString
    -- ^ Successfully read a full @line CRLF@; @line@ is the slice
    --   /without/ the trailing CRLF.  An empty line marks the end
    --   of the trailer section.

-- | Read one strict CRLF-terminated line.  Returns the slice (no
-- trailing CRLF), or 'Nothing' on EOF / oversize.  A bare LF
-- between the current position and the next CRLF is surfaced as
-- 'StrictBareLf' (smuggling guard for the chunked-TE trailer
-- section, RFC 9112 § 2.2).
readUntilCRLFStrict
  :: ReceiveTransport
  -> Word64
  -> Int          -- ^ hard byte cap
  -> IO (Either ReadError (Maybe StrictLine, Word64))
readUntilCRLFStrict t startPos cap = do
  let !ring = receiveRing t
      !base = ringBase ring
      !msk  = ringMask ring
  loop ring base msk 0
  where
    loop !ring !base !msk !scanFrom = do
      h <- receiveLoadHead t
      let !avail = fromIntegral (h - startPos) :: Int
      if avail > cap
        then pure (Right (Nothing, startPos))
        else do
          outcome <- scanCrlfOrBareLf base msk startPos scanFrom avail
          case outcome of
            ScanBareLf -> pure (Right (Just StrictBareLf, startPos))
            ScanFoundCrlf idx -> do
              let !lineLen = idx
                  !line    = ringSlice ring startPos lineLen
                  !nextPos = startPos + fromIntegral (lineLen + 2)
              receiveAdvanceTail t nextPos
              pure (Right (Just (StrictLine line), nextPos))
            ScanNeedMore -> do
              let !nextScan = max 0 (avail - 1)
              r <- receiveWaitData t h
              case r of
                ReceiveMoreData _         -> loop ring base msk nextScan
                ReceiveEndOfInput         -> pure (Right (Nothing, startPos))
                ReceiveFailed exc -> pure (Left (ReadTransportError exc))

data ScanResult
  = ScanFoundCrlf !Int
  | ScanBareLf
  | ScanNeedMore

scanCrlfOrBareLf
  :: Ptr Word8
  -> Int
  -> Word64
  -> Int            -- ^ scan-from offset
  -> Int            -- ^ avail
  -> IO ScanResult
scanCrlfOrBareLf base msk startPos scanFrom avail
  | avail < 2 && scanFrom >= avail = pure ScanNeedMore
  | otherwise = do
      let !startOff = fromIntegral startPos .&. msk
          !startPtr = base `plusPtr` startOff
      go startPtr scanFrom
  where
    go !ptr !i
      | i >= avail = pure ScanNeedMore
      | otherwise = do
          let !crPos = fromIntegral
                (c_find_cr (castPtr ptr) (fromIntegral i) (fromIntegral avail))
              !lfPos = fromIntegral
                (c_find_lf (castPtr ptr) (fromIntegral i) (fromIntegral avail))
          if lfPos < crPos
            then pure ScanBareLf
            else if crPos >= avail
              then pure ScanNeedMore
              else if crPos + 1 >= avail
                then pure ScanNeedMore
                else do
                  b1 <- peekByteOff ptr (crPos + 1) :: IO Word8
                  if b1 == 0x0a
                    then pure (ScanFoundCrlf crPos)
                    else go ptr (crPos + 1)

foreign import ccall unsafe "hs_http1_find_lf"
  c_find_lf :: Ptr Word8 -> CInt -> CInt -> CInt

-- | Tell the transport that bytes up to @pos@ are no longer needed
-- (releases space in the ring for the recv path to reuse).  Wraps
-- 'receiveAdvanceTail' so the connection-layer call sites don't
-- have to import the transport surface directly.
advancePast :: ReceiveTransport -> Word64 -> IO ()
advancePast = receiveAdvanceTail
{-# INLINE advancePast #-}

------------------------------------------------------------------------
-- Shared ensureBytes (also used by readFrame-style callers)
------------------------------------------------------------------------

-- | Block until at least @needed@ bytes are available past @startPos@.
-- Returns the (latest known) head on success.
ensureBytes
  :: ReceiveTransport
  -> Word64
  -> Word64
  -> IO (Either ReadError Word64)
ensureBytes t startPos needed = loop
  where
    loop = do
      h <- receiveLoadHead t
      if h - startPos >= needed
        then pure (Right h)
        else do
          r <- receiveWaitData t h
          case r of
            ReceiveMoreData _         -> loop
            ReceiveEndOfInput         -> pure (Left ReadUnexpectedEof)
            ReceiveFailed exc -> pure (Left (ReadTransportError exc))
{-# INLINE ensureBytes #-}

